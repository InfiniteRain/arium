const std = @import("std");
const shared = @import("shared");
const parsed_ast_mod = @import("../parser/parsed_ast.zig");
const sema_ast_mod = @import("sema_ast.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const parser_mod = @import("../parser/parser.zig");
const limits = @import("../limits.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const meta = std.meta;
const allocPrint = std.fmt.allocPrint;
const expectError = std.testing.expectError;
const SharedDiags = shared.Diags;
const Writer = shared.Writer;
const ParsedExpr = parsed_ast_mod.ParsedExpr;
const ParsedStmt = parsed_ast_mod.ParsedStmt;
const ParsedType = parsed_ast_mod.ParsedType;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const SemaType = sema_ast_mod.SemaType;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Position = tokenizer_mod.Position;
const Parser = parser_mod.Parser;

pub const Sema = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        SemaFailure,
    };

    pub const DiagEntry = struct {
        pub const SemaTypeTuple = struct {
            SemaType,
            SemaType,
        };

        pub const Kind = union(enum) {
            expected_expr_type: SemaType,
            unexpected_arithmetic_type: SemaType,
            unexpected_operand_type: SemaTypeTuple,
            unexpected_concat_type: SemaTypeTuple,
            unexpected_equality_type: SemaTypeTuple,
            unexpected_comparison_type: SemaType,
            unexpected_logical_type: SemaType,
            unexpected_logical_negation_type: SemaType,
            unexpected_arithmetic_negation_type: SemaType,
            too_many_locals,
            value_not_found: []const u8,
            unexpected_assignment_type: SemaTypeTuple,
            immutable_mutation: []const u8,
            type_not_found: []const u8,
            value_not_assigned: []const u8,
        };

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            switch (self.kind) {
                .value_not_found,
                .immutable_mutation,
                .type_not_found,
                .value_not_assigned,
                => |name| allocator.free(name),

                .expected_expr_type,
                .unexpected_arithmetic_type,
                .unexpected_operand_type,
                .unexpected_concat_type,
                .unexpected_equality_type,
                .unexpected_comparison_type,
                .unexpected_logical_type,
                .unexpected_logical_negation_type,
                .unexpected_arithmetic_negation_type,
                .too_many_locals,
                .unexpected_assignment_type,
                => {},
            }
        }

        kind: Kind,
        position: Position,
    };

    pub const Diags = SharedDiags(DiagEntry);

    const Scope = struct {
        allocator: Allocator,
        prev: ?*Scope = null,
        types_map: StringHashMap(SemaType),
        locals_map: StringHashMap(usize),
        locals_top: usize,

        fn init(allocator: Allocator, prev_opt: ?*Scope) Scope {
            return .{
                .allocator = allocator,
                .prev = prev_opt,
                .types_map = StringHashMap(SemaType).init(allocator),
                .locals_map = StringHashMap(usize).init(allocator),
                .locals_top = if (prev_opt) |prev| prev.locals_top else 0,
            };
        }
    };

    const Local = struct {
        sema_type: ?SemaType,
        is_mutable: bool,
        is_assigned: bool,
    };

    allocator: Allocator,
    diags: ?*Diags = null,
    had_error: bool = false,
    locals: ArrayList(Local) = undefined,
    current_scope: *Scope = undefined,

    pub fn init(allocator: *ArenaAllocator) Self {
        return .{
            .allocator = allocator.allocator(),
        };
    }

    pub fn analyze(
        self: *Self,
        block: *ParsedExpr,
        diags: ?*Diags,
    ) Error!*SemaExpr {
        self.diags = diags;
        self.had_error = false;

        var current_scope = Scope.init(self.allocator, null);
        self.current_scope = &current_scope;

        // todo: is this the best place for this?
        try self.current_scope.types_map.put("Int", .int);
        try self.current_scope.types_map.put("Float", .float);
        try self.current_scope.types_map.put("Bool", .bool);
        try self.current_scope.types_map.put("String", .string);

        self.locals = ArrayList(Local).init(self.allocator);

        const sema_block = try self.analyzeExpr(block);

        if (self.had_error) {
            return error.SemaFailure;
        }

        return sema_block;
    }

    fn analyzeStmt(self: *Self, stmt: *ParsedStmt) Error!*SemaStmt {
        return switch (stmt.kind) {
            .assert => |*assert| try self.analyzeAssertStmt(assert, stmt.position),
            .print => |*print| try self.analyzePrintStmt(print, stmt.position),
            .expr => |*expr| try self.analyzeExprStmt(expr, stmt.position),
            .let => |*let| try self.analyzeLetStmt(let, stmt.position),
        };
    }

    fn analyzeAssertStmt(
        self: *Self,
        assert: *ParsedStmt.Kind.Assert,
        position: Position,
    ) Error!*SemaStmt {
        const expr = try self.analyzeExpr(assert.expr);

        if (expr.sema_type != .bool) {
            return self.semaFailure(
                position,
                .{ .expected_expr_type = .bool },
            );
        }

        return try SemaStmt.Kind.Assert.create(self.allocator, expr, position);
    }

    fn analyzePrintStmt(
        self: *Self,
        print: *ParsedStmt.Kind.Print,
        position: Position,
    ) Error!*SemaStmt {
        const expr = try self.analyzeExpr(print.expr);

        return try SemaStmt.Kind.Print.create(self.allocator, expr, position);
    }

    fn analyzeExprStmt(
        self: *Self,
        expr: *ParsedStmt.Kind.Expr,
        position: Position,
    ) Error!*SemaStmt {
        const inner_expr = try self.analyzeExpr(expr.expr);

        return try SemaStmt.Kind.Expr.create(
            self.allocator,
            inner_expr,
            position,
        );
    }

    fn analyzeLetStmt(
        self: *Self,
        let: *ParsedStmt.Kind.Let,
        position: Position,
    ) Error!*SemaStmt {
        const analyzed_type_opt: ?SemaType = if (let.parsed_type) |parsed_type|
            try self.analyzeType(parsed_type)
        else
            null;

        if (let.expr == null) {
            const index = try self.declareVariable(
                let.is_mutable,
                false,
                let.name,
                analyzed_type_opt,
            );

            return try SemaStmt.Kind.Let.create(
                self.allocator,
                index,
                null,
                position,
            );
        }

        const expr = self.analyzeExpr(let.expr.?) catch |err| switch (err) {
            error.SemaFailure => try SemaExpr.Kind.Literal.create(
                self.allocator,
                .invalid,
                position,
            ),
            error.OutOfMemory => return error.OutOfMemory,
        };

        if (self.current_scope.locals_top >= limits.max_locals) {
            try self.addDiag(position, .too_many_locals);
        }

        var sema_type = expr.sema_type;

        if (analyzed_type_opt) |analyzed_type| {
            if (typeSatisfies(expr.sema_type, analyzed_type)) {
                sema_type = analyzed_type;
            } else {
                try self.addDiag(position, .{ .expected_expr_type = analyzed_type });
                sema_type = .invalid;
            }
        }

        const index = try self.declareVariable(
            let.is_mutable,
            true,
            let.name,
            sema_type,
        );

        return try SemaStmt.Kind.Let.create(
            self.allocator,
            index,
            expr,
            position,
        );
    }

    fn analyzeType(self: *Self, parsed_type: *ParsedType) Error!SemaType {
        return switch (parsed_type.kind) {
            .identifier => |identifier| try self.analyzeIdentifierType(identifier, parsed_type.position),
        };
    }

    fn analyzeIdentifierType(
        self: *Self,
        identifier: ParsedType.Kind.Identifier,
        position: Position,
    ) Error!SemaType {
        return self.getType(identifier.name) orelse
            self.semaFailure(position, .{ .type_not_found = identifier.name });
    }

    fn analyzeExpr(
        self: *Self,
        expr: *ParsedExpr,
    ) Error!*SemaExpr {
        return switch (expr.kind) {
            .literal => |*literal| try self.analyzeLiteralExpr(literal, expr.position),
            .binary => |*binary| try self.analyzeBinaryExpr(binary, expr.position),
            .unary => |*unary| try self.analyzeUnaryExpr(unary, expr.position),
            .block => |*block| try self.analyzeBlockExpr(block, expr.position),
            .variable => |*variable| try self.analyzeVariableExpr(variable, expr.position),
            .assignment => |*assignment| try self.analyzeAssignmentExpr(assignment, expr.position),
        };
    }

    fn analyzeLiteralExpr(
        self: *Self,
        literal: *ParsedExpr.Kind.Literal,
        position: Position,
    ) Error!*SemaExpr {
        const literal_variant: SemaExpr.Kind.Literal = switch (literal.kind) {
            .unit => .unit,
            .int => |int| .{ .int = int },
            .float => |float| .{ .float = float },
            .bool => |bool_| .{ .bool = bool_ },
            .string => |string| blk: {
                const duped_string = try self.allocator.dupe(u8, string);

                break :blk .{ .string = duped_string };
            },
        };

        return SemaExpr.Kind.Literal.create(self.allocator, literal_variant, position);
    }

    fn analyzeBinaryExpr(
        self: *Self,
        binary: *ParsedExpr.Kind.Binary,
        position: Position,
    ) Error!*SemaExpr {
        const left = try self.analyzeExpr(binary.left);
        const right = try self.analyzeExpr(binary.right);

        if (left.sema_type == .invalid or right.sema_type == .invalid) {
            return try SemaExpr.Kind.Binary.create(
                self.allocator,
                .add_int, // binary kind doesn't matter here
                .invalid,
                position,
                left,
                right,
            );
        }

        const sema_type = switch (binary.kind) {
            .add,
            .subtract,
            .divide,
            .multiply,
            => try self.analyzeArithmeticBinaryExpr(left, right, position),

            .concat,
            => try self.analyzeConcatBinaryExpr(left, right, position),

            .equal,
            .not_equal,
            => try self.analyzeEqualBinaryExpr(left, right, position),

            .greater,
            .greater_equal,
            .less,
            .less_equal,
            => try self.analyzeComparisonBinaryExpr(left, right, position),

            .or_,
            .and_,
            => try self.analyzeLogicalBinaryExpr(left, right, position),
        };

        return try SemaExpr.Kind.Binary.create(
            self.allocator,
            translateBinaryKind(left.sema_type, binary.kind),
            sema_type,
            position,
            left,
            right,
        );
    }

    fn analyzeArithmeticBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (left.sema_type != .int and left.sema_type != .float) {
            return self.semaFailure(
                position,
                .{ .unexpected_arithmetic_type = left.sema_type },
            );
        }

        if (left.sema_type.tag() != right.sema_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return left.sema_type;
    }

    fn analyzeConcatBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (left.sema_type != .string or right.sema_type != .string) {
            return self.semaFailure(
                position,
                .{ .unexpected_concat_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return left.sema_type;
    }

    fn analyzeEqualBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (left.sema_type.tag() != right.sema_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_equality_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeComparisonBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (left.sema_type != .int and left.sema_type != .float) {
            return self.semaFailure(
                position,
                .{ .unexpected_comparison_type = left.sema_type },
            );
        }

        if (left.sema_type.tag() != right.sema_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeLogicalBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (left.sema_type != .bool) {
            return self.semaFailure(
                position,
                .{ .unexpected_logical_type = left.sema_type },
            );
        }

        if (left.sema_type.tag() != right.sema_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.sema_type, right.sema_type } },
            );
        }

        return .bool;
    }

    fn analyzeUnaryExpr(
        self: *Self,
        unary: *ParsedExpr.Kind.Unary,
        position: Position,
    ) Error!*SemaExpr {
        const right = try self.analyzeExpr(unary.right);

        if (right.sema_type == .invalid) {
            return try SemaExpr.Kind.Unary.create(
                self.allocator,
                .negate_bool, // unary kind doesn't matter here
                .invalid,
                position,
                right,
            );
        }

        const sema_type = switch (unary.kind) {
            .negate_bool,
            => try self.analyzeNegateBoolUnaryExpr(right, position),

            .negate_num,
            => try self.analyzeNegateNumUnaryExpr(right, position),
        };

        return try SemaExpr.Kind.Unary.create(
            self.allocator,
            translateUnaryKind(right.sema_type, unary.kind),
            sema_type,
            position,
            right,
        );
    }

    fn analyzeNegateBoolUnaryExpr(
        self: *Self,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (right.sema_type != .bool) {
            return self.semaFailure(
                position,
                .{ .unexpected_logical_negation_type = right.sema_type },
            );
        }

        return right.sema_type;
    }

    fn analyzeNegateNumUnaryExpr(
        self: *Self,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaType {
        if (right.sema_type != .int or right.sema_type != .float) {
            return self.semaFailure(
                position,
                .{ .unexpected_arithmetic_negation_type = right.sema_type },
            );
        }

        return right.sema_type;
    }

    fn analyzeBlockExpr(
        self: *Self,
        block: *ParsedExpr.Kind.Block,
        position: Position,
    ) Error!*SemaExpr {
        var scope = Scope.init(self.allocator, self.current_scope);

        self.current_scope = &scope;

        var sema_stmts = ArrayList(*SemaStmt).init(self.allocator);

        for (block.stmts.items) |parser_stmt| {
            const sema_stmt = self.analyzeStmt(parser_stmt) catch |err| switch (err) {
                error.SemaFailure => try SemaStmt.Kind.Expr.create(
                    self.allocator,
                    try SemaExpr.Kind.Literal.create(
                        self.allocator,
                        .invalid,
                        parser_stmt.position,
                    ),
                    parser_stmt.position,
                ),
                error.OutOfMemory => return error.OutOfMemory,
            };

            try sema_stmts.append(sema_stmt);
        }

        const last_stmt = sema_stmts.items[sema_stmts.items.len - 1];
        const sema_type = last_stmt.kind.expr.expr.sema_type;

        self.current_scope = self.current_scope.prev.?;

        return try SemaExpr.Kind.Block.create(
            self.allocator,
            sema_stmts,
            sema_type,
            position,
        );
    }

    fn analyzeVariableExpr(
        self: *Self,
        variable: *ParsedExpr.Kind.Variable,
        position: Position,
    ) Error!*SemaExpr {
        const index, const local = self.getVariable(variable.name) orelse {
            return self.semaFailure(
                position,
                .{ .value_not_found = variable.name },
            );
        };

        if (!local.is_assigned) {
            return self.semaFailure(
                position,
                .{ .value_not_assigned = variable.name },
            );
        }

        return try SemaExpr.Kind.Variable.create(
            self.allocator,
            index,
            local.sema_type.?,
            position,
        );
    }

    fn analyzeAssignmentExpr(
        self: *Self,
        assignment: *ParsedExpr.Kind.Assigment,
        position: Position,
    ) Error!*SemaExpr {
        const index, const local = self.getVariable(assignment.name) orelse {
            return self.semaFailure(
                position,
                .{ .value_not_found = assignment.name },
            );
        };

        if (!local.is_mutable and local.is_assigned) {
            return self.semaFailure(
                position,
                .{ .immutable_mutation = assignment.name },
            );
        }

        const right = try self.analyzeExpr(assignment.right);

        self.locals.items[index].is_assigned = true;

        if (local.sema_type != null and right.sema_type.tag() != local.sema_type.?.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_assignment_type = .{ local.sema_type.?, right.sema_type } },
            );
        } else if (local.sema_type == null) {
            self.locals.items[index].sema_type = right.sema_type;
        }

        return try SemaExpr.Kind.Assignment.create(
            self.allocator,
            index,
            right,
            .unit,
            position,
        );
    }

    fn getType(
        self: *Self,
        name: []const u8,
    ) ?SemaType {
        var current_scope: ?*Scope = self.current_scope;

        while (current_scope) |scope| {
            if (scope.types_map.get(name)) |sema_type| {
                return sema_type;
            }

            current_scope = scope.prev;
        }

        return null;
    }

    fn declareVariable(
        self: *Self,
        is_mutable: bool,
        is_assigned: bool,
        name: []const u8,
        sema_type: ?SemaType,
    ) error{OutOfMemory}!usize {
        const scope = self.current_scope;
        const local: Local = .{
            .sema_type = sema_type,
            .is_mutable = is_mutable,
            .is_assigned = is_assigned,
        };

        if (scope.locals_top == self.locals.items.len) {
            try self.locals.append(local);
        } else {
            self.locals.items[scope.locals_top] = local;
        }

        try self.current_scope.locals_map.put(name, scope.locals_top);
        scope.locals_top += 1;

        return scope.locals_top - 1;
    }

    fn getVariable(
        self: *Self,
        name: []const u8,
    ) ?struct { usize, Local } {
        var current_scope: ?*Scope = self.current_scope;

        while (current_scope) |scope| {
            if (scope.locals_map.get(name)) |index| {
                return .{ index, self.locals.items[index] };
            }

            current_scope = scope.prev;
        }

        return null;
    }

    fn typeSatisfies(sema_type: SemaType, target: SemaType) bool {
        return sema_type.tag() == target.tag();
    }

    fn semaFailure(
        self: *Self,
        position: Position,
        diag_kind: DiagEntry.Kind,
    ) Error {
        try self.addDiag(position, diag_kind);
        return error.SemaFailure;
    }

    fn addDiag(
        self: *Self,
        position: Position,
        diag_kind: DiagEntry.Kind,
    ) Error!void {
        self.had_error = true;

        if (self.diags) |diags| {
            // in case of ever needing to alloc something in here, make sure to
            // use diags.allocator instead of self.allocator. this is
            // necessary for lang-tests where a new allocator is created for
            // each test to detect memory leaks. that allocator then gets
            // deinited while diags are owned by the tests.
            try diags.add(.{
                .kind = switch (diag_kind) {
                    .value_not_found,
                    => |name| .{ .value_not_found = try diags.allocator.dupe(u8, name) },

                    .immutable_mutation,
                    => |name| .{ .immutable_mutation = try diags.allocator.dupe(u8, name) },

                    .type_not_found,
                    => |name| .{ .type_not_found = try diags.allocator.dupe(u8, name) },

                    .value_not_assigned,
                    => |name| .{ .value_not_assigned = try diags.allocator.dupe(u8, name) },

                    .expected_expr_type,
                    .unexpected_arithmetic_type,
                    .unexpected_operand_type,
                    .unexpected_concat_type,
                    .unexpected_equality_type,
                    .unexpected_comparison_type,
                    .unexpected_logical_type,
                    .unexpected_logical_negation_type,
                    .unexpected_arithmetic_negation_type,
                    .too_many_locals,
                    .unexpected_assignment_type,
                    => diag_kind,
                },
                .position = position,
            });
        }
    }

    fn translateBinaryKind(
        sema_type: SemaType,
        binary_kind: ParsedExpr.Kind.Binary.Kind,
    ) SemaExpr.Kind.Binary.Kind {
        return switch (sema_type) {
            .int => switch (binary_kind) {
                .add => .add_int,
                .subtract => .subtract_int,
                .divide => .divide_int,
                .multiply => .multiply_int,
                .equal => .equal_int,
                .not_equal => .not_equal_int,
                .greater => .greater_int,
                .greater_equal => .greater_equal_int,
                .less => .less_int,
                .less_equal => .less_equal_int,

                .concat,
                .and_,
                .or_,
                => unreachable,
            },
            .float => switch (binary_kind) {
                .add => .add_float,
                .subtract => .subtract_float,
                .divide => .divide_float,
                .multiply => .multiply_float,
                .equal => .equal_float,
                .not_equal => .not_equal_float,
                .greater => .greater_float,
                .greater_equal => .greater_equal_float,
                .less => .less_float,
                .less_equal => .less_equal_float,

                .concat,
                .and_,
                .or_,
                => unreachable,
            },
            .bool => switch (binary_kind) {
                .equal => .equal_bool,
                .not_equal => .not_equal_bool,
                .and_ => .and_,
                .or_ => .or_,

                .add,
                .subtract,
                .divide,
                .multiply,
                .concat,
                .greater,
                .greater_equal,
                .less,
                .less_equal,
                => unreachable,
            },
            .string => switch (binary_kind) {
                .concat => .concat,
                .equal => .equal_obj,
                .not_equal => .not_equal_obj,

                .add,
                .subtract,
                .divide,
                .multiply,
                .greater,
                .greater_equal,
                .less,
                .less_equal,
                .and_,
                .or_,
                => unreachable,
            },

            .unit,
            .invalid,
            => unreachable,
        };
    }

    fn translateUnaryKind(
        sema_type: SemaType,
        unary_kind: ParsedExpr.Kind.Unary.Kind,
    ) SemaExpr.Kind.Unary.Kind {
        return switch (sema_type) {
            .int => switch (unary_kind) {
                .negate_num,
                => .negate_int,

                .negate_bool,
                => unreachable,
            },
            .float => switch (unary_kind) {
                .negate_num,
                => .negate_float,

                .negate_bool,
                => unreachable,
            },
            .bool => switch (unary_kind) {
                .negate_bool,
                => .negate_bool,

                .negate_num,
                => unreachable,
            },

            .string,
            .unit,
            .invalid,
            => unreachable,
        };
    }
};

test "should free all memory on successful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    var arena_allocator = ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const source = "print (2 + 2) * -2";
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(&arena_allocator);
    const parsed_expr = try parser.parse(&tokenizer, null);

    // WHEN - THEN
    var sema = Sema.init(&arena_allocator);
    _ = try sema.analyze(parsed_expr, null);
}

test "should free all memory on unsuccessful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    var arena_allocator = ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const source = "print (2 + 2) * -false";
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(&arena_allocator);

    const parsed_expr = try parser.parse(&tokenizer, null);

    // WHEN - THEN
    var sema = Sema.init(&arena_allocator);

    const result = sema.analyze(parsed_expr, null);
    try expectError(Sema.Error.SemaFailure, result);
}
