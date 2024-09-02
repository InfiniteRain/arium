const std = @import("std");
const shared = @import("shared");
const parsed_expr_mod = @import("../parser/parsed_expr.zig");
const parsed_stmt_mod = @import("../parser/parsed_stmt.zig");
const sema_expr_mod = @import("sema_expr.zig");
const sema_stmt_mod = @import("sema_stmt.zig");
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
const ParsedExpr = parsed_expr_mod.ParsedExpr;
const ParsedStmt = parsed_stmt_mod.ParsedStmt;
const SemaExpr = sema_expr_mod.SemaExpr;
const SemaStmt = sema_stmt_mod.SemaStmt;
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
        pub const EvalTypeTuple = struct {
            SemaExpr.EvalType,
            SemaExpr.EvalType,
        };

        pub const Kind = union(enum) {
            expected_expr_type: SemaExpr.EvalType,
            unexpected_arithmetic_type: SemaExpr.EvalType,
            unexpected_operand_type: EvalTypeTuple,
            unexpected_concat_type: EvalTypeTuple,
            unexpected_equality_type: EvalTypeTuple,
            unexpected_comparison_type: SemaExpr.EvalType,
            unexpected_logical_type: SemaExpr.EvalType,
            unexpected_logical_negation_type: SemaExpr.EvalType,
            unexpected_arithmetic_negation_type: SemaExpr.EvalType,
            too_many_locals,
            value_not_found: []const u8,
            unexpected_assignment_type: EvalTypeTuple,
            immutable_mutation: []const u8,
        };

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            switch (self.kind) {
                .value_not_found,
                => |name| allocator.free(name),

                .immutable_mutation,
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
        locals_map: StringHashMap(usize),
        locals_top: usize,

        fn init(allocator: Allocator, prev_opt: ?*Scope) Scope {
            return .{
                .allocator = allocator,
                .prev = prev_opt,
                .locals_map = StringHashMap(usize).init(allocator),
                .locals_top = if (prev_opt) |prev| prev.locals_top else 0,
            };
        }
    };

    const Local = struct {
        eval_type: SemaExpr.EvalType,
        is_mutable: bool,
    };

    allocator: Allocator,
    diags: ?*Diags = null,
    had_error: bool = false,
    locals: ArrayList(Local) = undefined,
    current_scope: ?*Scope = null,

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
        self.current_scope = null;
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

        if (expr.eval_type != .bool) {
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
        const expr = self.analyzeExpr(let.expr) catch |err| switch (err) {
            error.SemaFailure => try SemaExpr.Kind.Literal.create(
                self.allocator,
                .invalid,
                position,
            ),
            error.OutOfMemory => return error.OutOfMemory,
        };

        if (self.current_scope.?.locals_top >= limits.max_locals) {
            try self.addDiag(position, .too_many_locals);
        }

        const index = try self.declareVariable(let.is_mutable, let.name, expr.eval_type);

        return try SemaStmt.Kind.Let.create(
            self.allocator,
            index,
            expr,
            position,
        );
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

        if (left.eval_type == .invalid or right.eval_type == .invalid) {
            return try SemaExpr.Kind.Binary.create(
                self.allocator,
                .add_int, // binary kind doesn't matter here
                .invalid,
                position,
                left,
                right,
            );
        }

        const eval_type = switch (binary.kind) {
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
            translateBinaryKind(left.eval_type, binary.kind),
            eval_type,
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
    ) Error!SemaExpr.EvalType {
        if (left.eval_type != .int and left.eval_type != .float) {
            return self.semaFailure(
                position,
                .{ .unexpected_arithmetic_type = left.eval_type },
            );
        }

        if (left.eval_type.tag() != right.eval_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } },
            );
        }

        return left.eval_type;
    }

    fn analyzeConcatBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaExpr.EvalType {
        if (!isString(left.eval_type) or !isString(right.eval_type)) {
            return self.semaFailure(
                position,
                .{ .unexpected_concat_type = .{ left.eval_type, right.eval_type } },
            );
        }

        return left.eval_type;
    }

    fn analyzeEqualBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaExpr.EvalType {
        if (left.eval_type.tag() != right.eval_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_equality_type = .{ left.eval_type, right.eval_type } },
            );
        }

        return .bool;
    }

    fn analyzeComparisonBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaExpr.EvalType {
        if (left.eval_type != .int and left.eval_type != .float) {
            return self.semaFailure(
                position,
                .{ .unexpected_comparison_type = left.eval_type },
            );
        }

        if (left.eval_type.tag() != right.eval_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } },
            );
        }

        return .bool;
    }

    fn analyzeLogicalBinaryExpr(
        self: *Self,
        left: *SemaExpr,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaExpr.EvalType {
        if (left.eval_type != .bool) {
            return self.semaFailure(
                position,
                .{ .unexpected_logical_type = left.eval_type },
            );
        }

        if (left.eval_type.tag() != right.eval_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } },
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

        if (right.eval_type == .invalid) {
            return try SemaExpr.Kind.Unary.create(
                self.allocator,
                .negate_bool, // unary kind doesn't matter here
                .invalid,
                position,
                right,
            );
        }

        const eval_type = switch (unary.kind) {
            .negate_bool,
            => try self.analyzeNegateBoolUnaryExpr(right, position),

            .negate_num,
            => try self.analyzeNegateNumUnaryExpr(right, position),
        };

        return try SemaExpr.Kind.Unary.create(
            self.allocator,
            translateUnaryKind(right.eval_type, unary.kind),
            eval_type,
            position,
            right,
        );
    }

    fn analyzeNegateBoolUnaryExpr(
        self: *Self,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaExpr.EvalType {
        if (right.eval_type != .bool) {
            return self.semaFailure(
                position,
                .{ .unexpected_logical_negation_type = right.eval_type },
            );
        }

        return right.eval_type;
    }

    fn analyzeNegateNumUnaryExpr(
        self: *Self,
        right: *SemaExpr,
        position: Position,
    ) Error!SemaExpr.EvalType {
        if (right.eval_type != .int or right.eval_type != .float) {
            return self.semaFailure(
                position,
                .{ .unexpected_arithmetic_negation_type = right.eval_type },
            );
        }

        return right.eval_type;
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
        const eval_type = last_stmt.kind.expr.expr.eval_type;

        self.current_scope = self.current_scope.?.prev;

        return try SemaExpr.Kind.Block.create(
            self.allocator,
            sema_stmts,
            eval_type,
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

        return try SemaExpr.Kind.Variable.create(
            self.allocator,
            index,
            local.eval_type,
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

        const right = try self.analyzeExpr(assignment.right);

        if (right.eval_type.tag() != local.eval_type.tag()) {
            return self.semaFailure(
                position,
                .{ .unexpected_assignment_type = .{ local.eval_type, right.eval_type } },
            );
        }

        if (!local.is_mutable) {
            return self.semaFailure(
                position,
                .{ .immutable_mutation = assignment.name },
            );
        }

        return try SemaExpr.Kind.Assignment.create(
            self.allocator,
            index,
            right,
            .unit,
            position,
        );
    }

    fn declareVariable(
        self: *Self,
        is_mutable: bool,
        name: []const u8,
        eval_type: SemaExpr.EvalType,
    ) error{OutOfMemory}!usize {
        const scope = self.current_scope orelse @panic("unitialized scope");
        const local: Local = .{
            .eval_type = eval_type,
            .is_mutable = is_mutable,
        };

        if (scope.locals_top == self.locals.items.len) {
            try self.locals.append(local);
        } else {
            self.locals.items[scope.locals_top] = local;
        }

        try self.current_scope.?.locals_map.put(name, scope.locals_top);
        scope.locals_top += 1;

        return scope.locals_top - 1;
    }

    fn getVariable(
        self: *Self,
        name: []const u8,
    ) ?struct { usize, Local } {
        var current_map = self.current_scope;

        while (current_map) |local_map| {
            if (local_map.locals_map.get(name)) |index| {
                return .{ index, self.locals.items[index] };
            }

            current_map = local_map.prev;
        }

        return null;
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
        eval_type: SemaExpr.EvalType,
        binary_kind: ParsedExpr.Kind.Binary.Kind,
    ) SemaExpr.Kind.Binary.Kind {
        return switch (eval_type) {
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
            .obj => switch (binary_kind) {
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
        eval_type: SemaExpr.EvalType,
        unary_kind: ParsedExpr.Kind.Unary.Kind,
    ) SemaExpr.Kind.Unary.Kind {
        return switch (eval_type) {
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

            .obj,
            .unit,
            .invalid,
            => unreachable,
        };
    }

    fn isString(eval_type: SemaExpr.EvalType) bool {
        return eval_type == .obj and eval_type.obj == .string;
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
