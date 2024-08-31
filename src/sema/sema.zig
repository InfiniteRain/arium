const std = @import("std");
const shared = @import("shared");
const parsed_expr_mod = @import("../parser/parsed_expr.zig");
const parsed_stmt_mod = @import("../parser/parsed_stmt.zig");
const sema_expr_mod = @import("sema_expr.zig");
const sema_stmt_mod = @import("sema_stmt.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const parser_mod = @import("../parser/parser.zig");

const Allocator = std.mem.Allocator;
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
        };

        pub fn deinit(self: *DiagEntry, allocator: Allocator) void {
            switch (self.kind) {
                .value_not_found,
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
                => {},
            }
        }

        kind: Kind,
        position: Position,
    };

    pub const Diags = SharedDiags(DiagEntry);

    const AnalyzeExprResult = union(enum) {
        ok: SemaExpr.EvalType,
        fail: DiagEntry.Kind,
    };

    const LocalsMap = struct {
        allocator: Allocator,
        prev: ?*LocalsMap = null,
        map: StringHashMap(usize),

        fn init(allocator: Allocator, prev: ?*LocalsMap) LocalsMap {
            return .{
                .allocator = allocator,
                .prev = prev,
                .map = StringHashMap(usize).init(allocator),
            };
        }

        fn deinit(self: *LocalsMap) void {
            self.map.deinit();
        }
    };

    const max_locals = 256;

    allocator: Allocator,
    diags: ?*Diags = null,
    had_error: bool = false,
    locals: [max_locals]SemaExpr.EvalType = undefined,
    locals_top: usize = 0,
    locals_map: ?*LocalsMap = null,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn analyze(
        self: *Self,
        block: *ParsedExpr,
        diags: ?*Diags,
    ) Error!*SemaExpr {
        self.diags = diags;
        self.had_error = false;
        self.locals_top = 0;
        self.locals_map = null;

        const sema_block = try self.analyzeExpr(block);

        if (self.had_error) {
            sema_block.destroy(self.allocator);
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
        errdefer expr.destroy(self.allocator);

        if (expr.eval_type != .bool) {
            return try self.semaErrorWithInvalidStmt(
                position,
                .{ .expected_expr_type = .bool },
                .{expr},
                .{},
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
        errdefer expr.destroy(self.allocator);

        return try SemaStmt.Kind.Print.create(self.allocator, expr, position);
    }

    fn analyzeExprStmt(
        self: *Self,
        expr: *ParsedStmt.Kind.Expr,
        position: Position,
    ) Error!*SemaStmt {
        const inner_expr = try self.analyzeExpr(expr.expr);
        errdefer inner_expr.destroy(self.allocator);

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
        const expr = try self.analyzeExpr(let.expr);
        errdefer expr.destroy(self.allocator);

        const index = self.declareVariable(let.name, expr.eval_type) catch |err| switch (err) {
            error.TooManyLocals => {
                return try self.semaErrorWithInvalidStmt(
                    position,
                    .too_many_locals,
                    .{expr},
                    .{},
                );
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

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
                errdefer self.allocator.free(duped_string);

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
        errdefer left.destroy(self.allocator);

        const right = try self.analyzeExpr(binary.right);
        errdefer right.destroy(self.allocator);

        if (left.kind == .invalid or right.kind == .invalid) {
            return try self.createInvalidExpr(.{ left, right });
        }

        const result = switch (binary.kind) {
            .add,
            .subtract,
            .divide,
            .multiply,
            => analyzeArithmeticBinaryExpr(left, right),

            .concat,
            => analyzeConcatBinaryExpr(left, right),

            .equal,
            .not_equal,
            => analyzeEqualBinaryExpr(left, right),

            .greater,
            .greater_equal,
            .less,
            .less_equal,
            => analyzeComparisonBinaryExpr(left, right),

            .or_,
            .and_,
            => analyzeLogicalBinaryExpr(left, right),
        };

        return switch (result) {
            .ok => |eval_type| try SemaExpr.Kind.Binary.create(
                self.allocator,
                translateBinaryKind(left.eval_type, binary.kind),
                eval_type,
                position,
                left,
                right,
            ),
            .fail => |diag| try self.semaErrorWithInvalidExpr(
                position,
                diag,
                .{ left, right },
            ),
        };
    }

    fn analyzeArithmeticBinaryExpr(
        left: *SemaExpr,
        right: *SemaExpr,
    ) AnalyzeExprResult {
        if (left.eval_type != .int and left.eval_type != .float) {
            return .{ .fail = .{ .unexpected_arithmetic_type = left.eval_type } };
        }

        if (left.eval_type.tag() != right.eval_type.tag()) {
            return .{ .fail = .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } } };
        }

        return .{ .ok = left.eval_type };
    }

    fn analyzeConcatBinaryExpr(
        left: *SemaExpr,
        right: *SemaExpr,
    ) AnalyzeExprResult {
        if (!isString(left.eval_type) or !isString(right.eval_type)) {
            return .{ .fail = .{ .unexpected_concat_type = .{ left.eval_type, right.eval_type } } };
        }

        return .{ .ok = left.eval_type };
    }

    fn analyzeEqualBinaryExpr(
        left: *SemaExpr,
        right: *SemaExpr,
    ) AnalyzeExprResult {
        if (left.eval_type.tag() != right.eval_type.tag()) {
            return .{ .fail = .{ .unexpected_equality_type = .{ left.eval_type, right.eval_type } } };
        }

        return .{ .ok = .bool };
    }

    fn analyzeComparisonBinaryExpr(
        left: *SemaExpr,
        right: *SemaExpr,
    ) AnalyzeExprResult {
        if (left.eval_type != .int and left.eval_type != .float) {
            return .{ .fail = .{ .unexpected_comparison_type = left.eval_type } };
        }

        if (left.eval_type.tag() != right.eval_type.tag()) {
            return .{ .fail = .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } } };
        }

        return .{ .ok = .bool };
    }

    fn analyzeLogicalBinaryExpr(
        left: *SemaExpr,
        right: *SemaExpr,
    ) AnalyzeExprResult {
        if (left.eval_type != .bool) {
            return .{ .fail = .{ .unexpected_logical_type = left.eval_type } };
        }

        if (left.eval_type.tag() != right.eval_type.tag()) {
            return .{ .fail = .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } } };
        }

        return .{ .ok = .bool };
    }

    fn analyzeUnaryExpr(
        self: *Self,
        unary: *ParsedExpr.Kind.Unary,
        position: Position,
    ) Error!*SemaExpr {
        const right = try self.analyzeExpr(unary.right);
        errdefer right.destroy(self.allocator);

        if (right.kind == .invalid) {
            return try self.createInvalidExpr(.{right});
        }

        const result = switch (unary.kind) {
            .negate_bool,
            => analyzeNegateBoolUnaryExpr(right),

            .negate_num,
            => analyzeNegateNumUnaryExpr(right),
        };

        return switch (result) {
            .ok => |eval_type| try SemaExpr.Kind.Unary.create(
                self.allocator,
                translateUnaryKind(right.eval_type, unary.kind),
                eval_type,
                position,
                right,
            ),
            .fail => |diag| try self.semaErrorWithInvalidExpr(
                position,
                diag,
                .{right},
            ),
        };
    }

    fn analyzeNegateBoolUnaryExpr(right: *SemaExpr) AnalyzeExprResult {
        if (right.eval_type != .bool) {
            return .{ .fail = .{ .unexpected_logical_negation_type = right.eval_type } };
        }

        return .{ .ok = right.eval_type };
    }

    fn analyzeNegateNumUnaryExpr(right: *SemaExpr) AnalyzeExprResult {
        if (right.eval_type != .int or right.eval_type != .float) {
            return .{ .fail = .{ .unexpected_arithmetic_negation_type = right.eval_type } };
        }

        return .{ .ok = right.eval_type };
    }

    fn analyzeBlockExpr(
        self: *Self,
        block: *ParsedExpr.Kind.Block,
        position: Position,
    ) Error!*SemaExpr {
        var locals_map = LocalsMap.init(self.allocator, self.locals_map);
        defer locals_map.deinit();

        self.locals_map = &locals_map;

        const old_locals_top = self.locals_top;

        var sema_stmts = ArrayList(*SemaStmt).init(self.allocator);
        errdefer {
            for (sema_stmts.items) |stmt| {
                stmt.destroy(self.allocator);
            }

            sema_stmts.clearAndFree();
        }

        for (block.stmts.items) |parser_stmt| {
            const sema_stmt = try self.analyzeStmt(parser_stmt);
            errdefer sema_stmt.destroy(self.allocator);

            try sema_stmts.append(sema_stmt);
        }

        const last_stmt = sema_stmts.items[sema_stmts.items.len - 1];
        const eval_type = last_stmt.kind.expr.expr.eval_type;

        self.locals_map = self.locals_map.?.prev;
        self.locals_top = old_locals_top;

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
        const index, const eval_type = self.getVariable(variable.name) orelse {
            return self.semaErrorWithInvalidExpr(
                position,
                .{ .value_not_found = variable.name },
                .{},
            );
        };

        return try SemaExpr.Kind.Variable.create(
            self.allocator,
            index,
            eval_type,
            position,
        );
    }

    fn declareVariable(
        self: *Self,
        name: []const u8,
        eval_type: SemaExpr.EvalType,
    ) error{ TooManyLocals, OutOfMemory }!usize {
        if (self.locals_top == max_locals) {
            return error.TooManyLocals;
        }

        self.locals[self.locals_top] = eval_type;
        try self.locals_map.?.map.put(name, self.locals_top);
        self.locals_top += 1;

        return self.locals_top - 1;
    }

    fn getVariable(
        self: *Self,
        name: []const u8,
    ) ?struct { usize, SemaExpr.EvalType } {
        var current_map = self.locals_map;

        while (current_map) |local_map| {
            if (local_map.map.get(name)) |index| {
                return .{ index, self.locals[index] };
            }

            current_map = local_map.prev;
        }

        return null;
    }

    fn createInvalidExpr(
        self: *const Self,
        child_exprs: anytype,
    ) Error!*SemaExpr {
        return try SemaExpr.Kind.Invalid.create(self.allocator, child_exprs);
    }

    fn semaErrorWithInvalidExpr(
        self: *Self,
        position: Position,
        diag_kind: DiagEntry.Kind,
        child_exprs: anytype,
    ) Error!*SemaExpr {
        try self.semaError(position, diag_kind);
        return try self.createInvalidExpr(child_exprs);
    }

    fn semaErrorWithInvalidStmt(
        self: *Self,
        position: Position,
        diag_kind: DiagEntry.Kind,
        child_exprs: anytype,
        child_stmts: anytype,
    ) Error!*SemaStmt {
        try self.semaError(position, diag_kind);
        return try SemaStmt.Kind.Invalid.create(
            self.allocator,
            child_exprs,
            child_stmts,
        );
    }

    fn semaError(
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

    const source = "print (2 + 2) * -2";

    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(allocator);

    const parsed_expr = try parser.parse(&tokenizer, null);
    defer parsed_expr.destroy(allocator);

    // WHEN - THEN
    var sema = Sema.init(allocator);

    const sema_expr = try sema.analyze(parsed_expr, null);
    defer sema_expr.destroy(allocator);
}

test "should free all memory on unsuccessful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "print (2 + 2) * -false";

    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(allocator);

    const parsed_expr = try parser.parse(&tokenizer, null);
    defer parsed_expr.destroy(allocator);

    // WHEN - THEN
    var sema = Sema.init(allocator);

    const result = sema.analyze(parsed_expr, null);
    try expectError(Sema.Error.SemaFailure, result);
}
