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
const meta = std.meta;
const allocPrint = std.fmt.allocPrint;
const expectError = std.testing.expectError;
const SharedDiagnostics = shared.Diagnostics;
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

    pub const DiagnosticEntry = struct {
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
        };

        kind: Kind,
        position: Position,
    };

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

    allocator: Allocator,
    diagnostics: ?*Diagnostics = null,
    had_error: bool = false,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn analyze(
        self: *Self,
        block: *ParsedExpr,
        diagnostics: ?*Diagnostics,
    ) Error!*SemaExpr {
        self.diagnostics = diagnostics;
        self.had_error = false;

        const sema_block = try self.analyzeExpr(block);

        if (self.had_error) {
            sema_block.destroy(self.allocator);
            return error.SemaFailure;
        }

        return sema_block;
    }

    fn analyzeStmt(self: *Self, stmt: *ParsedStmt) Error!*SemaStmt {
        switch (stmt.kind) {
            .assert => |assert| {
                const expr = try self.analyzeExpr(assert.expr);

                if (expr.eval_type != .bool) {
                    return try self.semaErrorWithInvalidStmt(
                        stmt.position,
                        .{ .expected_expr_type = .bool },
                        .{expr},
                        .{},
                    );
                }

                return try SemaStmt.Kind.Assert.create(self.allocator, expr, stmt.position);
            },
            .print => |print| {
                const expr = try self.analyzeExpr(print.expr);
                return try SemaStmt.Kind.Print.create(self.allocator, expr, stmt.position);
            },
        }
    }

    fn analyzeExpr(
        self: *Self,
        expr: *ParsedExpr,
    ) Error!*SemaExpr {
        switch (expr.kind) {
            .literal => |literal| {
                const literal_variant: SemaExpr.Kind.Literal = switch (literal.kind) {
                    .int => |int| .{ .int = int },
                    .float => |float| .{ .float = float },
                    .bool => |bool_| .{ .bool = bool_ },
                    .string => |string| .{ .string = string },
                };

                return SemaExpr.Kind.Literal.create(self.allocator, literal_variant, expr.position);
            },
            .binary => |binary| {
                const BinaryKind = SemaExpr.Kind.Binary.Kind;
                const left = try self.analyzeExpr(binary.left);
                const right = try self.analyzeExpr(binary.right);
                var binary_kind: BinaryKind = undefined;
                var eval_type = left.eval_type;

                if (left.kind == .invalid or right.kind == .invalid) {
                    return self.createInvalidExpr(.{ left, right });
                }

                switch (binary.kind) {
                    .add, .subtract, .divide, .multiply => {
                        if (left.eval_type != .int and left.eval_type != .float) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_arithmetic_type = left.eval_type },
                                .{ left, right },
                            );
                        }

                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } },
                                .{ left, right },
                            );
                        }

                        binary_kind = if (left.eval_type == .int) switch (binary.kind) {
                            .add => .add_int,
                            .subtract => .subtract_int,
                            .multiply => .multiply_int,
                            .divide => .divide_int,
                            else => unreachable,
                        } else switch (binary.kind) {
                            .add => .add_float,
                            .subtract => .subtract_float,
                            .multiply => .multiply_float,
                            .divide => .divide_float,
                            else => unreachable,
                        };
                    },
                    .concat => {
                        if (isString(eval_type) or !isString(right.eval_type)) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_concat_type = .{ left.eval_type, right.eval_type } },
                                .{ left, right },
                            );
                        }

                        binary_kind = .concat;
                    },
                    .equal, .not_equal => {
                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_equality_type = .{ left.eval_type, right.eval_type } },
                                .{ left, right },
                            );
                        }

                        eval_type = .bool;
                        binary_kind = if (binary.kind == .equal) switch (left.eval_type) {
                            .int => .equal_int,
                            .float => .equal_float,
                            .bool => .equal_bool,
                            .obj => .equal_obj,
                            .invalid => unreachable,
                        } else switch (left.eval_type) {
                            .int => .not_equal_int,
                            .float => .not_equal_float,
                            .bool => .not_equal_bool,
                            .obj => .not_equal_obj,
                            .invalid => unreachable,
                        };
                    },
                    .greater, .greater_equal, .less, .less_equal => {
                        if (left.eval_type != .int and left.eval_type != .float) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_comparison_type = left.eval_type },
                                .{ left, right },
                            );
                        }

                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } },
                                .{ left, right },
                            );
                        }

                        eval_type = .bool;
                        binary_kind = switch (left.eval_type) {
                            .int => switch (binary.kind) {
                                .greater => .greater_int,
                                .greater_equal => .greater_equal_int,
                                .less => .less_int,
                                .less_equal => .less_equal_int,
                                else => unreachable,
                            },
                            .float => switch (binary.kind) {
                                .greater => .greater_float,
                                .greater_equal => .greater_equal_float,
                                .less => .less_float,
                                .less_equal => .less_equal_float,
                                else => unreachable,
                            },
                            else => unreachable,
                        };
                    },
                    .or_, .and_ => {
                        if (left.eval_type != .bool) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_logical_type = left.eval_type },
                                .{ left, right },
                            );
                        }

                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                expr.position,
                                .{ .unexpected_operand_type = .{ left.eval_type, right.eval_type } },
                                .{ left, right },
                            );
                        }

                        eval_type = .bool;
                        binary_kind = if (binary.kind == .and_) .and_ else .or_;
                    },
                }

                return SemaExpr.Kind.Binary.create(
                    self.allocator,
                    binary_kind,
                    eval_type,
                    expr.position,
                    left,
                    right,
                );
            },
            .unary => |unary| {
                const UnaryKind = SemaExpr.Kind.Unary.Kind;
                const right = try self.analyzeExpr(unary.right);

                if (right.kind == .invalid) {
                    return self.createInvalidExpr(.{right});
                }

                const eval_type = right.eval_type;
                const unary_kind: UnaryKind = if (unary.kind == .negate_bool)
                    switch (right.eval_type) {
                        .bool => .negate_bool,
                        else => return try self.semaErrorWithInvalidExpr(
                            expr.position,
                            .{ .unexpected_logical_negation_type = eval_type },
                            .{right},
                        ),
                    }
                else switch (right.eval_type) {
                    .int => .negate_int,
                    .float => .negate_float,
                    else => return try self.semaErrorWithInvalidExpr(
                        expr.position,
                        .{ .unexpected_arithmetic_negation_type = eval_type },
                        .{right},
                    ),
                };

                return SemaExpr.Kind.Unary.create(
                    self.allocator,
                    unary_kind,
                    eval_type,
                    expr.position,
                    right,
                );
            },
            .block => |block| {
                var sema_stmts = ArrayList(*SemaStmt).init(self.allocator);

                for (block.stmts.items) |parser_stmt| {
                    const sema_stmt = try self.analyzeStmt(parser_stmt);
                    try sema_stmts.append(sema_stmt);
                }

                return try SemaExpr.Kind.Block.create(
                    self.allocator,
                    sema_stmts,
                    .int, // todo: temp
                    expr.position,
                );
            },
        }
    }

    fn createInvalidExpr(
        self: *const Self,
        child_exprs: anytype,
    ) Error!*SemaExpr {
        return try SemaExpr.Kind.Invalid.create(self.allocator, child_exprs);
    }

    fn isString(eval_type: SemaExpr.EvalType) bool {
        return eval_type == .obj and eval_type.obj == .string;
    }

    fn semaErrorWithInvalidExpr(
        self: *Self,
        position: Position,
        diagnostic_kind: DiagnosticEntry.Kind,
        child_exprs: anytype,
    ) Error!*SemaExpr {
        try self.semaError(position, diagnostic_kind);
        return try self.createInvalidExpr(child_exprs);
    }

    fn semaErrorWithInvalidStmt(
        self: *Self,
        position: Position,
        diagnostic_kind: DiagnosticEntry.Kind,
        child_exprs: anytype,
        child_stmts: anytype,
    ) Error!*SemaStmt {
        try self.semaError(position, diagnostic_kind);
        return try SemaStmt.Kind.Invalid.create(
            self.allocator,
            child_exprs,
            child_stmts,
        );
    }

    fn semaError(
        self: *Self,
        position: Position,
        diagnostic_kind: DiagnosticEntry.Kind,
    ) Error!void {
        self.had_error = true;

        if (self.diagnostics) |diagnostics| {
            // in case of ever needing to alloc something in here, make sure to
            // use diagnostics.allocator instead of self.allocator. this is
            // necessary for lang-tests where a new allocator is created for
            // each test to detect memory leaks. that allocator then gets
            // deinited while diagnostics are owned by the tests.
            try diagnostics.add(.{
                .kind = diagnostic_kind,
                .position = position,
            });
        }
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
