const std = @import("std");
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
const ParsedExpr = parsed_expr_mod.ParsedExpr;
const ParsedStmt = parsed_stmt_mod.ParsedStmt;
const SemaExpr = sema_expr_mod.SemaExpr;
const SemaStmt = sema_stmt_mod.SemaStmt;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const Position = tokenizer_mod.Position;
const Parser = parser_mod.Parser;

pub const SemaError = error{
    OutOfMemory,
    SemaFailure,
};

pub const SemaErrorInfo = struct {
    message: []u8,
    position: Position,
};

pub const Sema = struct {
    const Self = @This();

    allocator: Allocator,
    errs: ArrayList(SemaErrorInfo),

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .errs = ArrayList(SemaErrorInfo).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.deinitErrs();
    }

    pub fn analyze(self: *Self, block: *ParsedStmt) SemaError!*SemaStmt {
        self.deinitErrs();
        self.errs = ArrayList(SemaErrorInfo).init(self.allocator);

        const sema_block = try self.analyzeStmt(block);

        if (self.errs.items.len > 0) {
            sema_block.destroy(self.allocator);
            return error.SemaFailure;
        }

        return sema_block;
    }

    fn analyzeStmt(self: *Self, stmt: *ParsedStmt) SemaError!*SemaStmt {
        switch (stmt.kind) {
            .block => |block| {
                var sema_stmts = ArrayList(*SemaStmt).init(self.allocator);

                for (block.stmts.items) |parsed_stmt| {
                    const sema_stmt = try self.analyzeStmt(parsed_stmt);
                    try sema_stmts.append(sema_stmt);
                }

                return try SemaStmt.Kind.Block.create(self.allocator, sema_stmts, stmt.position);
            },
            .assert => |assert| {
                const expr = try self.analyzeExpr(assert.expr);

                if (expr.eval_type != .bool) {
                    return try self.semaErrorWithInvalidStmt(
                        stmt.position,
                        "Expected a Bool for assertion.",
                        .{},
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
    ) SemaError!*SemaExpr {
        switch (expr.*) {
            .literal => |literal| {
                var lexeme = literal.token.lexeme;
                const literal_variant: SemaExpr.Kind.Literal = switch (literal.kind) {
                    .int => .{ .int = std.fmt.parseInt(i64, lexeme, 10) catch unreachable },
                    .float => .{ .float = std.fmt.parseFloat(f64, lexeme) catch unreachable },
                    .bool => .{ .bool = lexeme.len == 4 },
                    .string => .{ .string = lexeme[1 .. lexeme.len - 1] },
                };
                const position = literal.token.position;

                return SemaExpr.Kind.Literal.create(self.allocator, literal_variant, position);
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

                switch (binary.operator_kind) {
                    .add, .subtract, .divide, .multiply => {
                        if (left.eval_type != .int and left.eval_type != .float) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Can't perform arithmetic operation on {s}.",
                                .{left.eval_type.stringify()},
                                .{ left, right },
                            );
                        }

                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Operand is expected to be of type {s}, got {s}.",
                                .{ left.eval_type.stringify(), right.eval_type.stringify() },
                                .{ left, right },
                            );
                        }

                        binary_kind = if (left.eval_type == .int) switch (binary.operator_kind) {
                            .add => .add_int,
                            .subtract => .subtract_int,
                            .multiply => .multiply_int,
                            .divide => .divide_int,
                            else => unreachable,
                        } else switch (binary.operator_kind) {
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
                                binary.operator_token.position,
                                "Can't perform a concatenation on types {s} and {s}.",
                                .{ left.eval_type.stringify(), right.eval_type.stringify() },
                                .{ left, right },
                            );
                        }

                        binary_kind = .concat;
                    },
                    .equal, .not_equal => {
                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Can't perform equality operation between types {s} and {s}.",
                                .{ left.eval_type.stringify(), right.eval_type.stringify() },
                                .{ left, right },
                            );
                        }

                        eval_type = .bool;
                        binary_kind = if (binary.operator_kind == .equal) switch (left.eval_type) {
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
                                binary.operator_token.position,
                                "Can't perform comparison operation on {s}.",
                                .{left.eval_type.stringify()},
                                .{ left, right },
                            );
                        }

                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Operand is expected to be of type {s}, got {s}.",
                                .{ left.eval_type.stringify(), right.eval_type.stringify() },
                                .{ left, right },
                            );
                        }

                        eval_type = .bool;
                        binary_kind = switch (left.eval_type) {
                            .int => switch (binary.operator_kind) {
                                .greater => .greater_int,
                                .greater_equal => .greater_equal_int,
                                .less => .less_int,
                                .less_equal => .less_equal_int,
                                else => unreachable,
                            },
                            .float => switch (binary.operator_kind) {
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
                                binary.operator_token.position,
                                "Can't perform logical and operation on {s}.",
                                .{left.eval_type.stringify()},
                                .{ left, right },
                            );
                        }

                        if (left.eval_type.tag() != right.eval_type.tag()) {
                            return try self.semaErrorWithInvalidExpr(
                                binary.operator_token.position,
                                "Operand is expected to be of type {s}, got {s}.",
                                .{ left.eval_type.stringify(), right.eval_type.stringify() },
                                .{ left, right },
                            );
                        }

                        eval_type = .bool;
                        binary_kind = if (binary.operator_kind == .and_) .and_ else .or_;
                    },
                }

                const position = binary.operator_token.position;

                return SemaExpr.Kind.Binary.create(
                    self.allocator,
                    binary_kind,
                    eval_type,
                    position,
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
                const unary_kind: UnaryKind = if (unary.operator_kind == .negate_bool)
                    switch (right.eval_type) {
                        .bool => .negate_bool,
                        else => return try self.semaErrorWithInvalidExpr(
                            unary.operator_token.position,
                            "Can't perform logical negation on {s}.",
                            .{eval_type.stringify()},
                            .{right},
                        ),
                    }
                else switch (right.eval_type) {
                    .int => .negate_int,
                    .float => .negate_float,
                    else => return try self.semaErrorWithInvalidExpr(
                        unary.operator_token.position,
                        "Can't perform arithmetic negation on {s}.",
                        .{eval_type.stringify()},
                        .{right},
                    ),
                };
                const position = unary.operator_token.position;

                return SemaExpr.Kind.Unary.create(
                    self.allocator,
                    unary_kind,
                    eval_type,
                    position,
                    right,
                );
            },
        }
    }

    fn createInvalidExpr(
        self: *const Self,
        child_exprs: anytype,
    ) SemaError!*SemaExpr {
        return try SemaExpr.Kind.Invalid.create(self.allocator, child_exprs);
    }

    fn isString(eval_type: SemaExpr.EvalType) bool {
        return eval_type == .obj and eval_type.obj == .string;
    }

    fn semaErrorWithInvalidExpr(
        self: *Self,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
        child_exprs: anytype,
    ) SemaError!*SemaExpr {
        try self.semaError(position, fmt, args);
        return try self.createInvalidExpr(child_exprs);
    }

    fn semaErrorWithInvalidStmt(
        self: *Self,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
        child_exprs: anytype,
        child_stmts: anytype,
    ) SemaError!*SemaStmt {
        try self.semaError(position, fmt, args);
        return try SemaStmt.Kind.Invalid.create(
            self.allocator,
            child_exprs,
            child_stmts,
        );
    }

    fn semaError(
        self: *Self,
        position: Position,
        comptime fmt: []const u8,
        args: anytype,
    ) SemaError!void {
        const message = try allocPrint(self.allocator, fmt, args);

        try self.errs.append(.{
            .message = message,
            .position = position,
        });
    }

    fn deinitErrs(self: *Self) void {
        for (self.errs.items) |semaErr| {
            self.allocator.free(semaErr.message);
        }

        self.errs.deinit();
    }
};

test "should free all memory on successful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "print (2 + 2) * -2";

    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(allocator);

    const parsed_expr = try parser.parse(&tokenizer);
    defer parsed_expr.destroy(allocator);

    // WHEN - THEN
    var sema = Sema.init(allocator);
    defer sema.deinit();

    const sema_expr = try sema.analyze(parsed_expr);
    defer sema_expr.destroy(allocator);
}

test "should free all memory on unsuccessful analysis" {
    // GIVEN
    const allocator = std.testing.allocator;

    const source = "print (2 + 2) * -false";

    var tokenizer = Tokenizer.init(source);

    var parser = Parser.init(allocator);
    defer parser.deinit();

    const parsed_expr = try parser.parse(&tokenizer);
    defer parsed_expr.destroy(allocator);

    // WHEN - THEN
    var sema = Sema.init(allocator);
    defer sema.deinit();

    const result = sema.analyze(parsed_expr);
    try expectError(SemaError.SemaFailure, result);
}
