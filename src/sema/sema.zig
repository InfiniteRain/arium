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
            too_many_locals,
            value_not_found: []const u8,
        };

        pub fn deinit(self: *DiagnosticEntry, allocator: Allocator) void {
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

    pub const Diagnostics = SharedDiagnostics(DiagnosticEntry);

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
    diagnostics: ?*Diagnostics = null,
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
        diagnostics: ?*Diagnostics,
    ) Error!*SemaExpr {
        self.diagnostics = diagnostics;
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
        switch (stmt.kind) {
            .assert => |assert| {
                const expr = try self.analyzeExpr(assert.expr);
                errdefer expr.destroy(self.allocator);

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
                errdefer expr.destroy(self.allocator);

                return try SemaStmt.Kind.Print.create(self.allocator, expr, stmt.position);
            },
            .expr => |expr| {
                const inner_expr = try self.analyzeExpr(expr.expr);
                errdefer inner_expr.destroy(self.allocator);

                return try SemaStmt.Kind.Expr.create(self.allocator, inner_expr, stmt.position);
            },
            .let => |let| {
                const expr = try self.analyzeExpr(let.expr);
                errdefer expr.destroy(self.allocator);

                const index = self.declareVariable(let.name, expr.eval_type) catch |err| switch (err) {
                    error.TooManyLocals => {
                        return try self.semaErrorWithInvalidStmt(
                            stmt.position,
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
                    stmt.position,
                );
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

                return SemaExpr.Kind.Literal.create(self.allocator, literal_variant, expr.position);
            },
            .binary => |binary| {
                const BinaryKind = SemaExpr.Kind.Binary.Kind;

                const left = try self.analyzeExpr(binary.left);
                errdefer left.destroy(self.allocator);

                const right = try self.analyzeExpr(binary.right);
                errdefer right.destroy(self.allocator);

                var binary_kind: BinaryKind = undefined;
                var eval_type = left.eval_type;

                if (left.kind == .invalid or right.kind == .invalid) {
                    return try self.createInvalidExpr(.{ left, right });
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
                        if (!isString(eval_type) or !isString(right.eval_type)) {
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
                            .unit => unreachable, // todo: change to empty product type
                            .int => .equal_int,
                            .float => .equal_float,
                            .bool => .equal_bool,
                            .obj => .equal_obj,
                            .invalid => unreachable,
                        } else switch (left.eval_type) {
                            .unit => unreachable, // todo: change to empty product type
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
                errdefer right.destroy(self.allocator);

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
                    expr.position,
                );
            },
            .variable => |variable| {
                const index, const eval_type = self.getVariable(variable.name) orelse {
                    return self.semaErrorWithInvalidExpr(
                        expr.position,
                        .{ .value_not_found = variable.name },
                        .{},
                    );
                };

                return try SemaExpr.Kind.Variable.create(
                    self.allocator,
                    index,
                    eval_type,
                    expr.position,
                );
            },
        }
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
                .kind = switch (diagnostic_kind) {
                    .value_not_found,
                    => |name| .{ .value_not_found = try diagnostics.allocator.dupe(u8, name) },

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
                    => diagnostic_kind,
                },
                .position = position,
            });
        }
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
