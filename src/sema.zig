const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const Allocator = std.mem.Allocator;
const fmt = std.fmt;
const assert = std.debug.assert;
const BoundedArray = std.BoundedArray;
const builtin = @import("builtin");

const shared = @import("shared");
const nullableArrayFrom = shared.meta.nullableArrayFrom;

const air_mod = @import("air.zig");
const Air = air_mod.Air;
const ast_mod = @import("ast.zig");
const Ast = ast_mod.Ast;
const fixed_array_mod = @import("fixed_array.zig");
const FixedArray = fixed_array_mod.FixedArray;
const intern_pool_mod = @import("intern_pool.zig");
const InternPool = intern_pool_mod.InternPool;
const tokenizer_mod = @import("tokenizer.zig");
const Loc = tokenizer_mod.Loc;

pub const Sema = struct {
    allocator: Allocator,
    intern_pool: *InternPool,
    ast: *const Ast,
    air: *Air,
    diags: *ArrayListUnmanaged(Diag),
    scratch: *ArrayListUnmanaged(Air.Index),

    pub const Error = error{AnalyzeFailure} || Allocator.Error;

    pub const Diag = struct {
        tag: Tag,
        loc: Loc,

        const Tag = union(enum) {
            expected_expr_type: InternPool.Index,
            unexpected_binary_operand_type: BadBinaryOperandType,
            unexpected_unary_operand_type: BadUnaryOperandType,
            integer_overflow,

            pub const BadBinaryOperandType = struct {
                kind: Kind,
                expected: TypeArray,
                actual: InternPool.Index,

                pub const Kind = enum {
                    lhs,
                    rhs,
                };
            };

            pub const BadUnaryOperandType = struct {
                expected: TypeArray,
                actual: InternPool.Index,
            };
        };
    };

    pub const TypeArray = FixedArray(InternPool.Index, 2);

    pub const Local = struct {
        identifier: Ast.Index,
    };

    pub fn analyze(
        allocator: Allocator,
        intern_pool: *InternPool,
        ast: *const Ast,
        diags: *ArrayListUnmanaged(Diag),
        scratch: *ArrayListUnmanaged(Air.Index),
    ) Error!Air {
        const diags_top = diags.items.len;
        var air: Air = .empty;
        var sema: Sema = .{
            .allocator = allocator,
            .intern_pool = intern_pool,
            .ast = ast,
            .air = &air,
            .diags = diags,
            .scratch = scratch,
        };
        errdefer sema.air.deinit(allocator);

        try sema.air.nodes.append(allocator, undefined);

        const block = try sema.analyzeBlockExprKey(Ast.Index.from(0).toKey(ast));

        if (diags.items.len > diags_top) {
            return error.AnalyzeFailure;
        }

        sema.air.nodes.set(0, try sema.prepareNode(block));

        return air;
    }

    fn analyzeStmt(self: *Sema, ast_index: Ast.Index) Error!Air.Index {
        const key = ast_index.toKey(self.ast);

        return switch (key) {
            .assert,
            => |child_expr| try self.analyzeAssertStmt(child_expr),

            .print,
            => |child_expr| try self.analyzePrintStmt(child_expr),

            .expr_stmt,
            => |expr| try self.analyzeExpr(expr),

            else => unreachable, // non-stmt node
        };
    }

    fn analyzeAssertStmt(
        self: *Sema,
        child_ast_expr: Ast.Index,
    ) Error!Air.Index {
        const sema_child_expr = try self.analyzeExpr(child_ast_expr);

        if (!self.typeSatisfies(
            sema_child_expr.toType(self.air, self.intern_pool),
            .from(.type_bool),
        )) {
            try self.addDiag(
                .{ .expected_expr_type = .type_bool },
                child_ast_expr.toLoc(self.ast),
            );
            return error.AnalyzeFailure;
        }

        return try self.addNode(.{ .assert = sema_child_expr });
    }

    fn analyzePrintStmt(
        self: *Sema,
        child_ast_expr: Ast.Index,
    ) Error!Air.Index {
        const sema_child_expr = try self.analyzeExpr(child_ast_expr);

        return try self.addNode(.{ .print = sema_child_expr });
    }

    fn analyzeLetStmt(self: *Sema, child_ast_expr: Ast.Index) Error!Air.Index {
        _ = self; // autofix
        _ = child_ast_expr; // autofix
        //
    }

    fn analyzeExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!Air.Index {
        const ast_key = ast_expr.toKey(self.ast);

        return switch (ast_key) {
            .literal_unit,
            .literal_int,
            .literal_float,
            .literal_bool,
            .literal_string,
            => try self.analyzeLiteralExpr(ast_expr, ast_key),

            .add,
            .sub,
            .mul,
            .div,
            .concat,
            .equal,
            .not_equal,
            .greater_than,
            .greater_equal,
            .less_than,
            .less_equal,
            .@"and",
            .@"or",
            => |ast_binary| try self.analyzeBinaryExpr(ast_key, ast_binary),

            .neg_bool,
            .neg_num,
            => |child_ast_expr| try self.analyzeUnaryExpr(
                ast_key,
                child_ast_expr,
            ),

            .block,
            .block_semicolon,
            => try self.analyzeBlockExpr(ast_key),

            else => unreachable, // non-expr node
        };
    }

    fn analyzeLiteralExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
    ) Error!Air.Index {
        const intern_pool_key: InternPool.Key = switch (ast_expr_key) {
            .literal_unit => .{ .value_simple = .unit },

            .literal_int => blk: {
                const parsed_int: i64 = std.fmt.parseInt(
                    i64,
                    ast_expr.toStr(self.ast),
                    10,
                ) catch |err| switch (err) {
                    error.Overflow => {
                        try self.addDiag(
                            .integer_overflow,
                            ast_expr.toLoc(self.ast),
                        );
                        break :blk .invalid;
                    },
                    error.InvalidCharacter => unreachable,
                };

                break :blk .{ .value_int = parsed_int };
            },

            .literal_float => .{ .value_float = std.fmt.parseFloat(
                f64,
                ast_expr.toStr(self.ast),
            ) catch unreachable },

            .literal_bool => .{
                .value_simple = if (ast_expr.toStr(self.ast).len == 4)
                    .bool_true
                else
                    .bool_false,
            },

            .literal_string => blk: {
                const lexeme = ast_expr.toStr(self.ast);
                break :blk .{ .value_string = lexeme[1 .. lexeme.len - 1] };
            },

            else => unreachable, // non-literal node
        };

        const intern_pool_index = try self.intern_pool.get(
            self.allocator,
            intern_pool_key,
        );

        return try self.addNode(.{ .constant = intern_pool_index });
    }

    fn analyzeBinaryExpr(
        self: *Sema,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
    ) Error!Air.Index {
        const air_binary: Air.Key.Binary = .{
            .lhs = try self.analyzeExpr(ast_binary.lhs),
            .rhs = try self.analyzeExpr(ast_binary.rhs),
        };

        if (air_binary.lhs.toType(self.air, self.intern_pool) == .invalid or
            air_binary.rhs.toType(self.air, self.intern_pool) == .invalid)
        {
            return try self.addInvalidNode();
        }

        return switch (ast_expr_key) {
            .add,
            .sub,
            .mul,
            .div,
            .greater_than,
            .greater_equal,
            .less_than,
            .less_equal,
            => try self.analyzeNumericBinaryExpr(
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            .concat,
            => try self.analyzeConcatBinaryExpr(
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            .equal,
            .not_equal,
            => try self.analyzeEqualBinaryExpr(
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            .@"and",
            .@"or",
            => try self.analyzeCondBinaryExpr(
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            else => unreachable,
        };
    }

    fn analyzeNumericBinaryExpr(
        self: *Sema,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        if (try self.typeCheckBinary(
            ast_binary,
            air_binary,
            .from(.{ .type_int, .type_float }),
        ) == .mismatch) {
            return try self.addInvalidNode();
        }

        return try self.addNode(switch (ast_expr_key) {
            .add => .{ .add = air_binary },
            .sub => .{ .sub = air_binary },
            .mul => .{ .mul = air_binary },
            .div => .{ .div = air_binary },
            .greater_than => .{ .greater_than = air_binary },
            .greater_equal => .{ .greater_equal = air_binary },
            .less_than => .{ .less_than = air_binary },
            .less_equal => .{ .less_equal = air_binary },
            else => unreachable, // non-numeric binary node
        });
    }

    fn analyzeConcatBinaryExpr(
        self: *Sema,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        if (try self.typeCheckBinary(
            ast_binary,
            air_binary,
            .from(.type_string),
        ) == .mismatch) {
            return try self.addInvalidNode();
        }

        return try self.addNode(switch (ast_expr_key) {
            .concat => .{ .concat = air_binary },
            else => unreachable, // non-concat binary node
        });
    }

    fn analyzeCondBinaryExpr(
        self: *Sema,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        if (try self.typeCheckBinary(
            ast_binary,
            air_binary,
            .from(.type_bool),
        ) == .mismatch) {
            return try self.addInvalidNode();
        }

        return try self.addNode(switch (ast_expr_key) {
            .@"and" => .{ .cond = .{
                .cond = air_binary.lhs,
                .then_branch = air_binary.rhs,
                .else_branch = try self.addNode(.{
                    .constant = .value_bool_false,
                }),
            } },
            .@"or" => .{ .cond = .{
                .cond = air_binary.lhs,
                .then_branch = try self.addNode(.{
                    .constant = .value_bool_true,
                }),
                .else_branch = air_binary.rhs,
            } },
            else => unreachable, // non-cond binary node
        });
    }

    fn analyzeEqualBinaryExpr(
        self: *Sema,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        const lhs_type = air_binary.lhs.toType(self.air, self.intern_pool);
        const rhs_type = air_binary.rhs.toType(self.air, self.intern_pool);

        if (!self.typeSatisfies(rhs_type, .from(lhs_type))) {
            try self.addDiag(
                .{ .unexpected_binary_operand_type = .{
                    .kind = .rhs,
                    .expected = .from(lhs_type),
                    .actual = rhs_type,
                } },
                ast_binary.rhs.toLoc(self.ast),
            );
            return try self.addInvalidNode();
        }

        return try self.addNode(switch (ast_expr_key) {
            .equal => .{ .equal = air_binary },
            .not_equal => .{ .not_equal = air_binary },
            else => unreachable, // non-equality binary node
        });
    }

    fn analyzeUnaryExpr(
        self: *Sema,
        ast_key: Ast.Key,
        child_ast_expr: Ast.Index,
    ) Error!Air.Index {
        const child_air_expr = try self.analyzeExpr(child_ast_expr);
        const child_type = child_air_expr.toType(self.air, self.intern_pool);

        if (child_type == .invalid) {
            return try self.addInvalidNode();
        }

        const expected_types: TypeArray = switch (ast_key) {
            .neg_bool => .from(.type_bool),
            .neg_num => .from(.{ .type_int, .type_float }),
            else => unreachable, // non-unary node
        };

        if (!self.typeSatisfies(child_type, expected_types)) {
            try self.addDiag(
                .{ .unexpected_unary_operand_type = .{
                    .expected = expected_types,
                    .actual = child_type,
                } },
                child_ast_expr.toLoc(self.ast),
            );
            return try self.addInvalidNode();
        }

        return try self.addNode(.{ .neg = child_air_expr });
    }

    fn analyzeBlockExpr(self: *Sema, ast_expr_key: Ast.Key) Error!Air.Index {
        return try self.addNode(try self.analyzeBlockExprKey(ast_expr_key));
    }

    fn analyzeBlockExprKey(self: *Sema, ast_expr_key: Ast.Key) Error!Air.Key {
        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);

        const stmts = switch (ast_expr_key) {
            .block,
            .block_semicolon,
            => |stmts| stmts,

            else => unreachable, // non-block node
        };

        for (stmts) |stmt| {
            const sema_stmt = self.analyzeStmt(stmt) catch |err| switch (err) {
                error.AnalyzeFailure => continue,
                else => return err,
            };
            try self.scratch.append(self.allocator, sema_stmt);
        }

        const should_append_unit =
            scratch_top == self.scratch.items.len or
            (ast_expr_key == .block_semicolon and
                self.scratch.getLast().toType(self.air, self.intern_pool) !=
                    .type_unit);

        if (should_append_unit) {
            const unit_node = try self.addNode(.{
                .constant = try self.intern_pool.get(
                    self.allocator,
                    .{ .value_simple = .unit },
                ),
            });
            try self.scratch.append(self.allocator, unit_node);
        }

        return .{ .block = self.scratch.items[scratch_top..] };
    }

    fn typeSatisfies(
        self: *Sema,
        subject: InternPool.Index,
        targets: TypeArray,
    ) bool {
        assert(subject.toType(self.intern_pool) == .type_type);

        if (subject == .invalid or subject == .type_never) {
            return true;
        }

        for (targets.slice()) |target| {
            assert(target.toType(self.intern_pool) == .type_type);

            if (target == .invalid) {
                return true;
            }

            switch (subject) {
                .type_int,
                .type_float,
                .type_bool,
                .type_string,
                .type_unit,
                .type_never,
                .type_type,
                => {
                    if (subject == target) {
                        return true;
                    }
                },

                else => unreachable, // non-type node
            }
        }

        return false;
    }

    fn typeCheckBinary(
        self: *Sema,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
        target_types: TypeArray,
    ) Error!enum { ok, mismatch } {
        const lhs_type = air_binary.lhs.toType(self.air, self.intern_pool);

        if (!self.typeSatisfies(lhs_type, target_types)) {
            try self.addDiag(
                .{
                    .unexpected_binary_operand_type = .{
                        .kind = .lhs,
                        .expected = target_types,
                        .actual = lhs_type,
                    },
                },
                ast_binary.lhs.toLoc(self.ast),
            );
            return .mismatch;
        }

        const rhs_type = air_binary.rhs.toType(self.air, self.intern_pool);

        if (!self.typeSatisfies(rhs_type, .from(lhs_type))) {
            try self.addDiag(
                .{ .unexpected_binary_operand_type = .{
                    .kind = .rhs,
                    .expected = .from(lhs_type),
                    .actual = rhs_type,
                } },
                ast_binary.rhs.toLoc(self.ast),
            );
            return .mismatch;
        }

        return .ok;
    }

    fn addNode(self: *Sema, key: Air.Key) Allocator.Error!Air.Index {
        try self.air.nodes.append(self.allocator, try self.prepareNode(key));

        return .from(self.air.nodes.len - 1);
    }

    fn addInvalidNode(self: *Sema) Allocator.Error!Air.Index {
        return self.addNode(.{ .constant = .invalid });
    }

    fn prepareNode(self: *Sema, key: Air.Key) Allocator.Error!Air.Node {
        return switch (key) {
            .constant => |intern_pool_index| .{
                .tag = .constant,
                .a = intern_pool_index.toInt(),
            },
            .add => |binary| prepareBinary(.add, binary),
            .sub => |binary| prepareBinary(.sub, binary),
            .mul => |binary| prepareBinary(.mul, binary),
            .div => |binary| prepareBinary(.div, binary),
            .concat => |binary| prepareBinary(.concat, binary),
            .equal => |binary| prepareBinary(.equal, binary),
            .not_equal => |binary| prepareBinary(.not_equal, binary),
            .greater_than => |binary| prepareBinary(.greater_than, binary),
            .greater_equal => |binary| prepareBinary(.greater_equal, binary),
            .less_than => |binary| prepareBinary(.less_than, binary),
            .less_equal => |binary| prepareBinary(.less_equal, binary),
            .cond => |cond| blk: {
                try self.air.extra.appendSlice(
                    self.allocator,
                    &[_]u32{
                        cond.then_branch.toInt(),
                        cond.else_branch.toInt(),
                    },
                );

                break :blk .{
                    .tag = .cond,
                    .a = cond.cond.toInt(),
                    .b = @intCast(self.air.extra.items.len - 2),
                };
            },
            .neg => |index| .{
                .tag = .neg,
                .a = index.toInt(),
            },
            .block => |indexes| blk: {
                try self.air.extra.appendSlice(
                    self.allocator,
                    @ptrCast(indexes),
                );

                break :blk .{
                    .tag = .block,
                    .a = @intCast(indexes.len),
                    .b = @intCast(self.air.extra.items.len - indexes.len),
                };
            },
            .variable => |variable| .{
                .tag = .variable,
                .a = variable.stack_index,
                .b = variable.type.toInt(),
            },

            .assert => |index| .{
                .tag = .assert,
                .a = index.toInt(),
            },
            .print => |index| .{
                .tag = .print,
                .a = index.toInt(),
            },
            .let => |let| .{
                .tag = .let,
                .a = let.stack_index,
                .b = if (let.rhs) |rhs|
                    rhs.toInt()
                else
                    0,
            },
        };
    }

    fn prepareBinary(
        tag: Air.Node.Tag,
        binary: Air.Key.Binary,
    ) Air.Node {
        return .{
            .tag = tag,
            .a = binary.lhs.toInt(),
            .b = binary.rhs.toInt(),
        };
    }

    fn addDiag(self: *Sema, diag: Diag.Tag, loc: Loc) Allocator.Error!void {
        try self.diags.append(self.allocator, .{ .tag = diag, .loc = loc });
    }
};
