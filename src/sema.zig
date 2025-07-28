const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;
const assert = std.debug.assert;
const BoundedArray = std.BoundedArray;
const MultiArrayListUnmanaged = std.MultiArrayList;
const meta = std.meta;
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
const limits = @import("limits.zig");

pub const Sema = struct {
    allocator: Allocator,
    intern_pool: *InternPool,
    ast: *const Ast,
    air: *Air,
    diags: *ArrayListUnmanaged(Diag),
    scratch: *ArrayListUnmanaged(Air.Index),

    scope: Scope,
    loop_mode: LoopMode,

    pub const Error = error{AnalyzeFailure} || Allocator.Error;

    pub const Diag = struct {
        tag: Tag,
        loc: Loc,

        const Tag = union(enum) {
            unexpected_expr_type: BadExprType,
            integer_overflow,
            undeclared_identifier,
            too_many_locals,
            unassigned_variable,
            immutable_mutation: Loc,
            unreachable_stmt,
            break_outside_loop,
            continue_outside_loop,

            pub const BadExprType = struct {
                expected: TypeArray,
                actual: InternPool.Index,
            };
        };
    };

    pub const TypeArray = FixedArray(InternPool.Index, 2);

    const Scope = struct {
        locals: MultiArrayListUnmanaged(Local),
        runtime_scope_top: usize,

        pub const Index = enum(u32) {
            _,

            pub fn from(int: anytype) Index {
                return @enumFromInt(int);
            }

            pub fn toItem(
                self: Index,
                scope: *const Scope,
                comptime field: meta.FieldEnum(Local),
            ) meta.fieldInfo(Local, field).type {
                return scope.locals.items(field)[@intFromEnum(self)];
            }

            pub fn toItemPtr(
                self: Index,
                scope: *const Scope,
                comptime field: meta.FieldEnum(Local),
            ) *meta.fieldInfo(Local, field).type {
                return &scope.locals.items(field)[@intFromEnum(self)];
            }
        };

        const Top = struct {
            scope_top: usize,
            runtime_scope_top: usize,
        };

        fn init() Scope {
            return .{
                .locals = .empty,
                .runtime_scope_top = 0,
            };
        }

        fn deinit(self: *Scope, allocator: Allocator) void {
            self.locals.clearAndFree(allocator);
        }

        fn append(
            self: *Scope,
            allocator: Allocator,
            local: Local,
        ) Allocator.Error!void {
            if (local.flags.eval_time == .runtime) {
                self.runtime_scope_top += 1;
            }

            try self.locals.append(allocator, local);
        }

        fn top(self: *Scope) Top {
            return .{
                .scope_top = self.locals.len,
                .runtime_scope_top = self.runtime_scope_top,
            };
        }

        fn restore(self: *Scope, top_info: Top) void {
            self.locals.shrinkRetainingCapacity(top_info.scope_top);
            self.runtime_scope_top = top_info.runtime_scope_top;
        }
    };

    const Local = struct {
        name: Name,
        type: InternPool.Index,
        flags: packed struct {
            mutability: Mutability,
            assignment: Assignement,
            eval_time: EvalTime,
            type_hood: TypeHood,
        },

        pub const Mutability = enum(u1) { mutable, immutable };

        pub const Assignement = enum(u1) { assigned, unassigned };

        pub const EvalTime = enum(u1) { @"comptime", runtime };

        pub const TypeHood = enum(u1) { type, not_type };

        pub const Name = packed struct(u32) {
            tag: enum(u1) { ast, intern_pool },
            index: u31,

            fn from(value: anytype) @This() {
                return .{
                    .tag = switch (@TypeOf(value)) {
                        Ast.Index => .ast,
                        InternPool.Index => .intern_pool,
                        else => unreachable, // not an intern pool/ast index
                    },
                    .index = @intCast(value.toInt()),
                };
            }
        };
    };

    const EvalMode = enum {
        eval,
        no_eval,
    };

    const ForceAppendUnitMode = enum {
        force_append_unit,
        no_force_append_unit,
    };

    const LoopMode = enum {
        in_loop,
        not_in_loop,
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

        var scope = Scope.init();
        defer scope.deinit(allocator);

        inline for (.{
            .{ "Int", .type_int },
            .{ "Float", .type_float },
            .{ "Bool", .type_bool },
            .{ "String", .type_string },
            .{ "Unit", .type_unit },
        }) |entry| {
            try scope.append(allocator, .{
                .name = .from(try intern_pool.get(
                    allocator,
                    .{ .value_string = entry[0] },
                )),
                .type = entry[1],
                .flags = .{
                    .mutability = .immutable,
                    .assignment = .assigned,
                    .eval_time = .@"comptime",
                    .type_hood = .type,
                },
            });
        }

        var sema: Sema = .{
            .allocator = allocator,
            .intern_pool = intern_pool,
            .ast = ast,
            .air = &air,
            .diags = diags,
            .scratch = scratch,

            .scope = scope,
            .loop_mode = .not_in_loop,
        };

        errdefer sema.air.deinit(allocator);

        try sema.air.nodes.append(allocator, undefined);

        const block = try sema.analyzeBlockExprKey(
            Ast.Index.from(0).toKey(ast),
            .no_eval,
            .no_force_append_unit,
        );

        if (diags.items.len > diags_top) {
            return error.AnalyzeFailure;
        }

        sema.air.nodes.set(0, try sema.prepareNode(block));

        return air;
    }

    fn analyzeStmt(
        self: *Sema,
        ast_stmt: Ast.Index,
        eval_mode: EvalMode,
    ) Error!Air.Index {
        const ast_key = ast_stmt.toKey(self.ast);

        return switch (ast_key) {
            .assert,
            => |child_expr| try self.analyzeAssertStmt(child_expr),

            .print,
            => |child_expr| try self.analyzePrintStmt(child_expr),

            .expr_stmt,
            => |expr| try self.analyzeExpr(expr, eval_mode),

            .let,
            .let_mut,
            => |let| try self.analyzeLetStmt(ast_stmt, ast_key, let),

            else => unreachable, // non-stmt node
        };
    }

    fn analyzeAssertStmt(
        self: *Sema,
        child_ast_expr: Ast.Index,
    ) Error!Air.Index {
        const sema_child_expr = try self.analyzeExpr(
            child_ast_expr,
            .eval,
        );
        const child_type = sema_child_expr.toType(self.air, self.intern_pool);

        if (self.typeCheck(child_type, .from(.type_bool)) == .mismatch) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
                    .expected = .from(.type_bool),
                    .actual = child_type,
                } },
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
        const sema_child_expr = try self.analyzeExpr(
            child_ast_expr,
            .eval,
        );

        return try self.addNode(.{ .print = sema_child_expr });
    }

    fn analyzeLetStmt(
        self: *Sema,
        ast_stmt: Ast.Index,
        ast_stmt_key: Ast.Key,
        let: Ast.Key.Let,
    ) Error!Air.Index {
        const let_type_opt = if (let.type) |ast_type_expr|
            try self.analyzeTypeExpr(ast_type_expr)
        else
            null;
        const mutability: Local.Mutability = if (ast_stmt_key == .let)
            .immutable
        else
            .mutable;
        const identifier = let.identifier;

        const ast_expr = let.expr orelse {
            try self.scope.append(self.allocator, .{
                .name = .from(identifier),
                .type = if (let_type_opt) |let_type| let_type else .none,
                .flags = .{
                    .mutability = mutability,
                    .assignment = .unassigned,
                    .eval_time = .runtime,
                    .type_hood = .not_type,
                },
            });

            return try self.addNode(.{ .let = .{
                .stack_index = @intCast(self.scope.runtime_scope_top - 1),
                .rhs = null,
            } });
        };

        const air_expr = try self.analyzeExpr(ast_expr, .eval);

        if (self.scope.runtime_scope_top >= limits.max_locals) {
            try self.addDiag(
                .too_many_locals,
                ast_stmt.toLoc(self.ast),
            );
        }

        const local_type = air_expr.toType(self.air, self.intern_pool);
        var final_type = local_type;

        if (let_type_opt) |let_type| {
            if (self.typeCheck(local_type, .from(let_type)) == .ok) {
                final_type = let_type;
            } else {
                try self.addDiag(
                    .{ .unexpected_expr_type = .{
                        .expected = .from(let_type),
                        .actual = local_type,
                    } },
                    ast_expr.toLoc(self.ast),
                );
                final_type = .invalid;
            }
        }

        try self.scope.append(self.allocator, .{
            .name = .from(identifier),
            .type = final_type,
            .flags = .{
                .mutability = mutability,
                .assignment = .assigned,
                .eval_time = .runtime,
                .type_hood = .not_type,
            },
        });

        return try self.addNode(.{ .let = .{
            .stack_index = @intCast(self.scope.runtime_scope_top - 1),
            .rhs = air_expr,
        } });
    }

    fn analyzeExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        eval_mode: EvalMode,
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
            => try self.analyzeBlockExpr(
                ast_key,
                eval_mode,
                .no_force_append_unit,
            ),

            .identifier,
            => try self.analyzeVariableExpr(ast_expr),

            .assignment,
            => |binary| try self.analyzeAssignmentExpr(ast_expr, binary),

            .@"if",
            .if_else,
            .if_elseif,
            .if_elseif_else,
            => try self.analyzeIfExpr(ast_key, eval_mode),

            .@"for",
            .for_conditional,
            => try self.analyzeForExpr(ast_key),

            .@"break",
            => try self.analyzeBreakExpr(ast_expr),

            .@"continue",
            => try self.analyzeContinueExpr(ast_expr),

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
            .lhs = try self.analyzeExpr(ast_binary.lhs, .eval),
            .rhs = try self.analyzeExpr(ast_binary.rhs, .eval),
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

        if (self.typeCheck(rhs_type, .from(lhs_type)) == .mismatch) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
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
        const child_air_expr = try self.analyzeExpr(child_ast_expr, .eval);
        const child_type = child_air_expr.toType(self.air, self.intern_pool);

        if (child_type == .invalid) {
            return try self.addInvalidNode();
        }

        const expected_types: TypeArray = switch (ast_key) {
            .neg_bool => .from(.type_bool),
            .neg_num => .from(.{ .type_int, .type_float }),
            else => unreachable, // non-unary node
        };

        if (self.typeCheck(child_type, expected_types) == .mismatch) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
                    .expected = expected_types,
                    .actual = child_type,
                } },
                child_ast_expr.toLoc(self.ast),
            );
            return try self.addInvalidNode();
        }

        return try self.addNode(.{ .neg = child_air_expr });
    }

    fn analyzeBlockExpr(
        self: *Sema,
        ast_expr_key: Ast.Key,
        eval_mode: EvalMode,
        force_append_unit_mode: ForceAppendUnitMode,
    ) Error!Air.Index {
        return try self.addNode(
            try self.analyzeBlockExprKey(
                ast_expr_key,
                eval_mode,
                force_append_unit_mode,
            ),
        );
    }

    fn analyzeBlockExprKey(
        self: *Sema,
        ast_expr_key: Ast.Key,
        eval_mode: EvalMode,
        force_append_unit_mode: ForceAppendUnitMode,
    ) Error!Air.Key {
        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);

        const scope_top = self.scope.top();
        defer self.scope.restore(scope_top);

        const stmts = switch (ast_expr_key) {
            .block,
            .block_semicolon,
            => |stmts| stmts,

            else => unreachable, // non-block node
        };
        var is_last_stmt_never = false;

        for (stmts, 0..) |stmt, i| {
            const is_last = i == stmts.len - 1;
            const stmt_eval_mode = if (is_last)
                eval_mode
            else
                .no_eval;
            const sema_stmt = self.analyzeStmt(stmt, stmt_eval_mode) catch |err|
                switch (err) {
                    error.AnalyzeFailure => continue,
                    else => return err,
                };
            try self.scratch.append(self.allocator, sema_stmt);

            if (sema_stmt.toType(self.air, self.intern_pool) == .type_never) {
                is_last_stmt_never = true;

                if (!is_last) {
                    try self.addDiag(
                        .unreachable_stmt,
                        stmts[i + 1].toLoc(self.ast),
                    );
                    break;
                }
            }
        }

        const last_stmt = self.scratch.getLast();
        const is_block_empty = scratch_top == self.scratch.items.len;
        const is_last_stmt_not_expr = switch (last_stmt.toKey(self.air)) {
            .assert,
            .print,
            .let,
            .assignment,
            => true,

            else => false,
        };
        const is_last_stmt_unit = !is_block_empty and
            last_stmt.toType(self.air, self.intern_pool) != .type_unit;
        const is_block_sm_no_unit = ast_expr_key == .block_semicolon and
            is_last_stmt_unit;

        const should_append_unit =
            !is_last_stmt_never and
            eval_mode == .eval and
            (is_block_empty or
                is_last_stmt_not_expr or
                is_block_sm_no_unit or
                force_append_unit_mode == .force_append_unit);

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

    fn analyzeVariableExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!Air.Index {
        const result = self.resolveIdentifier(ast_expr) catch |err|
            switch (err) {
                error.UndeclaredIdentifier => {
                    try self.addDiag(
                        .undeclared_identifier,
                        ast_expr.toLoc(self.ast),
                    );
                    return try self.addInvalidNode();
                },
            };

        const local = switch (result) {
            .@"comptime" => @panic("TODO: variables can't be comptime"), // todo: add proper logic after proper comptime support
            .runtime => |data| data,
        };

        const flags = local.index.toItem(&self.scope, .flags);

        if (flags.assignment == .unassigned) {
            try self.addDiag(
                .unassigned_variable,
                ast_expr.toLoc(self.ast),
            );
            return try self.addInvalidNode();
        }

        return try self.addNode(.{ .variable = .{
            .stack_index = @intCast(local.stack_index),
            .type = local.index.toItem(&self.scope, .type),
        } });
    }

    fn analyzeAssignmentExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        binary: Ast.Key.Binary,
    ) Error!Air.Index {
        const result = self.resolveIdentifier(binary.lhs) catch |err|
            switch (err) {
                error.UndeclaredIdentifier => {
                    try self.addDiag(
                        .undeclared_identifier,
                        binary.lhs.toLoc(self.ast),
                    );
                    return try self.addInvalidNode();
                },
            };

        const local = switch (result) {
            .@"comptime" => @panic("TODO: variables can't be comptime"), // todo: add proper logic after proper comptime support
            .runtime => |data| data,
        };

        const flags = local.index.toItemPtr(&self.scope, .flags);

        if (flags.mutability == .immutable and flags.assignment == .assigned) {
            try self.addDiag(
                .{ .immutable_mutation = binary.lhs.toLoc(self.ast) },
                ast_expr.toLoc(self.ast),
            );
            return try self.addInvalidNode();
        }

        const rhs = try self.analyzeExpr(binary.rhs, .eval);
        const rhs_type = rhs.toType(self.air, self.intern_pool);
        const lhs_type = local.index.toItemPtr(&self.scope, .type);

        flags.assignment = .assigned;

        if (lhs_type.* != .none and
            self.typeCheck(rhs_type, .from(lhs_type.*)) == .mismatch)
        {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
                    .expected = .from(lhs_type.*),
                    .actual = rhs_type,
                } },
                binary.rhs.toLoc(self.ast),
            );
            return try self.addInvalidNode();
        } else if (lhs_type.* == .none) {
            lhs_type.* = rhs_type;
        }

        return try self.addNode(.{ .assignment = .{
            .stack_index = @intCast(local.stack_index),
            .rhs = rhs,
        } });
    }

    fn analyzeIfExpr(
        self: *Sema,
        ast_key: Ast.Key,
        eval_mode: EvalMode,
    ) Error!Air.Index {
        var condition: Ast.Key.Conditional = undefined;
        var elseif_blocks: []const Ast.Key.Conditional = &[_]Ast.Key.Conditional{};
        var else_block: ?Ast.Index = null;

        switch (ast_key) {
            .@"if" => |index| {
                condition = index;
            },
            .if_else => |if_else| {
                condition = if_else.conditional;
                else_block = if_else.else_block;
            },
            .if_elseif => |if_elseif| {
                condition = if_elseif.conditionals[0];
                elseif_blocks = if_elseif.conditionals[1..];
            },
            .if_elseif_else => |if_elseif_else| {
                condition = if_elseif_else.conditionals[0];
                elseif_blocks = if_elseif_else.conditionals[1..];
                else_block = if_elseif_else.else_block;
            },
            else => unreachable,
        }

        return try self.analyzeIfExprAux(
            condition,
            elseif_blocks,
            else_block,
            null,
            eval_mode,
        );
    }

    fn analyzeIfExprAux(
        self: *Sema,
        cond: Ast.Key.Conditional,
        elseif_blocks: []const Ast.Key.Conditional,
        else_block_opt: ?Ast.Index,
        type_opt: ?InternPool.Index,
        eval_mode: EvalMode,
    ) Error!Air.Index {
        const air_cond = try self.analyzeExpr(cond.condition, .eval);
        const air_cond_type = air_cond.toType(self.air, self.intern_pool);

        if (self.typeCheck(air_cond_type, .from(.type_bool)) == .mismatch) {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(.type_bool),
                .actual = air_cond_type,
            } }, cond.condition.toLoc(self.ast));
            return try self.addInvalidNode();
        }

        const then_key = cond.body.toKey(self.ast);
        const then_stmts = then_key.block;
        const force_append_unit_mode: ForceAppendUnitMode =
            if (eval_mode == .eval and else_block_opt == null)
                if (then_stmts.len > 0)
                    switch (then_stmts[then_stmts.len - 1].toKey(self.ast)) {
                        .expr_stmt,
                        => |stmt| if (stmt.toKey(self.ast) == .literal_unit)
                            .no_force_append_unit
                        else
                            .force_append_unit,

                        else => .force_append_unit, // the last stmt isn't expr stmt
                    }
                else
                    .force_append_unit // then block is empty
            else
                .no_force_append_unit; // doesn't eval or else block is present

        const then_block = try self.analyzeBlockExpr(
            then_key,
            eval_mode,
            force_append_unit_mode,
        );
        const then_block_type = then_block.toType(self.air, self.intern_pool);

        const @"type" = if (type_opt != null and type_opt.? != .type_never)
            type_opt.?
        else
            then_block_type;

        if (eval_mode == .eval and
            @"type" != .type_never and
            self.typeCheck(then_block_type, .from(@"type")) == .mismatch)
        {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(@"type"),
                .actual = then_block_type,
            } }, cond.body.toLoc(self.ast));
            return try self.addInvalidNode();
        }

        const else_block, const else_block_loc = if (elseif_blocks.len > 0)
            .{
                try self.analyzeIfExprAux(
                    elseif_blocks[0],
                    elseif_blocks[1..],
                    else_block_opt,
                    @"type",
                    eval_mode,
                ),
                elseif_blocks[0].body.toLoc(self.ast),
            }
        else if (else_block_opt) |else_block|
            .{
                try self.analyzeExpr(else_block, eval_mode),
                else_block.toLoc(self.ast),
            }
        else blk: {
            const unit = try self.addNode(.{ .constant = .value_unit });
            break :blk .{
                try self.addNode(.{ .block = &[_]Air.Index{unit} }),
                cond.body.toLoc(self.ast),
            };
        };
        const else_block_type = else_block.toType(self.air, self.intern_pool);

        std.debug.print("{any}\n", .{else_block_type});

        if (eval_mode == .eval and
            @"type" != .type_never and
            self.typeCheck(else_block_type, .from(@"type")) == .mismatch)
        {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(@"type"),
                .actual = else_block_type,
            } }, else_block_loc);
            return try self.addInvalidNode();
        }

        return try self.addNode(.{ .cond = .{
            .cond = air_cond,
            .then_branch = then_block,
            .else_branch = else_block,
        } });
    }

    fn analyzeForExpr(
        self: *Sema,
        ast_key: Ast.Key,
    ) Error!Air.Index {
        const air_cond, const body = switch (ast_key) {
            .@"for" => |body| .{
                try self.addNode(.{ .constant = .value_bool_true }),
                body,
            },
            .for_conditional => |cond| .{
                try self.analyzeExpr(cond.condition, .eval),
                cond.body,
            },
            else => unreachable, // non-for expr
        };

        const air_cond_type = air_cond.toType(self.air, self.intern_pool);

        if (ast_key == .for_conditional and
            self.typeCheck(air_cond_type, .from(.type_bool)) == .mismatch)
        {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(.type_bool),
                .actual = air_cond_type,
            } }, ast_key.for_conditional.condition.toLoc(self.ast));
            return try self.addInvalidNode();
        }

        const prev_loop_mode = self.loop_mode;

        self.loop_mode = .in_loop;
        defer self.loop_mode = prev_loop_mode;

        const air_body = try self.analyzeExpr(body, .no_eval);

        return try self.addNode(.{ .@"for" = .{
            .cond = air_cond,
            .body = air_body,
        } });
    }

    fn analyzeBreakExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!Air.Index {
        if (self.loop_mode != .in_loop) {
            try self.addDiag(
                .break_outside_loop,
                ast_expr.toLoc(self.ast),
            );
            return self.addInvalidNode();
        }

        return try self.addNode(.@"break");
    }

    fn analyzeContinueExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!Air.Index {
        if (self.loop_mode != .in_loop) {
            try self.addDiag(
                .continue_outside_loop,
                ast_expr.toLoc(self.ast),
            );
            return self.addInvalidNode();
        }

        return try self.addNode(.@"continue");
    }

    fn analyzeTypeExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!InternPool.Index {
        const key = ast_expr.toKey(self.ast);

        return switch (key) {
            .identifier => try self.analyzeIdentifierTypeExpr(ast_expr),
            else => unreachable, // non-type expr
        };
    }

    fn analyzeIdentifierTypeExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!InternPool.Index {
        const result = self.resolveIdentifier(ast_expr) catch |err|
            switch (err) {
                error.UndeclaredIdentifier => {
                    try self.addDiag(
                        .undeclared_identifier,
                        ast_expr.toLoc(self.ast),
                    );
                    return InternPool.Index.invalid;
                },
            };

        const local = switch (result) {
            .@"comptime" => |data| data.index,
            .runtime => @panic("TODO: types can't be runtime"), // todo: add proper logic after proper comptime support
        };

        const local_type = local.toItem(&self.scope, .type);
        const flags = local.toItem(&self.scope, .flags);

        if (flags.type_hood != .type) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
                    .expected = .from(.type_type),
                    .actual = local_type,
                } },
                ast_expr.toLoc(self.ast),
            );
            return .invalid;
        }

        return local_type;
    }

    fn resolveIdentifier(
        self: *const Sema,
        ast_expr: Ast.Index,
    ) error{UndeclaredIdentifier}!union(enum) {
        runtime: struct { index: Scope.Index, stack_index: usize },
        @"comptime": struct { index: Scope.Index },
    } {
        const identifier = ast_expr.toStr(self.ast);
        var runtime_locals_count: usize = 0;
        var idx = self.scope.locals.len;

        while (idx > 0) {
            idx -= 1;

            const eval_time = self.scope.locals.items(.flags)[idx].eval_time;

            if (eval_time == .runtime) {
                runtime_locals_count += 1;
            }

            const local_name = self.scope.locals.items(.name)[idx];
            const local_name_str = if (local_name.tag == .ast)
                Ast.Index.from(local_name.index).toStr(self.ast)
            else
                InternPool.Index
                    .from(local_name.index)
                    .toKey(self.intern_pool)
                    .value_string;

            if (mem.eql(u8, identifier, local_name_str)) {
                const index = Scope.Index.from(idx);

                return if (eval_time == .runtime) .{ .runtime = .{
                    .index = index,
                    .stack_index = self.scope.runtime_scope_top -
                        runtime_locals_count,
                } } else .{ .@"comptime" = .{
                    .index = index,
                } };
            }
        }

        return error.UndeclaredIdentifier;
    }

    fn typeCheck(
        self: *Sema,
        subject: InternPool.Index,
        targets: TypeArray,
    ) enum { ok, mismatch } {
        assert(subject == .invalid or
            subject.toType(self.intern_pool) == .type_type);

        if (subject == .invalid or subject == .type_never) {
            return .ok;
        }

        for (targets.slice()) |target| {
            assert(target.toType(self.intern_pool) == .type_type);

            if (target == .invalid) {
                return .ok;
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
                        return .ok;
                    }
                },

                else => unreachable, // non-type node
            }
        }

        return .mismatch;
    }

    fn typeCheckBinary(
        self: *Sema,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
        target_types: TypeArray,
    ) Error!enum { ok, mismatch } {
        const lhs_type = air_binary.lhs.toType(self.air, self.intern_pool);

        if (self.typeCheck(lhs_type, target_types) == .mismatch) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
                    .expected = target_types,
                    .actual = lhs_type,
                } },
                ast_binary.lhs.toLoc(self.ast),
            );
            return .mismatch;
        }

        const rhs_type = air_binary.rhs.toType(self.air, self.intern_pool);

        if (self.typeCheck(rhs_type, target_types) == .mismatch) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
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
            .@"for" => |@"for"| .{
                .tag = .@"for",
                .a = @"for".cond.toInt(),
                .b = @"for".body.toInt(),
            },
            .@"break" => .{ .tag = .@"break" },
            .@"continue" => .{ .tag = .@"continue" },

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
            .assignment => |assignment| .{
                .tag = .assignment,
                .a = assignment.stack_index,
                .b = assignment.rhs.toInt(),
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
