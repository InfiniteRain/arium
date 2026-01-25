const std = @import("std");
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const MultiArrayList = std.MultiArrayList;

const air_mod = @import("air.zig");
const Air = air_mod.Air;
const ast_mod = @import("ast.zig");
const Ast = ast_mod.Ast;
const fixed_array_mod = @import("fixed_array.zig");
const FixedArray = fixed_array_mod.FixedArray;
const intern_pool_mod = @import("intern_pool.zig");
const InternPool = intern_pool_mod.InternPool;
const limits = @import("limits.zig");
const Span = @import("span.zig").Span;

pub const Sema = struct {
    allocator: Allocator,
    source: []const u8,
    intern_pool: *InternPool,
    ast: *const Ast,
    air: Air,
    diags: *Diags,
    scratch: *Scratch,

    func: Func,
    scope: Scope,
    loop: ?Loop,

    pub const Error = error{AnalyzeFailure} || Allocator.Error;

    pub const Diags = struct {
        entries: ArrayList(Entry),

        pub const Entry = struct {
            tag: Tag,
            loc: Span(u8),

            pub const Tag = union(enum) {
                unexpected_expr_type: ExprTypeMismatch,
                integer_overflow,
                undeclared_identifier,
                too_many_locals,
                unassigned_variable,
                immutable_mutation: Span(u8),
                unreachable_stmt,
                break_outside_loop,
                continue_outside_loop,
                not_all_branches_return,
                non_callable_call,
                arity_mismatch: ArityMismatch,
                unexpected_arg_type: ArgTypeMismatch,
                comptime_var_mutation_at_runtime: Span(u8),
                runtime_var_access_at_comptime,
                too_many_upvalues,
                unassigned_upvalue,
                runtime_upvalue_capture_at_comptime,

                pub const ExprTypeMismatch = struct {
                    expected: TypeArray,
                    actual: InternPool.Index,
                };

                pub const ArityMismatch = struct {
                    expected: usize,
                    actual: usize,
                };

                pub const ArgTypeMismatch = struct {
                    index: usize,
                    expected: InternPool.Index,
                    actual: InternPool.Index,
                };
            };
        };

        pub const empty: Diags = .{
            .entries = .empty,
        };

        pub fn deinit(self: *Diags, allocator: Allocator) void {
            self.entries.deinit(allocator);
        }
    };

    pub const Scratch = struct {
        nodes: ArrayList(u32),

        pub const empty: Scratch = .{
            .nodes = .empty,
        };

        pub fn deinit(self: *Scratch, allocator: Allocator) void {
            self.nodes.deinit(allocator);
        }
    };

    const TypeArray = FixedArray(InternPool.Index, 2);

    const Scope = struct {
        locals: MultiArrayList(Local),
        runtime_locals_count: usize,
        max_runtime_locals_count: usize,

        const Snapshot = struct {
            scope_top: usize,
            max_runtime_locals_count: usize,
            runtime_locals_count: usize,
        };

        const init: Scope = .{
            .locals = .empty,
            .max_runtime_locals_count = 0,
            .runtime_locals_count = 0,
        };

        fn deinit(self: *Scope, allocator: Allocator) void {
            self.locals.deinit(allocator);
        }

        fn append(
            self: *Scope,
            allocator: Allocator,
            name: Local.Name,
            ip_value: InternPool.Index,
            flags: Local.Flags,
        ) Allocator.Error!void {
            if (flags.eval_time == .runtime) {
                self.runtime_locals_count += 1;

                if (self.runtime_locals_count > self.max_runtime_locals_count) {
                    self.max_runtime_locals_count = self.runtime_locals_count;
                }
            }

            try self.locals.append(allocator, .{
                .name = name,
                .ip_value = ip_value,
                .flags = flags,
                .stack_index = if (flags.eval_time == .runtime)
                    @intCast(self.runtime_locals_count - 1)
                else
                    undefined,
            });
        }
    };

    const Func = struct {
        next_id: u32,
        return_type: InternPool.Index,
        upvalues: ArrayListUnmanaged(u32),

        fn init(upvalue_buffer: []u32) Func {
            return .{
                .next_id = 0,
                .return_type = .type_unit,
                .upvalues = .initBuffer(upvalue_buffer),
            };
        }
    };

    const Loop = struct {
        scope_top: usize,
    };

    const Local = struct {
        name: Name,
        /// For locals with the comptime flag set, this field represents the
        /// comptime value. For locals without the comptime flag set, represents
        /// the type of that local.
        ip_value: InternPool.Index,
        flags: Flags,
        /// Not defined for comptime locals.
        stack_index: u32,

        const Flags = packed struct {
            mutability: Mutability,
            assignment: Assignement,
            eval_time: EvalTime,
            binding: Binding,
        };

        const Mutability = enum(u1) { mutable, immutable };

        const Assignement = enum(u1) { assigned, unassigned };

        const EvalTime = enum(u1) { @"comptime", runtime };

        const Binding = enum(u1) { captured, free };

        const Name = packed struct(u32) {
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

    const ValueUsage = enum {
        use,
        discard,
    };

    const ForceAppendUnitMode = enum {
        force_append_unit,
        no_force_append_unit,
    };

    pub fn analyze(
        allocator: Allocator,
        source: []const u8,
        intern_pool: *InternPool,
        ast: *const Ast,
        diags: *Diags,
        scratch: *Scratch,
    ) Error!Air {
        const diags_top = diags.entries.items.len;

        var upvalue_buffer: [limits.max_locals]u32 = undefined;
        const func: Func = .init(&upvalue_buffer);

        var scope: Scope = .init;
        defer scope.deinit(allocator);

        inline for (.{
            .{ "Int", .type_int },
            .{ "Float", .type_float },
            .{ "Bool", .type_bool },
            .{ "String", .type_string },
            .{ "Unit", .type_unit },
        }) |entry| {
            try scope.append(
                allocator,
                .from(try intern_pool.get(
                    allocator,
                    .{ .value_string = entry[0] },
                )),
                entry[1],
                .{
                    .mutability = .immutable,
                    .assignment = .assigned,
                    .eval_time = .@"comptime",
                    .binding = .free,
                },
            );
        }

        var sema: Sema = .{
            .allocator = allocator,
            .source = source,
            .intern_pool = intern_pool,
            .ast = ast,
            .air = .empty,
            .diags = diags,
            .scratch = scratch,

            .func = func,
            .scope = scope,
            .loop = null,
        };
        errdefer sema.air.deinit(allocator);

        try sema.air.nodes.append(allocator, undefined);
        try sema.air.locs.append(allocator, .zero);

        const @"fn" = try sema.analyzeFnExpr(
            null,
            &[_]Ast.Key.FnArg{},
            null,
            .from(0),
        );

        if (diags.entries.items.len > diags_top) {
            return error.AnalyzeFailure;
        }

        sema.air.nodes.set(0, try sema.prepareNode(@"fn"));

        assert(sema.air.nodes.len == sema.air.locs.items.len);

        return sema.air;
    }

    fn analyzeStmt(
        self: *Sema,
        ast_stmt: Ast.Index,
        value_usage: ValueUsage,
    ) Error!Air.Index {
        const ast_key = ast_stmt.toKey(self.ast);

        return switch (ast_key) {
            .assert,
            => |child_expr| try self.analyzeAssertStmt(ast_stmt, child_expr),

            .print,
            => |child_expr| try self.analyzePrintStmt(ast_stmt, child_expr),

            .expr_stmt,
            => |expr| try self.analyzeExpr(expr, value_usage),

            .let,
            .let_mut,
            => |let| try self.analyzeLetStmt(ast_stmt, ast_key, let),

            .@"fn",
            => |@"fn"| try self.analyzeFnStmt(
                ast_stmt,
                @"fn".identifier,
                @"fn".params,
                @"fn".return_type,
                @"fn".body,
            ),

            else => unreachable, // non-stmt node
        };
    }

    fn analyzeAssertStmt(
        self: *Sema,
        ast_stmt: Ast.Index,
        child_ast_expr: Ast.Index,
    ) Error!Air.Index {
        const sema_child_expr = try self.analyzeExpr(
            child_ast_expr,
            .use,
        );
        const child_type = sema_child_expr.toType(&self.air, self.intern_pool);

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

        return self.addNode(
            .{ .assert = sema_child_expr },
            ast_stmt.toLoc(self.ast),
        );
    }

    fn analyzePrintStmt(
        self: *Sema,
        ast_stmt: Ast.Index,
        child_ast_expr: Ast.Index,
    ) Error!Air.Index {
        const sema_child_expr = try self.analyzeExpr(
            child_ast_expr,
            .use,
        );

        return self.addNode(
            .{ .print = sema_child_expr },
            ast_stmt.toLoc(self.ast),
        );
    }

    fn analyzeLetStmt(
        self: *Sema,
        ast_stmt: Ast.Index,
        ast_stmt_key: Ast.Key,
        let: Ast.Key.Let,
    ) Error!Air.Index {
        const let_type_opt = if (let.type) |ast_type_expr|
            try self.evaluateTypeExpr(ast_type_expr)
        else
            null;
        const mutability: Local.Mutability = if (ast_stmt_key == .let)
            .immutable
        else
            .mutable;
        const identifier = let.identifier;

        const ast_expr = let.expr orelse {
            try self.scope.append(
                self.allocator,
                .from(identifier),
                if (let_type_opt) |let_type| let_type else .none,
                .{
                    .mutability = mutability,
                    .assignment = .unassigned,
                    .eval_time = .runtime,
                    .binding = .free,
                },
            );

            return self.addNode(
                .{ .let = .{
                    .stack_index = @intCast(
                        self.scope.runtime_locals_count - 1,
                    ),
                    .rhs = null,
                } },
                ast_stmt.toLoc(self.ast),
            );
        };

        const air_expr = try self.analyzeExpr(ast_expr, .use);

        if (self.scope.runtime_locals_count >= limits.max_locals) {
            try self.addDiag(
                .too_many_locals,
                ast_stmt.toLoc(self.ast),
            );
        }

        const local_type = air_expr.toType(&self.air, self.intern_pool);
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

        try self.scope.append(
            self.allocator,
            .from(identifier),
            final_type,
            .{
                .mutability = mutability,
                .assignment = .assigned,
                .eval_time = .runtime,
                .binding = .free,
            },
        );

        return self.addNode(
            .{ .let = .{
                .stack_index = @intCast(self.scope.runtime_locals_count - 1),
                .rhs = air_expr,
            } },
            ast_stmt.toLoc(self.ast),
        );
    }

    fn analyzeFnStmt(
        self: *Sema,
        ast_stmt: Ast.Index,
        identifier: Ast.Index,
        args: []const Ast.Key.FnArg,
        return_type: ?Ast.Index,
        body: Ast.Index,
    ) Error!Air.Index {
        const loc = ast_stmt.toLoc(self.ast);
        const fn_air_key = try self.analyzeFnExpr(
            identifier,
            args,
            return_type,
            body,
        );
        const fn_air = try self.addNode(fn_air_key, loc);

        try self.scope.append(
            self.allocator,
            .from(identifier),
            fn_air_key.@"fn".fn_type,
            .{
                .mutability = .immutable,
                .assignment = .assigned,
                .eval_time = .runtime,
                .binding = .free,
            },
        );

        return self.addNode(.{ .let = .{
            .stack_index = @intCast(self.scope.runtime_locals_count - 1),
            .rhs = fn_air,
        } }, loc);
    }

    fn analyzeFnExpr(
        self: *Sema,
        identifier_opt: ?Ast.Index,
        args: []const Ast.Key.FnArg,
        return_type: ?Ast.Index,
        body: Ast.Index,
    ) Error!Air.Key {
        const scratch_top = self.scratch.nodes.items.len;
        defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

        const id = self.func.next_id;

        self.func.next_id += 1;
        try self.scratch.nodes.ensureUnusedCapacity(self.allocator, args.len);

        for (args) |arg| {
            const arg_type = try self.evaluateTypeExpr(arg.type);

            try self.scratch.nodes.append(
                self.allocator,
                arg_type.toInt(),
            );
        }

        const return_type_index =
            if (return_type) |@"type"|
                try self.evaluateTypeExpr(@"type")
            else
                .type_unit;

        const fn_type = try self.intern_pool.get(
            self.allocator,
            .{
                .type_fn = .{
                    .arg_types = @ptrCast(
                        self.scratch.nodes.items[scratch_top..],
                    ),
                    .return_type = return_type_index,
                },
            },
        );

        const fn_name: Local.Name = if (identifier_opt) |identifier|
            .from(identifier)
        else
            .from(try self.intern_pool.get(
                self.allocator,
                .{ .value_string = "" },
            ));

        var air_body: Air.Index = undefined;

        const upvalues_top = self.func.upvalues.items.len;
        defer self.func.upvalues.shrinkRetainingCapacity(upvalues_top);

        const locals_count: u32 = blk: {
            const scope_snapshot = self.beginScope();
            defer self.endFunctionScope(scope_snapshot);

            self.scope.runtime_locals_count = 0;
            self.scope.max_runtime_locals_count = 0;

            const prev_fn_return_type = self.func.return_type;
            defer self.func.return_type = prev_fn_return_type;

            self.func.return_type = return_type_index;

            try self.scope.append(
                self.allocator,
                fn_name,
                fn_type,
                .{
                    .mutability = .immutable,
                    .assignment = .assigned,
                    .eval_time = .runtime,
                    .binding = .free,
                },
            );

            for (
                args,
                self.scratch.nodes.items[scratch_top..],
            ) |arg, type_idx| {
                try self.scope.append(
                    self.allocator,
                    .from(arg.identifier),
                    .from(type_idx),
                    .{
                        .mutability = .immutable,
                        .assignment = .assigned,
                        .eval_time = .runtime,
                        .binding = .free,
                    },
                );
            }

            const body_key = body.toKey(self.ast);
            air_body = try self.analyzeBlockExprInheritScope(
                body,
                body_key,
                .discard,
                .no_force_append_unit,
                scope_snapshot,
            );

            // todo: we should not need this at all
            if (identifier_opt != null and
                return_type_index != .type_unit and
                air_body.toType(&self.air, self.intern_pool) != .type_never)
            {
                try self.addDiag(
                    .not_all_branches_return,
                    identifier_opt.?.toLoc(self.ast),
                );
                return .{ .constant = .invalid };
            }

            break :blk @intCast(self.scope.max_runtime_locals_count);
        };

        const stack_index = self.scope.locals.items(.stack_index);

        for (
            self.func.upvalues.items[upvalues_top..],
            upvalues_top..,
        ) |upvalue, upvalue_index| {
            self.func.upvalues.items[upvalue_index] = stack_index[upvalue];
        }

        return .{ .@"fn" = .{
            .id = id,
            .body = air_body,
            .locals_count = locals_count,
            .fn_type = fn_type,
            .upvalues = self.func.upvalues.items[upvalues_top..],
        } };
    }

    fn analyzeExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        value_usage: ValueUsage,
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
            => |ast_binary| try self.analyzeBinaryExpr(
                ast_expr,
                ast_key,
                ast_binary,
            ),

            .neg_bool,
            .neg_num,
            => |child_ast_expr| try self.analyzeUnaryExpr(
                ast_expr,
                ast_key,
                child_ast_expr,
            ),

            .block,
            .block_semicolon,
            => try self.analyzeBlockExpr(
                ast_expr,
                ast_key,
                value_usage,
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
            => try self.analyzeIfExpr(ast_expr, ast_key, value_usage),

            .@"for",
            .for_conditional,
            => try self.analyzeForExpr(ast_expr, ast_key),

            .@"break",
            => try self.analyzeBreakExpr(ast_expr),

            .@"continue",
            => try self.analyzeContinueExpr(ast_expr),

            .@"return",
            .return_value,
            => try self.analyzeReturnExpr(ast_expr, ast_key),

            .call_simple,
            .call,
            => try self.analyzeCallExpr(ast_expr, ast_key),

            .fn_type,
            => try self.addNode(
                .{ .constant = try self.evaluateExpr(ast_expr) },
                ast_expr.toLoc(self.ast),
            ),

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
                    ast_expr.toLoc(self.ast).toSlice(self.source),
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
                ast_expr.toLoc(self.ast).toSlice(self.source),
            ) catch @panic("invalid float format") },

            .literal_bool => .{
                .value_simple = if (ast_expr.toLoc(self.ast)
                    .toSlice(self.source).len == 4)
                    .bool_true
                else
                    .bool_false,
            },

            .literal_string => blk: {
                const lexeme = ast_expr.toLoc(self.ast)
                    .toSlice(self.source);
                break :blk .{ .value_string = lexeme[1 .. lexeme.len - 1] };
            },

            else => unreachable, // non-literal node
        };

        const intern_pool_index = try self.intern_pool.get(
            self.allocator,
            intern_pool_key,
        );

        return self.addNode(
            .{ .constant = intern_pool_index },
            ast_expr.toLoc(self.ast),
        );
    }

    fn analyzeBinaryExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
    ) Error!Air.Index {
        const air_binary: Air.Key.Binary = .{
            .lhs = try self.analyzeExpr(ast_binary.lhs, .use),
            .rhs = try self.analyzeExpr(ast_binary.rhs, .use),
        };

        if (air_binary.lhs.toType(&self.air, self.intern_pool) == .invalid or
            air_binary.rhs.toType(&self.air, self.intern_pool) == .invalid)
        {
            return self.addInvalidNode();
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
                ast_expr,
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            .concat,
            => try self.analyzeConcatBinaryExpr(
                ast_expr,
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            .equal,
            .not_equal,
            => try self.analyzeEqualBinaryExpr(
                ast_expr,
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            .@"and",
            .@"or",
            => try self.analyzeCondBinaryExpr(
                ast_expr,
                ast_expr_key,
                ast_binary,
                air_binary,
            ),

            else => unreachable,
        };
    }

    fn analyzeNumericBinaryExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        if (try self.typeCheckBinary(
            ast_binary,
            air_binary,
            .from(.{ .type_int, .type_float }),
        ) == .mismatch) {
            return self.addInvalidNode();
        }

        return self.addNode(switch (ast_expr_key) {
            .add => .{ .add = air_binary },
            .sub => .{ .sub = air_binary },
            .mul => .{ .mul = air_binary },
            .div => .{ .div = air_binary },
            .greater_than => .{ .greater_than = air_binary },
            .greater_equal => .{ .greater_equal = air_binary },
            .less_than => .{ .less_than = air_binary },
            .less_equal => .{ .less_equal = air_binary },
            else => unreachable, // non-numeric binary node
        }, ast_expr.toLoc(self.ast));
    }

    fn analyzeConcatBinaryExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        if (try self.typeCheckBinary(
            ast_binary,
            air_binary,
            .from(.type_string),
        ) == .mismatch) {
            return self.addInvalidNode();
        }

        return self.addNode(switch (ast_expr_key) {
            .concat => .{ .concat = air_binary },
            else => unreachable, // non-concat binary node
        }, ast_expr.toLoc(self.ast));
    }

    fn analyzeCondBinaryExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        if (try self.typeCheckBinary(
            ast_binary,
            air_binary,
            .from(.type_bool),
        ) == .mismatch) {
            return self.addInvalidNode();
        }

        const loc = ast_expr.toLoc(self.ast);

        return self.addNode(switch (ast_expr_key) {
            .@"and" => .{ .cond = .{
                .cond = air_binary.lhs,
                .then_branch = air_binary.rhs,
                .else_branch = try self.addNode(.{
                    .constant = .value_bool_false,
                }, loc),
            } },
            .@"or" => .{ .cond = .{
                .cond = air_binary.lhs,
                .then_branch = try self.addNode(.{
                    .constant = .value_bool_true,
                }, loc),
                .else_branch = air_binary.rhs,
            } },
            else => unreachable, // non-cond binary node
        }, loc);
    }

    fn analyzeEqualBinaryExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
        ast_binary: Ast.Key.Binary,
        air_binary: Air.Key.Binary,
    ) Error!Air.Index {
        const lhs_type = air_binary.lhs.toType(&self.air, self.intern_pool);
        const rhs_type = air_binary.rhs.toType(&self.air, self.intern_pool);

        if (self.typeCheck(rhs_type, .from(lhs_type)) == .mismatch) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
                    .expected = .from(lhs_type),
                    .actual = rhs_type,
                } },
                ast_binary.rhs.toLoc(self.ast),
            );
            return self.addInvalidNode();
        }

        return self.addNode(switch (ast_expr_key) {
            .equal => .{ .equal = air_binary },
            .not_equal => .{ .not_equal = air_binary },
            else => unreachable, // non-equality binary node
        }, ast_expr.toLoc(self.ast));
    }

    fn analyzeUnaryExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_key: Ast.Key,
        child_ast_expr: Ast.Index,
    ) Error!Air.Index {
        const child_air_expr = try self.analyzeExpr(child_ast_expr, .use);
        const child_type = child_air_expr.toType(&self.air, self.intern_pool);

        if (child_type == .invalid) {
            return self.addInvalidNode();
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
            return self.addInvalidNode();
        }

        return self.addNode(
            .{ .neg = child_air_expr },
            ast_expr.toLoc(self.ast),
        );
    }

    fn analyzeBlockExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
        value_usage: ValueUsage,
        force_append_unit_mode: ForceAppendUnitMode,
    ) Error!Air.Index {
        const scope_snapshot = self.beginScope();
        defer self.endScope(scope_snapshot);

        return self.analyzeBlockExprInheritScope(
            ast_expr,
            ast_expr_key,
            value_usage,
            force_append_unit_mode,
            scope_snapshot,
        );
    }

    fn analyzeBlockExprInheritScope(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_expr_key: Ast.Key,
        value_usage: ValueUsage,
        force_append_unit_mode: ForceAppendUnitMode,
        scope_snapshot: Scope.Snapshot,
    ) Error!Air.Index {
        const scratch_top = self.scratch.nodes.items.len;
        defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

        const stmts = switch (ast_expr_key) {
            .block,
            .block_semicolon,
            => |stmts| stmts,

            else => unreachable, // non-block node
        };
        var is_last_stmt_never = false;

        for (stmts, 0..) |stmt, i| {
            const is_last = i == stmts.len - 1;

            const stmt_value_usage = if (is_last)
                value_usage
            else
                .discard;

            const sema_stmt = self.analyzeStmt(
                stmt,
                stmt_value_usage,
            ) catch |err| switch (err) {
                error.AnalyzeFailure => continue,
                else => return err,
            };

            try self.scratch.nodes.append(self.allocator, sema_stmt.toInt());

            if (sema_stmt.toType(&self.air, self.intern_pool) == .type_never) {
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

        const is_block_empty = scratch_top == self.scratch.nodes.items.len;
        const last_stmt_opt =
            if (!is_block_empty)
                Air.Index.from(self.scratch.nodes.getLast())
            else
                null;
        const is_last_stmt_not_expr =
            if (last_stmt_opt) |last_stmt|
                last_stmt.toKind(&self.air) != .expr
            else
                false;
        const is_last_stmt_unit =
            !is_block_empty and
            last_stmt_opt.?.toType(&self.air, self.intern_pool) != .type_unit;
        const is_block_sm_no_unit =
            ast_expr_key == .block_semicolon and
            is_last_stmt_unit;
        const should_append_unit =
            is_block_empty or
            (!is_last_stmt_never and
                value_usage == .use and
                (is_last_stmt_not_expr or
                    is_block_sm_no_unit or
                    force_append_unit_mode == .force_append_unit));

        if (should_append_unit) {
            const unit_node = try self.addNode(
                .{
                    .constant = try self.intern_pool.get(
                        self.allocator,
                        .{ .value_simple = .unit },
                    ),
                },
                ast_expr.toLoc(self.ast),
            );
            try self.scratch.nodes.append(self.allocator, unit_node.toInt());
        }

        const exprs_len = self.scratch.nodes.items.len - scratch_top;
        const locals_slice = self.scope.locals.slice();
        const flags = locals_slice.items(.flags);
        const stack_index = locals_slice.items(.stack_index);
        var idx = self.scope.locals.len;

        while (idx > scope_snapshot.scope_top) {
            idx -= 1;

            if (flags[idx].eval_time == .@"comptime") {
                continue;
            }

            if (flags[idx].binding == .captured) {
                try self.scratch.nodes.append(
                    self.allocator,
                    stack_index[idx],
                );
            }
        }

        return self.addNode(
            .{ .block = .{
                .exprs = @ptrCast(
                    self.scratch.nodes.items[scratch_top..][0..exprs_len],
                ),
                .closed = self.scratch.nodes.items[scratch_top + exprs_len ..],
            } },
            ast_expr.toLoc(self.ast),
        );
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
                    return self.addInvalidNode();
                },
            };
        const loc = ast_expr.toLoc(self.ast);
        const scope_slice = self.scope.locals.slice();
        const flags = scope_slice.items(.flags);
        const types = scope_slice.items(.ip_value);

        switch (result) {
            .runtime => |data| {
                if (flags[data.index].assignment == .unassigned) {
                    try self.addDiag(
                        .unassigned_variable,
                        ast_expr.toLoc(self.ast),
                    );
                    return self.addInvalidNode();
                }

                return self.addNode(
                    .{ .variable = .{
                        .stack_index = @intCast(data.stack_index),
                        .type = types[data.index],
                    } },
                    loc,
                );
            },
            .runtime_upvalue => |data| {
                const upvalue_index =
                    self.appendUpvalue(@intCast(data.index)) catch |err|
                        switch (err) {
                            error.TooManyUpvalues => {
                                try self.addDiag(.too_many_upvalues, loc);
                                return self.addInvalidNode();
                            },
                            else => |sema_error| return sema_error,
                        };

                if (flags[data.index].assignment == .unassigned) {
                    try self.addDiag(
                        .unassigned_upvalue,
                        ast_expr.toLoc(self.ast),
                    );
                    return self.addInvalidNode();
                }

                return self.addNode(
                    .{ .get_upvalue = .{
                        .stack_index = @intCast(upvalue_index),
                        .type = types[data.index],
                    } },
                    loc,
                );
            },
            .@"comptime" => |data| {
                return self.addNode(.{ .constant = data.ip_index }, loc);
            },
            .comptime_upvalue => |data| {
                return self.addNode(.{ .constant = data.ip_index }, loc);
            },
        }
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
                    return self.addInvalidNode();
                },
            };

        const scope_slice = self.scope.locals.slice();
        const flags = scope_slice.items(.flags);
        const expr_loc = ast_expr.toLoc(self.ast);
        const lhs_loc = binary.lhs.toLoc(self.ast);
        const local_index, const stack_index, const binding: Local.Binding =
            switch (result) {
                .runtime,
                => |data| blk: {
                    if (flags[data.index].mutability == .immutable and
                        flags[data.index].assignment == .assigned)
                    {
                        try self.addDiag(
                            .{ .immutable_mutation = lhs_loc },
                            expr_loc,
                        );
                        return self.addInvalidNode();
                    }

                    flags[data.index].assignment = .assigned;

                    break :blk .{ data.index, data.stack_index, .free };
                },

                .runtime_upvalue,
                => |data| blk: {
                    if (flags[data.index].assignment == .unassigned) {
                        try self.addDiag(.unassigned_upvalue, expr_loc);
                        return self.addInvalidNode();
                    }

                    if (flags[data.index].mutability == .immutable) {
                        try self.addDiag(
                            .{ .immutable_mutation = lhs_loc },
                            expr_loc,
                        );
                        return self.addInvalidNode();
                    }

                    const upvalue_index =
                        self.appendUpvalue(@intCast(data.index)) catch |err|
                            switch (err) {
                                error.TooManyUpvalues => {
                                    try self.addDiag(
                                        .too_many_upvalues,
                                        expr_loc,
                                    );
                                    return self.addInvalidNode();
                                },
                                else => |sema_error| return sema_error,
                            };

                    break :blk .{ data.index, upvalue_index, .captured };
                },

                .@"comptime",
                .comptime_upvalue,
                => {
                    try self.addDiag(
                        .{ .comptime_var_mutation_at_runtime = lhs_loc },
                        expr_loc,
                    );
                    return self.addInvalidNode();
                },
            };

        const rhs = try self.analyzeExpr(binary.rhs, .use);
        const rhs_type = rhs.toType(&self.air, self.intern_pool);
        const lhs_type = &scope_slice.items(.ip_value)[local_index];

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
            return self.addInvalidNode();
        } else if (lhs_type.* == .none) {
            lhs_type.* = rhs_type;
        }

        const assignment: Air.Key.Assignment = .{
            .stack_index = @intCast(stack_index),
            .rhs = rhs,
        };

        if (binding == .captured) {
            return self.addNode(.{ .set_upvalue = assignment }, expr_loc);
        } else {
            return self.addNode(.{ .assignment = assignment }, expr_loc);
        }
    }

    fn analyzeIfExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_key: Ast.Key,
        value_usage: ValueUsage,
    ) Error!Air.Index {
        var condition: Ast.Key.Conditional = undefined;
        var elseif_blocks: []const Ast.Key.Conditional =
            &[_]Ast.Key.Conditional{};
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

        return self.analyzeIfExprAux(
            ast_expr.toLoc(self.ast),
            condition,
            elseif_blocks,
            else_block,
            null,
            value_usage,
        );
    }

    fn analyzeIfExprAux(
        self: *Sema,
        loc: Span(u8),
        cond: Ast.Key.Conditional,
        elseif_blocks: []const Ast.Key.Conditional,
        else_block_opt: ?Ast.Index,
        type_opt: ?InternPool.Index,
        value_usage: ValueUsage,
    ) Error!Air.Index {
        const air_cond = try self.analyzeExpr(cond.condition, .use);
        const air_cond_type = air_cond.toType(&self.air, self.intern_pool);

        if (self.typeCheck(air_cond_type, .from(.type_bool)) == .mismatch) {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(.type_bool),
                .actual = air_cond_type,
            } }, cond.condition.toLoc(self.ast));
            return self.addInvalidNode();
        }

        const then_key = cond.body.toKey(self.ast);
        const then_stmts = then_key.block;
        const force_append_unit_mode: ForceAppendUnitMode =
            if (value_usage == .use and else_block_opt == null)
                if (then_stmts.len > 0)
                    switch (then_stmts[then_stmts.len - 1].toKey(self.ast)) {
                        .expr_stmt,
                        => |stmt| if (stmt.toKey(self.ast) == .literal_unit)
                            .no_force_append_unit
                        else
                            .force_append_unit,

                        // the last stmt isn't expr stmt
                        else => .force_append_unit,
                    }
                else
                    .force_append_unit // then block is empty
            else
                .no_force_append_unit; // doesn't eval or else block is present

        const then_block = try self.analyzeBlockExpr(
            cond.body,
            then_key,
            value_usage,
            force_append_unit_mode,
        );
        const then_block_type = then_block.toType(&self.air, self.intern_pool);

        const @"type" = if (type_opt != null and type_opt.? != .type_never)
            type_opt.?
        else
            then_block_type;

        if (value_usage == .use and
            @"type" != .type_never and
            self.typeCheck(then_block_type, .from(@"type")) == .mismatch)
        {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(@"type"),
                .actual = then_block_type,
            } }, cond.body.toLoc(self.ast));
            return self.addInvalidNode();
        }

        const else_block, const else_block_loc = if (elseif_blocks.len > 0)
            .{
                try self.analyzeIfExprAux(
                    elseif_blocks[0]
                        .condition
                        .toLoc(self.ast)
                        .extend(elseif_blocks[0].body.toLoc(self.ast)),
                    elseif_blocks[0],
                    elseif_blocks[1..],
                    else_block_opt,
                    @"type",
                    value_usage,
                ),
                elseif_blocks[0].body.toLoc(self.ast),
            }
        else if (else_block_opt) |else_block|
            .{
                try self.analyzeExpr(else_block, value_usage),
                else_block.toLoc(self.ast),
            }
        else blk: {
            const unit = try self.addNode(.{ .constant = .value_unit }, loc);
            break :blk .{
                try self.addNode(.{ .block = .{
                    .exprs = &[_]Air.Index{unit},
                    .closed = &.{},
                } }, loc),
                cond.body.toLoc(self.ast),
            };
        };
        const else_block_type = else_block.toType(&self.air, self.intern_pool);

        if (value_usage == .use and
            @"type" != .type_never and
            self.typeCheck(else_block_type, .from(@"type")) == .mismatch)
        {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(@"type"),
                .actual = else_block_type,
            } }, else_block_loc);
            return self.addInvalidNode();
        }

        return self.addNode(
            .{ .cond = .{
                .cond = air_cond,
                .then_branch = then_block,
                .else_branch = else_block,
            } },
            loc,
        );
    }

    fn analyzeForExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_key: Ast.Key,
    ) Error!Air.Index {
        const loc = ast_expr.toLoc(self.ast);
        const air_cond, const body = switch (ast_key) {
            .@"for" => |body| .{
                try self.addNode(.{ .constant = .value_bool_true }, loc),
                body,
            },
            .for_conditional => |cond| .{
                try self.analyzeExpr(cond.condition, .use),
                cond.body,
            },
            else => unreachable, // non-for expr
        };

        const air_cond_type = air_cond.toType(&self.air, self.intern_pool);

        if (ast_key == .for_conditional and
            self.typeCheck(air_cond_type, .from(.type_bool)) == .mismatch)
        {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(.type_bool),
                .actual = air_cond_type,
            } }, ast_key.for_conditional.condition.toLoc(self.ast));
            return self.addInvalidNode();
        }

        const prev_loop = self.loop;

        self.loop = .{ .scope_top = self.scope.locals.len };
        defer self.loop = prev_loop;

        const air_body = try self.analyzeExpr(body, .discard);

        return self.addNode(.{ .@"for" = .{
            .cond = air_cond,
            .body = air_body,
        } }, loc);
    }

    fn analyzeBreakExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!Air.Index {
        if (self.loop == null) {
            try self.addDiag(
                .break_outside_loop,
                ast_expr.toLoc(self.ast),
            );
            return self.addInvalidNode();
        }

        return self.addNode(.@"break", ast_expr.toLoc(self.ast));
    }

    fn analyzeContinueExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!Air.Index {
        if (self.loop == null) {
            try self.addDiag(
                .continue_outside_loop,
                ast_expr.toLoc(self.ast),
            );
            return self.addInvalidNode();
        }

        return self.addNode(.@"continue", ast_expr.toLoc(self.ast));
    }

    fn analyzeReturnExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_key: Ast.Key,
    ) Error!Air.Index {
        const loc = ast_expr.toLoc(self.ast);
        const rhs = if (ast_key == .return_value)
            try self.analyzeExpr(ast_key.return_value, .use)
        else
            try self.addNode(.{ .constant = .value_unit }, loc);
        const rhs_type = rhs.toType(&self.air, self.intern_pool);

        if (self.typeCheck(rhs_type, .from(self.func.return_type)) ==
            .mismatch)
        {
            try self.addDiag(.{ .unexpected_expr_type = .{
                .expected = .from(self.func.return_type),
                .actual = rhs_type,
            } }, switch (ast_key) {
                .return_value => |ast_rhs| ast_rhs.toLoc(self.ast),
                .@"return" => ast_expr.toLoc(self.ast),
                else => unreachable, // non-return expr
            });
        }

        return self.addNode(.{ .@"return" = rhs }, loc);
    }

    fn analyzeCallExpr(
        self: *Sema,
        ast_expr: Ast.Index,
        ast_key: Ast.Key,
    ) Error!Air.Index {
        const ast_callee = switch (ast_key) {
            .call_simple => |call_simple| call_simple.callee,
            .call => |call| call.callee,
            else => unreachable, // non-call expr
        };
        const callee = try self.analyzeExpr(ast_callee, .use);
        const callee_type = callee.toType(&self.air, self.intern_pool);

        if (callee_type == .invalid) {
            return self.addInvalidNode();
        }

        const callee_type_key = callee_type.toKey(self.intern_pool);

        if (callee_type_key != .type_fn) {
            try self.addDiag(.non_callable_call, ast_expr.toLoc(self.ast));
            return self.addInvalidNode();
        }

        const args = switch (ast_key) {
            .call_simple,
            => |call_simple| blk: {
                if (call_simple.arg) |arg| {
                    break :blk &[_]Ast.Index{arg};
                } else {
                    break :blk &[_]Ast.Index{};
                }
            },
            .call => |call| call.args,
            else => unreachable, // should never happen
        };

        const arg_types = callee_type_key.type_fn.arg_types;

        if (arg_types.len != args.len) {
            try self.addDiag(.{ .arity_mismatch = .{
                .expected = arg_types.len,
                .actual = args.len,
            } }, ast_expr.toLoc(self.ast));
            return self.addInvalidNode();
        }

        const scratch_top = self.scratch.nodes.items.len;
        defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

        try self.scratch.nodes.ensureUnusedCapacity(
            self.allocator,
            arg_types.len,
        );

        for (arg_types, args, 0..) |arg_type, arg, index| {
            const expr = try self.analyzeExpr(arg, .use);
            const expr_type = expr.toType(&self.air, self.intern_pool);

            if (self.typeCheck(expr_type, .from(arg_type)) == .mismatch) {
                try self.addDiag(.{ .unexpected_arg_type = .{
                    .index = index,
                    .expected = arg_type,
                    .actual = expr_type,
                } }, arg.toLoc(self.ast));
                return self.addInvalidNode();
            }

            self.scratch.nodes.appendAssumeCapacity(expr.toInt());
        }

        return self.addNode(
            switch (arg_types.len) {
                else => .{ .call = .{
                    .callee = callee,
                    .args = @ptrCast(self.scratch.nodes.items[scratch_top..]),
                } },
            },
            ast_expr.toLoc(self.ast),
        );
    }

    fn evaluateTypeExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!InternPool.Index {
        const value = try self.evaluateExpr(ast_expr);

        if (self.typeCheck(
            value.toType(self.intern_pool),
            .from(.type_type),
        ) == .mismatch) {
            try self.addDiag(
                .{ .unexpected_expr_type = .{
                    .expected = .from(.type_type),
                    .actual = value.toType(self.intern_pool),
                } },
                ast_expr.toLoc(self.ast),
            );
            return .invalid;
        }

        return value;
    }

    fn evaluateExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!InternPool.Index {
        const key = ast_expr.toKey(self.ast);

        return switch (key) {
            .identifier => try self.evaluateVariableExpr(ast_expr),
            .fn_type => |fn_type| try self.evaluateFnTypeExpr(fn_type),
            else => @panic("todo: implement comptime eval"),
        };
    }

    fn evaluateVariableExpr(
        self: *Sema,
        ast_expr: Ast.Index,
    ) Error!InternPool.Index {
        const expr_loc = ast_expr.toLoc(self.ast);
        const result = self.resolveIdentifier(ast_expr) catch |err|
            switch (err) {
                error.UndeclaredIdentifier => {
                    try self.addDiag(.undeclared_identifier, expr_loc);
                    return .invalid;
                },
            };

        const local = switch (result) {
            .runtime,
            => {
                try self.addDiag(.runtime_var_access_at_comptime, expr_loc);
                return .invalid;
            },

            .runtime_upvalue,
            => {
                try self.addDiag(
                    .runtime_upvalue_capture_at_comptime,
                    expr_loc,
                );
                return .invalid;
            },

            .@"comptime",
            .comptime_upvalue,
            => |data| data.index,
        };

        return self.scope.locals.items(.ip_value)[local];
    }

    fn evaluateFnTypeExpr(
        self: *Sema,
        fn_type: Ast.Key.FnType,
    ) Error!InternPool.Index {
        const scratch_top = self.scratch.nodes.items.len;
        defer self.scratch.nodes.shrinkRetainingCapacity(scratch_top);

        try self.scratch.nodes.ensureUnusedCapacity(
            self.allocator,
            fn_type.arg_types.len,
        );

        for (fn_type.arg_types) |arg_type_expr| {
            const arg_type = try self.evaluateTypeExpr(arg_type_expr);
            self.scratch.nodes.appendAssumeCapacity(arg_type.toInt());
        }

        const return_type = if (fn_type.return_type) |return_type|
            try self.evaluateTypeExpr(return_type)
        else
            .type_unit;

        return self.intern_pool.get(
            self.allocator,
            .{ .type_fn = .{
                .arg_types = @ptrCast(self.scratch.nodes.items[scratch_top..]),
                .return_type = return_type,
            } },
        );
    }

    pub const ResolveIdentifierResult = union(enum) {
        runtime: struct { index: usize, stack_index: usize },
        runtime_upvalue: struct { index: usize },
        @"comptime": Comptime,
        comptime_upvalue: Comptime,

        pub const Comptime = struct {
            index: usize,
            ip_index: InternPool.Index,
        };
    };

    fn resolveIdentifier(
        self: *const Sema,
        ast_expr: Ast.Index,
    ) error{UndeclaredIdentifier}!ResolveIdentifierResult {
        const locals_slice = self.scope.locals.slice();
        const flags = locals_slice.items(.flags);
        const name = locals_slice.items(.name);
        const ip_value = locals_slice.items(.ip_value);
        const stack_index = locals_slice.items(.stack_index);
        const identifier = ast_expr.toLoc(self.ast).toSlice(self.source);
        var runtime_locals_count: usize = 0;
        var idx = self.scope.locals.len;

        while (idx > 0) {
            idx -= 1;

            const eval_time = flags[idx].eval_time;

            if (eval_time == .runtime) {
                runtime_locals_count += 1;
            }

            const local_name = name[idx];
            const local_name_str = if (local_name.tag == .ast)
                Ast.Index.from(local_name.index)
                    .toLoc(self.ast)
                    .toSlice(self.source)
            else
                InternPool.Index
                    .from(local_name.index)
                    .toKey(self.intern_pool)
                    .value_string;

            if (mem.eql(u8, identifier, local_name_str)) {
                if (eval_time == .@"comptime") {
                    const comptime_result: ResolveIdentifierResult.Comptime = .{
                        .index = idx,
                        .ip_index = ip_value[idx],
                    };

                    if (runtime_locals_count >
                        self.scope.runtime_locals_count)
                    {
                        return .{ .comptime_upvalue = comptime_result };
                    } else {
                        return .{ .@"comptime" = comptime_result };
                    }
                }

                if (runtime_locals_count > self.scope.runtime_locals_count) {
                    return .{ .runtime_upvalue = .{ .index = idx } };
                }

                return .{ .runtime = .{
                    .index = idx,
                    .stack_index = stack_index[idx],
                } };
            }
        }

        return error.UndeclaredIdentifier;
    }

    fn beginScope(self: *Sema) Scope.Snapshot {
        return .{
            .scope_top = self.scope.locals.len,
            .max_runtime_locals_count = self.scope.max_runtime_locals_count,
            .runtime_locals_count = self.scope.runtime_locals_count,
        };
    }

    fn endScope(self: *Sema, snap: Scope.Snapshot) void {
        self.scope.locals.shrinkRetainingCapacity(snap.scope_top);
        self.scope.runtime_locals_count = snap.runtime_locals_count;
    }

    fn endFunctionScope(self: *Sema, snap: Scope.Snapshot) void {
        self.endScope(snap);
        self.scope.max_runtime_locals_count = snap.max_runtime_locals_count;
    }

    fn appendUpvalue(
        self: *Sema,
        scope_index: u32,
    ) (error{TooManyUpvalues} || Allocator.Error)!usize {
        for (self.func.upvalues.items, 0..) |upvalue, upvalue_index| {
            if (upvalue == scope_index) {
                return upvalue_index;
            }
        }

        if (self.func.upvalues.items.len == self.func.upvalues.capacity) {
            return error.TooManyUpvalues;
        }

        self.func.upvalues.appendAssumeCapacity(scope_index);

        self.scope.locals.items(.flags)[scope_index].binding = .captured;

        return self.func.upvalues.items.len - 1;
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
            if (target == .invalid) {
                return .ok;
            }

            assert(target.toType(self.intern_pool) == .type_type);

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

                else => {
                    const subject_key = subject.toKey(self.intern_pool);
                    const target_key = target.toKey(self.intern_pool);

                    switch (subject_key) {
                        .type_simple,
                        => unreachable, // handled above

                        .type_fn,
                        => |@"fn"| {
                            if (target_key != .type_fn) {
                                return .mismatch;
                            }

                            const target_fn = target_key.type_fn;

                            if (@"fn".return_type != target_fn.return_type) {
                                return .mismatch;
                            }

                            if (@"fn".arg_types.len !=
                                target_fn.arg_types.len)
                            {
                                return .mismatch;
                            }

                            for (@"fn".arg_types, 0..) |arg_type, index| {
                                if (arg_type != target_fn.arg_types[index]) {
                                    return .mismatch;
                                }
                            }

                            return .ok;
                        },

                        else => unreachable, // non-type node
                    }
                },
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
        const lhs_type = air_binary.lhs.toType(&self.air, self.intern_pool);

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

        const rhs_type = air_binary.rhs.toType(&self.air, self.intern_pool);

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

    fn addNode(
        self: *Sema,
        key: Air.Key,
        loc: Span(u8),
    ) Allocator.Error!Air.Index {
        try self.air.nodes.append(self.allocator, try self.prepareNode(key));
        try self.air.locs.append(self.allocator, loc);

        return .from(self.air.nodes.len - 1);
    }

    fn addInvalidNode(self: *Sema) Allocator.Error!Air.Index {
        return self.addNode(
            .{ .constant = .invalid },
            .zero,
        );
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
            .neg => |index| .{
                .tag = .neg,
                .a = index.toInt(),
            },
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
            .block => |block| blk: {
                const extra_len = block.exprs.len + block.closed.len + 1;

                try self.air.extra.ensureUnusedCapacity(
                    self.allocator,
                    extra_len,
                );

                self.air.extra.appendAssumeCapacity(@intCast(block.closed.len));
                self.air.extra.appendSliceAssumeCapacity(@ptrCast(block.exprs));
                self.air.extra.appendSliceAssumeCapacity(block.closed);

                break :blk .{
                    .tag = .block,
                    .a = @intCast(block.exprs.len),
                    .b = @intCast(self.air.extra.items.len - extra_len),
                };
            },
            .variable => |variable| .{
                .tag = .variable,
                .a = variable.stack_index,
                .b = variable.type.toInt(),
            },
            .assignment => |assignment| .{
                .tag = .assignment,
                .a = assignment.stack_index,
                .b = assignment.rhs.toInt(),
            },
            .get_upvalue => |get_upvalue| .{
                .tag = .get_upvalue,
                .a = get_upvalue.stack_index,
                .b = get_upvalue.type.toInt(),
            },
            .set_upvalue => |set_upvalue| .{
                .tag = .set_upvalue,
                .a = set_upvalue.stack_index,
                .b = set_upvalue.rhs.toInt(),
            },
            .@"for" => |@"for"| .{
                .tag = .@"for",
                .a = @"for".cond.toInt(),
                .b = @"for".body.toInt(),
            },
            .@"break" => .{ .tag = .@"break" },
            .@"continue" => .{ .tag = .@"continue" },
            .@"return" => |@"return"| .{
                .tag = .@"return",
                .a = @"return".toInt(),
            },
            .call => |call| if (call.args.len < 2) .{
                .tag = .call_simple,
                .a = call.callee.toInt(),
                .b = if (call.args.len == 1)
                    call.args[0].toInt()
                else
                    0,
            } else blk: {
                try self.air.extra.ensureUnusedCapacity(
                    self.allocator,
                    call.args.len + 1,
                );

                self.air.extra.appendAssumeCapacity(@intCast(call.args.len));
                self.air.extra.appendSliceAssumeCapacity(@ptrCast(call.args));

                break :blk .{
                    .tag = .call,
                    .a = call.callee.toInt(),
                    .b = @intCast(
                        self.air.extra.items.len - (call.args.len + 1),
                    ),
                };
            },
            .@"fn" => |@"fn"| blk: {
                try self.air.extra.ensureUnusedCapacity(
                    self.allocator,
                    @"fn".upvalues.len + 4,
                );

                self.air.extra.appendAssumeCapacity(
                    @intCast(@"fn".upvalues.len),
                );
                self.air.extra.appendSliceAssumeCapacity(@"fn".upvalues);
                self.air.extra.appendSliceAssumeCapacity(
                    &[_]u32{
                        @"fn".body.toInt(),
                        @"fn".locals_count,
                        @"fn".fn_type.toInt(),
                    },
                );

                break :blk .{
                    .tag = .@"fn",
                    .a = @"fn".id,
                    .b = @intCast(
                        self.air.extra.items.len - @"fn".upvalues.len - 4,
                    ),
                };
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

    fn addDiag(
        self: *const Sema,
        diag: Diags.Entry.Tag,
        loc: Span(u8),
    ) Allocator.Error!void {
        try self.diags.entries.append(
            self.allocator,
            .{ .tag = diag, .loc = loc },
        );
    }
};
