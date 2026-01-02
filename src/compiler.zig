const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const math = std.math;
const StringHashMap = std.StringHashMap;
const debug = std.debug;
const assert = debug.assert;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const air_mod = @import("air.zig");
const Air = air_mod.Air;
const chunk_mod = @import("compiler/chunk.zig");
const Chunk = chunk_mod.Chunk;
const debug_mod = @import("debug.zig");
const Mode = debug_mod.Mode;
const intern_pool_mod = @import("intern_pool.zig");
const InternPool = intern_pool_mod.InternPool;
const memory_mod = @import("memory.zig");
const Object = memory_mod.Object;
const Memory = memory_mod.Memory;
const Value = memory_mod.Value;
const module_mod = @import("module.zig");
const Module = module_mod.Module;
const OpCode = module_mod.OpCode;

pub const Compiler = struct {
    allocator: Allocator,
    memory: *Memory,
    module: *Module,
    intern_pool: *const InternPool,
    air: *const Air,
    diags: *ArrayListUnmanaged(Diag),
    scratch: *Scratch,
    mode: Mode,

    pub const Error = error{CompileFailure} || Allocator.Error;

    pub const Diag = union(enum) {
        too_many_constants,
        jump_too_big,
    };

    pub const DebugInfo = struct {
        constants: ArrayListUnmanaged(Value.DebugTag),

        pub const empty = DebugInfo{
            .constants = .empty,
        };

        pub fn deinit(self: *DebugInfo, allocator: Allocator) void {
            self.constants.deinit(allocator);
        }
    };

    pub const Scratch = struct {
        code: ArrayListUnmanaged(u8),
        code_bottom: usize,
        then_jumps: ArrayListUnmanaged(usize),
        else_jumps: ArrayListUnmanaged(usize),
        last_jump: usize,
        break_jumps: ArrayListUnmanaged(usize),
        break_never_pops: usize,
        loop_top: usize,

        pub const CodeSnapshot = struct {
            top: usize,
            bottom: usize,
        };

        pub const empty: Scratch = .{
            .code = .empty,
            .code_bottom = 0,
            .then_jumps = .empty,
            .else_jumps = .empty,
            .last_jump = undefined,
            .break_jumps = .empty,
            .break_never_pops = 0,
            .loop_top = 0,
        };

        pub fn deinit(self: *Scratch, allocator: Allocator) void {
            self.code.deinit(allocator);
            self.then_jumps.deinit(allocator);
            self.else_jumps.deinit(allocator);
            self.break_jumps.deinit(allocator);
        }

        pub fn code_snapshot(self: *const Scratch) CodeSnapshot {
            return .{
                .top = self.code.items.len,
                .bottom = self.code_bottom,
            };
        }

        pub fn code_restore(self: *Scratch, snap: CodeSnapshot) void {
            self.code.shrinkRetainingCapacity(snap.top);
            self.code_bottom = snap.bottom;
        }
    };

    const ValueUsage = enum {
        use,
        discard,
    };

    pub fn compile(
        allocator: Allocator,
        memory: *Memory,
        air_fn: Air.Index,
        intern_pool: *const InternPool,
        air: *const Air,
        diags: *ArrayListUnmanaged(Diag),
        scratch: *Scratch,
        mode: Mode,
    ) Error!Module {
        var module: Module = .empty;

        var compiler = Compiler{
            .allocator = allocator,
            .memory = memory,
            .module = &module,
            .intern_pool = intern_pool,
            .air = air,
            .diags = diags,
            .scratch = scratch,
            .mode = mode,
        };

        module.main = try compiler.compileFn(air_fn.toKey(air).@"fn");

        return module;
    }

    fn compileStmt(
        self: *Compiler,
        stmt: Air.Index,
        value_usage: ValueUsage,
    ) Error!void {
        const air_key = stmt.toKey(self.air);

        switch (air_key) {
            .assert => |expr| try self.compileAssertStmt(expr),
            .print => |expr| try self.compilePrintStmt(expr),
            .let => |let| if (let.rhs) |rhs| {
                try self.compileVariableMutation(let.stack_index, rhs);
            },
            .@"fn" => |@"fn"| try self.compileFnStmt(@"fn"),
            else => try self.compileExpr(stmt, value_usage),
        }
    }

    fn compileAssertStmt(self: *Compiler, expr: Air.Index) Error!void {
        try self.compileExpr(expr, .use);
        try self.writeU8(.{.assert});
    }

    fn compilePrintStmt(self: *Compiler, expr: Air.Index) Error!void {
        const expr_type = expr.toType(self.air, self.intern_pool);

        try self.compileExpr(expr, .use);

        switch (expr_type) {
            .type_unit => try self.writeU8(.print_unit),
            .type_bool => try self.writeU8(.print_bool),
            .type_int => try self.writeU8(.print_int),
            .type_float => try self.writeU8(.print_float),
            .type_string => try self.writeU8(.print_object),
            else => {
                switch (expr_type.toKey(self.intern_pool)) {
                    .type_fn => try self.writeU8(.print_fn),
                    else => unreachable, // invalid expr type
                }
            },
        }
    }

    fn compileVariableMutation(
        self: *Compiler,
        index: u32,
        expr_opt: ?Air.Index,
    ) Error!void {
        const index_u8: u8 = @intCast(index);

        if (expr_opt) |expr| {
            try self.compileExpr(expr, .use);
        }

        switch (index_u8) {
            0 => try self.writeU8(.store_local_0),
            1 => try self.writeU8(.store_local_1),
            2 => try self.writeU8(.store_local_2),
            3 => try self.writeU8(.store_local_3),
            4 => try self.writeU8(.store_local_4),
            else => {
                try self.writeU8(.store_local);
                try self.writeU8(index_u8);
            },
        }
    }

    fn compileFnStmt(self: *Compiler, air_fn: Air.Key.Fn) Error!void {
        const fn_index = try self.compileFn(air_fn);

        try self.writeConstant(.{ .@"fn" = fn_index }, .@"fn");
        try self.compileVariableMutation(air_fn.stack_index, null);
    }

    fn compileFn(
        self: *Compiler,
        air_fn: Air.Key.Fn,
    ) Error!u64 {
        const code_scratch_snap = self.scratch.code_snapshot();
        self.scratch.code_bottom = code_scratch_snap.top;
        defer self.scratch.code_restore(code_scratch_snap);

        try self.compileExpr(air_fn.body, .discard);
        try self.writeU8(.{ .constant_int_0, .@"return" });

        return try self.module.writeFn(
            self.allocator,
            air_fn.locals_count,
            self.scratch.code.items[code_scratch_snap.top..],
        );
    }

    fn compileExpr(
        self: *Compiler,
        air_expr: Air.Index,
        value_usage: ValueUsage,
    ) Error!void {
        const key = air_expr.toKey(self.air);

        switch (key) {
            .constant,
            => |constant| try self.compileConstantExpr(constant, value_usage),

            .add,
            .sub,
            .mul,
            .div,
            .concat,
            => try self.compileBinaryExpr(key, value_usage),

            .neg,
            => try self.compileUnaryExpr(key),

            .equal,
            .not_equal,
            .greater_than,
            .greater_equal,
            .less_than,
            .less_equal,
            => |binary| try self.compileComparisonExpr(
                key,
                binary,
                value_usage,
            ),

            .cond,
            => |cond| try self.compileCondExpr(cond, value_usage),

            .block,
            => |block| try self.compileBlockExpr(block, value_usage),

            .variable,
            => |variable| try self.compileVariableExpr(variable, value_usage),

            .assignment,
            => |assignment| try self.compileAssignmentExpr(
                assignment,
                value_usage,
            ),

            .@"for",
            => |@"for"| try self.compileForExpr(@"for", value_usage),

            .@"break",
            => try self.compileBreakExpr(),

            .@"continue",
            => try self.compileContinueExpr(),

            .@"return",
            => |@"return"| try self.compileReturnExpr(@"return"),

            .call,
            => |call| try self.compileCallExpr(call, value_usage),

            else => {
                std.debug.print("not implemented: {any}\n", .{key});
                unreachable;
            },
        }
    }

    fn compileConstantExpr(
        self: *Compiler,
        constant: InternPool.Index,
        value_usage: ValueUsage,
    ) Error!void {
        assert(constant.toKind(self.intern_pool) == .value);

        if (value_usage == .discard) {
            // no side effects are possible, so const exprs can be ignored when
            // discarded
            return;
        }

        switch (constant.toKey(self.intern_pool)) {
            .value_simple => |value_simple| switch (value_simple) {
                .unit => try self.writeU8(.constant_int_0),
                .bool_true => try self.writeU8(.constant_int_1),
                .bool_false => try self.writeU8(.constant_int_0),
            },
            .value_int => |int| switch (int) {
                -1 => try self.writeU8(.constant_int_n1),
                0 => try self.writeU8(.constant_int_0),
                1 => try self.writeU8(.constant_int_1),
                2 => try self.writeU8(.constant_int_2),
                3 => try self.writeU8(.constant_int_3),
                4 => try self.writeU8(.constant_int_4),
                5 => try self.writeU8(.constant_int_5),
                else => try self.writeConstant(.{ .int = int }, .int),
            },
            .value_float => |float| {
                if (float == 0) {
                    try self.writeU8(.constant_float_0);
                } else if (float == 1) {
                    try self.writeU8(.constant_float_1);
                } else if (float == 2) {
                    try self.writeU8(.constant_float_2);
                } else {
                    try self.writeConstant(.{ .float = float }, .float);
                }
            },
            .value_string => |string| {
                const object = try Object.String.create(
                    self.memory,
                    string,
                );
                try self.writeConstant(.{ .object = &object.object }, .object);
            },
            else => unreachable, // invalid constant
        }
    }

    fn compileBinaryExpr(
        self: *Compiler,
        air_key: Air.Key,
        value_usage: ValueUsage,
    ) Error!void {
        const binary = switch (air_key) {
            .add,
            .sub,
            .mul,
            .div,
            .equal,
            .not_equal,
            .greater_than,
            .greater_equal,
            .less_than,
            .less_equal,
            .concat,
            => |binary| binary,

            else => unreachable, // non-binary expr
        };

        _ = self.compileBinaryExprOperands(binary) catch |err| switch (err) {
            error.EncounteredNever => return,
            else => |compiler_err| return compiler_err,
        };

        const is_int =
            binary.lhs.toType(self.air, self.intern_pool) == .type_int;

        const op_code: OpCode = switch (air_key) {
            .add => if (is_int) .add_int else .add_float,
            .sub => if (is_int) .subtract_int else .subtract_float,
            .mul => if (is_int) .multiply_int else .multiply_float,
            .div => if (is_int) .divide_int else .divide_float,
            .concat => .concat,

            else => unreachable, // non-binary expr
        };

        try self.writeU8(op_code);

        if (value_usage == .discard) {
            try self.writeU8(.pop);
        }
    }

    fn compileBinaryExprOperands(
        self: *Compiler,
        binary: Air.Key.Binary,
    ) (Error || error{EncounteredNever})!struct {
        lhs: InternPool.Index,
        rhs: InternPool.Index,
    } {
        const lhs_type = binary.lhs.toType(self.air, self.intern_pool);
        const rhs_type = binary.rhs.toType(self.air, self.intern_pool);

        try self.compileExpr(binary.lhs, .use);

        if (lhs_type == .type_never) {
            // rhs will never get reached, so no need to compile further
            return error.EncounteredNever;
        }

        if (rhs_type == .type_never) {
            // rhs results in never, so popping lhs beforehand
            try self.writeU8(.pop);
        } else {
            self.scratch.break_never_pops += 1;
        }

        try self.compileExpr(binary.rhs, .use);

        if (rhs_type == .type_never) {
            return error.EncounteredNever;
        } else {
            self.scratch.break_never_pops -= 1;
        }

        return .{ .lhs = lhs_type, .rhs = rhs_type };
    }

    fn compileUnaryExpr(
        self: *Compiler,
        air_key: Air.Key,
    ) Error!void {
        const rhs = air_key.neg;
        const rhs_type = rhs.toType(self.air, self.intern_pool);

        try self.compileExpr(rhs, .use);

        if (rhs_type == .type_never) {
            // the expr will never get reached, so no need to compile further
            return;
        }

        const op_code: OpCode = switch (rhs_type) {
            .type_int => .negate_int,
            .type_float => .negate_float,
            .type_bool => .negate_bool,

            else => unreachable, // invalid negation type
        };

        try self.writeU8(op_code);
    }

    fn compileComparisonExpr(
        self: *Compiler,
        air_key: Air.Key,
        binary: Air.Key.Binary,
        value_usage: ValueUsage,
    ) Error!void {
        const else_offset = self.compileComparisonExprJumpOps(
            air_key,
            binary,
        ) catch |err| switch (err) {
            error.EncounteredNever => return,
            else => |compiler_err| return compiler_err,
        };

        try self.writeU8(.constant_int_1);
        const then_offset = try self.writeJump(.jump);
        try self.patchJump(else_offset);
        try self.writeU8(.constant_int_0);
        try self.patchJump(then_offset);

        if (value_usage == .discard) {
            try self.writeU8(.pop);
        }
    }

    fn compileComparisonExprJumpOps(
        self: *Compiler,
        air_key: Air.Key,
        binary: Air.Key.Binary,
    ) (Error || error{EncounteredNever})!usize {
        const types = try self.compileBinaryExprOperands(binary);

        switch (types.lhs) {
            .type_int,
            .type_bool,
            .type_unit,
            => try self.writeU8(.compare_int),

            .type_float,
            => try self.writeU8(.compare_float),

            .type_string,
            => try self.writeU8(.compare_object),

            else => switch (types.lhs.toKey(self.intern_pool)) {
                .type_fn => try self.writeU8(.compare_fn),
                else => unreachable, // unexpected comparison type
            },
        }

        return try self.writeJump(
            invertComparisonJumpOp(airComparisonKeyToJumpOp(air_key)),
        );
    }

    fn compileBlockExpr(
        self: *Compiler,
        block: []const Air.Index,
        value_usage: ValueUsage,
    ) Error!void {
        for (block, 0..) |stmt, index| {
            try self.compileStmt(
                stmt,
                if (value_usage == .use and index == block.len - 1)
                    .use
                else
                    .discard,
            );
        }
    }

    fn compileVariableExpr(
        self: *Compiler,
        variable: Air.Key.Variable,
        value_usage: ValueUsage,
    ) Error!void {
        const index: u8 = @intCast(variable.stack_index);

        switch (index) {
            0 => try self.writeU8(.load_local_0),
            1 => try self.writeU8(.load_local_1),
            2 => try self.writeU8(.load_local_2),
            3 => try self.writeU8(.load_local_3),
            4 => try self.writeU8(.load_local_4),
            else => {
                try self.writeU8(.load_local);
                try self.writeU8(index);
            },
        }

        if (value_usage == .discard) {
            try self.writeU8(.pop);
        }
    }

    fn compileAssignmentExpr(
        self: *Compiler,
        assignment: Air.Key.Assignment,
        value_usage: ValueUsage,
    ) Error!void {
        try self.compileVariableMutation(
            assignment.stack_index,
            assignment.rhs,
        );

        if (value_usage == .use) {
            try self.writeU8(.constant_int_0);
        }
    }

    fn compileCondExpr(
        self: *Compiler,
        cond: Air.Key.Cond,
        value_usage: ValueUsage,
    ) Error!void {
        const then_scratch_top = self.scratch.then_jumps.items.len;
        const else_scratch_top = self.scratch.else_jumps.items.len;
        defer {
            self.scratch.then_jumps.shrinkRetainingCapacity(then_scratch_top);
            self.scratch.else_jumps.shrinkRetainingCapacity(else_scratch_top);
        }

        const is_logical_operator = blk: {
            self.compileCondExprAsLogicalOp(cond) catch |err| switch (err) {
                error.NotLogicalOp => {
                    break :blk false;
                },
                error.EncounteredNever => return,
                else => |compiler_err| return compiler_err,
            };

            break :blk true;
        };

        if (!is_logical_operator) {
            self.compileCondExprBranch(cond.cond, .else_jumps) catch |err|
                switch (err) {
                    error.EncounteredNever => return,
                    else => |compiler_err| return compiler_err,
                };
        }

        try self.patchJumps(self.scratch.then_jumps.items[then_scratch_top..]);

        if (is_logical_operator) {
            try self.writeU8(.constant_int_1);
        } else {
            try self.compileExpr(cond.then_branch, value_usage);
        }

        const out_offset = try self.writeJump(.jump);

        try self.patchJumps(self.scratch.else_jumps.items[else_scratch_top..]);

        if (is_logical_operator) {
            try self.writeU8(.constant_int_0);
        } else {
            try self.compileExpr(cond.else_branch, value_usage);
        }

        try self.patchJump(out_offset);
    }

    fn compileCondExprAsLogicalOp(
        self: *Compiler,
        cond: Air.Key.Cond,
    ) (Error || error{ EncounteredNever, NotLogicalOp })!void {
        const then_key = cond.then_branch.toKey(self.air);
        const else_key = cond.else_branch.toKey(self.air);

        const is_and = else_key == .constant and
            else_key.constant == .value_bool_false;
        const is_or = !is_and and then_key == .constant and
            then_key.constant == .value_bool_true;

        if (!is_and and !is_or) {
            // normal if expr
            return error.NotLogicalOp;
        }

        const lhs = cond.cond;
        const rhs = if (is_and) cond.then_branch else cond.else_branch;

        {
            const then_scratch_top = self.scratch.then_jumps.items.len;
            const else_scratch_top = self.scratch.else_jumps.items.len;
            defer if (is_and) {
                self.scratch.then_jumps.shrinkRetainingCapacity(
                    then_scratch_top,
                );
            } else if (is_or) {
                self.scratch.else_jumps.shrinkRetainingCapacity(
                    else_scratch_top,
                );
            };

            try self.compileCondExprBranch(
                lhs,
                if (is_and) .else_jumps else .then_jumps,
            );

            if (is_or) {
                // invert last jump
                const byte = self.scratch.code.items[self.scratch.last_jump];

                self.scratch.code.items[self.scratch.last_jump] =
                    @intFromEnum(invertComparisonJumpOp(@enumFromInt(byte)));

                // if the last jump is in `else`, then move it to `then`
                const jumps = self.scratch.else_jumps.items[else_scratch_top..];

                if (jumps.len > 0 and
                    jumps[jumps.len - 1] == self.scratch.last_jump + 1)
                {
                    const top = self.scratch.else_jumps.pop() orelse
                        unreachable;

                    try self.scratch.then_jumps.append(self.allocator, top);
                }

                try self.patchJumps(jumps);
            } else {
                const jumps = self.scratch.then_jumps.items[then_scratch_top..];
                try self.patchJumps(jumps);
            }
        }

        // not always evaluated, therefore can't ignore when never
        try self.compileCondExprBranch(rhs, .else_jumps);
    }

    fn compileCondExprBranch(
        self: *Compiler,
        air_expr: Air.Index,
        jumps_scratch_kind: enum { then_jumps, else_jumps },
    ) (Error || error{EncounteredNever})!void {
        const expr_key = air_expr.toKey(self.air);
        const expr_type = air_expr.toType(self.air, self.intern_pool);
        const jumps = switch (jumps_scratch_kind) {
            .then_jumps => &self.scratch.then_jumps,
            .else_jumps => &self.scratch.else_jumps,
        };

        swt: switch (airKeyToBranchingKind(expr_key)) {
            .comparison => |binary| {
                const offset = try self.compileComparisonExprJumpOps(
                    expr_key,
                    binary,
                );

                try jumps.append(self.allocator, offset);
            },
            .cond => |nested_cond| {
                self.compileCondExprAsLogicalOp(nested_cond) catch |err|
                    switch (err) {
                        error.NotLogicalOp => continue :swt .non_branching,
                        else => |compiler_err| return compiler_err,
                    };
            },
            .non_branching => {
                try self.compileExpr(air_expr, .use);

                if (expr_type == .type_never) {
                    return error.EncounteredNever;
                }

                const offset = try self.writeJump(.if_false);
                try jumps.append(self.allocator, offset);
            },
        }
    }

    fn compileForExpr(
        self: *Compiler,
        @"for": Air.Key.For,
        value_usage: ValueUsage,
    ) Error!void {
        const then_scratch_top = self.scratch.then_jumps.items.len;
        const else_scratch_top = self.scratch.else_jumps.items.len;
        const break_scratch_top = self.scratch.break_jumps.items.len;
        const last_break_pops = self.scratch.break_never_pops;
        const last_loop_top = self.scratch.loop_top;
        defer {
            self.scratch.then_jumps.shrinkRetainingCapacity(then_scratch_top);
            self.scratch.else_jumps.shrinkRetainingCapacity(else_scratch_top);
            self.scratch.break_jumps.shrinkRetainingCapacity(break_scratch_top);
            self.scratch.break_never_pops = last_break_pops;
            self.scratch.loop_top = last_loop_top;
        }

        self.scratch.loop_top = self.scratch.code.items.len;

        self.compileCondExprBranch(@"for".cond, .else_jumps) catch |err|
            switch (err) {
                error.EncounteredNever => return,
                else => |compare_err| return compare_err,
            };

        try self.patchJumps(self.scratch.then_jumps.items[then_scratch_top..]);
        try self.compileExpr(@"for".body, .discard);
        try self.writeNegativeJump(self.scratch.loop_top);
        try self.patchJumps(self.scratch.else_jumps.items[else_scratch_top..]);
        try self.patchJumps(
            self.scratch.break_jumps.items[break_scratch_top..],
        );

        if (value_usage == .use) {
            try self.writeU8(.constant_int_0);
        }
    }

    fn compileBreakExpr(self: *Compiler) Error!void {
        for (0..self.scratch.break_never_pops) |_| {
            try self.writeU8(.pop);
        }

        const offset = try self.writeJump(.jump);
        try self.scratch.break_jumps.append(self.allocator, offset);
    }

    fn compileContinueExpr(self: *Compiler) Error!void {
        for (0..self.scratch.break_never_pops) |_| {
            try self.writeU8(.pop);
        }

        try self.writeNegativeJump(self.scratch.loop_top);
    }

    fn compileReturnExpr(
        self: *Compiler,
        @"return": Air.Index,
    ) Error!void {
        try self.compileExpr(@"return", .use);
        try self.writeU8(.@"return");
    }

    fn compileCallExpr(
        self: *Compiler,
        call: Air.Key.Call,
        value_usage: ValueUsage,
    ) Error!void {
        // todo: add a test for this (break pops)
        try self.compileExpr(call.callee, .use);

        if (call.callee.toType(self.air, self.intern_pool) == .type_never) {
            return;
        }

        self.scratch.break_never_pops += 1;

        for (call.args) |arg| {
            try self.compileExpr(arg, .use);

            if (arg.toType(self.air, self.intern_pool) == .type_never) {
                return;
            }

            self.scratch.break_never_pops += 1;
        }

        self.scratch.break_never_pops -= call.args.len + 1;

        try self.writeU8(.call);

        if (value_usage == .discard) {
            try self.writeU8(.pop);
        }
    }

    fn writeU8(self: *Compiler, data: anytype) Allocator.Error!void {
        const DataType = @TypeOf(data);
        const data_type_info = @typeInfo(DataType);
        const bytes = if (data_type_info == .@"struct" and
            data_type_info.@"struct".is_tuple)
            data
        else
            .{data};

        inline for (bytes) |byte| {
            const resolved_byte = resolveU8(byte);
            try self.scratch.code.append(self.allocator, resolved_byte);
        }
    }

    fn writeU16(self: *Compiler, data: u16) Allocator.Error!void {
        const bytes: [2]u8 = @bitCast(data);

        try self.writeU8(bytes[0]);
        try self.writeU8(bytes[1]);
    }

    fn resolveU8(byte: anytype) u8 {
        const ByteType = @TypeOf(byte);

        switch (ByteType) {
            @TypeOf(.enum_literal), OpCode => {
                if (ByteType == @TypeOf(.enum_literal) and
                    !@hasField(OpCode, @tagName(byte)))
                {
                    @compileError("expected valid OpCode");
                }

                return @intFromEnum(@as(OpCode, byte));
            },
            comptime_int, u8 => {
                return byte;
            },
            else => @compileError(
                "expected byte to be of type OpCode or u8, found " ++
                    @typeName(ByteType),
            ),
        }
    }

    fn writeConstant(
        self: *Compiler,
        value: Value,
        debug_tag: Value.DebugTag,
    ) Error!void {
        const result =
            if (self.mode == .debug)
                self.module.writeConstant(
                    .debug,
                    self.allocator,
                    .from(value, debug_tag),
                )
            else
                self.module.writeConstant(
                    .release,
                    self.allocator,
                    value,
                );

        const index = if (result) |index|
            index
        else |err| switch (err) {
            error.TooManyConstants => {
                return self.compilerError(.too_many_constants);
            },
            else => |alloc_err| return alloc_err,
        };

        try self.writeU8(.{ .constant, index });
    }

    fn writeJump(
        self: *Compiler,
        op_code: OpCode,
    ) Error!usize {
        try self.writeU8(op_code);
        try self.writeU16(0);

        self.scratch.last_jump = self.scratch.code.items.len - 3;

        return self.scratch.code.items.len - 2;
    }

    fn writeNegativeJump(
        self: *Compiler,
        offset: usize,
    ) Error!void {
        const jump = (self.scratch.code.items.len - self.scratch.code_bottom) -
            offset + 3;

        if (jump > math.maxInt(u16)) {
            return self.compilerError(.jump_too_big);
        }

        try self.writeU8(.negative_jump);
        try self.writeU16(@intCast(jump));
    }

    fn patchJump(
        self: *Compiler,
        offset: usize,
    ) Error!void {
        const jump = (self.scratch.code.items.len - self.scratch.code_bottom) -
            offset - 2;

        if (jump > math.maxInt(u16)) {
            return self.compilerError(.jump_too_big);
        }

        const jump_bytes: [2]u8 = @bitCast(@as(u16, @intCast(jump)));

        self.scratch.code.items[offset] = jump_bytes[0];
        self.scratch.code.items[offset + 1] = jump_bytes[1];
    }

    fn patchJumps(
        self: *Compiler,
        jumps: []const usize,
    ) Error!void {
        for (jumps) |jump| {
            try self.patchJump(jump);
        }
    }

    fn compilerError(self: *Compiler, diag: Diag) Error {
        try self.diags.append(self.allocator, diag);
        return error.CompileFailure;
    }

    fn airKeyToBranchingKind(
        air_key: Air.Key,
    ) union(enum) {
        comparison: Air.Key.Binary,
        cond: Air.Key.Cond,
        non_branching,
    } {
        return switch (air_key) {
            .equal,
            .not_equal,
            .greater_than,
            .greater_equal,
            .less_than,
            .less_equal,
            => |binary| .{ .comparison = binary },

            .cond,
            => |cond| .{ .cond = cond },

            .constant,
            .add,
            .sub,
            .mul,
            .div,
            .concat,
            .neg,
            .block,
            .variable,
            .assignment,
            .@"for",
            .@"break",
            .@"continue",
            .@"return",
            .call,
            .assert,
            .print,
            .let,
            .@"fn",
            => .non_branching,
        };
    }

    fn invertComparisonJumpOp(op_code: OpCode) OpCode {
        return switch (op_code) {
            .if_equal => .if_not_equal,
            .if_not_equal => .if_equal,
            .if_greater => .if_less_equal,
            .if_greater_equal => .if_less,
            .if_less => .if_greater_equal,
            .if_less_equal => .if_greater,
            .if_true => .if_false,
            .if_false => .if_true,
            else => unreachable, // non-comparison jump op-code
        };
    }

    fn airComparisonKeyToJumpOp(air_key: Air.Key) OpCode {
        return switch (air_key) {
            .equal => .if_equal,
            .not_equal => .if_not_equal,
            .greater_than => .if_greater,
            .greater_equal => .if_greater_equal,
            .less_than => .if_less,
            .less_equal => .if_less_equal,
            else => unreachable, // non-comparison air key
        };
    }
};
