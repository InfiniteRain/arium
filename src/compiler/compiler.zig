const std = @import("std");
const shared = @import("shared");
const managed_memory_mod = @import("../state/managed_memory.zig");
const chunk_mod = @import("chunk.zig");
const sema_ast_mod = @import("../sema/sema_ast.zig");
const stack_mod = @import("../state/stack.zig");
const value_mod = @import("../state/value.zig");
const obj_mod = @import("../state/obj.zig");
const hash_table_mod = @import("../state/hash_table.zig");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const mem = std.mem;
const Allocator = mem.Allocator;
const BoundedArray = std.BoundedArray;
const assert = std.debug.assert;
const meta = shared.meta;
const SharedDiags = shared.Diags;
const ManagedMemory = managed_memory_mod.ManagedMemory;
const VmState = managed_memory_mod.VmState;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const Stack = stack_mod.Stack;
const Value = value_mod.Value;
const Obj = obj_mod.Obj;
const HashTable = hash_table_mod.HashTable;
const Position = tokenizer_mod.Position;

pub const Compiler = struct {
    const Self = @This();

    pub const Error = error{
        OutOfMemory,
        CompileFailure,
    };

    pub const DiagEntry = struct {
        pub const Kind = enum {
            too_many_constants,
            too_many_branch_jumps,
            jump_too_big,
        };

        kind: Kind,
        position: Position,
    };

    pub const Diags = SharedDiags(DiagEntry);

    const ExprContext = struct {
        const JumpInfo = struct {
            index: usize,
            is_inverted: bool,
        };

        const BranchOffsets = BoundedArray(usize, 128);

        const LogicalOperation = enum {
            or_,
            and_,
        };

        is_child_to_logical: bool = false,
        current_logical: ?LogicalOperation = null,
        prev_logical: ?LogicalOperation = null,
        last_jump: *?JumpInfo,
        else_branch_offsets: *BranchOffsets,
        then_branch_offsets: *BranchOffsets,
    };

    vm_state: *VmState,
    allocator: Allocator,
    chunk: Chunk,
    diags: ?*Diags,

    pub fn compile(
        memory: *ManagedMemory,
        block: *const SemaExpr,
        diags: ?*Diags,
    ) Error!void {
        const allocator = memory.allocator();

        var vm_state: VmState = undefined;

        vm_state.objs = null;
        vm_state.strings = try HashTable.init(allocator);
        vm_state.stack = try Stack.init(allocator);

        var compiler = Self{
            .vm_state = &vm_state,
            .allocator = allocator,
            .chunk = try Chunk.init(allocator),
            .diags = diags,
        };

        errdefer {
            compiler.chunk.deinit();
            vm_state.stack.deinit();
            vm_state.strings.deinit();

            var current = vm_state.objs;

            while (current) |obj| {
                const next = obj.next;
                obj.destroy(allocator);
                current = next;
            }
        }

        try compiler.compileExpr(block, null);
        try compiler.eraseOrPopEvalValue(block.position);
        try compiler.chunk.writeU8(.return_, .{ .line = 0, .column = 0 });

        vm_state.chunk = compiler.chunk;
        vm_state.ip = @ptrCast(&compiler.chunk.code.items[0]);

        memory.vm_state = vm_state;
    }

    fn compileStmt(
        self: *Self,
        stmt: *const SemaStmt,
        is_last_statement: bool,
    ) Error!void {
        switch (stmt.kind) {
            .assert => |*assert_stmt| try self.compileAssertStmt(assert_stmt, stmt.position),
            .print => |*print| try self.compilePrintStmt(print, stmt.position),
            .expr => |*expr| try self.compileExprStmt(expr, is_last_statement, stmt.position),
            .let => |*let| try self.compileVariableMutation(let.index, let.expr, stmt.position),
        }
    }

    fn compileAssertStmt(
        self: *Self,
        assert_stmt: *const SemaStmt.Kind.Assert,
        position: Position,
    ) Error!void {
        try self.compileExpr(assert_stmt.expr, null);
        try self.chunk.writeU8(.assert, position);
    }

    fn compilePrintStmt(
        self: *Self,
        print: *const SemaStmt.Kind.Print,
        position: Position,
    ) Error!void {
        try self.compileExpr(print.expr, null);
        try self.chunk.writeU8(.print, position);
    }

    fn compileExprStmt(
        self: *Self,
        expr: *const SemaStmt.Kind.Expr,
        is_last_statement: bool,
        position: Position,
    ) Error!void {
        try self.compileExpr(expr.expr, null);

        if (!is_last_statement) {
            try self.eraseOrPopEvalValue(position);
        }
    }

    fn compileVariableMutation(
        self: *Self,
        index: usize,
        expr: *const SemaExpr,
        position: Position,
    ) Error!void {
        const index_u8: u8 = @intCast(index);

        try self.compileExpr(expr, null);

        switch (index_u8) {
            0 => try self.chunk.writeU8(.store_local_0, position),
            1 => try self.chunk.writeU8(.store_local_1, position),
            2 => try self.chunk.writeU8(.store_local_2, position),
            3 => try self.chunk.writeU8(.store_local_3, position),
            4 => try self.chunk.writeU8(.store_local_4, position),
            else => {
                try self.chunk.writeU8(.store_local, position);
                try self.chunk.writeU8(index_u8, position);
            },
        }
    }

    fn compileExpr(
        self: *Self,
        expr: *const SemaExpr,
        ctx_opt: ?ExprContext,
    ) Error!void {
        var is_branching = false;
        var last_jump: ?ExprContext.JumpInfo = null;
        var else_branch_offsets: ExprContext.BranchOffsets = undefined;
        var then_branch_offsets: ExprContext.BranchOffsets = undefined;
        const ctx = if (ctx_opt) |passed_ctx| passed_ctx else blk: {
            else_branch_offsets = ExprContext.BranchOffsets.init(0) catch unreachable;
            then_branch_offsets = ExprContext.BranchOffsets.init(0) catch unreachable;

            break :blk ExprContext{
                .last_jump = &last_jump,
                .else_branch_offsets = &else_branch_offsets,
                .then_branch_offsets = &then_branch_offsets,
            };
        };

        switch (expr.kind) {
            .literal => |*literal| try self.compileLiteralExpr(literal, expr.position),
            .binary => |*binary| {
                is_branching = try self.compileBinaryExpr(binary, ctx, expr.position);
            },
            .unary => |*unary| try self.compileUnaryExpr(unary, expr.position),
            .block => |*block| try self.compileBlockExpr(block),
            .variable => |*variable| try self.compileVariableExpr(variable, expr.position),
            .assignment => |*assignment| try self.compileVariableMutation(
                assignment.index,
                assignment.right,
                expr.position,
            ),
        }

        if (!is_branching and ctx.is_child_to_logical) {
            const cmp_op_code = .compare_bool;
            const if_op_code = .if_not_equal;

            try self.chunk.writeU8(.constant_bool_true, expr.position);
            const offset, const is_inverted = try self.branchOff(
                cmp_op_code,
                if_op_code,
                expr.position,
                ctx,
            );

            if (is_inverted) {
                ctx.then_branch_offsets.append(offset) catch
                    return self.tooManyBranchJumps(offset);
            } else {
                ctx.else_branch_offsets.append(offset) catch
                    return self.tooManyBranchJumps(offset);
            }
        }
    }

    fn compileLiteralExpr(
        self: *Self,
        literal: *const SemaExpr.Kind.Literal,
        position: Position,
    ) Error!void {
        switch (literal.*) {
            .unit => try self.chunk.writeU8(.constant_unit, position),
            .int => |int| switch (int) {
                -1 => try self.chunk.writeU8(.constant_int_n1, position),
                0 => try self.chunk.writeU8(.constant_int_0, position),
                1 => try self.chunk.writeU8(.constant_int_1, position),
                2 => try self.chunk.writeU8(.constant_int_2, position),
                3 => try self.chunk.writeU8(.constant_int_3, position),
                4 => try self.chunk.writeU8(.constant_int_4, position),
                5 => try self.chunk.writeU8(.constant_int_5, position),
                else => try self.writeConstant(.{ .int = int }, position),
            },
            .float => |float| {
                if (float == 0) {
                    try self.chunk.writeU8(.constant_float_0, position);
                } else if (float == 1) {
                    try self.chunk.writeU8(.constant_float_1, position);
                } else if (float == 2) {
                    try self.chunk.writeU8(.constant_float_2, position);
                } else {
                    try self.writeConstant(.{ .float = float }, position);
                }
            },
            .bool => |bool_| switch (bool_) {
                true => try self.chunk.writeU8(.constant_bool_true, position),
                false => try self.chunk.writeU8(.constant_bool_false, position),
            },
            .string => |string| try self.writeConstant(
                .{
                    .obj = &(try Obj.String.createFromCopied(
                        self.allocator,
                        self.vm_state,
                        string,
                    )).obj,
                },
                position,
            ),
            .invalid => @panic("invalid expr"),
        }
    }

    fn compileBinaryExpr(
        self: *Self,
        binary: *const SemaExpr.Kind.Binary,
        ctx: ExprContext,
        position: Position,
    ) Error!bool {
        switch (binary.kind) {
            .add_int,
            .add_float,
            .subtract_int,
            .subtract_float,
            .multiply_int,
            .multiply_float,
            .divide_int,
            .divide_float,
            .concat,
            => {
                try self.compileArithmeticBinaryExpr(binary, position, ctx);
                return false;
            },

            .equal_int,
            .equal_float,
            .equal_bool,
            .equal_obj,
            .not_equal_int,
            .not_equal_float,
            .not_equal_bool,
            .not_equal_obj,
            .greater_int,
            .greater_float,
            .greater_equal_int,
            .greater_equal_float,
            .less_int,
            .less_float,
            .less_equal_int,
            .less_equal_float,
            => {
                try self.compileComparisonBinaryExpr(binary, position, ctx);
                return true;
            },

            .or_,
            .and_,
            => {
                try self.compileLogicalBinaryExpr(binary, position, ctx);
                return true;
            },
        }
    }

    fn compileComparisonBinaryExpr(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprContext,
    ) Error!void {
        try self.compileExpr(expr.left, null);
        try self.compileExpr(expr.right, null);

        const cmp_op_code, const if_op_code = getComparisonOpCodes(expr);
        const offset, const is_inverted = try self.branchOff(
            cmp_op_code,
            if_op_code,
            position,
            ctx,
        );

        if (!ctx.is_child_to_logical or ctx.current_logical == null) {
            try self.chunk.writeU8(.constant_bool_true, position);
            const then_offset = try self.chunk.writeJump(.jump, position);
            try self.patchJump(offset);

            try self.chunk.writeU8(.constant_bool_false, position);
            try self.patchJump(then_offset);
        } else if (is_inverted) {
            ctx.then_branch_offsets.append(offset) catch
                return self.tooManyBranchJumps(offset);
        } else {
            ctx.else_branch_offsets.append(offset) catch
                return self.tooManyBranchJumps(offset);
        }
    }

    fn compileLogicalBinaryExpr(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprContext,
    ) Error!void {
        const prev_logical = ctx.current_logical;
        const current_logical: ExprContext.LogicalOperation =
            if (expr.kind == .or_) .or_ else if (expr.kind == .and_) .and_ else unreachable;

        var new_else_branch_offsets: ExprContext.BranchOffsets = undefined;
        var new_then_branch_offsets: ExprContext.BranchOffsets = undefined;
        var else_branch_offsets = ctx.else_branch_offsets;
        var then_branch_offsets = ctx.then_branch_offsets;

        if (expr.kind == .or_) {
            new_else_branch_offsets = ExprContext.BranchOffsets.init(0) catch unreachable;
            else_branch_offsets = &new_else_branch_offsets;
        } else {
            new_then_branch_offsets = ExprContext.BranchOffsets.init(0) catch unreachable;
            then_branch_offsets = &new_then_branch_offsets;
        }

        const left_ctx = meta.spread(ctx, .{
            .is_child_to_logical = true,
            .prev_logical = prev_logical,
            .current_logical = current_logical,
            .else_branch_offsets = else_branch_offsets,
            .then_branch_offsets = then_branch_offsets,
        });

        try self.compileExpr(expr.left, left_ctx);

        if (expr.kind == .or_) {
            if (ctx.last_jump.* != null and !ctx.last_jump.*.?.is_inverted) {
                try self.invertLastBranchJump(left_ctx);
            }

            try self.patchJumps(else_branch_offsets);
        } else {
            if (ctx.last_jump.* != null and ctx.last_jump.*.?.is_inverted) {
                try self.invertLastBranchJump(left_ctx);
            }

            try self.patchJumps(then_branch_offsets);
        }

        const right_ctx = meta.spread(left_ctx, .{
            .else_branch_offsets = ctx.else_branch_offsets,
            .then_branch_offsets = ctx.then_branch_offsets,
        });

        try self.compileExpr(expr.right, right_ctx);

        if (prev_logical == null) {
            // if the deepest branch condition was inverted, invert it back
            // as it is the end of the logical expression
            if (ctx.last_jump.* != null and ctx.last_jump.*.?.is_inverted) {
                try self.invertLastBranchJump(ctx);
            }

            try self.patchJumps(ctx.then_branch_offsets);

            try self.chunk.writeU8(.constant_bool_true, position);
            const then_offset = try self.chunk.writeJump(.jump, position);

            try self.patchJumps(ctx.else_branch_offsets);

            try self.chunk.writeU8(.constant_bool_false, position);
            try self.patchJump(then_offset);
        }
    }

    fn compileArithmeticBinaryExpr(
        self: *Self,
        expr: *const SemaExpr.Kind.Binary,
        position: Position,
        ctx: ExprContext,
    ) Error!void {
        const new_ctx = meta.spread(ctx, .{
            .is_child_to_logical = false,
            .current_logical = ctx.current_logical,
            .prev_logical = ctx.prev_logical,
            .else_branch_offsets = ctx.else_branch_offsets,
            .then_branch_offsets = ctx.then_branch_offsets,
        });

        try self.compileExpr(expr.left, new_ctx);
        try self.compileExpr(expr.right, new_ctx);

        const op_code: OpCode = switch (expr.kind) {
            .add_int => .add_int,
            .add_float => .add_float,
            .subtract_int => .subtract_int,
            .subtract_float => .subtract_float,
            .multiply_int => .multiply_int,
            .multiply_float => .multiply_float,
            .divide_int => .divide_int,
            .divide_float => .divide_float,
            .concat => .concat,
            else => unreachable,
        };

        try self.chunk.writeU8(op_code, position);
    }

    fn compileUnaryExpr(
        self: *Self,
        unary: *const SemaExpr.Kind.Unary,
        position: Position,
    ) Error!void {
        try self.compileExpr(unary.right, null);
        try self.chunk.writeU8(
            switch (unary.kind) {
                .negate_bool => OpCode.negate_bool,
                .negate_int => OpCode.negate_int,
                .negate_float => OpCode.negate_float,
            },
            position,
        );
    }

    fn compileBlockExpr(
        self: *Self,
        block: *const SemaExpr.Kind.Block,
    ) Error!void {
        for (block.stmts.items, 0..) |child_stmt, index| {
            try self.compileStmt(
                child_stmt,
                index == block.stmts.items.len - 1,
            );
        }
    }

    fn compileVariableExpr(
        self: *Self,
        variable: *const SemaExpr.Kind.Variable,
        position: Position,
    ) Error!void {
        const index: u8 = @intCast(variable.index);

        switch (index) {
            0 => try self.chunk.writeU8(.load_local_0, position),
            1 => try self.chunk.writeU8(.load_local_1, position),
            2 => try self.chunk.writeU8(.load_local_2, position),
            3 => try self.chunk.writeU8(.load_local_3, position),
            4 => try self.chunk.writeU8(.load_local_4, position),
            else => {
                try self.chunk.writeU8(.load_local, position);
                try self.chunk.writeU8(index, position);
            },
        }
    }

    fn branchOff(
        self: *Self,
        cmp_op_code: OpCode,
        if_op_code: OpCode,
        position: Position,
        ctx: ExprContext,
    ) Error!struct { usize, bool } {
        const invert = ctx.current_logical == .or_ and ctx.is_child_to_logical;
        const final_if_op_code = if (invert)
            invertComparisonOpCode(if_op_code)
        else
            if_op_code;
        try self.chunk.writeU8(cmp_op_code, position);
        const offset = try self.chunk.writeJump(final_if_op_code, position);
        ctx.last_jump.* = .{ .index = offset - 1, .is_inverted = invert };

        return .{ offset, invert };
    }

    fn invertComparisonOpCode(op_code: OpCode) OpCode {
        return switch (op_code) {
            .if_equal => .if_not_equal,
            .if_not_equal => .if_equal,
            .if_greater => .if_less_equal,
            .if_greater_equal => .if_less,
            .if_less => .if_greater_equal,
            .if_less_equal => .if_greater,
            else => @panic("non-comparison opcode provided"),
        };
    }

    fn getComparisonOpCodes(
        expr: *const SemaExpr.Kind.Binary,
    ) struct { OpCode, OpCode } {
        const cmp_op_code: OpCode, const if_op_code: OpCode = switch (expr.kind) {
            .equal_int => .{ .compare_int, .if_not_equal },
            .equal_float => .{ .compare_float, .if_not_equal },
            .equal_bool => .{ .compare_bool, .if_not_equal },
            .equal_obj => .{ .compare_obj, .if_not_equal },

            .not_equal_int => .{ .compare_int, .if_equal },
            .not_equal_float => .{ .compare_float, .if_equal },
            .not_equal_bool => .{ .compare_bool, .if_equal },
            .not_equal_obj => .{ .compare_obj, .if_equal },

            .greater_int => .{ .compare_int, .if_less_equal },
            .greater_float => .{ .compare_float, .if_less_equal },

            .greater_equal_int => .{ .compare_int, .if_less },
            .greater_equal_float => .{ .compare_float, .if_less },

            .less_int => .{ .compare_int, .if_greater_equal },
            .less_float => .{ .compare_float, .if_greater_equal },

            .less_equal_int => .{ .compare_int, .if_greater },
            .less_equal_float => .{ .compare_float, .if_greater },

            else => unreachable,
        };

        return .{ cmp_op_code, if_op_code };
    }

    fn invertLastBranchJump(
        self: *Self,
        ctx: ExprContext,
    ) Error!void {
        const last_jump = ctx.last_jump.*.?;
        const if_op_code: OpCode = @enumFromInt(self.chunk.code.items[last_jump.index]);

        try self.chunk.updateU8(invertComparisonOpCode(if_op_code), last_jump.index);

        if (last_jump.is_inverted) {
            const offset = ctx.then_branch_offsets.pop();
            ctx.else_branch_offsets.append(offset) catch
                return self.tooManyBranchJumps(offset);
        } else {
            const offset = ctx.else_branch_offsets.pop();
            ctx.then_branch_offsets.append(offset) catch
                return self.tooManyBranchJumps(offset);
        }
    }

    fn patchJumps(self: *Self, jumps: *ExprContext.BranchOffsets) Error!void {
        while (jumps.popOrNull()) |jump| {
            try self.patchJump(jump);
        }
    }

    fn patchJump(self: *Self, offset: usize) Error!void {
        self.chunk.patchJump(offset) catch |err| switch (err) {
            error.JumpTooBig => {
                try self.addDiag(.jump_too_big, self.chunk.positions.items[offset]);
                return error.CompileFailure;
            },
        };
    }

    fn writeConstant(self: *Self, value: Value, position: Position) Error!void {
        self.chunk.writeConstant(value, position) catch |err| switch (err) {
            error.TooManyConstants => {
                try self.addDiag(.too_many_constants, position);
                return error.CompileFailure;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
    }

    fn eraseOrPopEvalValue(self: *Self, position: Position) Error!void {
        switch (self.chunk.current_op_code) {
            .constant,
            .constant_unit,
            .constant_bool_false,
            .constant_bool_true,
            .constant_int_n1,
            .constant_int_0,
            .constant_int_1,
            .constant_int_2,
            .constant_int_3,
            .constant_int_4,
            .constant_int_5,
            .constant_float_0,
            .constant_float_1,
            .constant_float_2,
            => {
                try self.chunk.eraseLast();
            },

            else => {
                try self.chunk.writeU8(.pop, position);
            },
        }
    }

    fn tooManyBranchJumps(self: *Self, offset: usize) Error {
        try self.addDiag(.too_many_branch_jumps, self.chunk.positions.items[offset]);
        return error.CompileFailure;
    }

    fn addDiag(
        self: *Self,
        diag_kind: DiagEntry.Kind,
        position: Position,
    ) Error!void {
        if (self.diags) |diags| {
            // in case of ever needing to alloc something in here, make sure to
            // use diags.allocator instead of self.allocator. this is
            // necessary for lang-tests where a new allocator is created for
            // each test to detect memory leaks. that allocator then gets
            // deinited while diags are owned by the tests.
            try diags.add(.{
                .kind = diag_kind,
                .position = position,
            });
        }
    }
};
