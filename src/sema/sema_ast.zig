const std = @import("std");
const tokenizer_mod = @import("../parser/tokenizer.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const meta = std.meta;
const Position = tokenizer_mod.Position;

pub const SemaExpr = struct {
    const Self = @This();

    const Error = error{OutOfMemory};

    pub const Kind = union(enum) {
        pub const Literal = union(enum) {
            unit,
            int: i64,
            float: f64,
            bool: bool,
            string: []const u8,

            // Takes ownership of heap data (string).
            pub fn create(
                allocator: Allocator,
                literal: Literal,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{ .literal = literal },
                    switch (literal) {
                        .unit => .unit,
                        .int => .int,
                        .float => .float,
                        .bool => .bool,
                        .string => .string,
                    },
                    evals,
                    position,
                );
            }
        };

        pub const Binary = struct {
            pub const Kind = enum {
                add_int,
                add_float,
                subtract_int,
                subtract_float,
                multiply_int,
                multiply_float,
                divide_int,
                divide_float,

                concat,

                equal_int,
                equal_float,
                equal_bool,
                equal_obj,
                not_equal_int,
                not_equal_float,
                not_equal_bool,
                not_equal_obj,

                greater_int,
                greater_float,
                greater_equal_int,
                greater_equal_float,
                less_int,
                less_float,
                less_equal_int,
                less_equal_float,

                @"or",
                @"and",
            };

            kind: Binary.Kind,
            left: *Self,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                kind: Binary.Kind,
                left: *Self,
                right: *Self,
                sema_type: SemaType,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .binary = .{
                            .kind = kind,
                            .left = left,
                            .right = right,
                        },
                    },
                    sema_type,
                    evals,
                    position,
                );
            }
        };

        pub const Unary = struct {
            pub const Kind = enum {
                negate_bool,
                negate_int,
                negate_float,
            };

            kind: Unary.Kind,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                kind: Unary.Kind,
                right: *Self,
                sema_type: SemaType,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    sema_type,
                    evals,
                    position,
                );
            }
        };

        pub const Block = struct {
            stmts: ArrayList(*SemaStmt),

            pub fn create(
                allocator: Allocator,
                stmts: ArrayList(*SemaStmt),
                sema_type: SemaType,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .block = .{
                            .stmts = stmts,
                        },
                    },
                    sema_type,
                    evals,
                    position,
                );
            }
        };

        pub const Variable = struct {
            index: usize,

            pub fn create(
                allocator: Allocator,
                index: usize,
                sema_type: SemaType,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .variable = .{
                            .index = index,
                        },
                    },
                    sema_type,
                    evals,
                    position,
                );
            }
        };

        pub const Assignment = struct {
            index: usize,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                index: usize,
                right: *Self,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .assignment = .{
                            .index = index,
                            .right = right,
                        },
                    },
                    .unit,
                    evals,
                    position,
                );
            }
        };

        pub const If = struct {
            condition: *Self,
            then_block: *Self,
            else_block: ?*Self,

            pub fn create(
                allocator: Allocator,
                condition: *Self,
                then_block: *Self,
                else_block: ?*Self,
                sema_type: SemaType,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .@"if" = .{
                            .condition = condition,
                            .then_block = then_block,
                            .else_block = else_block,
                        },
                    },
                    sema_type,
                    evals,
                    position,
                );
            }
        };

        pub const For = struct {
            condition: *Self,
            body_block: *Self,

            pub fn create(
                allocator: Allocator,
                condition: *Self,
                body_block: *Self,
                evals: bool,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .@"for" = .{
                            .condition = condition,
                            .body_block = body_block,
                        },
                    },
                    .unit,
                    evals,
                    position,
                );
            }
        };

        literal: Literal,
        binary: Binary,
        unary: Unary,
        block: Block,
        variable: Variable,
        assignment: Assignment,
        @"if": If,
        @"for": For,
    };

    fn createExpr(
        allocator: Allocator,
        kind: Kind,
        sema_type: SemaType,
        evals: bool,
        position: Position,
    ) Error!*Self {
        const expr = try allocator.create(Self);

        expr.* = .{
            .kind = kind,
            .sema_type = sema_type,
            .evals = evals,
            .position = position,
        };

        return expr;
    }

    kind: Kind,
    sema_type: SemaType,
    evals: bool,
    position: Position,
};

pub const SemaStmt = struct {
    const Self = @This();

    const Error = error{OutOfMemory};

    pub const Kind = union(enum) {
        pub const Assert = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) Error!*Self {
                return try createStmt(
                    allocator,
                    .{
                        .assert = .{
                            .expr = expr,
                        },
                    },
                    position,
                );
            }
        };

        pub const Print = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) Error!*Self {
                return try createStmt(
                    allocator,
                    .{
                        .print = .{
                            .expr = expr,
                        },
                    },
                    position,
                );
            }
        };

        pub const Expr = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) Error!*Self {
                return try createStmt(
                    allocator,
                    .{
                        .expr = .{
                            .expr = expr,
                        },
                    },
                    position,
                );
            }
        };

        pub const Let = struct {
            index: usize,
            expr: ?*SemaExpr,

            pub fn create(
                allocator: Allocator,
                index: usize,
                expr: ?*SemaExpr,
                position: Position,
            ) Error!*Self {
                return try createStmt(
                    allocator,
                    .{
                        .let = .{
                            .index = index,
                            .expr = expr,
                        },
                    },
                    position,
                );
            }
        };

        assert: Assert,
        print: Print,
        expr: Expr,
        let: Let,
    };

    fn createStmt(
        allocator: Allocator,
        kind: Kind,
        position: Position,
    ) Error!*Self {
        const stmt = try allocator.create(Self);

        stmt.* = .{
            .kind = kind,
            .position = position,
        };

        return stmt;
    }

    kind: Kind,
    position: Position,
};

pub const SemaType = enum {
    const Self = @This();

    unit,
    int,
    float,
    bool,
    string,
    invalid,
    never,

    pub fn stringify(self: Self) []const u8 {
        return switch (self) {
            .unit => "Unit",
            .int => "Int",
            .float => "Float",
            .bool => "Bool",
            .string => "String",
            .invalid => "Invalid",
            .never => "Never",
        };
    }
};
