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
            invalid,

            // Takes ownership of heap data (string).
            pub fn create(
                allocator: Allocator,
                literal: Literal,
                evals: bool,
                position: Position,
            ) Error!*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{ .literal = literal },
                    .sema_type = switch (literal) {
                        .unit => .unit,
                        .int => .int,
                        .float => .float,
                        .bool => .bool,
                        .string => .string,
                        .invalid => .invalid,
                    },
                    .evals = evals,
                    .position = position,
                };

                return expr;
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
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .binary = .{
                            .kind = kind,
                            .left = left,
                            .right = right,
                        },
                    },
                    .sema_type = sema_type,
                    .evals = evals,
                    .position = position,
                };

                return expr;
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
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .sema_type = sema_type,
                    .evals = evals,
                    .position = position,
                };

                return expr;
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
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .block = .{
                            .stmts = stmts,
                        },
                    },
                    .sema_type = sema_type,
                    .evals = evals,
                    .position = position,
                };

                return expr;
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
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .variable = .{
                            .index = index,
                        },
                    },
                    .position = position,
                    .evals = evals,
                    .sema_type = sema_type,
                };

                return expr;
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
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .assignment = .{
                            .index = index,
                            .right = right,
                        },
                    },
                    .position = position,
                    .evals = evals,
                    .sema_type = .unit,
                };

                return expr;
            }
        };

        pub const If = struct {
            condition: *SemaExpr,
            then_block: *SemaExpr,
            else_block: ?*SemaExpr,

            pub fn create(
                allocator: Allocator,
                condition: *SemaExpr,
                then_block: *SemaExpr,
                else_block: ?*SemaExpr,
                sema_type: SemaType,
                evals: bool,
                position: Position,
            ) Error!*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .@"if" = .{
                            .condition = condition,
                            .then_block = then_block,
                            .else_block = else_block,
                        },
                    },
                    .position = position,
                    .evals = evals,
                    .sema_type = sema_type,
                };

                return expr;
            }
        };

        pub const For = struct {
            condition: *SemaExpr,
            body_block: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                condition: *SemaExpr,
                body_block: *SemaExpr,
                evals: bool,
                position: Position,
            ) Error!*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .@"for" = .{
                            .condition = condition,
                            .body_block = body_block,
                        },
                    },
                    .position = position,
                    .evals = evals,
                    .sema_type = .unit,
                };

                return expr;
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
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .assert = .{
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        pub const Print = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) Error!*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .print = .{
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        pub const Expr = struct {
            expr: *SemaExpr,

            pub fn create(
                allocator: Allocator,
                expr: *SemaExpr,
                position: Position,
            ) Error!*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .expr = .{
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
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
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .let = .{
                            .index = index,
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        assert: Assert,
        print: Print,
        expr: Expr,
        let: Let,
    };

    kind: Kind,
    position: Position,
};

pub const SemaType = union(enum) {
    const Self = @This();
    const Tag = meta.Tag(SemaType);

    unit,
    int,
    float,
    bool,
    string,
    invalid,

    pub fn stringify(self: Self) []const u8 {
        return switch (self) {
            .unit => "Unit",
            .int => "Int",
            .float => "Float",
            .bool => "Bool",
            .string => "String",
            .invalid => "Invalid",
        };
    }

    pub fn tag(self: Self) Tag {
        return self;
    }
};
