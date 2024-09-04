const std = @import("std");
const tokenizer = @import("tokenizer.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const Position = tokenizer.Position;

pub const ParsedExpr = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Literal = struct {
            pub const Kind = union(enum) {
                unit,
                int: i64,
                float: f64,
                bool: bool,
                string: []const u8,
            };

            kind: Literal.Kind,

            // Takes ownership of heap data (string).
            pub fn create(
                allocator: Allocator,
                literal_kind: Literal.Kind,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .literal = .{
                            .kind = literal_kind,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Binary = struct {
            pub const Kind = enum {
                add,
                subtract,
                divide,
                multiply,
                concat,

                equal,
                not_equal,
                greater,
                greater_equal,
                less,
                less_equal,

                and_,
                or_,
            };

            left: *Self,
            kind: Binary.Kind,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                left: *Self,
                kind: Binary.Kind,
                right: *Self,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .binary = .{
                            .left = left,
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Unary = struct {
            pub const Kind = enum {
                negate_bool,
                negate_num,
            };

            kind: Unary.Kind,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                kind: Unary.Kind,
                right: *Self,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Block = struct {
            stmts: ArrayList(*ParsedStmt),
            ends_with_semicolon: bool,

            pub fn create(
                allocator: Allocator,
                stmts: ArrayList(*ParsedStmt),
                ends_with_semicolon: bool,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .block = .{
                            .stmts = stmts,
                            .ends_with_semicolon = ends_with_semicolon,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Variable = struct {
            name: []const u8,

            pub fn create(
                allocator: Allocator,
                name: []const u8,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .variable = .{
                            .name = try allocator.dupe(u8, name),
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const Assigment = struct {
            name: []const u8,
            right: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                name: []const u8,
                right: *ParsedExpr,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .assignment = .{
                            .name = try allocator.dupe(u8, name),
                            .right = right,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        pub const If = struct {
            condition: *ParsedExpr,
            then_block: *ParsedExpr,
            else_block: ?*ParsedExpr,

            pub fn create(
                allocator: Allocator,
                condition: *ParsedExpr,
                then_block: *ParsedExpr,
                else_block: ?*ParsedExpr,
                position: Position,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .if_ = .{
                            .condition = condition,
                            .then_block = then_block,
                            .else_block = else_block,
                        },
                    },
                    .position = position,
                };

                return expr;
            }
        };

        literal: Literal,
        binary: Binary,
        unary: Unary,
        block: Block,
        variable: Variable,
        assignment: Assigment,
        if_: If,
    };

    kind: Kind,
    position: Position,
};

pub const ParsedStmt = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Assert = struct {
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
                position: Position,
            ) !*Self {
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
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
                position: Position,
            ) !*Self {
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
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
                position: Position,
            ) !*Self {
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
            is_mutable: bool,
            name: []const u8,
            parsed_type: ?*ParsedType,
            expr: ?*ParsedExpr,

            pub fn create(
                allocator: Allocator,
                is_mutable: bool,
                name: []const u8,
                parsed_type: ?*ParsedType,
                expr: ?*ParsedExpr,
                position: Position,
            ) !*Self {
                const stmt = try allocator.create(Self);

                stmt.* = .{
                    .kind = .{
                        .let = .{
                            .is_mutable = is_mutable,
                            .name = try allocator.dupe(u8, name),
                            .parsed_type = parsed_type,
                            .expr = expr,
                        },
                    },
                    .position = position,
                };

                return stmt;
            }
        };

        print: Print,
        assert: Assert,
        expr: Expr,
        let: Let,
    };

    kind: Kind,
    position: Position,
};

pub const ParsedType = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Identifier = struct {
            name: []const u8,

            pub fn create(
                allocator: Allocator,
                name: []const u8,
                position: Position,
            ) !*Self {
                const parsed_type = try allocator.create(Self);

                parsed_type.* = .{
                    .kind = .{
                        .identifier = .{
                            .name = try allocator.dupe(u8, name),
                        },
                    },
                    .position = position,
                };

                return parsed_type;
            }
        };

        identifier: Identifier,
    };

    kind: Kind,
    position: Position,
};
