const std = @import("std");
const tokenizer = @import("tokenizer.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const Position = tokenizer.Position;

pub const ParsedExpr = struct {
    const Self = @This();

    const Error = error{OutOfMemory};

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
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .literal = .{
                            .kind = literal_kind,
                        },
                    },
                    position,
                );
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

                @"and",
                @"or",
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
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .binary = .{
                            .left = left,
                            .kind = kind,
                            .right = right,
                        },
                    },
                    position,
                );
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
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    position,
                );
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
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .block = .{
                            .stmts = stmts,
                            .ends_with_semicolon = ends_with_semicolon,
                        },
                    },
                    position,
                );
            }
        };

        pub const Variable = struct {
            name: []const u8,

            pub fn create(
                allocator: Allocator,
                name: []const u8,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .variable = .{
                            .name = try allocator.dupe(u8, name),
                        },
                    },
                    position,
                );
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
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .assignment = .{
                            .name = try allocator.dupe(u8, name),
                            .right = right,
                        },
                    },
                    position,
                );
            }
        };

        pub const If = struct {
            pub const ConditionalBlock = struct {
                condition: *ParsedExpr,
                block: *ParsedExpr,
            };

            conditional_block: ConditionalBlock,
            elseif_blocks: ArrayList(ConditionalBlock),
            else_block: ?*ParsedExpr,

            pub fn create(
                allocator: Allocator,
                conditional_block: ConditionalBlock,
                elseif_blocks: ArrayList(ConditionalBlock),
                else_block: ?*ParsedExpr,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .@"if" = .{
                            .conditional_block = conditional_block,
                            .elseif_blocks = elseif_blocks,
                            .else_block = else_block,
                        },
                    },
                    position,
                );
            }
        };

        pub const For = struct {
            condition: ?*ParsedExpr,
            body_block: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                condition: ?*ParsedExpr,
                body_block: *ParsedExpr,
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
                    position,
                );
            }
        };

        pub const Break = struct {
            pub fn create(
                allocator: Allocator,
                position: Position,
            ) Error!*Self {
                return try createExpr(allocator, .@"break", position);
            }
        };

        pub const Continue = struct {
            pub fn create(
                allocator: Allocator,
                position: Position,
            ) Error!*Self {
                return try createExpr(allocator, .@"continue", position);
            }
        };

        pub const Return = struct {
            right: ?*ParsedExpr,

            pub fn create(
                allocator: Allocator,
                right: ?*ParsedExpr,
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .@"return" = .{
                            .right = right,
                        },
                    },
                    position,
                );
            }
        };

        pub const Call = struct {
            callee: *ParsedExpr,
            args: ArrayList(*ParsedExpr),

            pub fn create(
                allocator: Allocator,
                callee: *ParsedExpr,
                args: ArrayList(*ParsedExpr),
                position: Position,
            ) Error!*Self {
                return try createExpr(
                    allocator,
                    .{
                        .call = .{
                            .callee = callee,
                            .args = args,
                        },
                    },
                    position,
                );
            }
        };

        fn createExpr(
            allocator: Allocator,
            kind: Kind,
            position: Position,
        ) Error!*Self {
            const expr = try allocator.create(Self);

            expr.* = .{
                .kind = kind,
                .position = position,
            };

            return expr;
        }

        literal: Literal,
        binary: Binary,
        unary: Unary,
        block: Block,
        variable: Variable,
        assignment: Assigment,
        @"if": If,
        @"for": For,
        @"break": Break,
        @"continue": Continue,
        @"return": Return,
        call: Call,
    };

    kind: Kind,
    position: Position,
};

pub const ParsedStmt = struct {
    const Self = @This();

    const Error = error{OutOfMemory};

    pub const Kind = union(enum) {
        pub const Assert = struct {
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
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
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
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
            expr: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                expr: *ParsedExpr,
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
            ) Error!*Self {
                return try createStmt(
                    allocator,
                    .{
                        .let = .{
                            .is_mutable = is_mutable,
                            .name = try allocator.dupe(u8, name),
                            .parsed_type = parsed_type,
                            .expr = expr,
                        },
                    },
                    position,
                );
            }
        };

        pub const Fn = struct {
            pub const Arg = struct {
                name: []const u8,
                type: *ParsedType,
                name_position: Position,
                type_position: Position,
            };

            name: []const u8,
            args: ArrayList(Arg),
            return_type: *ParsedType,
            body: *ParsedExpr,

            pub fn create(
                allocator: Allocator,
                name: []const u8,
                args: ArrayList(Arg),
                return_type: *ParsedType,
                body: *ParsedExpr,
                position: Position,
            ) Error!*Self {
                return try createStmt(
                    allocator,
                    .{
                        .@"fn" = .{
                            .name = try allocator.dupe(u8, name),
                            .args = args,
                            .return_type = return_type,
                            .body = body,
                        },
                    },
                    position,
                );
            }
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

        print: Print,
        assert: Assert,
        expr: Expr,
        let: Let,
        @"fn": Fn,
    };

    kind: Kind,
    position: Position,
};

pub const ParsedType = struct {
    const Self = @This();

    const Error = error{OutOfMemory};

    pub const Kind = union(enum) {
        pub const Identifier = struct {
            name: []const u8,

            pub fn create(
                allocator: Allocator,
                name: []const u8,
                position: Position,
            ) Error!*Self {
                return try createType(
                    allocator,
                    .{
                        .identifier = .{
                            .name = try allocator.dupe(u8, name),
                        },
                    },
                    position,
                );
            }
        };

        identifier: Identifier,
    };

    fn createType(
        allocator: Allocator,
        kind: Kind,
        position: Position,
    ) Error!*Self {
        const parsed_type = try allocator.create(Self);

        parsed_type.* = .{
            .kind = kind,
            .position = position,
        };

        return parsed_type;
    }

    kind: Kind,
    position: Position,
};
