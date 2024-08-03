const std = @import("std");
const tokenizer_mod = @import("../parser/tokenizer.zig");
const io_handler_mod = @import("../io_handler.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const meta = std.meta;
const Position = tokenizer_mod.Position;
const IoHandler = io_handler_mod.IoHandler;

pub const SemaExpr = struct {
    const Self = @This();

    pub const Kind = union(enum) {
        pub const Literal = union(enum) {
            int: i64,
            float: f64,
            bool: bool,
            string: []const u8, // not owned by SemaExpr, but by tokenizer

            pub fn create(allocator: Allocator, literal: Literal, position: Position) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{ .literal = literal },
                    .eval_type = switch (literal) {
                        .int => .int,
                        .float => .float,
                        .bool => .bool,
                        .string => .{ .obj = .string },
                    },
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

                or_,
                and_,
            };

            kind: Binary.Kind,
            left: *Self,
            right: *Self,

            pub fn create(
                allocator: Allocator,
                kind: Binary.Kind,
                eval_type: EvalType,
                position: Position,
                left: *Self,
                right: *Self,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .binary = .{
                            .kind = kind,
                            .left = left,
                            .right = right,
                        },
                    },
                    .eval_type = eval_type,
                    .position = position,
                };

                return expr;
            }

            pub fn isComparison(self: *const SemaExpr.Kind.Binary) bool {
                return switch (self.kind) {
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
                    => true,
                    else => false,
                };
            }

            pub fn isLogical(self: *const SemaExpr.Kind.Binary) bool {
                return switch (self.kind) {
                    .or_,
                    .and_,
                    => true,
                    else => false,
                };
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
                eval_type: EvalType,
                position: Position,
                right: *Self,
            ) !*Self {
                const expr = try allocator.create(Self);

                expr.* = .{
                    .kind = .{
                        .unary = .{
                            .kind = kind,
                            .right = right,
                        },
                    },
                    .eval_type = eval_type,
                    .position = position,
                };

                return expr;
            }
        };

        pub const Invalid = struct {
            child_exprs: ArrayList(*Self),

            pub fn create(allocator: Allocator, child_exprs_struct: anytype) !*Self {
                const expr = try allocator.create(Self);
                var child_exprs = ArrayList(*Self).init(allocator);

                inline for (child_exprs_struct) |child_expr| {
                    try child_exprs.append(child_expr);
                }

                expr.* = .{
                    .kind = .{
                        .invalid = .{
                            .child_exprs = child_exprs,
                        },
                    },
                    .eval_type = .invalid,
                    .position = .{
                        .line = 0,
                        .column = 0,
                    },
                };

                return expr;
            }
        };

        literal: Literal,
        binary: Binary,
        unary: Unary,
        invalid: Invalid,
    };

    pub const EvalType = union(enum) {
        const Tag = meta.Tag(EvalType);

        pub const ObjKind = enum {
            string,
        };

        int,
        float,
        bool,
        obj: ObjKind,
        invalid,

        pub fn stringify(self: EvalType) []const u8 {
            return switch (self) {
                .int => "Int",
                .float => "Float",
                .bool => "Bool",
                .obj => switch (self.obj) {
                    .string => "String",
                },
                .invalid => "Invalid",
            };
        }

        pub fn tag(self: EvalType) Tag {
            return self;
        }
    };

    kind: Kind,
    eval_type: EvalType,
    position: Position,

    pub fn destroy(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .literal => {},
            .binary => |binary| {
                binary.left.destroy(allocator);
                binary.right.destroy(allocator);
            },
            .unary => |unary| {
                unary.right.destroy(allocator);
            },
            .invalid => |*invalid| {
                for (invalid.child_exprs.items) |child_expr| {
                    child_expr.destroy(allocator);
                }

                invalid.child_exprs.clearAndFree();
            },
        }
        allocator.destroy(self);
    }

    pub fn print(self: *const Self, io: *IoHandler) void {
        switch (self.kind) {
            .literal => |literal| {
                io.outf("{s}[", .{self.eval_type.stringify()});

                switch (literal) {
                    .int => |int| io.outf("{}", .{int}),
                    .float => |float| io.outf("{d}", .{float}),
                    .bool => |bool_| io.outf("{}", .{bool_}),
                    .string => |string| io.outf("{s}", .{string}),
                }

                io.out("]");
            },
            .binary => |binary| Self.printParenthesized(
                io,
                @tagName(binary.kind),
                self.eval_type,
                .{ binary.left, binary.right },
            ),
            .unary => |unary| Self.printParenthesized(
                io,
                @tagName(unary.kind),
                self.eval_type,
                .{unary.right},
            ),
            .invalid => |_| {
                io.out("<invalid>");
            },
        }
    }

    fn printParenthesized(
        io: *IoHandler,
        name: []const u8,
        eval_type: EvalType,
        expr: anytype,
    ) void {
        const ExprsType = @TypeOf(expr);
        const exprs_type_info = @typeInfo(ExprsType);

        if (exprs_type_info != .Struct) {
            @compileError("expected tuple, found " ++ @typeName(ExprsType));
        }

        const fields = exprs_type_info.Struct.fields;

        io.outf("({s}[{s}] ", .{ name, eval_type.stringify() });

        inline for (fields, 0..) |field, i| {
            const union_index = comptime std.fmt.parseInt(usize, field.name, 10) catch unreachable;
            expr[union_index].print(io);

            if (i != fields.len - 1) {
                io.out(" ");
            }
        }

        io.out(")");
    }
};
