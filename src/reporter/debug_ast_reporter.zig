const std = @import("std");
const shared = @import("shared");
const sema_ast_mod = @import("../sema/sema_ast.zig");
const ast_mod = @import("../ast.zig");
const intern_pool_mod = @import("../intern_pool.zig");

const ArrayList = std.ArrayList;
const comptimePrint = std.fmt.comptimePrint;
const meta = std.meta;
const Writer = shared.Writer;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const SemaType = sema_ast_mod.SemaType;
const Ast = ast_mod.Ast;
const InternPool = intern_pool_mod.InternPool;

const style_end = "\x1b[0m";
const styles = [_][]const u8{
    "\x1b[4m\x1b[1;31m",
    "\x1b[4m\x1b[1;32m",
    "\x1b[4m\x1b[1;33m",
    "\x1b[4m\x1b[1;34m",
    "\x1b[4m\x1b[1;35m",
};
const style_underline = "\x1b[4m";

pub const Indent = struct {
    const Self = @This();

    const Kind = enum {
        entry,
        last_entry,
        none,
    };

    level: u16,
    is_last: bool,
    prev: ?*const Self,

    fn wrap(self: ?*const Self, is_last: bool) Self {
        return .{
            .is_last = is_last,
            .level = if (self) |prev| prev.level else 0,
            .prev = self,
        };
    }

    fn wrapNewLevel(self: ?*const Self, is_last: bool) Self {
        return .{
            .is_last = is_last,
            .level = if (self) |prev| prev.level + 1 else 1,
            .prev = self,
        };
    }
};

pub fn printAstIndex(
    Index: type,
    Key: type,
    intern_pool: ?*InternPool,
    ast: anytype,
    index: Index,
    indent_opt: ?Indent,
    writer: *const Writer,
) void {
    const key = index.toKey(ast);

    writer.printf("{s}.{s}{s}", .{ style_underline, @tagName(key), style_end });

    inline for (meta.fields(Key)) |field| {
        if (std.mem.eql(u8, field.name, @tagName(key))) {
            if (field.type != void) {
                writer.print(" = ");
            } else {
                writer.printf(" '{s}'", .{index.toStr(ast)});
            }

            printAstField(
                Index,
                Key,
                intern_pool,
                ast,
                @field(key, field.name),
                indent_opt,
                writer,
            );
        }
    }

    if (indent_opt == null) {
        writer.print("\n");
    }
}

pub fn printInternPoolIndex(
    Index: type,
    Key: type,
    intern_pool: *InternPool,
    ast: anytype,
    index: InternPool.Index,
    indent_opt: ?Indent,
    writer: *const Writer,
) void {
    const key = index.toKey(intern_pool);

    writer.printf(
        ".{s}",
        .{@tagName(key)},
    );

    inline for (meta.fields(InternPool.Key)) |field| {
        if (std.mem.eql(u8, field.name, @tagName(key))) {
            writer.print(" = ");
            printAstField(
                Index,
                Key,
                intern_pool,
                ast,
                @field(key, field.name),
                indent_opt,
                writer,
            );
        }
    }
}

fn printAstField(
    Index: type,
    Key: type,
    intern_pool: ?*InternPool,
    ast: anytype,
    field: anytype,
    indent_opt: ?Indent,
    writer: *const Writer,
) void {
    const indent_ptr: ?*const Indent = if (indent_opt) |indent| &indent else null;
    const Type = @TypeOf(field);
    const type_info = @typeInfo(Type);

    if (Type == void) {
        return;
    }

    if (Type == []u8 or Type == []const u8) {
        writer.printf("\"{s}\"", .{field});
        return;
    }

    if (Type == Index) {
        printAstIndex(Index, Key, intern_pool, ast, field, indent_opt, writer);
        return;
    }

    if (Type == InternPool.Index) {
        if (intern_pool) |ip| {
            printInternPoolIndex(
                Index,
                Key,
                ip,
                ast,
                field,
                indent_opt,
                writer,
            );
        } else {
            @panic("attempt to print intern pool index while being set to null");
        }
        return;
    }

    switch (type_info) {
        .pointer => |pointer| {
            if (pointer.size != .slice) {
                @compileError("ast printer doesn't support non-slice pointers");
            }

            if (field.len == 0) {
                writer.print("[]");
                return;
            }

            writer.print("[\n");

            for (field, 0..) |element, index| {
                const is_last = index == field.len - 1;

                printIndent(Indent.wrap(indent_ptr, is_last), writer);
                printAstField(
                    Index,
                    Key,
                    intern_pool,
                    ast,
                    element,
                    Indent.wrapNewLevel(indent_ptr, is_last),
                    writer,
                );
                writer.print("\n");
            }

            printIndentNoConnect(indent_opt, writer);
            writer.print("]");
        },

        .@"struct" => {
            const fields = meta.fields(Type);

            if (fields.len == 0) {
                writer.print("{{}}");
            }

            writer.print("{\n");

            inline for (fields, 0..) |child_field, index| {
                const is_last = index == fields.len - 1;

                printIndent(Indent.wrap(indent_ptr, is_last), writer);
                writer.printf(".{s} = ", .{child_field.name});
                printAstField(
                    Index,
                    Key,
                    intern_pool,
                    ast,

                    @field(field, child_field.name),
                    Indent.wrapNewLevel(indent_ptr, is_last),
                    writer,
                );
                writer.print("\n");
            }

            printIndentNoConnect(indent_opt, writer);
            writer.print("}");
        },

        .@"enum" => {
            writer.printf(".{s}", .{@tagName(field)});
        },

        .optional => {
            if (field == null) {
                writer.print("null");
            } else {
                printAstField(
                    Index,
                    Key,
                    intern_pool,
                    ast,
                    field.?,
                    indent_opt,
                    writer,
                );
            }
        },

        .int => {
            writer.printf("{d}", .{field});
        },

        .float => {
            writer.printf("{d}", .{field});
        },

        else => @compileError(comptimePrint(
            "ast printer doesn't support {s} / {s}",
            .{ @typeName(Type), @tagName(type_info) },
        )),
    }
}

pub fn printAstNode(node: anytype, indent_opt: ?Indent, writer: *const Writer) void {
    const indent_ptr: ?*const Indent = if (indent_opt) |indent| &indent else null;
    const Type = @TypeOf(node);
    const type_info = @typeInfo(Type);

    if (type_info != .pointer) {
        @compileError("expected node to be a pointer");
    }

    const child_type_info = @typeInfo(type_info.pointer.child);
    const child_variant = switch (child_type_info) {
        .@"struct" => child_type_info.@"struct",
        .@"union" => child_type_info.@"union",

        else => @compileError("expected node to be a struct or a union"),
    };

    if (!@hasField(type_info.pointer.child, "kind")) {
        @compileError("expected node to have a 'kind' field");
    }

    const indent_level = if (indent_opt) |indent| indent.level else 0;
    const style = styles[indent_level % styles.len];

    writer.printf(
        "{s}{s}{s} {{\n",
        .{ style, shared.meta.typeName(Type), style_end },
    );

    inline for (child_variant.fields) |field| {
        if (!std.mem.eql(u8, field.name, "kind")) {
            printIndent(Indent.wrap(indent_ptr, false), writer);
            writer.printf("{s} = ", .{field.name});
            printField(@field(node, field.name), Indent.wrap(indent_ptr, false), false, writer);
            writer.print(",\n");
        }
    }

    const last_indent = Indent.wrap(indent_ptr, true);

    printIndent(last_indent, writer);
    writer.print("kind = ");
    printUnion(node.kind, Indent.wrapNewLevel(indent_ptr, true), true, style, writer);

    writer.print("\n");
    printIndentNoConnect(indent_opt, writer);
    writer.print("}");

    if (indent_opt == null) {
        writer.print("\n");
    }
}

pub fn printField(field: anytype, indent: Indent, multiline: bool, writer: *const Writer) void {
    const Type = @TypeOf(field);
    const type_info = @typeInfo(Type);

    if (comptime shared.meta.isArrayList(Type)) {
        printArrayList(field, indent, writer);
        return;
    }

    if (type_info == .pointer and type_info.pointer.size == .one) {
        switch (Type) {
            *SemaExpr,
            *const SemaExpr,
            *SemaStmt,
            *const SemaStmt,
            => printAstNode(field, indent, writer),

            else => {
                writer.print("*");
                printField(field.*, indent, multiline, writer);
            },
        }
        return;
    }

    if (Type == []u8 or Type == []const u8) {
        writer.print(field);
        return;
    }

    switch (type_info) {
        .bool,
        .int,
        => writer.printf("{}", .{field}),

        .float,
        => writer.printf("{d}", .{field}),

        .@"union",
        => printUnion(field, indent, false, null, writer),

        .@"struct",
        => printStruct(field, indent, multiline, writer),

        .@"enum",
        => printEnum(field, writer),

        .optional,
        => if (field) |unwrapped| {
            printField(unwrapped, indent, multiline, writer);
        } else {
            writer.print("null");
        },

        else => @compileError(comptimePrint(
            "no reporting is implemented for {s} / {s}",
            .{ @typeName(Type), @tagName(type_info) },
        )),
    }
}

pub fn printUnion(
    @"union": anytype,
    indent: Indent,
    multiline: bool,
    style_opt: ?[]const u8,
    writer: *const Writer,
) void {
    const Type = @TypeOf(@"union");
    const type_info = @typeInfo(Type);

    inline for (type_info.@"union".fields) |field| {
        if (!std.mem.eql(u8, @tagName(@"union"), field.name)) {
            comptime continue;
        }

        var last_indent = indent;

        if (field.type != void) {
            writer.print("{");

            if (multiline) {
                last_indent = indent.wrap(true);

                writer.print("\n");
                printIndent(last_indent, writer);
            } else {
                writer.print(" ");
            }
        }

        if (style_opt) |style| {
            writer.print(style);
        }

        writer.print(field.name);

        if (style_opt != null) {
            writer.print(style_end);
        }

        if (field.type != void) {
            writer.print(" = ");
            printField(@field(@"union", field.name), last_indent, multiline, writer);

            if (multiline) {
                writer.print("\n");
                printIndentNoConnect(indent, writer);
            } else {
                writer.print(" ");
            }

            writer.print("}");
        }
    }
}

pub fn printStruct(
    @"struct": anytype,
    indent: Indent,
    multiline: bool,
    writer: *const Writer,
) void {
    const Type = @TypeOf(@"struct");
    const type_info = @typeInfo(Type);

    writer.print("{");

    inline for (type_info.@"struct".fields, 0..) |field, index| {
        const is_last = index == type_info.@"struct".fields.len - 1;

        if (multiline) {
            writer.print("\n");
            printIndent(indent.wrap(is_last), writer);
        } else {
            writer.print(" ");
        }

        writer.printf("{s} = ", .{field.name});
        printField(
            @field(@"struct", field.name),
            indent.wrap(is_last),
            multiline,
            writer,
        );

        if (!is_last) {
            writer.print(",");
        }
    }

    if (multiline) {
        writer.print("\n");
        printIndentNoConnect(indent, writer);
    } else {
        writer.print(" ");
    }

    writer.print("}");
}

pub fn printEnum(
    @"enum": anytype,
    writer: *const Writer,
) void {
    const Type = @TypeOf(@"enum");
    const type_info = @typeInfo(Type);

    inline for (type_info.@"enum".fields) |field| {
        if (std.mem.eql(u8, @tagName(@"enum"), field.name)) {
            writer.print(field.name);
        }
    }
}

fn printArrayList(field: anytype, indent: Indent, writer: *const Writer) void {
    if (field.items.len == 0) {
        writer.print("[]");
        return;
    }

    writer.print("[\n");

    for (field.items, 0..) |item, index| {
        const is_last = index == field.items.len - 1;

        printIndent(indent.wrap(is_last), writer);
        printField(item, indent.wrap(is_last), true, writer);

        if (!is_last) {
            writer.print(",\n");
        }
    }

    writer.print("\n");
    printIndentNoConnect(indent, writer);
    writer.print("]");
}

pub fn printIndentNoConnect(indent_opt: ?Indent, writer: *const Writer) void {
    printIndentAux(indent_opt, writer, true, true);
}

pub fn printIndent(indent_opt: ?Indent, writer: *const Writer) void {
    printIndentAux(indent_opt, writer, true, false);
}

pub fn printIndentAux(
    indent_opt: ?Indent,
    writer: *const Writer,
    is_last: bool,
    no_connect: bool,
) void {
    const indent = indent_opt orelse return;

    if (indent.prev) |prev| {
        printIndentAux(prev.*, writer, false, false);
    }

    writer.print(
        if (indent.is_last)
            if (is_last and !no_connect)
                "└─"
            else
                "  "
        else if (is_last and !no_connect)
            "├─"
        else
            "│ ",
    );
}
