const std = @import("std");
const shared = @import("shared");
const parsed_ast_mod = @import("../parser/parsed_ast.zig");
const sema_ast_mod = @import("../sema/sema_ast.zig");

const ArrayList = std.ArrayList;
const comptimePrint = std.fmt.comptimePrint;
const Writer = shared.Writer;
const meta = shared.meta;
const SemaExpr = sema_ast_mod.SemaExpr;
const SemaStmt = sema_ast_mod.SemaStmt;
const SemaType = sema_ast_mod.SemaType;
const ParsedExpr = parsed_ast_mod.ParsedExpr;
const ParsedStmt = parsed_ast_mod.ParsedStmt;
const ParsedType = parsed_ast_mod.ParsedType;

const style_end = "\x1b[0m";
const styles = [_][]const u8{
    "\x1b[4m\x1b[1;31m",
    "\x1b[4m\x1b[1;32m",
    "\x1b[4m\x1b[1;33m",
    "\x1b[4m\x1b[1;34m",
    "\x1b[4m\x1b[1;35m",
};

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

pub fn printAstNode(node: anytype, indent_opt: ?Indent, writer: *const Writer) void {
    const indent_ptr: ?*const Indent = if (indent_opt) |indent| &indent else null;
    const Type = @TypeOf(node);
    const type_info = @typeInfo(Type);

    if (type_info != .Pointer) {
        @compileError("expected node to be a pointer");
    }

    const child_type_info = @typeInfo(type_info.Pointer.child);
    const child_variant = switch (child_type_info) {
        .Struct => child_type_info.Struct,
        .Union => child_type_info.Union,

        else => @compileError("expected node to be a struct or a union"),
    };

    if (!@hasField(type_info.Pointer.child, "kind")) {
        @compileError("expected node to have a 'kind' field");
    }

    const indent_level = if (indent_opt) |indent| indent.level else 0;
    const style = styles[indent_level % styles.len];

    writer.printf(
        "{s}{s}{s} {{\n",
        .{ style, meta.typeName(Type), style_end },
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

    switch (Type) {
        ArrayList(*SemaStmt),
        ArrayList(*const SemaStmt),
        ArrayList(*ParsedStmt),
        ArrayList(*const ParsedStmt),
        => printArrayList(field, indent, writer),

        []u8,
        []const u8,
        => writer.print(field),

        *SemaExpr,
        *const SemaExpr,
        *SemaStmt,
        *const SemaStmt,
        *ParsedExpr,
        *const ParsedExpr,
        *ParsedStmt,
        *const ParsedStmt,
        => printAstNode(field, indent, writer),

        else => switch (type_info) {
            .Bool,
            .Int,
            => writer.printf("{}", .{field}),

            .Float,
            => writer.printf("{d}", .{field}),

            .Union,
            => printUnion(field, indent, false, null, writer),

            .Struct,
            => printStruct(field, indent, multiline, writer),

            .Enum,
            => printEnum(field, writer),

            .Optional,
            => if (field) |unwrapped| {
                printField(unwrapped, indent, multiline, writer);
            } else {
                writer.print("null");
            },

            else => @panic(comptimePrint(
                "no reporting is implemented for {s} / {s}",
                .{ @typeName(Type), @tagName(type_info) },
            )),
        },
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

    inline for (type_info.Union.fields) |field| {
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

    inline for (type_info.Struct.fields, 0..) |field, index| {
        const is_last = index == type_info.Struct.fields.len - 1;

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

    inline for (type_info.Enum.fields) |field| {
        if (std.mem.eql(u8, @tagName(@"enum"), field.name)) {
            writer.print(field.name);
        }
    }
}

fn printArrayList(field: anytype, indent: Indent, writer: *const Writer) void {
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
