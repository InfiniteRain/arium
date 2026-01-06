const std = @import("std");
const meta = std.meta;
const fmt = std.fmt;

const shared = @import("shared");

const arium = @import("arium");
const Output = arium.Output;
const InternPool = arium.InternPool;
const Ast = arium.Ast;
const Air = arium.Air;

const style_end = "\x1b[0m";
const style_underline = "\x1b[4m";

const Indent = struct {
    level: u16,
    row_finality: RowFinality,
    prev: ?*const Indent,

    const RowFinality = enum {
        middle_row,
        final_row,
    };

    const root: Indent = .{
        .level = 0,
        .row_finality = .middle_row,
        .prev = null,
    };

    fn wrap(self: *const Indent, row_finality: RowFinality) Indent {
        return .{
            .level = self.level,
            .row_finality = row_finality,
            .prev = self,
        };
    }

    fn wrapNewLevel(self: *const Indent, finality: RowFinality) Indent {
        return .{
            .level = self.level + 1,
            .row_finality = finality,
            .prev = self,
        };
    }
};

fn GenericTreePrinter(AstType: type) type {
    return struct {
        source: []const u8,
        output: *const Output,
        intern_pool: ?*const InternPool,
        ast: *const AstType,

        const Self = @This();

        const LineMode = enum { multi_line, single_line };

        fn print(
            source: []const u8,
            output: *const Output,
            intern_pool: ?*const InternPool,
            ast: *const AstType,
        ) void {
            var ast_printer: Self = .{
                .source = source,
                .output = output,
                .intern_pool = intern_pool,
                .ast = ast,
            };

            ast_printer.printAstIndex(.from(0), .root);
        }

        fn printAstIndex(
            self: *const Self,
            index: AstType.Index,
            indent: Indent,
        ) void {
            const key = index.toKey(self.ast);

            self.output.printf(
                "{s}.{s}{s}",
                .{ style_underline, @tagName(key), style_end },
            );

            inline for (meta.fields(AstType.Key)) |field| {
                if (std.mem.eql(u8, field.name, @tagName(key))) {
                    if (field.type != void) {
                        self.output.print(" = ");
                    } else {
                        const str = index.toLoc(self.ast)
                            .toSlice(self.source);

                        if (str.len > 0) {
                            self.output.printf(" '{s}'", .{str});
                        }
                    }

                    self.printAstField(
                        @field(key, field.name),
                        indent,
                    );
                }
            }

            if (indent.prev == null) {
                self.output.print("\n");
            }
        }

        fn printInternPoolIndex(
            self: *const Self,
            index: InternPool.Index,
            indent: Indent,
        ) void {
            const key = index.toKey(self.intern_pool.?);

            self.output.printf(
                ".{s}",
                .{@tagName(key)},
            );

            inline for (meta.fields(InternPool.Key)) |field| {
                if (std.mem.eql(u8, field.name, @tagName(key))) {
                    self.output.print(" = ");
                    self.printAstField(
                        @field(key, field.name),
                        indent,
                    );
                }
            }
        }

        fn printAstField(
            self: *const Self,
            field: anytype,
            indent: Indent,
        ) void {
            const Type = @TypeOf(field);
            const type_info = @typeInfo(Type);

            if (Type == void) {
                return;
            }

            if (Type == []u8 or Type == []const u8) {
                self.output.printf("\"{s}\"", .{field});
                return;
            }

            if (Type == AstType.Index) {
                self.printAstIndex(
                    field,
                    indent,
                );
                return;
            }

            if (Type == InternPool.Index) {
                self.printInternPoolIndex(
                    field,
                    indent,
                );
                return;
            }

            switch (type_info) {
                .pointer => |pointer| {
                    if (pointer.size != .slice) {
                        @compileError(
                            "ast printer doesn't support non-slice pointers",
                        );
                    }

                    if (field.len == 0) {
                        self.output.print("[]");
                        return;
                    }

                    self.output.print("[\n");

                    for (field, 0..) |element, index| {
                        const row_finality: Indent.RowFinality =
                            if (index == field.len - 1)
                                .final_row
                            else
                                .middle_row;

                        self.printIndent(indent.wrap(row_finality));
                        self.printAstField(
                            element,
                            indent.wrapNewLevel(row_finality),
                        );
                        self.output.print("\n");
                    }

                    self.printIndentNoConnect(indent);
                    self.output.print("]");
                },

                .@"struct" => {
                    const fields = meta.fields(Type);

                    if (fields.len == 0) {
                        self.output.print("{{}}");
                    }

                    self.output.print("{\n");

                    inline for (fields, 0..) |child_field, index| {
                        const row_finality: Indent.RowFinality =
                            if (index == fields.len - 1)
                                .final_row
                            else
                                .middle_row;

                        self.printIndent(indent.wrap(row_finality));
                        self.output.printf(".{s} = ", .{child_field.name});
                        self.printAstField(
                            @field(field, child_field.name),
                            indent.wrapNewLevel(row_finality),
                        );
                        self.output.print("\n");
                    }

                    self.printIndentNoConnect(indent);
                    self.output.print("}");
                },

                .@"enum" => {
                    self.output.printf(".{s}", .{@tagName(field)});
                },

                .optional => {
                    if (field == null) {
                        self.output.print("null");
                    } else {
                        self.printAstField(
                            field.?,
                            indent,
                        );
                    }
                },

                .int => {
                    self.output.printf("{d}", .{field});
                },

                .float => {
                    self.output.printf("{d}", .{field});
                },

                else => @compileError(fmt.comptimePrint(
                    "ast printer doesn't support {s} / {s}",
                    .{ @typeName(Type), @tagName(type_info) },
                )),
            }
        }

        fn printIndentNoConnect(self: *const Self, indent: Indent) void {
            self.printIndentAux(indent, .final_column, .disconnected);
        }

        fn printIndent(self: *const Self, indent: Indent) void {
            self.printIndentAux(indent, .final_column, .connected);
        }

        fn printIndentAux(
            self: *const Self,
            indent: Indent,
            column_finality: enum { middle_column, final_column },
            connectedness: enum { connected, disconnected },
        ) void {
            if (indent.prev) |prev| {
                self.printIndentAux(prev.*, .middle_column, .connected);
            } else {
                return;
            }

            const is_final_column_and_connected =
                column_finality == .final_column and
                connectedness == .connected;

            self.output.print(
                if (indent.row_finality == .final_row)
                    if (is_final_column_and_connected)
                        "└─"
                    else
                        "  "
                else if (is_final_column_and_connected)
                    "├─"
                else
                    "│ ",
            );
        }
    };
}

pub const TreePrinter = struct {
    pub fn printAst(
        source: []const u8,
        output: *const Output,
        ast: *const Ast,
    ) void {
        GenericTreePrinter(Ast).print(source, output, null, ast);
    }

    pub fn printAir(
        source: []const u8,
        output: *const Output,
        intern_pool: *const InternPool,
        air: *const Air,
    ) void {
        GenericTreePrinter(Air).print(source, output, intern_pool, air);
    }
};
