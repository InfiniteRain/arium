const std = @import("std");
const meta = std.meta;
const mem = std.mem;

const arium = @import("arium");
const Span = arium.Span;
const Output = arium.Output;
const InternPool = arium.InternPool;

const checkForFixedArray = @import("util.zig").checkForFixedArray;
const Runner = @import("runner.zig").Runner;
const test_parser_mod = @import("test_parser.zig");
const TestParser = test_parser_mod.TestParser;
const TestSetup = test_parser_mod.TestSetup;

pub const TestDiagsPrinter = struct {
    diags: *const Runner.Diags,
    output: *const Output,
    current_validation_failure: Runner.Diags.Entry.Tag.ValidationFailure,

    pub fn printRunnerDiags(
        diags: *const Runner.Diags,
        output: *const Output,
    ) void {
        var diags_printer: TestDiagsPrinter = .{
            .diags = diags,
            .output = output,
            .current_validation_failure = undefined,
        };

        for (diags.entries.items) |entry| {
            diags_printer.printRunnerDiagEntry(entry);
            output.print("\n");
        }
    }

    fn printRunnerDiagEntry(
        self: *TestDiagsPrinter,
        diag: Runner.Diags.Entry,
    ) void {
        self.output.printf(
            "Diagnostics for '{s}':\n",
            .{diag.file_path.toSlice(self.diags.strings.items)},
        );

        self.printIndent(1);

        switch (diag.tag) {
            .file_read_failure,
            => self.output.print("Failed to read the test file."),

            .test_parse_failure,
            => |parse_failure| self.printTestParserDiagEntries(
                parse_failure.source.toSlice(
                    self.diags.strings.items,
                ),
                parse_failure.entries.toSlice(
                    self.diags.test_parser_diag_entries.items,
                ),
            ),

            .validation_failure => |mismatch| {
                self.printValidationFailure(mismatch);
            },
        }
    }

    fn printTestParserDiagEntries(
        self: *const TestDiagsPrinter,
        source: []const u8,
        diag_entries: []const TestParser.Diags.Entry,
    ) void {
        for (diag_entries) |entry| {
            self.printTestParserDiagEntry(source, entry);
            self.output.print("\n");
        }
    }

    fn printTestParserDiagEntry(
        self: *const TestDiagsPrinter,
        source: []const u8,
        entry: TestParser.Diags.Entry,
    ) void {
        const line, const column = entry.loc.toLineCol(source);

        self.output.printf("Test parser error at {}:{}: ", .{
            line,
            column,
        });

        switch (entry.tag) {
            .expected_directive_identifier,
            => self.output.print("Expected a directive identifier."),

            .unexpected_directive,
            => self.output.printf(
                "Unexpected directive \"{s}\".",
                .{entry.loc.toSlice(source)},
            ),

            .expected_left_paren_before_directive,
            => self.output.print(
                "Expected a left parenthesis before the directive identifier.",
            ),

            .expected_right_paren_after_directive_args,
            => self.output.print(
                "Expected a right parenthesis after the directive " ++
                    "identifier arguments.",
            ),

            .expected_dot_before_enum_variant,
            => self.output.print("Expected a dot before enum variant."),

            .expected_enum_variant_identifier,
            => self.output.print("Expected an enum variant identifier."),

            .unknown_enum_variant,
            => self.output.printf(
                "Unknown enum variant \"{s}\".",
                .{entry.loc.toSlice(source)},
            ),

            .expected_test_directive,
            => self.output.print("Expected a \"test\" directive."),

            .expected_int,
            => self.output.print("Expected an integer."),

            .expected_float,
            => self.output.print("Expected a float."),

            .expected_empty_block_as_void,
            => self.output.print("Expected a void value \"{}\"."),

            .overflow,
            => |int| {
                self.output.print("Parsed value for ");
                self.output.print(
                    if (int.signedness == .signed)
                        "i"
                    else
                        "u",
                );
                self.output.printf("{} overflows.", .{int.bits});
            },

            .expected_left_brace_before_struct,
            => self.output.print("Expected a left brace before struct."),

            .expected_right_brace_after_struct_fields,
            => self.output.print("Expected a right brace after struct fields."),

            .expected_dot_before_field_name,
            => self.output.print("Expected a dot before a field identifier."),

            .expected_field_identifier,
            => self.output.print("Expected a field identifier."),

            .expected_equal_after_field_identifier,
            => self.output.print(
                "Expected equals after a field identifier.",
            ),

            .unknown_struct_field,
            => self.output.printf(
                "Unknown struct field \"{s}\".",
                .{entry.loc.toSlice(source)},
            ),

            .not_all_fields_initialized,
            => self.output.print("Not all struct fields are initialized."),

            .field_redifinition,
            => self.output.printf(
                "Field \"{s}\" is redefined.",
                .{entry.loc.toSlice(source)},
            ),

            .expected_comma_after_loc,
            => self.output.print(
                "Expected a comma after the location argument.",
            ),

            .expected_dot_before_union_variant,
            => self.output.print("Expected a dot before a union variant."),

            .expected_dot_or_left_brace,
            => self.output.print(
                "Expected a dot or a left brace.",
            ),

            .union_variant_is_not_void,
            => self.output.printf(
                "Union variant \"{s}\" is not void.",
                .{entry.loc.toSlice(source)},
            ),

            .unknown_union_variant,
            => self.output.printf(
                "Unknown union variant \"{s}\".",
                .{entry.loc.toSlice(source)},
            ),

            .expected_union_variant_identifier,
            => self.output.print("Expected a union variant identifier."),

            .expected_equal_after_union_variant_identifier,
            => self.output.print(
                "Expected equals after a union variant identifier",
            ),

            .expected_right_brace_after_union_variant_value,
            => self.output.print(
                "Expected a right brace after union variant value.",
            ),

            .expected_left_brace_before_list_elems,
            => self.output.print("Expected a left brace before list elements."),

            .expected_right_brace_after_list_elems,
            => self.output.print("Expected a right brace after list elements."),

            .fixed_array_limit_reached,
            => |limit| self.output.printf(
                "List elements limit of {} is exceeded.",
                .{limit},
            ),

            .expected_string,
            => self.output.print("Expected a string."),

            .expected_comma_after_span_index,
            => self.output.print("Expected a comma after the span index."),
        }
    }

    fn printValidationFailure(
        self: *TestDiagsPrinter,
        validation_failure: Runner.Diags.Entry.Tag.ValidationFailure,
    ) void {
        self.current_validation_failure = validation_failure;

        self.output.print("Validation failure.\n");
        self.printIndent(1);
        self.output.print("Expected:\n");

        for (validation_failure.expects.toSlice(
            self.diags.expects.items,
        )) |expect| {
            self.printIndent(2);
            self.printValidationFailureExpect(expect);
            self.output.print("\n");
        }

        self.printIndent(1);
        self.output.print("Actual:\n");

        for (validation_failure.actuals.toSlice(
            self.diags.actuals.items,
        )) |actual| {
            self.printIndent(2);
            self.printValidationFailureActual(
                validation_failure.source.toSlice(self.diags.strings.items),
                actual,
            );
            self.output.print("\n");
        }
    }

    fn printValidationFailureExpect(
        self: *const TestDiagsPrinter,
        expect: TestSetup.Expect,
    ) void {
        switch (expect) {
            .parse_err => |parse_err| {
                self.output.print("Parse error ");
                self.printExpectLoc(parse_err.loc);
                self.output.print(": ");
                self.printValue(parse_err.tag);
            },
            .sema_err => |sema_err| {
                self.output.print("Sema error ");
                self.printExpectLoc(sema_err.loc);
                self.output.print(": ");
                self.printValue(sema_err.tag);
            },
            .compile_err => |compile_err| {
                self.output.print("Compile error ");
                self.printExpectLoc(compile_err.loc);
                self.output.print(": ");
                self.printValue(compile_err.tag);
            },
            .vm_err => |vm_err| {
                self.output.print("VM error ");
                self.printExpectLoc(vm_err.loc);
                self.output.print(": ");
                self.printValue(vm_err.tag);
            },
            .out => |out| self.output.printf(
                "Output line: '{s}'.",
                .{out.toSlice(self.diags.strings.items)},
            ),
        }
    }

    fn printValidationFailureActual(
        self: *const TestDiagsPrinter,
        source: []const u8,
        expect: Runner.Actual,
    ) void {
        switch (expect) {
            .parse_err => |parse_err| {
                self.output.print("Parse error ");
                self.printActualLoc(source, parse_err.loc);
                self.output.print(": ");
                self.printValue(parse_err.tag);
            },
            .sema_err => |sema_err| {
                self.output.print("Sema error ");
                self.printActualLoc(source, sema_err.loc);
                self.output.print(": ");
                self.printValue(sema_err.tag);
            },
            .compile_err => |compile_err| {
                self.output.print("Compile error ");
                self.printActualLoc(source, compile_err.loc);
                self.output.print(": ");
                self.printValue(compile_err.tag);
            },
            .vm_err => |vm_err| {
                self.output.print("VM error ");
                self.printActualLoc(source, vm_err.loc);
                self.output.print(": ");
                self.printValue(vm_err.tag);
            },
            .out => |out| self.output.printf(
                "Output line: '{s}'.",
                .{out.toSlice(self.diags.strings.items)},
            ),
        }
    }

    fn printValue(
        self: *const TestDiagsPrinter,
        value: anytype,
    ) void {
        const Type = @TypeOf(value);
        const type_info = @typeInfo(Type);

        if (Type == []const u8) {
            self.output.printf("{s}", .{value});
            return;
        }

        if (Type == InternPool.Index) {
            self.printIpIndex(value);
            return;
        }

        if (type_info == .array or
            (type_info == .pointer and type_info.pointer.size == .slice))
        {
            self.printArray(value);
            return;
        }

        if (checkForFixedArray(Type) != null) {
            self.printArray(value.slice());
            return;
        }

        switch (type_info) {
            .void => self.output.print("{{}"),
            .int => self.output.printf("{}", .{value}),
            .float => self.output.printf("{d}", .{value}),
            .@"struct" => self.printStruct(value),
            .@"enum" => self.output.printf(".{s}", .{@tagName(value)}),
            .@"union" => self.printUnion(value),
            else => @compileError(
                "printing for " ++ @typeName(Type) ++ " is not supported",
            ),
        }
    }

    fn printIpIndex(
        self: *const TestDiagsPrinter,
        index: InternPool.Index,
    ) void {
        switch (index) {
            _ => {},
            else => {
                self.output.printf(".{s}", .{@tagName(index)});
                return;
            },
        }

        const key = index.toKeyWith(
            self.diags.ip_items.slice().subslice(
                self.current_validation_failure.ip_items.index,
                self.current_validation_failure.ip_items.len,
            ),
            self.current_validation_failure.ip_extra.toSlice(
                self.diags.ip_extra.items,
            ),
            self.current_validation_failure.ip_strings.toSlice(
                self.diags.strings.items,
            ),
        );

        self.printUnion(key);
    }

    fn printArray(
        self: *const TestDiagsPrinter,
        value: anytype,
    ) void {
        self.output.print("{");

        for (value, 0..) |item, index| {
            self.printValue(item);

            if (index != value.len - 1) {
                self.output.print(", ");
            }
        }

        self.output.print("}");
    }

    fn printStruct(
        self: *const TestDiagsPrinter,
        value: anytype,
    ) void {
        const Type = @TypeOf(value);

        self.output.print("{");

        const fields = meta.fields(Type);

        inline for (fields, 0..) |field, index| {
            self.output.printf(".{s} = ", .{field.name});
            self.printValue(@field(value, field.name));

            if (index != fields.len - 1) {
                self.output.print(", ");
            }
        }

        self.output.print("}");
    }

    fn printUnion(
        self: *const TestDiagsPrinter,
        value: anytype,
    ) void {
        inline for (meta.fields(@TypeOf(value))) |field| {
            if (mem.eql(u8, @tagName(value), field.name)) {
                if (field.type == void) {
                    self.output.printf(".{s}", .{field.name});
                } else {
                    self.output.printf("{{.{s} = ", .{field.name});
                    self.printValue(@field(value, field.name));
                    self.output.print("}");
                }
            }
        }
    }

    fn printExpectLoc(
        self: *const TestDiagsPrinter,
        loc: TestSetup.Expect.Loc,
    ) void {
        switch (loc) {
            .line => |line| self.output.printf("at line {}", .{line}),
            .span => |span| self.output.printf(
                "at location span ({}, {})",
                .{ span.index, span.len },
            ),
        }
    }

    fn printActualLoc(
        self: *const TestDiagsPrinter,
        source: []const u8,
        loc: Span(u8),
    ) void {
        const line, const col = loc.toLineCol(source);
        self.output.printf("at {}:{}", .{ line, col });
    }

    fn printIndent(self: *const TestDiagsPrinter, level: u8) void {
        for (0..level) |_| {
            self.output.print(" " ** 4);
        }
    }
};
