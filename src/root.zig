const std = @import("std");

const air_mod = @import("air.zig");
pub const Air = air_mod.Air;
const ast_mod = @import("ast.zig");
pub const Ast = ast_mod.Ast;
const intern_pool_mod = @import("intern_pool.zig");
pub const InternPool = intern_pool_mod.InternPool;
const memory_mod = @import("memory.zig");
pub const Memory = memory_mod.Memory;
pub const Object = memory_mod.Object;
const new_compiler_mod = @import("compiler.zig");
pub const Compiler = new_compiler_mod.Compiler;
const new_sema_mod = @import("sema.zig");
pub const Sema = new_sema_mod.Sema;
const new_vm_mod = @import("vm.zig");
pub const Vm = new_vm_mod.Vm;
pub const Output = @import("output.zig").Output;
const parser_mod = @import("parser.zig");
pub const Parser = parser_mod.Parser;
const tokenizer_mod = @import("tokenizer.zig");
pub const Token = tokenizer_mod.Token;
pub const Tokenizer = tokenizer_mod.Tokenizer;
const span_mod = @import("span.zig");
pub const Span = @import("span.zig").Span;
const module_mod = @import("module.zig");
pub const Module = module_mod.Module;
pub const OpCode = module_mod.OpCode;
const debug_mod = @import("debug.zig");
pub const BuildMode = debug_mod.BuildMode;
pub const FixedArray = @import("fixed_array.zig").FixedArray;

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}
