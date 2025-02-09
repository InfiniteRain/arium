const tokenizer_mod = @import("tokenizer.zig");
const parser_mod = @import("parser/parser.zig");
const sema_mod = @import("sema/sema.zig");
const sema_ast_mod = @import("sema/sema_ast.zig");
const managed_memory_mod = @import("state/managed_memory.zig");
const compiler_mod = @import("compiler/compiler.zig");
const vm_mod = @import("vm/vm.zig");
const error_reporter_mod = @import("reporter/error_reporter.zig");
const debug_ast_reporter_mod = @import("reporter/debug_ast_reporter.zig");
const debug_reporter_mod = @import("reporter/debug_reporter.zig");
const obj_mod = @import("state/obj.zig");

pub const Token = tokenizer_mod.Token;
pub const Tokenizer = tokenizer_mod.Tokenizer;
pub const Loc = tokenizer_mod.Loc;
pub const Parser = parser_mod.Parser;
pub const Sema = sema_mod.Sema;
pub const SemaExpr = sema_ast_mod.SemaExpr;
pub const SemaType = sema_ast_mod.SemaType;
pub const ManagedMemory = managed_memory_mod.ManagedMemory;
pub const Compiler = compiler_mod.Compiler;
pub const Vm = vm_mod.Vm;
pub const Obj = obj_mod.Obj;
pub const error_reporter = error_reporter_mod;
pub const debug_ast_reporter = debug_ast_reporter_mod;
pub const debug_reporter = debug_reporter_mod;

test {
    _ = @import("std").testing.refAllDeclsRecursive(@This());
}
