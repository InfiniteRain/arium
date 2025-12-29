const tokenizer_mod = @import("tokenizer.zig");
const ast_mod = @import("ast.zig");
const parser_mod = @import("parser.zig");
const intern_pool_mod = @import("intern_pool.zig");
const air_mod = @import("air.zig");
const new_sema_mod = @import("sema.zig");
const sema_mod = @import("sema/sema.zig");
const sema_ast_mod = @import("sema/sema_ast.zig");
const managed_memory_mod = @import("state/managed_memory.zig");
const compiler_mod = @import("compiler/compiler.zig");
const vm_mod = @import("vm/vm.zig");
const error_reporter_mod = @import("reporter/error_reporter.zig");
const debug_ast_reporter_mod = @import("reporter/debug_ast_reporter.zig");
const debug_reporter_mod = @import("reporter/debug_reporter.zig");
const obj_mod = @import("state/obj.zig");
const memory_mod = @import("memory.zig");
const new_compiler_mod = @import("compiler.zig");
const new_vm_mod = @import("vm.zig");

pub const Token = tokenizer_mod.Token;
pub const Tokenizer = tokenizer_mod.Tokenizer;
pub const Loc = tokenizer_mod.Loc;
pub const Ast = ast_mod.Ast;
pub const Parser = parser_mod.Parser;
pub const InternPool = intern_pool_mod.InternPool;
pub const Air = air_mod.Air;
pub const SemaNew = new_sema_mod.Sema;
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
pub const module_reporter = @import("reporter/module_reporter.zig");
pub const Memory = memory_mod.Memory;
pub const Object = memory_mod.Object;
pub const CompilerNew = new_compiler_mod.Compiler;
pub const VmNew = new_vm_mod.Vm;

test {
    _ = @import("std").testing.refAllDeclsRecursive(@This());
}
