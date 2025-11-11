pub mod bytecode_file;
pub mod compiler;
pub mod compiler_v2;
pub mod register_alloc;
pub mod value;
pub mod vm;
pub mod vm_v2;

pub use bytecode_file::{
    BYTECODE_EXTENSION, FileType, SOURCE_EXTENSION, compile_source, compile_to_file,
    load_bytecode_file, run_bytecode_file, run_file,
};
pub use compiler::BytecodeCompiler;
pub use compiler_v2::RegisterCompiler;
pub use register_alloc::RegisterAllocator;
pub use vm::{InterpretResult, VirtualMachine};

// Re-export v2 VM types with different names to avoid conflicts
pub use vm_v2::{InterpretResult as InterpretResultV2, VirtualMachine as VirtualMachineV2};
