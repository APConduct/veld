pub mod compiler;
pub mod compiler_v2;
pub mod register_alloc;
pub mod value;
pub mod vm;
pub mod vm_v2;

pub use compiler::BytecodeCompiler;
pub use compiler_v2::RegisterCompiler;
pub use register_alloc::RegisterAllocator;
pub use vm::{InterpretResult, VirtualMachine};

// Re-export v2 VM types with different names to avoid conflicts
pub use vm_v2::{InterpretResult as InterpretResultV2, VirtualMachine as VirtualMachineV2};
