# Proper Module Resolution Solution

## The Problem with Current Approach

### What's Wrong:
1. **Hardcoding stdlib functions** - Not scalable, requires manual updates
2. **Only works for stdlib** - User modules can't be supported
3. **Incomplete coverage** - Only handles `io.println`, `io.print`, `io.read_line`
4. **Maintenance burden** - Every new function needs a hardcoded case
5. **No type safety** - Easy to make mistakes in hardcoded types

### Current Issues:
- `main()` still shows `-> T5` (TypeVar)
- Even with hardcoded cases, they're not being hit
- Type checker debug logs aren't showing module method calls
- The hardcoded approach is fundamentally flawed

## The Right Solution

### Phase 1: Module Function Registry (Foundation)

Create a proper module system that can:
1. Register modules and their exported functions
2. Look up function types by qualified name
3. Support both stdlib and user modules
4. Be populated automatically from source files

### Phase 2: Stdlib Signature Loading

Instead of hardcoding, actually parse stdlib files:
1. Parse `stdlib/io.veld`, `stdlib/option.veld`, etc.
2. Extract function signatures (name, params, return type)
3. Register them in the module registry
4. Do this once at type checker initialization

### Phase 3: User Module Support

Extend to support user modules:
1. Parse imported modules from source files
2. Extract their exports
3. Register in the same module registry
4. Cross-file type checking

## Implementation Plan

### Step 1: Create ModuleRegistry

```rust
// crates/common/src/types/module_registry.rs

pub struct ModuleRegistry {
    modules: HashMap<String, Module>,
}

pub struct Module {
    pub path: String,
    pub functions: HashMap<String, FunctionSignature>,
    pub types: HashMap<String, Type>,
    pub imports: Vec<Import>,
}

pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub is_public: bool,
}

impl ModuleRegistry {
    pub fn new() -> Self { ... }
    
    pub fn register_module(&mut self, module: Module) { ... }
    
    pub fn lookup_function(&self, module_path: &str, function_name: &str) -> Option<&FunctionSignature> {
        // Handle both "std.io" and "io" (alias resolution)
        ...
    }
    
    pub fn load_stdlib(&mut self) -> Result<()> {
        // Parse stdlib files and register all exports
        ...
    }
}
```

### Step 2: Integrate with TypeChecker

```rust
// In TypeChecker
pub struct TypeChecker {
    env: TypeEnvironment,
    module_registry: Arc<ModuleRegistry>,  // NEW
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut module_registry = ModuleRegistry::new();
        module_registry.load_stdlib().expect("Failed to load stdlib");
        
        Self {
            env: TypeEnvironment::new(),
            module_registry: Arc::new(module_registry),
        }
    }
}
```

### Step 3: Update Method Call Resolution

```rust
// In infer_method_call_type for Type::Module
Type::Module(module_path) => {
    // Look up in module registry first
    if let Some(sig) = self.module_registry.lookup_function(module_path, method) {
        // Build function type from signature
        let func_type = Type::Function {
            params: sig.params.iter().map(|(_, t)| t.clone()).collect(),
            return_type: Box::new(sig.return_type.clone()),
        };
        
        // Type check arguments against signature
        return self.check_function_call_args(&func_type, args);
    }
    
    // Fallback to environment lookup
    ...
}
```

### Step 4: Stdlib Parser

```rust
// crates/common/src/stdlib_loader.rs

pub fn load_stdlib_module(module_name: &str) -> Result<Module> {
    let stdlib_path = format!("./stdlib/{}.veld", module_name);
    let source = fs::read_to_string(stdlib_path)?;
    
    // Parse the stdlib file
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.collect_tokens()?;
    let mut parser = Parser::new(tokens);
    let statements = parser.parse()?;
    
    // Extract exports (public functions, types, etc.)
    extract_module_exports(module_name, &statements)
}

fn extract_module_exports(module_name: &str, statements: &[Statement]) -> Result<Module> {
    let mut functions = HashMap::new();
    let mut types = HashMap::new();
    
    for stmt in statements {
        match stmt {
            Statement::FunctionDeclaration { name, params, return_type, is_public: true, .. } => {
                // Extract parameter types and return type
                let param_types = params.iter().map(|(name, type_ann)| {
                    (name.clone(), type_annotation_to_type(type_ann))
                }).collect();
                
                let ret_type = type_annotation_to_type(return_type);
                
                functions.insert(name.clone(), FunctionSignature {
                    name: name.clone(),
                    params: param_types,
                    return_type: ret_type,
                    is_public: true,
                });
            }
            Statement::TypeDeclaration { name, type_annotation, .. } => {
                types.insert(name.clone(), type_annotation_to_type(type_annotation));
            }
            // Handle structs, enums, etc.
            _ => {}
        }
    }
    
    Ok(Module {
        path: module_name.to_string(),
        functions,
        types,
        imports: vec![],
    })
}
```

## Benefits of Proper Solution

### ✅ Advantages:
1. **Automatic** - Parses actual stdlib source files
2. **Type-safe** - Uses real type annotations from source
3. **Complete** - Gets ALL exported functions, not just hardcoded ones
4. **Maintainable** - Add new stdlib functions by adding to source files
5. **Extensible** - Works for user modules too
6. **Documentation** - Can extract doc comments
7. **Accurate** - Always matches actual implementation

### 📊 Comparison:

**Hardcoding Approach:**
```rust
match function_path.as_str() {
    "std.io.println" => Ok(Type::Unit),
    "std.io.print" => Ok(Type::Unit),
    "std.io.read_line" => Ok(Type::String),
    // Need to add EVERY function manually
    // What about io.read_file, io.write_file, etc.?
}
```

**Proper Approach:**
```rust
// Automatically loads from stdlib/io.veld:
pub fn println(s: str) => do ... end
pub fn print(s: str) => do ... end
pub fn read_line() -> str => do ... end
pub fn read_file(path: str) -> str => do ... end
pub fn write_file(path: str, content: str) => do ... end

// All registered automatically!
```

## Implementation Steps

### Quick Win (Now):
1. Create `ModuleRegistry` struct
2. Add `load_stdlib()` method that parses stdlib files
3. Integrate with `TypeChecker`
4. Update module method resolution to use registry

### Medium Term:
1. Add import resolution (track aliases)
2. Support cross-file references
3. Extract doc comments for hover
4. Cache parsed modules

### Long Term:
1. Workspace-wide analysis
2. Go-to-definition across files
3. Find all references
4. Refactoring support

## Stdlib File Example

What `stdlib/io.veld` should look like:

```veld
# Standard library I/O functions

pub fn println(message: str) => do
    # Print a line to stdout
    native_println(message)
end

pub fn print(message: str) => do
    # Print without newline
    native_print(message)
end

pub fn read_line() -> str => do
    # Read a line from stdin
    native_read_line()
end

pub fn read_file(path: str) -> str => do
    # Read entire file contents
    native_read_file(path)
end

pub fn write_file(path: str, content: str) => do
    # Write content to file
    native_write_file(path, content)
end
```

Then the type checker automatically knows:
- `io.println(str) -> ()`
- `io.print(str) -> ()`
- `io.read_line() -> str`
- `io.read_file(str) -> str`
- `io.write_file(str, str) -> ()`

## Why Current Approach Fails

### Debug Investigation:

The reason `-> T5` still appears despite hardcoding:

1. **Type checker runs during LSP analysis** - Fresh type checker instance
2. **LSP registers modules in environment** - Different from type checker internals
3. **Module method resolution** - May not be using the hardcoded path
4. **Import alias tracking** - `import std.io as io` creates complex resolution
5. **Expression parsing** - `io.println()` might parse differently than expected

### Real Issue:

The type checker and LSP analyzer are somewhat disconnected. The LSP registers things in the environment, but the type checker's module method resolution happens independently.

**Proper solution fixes this** by having ONE source of truth (the module registry) that both LSP and type checker use.

## Migration Path

### Phase 1: Core Infrastructure (1-2 days)
- Create `ModuleRegistry`
- Add stdlib parsing
- Basic function lookup

### Phase 2: Integration (1 day)
- Connect to `TypeChecker`
- Update method call resolution
- Test with stdlib

### Phase 3: Enhancement (1-2 days)
- Add import alias resolution
- Support user modules
- Extract doc comments

### Phase 4: Polish (1 day)
- Error messages
- Performance optimization
- Documentation

**Total: ~1 week for complete solution**

## Decision Point

### Option A: Continue with Hardcoding
- ⏱️ Quick (30 minutes per module)
- ⚠️ Not scalable
- ⚠️ Maintenance burden
- ⚠️ Only works for stdlib
- ⚠️ Doesn't solve root cause

### Option B: Implement Proper Solution
- ⏱️ Takes ~1 week
- ✅ Scalable to any module
- ✅ Automatic - just add .veld files
- ✅ Works for user modules
- ✅ Solves root cause
- ✅ Foundation for advanced features

## Recommendation

**Implement the proper solution.** 

The hardcoded approach has already failed (still shows `-> T5`), and even if we fix it, we'll hit the same issues with every new module/function. 

The proper solution provides:
1. A real module system for Veld
2. Foundation for workspace analysis
3. Support for user modules
4. Automatic stdlib coverage
5. Long-term maintainability

**Next Steps:**
1. Agree on approach
2. I'll implement `ModuleRegistry` and stdlib loading
3. Integrate with type checker
4. Test with your examples
5. Extend to user modules

**What do you think?** Should we do this properly?