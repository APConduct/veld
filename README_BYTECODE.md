# Veld Bytecode Compilation - Quick Start

## 🚀 Quick Start

```bash
# Create a Veld program
echo 'fn add(a,b) a+b end; add(5,10)' > program.veld

# Compile to bytecode
cargo run --bin veld build program.veld

# Run the bytecode
cargo run --bin veld program.veldc
# Output: Program result: 15
```

## 📦 What is Bytecode?

Veld can now compile source code (`.veld`) to bytecode files (`.veldc`) for:
- ⚡ Faster loading (no parsing needed)
- 📦 Smaller file sizes (~50-70% reduction)
- 🔒 Source code protection
- 🚀 Easier distribution

## 🛠️ Commands

```bash
# Compile source to bytecode
veld build script.veld              # Creates script.veldc
veld build script.veld output.veldc # Custom output name

# Run files (auto-detects type)
veld script.veld   # Runs source
veld script.veldc  # Runs bytecode

# Inspect bytecode
veld disasm program.veldc  # Show instructions

# Get help
veld help
```

## ✨ Features

- ✅ All language features supported (functions, closures, loops)
- ✅ Auto-detect file types
- ✅ Binary format with magic bytes (`VELDC`)
- ✅ Preserves line info for debugging
- ✅ Nested functions and upvalue capture work perfectly

## 📖 Documentation

See [BYTECODE_FILES.md](docs/BYTECODE_FILES.md) for complete documentation.

## 🎯 Example: Closures

```veld
fn make_adder(x)
    fn inner(y)
        x + y
    end
    inner
end

let add5 = make_adder(5)
add5(10)  # Returns 15
```

```bash
$ veld build closure.veld
$ veld closure.veldc
Program result: 15
```

Closures work perfectly in bytecode! ✨
