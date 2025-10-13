fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // Fix pthread multiple definition issue on Windows/MinGW
    #[cfg(target_os = "windows")]
    {
        println!("cargo:rustc-link-arg=-Wl,--allow-multiple-definition");
    }

    // Link required system libraries for LLVM
    println!("cargo:rustc-link-lib=ffi");

    // Ensure LLVM system libraries are linked
    #[cfg(target_os = "windows")]
    {
        println!("cargo:rustc-link-lib=shell32");
        println!("cargo:rustc-link-lib=ole32");
        println!("cargo:rustc-link-lib=uuid");
        println!("cargo:rustc-link-lib=advapi32");
        println!("cargo:rustc-link-lib=ws2_32");
    }

    // Help llvm-sys find the LLVM installation
    if let Ok(llvm_prefix) = std::env::var("LLVM_SYS_181_PREFIX") {
        println!("cargo:rustc-link-search=native={}/lib", llvm_prefix);
        println!("cargo:rerun-if-env-changed=LLVM_SYS_181_PREFIX");
    }

    // Print some debug information during build
    if let Ok(target) = std::env::var("TARGET") {
        println!("cargo:warning=Building for target: {}", target);
    }

    if let Ok(llvm_prefix) = std::env::var("LLVM_SYS_181_PREFIX") {
        println!("cargo:warning=Using LLVM prefix: {}", llvm_prefix);
    } else {
        println!("cargo:warning=LLVM_SYS_181_PREFIX not set - llvm-sys will try to auto-detect");
    }
}
