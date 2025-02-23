A work-in-progress to re-implement my [B-Minor Compiler](https://github.com/joculatrix/bminor) in Rust.

Currently, code generation is partially working (at least using GCC - I'd appreciate feedback on output using
Clang or LLVM as linkers). Here is the output on my machine of compiling and running the `fib.bm` example to
print the 10th fibonacci number:

![image](https://github.com/user-attachments/assets/57e6f627-df2e-4ebd-b2e9-d1060f4ea06c)

Aside from fixing the incorrect artifact symbol, `hello_world.bm` also doesn't work yet due to another error
with string handling. Both problems are next on my list to fix.

> [!IMPORTANT]
> Building the compiler from source requires an installation of LLVM v18, as LLVM's API headers are called
> externally.
>
> Running the compiler also requires Clang, GCC, or MSVC. This implementation of B-Minor's `print` keyword
> relies on the C library, as well as a C compiler toolchain to link it.

If you're unfamiliar with Rust and want to build/run the project (after installing Rust and Cargo via Rustup):
```
cargo run -- examples/hello_world.bm
```
Currently, running `hello_world` will cause an error during code generation. Feel free to output the AST instead 
(see the program's `--help` output), or try another example, including those in the `should_error` folder to view
compiler diagnostics.

If you want to build the documentation:
```
cargo doc --document-private-items
```
