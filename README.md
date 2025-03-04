An implementation of the B-Minor toy language from Douglas Thain's "Introduction to Compilers and Language
Design". 

Currently, code generation works on my machine using GCC as a linker - I'd love to hear from anyone who
tests on Windows or with Clang as a linker. Here is example output of compiling and running the
`hello_world.bm` example:

![image](https://github.com/user-attachments/assets/5ac6f738-0086-4331-9216-d895aa634e47)

Similarly, here's compiler diagnostic output from one of the examples intended to produce errors:

![image](https://github.com/user-attachments/assets/50f12360-d488-4ff9-90d1-b9a9202d61c0)

## Building and Running

> [!IMPORTANT]
> Building the compiler from source requires an installation of LLVM v18, as LLVM's API headers are called
> externally.
>
> Running the compiler also requires Clang, GCC, or MSVC. This implementation of B-Minor's `print` keyword
> relies on the C library, as well as a C compiler toolchain to link it.

If you're unfamiliar with Rust and want to run the project (after installing Rust and Cargo via Rustup):
```
cargo run -- examples/hello_world.bm
```
(or, if you've installed the release binary):
```
./bmcc examples/hello_world.bm
```

If you want to build the documentation:
```
cargo doc --document-private-items
```
