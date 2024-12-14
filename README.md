A work-in-progress to re-implement my [B-Minor Compiler](https://github.com/joculatrix/bminor) in Rust.

Currently, lexing, parsing, name resolution, typechecking, and some other analysis/verification are
all "done". All that's left (besides a lot of polish) is to tie it in to a backend powered by LLVM
via the Inkwell crate. Until I've done that, you can see a much smaller practice project I did to
learn about LLVM [here](https://github.com/joculatrix/foo_llvm).

If you're unfamiliar with Rust and want to build/run the project (after installing Rust and Cargo via Rustup):
```
cargo run -- examples/hello_world.bm
```
(Currently, running it with the hello_world example also won't output much as there's no code generation,
so you can either input a print statement with the AST after parsing, or run one of the examples in the
`should_error` folder to see diagnostic output!)

> [!IMPORTANT]
> Building the compiler from source requires an installation of LLVM v18, as LLVM's API headers are called
> externally.

If you want to build the documentation:
```
cargo doc --document-private-items
```
