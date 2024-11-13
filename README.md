A work-in-progress to re-implement my [B-Minor Compiler](https://github.com/joculatrix/bminor) in Rust.
Ultimately, the backend will be done with LLVM via Inkwell, a model I practiced in a smaller example project
[here](https://github.com/joculatrix/foo_llvm).

I'm currently working on typechecking in my local branch.

If you're unfamiliar with Rust and want to build/run the project (after installing Rust and Cargo via Rustup):
```
cargo run -- examples/hello_world.bm
```
If you want to build the in-progress documentation:
```
cargo doc --document-private-items
```
