# Implementation

This file explains some of the implementation details of this particular compiler,
particularly where it differs or works without guidance from Thain's book.

- [Pipeline](#pipeline)
- [Abstract Syntax Tree](#abstract-syntax-tree)
- [Backend](#backend)
- [Print statements](#print-statements)
- [Visitors](#visitors)

## Pipeline

The compiler goes through these steps:

- Lexing & parsing (using Chumsky)
- Name resolution
- Typechecking
- Control flow analysis
- Codegen

The control flow analysis step deserves explanation. It does _not_ fully convert
the Abstract Syntax Tree into a Control-Flow Graph; constructing phi nodes for
single-assignment form wasn't something I cared to implement. Instead, this step
only analyzes control flow to ensure every function returns in all branches of
execution, and warns of dead code.

## Abstract Syntax Tree

The main differences between the AST described in Thain's book and the one implemented
here are:
- The use of Rust's type system to provide tuple- and struct-like enums instead
  of housing `kind`s in a field inside of structs, when convenient. (This is the
  main reason I elected to start over in Rust rather than finish my implementation
  in C.)
- The use of `Vec`s rather than pointer-based trees, in many instances. There are
  certain instances where it is more natural to think of, say, the body of a function
  as a list of statements rather than as a tree. There are also, of course, a lot
  of arguments to suggest pointer-based data structures like linked lists aren't
  faster than vectors for a lot of use-cases.

## Backend

This compiler uses LLVM as a backend. The reason for this is sort of simple: if I
manage Assembly generation and register management on my own, I'm limited in the
machines I can run the compiler on until I explicitly program more backends. With
LLVM, it's far more doable to produce a relatively cross-platform compiler that
my friends can play with on different platforms to see the project I worked on.

## Print statements

Thain's book, as far as I can tell, or at least the edition I read, didn't offer
any guidance on how to actually implement the print statements B-Minor's specification
requires. (It briefly discusses implementing a "runtime library" with separate functions
for different types, but still doesn't broach actually getting the things to print.)
Because of my cross-platform goals (see the Backend section) and the complexity of printing,
I opted to rely on the user having an existing installation of a C compiler toolchain in
order to link the binary with the C library, allowing B-Minor's backend to simply call libc's
`printf()` externally.

## Visitors

Though they might not be identical to their object-oriented counterparts in languages
such as Java (due to the lack of a central interface to define how every visitor
should be implemented; this proved tricky due to needing to use the return values of
recursive calls and different passes needing those return values to be different types),
all analysis and modification passes I make against the AST are done
with separate structs I call "visitors", inspired by the pattern's method of
applying algorithms to objects without adding the methods to the objects themselves.
This is a minor shift in perspective from the C implementation described by Thain,
wherein everything has equal presence as a struct associated function. Using Rust
to allow for separating different functionality into different `impl` blocks or
separate objects entirely feels much nicer (sorry, C).

Each visitor recursively visits each part of the AST, accumulating a list of
unrecoverable errors which it emits at the end.
