//! The Abstract Syntax Tree for B-Minor programs. The top-level structure of
//! this is a `Vec` of [`Decl`]s.
//!
//! This AST isn't represented as a pure tree of nodes connected by pointers
//! the way many are. The top level is a `Vec`, and [`Stmt::Block`], for example,
//! also uses a `Vec` of [`Stmt`]s rather than letting each have a `next`
//! pointer. It also looks nothing like Thain's example code snippets. There
//! are a few reasons for all of these differences.
//!
//! * There are instances, like binary operations, where a branch of a left and
//!   right child node of sub-expressions makes logical sense. I use that
//!   pattern in those cases. In other cases, a sequence of statements logically
//!   lends itself to a list or vector. In those cases I use vectors. With
//!   modern hardware, I prioritize sensible structures unless there's a real
//!   substantial case to be made for performance.
//! * Iterating over all members of a vector is easy and doesn't have the
//!   overhead pointers do. Similarly, it allows more flexible access without
//!   worrying about juggling pointers in doubly-linked lists.
//! * The performance benefits of linked lists are absent until you deal with
//!   thousands of elements. A single block of code is unlikely to contain
//!   thousands of statements. If it does, it's probably obviously bad code
//!   regardless.
//! * My first version of this compiler and the example code from Thain's
//!   book are in C. That implementation leads to (in my opinion) ugly structs,
//!   with often extraneous fields for all cases included in every instance of
//!   the struct. Keeping track of which fields are utilized for which kinds for
//!   which purpose has a lot of mental and sometimes practical overhead. Rust's
//!   type system, on the other hand, allows for much more elegant enum types,
//!   where you know exactly what it uses.
pub mod decl;
pub mod expr;
pub mod stmt;
pub mod r#type;

pub use self::decl::Decl;
pub use self::expr::Expr;
pub use self::r#type::Atomic;
pub use self::r#type::Type;
pub use self::stmt::Stmt;

use chumsky::span::SimpleSpan;
use crate::symbol::SymbolRef;

pub type Spanned<T> = (T, SimpleSpan);

