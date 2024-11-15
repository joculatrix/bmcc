use super::*;

#[derive(Clone, Debug)]
/// AST type for B-Minor declarations. A B-Minor program, at its top level, is
/// a list of declarations.
pub enum Decl<'src> {
	Var(Var<'src>),
	Function(Function<'src>),
}

#[derive(Clone, Debug)]
pub struct Var<'src> {
	pub name: &'src str,
	pub r#type: Box<Type<'src>>,
	pub rhs: Option<Expr<'src>>,
	pub span: SimpleSpan,
    pub symbol: Option<SymbolRef<'src>>,
}

#[derive(Clone, Debug)]
pub struct Function<'src> {
	pub name: &'src str,
	pub r#type: Box<Type<'src>>,
	pub body: Option<Stmt<'src>>,
	pub span: SimpleSpan,
    pub symbol: Option<SymbolRef<'src>>,
}
