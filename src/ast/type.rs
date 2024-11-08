use super::*;

#[derive(Clone)]
/// Warning: The `Option<SimpleSpan>` must be the last field in ordered tuples
/// or else the `Spanned` trait will break.
pub enum Type<'src> {
    Atomic(Atomic, SimpleSpan),
    Array(ArrayType<'src>),
    Function(FunctionType<'src>),
}

#[derive(Clone)]
pub enum ArraySize<'src> {
    Expr(Option<Expr<'src>>),
    Known(usize),
}

#[derive(Clone)]
pub struct ArrayType<'src> {
    pub r#type: Box<Type<'src>>,
    pub size: ArraySize<'src>,
    pub span: SimpleSpan,
}

#[derive(Clone)]
pub struct FunctionType<'src> {
    pub return_type: Box<Type<'src>>,
    pub params: Vec<Param<'src>>,
    pub span: SimpleSpan,
}

#[derive(Clone)]
pub struct Param<'src> {
    pub ident: &'src str,
    pub r#type: Type<'src>,
    pub span: SimpleSpan,
}

#[derive(Clone, PartialEq)]
pub enum Atomic {
	Boolean,
	Char,
	Integer,
	String,
	Void,
}
