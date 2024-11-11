use super::*;

#[derive(Clone, Debug)]
pub enum Type<'src> {
    Atomic(Atomic, SimpleSpan),
    Array(ArrayType<'src>),
    Function(FunctionType<'src>),
}

#[derive(Clone, Debug)]
pub enum ArraySize<'src> {
    Expr(Option<Expr<'src>>),
    Known(usize),
}

#[derive(Clone, Debug)]
pub struct ArrayType<'src> {
    pub r#type: Box<Type<'src>>,
    pub size: ArraySize<'src>,
    pub span: SimpleSpan,
}

#[derive(Clone, Debug)]
pub struct FunctionType<'src> {
    pub return_type: Box<Type<'src>>,
    pub params: Vec<Param<'src>>,
    pub span: SimpleSpan,
}

#[derive(Clone, Debug)]
pub struct Param<'src> {
    pub ident: &'src str,
    pub r#type: Type<'src>,
    pub span: SimpleSpan,
    pub symbol: Option<SymbolRef<'src>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Atomic {
	Boolean,
	Char,
	Integer,
	String,
	Void,
}
