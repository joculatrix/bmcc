use std::fmt::Display;

use super::*;

#[derive(Clone, Debug)]
pub enum Type<'src> {
    Atomic(Atomic, SimpleSpan),
    Array(ArrayType<'src>),
    Function(FunctionType<'src>),
}

impl<'src> Type<'src> {
    pub fn get_span(&self) -> SimpleSpan {
        match self {
            Type::Atomic(.., span) => *span,
            Type::Array(ArrayType{ span, .. }) => *span,
            Type::Function(FunctionType { span, .. }) => *span,
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Atomic(Atomic::Boolean, ..))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Atomic(Atomic::Integer, ..))
    }
}

impl<'src> Display for Type<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Atomic(atomic, ..) =>  match atomic {
                    Atomic::Boolean => write!(f, "boolean"),
                    Atomic::Char => write!(f, "char"),
                    Atomic::Integer => write!(f, "integer"),
                    Atomic::String => write!(f, "string"),
                    Atomic::Void => write!(f, "void"),
            }
            Type::Array(a_type) => write!(f, "array [] `{}`", a_type.r#type),
            Type::Function(f_type) => {
                write!(f, "function {} (", f_type.return_type);
                for (index, param) in f_type.params.iter().enumerate() {
                    if index == 0 {
                        write!(f, "{}", param.r#type);
                    } else {
                        write!(f, ", {}", param.r#type);
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl<'src> PartialEq for Type<'src> {
    /// Equivalence for typechecking. Note that this implementation does not
    /// enforce that arrays must be of the same size for their type to be
    /// equivalent.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Atomic(type1, ..), Self::Atomic(type2, ..))
                => type1 == type2,
            (Self::Array(type1), Self::Array(type2))
                => type1.r#type == type2.r#type,
            (Self::Function(type1), Self::Function(type2))
                => type1.return_type == type2.return_type
                    && type1.params.len() == type2.params.len()
                    && type1.params.iter()
                        .zip(type2.params.iter())
                        .fold(true, |acc, (p1, p2)|
                            acc && p1.r#type == p2.r#type
                        ),
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ArraySize {
    Known(usize),
    Unknown,
}

#[derive(Clone, Debug)]
pub struct ArrayType<'src> {
    pub r#type: Box<Type<'src>>,
    pub size: ArraySize,
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
