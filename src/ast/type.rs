use super::*;

/// Warning: The `Option<SimpleSpan>` must be the last field in ordered tuples
/// or else the `Spanned` trait will break.
pub enum Type<'src> {
	Atomic(Atomic, Option<SimpleSpan>),
	/// An array.
	/// 
	/// ### Fields
	/// 
	/// * `0` ([`Atomic`]) - the type of the elements in the array
	/// * `1` (`usize`) - size of the array; arrays of size 0 are illegal.
	Array(Atomic, usize, Option<SimpleSpan>),
	/// A function.
	/// 
	/// ### Fields
	/// 
	/// * `0` ([`Atomic`]) - the return type of the function
	/// * `1` (`Vec<(&str, Type)>`) - the parameters of the function (name, type)
	Function(Atomic, Vec<(&'src str, Type<'src>)>, Option<SimpleSpan>),
}

impl PartialEq for Type<'_> {
	/// Checks types are the same for semantic typechecking.
	/// 
	/// Currently, the sizes of two arrays don't have to be the same for their
	/// types to be equal -- this allows functions returning or accepting arrays
	/// to not be forced to specify one size of array.
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Type::Atomic(left, _), Type::Atomic(right, _)) => {
				left == right
			}
			(Type::Array(left, ..), Type::Array(right, ..)) => {
				left == right
			}
			(Type::Function(left, left_params, _),
				Type::Function(right, right_params, _)) =>
			{
				left == right && left_params.iter()
					.zip(right_params.iter())
					.fold(true, |eq, (left, right)| eq && (left == right))
			}
			_ => false,
		}
	}
}

impl Spanned for Type<'_> {
	fn set_span(&mut self, span: SimpleSpan) {
		let s = match self {
			Type::Atomic(.., s) => s,
			Type::Array(.., s) => s,
			Type::Function(.., s) => s,
		};

		*s = Some(span);
	}
}

#[derive(PartialEq)]
pub enum Atomic {
	Boolean,
	Char,
	Integer,
	String,
	Void,
}