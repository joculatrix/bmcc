pub enum Type<'src> {
	Atomic(Atomic),
	/// An array.
	/// 
	/// ### Fields
	/// 
	/// * `0` ([`Atomic`]) - the type of the elements in the array
	/// * `1` (`usize`) - size of the array; arrays of size 0 are illegal.
	Array(Atomic, usize),
	/// A function.
	/// 
	/// ### Fields
	/// 
	/// * `0` ([`Atomic`]) - the return type of the function
	/// * `1` (`Vec<(&str, Type)>`) - the parameters of the function (name, type)
	Function(Atomic, Vec<(&'src str, Type<'src>)>),
}

impl PartialEq for Type<'_> {
	/// Checks types are the same for semantic typechecking.
	/// 
	/// Currently, the sizes of two arrays don't have to be the same for their
	/// types to be equal -- this allows functions returning or accepting arrays
	/// to not be forced to specify one size of array.
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Type::Atomic(left), Type::Atomic(right)) => {
				left == right
			}
			(Type::Array(left, _), Type::Array(right, _)) => {
				left == right
			}
			(Type::Function(left, left_params),
				Type::Function(right, right_params)) =>
			{
				left == right && left_params.iter()
					.zip(right_params.iter())
					.fold(true, |eq, (left, right)| eq && (left == right))
			}
			_ => false,
		}
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