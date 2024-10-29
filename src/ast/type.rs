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

pub enum Atomic {
	Boolean,
	Char,
	Integer,
	String,
	Void,
}