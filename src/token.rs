pub enum Token<'src> {
	/// `[`
	BraceLeft,
	/// `]`
	BraceRight,
	/// `:`
	Colon,
	/// `{`
	CurlyLeft,
	/// `}`
	CurlyRight,
	/// variable or function identifier
	Ident(&'src str),
	/// ASCII char
	LitChar(char),
	/// 64-bit int literal
	LitInt(i64),
	/// immutable string value
	LitString(&'src str),
	/// keyword `array`
	KwArray,
	/// keyword `boolean`
	KwBoolean,
	/// keyword `char`
	KwChar,
	/// keyword `else`
	KwElse,
	/// keyword `false`
	KwFalse,
	/// keyword `for`
	KwFor,
	/// keyword `function`
	KwFunction,
	/// keyword `if`
	KwIf,
	/// keyword `integer`
	KwInteger,
	/// keyword `print`
	KwPrint,
	/// keyword `return`
	KwReturn,
	/// keyword `string`
	KwString,
	/// keyword `true`
	KwTrue,
	/// keyword `void`
	KwVoid,
	/// `+`
	OpAdd,
	/// `&&`
	OpAnd,
	/// `=`
	OpAssign,
	/// `--`
	OpDec,
	/// `/`
	OpDiv,
	/// `==`
	OpEq,
	/// `^`
	OpExp,
	/// `>`
	OpGreater,
	/// `>=`
	OpGreaterEq,
	/// `++`
	OpInc,
	/// `<`
	OpLess,
	/// `<=`
	OpLessEq,
	/// `%`
	OpMod,
	/// `*`
	OpMul,
	/// `!`
	OpNot,
	/// `!=`
	OpNotEq,
	/// `||`
	OpOr,
	/// `-`
	OpSub,
	/// `(`
	ParenLeft,
	/// `)`
	ParenRight,
	/// `;`
	Semicolon,
}