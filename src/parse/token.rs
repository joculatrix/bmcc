#[derive(Clone, PartialEq)]
pub enum Token<'src> {
    /// `[`
    BraceLeft,
    /// `]`
    BraceRight,
    /// ','
    Comma,
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
    /// keywords
    Keyword(Keyword),
    /// operators
    Operator(Op),
    /// `(`
    ParenLeft,
    /// `)`
    ParenRight,
    /// `;`
    Semicolon,
}

#[derive(Clone, PartialEq)]
pub enum Keyword {
    Array,
    Boolean,
    Char,
    Else,
    False,
    For,
    Function,
    If,
    Integer,
    Print,
    Return,
    r#String,
    True,
    Void,
    While,
}

#[derive(Clone, PartialEq)]
pub enum Op {
    /// `+`
    Add,
    /// `&&`
    And,
    /// `=`
    Assign,
    /// `--`
    Dec,
    /// `/`
    Div,
    /// `==`
    Eq,
    /// `^`
    Exp,
    /// `>`
    Greater,
    /// `>=`
    GreaterEq,
    /// `++`
    Inc,
    /// `<`
    Less,
    /// `<=`
    LessEq,
    /// `%`
    Mod,
    /// `*`
    Mul,
    /// `!`
    Not,
    /// `!=`
    NotEq,
    /// `||`
    Or,
    /// `-`
    Sub,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::BraceLeft => write!(f, "`[`"),
            Token::BraceRight => write!(f, "`]`"),
            Token::Comma => write!(f, "`,`"),
            Token::Colon => write!(f, "`:`"),
            Token::CurlyLeft => write!(f, "`{{`"),
            Token::CurlyRight => write!(f, "`}}`"),
            Token::Ident(..) => write!(f, "identifier"),
            Token::LitChar(..) => write!(f, "char literal"),
            Token::LitInt(..) => write!(f, "integer literal"),
            Token::LitString(..) => write!(f, "string literal"),
            Token::Keyword(k, ..) => match k {
                Keyword::Array => write!(f, "`array`"),
                Keyword::Boolean => write!(f, "`boolean`"),
                Keyword::Char => write!(f, "`char`"),
                Keyword::Else => write!(f, "`else`"),
                Keyword::False => write!(f, "`false`"),
                Keyword::For => write!(f, "`for`"),
                Keyword::Function => write!(f, "`function`"),
                Keyword::If => write!(f, "`if`"),
                Keyword::Integer => write!(f, "`integer`"),
                Keyword::Print => write!(f, "`print`"),
                Keyword::Return => write!(f, "`return`"),
                Keyword::r#String => write!(f, "`string`"),
                Keyword::True => write!(f, "`true`"),
                Keyword::Void => write!(f, "`void`"),
                Keyword::While => write!(f, "`while`"),
            },
            Token::Operator(op, ..) => match op {
                Op::Add => write!(f, "`+`"),
                Op::And => write!(f, "`&&`"),
                Op::Assign => write!(f, "`=`"),
                Op::Dec => write!(f, "`--`"),
                Op::Div => write!(f, "`/`"),
                Op::Eq => write!(f, "`==`"),
                Op::Exp => write!(f, "`^`"),
                Op::Greater => write!(f, "`>`"),
                Op::GreaterEq => write!(f, "`>=`"),
                Op::Inc => write!(f, "`++`"),
                Op::Less => write!(f, "`<`"),
                Op::LessEq => write!(f, "`<=`"),
                Op::Mod => write!(f, "`%`"),
                Op::Mul => write!(f, "`*`"),
                Op::Not => write!(f, "`!`"),
                Op::NotEq => write!(f, "`!=`"),
                Op::Or => write!(f, "`||`"),
                Op::Sub => write!(f, "`-`"),
            },
            Token::ParenLeft => write!(f, "`(`"),
            Token::ParenRight => write!(f, "`)`"),
            Token::Semicolon => write!(f, "`;`"),
        }
    }
}

