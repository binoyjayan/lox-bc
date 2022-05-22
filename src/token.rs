pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: &str, line: usize) -> Self {
        Self {
            ttype,
            lexeme: lexeme.to_string(),
            line,
        }
    }
}

impl Clone for Token {
    fn clone(&self) -> Self {
        Self::new(self.ttype, &self.lexeme, self.line)
    }
}

impl Default for Token {
    fn default() -> Self {
        Self::new(TokenType::Undefined, "", 0)
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen = 0,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    StringLiteral,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    Eof,
    Undefined,
    NumberOfTokens,
}
