use crate::token::*;

pub struct Scanner {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect::<Vec<char>>(),
            start: 0,
            current: 0,
            line: 0,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();
        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                let ne = self.matches('=');
                self.make_token(if ne {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            '=' => {
                let eq = self.matches('=');
                self.make_token(if eq {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '<' => {
                let lt = self.matches('=');
                self.make_token(if lt {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            '>' => {
                let gt = self.matches('=');
                self.make_token(if gt {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                })
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            _ if c.is_alphabetic() || c == '_' => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }

    pub fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        // A comment goes until end of the line
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => {
                    return;
                }
            }
        }
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            return self.error_token("Unterminated string");
        }
        self.advance();
        self.make_token(TokenType::StringLiteral)
    }

    fn number(&mut self) -> Token {
        while self.peek().is_digit(10) {
            self.advance();
        }
        // Look for fraction
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            // consume '.'
            self.advance();
            while self.peek().is_digit(10) {
                self.advance();
            }
        }
        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token {
        while self.peek_is_identifier() {
            self.advance();
        }
        let ttype = self.identifier_type();
        self.make_token(ttype)
    }

    fn peek_is_identifier(&self) -> bool {
        let c = self.peek();
        c.is_alphabetic() || c.is_digit(10) || c == '_'
    }

    fn identifier_type(&mut self) -> TokenType {
        match self.source[self.start] {
            'a' => return self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => return self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => return self.check_keyword(1, 3, "lse", TokenType::Else),
            'i' => return self.check_keyword(1, 1, "f", TokenType::If),
            'f' => {
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        'a' => return self.check_keyword(2, 3, "lse", TokenType::False),
                        'o' => return self.check_keyword(2, 1, "r", TokenType::For),
                        'u' => return self.check_keyword(2, 1, "n", TokenType::Fun),
                        _ => {}
                    }
                }
            }
            'n' => return self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => return self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => return self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => return self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => return self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => {
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        'h' => return self.check_keyword(2, 2, "is", TokenType::This),
                        'r' => return self.check_keyword(2, 2, "ue", TokenType::True),
                        _ => {}
                    }
                }
            }
            'v' => return self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => return self.check_keyword(1, 4, "hile", TokenType::While),
            _ => {}
        }
        TokenType::Identifier
    }

    fn check_keyword(
        &mut self,
        start: usize,
        length: usize,
        rest: &str,
        ttype: TokenType,
    ) -> TokenType {
        if self.current - self.start == start + length {
            let rest_str: String = self.source[self.start + start..self.current]
                .iter()
                .collect();
            if rest_str == rest {
                return ttype;
            }
        }
        TokenType::Identifier
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    fn make_token(&self, ttype: TokenType) -> Token {
        Token {
            ttype,
            lexeme: self.source[self.start..self.current].iter().collect(),
            line: self.line,
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            ttype: TokenType::Error,
            lexeme: message.to_string(),
            line: self.line,
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }
}
