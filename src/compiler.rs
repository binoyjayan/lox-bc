use std::cell::RefCell;

use crate::chunk::*;
use crate::error::*;
use crate::precedence::*;
use crate::scanner::*;
use crate::token::*;
use crate::value::*;

pub struct Compiler<'a> {
    parser: Parser,
    scanner: Scanner,
    chunk: &'a mut Chunk,
    rules: Vec<ParseRule>,
}

#[derive(Default)]
pub struct Parser {
    current: Token,
    previous: Token,
    had_error: RefCell<bool>,
    panic_mode: RefCell<bool>,
}

/*
 * A Pratt Parser
 *
 * There exist a function for each grammar production, namely:
 * number(), grouping(), unary(), binary() etc.
 *
 * ParseRule helps us find the following, given a token type:
 *  - The function to compile a prefix expression starting with
 *    a token of that type
 *  - The function to compile an infix expression whose left operand
 *    is followed by a token of that type and
 *  - The precedence of an infix expression that uses that token
 *    as an operator
 * The three properties above is wrapped in the struct 'ParseRule'
 * and represents a single row in the parser table.
 */

#[derive(Copy, Clone)]
pub struct ParseRule {
    prefix: Option<fn(&mut Compiler)>,
    infix: Option<fn(&mut Compiler)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Compiler)>,
        infix: Option<fn(&mut Compiler)>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

impl Default for ParseRule {
    fn default() -> Self {
        Self::new(None, None, Precedence::None)
    }
}

impl<'a> Compiler<'a> {
    fn create_rules() -> Vec<ParseRule> {
        let mut rules: Vec<ParseRule> =
            vec![ParseRule::default(); TokenType::NumberOfTokens as usize];
        rules[TokenType::LeftParen as usize] =
            ParseRule::new(Some(|c| c.grouping()), None, Precedence::None);
        rules[TokenType::RightParen as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::LeftBrace as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::RightBrace as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Comma as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Dot as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Minus as usize] =
            ParseRule::new(Some(|c| c.unary()), Some(|c| c.binary()), Precedence::Term);
        rules[TokenType::Plus as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Term);
        rules[TokenType::Semicolon as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Slash as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Factor);
        rules[TokenType::Star as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Factor);
        rules[TokenType::Bang as usize] =
            ParseRule::new(Some(|c| c.unary()), None, Precedence::None);
        rules[TokenType::BangEqual as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Equality);
        rules[TokenType::Equal as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::EqualEqual as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Equality);
        rules[TokenType::Greater as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison);
        rules[TokenType::GreaterEqual as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison);
        rules[TokenType::Less as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison);
        rules[TokenType::LessEqual as usize] =
            ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison);
        rules[TokenType::Identifier as usize] =
            ParseRule::new(Some(|c| c.variable()), None, Precedence::None);
        rules[TokenType::StringLiteral as usize] =
            ParseRule::new(Some(|c| c.string()), None, Precedence::None);
        rules[TokenType::Number as usize] =
            ParseRule::new(Some(|c| c.number()), None, Precedence::None);
        rules[TokenType::And as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Class as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Else as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::False as usize] =
            ParseRule::new(Some(|c| c.literal()), None, Precedence::None);
        rules[TokenType::For as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::For as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Fun as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::If as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Nil as usize] =
            ParseRule::new(Some(|c| c.literal()), None, Precedence::None);
        rules[TokenType::Or as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Print as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Return as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Super as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::This as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::True as usize] =
            ParseRule::new(Some(|c| c.literal()), None, Precedence::None);
        rules[TokenType::Var as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::While as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Error as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Eof as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Undefined as usize] = ParseRule::new(None, None, Precedence::None);

        rules
    }
    pub fn new(chunk: &'a mut Chunk) -> Self {
        Self {
            parser: Parser::default(),
            scanner: Scanner::new(""),
            chunk,
            rules: Self::create_rules(),
        }
    }

    pub fn compile(&mut self, source: &str) -> Result<(), InterpretResult> {
        self.scanner = Scanner::new(source);

        self.advance();
        while !self.matches(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::Eof, "Expect end of expression.");
        self.end_compiler();

        if *self.parser.had_error.borrow() {
            Err(InterpretResult::CompileError)
        } else {
            Ok(())
        }
    }

    pub fn advance(&mut self) {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token();
            if self.parser.current.ttype != TokenType::Error {
                break;
            }
            self.error_at_current(&self.parser.current.lexeme)
        }
    }

    pub fn consume(&mut self, ttype: TokenType, message: &str) {
        if self.parser.current.ttype == ttype {
            self.advance();
            return;
        }
        self.error_at_current(message);
    }

    fn check(&self, ttype: TokenType) -> bool {
        self.parser.current.ttype == ttype
    }

    fn matches(&mut self, ttype: TokenType) -> bool {
        if self.check(ttype) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.parser.previous.line)
    }

    fn emit_bytes(&mut self, byte1: Opcode, byte2: u8) {
        self.emit_byte(byte1.into());
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(Opcode::Return.into())
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        if let Some(constant) = self.chunk.add_constant(value) {
            constant
        } else {
            self.error("Too many constants in one chunk.");
            0
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(Opcode::Constant, constant);
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        #[cfg(feature = "debug_print_code")]
        if !*self.parser.had_error.borrow() {
            self.chunk.disassemble_chunk("code");
        }
    }

    /*
     * When a prefix parser is called, the leading token has already been consumed.
     * In a binary expression, the left operand gets compiled first and the value
     * ends up on the stack. When the binary operator is encountered, the function
     * compiles the right operand much like the unary() compiles the trailing operand.
     * Finally, the bytecode instruction that performs the binary operation is emitted.
     */
    fn binary(&mut self) {
        let operator_type = self.parser.previous.ttype;
        let rule = self.get_rule(operator_type).precedence.next();
        self.parse_precedence(rule);

        match operator_type {
            // 'a != b' is same as '!(a == b)'
            TokenType::BangEqual => self.emit_bytes(Opcode::Equal, Opcode::Not.into()),
            TokenType::EqualEqual => self.emit_byte(Opcode::Equal.into()),
            TokenType::Greater => self.emit_byte(Opcode::Greater.into()),
            // 'a >= b' is same as '!(a < b)'
            TokenType::GreaterEqual => self.emit_bytes(Opcode::Less, Opcode::Not.into()),
            TokenType::Less => self.emit_byte(Opcode::Less.into()),
            // 'a <= b' is same as '!(a > b)'
            TokenType::LessEqual => self.emit_bytes(Opcode::Greater, Opcode::Not.into()),
            TokenType::Plus => self.emit_byte(Opcode::Add.into()),
            TokenType::Minus => self.emit_byte(Opcode::Subtract.into()),
            TokenType::Star => self.emit_byte(Opcode::Multiply.into()),
            TokenType::Slash => self.emit_byte(Opcode::Divide.into()),
            _ => panic!("Unreachable"),
        }
    }

    fn literal(&mut self) {
        match self.parser.previous.ttype {
            TokenType::Nil => self.emit_byte(Opcode::Nil.into()),
            TokenType::True => self.emit_byte(Opcode::True.into()),
            TokenType::False => self.emit_byte(Opcode::False.into()),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self) {
        let value: f64 = self.parser.previous.lexeme.parse().unwrap();
        self.emit_constant(Value::Number(value));
    }

    // If lox supported escape sequences, it would have been translated here.
    fn string(&mut self) {
        // Remove quotes
        let len = self.parser.previous.lexeme.len() - 1;
        let value = self.parser.previous.lexeme[1..len].to_string();
        self.emit_constant(Value::Str(value));
    }

    fn named_variable(&mut self, name: &Token) {
        let arg = self.identifier_constant(name);
        self.emit_bytes(Opcode::GetGlobal, arg);
    }

    fn variable(&mut self) {
        self.named_variable(&self.parser.previous.clone())
    }

    fn unary(&mut self) {
        let operator_type = self.parser.previous.ttype;
        // compile the operand
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit_byte(Opcode::Negate.into()),
            TokenType::Bang => self.emit_byte(Opcode::Not.into()),
            _ => unimplemented!(),
        }
    }

    /* This function will parse any expression passed to it and
     * stop at an expression having an operator having precedence
     * higher than the one passed to it. e.g. if called with the
     * precedence 'Assignment' and if the expresion contains a 'Term'
     * operator, it will also parse that. Instead if a 'Unary' precedence
     * is passed, it will stop if an operator with a lower precedence
     * such as a 'Term' operator is encountered.
     *
     * Read the next token and lookup the corresponding parse rule.
     * If there is no prefix parser, then the token must be a syntax error.
     * Otherwise, call the prefix parse function to do its thing.
     * The prefix parser compiles the rest of the prefix expression,
     * consuming any other token that it needs.
     *
     * At the beginning of the parse_precedence(), lookup a prefix parser
     * for the current token. The first token will always belong to some
     * kind of prefix expression. It may turn out to be nested as an operand
     * inside one or more infix expressions but as the code is read from left
     * to right, the first token always belong to a prefix expression.
     *
     * After parsing that, which may consume more tokens, the prefix expression
     * is done. Now look for an infix parser for the next token. If one is found,
     * it means the prefix expression that was already compiled might be an operand
     * to the infix operator. But only if the call to parse_precedence() has a
     * precedence that is low enough to permit the infix operator.
     *
     * If the token is too low precedence, or isn't an infix operator at all,
     * the parsing is done. The expression is parsed as much as it could be.
     * Otherwise, consume the operator and hand off control to the infix parser
     * that was found. It consumes whatever other tokens it needs (usually the
     * right operand). and returns back to parse_precedence(). Then the loop
     * continues and see if the next token is also a valid infix operator that
     * can take the entire preceding expression as its operand. Continue the loop
     * crunching through infix operators and their operands until a token is hit
     * that isn't an infix operator or is too low precedence.
     *
     */

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        if let Some(prefix_rule) = self.get_rule(self.parser.previous.ttype).prefix {
            prefix_rule(self);
            while precedence <= self.get_rule(self.parser.current.ttype).precedence {
                self.advance();
                if let Some(infix_rule) = self.get_rule(self.parser.previous.ttype).infix {
                    infix_rule(self);
                }
            }
        } else {
            self.error("Expect expression.");
        }
    }

    /*
     * Global variables are looked up by name at runtime. Store the identifier
     * in the constant table and refer to it in instruction by its index.
     */
    fn identifier_constant(&mut self, name: &Token) -> u8 {
        self.make_constant(Value::Str(name.lexeme.clone()))
    }

    fn parse_variable(&mut self, err_msg: &str) -> u8 {
        self.consume(TokenType::Identifier, err_msg);
        self.identifier_constant(&self.parser.previous.clone())
    }

    /* Outputs the bytecode instruction that defines the new variable
     * and store its initial value. The index of the variable's name in
     * the constant table is the instruction's operand.
     */
    fn define_variable(&mut self, global: u8) {
        self.emit_bytes(Opcode::DefineGlobal, global);
    }

    fn get_rule(&self, ttype: TokenType) -> &ParseRule {
        &self.rules[ttype as usize]
    }

    // Start with parsing expression with lowest precedence level.
    // This subsumes all of the higher-precedence expressions too.
    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    /*
     * A var keyword is followed by a variable name that is compiled by 'parse_variable'.
     * Then look for an '=' followed by an initializer expression. Use 'nil' if the user
     * does not use an initializer. Expect the statement to be terminated with a semicolon.
     */
    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect a variable name.");
        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            // 'var a' is equivalent to 'var a = nil'
            self.emit_byte(Opcode::Nil.into());
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );
        self.define_variable(global);
    }

    /*
     * An expression statement is an expression followed by a semicolon.
     * Semantically, an expression statement evaluates the expression and
     * discards the result. The compiler directly encodes that behavior by
     * emiting a POP instruction so that the result is removed from the stack.
     */
    fn expr_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(Opcode::Pop.into());
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(Opcode::Print.into());
    }

    fn synchronize(&mut self) {
        self.parser.panic_mode.replace(false);
        while self.parser.current.ttype != TokenType::Eof {
            if self.parser.previous.ttype == TokenType::Semicolon {
                return;
            }
            if matches!(
                self.parser.current.ttype,
                TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return
            ) {
                return;
            }
            self.advance()
        }
    }

    fn declaration(&mut self) {
        if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }
        let paniced = *self.parser.panic_mode.borrow();
        if paniced {
            self.synchronize()
        }
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else {
            self.expr_statement();
        }
    }

    pub fn error_at_current(&self, message: &str) {
        self.error_at(&self.parser.current, message)
    }

    pub fn error(&self, message: &str) {
        self.error_at(&self.parser.previous, message);
    }

    pub fn error_at(&self, token: &Token, message: &str) {
        if *self.parser.panic_mode.borrow() {
            // Do not report errors until the parser synchronizes
            return;
        }
        self.parser.panic_mode.replace(true);
        eprint!("[line {}] Error", token.line);
        if token.ttype == TokenType::Eof {
            eprint!(" at end");
        } else if token.ttype == TokenType::Error {
            // Nothing
        } else {
            eprint!(" at {}", token.lexeme);
        }
        eprintln!(": {}", message);
        self.parser.had_error.replace(true);
    }
}
