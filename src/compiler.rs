use std::cell::RefCell;
use std::rc::Rc;

use crate::chunk::*;
use crate::error::*;
use crate::function::*;
use crate::precedence::*;
use crate::scanner::*;
use crate::token::*;
use crate::value::*;

pub struct Compiler {
    rules: Vec<ParseRule>,
    parser: Parser,
    scanner: Scanner,
    result: RefCell<CompileResult>,
}

#[derive(Default)]
struct CompileResult {
    chunk: RefCell<Chunk>,
    locals: RefCell<Vec<Local>>,
    scope_depth: RefCell<usize>,
    arity: RefCell<usize>,
    current_function: RefCell<String>,
}

#[derive(Debug)]
enum FindResult {
    Uninitialized,
    NotFound,
    Depth(u8),
}

impl CompileResult {
    fn new<T: ToString>(name: T) -> Self {
        Self {
            current_function: RefCell::new(name.to_string()),
            ..Default::default()
        }
    }

    fn get_arity(&self) -> usize {
        *self.arity.borrow()
    }

    fn inc_arity(&self, offset: usize) -> usize {
        *self.arity.borrow_mut() += offset;
        *self.arity.borrow()
    }

    fn locals_len(&self) -> usize {
        self.locals.borrow().len()
    }

    fn find_variable(&self, name: &str) -> FindResult {
        for (e, v) in self.locals.borrow().iter().rev().enumerate() {
            if v.name.lexeme == *name {
                if v.depth.is_none() {
                    return FindResult::Uninitialized;
                }
                return FindResult::Depth((self.locals.borrow().len() - e - 1) as u8);
            }
        }
        FindResult::NotFound
    }

    fn in_scope(&self) -> bool {
        *self.scope_depth.borrow() != 0
    }

    fn set_local_scope(&self) {
        // let len = self.locals.borrow_mut().len();
        // self.locals.borrow_mut()[len - 1].depth = Some(*self.scope_depth.borrow());
        let last = self.locals.borrow().len() - 1;
        let mut locals = self.locals.borrow_mut();
        locals[last].depth = Some(*self.scope_depth.borrow());
    }

    fn is_scope_poppable(&self) -> bool {
        self.locals.borrow().len() > 0
            && self.locals.borrow().last().unwrap().depth.unwrap() > *self.scope_depth.borrow()
    }

    fn inc_scope(&self, offset: usize) {
        *self.scope_depth.borrow_mut() += offset;
    }

    fn dec_scope(&self, offset: usize) {
        *self.scope_depth.borrow_mut() -= offset;
    }

    fn push(&self, local: Local) {
        self.locals.borrow_mut().push(local)
    }

    fn pop(&self) {
        self.locals.borrow_mut().pop();
    }

    fn write_byte(&self, byte: u8, line: usize) {
        self.chunk.borrow_mut().write_byte(byte, line)
    }

    fn count(&self) -> usize {
        self.chunk.borrow().count()
    }

    fn add_constant(&self, value: Value) -> Option<u8> {
        self.chunk.borrow_mut().add_constant(value)
    }

    fn write_at(&self, offset: usize, byte: u8) {
        self.chunk.borrow_mut().write_at(offset, byte)
    }

    #[cfg(feature = "debug_print_code")]
    fn disassemble_chunk<T: ToString>(&self, name: T) {
        self.chunk.borrow().disassemble_chunk(name)
    }
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
    prefix: Option<fn(&mut Compiler, bool)>,
    infix: Option<fn(&mut Compiler, bool)>,
    precedence: Precedence,
}

pub struct Local {
    name: Token,
    depth: Option<usize>,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Compiler, bool)>,
        infix: Option<fn(&mut Compiler, bool)>,
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

impl Compiler {
    fn create_rules() -> Vec<ParseRule> {
        let mut rules: Vec<ParseRule> =
            vec![ParseRule::default(); TokenType::NumberOfTokens as usize];
        rules[TokenType::LeftParen as usize] =
            ParseRule::new(Some(Compiler::grouping), None, Precedence::None);
        rules[TokenType::RightParen as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::LeftBrace as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::RightBrace as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Comma as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Dot as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Minus as usize] = ParseRule::new(
            Some(Compiler::unary),
            Some(Compiler::binary),
            Precedence::Term,
        );
        rules[TokenType::Plus as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Term);
        rules[TokenType::Semicolon as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Slash as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Factor);
        rules[TokenType::Star as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Factor);
        rules[TokenType::Bang as usize] =
            ParseRule::new(Some(Compiler::unary), None, Precedence::None);
        rules[TokenType::BangEqual as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Equality);
        rules[TokenType::Equal as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::EqualEqual as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Equality);
        rules[TokenType::Greater as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison);
        rules[TokenType::GreaterEqual as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison);
        rules[TokenType::Less as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison);
        rules[TokenType::LessEqual as usize] =
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison);
        rules[TokenType::Identifier as usize] =
            ParseRule::new(Some(Compiler::variable), None, Precedence::None);
        rules[TokenType::StringLiteral as usize] =
            ParseRule::new(Some(Compiler::string), None, Precedence::None);
        rules[TokenType::Number as usize] =
            ParseRule::new(Some(Compiler::number), None, Precedence::None);
        rules[TokenType::And as usize] = ParseRule::new(
            Some(Compiler::literal),
            Some(Compiler::and),
            Precedence::And,
        );
        rules[TokenType::Class as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Else as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::False as usize] =
            ParseRule::new(Some(Compiler::literal), None, Precedence::None);
        rules[TokenType::For as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::For as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Fun as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::If as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Nil as usize] =
            ParseRule::new(Some(Compiler::literal), None, Precedence::None);
        rules[TokenType::Or as usize] =
            ParseRule::new(Some(Compiler::literal), Some(Compiler::or), Precedence::Or);
        rules[TokenType::Print as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Return as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Super as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::This as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::True as usize] =
            ParseRule::new(Some(Compiler::literal), None, Precedence::None);
        rules[TokenType::Var as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::While as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Error as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Eof as usize] = ParseRule::new(None, None, Precedence::None);
        rules[TokenType::Undefined as usize] = ParseRule::new(None, None, Precedence::None);

        rules
    }
    pub fn new() -> Self {
        Self {
            rules: Self::create_rules(),
            parser: Parser::default(),
            scanner: Scanner::new(""),
            result: RefCell::new(CompileResult::default()),
        }
    }

    pub fn compile(&mut self, source: &str) -> Result<Function, InterpretResult> {
        self.result.borrow().push(Local {
            name: Token::default(),
            depth: Some(0),
        });

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
            let result = self.result.replace(CompileResult::default());
            let chunk = result.chunk.replace(Chunk::new());
            Ok(Function::toplevel(&Rc::new(chunk)))
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
        self.result
            .borrow()
            .write_byte(byte, self.parser.previous.line)
    }

    fn emit_bytes(&mut self, byte1: Opcode, byte2: u8) {
        self.emit_byte(byte1.into());
        self.emit_byte(byte2);
    }

    /*
     * The funtion emits a loop instruction, which unconditionally jumps backward
     * by a given offset. Like the jump instructions, after that we have a 16-bit
     * operand. We calculate the offset from the instruction we're currently at to
     * the loop_start point that we want to jump back to. The '+2' is to take into
     * account the size of the OP_LOOP instruction's own operands which we also
     * need to jump over.
     */
    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(Opcode::Loop.into());
        let offset = self.result.borrow().count() - loop_start + 2;
        if offset > u16::MAX.into() {
            self.error("Loop body too large.");
        }
        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    /*
     * emit a bytecode instruction and a placeholder operand for the jump offset.
     * Pass in the opcode as an argument because, there are two different
     * instructions that uses this helper.
     */
    fn emit_jump(&mut self, instruction: Opcode) -> usize {
        self.emit_byte(instruction.into());
        // Two byte operand allows upto 65,535 bytes of code.
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.result.borrow().count() - 2
    }

    fn emit_return(&mut self) {
        self.emit_byte(Opcode::Return.into())
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        if let Some(constant) = self.result.borrow().add_constant(value) {
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

    fn patch_jump(&mut self, offset: usize) {
        // subtract -2 to adjust for the bytecode for the jump offset itself
        let jump = self.result.borrow().count() - offset - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump over.");
        }

        self.result
            .borrow()
            .write_at(offset, ((jump >> 8) & 0xff) as u8);
        self.result
            .borrow()
            .write_at(offset + 1, (jump & 0xff) as u8);
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        #[cfg(feature = "debug_print_code")]
        {
            let temp_name = self.result.borrow().current_function.borrow().clone();
            let function_name = if temp_name.is_empty() {
                "<script>".to_string()
            } else {
                temp_name
            };
            if !*self.parser.had_error.borrow() {
                self.result.borrow().disassemble_chunk(function_name);
            }
        }
    }

    /*
     * Scopes are created by block statements
     * In order to create a scope, increment the current depth.
     * While ending a scope, walk backward through the local
     * array looking for any variables declared at the scope depth
     * we just left. Discard them by popping elements.
     *
     * Local variables also occupy slots on the stack. When a local
     * variable goes out of scope, that slot is no longer needed and
     * should be freed. For each variable that is discarded, also emit
     * a Pop instruction.
     */
    fn begin_scope(&mut self) {
        self.result.borrow().inc_scope(1);
    }

    fn end_scope(&mut self) {
        self.result.borrow().dec_scope(1);
        while self.result.borrow().is_scope_poppable() {
            self.emit_byte(Opcode::Pop.into());
            self.result.borrow().pop();
        }
    }

    /*
     * When a prefix parser is called, the leading token has already been consumed.
     * In a binary expression, the left operand gets compiled first and the value
     * ends up on the stack. When the binary operator is encountered, the function
     * compiles the right operand much like the unary() compiles the trailing operand.
     * Finally, the bytecode instruction that performs the binary operation is emitted.
     */
    fn binary(&mut self, _can_assign: bool) {
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

    fn literal(&mut self, _can_assign: bool) {
        match self.parser.previous.ttype {
            TokenType::Nil => self.emit_byte(Opcode::Nil.into()),
            TokenType::True => self.emit_byte(Opcode::True.into()),
            TokenType::False => self.emit_byte(Opcode::False.into()),
            _ => unreachable!(),
        }
    }

    // Parenthesized grouping expression. Parsing function for an expression
    // type can consume any additional token that it wants to, just like in
    // a regular recursive descent parser. A Pratt parser isn't a recursive descent
    // parser but it is still recursive.
    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self, _can_assign: bool) {
        let value: f64 = self.parser.previous.lexeme.parse().unwrap();
        self.emit_constant(Value::Number(value));
    }

    /* In an or expression, if the left-hand side is truthy, then skip over the
     * right operand. Thus we need to jump when a value is truthy.
     * When the left-hand side is falsey, do a tiny jump over the next statement.
     * That statement is an unconditional jump over the code for the right operand.
     *
     * Control Flow:
     *
     * left operand expression
     * OP_JUMP_IF_FALSE  ---+
     * OP_JUMP      --------|------+
     * OP_POP           <---+      |
     * right operand expression    |
     * continue         <----------+
     */
    fn or(&mut self, _can_assign: bool) {
        // jump around the jump
        let else_jump = self.emit_jump(Opcode::JumpIfFalse);
        // jump to end of the expression
        let end_jump = self.emit_jump(Opcode::Jump);
        self.patch_jump(else_jump);
        self.emit_byte(Opcode::Pop.into());
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    // If lox supported escape sequences, it would have been translated here.
    fn string(&mut self, _can_assign: bool) {
        // Remove quotes
        let len = self.parser.previous.lexeme.len() - 1;
        let value = self.parser.previous.lexeme[1..len].to_string();
        self.emit_constant(Value::Str(value));
    }

    /*
     * While resolving references, check the scope depth to see
     * if the variable is fully defined. If scope depth is none,
     * it means that the variable is in the process of initialization.
     */
    fn resolve_local(&self, name: &Token) -> Option<u8> {
        return match self.result.borrow().find_variable(&name.lexeme) {
            FindResult::Uninitialized => {
                self.error("Can't read local variable in its own initializer.");
                None
            }
            FindResult::NotFound => None,
            FindResult::Depth(d) => Some(d),
        };
    }
    /*
     * In the parse function for identifier expression, look for an equals sign
     * right after the identifier. If found one, instead of emitting code for
     * a variable access, compile the assigned value (which is an expression itself)
     * and emit an assignment instruction.
     *
     * RE: assignment statements, if the variable is nested inside some expression
     * with higher precedence (that needs to be evaluated first), can_assign will be
     * false and this will ignore the '=' even if there is one. named_variable returns
     * control to parse_precedence() without the '=' consumed, so that will be the
     * current token. Now the prefix parser has returned. Now parse_precedence()
     * enters the infix parser loop and since there is no infix parsing function
     * associated with '=', it skips the loop. parse_precedence() returns to the
     * caller silently without consuming the '='. Report error in that case.
     */
    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let (arg, get_op, set_op) = if let Some(local_arg) = self.resolve_local(name) {
            (local_arg, Opcode::GetLocal, Opcode::SetLocal)
        } else {
            (
                self.identifier_constant(name),
                Opcode::GetGlobal,
                Opcode::SetGlobal,
            )
        };

        // Don't consume '=' if can_assign is false
        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op, arg);
        } else {
            self.emit_bytes(get_op, arg);
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(&self.parser.previous.clone(), can_assign)
    }

    fn unary(&mut self, _can_assign: bool) {
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
     * Special note about assignment statements:
     *
     * To avoid statements such as 'a * b = c + d;'
     *
     * Looks for and consume '=' only if it's in the context of a low-precedence
     * expression. The 'variable()' function doesn't need to know the actual level.
     * It just cares that the precedence is low enough to allow assignment, so pass
     * a boolean to indicate that. Since assignment is the lowest-precedence expression,
     * the only time it is allowed is when parsing an assignment expression or top-level
     * expression like an expression statement.
     */

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        if let Some(prefix_rule) = self.get_rule(self.parser.previous.ttype).prefix {
            // Allow variable assignment only if precedence is <= that of assignment
            let can_assign = precedence <= Precedence::Assignment;

            prefix_rule(self, can_assign);
            while precedence <= self.get_rule(self.parser.current.ttype).precedence {
                self.advance();
                if let Some(infix_rule) = self.get_rule(self.parser.previous.ttype).infix {
                    infix_rule(self, can_assign);
                }
            }
            // Report error if the '=' was not consumed.
            if can_assign && self.matches(TokenType::Equal) {
                self.error("Invalid assignment target.");
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

    /*
     * Add the local variable to the compiler's list of variables
     * in the current scope.
     *
     * This initializes the next available local in the compiler's
     * array of variables. It stores the variable's name and the depth
     * of the scope that owns the variable. Also handle error cases.
     * The first error to handle is a limitation of the VM. The instructions
     * to work with local variables refer to them by slot index. That index
     * is stored in a single-byte operand, which means the VM only supports
     * up to 256 local variables in scope at one time.
     */
    fn add_local(&self, name: &Token) {
        if self.result.borrow().locals_len() >= 256 {
            self.error("Too many local variables in function.");
            return;
        }
        let local = Local {
            name: name.clone(),
            // variable is not defined fully (yet to compile initializer)
            depth: None,
        };
        self.result.borrow().push(local);
    }

    /*
     * This is the point where the compiler records the existence of
     * a local variable. This is only done for local but not globals
     * since globals are late bound (resolved at runtime using by a
     * a lookup in the hash table). Because of this, the compiler doesn't
     * keep track of which declarations for them it has seen.
     *
     * But for local variables, the compiler does not need to remember
     * that the variable exists. That's what declaring does - adding it
     * to the compiler's list of variables in the current scope. It is done
     * using the function 'add_local()'
     */
    fn declare_variable(&mut self) {
        if !self.result.borrow().in_scope() {
            return;
        }
        let name = self.parser.previous.lexeme.clone();
        // count number of matches
        if let FindResult::Depth(_d) = self.result.borrow().find_variable(&name) {
            self.error("Already a variable with this name in this scope.");
        } else {
            self.add_local(&self.parser.previous);
        }
    }

    /*
     * Variable declaration parsing begins in var_declaration() and relies on a couple
     * of other functions. First, parse_variable() consumes the identifier token for
     * the variable name, adds its lexeme to the chunk's constant table as a string,
     * and then returns the constant table index where it was added. Then, after
     * var_declaration() compiles the initializer, it calls define_variable() to emit
     * the bytecode for storing the variable's value in the global variable hash table.
     *
     * var_declaration() --> CALL --> parse_variable() --> CONSTANT -->
     *
     * COMPILER INITIALIZER --> CALL --> define_variable()
     *
     * First "declare" the variable after consuming the identifier token.
     * Exit the function if we're in a local scope. At runtime, locals aren't
     * looked up by name. There is no need to stuff the variable name into the
     * constant table, so if the declaration is inside a local scope, we return
     * a dummy table index instead.
     */
    fn parse_variable(&mut self, err_msg: &str) -> u8 {
        self.consume(TokenType::Identifier, err_msg);
        self.declare_variable();

        // Exit function if inside local scope
        if self.result.borrow().in_scope() {
            return 0;
        }
        self.identifier_constant(&self.parser.previous.clone())
    }

    /*
     * Mark local variables as initialized. A scope depth of 0 means it
     * is called for a global (function) declaration. If so do nothing,
     * as the function is bound to a global variable.
     */
    fn mark_initialized(&mut self) {
        if self.result.borrow().in_scope() {
            self.result.borrow().set_local_scope();
        }
    }

    /* Outputs the bytecode instruction that defines the new variable
     * and store its initial value. The index of the variable's name in
     * the constant table is the instruction's operand.
     *
     * There is no need to create a local variable at runtime. The VM now
     * has already executed code for the variable's initializer (or the
     * implicit nil), and that value is sitting on top of the stack as the
     * only remaining temporary. The temporary becomes the local variable.
     */
    fn define_variable(&mut self, global: u8) {
        if self.result.borrow().in_scope() {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(Opcode::DefineGlobal, global);
    }

    /* At the point when this function is called, the left-hand side expression
     * has already been compiled. That means, at runtime, its value will be on
     * top of the stack. If that value is falsey, then the entire expression
     * must be false, so skip the right operand and leave the left-hand side
     * value as the result of the entire expression. Otherwise, discard the
     * left-hand value and evaluate the right operand which becomes the
     * result of the whole 'and' expression.
     *
     * Control Flow:
     * left operand expression
     * OP_JUMP_IF_FALSE     ------+
     * OP_POP                     |
     * right operand expression   |
     * continue            <------+
     */
    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Pop.into());
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn get_rule(&self, ttype: TokenType) -> &ParseRule {
        &self.rules[ttype as usize]
    }

    // Start with parsing expression with lowest precedence level.
    // This subsumes all of the higher-precedence expressions too.
    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    /*
     * Parse a function or a method
     */
    fn function(&mut self, _chunk_type: ChunkType) {
        let function_name = self.parser.previous.lexeme.clone();
        let prev_compile_result = self.result.replace(CompileResult::new(function_name));

        // Begin scope does not have a corresponding end scope since the compile result
        // itself ends upon reaching the end of the function body
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        /* Compile parameters until a right parentheses is found. Semantically,
         * a parameter is simply a local variable declared in the outermost lexical scope
         * scope of the of the body of a function. Unlike local variables, there are no
         * initializers for the parametersas part of the function declaration.
         */

        if !self.check(TokenType::RightParen) {
            loop {
                if self.result.borrow().inc_arity(1) > 255 {
                    self.error_at_current("Can't have more than 255 parameters.")
                }
                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);
                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' after function body.");

        self.block();

        self.end_compiler();

        let result = self.result.replace(prev_compile_result);

        let function_arity = self.result.borrow().get_arity();
        let function_name = result.current_function.borrow().clone();

        if !*self.parser.had_error.borrow() {
            let chunk = result.chunk.replace(Chunk::new());
            let function = Function::new(function_arity, &Rc::new(chunk), function_name);
            let constant = self.make_constant(Value::Func(function));
            self.emit_bytes(Opcode::Constant, constant);
        }
    }

    /*
     * Functions are first-class values and are parsed like any other variable
     * declaration. A function declaration creates and stores one in a newly
     * declared variable. A function declaration at the top level will bind
     * the function to a global variable. Inside a block or another function,
     * a function declaration creates a local variable.
     *
     * The helper 'function()' compiles the functions and leaves the object
     * on stack. 'define_variable()', then stores the function back into
     * the variable that was defined before. A helper is used here because
     * it is also reused to compile class methods.
     */
    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect a function name.");
        self.mark_initialized();
        self.function(ChunkType::Function);
        self.define_variable(global);
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

    /*
     * In the for loop condition, see if it is actually present. If the clause
     * is omitted, the next token is the ';'. If there is a condition, compile it.
     * Then, just like a while loop, emit a conditional jump that exits the loop
     * if the condition is falsey. Since the jump leaves the value on the stack,
     * pop the value before executing the body. This ensures that the value is
     * discarded when the condition is true.
     *
     * The increment clause is a bit convoluted. It appears before the body but
     * executes after it. The increment clause cannot be compiled later, since
     * the compiler only makes a single pass over the code. Instead, 'jump' over
     * the increment, run the body, jump back up to the increment, run it, and
     * then go to the next iteration of the loop. When an increment clause is
     * present, it needs to be compiled but not executed yet. So, first emit an
     * unconditional jump that hops over the increment clause's code to the body
     * of the loop. Next, compile the increment expression itself. Also, emit a
     * pop to discard the result of the increment clause. As the last step, emit
     * a loop instruction. This is the main loop that takes the control back to
     * the top of the for loop - right before the condition expression if there
     * is one. That loop happens right after the increment, since the increment
     * executes at the end of each loop iteration. Finally, change the loop_start
     * to point to the offset where the increment expression begins. Later, when
     * the loop instruction is emitted, after the body statement, this will cause
     * it to jump up to the increment expression instead of the top of the loop
     * like it does when there is no increment.
     *
     * Control Flow:
     *
     * initializer clause
     *
     * condition expression   <------------+
     *                                     |
     * OP_JUMP_IF_FALSE  ------+           |
     * OP_POP                  |           |
     * OP_JUMP          -------|-----+     |
     *                         |     |     |
     * increment expression <--|-----|-----|-----+
     *                         |     |     |     |
     * OP_POP                  |     |     |     |
     * OP_LOOP      -----------|-----|-----+     |
     *                         |     |           |
     * body statement <--------|-----+           |
     *                         |                 |
     * OP_LOOP       ----------|-----------------+
     *                         |
     * OP_POP        <---------+
     *
     * continue
     */
    fn for_statement(&mut self) {
        // If a for statement declares a variable, that variable should be
        // scoped to the loop's body
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

        // Initializer clause
        if self.matches(TokenType::Semicolon) {
            // No initializer
        } else if self.matches(TokenType::Var) {
            // Initializer has a variable declaration
            self.var_declaration();
        } else {
            // use expression statement instead of expression since it looks for ';'
            self.expr_statement();
        }

        // record bytecode offset at the top of the body
        let mut loop_start = self.result.borrow().count();

        // Condition clause
        let exit_jump = if self.matches(TokenType::Semicolon) {
            None
        } else {
            // Condition exists
            self.expression();
            self.consume(TokenType::Semicolon, "Expect '; after loop condition");
            // Jump out of the loop if the condition is false
            let result = self.emit_jump(Opcode::JumpIfFalse);
            self.emit_byte(Opcode::Pop.into()); // pop condition
            Some(result)
        };

        // The increment clause
        if !self.matches(TokenType::RightParen) {
            let body_jump = self.emit_jump(Opcode::Jump);
            let increment_start = self.result.borrow().count();
            self.expression(); // increment expression
            self.emit_byte(Opcode::Pop.into());
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");
            // jump that takes control to the top of the main loop
            self.emit_loop(loop_start);
            // change the loop_start to the offset where increment expression begins
            loop_start = increment_start;
            // patch the unconditional jump to the body of the loop
            self.patch_jump(body_jump);
        }

        self.statement();
        // emit loop instruction after the body
        self.emit_loop(loop_start);

        // Patch the jump after the loop body if condition clause exists
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(Opcode::Pop.into());
        }

        self.end_scope();
    }

    /*
     * First we compile the condition expression, bracketed by parentheses.
     * At runtime, that will leave the condition value on the stack.
     * We'll use that to determine whether to execute the 'then' branch or
     * to skip it. Then we omit a new JumpIfFalse instruction. It has an operand
     * for how much to offset the ip - how many bytes of code to skip.
     * If the condition is falsey, it adjusts the ip by that amount.
     * While writing the JumpIfFalse instruction's operand the jump offset is
     * not known. To fix that a trick called backpatching is used. First, emit
     * the jump instruction with a placeholder for the operand. Keep track of
     * where the half finished instruction is. Now compile the 'then' body.
     * Once that is done, we know how far to jump. So, go back and replace the
     * placeholder offset with the real one.
     *
     * Control Flow
     *
     * condition expression
     * OP_JUMP_IF_FALSE   ------+
     * OP_POP                   |
     * then branch statement    |
     * OP_JUMP       -----------|------+
     * OP_POP            <------+      |
     * else branch statement           |
     * continue...     <---------------+
     */
    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after if.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect '(' after condition.");

        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        /* If the condition is truthy, emit instruction to pop the condition
         * from the stack, right before the code inside 'then' branch
         */
        self.emit_byte(Opcode::Pop.into());
        self.statement();

        /* After executing the then branch, the control flow should continue
         * after else branch. The else_jump takes care of that.
         */

        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump);
        /* If the condition is falsey, emit instruction to pop the condition
         * from the stack, right before the code inside 'else' branch.
         * This little instruction also means that every 'if' statement has
         * a implicit 'else' branch even if the user did not write an 'else'
         * clause. In the case where it was left off, all the branch does is
         * discard the condition value.
         */
        self.emit_byte(Opcode::Pop.into());
        if self.matches(TokenType::Else) {
            self.statement()
        }
        self.patch_jump(else_jump);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(Opcode::Print.into());
    }

    /* Works similar to if. Compile the condition surrounded by parantheses.
     * It is followed by a jump instruction that skips over the subsequent body
     * of the while statement if the condition is falsey. Patch the jump statement
     * after compiling the 'while' body and pop the condition value from the stack
     * on either path. After the body, call the function 'emit_loop' to emit the
     * 'loop' instruction. This instruction needs to know how far back to jump.
     * We have already compiled the point in code that we want to jump back to.
     * It is right before the condition expression. Capture that location while
     * compiling it.
     *
     * Control Flow:
     *
     * condtition expression  <----+
     * OP_JUMP_IF_FALSE ---+       |
     * OP_POP              |       |
     * body statement      |       |
     * OP_LOOP  -----------|------+|
     * OP_POP        <-----+
     * continue
     */
    fn while_statement(&mut self) {
        // loop start offset
        let loop_start = self.result.borrow().count();
        self.consume(TokenType::LeftParen, "Expect '(' after while.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Pop.into());
        self.statement();
        // emit loop instruction after the body
        self.emit_loop(loop_start);
        // Patch the jump after compiling the 'while' body
        self.patch_jump(exit_jump);
        self.emit_byte(Opcode::Pop.into());
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
        if self.matches(TokenType::Fun) {
            self.fun_declaration();
        } else if self.matches(TokenType::Var) {
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
        } else if self.matches(TokenType::For) {
            self.for_statement();
        } else if self.matches(TokenType::If) {
            self.if_statement();
        } else if self.matches(TokenType::While) {
            self.while_statement();
        } else if self.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
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
            eprint!(" at '{}'", token.lexeme);
        }
        eprintln!(": {}", message);
        self.parser.had_error.replace(true);
    }
}
