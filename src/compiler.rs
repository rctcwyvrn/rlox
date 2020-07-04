use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{OpCode, Instr, Chunk};
use crate::value::Value;
use crate::prec::{Precedence, ParseFn, get_rule};
use crate::debug::disassemble_chunk;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    compiler: Compiler,
    tokens: Vec<Token>,
    chunk: &'a mut Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn advance(&mut self){
        self.tokens.push(self.scanner.scan_token()); // Wastes memory by not just dropping the older tokens, but w/e this makes the borrow checker happy so it works 4 me
        if self.current().token_type == TokenType::TokenError {
            self.error("Error in scanning");
            self.advance();
        }
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.tokens.len() - 2]
    }

    fn current(&self) -> &Token {
        &self.tokens[self.tokens.len() - 1]
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) {
        if self.current().token_type == token_type {
            self.advance();
        } else {
            self.error(msg);
        }
    }

    fn match_cur(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        } else {
            self.advance();
            return true;
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current().token_type == token_type
    }

    fn error(&mut self, message: &str) {
        if self.panic_mode { return } // Ignore other errors while in panic_mode
        let token = self.previous();
        eprintln!("[Line {}] Error", token.line_num);
        match token.token_type {
            TokenType::TokenEOF => eprintln!(" at end of file"),
            TokenType::TokenError => (), // nothing
            _ => eprint!(" at '{}'", token.lexemme)
        }

        println!(": {}", message);

        self.had_error = true;
        self.panic_mode = true;
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.check(TokenType::TokenEOF) {
            if self.previous().token_type == TokenType::TokenSemicolon { return; }
            match self.current().token_type {
                TokenType::TokenClass | TokenType::TokenFun | TokenType::TokenVar | TokenType::TokenFor | TokenType::TokenIf | TokenType::TokenWhile | TokenType::TokenPrint | TokenType::TokenReturn => return,
                _ => (),
            }
            self.advance();
        }
    }

    fn emit_instr(&mut self, op_code: OpCode) {
        // println!("Emitting instr {:?} from token {:?}", op_code, self.previous()); kinda useful
        self.chunk.write_instruction(Instr {
            op_code,
            line_num: self.previous().line_num
        })
    }

    fn emit_instrs(&mut self, op_codes: &[OpCode]) {
        for oc in op_codes {
            self.emit_instr(*oc)
        }
    }

    fn emit_constant(&mut self, value: Value) -> usize {
        let index = self.chunk.add_constant(value);
        self.emit_instr(OpCode::OpConstant(index));
        index
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth+=1;
    }

    // Decrements the scope depth and pops off the values that went out of scope
    fn end_scope(&mut self) {
        self.compiler.scope_depth-=1;
        let mut pops = 0;
        for local in self.compiler.locals.iter().rev() {
            if local.depth > self.compiler.scope_depth {
                pops+=1;
            } else {
                break;
            }
        }

        for _ in 0..pops {
            self.emit_instr(OpCode::OpPop); // Remove old local variables
            self.compiler.locals.pop();
        }
    }

    fn end_compilation(&mut self) {
        self.emit_instr(OpCode::OpReturn)
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();

        // Parse the start of the prefix expression
        // We know this must be a prefix because we can't start with something that is an infix (eg + 3 2)
        let prefix_rule = get_rule(self.previous().token_type).prefix;
        // Used only by variable() to determine if a TokenIdentifier is for an assignment or get
        let can_assign = prec <= Precedence::PrecAssignment;
        self.call_parse_fn(prefix_rule, can_assign);

        // Parse any number of infix expressions, as long as they have higher precedence
        while prec <= get_rule(self.current().token_type).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous().token_type).infix;
            self.call_parse_fn(infix_rule, can_assign);
        }

        // Show compilation error for a TokenEqual found in an infix position
        // Note: I think this just does not work at all, maybe we need to match against previous?
        if can_assign && self.match_cur(TokenType::TokenEqual) {
            self.error("Invalid assignment target");
        }
    }
    
    fn call_parse_fn(&mut self, parse_fn: ParseFn, can_assign: bool) {
        match parse_fn {
            ParseFn::None       => self.error("Expected expression"),
            ParseFn::Binary     => self.binary(),
            ParseFn::Grouping   => self.grouping(),
            ParseFn::Unary      => self.unary(),
            ParseFn::Number     => self.number(),
            ParseFn::Literal    => self.literal(),
            ParseFn::String     => self.string(),
            ParseFn::Variable   => self.variable(can_assign),
        }
    }

    fn declaration(&mut self) {
        if self.match_cur(TokenType::TokenVar) {
            self.var_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode { self.synchronize(); }
    }

    // parse_variable => consumes TokenIdentifier, adds the constant name to the cosntants vec, and returns the index
    // define_variable => takes that index, and emits the instruction to bind it to the value on the top of the the stack
    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name.");
        if self.match_cur(TokenType::TokenEqual) {
            self.expression();
        } else {
            self.emit_instr(OpCode::OpNil);
        }
        self.consume(TokenType::TokenSemicolon, "Expected ';' after variable declaration");
        self.define_variable(global);
    }

    // Match the identifier token and pass it into identifier_constant to be added to the chunk if global
    // Calls declare_variable() if local
    fn parse_variable(&mut self, error_msg: &str) -> usize {
        self.consume(TokenType::TokenIdentifier, error_msg);
        self.declare_variable();

        if self.compiler.is_global() {
            let str_val = self.previous().lexemme.clone();
            self.identifier_constant(&str_val)
        } else {
            0
        }
    }

    // Declare new local variables by pushing them onto self.compiler.locals
    // New locals are set to a special "uninitialized" state until define_variable() is called
    fn declare_variable(&mut self) {
        if !self.compiler.is_global() { // Must not be in the global scope in order to define local vars
            let str_val = self.previous().lexemme.clone();
            let mut found_eq = false; // Is this the idiomatic way of doing this?? I have no idea

            for local in self.compiler.locals.iter() {
                if local.depth != -1 && local.depth < self.compiler.scope_depth {
                    break;
                }
                if str_val.eq(&local.name) {
                    found_eq = true;
                    break;
                }
            }

            if found_eq {
                self.error("Variable with this name already declared in this scope"); // Note: Can't just put this in the loop because it mutably borrows self
            }
            self.compiler.add_local(str_val);
        }
    }

    // Walk and look for a local variable with the same name, returns -1 to signal that this name is a global
    fn resolve_local(&mut self, name: &String) -> isize {
        let mut error = false;
        for (i, local) in self.compiler.locals.iter().enumerate() {
            if local.name.eq(name){
                if local.depth == -1  {
                    error = true;
                    break;
                } else {
                    return i as isize;
                }
            }
        }

        if error {
            self.error("Cannot read local variable in its own initializer");
        }
        return -1;
    }

    // Given a token, add the string lexemme to the chunk as a constant and return the index
    // Only used for global variables
    fn identifier_constant(&mut self, str_val: &String) -> usize {
        self.chunk.add_constant(Value::LoxString(str_val.to_string()))
    }

    // Emits the instruction to define the global variable
    // or to set the local variable as initialized
    fn define_variable(&mut self, global: usize) {
        if self.compiler.is_global() {
            self.emit_instr(OpCode::OpDefineGlobal(global));
        } else {
            self.compiler.mark_initialized();
        }
    }

    fn statement(&mut self) {
        if self.match_cur(TokenType::TokenPrint) {
            self.print_statement();
        } else if self.match_cur(TokenType::TokenLeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::TokenSemicolon, "Expected ';' after value in print statement");
        self.emit_instr(OpCode::OpPrint);
    }

    fn block(&mut self) {
        while !self.check(TokenType::TokenRightBrace) && !self.check(TokenType::TokenEOF) {
            self.declaration();
        }

        self.consume(TokenType::TokenRightBrace, "Expected '}' after block"); // Fails if we hit EOF instead
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::TokenSemicolon, "Expected ';' after value");
        self.emit_instr(OpCode::OpPop);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::PrecAssignment)
    }

    fn number(&mut self) {
        let value = self.previous().lexemme.parse::<f64>().unwrap(); // sketch but ok
        self.emit_constant(Value::Double(value));
    }

    fn literal(&mut self) {
        match self.previous().token_type {
            TokenType::TokenFalse => self.emit_instr(OpCode::OpFalse),
            TokenType::TokenTrue => self.emit_instr(OpCode::OpTrue),
            TokenType::TokenNil => self.emit_instr(OpCode::OpNil),
            _ => panic!("Unreachable state reached, attempted to make a literal out of a non-literal type?"),
        }
    }

    fn string(&mut self) {
        let str_val = self.previous().lexemme.clone();
        self.emit_constant(Value::LoxString(unwrap_string(&str_val)));
    }

    fn variable(&mut self, can_assign: bool) {
        let name = &self.previous().lexemme.clone();
        self.named_variable(name, can_assign)
    }

    // parsePrecedence with TokenIdentifier => variable() -> this(previous.lexemme) 
    // Could be a getter or a setter, so lookahead for a '='
    fn named_variable(&mut self, name: &String, can_assign: bool) {
        let local_arg = self.resolve_local(name);
        let (get_op, set_op) = if local_arg >= 0  {
            (OpCode::OpGetLocal(local_arg as usize), OpCode::OpSetLocal(local_arg as usize))
        } else {
            let global_arg = self.identifier_constant(name); // Does NOT check at compile time if this variable can be resolved
            (OpCode::OpGetGlobal(global_arg), OpCode::OpSetGlobal(global_arg))
        };

        if self.match_cur(TokenType::TokenEqual) && can_assign {
            self.expression();
            self.emit_instr(set_op);
        } else {
            self.emit_instr(get_op);
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::TokenRightParen, "Expected ')' after expression");
    }

    fn unary(&mut self) { 
        let operator_type = self.previous().token_type;
        self.parse_precedence(Precedence::PrecUnary); // evaluate the expression in the unary
        match operator_type {
            TokenType::TokenMinus => self.emit_instr(OpCode::OpNegate),
            TokenType::TokenBang => self.emit_instr(OpCode::OpNot),
            _ => () // Error?
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous().token_type;

        let rule = get_rule(operator_type);
        self.parse_precedence(rule.next_precedence());

        // Stack based vm, so emit the binary instr after
        match operator_type {
            TokenType::TokenPlus        => self.emit_instr(OpCode::OpAdd),
            TokenType::TokenMinus       => self.emit_instr(OpCode::OpSubtract),
            TokenType::TokenStar        => self.emit_instr(OpCode::OpMultiply),
            TokenType::TokenSlash       => self.emit_instr(OpCode::OpDivide),
            TokenType::TokenBangEqual   => self.emit_instrs(&[OpCode::OpEqual, OpCode::OpNot]),
            TokenType::TokenEqualEqual  => self.emit_instr(OpCode::OpEqual),
            TokenType::TokenGreater     => self.emit_instr(OpCode::OpGreater),
            TokenType::TokenGreaterEqual => self.emit_instrs(&[OpCode::OpLess, OpCode::OpNot]),
            TokenType::TokenLess        => self.emit_instr(OpCode::OpLess),
            TokenType::TokenLessEqual   => self.emit_instrs(&[OpCode::OpGreater, OpCode::OpNot]),
            _ => () // error?
        }
    }

    pub fn init_parser(code: &'a String, chunk: &'a mut Chunk) -> Parser<'a> {
        let mut scanner = Scanner::init_scanner(code);
        let compiler = Compiler { locals: Vec::new(), scope_depth: 0};
        let mut tokens = Vec::new();
        tokens.push(scanner.scan_token());
        Parser {
            scanner,
            compiler, 
            tokens,
            chunk,
            had_error: false,
            panic_mode: false,
        }
    }

    // looks like the book will have multiple bytecode chunks being produced, so we're gonna have to switch to some vectors eventually
    pub fn compile(&mut self) -> bool { 
        while !self.match_cur(TokenType::TokenEOF) {
            self.declaration();
        }
        self.end_compilation();
        //disassemble_chunk(self.chunk, "test");
        return !self.had_error
    }
}

// Why is this named this way...
struct Compiler {
    locals: Vec<Local>,
    scope_depth: isize
}

impl Compiler {
    fn is_global(&self) -> bool {
        self.scope_depth == 0
    }

    fn add_local(&mut self, name: String) {
        let local = Local {
            name, 
            depth: -1 // Scope depth is initially set to -1 to indicate that the value is not yet initialized and cannot be used
        };
        self.locals.push(local);
    }

    fn mark_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }
}
struct Local {
    name: String,
    depth: isize
}

// Removes the surrounding "" around TokenString lexemmes
fn unwrap_string(s: &str) -> String {
    s[1..s.len() -1].to_string()
}