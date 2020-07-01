use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{OpCode, Instr, Chunk};
use crate::value::Value;
use crate::prec::{Precedence, ParseFn, get_rule};
use crate::debug::disassemble_chunk;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
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

    fn emit_constant(&mut self, value: Value) {
        let index = self.chunk.add_constant(value);
        self.emit_instr(OpCode::OpConstant(index));
    }

    fn end_compilation(&mut self) {
        self.emit_instr(OpCode::OpReturn)
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();

        // Parse the start of the prefix expression
        // We know this must be a prefix because we can't start with something that is an infix (eg + 3 2)
        let prefix_rule = get_rule(self.previous().token_type).prefix;
        self.call_parse_fn(prefix_rule);

        // Parse any number of infix expressions, as long as they have higher precedence
        while prec <= get_rule(self.current().token_type).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous().token_type).infix;
            self.call_parse_fn(infix_rule);
        }
    }
    
    fn call_parse_fn(&mut self, parse_fn: ParseFn) {
        match parse_fn {
            ParseFn::None       => self.error("Expected expression"),
            ParseFn::Binary     => self.binary(),
            ParseFn::Grouping   => self.grouping(),
            ParseFn::Unary      => self.unary(),
            ParseFn::Number     => self.number(),
            ParseFn::Literal    => self.literal(),
            ParseFn::String     => self.string(),
        }
    }

    fn declaration(&mut self) {
        self.statement();
        if self.panic_mode { self.synchronize(); }
    }

    fn statement(&mut self) {
        if self.match_cur(TokenType::TokenPrint) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::TokenSemicolon, "Expected ';' after value");
        self.emit_instr(OpCode::OpPrint);
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
        self.emit_constant(Value::LoxString(str_val[1..str_val.len() -1].to_string()));
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
        let mut tokens = Vec::new();
        tokens.push(scanner.scan_token()); // Only load up one value, because we start the parsing with a call to expression() -> parse_precedence() which will call advance()
        Parser {
            scanner,
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
        disassemble_chunk(self.chunk, "test");
        return !self.had_error
    }
}