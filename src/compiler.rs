use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{OpCode, Instr, Chunk};
use crate::value::Value;
use crate::prec::{Precedence, ParseFn, get_rule};

struct Parser<'a> {
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

    fn error(&mut self, message: &str) {
        if self.panic_mode { return } // Ignore other errors while in panic_mode

        let token = self.previous();
        println!("[Line {}] Error", token.line_num);
        match token.token_type {
            TokenType::TokenEOF => println!(" at end of file"),
            TokenType::TokenError => (), // nothing
            _ => print!(" at '{}'", token.lexemme)
        }

        println!(": {}", message);

        self.had_error = true;
        self.panic_mode = true;
    }

    fn emit_instr(&mut self, op_code: OpCode) {
        self.chunk.write_instruction(Instr {
            op_code,
            line_num: self.previous().line_num
        })
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
            ParseFn::Number     => self.number()
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::PrecAssignment)
    }

    fn number(&mut self) {
        let value = self.previous().lexemme.parse::<f64>().unwrap(); // sketch but ok
        self.emit_constant(Value::Double(value));
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
            _ => () // Error?
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous().token_type;

        let rule = get_rule(operator_type);
        self.parse_precedence(rule.next_precedence());

        // Stack based vm, so emit the binary instr after
        match operator_type {
            TokenType::TokenPlus => self.emit_instr(OpCode::OpAdd),
            TokenType::TokenMinus => self.emit_instr(OpCode::OpSubtract),
            TokenType::TokenStar => self.emit_instr(OpCode::OpMultiply),
            TokenType::TokenSlash => self.emit_instr(OpCode::OpDivide),
            _ => () // error?
        }
    }
}

fn init_parser<'a>(code: &'a String, chunk: &'a mut Chunk) -> Parser<'a> {
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

pub fn compile(code: &String, chunk: &mut Chunk) -> bool{
    let mut parser = init_parser(code, chunk);
    parser.expression();
    parser.consume(TokenType::TokenEOF, "Expected end of file");
    parser.end_compilation();
    return !parser.had_error
}