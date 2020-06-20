use crate::scanner::{Scanner, Token};
use crate::chunk::Chunk;

struct Parser<'a> {
    scanner: &'a mut Scanner<'a>,
    current: Token<'a>,
    previous: Token<'a>,
}

impl Parser<'_> {
    fn advance(&mut self) {

    }
}

pub fn compile(code: &String, chunk: &mut Chunk) -> bool{
    let mut scanner = Scanner::init_scanner(code);
    let previous = scanner.scan_token();
    let previous = previous.clone();
    let current = scanner.scan_token();
    let mut parser = Parser {
        scanner: &mut scanner,
        current,
        previous
    };
    parser.advance();
    true
}