#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    TokenLeftParen,
    TokenRightParen,
    TokenLeftBrace,
    TokenRightBrace,
    TokenComma,
    TokenDot,
    TokenSemicolon,
    TokenMinus,
    TokenPlus,
    TokenSlash,
    TokenStar,
    TokenBang,
    TokenBangEqual,
    TokenEqual,
    TokenEqualEqual,
    TokenGreater,
    TokenGreaterEqual,
    TokenLess,
    TokenLessEqual,

    TokenIdentifier,
    TokenString,
    TokenNumber,

    TokenAnd,
    TokenClass,
    TokenElse,
    TokenFalse,
    TokenFor,
    TokenFun,
    TokenIf,
    TokenNil,
    TokenOr,
    TokenPrint,
    TokenReturn,
    TokenSuper,
    TokenThis,
    TokenTrue,
    TokenVar,
    TokenWhile,
    TokenError,
    TokenEOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line_num: usize,
    pub lexemme: String,
}

#[derive(Debug, Clone, Copy)]
pub struct Scanner<'a> {
    code: &'a str,
    cur_line: usize,
    start_pos: usize,
    cur_pos: usize,
}

impl Scanner<'_> {
    pub fn new(code: &str) -> Scanner {
        Scanner {
            code,
            cur_line: 1,
            start_pos: 0,
            cur_pos: 0,
        }
    }

    fn create_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            line_num: self.cur_line,
            lexemme: self.code[self.start_pos..self.cur_pos].to_string(),
        }
    }

    fn error_token(&self, msg: String) -> Token {
        Token {
            token_type: TokenType::TokenError,
            line_num: self.cur_line,
            lexemme: msg,
        }
    }

    /// If this is true, then the current position is invalid and cannot be peeked
    fn is_at_end(&self) -> bool {
        self.cur_pos == self.code.len()
    }

    /// One stricter than is_at_end, must be checked before calling peek_next()
    fn can_peek_next(&self) -> bool {
        self.cur_pos <= self.code.len() - 2
    }

    fn advance(&mut self) -> u8 {
        let ret = self.peek();
        self.cur_pos += 1;
        ret
    }

    fn match_char(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        } else if self.peek() != expected {
            return false;
        } else {
            self.cur_pos += 1;
            return true;
        }
    }

    fn peek(&self) -> u8 {
        self.code.as_bytes()[self.cur_pos]
    }

    fn peek_next(&self) -> u8 {
        self.code.as_bytes()[self.cur_pos + 1]
    }

    /// This function is gross and it also messes up sometimes near the end of files
    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let next = self.peek();
            if (next == b' ') || (next == b'\t') || (next == b'\r') {
                self.advance();
            } else if next == b'\n' {
                self.advance();
                self.cur_line += 1;
            } else if next == b'/' {
                if self.can_peek_next() && self.peek_next() == b'/' {
                    while !self.is_at_end() && self.peek() != b'\n' {
                        self.advance(); // Advance over the second '/' and the rest of the line, or until we hit the end of the file
                    }

                    if !self.is_at_end() {
                        self.advance(); // consume the \n
                        self.cur_line += 1;
                    }
                } else {
                    return; // Return on single slash
                }
            } else {
                return;
            }
        }
    }

    fn create_string(&mut self) -> Token {
        while !self.is_at_end() && self.peek() != b'"' {
            if self.peek() == b'\n' {
                self.cur_line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token(String::from("Missing delimiter for string"));
        }

        self.advance(); // Step over the closing quote
        return self.create_token(TokenType::TokenString);
    }

    fn create_number(&mut self) -> Token {
        while !self.is_at_end() && is_digit(self.peek()) {
            self.advance();
        }

        if self.can_peek_next() && self.peek() == b'.' && is_digit(self.peek_next()) {
            self.advance();
            while !self.is_at_end() && is_digit(self.peek()) {
                self.advance();
            }
        }

        return self.create_token(TokenType::TokenNumber);
    }

    // Helper function for get_identifier_type(), checks that the remaining characters match the keyword_type that was given
    // Returns TokenIdentifier otherwise
    fn check_for_keyword(
        &self,
        start: usize,
        length: usize,
        rest: &str,
        keyword_type: TokenType,
    ) -> TokenType {
        if self.cur_pos - self.start_pos == start + length {
            // this will check that begin + length is within the array, since we already moved cur_pos exactly that far
            let begin = self.start_pos + start;
            if &self.code[begin..begin + length] == rest {
                return keyword_type;
            }
        }
        TokenType::TokenIdentifier
    }

    // Implements a simple trie to determine if the characters we just parsed make up a keyword or are just an identifier
    fn get_identifier_type(&self) -> TokenType {
        let c = self.code.as_bytes()[self.start_pos];
        return match c {
            b'a' => self.check_for_keyword(1, 2, "nd", TokenType::TokenAnd),
            b'c' => self.check_for_keyword(1, 4, "lass", TokenType::TokenClass),
            b'e' => self.check_for_keyword(1, 3, "lse", TokenType::TokenElse),
            b'i' => self.check_for_keyword(1, 1, "f", TokenType::TokenIf),
            b'n' => self.check_for_keyword(1, 2, "il", TokenType::TokenNil),
            b'o' => self.check_for_keyword(1, 1, "r", TokenType::TokenOr),
            b'p' => self.check_for_keyword(1, 4, "rint", TokenType::TokenPrint),
            b'r' => self.check_for_keyword(1, 5, "eturn", TokenType::TokenReturn),
            b's' => self.check_for_keyword(1, 4, "uper", TokenType::TokenSuper),
            b'v' => self.check_for_keyword(1, 2, "ar", TokenType::TokenVar),
            b'w' => self.check_for_keyword(1, 4, "hile", TokenType::TokenWhile),
            b'f' => {
                if self.cur_pos - self.start_pos > 1 {
                    // more than 1 char in this maybe keyword
                    match self.code.as_bytes()[self.start_pos + 1] {
                        b'a' => self.check_for_keyword(2, 3, "lse", TokenType::TokenFalse),
                        b'o' => self.check_for_keyword(2, 1, "r", TokenType::TokenFor),
                        b'u' => self.check_for_keyword(2, 1, "n", TokenType::TokenFun),
                        _ => TokenType::TokenIdentifier,
                    }
                } else {
                    TokenType::TokenIdentifier
                }
            }
            b't' => {
                if self.cur_pos - self.start_pos > 1 {
                    // more than 1 char in this maybe keyword
                    match self.code.as_bytes()[self.start_pos + 1] {
                        b'h' => self.check_for_keyword(2, 2, "is", TokenType::TokenThis),
                        b'r' => self.check_for_keyword(2, 2, "ue", TokenType::TokenTrue),
                        _ => TokenType::TokenIdentifier,
                    }
                } else {
                    TokenType::TokenIdentifier
                }
            }
            _ => TokenType::TokenIdentifier,
        };
    }

    fn create_identifier(&mut self) -> Token {
        while !self.is_at_end() && (is_alpha(self.peek()) || is_digit(self.peek())) {
            self.advance();
        }
        self.create_token(self.get_identifier_type())
    }

    pub fn scan_token(&mut self) -> Token {
        self.start_pos = self.cur_pos;
        self.skip_whitespace();
        self.start_pos = self.cur_pos; // reset any seeking we did while we were removing whitespace

        if self.is_at_end() {
            return self.create_token(TokenType::TokenEOF);
        }

        let c = self.advance();

        if is_digit(c) {
            return self.create_number();
        } else if is_alpha(c) {
            return self.create_identifier();
        }

        // Punctuation and string literal tokens
        return match c {
            b'(' => self.create_token(TokenType::TokenLeftParen),
            b')' => self.create_token(TokenType::TokenRightParen),
            b'{' => self.create_token(TokenType::TokenLeftBrace),
            b'}' => self.create_token(TokenType::TokenRightBrace),
            b';' => self.create_token(TokenType::TokenSemicolon),
            b',' => self.create_token(TokenType::TokenComma),
            b'.' => self.create_token(TokenType::TokenDot),
            b'-' => self.create_token(TokenType::TokenMinus),
            b'+' => self.create_token(TokenType::TokenPlus),
            b'/' => self.create_token(TokenType::TokenSlash),
            b'*' => self.create_token(TokenType::TokenStar),
            b'!' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::TokenBangEqual
                } else {
                    TokenType::TokenBang
                };
                self.create_token(token_type)
            }
            b'=' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::TokenEqualEqual
                } else {
                    TokenType::TokenEqual
                };
                self.create_token(token_type)
            }
            b'<' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::TokenLessEqual
                } else {
                    TokenType::TokenLess
                };
                self.create_token(token_type)
            }
            b'>' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::TokenGreaterEqual
                } else {
                    TokenType::TokenGreater
                };
                self.create_token(token_type)
            }
            b'"' => self.create_string(),
            _ => self.error_token(String::from("Invalid character")),
        };
    }
}

fn is_digit(c: u8) -> bool {
    c >= b'0' && c <= b'9'
}

fn is_alpha(c: u8) -> bool {
    (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || c == b'_'
}
