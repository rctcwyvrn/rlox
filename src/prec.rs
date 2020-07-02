use crate::scanner::TokenType;

// Please forgive me for my sins, do not read this file :c 


#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    PrecNone,
    PrecAssignment,
    PrecOr,
    PrecAnd,
    PrecEquality,
    PrecComparison,
    PrecTerm,
    PrecFactor,
    PrecUnary,
    PrecCall,
    PrecPrimary
}

pub enum ParseFn {
    None,
    Unary,
    Grouping,
    Number,
    Binary,
    Literal,
    String,
    Variable,
}

pub struct ParseRule {
    pub prefix: ParseFn,
    pub infix: ParseFn,
    pub precedence: Precedence
}

impl ParseRule {
    pub fn next_precedence(&self) -> Precedence {
        match self.precedence {
            Precedence::PrecNone        => Precedence::PrecAssignment,
            Precedence::PrecAssignment  => Precedence::PrecOr,
            Precedence::PrecOr          => Precedence::PrecAnd,
            Precedence::PrecAnd         => Precedence::PrecEquality,
            Precedence::PrecEquality    => Precedence::PrecComparison,
            Precedence::PrecComparison  => Precedence::PrecTerm,
            Precedence::PrecTerm        => Precedence::PrecFactor,
            Precedence::PrecFactor      => Precedence::PrecUnary,
            Precedence::PrecUnary       => Precedence::PrecCall,
            Precedence::PrecCall        => Precedence::PrecPrimary,
            Precedence::PrecPrimary     => Precedence::PrecPrimary,
        }
    }
}

const PARSE_RULE_NONE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

const PARSE_RULE_LP: ParseRule = ParseRule {
    prefix: ParseFn::Grouping,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

const PARSE_RULE_MINUS: ParseRule = ParseRule {
    prefix: ParseFn::Unary,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecTerm
};

const PARSE_RULE_PLUS: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecTerm
};

const PARSE_RULE_SLASH: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecFactor
};

const PARSE_RULE_STAR: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecFactor
};

const PARSE_RULE_NUM: ParseRule = ParseRule {
    prefix: ParseFn::Number,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

const PARSE_RULE_TRUE: ParseRule = ParseRule {
    prefix: ParseFn::Literal,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

const PARSE_RULE_FALSE: ParseRule = ParseRule {
    prefix: ParseFn::Literal,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

const PARSE_RULE_NIL: ParseRule = ParseRule {
    prefix: ParseFn::Literal,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

const PARSE_RULE_BANG: ParseRule = ParseRule {
    prefix: ParseFn::Unary,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

const PARSE_RULE_BE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecEquality
};

const PARSE_RULE_EE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecEquality
};


const PARSE_RULE_G: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecComparison
};

const PARSE_RULE_GE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecComparison
};
const PARSE_RULE_L: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecComparison
};
const PARSE_RULE_LE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::PrecComparison
};
const PARSE_RULE_STR: ParseRule = ParseRule {
    prefix: ParseFn::String,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};
const PARSE_RULE_ID: ParseRule = ParseRule {
    prefix: ParseFn::Variable,
    infix: ParseFn::None,
    precedence: Precedence::PrecNone
};

pub fn get_rule(operator: TokenType) -> ParseRule {
    match operator {
        TokenType::TokenLeftParen   => PARSE_RULE_LP,
        TokenType::TokenMinus       => PARSE_RULE_MINUS,
        TokenType::TokenPlus        => PARSE_RULE_PLUS,
        TokenType::TokenSlash       => PARSE_RULE_SLASH,
        TokenType::TokenStar        => PARSE_RULE_STAR,
        TokenType::TokenNumber      => PARSE_RULE_NUM,
        TokenType::TokenTrue        => PARSE_RULE_TRUE,
        TokenType::TokenFalse       => PARSE_RULE_FALSE,
        TokenType::TokenNil         => PARSE_RULE_NIL,
        TokenType::TokenBang        => PARSE_RULE_BANG,
        TokenType::TokenBangEqual   => PARSE_RULE_BE,
        TokenType::TokenEqualEqual  => PARSE_RULE_EE,
        TokenType::TokenGreater     => PARSE_RULE_G,
        TokenType::TokenGreaterEqual=> PARSE_RULE_GE,
        TokenType::TokenLess        => PARSE_RULE_L,
        TokenType::TokenLessEqual   => PARSE_RULE_LE,
        TokenType::TokenString      => PARSE_RULE_STR,
        TokenType::TokenIdentifier  => PARSE_RULE_ID,
        _                           => PARSE_RULE_NONE
    }
}