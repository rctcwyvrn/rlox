use crate::scanner::TokenType;

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
            Precedence::PrecCall        =>  Precedence::PrecPrimary,
            Precedence::PrecPrimary     =>  Precedence::PrecPrimary,
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

pub fn get_rule(operator: TokenType) -> ParseRule {
    match operator {
        TokenType::TokenLeftParen   => PARSE_RULE_LP,
        TokenType::TokenMinus       => PARSE_RULE_MINUS,
        TokenType::TokenPlus        => PARSE_RULE_PLUS,
        TokenType::TokenSlash       => PARSE_RULE_SLASH,
        TokenType::TokenStar        => PARSE_RULE_STAR,
        TokenType::TokenNumber      => PARSE_RULE_NUM,
        _                           => PARSE_RULE_NONE
    }
}