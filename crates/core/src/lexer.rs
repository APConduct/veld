use std::fmt::Display;
use tracing::{self, Level, span};

use logos::{Lexer as LLexer, Logos, Skip};

use veld_common::{self, source::Position};

/// Update the line count and the char index.
fn newline_callback(lex: &mut LLexer<Token>) -> Skip {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    Skip
}

/// Compute the line and column position for the current word.
fn word_callback(lex: &mut LLexer<Token>) -> (usize, usize) {
    let line = lex.extras.0;
    let column = lex.span().start - lex.extras.1;

    (line, column)
}

fn int_lit_callback(lex: &mut LLexer<Token>) -> (i64, (usize, usize)) {
    let value = lex.slice().parse().unwrap();
    let position = word_callback(lex);

    (value, position)
}

fn identifier_callback(lex: &mut LLexer<Token>) -> (String, (usize, usize)) {
    let value = lex.slice().to_string();
    let position = word_callback(lex);

    (value, position)
}

fn string_lit_callback(lex: &mut LLexer<Token>) -> (String, (usize, usize)) {
    let mut content = lex.slice().to_string();
    content.remove(0);
    content.pop();
    let value = content;
    let position = word_callback(lex);

    (value, position)
}

fn float_lit_callback(lex: &mut LLexer<Token>) -> (f64, (usize, usize)) {
    let value = lex.slice().parse().unwrap();
    let position = word_callback(lex);

    (value, position)
}

#[derive(Debug, Logos, Clone)]
#[logos(extras = (usize, usize))]
#[logos(skip(r"[ \t\f]+"))] // Skip whitespace
#[logos(skip r"#.*")] // Skip single-line comments
#[logos(skip r"#\|")] // Skip multi-line comments
#[logos(skip(r"\n", newline_callback))]
pub enum Token {
    // Keywords
    #[token("fn", word_callback)]
    Fn((usize, usize)),
    #[token("proc", word_callback)]
    Proc((usize, usize)),
    #[token("let", word_callback)]
    Let((usize, usize)),
    #[token("struct", word_callback)]
    Struct((usize, usize)),
    #[token("kind", word_callback)]
    Kind((usize, usize)),
    #[token("impl", word_callback)]
    Impl((usize, usize)),
    #[token("end", word_callback)]
    End((usize, usize)),
    #[token("for", word_callback)]
    For((usize, usize)),
    #[token("in", word_callback)]
    In((usize, usize)),
    #[token("where", word_callback)]
    Where((usize, usize)),
    #[token("as", word_callback)]
    As((usize, usize)),
    #[token("or", word_callback)]
    Or((usize, usize)),
    #[token("mod", word_callback)]
    Mod((usize, usize)),
    #[token("import", word_callback)]
    Import((usize, usize)),
    #[token("pub", word_callback)]
    Pub((usize, usize)),
    #[token("from", word_callback)]
    From((usize, usize)),
    #[token("var", word_callback)]
    Var((usize, usize)),
    #[token("mut", word_callback)]
    Mut((usize, usize)),
    #[token("break", word_callback)]
    Break((usize, usize)),
    #[token("continue", word_callback)]
    Continue((usize, usize)),
    #[token("enum", word_callback)]
    Enum((usize, usize)),
    #[token("plex", word_callback)]
    Plex((usize, usize)),

    // Operators
    #[token("=", word_callback)]
    Equals((usize, usize)),
    #[token("+", word_callback)]
    Plus((usize, usize)),
    #[token("-", word_callback)]
    Minus((usize, usize)),
    #[token("*", word_callback)]
    Star((usize, usize)),
    #[token("/", word_callback)]
    Slash((usize, usize)),
    #[token("->", word_callback)]
    Arrow((usize, usize)),
    #[token("=>", word_callback)]
    FatArrow((usize, usize)),
    #[token("*^", word_callback)]
    ExpOp((usize, usize)),
    #[token("%", word_callback)]
    Modulo((usize, usize)),
    #[token("+=", word_callback)]
    PlusEq((usize, usize)),
    #[token("-=", word_callback)]
    MinusEq((usize, usize)),
    #[token("*=", word_callback)]
    StarEq((usize, usize)),
    #[token("/=", word_callback)]
    SlashEq((usize, usize)),

    // Comparison operators
    #[token("<=", word_callback)]
    LessEq((usize, usize)),
    #[token(">=", word_callback)]
    GreaterEq((usize, usize)),
    #[token("<", word_callback)]
    Less((usize, usize)),
    #[token(">", word_callback)]
    Greater((usize, usize)),
    #[token("==", word_callback)]
    EqualEqual((usize, usize)),
    #[token("!=", word_callback)]
    NotEqual((usize, usize)),

    // Delimiters
    #[token("(", word_callback)]
    LParen((usize, usize)),
    #[token(")", word_callback)]
    RParen((usize, usize)),
    #[token("{", word_callback)]
    LBrace((usize, usize)),
    #[token("}", word_callback)]
    RBrace((usize, usize)),
    #[token("[", word_callback)]
    LBracket((usize, usize)),
    #[token("]", word_callback)]
    RBracket((usize, usize)),
    #[token(",", word_callback)]
    Comma((usize, usize)),
    #[token(":", word_callback)]
    Colon((usize, usize)),
    #[token(";", word_callback)]
    Semicolon((usize, usize)),
    #[token("@", word_callback)]
    At((usize, usize)),
    #[token(".", word_callback)]
    Dot((usize, usize)),
    #[token("..=", word_callback)]
    DotDotEq((usize, usize)),
    #[token("..", word_callback)]
    DotDot((usize, usize)),
    #[token("...", word_callback)]
    DotDotDot((usize, usize)),
    #[token("~", word_callback)]
    Tilde((usize, usize)),
    #[token("!", word_callback)]
    Bang((usize, usize)),

    // Literals and Identifiers
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", identifier_callback)]
    Identifier((String, (usize, usize))),

    #[regex(r#""[^"]*""#, string_lit_callback)]
    StringLiteral((String, (usize, usize))),

    #[regex(r#"[0-9]+"#, int_lit_callback)]
    IntegerLiteral((i64, (usize, usize))),

    #[regex(r#"[0-9]+\.[0-9]+"#, float_lit_callback)]
    FloatLiteral((f64, (usize, usize))),

    #[token("if", word_callback)]
    If((usize, usize)),
    #[token("else", word_callback)]
    Else((usize, usize)),
    #[token("while", word_callback)]
    While((usize, usize)),
    #[token("return", word_callback)]
    Return((usize, usize)),
    #[token("true", word_callback)]
    True((usize, usize)),
    #[token("false", word_callback)]
    False((usize, usize)),
    #[token("and", word_callback)]
    And((usize, usize)),
    #[token("match", word_callback)]
    Match((usize, usize)),
    #[token("do", word_callback)]
    Do((usize, usize)),
    #[token("then", word_callback)]
    Then((usize, usize)),
    #[token("with", word_callback)]
    With((usize, usize)),
    #[token("macro", word_callback)]
    Macro((usize, usize)),
    #[token("const", word_callback)]
    Const((usize, usize)),
    #[token("self", word_callback)]
    SelfToken((usize, usize)),
    #[token("|>", word_callback)]
    Pipe((usize, usize)),
    #[token("[[", word_callback)]
    LDoubleBracket((usize, usize)),
    #[token("]]", word_callback)]
    RDoubleBracket((usize, usize)),
    #[token("<-", word_callback)]
    LeftArrow((usize, usize)),
    #[token("$", word_callback)]
    Dollar((usize, usize)),
    #[token(":expr", word_callback)]
    ExprFragment((usize, usize)),

    #[token("static", word_callback)]
    Static((usize, usize)),
    #[token("async", word_callback)]
    Async((usize, usize)),
    #[token("await", word_callback)]
    Await((usize, usize)),
    #[token("spawn", word_callback)]
    Spawn((usize, usize)),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Fn((x, y)) => write!(f, "fn at {}, {}", x, y),
            Token::Proc((x, y)) => write!(f, "proc at {}, {}", x, y),
            Token::Let((x, y)) => write!(f, "let at {}, {}", x, y),
            Token::Struct((x, y)) => write!(f, "struct at {}, {}", x, y),
            Token::Kind((x, y)) => write!(f, "kind at {}, {}", x, y),
            Token::Impl((x, y)) => write!(f, "impl at {}, {}", x, y),
            Token::End((x, y)) => write!(f, "end at {}, {}", x, y),
            Token::For((x, y)) => write!(f, "for at {}, {}", x, y),
            Token::In((x, y)) => write!(f, "in at {}, {}", x, y),
            Token::Where((x, y)) => write!(f, "where at {}, {}", x, y),
            Token::As((x, y)) => write!(f, "as at {}, {}", x, y),
            Token::Or((x, y)) => write!(f, "or at {}, {}", x, y),
            Token::Mod((x, y)) => write!(f, "mod at {}, {}", x, y),
            Token::Import((x, y)) => write!(f, "import at {}, {}", x, y),
            Token::Pub((x, y)) => write!(f, "pub at {}, {}", x, y),
            Token::From((x, y)) => write!(f, "from at {}, {}", x, y),
            Token::Var((x, y)) => write!(f, "var at {}, {}", x, y),
            Token::Mut((x, y)) => write!(f, "mut at {}, {}", x, y),
            Token::Break((x, y)) => write!(f, "break at {}, {}", x, y),
            Token::Continue((x, y)) => write!(f, "continue at {}, {}", x, y),
            Token::Enum((x, y)) => write!(f, "enum at {}, {}", x, y),
            Token::Equals((x, y)) => write!(f, "= at {},{}", x, y),
            Token::Plus((x, y)) => write!(f, "+ at {},{}", x, y),
            Token::Minus((x, y)) => write!(f, "- at {},{}", x, y),
            Token::Star((x, y)) => write!(f, "* at {},{}", x, y),
            Token::Slash((x, y)) => write!(f, "/ at {},{}", x, y),
            Token::Arrow((x, y)) => write!(f, "-> at {},{}", x, y),
            Token::FatArrow((x, y)) => write!(f, "=> at {},{}", x, y),
            Token::ExpOp((x, y)) => write!(f, "*^ at {},{}", x, y),
            Token::Modulo((x, y)) => write!(f, "% at {},{}", x, y),
            Token::PlusEq((x, y)) => write!(f, "+= at {},{}", x, y),
            Token::MinusEq((x, y)) => write!(f, "-= at {},{}", x, y),
            Token::StarEq((x, y)) => write!(f, "*= at {},{}", x, y),
            Token::SlashEq((x, y)) => write!(f, "/= at {},{}", x, y),
            Token::LessEq((x, y)) => write!(f, "<= at {},{}", x, y),
            Token::GreaterEq((x, y)) => write!(f, ">= at {},{}", x, y),
            Token::Less((x, y)) => write!(f, "< at {},{}", x, y),
            Token::Greater((x, y)) => write!(f, "> at {},{}", x, y),
            Token::EqualEqual((x, y)) => write!(f, "== at {},{}", x, y),
            Token::NotEqual((x, y)) => write!(f, "!= at {},{}", x, y),
            Token::LParen((x, y)) => write!(f, "( at {},{}", x, y),
            Token::RParen((x, y)) => write!(f, ") at {},{}", x, y),
            Token::LBrace((x, y)) => write!(f, "{{ at {},{}", x, y),
            Token::RBrace((x, y)) => write!(f, "}} at {},{}", x, y),
            Token::LBracket((x, y)) => write!(f, "[ at {},{}", x, y),
            Token::RBracket((x, y)) => write!(f, "] at {},{}", x, y),
            Token::Comma((x, y)) => write!(f, ", at {},{}", x, y),
            Token::Colon((x, y)) => write!(f, ": at {},{}", x, y),
            Token::Semicolon((x, y)) => write!(f, "; at {},{}", x, y),
            Token::At((x, y)) => write!(f, "@ at {},{}", x, y),
            Token::Dot((x, y)) => write!(f, ". at {},{}", x, y),
            Token::DotDotEq((x, y)) => write!(f, "..= at {},{}", x, y),
            Token::DotDot((x, y)) => write!(f, ".. at {},{}", x, y),
            Token::DotDotDot((x, y)) => write!(f, "... at {},{}", x, y),
            Token::Tilde((x, y)) => write!(f, "~ at {},{}", x, y),
            Token::Bang((x, y)) => write!(f, "! at {},{}", x, y),
            Token::Identifier(s) => write!(f, "{} at {},{}", s.0, s.1.0, s.1.1),
            Token::StringLiteral(s) => write!(f, "\"{}\" at {},{}", s.0, s.1.0, s.1.1),
            Token::IntegerLiteral(n) => write!(f, "{} at ({}, {})", n.0, n.1.0, n.1.1),
            Token::FloatLiteral(n) => write!(f, "{} at {},{}", n.0, n.1.0, n.1.1),
            Token::If((x, y)) => write!(f, "if at {},{}", x, y),
            Token::Else((x, y)) => write!(f, "else at {},{}", x, y),
            Token::While((x, y)) => write!(f, "while at {},{}", x, y),
            Token::Return((x, y)) => write!(f, "return at {},{}", x, y),
            Token::True((x, y)) => write!(f, "true at {},{}", x, y),
            Token::False((x, y)) => write!(f, "false at {},{}", x, y),
            Token::And((x, y)) => write!(f, "and at {},{}", x, y),
            Token::Match((x, y)) => write!(f, "match at {},{}", x, y),
            Token::Do((x, y)) => write!(f, "do at {},{}", x, y),
            Token::Then((x, y)) => write!(f, "then at {},{}", x, y),
            Token::With((x, y)) => write!(f, "with at {},{}", x, y),
            Token::Macro((x, y)) => write!(f, "macro at {},{}", x, y),
            Token::Const((x, y)) => write!(f, "const at {},{}", x, y),
            Token::Plex((x, y)) => write!(f, "plex at {},{}", x, y),
            Token::SelfToken((x, y)) => write!(f, "self at {},{}", x, y),
            Token::Pipe((x, y)) => write!(f, "|> at {},{}", x, y),
            Token::LDoubleBracket((x, y)) => write!(f, "[[ at {},{}", x, y),
            Token::RDoubleBracket((x, y)) => write!(f, "]] at {},{}", x, y),
            Token::LeftArrow((x, y)) => write!(f, "<- at {},{}", x, y),
            Token::Dollar((x, y)) => write!(f, "$ at {},{}", x, y),
            Token::ExprFragment((x, y)) => write!(f, ":expr at {},{}", x, y),
            Token::Static((x, y)) => write!(f, "static at {},{}", x, y),
            Token::Async((x, y)) => write!(f, "async at {},{}", x, y),
            Token::Await((x, y)) => write!(f, "await at {},{}", x, y),
            Token::Spawn((x, y)) => write!(f, "spawn at {},{}", x, y),
        }
    }
}

impl Token {
    pub fn locate(&self) -> Option<(usize, usize)> {
        match self {
            Token::Fn((x, y)) => Some((*x, *y)),
            Token::Proc((x, y)) => Some((*x, *y)),
            Token::Let((x, y)) => Some((*x, *y)),
            Token::Struct((x, y)) => Some((*x, *y)),
            Token::Kind((x, y)) => Some((*x, *y)),
            Token::Impl((x, y)) => Some((*x, *y)),
            Token::End((x, y)) => Some((*x, *y)),
            Token::For((x, y)) => Some((*x, *y)),
            Token::In((x, y)) => Some((*x, *y)),
            Token::Where((x, y)) => Some((*x, *y)),
            Token::As((x, y)) => Some((*x, *y)),
            Token::Or((x, y)) => Some((*x, *y)),
            Token::Mod((x, y)) => Some((*x, *y)),
            Token::Import((x, y)) => Some((*x, *y)),
            Token::Pub((x, y)) => Some((*x, *y)),
            Token::From((x, y)) => Some((*x, *y)),
            Token::Var((x, y)) => Some((*x, *y)),
            Token::Mut((x, y)) => Some((*x, *y)),
            Token::Break((x, y)) => Some((*x, *y)),
            Token::Continue((x, y)) => Some((*x, *y)),
            Token::Enum((x, y)) => Some((*x, *y)),
            Token::Plex((x, y)) => Some((*x, *y)),
            Token::Equals((x, y)) => Some((*x, *y)),
            Token::Plus((x, y)) => Some((*x, *y)),
            Token::Minus((x, y)) => Some((*x, *y)),
            Token::Star((x, y)) => Some((*x, *y)),
            Token::Slash((x, y)) => Some((*x, *y)),
            Token::Arrow((x, y)) => Some((*x, *y)),
            Token::FatArrow((x, y)) => Some((*x, *y)),
            Token::ExpOp((x, y)) => Some((*x, *y)),
            Token::Modulo((x, y)) => Some((*x, *y)),
            Token::PlusEq((x, y)) => Some((*x, *y)),
            Token::MinusEq((x, y)) => Some((*x, *y)),
            Token::StarEq((x, y)) => Some((*x, *y)),
            Token::SlashEq((x, y)) => Some((*x, *y)),
            Token::LessEq((x, y)) => Some((*x, *y)),
            Token::GreaterEq((x, y)) => Some((*x, *y)),
            Token::Less((x, y)) => Some((*x, *y)),
            Token::Greater((x, y)) => Some((*x, *y)),
            Token::EqualEqual((x, y)) => Some((*x, *y)),
            Token::NotEqual((x, y)) => Some((*x, *y)),
            Token::LParen((x, y)) => Some((*x, *y)),
            Token::RParen((x, y)) => Some((*x, *y)),
            Token::LBrace((x, y)) => Some((*x, *y)),
            Token::RBrace((x, y)) => Some((*x, *y)),
            Token::LBracket((x, y)) => Some((*x, *y)),
            Token::RBracket((x, y)) => Some((*x, *y)),
            Token::Comma((x, y)) => Some((*x, *y)),
            Token::Colon((x, y)) => Some((*x, *y)),
            Token::Semicolon((x, y)) => Some((*x, *y)),
            Token::At((x, y)) => Some((*x, *y)),
            Token::Dot((x, y)) => Some((*x, *y)),
            Token::DotDotEq((x, y)) => Some((*x, *y)),
            Token::DotDot((x, y)) => Some((*x, *y)),
            Token::DotDotDot((x, y)) => Some((*x, *y)),
            Token::Tilde((x, y)) => Some((*x, *y)),
            Token::Bang((x, y)) => Some((*x, *y)),
            Token::If((x, y)) => Some((*x, *y)),
            Token::Else((x, y)) => Some((*x, *y)),
            Token::While((x, y)) => Some((*x, *y)),
            Token::Return((x, y)) => Some((*x, *y)),
            Token::True((x, y)) => Some((*x, *y)),
            Token::False((x, y)) => Some((*x, *y)),
            Token::And((x, y)) => Some((*x, *y)),
            Token::Match((x, y)) => Some((*x, *y)),
            Token::Do((x, y)) => Some((*x, *y)),
            Token::Then((x, y)) => Some((*x, *y)),
            Token::With((x, y)) => Some((*x, *y)),
            Token::Macro((x, y)) => Some((*x, *y)),
            Token::Const((x, y)) => Some((*x, *y)),
            Token::SelfToken((x, y)) => Some((*x, *y)),
            Token::Pipe((x, y)) => Some((*x, *y)),
            Token::LDoubleBracket((x, y)) => Some((*x, *y)),
            Token::RDoubleBracket((x, y)) => Some((*x, *y)),
            Token::LeftArrow((x, y)) => Some((*x, *y)),
            Token::Dollar((x, y)) => Some((*x, *y)),
            Token::ExprFragment((x, y)) => Some((*x, *y)),
            Token::Static((x, y)) => Some((*x, *y)),
            Token::Async((x, y)) => Some((*x, *y)),
            Token::Await((x, y)) => Some((*x, *y)),
            Token::Spawn((x, y)) => Some((*x, *y)),
            Token::Identifier(_) => None,
            Token::StringLiteral(_) => None,
            Token::IntegerLiteral(_) => None,
            Token::FloatLiteral(_) => None,
        }
    }

    pub fn source_pos(&self) -> Position {
        match self {
            Token::Fn((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Proc((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Let((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Struct((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Kind((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Impl((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::End((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::For((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::In((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Where((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::As((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Or((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Mod((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Import((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Pub((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::From((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Var((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Mut((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Break((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Continue((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Enum((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Plex((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Equals((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Plus((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Minus((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Star((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Slash((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Arrow((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::FatArrow((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::ExpOp((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Modulo((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::PlusEq((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::MinusEq((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::StarEq((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::SlashEq((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::LessEq((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::GreaterEq((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Less((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Greater((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::EqualEqual((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::NotEqual((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::LParen((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::RParen((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::LBrace((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::RBrace((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::LBracket((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::RBracket((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Comma((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Colon((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Semicolon((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::At((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Dot((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::DotDotEq((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::DotDot((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::DotDotDot((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Tilde((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Bang((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::If((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Else((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::While((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Return((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::True((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::False((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::And((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Match((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Do((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Then((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::With((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Macro((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Const((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::SelfToken((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Pipe((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::LDoubleBracket((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::RDoubleBracket((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::LeftArrow((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Dollar((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::ExprFragment((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Static((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Async((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Await((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Spawn((x, y)) => Position {
                line: *x as u32,
                column: *y as u32,
            },
            Token::Identifier(word) => Position {
                line: word.1.0 as u32,
                column: word.1.1 as u32,
            },
            Token::StringLiteral(word) => Position {
                line: word.1.0 as u32,
                column: word.1.1 as u32,
            },
            Token::IntegerLiteral(int) => Position {
                line: int.1.0 as u32,
                column: int.1.1 as u32,
            },
            Token::FloatLiteral(float) => Position {
                line: float.1.0 as u32,
                column: float.1.1 as u32,
            },
        }
    }
}

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    /// Preprocess the input string to remove comments and handle multiline comments.
    pub fn preprocess(input: &'a str) -> String {
        let _span = span!(
            Level::TRACE,
            "preprocess",
            purpose = "Handle complex comments"
        );
        let _enter = _span.enter();
        let mut result = String::new();
        let mut in_multiline_comment = false;
        let mut nesting_level = 0;

        for line in input.lines() {
            // Check for both types of multiline comment starts
            if let Some(pos) = line.find("#[[") {
                if !in_multiline_comment {
                    // Keep text before the comment start
                    result.push_str(&line[0..pos]);
                    in_multiline_comment = true;
                    nesting_level = 1;
                } else {
                    // Nested comment
                    nesting_level += 1;
                }
            } else if let Some(pos) = line.find("#|[[") {
                // Added check for doc comments
                if !in_multiline_comment {
                    // Keep text before the comment start
                    result.push_str(&line[0..pos]);
                    in_multiline_comment = true;
                    nesting_level = 1;
                } else {
                    // Nested comment
                    nesting_level += 1;
                }
            } else if !in_multiline_comment {
                // Keep the whole line if not in a comment
                result.push_str(line);
                result.push('\n');
            }

            // Check for the comment end
            if in_multiline_comment && line.contains("]]") {
                nesting_level -= 1;
                if nesting_level == 0 {
                    in_multiline_comment = false;
                    // If there's text after the comment end, add it
                    if let Some(pos) = line.rfind("]]") {
                        result.push_str(&line[(pos + 2)..]);
                        result.push('\n');
                    }
                }
            }
        }

        result
    }
    pub fn new(input: &'a str) -> Self {
        let preprocessed_input = Self::preprocess(input);
        Self {
            inner: Token::lexer(Box::leak(preprocessed_input.into_boxed_str())),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match (self, other) {
            (Token::Fn(_), Token::Fn(_)) => true,
            (Token::Proc(_), Token::Proc(_)) => true,
            (Token::Let(_), Token::Let(_)) => true,
            (Token::Struct(_), Token::Struct(_)) => true,
            (Token::Kind(_), Token::Kind(_)) => true,
            (Token::Impl(_), Token::Impl(_)) => true,
            (Token::End(_), Token::End(_)) => true,
            (Token::For(_), Token::For(_)) => true,
            (Token::In(_), Token::In(_)) => true,
            (Token::Where(_), Token::Where(_)) => true,
            (Token::As(_), Token::As(_)) => true,
            (Token::Or(_), Token::Or(_)) => true,
            (Token::Mod(_), Token::Mod(_)) => true,
            (Token::Import(_), Token::Import(_)) => true,
            (Token::Pub(_), Token::Pub(_)) => true,
            (Token::From(_), Token::From(_)) => true,
            (Token::Var(_), Token::Var(_)) => true,
            (Token::Mut(_), Token::Mut(_)) => true,
            (Token::Break(_), Token::Break(_)) => true,
            (Token::Continue(_), Token::Continue(_)) => true,
            (Token::Enum(_), Token::Enum(_)) => true,
            (Token::Plex(_), Token::Plex(_)) => true,
            (Token::Equals(_), Token::Equals(_)) => true,
            (Token::Plus(_), Token::Plus(_)) => true,
            (Token::Minus(_), Token::Minus(_)) => true,
            (Token::Star(_), Token::Star(_)) => true,
            (Token::Slash(_), Token::Slash(_)) => true,
            (Token::Arrow(_), Token::Arrow(_)) => true,
            (Token::FatArrow(_), Token::FatArrow(_)) => true,
            (Token::ExpOp(_), Token::ExpOp(_)) => true,
            (Token::Modulo(_), Token::Modulo(_)) => true,
            (Token::PlusEq(_), Token::PlusEq(_)) => true,
            (Token::MinusEq(_), Token::MinusEq(_)) => true,
            (Token::StarEq(_), Token::StarEq(_)) => true,
            (Token::SlashEq(_), Token::SlashEq(_)) => true,
            (Token::LessEq(_), Token::LessEq(_)) => true,
            (Token::GreaterEq(_), Token::GreaterEq(_)) => true,
            (Token::Less(_), Token::Less(_)) => true,
            (Token::Greater(_), Token::Greater(_)) => true,
            (Token::EqualEqual(_), Token::EqualEqual(_)) => true,
            (Token::NotEqual(_), Token::NotEqual(_)) => true,
            (Token::LParen(_), Token::LParen(_)) => true,
            (Token::RParen(_), Token::RParen(_)) => true,
            (Token::LBrace(_), Token::LBrace(_)) => true,
            (Token::RBrace(_), Token::RBrace(_)) => true,
            (Token::LBracket(_), Token::LBracket(_)) => true,
            (Token::RBracket(_), Token::RBracket(_)) => true,
            (Token::Comma(_), Token::Comma(_)) => true,
            (Token::Colon(_), Token::Colon(_)) => true,
            (Token::Semicolon(_), Token::Semicolon(_)) => true,
            (Token::At(_), Token::At(_)) => true,
            (Token::Dot(_), Token::Dot(_)) => true,
            (Token::DotDotEq(_), Token::DotDotEq(_)) => true,
            (Token::DotDot(_), Token::DotDot(_)) => true,
            (Token::DotDotDot(_), Token::DotDotDot(_)) => true,
            (Token::Tilde(_), Token::Tilde(_)) => true,
            (Token::Bang(_), Token::Bang(_)) => true,
            (Token::Identifier(a), Token::Identifier(b)) => a == b,
            (Token::StringLiteral(a), Token::StringLiteral(b)) => a == b,
            (Token::IntegerLiteral(a), Token::IntegerLiteral(b)) => a == b,
            (Token::FloatLiteral(a), Token::FloatLiteral(b)) => a == b,
            (Token::If(_), Token::If(_)) => true,
            (Token::Else(_), Token::Else(_)) => true,
            (Token::While(_), Token::While(_)) => true,
            (Token::Return(_), Token::Return(_)) => true,
            (Token::True(_), Token::True(_)) => true,
            (Token::False(_), Token::False(_)) => true,
            (Token::And(_), Token::And(_)) => true,
            (Token::Match(_), Token::Match(_)) => true,
            (Token::Do(_), Token::Do(_)) => true,
            (Token::Then(_), Token::Then(_)) => true,
            (Token::With(_), Token::With(_)) => true,
            (Token::Macro(_), Token::Macro(_)) => true,
            (Token::Const(_), Token::Const(_)) => true,
            (Token::SelfToken(_), Token::SelfToken(_)) => true,
            (Token::Pipe(_), Token::Pipe(_)) => true,
            (Token::LDoubleBracket(_), Token::LDoubleBracket(_)) => true,
            (Token::RDoubleBracket(_), Token::RDoubleBracket(_)) => true,
            (Token::LeftArrow(_), Token::LeftArrow(_)) => true,
            (Token::Dollar(_), Token::Dollar(_)) => true,
            (Token::ExprFragment(_), Token::ExprFragment(_)) => true,
            (Token::Static(_), Token::Static(_)) => true,
            (Token::Async(_), Token::Async(_)) => true,
            (Token::Await(_), Token::Await(_)) => true,
            (Token::Spawn(_), Token::Spawn(_)) => true,
            _ => false,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|result| result.map_err(|_| "Invalid token".to_string()))
    }
}

impl<'a> Lexer<'a> {
    pub fn collect_tokens(&mut self) -> Result<Vec<Token>, String> {
        let _span = span!(
            Level::TRACE,
            "collect_tokens",
            purpose = "Collect tokens from the input string"
        );
        let _enter = _span.enter();

        self.collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = r#"
fn main()
    let x = 42
    let y = "Hello, world!"
    if x > 0 then
        println~(y)
    end
end
                        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.collect_tokens().unwrap();

        assert_eq!(tokens.len(), 24);
        assert!(
            matches!(tokens[0], Token::Fn((1, 0))),
            "Expected {} ",
            tokens[0],
        );
        assert!(matches!(
            tokens[1],
            Token::Identifier((ref name, (_, _))) if name == "main"
        ));
        assert!(matches!(tokens[2], Token::LParen((1, 7))));
        assert!(
            matches!(tokens[3], Token::RParen((1, 8))),
            "Expected {}, got {}",
            tokens[3],
            Token::RParen((1, 8))
        );
        assert!(
            matches!(tokens[4], Token::Let((2, 4))),
            "Expected {}, got {}",
            tokens[4],
            Token::Let((2, 4)),
        );
        assert!(matches!(
            tokens[5],
            Token::Identifier((ref name, (_, _))) if name == "x"
        ));
        assert!(
            matches!(tokens[6], Token::Equals((2, 10))),
            "Expected {}, got {}",
            tokens[6],
            Token::Equals((0, 0)),
        );
        assert!(
            matches!(tokens[7], Token::IntegerLiteral((42, (2, 12)))),
            "Expected integer literal at position (2, 12), got {}",
            tokens[7]
        );
        assert!(
            matches!(tokens[8], Token::Let((3, 4))),
            "Expected {} ",
            tokens[0],
        );
        assert!(matches!(
            tokens[9],
            Token::Identifier((ref name, (_, _))) if name == "y"
        ));
        assert!(
            matches!(tokens[10], Token::Equals((3, 10))),
            "Expected {}, got {}",
            tokens[10],
            Token::Equals((3, 10)),
        );
        assert_eq!(
            tokens[11],
            Token::StringLiteral(("Hello, world!".to_string(), (3, 12)))
        );
        assert!(
            matches!(tokens[12], Token::If((4, 4))),
            "Expected {}, got {}",
            tokens[12],
            Token::If((4, 4))
        );
        assert!(matches!(
            tokens[13],
            Token::Identifier((ref name, (_, _))) if name == "x"
        ));
        assert!(
            matches!(tokens[14], Token::Greater((4, 9))),
            "Expected {}, got {}",
            tokens[14],
            Token::Greater((0, 0))
        );
        assert!(matches!(tokens[15], Token::IntegerLiteral((0, _))));
        assert!(
            matches!(tokens[16], Token::Then(_)),
            "Expected {}, got {}",
            tokens[16],
            Token::Then((0, 0))
        );
        assert!(matches!(
            tokens[17],
            Token::Identifier((ref name, (_, _))) if name == "println"
        ));
        assert!(
            matches!(tokens[18], Token::Tilde((5, 15))),
            "Expected {}, got {}",
            tokens[18],
            Token::Tilde((5, 15))
        );
        assert!(
            matches!(tokens[19], Token::LParen((5, 16))),
            "Expected {}, got {}",
            tokens[19],
            Token::LParen((5, 16))
        );
        assert!(matches!(
            tokens[20],
            Token::Identifier((ref name, (_, _))) if name == "y"
        ));
        assert!(
            matches!(tokens[21], Token::RParen((5, 18))),
            "Expected {}, got {}",
            tokens[21],
            Token::RParen((5, 18))
        );
        // assert_eq!(tokens[22], Token::End((6, 4)));
        assert!(
            matches!(tokens[22], Token::End((6, 4))),
            "Expected {} ",
            tokens[0],
        );
    }
}
