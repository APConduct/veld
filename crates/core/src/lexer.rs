use std::fmt::Display;

use logos::{Lexer as LLexer, Logos, Skip};

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
    #[token("<=")]
    LessEq,
    #[token(">=")]
    GreaterEq,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("@")]
    At,
    #[token(".")]
    Dot,
    #[token("..=")]
    DotDotEq,
    #[token("..")]
    DotDot,
    #[token("...")]
    DotDotDot,
    #[token("~")]
    Tilde,
    #[token("!")]
    Bang,

    // Literals and Identifiers
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r#""[^"]*""#, |lex| {
        let mut content = lex.slice().to_string();
        content.remove(0);
        content.pop();
        content
    })]
    StringLiteral(String),

    #[regex(r#"[0-9]+"#, |lex| lex.slice().parse().ok())]
    IntegerLiteral(i64),

    #[regex(r#"[0-9]+\.[0-9]+"#, |lex| lex.slice().parse().ok())]
    FloatLiteral(f64),

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("and")]
    And,
    #[token("match")]
    Match,
    #[token("do")]
    Do,
    #[token("then")]
    Then,
    #[token("with")]
    With,
    #[token("macro")]
    Macro,
    #[token("const")]
    Const,
    #[token("self")]
    SelfToken,
    #[token("|>")]
    Pipe,
    #[token("[[")]
    LDoubleBracket,
    #[token("]]")]
    RDoubleBracket,
    #[token("<-")]
    LeftArrow,
    #[token("$")]
    Dollar,
    #[token(":expr")]
    ExprFragment,

    #[token("static")]
    Static,
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("spawn")]
    Spawn,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Fn(_) => write!(f, "fn"),
            Token::Proc(_) => write!(f, "proc"),
            Token::Let(_) => write!(f, "let"),
            Token::Struct(_) => write!(f, "struct"),
            Token::Kind(_) => write!(f, "kind"),
            Token::Impl(_) => write!(f, "impl"),
            Token::End(_) => write!(f, "end"),
            Token::For(_) => write!(f, "for"),
            Token::In(_) => write!(f, "in"),
            Token::Where(_) => write!(f, "where"),
            Token::As(_) => write!(f, "as"),
            Token::Or(_) => write!(f, "or"),
            Token::Mod(_) => write!(f, "mod"),
            Token::Import(_) => write!(f, "import"),
            Token::Pub(_) => write!(f, "pub"),
            Token::From(_) => write!(f, "from"),
            Token::Var(_) => write!(f, "var"),
            Token::Mut(_) => write!(f, "mut"),
            Token::Break(_) => write!(f, "break"),
            Token::Continue(_) => write!(f, "continue"),
            Token::Enum(_) => write!(f, "enum"),
            Token::Equals((x, y)) => write!(f, "= at {},{}", x, y),
            Token::Plus(_) => write!(f, "+"),
            Token::Minus(_) => write!(f, "-"),
            Token::Star(_) => write!(f, "*"),
            Token::Slash(_) => write!(f, "/"),
            Token::Arrow(_) => write!(f, "->"),
            Token::FatArrow(_) => write!(f, "=>"),
            Token::ExpOp(_) => write!(f, "*^"),
            Token::Modulo(_) => write!(f, "%"),
            Token::PlusEq(_) => write!(f, "+="),
            Token::MinusEq(_) => write!(f, "-="),
            Token::StarEq(_) => write!(f, "*="),
            Token::SlashEq(_) => write!(f, "/="),
            Token::LessEq => write!(f, "<="),
            Token::GreaterEq => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::EqualEqual => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::At => write!(f, "@"),
            Token::Dot => write!(f, "."),
            Token::DotDotEq => write!(f, "..="),
            Token::DotDot => write!(f, ".."),
            Token::DotDotDot => write!(f, "..."),
            Token::Tilde => write!(f, "~"),
            Token::Bang => write!(f, "!"),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::IntegerLiteral(n) => write!(f, "{}", n),
            Token::FloatLiteral(n) => write!(f, "{}", n),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::Return => write!(f, "return"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::And => write!(f, "and"),
            Token::Match => write!(f, "match"),
            Token::Do => write!(f, "do"),
            Token::Then => write!(f, "then"),
            Token::With => write!(f, "with"),
            Token::Macro => write!(f, "macro"),
            Token::Const => write!(f, "const"),
            Token::Plex(_) => write!(f, "plex"),
            Token::SelfToken => write!(f, "self"),
            Token::Pipe => write!(f, "|>"),
            Token::LDoubleBracket => write!(f, "[["),
            Token::RDoubleBracket => write!(f, "]]"),
            Token::LeftArrow => write!(f, "<-"),
            Token::Dollar => write!(f, "$"),
            Token::ExprFragment => write!(f, ":expr"),
            Token::Static => write!(f, "static"),
            Token::Async => write!(f, "async"),
            Token::Await => write!(f, "await"),
            Token::Spawn => write!(f, "spawn"),
        }
    }
}

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    /// Preprocess the input string to remove comments and handle multiline comments.
    pub fn preprocess(input: &'a str) -> String {
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
            (Token::LessEq, Token::LessEq) => true,
            (Token::GreaterEq, Token::GreaterEq) => true,
            (Token::Less, Token::Less) => true,
            (Token::Greater, Token::Greater) => true,
            (Token::EqualEqual, Token::EqualEqual) => true,
            (Token::NotEqual, Token::NotEqual) => true,
            (Token::LParen, Token::LParen) => true,
            (Token::RParen, Token::RParen) => true,
            (Token::LBrace, Token::LBrace) => true,
            (Token::RBrace, Token::RBrace) => true,
            (Token::LBracket, Token::LBracket) => true,
            (Token::RBracket, Token::RBracket) => true,
            (Token::Comma, Token::Comma) => true,
            (Token::Colon, Token::Colon) => true,
            (Token::Semicolon, Token::Semicolon) => true,
            (Token::At, Token::At) => true,
            (Token::Dot, Token::Dot) => true,
            (Token::DotDotEq, Token::DotDotEq) => true,
            (Token::DotDot, Token::DotDot) => true,
            (Token::DotDotDot, Token::DotDotDot) => true,
            (Token::Tilde, Token::Tilde) => true,
            (Token::Bang, Token::Bang) => true,
            (Token::Identifier(a), Token::Identifier(b)) => a == b,
            (Token::StringLiteral(a), Token::StringLiteral(b)) => a == b,
            (Token::IntegerLiteral(a), Token::IntegerLiteral(b)) => a == b,
            (Token::FloatLiteral(a), Token::FloatLiteral(b)) => a == b,
            (Token::If, Token::If) => true,
            (Token::Else, Token::Else) => true,
            (Token::While, Token::While) => true,
            (Token::Return, Token::Return) => true,
            (Token::True, Token::True) => true,
            (Token::False, Token::False) => true,
            (Token::And, Token::And) => true,
            (Token::Match, Token::Match) => true,
            (Token::Do, Token::Do) => true,
            (Token::Then, Token::Then) => true,
            (Token::With, Token::With) => true,
            (Token::Macro, Token::Macro) => true,
            (Token::Const, Token::Const) => true,
            (Token::SelfToken, Token::SelfToken) => true,
            (Token::Pipe, Token::Pipe) => true,
            (Token::LDoubleBracket, Token::LDoubleBracket) => true,
            (Token::RDoubleBracket, Token::RDoubleBracket) => true,
            (Token::LeftArrow, Token::LeftArrow) => true,
            (Token::Dollar, Token::Dollar) => true,
            (Token::ExprFragment, Token::ExprFragment) => true,
            (Token::Static, Token::Static) => true,
            (Token::Async, Token::Async) => true,
            (Token::Await, Token::Await) => true,
            (Token::Spawn, Token::Spawn) => true,
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
        assert_eq!(tokens[1], Token::Identifier("main".to_string()));
        assert_eq!(tokens[2], Token::LParen);
        assert_eq!(tokens[3], Token::RParen);
        assert!(
            matches!(tokens[4], Token::Let((2, 4))),
            "Expected {}, got {}",
            tokens[4],
            Token::Let((2, 4)),
        );
        assert_eq!(tokens[5], Token::Identifier("x".to_string()));
        assert!(
            matches!(tokens[6], Token::Equals((2, 10))),
            "Expected {}, got {}",
            tokens[6],
            Token::Equals((0, 0)),
        );
        assert_eq!(tokens[7], Token::IntegerLiteral(42));
        assert!(
            matches!(tokens[8], Token::Let((3, 4))),
            "Expected {} ",
            tokens[0],
        );
        assert_eq!(tokens[9], Token::Identifier("y".to_string()));
        assert!(
            matches!(tokens[10], Token::Equals((3, 10))),
            "Expected {}, got {}",
            tokens[10],
            Token::Equals((3, 10)),
        );
        assert_eq!(
            tokens[11],
            Token::StringLiteral("Hello, world!".to_string())
        );
        assert_eq!(tokens[12], Token::If);
        assert_eq!(tokens[13], Token::Identifier("x".to_string()));
        assert_eq!(tokens[14], Token::Greater);
        assert_eq!(tokens[15], Token::IntegerLiteral(0));
        assert_eq!(tokens[16], Token::Then);
        assert_eq!(tokens[17], Token::Identifier("println".to_string()));
        assert_eq!(tokens[18], Token::Tilde);
        assert_eq!(tokens[19], Token::LParen);
        assert_eq!(tokens[20], Token::Identifier("y".to_string()));
        assert_eq!(tokens[21], Token::RParen);
        // assert_eq!(tokens[22], Token::End((6, 4)));
        assert!(
            matches!(tokens[22], Token::End((6, 4))),
            "Expected {} ",
            tokens[0],
        );
    }
}
