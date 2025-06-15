use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")] // Skip whitespace
#[logos(skip r"#.*")] // Skip single-line comments
#[logos(skip r"#\|")] // Skip multi-line comments
pub enum Token {
    // Keywords
    #[token("fn")]
    Fn,
    #[token("proc")]
    Proc,
    #[token("let")]
    Let,
    #[token("struct")]
    Struct,
    #[token("kind")]
    Kind,
    #[token("impl")]
    Impl,
    #[token("end")]
    End,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("where")]
    Where,
    #[token("as")]
    As,
    #[token("or")]
    Or,
    #[token("mod")]
    Mod,
    #[token("import")]
    Import,
    #[token("pub")]
    Pub,
    #[token("from")]
    From,
    #[token("var")]
    Var,
    #[token("mut")]
    Mut,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("enum")]
    Enum,

    // Operators
    #[token("=")]
    Equals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("*^")]
    ExpOp,
    #[token("%")]
    Modulo,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,

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
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("spawn")]
    Spawn,
    #[token("const")]
    Const,
    #[token("static")]
    Static,
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
                    println(y)
                end
            end
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.collect_tokens().unwrap();

        assert_eq!(tokens.len(), 23);
        assert_eq!(tokens[0], Token::Fn);
        assert_eq!(tokens[1], Token::Identifier("main".to_string()));
        assert_eq!(tokens[2], Token::LParen);
        assert_eq!(tokens[3], Token::RParen);
        assert_eq!(tokens[4], Token::Let);
        assert_eq!(tokens[5], Token::Identifier("x".to_string()));
        assert_eq!(tokens[6], Token::Equals);
        assert_eq!(tokens[7], Token::IntegerLiteral(42));
        assert_eq!(tokens[8], Token::Let);
        assert_eq!(tokens[9], Token::Identifier("y".to_string()));
        assert_eq!(tokens[10], Token::Equals);
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
        assert_eq!(tokens[18], Token::LParen);
        assert_eq!(tokens[19], Token::Identifier("y".to_string()));
        assert_eq!(tokens[20], Token::RParen);
        assert_eq!(tokens[21], Token::End);
    }
}
