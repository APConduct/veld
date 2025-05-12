use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")] // Skip whitespace
#[logos(skip r"--.*")]
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
    #[token("--|")]
    DashDashPipe,

    // Literals and Identifiers
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r#""[^"]*""#, |lex| {
        let mut content = lex.slice().to_string();
        // Remove the surrounding quotes
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
    // #[token("yield")]
    // Yield,
    #[token("const")]
    Const,
    #[token("static")]
    Static,
    // #[token("self")]
    // Self,
}

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: Token::lexer(input),
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

// Convenience method to collect all tokens
impl<'a> Lexer<'a> {
    pub fn collect_tokens(&mut self) -> Result<Vec<Token>, String> {
        self.collect()
    }
}
