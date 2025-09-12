use veld_core::common::source::NodeId;

use crate::{ExpansionError, FragmentType, PatternElement};

#[derive(Debug, Clone)]
pub struct MacroParser {
    tokens: Vec<Token>,
    position: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Boolean(bool),

    // Keywords
    Macro,
    End,
    Do,

    // Operators and punctuation
    Tilde,        // ~
    Dollar,       // $
    Colon,        // :
    Comma,        // ,
    Semicolon,    // ;
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    LeftAngle,    // <
    RightAngle,   // >
    Plus,         // +
    Minus,        // -
    Star,         // *
    Question,     // ?
    Arrow,        // =>
    FatArrow,     // =>

    // Special
    Whitespace,
    Newline,
    Eof,
}

#[derive(Debug, Clone)]
pub struct MacroPatternParser {
    parser: MacroParser,
}

impl MacroParser {
    pub fn new(input: &str) -> Self {
        let tokens = Self::tokenize(input);
        Self {
            tokens,
            position: 0,
        }
    }

    fn tokenize(input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                ' ' | '\t' => {
                    chars.next();
                    tokens.push(Token::Whitespace);
                }
                '\n' | '\r' => {
                    chars.next();
                    tokens.push(Token::Newline);
                }
                '~' => {
                    chars.next();
                    tokens.push(Token::Tilde);
                }
                '$' => {
                    chars.next();
                    tokens.push(Token::Dollar);
                }
                ':' => {
                    chars.next();
                    tokens.push(Token::Colon);
                }
                ',' => {
                    chars.next();
                    tokens.push(Token::Comma);
                }
                ';' => {
                    chars.next();
                    tokens.push(Token::Semicolon);
                }
                '(' => {
                    chars.next();
                    tokens.push(Token::LeftParen);
                }
                ')' => {
                    chars.next();
                    tokens.push(Token::RightParen);
                }
                '{' => {
                    chars.next();
                    tokens.push(Token::LeftBrace);
                }
                '}' => {
                    chars.next();
                    tokens.push(Token::RightBrace);
                }
                '[' => {
                    chars.next();
                    tokens.push(Token::LeftBracket);
                }
                ']' => {
                    chars.next();
                    tokens.push(Token::RightBracket);
                }
                '<' => {
                    chars.next();
                    tokens.push(Token::LeftAngle);
                }
                '>' => {
                    chars.next();
                    tokens.push(Token::RightAngle);
                }
                '+' => {
                    chars.next();
                    tokens.push(Token::Plus);
                }
                '-' => {
                    chars.next();
                    tokens.push(Token::Minus);
                }
                '*' => {
                    chars.next();
                    tokens.push(Token::Star);
                }
                '?' => {
                    chars.next();
                    tokens.push(Token::Question);
                }
                '=' => {
                    chars.next();
                    if chars.peek() == Some(&'>') {
                        chars.next();
                        tokens.push(Token::FatArrow);
                    } else {
                        // Handle other = cases if needed
                        tokens.push(Token::Identifier("=".to_string()));
                    }
                }
                '"' => {
                    chars.next(); // consume opening quote
                    let mut string_content = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == '"' {
                            chars.next(); // consume closing quote
                            break;
                        } else if ch == '\\' {
                            chars.next(); // consume backslash
                            if let Some(&escaped) = chars.peek() {
                                chars.next();
                                match escaped {
                                    'n' => string_content.push('\n'),
                                    't' => string_content.push('\t'),
                                    'r' => string_content.push('\r'),
                                    '\\' => string_content.push('\\'),
                                    '"' => string_content.push('"'),
                                    _ => {
                                        string_content.push('\\');
                                        string_content.push(escaped);
                                    }
                                }
                            }
                        } else {
                            string_content.push(chars.next().unwrap());
                        }
                    }
                    tokens.push(Token::String(string_content));
                }
                '\'' => {
                    chars.next(); // consume opening quote
                    if let Some(&ch) = chars.peek() {
                        let char_value = if ch == '\\' {
                            chars.next(); // consume backslash
                            if let Some(&escaped) = chars.peek() {
                                chars.next();
                                match escaped {
                                    'n' => '\n',
                                    't' => '\t',
                                    'r' => '\r',
                                    '\\' => '\\',
                                    '\'' => '\'',
                                    _ => escaped,
                                }
                            } else {
                                ch
                            }
                        } else {
                            chars.next().unwrap()
                        };

                        if chars.peek() == Some(&'\'') {
                            chars.next(); // consume closing quote
                        }
                        tokens.push(Token::Char(char_value));
                    }
                }
                ch if ch.is_alphabetic() || ch == '_' => {
                    let mut identifier = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            identifier.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    let token = match identifier.as_str() {
                        "macro" => Token::Macro,
                        "end" => Token::End,
                        "do" => Token::Do,
                        "true" => Token::Boolean(true),
                        "false" => Token::Boolean(false),
                        _ => Token::Identifier(identifier),
                    };
                    tokens.push(token);
                }
                ch if ch.is_numeric() => {
                    let mut number = String::new();
                    let mut is_float = false;

                    while let Some(&ch) = chars.peek() {
                        if ch.is_numeric() {
                            number.push(chars.next().unwrap());
                        } else if ch == '.' && !is_float {
                            is_float = true;
                            number.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    if is_float {
                        if let Ok(f) = number.parse::<f64>() {
                            tokens.push(Token::Float(f));
                        }
                    } else {
                        if let Ok(i) = number.parse::<i64>() {
                            tokens.push(Token::Integer(i));
                        }
                    }
                }
                _ => {
                    chars.next(); // Skip unknown characters
                }
            }
        }

        tokens.push(Token::Eof);
        tokens
    }

    fn current_token(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or(&Token::Eof)
    }

    #[allow(unused)]
    fn peek_token(&self) -> &Token {
        self.tokens.get(self.position + 1).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> &Token {
        if self.position < self.tokens.len() - 1 {
            self.position += 1;
        }
        self.current_token()
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.current_token(), Token::Whitespace | Token::Newline) {
            self.advance();
        }
    }

    pub fn parse_macro_definition(
        &mut self,
    ) -> Result<(String, Vec<(String, String)>), ExpansionError> {
        self.skip_whitespace();

        // Expect 'macro~'
        if !matches!(self.current_token(), Token::Macro) {
            return Err(ExpansionError::InvalidSyntaxGenerated {
                macro_name: "unknown".to_string(),
                error: "Expected 'macro' keyword".to_string(),
                call_site: NodeId::new(),
            });
        }
        self.advance();

        if !matches!(self.current_token(), Token::Tilde) {
            return Err(ExpansionError::InvalidSyntaxGenerated {
                macro_name: "unknown".to_string(),
                error: "Expected '~' after 'macro'".to_string(),
                call_site: NodeId::new(),
            });
        }
        self.advance();

        self.skip_whitespace();

        // Get macro name
        let macro_name = match self.current_token() {
            Token::Identifier(name) => name.clone(),
            _ => {
                return Err(ExpansionError::InvalidSyntaxGenerated {
                    macro_name: "unknown".to_string(),
                    error: "Expected macro name".to_string(),
                    call_site: NodeId::new(),
                });
            }
        };
        self.advance();

        self.skip_whitespace();

        // Parse macro patterns and templates
        let mut patterns_and_templates = Vec::new();

        while !matches!(self.current_token(), Token::End | Token::Eof) {
            let pattern = self.parse_macro_pattern()?;

            self.skip_whitespace();

            // Expect '=>'
            if !matches!(self.current_token(), Token::FatArrow) {
                return Err(ExpansionError::InvalidSyntaxGenerated {
                    macro_name: macro_name.clone(),
                    error: "Expected '=>' after pattern".to_string(),
                    call_site: NodeId::new(),
                });
            }
            self.advance();

            self.skip_whitespace();

            let template = self.parse_macro_template()?;

            patterns_and_templates.push((pattern, template));

            self.skip_whitespace();

            // Skip optional comma
            if matches!(self.current_token(), Token::Comma) {
                self.advance();
                self.skip_whitespace();
            }
        }

        // Expect 'end'
        if !matches!(self.current_token(), Token::End) {
            return Err(ExpansionError::InvalidSyntaxGenerated {
                macro_name: macro_name.clone(),
                error: "Expected 'end' to close macro".to_string(),
                call_site: NodeId::new(),
            });
        }

        Ok((macro_name, patterns_and_templates))
    }

    fn parse_macro_pattern(&mut self) -> Result<String, ExpansionError> {
        let mut pattern = String::new();
        let mut paren_depth = 0;

        while !matches!(self.current_token(), Token::FatArrow | Token::Eof) {
            match self.current_token() {
                Token::LeftParen => {
                    paren_depth += 1;
                    pattern.push('(');
                }
                Token::RightParen => {
                    paren_depth -= 1;
                    pattern.push(')');
                    if paren_depth == 0 {
                        self.advance();
                        break;
                    }
                }
                Token::Identifier(name) => {
                    pattern.push_str(name);
                }
                Token::Dollar => {
                    pattern.push('$');
                }
                Token::Colon => {
                    pattern.push(':');
                }
                Token::Comma => {
                    pattern.push(',');
                }
                Token::Plus => {
                    pattern.push('+');
                }
                Token::Star => {
                    pattern.push('*');
                }
                Token::Question => {
                    pattern.push('?');
                }
                Token::Whitespace => {
                    pattern.push(' ');
                }
                Token::Integer(i) => {
                    pattern.push_str(&i.to_string());
                }
                Token::String(s) => {
                    pattern.push('"');
                    pattern.push_str(s);
                    pattern.push('"');
                }
                _ => {
                    // Add other token representations as needed
                }
            }
            self.advance();
        }

        Ok(pattern.trim().to_string())
    }

    fn parse_macro_template(&mut self) -> Result<String, ExpansionError> {
        let mut template = String::new();

        // Handle 'do' block templates
        if matches!(self.current_token(), Token::Do) {
            self.advance();
            self.skip_whitespace();

            // Parse until 'end'
            let mut depth = 1;
            while depth > 0 && !matches!(self.current_token(), Token::Eof) {
                match self.current_token() {
                    Token::Do => {
                        depth += 1;
                        template.push_str("do");
                    }
                    Token::End => {
                        depth -= 1;
                        if depth > 0 {
                            template.push_str("end");
                        }
                    }
                    Token::Identifier(name) => {
                        template.push_str(name);
                    }
                    Token::Dollar => {
                        template.push('$');
                    }
                    Token::LeftParen => {
                        template.push('(');
                    }
                    Token::RightParen => {
                        template.push(')');
                    }
                    Token::Comma => {
                        template.push(',');
                    }
                    Token::Whitespace => {
                        template.push(' ');
                    }
                    Token::Newline => {
                        template.push('\n');
                    }
                    Token::Integer(i) => {
                        template.push_str(&i.to_string());
                    }
                    Token::String(s) => {
                        template.push('"');
                        template.push_str(s);
                        template.push('"');
                    }
                    _ => {
                        // Add other token representations as needed
                    }
                }
                self.advance();
            }
        } else {
            // Simple expression template
            while !matches!(self.current_token(), Token::Comma | Token::End | Token::Eof) {
                match self.current_token() {
                    Token::Identifier(name) => {
                        template.push_str(name);
                    }
                    Token::Dollar => {
                        template.push('$');
                    }
                    Token::LeftParen => {
                        template.push('(');
                    }
                    Token::RightParen => {
                        template.push(')');
                    }
                    Token::Whitespace => {
                        template.push(' ');
                    }
                    Token::Integer(i) => {
                        template.push_str(&i.to_string());
                    }
                    Token::String(s) => {
                        template.push('"');
                        template.push_str(s);
                        template.push('"');
                    }
                    _ => {
                        // Add other token representations as needed
                    }
                }
                self.advance();
            }
        }

        Ok(template.trim().to_string())
    }
}

impl MacroPatternParser {
    pub fn new(pattern: &str) -> Self {
        Self {
            parser: MacroParser::new(pattern),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<PatternElement>, ExpansionError> {
        let mut elements = Vec::new();

        self.parser.skip_whitespace();

        while !matches!(self.parser.current_token(), Token::Eof) {
            elements.push(self.parse_pattern_element()?);
            self.parser.skip_whitespace();
        }

        Ok(elements)
    }

    fn parse_pattern_element(&mut self) -> Result<PatternElement, ExpansionError> {
        match self.parser.current_token() {
            Token::Dollar => {
                self.parser.advance();
                self.parse_variable()
            }
            Token::LeftParen => self.parse_group_or_repetition(),
            Token::Identifier(name) => {
                let name = name.clone();
                self.parser.advance();
                Ok(PatternElement::Literal(name))
            }
            Token::Integer(i) => {
                let value = i.to_string();
                self.parser.advance();
                Ok(PatternElement::Literal(value))
            }
            Token::String(s) => {
                let value = format!("\"{}\"", s);
                self.parser.advance();
                Ok(PatternElement::Literal(value))
            }
            Token::Comma => {
                self.parser.advance();
                Ok(PatternElement::Literal(",".to_string()))
            }
            _ => Err(ExpansionError::InvalidSyntaxGenerated {
                macro_name: "pattern".to_string(),
                error: format!(
                    "Unexpected token in pattern: {:?}",
                    self.parser.current_token()
                ),
                call_site: NodeId::new(),
            }),
        }
    }

    fn parse_variable(&mut self) -> Result<PatternElement, ExpansionError> {
        // Parse variable name
        let var_name = match self.parser.current_token() {
            Token::Identifier(name) => name.clone(),
            _ => {
                return Err(ExpansionError::InvalidSyntaxGenerated {
                    macro_name: "pattern".to_string(),
                    error: "Expected variable name after $".to_string(),
                    call_site: NodeId::new(),
                });
            }
        };
        self.parser.advance();

        // Parse optional type specifier
        let fragment_type = if matches!(self.parser.current_token(), Token::Colon) {
            self.parser.advance();
            match self.parser.current_token() {
                Token::Identifier(type_name) => {
                    let frag_type = match type_name.as_str() {
                        "expr" => FragmentType::Expr,
                        "stmt" => FragmentType::Stmt,
                        "ident" => FragmentType::Ident,
                        "literal" => FragmentType::Literal,
                        "tt" => FragmentType::TokenTree,
                        "item" => FragmentType::Item,
                        "block" => FragmentType::Block,
                        _ => FragmentType::TokenTree,
                    };
                    self.parser.advance();
                    frag_type
                }
                _ => FragmentType::TokenTree,
            }
        } else {
            FragmentType::TokenTree
        };

        Ok(PatternElement::Variable {
            name: var_name,
            fragment_type,
        })
    }

    fn parse_group_or_repetition(&mut self) -> Result<PatternElement, ExpansionError> {
        // Consume '('
        self.parser.advance();

        let mut group_elements = Vec::new();

        // Parse elements inside the group
        while !matches!(self.parser.current_token(), Token::RightParen | Token::Eof) {
            group_elements.push(self.parse_pattern_element()?);
            self.parser.skip_whitespace();
        }

        // Consume ')'
        if matches!(self.parser.current_token(), Token::RightParen) {
            self.parser.advance();
        }

        // Check for repetition operators
        match self.parser.current_token() {
            Token::Plus => {
                self.parser.advance();
                Ok(PatternElement::Repetition {
                    pattern: group_elements,
                    separator: None, // TODO: parse separator
                    min_count: 1,
                    max_count: None,
                })
            }
            Token::Star => {
                self.parser.advance();
                Ok(PatternElement::Repetition {
                    pattern: group_elements,
                    separator: None,
                    min_count: 0,
                    max_count: None,
                })
            }
            Token::Question => {
                self.parser.advance();
                Ok(PatternElement::Optional(group_elements))
            }
            _ => Ok(PatternElement::Group(group_elements)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenization() {
        let parser = MacroParser::new("macro~ test ($x:expr) => $x end");
        assert!(parser.tokens.len() > 0);
    }

    #[test]
    fn test_pattern_parsing() {
        let mut parser = MacroPatternParser::new("$x:expr");
        let elements = parser.parse().unwrap();
        assert_eq!(elements.len(), 1);

        match &elements[0] {
            PatternElement::Variable {
                name,
                fragment_type,
            } => {
                assert_eq!(name, "x");
                assert!(matches!(fragment_type, FragmentType::Expr));
            }
            _ => panic!("Expected variable pattern"),
        }
    }

    #[test]
    fn test_repetition_parsing() {
        let mut parser = MacroPatternParser::new("($x:expr)+");
        let elements = parser.parse().unwrap();
        assert_eq!(elements.len(), 1);

        match &elements[0] {
            PatternElement::Repetition {
                min_count,
                max_count,
                ..
            } => {
                assert_eq!(*min_count, 1);
                assert_eq!(*max_count, None);
            }
            _ => panic!("Expected repetition pattern"),
        }
    }

    #[test]
    fn test_string_tokenization() {
        let parser = MacroParser::new("\"hello world\"");
        assert_eq!(parser.tokens.len(), 2); // String + Eof
        match &parser.tokens[0] {
            Token::String(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected string token"),
        }
    }

    #[test]
    fn test_string_escaping() {
        let parser = MacroParser::new("\"hello\\nworld\"");
        match &parser.tokens[0] {
            Token::String(s) => assert_eq!(s, "hello\nworld"),
            _ => panic!("Expected string token"),
        }
    }

    #[test]
    fn test_char_tokenization() {
        let parser = MacroParser::new("'a'");
        match &parser.tokens[0] {
            Token::Char(c) => assert_eq!(*c, 'a'),
            _ => panic!("Expected char token"),
        }
    }

    #[test]
    fn test_escaped_char_tokenization() {
        let parser = MacroParser::new("'\\n'");
        match &parser.tokens[0] {
            Token::Char(c) => assert_eq!(*c, '\n'),
            _ => panic!("Expected char token"),
        }
    }

    #[test]
    fn test_integer_tokenization() {
        let parser = MacroParser::new("42");
        match &parser.tokens[0] {
            Token::Integer(i) => assert_eq!(*i, 42),
            _ => panic!("Expected integer token"),
        }
    }

    #[test]
    fn test_float_tokenization() {
        let parser = MacroParser::new("3.14");
        match &parser.tokens[0] {
            Token::Float(f) => assert_eq!(*f, 3.14),
            _ => panic!("Expected float token"),
        }
    }

    #[test]
    fn test_boolean_tokenization() {
        let parser = MacroParser::new("true false");
        assert_eq!(parser.tokens.len(), 4); // true + whitespace + false + Eof
        match &parser.tokens[0] {
            Token::Boolean(b) => assert_eq!(*b, true),
            _ => panic!("Expected boolean token"),
        }
        match &parser.tokens[2] {
            Token::Boolean(b) => assert_eq!(*b, false),
            _ => panic!("Expected boolean token"),
        }
    }

    #[test]
    fn test_keyword_tokenization() {
        let parser = MacroParser::new("macro end do");
        assert_eq!(parser.tokens[0], Token::Macro);
        assert_eq!(parser.tokens[2], Token::End);
        assert_eq!(parser.tokens[4], Token::Do);
    }

    #[test]
    fn test_punctuation_tokenization() {
        let parser = MacroParser::new("(){}[]<>+-*?:,;~$");
        let expected = vec![
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::RightBrace,
            Token::LeftBracket,
            Token::RightBracket,
            Token::LeftAngle,
            Token::RightAngle,
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Question,
            Token::Colon,
            Token::Comma,
            Token::Semicolon,
            Token::Tilde,
            Token::Dollar,
            Token::Eof,
        ];
        assert_eq!(parser.tokens, expected);
    }

    #[test]
    fn test_fat_arrow_tokenization() {
        let parser = MacroParser::new("=>");
        assert_eq!(parser.tokens[0], Token::FatArrow);
    }

    #[test]
    fn test_identifier_tokenization() {
        let parser = MacroParser::new("test_var");
        match &parser.tokens[0] {
            Token::Identifier(name) => assert_eq!(name, "test_var"),
            _ => panic!("Expected identifier token"),
        }
    }

    #[test]
    fn test_complex_pattern_parsing() {
        let mut parser = MacroPatternParser::new("$x:expr, $y:stmt");
        let elements = parser.parse().unwrap();
        assert_eq!(elements.len(), 3); // var, literal comma, var

        match &elements[0] {
            PatternElement::Variable {
                name,
                fragment_type,
            } => {
                assert_eq!(name, "x");
                assert!(matches!(fragment_type, FragmentType::Expr));
            }
            _ => panic!("Expected variable pattern"),
        }

        match &elements[1] {
            PatternElement::Literal(lit) => assert_eq!(lit, ","),
            _ => panic!("Expected literal pattern"),
        }

        match &elements[2] {
            PatternElement::Variable {
                name,
                fragment_type,
            } => {
                assert_eq!(name, "y");
                assert!(matches!(fragment_type, FragmentType::Stmt));
            }
            _ => panic!("Expected variable pattern"),
        }
    }

    #[test]
    fn test_optional_pattern_parsing() {
        let mut parser = MacroPatternParser::new("($x:expr)?");
        let elements = parser.parse().unwrap();
        assert_eq!(elements.len(), 1);

        match &elements[0] {
            PatternElement::Optional(_) => {}
            _ => panic!("Expected optional pattern"),
        }
    }

    #[test]
    fn test_star_repetition_parsing() {
        let mut parser = MacroPatternParser::new("($x:expr)*");
        let elements = parser.parse().unwrap();
        assert_eq!(elements.len(), 1);

        match &elements[0] {
            PatternElement::Repetition {
                min_count,
                max_count,
                ..
            } => {
                assert_eq!(*min_count, 0);
                assert_eq!(*max_count, None);
            }
            _ => panic!("Expected repetition pattern"),
        }
    }

    #[test]
    fn test_group_parsing() {
        let mut parser = MacroPatternParser::new("($x:expr, $y:stmt)");
        let elements = parser.parse().unwrap();
        assert_eq!(elements.len(), 1);

        match &elements[0] {
            PatternElement::Group(group) => {
                assert_eq!(group.len(), 3); // var, comma, var
            }
            _ => panic!("Expected group pattern"),
        }
    }

    #[test]
    fn test_fragment_type_parsing() {
        let fragment_types = vec![
            ("expr", FragmentType::Expr),
            ("stmt", FragmentType::Stmt),
            ("ident", FragmentType::Ident),
            ("literal", FragmentType::Literal),
            ("tt", FragmentType::TokenTree),
            ("item", FragmentType::Item),
            ("block", FragmentType::Block),
            ("unknown", FragmentType::TokenTree), // default
        ];

        for (type_str, _expected) in fragment_types {
            let mut parser = MacroPatternParser::new(&format!("$x:{}", type_str));
            let elements = parser.parse().unwrap();

            match &elements[0] {
                PatternElement::Variable { fragment_type, .. } => {
                    assert!(matches!(fragment_type, _expected));
                }
                _ => panic!("Expected variable pattern"),
            }
        }
    }

    #[test]
    fn test_macro_definition_parsing() {
        let mut parser = MacroParser::new("macro~ test ($x:expr) => $x end");
        let result = parser.parse_macro_definition().unwrap();
        assert_eq!(result.0, "test");
        assert_eq!(result.1.len(), 1);
    }

    #[test]
    fn test_multiple_pattern_macro() {
        let mut parser = MacroParser::new("macro~ test ($x:expr) => $x, () => nothing end");
        let result = parser.parse_macro_definition().unwrap();
        assert_eq!(result.0, "test");
        assert_eq!(result.1.len(), 2);
    }

    #[test]
    fn test_do_block_template() {
        let mut parser = MacroParser::new("macro~ test () => do let x = 42; x end end");
        let result = parser.parse_macro_definition().unwrap();
        assert_eq!(result.0, "test");
        assert!(result.1[0].1.contains("let x = 42"));
    }

    #[test]
    fn test_whitespace_handling() {
        let parser = MacroParser::new("   macro~   test   ");
        let non_whitespace_tokens: Vec<_> = parser
            .tokens
            .iter()
            .filter(|t| !matches!(t, Token::Whitespace | Token::Newline))
            .collect();

        assert_eq!(non_whitespace_tokens.len(), 4); // macro, ~, test, eof
    }

    #[test]
    fn test_newline_handling() {
        let parser = MacroParser::new("macro~\ntest\n");
        assert!(parser.tokens.contains(&Token::Newline));
    }

    #[test]
    fn test_error_handling_missing_macro_keyword() {
        let mut parser = MacroParser::new("test => body end");
        let result = parser.parse_macro_definition();
        assert!(result.is_err());
    }

    #[test]
    fn test_error_handling_missing_tilde() {
        let mut parser = MacroParser::new("macro test => body end");
        let result = parser.parse_macro_definition();
        assert!(result.is_err());
    }

    #[test]
    fn test_error_handling_missing_fat_arrow() {
        let mut parser = MacroParser::new("macro~ test pattern body end");
        let result = parser.parse_macro_definition();
        assert!(result.is_err());
    }

    #[test]
    fn test_error_handling_missing_end() {
        let mut parser = MacroParser::new("macro~ test pattern => body");
        let result = parser.parse_macro_definition();
        assert!(result.is_err());
    }
}
