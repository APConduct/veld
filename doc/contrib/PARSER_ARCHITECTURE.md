# Veld Parser Architecture

## Table of Contents

- [Overview](#overview)
- [Phase 1: Lexical Analysis](#phase-1-lexical-analysis)
- [Phase 2: Syntax Analysis](#phase-2-syntax-analysis)
- [Parsing Hierarchy](#parsing-hierarchy)
- [Expression Parsing](#expression-parsing)
- [Type Parsing](#type-parsing)
- [Key Parser Techniques](#key-parser-techniques)
- [Special Parsing Features](#special-parsing-features)
- [Helper Methods](#helper-methods)
- [Complete Parsing Example](#complete-parsing-example)
- [Adding New Language Features](#adding-new-language-features)

---

## Overview

The Veld parser is a **recursive descent parser** that transforms source code into an Abstract Syntax Tree (AST). It operates in two distinct phases:

1. **Lexical Analysis** (Tokenization) - converts source text into tokens
2. **Syntax Analysis** (Parsing) - builds an AST from tokens

The parser is located in:
- **Lexer**: `crates/common/src/lexer.rs`
- **Parser**: `crates/common/src/parser.rs`

### Design Principles

- **Separation of Concerns**: Lexer handles character-level processing, parser handles structure
- **Recursive Descent**: Natural mapping from grammar rules to code
- **Predictive Parsing**: Uses lookahead to make parsing decisions without backtracking
- **Operator Precedence Climbing**: Elegant expression parsing
- **Error Recovery**: Guards against infinite loops and provides meaningful errors

---

## Phase 1: Lexical Analysis

### Architecture

The lexer uses the **Logos** library, a compile-time lexer generator that creates optimized lexers from declarative token patterns.

```rust
use logos::Logos;

#[derive(Debug, Logos, Clone)]
#[logos(extras = (usize, usize))]  // Tracks (line, column_offset)
pub enum Token {
    #[token("fn", word_callback)]
    Fn((usize, usize)),
    
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", identifier_callback)]
    Identifier((String, (usize, usize))),
    
    // ... more tokens
}
```

### Token Structure

Each token carries:
- **Type information** (keyword, operator, literal, etc.)
- **Position data** (`(line, column)` tuple)
- **Optional payload** (string content, numeric value, etc.)

### Preprocessing

Before tokenization, the lexer preprocesses source code to handle:

1. **Multi-line comments** with nesting: `#[[ comment ]]`
2. **Documentation comments**: `#|[[ doc comment ]]`
3. **Nested comment tracking**: Maintains nesting level counter

```rust
pub fn preprocess(input: &str) -> String {
    // Removes comments while preserving line structure
    // Supports nested multi-line comments
}
```

### Position Tracking

Callback functions compute accurate source positions:

```rust
fn word_callback(lex: &mut LLexer<Token>) -> (usize, usize) {
    let line = lex.extras.0;
    let column = lex.span().start - lex.extras.1;
    (line, column)
}

fn newline_callback(lex: &mut LLexer<Token>) -> Skip {
    lex.extras.0 += 1;  // Increment line
    lex.extras.1 = lex.span().end;  // Update column offset
    Skip
}
```

### Escape Sequence Processing

String and character literals support standard escape sequences:

- `\n` - newline
- `\t` - tab
- `\r` - carriage return
- `\\` - backslash
- `\"` - double quote
- `\'` - single quote
- `\0` - null character

### Lexer Usage

```rust
let source = "let x = 42";
let mut lexer = Lexer::new(source);
let tokens = lexer.collect_tokens()?;
```

---

## Phase 2: Syntax Analysis

### Parser Structure

```rust
pub struct Parser {
    tokens: Vec<Token>,      // Complete token stream
    current: usize,          // Current position in stream
    recursive_depth: usize,  // Prevents stack overflow
    total_steps: usize,      // Tracks complexity
}
```

### Core Methods

```rust
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self;
    pub fn parse(&mut self) -> Result<Vec<Statement>>;
    pub fn parse_with_context(&mut self, context: &mut ParseContext) -> Result<Vec<Statement>>;
}
```

### ParseContext

The parser can optionally track source spans for better error reporting:

```rust
pub struct ParseContext<'a> {
    current_file_id: FileId,
    source_map: &'a mut SourceMap,
}
```

This enables:
- Precise error messages with line/column information
- IDE features (go-to-definition, hover info)
- Source-level debugging

---

## Parsing Hierarchy

### Top-Level: Declarations

The entry point is `declaration()`, which dispatches based on the current token:

```
declaration()
├─ let/var/const      → variable_declaration()
├─ fn                 → function_declaration()
├─ proc               → proc_declaration()
├─ struct             → struct_declaration()
├─ enum               → enum_declaration()
├─ type               → type_declaration()
├─ kind               → kind_declaration()
├─ impl               → implementation_declaration()
├─ mod                → module_declaration()
├─ import             → import_declaration()
├─ macro              → macro_declaration()
├─ pub                → (handle visibility, then dispatch)
└─ (other)            → statement() (expression statement)
```

### Declaration Parsing Flow

```rust
fn declaration(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Statement> {
    // Pattern matching on current token
    if self.match_token(&[Token::Let(ZTUP)]) {
        if self.match_token(&[Token::Mut(ZTUP)]) {
            self.variable_declaration(VarKind::LetMut, false, ctx)
        } else {
            self.variable_declaration(VarKind::Let, false, ctx)
        }
    } else if self.match_token(&[Token::Fn(ZTUP)]) {
        self.function_declaration(ctx)
    } 
    // ... more cases
    else {
        self.statement(ctx)  // Fallback to expression statement
    }
}
```

### Statement Types

Statements include:
- Variable declarations
- Function/procedure declarations
- Control flow (if, while, for, match, return)
- Expression statements
- Assignment statements

---

## Expression Parsing

### Operator Precedence

Expressions use **precedence climbing** through a chain of methods. Each method handles operators at the same precedence level:

```
expression()        ← Entry point (handles records, lambdas)
    ↓
pipeline()          ← |> (lowest precedence)
    ↓
range()             ← .., ..=
    ↓
logical()           ← and, or
    ↓
comparison()        ← ==, !=, <, >, <=, >=
    ↓
term()              ← +, -
    ↓
factor()            ← *, /, %
    ↓
exponent()          ← ** (power)
    ↓
unary()             ← -, !
    ↓
postfix()           ← ., [], (), method calls (highest precedence)
    ↓
primary()           ← Literals, identifiers, grouping
```

### Expression Examples

#### Primary Expressions

```rust
// Literals
42                    → Expr::Literal(Literal::Integer(42))
3.14                  → Expr::Literal(Literal::Float(3.14))
"hello"               → Expr::Literal(Literal::String("hello"))
'c'                   → Expr::Literal(Literal::Char('c'))
true                  → Expr::Literal(Literal::Boolean(true))
()                    → Expr::UnitLiteral

// Identifiers
foo                   → Expr::Identifier("foo")

// Tuples
(1, 2, 3)             → Expr::TupleLiteral([1, 2, 3])

// Arrays
[1, 2, 3]             → Expr::Array([1, 2, 3])

// Records
{x: 10, y: 20}        → Expr::Record { fields: [("x", 10), ("y", 20)] }
```

#### Binary Operations

Left-associative operators are handled with a loop:

```rust
fn term(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Expr> {
    let mut expr = self.factor(ctx)?;
    
    while self.match_token(&[Token::Plus(ZTUP), Token::Minus(ZTUP)]) {
        let operator = match self.previous() {
            Token::Plus(_) => BinaryOperator::Add,
            Token::Minus(_) => BinaryOperator::Subtract,
            _ => unreachable!(),
        };
        let right = self.factor(ctx)?;
        expr = Expr::BinaryOp {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }
    
    Ok(expr)
}
```

#### Postfix Operations

Postfix operations include:
- Property access: `obj.field`
- Method calls: `obj.method(args)`
- Function calls: `func(args)`
- Array indexing: `arr[index]`
- Type casting: `expr as Type`
- Enum variant creation: `Color.Red(255, 0, 0)`

These are handled in `postfix_with_expr()` with a loop:

```rust
fn postfix_with_expr(&mut self, mut expr: Expr, ctx: &mut Option<&mut ParseContext>) 
    -> Result<Expr> 
{
    loop {
        if self.match_token(&[Token::Dot(ZTUP)]) {
            let property = self.consume_method_name("Expected property")?;
            
            if self.check(&Token::LParen(ZTUP)) {
                // Method call
                expr = self.parse_method_call(expr, property, ctx)?;
            } else {
                // Property access
                expr = Expr::PropertyAccess {
                    object: Box::new(expr),
                    property,
                };
            }
        } else if self.check(&Token::LParen(ZTUP)) && !self.is_new_line() {
            // Function call (only if on same line)
            expr = self.parse_function_call(expr, ctx)?;
        } else {
            break;
        }
    }
    Ok(expr)
}
```

### Lambda Expression Detection

Lambda parsing requires sophisticated lookahead to distinguish:

```
(x, y)              ← Tuple expression
func(x, y)          ← Function call
(x, y) => x + y     ← Lambda expression
```

The `check_lambda_start()` method performs multi-token lookahead:

```rust
fn check_lambda_start(&self) -> bool {
    // Case 1: fn<T>(x: Int) -> Int => expr
    if self.check(&Token::Fn((0, 0))) {
        // Look past generics, params, return type to find '=>'
    }
    
    // Case 2: x => expr (single parameter)
    if let Token::Identifier(_) = self.peek() {
        return self.tokens.get(self.current + 1) == Some(&Token::FatArrow(ZTUP));
    }
    
    // Case 3: (x, y) => expr (multiple parameters)
    if self.check(&Token::LParen(ZTUP)) {
        // Find matching ')' then check for '=>'
    }
    
    // Case 4: <T>(x) => expr (generic lambda)
    if self.check(&Token::Less(ZTUP)) {
        // Skip generic params, find params, check for '=>'
    }
    
    false
}
```

### Lambda Syntax Forms

Veld supports multiple lambda syntaxes:

```rust
// Explicit function syntax
fn(x: Int, y: Int) -> Int => x + y

// Inferred parameter types
fn(x, y) => x + y

// Single parameter (no parens)
x => x * 2

// No parameters
() => 42

// Block body
fn(x) => do
    let y = x * 2
    y + 1
end

// Generic lambda
<T>(x: T) => x
```

---

## Type Parsing

Types have their own grammar and are parsed by `parse_type()`:

### Type Syntax

```rust
// Basic types
Int                              → TypeAnnotation::Basic("Int")
String                           → TypeAnnotation::Basic("String")

// Generic types
List<Int>                        → TypeAnnotation::Generic { 
                                      base: "List", 
                                      type_args: [Basic("Int")] 
                                   }

// Tuple types
(Int, String)                    → TypeAnnotation::Tuple([Basic("Int"), Basic("String")])

// Function types
(Int, Int) -> Int                → TypeAnnotation::Function { 
                                      params: [Basic("Int"), Basic("Int")], 
                                      return_type: Basic("Int") 
                                   }

// Record types
{x: Int, y: Int}                 → TypeAnnotation::Record { 
                                      fields: [("x", Basic("Int")), ("y", Basic("Int"))] 
                                   }

// Array types
[Int]                            → TypeAnnotation::Array(Box::new(Basic("Int")))

// Union types
Int | String | Bool              → TypeAnnotation::Union { 
                                      variants: [Basic("Int"), Basic("String"), Basic("Bool")] 
                                   }

// Self type
Self                             → TypeAnnotation::Self_
```

### Type Parsing Algorithm

```rust
fn parse_type(&mut self) -> Result<TypeAnnotation> {
    let mut ty = {
        if self.match_token(&[Token::LParen(ZTUP)]) {
            // Tuple type or function type
            let types = self.parse_type_list()?;
            self.consume(&Token::RParen(ZTUP), "Expected ')'")?;
            
            if self.match_token(&[Token::Arrow(ZTUP)]) {
                // Function type: (T, U) -> R
                let return_type = Box::new(self.parse_type()?);
                TypeAnnotation::Function { params: types, return_type }
            } else {
                // Tuple type: (T, U)
                TypeAnnotation::Tuple(types)
            }
        } else if self.match_token(&[Token::LBrace(ZTUP)]) {
            // Record type: {field: Type, ...}
            self.parse_record_type()?
        } else if self.match_token(&[Token::LBracket(ZTUP)]) {
            // Array type: [T]
            let elem_type = self.parse_type()?;
            self.consume(&Token::RBracket(ZTUP), "Expected ']'")?;
            TypeAnnotation::Array(Box::new(elem_type))
        } else {
            // Basic or generic type
            self.parse_named_type()?
        }
    };
    
    // Handle union types: T | U | V
    if self.match_token(&[Token::PipeOr(ZTUP)]) {
        let mut variants = vec![ty];
        loop {
            variants.push(self.parse_type()?);
            if !self.match_token(&[Token::PipeOr(ZTUP)]) {
                break;
            }
        }
        ty = TypeAnnotation::Union { variants };
    }
    
    Ok(ty)
}
```

---

## Key Parser Techniques

### 1. Recursive Descent

Each syntactic construct has a dedicated method:

- Method checks current token (via `peek()`)
- Consumes expected tokens (via `consume()` or `match_token()`)
- Recursively calls other parsing methods
- Returns AST nodes

**Example**: Variable Declaration

```rust
fn variable_declaration(&mut self, var_kind: VarKind, is_public: bool, 
                       ctx: &mut Option<&mut ParseContext>) -> Result<Statement> 
{
    let pattern = self.parse_binding_pattern()?;
    
    let type_annotation = if self.match_token(&[Token::Colon(ZTUP)]) {
        Some(self.parse_type()?)
    } else {
        None
    };
    
    self.consume(&Token::Equals(ZTUP), "Expected '=' in variable declaration")?;
    
    let initializer = self.expression(ctx)?;
    
    Ok(Statement::VariableDeclaration {
        pattern,
        type_annotation,
        initializer: Some(initializer),
        var_kind,
        is_public,
    })
}
```

### 2. Lookahead Without Backtracking

The parser uses **LL(k)** lookahead but avoids expensive backtracking:

```rust
// Single token lookahead
fn peek(&self) -> &Token {
    if self.is_at_end() {
        &Token::EOF
    } else {
        &self.tokens[self.current]
    }
}

// Multi-token lookahead (manual indexing)
fn peek_next_token_is(&self, token: &Token) -> bool {
    if self.current + 1 < self.tokens.len() {
        &self.tokens[self.current + 1] == token
    } else {
        false
    }
}

// Complex lookahead for lambda detection
fn check_lambda_start(&self) -> bool {
    // Uses self.tokens[index] to look ahead multiple tokens
    // without consuming them
}
```

### 3. Operator Precedence Climbing

Methods form a precedence chain:

```rust
// Lower precedence (calls higher precedence)
fn logical(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Expr> {
    let mut expr = self.comparison(ctx)?;  // Higher precedence
    
    while self.match_token(&[Token::And(ZTUP), Token::Or(ZTUP)]) {
        let operator = /* determine operator */;
        let right = self.comparison(ctx)?;
        expr = Expr::BinaryOp { 
            left: Box::new(expr), 
            operator, 
            right: Box::new(right) 
        };
    }
    
    Ok(expr)
}
```

This naturally handles:
- **Left associativity**: Loop continues while operator matches
- **Correct precedence**: Higher precedence methods called first
- **No ambiguity**: Each operator at exactly one level

### 4. Context Propagation

Optional `ParseContext` is threaded through the call stack:

```rust
fn expression(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Expr> {
    let start = self.get_current_position();
    
    // ... parse expression ...
    
    let end = self.get_current_position();
    
    if let Some(ctx) = ctx {
        ctx.add_span(NodeId::new(), start, end);
    }
    
    Ok(expr)
}
```

This enables:
- Source span tracking
- Error reporting with exact locations
- IDE integration (hover, go-to-def)

### 5. Error Recovery

The parser includes guards against infinite loops:

```rust
fn parse_statements(&mut self, context: &mut ParseContext) -> Result<Vec<Statement>> {
    let mut statements = Vec::new();
    let mut step_count = 0;
    const MAX_STEPS: usize = 1000;
    
    while !self.is_at_end() {
        step_count += 1;
        if step_count > MAX_STEPS {
            return Err(VeldError::ParserError(
                format!("Parser exceeded maximum step count at token position: {}", 
                        self.current)
            ));
        }
        
        let prev_position = self.current;
        
        match self.declaration_with_context(context) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => {
                // Try fallback parsing
                if !self.is_at_end() {
                    match self.statement(&mut Some(context)) {
                        Ok(stmt) => statements.push(stmt),
                        Err(_) => self.advance(),  // Skip problematic token
                    }
                }
            }
        }
        
        // Force advance if we didn't consume anything
        if self.current == prev_position && !self.is_at_end() {
            self.advance();
        }
    }
    
    Ok(statements)
}
```

### 6. Recursion Depth Limiting

Prevents stack overflow on deeply nested expressions:

```rust
fn expression(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Expr> {
    self.recursive_depth += 1;
    if self.recursive_depth > 100 {
        self.recursive_depth -= 1;
        return Err(VeldError::ParserError(
            "Parser recursion limit exceeded".to_string()
        ));
    }
    
    let result = self.pipeline(ctx);
    
    self.recursive_depth -= 1;
    result
}
```

---

## Special Parsing Features

### 1. Newline Sensitivity

Some constructs are newline-sensitive to support implicit statement separation:

```rust
fn is_new_line(&self) -> bool {
    if self.current == 0 || self.current >= self.tokens.len() {
        return false;
    }
    let prev_line = self.tokens[self.current - 1].source_pos().line;
    let curr_line = self.tokens[self.current].source_pos().line;
    curr_line > prev_line
}

// Used in postfix parsing
if self.check(&Token::LParen(ZTUP)) && !self.is_new_line() {
    // Only treat as function call if on same line
    self.parse_function_call(expr, ctx)?
}
```

### 2. Macro Pattern Matching

Macros can have multiple pattern-expansion pairs:

```
macro ~name
    (pattern1) => expansion1,
    (pattern2) => expansion2
end
```

Parser collects patterns and expansions:

```rust
fn macro_declaration(&mut self, ctx: &mut Option<&mut ParseContext>) 
    -> Result<Statement> 
{
    let name = self.consume_identifier("Expected macro name")?;
    
    let mut patterns = Vec::new();
    
    while !self.check(&Token::End(ZTUP)) && !self.is_at_end() {
        let pattern = self.parse_macro_pattern()?;
        self.consume(&Token::FatArrow(ZTUP), "Expected '=>'")?;
        let expansion = self.parse_macro_expansion(ctx)?;
        
        patterns.push((pattern, expansion));
    }
    
    self.consume(&Token::End(ZTUP), "Expected 'end'")?;
    
    Ok(Statement::MacroDeclaration { name, patterns, body: None })
}
```

### 3. Named Arguments vs. Struct Instantiation

Ambiguity resolution:

```rust
// All named arguments → struct instantiation
Point(x: 10, y: 20)   

// Positional arguments → function call
func(10, 20)          

// Mixed → function call with named args
func(10, y: 20)       
```

Implementation:

```rust
fn parse_function_call(&mut self, callee: Expr, ctx: &mut Option<&mut ParseContext>) 
    -> Result<Expr> 
{
    let mut args = Vec::new();
    
    if !self.check(&Token::RParen(ZTUP)) {
        if self.check_named_arguments() {
            args = self.parse_named_arguments(ctx)?;
        } else {
            // Parse positional arguments
        }
    }
    
    // If ALL arguments are named, might be struct instantiation
    let all_named = !args.is_empty() && 
                    args.iter().all(|arg| matches!(arg, Argument::Named { .. }));
    
    if all_named && matches!(callee, Expr::Identifier(_)) {
        // Convert to struct instantiation
        let struct_name = /* extract from callee */;
        Ok(Expr::StructCreate { struct_name, fields: convert_args_to_fields(args) })
    } else {
        Ok(Expr::Call { callee: Box::new(callee), arguments: args })
    }
}
```

### 4. Pipeline Operator

The `|>` operator threads values through functions:

```rust
value |> func1 |> func2
// Equivalent to: func2(func1(value))
```

Implemented as left-associative binary operator:

```rust
fn pipeline(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Expr> {
    let mut expr = self.range(ctx)?;
    
    while self.match_token(&[Token::Pipe(ZTUP)]) {
        let right = self.range(ctx)?;
        expr = Expr::BinaryOp {
            left: Box::new(expr),
            operator: BinaryOperator::Pipeline,
            right: Box::new(right),
        };
    }
    
    Ok(expr)
}
```

### 5. Do-End Blocks

Veld supports expression blocks:

```rust
let result = do
    let x = 10
    let y = 20
    x + y  // final expression is the value
end
```

Parsing:

```rust
fn parse_block_expression(&mut self, ctx: &mut Option<&mut ParseContext>) 
    -> Result<Expr> 
{
    let mut statements = Vec::new();
    
    while !self.check(&Token::End(ZTUP)) && !self.is_at_end() {
        // Check if this might be the final expression
        if self.is_likely_final_expression() {
            let expr = self.expression(ctx)?;
            if self.check(&Token::End(ZTUP)) {
                // This is the final expression
                return Ok(Expr::Block {
                    statements,
                    final_expr: Some(Box::new(expr)),
                });
            } else {
                statements.push(Statement::ExprStatement(expr));
            }
        } else {
            statements.push(self.declaration(ctx)?);
        }
    }
    
    self.consume(&Token::End(ZTUP), "Expected 'end'")?;
    
    Ok(Expr::Block { statements, final_expr: None })
}
```

---

## Helper Methods

### Token Manipulation

```rust
// Look at current token without consuming
fn peek(&self) -> &Token {
    if self.is_at_end() {
        &Token::EOF
    } else {
        &self.tokens[self.current]
    }
}

// Consume and return current token
fn advance(&mut self) -> Token {
    if !self.is_at_end() {
        self.current += 1;
    }
    self.previous()
}

// Get last consumed token
fn previous(&self) -> Token {
    if self.current == 0 {
        Token::EOF
    } else {
        self.tokens.get(self.current - 1).cloned().unwrap_or(Token::EOF)
    }
}

// Check if at end of token stream
fn is_at_end(&self) -> bool {
    self.current >= self.tokens.len()
}
```

### Token Checking

```rust
// Check if current token matches without consuming
fn check(&self, token: &Token) -> bool {
    if self.is_at_end() {
        false
    } else {
        &self.tokens[self.current] == token
    }
}

// Try to match and consume one of several tokens
fn match_token(&mut self, tokens: &[Token]) -> bool {
    for token in tokens {
        if self.check(token) {
            self.advance();
            return true;
        }
    }
    false
}
```

### Token Consumption

```rust
// Consume expected token or error
fn consume(&mut self, token: &Token, message: &str) -> Result<Token> {
    if self.check(token) {
        Ok(self.advance())
    } else {
        Err(VeldError::ParserError(message.to_string()))
    }
}

// Consume identifier or error
fn consume_identifier(&mut self, message: &str) -> Result<String> {
    match self.advance() {
        Token::Identifier(s) => Ok(s.0),
        _ => Err(VeldError::ParserError(message.to_string())),
    }
}

// Consume parameter name (handles 'mut' prefix)
fn consume_parameter_name(&mut self, message: &str) -> Result<String> {
    if self.match_token(&[Token::Mut(ZTUP)]) {
        match self.advance() {
            Token::Identifier(s) => Ok(format!("mut {}", s.0)),
            Token::SelfToken(_) => Ok("mut self".to_string()),
            _ => Err(VeldError::ParserError("Expected parameter name".to_string())),
        }
    } else {
        match self.advance() {
            Token::Identifier(s) => Ok(s.0),
            Token::SelfToken(_) => Ok("self".to_string()),
            _ => Err(VeldError::ParserError(message.to_string())),
        }
    }
}
```

---

## Complete Parsing Example

Let's trace the parsing of: `let x = 10 + 20 * 3`

### Token Stream

```
[Let(0,0), Identifier("x", (0,4)), Equals(0,6), IntegerLiteral(10, (0,8)), 
 Plus(0,11), IntegerLiteral(20, (0,13)), Star(0,16), IntegerLiteral(3, (0,18))]
```

### Parsing Steps

```
1. parse() calls declaration()

2. declaration() sees Token::Let
   └─ Calls variable_declaration(VarKind::Let, false, ctx)

3. variable_declaration()
   ├─ consume_identifier("x") → pattern = "x"
   ├─ match Token::Colon → false (no type annotation)
   ├─ consume Token::Equals → success
   └─ Calls expression(ctx)

4. expression()
   └─ Calls pipeline(ctx)

5. pipeline()
   └─ Calls range(ctx)

6. range()
   └─ Calls logical(ctx)

7. logical()
   └─ Calls comparison(ctx)

8. comparison()
   └─ Calls term(ctx)

9. term()
   ├─ Calls factor(ctx)
   │  ├─ Calls unary(ctx)
   │  │  └─ Calls postfix(ctx)
   │  │     └─ Calls primary(ctx)
   │  │        └─ Returns Expr::Literal(Integer(10))
   │  └─ Returns Expr::Literal(Integer(10))
   │
   ├─ Sees Token::Plus → matched!
   ├─ operator = BinaryOperator::Add
   ├─ Calls factor(ctx) for right side
   │  ├─ Calls unary(ctx)
   │  │  └─ Calls postfix(ctx)
   │  │     └─ Calls primary(ctx)
   │  │        └─ Returns Expr::Literal(Integer(20))
   │  │
   │  ├─ Sees Token::Star → matched!
   │  ├─ operator = BinaryOperator::Multiply
   │  ├─ Calls exponent(ctx) for right side
   │  │  └─ Returns Expr::Literal(Integer(3))
   │  │
   │  └─ Returns Expr::BinaryOp {
   │       left: Literal(Integer(20)),
   │       operator: Multiply,
   │       right: Literal(Integer(3))
   │     }
   │
   └─ Returns Expr::BinaryOp {
        left: Literal(Integer(10)),
        operator: Add,
        right: BinaryOp {
          left: Literal(Integer(20)),
          operator: Multiply,
          right: Literal(Integer(3))
        }
      }

10. Back to variable_declaration()
    └─ Creates Statement::VariableDeclaration {
         pattern: "x",
         type_annotation: None,
         initializer: Some(BinaryOp { ... }),
         var_kind: Let,
         is_public: false
       }
```

### Resulting AST

```rust
Statement::VariableDeclaration {
    pattern: Pattern::Identifier("x"),
    type_annotation: None,
    initializer: Some(
        Expr::BinaryOp {
            left: Box::new(Expr::Literal(Literal::Integer(10))),
            operator: BinaryOperator::Add,
            right: Box::new(
                Expr::BinaryOp {
                    left: Box::new(Expr::Literal(Literal::Integer(20))),
                    operator: BinaryOperator::Multiply,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                }
            ),
        }
    ),
    var_kind: VarKind::Let,
    is_public: false,
}
```

**Note**: Multiplication has higher precedence than addition, so `20 * 3` becomes a subtree before being added to `10`.

---

## Adding New Language Features

### Step-by-Step Guide

#### 1. Add Token(s) to Lexer

Edit `crates/common/src/lexer.rs`:

```rust
#[derive(Debug, Logos, Clone)]
pub enum Token {
    // ... existing tokens ...
    
    #[token("mynewkeyword", word_callback)]
    MyNewKeyword((usize, usize)),
}
```

#### 2. Update Display Implementation

Add display case for your token:

```rust
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // ... existing cases ...
            Token::MyNewKeyword(_) => write!(f, "mynewkeyword"),
        }
    }
}
```

#### 3. Update PartialEq Implementation

Add comparison case:

```rust
impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match (self, other) {
            // ... existing cases ...
            (Token::MyNewKeyword(_), Token::MyNewKeyword(_)) => true,
            _ => false,
        }
    }
}
```

#### 4. Add AST Node(s)

Edit `crates/common/src/ast.rs`:

```rust
pub enum Statement {
    // ... existing variants ...
    MyNewStatement {
        field1: Type1,
        field2: Type2,
    },
}

// Or for expressions:
pub enum Expr {
    // ... existing variants ...
    MyNewExpr {
        field1: Type1,
        field2: Type2,
    },
}
```

#### 5. Add Parsing Method

Edit `crates/common/src/parser.rs`:

```rust
impl Parser {
    fn parse_my_new_feature(&mut self, ctx: &mut Option<&mut ParseContext>) 
        -> Result<Statement> 
    {
        let start = self.get_current_position();
        
        // Consume tokens and build AST
        self.consume(&Token::MyNewKeyword(ZTUP), "Expected 'mynewkeyword'")?;
        
        let field1 = /* parse field1 */;
        let field2 = /* parse field2 */;
        
        let end = self.get_current_position();
        if let Some(ctx) = ctx {
            ctx.add_span(NodeId::new(), start, end);
        }
        
        Ok(Statement::MyNewStatement { field1, field2 })
    }
}
```

#### 6. Integrate into Declaration/Statement Parsing

Add case to `declaration()` or `statement()`:

```rust
fn declaration(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Statement> {
    // ... existing cases ...
    
    if self.match_token(&[Token::MyNewKeyword(ZTUP)]) {
        return self.parse_my_new_feature(ctx);
    }
    
    // ... rest of cases ...
}
```

#### 7. Add Tests

Add parser tests in `crates/common/src/parser.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_my_new_feature() {
        let source = "mynewkeyword field1 field2";
        let tokens = Lexer::new(source).collect_tokens().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        
        assert!(result.is_ok());
        let statements = result.unwrap();
        assert_eq!(statements.len(), 1);
        
        match &statements[0] {
            Statement::MyNewStatement { field1, field2 } => {
                // Assert expected values
            }
            _ => panic!("Expected MyNewStatement"),
        }
    }
}
```

### Example: Adding a `loop` Statement

```rust
// 1. Add token
#[token("loop", word_callback)]
Loop((usize, usize)),

// 2. Add AST node
pub enum Statement {
    // ...
    Loop {
        body: Vec<Statement>,
    },
}

// 3. Add parsing method
fn parse_loop(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Statement> {
    self.consume(&Token::Loop(ZTUP), "Expected 'loop'")?;
    
    let mut body = Vec::new();
    while !self.check(&Token::End(ZTUP)) && !self.is_at_end() {
        body.push(self.statement(ctx)?);
    }
    
    self.consume(&Token::End(ZTUP), "Expected 'end' after loop body")?;
    
    Ok(Statement::Loop { body })
}

// 4. Integrate
fn statement(&mut self, ctx: &mut Option<&mut ParseContext>) -> Result<Statement> {
    if self.match_token(&[Token::Loop(ZTUP)]) {
        return self.parse_loop(ctx);
    }
    // ... rest of cases ...
}

// 5. Test
#[test]
fn test_loop_statement() {
    let source = "loop\n  x = x + 1\n  if x > 10 then break end\nend";
    let tokens = Lexer::new(source).collect_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let result = parser.parse().unwrap();
    
    assert_eq!(result.len(), 1);
    assert!(matches!(result[0], Statement::Loop { .. }));
}
```

---

## Best Practices

### 1. Always Track Source Positions

```rust
let start = self.get_current_position();
// ... parse construct ...
let end = self.get_current_position();

if let Some(ctx) = ctx {
    ctx.add_span(NodeId::new(), start, end);
}
```

### 2. Use Descriptive Error Messages

```rust
self.consume(&Token::Equals(ZTUP), "Expected '=' after variable name in declaration")?;
```

Not:
```rust
self.consume(&Token::Equals(ZTUP), "Expected '='")?;
```

### 3. Handle Recursive Depth

```rust
self.recursive_depth += 1;
if self.recursive_depth > 100 {
    self.recursive_depth -= 1;
    return Err(VeldError::ParserError("Recursion limit exceeded".to_string()));
}

let result = /* parse */;

self.recursive_depth -= 1;
result
```

### 4. Add Comprehensive Tests

Test edge cases:
- Empty constructs
- Nested constructs
- Error conditions
- Boundary conditions

### 5. Document Grammar Rules

Add comments describing the grammar:

```rust
/// Parses a function declaration.
///
/// Grammar:
///   fn <name> ( <params> ) [ -> <return_type> ] [ where <constraints> ]
///     <body>
///   end
fn function_declaration(&mut self, ctx: &mut Option<&mut ParseContext>) 
    -> Result<Statement>
{
    // ...
}
```

---

## Debugging Tips

### 1. Add Tracing

The parser uses `tracing` for debug output:

```rust
fn my_parsing_method(&mut self) -> Result<Statement> {
    tracing::debug!("Parsing my feature, current token: {:?}", self.peek());
    
    // ... parse ...
    
    tracing::debug!("Successfully parsed my feature");
    Ok(result)
}
```

Run with: `RUST_LOG=debug cargo run`

### 2. Inspect Token Stream

```rust
println!("Tokens: {:?}", self.tokens);
println!("Current index: {}", self.current);
println!("Current token: {:?}", self.peek());
```

### 3. Check Precedence

If expression parsing seems wrong, verify the precedence chain and ensure operators are at the correct level.

### 4. Verify Lookahead Logic

For complex lookahead (like lambda detection), add debug output at each step to verify the lookahead logic is working correctly.

---

## Summary

The Veld parser is a well-structured recursive descent parser that:

- **Separates concerns** between lexical and syntactic analysis
- **Uses predictive parsing** with lookahead to avoid backtracking
- **Employs precedence climbing** for clean expression parsing
- **Tracks source positions** for excellent error reporting
- **Includes safety guards** against infinite loops and stack overflow
- **Is highly extensible** with clear patterns for adding features

The architecture makes it straightforward to understand, modify, and extend the language grammar while maintaining correctness and performance.