use crate::types::{self, Type};

use super::source::SourceMap;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
};
use veld_error::VeldError;

#[derive(Debug)]
pub struct AST {
    pub statements: Vec<Statement>,
    pub source_map: Option<SourceMap>,
    pub errors: Vec<VeldError>,
}

impl AST {
    pub fn new(statements: Vec<Statement>) -> Self {
        AST {
            statements,
            source_map: Some(SourceMap::new()),
            errors: Vec::new(),
        }
    }

    #[allow(unused)]
    pub fn find_at_pos(&self, line: u32, col: u32) -> Option<&Statement> {
        todo!("Implement find_at_pos")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Eq, Hash)]
pub struct MacroPattern(pub String);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MacroTemplate {
    pub pattern: MacroPattern,
    pub expansion: MacroExpansion,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MacroExpansion(pub Vec<Statement>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Eq, Hash)]
pub enum VarKind {
    Let,
    Var,
    Const,
    LetMut,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Char(char),
    Float(f64),
    String(String),
    Boolean(bool),
    Unit, // unit value: ()
}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::Integer(i) => i.hash(state),
            Literal::Char(c) => c.hash(state),
            Literal::Float(ff) => ff.to_bits().hash(state),
            Literal::String(s) => s.hash(state),
            Literal::Boolean(b) => b.hash(state),
            Literal::Unit => ().hash(state),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::Float(ff) => write!(f, "{}", ff),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
        }
    }
}

impl Display for VarKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VarKind::Let => write!(f, "let"),
            VarKind::Var => write!(f, "var"),
            VarKind::Const => write!(f, "const"),
            VarKind::LetMut => write!(f, "let mut"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Argument {
    Positional(Expr),
    Named { name: String, value: Expr },
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Positional(expr) => write!(f, "{}", expr),
            Argument::Named { name, value } => write!(f, "{} = {}", name, value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeAnnotation {
    Basic(String),
    Unit,
    Function {
        params: Vec<TypeAnnotation>,
        return_type: Box<TypeAnnotation>,
    },
    Generic {
        base: String,
        type_args: Vec<TypeAnnotation>,
        // type_params: Vec<String>,
    },
    Constrained {
        base_type: Box<TypeAnnotation>,
        constraints: Vec<TypeAnnotation>,
    },
    Array(Box<TypeAnnotation>), // Array type, e.g., [i32]
    Tuple(Vec<TypeAnnotation>), // (e.g., (i32, f64, str))
    Enum {
        name: String,
        variants: std::collections::HashMap<String, EnumVariant>,
    }, // like | Typ1 of str | Typ2 of i32 | Typ3 | Typ4 of bool
    Record {
        fields: Vec<(String, TypeAnnotation)>, // Record type, e.g., { name: str, age: i32 }
    },
    Union {
        variants: Vec<TypeAnnotation>, // Union type, e.g., i32 | f64 | str
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhereClause {
    pub constraints: Vec<TypeConstraint>,
}

impl Display for WhereClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "where ")?;
        for (i, constraint) in self.constraints.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", constraint)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeConstraint {
    pub type_param: String,
    pub bounds: Vec<String>, // Kind names that the type parameter must implement
}

impl Display for TypeConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.type_param, self.bounds.join(" + "))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOperator {
    Negate, // For -x
    Not,    // For !x or not x
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            UnaryOperator::Negate => "-",
            UnaryOperator::Not => "!",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    BlockExpression {
        statements: Vec<Statement>,
        final_expr: Option<Box<Expr>>,
    },
    SelfReference,
    IfExpression {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
    BlockLambda {
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Vec<Statement>,
        return_type: Option<TypeAnnotation>,
        generic_params: Vec<GenericArgument>,
    },
    Literal(Literal),
    Identifier(String),
    UnitLiteral, // Unit Literal: ()
    BinaryOp {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expr>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Argument>,
    },
    Lambda {
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Box<Expr>,
        return_type: Option<TypeAnnotation>,
        generic_params: Vec<GenericArgument>,
    },
    /// General function/method call: callee can be any Expr (including PropertyAccess)
    Call {
        callee: Box<Expr>,
        arguments: Vec<Argument>,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        arguments: Vec<Argument>,
    },

    PropertyAccess {
        // Variant for property access without method call
        object: Box<Expr>,
        property: String,
    },
    StructCreate {
        struct_name: String,
        fields: Vec<(String, Expr)>,
    },
    Record {
        fields: Vec<(String, Expr)>,
    },
    ArrayLiteral(Vec<Expr>),
    IndexAccess {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: Vec<Expr>, // Fields for the variant if any
        type_args: Option<Vec<TypeAnnotation>>,
    },
    TupleLiteral(Vec<Expr>), // (1, 2.0, "hello")
    TupleAccess {
        tuple: Box<Expr>,
        index: usize,
    },
    MacroExpr {
        name: String,
        arguments: Vec<Expr>,
    },

    MacroVar(String), // For $identifier in macro expansions

    TypeCast {
        expr: Box<Expr>,
        target_type: TypeAnnotation,
    },
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
    },
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                write!(f, "if ")?;
                Display::fmt(condition, f)?;
                write!(f, " {{")?;
                Display::fmt(then_expr, f)?;
                if let Some(else_expr) = else_expr {
                    write!(f, " }} else {{")?;
                    Display::fmt(else_expr, f)?;
                }
                write!(f, " }}")
            }
            Expr::Match { value, arms } => {
                write!(f, "match ")?;
                Display::fmt(value, f)?;
                write!(f, " {{")?;
                for arm in arms {
                    Display::fmt(arm, f)?;
                }
                write!(f, " }}")
            }
            _ => write!(f, "unknown expression"),
        }
    }
}

impl Expr {
    pub fn is_record(&self) -> bool {
        matches!(self, Expr::Record { .. })
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self, Expr::TupleLiteral { .. })
    }

    pub fn is_tuple_access(&self) -> bool {
        matches!(self, Expr::TupleAccess { .. })
    }

    pub fn is_tuple_literal(&self) -> bool {
        matches!(self, Expr::TupleLiteral { .. })
    }

    pub fn is_lambda(&self) -> bool {
        matches!(self, Expr::Lambda { .. })
    }

    pub fn is_block_lambda(&self) -> bool {
        matches!(self, Expr::BlockLambda { .. })
    }

    pub fn is_enum_variant(&self) -> bool {
        matches!(self, Expr::EnumVariant { .. })
    }

    pub fn is_range(&self) -> bool {
        matches!(self, Expr::Range { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessEq,
    GreaterEq,
    Less,
    Greater,
    EqualEqual,
    NotEqual,
    And,
    Or,
    Exponent,
    Modulo,
    Pipe,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::LessEq => "<=",
            BinaryOperator::GreaterEq => ">=",
            BinaryOperator::Less => "<",
            BinaryOperator::Greater => ">",
            BinaryOperator::EqualEqual => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
            BinaryOperator::Exponent => "^",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Pipe => "|>",
        };
        write!(f, "{}", op)
    }
}

/// Represents a generic argument in a generic parameter list
/// Examples:
/// - In `Add<T>`, T is an unnamed generic argument
/// - In `Add<T, Output = U>`, T is unnamed and Output = U is named
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GenericArgument {
    /// Optional name for named arguments (e.g., "Output" in `Output = T`)
    /// None for positional/unnamed type arguments
    pub name: Option<String>,

    /// The type annotation for this generic argument
    pub type_annotation: TypeAnnotation,

    /// Optional constraints for this generic argument (e.g., T: Add + Mul)
    pub constraints: Vec<TypeAnnotation>,
}

impl GenericArgument {
    /// Create a new unnamed generic argument
    pub fn new(type_annotation: TypeAnnotation) -> Self {
        Self {
            name: None,
            type_annotation,
            constraints: vec![],
        }
    }

    /// Create a new named generic argument
    pub fn named(name: String, type_annotation: TypeAnnotation) -> Self {
        Self {
            name: Some(name),
            type_annotation,
            constraints: vec![],
        }
    }

    /// Create a new constrained generic argument
    pub fn with_constraints(
        type_annotation: TypeAnnotation,
        constraints: Vec<TypeAnnotation>,
    ) -> Self {
        Self {
            name: None,
            type_annotation,
            constraints,
        }
    }

    /// Create a new named constrained generic argument
    pub fn named_with_constraints(
        name: String,
        type_annotation: TypeAnnotation,
        constraints: Vec<TypeAnnotation>,
    ) -> Self {
        Self {
            name: Some(name),
            type_annotation,
            constraints,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructMethod {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumMethod {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Option<Vec<TypeAnnotation>>, // For variants with fields
    pub methods: HashMap<String, EnumMethod>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructField {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub is_public: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Hash)]
pub enum Pattern {
    Literal(Literal),
    Identifier(String),
    Wildcard, // _ (underscore)
    EnumPattern {
        enum_name: String,
        variant_name: String,
        fields: Vec<Pattern>, // Fields for the variant if any
    },
    TuplePattern(Vec<Pattern>), // (1, 2.0, "hello")
    StructPattern {
        struct_name: String,
        fields: Vec<(String, Pattern)>, // Field name and its pattern
    },
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Statement {
    BlockScope {
        body: Vec<Statement>,
    },
    TypeDeclaration {
        name: String,
        type_annotation: TypeAnnotation,
    },

    ExprStatement(Expr),
    FunctionDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>, // (name, type)
        return_type: TypeAnnotation,
        body: Vec<Statement>,
        is_proc: bool, // Mark as procedure (returns void/unit)
        is_public: bool,
        generic_params: Vec<GenericArgument>,
    },
    ProcDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>,
        body: Vec<Statement>,
        is_public: bool,
        generic_params: Vec<GenericArgument>,
    },
    VariableDeclaration {
        name: String,
        var_kind: VarKind,
        type_annotation: Option<TypeAnnotation>,
        value: Box<Expr>,
        is_public: bool, // Field to track visibility
    },
    If {
        condition: Expr,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    While {
        condition: Expr,
        body: Vec<Statement>,
    },
    For {
        iterator: String,
        iterable: Expr,
        body: Vec<Statement>,
    },
    Return(Option<Expr>),
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
        methods: Vec<StructMethod>,
        is_public: bool, // New field to track visibility
        generic_params: Vec<GenericArgument>,
    },
    KindDeclaration {
        name: String,
        methods: Vec<KindMethod>,
        is_public: bool, // New field to track visibility
        generic_params: Vec<GenericArgument>,
    },
    Implementation {
        type_name: String,
        kind_name: Option<String>,
        methods: Vec<MethodImpl>,
        generic_args: Vec<GenericArgument>,
        where_clause: Option<WhereClause>,
    },

    ModuleDeclaration {
        name: String,
        // Body is optional - might just be a declaration referencing another file
        body: Option<Vec<Statement>>,
        is_public: bool,
    },

    ImportDeclaration {
        path: Vec<String>, // Module path components (e.g., "math.vector")
        items: Vec<ImportItem>,
        alias: Option<String>, // For "import math as m"
        is_public: bool,       // to track visibility
    },
    CompoundAssignment {
        name: String,
        operator: BinaryOperator,
        value: Box<Expr>,
    },
    Assignment {
        name: String,
        value: Box<Expr>,
    },
    PropertyAssignment {
        target: Box<Expr>,
        operator: Option<BinaryOperator>, // None for regular assignment, Some for compound assignment
        value: Box<Expr>,
    },
    Break,
    Continue,
    EnumDeclaration {
        name: String,
        variants: Vec<EnumVariant>,
        is_public: bool,
        generic_params: Vec<GenericArgument>,
    },
    InherentImpl {
        type_name: String,
        generic_params: Vec<GenericArgument>,
        methods: Vec<MethodImpl>,
    },
    Match {
        value: Expr,
        arms: Vec<MatchArm>, // (pattern, guard, body)
    },
    MacroDeclaration {
        name: String,
        patterns: Vec<(MacroPattern, MacroExpansion)>,
        body: Option<Vec<Statement>>,
    },
    MacroInvocation {
        name: String,
        arguments: Vec<Expr>,
    },
    PlexDeclaration {
        name: String,
        type_annotation: TypeAnnotation,
        is_public: bool,
        generic_params: Vec<GenericArgument>,
    },
    UnionDeclaration {
        name: String,
        variants: Vec<Type>,
        is_public: bool,
        generic_params: Vec<GenericArgument>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pat: MatchPattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

impl std::fmt::Display for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.pat, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MatchPattern {
    Literal(Literal),
    Identifier(String),
    Struct {
        name: String,
        fields: Vec<(String, Option<Box<MatchPattern>>)>,
    },
    Enum {
        name: String,
        variant: String,
        fields: Vec<(String, Option<Box<MatchPattern>>)>,
    },
    Wildcard,
}

impl std::fmt::Display for MatchPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MatchPattern::Literal(lit) => write!(f, "{}", lit),
            MatchPattern::Identifier(id) => write!(f, "{}", id),
            MatchPattern::Struct { name, fields } => {
                write!(f, "{} {{", name)?;
                for (field, pat) in fields {
                    write!(f, " {}:", field)?;
                    if let Some(pat) = pat {
                        write!(f, " {}", pat)?;
                    }
                }
                write!(f, " }}")
            }
            MatchPattern::Enum {
                name,
                variant,
                fields,
            } => {
                write!(f, "{}::{} {{", name, variant)?;
                for (field, pat) in fields {
                    write!(f, " {}:", field)?;
                    if let Some(pat) = pat {
                        write!(f, " {}", pat)?;
                    }
                }
                write!(f, " }}")
            }
            MatchPattern::Wildcard => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ImportItem {
    All,           // import math.*
    Named(String), // import math.{sqrt}
    NamedWithAlias {
        // import math.{Vector as Vec}
        name: String,
        alias: String,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
    pub is_proc: bool,
    pub is_public: bool, // New field to track visibility
    pub generic_params: Vec<GenericArgument>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct KindMethod {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub default_impl: Option<Vec<Statement>>,
    pub is_public: bool, //   To track visibility
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MethodImpl {
    pub name: String,
    pub generic_params: Vec<GenericArgument>,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
    pub is_public: bool, // New field to track visibility
}
