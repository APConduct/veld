use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct MacroPattern(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct MacroExpansion(pub Vec<Statement>);

#[derive(Debug, Clone, PartialEq)]
pub enum VarKind {
    Let,
    Var,
    Const,
    LetMut,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Char(char),
    Float(f64),
    String(String),
    Boolean(bool),
    Unit, // unit value: ()
}

// #[derive(Debug, Clone)]
// pub enum Tuple {
//     Empty,
//     Single(Literal),
//     Pair(Literal, Literal),
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    Positional(Expr),
    Named { name: String, value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
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
    Array(Box<TypeAnnotation>), // Array type, e.g., [i32]
    Tuple(Vec<TypeAnnotation>), // (e.g., (i32, f64, str))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BlockExpression {
        statements: Vec<Statement>,
        final_expr: Option<Box<Expr>>,
    },
    IfExpression {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
    BlockLambda {
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Vec<Statement>,
        return_type: Option<TypeAnnotation>,
    },
    Literal(Literal),
    Identifier(String),
    UnitLiteral, // Unit Literal: ()
    BinaryOp {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Argument>,
    },
    Lambda {
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Box<Expr>,
        return_type: Option<TypeAnnotation>,
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
    ArrayLiteral(Vec<Expr>),
    IndexAccess {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: Vec<Expr>, // Fields for the variant if any
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

    TypeCast {
        expr: Box<Expr>,
        target_type: TypeAnnotation,
    },
}

#[derive(Debug, Clone, PartialEq)]
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
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
        };
        write!(f, "{}", op)
    }
}

/// Represents a generic argument in a generic parameter list
/// Examples:
/// - In `Add<T>`, T is an unnamed generic argument
/// - In `Add<T, Output = U>`, T is unnamed and Output = U is named
#[derive(Debug, Clone, PartialEq)]
pub struct GenericArgument {
    /// Optional name for named arguments (e.g., "Output" in `Output = T`)
    /// None for positional/unnamed type arguments
    pub name: Option<String>,

    /// The type annotation for this generic argument
    pub type_annotation: TypeAnnotation,
}

impl GenericArgument {
    /// Create a new unnamed generic argument
    pub fn new(type_annotation: TypeAnnotation) -> Self {
        Self {
            name: None,
            type_annotation,
        }
    }

    /// Create a new named generic argument
    pub fn named(name: String, type_annotation: TypeAnnotation) -> Self {
        Self {
            name: Some(name),
            type_annotation,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructMethod {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Option<Vec<TypeAnnotation>>, // For variants with fields
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    BlockScope {
        body: Vec<Statement>,
    },

    ExprStatement(Expr),
    FunctionDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>, // (name, type)
        return_type: TypeAnnotation,
        body: Vec<Statement>,
        is_proc: bool, // Mark as procedure (returns void/unit)
        is_public: bool,
    },
    ProcDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>,
        body: Vec<Statement>,
        is_public: bool,
    },
    VariableDeclaration {
        name: String,
        var_kind: VarKind,
        type_annotation: Option<TypeAnnotation>,
        value: Box<Expr>,
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
    Break,
    Continue,
    EnumDeclaration {
        name: String,
        variants: Vec<EnumVariant>,
        is_public: bool,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pat: MatchPattern,
    pub gaurd: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum ImportItem {
    All,           // import math.*
    Named(String), // import math.{sqrt}
    NamedWithAlias {
        // import math.{Vector as Vec}
        name: String,
        alias: String,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
    pub is_proc: bool,
    pub is_public: bool, // New field to track visibility
}

#[derive(Debug, Clone, PartialEq)]
pub struct KindMethod {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub default_impl: Option<Vec<Statement>>,
    pub is_public: bool, //   To track visibility
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodImpl {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
    pub is_public: bool, // New field to track visibility
}
