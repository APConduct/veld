#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
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

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
pub enum Expr {
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
}

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExprStatement(Expr),
    FunctionDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>, // (name, type)
        return_type: TypeAnnotation,
        body: Vec<Statement>,
        is_proc: bool, // Mark as procedure (returns void/unit)
    },
    ProcDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>,
        // return_type: TypeAnnotation,
        body: Vec<Statement>,
    },
    VariableDeclaration {
        name: String,
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
        fields: Vec<(String, TypeAnnotation)>,
        methods: Vec<StructMethod>,
    },
    KindDeclaration {
        name: String,
        methods: Vec<KindMethod>,
    },
    Implementation {
        type_name: String,
        kind_name: Option<String>,
        methods: Vec<MethodImpl>,
    },

    ModuleDeclaration {
        name: String,
        // Body is optional - might just be a declaration referencing another file
        body: Option<Vec<Statement>>,
    },

    ImportDeclaration {
        path: Vec<String>, // Module path components (e.g., "math.vector")
        items: Vec<ImportItem>,
        alias: Option<String>, // For "import math as m"
    },
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct KindMethod {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub default_impl: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct MethodImpl {
    pub name: String,
    pub params: Vec<(String, TypeAnnotation)>,
    pub return_type: TypeAnnotation,
    pub body: Vec<Statement>,
}
