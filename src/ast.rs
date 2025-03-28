#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    BinaryOp {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expr>,
    },
    // Add more variants as needed
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    // Add more operators
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExprStatement(Expr),
    FunctionDeclaration {
        name: String,
        params: Vec<(String, String)>, // (name, type)
        return_type: Option<String>,
        body: Vec<Statement>,
    },
    VariableDeclaration {
        name: String,
        type_annotation: Option<String>,
        value: Box<Expr>,
    },
    // Add more variants
}