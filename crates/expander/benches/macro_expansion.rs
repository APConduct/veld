use criterion::{black_box, criterion_group, criterion_main, Criterion};
use veld_common::ast::*;
use veld_common::source::NodeId;
use veld_expander::MacroSystem;

fn bench_vec_macro_empty(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();

    c.bench_function("vec_macro_empty", |b| {
        b.iter(|| {
            let result = macro_system
                .expand_macro_call("vec", &[], NodeId::new())
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_vec_macro_small(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();
    let args = vec![
        Expr::Literal(Literal::Integer(1)),
        Expr::Literal(Literal::Integer(2)),
        Expr::Literal(Literal::Integer(3)),
    ];

    c.bench_function("vec_macro_small", |b| {
        b.iter(|| {
            let result = macro_system
                .expand_macro_call("vec", black_box(&args), NodeId::new())
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_vec_macro_large(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();
    let args: Vec<Expr> = (0..100)
        .map(|i| Expr::Literal(Literal::Integer(i)))
        .collect();

    c.bench_function("vec_macro_large", |b| {
        b.iter(|| {
            let result = macro_system
                .expand_macro_call("vec", black_box(&args), NodeId::new())
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_format_macro(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();
    let args = vec![
        Expr::Literal(Literal::String("Hello {} {} {}!".to_string())),
        Expr::Literal(Literal::String("beautiful".to_string())),
        Expr::Literal(Literal::String("wonderful".to_string())),
        Expr::Literal(Literal::String("world".to_string())),
    ];

    c.bench_function("format_macro", |b| {
        b.iter(|| {
            let result = macro_system
                .expand_macro_call("format", black_box(&args), NodeId::new())
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_debug_macro(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();
    let args = vec![Expr::BinaryOp {
        left: Box::new(Expr::Identifier("x".to_string())),
        operator: BinaryOperator::Add,
        right: Box::new(Expr::Literal(Literal::Integer(42))),
    }];

    c.bench_function("debug_macro", |b| {
        b.iter(|| {
            let result = macro_system
                .expand_macro_call("debug", black_box(&args), NodeId::new())
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_assert_macro(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();
    let args = vec![
        Expr::BinaryOp {
            left: Box::new(Expr::Identifier("x".to_string())),
            operator: BinaryOperator::Greater,
            right: Box::new(Expr::Literal(Literal::Integer(0))),
        },
        Expr::Literal(Literal::String("x must be positive".to_string())),
    ];

    c.bench_function("assert_macro", |b| {
        b.iter(|| {
            let result = macro_system
                .expand_macro_call("assert", black_box(&args), NodeId::new())
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_nested_macro_expansion(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();

    // Create a nested expression: debug!(vec!(1, 2, 3))
    let inner_expr = Expr::MacroExpr {
        name: "vec".to_string(),
        arguments: vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
        ],
    };

    c.bench_function("nested_macro_expansion", |b| {
        b.iter(|| {
            let result = macro_system
                .preprocess_expr(black_box(inner_expr.clone()))
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_statement_preprocessing(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();

    let stmt = Statement::MacroInvocation {
        name: "vec".to_string(),
        arguments: vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
            Expr::Literal(Literal::Integer(4)),
            Expr::Literal(Literal::Integer(5)),
        ],
    };

    c.bench_function("statement_preprocessing", |b| {
        b.iter(|| {
            let result = macro_system
                .preprocess_statement(black_box(stmt.clone()))
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_macro_lookup(c: &mut Criterion) {
    let macro_system = MacroSystem::new();

    c.bench_function("macro_lookup_exists", |b| {
        b.iter(|| {
            let exists = macro_system.is_macro_defined(black_box("vec"));
            black_box(exists);
        })
    });

    c.bench_function("macro_lookup_nonexistent", |b| {
        b.iter(|| {
            let exists = macro_system.is_macro_defined(black_box("nonexistent_macro"));
            black_box(exists);
        })
    });
}

fn bench_complex_expression_preprocessing(c: &mut Criterion) {
    let mut macro_system = MacroSystem::new();

    // Complex expression with multiple nested macro calls
    let expr = Expr::BinaryOp {
        left: Box::new(Expr::MacroExpr {
            name: "vec".to_string(),
            arguments: vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2)),
            ],
        }),
        operator: BinaryOperator::Add,
        right: Box::new(Expr::MacroExpr {
            name: "vec".to_string(),
            arguments: vec![
                Expr::Literal(Literal::Integer(3)),
                Expr::Literal(Literal::Integer(4)),
            ],
        }),
    };

    c.bench_function("complex_expression_preprocessing", |b| {
        b.iter(|| {
            let result = macro_system
                .preprocess_expr(black_box(expr.clone()))
                .unwrap();
            black_box(result);
        })
    });
}

fn bench_macro_system_creation(c: &mut Criterion) {
    c.bench_function("macro_system_creation", |b| {
        b.iter(|| {
            let macro_system = MacroSystem::new();
            black_box(macro_system);
        })
    });
}

criterion_group!(
    macro_benches,
    bench_vec_macro_empty,
    bench_vec_macro_small,
    bench_vec_macro_large,
    bench_format_macro,
    bench_debug_macro,
    bench_assert_macro,
    bench_nested_macro_expansion,
    bench_statement_preprocessing,
    bench_macro_lookup,
    bench_complex_expression_preprocessing,
    bench_macro_system_creation
);

criterion_main!(macro_benches);
