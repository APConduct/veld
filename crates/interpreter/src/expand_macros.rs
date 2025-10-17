use veld_common::ast::{AST, Expr, Statement};
use veld_expander::{ExpansionError, MacroSystem};

/// Recursively expand macros in the entire AST.
/// Returns a new AST with all macros expanded, or an error.
pub fn expand_macros_in_ast(
    ast: AST,
    macro_system: &mut MacroSystem,
) -> Result<AST, ExpansionError> {
    let mut expanded_statements = Vec::new();
    for stmt in ast.statements {
        let stmts = expand_macros_in_statement(stmt, macro_system)?;
        expanded_statements.extend(stmts);
    }

    Ok(AST {
        statements: expanded_statements,
        source_map: ast.source_map,
        errors: ast.errors,
    })
}

/// Recursively expand macros in a statement.
/// Returns a vector because a macro can expand to multiple statements.
fn expand_macros_in_statement(
    stmt: Statement,
    macro_system: &mut MacroSystem,
) -> Result<Vec<Statement>, ExpansionError> {
    // First, expand macros at this statement level
    let expanded = macro_system.preprocess_statement(stmt)?;
    // Then, recursively expand macros in any child statements/expressions
    let mut result = Vec::new();
    for s in expanded {
        result.extend(expand_macros_in_statement_children(s, macro_system)?);
    }
    Ok(result)
}

/// Helper to recursively expand macros in child nodes of a statement
fn expand_macros_in_statement_children(
    stmt: Statement,
    macro_system: &mut MacroSystem,
) -> Result<Vec<Statement>, ExpansionError> {
    use veld_common::ast::Statement::*;
    match stmt {
        BlockScope { body } => {
            let mut new_body = Vec::new();
            for s in body {
                let stmts = expand_macros_in_statement(s, macro_system)?;
                new_body.extend(stmts);
            }
            Ok(vec![BlockScope { body: new_body }])
        }
        ExprStatement(expr) => {
            let expr = expand_macros_in_expr(expr, macro_system)?;
            Ok(vec![ExprStatement(expr)])
        }
        VariableDeclaration {
            name,
            var_kind,
            type_annotation,
            value,
            is_public,
        } => {
            let value = Box::new(expand_macros_in_expr(*value, macro_system)?);
            Ok(vec![VariableDeclaration {
                name,
                var_kind,
                type_annotation,
                value,
                is_public,
            }])
        }
        If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = expand_macros_in_expr(condition, macro_system)?;
            let then_branch = then_branch
                .into_iter()
                .map(|s| expand_macros_in_statement(s, macro_system))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
            let else_branch = match else_branch {
                Some(else_stmts) => Some(
                    else_stmts
                        .into_iter()
                        .map(|s| expand_macros_in_statement(s, macro_system))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect(),
                ),
                None => None,
            };
            Ok(vec![If {
                condition,
                then_branch,
                else_branch,
            }])
        }
        While { condition, body } => {
            let condition = expand_macros_in_expr(condition, macro_system)?;
            let body = body
                .into_iter()
                .map(|s| expand_macros_in_statement(s, macro_system))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
            Ok(vec![While { condition, body }])
        }
        For {
            iterator,
            iterable,
            body,
        } => {
            let iterable = expand_macros_in_expr(iterable, macro_system)?;
            let body = body
                .into_iter()
                .map(|s| expand_macros_in_statement(s, macro_system))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
            Ok(vec![For {
                iterator,
                iterable,
                body,
            }])
        }
        Return(opt_expr) => {
            let opt_expr = match opt_expr {
                Some(expr) => Some(expand_macros_in_expr(expr, macro_system)?),
                None => None,
            };
            Ok(vec![Return(opt_expr)])
        }
        Assignment { name, value } => {
            let value = Box::new(expand_macros_in_expr(*value, macro_system)?);
            Ok(vec![Assignment { name, value }])
        }
        PropertyAssignment {
            target,
            operator,
            value,
        } => {
            let target = Box::new(expand_macros_in_expr(*target, macro_system)?);
            let value = Box::new(expand_macros_in_expr(*value, macro_system)?);
            Ok(vec![PropertyAssignment {
                target,
                operator,
                value,
            }])
        }
        CompoundAssignment {
            name,
            operator,
            value,
        } => {
            let value = Box::new(expand_macros_in_expr(*value, macro_system)?);
            Ok(vec![CompoundAssignment {
                name,
                operator,
                value,
            }])
        }
        FunctionDeclaration {
            name,
            params,
            return_type,
            body,
            is_proc,
            is_public,
            generic_params,
        } => {
            let body = body
                .into_iter()
                .map(|s| expand_macros_in_statement(s, macro_system))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
            Ok(vec![FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                is_proc,
                is_public,
                generic_params,
            }])
        }
        ProcDeclaration {
            name,
            params,
            body,
            is_public,
            generic_params,
        } => {
            let body = body
                .into_iter()
                .map(|s| expand_macros_in_statement(s, macro_system))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
            Ok(vec![ProcDeclaration {
                name,
                params,
                body,
                is_public,
                generic_params,
            }])
        }
        StructDeclaration {
            name,
            fields,
            methods,
            is_public,
            generic_params,
        } => {
            let methods = methods
                .into_iter()
                .map(|mut m| {
                    m.body = m
                        .body
                        .into_iter()
                        .map(|s| expand_macros_in_statement(s, macro_system))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect();
                    Ok(m)
                })
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(vec![StructDeclaration {
                name,
                fields,
                methods,
                is_public,
                generic_params,
            }])
        }
        EnumDeclaration {
            name,
            variants,
            is_public,
            generic_params,
        } => Ok(vec![EnumDeclaration {
            name,
            variants,
            is_public,
            generic_params,
        }]),
        Implementation {
            type_name,
            kind_name,
            methods,
            generic_args,
            where_clause: _,
        } => {
            let methods = methods
                .into_iter()
                .map(|mut m| {
                    m.body = m
                        .body
                        .into_iter()
                        .map(|s| expand_macros_in_statement(s, macro_system))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect();
                    Ok(m)
                })
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(vec![Implementation {
                type_name,
                kind_name,
                methods,
                generic_args,
                where_clause: None,
            }])
        }
        InherentImpl {
            type_name,
            generic_params,
            methods,
        } => {
            let methods = methods
                .into_iter()
                .map(|mut m| {
                    m.body = m
                        .body
                        .into_iter()
                        .map(|s| expand_macros_in_statement(s, macro_system))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect();
                    Ok(m)
                })
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(vec![InherentImpl {
                type_name,
                generic_params,
                methods,
            }])
        }
        Match { value, arms } => {
            let value = expand_macros_in_expr(value, macro_system)?;
            let arms = arms
                .into_iter()
                .map(|mut arm| {
                    arm.body = expand_macros_in_expr(arm.body, macro_system)?;
                    if let Some(guard) = arm.guard {
                        arm.guard = Some(expand_macros_in_expr(guard, macro_system)?);
                    }
                    Ok(arm)
                })
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(vec![Match { value, arms }])
        }
        MacroDeclaration {
            name,
            patterns,
            body,
        } => {
            // Register the macro with the macro system
            // For now, we'll just pass through the declaration
            // In the future, we might want to register user-defined macros
            Ok(vec![MacroDeclaration {
                name,
                patterns,
                body,
            }])
        }
        ModuleDeclaration {
            name,
            body,
            is_public,
        } => {
            let body = match body {
                Some(stmts) => Some(
                    stmts
                        .into_iter()
                        .map(|s| expand_macros_in_statement(s, macro_system))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect(),
                ),
                None => None,
            };
            Ok(vec![ModuleDeclaration {
                name,
                body,
                is_public,
            }])
        }
        KindDeclaration {
            name,
            methods,
            is_public,
            generic_params,
        } => Ok(vec![KindDeclaration {
            name,
            methods,
            is_public,
            generic_params,
        }]),
        ImportDeclaration {
            path,
            items,
            alias,
            is_public,
        } => Ok(vec![ImportDeclaration {
            path,
            items,
            alias,
            is_public,
        }]),
        PlexDeclaration {
            name,
            type_annotation,
            is_public,
            generic_params,
        } => Ok(vec![PlexDeclaration {
            name,
            type_annotation,
            is_public,
            generic_params,
        }]),
        MacroInvocation { name, arguments: _ } => {
            // MacroInvocation should be handled by the macro system's preprocess_statement
            // If we reach here, it means the macro wasn't expanded, so we treat it as an error
            Err(ExpansionError::MacroNotFound {
                name,
                call_site: veld_common::source::NodeId::new(),
            })
        }
        // Handle any remaining statement types
        Break => Ok(vec![Break]),
        Continue => Ok(vec![Continue]),
        UnionDeclaration {
            name,
            variants,
            is_public,
            generic_params,
        } => Ok(vec![UnionDeclaration {
            name,
            variants,
            is_public,
            generic_params,
        }]),
    }
}

/// Recursively expand macros in an expression
fn expand_macros_in_expr(
    expr: Expr,
    macro_system: &mut MacroSystem,
) -> Result<Expr, ExpansionError> {
    // The preprocess_expr method from MacroSystem handles the recursive expansion
    // of expressions, including MacroExpr and other expression types
    let expanded = macro_system.preprocess_expr(expr)?;

    // Handle any child expressions that might need expansion
    match expanded {
        Expr::BlockExpression {
            statements,
            final_expr,
        } => {
            let statements = statements
                .into_iter()
                .map(|s| expand_macros_in_statement(s, macro_system))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
            let final_expr = match final_expr {
                Some(expr) => Some(Box::new(expand_macros_in_expr(*expr, macro_system)?)),
                None => None,
            };
            Ok(Expr::BlockExpression {
                statements,
                final_expr,
            })
        }
        Expr::IfExpression {
            condition,
            then_expr,
            else_expr,
        } => {
            let condition = Box::new(expand_macros_in_expr(*condition, macro_system)?);
            let then_expr = Box::new(expand_macros_in_expr(*then_expr, macro_system)?);
            let else_expr = match else_expr {
                Some(expr) => Some(Box::new(expand_macros_in_expr(*expr, macro_system)?)),
                None => None,
            };
            Ok(Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            })
        }
        Expr::Lambda {
            params,
            body,
            return_type,
            generic_params,
        } => {
            let body = Box::new(expand_macros_in_expr(*body, macro_system)?);
            Ok(Expr::Lambda {
                params,
                body,
                return_type,
                generic_params,
            })
        }
        Expr::BlockLambda {
            params,
            body,
            return_type,
            generic_params,
        } => {
            let body = body
                .into_iter()
                .map(|s| expand_macros_in_statement(s, macro_system))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
            Ok(Expr::BlockLambda {
                params,
                body,
                return_type,
                generic_params,
            })
        }
        Expr::BinaryOp {
            left,
            operator,
            right,
        } => {
            let left = Box::new(expand_macros_in_expr(*left, macro_system)?);
            let right = Box::new(expand_macros_in_expr(*right, macro_system)?);
            Ok(Expr::BinaryOp {
                left,
                operator,
                right,
            })
        }
        Expr::UnaryOp { operator, operand } => {
            let operand = Box::new(expand_macros_in_expr(*operand, macro_system)?);
            Ok(Expr::UnaryOp { operator, operand })
        }
        Expr::FunctionCall { name, arguments } => {
            // Patch: Rewrite to_string(x) as x.to_string()
            if name == "to_string" && arguments.len() == 1 {
                if let veld_common::ast::Argument::Positional(expr) = &arguments[0] {
                    return Ok(Expr::MethodCall {
                        object: Box::new(expand_macros_in_expr(expr.clone(), macro_system)?),
                        method: "to_string".to_string(),
                        arguments: vec![],
                    });
                }
            }
            let arguments = arguments
                .into_iter()
                .map(|arg| match arg {
                    veld_common::ast::Argument::Positional(expr) => {
                        Ok(veld_common::ast::Argument::Positional(
                            expand_macros_in_expr(expr, macro_system)?,
                        ))
                    }
                    veld_common::ast::Argument::Named { name, value } => {
                        Ok(veld_common::ast::Argument::Named {
                            name,
                            value: expand_macros_in_expr(value, macro_system)?,
                        })
                    }
                })
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::FunctionCall { name, arguments })
        }
        Expr::Call { callee, arguments } => {
            let callee = Box::new(expand_macros_in_expr(*callee, macro_system)?);
            let arguments = arguments
                .into_iter()
                .map(|arg| match arg {
                    veld_common::ast::Argument::Positional(expr) => {
                        Ok(veld_common::ast::Argument::Positional(
                            expand_macros_in_expr(expr, macro_system)?,
                        ))
                    }
                    veld_common::ast::Argument::Named { name, value } => {
                        Ok(veld_common::ast::Argument::Named {
                            name,
                            value: expand_macros_in_expr(value, macro_system)?,
                        })
                    }
                })
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::Call { callee, arguments })
        }
        Expr::MethodCall {
            object,
            method,
            arguments,
        } => {
            let object = Box::new(expand_macros_in_expr(*object, macro_system)?);
            let arguments = arguments
                .into_iter()
                .map(|arg| match arg {
                    veld_common::ast::Argument::Positional(expr) => {
                        Ok(veld_common::ast::Argument::Positional(
                            expand_macros_in_expr(expr, macro_system)?,
                        ))
                    }
                    veld_common::ast::Argument::Named { name, value } => {
                        Ok(veld_common::ast::Argument::Named {
                            name,
                            value: expand_macros_in_expr(value, macro_system)?,
                        })
                    }
                })
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::MethodCall {
                object,
                method,
                arguments,
            })
        }
        Expr::PropertyAccess { object, property } => {
            let object = Box::new(expand_macros_in_expr(*object, macro_system)?);
            Ok(Expr::PropertyAccess { object, property })
        }
        Expr::StructCreate {
            struct_name,
            fields,
        } => {
            let fields = fields
                .into_iter()
                .map(|(name, expr)| Ok((name, expand_macros_in_expr(expr, macro_system)?)))
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::StructCreate {
                struct_name,
                fields,
            })
        }
        Expr::Record { fields } => {
            let fields = fields
                .into_iter()
                .map(|(name, expr)| Ok((name, expand_macros_in_expr(expr, macro_system)?)))
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::Record { fields })
        }
        Expr::ArrayLiteral(elements) => {
            let elements = elements
                .into_iter()
                .map(|expr| expand_macros_in_expr(expr, macro_system))
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::ArrayLiteral(elements))
        }
        Expr::IndexAccess { object, index } => {
            let object = Box::new(expand_macros_in_expr(*object, macro_system)?);
            let index = Box::new(expand_macros_in_expr(*index, macro_system)?);
            Ok(Expr::IndexAccess { object, index })
        }
        Expr::EnumVariant {
            enum_name,
            variant_name,
            fields,
            type_args,
        } => {
            let fields = fields
                .into_iter()
                .map(|expr| expand_macros_in_expr(expr, macro_system))
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::EnumVariant {
                enum_name,
                variant_name,
                fields,
                type_args,
            })
        }
        Expr::TupleLiteral(elements) => {
            let elements = elements
                .into_iter()
                .map(|expr| expand_macros_in_expr(expr, macro_system))
                .collect::<Result<Vec<_>, ExpansionError>>()?;
            Ok(Expr::TupleLiteral(elements))
        }
        Expr::TupleAccess { tuple, index } => {
            let tuple = Box::new(expand_macros_in_expr(*tuple, macro_system)?);
            Ok(Expr::TupleAccess { tuple, index })
        }
        Expr::TypeCast { expr, target_type } => {
            let expr = Box::new(expand_macros_in_expr(*expr, macro_system)?);
            Ok(Expr::TypeCast { expr, target_type })
        }
        Expr::Range {
            start,
            end,
            inclusive,
        } => {
            let start = match start {
                Some(expr) => Some(Box::new(expand_macros_in_expr(*expr, macro_system)?)),
                None => None,
            };
            let end = match end {
                Some(expr) => Some(Box::new(expand_macros_in_expr(*expr, macro_system)?)),
                None => None,
            };
            Ok(Expr::Range {
                start,
                end,
                inclusive,
            })
        }
        // For expressions that don't need recursive expansion, return as-is
        other => Ok(other),
    }
}
