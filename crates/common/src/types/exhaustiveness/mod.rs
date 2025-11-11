//! Pattern Match Exhaustiveness Checking
//!
//! This module implements exhaustiveness checking for pattern matching to ensure
//! that all possible values are handled. This prevents runtime errors from
//! unmatched patterns.
//!
//! ## Algorithm
//!
//! The checker uses a "usefulness" algorithm:
//! 1. Represent the space of all possible values for a type
//! 2. For each pattern, compute what portion of the value space it covers
//! 3. Check if the union of all patterns covers the entire space
//! 4. Report any missing patterns
//!
//! ## Example
//!
//! ```veld
//! enum Option
//!     some(value)
//!     none
//! end
//!
//! match opt
//!     Option.some(x) => x
//!     // Missing: Option.none
//! end  // ERROR: non-exhaustive patterns
//! ```

use crate::ast::{Expr, Literal, MatchPattern};
use crate::types::Type;
use std::collections::{HashMap, HashSet};
use veld_error::{Result, VeldError};

/// Represents the coverage of a pattern - what values it matches
#[derive(Debug, Clone, PartialEq)]
pub enum Coverage {
    /// Matches everything (wildcard or variable binding)
    All,

    /// Matches a specific integer literal
    Integer(i64),

    /// Matches a specific float literal
    Float(f64),

    /// Matches a specific string literal
    String(String),

    /// Matches a specific boolean literal
    Boolean(bool),

    /// Matches a specific enum variant with field patterns
    EnumVariant {
        enum_name: String,
        variant_name: String,
        field_coverage: Vec<Coverage>,
    },

    /// Matches a tuple with element patterns
    Tuple(Vec<Coverage>),

    /// Matches a struct with field patterns
    Struct {
        name: String,
        field_coverage: HashMap<String, Coverage>,
    },

    /// Union of multiple coverages (for match arms)
    Union(Vec<Coverage>),
}

impl Coverage {
    /// Check if this coverage subsumes another (i.e., covers all its cases)
    pub fn subsumes(&self, other: &Coverage) -> bool {
        match (self, other) {
            (Coverage::All, _) => true,
            (_, Coverage::All) => false,

            (Coverage::Integer(a), Coverage::Integer(b)) => a == b,
            (Coverage::Float(a), Coverage::Float(b)) => a == b,
            (Coverage::String(a), Coverage::String(b)) => a == b,
            (Coverage::Boolean(a), Coverage::Boolean(b)) => a == b,

            (
                Coverage::EnumVariant {
                    enum_name: e1,
                    variant_name: v1,
                    field_coverage: f1,
                },
                Coverage::EnumVariant {
                    enum_name: e2,
                    variant_name: v2,
                    field_coverage: f2,
                },
            ) => {
                // Enum variants match if names match
                // If either has empty field coverage, it matches any field count
                // Otherwise, field counts must match and each field must subsume
                if e1 != e2 || v1 != v2 {
                    return false;
                }
                if f1.is_empty() || f2.is_empty() {
                    return true; // Empty field coverage matches any
                }
                f1.len() == f2.len() && f1.iter().zip(f2.iter()).all(|(a, b)| a.subsumes(b))
            }

            (Coverage::Tuple(t1), Coverage::Tuple(t2)) => {
                t1.len() == t2.len() && t1.iter().zip(t2.iter()).all(|(a, b)| a.subsumes(b))
            }

            // Union vs Union: check if every required case is covered by at least one actual case
            (Coverage::Union(actual_cases), Coverage::Union(required_cases)) => required_cases
                .iter()
                .all(|req| actual_cases.iter().any(|act| act.subsumes(req))),

            // If the actual coverage is a Union, check if any of its cases subsumes the required
            (Coverage::Union(actual_cases), other) => {
                actual_cases.iter().any(|c| c.subsumes(other))
            }

            // If the required coverage is a Union, all its cases must be subsumed by actual
            (actual, Coverage::Union(required_cases)) => {
                required_cases.iter().all(|req| actual.subsumes(req))
            }

            _ => false,
        }
    }

    /// Merge two coverages into a union
    pub fn merge(self, other: Coverage) -> Coverage {
        match (self, other) {
            (Coverage::All, _) | (_, Coverage::All) => Coverage::All,
            (Coverage::Union(mut v1), Coverage::Union(v2)) => {
                v1.extend(v2);
                Coverage::Union(v1)
            }
            (Coverage::Union(mut v), c) | (c, Coverage::Union(mut v)) => {
                v.push(c);
                Coverage::Union(v)
            }
            (c1, c2) => Coverage::Union(vec![c1, c2]),
        }
    }
}

/// Exhaustiveness checker for pattern matching
pub struct ExhaustivenessChecker {
    /// Known enum definitions (name -> variants)
    enum_variants: HashMap<String, Vec<String>>,

    /// Known struct definitions (name -> field names)
    struct_fields: HashMap<String, Vec<String>>,
}

impl ExhaustivenessChecker {
    pub fn new() -> Self {
        Self {
            enum_variants: HashMap::new(),
            struct_fields: HashMap::new(),
        }
    }

    /// Register an enum type with its variants
    pub fn register_enum(&mut self, name: String, variants: Vec<String>) {
        self.enum_variants.insert(name, variants);
    }

    /// Register a struct type with its fields
    pub fn register_struct(&mut self, name: String, fields: Vec<String>) {
        self.struct_fields.insert(name, fields);
    }

    /// Check if a match expression is exhaustive
    pub fn check_match_exhaustiveness(
        &self,
        matched_type: &Type,
        patterns: &[MatchPattern],
    ) -> Result<ExhaustivenessResult> {
        // Compute coverage for each pattern
        let coverages: Vec<Coverage> = patterns.iter().map(|p| self.pattern_coverage(p)).collect();

        // Build the total coverage as a union of all patterns
        let total_coverage = if coverages.is_empty() {
            Coverage::Union(vec![])
        } else if coverages.len() == 1 {
            coverages.into_iter().next().unwrap()
        } else {
            Coverage::Union(coverages)
        };

        // Check if total coverage is exhaustive for the type
        let required_coverage = self.type_coverage(matched_type)?;

        if total_coverage.subsumes(&required_coverage) {
            Ok(ExhaustivenessResult::Exhaustive)
        } else {
            let missing = self.compute_missing_patterns(&required_coverage, &total_coverage);
            Ok(ExhaustivenessResult::NonExhaustive { missing })
        }
    }

    /// Compute the coverage of a single pattern
    fn pattern_coverage(&self, pattern: &MatchPattern) -> Coverage {
        match pattern {
            MatchPattern::Wildcard => Coverage::All,

            MatchPattern::Identifier(_) => Coverage::All, // Variable binding matches everything

            MatchPattern::Literal(lit) => match lit {
                Literal::Integer(n) => Coverage::Integer(*n),
                Literal::Float(f) => Coverage::Float(*f),
                Literal::String(s) => Coverage::String(s.clone()),
                Literal::Boolean(b) => Coverage::Boolean(*b),
                _ => Coverage::All, // Conservative: treat unknown literals as wildcards
            },

            MatchPattern::Struct { name, fields } => {
                let field_coverage = fields
                    .iter()
                    .map(|(k, v)| {
                        let cov = match v {
                            Some(nested) => self.pattern_coverage(nested),
                            None => Coverage::All,
                        };
                        (k.clone(), cov)
                    })
                    .collect();
                Coverage::Struct {
                    name: name.clone(),
                    field_coverage,
                }
            }

            MatchPattern::Enum {
                name: enum_name,
                variant,
                fields,
            } => {
                let field_coverage = fields
                    .iter()
                    .map(|(_name, pattern)| match pattern {
                        Some(p) => self.pattern_coverage(p),
                        None => Coverage::All,
                    })
                    .collect();
                Coverage::EnumVariant {
                    enum_name: enum_name.clone(),
                    variant_name: variant.clone(),
                    field_coverage,
                }
            }
        }
    }

    /// Compute the coverage required for a type to be exhaustive
    fn type_coverage(&self, ty: &Type) -> Result<Coverage> {
        match ty {
            Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::String => {
                // Infinite types require a wildcard
                Ok(Coverage::All)
            }

            Type::Bool => {
                // Boolean requires both true and false (or a wildcard)
                Ok(Coverage::Union(vec![
                    Coverage::Boolean(true),
                    Coverage::Boolean(false),
                ]))
            }

            Type::EnumType(enum_name) => {
                // Enum requires all variants to be covered
                if let Some(variants) = self.enum_variants.get(enum_name) {
                    if variants.is_empty() {
                        return Ok(Coverage::All);
                    }
                    let variant_coverages: Vec<Coverage> = variants
                        .iter()
                        .map(|v| Coverage::EnumVariant {
                            enum_name: enum_name.clone(),
                            variant_name: v.clone(),
                            field_coverage: vec![], // Empty fields - will match any field count
                        })
                        .collect();

                    if variant_coverages.len() == 1 {
                        Ok(variant_coverages.into_iter().next().unwrap())
                    } else {
                        Ok(Coverage::Union(variant_coverages))
                    }
                } else {
                    // Unknown enum, be conservative
                    Ok(Coverage::All)
                }
            }

            Type::Tuple(elements) => {
                let element_coverages: Result<Vec<Coverage>> =
                    elements.iter().map(|t| self.type_coverage(t)).collect();
                Ok(Coverage::Tuple(element_coverages?))
            }

            _ => {
                // For other types, require a wildcard
                Ok(Coverage::All)
            }
        }
    }

    /// Compute missing patterns for better error messages
    fn compute_missing_patterns(
        &self,
        required: &Coverage,
        covered: &Coverage,
    ) -> Vec<MissingPattern> {
        match required {
            Coverage::Union(required_cases) => required_cases
                .iter()
                .filter(|req| !covered.subsumes(req))
                .map(|c| self.coverage_to_missing_pattern(c))
                .collect(),
            _ => {
                if covered.subsumes(required) {
                    vec![]
                } else {
                    vec![self.coverage_to_missing_pattern(required)]
                }
            }
        }
    }

    /// Convert coverage to a human-readable missing pattern
    fn coverage_to_missing_pattern(&self, coverage: &Coverage) -> MissingPattern {
        match coverage {
            Coverage::All => MissingPattern::Wildcard,
            Coverage::Integer(n) => MissingPattern::Literal(format!("{}", n)),
            Coverage::Float(f) => MissingPattern::Literal(format!("{}", f)),
            Coverage::String(s) => MissingPattern::Literal(format!("\"{}\"", s)),
            Coverage::Boolean(b) => MissingPattern::Literal(format!("{}", b)),
            Coverage::EnumVariant {
                enum_name,
                variant_name,
                field_coverage,
            } => {
                let fields = if field_coverage.is_empty() {
                    String::new()
                } else {
                    let field_strs: Vec<String> = field_coverage
                        .iter()
                        .map(|_| "_".to_string()) // Simplified: just use wildcards
                        .collect();
                    format!("({})", field_strs.join(", "))
                };
                MissingPattern::EnumVariant(format!("{}.{}{}", enum_name, variant_name, fields))
            }
            Coverage::Tuple(elements) => {
                let element_strs: Vec<String> = elements
                    .iter()
                    .map(|c| match self.coverage_to_missing_pattern(c) {
                        MissingPattern::Wildcard => "_".to_string(),
                        MissingPattern::Literal(s) => s,
                        MissingPattern::EnumVariant(s) => s,
                        MissingPattern::Other(s) => s,
                    })
                    .collect();
                MissingPattern::Other(format!("({})", element_strs.join(", ")))
            }
            _ => MissingPattern::Other("_".to_string()),
        }
    }
}

impl Default for ExhaustivenessChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of exhaustiveness checking
#[derive(Debug, Clone, PartialEq)]
pub enum ExhaustivenessResult {
    /// Match is exhaustive
    Exhaustive,

    /// Match is non-exhaustive with missing patterns
    NonExhaustive { missing: Vec<MissingPattern> },
}

/// Representation of a missing pattern for error messages
#[derive(Debug, Clone, PartialEq)]
pub enum MissingPattern {
    Wildcard,
    Literal(String),
    EnumVariant(String),
    Other(String),
}

impl std::fmt::Display for MissingPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MissingPattern::Wildcard => write!(f, "_"),
            MissingPattern::Literal(s) => write!(f, "{}", s),
            MissingPattern::EnumVariant(s) => write!(f, "{}", s),
            MissingPattern::Other(s) => write!(f, "{}", s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coverage_subsumes() {
        assert!(Coverage::All.subsumes(&Coverage::Integer(42)));
        assert!(!Coverage::Integer(42).subsumes(&Coverage::All));
        assert!(Coverage::Integer(42).subsumes(&Coverage::Integer(42)));
        assert!(!Coverage::Integer(42).subsumes(&Coverage::Integer(43)));
    }

    #[test]
    fn test_boolean_exhaustiveness() {
        let checker = ExhaustivenessChecker::new();

        // Both true and false covered - exhaustive
        let patterns = vec![
            MatchPattern::Literal(Literal::Boolean(true)),
            MatchPattern::Literal(Literal::Boolean(false)),
        ];

        let result = checker
            .check_match_exhaustiveness(&Type::Bool, &patterns)
            .unwrap();
        assert_eq!(result, ExhaustivenessResult::Exhaustive);

        // Only true covered - non-exhaustive
        let patterns = vec![MatchPattern::Literal(Literal::Boolean(true))];

        let result = checker
            .check_match_exhaustiveness(&Type::Bool, &patterns)
            .unwrap();
        match result {
            ExhaustivenessResult::NonExhaustive { missing } => {
                assert_eq!(missing.len(), 1);
            }
            _ => panic!("Expected non-exhaustive"),
        }
    }

    #[test]
    fn test_wildcard_makes_exhaustive() {
        let checker = ExhaustivenessChecker::new();

        // Wildcard makes any match exhaustive
        let patterns = vec![MatchPattern::Wildcard];

        let result = checker
            .check_match_exhaustiveness(&Type::I64, &patterns)
            .unwrap();
        assert_eq!(result, ExhaustivenessResult::Exhaustive);

        let result = checker
            .check_match_exhaustiveness(&Type::Bool, &patterns)
            .unwrap();
        assert_eq!(result, ExhaustivenessResult::Exhaustive);
    }

    #[test]
    fn test_enum_exhaustiveness() {
        let mut checker = ExhaustivenessChecker::new();
        checker.register_enum(
            "Option".to_string(),
            vec!["some".to_string(), "none".to_string()],
        );

        // Both variants covered - exhaustive
        let patterns = vec![
            MatchPattern::Enum {
                name: "Option".to_string(),
                variant: "some".to_string(),
                fields: vec![("value".to_string(), Some(Box::new(MatchPattern::Wildcard)))],
            },
            MatchPattern::Enum {
                name: "Option".to_string(),
                variant: "none".to_string(),
                fields: vec![],
            },
        ];

        let result = checker
            .check_match_exhaustiveness(&Type::EnumType("Option".to_string()), &patterns)
            .unwrap();
        assert_eq!(result, ExhaustivenessResult::Exhaustive);

        // Only 'some' covered - non-exhaustive
        let patterns = vec![MatchPattern::Enum {
            name: "Option".to_string(),
            variant: "some".to_string(),
            fields: vec![("value".to_string(), Some(Box::new(MatchPattern::Wildcard)))],
        }];

        let result = checker
            .check_match_exhaustiveness(&Type::EnumType("Option".to_string()), &patterns)
            .unwrap();
        match result {
            ExhaustivenessResult::NonExhaustive { missing } => {
                assert!(!missing.is_empty());
            }
            _ => panic!("Expected non-exhaustive"),
        }
    }
}
