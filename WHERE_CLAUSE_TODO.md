# Where Clause Implementation Status

## Overview
This document tracks the implementation status of where clauses in Veld, which allow conditional implementations based on type constraints.

## Target Syntax
```veld
impl<T> Option<T> <- ToString where T: ToString
    pub fn to_string(self) -> str
        match self
            Option.Some(value) => "Some(" + value.to_string() + ")",
            Option.None => "None",
        end
    end
end
```

## Current Status

### ✅ Completed
1. **AST Support**: Added `WhereClause` and `TypeConstraint` structures to AST
2. **Parser Support**: Basic parsing of `where T: Kind` syntax
3. **Compilation**: All existing code compiles with where clause fields added
4. **Infrastructure**: Updated all pattern matches and construction sites

### 🔄 In Progress
- Parser handles basic single constraint syntax but needs extension

### ❌ Not Implemented (TODOs)

#### 1. Parser Enhancements
**Location**: `crates/common/src/parser.rs:parse_where_clause()`
- Support for multiple constraints per type param: `where T: Kind1 + Kind2`
- Support for associated type constraints: `where T: Iterator<Item = U>`
- Better error handling for malformed constraints
- Integration with complex generic syntax

#### 2. Type Checker Integration
**Location**: `crates/common/src/types/checker.rs:type_check_implementation()`
**Current**: Where clauses are parsed but ignored
**Needed**:
- Validate that type parameters in constraints match generic parameters
- Verify that implementing types actually satisfy the constraints
- Store constraints with implementation for later validation
- Constraint checking during method resolution

#### 3. Implementation Storage
**Location**: `crates/common/src/types.rs:ImplementationInfo`
**Current**: `ImplementationInfo` has TODO for where clause field
**Needed**:
- Add `where_clause: Option<WhereClause>` field
- Update all construction sites to pass where clauses
- Modify lookup methods to validate constraints

#### 4. Runtime Constraint Validation
**Location**: `crates/interpreter/src/interpreter.rs`

**Method Call Validation** (`call_method_value_with_mutation` around line 6110):
- Before allowing enum method calls, validate where clause constraints
- Example: `Option<Point>.to_string()` should only work if `Point: ToString`

**Type Coercion** (`try_safe_coerce_value` around line 6810):
- When coercing enum types, validate that target type arguments satisfy constraints
- Prevent coercion if where clause requirements aren't met

#### 5. Method Resolution
**Location**: `crates/common/src/types.rs:find_implementation()`
**Current**: Simple name matching
**Needed**:
- Check that concrete types satisfy where clause constraints
- Return implementation only if all constraints are met
- Proper error messages when constraints fail

## Implementation Priority

1. **High Priority**: Type checker constraint validation
2. **Medium Priority**: Runtime method call validation  
3. **Low Priority**: Parser enhancements for complex constraints

## Test Cases Needed

1. **Basic where clause**: `where T: ToString`
2. **Multiple constraints**: `where T: ToString + Clone`
3. **Nested constraints**: `where T: Iterator<Item = U>, U: Display`
4. **Constraint violations**: Should produce clear error messages
5. **Method availability**: Methods should only be callable when constraints are satisfied

## Related Files
- `veld/stdlib/option.veld` - Example usage with TODO comment
- `veld/tests/test_type_annotation_fix.veld` - Test that currently works without constraints
- All enum and generic type implementations in stdlib

## Notes
- The basic enum coercion fix (for `None` type annotations) works independently
- Where clauses are syntactic sugar for better type safety and clearer APIs
- Implementation should be backward compatible with existing code