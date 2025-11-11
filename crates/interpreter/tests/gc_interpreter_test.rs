use veld_common::value::Value;
use veld_interpreter::interpreter::Interpreter;

#[test]
fn stack_roots_survive_gc() {
    let mut interpreter = Interpreter::new(".");
    let value_a = interpreter.allocate_value(Value::Integer(100)).unwrap();
    let value_b = interpreter
        .allocate_value(Value::String("persistent".into()))
        .unwrap();
    let value_c = interpreter
        .allocate_value(Value::Array(vec![Value::Integer(1), Value::Integer(2)]))
        .unwrap();

    interpreter.set_stack_var("a".to_string(), value_a.clone());
    interpreter.set_stack_var("b".to_string(), value_b.clone());
    // value_c is not rooted

    // Perform garbage collection to remove unreachable value_c
    interpreter.perform_full_gc().unwrap();

    let allocator = interpreter.allocator.read().unwrap();

    if let Value::GcRef(handle_a) = value_a {
        assert!(allocator.get_value(&handle_a).is_some());
    }
    if let Value::GcRef(handle_b) = value_b {
        assert!(allocator.get_value(&handle_b).is_some());
    }
    if let Value::GcRef(handle_c) = value_c {
        assert!(allocator.get_value(&handle_c).is_none());
    }
}

#[test]
fn closure_captured_roots_survive_gc() {
    let mut interpreter = Interpreter::new(".");
    let captured_value = interpreter
        .allocate_value(Value::String("captured".into()))
        .unwrap();

    let mut captured_vars = std::collections::HashMap::new();
    captured_vars.insert("x".to_string(), captured_value.clone());
    let closure = Value::Function {
        params: vec![],
        body: vec![],
        return_type: veld_common::ast::TypeAnnotation::Unit,
        captured_vars,
    };
    let closure_value = interpreter.allocate_value(closure).unwrap();

    interpreter.set_stack_var("closure".to_string(), closure_value.clone());

    // Perform garbage collection
    interpreter.perform_full_gc().unwrap();

    let allocator = interpreter.allocator.read().unwrap();

    if let Value::GcRef(closure_handle) = closure_value {
        assert!(allocator.get_value(&closure_handle).is_some());
    }
    if let Value::GcRef(captured_handle) = captured_value {
        assert!(allocator.get_value(&captured_handle).is_some());
    }
}

#[test]
fn nested_gc_values_survive_gc() {
    let mut interpreter = Interpreter::new(".");
    let string_value = interpreter
        .allocate_value(Value::String("deep".into()))
        .unwrap();
    let array_value = interpreter
        .allocate_value(Value::Array(vec![string_value.clone()]))
        .unwrap();
    let mut fields = std::collections::HashMap::new();
    fields.insert("arr".to_string(), array_value.clone());
    let struct_value = interpreter
        .allocate_value(Value::Struct {
            name: "TestStruct".to_string(),
            fields,
        })
        .unwrap();

    interpreter.set_stack_var("struct".to_string(), struct_value.clone());

    // Perform garbage collection
    interpreter.perform_full_gc().unwrap();

    let allocator = interpreter.allocator.read().unwrap();

    if let Value::GcRef(struct_handle) = struct_value {
        assert!(allocator.get_value(&struct_handle).is_some());
    }
    if let Value::GcRef(array_handle) = array_value {
        assert!(allocator.get_value(&array_handle).is_some());
    }
    if let Value::GcRef(string_handle) = string_value {
        assert!(allocator.get_value(&string_handle).is_some());
    }
}

#[test]
fn unreachable_nested_gc_values_are_collected() {
    let mut interpreter = Interpreter::new(".");
    let string_value = interpreter
        .allocate_value(Value::String("gone".into()))
        .unwrap();
    let array_value = interpreter
        .allocate_value(Value::Array(vec![string_value.clone()]))
        .unwrap();
    let mut fields = std::collections::HashMap::new();
    fields.insert("arr".to_string(), array_value.clone());
    let struct_value = interpreter
        .allocate_value(Value::Struct {
            name: "TestStruct".to_string(),
            fields,
        })
        .unwrap();

    // No roots set - all values should be collected

    // Perform garbage collection to collect unreachable values
    interpreter.perform_full_gc().unwrap();

    let allocator = interpreter.allocator.read().unwrap();

    if let Value::GcRef(struct_handle) = struct_value {
        assert!(allocator.get_value(&struct_handle).is_none());
    }
    if let Value::GcRef(array_handle) = array_value {
        assert!(allocator.get_value(&array_handle).is_none());
    }
    if let Value::GcRef(string_handle) = string_value {
        assert!(allocator.get_value(&string_handle).is_none());
    }
}
