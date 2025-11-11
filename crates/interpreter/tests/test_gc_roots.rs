use veld_common::value::Value;
use veld_interpreter::interpreter::Interpreter;

#[test]
fn test_gc_roots_collected() {
    let mut interpreter = Interpreter::new(".");

    // Allocate a simple value
    let value = interpreter.allocate_value(Value::Integer(42)).unwrap();

    println!("Allocated value: {:?}", value);

    // Add it to the stack
    interpreter.set_stack_var("test_var".to_string(), value.clone());

    // Collect roots
    let roots = interpreter.collect_gc_roots();

    println!("Collected {} roots", roots.roots().count());
    for root in roots.roots() {
        println!("Root: object_id={}", root.object_id());
    }

    // Check if our value is in the roots
    if let Value::GcRef(handle) = &value {
        println!("Looking for object_id={}", handle.object_id());
        let found = roots.roots().any(|r| r.object_id() == handle.object_id());
        println!("Found in roots: {}", found);
        assert!(found, "Value should be in roots");
    }
}

#[test]
fn test_integer_not_allocated() {
    let mut interpreter = Interpreter::new(".");

    // Integer shouldn't be allocated on heap
    let value = interpreter.allocate_value(Value::Integer(42)).unwrap();
    println!("Integer value: {:?}", value);

    match value {
        Value::GcRef(_) => panic!("Integer should not be a GcRef"),
        Value::Integer(n) => assert_eq!(n, 42),
        _ => panic!("Expected Integer"),
    }
}

#[test]
fn test_string_is_allocated() {
    let interpreter = Interpreter::new(".");

    // String should be allocated on heap
    let value = interpreter
        .allocate_value(Value::String("test".to_string()))
        .unwrap();
    println!("String value: {:?}", value);

    match value {
        Value::GcRef(_) => {} // Expected
        _ => panic!("String should be a GcRef"),
    }
}

#[test]
fn test_string_root_survives_gc() {
    let mut interpreter = Interpreter::new(".");

    // String should be allocated on heap
    let value = interpreter
        .allocate_value(Value::String("persistent".to_string()))
        .unwrap();
    println!("String value before GC: {:?}", value);

    // Add to stack as root
    interpreter.set_stack_var("my_string".to_string(), value.clone());

    // Collect roots
    let roots = interpreter.collect_gc_roots();
    println!("Collected {} roots", roots.roots().count());
    for root in roots.roots() {
        println!("Root: object_id={}", root.object_id());
    }

    // Verify it's in roots
    if let Value::GcRef(handle) = &value {
        println!("String handle: object_id={}", handle.object_id());
        let found = roots.roots().any(|r| r.object_id() == handle.object_id());
        println!("Found in roots: {}", found);
        assert!(found, "String should be in roots before GC");
    } else {
        panic!("String should be a GcRef");
    }

    // Perform GC
    let result = interpreter.perform_full_gc().unwrap();
    println!("GC result: {:?}", result);

    // Check if it survived
    let allocator = interpreter.allocator.read().unwrap();
    if let Value::GcRef(handle) = &value {
        let survived = allocator.get_value(&handle).is_some();
        println!("String survived GC: {}", survived);
        if let Some(val) = allocator.get_value(&handle) {
            println!("Value after GC: {:?}", val);
        }
        assert!(survived, "Rooted string should survive GC");
    }
}
