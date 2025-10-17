#![allow(unused)]
//! Simple integration test for Veld garbage collector
//! This test allocates objects, drops references, triggers GC, and checks collection statistics.

use veld_common::gc::GcConfig;
use veld_common::gc::allocator::GcAllocator;
use veld_common::gc::collector::{CollectionStrategy, GcCollector};
use veld_common::gc::handle::GcHandle;
use veld_common::gc::root_set::RootSet;
use veld_common::gc::statistics::GcStatistics;
use veld_common::value::Value;

#[test]
fn gc_alloc_drop_collect() {
    // Create GC config, allocator, root set, and collector
    let config = GcConfig::default();
    let mut allocator = GcAllocator::default();
    let mut root_set = RootSet::new();
    let mut collector = GcCollector::new(&config);

    // Allocate several objects
    let handle1 = allocator
        .allocate(Value::Integer(42))
        .expect("allocation failed");
    let handle2 = allocator
        .allocate(Value::String("hello".to_string()))
        .expect("allocation failed");
    let handle3 = allocator
        .allocate(Value::Array(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]))
        .expect("allocation failed");

    // Add some to root set, drop others
    root_set.add_global_root(handle1.clone());
    // handle2 and handle3 are not rooted

    // Trigger GC (minor collection)
    let result_minor = collector
        .collect(
            CollectionStrategy::Minor,
            &mut allocator,
            &root_set,
            &config,
        )
        .expect("GC failed");

    // handle2 and handle3 should be collected, handle1 should survive
    assert!(
        allocator.is_alive(&handle1),
        "Rooted handle1 should survive GC"
    );
    assert!(
        !allocator.is_alive(&handle2),
        "Unrooted handle2 should be collected"
    );
    assert!(
        !allocator.is_alive(&handle3),
        "Unrooted handle3 should be collected"
    );

    // Print statistics for manual inspection
    println!("Minor GC result: {:?}", result_minor);

    // Remove handle1 from root set and trigger another GC
    root_set.remove_global_root(&handle1);
    let result_major = collector
        .collect(
            CollectionStrategy::Major,
            &mut allocator,
            &root_set,
            &config,
        )
        .expect("GC failed");

    // Now handle1 should be collected
    assert!(
        !allocator.is_alive(&handle1),
        "handle1 should be collected after dropping root"
    );

    println!("Major GC result: {:?}", result_major);
}
