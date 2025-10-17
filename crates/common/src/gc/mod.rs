//! Garbage Collection System for Veld Interpreter
//!
//! This module implements a modern, generational garbage collector optimized for
//! functional programming languages with immutable data structures and closures.
//!
//! ## Design Philosophy
//!
//! - **Generational Collection**: Most objects die young, so we segregate by age
//! - **Incremental Collection**: Avoid stop-the-world pauses with incremental marking
//! - **Precise Collection**: Track exact object references, not conservative scanning
//! - **Value Integration**: Work seamlessly with Veld's Value enum system
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
//! │   Young Gen     │────▶│   Old Gen       │────▶│   Permanent     │
//! │ (Eden + S0/S1)  │    │  (Tenured)      │    │   (Builtins)    │
//! └─────────────────┘    └─────────────────┘    └─────────────────┘
//!        │                       │                       │
//!        ▼                       ▼                       ▼
//!  Fast allocation          Infrequent GC           Never collected
//! ```

pub mod allocator;
pub mod collector;
pub mod handle;
pub mod mark;
pub mod root_set;
pub mod statistics;
pub mod value_ref;

use std::sync::{Arc, Mutex, RwLock};
use std::time::Instant;

pub use allocator::GcAllocator;
pub use collector::{CollectionResult, GcCollector};
pub use handle::{GcHandle, WeakGcHandle};
pub use mark::{MarkPhase, MarkState};
pub use root_set::RootSet;
pub use statistics::GcStatistics;
pub use value_ref::GcValueRef;

use super::value::Value;
use veld_error::{Result, VeldError};

/// Configuration for the garbage collector
#[derive(Debug, Clone)]
pub struct GcConfig {
    /// Initial heap size in bytes
    pub initial_heap_size: usize,
    /// Maximum heap size before OOM (0 = unlimited)
    pub max_heap_size: usize,
    /// Threshold for triggering young generation GC (as ratio of young gen size)
    pub young_gen_threshold: f64,
    /// Threshold for promoting objects to old generation (number of collections survived)
    pub promotion_threshold: u32,
    /// Maximum time for incremental collection step in microseconds
    pub max_incremental_time: u64,
    /// Enable/disable concurrent collection
    pub concurrent_collection: bool,
    /// Enable/disable generational collection
    pub generational: bool,
}

impl Default for GcConfig {
    fn default() -> Self {
        Self {
            initial_heap_size: 1024 * 1024, // 1MB
            max_heap_size: 0,               // Unlimited
            young_gen_threshold: 0.8,       // 80% full
            promotion_threshold: 2,         // Survive 2 collections
            max_incremental_time: 1000,     // 1ms per step
            concurrent_collection: false,   // Start simple
            generational: true,
        }
    }
}

/// Main garbage collector for the Veld interpreter
pub struct GarbageCollector {
    /// GC configuration
    config: GcConfig,
    /// Object allocator
    allocator: Arc<Mutex<GcAllocator>>,
    /// Collection engine
    collector: Arc<Mutex<GcCollector>>,
    /// Root set tracking
    root_set: Arc<RwLock<RootSet>>,
    /// GC statistics
    statistics: Arc<Mutex<GcStatistics>>,
    /// Whether GC is currently running
    collecting: Arc<Mutex<bool>>,
}

impl GarbageCollector {
    /// Create a new garbage collector with default configuration
    pub fn new() -> Self {
        Self::with_config(GcConfig::default())
    }

    /// Create a new garbage collector with custom configuration
    pub fn with_config(config: GcConfig) -> Self {
        let allocator = Arc::new(Mutex::new(GcAllocator::new(&config)));
        let collector = Arc::new(Mutex::new(GcCollector::new(&config)));
        let root_set = Arc::new(RwLock::new(RootSet::new()));
        let statistics = Arc::new(Mutex::new(GcStatistics::new()));

        Self {
            config,
            allocator,
            collector,
            root_set,
            statistics,
            collecting: Arc::new(Mutex::new(false)),
        }
    }

    /// Allocate a new value in the garbage-collected heap
    pub fn allocate(&self, value: Value) -> Result<GcHandle> {
        let mut allocator = self.allocator.lock().unwrap();
        let handle = allocator.allocate(value)?;

        // Check if we should trigger a collection
        if allocator.should_collect(&self.config) {
            drop(allocator); // Release lock before collection
            self.maybe_collect()?;
        }

        Ok(handle)
    }

    /// Add a root reference (prevents collection)
    pub fn add_root(&self, handle: GcHandle) -> Result<()> {
        let mut root_set = self.root_set.write().unwrap();
        root_set.add_root(handle);
        Ok(())
    }

    /// Remove a root reference
    pub fn remove_root(&self, handle: &GcHandle) -> Result<()> {
        let mut root_set = self.root_set.write().unwrap();
        root_set.remove_root(handle);
        Ok(())
    }

    /// Force a garbage collection cycle
    pub fn collect(&self) -> Result<CollectionResult> {
        let mut collecting = self.collecting.lock().unwrap();
        if *collecting {
            // Collection already in progress
            return Ok(CollectionResult::default());
        }
        *collecting = true;
        drop(collecting);

        let result = self.do_collect();

        let mut collecting = self.collecting.lock().unwrap();
        *collecting = false;

        result
    }

    /// Perform incremental collection step
    pub fn collect_incremental(&self) -> Result<bool> {
        let mut collector = self.collector.lock().unwrap();
        let mut allocator = self.allocator.lock().unwrap();
        let root_set = self.root_set.read().unwrap();

        let completed = collector.collect_incremental(&mut *allocator, &*root_set, &self.config)?;

        if completed {
            let mut stats = self.statistics.lock().unwrap();
            stats.record_collection(collector.last_collection_result());
        }

        Ok(completed)
    }

    /// Get current GC statistics
    pub fn statistics(&self) -> GcStatistics {
        self.statistics.lock().unwrap().clone()
    }

    /// Get current heap size
    pub fn heap_size(&self) -> usize {
        self.allocator.lock().unwrap().heap_size()
    }

    /// Get number of allocated objects
    pub fn object_count(&self) -> usize {
        self.allocator.lock().unwrap().object_count()
    }

    /// Check if an object is still alive
    pub fn is_alive(&self, handle: &GcHandle) -> bool {
        let allocator = self.allocator.lock().unwrap();
        allocator.is_alive(handle)
    }

    /// Dereference a handle to get the value (if still alive)
    pub fn deref(&self, handle: &GcHandle) -> Option<Value> {
        let allocator = self.allocator.lock().unwrap();
        allocator.get_value(handle).cloned()
    }

    /// Update the value at a handle (for mutable references)
    pub fn update(&self, handle: &GcHandle, value: Value) -> Result<()> {
        let mut allocator = self.allocator.lock().unwrap();
        allocator.update_value(handle, value)
    }

    /// Create a weak reference that doesn't prevent collection
    pub fn weak_ref(&self, handle: &GcHandle) -> WeakGcHandle {
        WeakGcHandle::new(handle)
    }

    /// Upgrade a weak reference to a strong reference (if still alive)
    pub fn upgrade(&self, weak: &WeakGcHandle) -> Option<GcHandle> {
        weak.upgrade(self)
    }

    /// Run finalizers for collected objects
    pub fn run_finalizers(&self) -> Result<()> {
        let mut collector = self.collector.lock().unwrap();
        collector.run_finalizers()
    }

    /// Set GC parameters at runtime
    pub fn tune(&mut self, config: GcConfig) {
        self.config = config;
    }

    /// Private method to maybe trigger collection
    fn maybe_collect(&self) -> Result<()> {
        // Simple heuristic: collect if we're not already collecting
        let collecting = self.collecting.lock().unwrap();
        if !*collecting {
            drop(collecting);
            self.collect()?;
        }
        Ok(())
    }

    /// Private method to perform the actual collection
    fn do_collect(&self) -> Result<CollectionResult> {
        let start_time = Instant::now();

        let mut collector = self.collector.lock().unwrap();
        let mut allocator = self.allocator.lock().unwrap();
        let root_set = self.root_set.read().unwrap();

        // Perform the collection
        let result = collector.collect_full(&mut *allocator, &*root_set, &self.config)?;

        // Update statistics
        let mut stats = self.statistics.lock().unwrap();
        stats.record_collection(&result);

        tracing::info!(
            "GC completed: collected {} objects, freed {} bytes in {:?}",
            result.objects_collected,
            result.bytes_freed,
            start_time.elapsed()
        );

        Ok(result)
    }
}

impl Default for GarbageCollector {
    fn default() -> Self {
        Self::new()
    }
}

/// Safe wrapper for GC operations
pub struct SafeGc {
    gc: Arc<GarbageCollector>,
}

impl SafeGc {
    pub fn new(gc: GarbageCollector) -> Self {
        Self { gc: Arc::new(gc) }
    }

    pub fn allocate(&self, value: Value) -> Result<GcHandle> {
        self.gc.allocate(value)
    }

    pub fn deref(&self, handle: &GcHandle) -> Option<Value> {
        self.gc.deref(handle)
    }

    pub fn collect(&self) -> Result<CollectionResult> {
        self.gc.collect()
    }

    pub fn statistics(&self) -> GcStatistics {
        self.gc.statistics()
    }

    pub fn add_root(&self, handle: GcHandle) -> Result<()> {
        self.gc.add_root(handle)
    }

    pub fn remove_root(&self, handle: &GcHandle) -> Result<()> {
        self.gc.remove_root(handle)
    }
}

impl Clone for SafeGc {
    fn clone(&self) -> Self {
        Self {
            gc: Arc::clone(&self.gc),
        }
    }
}

/// Thread-local GC context for performance
thread_local! {
    static GC_CONTEXT: std::cell::RefCell<Option<SafeGc>> = std::cell::RefCell::new(None);
}

/// Initialize thread-local GC context
pub fn init_gc_context(gc: SafeGc) {
    GC_CONTEXT.with(|ctx| {
        *ctx.borrow_mut() = Some(gc);
    });
}

/// Get thread-local GC context
pub fn with_gc<T>(f: impl FnOnce(&SafeGc) -> T) -> Option<T> {
    GC_CONTEXT.with(|ctx| ctx.borrow().as_ref().map(f))
}

/// Convenience macro for GC allocation
#[macro_export]
macro_rules! gc_alloc {
    ($value:expr) => {
        $crate::gc::with_gc(|gc| gc.allocate($value)).unwrap_or_else(|| {
            Err($crate::VeldError::RuntimeError(
                "GC not initialized".to_string(),
            ))
        })
    };
}

/// Convenience macro for GC dereferencing
#[macro_export]
macro_rules! gc_deref {
    ($handle:expr) => {
        $crate::gc::with_gc(|gc| gc.deref($handle)).unwrap_or(None)
    };
}

#[cfg(test)]
mod tests {
    use super::super::value::Value;
    use super::*;

    #[test]
    fn test_gc_basic_allocation() {
        let gc = GarbageCollector::new();
        let handle = gc.allocate(Value::Integer(42)).unwrap();
        assert_eq!(gc.deref(&handle), Some(Value::Integer(42)));
    }

    #[test]
    fn test_gc_collection() {
        let gc = GarbageCollector::new();

        // Allocate some values
        let _handle1 = gc.allocate(Value::Integer(1)).unwrap();
        let handle2 = gc.allocate(Value::Integer(2)).unwrap();

        // Add handle2 as root
        gc.add_root(handle2.clone()).unwrap();

        // Collect - handle1 should be collected, handle2 should survive
        let result = gc.collect().unwrap();
        assert!(result.objects_collected > 0);

        // handle2 should still be alive
        assert!(gc.is_alive(&handle2));
    }

    #[test]
    fn test_weak_references() {
        let gc = GarbageCollector::new();
        let handle = gc.allocate(Value::String("test".to_string())).unwrap();
        let weak = gc.weak_ref(&handle);

        // Should be able to upgrade while handle exists
        assert!(gc.upgrade(&weak).is_some());

        drop(handle);
        gc.collect().unwrap();

        // Should not be able to upgrade after collection
        assert!(gc.upgrade(&weak).is_none());
    }
}
