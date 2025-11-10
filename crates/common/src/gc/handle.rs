//! GC Handle System for Safe Object References
//!
//! Handles provide a safe way to reference garbage-collected objects without
//! directly holding pointers. They use generation counters to detect use-after-free
//! and provide both strong and weak reference semantics.

use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::types::Type;

/// Unique identifier for a GC handle
static HANDLE_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generation counter for detecting stale handles
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Generation(pub u64);

impl Generation {
    pub fn new() -> Self {
        Self(HANDLE_COUNTER.fetch_add(1, Ordering::SeqCst))
    }

    pub fn next(&self) -> Self {
        Self(self.0 + 1)
    }

    pub fn value(&self) -> u64 {
        self.0
    }
}

impl Default for Generation {
    fn default() -> Self {
        Self::new()
    }
}

/// Strong reference to a garbage-collected object
///
/// GcHandle provides safe access to GC-managed objects. The handle becomes
/// invalid if the object is collected, preventing use-after-free errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GcHandle {
    /// Unique object identifier
    pub(crate) object_id: u64,
    /// Generation counter for validity checking
    pub(crate) generation: Generation,
}

impl GcHandle {
    pub fn type_of(&self) -> Type {
        // TODO: Implement proper type tracking for GC-managed values
        // For now, return Any to allow the system to work
        Type::Any
    }
}

impl GcHandle {
    /// Create a new GC handle
    pub(crate) fn new(object_id: u64, generation: Generation) -> Self {
        Self {
            object_id,
            generation,
        }
    }

    /// Get the object ID
    pub fn object_id(&self) -> u64 {
        self.object_id
    }

    /// Get the generation
    pub fn generation(&self) -> Generation {
        self.generation
    }

    /// Check if this handle is potentially valid (generation-based check)
    pub fn is_valid_generation(&self, current_gen: Generation) -> bool {
        self.generation == current_gen
    }
}

impl fmt::Display for GcHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GcHandle({}, gen={})", self.object_id, self.generation.0)
    }
}

/// Weak reference to a garbage-collected object
///
/// WeakGcHandle doesn't prevent the object from being collected.
/// It can be upgraded to a strong reference if the object is still alive.
#[derive(Debug, Clone)]
pub struct WeakGcHandle {
    /// Unique object identifier
    pub(crate) object_id: u64,
    /// Generation when the weak reference was created
    pub(crate) generation: Generation,
}

impl WeakGcHandle {
    /// Create a new weak reference from a strong handle
    pub fn new(handle: &GcHandle) -> Self {
        Self {
            object_id: handle.object_id,
            generation: handle.generation,
        }
    }

    /// Get the object ID
    pub fn object_id(&self) -> u64 {
        self.object_id
    }

    /// Get the generation
    pub fn generation(&self) -> Generation {
        self.generation
    }

    /// Try to upgrade to a strong reference
    pub fn upgrade(&self, gc: &crate::gc::GarbageCollector) -> Option<GcHandle> {
        let handle = GcHandle {
            object_id: self.object_id,
            generation: self.generation,
        };

        if gc.is_alive(&handle) {
            Some(handle)
        } else {
            None
        }
    }
}

impl PartialEq for WeakGcHandle {
    fn eq(&self, other: &Self) -> bool {
        self.object_id == other.object_id && self.generation == other.generation
    }
}

impl Eq for WeakGcHandle {}

impl Hash for WeakGcHandle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.object_id.hash(state);
        self.generation.hash(state);
    }
}

impl fmt::Display for WeakGcHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "WeakGcHandle({}, gen={})",
            self.object_id, self.generation.0
        )
    }
}

/// Handle pool for managing object lifetimes
#[derive(Debug, Clone)]
pub struct HandlePool {
    /// Current generation counter
    current_generation: Generation,
    /// Pool of reusable object IDs
    free_ids: Vec<u64>,
    /// Next object ID to allocate
    next_id: u64,
}

impl HandlePool {
    /// Create a new handle pool
    pub fn new() -> Self {
        Self {
            current_generation: Generation::new(),
            free_ids: Vec::new(),
            next_id: 1,
        }
    }

    /// Allocate a new object ID and create a handle
    pub fn allocate(&mut self) -> GcHandle {
        let object_id = if let Some(id) = self.free_ids.pop() {
            id
        } else {
            let id = self.next_id;
            self.next_id += 1;
            id
        };

        GcHandle::new(object_id, self.current_generation)
    }

    /// Free an object ID for reuse
    pub fn deallocate(&mut self, handle: &GcHandle) {
        if handle.generation == self.current_generation {
            self.free_ids.push(handle.object_id);
        }
    }

    /// Advance to the next generation (invalidates all current handles)
    pub fn next_generation(&mut self) {
        self.current_generation = self.current_generation.next();
        self.free_ids.clear(); // Don't reuse IDs across generations
    }

    /// Get the current generation
    pub fn current_generation(&self) -> Generation {
        self.current_generation
    }

    /// Check if a handle is valid for the current generation
    pub fn is_valid(&self, handle: &GcHandle) -> bool {
        handle.generation == self.current_generation
    }
}

impl Default for HandlePool {
    fn default() -> Self {
        Self::new()
    }
}

/// Scoped handle for automatic cleanup
pub struct ScopedHandle {
    handle: Option<GcHandle>,
    gc: Option<crate::gc::SafeGc>,
}

impl ScopedHandle {
    /// Create a new scoped handle
    pub fn new(handle: GcHandle, gc: crate::gc::SafeGc) -> Self {
        Self {
            handle: Some(handle),
            gc: Some(gc),
        }
    }

    /// Get the underlying handle
    pub fn handle(&self) -> Option<&GcHandle> {
        self.handle.as_ref()
    }

    /// Take ownership of the handle (prevents automatic cleanup)
    pub fn take(mut self) -> Option<GcHandle> {
        self.handle.take()
    }
}

impl Drop for ScopedHandle {
    fn drop(&mut self) {
        if let (Some(handle), Some(gc)) = (self.handle.take(), self.gc.take()) {
            // Remove from root set when dropped
            let _ = gc.remove_root(&handle);
        }
    }
}

/// Handle set for managing collections of handles
#[derive(Debug, Default)]
pub struct HandleSet {
    handles: std::collections::HashSet<GcHandle>,
}

impl HandleSet {
    /// Create a new handle set
    pub fn new() -> Self {
        Self {
            handles: std::collections::HashSet::new(),
        }
    }

    /// Add a handle to the set
    pub fn insert(&mut self, handle: GcHandle) -> bool {
        self.handles.insert(handle)
    }

    /// Remove a handle from the set
    pub fn remove(&mut self, handle: &GcHandle) -> bool {
        self.handles.remove(handle)
    }

    /// Check if the set contains a handle
    pub fn contains(&self, handle: &GcHandle) -> bool {
        self.handles.contains(handle)
    }

    /// Get all handles in the set
    pub fn handles(&self) -> impl Iterator<Item = &GcHandle> {
        self.handles.iter()
    }

    /// Clear all handles
    pub fn clear(&mut self) {
        self.handles.clear();
    }

    /// Get the number of handles
    pub fn len(&self) -> usize {
        self.handles.len()
    }

    /// Check if the set is empty
    pub fn is_empty(&self) -> bool {
        self.handles.is_empty()
    }

    /// Filter handles by a predicate
    pub fn filter<F>(&mut self, mut predicate: F)
    where
        F: FnMut(&GcHandle) -> bool,
    {
        self.handles.retain(|handle| predicate(handle));
    }

    /// Convert to a vector of handles
    pub fn to_vec(&self) -> Vec<GcHandle> {
        self.handles.iter().cloned().collect()
    }
}

impl FromIterator<GcHandle> for HandleSet {
    fn from_iter<T: IntoIterator<Item = GcHandle>>(iter: T) -> Self {
        Self {
            handles: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for HandleSet {
    type Item = GcHandle;
    type IntoIter = std::collections::hash_set::IntoIter<GcHandle>;

    fn into_iter(self) -> Self::IntoIter {
        self.handles.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generation_uniqueness() {
        let gen1 = Generation::new();
        let gen2 = Generation::new();
        assert_ne!(gen1, gen2);
    }

    #[test]
    fn test_handle_equality() {
        let generation = Generation::new();
        let handle1 = GcHandle::new(1, generation);
        let handle2 = GcHandle::new(1, generation);
        let handle3 = GcHandle::new(2, generation);

        assert_eq!(handle1, handle2);
        assert_ne!(handle1, handle3);
    }

    #[test]
    fn test_handle_pool() {
        let mut pool = HandlePool::new();
        let handle1 = pool.allocate();
        let handle2 = pool.allocate();

        assert_ne!(handle1.object_id, handle2.object_id);
        assert_eq!(handle1.generation, handle2.generation);

        pool.deallocate(&handle1);
        let handle3 = pool.allocate();

        // Should reuse the ID
        assert_eq!(handle1.object_id, handle3.object_id);
    }

    #[test]
    fn test_generation_invalidation() {
        let mut pool = HandlePool::new();
        let handle = pool.allocate();

        assert!(pool.is_valid(&handle));

        pool.next_generation();

        assert!(!pool.is_valid(&handle));
    }

    #[test]
    fn test_weak_handle_creation() {
        let generation = Generation::new();
        let strong = GcHandle::new(42, generation);
        let weak = WeakGcHandle::new(&strong);

        assert_eq!(weak.object_id(), strong.object_id());
        assert_eq!(weak.generation(), strong.generation());
    }

    #[test]
    fn test_handle_set() {
        let mut set = HandleSet::new();
        let generation = Generation::new();
        let handle1 = GcHandle::new(1, generation);
        let handle2 = GcHandle::new(2, generation);

        assert!(set.insert(handle1.clone()));
        assert!(set.insert(handle2.clone()));
        assert!(!set.insert(handle1.clone())); // Duplicate

        assert_eq!(set.len(), 2);
        assert!(set.contains(&handle1));
        assert!(set.contains(&handle2));

        assert!(set.remove(&handle1));
        assert_eq!(set.len(), 1);
        assert!(!set.contains(&handle1));
    }
}
