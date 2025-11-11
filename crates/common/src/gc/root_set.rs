//! Root Set Management for Garbage Collection
//!
//! The root set contains all objects that should not be collected because
//! they are directly accessible by the program. This includes:
//! - Variables in scope
//! - Global variables
//! - Stack frames
//! - Native function references

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::thread::ThreadId;

use super::handle::{GcHandle, HandleSet};
use veld_error::{Result, VeldError};

/// Root set manager for tracking GC roots
#[derive(Debug)]
pub struct RootSet {
    /// Global roots (always reachable)
    global_roots: HandleSet,

    /// Thread-local roots by thread ID
    thread_roots: HashMap<ThreadId, HandleSet>,

    /// Temporary roots (for intermediate operations)
    temp_roots: HandleSet,

    /// Stack roots (current call frames)
    stack_roots: Vec<HandleSet>,

    /// Named roots for debugging and external references
    named_roots: HashMap<String, GcHandle>,
}

impl RootSet {
    /// Create a new root set
    pub fn new() -> Self {
        Self {
            global_roots: HandleSet::new(),
            thread_roots: HashMap::new(),
            temp_roots: HandleSet::new(),
            stack_roots: Vec::new(),
            named_roots: HashMap::new(),
        }
    }

    /// Add a global root
    pub fn add_global_root(&mut self, handle: GcHandle) {
        self.global_roots.insert(handle);
    }

    /// Remove a global root
    pub fn remove_global_root(&mut self, handle: &GcHandle) -> bool {
        self.global_roots.remove(handle)
    }

    /// Add a root (defaults to current thread)
    pub fn add_root(&mut self, handle: GcHandle) {
        let thread_id = std::thread::current().id();
        self.thread_roots
            .entry(thread_id)
            .or_insert_with(HandleSet::new)
            .insert(handle);
    }

    /// Remove a root
    pub fn remove_root(&mut self, handle: &GcHandle) -> bool {
        let thread_id = std::thread::current().id();
        if let Some(thread_set) = self.thread_roots.get_mut(&thread_id) {
            thread_set.remove(handle)
        } else {
            false
        }
    }

    /// Add a temporary root (cleared after each collection)
    pub fn add_temp_root(&mut self, handle: GcHandle) {
        self.temp_roots.insert(handle);
    }

    /// Clear all temporary roots
    pub fn clear_temp_roots(&mut self) {
        self.temp_roots.clear();
    }

    /// Push a new stack frame
    pub fn push_stack_frame(&mut self) -> usize {
        self.stack_roots.push(HandleSet::new());
        self.stack_roots.len() - 1
    }

    /// Pop the top stack frame
    pub fn pop_stack_frame(&mut self) -> Option<HandleSet> {
        self.stack_roots.pop()
    }

    /// Add a root to the current stack frame
    pub fn add_stack_root(&mut self, handle: GcHandle) -> Result<()> {
        if let Some(current_frame) = self.stack_roots.last_mut() {
            current_frame.insert(handle);
            Ok(())
        } else {
            Err(VeldError::RuntimeError("No active stack frame".to_string()))
        }
    }

    /// Add a named root for debugging
    pub fn add_named_root(&mut self, name: String, handle: GcHandle) {
        self.named_roots.insert(name, handle);
    }

    /// Remove a named root
    pub fn remove_named_root(&mut self, name: &str) -> Option<GcHandle> {
        self.named_roots.remove(name)
    }

    /// Get a named root
    pub fn get_named_root(&self, name: &str) -> Option<&GcHandle> {
        self.named_roots.get(name)
    }

    /// Get all roots across all categories
    pub fn roots(&self) -> impl Iterator<Item = &GcHandle> {
        self.global_roots
            .handles()
            .chain(self.thread_roots.values().flat_map(|set| set.handles()))
            .chain(self.temp_roots.handles())
            .chain(self.stack_roots.iter().flat_map(|frame| frame.handles()))
            .chain(self.named_roots.values())
    }

    /// Get all roots as a vector
    pub fn all_roots(&self) -> Vec<GcHandle> {
        self.roots().cloned().collect()
    }

    /// Get roots for a specific thread
    pub fn thread_roots(&self, thread_id: ThreadId) -> impl Iterator<Item = &GcHandle> {
        self.thread_roots
            .get(&thread_id)
            .map(|set| set.handles())
            .into_iter()
            .flatten()
    }

    /// Get global roots
    pub fn global_roots(&self) -> impl Iterator<Item = &GcHandle> {
        self.global_roots.handles()
    }

    /// Get stack roots (all frames)
    pub fn stack_roots(&self) -> impl Iterator<Item = &GcHandle> {
        self.stack_roots.iter().flat_map(|frame| frame.handles())
    }

    /// Get the current stack frame
    pub fn current_frame(&self) -> Option<&HandleSet> {
        self.stack_roots.last()
    }

    /// Get the current stack frame mutably
    pub fn current_frame_mut(&mut self) -> Option<&mut HandleSet> {
        self.stack_roots.last_mut()
    }

    /// Check if a handle is in the root set
    pub fn contains(&self, handle: &GcHandle) -> bool {
        self.roots().any(|root| root == handle)
    }

    /// Get statistics about the root set
    pub fn statistics(&self) -> RootSetStatistics {
        RootSetStatistics {
            global_count: self.global_roots.len(),
            thread_count: self.thread_roots.values().map(|s| s.len()).sum(),
            temp_count: self.temp_roots.len(),
            stack_count: self.stack_roots.iter().map(|f| f.len()).sum(),
            named_count: self.named_roots.len(),
            total_count: self.roots().count(),
            active_threads: self.thread_roots.len(),
            stack_depth: self.stack_roots.len(),
        }
    }

    /// Clean up dead threads
    pub fn cleanup_dead_threads(&mut self) {
        // This is a simplified cleanup - in practice you'd need a way to detect dead threads
        // For now, we keep all thread roots since we can't easily detect thread liveness
    }

    /// Merge another root set into this one
    pub fn merge(&mut self, other: RootSet) {
        // Merge global roots
        for handle in other.global_roots.into_iter() {
            self.global_roots.insert(handle);
        }

        // Merge thread roots
        for (thread_id, handle_set) in other.thread_roots {
            let target_set = self
                .thread_roots
                .entry(thread_id)
                .or_insert_with(HandleSet::new);
            for handle in handle_set.into_iter() {
                target_set.insert(handle);
            }
        }

        // Merge temp roots
        for handle in other.temp_roots.into_iter() {
            self.temp_roots.insert(handle);
        }

        // Merge named roots
        for (name, handle) in other.named_roots {
            self.named_roots.insert(name, handle);
        }

        // Stack roots are not merged as they're context-specific
    }

    /// Filter roots by a predicate (removes roots that don't match)
    pub fn filter<F>(&mut self, mut predicate: F)
    where
        F: FnMut(&GcHandle) -> bool,
    {
        self.global_roots.filter(&mut predicate);

        for handle_set in self.thread_roots.values_mut() {
            handle_set.filter(&mut predicate);
        }

        self.temp_roots.filter(&mut predicate);

        for frame in &mut self.stack_roots {
            frame.filter(&mut predicate);
        }

        self.named_roots.retain(|_, handle| predicate(handle));
    }

    /// Clear all roots (dangerous - use with caution)
    pub fn clear_all(&mut self) {
        self.global_roots.clear();
        self.thread_roots.clear();
        self.temp_roots.clear();
        self.stack_roots.clear();
        self.named_roots.clear();
    }
}

impl Default for RootSet {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about the root set
#[derive(Debug, Clone)]
pub struct RootSetStatistics {
    pub global_count: usize,
    pub thread_count: usize,
    pub temp_count: usize,
    pub stack_count: usize,
    pub named_count: usize,
    pub total_count: usize,
    pub active_threads: usize,
    pub stack_depth: usize,
}

impl std::fmt::Display for RootSetStatistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RootSet(total: {}, global: {}, thread: {}, temp: {}, stack: {}, named: {}, threads: {}, depth: {})",
            self.total_count,
            self.global_count,
            self.thread_count,
            self.temp_count,
            self.stack_count,
            self.named_count,
            self.active_threads,
            self.stack_depth
        )
    }
}

/// Scoped root guard for automatic cleanup
pub struct ScopedRoot {
    handle: Option<GcHandle>,
    root_set: Arc<RwLock<RootSet>>,
}

impl ScopedRoot {
    /// Create a new scoped root
    pub fn new(handle: GcHandle, root_set: Arc<RwLock<RootSet>>) -> Self {
        // Add to root set
        if let Ok(mut roots) = root_set.write() {
            roots.add_temp_root(handle.clone());
        }

        Self {
            handle: Some(handle),
            root_set,
        }
    }

    /// Get the handle
    pub fn handle(&self) -> Option<&GcHandle> {
        self.handle.as_ref()
    }

    /// Take ownership of the handle (prevents automatic cleanup)
    pub fn take(mut self) -> Option<GcHandle> {
        self.handle.take()
    }
}

impl Drop for ScopedRoot {
    fn drop(&mut self) {
        if let Some(handle) = self.handle.take() {
            if let Ok(mut roots) = self.root_set.write() {
                roots.remove_root(&handle);
            }
        }
    }
}

/// Stack frame guard for automatic frame management
pub struct StackFrameGuard {
    root_set: Arc<RwLock<RootSet>>,
    frame_id: Option<usize>,
}

impl StackFrameGuard {
    /// Create a new stack frame
    pub fn new(root_set: Arc<RwLock<RootSet>>) -> Self {
        let frame_id = if let Ok(mut roots) = root_set.write() {
            Some(roots.push_stack_frame())
        } else {
            None
        };

        Self { root_set, frame_id }
    }

    /// Add a root to the current frame
    pub fn add_root(&self, handle: GcHandle) -> Result<()> {
        if let Ok(mut roots) = self.root_set.write() {
            roots.add_stack_root(handle)
        } else {
            Err(VeldError::RuntimeError(
                "Cannot access root set".to_string(),
            ))
        }
    }
}

impl Drop for StackFrameGuard {
    fn drop(&mut self) {
        if self.frame_id.is_some() {
            if let Ok(mut roots) = self.root_set.write() {
                roots.pop_stack_frame();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::handle::{GcHandle, Generation};

    fn create_test_handle(id: u64) -> GcHandle {
        // Use a fixed generation based on the ID to ensure consistency
        // This ensures that create_test_handle(1) always returns the same handle
        GcHandle::new(id, Generation(1000 + id))
    }

    #[test]
    fn test_root_set_basic() {
        let mut root_set = RootSet::new();
        let handle = create_test_handle(1);

        root_set.add_root(handle.clone());
        assert!(root_set.contains(&handle));

        assert!(root_set.remove_root(&handle));
        assert!(!root_set.contains(&handle));
    }

    #[test]
    fn test_global_roots() {
        let mut root_set = RootSet::new();
        let handle = create_test_handle(1);

        root_set.add_global_root(handle.clone());
        assert!(root_set.contains(&handle));

        let globals: Vec<_> = root_set.global_roots().cloned().collect();
        assert_eq!(globals.len(), 1);
        assert_eq!(globals[0], handle);
    }

    #[test]
    fn test_stack_frames() {
        let mut root_set = RootSet::new();

        // Push frame and add root
        let _frame_id = root_set.push_stack_frame();
        let handle = create_test_handle(1);
        root_set.add_stack_root(handle.clone()).unwrap();

        assert!(root_set.contains(&handle));
        assert_eq!(root_set.current_frame().unwrap().len(), 1);

        // Pop frame
        let popped = root_set.pop_stack_frame().unwrap();
        assert_eq!(popped.len(), 1);
        assert!(!root_set.contains(&handle)); // Should be gone after pop
    }

    #[test]
    fn test_named_roots() {
        let mut root_set = RootSet::new();
        let handle = create_test_handle(1);

        root_set.add_named_root("test".to_string(), handle.clone());
        assert_eq!(root_set.get_named_root("test"), Some(&handle));

        let removed = root_set.remove_named_root("test");
        assert_eq!(removed, Some(handle));
        assert_eq!(root_set.get_named_root("test"), None);
    }

    #[test]
    fn test_temp_roots() {
        let mut root_set = RootSet::new();
        let handle = create_test_handle(1);

        root_set.add_temp_root(handle.clone());
        assert!(root_set.contains(&handle));

        root_set.clear_temp_roots();
        assert!(!root_set.contains(&handle));
    }

    #[test]
    fn test_statistics() {
        let mut root_set = RootSet::new();

        root_set.add_global_root(create_test_handle(1));
        root_set.add_root(create_test_handle(2));
        root_set.add_temp_root(create_test_handle(3));
        root_set.push_stack_frame();
        root_set.add_stack_root(create_test_handle(4)).unwrap();
        root_set.add_named_root("test".to_string(), create_test_handle(5));

        let stats = root_set.statistics();
        assert_eq!(stats.global_count, 1);
        assert_eq!(stats.thread_count, 1);
        assert_eq!(stats.temp_count, 1);
        assert_eq!(stats.stack_count, 1);
        assert_eq!(stats.named_count, 1);
        assert_eq!(stats.total_count, 5);
        assert_eq!(stats.stack_depth, 1);
    }

    #[test]
    fn test_filter_roots() {
        let mut root_set = RootSet::new();

        root_set.add_global_root(create_test_handle(1));
        root_set.add_global_root(create_test_handle(2));
        root_set.add_global_root(create_test_handle(3));

        // Filter to keep only even object IDs
        root_set.filter(|handle| handle.object_id() % 2 == 0);

        let remaining: Vec<_> = root_set.global_roots().collect();
        assert_eq!(remaining.len(), 1);
        assert_eq!(remaining[0].object_id(), 2);
    }

    #[test]
    fn test_merge_root_sets() {
        let mut root_set1 = RootSet::new();
        let mut root_set2 = RootSet::new();

        root_set1.add_global_root(create_test_handle(1));
        root_set2.add_global_root(create_test_handle(2));
        root_set2.add_named_root("test".to_string(), create_test_handle(3));

        root_set1.merge(root_set2);

        assert_eq!(root_set1.statistics().global_count, 2);
        assert_eq!(root_set1.statistics().named_count, 1);
        assert!(root_set1.contains(&create_test_handle(1)));
        assert!(root_set1.contains(&create_test_handle(2)));
        assert_eq!(
            root_set1.get_named_root("test"),
            Some(&create_test_handle(3))
        );
    }
}
