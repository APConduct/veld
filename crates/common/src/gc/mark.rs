//! Marking Phase Implementation for Garbage Collection
//!
//! The marking phase traverses all reachable objects from the root set
//! and marks them as live. This is the first phase of mark-and-sweep collection.

use std::collections::{HashMap, HashSet, VecDeque};
use std::time::{Duration, Instant};

use super::super::value::Value;
use super::allocator::{GcAllocator, ObjectGeneration};

use super::root_set::RootSet;
use veld_error::{Result, VeldError};

/// State of the marking phase
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MarkState {
    /// Not started
    NotStarted,
    /// Currently marking
    InProgress,
    /// Marking completed
    Completed,
    /// Marking failed
    Failed,
}

/// Marking phase implementation
#[derive(Debug)]
pub struct MarkPhase {
    /// Current state
    state: MarkState,
    /// Objects to be marked
    mark_queue: VecDeque<u64>,
    /// Already marked objects
    marked_objects: HashSet<u64>,
    /// Mark statistics
    statistics: MarkStatistics,
    /// Start time of current marking
    start_time: Option<Instant>,
}

/// Statistics for marking phase
#[derive(Debug, Clone, Default)]
pub struct MarkStatistics {
    /// Total objects marked
    pub objects_marked: usize,
    /// Total references traversed
    pub references_traversed: usize,
    /// Time taken for marking
    pub duration: Duration,
    /// Mark queue peak size
    pub peak_queue_size: usize,
    /// Number of marking iterations
    pub iterations: usize,
}

impl MarkPhase {
    /// Create a new marking phase
    pub fn new() -> Self {
        Self {
            state: MarkState::NotStarted,
            mark_queue: VecDeque::new(),
            marked_objects: HashSet::new(),
            statistics: MarkStatistics::default(),
            start_time: None,
        }
    }

    /// Start marking from the root set
    pub fn start_marking(&mut self, roots: &RootSet, allocator: &GcAllocator) -> Result<()> {
        self.state = MarkState::InProgress;
        self.start_time = Some(Instant::now());
        self.mark_queue.clear();
        self.marked_objects.clear();
        self.statistics = MarkStatistics::default();

        // Add all roots to the mark queue
        for root in roots.roots() {
            if allocator.is_alive(root) {
                self.mark_queue.push_back(root.object_id());
                tracing::trace!("Added root object {} to mark queue", root.object_id());
            }
        }

        self.statistics.peak_queue_size = self.mark_queue.len();

        tracing::debug!(
            "Started marking phase with {} root objects",
            self.mark_queue.len()
        );

        Ok(())
    }

    /// Perform complete marking
    pub fn mark_all(&mut self, allocator: &GcAllocator) -> Result<MarkStatistics> {
        if self.state != MarkState::InProgress {
            return Err(VeldError::RuntimeError(
                "Marking phase not started".to_string(),
            ));
        }

        while !self.mark_queue.is_empty() {
            self.mark_iteration(allocator)?;
        }

        self.complete_marking()
    }

    /// Perform incremental marking (mark up to a certain number of objects)
    pub fn mark_incremental(
        &mut self,
        allocator: &GcAllocator,
        max_objects: usize,
    ) -> Result<bool> {
        if self.state != MarkState::InProgress {
            return Err(VeldError::RuntimeError(
                "Marking phase not started".to_string(),
            ));
        }

        let mut marked_count = 0;
        while !self.mark_queue.is_empty() && marked_count < max_objects {
            if let Some(object_id) = self.mark_queue.pop_front() {
                self.mark_object(object_id, allocator)?;
                marked_count += 1;
            }
        }

        // Update queue size tracking
        self.statistics.peak_queue_size =
            self.statistics.peak_queue_size.max(self.mark_queue.len());

        // Check if marking is complete
        if self.mark_queue.is_empty() {
            self.complete_marking()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Check if an object is marked
    pub fn is_marked(&self, object_id: u64) -> bool {
        self.marked_objects.contains(&object_id)
    }

    /// Get all marked objects
    pub fn marked_objects(&self) -> &HashSet<u64> {
        &self.marked_objects
    }

    /// Get current state
    pub fn state(&self) -> MarkState {
        self.state
    }

    /// Get current statistics
    pub fn statistics(&self) -> &MarkStatistics {
        &self.statistics
    }

    /// Reset for next collection
    pub fn reset(&mut self) {
        self.state = MarkState::NotStarted;
        self.mark_queue.clear();
        self.marked_objects.clear();
        self.statistics = MarkStatistics::default();
        self.start_time = None;
    }

    // Private methods

    /// Perform one iteration of marking
    fn mark_iteration(&mut self, allocator: &GcAllocator) -> Result<()> {
        let batch_size = 100; // Mark objects in batches for better performance
        let mut marked_in_batch = 0;

        while !self.mark_queue.is_empty() && marked_in_batch < batch_size {
            if let Some(object_id) = self.mark_queue.pop_front() {
                self.mark_object(object_id, allocator)?;
                marked_in_batch += 1;
            }
        }

        self.statistics.iterations += 1;
        Ok(())
    }

    /// Mark a single object and add its references to the queue
    fn mark_object(&mut self, object_id: u64, allocator: &GcAllocator) -> Result<()> {
        // Skip if already marked
        if self.marked_objects.contains(&object_id) {
            return Ok(());
        }

        // Mark the object
        self.marked_objects.insert(object_id);
        self.statistics.objects_marked += 1;

        tracing::trace!("Marked object {}", object_id);

        // Find the object and add its references to the mark queue
        if let Some((_, object, _)) = allocator.all_objects().find(|(id, _, _)| **id == object_id) {
            // Add direct references from the object
            for reference in &object.references {
                if allocator.is_alive(reference)
                    && !self.marked_objects.contains(&reference.object_id())
                {
                    self.mark_queue.push_back(reference.object_id());
                    self.statistics.references_traversed += 1;
                }
            }

            // Add references from the value itself
            self.extract_value_references(&object.value, allocator)?;
        }

        // Update peak queue size
        self.statistics.peak_queue_size =
            self.statistics.peak_queue_size.max(self.mark_queue.len());

        Ok(())
    }

    /// Extract references from a Veld value and add them to mark queue
    fn extract_value_references(&mut self, value: &Value, allocator: &GcAllocator) -> Result<()> {
        match value {
            Value::Array(elements) => {
                for element in elements {
                    self.extract_value_references(element, allocator)?;
                }
            }
            Value::Tuple(elements) => {
                for element in elements {
                    self.extract_value_references(element, allocator)?;
                }
            }
            Value::Struct { fields, .. } => {
                for field_value in fields.values() {
                    self.extract_value_references(field_value, allocator)?;
                }
            }
            Value::Enum { fields, .. } => {
                for field_value in fields {
                    self.extract_value_references(field_value, allocator)?;
                }
            }
            Value::Function { captured_vars, .. } => {
                for captured_value in captured_vars.values() {
                    self.extract_value_references(captured_value, allocator)?;
                }
            }
            Value::Return(inner) => {
                self.extract_value_references(inner, allocator)?;
            }
            Value::Record(fields) => {
                for field_value in fields.values() {
                    self.extract_value_references(field_value, allocator)?;
                }
            }
            // Primitive values don't contain references
            Value::Integer(_)
            | Value::Float(_)
            | Value::String(_)
            | Value::Boolean(_)
            | Value::Char(_)
            | Value::Unit
            | Value::Break
            | Value::Continue => {}

            // Module and type objects might contain references
            Value::Module(module) => {
                // TODO: Extract references from module exports once module variable resolution is implemented
                // For now, skip module variable GC marking since the module system stores variables
                // by index rather than direct values, and variable resolution is not yet complete
                let _ = module; // Silence unused variable warning
            }

            // Type objects and enum/struct types
            Value::EnumType { methods, .. } | Value::StructType { methods, .. } => {
                if let Some(method_map) = methods {
                    for method in method_map.values() {
                        self.extract_value_references(method, allocator)?;
                    }
                }
            }

            // Handle numeric values
            Value::Numeric(numeric_val) => {
                // Numeric values are typically primitives, but check for any embedded references
                // In the current implementation, NumericValue doesn't contain GC references
            }
            Value::CompiledFunction { chunk, arity, name } => {
                todo!("Implement reference extraction for compiled functions")
            }
        }

        Ok(())
    }

    /// Complete the marking phase
    fn complete_marking(&mut self) -> Result<MarkStatistics> {
        if let Some(start_time) = self.start_time {
            self.statistics.duration = start_time.elapsed();
        }

        self.state = MarkState::Completed;

        tracing::info!(
            "Marking completed: {} objects marked, {} references traversed in {:?}",
            self.statistics.objects_marked,
            self.statistics.references_traversed,
            self.statistics.duration
        );

        Ok(self.statistics.clone())
    }
}

impl Default for MarkPhase {
    fn default() -> Self {
        Self::new()
    }
}

/// Mark-and-trace utilities
pub struct MarkTracer {
    /// Objects being traced
    trace_stack: Vec<u64>,
    /// Visited objects to prevent cycles
    visited: HashSet<u64>,
}

impl MarkTracer {
    /// Create a new mark tracer
    pub fn new() -> Self {
        Self {
            trace_stack: Vec::new(),
            visited: HashSet::new(),
        }
    }

    /// Trace from a root object
    pub fn trace_from_root(&mut self, root_id: u64, allocator: &GcAllocator) -> Result<Vec<u64>> {
        self.trace_stack.clear();
        self.visited.clear();

        self.trace_stack.push(root_id);
        let mut reachable = Vec::new();

        while let Some(object_id) = self.trace_stack.pop() {
            if self.visited.contains(&object_id) {
                continue;
            }

            self.visited.insert(object_id);
            reachable.push(object_id);

            // Add references to trace stack
            if let Some((_, object, _)) =
                allocator.all_objects().find(|(id, _, _)| **id == object_id)
            {
                for reference in &object.references {
                    if allocator.is_alive(reference)
                        && !self.visited.contains(&reference.object_id())
                    {
                        self.trace_stack.push(reference.object_id());
                    }
                }
            }
        }

        Ok(reachable)
    }

    /// Check if an object is reachable from any root
    pub fn is_reachable_from(
        &mut self,
        target_id: u64,
        root_ids: &[u64],
        allocator: &GcAllocator,
    ) -> Result<bool> {
        for &root_id in root_ids {
            let reachable = self.trace_from_root(root_id, allocator)?;
            if reachable.contains(&target_id) {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl Default for MarkTracer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{GcConfig, allocator::GcAllocator, root_set::RootSet};

    #[test]
    fn test_mark_phase_creation() {
        let mark_phase = MarkPhase::new();
        assert_eq!(mark_phase.state(), MarkState::NotStarted);
        assert_eq!(mark_phase.marked_objects().len(), 0);
    }

    #[test]
    fn test_mark_phase_start() {
        let mut mark_phase = MarkPhase::new();
        let roots = RootSet::new();
        let config = GcConfig::default();
        let allocator = GcAllocator::new(&config);

        mark_phase.start_marking(&roots, &allocator).unwrap();
        assert_eq!(mark_phase.state(), MarkState::InProgress);
    }

    #[test]
    fn test_mark_statistics() {
        let mark_phase = MarkPhase::new();
        let stats = mark_phase.statistics();

        assert_eq!(stats.objects_marked, 0);
        assert_eq!(stats.references_traversed, 0);
        assert_eq!(stats.duration, Duration::new(0, 0));
    }

    #[test]
    fn test_mark_tracer() {
        let mut tracer = MarkTracer::new();
        let config = GcConfig::default();
        let allocator = GcAllocator::new(&config);

        // Test with empty allocator
        let reachable = tracer.trace_from_root(1, &allocator).unwrap();
        assert!(reachable.is_empty() || reachable.len() == 1); // May include the root even if not found
    }

    #[test]
    fn test_mark_reset() {
        let mut mark_phase = MarkPhase::new();

        // Simulate some marking state
        mark_phase.state = MarkState::InProgress;
        mark_phase.marked_objects.insert(1);
        mark_phase.statistics.objects_marked = 1;

        mark_phase.reset();

        assert_eq!(mark_phase.state(), MarkState::NotStarted);
        assert!(mark_phase.marked_objects().is_empty());
        assert_eq!(mark_phase.statistics().objects_marked, 0);
    }
}
