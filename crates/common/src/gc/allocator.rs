//! Memory Allocator for Garbage-Collected Objects
//!
//! This module implements a generational allocator that efficiently manages
//! memory for Veld values with different allocation patterns and lifetimes.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use super::super::value::Value;
use super::GcConfig;
use super::handle::{GcHandle, Generation, HandlePool};
use veld_error::{Result, VeldError};

/// Unique object identifier counter
static OBJECT_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Metadata for a garbage-collected object
#[derive(Debug, Clone)]
pub struct ObjectMetadata {
    /// When this object was allocated
    pub allocated_at: Instant,
    /// How many GC cycles this object has survived
    pub survival_count: u32,
    /// Size of the object in bytes (estimated)
    pub size: usize,
    /// Whether this object is currently marked for collection
    pub marked: bool,
    /// Generation this object belongs to
    pub generation: ObjectGeneration,
    /// Whether this object has been moved (for compacting GC)
    pub moved: bool,
    /// Forwarding pointer for moved objects
    pub forwarding_address: Option<GcHandle>,
}

impl ObjectMetadata {
    fn new(size: usize) -> Self {
        Self {
            allocated_at: Instant::now(),
            survival_count: 0,
            size,
            marked: false,
            generation: ObjectGeneration::Young,
            moved: false,
            forwarding_address: None,
        }
    }

    /// Mark this object as surviving another GC cycle
    pub fn survive_collection(&mut self) {
        self.survival_count += 1;
        self.marked = false; // Reset mark for next cycle
    }

    /// Check if this object should be promoted to old generation
    pub fn should_promote(&self, threshold: u32) -> bool {
        self.survival_count >= threshold && self.generation == ObjectGeneration::Young
    }

    /// Promote this object to old generation
    pub fn promote(&mut self) {
        self.generation = ObjectGeneration::Old;
    }
}

/// Object generation for generational GC
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectGeneration {
    /// Young generation - most objects die here
    Young,
    /// Old generation - long-lived objects
    Old,
    /// Permanent generation - never collected (builtins, etc.)
    Permanent,
}

/// A garbage-collected object
#[derive(Debug, Clone)]
pub struct GcObject {
    /// The actual Veld value
    pub value: Value,
    /// Object metadata
    pub metadata: ObjectMetadata,
    /// References to other GC objects
    pub references: Vec<GcHandle>,
}

impl GcObject {
    fn new(value: Value) -> Self {
        let size = estimate_value_size(&value);
        let references = extract_references(&value);

        Self {
            value,
            metadata: ObjectMetadata::new(size),
            references,
        }
    }

    /// Update the value and recompute references
    pub fn update_value(&mut self, new_value: Value) {
        self.value = new_value;
        self.references = extract_references(&self.value);
        self.metadata.size = estimate_value_size(&self.value);
    }

    /// Check if this object references another object
    pub fn references(&self, handle: &GcHandle) -> bool {
        self.references.contains(handle)
    }
}

/// Memory region for a generation
#[derive(Debug)]
pub struct GenerationRegion {
    /// Objects in this generation
    objects: HashMap<u64, GcObject>,
    /// Total allocated bytes in this generation
    allocated_bytes: usize,
    /// Maximum size before triggering collection
    max_size: usize,
    /// Generation type
    generation: ObjectGeneration,
}

impl GenerationRegion {
    fn new(generation: ObjectGeneration, max_size: usize) -> Self {
        Self {
            objects: HashMap::new(),
            allocated_bytes: 0,
            max_size,
            generation,
        }
    }

    /// Add an object to this generation
    fn add_object(&mut self, object_id: u64, object: GcObject) {
        self.allocated_bytes += object.metadata.size;
        self.objects.insert(object_id, object);
    }

    /// Remove an object from this generation
    fn remove_object(&mut self, object_id: u64) -> Option<GcObject> {
        if let Some(object) = self.objects.remove(&object_id) {
            self.allocated_bytes = self.allocated_bytes.saturating_sub(object.metadata.size);
            Some(object)
        } else {
            None
        }
    }

    /// Get an object by ID
    fn get_object(&self, object_id: u64) -> Option<&GcObject> {
        self.objects.get(&object_id)
    }

    /// Get a mutable reference to an object
    fn get_object_mut(&mut self, object_id: u64) -> Option<&mut GcObject> {
        self.objects.get_mut(&object_id)
    }

    /// Check if this generation should trigger a collection
    fn should_collect(&self, threshold: f64) -> bool {
        self.allocated_bytes as f64 >= (self.max_size as f64 * threshold)
    }

    /// Get all object IDs in this generation
    fn object_ids(&self) -> Vec<u64> {
        self.objects.keys().copied().collect()
    }

    /// Get all objects in this generation
    fn objects(&self) -> impl Iterator<Item = (&u64, &GcObject)> {
        self.objects.iter()
    }

    /// Get all objects mutably
    fn objects_mut(&mut self) -> impl Iterator<Item = (&u64, &mut GcObject)> {
        self.objects.iter_mut()
    }

    /// Clear all objects (used after collection)
    fn clear(&mut self) {
        self.objects.clear();
        self.allocated_bytes = 0;
    }

    /// Get statistics for this generation
    fn stats(&self) -> GenerationStats {
        GenerationStats {
            object_count: self.objects.len(),
            allocated_bytes: self.allocated_bytes,
            max_size: self.max_size,
            generation: self.generation,
        }
    }
}

/// Statistics for a generation region
#[derive(Debug, Clone)]
pub struct GenerationStats {
    pub object_count: usize,
    pub allocated_bytes: usize,
    pub max_size: usize,
    pub generation: ObjectGeneration,
}

/// Main garbage collector allocator
pub struct GcAllocator {
    /// Handle management
    handle_pool: HandlePool,

    /// Young generation region
    young_generation: GenerationRegion,

    /// Old generation region
    old_generation: GenerationRegion,

    /// Permanent generation region
    permanent_generation: GenerationRegion,

    /// Lookup table for finding objects across generations
    object_locations: HashMap<u64, ObjectGeneration>,

    /// Free list for reusing memory
    free_list: VecDeque<u64>,

    /// Total heap size across all generations
    total_heap_size: usize,
}

impl GcAllocator {
    /// Create a new allocator with the given configuration
    pub fn new(config: &GcConfig) -> Self {
        let young_size = config.initial_heap_size / 3;
        let old_size = config.initial_heap_size * 2 / 3;
        let permanent_size = config.initial_heap_size / 10;

        Self {
            handle_pool: HandlePool::new(),
            young_generation: GenerationRegion::new(ObjectGeneration::Young, young_size),
            old_generation: GenerationRegion::new(ObjectGeneration::Old, old_size),
            permanent_generation: GenerationRegion::new(
                ObjectGeneration::Permanent,
                permanent_size,
            ),
            object_locations: HashMap::new(),
            free_list: VecDeque::new(),
            total_heap_size: 0,
        }
    }

    /// Allocate a new object in the appropriate generation
    pub fn allocate(&mut self, value: Value) -> Result<GcHandle> {
        let object = GcObject::new(value);
        let handle = self.handle_pool.allocate();

        // Most objects start in young generation
        let generation = ObjectGeneration::Young;

        match generation {
            ObjectGeneration::Young => {
                self.young_generation.add_object(handle.object_id(), object);
            }
            ObjectGeneration::Old => {
                self.old_generation.add_object(handle.object_id(), object);
            }
            ObjectGeneration::Permanent => {
                self.permanent_generation
                    .add_object(handle.object_id(), object);
            }
        }

        self.object_locations.insert(handle.object_id(), generation);
        self.update_heap_size();

        Ok(handle)
    }

    /// Allocate directly in old generation (for large objects)
    pub fn allocate_old(&mut self, value: Value) -> Result<GcHandle> {
        let object = GcObject::new(value);
        let handle = self.handle_pool.allocate();

        self.old_generation.add_object(handle.object_id(), object);
        self.object_locations
            .insert(handle.object_id(), ObjectGeneration::Old);
        self.update_heap_size();

        Ok(handle)
    }

    /// Allocate in permanent generation (never collected)
    pub fn allocate_permanent(&mut self, value: Value) -> Result<GcHandle> {
        let object = GcObject::new(value);
        let handle = self.handle_pool.allocate();

        self.permanent_generation
            .add_object(handle.object_id(), object);
        self.object_locations
            .insert(handle.object_id(), ObjectGeneration::Permanent);
        self.update_heap_size();

        Ok(handle)
    }

    /// Get the value for a handle
    pub fn get_value(&self, handle: &GcHandle) -> Option<&Value> {
        if !self.handle_pool.is_valid(handle) {
            return None;
        }

        let generation = self.object_locations.get(&handle.object_id())?;

        let object = match generation {
            ObjectGeneration::Young => self.young_generation.get_object(handle.object_id()),
            ObjectGeneration::Old => self.old_generation.get_object(handle.object_id()),
            ObjectGeneration::Permanent => self.permanent_generation.get_object(handle.object_id()),
        }?;

        Some(&object.value)
    }

    /// Update the value for a handle
    pub fn update_value(&mut self, handle: &GcHandle, new_value: Value) -> Result<()> {
        if !self.handle_pool.is_valid(handle) {
            return Err(VeldError::RuntimeError("Invalid handle".to_string()));
        }

        let generation = self
            .object_locations
            .get(&handle.object_id())
            .ok_or_else(|| VeldError::RuntimeError("Object not found".to_string()))?;

        let object = match generation {
            ObjectGeneration::Young => self.young_generation.get_object_mut(handle.object_id()),
            ObjectGeneration::Old => self.old_generation.get_object_mut(handle.object_id()),
            ObjectGeneration::Permanent => {
                self.permanent_generation.get_object_mut(handle.object_id())
            }
        }
        .ok_or_else(|| VeldError::RuntimeError("Object not found".to_string()))?;

        let old_size = object.metadata.size;
        object.update_value(new_value);

        // Update heap size tracking
        self.update_heap_size();

        Ok(())
    }

    /// Check if an object is still alive
    pub fn is_alive(&self, handle: &GcHandle) -> bool {
        self.handle_pool.is_valid(handle) && self.object_locations.contains_key(&handle.object_id())
    }

    /// Get metadata for an object
    pub fn get_metadata(&self, handle: &GcHandle) -> Option<&ObjectMetadata> {
        if !self.handle_pool.is_valid(handle) {
            return None;
        }

        let generation = self.object_locations.get(&handle.object_id())?;

        let object = match generation {
            ObjectGeneration::Young => self.young_generation.get_object(handle.object_id()),
            ObjectGeneration::Old => self.old_generation.get_object(handle.object_id()),
            ObjectGeneration::Permanent => self.permanent_generation.get_object(handle.object_id()),
        }?;

        Some(&object.metadata)
    }

    /// Remove an object from the heap
    pub fn deallocate(&mut self, handle: &GcHandle) -> Result<()> {
        if !self.handle_pool.is_valid(handle) {
            return Ok(()); // Already deallocated
        }

        let generation = self
            .object_locations
            .remove(&handle.object_id())
            .ok_or_else(|| VeldError::RuntimeError("Object not found".to_string()))?;

        match generation {
            ObjectGeneration::Young => {
                self.young_generation.remove_object(handle.object_id());
            }
            ObjectGeneration::Old => {
                self.old_generation.remove_object(handle.object_id());
            }
            ObjectGeneration::Permanent => {
                self.permanent_generation.remove_object(handle.object_id());
            }
        }

        self.handle_pool.deallocate(handle);
        self.update_heap_size();

        Ok(())
    }

    /// Promote an object from young to old generation
    pub fn promote_object(&mut self, handle: &GcHandle) -> Result<()> {
        if !self.handle_pool.is_valid(handle) {
            return Err(VeldError::RuntimeError("Invalid handle".to_string()));
        }

        let current_gen = self
            .object_locations
            .get(&handle.object_id())
            .ok_or_else(|| VeldError::RuntimeError("Object not found".to_string()))?;

        if *current_gen != ObjectGeneration::Young {
            return Ok(()); // Already promoted or permanent
        }

        // Move object from young to old generation
        if let Some(mut object) = self.young_generation.remove_object(handle.object_id()) {
            object.metadata.promote();
            self.old_generation.add_object(handle.object_id(), object);
            self.object_locations
                .insert(handle.object_id(), ObjectGeneration::Old);
        }

        Ok(())
    }

    /// Check if allocation should trigger a collection
    pub fn should_collect(&self, config: &GcConfig) -> bool {
        self.young_generation
            .should_collect(config.young_gen_threshold)
            || (config.max_heap_size > 0 && self.total_heap_size >= config.max_heap_size)
    }

    /// Get all objects in the young generation
    pub fn young_objects(&self) -> impl Iterator<Item = (&u64, &GcObject)> {
        self.young_generation.objects()
    }

    /// Get all objects in the old generation
    pub fn old_objects(&self) -> impl Iterator<Item = (&u64, &GcObject)> {
        self.old_generation.objects()
    }

    /// Get all objects in the permanent generation
    pub fn permanent_objects(&self) -> impl Iterator<Item = (&u64, &GcObject)> {
        self.permanent_generation.objects()
    }

    /// Get all objects across all generations
    pub fn all_objects(&self) -> impl Iterator<Item = (&u64, &GcObject, ObjectGeneration)> {
        self.young_generation
            .objects()
            .map(|(id, obj)| (id, obj, ObjectGeneration::Young))
            .chain(
                self.old_generation
                    .objects()
                    .map(|(id, obj)| (id, obj, ObjectGeneration::Old)),
            )
            .chain(
                self.permanent_generation
                    .objects()
                    .map(|(id, obj)| (id, obj, ObjectGeneration::Permanent)),
            )
    }

    /// Get current heap size
    pub fn heap_size(&self) -> usize {
        self.total_heap_size
    }

    /// Get number of allocated objects
    pub fn object_count(&self) -> usize {
        self.object_locations.len()
    }

    /// Get statistics for all generations
    pub fn generation_stats(&self) -> Vec<GenerationStats> {
        vec![
            self.young_generation.stats(),
            self.old_generation.stats(),
            self.permanent_generation.stats(),
        ]
    }

    /// Update total heap size
    fn update_heap_size(&mut self) {
        self.total_heap_size = self.young_generation.allocated_bytes
            + self.old_generation.allocated_bytes
            + self.permanent_generation.allocated_bytes;
    }

    /// Clear a generation (used during collection)
    pub fn clear_generation(&mut self, generation: ObjectGeneration) {
        match generation {
            ObjectGeneration::Young => {
                for object_id in self.young_generation.object_ids() {
                    self.object_locations.remove(&object_id);
                }
                self.young_generation.clear();
            }
            ObjectGeneration::Old => {
                for object_id in self.old_generation.object_ids() {
                    self.object_locations.remove(&object_id);
                }
                self.old_generation.clear();
            }
            ObjectGeneration::Permanent => {
                // Permanent objects are never cleared
            }
        }
        self.update_heap_size();
    }

    /// Get the current generation from handle pool
    pub fn current_generation(&self) -> Generation {
        self.handle_pool.current_generation()
    }
}

/// Estimate the memory size of a Veld value
fn estimate_value_size(value: &Value) -> usize {
    match value {
        Value::Integer(_) => std::mem::size_of::<i64>(),
        Value::Float(_) => std::mem::size_of::<f64>(),
        Value::Boolean(_) => std::mem::size_of::<bool>(),
        Value::Char(_) => std::mem::size_of::<char>(),
        Value::Unit => 0,
        Value::String(s) => s.len() + std::mem::size_of::<String>(),
        Value::Array(arr) => {
            arr.iter().map(estimate_value_size).sum::<usize>() + std::mem::size_of::<Vec<Value>>()
        }
        Value::Tuple(tuple) => {
            tuple.iter().map(estimate_value_size).sum::<usize>() + std::mem::size_of::<Vec<Value>>()
        }
        Value::Struct { fields, .. } => {
            fields.values().map(estimate_value_size).sum::<usize>()
                + std::mem::size_of::<HashMap<String, Value>>()
                + fields.keys().map(|k| k.len()).sum::<usize>()
        }
        Value::Enum { fields, .. } => {
            fields.iter().map(estimate_value_size).sum::<usize>()
                + std::mem::size_of::<Vec<Value>>()
        }
        Value::Function {
            body,
            captured_vars,
            ..
        } => {
            body.len() * 100 + // Rough estimate for statements
            captured_vars.values().map(estimate_value_size).sum::<usize>() +
            std::mem::size_of::<HashMap<String, Value>>()
        }
        Value::Return(val) => estimate_value_size(val) + std::mem::size_of::<Box<Value>>(),
        _ => std::mem::size_of::<Value>(), // Conservative estimate for other types
    }
}

/// Extract GC handles referenced by a value
fn extract_references(value: &Value) -> Vec<GcHandle> {
    // Note: In a real implementation, this would traverse the value
    // and extract any embedded GC handles. For now, we return empty
    // since the current Value enum doesn't contain GC handles directly.
    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocator_basic() {
        let config = GcConfig::default();
        let mut allocator = GcAllocator::new(&config);

        let handle = allocator.allocate(Value::Integer(42)).unwrap();
        assert_eq!(allocator.get_value(&handle), Some(&Value::Integer(42)));
        assert!(allocator.is_alive(&handle));
    }

    #[test]
    fn test_promotion() {
        let config = GcConfig::default();
        let mut allocator = GcAllocator::new(&config);

        let handle = allocator
            .allocate(Value::String("test".to_string()))
            .unwrap();

        // Should start in young generation
        assert_eq!(
            allocator.object_locations[&handle.object_id()],
            ObjectGeneration::Young
        );

        // Promote to old generation
        allocator.promote_object(&handle).unwrap();
        assert_eq!(
            allocator.object_locations[&handle.object_id()],
            ObjectGeneration::Old
        );
    }

    #[test]
    fn test_value_size_estimation() {
        assert_eq!(
            estimate_value_size(&Value::Integer(42)),
            std::mem::size_of::<i64>()
        );
        assert_eq!(estimate_value_size(&Value::Unit), 0);

        let string_val = Value::String("hello".to_string());
        assert!(estimate_value_size(&string_val) > 5); // At least the string length
    }

    #[test]
    fn test_heap_tracking() {
        let config = GcConfig::default();
        let mut allocator = GcAllocator::new(&config);

        let initial_size = allocator.heap_size();
        let handle = allocator.allocate(Value::Integer(42)).unwrap();

        assert!(allocator.heap_size() > initial_size);

        allocator.deallocate(&handle).unwrap();
        assert_eq!(allocator.heap_size(), initial_size);
    }
}
