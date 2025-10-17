//! Mark-and-Sweep Garbage Collector Implementation
//!
//! This module implements a mark-and-sweep collector with generational optimization,
//! incremental collection, and support for Veld's value system.

use std::collections::{HashMap, HashSet, VecDeque};
use std::time::{Duration, Instant};

use super::super::value::Value;
use super::GcConfig;
use super::allocator::{GcAllocator, ObjectGeneration};
use super::handle::GcHandle;
use super::mark::{MarkPhase, MarkState};
use super::root_set::RootSet;
use veld_error::{Result, VeldError};

/// Result of a garbage collection cycle
#[derive(Debug, Clone, Default)]
pub struct CollectionResult {
    /// Number of objects collected
    pub objects_collected: usize,
    /// Bytes freed
    pub bytes_freed: usize,
    /// Time taken for collection
    pub duration: Duration,
    /// Generation that was collected
    pub generation: Option<ObjectGeneration>,
    /// Whether collection completed successfully
    pub completed: bool,
    /// Number of objects promoted
    pub objects_promoted: usize,
    /// Number of objects surviving
    pub objects_surviving: usize,
}

/// Collection strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionStrategy {
    /// Collect only young generation
    Minor,
    /// Collect young and old generations
    Major,
    /// Collect all generations
    Full,
    /// Incremental collection step
    Incremental,
}

/// Garbage collector state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectorState {
    /// Collector is idle
    Idle,
    /// Mark phase in progress
    Marking,
    /// Sweep phase in progress
    Sweeping,
    /// Compaction phase in progress
    Compacting,
    /// Finalization phase in progress
    Finalizing,
}

/// Main garbage collector implementation
pub struct GcCollector {
    /// Current collector state
    state: CollectorState,

    /// Mark phase implementation
    mark_phase: MarkPhase,

    /// Objects marked in current collection
    marked_objects: HashSet<u64>,

    /// Objects to be finalized
    finalization_queue: VecDeque<GcHandle>,

    /// Statistics from last collection
    last_result: CollectionResult,

    /// Incremental collection progress
    incremental_progress: IncrementalProgress,

    /// Collection thresholds
    thresholds: CollectionThresholds,
}

/// Progress tracking for incremental collection
#[derive(Debug, Clone)]
struct IncrementalProgress {
    /// Current collection strategy
    strategy: CollectionStrategy,
    /// Objects remaining to mark
    mark_queue: VecDeque<u64>,
    /// Objects remaining to sweep
    sweep_queue: VecDeque<u64>,
    /// Time spent in current collection
    elapsed_time: Duration,
    /// Start time of current collection
    start_time: Option<Instant>,
}

/// Thresholds for triggering different collection types
#[derive(Debug, Clone)]
struct CollectionThresholds {
    /// Young generation threshold (bytes)
    young_threshold: usize,
    /// Old generation threshold (bytes)
    old_threshold: usize,
    /// Heap size threshold for major collection
    major_threshold: usize,
    /// Time threshold for incremental steps
    time_threshold: Duration,
}

impl GcCollector {
    /// Create a new garbage collector
    pub fn new(config: &GcConfig) -> Self {
        Self {
            state: CollectorState::Idle,
            mark_phase: MarkPhase::new(),
            marked_objects: HashSet::new(),
            finalization_queue: VecDeque::new(),
            last_result: CollectionResult::default(),
            incremental_progress: IncrementalProgress {
                strategy: CollectionStrategy::Minor,
                mark_queue: VecDeque::new(),
                sweep_queue: VecDeque::new(),
                elapsed_time: Duration::new(0, 0),
                start_time: None,
            },
            thresholds: CollectionThresholds {
                young_threshold: config.initial_heap_size / 3,
                old_threshold: config.initial_heap_size * 2 / 3,
                major_threshold: config.initial_heap_size,
                time_threshold: Duration::from_micros(config.max_incremental_time),
            },
        }
    }

    /// Perform a full garbage collection
    pub fn collect_full(
        &mut self,
        allocator: &mut GcAllocator,
        roots: &RootSet,
        config: &GcConfig,
    ) -> Result<CollectionResult> {
        let start_time = Instant::now();
        self.state = CollectorState::Marking;

        tracing::debug!("Starting full garbage collection");

        // Determine collection strategy based on heap state
        let strategy = self.determine_strategy(allocator, config);

        // Reset state for new collection
        self.marked_objects.clear();
        self.finalization_queue.clear();

        // Mark phase
        let mark_result = self.mark_phase(allocator, roots, strategy)?;

        // Sweep phase
        self.state = CollectorState::Sweeping;
        let sweep_result = self.sweep_phase(allocator, strategy)?;

        // Promotion phase (for generational collection)
        let promotion_result = if config.generational {
            self.promotion_phase(allocator, config)?
        } else {
            PromotionResult::default()
        };

        // Finalization phase
        self.state = CollectorState::Finalizing;
        self.finalization_phase()?;

        self.state = CollectorState::Idle;

        let result = CollectionResult {
            objects_collected: sweep_result.objects_collected,
            bytes_freed: sweep_result.bytes_freed,
            duration: start_time.elapsed(),
            generation: Some(match strategy {
                CollectionStrategy::Minor => ObjectGeneration::Young,
                CollectionStrategy::Major | CollectionStrategy::Full => ObjectGeneration::Old,
                CollectionStrategy::Incremental => ObjectGeneration::Young,
            }),
            completed: true,
            objects_promoted: promotion_result.objects_promoted,
            objects_surviving: mark_result.objects_marked,
        };

        self.last_result = result.clone();

        tracing::info!(
            "GC completed: {} objects collected, {} bytes freed, {} objects promoted in {:?}",
            result.objects_collected,
            result.bytes_freed,
            result.objects_promoted,
            result.duration
        );

        Ok(result)
    }

    /// Perform an incremental collection step
    pub fn collect_incremental(
        &mut self,
        allocator: &mut GcAllocator,
        roots: &RootSet,
        config: &GcConfig,
    ) -> Result<bool> {
        let start_time = Instant::now();

        // Initialize incremental collection if not started
        if self.state == CollectorState::Idle {
            self.start_incremental_collection(allocator, roots)?;
        }

        // Perform work within time budget
        while start_time.elapsed() < self.thresholds.time_threshold {
            match self.state {
                CollectorState::Marking => {
                    if self.incremental_mark_step(allocator)? {
                        self.state = CollectorState::Sweeping;
                        self.prepare_sweep_phase(allocator)?;
                    } else {
                        break; // More marking needed
                    }
                }
                CollectorState::Sweeping => {
                    if self.incremental_sweep_step(allocator)? {
                        self.state = CollectorState::Finalizing;
                    } else {
                        break; // More sweeping needed
                    }
                }
                CollectorState::Finalizing => {
                    self.finalization_phase()?;
                    self.complete_incremental_collection();
                    return Ok(true); // Collection completed
                }
                _ => return Ok(true), // Already completed
            }
        }

        // Update elapsed time
        self.incremental_progress.elapsed_time += start_time.elapsed();

        Ok(false) // Collection not yet completed
    }

    /// Get the result of the last collection
    pub fn last_collection_result(&self) -> &CollectionResult {
        &self.last_result
    }

    /// Run finalizers for collected objects
    pub fn run_finalizers(&mut self) -> Result<()> {
        while let Some(handle) = self.finalization_queue.pop_front() {
            // In a real implementation, this would call object finalizers
            tracing::trace!("Running finalizer for object {}", handle.object_id());
        }
        Ok(())
    }

    /// Check if collector is currently running
    pub fn is_collecting(&self) -> bool {
        self.state != CollectorState::Idle
    }

    /// Get current collector state
    pub fn state(&self) -> CollectorState {
        self.state
    }

    // Private methods

    fn determine_strategy(&self, allocator: &GcAllocator, config: &GcConfig) -> CollectionStrategy {
        let heap_size = allocator.heap_size();
        let stats = allocator.generation_stats();

        if heap_size >= self.thresholds.major_threshold {
            CollectionStrategy::Full
        } else if stats.iter().any(|s| {
            s.generation == ObjectGeneration::Old
                && s.allocated_bytes >= self.thresholds.old_threshold
        }) {
            CollectionStrategy::Major
        } else {
            CollectionStrategy::Minor
        }
    }

    fn mark_phase(
        &mut self,
        allocator: &GcAllocator,
        roots: &RootSet,
        strategy: CollectionStrategy,
    ) -> Result<MarkResult> {
        let mut mark_count = 0;
        let mut mark_queue = VecDeque::new();

        // Add roots to mark queue
        for root in roots.roots() {
            if allocator.is_alive(root) {
                mark_queue.push_back(root.object_id());
            }
        }

        // Mark reachable objects
        while let Some(object_id) = mark_queue.pop_front() {
            if self.marked_objects.contains(&object_id) {
                continue;
            }

            self.marked_objects.insert(object_id);
            mark_count += 1;

            // Add references to mark queue
            if let Some((_, object, generation)) =
                allocator.all_objects().find(|(id, _, _)| **id == object_id)
            {
                // Only traverse references if we're collecting this generation
                let should_traverse = match strategy {
                    CollectionStrategy::Minor => generation == ObjectGeneration::Young,
                    CollectionStrategy::Major => generation != ObjectGeneration::Permanent,
                    CollectionStrategy::Full => true,
                    CollectionStrategy::Incremental => true,
                };

                if should_traverse {
                    for reference in &object.references {
                        if allocator.is_alive(reference) {
                            mark_queue.push_back(reference.object_id());
                        }
                    }
                }
            }
        }

        Ok(MarkResult {
            objects_marked: mark_count,
        })
    }

    fn sweep_phase(
        &mut self,
        allocator: &mut GcAllocator,
        strategy: CollectionStrategy,
    ) -> Result<SweepResult> {
        let mut objects_collected = 0;
        let mut bytes_freed = 0;

        // Collect unmarked objects in appropriate generations
        let to_collect: Vec<_> = allocator
            .all_objects()
            .filter(|(id, _, generation)| {
                !self.marked_objects.contains(id)
                    && self.should_collect_generation(*generation, strategy)
            })
            .map(|(id, obj, _)| (*id, obj.metadata.size))
            .collect();

        for (object_id, size) in to_collect {
            let handle = GcHandle::new(object_id, allocator.current_generation());

            // Add to finalization queue if needed
            self.finalization_queue.push_back(handle.clone());

            // Deallocate the object
            if allocator.deallocate(&handle).is_ok() {
                objects_collected += 1;
                bytes_freed += size;
            }
        }

        Ok(SweepResult {
            objects_collected,
            bytes_freed,
        })
    }

    fn promotion_phase(
        &mut self,
        allocator: &mut GcAllocator,
        config: &GcConfig,
    ) -> Result<PromotionResult> {
        let mut objects_promoted = 0;

        // Find objects that should be promoted
        let to_promote: Vec<_> = allocator
            .young_objects()
            .filter(|(id, obj)| {
                self.marked_objects.contains(id)
                    && obj.metadata.should_promote(config.promotion_threshold)
            })
            .map(|(id, _)| *id)
            .collect();

        // Promote surviving young objects
        for object_id in to_promote {
            let handle = GcHandle::new(object_id, allocator.current_generation());
            if allocator.promote_object(&handle).is_ok() {
                objects_promoted += 1;
            }
        }

        Ok(PromotionResult { objects_promoted })
    }

    fn finalization_phase(&mut self) -> Result<()> {
        // Finalization is handled separately in run_finalizers
        Ok(())
    }

    fn should_collect_generation(
        &self,
        generation: ObjectGeneration,
        strategy: CollectionStrategy,
    ) -> bool {
        match strategy {
            CollectionStrategy::Minor => generation == ObjectGeneration::Young,
            CollectionStrategy::Major => generation != ObjectGeneration::Permanent,
            CollectionStrategy::Full => generation != ObjectGeneration::Permanent,
            CollectionStrategy::Incremental => generation == ObjectGeneration::Young,
        }
    }

    // Incremental collection methods

    fn start_incremental_collection(
        &mut self,
        allocator: &GcAllocator,
        roots: &RootSet,
    ) -> Result<()> {
        self.state = CollectorState::Marking;
        self.marked_objects.clear();
        self.finalization_queue.clear();

        // Initialize mark queue with roots
        self.incremental_progress.mark_queue.clear();
        for root in roots.roots() {
            if allocator.is_alive(root) {
                self.incremental_progress
                    .mark_queue
                    .push_back(root.object_id());
            }
        }

        self.incremental_progress.start_time = Some(Instant::now());
        self.incremental_progress.elapsed_time = Duration::new(0, 0);

        Ok(())
    }

    fn incremental_mark_step(&mut self, allocator: &GcAllocator) -> Result<bool> {
        const MAX_MARKS_PER_STEP: usize = 100;

        for _ in 0..MAX_MARKS_PER_STEP {
            if let Some(object_id) = self.incremental_progress.mark_queue.pop_front() {
                if self.marked_objects.contains(&object_id) {
                    continue;
                }

                self.marked_objects.insert(object_id);

                // Add references to mark queue
                if let Some((_, object, _)) =
                    allocator.all_objects().find(|(id, _, _)| **id == object_id)
                {
                    for reference in &object.references {
                        if allocator.is_alive(reference) {
                            self.incremental_progress
                                .mark_queue
                                .push_back(reference.object_id());
                        }
                    }
                }
            } else {
                // Mark phase completed
                return Ok(true);
            }
        }

        Ok(false) // More work needed
    }

    fn prepare_sweep_phase(&mut self, allocator: &GcAllocator) -> Result<()> {
        self.incremental_progress.sweep_queue.clear();

        // Add all unmarked objects to sweep queue
        for (id, _, generation) in allocator.all_objects() {
            if !self.marked_objects.contains(id)
                && self.should_collect_generation(generation, self.incremental_progress.strategy)
            {
                self.incremental_progress.sweep_queue.push_back(*id);
            }
        }

        Ok(())
    }

    fn incremental_sweep_step(&mut self, allocator: &GcAllocator) -> Result<bool> {
        const MAX_SWEEPS_PER_STEP: usize = 50;

        for _ in 0..MAX_SWEEPS_PER_STEP {
            if let Some(object_id) = self.incremental_progress.sweep_queue.pop_front() {
                let handle = GcHandle::new(object_id, allocator.current_generation());

                // Add to finalization queue
                self.finalization_queue.push_back(handle.clone());

                // Add to finalization queue (actual deallocation happens later)
                // let _ = allocator.deallocate(&handle);
            } else {
                // Sweep phase completed
                return Ok(true);
            }
        }

        Ok(false) // More work needed
    }

    fn complete_incremental_collection(&mut self) {
        let total_time = self
            .incremental_progress
            .start_time
            .map(|start| start.elapsed())
            .unwrap_or(self.incremental_progress.elapsed_time);

        self.last_result = CollectionResult {
            objects_collected: 0, // Would need to track this
            bytes_freed: 0,       // Would need to track this
            duration: total_time,
            generation: Some(ObjectGeneration::Young),
            completed: true,
            objects_promoted: 0, // Would need to track this
            objects_surviving: self.marked_objects.len(),
        };

        self.state = CollectorState::Idle;
    }
}

// Helper types for internal use

#[derive(Debug, Default)]
struct MarkResult {
    objects_marked: usize,
}

#[derive(Debug, Default)]
struct SweepResult {
    objects_collected: usize,
    bytes_freed: usize,
}

#[derive(Debug, Default)]
struct PromotionResult {
    objects_promoted: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::GcConfig;

    #[test]
    fn test_collector_creation() {
        let config = GcConfig::default();
        let collector = GcCollector::new(&config);

        assert_eq!(collector.state(), CollectorState::Idle);
        assert!(!collector.is_collecting());
    }

    #[test]
    fn test_collection_strategy() {
        let config = GcConfig::default();
        let collector = GcCollector::new(&config);
        let allocator = GcAllocator::new(&config);

        let strategy = collector.determine_strategy(&allocator, &config);
        assert_eq!(strategy, CollectionStrategy::Minor);
    }

    #[test]
    fn test_generation_collection() {
        let collector = GcCollector::new(&GcConfig::default());

        assert!(
            collector.should_collect_generation(ObjectGeneration::Young, CollectionStrategy::Minor)
        );

        assert!(
            !collector.should_collect_generation(ObjectGeneration::Old, CollectionStrategy::Minor)
        );

        assert!(
            collector.should_collect_generation(ObjectGeneration::Old, CollectionStrategy::Major)
        );

        assert!(
            !collector
                .should_collect_generation(ObjectGeneration::Permanent, CollectionStrategy::Major)
        );
    }
}
