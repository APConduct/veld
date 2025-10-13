//! Garbage Collection Statistics and Monitoring
//!
//! This module provides comprehensive statistics collection and monitoring
//! for the garbage collector, including performance metrics, memory usage,
//! and collection frequency analysis.

use std::collections::VecDeque;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use super::allocator::ObjectGeneration;
use super::collector::CollectionResult;

/// Comprehensive GC statistics
#[derive(Debug, Clone)]
pub struct GcStatistics {
    /// Total collections performed
    pub total_collections: u64,

    /// Collections by generation
    pub young_collections: u64,
    pub old_collections: u64,
    pub full_collections: u64,

    /// Total objects collected
    pub total_objects_collected: u64,

    /// Total bytes freed
    pub total_bytes_freed: u64,

    /// Total time spent in GC
    pub total_gc_time: Duration,

    /// Average collection time
    pub average_collection_time: Duration,

    /// Peak memory usage
    pub peak_memory_usage: usize,

    /// Current memory usage
    pub current_memory_usage: usize,

    /// Collection frequency statistics
    pub collection_frequency: CollectionFrequency,

    /// Recent collection history
    pub recent_collections: VecDeque<CollectionRecord>,

    /// Performance metrics
    pub performance_metrics: PerformanceMetrics,

    /// Heap growth statistics
    pub heap_growth: HeapGrowthStats,

    /// Statistics start time
    pub start_time: SystemTime,

    /// Last update time
    pub last_update: SystemTime,
}

/// Collection frequency analysis
#[derive(Debug, Clone)]
pub struct CollectionFrequency {
    /// Collections per second (recent average)
    pub collections_per_second: f64,

    /// Time between collections
    pub average_interval: Duration,

    /// Shortest interval between collections
    pub min_interval: Duration,

    /// Longest interval between collections
    pub max_interval: Duration,

    /// Last collection timestamp
    pub last_collection_time: Option<Instant>,
}

/// Individual collection record
#[derive(Debug, Clone)]
pub struct CollectionRecord {
    /// When the collection occurred
    pub timestamp: Instant,

    /// Type of collection
    pub collection_type: CollectionType,

    /// Duration of the collection
    pub duration: Duration,

    /// Objects collected
    pub objects_collected: usize,

    /// Bytes freed
    pub bytes_freed: usize,

    /// Objects promoted (for generational GC)
    pub objects_promoted: usize,

    /// Heap size before collection
    pub heap_before: usize,

    /// Heap size after collection
    pub heap_after: usize,
}

/// Type of garbage collection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    Minor,       // Young generation only
    Major,       // Old generation
    Full,        // All generations
    Incremental, // Incremental step
}

/// Performance metrics
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    /// Allocation rate (objects/second)
    pub allocation_rate: f64,

    /// Collection throughput (objects collected/second)
    pub collection_throughput: f64,

    /// GC overhead percentage
    pub gc_overhead_percent: f64,

    /// Memory efficiency (live data / total allocated)
    pub memory_efficiency: f64,

    /// Pause time percentiles
    pub pause_percentiles: PauseTimePercentiles,

    /// Promotion rate (objects promoted/allocated)
    pub promotion_rate: f64,

    /// Survival rate (objects surviving/allocated)
    pub survival_rate: f64,
}

/// Pause time percentile analysis
#[derive(Debug, Clone)]
pub struct PauseTimePercentiles {
    pub p50: Duration,   // 50th percentile (median)
    pub p90: Duration,   // 90th percentile
    pub p95: Duration,   // 95th percentile
    pub p99: Duration,   // 99th percentile
    pub p99_9: Duration, // 99.9th percentile
    pub max: Duration,   // Maximum pause time
}

/// Heap growth statistics
#[derive(Debug, Clone)]
pub struct HeapGrowthStats {
    /// Initial heap size
    pub initial_size: usize,

    /// Current heap size
    pub current_size: usize,

    /// Peak heap size
    pub peak_size: usize,

    /// Growth rate (bytes/second)
    pub growth_rate: f64,

    /// Heap utilization percentage
    pub utilization_percent: f64,

    /// Fragmentation percentage
    pub fragmentation_percent: f64,
}

impl GcStatistics {
    /// Create new GC statistics
    pub fn new() -> Self {
        let now = SystemTime::now();

        Self {
            total_collections: 0,
            young_collections: 0,
            old_collections: 0,
            full_collections: 0,
            total_objects_collected: 0,
            total_bytes_freed: 0,
            total_gc_time: Duration::new(0, 0),
            average_collection_time: Duration::new(0, 0),
            peak_memory_usage: 0,
            current_memory_usage: 0,
            collection_frequency: CollectionFrequency::new(),
            recent_collections: VecDeque::with_capacity(100), // Keep last 100 collections
            performance_metrics: PerformanceMetrics::new(),
            heap_growth: HeapGrowthStats::new(),
            start_time: now,
            last_update: now,
        }
    }

    /// Record a garbage collection
    pub fn record_collection(&mut self, result: &CollectionResult) {
        let now = Instant::now();
        let system_now = SystemTime::now();

        // Update basic counters
        self.total_collections += 1;
        self.total_objects_collected += result.objects_collected as u64;
        self.total_bytes_freed += result.bytes_freed as u64;
        self.total_gc_time += result.duration;

        // Update collection type counters
        if let Some(generation) = result.generation {
            match generation {
                ObjectGeneration::Young => self.young_collections += 1,
                ObjectGeneration::Old => self.old_collections += 1,
                ObjectGeneration::Permanent => self.full_collections += 1,
            }
        }

        // Update average collection time
        self.average_collection_time = self.total_gc_time / self.total_collections as u32;

        // Update collection frequency
        self.collection_frequency.record_collection(now);

        // Create collection record
        let collection_type = match result.generation {
            Some(ObjectGeneration::Young) => CollectionType::Minor,
            Some(ObjectGeneration::Old) => CollectionType::Major,
            Some(ObjectGeneration::Permanent) => CollectionType::Full,
            None => CollectionType::Incremental,
        };

        let record = CollectionRecord {
            timestamp: now,
            collection_type,
            duration: result.duration,
            objects_collected: result.objects_collected,
            bytes_freed: result.bytes_freed,
            objects_promoted: result.objects_promoted,
            heap_before: 0, // Would be set by caller
            heap_after: 0,  // Would be set by caller
        };

        // Add to recent collections (keep last 100)
        if self.recent_collections.len() >= 100 {
            self.recent_collections.pop_front();
        }
        self.recent_collections.push_back(record);

        // Update performance metrics
        self.update_performance_metrics();

        self.last_update = system_now;
    }

    /// Update memory usage statistics
    pub fn update_memory_usage(&mut self, current_usage: usize) {
        self.current_memory_usage = current_usage;
        if current_usage > self.peak_memory_usage {
            self.peak_memory_usage = current_usage;
        }

        // Update heap growth stats
        self.heap_growth.update(current_usage);
    }

    /// Get recent collection statistics
    pub fn recent_stats(&self, duration: Duration) -> RecentStats {
        let cutoff_time = Instant::now() - duration;

        let recent: Vec<_> = self
            .recent_collections
            .iter()
            .filter(|record| record.timestamp >= cutoff_time)
            .collect();

        if recent.is_empty() {
            return RecentStats::default();
        }

        let total_collections = recent.len();
        let total_duration: Duration = recent.iter().map(|r| r.duration).sum();
        let total_objects: usize = recent.iter().map(|r| r.objects_collected).sum();
        let total_bytes: usize = recent.iter().map(|r| r.bytes_freed).sum();

        RecentStats {
            collections: total_collections,
            total_duration,
            average_duration: total_duration / total_collections as u32,
            objects_collected: total_objects,
            bytes_freed: total_bytes,
            collections_per_second: total_collections as f64 / duration.as_secs_f64(),
        }
    }

    /// Get efficiency metrics
    pub fn efficiency_metrics(&self) -> EfficiencyMetrics {
        let uptime = self
            .last_update
            .duration_since(self.start_time)
            .unwrap_or(Duration::new(1, 0));

        let gc_overhead = if uptime.as_secs_f64() > 0.0 {
            self.total_gc_time.as_secs_f64() / uptime.as_secs_f64() * 100.0
        } else {
            0.0
        };

        let memory_efficiency = if self.peak_memory_usage > 0 {
            self.current_memory_usage as f64 / self.peak_memory_usage as f64 * 100.0
        } else {
            100.0
        };

        EfficiencyMetrics {
            gc_overhead_percent: gc_overhead,
            memory_efficiency_percent: memory_efficiency,
            collection_efficiency: if self.total_collections > 0 {
                self.total_bytes_freed as f64 / self.total_collections as f64
            } else {
                0.0
            },
            average_pause_time: self.average_collection_time,
        }
    }

    /// Generate a summary report
    pub fn summary_report(&self) -> String {
        let efficiency = self.efficiency_metrics();
        let recent = self.recent_stats(Duration::from_secs(60)); // Last minute

        format!(
            "GC Statistics Summary:\n\
             Total Collections: {} (Young: {}, Old: {}, Full: {})\n\
             Total Objects Collected: {}\n\
             Total Bytes Freed: {} MB\n\
             Total GC Time: {:?}\n\
             Average Collection Time: {:?}\n\
             Peak Memory Usage: {} MB\n\
             Current Memory Usage: {} MB\n\
             GC Overhead: {:.2}%\n\
             Memory Efficiency: {:.2}%\n\
             Recent Collections/sec: {:.2}\n\
             Collections Frequency: {:.2}/sec",
            self.total_collections,
            self.young_collections,
            self.old_collections,
            self.full_collections,
            self.total_objects_collected,
            self.total_bytes_freed / 1_048_576, // Convert to MB
            self.total_gc_time,
            self.average_collection_time,
            self.peak_memory_usage / 1_048_576,
            self.current_memory_usage / 1_048_576,
            efficiency.gc_overhead_percent,
            efficiency.memory_efficiency_percent,
            recent.collections_per_second,
            self.collection_frequency.collections_per_second
        )
    }

    // Private methods

    fn update_performance_metrics(&mut self) {
        self.performance_metrics
            .update_from_collections(&self.recent_collections);
    }
}

impl Default for GcStatistics {
    fn default() -> Self {
        Self::new()
    }
}

impl CollectionFrequency {
    fn new() -> Self {
        Self {
            collections_per_second: 0.0,
            average_interval: Duration::new(0, 0),
            min_interval: Duration::MAX,
            max_interval: Duration::new(0, 0),
            last_collection_time: None,
        }
    }

    fn record_collection(&mut self, timestamp: Instant) {
        if let Some(last_time) = self.last_collection_time {
            let interval = timestamp.duration_since(last_time);

            // Update interval statistics
            if interval < self.min_interval {
                self.min_interval = interval;
            }
            if interval > self.max_interval {
                self.max_interval = interval;
            }

            // Update average (simple moving average)
            if self.average_interval.is_zero() {
                self.average_interval = interval;
            } else {
                let avg_nanos = (self.average_interval.as_nanos() + interval.as_nanos()) / 2;
                self.average_interval = Duration::from_nanos(avg_nanos as u64);
            }

            // Update collections per second (based on recent average)
            if !self.average_interval.is_zero() {
                self.collections_per_second = 1.0 / self.average_interval.as_secs_f64();
            }
        }

        self.last_collection_time = Some(timestamp);
    }
}

impl PerformanceMetrics {
    fn new() -> Self {
        Self {
            allocation_rate: 0.0,
            collection_throughput: 0.0,
            gc_overhead_percent: 0.0,
            memory_efficiency: 0.0,
            pause_percentiles: PauseTimePercentiles::new(),
            promotion_rate: 0.0,
            survival_rate: 0.0,
        }
    }

    fn update_from_collections(&mut self, collections: &VecDeque<CollectionRecord>) {
        if collections.is_empty() {
            return;
        }

        // Calculate pause time percentiles
        let mut durations: Vec<Duration> = collections.iter().map(|c| c.duration).collect();
        durations.sort();

        self.pause_percentiles = PauseTimePercentiles::from_sorted_durations(&durations);

        // Calculate collection throughput
        let total_objects: usize = collections.iter().map(|c| c.objects_collected).sum();
        let total_duration: Duration = collections.iter().map(|c| c.duration).sum();

        if !total_duration.is_zero() {
            self.collection_throughput = total_objects as f64 / total_duration.as_secs_f64();
        }

        // Calculate promotion and survival rates
        let total_promoted: usize = collections.iter().map(|c| c.objects_promoted).sum();
        if total_objects > 0 {
            self.promotion_rate = total_promoted as f64 / total_objects as f64;
        }
    }
}

impl PauseTimePercentiles {
    fn new() -> Self {
        Self {
            p50: Duration::new(0, 0),
            p90: Duration::new(0, 0),
            p95: Duration::new(0, 0),
            p99: Duration::new(0, 0),
            p99_9: Duration::new(0, 0),
            max: Duration::new(0, 0),
        }
    }

    fn from_sorted_durations(durations: &[Duration]) -> Self {
        if durations.is_empty() {
            return Self::new();
        }

        let len = durations.len();

        Self {
            p50: durations[len * 50 / 100],
            p90: durations[len * 90 / 100],
            p95: durations[len * 95 / 100],
            p99: durations[len * 99 / 100],
            p99_9: durations[len * 999 / 1000],
            max: durations[len - 1],
        }
    }
}

impl HeapGrowthStats {
    fn new() -> Self {
        Self {
            initial_size: 0,
            current_size: 0,
            peak_size: 0,
            growth_rate: 0.0,
            utilization_percent: 0.0,
            fragmentation_percent: 0.0,
        }
    }

    fn update(&mut self, new_size: usize) {
        if self.initial_size == 0 {
            self.initial_size = new_size;
        }

        self.current_size = new_size;

        if new_size > self.peak_size {
            self.peak_size = new_size;
        }

        // Calculate utilization
        if self.peak_size > 0 {
            self.utilization_percent = (new_size as f64 / self.peak_size as f64) * 100.0;
        }
    }
}

/// Recent statistics for a time window
#[derive(Debug, Clone, Default)]
pub struct RecentStats {
    pub collections: usize,
    pub total_duration: Duration,
    pub average_duration: Duration,
    pub objects_collected: usize,
    pub bytes_freed: usize,
    pub collections_per_second: f64,
}

/// Efficiency metrics
#[derive(Debug, Clone)]
pub struct EfficiencyMetrics {
    pub gc_overhead_percent: f64,
    pub memory_efficiency_percent: f64,
    pub collection_efficiency: f64,
    pub average_pause_time: Duration,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc_statistics_creation() {
        let stats = GcStatistics::new();
        assert_eq!(stats.total_collections, 0);
        assert_eq!(stats.total_objects_collected, 0);
        assert_eq!(stats.total_bytes_freed, 0);
    }

    #[test]
    fn test_collection_recording() {
        let mut stats = GcStatistics::new();
        let result = CollectionResult {
            objects_collected: 100,
            bytes_freed: 1024,
            duration: Duration::from_millis(10),
            generation: Some(ObjectGeneration::Young),
            completed: true,
            objects_promoted: 5,
            objects_surviving: 50,
        };

        stats.record_collection(&result);

        assert_eq!(stats.total_collections, 1);
        assert_eq!(stats.young_collections, 1);
        assert_eq!(stats.total_objects_collected, 100);
        assert_eq!(stats.total_bytes_freed, 1024);
        assert_eq!(stats.recent_collections.len(), 1);
    }

    #[test]
    fn test_memory_usage_tracking() {
        let mut stats = GcStatistics::new();

        stats.update_memory_usage(1000);
        assert_eq!(stats.current_memory_usage, 1000);
        assert_eq!(stats.peak_memory_usage, 1000);

        stats.update_memory_usage(1500);
        assert_eq!(stats.current_memory_usage, 1500);
        assert_eq!(stats.peak_memory_usage, 1500);

        stats.update_memory_usage(800);
        assert_eq!(stats.current_memory_usage, 800);
        assert_eq!(stats.peak_memory_usage, 1500); // Peak should remain
    }

    #[test]
    fn test_recent_stats() {
        let mut stats = GcStatistics::new();

        // Add some collections
        for i in 0..5 {
            let result = CollectionResult {
                objects_collected: 10 + i,
                bytes_freed: 100 + i * 10,
                duration: Duration::from_millis(5),
                generation: Some(ObjectGeneration::Young),
                completed: true,
                objects_promoted: 1,
                objects_surviving: 5,
            };
            stats.record_collection(&result);
        }

        let recent = stats.recent_stats(Duration::from_secs(60));
        assert_eq!(recent.collections, 5);
        assert_eq!(recent.objects_collected, 10 + 11 + 12 + 13 + 14); // Sum of all
    }

    #[test]
    fn test_pause_percentiles() {
        let durations = vec![
            Duration::from_millis(1),
            Duration::from_millis(2),
            Duration::from_millis(3),
            Duration::from_millis(4),
            Duration::from_millis(5),
            Duration::from_millis(10),
        ];

        let percentiles = PauseTimePercentiles::from_sorted_durations(&durations);
        assert_eq!(percentiles.p50, Duration::from_millis(3));
        assert_eq!(percentiles.max, Duration::from_millis(10));
    }

    #[test]
    fn test_efficiency_metrics() {
        let mut stats = GcStatistics::new();
        stats.total_collections = 10;
        stats.total_bytes_freed = 10240;
        stats.current_memory_usage = 8000;
        stats.peak_memory_usage = 10000;

        let efficiency = stats.efficiency_metrics();
        assert_eq!(efficiency.memory_efficiency_percent, 80.0);
        assert_eq!(efficiency.collection_efficiency, 1024.0);
    }
}
