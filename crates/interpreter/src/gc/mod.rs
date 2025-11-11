//! Garbage Collection System for Veld Interpreter
//!
//! NOTE: This module is deprecated. The interpreter now uses veld_common::gc instead.
//! This code is kept for historical reference but is disabled to avoid type conflicts.
//!
//! To re-enable this legacy GC implementation, add the "legacy_gc" feature to Cargo.toml
//! and recompile. However, this is not recommended as it conflicts with the common GC types.

#[cfg(feature = "legacy_gc")]
pub mod allocator;
#[cfg(feature = "legacy_gc")]
pub mod collector;
#[cfg(feature = "legacy_gc")]
pub mod handle;
#[cfg(feature = "legacy_gc")]
pub mod mark;
#[cfg(feature = "legacy_gc")]
pub mod root_set;
#[cfg(feature = "legacy_gc")]
pub mod statistics;
#[cfg(feature = "legacy_gc")]
pub mod value_ref;
