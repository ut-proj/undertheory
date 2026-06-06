use music_comp_mt::quintal::BaseSpace;
use rustler::Resource;

/// Newtype wrapper so this crate can implement Rustler's `Resource` for
/// mt-rs's `BaseSpace`. The orphan rule (E0117) forbids implementing a foreign
/// trait on a foreign type directly, so the resource handle wraps `BaseSpace`.
/// This is invisible on the LFE side — the term is still an opaque handle.
pub struct BaseSpaceResource(pub BaseSpace);

#[rustler::resource_impl]
impl Resource for BaseSpaceResource {}
