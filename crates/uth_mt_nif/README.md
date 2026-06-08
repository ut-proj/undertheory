# uth_mt_nif

Rustler NIFs bridging the [music-comp-mt][mt] music theory crate to Erlang/LFE
through the BEAM's NIF interface.

This crate provides the Rust side of the binding — the NIF stubs, term
converters, and resource type wrappers. The LFE-side wrapper modules live in
[undertheory][uth].

## Usage

See [undertheory][uth] for the full integration pattern. Minimal example from
LFE:

```lisp
(uth.mt.note:parse-midi-pitch "C4")
;; => #(ok 60)
```

## Platforms

Verified on macOS (Apple Silicon) and Linux (Ubuntu LTS 24.04). The macOS build
requires an ad-hoc codesign step that the host's `rebar.config` handles
automatically.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or
  <http://opensource.org/licenses/MIT>)

at your option.

[mt]: https://crates.io/crates/music-comp-mt
[uth]: https://github.com/ut-proj/undertheory
