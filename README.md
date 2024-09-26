# undertheory

*Music-theoretic LFE modules: notes, intervals, keys, scales, modes, melody-generation, etc.*

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![][logo]][logo-large]

## Dependencies & Setup

This application assumes that the following are on your system:

* `git`
* A modern install of Erlang (v25+)
* [rebar3](https://www.rebar3.org/) (Erlang build tool)

## Build & Run

```shell
rebar3 compile
```

Start up the LFE REPL:

``` shell
rebar3 lfe repl
```

## Examples

Notes:

``` lisp
lfe> (uth.note:name 0)
C
lfe> (uth.note:name 0 #(all))
(C B# Dbb)
lfe> (uth.note:number 'Gb)
6
```

Intervals:

``` lisp
lfe> (uth.interval:name 'C 'Eb)
m3
lfe> (uth.interval:name 'C 'G)
P5
lfe> (uth.interval:name 'C 'Bb)
m7
lfe> (uth.interval:above 'C 'm3))
(D# Eb Fbb)
lfe> (uth.interval:above 'C 'm3 #(flat))
Eb
```

Scales:

``` lisp
lfe> (uth.scale:locrian)
(1 b2 b3 4 b5 b6 b7)
lfe> (uth.scale:as-intervals 'locrian)
(P0 m2 m3 P4 dim5 m6 m7)
lfe> (uth.scale:as-notes 'C 'lydian)
(C D E F# G A B)
lfe> (uth.scale:as-notes 'C 'aeolian '#(flat))
(C D Eb F G Ab Bb)
lfe> (uth.scale:as-notes 'Eb 'ionian)
(Eb F G Ab Bb C D)
```

Inversions:

``` lisp
lfe> (uth.notes:invert 'Bb '(D Eb F D F G Bb A G Bb D) #(flat))
(Bb A G Bb G F D Eb F D Bb)
```

[//]: ---Named-Links---

[logo]: priv/images/project-logo.png
[logo-large]: priv/images/project-logo-large.png
[gh-actions-badge]: https://github.com/ut-proj/undertheory/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/ut-proj/undertheory/actions
[lfe]: https://github.com/lfe/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-25+-blue.svg
[versions]: https://github.com/ut-proj/undertheory/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/ut-proj/undertheory/tags
[github-tag-badge]: https://img.shields.io/github/tag/ut-proj/undertheory.svg
