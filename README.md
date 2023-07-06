# undertheory

*Music-theoretic LFE modules: keys, scales, modes, melody-generation, etc.*

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]

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

## API

TBD

[//]: ---Named-Links---

[logo]: priv/images/project-logo.png
[logo-large]: priv/images/project-logo-large.png
[gh-actions-badge]: https://github.com/ut-proj/undertheory/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/ut-proj/undertheory/actions
[lfe]: https://github.com/lfe/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-25+-blue.svg
[versions]: https://github.com/ut-proj/undertheory/blob/master/.github/workflows/cicd.yml
