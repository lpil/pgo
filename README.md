# pgo

[![](https://github.com/gleam-experiments/pgo/workflows/test/badge.svg)](https://github.com/gleam-experiments/pgo/actions)

Bindings to the Erlang [pgo][pgo] Postgresql database client.

[pgo]: https://github.com/erleans/pgo


## Installation

If [available in Hex](https://www.rebar3.org/docs/dependencies#section-declaring-dependencies)
this package can be installed by adding `gleam_pgo` to your `rebar.config` dependencies:

```erlang
{deps, [
    gleam_pgo
]}.
```

## Development

```
createdb gleam_pgo_test
rebar3 eunit
```
