# Gleam PGO

A Postgresql database client for Gleam, based on [PGO][erlang-pgo].

[erlang-pgo]: https://github.com/erleans/pgo

```rust
import gleam/pgo

pub fn main() {
  let default = atom.create_from_string("default")
  let config = [pgo.Host("localhost"), pgo.Database("gleam_pgo_test")]
  let _ = pgo.start_link(default, config)

  let sql = "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true),
    (DEFAULT, 'felix', false)"

  assert Ok(response) = pgo.query(default, sql, [])

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([])
}
```


## Installation

This package can be installed by adding `gleam_pgo` to your `rebar.config` dependencies:

```erlang
{deps, [
    gleam_pgo
]}.
```
