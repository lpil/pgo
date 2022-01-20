# Gleam PGO

A PostgreSQL database client for Gleam, based on [PGO][erlang-pgo].

[erlang-pgo]: https://github.com/erleans/pgo

```rust
import gleam/pgo
import gleam/dynamic

pub fn main() {
  let pool = pgo.start_pool(pgo.Config(
    ..pgo.default_config(),
    host: "localhost",
    database: "my_database",
  ))

  let sql = "
  select
    name, age, colour, friends
  from
    cats
  where
    id = $1"
    
  let return_type = dynamic.tuple4(
    dynamic.string,
    dynamic.int,
    dynamic.string,
    dynamic.list(dynamic.string),
  )

  assert Ok(response) = 
    pgo.query(pool, sql, [pgo.int(1)], return_type)

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([
    #("Nubi", 3, "black", ["Al", "Cutlass"]),
  ])
}
```

## Installation

```sh
gleam add gleam_pgo
```
