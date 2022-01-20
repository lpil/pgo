# Gleam PGO

A PostgreSQL database client for Gleam, based on [PGO][erlang-pgo].

[erlang-pgo]: https://github.com/erleans/pgo

```rust
import gleam/pgo

pub fn main() {
  let pool = pgo.start_pool(pgo.Config(
    ..pgo.default_config(),
    host: "localhost",
    database: "my_database",
  ))

  let sql = "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true),
    (DEFAULT, 'felix', false)"

  assert Ok(response) = pgo.query(pool, sql, [])

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([])
}
```


## Installation

```sh
gleam add gleam_pgo
```
