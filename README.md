# Gleam PGO

A PostgreSQL database client for Gleam, based on [PGO][erlang-pgo].

[erlang-pgo]: https://github.com/erleans/pgo

```gleam
import gleam/pgo
import gleam/dynamic
import gleeunit/should

pub fn main() {
  // Start a database connection pool.
  // Typically you will want to create one pool for use in your program
  let db = pgo.connect(pgo.Config(
    ..pgo.default_config(),
    host: "localhost",
    database: "my_database",
    pool_size: 15,
  ))

  // An SQL statement to run. It takes one int as a parameter
  let sql = "
  select
    name, age, colour, friends
  from
    cats
  where
    id = $1"

  // This is the decoder for the value returned by the query
  let return_type = dynamic.tuple4(
    dynamic.string,
    dynamic.int,
    dynamic.string,
    dynamic.list(dynamic.string),
  )

  // Run the query against the PostgreSQL database
  // The int `1` is given as a parameter
  let assert Ok(response) = 
    pgo.execute(sql, db, [pgo.int(1)], return_type)

  // And then do something with the returned results
  response.count
  |> should.equal(2)
  response.rows
  |> should.equal([
    #("Nubi", 3, "black", ["Al", "Cutlass"]),
  ])
}
```

## Installation

```sh
gleam add gleam_pgo
```
