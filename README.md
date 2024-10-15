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

## Support of connection URI

Configuring a Postgres connection is done by using `Config` type in `gleam/pgo`.
To facilitate connection, and to provide easy integration with the rest of the
Postgres ecosystem, `gleam_pgo` provides handling of
[connection URI as defined by Postgres](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING-URIS).
Shape of connection URI is `postgresql://[username:password@][host:port][/dbname][?query]`.
Call `gleam/pgo.url_config` with your connection URI, and in case it's correct
against the Postgres standard, your `Config` will be automatically generated!

Here's an example, using [`envoy`](https://github.com/lpil/envoy) to read the
connection URI from the environment.

```gleam
import envoy
import gleam/pgo

/// Read the DATABASE_URL environment variable.
/// Generate the pgo.Config from that database URL.
/// Finally, connect to database.
pub fn read_connection_uri() -> Result(pgo.Connection, Nil) {
  use database_url <- result.try(envoy.get("DATABASE_URL"))
  use config <- result.try(pgo.url_config(database_url))
  Ok(pgo.connect(config))
}
```

## About JSON

In Postgres, you can define a type `json` or `jsonb`. Such a type can be query
in SQL, but Postgres returns it a simple string, and accepts it as a simple string!
When writing or reading a JSON, you can simply use
`pgo.text(json.to_string(my_json))` and `dynamic.string` to respectively write
and read them!

## Rows as maps

By default, `pgo` will return every selected value from your query as a tuple.
In case you want a different output, you can activate `rows_as_maps` in `Config`.
Once activated, every returned rows will take the form of a `Dict`.

## About timestamps

If you're used to manage timestamps in Postgres, you know timestamp are stored
as bytes, and clients transforms them in their desired formats. `pgo` uses the
Erlang datetime convention, i.e. managing a timestamp as a tuple
`#(#(year, month, day), #(hour, minutes, seconds))`. When querying a timestamp,
unless you're casting it (in ISO-8601 for example) voluntarily in your SQL query,
`pgo` will give you such a tuple. You're expected to decode it using
`pgo.decode_timestamp` that will do the work of decoding it for you.

## Atom generation

Creating a connection pool with the `pgo.connect` function dynamically generates
an Erlang atom. Atoms are not garbage collected and only a certain number of
them can exist in an Erlang VM instance, and hitting this limit will result in
the VM crashing. Due to this limitation you should not dynamically open new
connection pools, instead create the pools you need when your application starts
and reuse them throughout the lifetime of your program.
