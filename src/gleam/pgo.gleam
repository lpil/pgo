//// Postgresql client
////
//// Gleam wrapper around pgo library

import gleam/erlang/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/string
import gleam/io
import gleam/option.{None, Option, Some}
import gleam/uri.{Uri}
import gleam/otp/process.{Pid}

// TODO: use a record
/// Avaliable configuration options when starting a pool.
pub type PoolConfig {
  Host(String)
  Port(Int)
  Database(String)
  User(String)
  Password(String)

  Ssl(Bool)
  PoolSize(Int)
  QueueTarget(Int)
  QueueInterval(Int)
  IdleInterval(Int)

  Trace(Bool)
}

fn parse_database_url(
  database_url: String,
) -> Result(#(String, String, String, Int), Nil) {
  case uri.parse(database_url) {
    Uri(
      scheme: Some("postgres"),
      userinfo: Some(userinfo),
      host: Some(host),
      port: Some(db_port),
      path: path,
      ..,
    ) -> Ok(#(userinfo, host, path, db_port))
    _ -> Error(Nil)
  }
}

/// Parse a database url into an option list that can be used to start a pool.
pub fn url_config(database_url: String) -> Result(List(PoolConfig), Nil) {
  try #(userinfo, host, path, db_port) = parse_database_url(database_url)
  try #(user, password) = string.split_once(userinfo, ":")
  case string.split(path, "/") {
    ["", database] ->
      Ok([
        Host(host),
        Port(db_port),
        Database(database),
        User(user),
        Password(password),
      ])
    _ -> Error(Nil)
  }
}

// Ideally unnamed
pub external fn start_link(Atom, List(PoolConfig)) -> Result(Pid, Dynamic) =
  "pgo_pool" "start_link"

/// Type that can be passed as arguments to a query.
pub external type PgType

pub fn null() -> PgType {
  atom.create_from_string("null")
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn bool(value: Bool) -> PgType {
  value
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn int(value: Int) -> PgType {
  value
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn float(value: Float) -> PgType {
  value
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn text(value: String) -> PgType {
  value
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn bytea(value: BitString) -> PgType {
  value
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn nullable(value: Option(a), mapper: fn(a) -> PgType) {
  case value {
    Some(term) -> mapper(term)
    None -> null()
  }
}

external fn run_query(
  Atom,
  String,
  List(PgType),
) -> Result(#(QueryCommand, Int, List(Dynamic)), QueryError) =
  "gleam_pgo_ffi" "query"

pub type QueryCommand {
  Insert
  Update
  Select
  Delete
}

// TODO: redesign errors
// https://www.postgresql.org/docs/8.1/errcodes-appendix.html
pub type QueryError {
  ConstrainError(message: String, constraint: String, detail: String)
  PgsqlError(message: String)
  WrongNumberOfArguments(expected: Int, given: Int)
  // This is unsatisfying
  Other(Dynamic)
}

// TODO: use a decoder
// TODO: return some better type
pub fn query(
  pool: Atom,
  sql: String,
  arguments: List(PgType),
) -> Result(#(QueryCommand, Int, List(Dynamic)), QueryError) {
  run_query(pool, sql, arguments)
}
