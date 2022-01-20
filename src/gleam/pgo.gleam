//// Postgresql client
////
//// Gleam wrapper around pgo library

// TODO: pool shutdown function
// TODO: config is a record
// TODO: remove dynamic usage for arguments
// TODO: refine errors
// TODO: use a decode for returned rows?
// TODO: return a record from query
// TODO: transactions
// TODO: json support
import gleam/erlang/atom
import gleam/dynamic.{Dynamic}
import gleam/string
import gleam/io
import gleam/option.{None, Option, Some}
import gleam/uri.{Uri}

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
    Ok(Uri(
      scheme: Some("postgres"),
      userinfo: Some(userinfo),
      host: Some(host),
      port: Some(db_port),
      path: path,
      ..,
    )) -> Ok(#(userinfo, host, path, db_port))
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

pub external type ConnectionPool

// TODO: warn that it will crash if a connection cannot be established
pub external fn start_pool(List(PoolConfig)) -> ConnectionPool =
  "gleam_pgo_ffi" "start_pool"

pub external fn stop_pool(ConnectionPool) -> Nil =
  "gleam_pgo_ffi" "stop_pool"

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
  ConnectionPool,
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

// https://www.postgresql.org/docs/8.1/errcodes-appendix.html
pub type QueryError {
  ConstrainError(message: String, constraint: String, detail: String)
  PgsqlError(message: String)
  WrongNumberOfArguments(expected: Int, given: Int)
  // This is unsatisfying
  Other(Dynamic)
}

pub fn query(
  pool: ConnectionPool,
  sql: String,
  arguments: List(PgType),
) -> Result(#(QueryCommand, Int, List(Dynamic)), QueryError) {
  run_query(pool, sql, arguments)
}
