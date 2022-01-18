//// Postgresql client
////
//// Gleam wrapper around pgo library

import gleam/erlang/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/string
import gleam/io
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import gleam/uri.{Uri}
import gleam/otp/process.{Pid}

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

external fn erl_query(String, List(PgType), Map(Atom, Atom)) -> Dynamic =
  "pgo" "query"

pub type QueryCommand {
  Insert
  Update
  Select
  Delete
}

pub type QueryError {
  ConstrainError(message: String, constraint: String, detail: String)
  PgsqlError(message: String)
  WrongNumberOfArguments(expected: Int, given: Int)
  // This is unsatisfying
  Other(Dynamic)
}

fn atom_field(data: Dynamic, key: String) {
  dynamic.field(data, atom.create_from_string(key))
}

external fn map_response(Dynamic) -> #(QueryCommand, Int, List(Dynamic)) =
  "gleam_pgo_ffi" "map_response"

/// Run a SQL query with the given arguments
pub fn query(
  pool: Atom,
  sql: String,
  arguments: List(PgType),
) -> Result(#(QueryCommand, Int, List(Dynamic)), QueryError) {
  let query_options = map.from_list([#(atom.create_from_string("pool"), pool)])
  let query_result = erl_query(sql, arguments, query_options)

  let error_atom = dynamic.from(atom.create_from_string("error"))
  case dynamic.element(query_result, 0) {
    Ok(tag) if tag == error_atom -> {
      assert Ok(reason) = dynamic.element(query_result, 1)
      let pgsql_error_atom =
        dynamic.from(atom.create_from_string("pgsql_error"))
      let pgo_protocol_atom =
        dynamic.from(atom.create_from_string("pgo_protocol"))
      case dynamic.element(reason, 0) {
        Ok(tag) if tag == pgsql_error_atom -> {
          assert Ok(details) = dynamic.element(reason, 1)
          assert Ok(message) = atom_field(details, "message")
          assert Ok(message) = dynamic.string(message)
          case atom_field(details, "constraint") {
            Ok(constraint) -> {
              assert Ok(constraint) = dynamic.string(constraint)
              assert Ok(detail) = atom_field(details, "detail")
              assert Ok(detail) = dynamic.string(detail)
              Error(ConstrainError(message, constraint, detail))
            }
            Error(_) -> Error(PgsqlError(message))
          }
        }
        Ok(tag) if tag == pgo_protocol_atom -> {
          assert Ok(details) = dynamic.element(reason, 1)
          // Check parameters is atom in error
          assert Ok(expected) = dynamic.element(details, 1)
          assert Ok(given) = dynamic.element(details, 2)
          assert Ok(expected) = dynamic.int(expected)
          assert Ok(given) = dynamic.int(given)
          Error(WrongNumberOfArguments(expected, given))
        }
        Error(_) -> Error(Other(reason))
      }
    }
    Error(_) -> Ok(map_response(query_result))
  }
}
