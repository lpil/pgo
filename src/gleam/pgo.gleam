//// Postgresql client
////
//// Gleam wrapper around pgo library

import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/string
import gleam/io
import gleam/map.{Map}
import gleam/option.{Some}
import gleam/uri.{Uri}

/// Type representing the PID for a pool process
/// NOTE this type should be switched to a general PID type when one is available
pub external type Pid

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

/// Parse a database url into an option list that can be used to start a pool.
pub fn url_config(database_url: String) -> Result(List(PoolConfig), Nil) {
  case uri.parse(database_url) {
    Ok(
      Uri(
        scheme: Some("postgres"),
        userinfo: Some(userinfo),
        host: Some(host),
        port: Some(db_port),
        path: path,
        ..,
      ),
    ) -> case string.split_once(userinfo, ":") {
      Ok(tuple(user, password)) -> case string.split(path, "/") {
        [
          "",
          database,
        ] -> Ok(
          [
            Host(host),
            Port(db_port),
            Database(database),
            User(user),
            Password(password),
          ],
        )
      }
    }
  }
}

// Ideally unnamed
// TODO a dynamic.pid function
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

fn command_from_atom(command: Atom) -> Result(QueryCommand, Atom) {
  let insert_atom = atom.create_from_string("insert")
  let update_atom = atom.create_from_string("update")
  let select_atom = atom.create_from_string("select")
  let delete_atom = atom.create_from_string("delete")
  case command {
    command if command == insert_atom -> Ok(Insert)
    command if command == update_atom -> Ok(Update)
    command if command == select_atom -> Ok(Select)
    command if command == delete_atom -> Ok(Delete)
    other -> Error(other)
  }
}

fn map_response(query_result) {
  assert Ok(command) = atom_field(query_result, "command")
  assert Ok(command) = dynamic.atom(command)
  assert Ok(command) = command_from_atom(command)

  assert Ok(rows) = atom_field(query_result, "rows")
  assert Ok(rows) = dynamic.list(rows)

  assert Ok(num_rows) = atom_field(query_result, "num_rows")
  assert Ok(num_rows) = dynamic.int(num_rows)

  tuple(command, num_rows, rows)
}

/// Run a SQL query with the given arguments
pub fn query(
  pool: Atom,
  sql: String,
  arguments: List(PgType),
) -> Result(tuple(QueryCommand, Int, List(Dynamic)), QueryError) {
  let query_options = map.from_list(
    [tuple(atom.create_from_string("pool"), pool)],
  )
  let query_result = erl_query(sql, arguments, query_options)

  let error_atom = dynamic.from(atom.create_from_string("error"))
  case dynamic.element(query_result, 0) {
    Ok(tag) if tag == error_atom -> {
      assert Ok(reason) = dynamic.element(query_result, 1)
      let pgsql_error_atom = dynamic.from(
        atom.create_from_string("pgsql_error"),
      )
      let pgo_protocol_atom = dynamic.from(
        atom.create_from_string("pgo_protocol"),
      )
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
