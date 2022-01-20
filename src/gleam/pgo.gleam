//// Postgresql client
////
//// Gleam wrapper around pgo library

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

pub type Config {
  Config(
    /// (default: 127.0.0.1): Database server hostname.
    host: String,
    /// (default: 5432): Port the server is listening on.
    port: Int,
    /// Name of database to use.
    database: String,
    /// Username to connect to database as.
    user: String,
    /// Password for the user.
    password: Option(String),
    /// (default: false): Whether to use SSL or not.
    ssl: Bool,
    /// (default: []): List of 2-tuples, where key and value must be binary
    /// strings. You can include any Postgres connection parameter here, such as
    /// `#("application_name", "myappname")` and `#("timezone", "GMT")`.
    connection_parameters: List(#(String, String)),
    /// (default: 1): Number of connections to keep open with the database
    pool_size: Int,
    /// (default: 50) Checking out connections is handled through a queue. If it
    /// takes longer than queue_target to get out of the queue for longer than
    /// queue_interval then the queue_target will be doubled and checkouts will
    /// start to be dropped if that target is surpassed.
    queue_target: Int,
    /// (default: 1000)
    queue_interval: Int,
    /// (default: 1000): The database is pinged every idle_interval when the
    /// connection is idle.
    idle_interval: Int,
    /// trace (default: false): pgo is instrumented with [OpenCensus][1] and
    /// when this option is true a span will be created (if sampled).
    ///
    /// [1]: https://opencensus.io/
    trace: Bool,
  )
}

pub fn default_config() -> Config {
  Config(
    host: "127.0.0.1",
    port: 5432,
    database: "postgres",
    user: "postgres",
    password: None,
    ssl: False,
    connection_parameters: [],
    pool_size: 1,
    queue_target: 50,
    queue_interval: 1000,
    idle_interval: 1000,
    trace: False,
  )
}

/// Parse a database url into an option list that can be used to start a pool.
pub fn url_config(database_url: String) -> Result(Config, Nil) {
  try uri = uri.parse(database_url)
  try #(userinfo, host, path, db_port) = case uri {
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
  try #(user, password) = string.split_once(userinfo, ":")
  case string.split(path, "/") {
    ["", database] ->
      Ok(
        Config(
          ..default_config(),
          host: host,
          port: db_port,
          database: database,
          user: user,
          password: Some(password),
        ),
      )
    _ -> Error(Nil)
  }
}

pub external type ConnectionPool

// TODO: warn that it will crash if a connection cannot be established
pub external fn start_pool(Config) -> ConnectionPool =
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
