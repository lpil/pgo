//// Postgresql client
////
//// Gleam wrapper around pgo library

// TODO: refine errors
// TODO: return a record from query
// TODO: transactions
// TODO: json support
// TODO: docs
import gleam/erlang/atom
import gleam/dynamic.{DecodeErrors, Decoder, Dynamic}
import gleam/string
import gleam/result
import gleam/list
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

pub external type Connection

pub external fn connect(Config) -> Connection =
  "gleam_pgo_ffi" "connect"

pub external fn disconnect(Connection) -> Nil =
  "gleam_pgo_ffi" "disconnect"

/// A value that can be sent to PostgreSQL as one of the arguments to a
/// parameterised SQL query.
pub external type Value

pub external fn null() -> Value =
  "gleam_pgo_ffi" "null"

pub external fn bool(Bool) -> Value =
  "gleam_pgo_ffi" "coerce"

pub external fn int(Int) -> Value =
  "gleam_pgo_ffi" "coerce"

pub external fn float(Float) -> Value =
  "gleam_pgo_ffi" "coerce"

pub external fn text(String) -> Value =
  "gleam_pgo_ffi" "coerce"

pub external fn bytea(BitString) -> Value =
  "gleam_pgo_ffi" "coerce"

pub fn nullable(inner_type: fn(a) -> Value, value: Option(a)) -> Value {
  case value {
    Some(term) -> inner_type(term)
    None -> null()
  }
}

external fn run_query(
  Connection,
  String,
  List(Value),
) -> Result(#(Int, List(Dynamic)), QueryError) =
  "gleam_pgo_ffi" "query"

// https://www.postgresql.org/docs/8.1/errcodes-appendix.html
pub type QueryError {
  ConstrainError(message: String, constraint: String, detail: String)
  PgsqlError(message: String)
  WrongNumberOfArguments(expected: Int, given: Int)
  UnexpectedResult(DecodeErrors)
  // This is unsatisfying
  Other(Dynamic)
}

pub fn query(
  pool: Connection,
  sql: String,
  arguments: List(Value),
  decoder: Decoder(t),
) -> Result(#(Int, List(t)), QueryError) {
  try #(count, rows) = run_query(pool, sql, arguments)
  try rows =
    list.try_map(over: rows, with: decoder)
    |> result.map_error(UnexpectedResult)
  Ok(#(count, rows))
}

// TODO: test
pub fn execute(
  pool: Connection,
  sql: String,
  arguments: List(Value),
) -> Result(Int, QueryError) {
  try #(count, _rows) = run_query(pool, sql, arguments)
  Ok(count)
}
