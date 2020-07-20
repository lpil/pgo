import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/string
import gleam/map.{Map}
import gleam/option.{Some}
import gleam/uri.{Uri}

pub external type Pid

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
pub external fn start_link(Atom, List(PoolConfig)) -> Result(Pid, Dynamic) =
  "pgo_pool" "start_link"

pub external type PgType

pub fn null() -> PgType {
  atom.create_from_string("nil")
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

pub fn query(pool: Atom, sql: String, arguments: List(PgType)) -> Result(Dynamic, Dynamic) {
  let query_options = map.from_list([tuple(atom.create_from_string("pool"), pool)])
  todo
}
