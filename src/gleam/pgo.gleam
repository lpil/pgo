import gleam/result
import gleam/dynamic.{Dynamic}
import gleam/atom.{Atom}
import gleam/otp/process.{Pid, UnknownMessage}

external fn display(a) -> a =
  "erlang" "display"

/// https://github.com/erleans/pgo#options
pub type Option {
  Host(String)
  Post(Int)
  Database(String)
  Password(String)

  User(String)
  PoolSize(Int)
  QueueTarget(Int)
  QueueInterval(Int)
  IdleInterval(Int)

  Trace(Bool)
}

pub external type Pool

external type CompiledOptions

external fn pool_from_name(Atom) -> Pool =
  "gleam@dynamic" "unsafe_coerce"

external fn compile_options(List(Option)) -> CompiledOptions =
  "maps" "from_list"

external fn erl_start_link(
  Atom,
  CompiledOptions,
) -> Result(Pid(UnknownMessage), Nil) =
  "pgo_pool" "start_link"

pub fn start_link(
  named name: Atom,
  using options: List(Option),
) -> Result(tuple(Pid(UnknownMessage), Pool), Nil) {
  let x = options
    |> compile_options
    |> erl_start_link(name, _)
    |> result.map(fn(pid) { tuple(pid, pool_from_name(name)) })
  display(x)
  x
}

type QueryOption {
  Pool(Pool)
  Queue(Bool)
}

external type CompiledQueryOptions

external fn compile_query_options(List(QueryOption)) -> CompiledQueryOptions =
  "maps" "from_list"

pub type QueryResultField {
  Rows
}

pub type QueryError {
  // TODO
  QueryError(String)
}

pub external type Argument

external fn erl_query(String, List(Argument), CompiledQueryOptions) -> Dynamic =
  "pgo" "query"

pub fn query(
  run sql: String,
  on pool: Pool,
) -> Result(List(Dynamic), QueryError) {
  let options = compile_query_options([Pool(pool)])
  display(sql)
  display(options)
  let return = erl_query(sql, [], options)
  let rows = return
    |> dynamic.field(Rows)
    |> result.map(dynamic.unsafe_coerce)

  case rows {
    Ok(rows) -> Ok(rows)
    Error(x) -> Error(QueryError(x))
  }
}
