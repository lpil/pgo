import gleam/pgo
import gleam/atom
import gleam/dynamic
import gleam/should.{Expectation}

pub type EUnitSuite(state) {
  Setup(
    start: fn() -> state,
    stop: fn(state) -> Nil,
    tests: fn(state) -> List(fn() -> Expectation),
  )
}

fn start() {
  assert name = atom.create_from_string("test_pool")
  let opts = [
      pgo.Database("gleam_pgo_test"),
      pgo.Host("127.0.0.1"),
      pgo.PoolSize(5),
      pgo.User("postgres"),
      pgo.Password("postgres"),
    ]
  assert Ok(tuple(_pid, pool)) = pgo.start_link(name, opts)
  pool
}

fn hello_world() -> Expectation {
  1
  |> should.equal(1)
}

fn select_constant(pool) -> Expectation {
  "SELECT 1"
  |> pgo.query(pool)
  |> should.equal(Ok([dynamic.from(1)]))
}

pub fn suite_test_() {
  Setup(
    start: start,
    stop: fn(_) { Nil },
    tests: fn(pool) { [hello_world, fn() { select_constant(pool) }] },
  )
}
