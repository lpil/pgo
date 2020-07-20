import gleam/atom.{Atom}
import gleam/io
import gleam/pgo
import gleam/should

external fn sleep(Int) -> Atom = "timer" "sleep"

pub fn fetching_values_test() {
  let config = [
      pgo.Host("localhost"),
      pgo.Port(5432),
      pgo.Database("gleam_pgo_test"),
      pgo.User("postgres"),
      pgo.Password("postgres"),
      pgo.Ssl(False)
    ]
  assert Ok(pid) = pgo.start_link(atom.create_from_string("default"), config)

  sleep(100)
  // assert Ok(
  //   result,
  // ) = pgo.query(atom.create_from_string("default"), "SELECT 42", [])
  // io.debug(result)
  todo
}
