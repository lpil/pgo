import gleam_experimental_pgo
import gleam/expect

pub fn hello_world_test() {
  gleam_experimental_pgo.hello_world()
  |> expect.equal(_, "Hello, from gleam_experimental_pgo!")
}
