import gleam_pgo
import gleam/expect

pub fn hello_world_test() {
  gleam_pgo.hello_world()
  |> expect.equal("Hello, from gleam_pgo!")
}
