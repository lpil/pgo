import gleam_pgo
import gleam/should

pub fn hello_world_test() {
  gleam_pgo.hello_world()
  |> should.equal("Hello, from gleam_pgo!")
}
