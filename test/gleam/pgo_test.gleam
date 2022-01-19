import gleam/erlang/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/io
import gleam/pgo
import gleam/string
import gleeunit/should

pub fn url_config_test() {
  pgo.url_config("postgres://u:p@db.test:1234/my_db")
  |> should.equal(Ok([
    pgo.Host("db.test"),
    pgo.Port(1234),
    pgo.Database("my_db"),
    pgo.User("u"),
    pgo.Password("p"),
  ]))

  pgo.url_config("foo://u:p@db.test:1234/my_db")
  |> should.equal(Error(Nil))

  pgo.url_config("postgres://u@db.test:1234/my_db")
  |> should.equal(Error(Nil))

  pgo.url_config("postgres://u:p@db.test:1234/my_db/foo")
  |> should.equal(Error(Nil))
}

external fn ensure_all_started(Atom) -> Result(List(Atom), Dynamic) =
  "application" "ensure_all_started"

fn start_default() {
  assert Ok(_) = ensure_all_started(atom.create_from_string("pgo"))
  let default_atom = atom.create_from_string("default")
  let config = [pgo.Host("localhost"), pgo.Database("gleam_pgo_test")]
  let _ = pgo.start_link(atom.create_from_string("default"), config)
  Ok(default_atom)
}

pub fn inserting_new_rows_test() {
  assert Ok(conn) = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)"
  assert Ok(response) = pgo.query(conn, sql, [])
  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([])
}

pub fn inserting_new_rows_and_returning_test() {
  assert Ok(conn) = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)
  RETURNING
    name"
  assert Ok(response) = pgo.query(conn, sql, [])
  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([dynamic.from(#("bill")), dynamic.from(#("felix"))])
}

pub fn selecting_rows_test() {
  assert Ok(conn) = start_default()
  let sql =
    "
    INSERT INTO
      cats
    VALUES
      (DEFAULT, 'neo', true)
    RETURNING
      id"
  assert Ok(#(_, _, [row])) = pgo.query(conn, sql, [])
  assert Ok(id) = dynamic.element(0, dynamic.int)(row)
  let sql = string.append("SELECT * FROM cats WHERE id = ", int.to_string(id))
  assert Ok(response) = pgo.query(conn, sql, [])
  response.0
  |> should.equal(pgo.Select)
  response.1
  |> should.equal(1)
  response.2
  |> should.equal([dynamic.from(#(id, "neo", True))])
  let sql = "SELECT * FROM cats WHERE id = $1"
  // Test same response when using interpolation
  pgo.query(conn, sql, [pgo.int(id)])
  |> should.equal(Ok(response))
}

pub fn invalid_sql_test() {
  assert Ok(conn) = start_default()
  let sql = "select       select"
  assert Error(pgo.PgsqlError(message)) = pgo.query(conn, sql, [])
  message
  |> should.equal("syntax error at or near \"select\"")
}

pub fn insert_constraint_error_test() {
  assert Ok(conn) = start_default()
  let sql =
    "
    INSERT INTO
      cats
    VALUES
      (900, 'bill', true), (900, 'felix', false)"

  assert Error(pgo.ConstrainError(message, constraint, detail)) =
    pgo.query(conn, sql, [])

  constraint
  |> should.equal("cats_pkey")

  detail
  |> should.equal("Key (id)=(900) already exists.")

  message
  |> should.equal(
    "duplicate key value violates unique constraint \"cats_pkey\"",
  )
}

pub fn select_from_unknown_table_test() {
  assert Ok(conn) = start_default()
  let sql = "SELECT * FROM unknown"
  assert Error(pgo.PgsqlError(message)) = pgo.query(conn, sql, [])
  message
  |> should.equal("relation \"unknown\" does not exist")
}

pub fn insert_with_incorrect_type_test() {
  assert Ok(conn) = start_default()
  let sql =
    "
      INSERT INTO
        cats
      VALUES
        (true, true, true)"
  assert Error(pgo.PgsqlError(message)) = pgo.query(conn, sql, [])
  message
  |> should.equal(
    "column \"id\" is of type integer but expression is of type boolean",
  )
}

pub fn select_with_incorrect_type_test() {
  assert Ok(conn) = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"
  assert Error(pgo.Other(_)) = pgo.query(conn, sql, [pgo.text("True")])
  Nil
}

pub fn query_with_wrong_number_of_arguments_test() {
  assert Ok(conn) = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"
  pgo.query(conn, sql, [])
  |> should.equal(Error(pgo.WrongNumberOfArguments(1, 0)))
}
