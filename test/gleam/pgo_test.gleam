import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/io
import gleam/pgo
import gleam/string
import gleam/should

pub fn url_config_test() {
    pgo.url_config("postgres://u:p@db.test:1234/my_db")
    |> should.equal(Ok([pgo.Host("db.test"), pgo.Port(1234), pgo.Database("my_db"), pgo.User("u"), pgo.Password("p")]))

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
  let Ok(conn) = start_default()
  let sql = "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)"

  let Ok(response) = pgo.query(conn, sql, [])

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([])
}

pub fn inserting_new_rows_and_returning_test() {
  let Ok(conn) = start_default()
  let sql = "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)
  RETURNING
    name"

  let Ok(response) = pgo.query(conn, sql, [])

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([dynamic.from(tuple("bill")), dynamic.from(tuple("felix"))])
}

pub fn selecting_rows_test() {
  let Ok(conn) = start_default()
  let sql = "
    INSERT INTO
      cats
    VALUES
      (DEFAULT, 'neo', true)
    RETURNING
      id"

  let Ok(tuple(_, _, [row])) = pgo.query(conn, sql, [])
  let Ok(id) = dynamic.element(row, 0)
  let Ok(id) = dynamic.int(id)
  let sql = string.append("SELECT * FROM cats WHERE id = ", int.to_string(id))

  let Ok(response) = pgo.query(conn, sql, [])

  response.0
  |> should.equal(pgo.Select)
  response.1
  |> should.equal(1)
  response.2
  |> should.equal([dynamic.from(tuple(id, "neo", True))])

  let sql = "SELECT * FROM cats WHERE id = $1"
  // Test same response when using interpolation
  pgo.query(conn, sql, [pgo.int(id)])
  |> should.equal(Ok(response))
}

pub fn invalid_sql_test() {
  let Ok(conn) = start_default()
  let sql = "S"
  let Error(pgo.PgsqlError(message)) = pgo.query(conn, sql, [])
  message
  |> should.equal("syntax error at or near \"S\"")
}

pub fn insert_constraint_error_test() {
  let Ok(conn) = start_default()
  let sql = "
    INSERT INTO
      cats
    VALUES
      (900, 'bill', true), (900, 'felix', false)"

  let Error(
    pgo.ConstrainError(message, constraint, detail),
  ) = pgo.query(conn, sql, [])
  message
  |> should.equal(
    "duplicate key value violates unique constraint \"cats_pkey\"",
  )
}

pub fn select_from_unknown_table_test() {
  let Ok(conn) = start_default()
  let sql = "SELECT * FROM unknown"

  let Error(pgo.PgsqlError(message)) = pgo.query(conn, sql, [])
  message
  |> should.equal("relation \"unknown\" does not exist")
}

pub fn insert_with_incorrect_type_test() {
  let Ok(conn) = start_default()
  let sql = "
      INSERT INTO
        cats
      VALUES
        (true, true, true)"

  let Error(pgo.PgsqlError(message)) = pgo.query(conn, sql, [])
  message
  |> should.equal(
    "column \"id\" is of type integer but expression is of type boolean",
  )
}

pub fn select_with_incorrect_type_test() {
  let Ok(conn) = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  assert Error(pgo.Other(_)) = pgo.query(conn, sql, [pgo.text("True")])
  Nil
}

pub fn query_with_wrong_number_of_arguments_test() {
  let Ok(conn) = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  pgo.query(conn, sql, [])
  |> should.equal(Error(pgo.WrongNumberOfArguments(1, 0)))
}
