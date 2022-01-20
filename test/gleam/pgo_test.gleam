import gleam/dynamic
import gleam/int
import gleam/pgo
import gleam/option.{Some}
import gleam/string
import gleeunit/should

pub fn url_config_test() {
  pgo.url_config("postgres://u:p@db.test:1234/my_db")
  |> should.equal(Ok(
    pgo.Config(
      ..pgo.default_config(),
      host: "db.test",
      port: 1234,
      database: "my_db",
      user: "u",
      password: Some("p"),
    ),
  ))

  pgo.url_config("foo://u:p@db.test:1234/my_db")
  |> should.equal(Error(Nil))

  pgo.url_config("postgres://u@db.test:1234/my_db")
  |> should.equal(Error(Nil))

  pgo.url_config("postgres://u:p@db.test:1234/my_db/foo")
  |> should.equal(Error(Nil))
}

fn start_default() {
  pgo.Config(..pgo.default_config(), database: "gleam_pgo_test", pool_size: 1)
  |> pgo.connect
}

pub fn inserting_new_rows_test() {
  let db = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)"
  assert Ok(response) = pgo.query(db, sql, [], dynamic.dynamic)

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([])

  pgo.disconnect(db)
}

pub fn inserting_new_rows_and_returning_test() {
  let db = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)
  RETURNING
    name"
  assert Ok(response) =
    pgo.query(db, sql, [], dynamic.element(0, dynamic.string))

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal(["bill", "felix"])

  pgo.disconnect(db)
}

pub fn selecting_rows_test() {
  let db = start_default()
  let sql =
    "
    INSERT INTO
      cats
    VALUES
      (DEFAULT, 'neo', true)
    RETURNING
      id"

  assert Ok(#(_, _, [id])) =
    pgo.query(db, sql, [], dynamic.element(0, dynamic.int))

  assert Ok(response) =
    pgo.query(
      db,
      "SELECT * FROM cats WHERE id = $1",
      [pgo.int(id)],
      dynamic.tuple3(dynamic.int, dynamic.string, dynamic.bool),
    )

  response.0
  |> should.equal(pgo.Select)
  response.1
  |> should.equal(1)
  response.2
  |> should.equal([#(id, "neo", True)])

  pgo.disconnect(db)
}

pub fn invalid_sql_test() {
  let db = start_default()
  let sql = "select       select"

  assert Error(pgo.PgsqlError(message)) =
    pgo.query(db, sql, [], dynamic.dynamic)

  message
  |> should.equal("syntax error at or near \"select\"")

  pgo.disconnect(db)
}

pub fn insert_constraint_error_test() {
  let db = start_default()
  let sql =
    "
    INSERT INTO
      cats
    VALUES
      (900, 'bill', true), (900, 'felix', false)"

  assert Error(pgo.ConstrainError(message, constraint, detail)) =
    pgo.query(db, sql, [], dynamic.dynamic)

  constraint
  |> should.equal("cats_pkey")

  detail
  |> should.equal("Key (id)=(900) already exists.")

  message
  |> should.equal(
    "duplicate key value violates unique constraint \"cats_pkey\"",
  )

  pgo.disconnect(db)
}

pub fn select_from_unknown_table_test() {
  let db = start_default()
  let sql = "SELECT * FROM unknown"

  assert Error(pgo.PgsqlError(message)) =
    pgo.query(db, sql, [], dynamic.dynamic)

  message
  |> should.equal("relation \"unknown\" does not exist")

  pgo.disconnect(db)
}

pub fn insert_with_incorrect_type_test() {
  let db = start_default()
  let sql =
    "
      INSERT INTO
        cats
      VALUES
        (true, true, true)"
  assert Error(pgo.PgsqlError(message)) =
    pgo.query(db, sql, [], dynamic.dynamic)

  message
  |> should.equal(
    "column \"id\" is of type integer but expression is of type boolean",
  )

  pgo.disconnect(db)
}

pub fn select_with_incorrect_type_test() {
  let db = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  assert Error(pgo.Other(_)) =
    pgo.query(db, sql, [pgo.text("True")], dynamic.dynamic)

  pgo.disconnect(db)
}

pub fn query_with_wrong_number_of_arguments_test() {
  let db = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  pgo.query(db, sql, [], dynamic.dynamic)
  |> should.equal(Error(pgo.WrongNumberOfArguments(1, 0)))

  pgo.disconnect(db)
}
