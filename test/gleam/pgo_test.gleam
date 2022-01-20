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
  |> pgo.start_pool
}

pub fn inserting_new_rows_test() {
  let pool = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)"
  assert Ok(response) = pgo.query(pool, sql, [], dynamic.dynamic)

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal([])

  pgo.stop_pool(pool)
}

pub fn inserting_new_rows_and_returning_test() {
  let pool = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true), (DEFAULT, 'felix', false)
  RETURNING
    name"
  assert Ok(response) =
    pgo.query(pool, sql, [], dynamic.element(0, dynamic.string))

  response.0
  |> should.equal(pgo.Insert)
  response.1
  |> should.equal(2)
  response.2
  |> should.equal(["bill", "felix"])

  pgo.stop_pool(pool)
}

pub fn selecting_rows_test() {
  let pool = start_default()
  let sql =
    "
    INSERT INTO
      cats
    VALUES
      (DEFAULT, 'neo', true)
    RETURNING
      id"

  assert Ok(#(_, _, [id])) =
    pgo.query(pool, sql, [], dynamic.element(0, dynamic.int))

  assert Ok(response) =
    pgo.query(
      pool,
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

  pgo.stop_pool(pool)
}

pub fn invalid_sql_test() {
  let pool = start_default()
  let sql = "select       select"

  assert Error(pgo.PgsqlError(message)) =
    pgo.query(pool, sql, [], dynamic.dynamic)

  message
  |> should.equal("syntax error at or near \"select\"")

  pgo.stop_pool(pool)
}

pub fn insert_constraint_error_test() {
  let pool = start_default()
  let sql =
    "
    INSERT INTO
      cats
    VALUES
      (900, 'bill', true), (900, 'felix', false)"

  assert Error(pgo.ConstrainError(message, constraint, detail)) =
    pgo.query(pool, sql, [], dynamic.dynamic)

  constraint
  |> should.equal("cats_pkey")

  detail
  |> should.equal("Key (id)=(900) already exists.")

  message
  |> should.equal(
    "duplicate key value violates unique constraint \"cats_pkey\"",
  )

  pgo.stop_pool(pool)
}

pub fn select_from_unknown_table_test() {
  let pool = start_default()
  let sql = "SELECT * FROM unknown"

  assert Error(pgo.PgsqlError(message)) =
    pgo.query(pool, sql, [], dynamic.dynamic)

  message
  |> should.equal("relation \"unknown\" does not exist")

  pgo.stop_pool(pool)
}

pub fn insert_with_incorrect_type_test() {
  let pool = start_default()
  let sql =
    "
      INSERT INTO
        cats
      VALUES
        (true, true, true)"
  assert Error(pgo.PgsqlError(message)) =
    pgo.query(pool, sql, [], dynamic.dynamic)

  message
  |> should.equal(
    "column \"id\" is of type integer but expression is of type boolean",
  )

  pgo.stop_pool(pool)
}

pub fn select_with_incorrect_type_test() {
  let pool = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  assert Error(pgo.Other(_)) =
    pgo.query(pool, sql, [pgo.text("True")], dynamic.dynamic)

  pgo.stop_pool(pool)
}

pub fn query_with_wrong_number_of_arguments_test() {
  let pool = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  pgo.query(pool, sql, [], dynamic.dynamic)
  |> should.equal(Error(pgo.WrongNumberOfArguments(1, 0)))

  pgo.stop_pool(pool)
}
