import gleam/dynamic.{Decoder}
import gleam/int
import gleam/pgo
import gleam/option.{None, Some}
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
  assert Ok(returned) = pgo.query(db, sql, [], dynamic.dynamic)

  returned.count
  |> should.equal(2)
  returned.rows
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
  assert Ok(returned) =
    pgo.query(db, sql, [], dynamic.element(0, dynamic.string))

  returned.count
  |> should.equal(2)
  returned.rows
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

  assert Ok(pgo.Returned(rows: [id], ..)) =
    pgo.query(db, sql, [], dynamic.element(0, dynamic.int))

  assert Ok(returned) =
    pgo.query(
      db,
      "SELECT * FROM cats WHERE id = $1",
      [pgo.int(id)],
      dynamic.tuple3(dynamic.int, dynamic.string, dynamic.bool),
    )

  returned.count
  |> should.equal(1)
  returned.rows
  |> should.equal([#(id, "neo", True)])

  pgo.disconnect(db)
}

pub fn invalid_sql_test() {
  let db = start_default()
  let sql = "select       select"

  assert Error(pgo.PgsqlError(code, message)) =
    pgo.query(db, sql, [], dynamic.dynamic)

  code
  |> should.equal("42601")
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

  assert Error(pgo.ConstraintViolated(message, constraint, detail)) =
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

  assert Error(pgo.PgsqlError(code, message)) =
    pgo.query(db, sql, [], dynamic.dynamic)

  code
  |> should.equal("42P01")
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
  assert Error(pgo.PgsqlError(code, message)) =
    pgo.query(db, sql, [], dynamic.dynamic)

  code
  |> should.equal("42804")
  message
  |> should.equal(
    "column \"id\" is of type integer but expression is of type boolean",
  )

  pgo.disconnect(db)
}

pub fn query_with_wrong_number_of_arguments_test() {
  let db = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  pgo.query(db, sql, [], dynamic.dynamic)
  |> should.equal(Error(pgo.UnexpectedArgumentCount(expected: 1, got: 0)))

  pgo.disconnect(db)
}

pub fn execute_returns_nothing_test() {
  let db = start_default()
  assert Ok(_) = pgo.execute(db, "delete from cats", [])

  let sql =
    "
  insert into
    cats
  values
    (default, 'bill', true), (default, 'felix', false)"
  assert Ok(2) = pgo.execute(db, sql, [])

  let sql = "select * from cats"
  assert Ok(2) = pgo.execute(db, sql, [])
}

fn assert_roundtrip(
  db: pgo.Connection,
  value: a,
  type_name: String,
  encoder: fn(a) -> pgo.Value,
  decoder: Decoder(a),
) -> pgo.Connection {
  pgo.query(
    db,
    string.append("select $1::", type_name),
    [encoder(value)],
    dynamic.element(0, decoder),
  )
  |> should.equal(Ok(pgo.Returned(count: 1, rows: [value])))
  db
}

pub fn null_test() {
  let db = start_default()
  pgo.query(
    db,
    "select $1",
    [pgo.null()],
    dynamic.element(0, dynamic.optional(dynamic.int)),
  )
  |> should.equal(Ok(pgo.Returned(count: 1, rows: [None])))

  pgo.disconnect(db)
}

pub fn bool_test() {
  start_default()
  |> assert_roundtrip(True, "bool", pgo.bool, dynamic.bool)
  |> assert_roundtrip(False, "bool", pgo.bool, dynamic.bool)
  |> pgo.disconnect
}

pub fn int_test() {
  start_default()
  |> assert_roundtrip(0, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(1, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(2, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(3, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(4, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(5, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(-0, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(-1, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(-2, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(-3, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(-4, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(-5, "int", pgo.int, dynamic.int)
  |> assert_roundtrip(10000000, "int", pgo.int, dynamic.int)
  |> pgo.disconnect
}

pub fn float_test() {
  start_default()
  |> assert_roundtrip(0.123, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(1.123, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(2.123, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(3.123, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(4.123, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(5.123, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(-0.654, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(-1.654, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(-2.654, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(-3.654, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(-4.654, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(-5.654, "float", pgo.float, dynamic.float)
  |> assert_roundtrip(10000000.0, "float", pgo.float, dynamic.float)
  |> pgo.disconnect
}

pub fn text_test() {
  start_default()
  |> assert_roundtrip("", "text", pgo.text, dynamic.string)
  |> assert_roundtrip("✨", "text", pgo.text, dynamic.string)
  |> assert_roundtrip("Hello, Joe!", "text", pgo.text, dynamic.string)
  |> pgo.disconnect
}

pub fn bytea_test() {
  start_default()
  |> assert_roundtrip(<<"":utf8>>, "bytea", pgo.bytea, dynamic.bit_string)
  |> assert_roundtrip(<<"✨":utf8>>, "bytea", pgo.bytea, dynamic.bit_string)
  |> assert_roundtrip(
    <<"Hello, Joe!":utf8>>,
    "bytea",
    pgo.bytea,
    dynamic.bit_string,
  )
  |> assert_roundtrip(<<1>>, "bytea", pgo.bytea, dynamic.bit_string)
  |> assert_roundtrip(<<1, 2, 3>>, "bytea", pgo.bytea, dynamic.bit_string)
  |> pgo.disconnect
}

pub fn nullable_test() {
  start_default()
  |> assert_roundtrip(
    Some("Hello, Joe"),
    "text",
    pgo.nullable(pgo.text, _),
    dynamic.optional(dynamic.string),
  )
  |> assert_roundtrip(
    None,
    "text",
    pgo.nullable(pgo.text, _),
    dynamic.optional(dynamic.string),
  )
  |> assert_roundtrip(
    Some(123),
    "int",
    pgo.nullable(pgo.int, _),
    dynamic.optional(dynamic.int),
  )
  |> assert_roundtrip(
    None,
    "int",
    pgo.nullable(pgo.int, _),
    dynamic.optional(dynamic.int),
  )
  |> pgo.disconnect
}

pub fn expected_argument_type_test() {
  let db = start_default()
  pgo.query(db, "select $1::int", [pgo.float(1.2)], dynamic.int)
  |> should.equal(Error(pgo.UnexpectedArgumentType("int4", "1.2")))

  pgo.disconnect(db)
}

pub fn expected_return_type_test() {
  let db = start_default()
  pgo.query(db, "select 1", [], dynamic.element(0, dynamic.string))
  |> should.equal(Error(pgo.UnexpectedResultType([
    dynamic.DecodeError(expected: "String", found: "Int", path: ["0"]),
  ])))

  pgo.disconnect(db)
}
