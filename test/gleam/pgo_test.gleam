import gleam/dynamic.{type Decoder}
import gleam/option.{None, Some}
import gleam/pgo
import gleam/string
import gleeunit/should

pub fn url_config_everything_test() {
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
}

pub fn url_config_not_postgres_protocol_test() {
  pgo.url_config("foo://u:p@db.test:1234/my_db")
  |> should.equal(Error(Nil))
}

pub fn url_config_no_password_test() {
  pgo.url_config("postgres://u@db.test:1234/my_db")
  |> should.equal(Ok(
    pgo.Config(
      ..pgo.default_config(),
      host: "db.test",
      port: 1234,
      database: "my_db",
      user: "u",
      password: None,
    ),
  ))
}

pub fn url_config_path_slash_test() {
  pgo.url_config("postgres://u:p@db.test:1234/my_db/foo")
  |> should.equal(Error(Nil))
}

fn start_default() {
  pgo.Config(
    ..pgo.default_config(),
    database: "gleam_pgo_test",
    password: Some("postgres"),
    pool_size: 1,
  )
  |> pgo.connect
}

pub fn inserting_new_rows_test() {
  let db = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true, ARRAY ['black']), (DEFAULT, 'felix', false, ARRAY ['grey'])"
  let assert Ok(returned) = pgo.execute(sql, db, [], dynamic.dynamic)

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
    (DEFAULT, 'bill', true, ARRAY ['black']), (DEFAULT, 'felix', false, ARRAY ['grey'])
  RETURNING
    name"
  let assert Ok(returned) =
    pgo.execute(sql, db, [], dynamic.element(0, dynamic.string))

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
      (DEFAULT, 'neo', true, ARRAY ['black'])
    RETURNING
      id"

  let assert Ok(pgo.Returned(rows: [id], ..)) =
    pgo.execute(sql, db, [], dynamic.element(0, dynamic.int))

  let assert Ok(returned) =
    pgo.execute(
      "SELECT * FROM cats WHERE id = $1",
      db,
      [pgo.int(id)],
      dynamic.tuple4(
        dynamic.int,
        dynamic.string,
        dynamic.bool,
        dynamic.list(dynamic.string),
      ),
    )

  returned.count
  |> should.equal(1)
  returned.rows
  |> should.equal([#(id, "neo", True, ["black"])])

  pgo.disconnect(db)
}

pub fn invalid_sql_test() {
  let db = start_default()
  let sql = "select       select"

  let assert Error(pgo.PostgresqlError(code, name, message)) =
    pgo.execute(sql, db, [], dynamic.dynamic)

  code
  |> should.equal("42601")
  name
  |> should.equal("syntax_error")
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
      (900, 'bill', true, ARRAY ['black']), (900, 'felix', false, ARRAY ['black'])"

  let assert Error(pgo.ConstraintViolated(message, constraint, detail)) =
    pgo.execute(sql, db, [], dynamic.dynamic)

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

  let assert Error(pgo.PostgresqlError(code, name, message)) =
    pgo.execute(on: db, query: sql, with: [], expecting: dynamic.dynamic)

  code
  |> should.equal("42P01")
  name
  |> should.equal("undefined_table")
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
        (true, true, true, true)"
  let assert Error(pgo.PostgresqlError(code, name, message)) =
    pgo.execute(sql, db, [], dynamic.dynamic)

  code
  |> should.equal("42804")
  name
  |> should.equal("datatype_mismatch")
  message
  |> should.equal(
    "column \"id\" is of type integer but expression is of type boolean",
  )

  pgo.disconnect(db)
}

pub fn execute_with_wrong_number_of_arguments_test() {
  let db = start_default()
  let sql = "SELECT * FROM cats WHERE id = $1"

  pgo.execute(sql, db, [], dynamic.dynamic)
  |> should.equal(Error(pgo.UnexpectedArgumentCount(expected: 1, got: 0)))

  pgo.disconnect(db)
}

fn assert_roundtrip(
  db: pgo.Connection,
  value: a,
  type_name: String,
  encoder: fn(a) -> pgo.Value,
  decoder: Decoder(a),
) -> pgo.Connection {
  pgo.execute(
    string.append("select $1::", type_name),
    db,
    [encoder(value)],
    dynamic.element(0, decoder),
  )
  |> should.equal(Ok(pgo.Returned(count: 1, rows: [value])))
  db
}

pub fn null_test() {
  let db = start_default()
  pgo.execute(
    "select $1",
    db,
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
  |> assert_roundtrip(10_000_000, "int", pgo.int, dynamic.int)
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
  |> assert_roundtrip(10_000_000.0, "float", pgo.float, dynamic.float)
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
  |> assert_roundtrip(<<"":utf8>>, "bytea", pgo.bytea, dynamic.bit_array)
  |> assert_roundtrip(<<"✨":utf8>>, "bytea", pgo.bytea, dynamic.bit_array)
  |> assert_roundtrip(
    <<"Hello, Joe!":utf8>>,
    "bytea",
    pgo.bytea,
    dynamic.bit_array,
  )
  |> assert_roundtrip(<<1>>, "bytea", pgo.bytea, dynamic.bit_array)
  |> assert_roundtrip(<<1, 2, 3>>, "bytea", pgo.bytea, dynamic.bit_array)
  |> pgo.disconnect
}

pub fn array_test() {
  let decoder = dynamic.list(dynamic.string)
  start_default()
  |> assert_roundtrip(["black"], "text[]", pgo.array, decoder)
  |> assert_roundtrip(["gray"], "text[]", pgo.array, decoder)
  |> assert_roundtrip(["gray", "black"], "text[]", pgo.array, decoder)
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
  pgo.execute("select $1::int", db, [pgo.float(1.2)], dynamic.int)
  |> should.equal(Error(pgo.UnexpectedArgumentType("int4", "1.2")))

  pgo.disconnect(db)
}

pub fn expected_return_type_test() {
  let db = start_default()
  pgo.execute("select 1", db, [], dynamic.element(0, dynamic.string))
  |> should.equal(
    Error(
      pgo.UnexpectedResultType([
        dynamic.DecodeError(expected: "String", found: "Int", path: ["0"]),
      ]),
    ),
  )

  pgo.disconnect(db)
}
