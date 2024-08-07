import exception
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

pub fn url_config_alternative_postgres_protocol_test() {
  pgo.url_config("postgresql://u:p@db.test:1234/my_db")
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

fn default_config() {
  pgo.Config(
    ..pgo.default_config(),
    database: "gleam_pgo_test",
    password: Some("postgres"),
    pool_size: 1,
  )
}

pub fn inserting_new_rows_test() {
  let db = start_default()
  let sql =
    "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, 'bill', true, ARRAY ['black'], now(), '2020-03-04'),
    (DEFAULT, 'felix', false, ARRAY ['grey'], now(), '2020-03-05')"
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
    (DEFAULT, 'bill', true, ARRAY ['black'], now(), '2020-03-04'),
    (DEFAULT, 'felix', false, ARRAY ['grey'], now(), '2020-03-05')
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
      (DEFAULT, 'neo', true, ARRAY ['black'], '2022-10-10 11:30:30', '2020-03-04')
    RETURNING
      id"

  let assert Ok(pgo.Returned(rows: [id], ..)) =
    pgo.execute(sql, db, [], dynamic.element(0, dynamic.int))

  let assert Ok(returned) =
    pgo.execute(
      "SELECT * FROM cats WHERE id = $1",
      db,
      [pgo.int(id)],
      dynamic.tuple6(
        dynamic.int,
        dynamic.string,
        dynamic.bool,
        dynamic.list(dynamic.string),
        pgo.decode_timestamp,
        pgo.decode_date,
      ),
    )

  returned.count
  |> should.equal(1)
  returned.rows
  |> should.equal([
    #(id, "neo", True, ["black"], #(#(2022, 10, 10), #(11, 30, 30)), #(
      2020,
      3,
      4,
    )),
  ])

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
      (900, 'bill', true, ARRAY ['black'], now(), '2020-03-04'),
      (900, 'felix', false, ARRAY ['black'], now(), '2020-03-05')"

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

pub fn datetime_test() {
  start_default()
  |> assert_roundtrip(
    #(#(2022, 10, 12), #(11, 30, 33)),
    "timestamp",
    pgo.timestamp,
    pgo.decode_timestamp,
  )
  |> pgo.disconnect
}

pub fn date_test() {
  start_default()
  |> assert_roundtrip(#(2022, 10, 11), "date", pgo.date, pgo.decode_date)
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

pub fn expected_maps_test() {
  let db = pgo.Config(..default_config(), rows_as_map: True) |> pgo.connect

  let sql =
    "
    INSERT INTO
      cats
    VALUES
      (DEFAULT, 'neo', true, ARRAY ['black'], '2022-10-10 11:30:30', '2020-03-04')
    RETURNING
      id"

  let assert Ok(pgo.Returned(rows: [id], ..)) =
    pgo.execute(sql, db, [], dynamic.field("id", dynamic.int))

  let assert Ok(returned) =
    pgo.execute(
      "SELECT * FROM cats WHERE id = $1",
      db,
      [pgo.int(id)],
      dynamic.decode6(
        fn(id, name, is_cute, colors, last_petted_at, birthday) {
          #(id, name, is_cute, colors, last_petted_at, birthday)
        },
        dynamic.field("id", dynamic.int),
        dynamic.field("name", dynamic.string),
        dynamic.field("is_cute", dynamic.bool),
        dynamic.field("colors", dynamic.list(dynamic.string)),
        dynamic.field("last_petted_at", pgo.decode_timestamp),
        dynamic.field("birthday", pgo.decode_date),
      ),
    )

  returned.count
  |> should.equal(1)
  returned.rows
  |> should.equal([
    #(id, "neo", True, ["black"], #(#(2022, 10, 10), #(11, 30, 30)), #(
      2020,
      3,
      4,
    )),
  ])

  pgo.disconnect(db)
}

pub fn transaction_commit_test() {
  let db = start_default()
  let id_decoder = dynamic.element(0, dynamic.int)
  let assert Ok(_) = pgo.execute("truncate table cats", db, [], Ok)

  let insert = fn(db, name) {
    let sql = "
  INSERT INTO
    cats
  VALUES
    (DEFAULT, '" <> name <> "', true, ARRAY ['black'], now(), '2020-03-04')
  RETURNING id"
    let assert Ok(pgo.Returned(rows: [id], ..)) =
      pgo.execute(sql, db, [], id_decoder)
    id
  }

  // A succeeding transaction
  let assert Ok(#(id1, id2)) =
    pgo.transaction(db, fn(db) {
      let id1 = insert(db, "one")
      let id2 = insert(db, "two")
      Ok(#(id1, id2))
    })

  // An error returning transaction, it gets rolled back
  let assert Error(pgo.TransactionRolledBack("Nah bruv!")) =
    pgo.transaction(db, fn(db) {
      let _id1 = insert(db, "two")
      let _id2 = insert(db, "three")
      Error("Nah bruv!")
    })

  // A crashing transaction, it gets rolled back
  let _ =
    exception.rescue(fn() {
      pgo.transaction(db, fn(db) {
        let _id1 = insert(db, "four")
        let _id2 = insert(db, "five")
        panic as "testing rollbacks"
      })
    })

  let assert Ok(returned) =
    pgo.execute("select id from cats order by id", db, [], id_decoder)

  let assert [got1, got2] = returned.rows
  let assert True = id1 == got1
  let assert True = id2 == got2

  pgo.disconnect(db)
}
