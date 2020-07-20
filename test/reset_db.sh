#!/bin/sh

set -eu

echo
echo Resetting database

psql -h localhost -U postgres <<SQL
SELECT 'CREATE DATABASE gleam_pgo_test'
WHERE NOT EXISTS (
  SELECT FROM pg_database WHERE datname = 'gleam_pgo_test'
)\\gexec
SQL

psql -v "ON_ERROR_STOP=1" -h localhost -U postgres -d gleam_pgo_test <<SQL
DROP TABLE IF EXISTS cats;
CREATE TABLE cats (
  id INTEGER PRIMARY KEY,
  name VARCHAR(50) NOT NULL,
  is_cute boolean NOT NULL DEFAULT true
);
SQL

echo Done
echo
