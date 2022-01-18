-module(gleam_pgo_ffi).

-export([map_response/1]).

map_response(#{
    command := Command,
    num_rows := NumRows,
    rows := Rows
}) ->
    {Command, NumRows, Rows}.
