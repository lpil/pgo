-module(gleam_pgo_ffi).

-export([query/3]).

query(Pool, Sql, Arguments) ->
    case pgo:query(Sql, Arguments, #{pool => Pool}) of
        #{command := Command, rows := Rows, num_rows := NumRows} ->
            {ok, {Command, NumRows, Rows}};

        {error, Error} ->
            {error, convert_error(Error)}
        
    end.

convert_error({pgo_protocol, {parameters, Expected, Got}}) ->
    {wrong_number_of_arguments, Expected, Got};
convert_error({pgsql_error, #{
    message := Message, 
    constraint := Constraint, 
    detail := Detail
}}) ->
    {constrain_error, Message, Constraint, Detail};
convert_error({pgsql_error, #{message := Message}}) ->
    {pgsql_error, Message};
convert_error(Other) ->
    {other, Other}.
