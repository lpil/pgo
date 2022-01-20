-module(gleam_pgo_ffi).

-export([query/3, start_pool/1, stop_pool/1]).

-record(pgo_pool, {name, pid}).

start_pool(Options) ->
    Id = integer_to_list(erlang:unique_integer([positive])),
    PoolName = list_to_atom("gleam_pgo_pool_" ++ Id),
    case pgo_pool:start_link(PoolName, Options) of
        {ok, Pid} -> {ok, #pgo_pool{name = PoolName, pid = Pid}}
        % TODO: return suitable errors
    end.

stop_pool(#pgo_pool{pid = Pid}) ->
    erlang:exit(Pid, normal),
    nil.

query(#pgo_pool{name = Name}, Sql, Arguments) ->
    case pgo:query(Sql, Arguments, #{pool => Name}) of
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
