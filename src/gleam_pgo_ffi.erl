-module(gleam_pgo_ffi).

-export([query/3, connect/1, disconnect/1, coerce/1, null/0]).

-record(pgo_pool, {name, pid}).

-include_lib("gleam_pgo/include/gleam@pgo_Config.hrl").

null() ->
    null.

coerce(Value) ->
    Value.

connect(Config) ->
    Id = integer_to_list(erlang:unique_integer([positive])),
    PoolName = list_to_atom("gleam_pgo_pool_" ++ Id),
    #config{
        host = Host,
        port = Port,
        database = Database,
        user = User,
        password = Password,
        ssl = Ssl,
        connection_parameters = ConnectionParameters,
        pool_size = PoolSize,
        queue_target = QueueTarget,
        queue_interval = QueueInterval,
        idle_interval = IdleInterval,
        trace = Trace
    } = Config,
    Options1 = #{
        host => Host,
        port => Port,
        database => Database,
        user => User,
        ssl => Ssl,
        connection_parameters => ConnectionParameters,
        pool_size => PoolSize,
        queue_target => QueueTarget,
        queue_interval => QueueInterval,
        idle_interval => IdleInterval,
        trace => Trace
    },
    Options2 = case Password of
        {some, Pw} -> maps:put(password, Pw, Options1);
        none -> Options1
    end,
    {ok, Pid} = pgo_pool:start_link(PoolName, Options2),
    #pgo_pool{name = PoolName, pid = Pid}.

disconnect(#pgo_pool{pid = Pid}) ->
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
