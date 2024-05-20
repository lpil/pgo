-module(gleam_pgo_ffi).

-export([query/3, connect/1, disconnect/1, coerce/1, null/0]).

-record(pgo_pool, {name, pid}).

-include_lib("gleam_pgo/include/gleam@pgo_Config.hrl").
-include_lib("pg_types/include/pg_types.hrl").

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
        ssl_options = SslOptions,
        connection_parameters = ConnectionParameters,
        pool_size = PoolSize,
        queue_target = QueueTarget,
        queue_interval = QueueInterval,
        idle_interval = IdleInterval,
        trace = Trace,
        ip_version = IpVersion
    } = Config,
    Options1 = #{
        host => Host,
        port => Port,
        database => Database,
        user => User,
        ssl => Ssl,
        ssl_options => SslOptions,
        connection_parameters => ConnectionParameters,
        pool_size => PoolSize,
        queue_target => QueueTarget,
        queue_interval => QueueInterval,
        idle_interval => IdleInterval,
        trace => Trace,
        socket_options => case IpVersion of
            ipv4 -> [];
            ipv6 -> [inet6]
        end
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
        #{rows := Rows, num_rows := NumRows} ->
            {ok, {NumRows, Rows}};

        {error, Error} ->
            {error, convert_error(Error)}
    end.

convert_error(none_available) ->
    connection_unavailable;
convert_error({pgo_protocol, {parameters, Expected, Got}}) ->
    {unexpected_argument_count, Expected, Got};
convert_error({pgsql_error, #{
    message := Message, 
    constraint := Constraint, 
    detail := Detail
}}) ->
    {constraint_violated, Message, Constraint, Detail};
convert_error({pgsql_error, #{code := Code, message := Message}}) ->
    Constant = case gleam@pgo:error_code_name(Code) of
        {ok, X} -> X;
        {error, nil} -> <<"unknown">>
    end,
    {postgresql_error, Code, Constant, Message};
convert_error(#{
    error := badarg_encoding,
    type_info := #type_info{name = Expected},
    value := Value
}) ->
    Got = list_to_binary(io_lib:format("~p", [Value])),
    {unexpected_argument_type, Expected, Got}.
