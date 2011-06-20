-module(rabbit_exchange_type_riak).
-include("riak_exchange.hrl").
-behaviour(rabbit_exchange_type).

-define(EXCHANGE_TYPE_BIN, <<"x-riak">>).
-define(HOST, <<"host">>).
-define(PORT, <<"port">>).
-define(MAX_CLIENTS, <<"maxclients">>).
-define(TYPE, <<"type-module">>).
-define(BUCKET, <<"X-Riak-Bucket">>).
-define(KEY, <<"X-Riak-Key">>).

-rabbit_boot_step({?MODULE,
                   [{description, "exchange type riak"},
                    {mfa, {rabbit_registry, register, [exchange, ?EXCHANGE_TYPE_BIN, ?MODULE]}},
                    {requires, rabbit_registry},
                    {enables, kernel_ready}]}).

-export([
  description/0, 
  route/2
]).
-export([
  serialise_events/0,
  validate/1, 
  create/2, 
  recover/2, 
  delete/3, 
  add_binding/3, 
  remove_bindings/3,
  assert_args_equivalence/2
]).

description() ->
  [{name, ?EXCHANGE_TYPE_BIN}, {description, <<"exchange type Riak">>}].

serialise_events() -> 
  false.

validate(X) ->
  Exchange = exchange_type(X),
  Exchange:validate(X).
  
create(Tx, X = #exchange{name = #resource{virtual_host=_VirtualHost, name=_Name}, arguments = _Args}) ->
  XA = exchange_a(X),
  pg2:create(XA),
  
  case get_riak_client(X) of
    {ok, _Client} ->
      Exchange = exchange_type(X),
      Exchange:create(Tx, X);
    _ -> 
      error_logger:error_msg("Could not connect to Riak"),
      {error, "could not connect to riak"}
  end.

recover(X, _Bs) ->
  create(none, X).

delete(Tx, X, Bs) ->
  XA = exchange_a(X),
  pg2:delete(XA),
  Exchange = exchange_type(X),
  Exchange:delete(Tx, X, Bs).

add_binding(true, X, B) ->
  do_add_binding(X, B);
add_binding(false, _X, _B) ->
  ok.

remove_bindings(Tx, X, Bs) ->
  Exchange = exchange_type(X),
  Exchange:remove_bindings(Tx, X, Bs).

assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).
  
route(X=#exchange{name = #resource{virtual_host = _VirtualHost, name = Name}}, 
      D=#delivery{message = _Message0 = #basic_message{
          routing_keys = Routes, 
          content = Content0}}) ->
  #content{
    properties = _Props = #'P_basic'{ 
      content_type = ContentType, 
      headers = Headers, 
      reply_to = _ReplyTo
    },
    payload_fragments_rev = PayloadRev
  } = rabbit_binary_parser:ensure_content_decoded(Content0),

  case get_riak_client(X) of
    {ok, Client} ->
      % Convert payload to list, concat together
      Payload = lists:foldl(fun(Chunk, NewPayload) ->
        <<Chunk/binary, NewPayload/binary>>
      end, <<>>, PayloadRev),
      io:format("payload: ~p~n", [Payload]),

      lists:foldl(fun(Route, _) ->
        % Look for bucket from headers or default to exchange name
        Bucket = case lists:keyfind(?BUCKET, 1, Headers) of
          {?BUCKET, _, B} -> B;
                        _ -> Name
        end,
        % Look for key from headers or default to routing key
        Key = case lists:keyfind(?KEY, 1, Headers) of
          {?KEY, _, K} -> K;
                     _ -> Route
        end,

        % Insert or update everything
        io:format("storing message to url: /~s/~s~n", [Bucket, Key]),
        Obj0 = case riakc_pb_socket:get(Client, Bucket, Key) of
          {ok, OldObj} -> riakc_obj:update_value(OldObj, Payload, binary_to_list(ContentType));
                     _ -> riakc_obj:new(Bucket, Key, Payload, binary_to_list(ContentType))
        end,

        % Populate metadata from msg properties
        Obj1 = case lists:foldl(fun({PropKey, _Type, PropVal}, NewProps) ->
              case PropKey of
                <<"X-Riak-Bucket", _/binary>> -> NewProps;
                <<"X-Riak-Key", _/binary>>    -> NewProps;
                _                             -> [{<<"X-Riak-Meta-", PropKey/binary>>, PropVal} | NewProps]
              end
            end, [], Headers) of
             [] -> Obj0;
          CMeta -> riakc_obj:update_metadata(Obj0, dict:store(<<"X-Riak-Meta">>, CMeta, riakc_obj:get_update_metadata(Obj0)))
        end,

        % Insert/Update data
        _Result = riakc_pb_socket:put(Client, Obj1)
        % io:format("result: ~p~n", [Result])
      end, [], Routes);
    _Err -> 
      %io:format("err: ~p~n", [Err]),
      error_logger:error_msg("Could not connect to Riak")
  end,
  Exchange = exchange_type(X),
  Exchange:route(X, D).
  
do_add_binding(X, B) ->
  Exchange = exchange_type(X),
  Exchange:add_binding(true, X, B).
  
exchange_a(#exchange{name = #resource{virtual_host=VirtualHost, name=Name}}) ->
  list_to_atom(lists:flatten(io_lib:format("~s ~s", [VirtualHost, Name]))).
  
get_riak_client(X=#exchange{arguments = Args}) ->
  Host = case lists:keyfind(?HOST, 1, Args) of
     {_, _, H} -> binary_to_list(H);
             _ -> "127.0.0.1"
  end,
  Port = case lists:keyfind(?PORT, 1, Args) of
     {_, _, P} -> 
       {Pn, _} = string:to_integer(binary_to_list(P)),
       Pn;
             _ -> 8087
  end,
  MaxClients = case lists:keyfind(?MAX_CLIENTS, 1, Args) of
    {_, _, MC} -> 
      {MCn, _} = string:to_integer(binary_to_list(MC)),
      MCn;
             _ -> 5
  end,
  XA = exchange_a(X),

  try 
    case pg2:get_closest_pid(XA) of
      {error, _} -> create_riak_client(XA, Host, Port, MaxClients);
        PbClient -> 
          case riakc_pb_socket:ping(PbClient) of
            pong -> {ok, PbClient};
            _    -> 
              error_logger:error_report("Disconnected Riak client discarded."),
              pg2:leave(XA, PbClient),
              get_riak_client(X)
          end
    end
  catch
    _ -> create_riak_client(XA, Host, Port, MaxClients)
  end.

create_riak_client(XA, Host, Port, MaxClients) ->
  error_logger:info_report(io_lib:format("Starting ~p Riak PB clients to ~p:~p", [MaxClients, Host, Port])),
  case riakc_pb_socket:start_link(Host, Port) of
    {ok, PbClient} -> 
      pg2:join(XA, PbClient),
      case length(pg2:get_members(XA)) of
        S when (S < MaxClients) -> create_riak_client(XA, Host, Port, MaxClients);
        _ -> {ok, PbClient}
      end;
    Err -> Err
  end.

exchange_type(#exchange{ arguments=Args }) ->
  case lists:keyfind(?TYPE, 1, Args) of
    {?TYPE, _, Type} -> 
      case list_to_atom(binary_to_list(Type)) of
        rabbit_exchange_type_riak -> 
          error_logger:error_report("Cannot base a Riak exchange on a Riak exchange. An infinite loop would occur."),
          rabbit_exchange_type_topic;
        Else -> Else
      end;
    _ -> rabbit_exchange_type_topic
  end.