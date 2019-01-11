%%%-------------------------------------------------------------------
%%% @author bolt
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 2:38 PM
%%%-------------------------------------------------------------------
-module('frontend').

%% API
-export([start/0]).

start() ->
  Investidores = #{
    "armando"=>"armando123",
    "ana"=>"ana123",
    "claudia"=>"claudia123",
    "felipe"=>"felipe123",
    "daniela"=>"daniela123"
    },
  Empresas = #{
    "apple"=>"apple123",
    "ibm"=>"ibm123",
    "google"=>"google123",
    "primavera"=>"primavera123",
    "edp"=>"edp123",
    "farfetch"=>"farfetch123"
    },
  io:fwrite("Frontend server starting...\n"),
  server(Investidores, Empresas).

server(Investidores, Empresas) ->
  {ok, LSock} = gen_tcp:listen(1231, [binary, {packet, line}]),
  spawn(fun() -> client_acceptor(LSock, Investidores, Empresas) end).

client_acceptor(LSock, Investidores, Empresas) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  io:fwrite("Nova conexão\n"),
  spawn(fun() -> client_acceptor(LSock, Investidores, Empresas) end),
  receive
    {tcp, Sock, Data} ->
      Msg = #{result=>true, entity=>"client"},
      gen_tcp:send(Sock, clientProtos:encode_msg(Msg, 'Result')),

      Msg = clientProtos:decode_msg(Data, 'Authentication'),
      Username = maps:get(username, Msg),
      Password = maps:get(password, Msg),
      case maps:is_key(Username, Investidores) and string:equal(maps:get(Username, Investidores), Password) of
        true ->
          Msg = #{result=>true, entity=>"client"},
          gen_tcp:send(Sock, clientProtos:encode_msg(Msg, 'Result')),
          client_handler(Sock, Username);
        false ->
          Msg = #{result=>false},
          gen_tcp:send(Sock, clientProtos:encode_msg(Msg, 'Result'))
      end,
      case maps:is_key(Username, Empresas) and maps:get(Username, Empresas) == Password of
        false ->
          Msg = #{result=>false},
          gen_tcp:send(Sock, clientProtos:encode_msg(Msg, 'Result'));
        true ->
          Msg = #{result=>true, entity=>"company"},
          gen_tcp:send(Sock, clientProtos:encode_msg(Msg, 'Result')),
          client_handler(Sock, Username)
      end;
    false ->
      Msg = #{result=>false},
      gen_tcp:send(Sock, clientProtos:encode_msg(Msg, 'Result')),
      io:fwrite("Protocolo desconhecido\n")
  end.

client_handler(Sock, Username) ->
  io:fwrite("client_handler").