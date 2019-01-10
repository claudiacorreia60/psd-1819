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
  io:fwrite("Conexão nova!\n"),
  spawn(fun() -> client_acceptor(LSock, Investidores, Empresas) end),
  receive
    {line, Data} ->
      Tokens = string:split(Data, ":", all),
      case lists:nth(1,Tokens) of
        "authentication" ->
          Username = lists:nth(2, Tokens),
          Password = lists:nth(3, Tokens),
          case maps:is_key(Username, Investidores) and maps:get(Username, Investidores) == Password of
            false -> gen_tcp:send(Sock, "authentication:fail");
            true ->
              gen_tcp:send(Sock, "authentication:client:success"),
              client_handler(Sock, Username)
          end,
          case maps:is_key(Username, Empresas) and maps:get(Username, Empresas) == Password of
            false -> gen_tcp:send(Sock, "authentication:fail");
            true ->
              gen_tcp:send(Sock, "authentication:company:success"),
              client_handler(Sock, Username)
          end;
        _ -> io:fwrite("Protocolo desconhecido")
      end
  end.

client_handler(Sock, Username) ->
  io:fwrite("client_handler").