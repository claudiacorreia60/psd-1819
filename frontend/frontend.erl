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
    server(Investidores, Empresas).

server(Investidores, Empresas) ->
    case gen_tcp:listen(3000, [binary,{packet, 0}, {reuseaddr, true}, {active, true}]) of
        {ok, LSock} ->
            io:format("Frontend: iniciado"),
            spawn(fun() -> client_acceptor(LSock, Investidores, Empresas) end);
        _ -> io:format("Frontend: erro")
    end.

client_acceptor(LSock, Investidores, Empresas) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:fwrite("Nova conexão\n"),
    spawn(fun() -> client_acceptor(LSock, Investidores, Empresas) end),
    receive
        {tcp, Sock, Data} ->
            Msg = clientProtos:decode_msg(Data, 'Authentication'),
            Username = maps:get(username, Msg),
            Password = maps:get(password, Msg),
            case maps:is_key(Username, Investidores) and string:equal(maps:get(Username, Investidores), Password) of
                true ->
                    MsgR = #{result=>true, entity=>"investor"},
                    gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Result')),
                    client_handler(Sock, Username);
                false ->
                    case maps:is_key(Username, Empresas) and maps:get(Username, Empresas) == Password of
                        true ->
                            MsgR = #{result=>true, entity=>"company"},
                            gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Result')),
                            client_handler(Sock, Username);
                        false ->
                            MsgR = #{result=>false},
                            gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Result'))
                    end
            end;
        false ->
            MsgR = #{result=>false},
            gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Result')),
            io:fwrite("Protocolo desconhecido\n")
    end.

client_handler(Sock, Username) ->
    receive
        {tcp, Sock, Data} ->
            case decode_msg(Data, 'Bid') of
                pattern ->
                    body;
            end

    end.
