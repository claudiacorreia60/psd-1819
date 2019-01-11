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
    Enderecos = #{
      "apple"=>1,
      "ibm"=>1,    
      "google"=>2,
      "primavera"=>2,
      "edp"=>3,
      "farfetch"=>3
     },
    server(Investidores, Empresas, Enderecos).

server(Investidores, Empresas, Enderecos) ->
    case gen_tcp:listen(3000, [binary,{packet, 0}, {reuseaddr, true}, {active, true}]) of
        {ok, LSock} ->
            io:format("Frontend: iniciado"),
            spawn(fun() -> client_acceptor(LSock, Investidores, Empresas, Enderecos) end);
        _ -> io:format("Frontend: erro")
    end.

client_acceptor(LSock, Investidores, Empresas, Enderecos) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:fwrite("Nova conexão\n"),
    spawn(fun() -> client_acceptor(LSock, Investidores, Empresas, Enderecos) end),
    receive
        {tcp, Sock, Data} ->
            Msg = maps:get(auth, clientProtos:decode_msg(Data, 'Message')),
            Username = maps:get(username, Msg),
            Password = maps:get(password, Msg),
            case maps:is_key(Username, Investidores) and string:equal(maps:get(Username, Investidores), Password) of
                true ->
                    MsgR = #{res=>#{result=>true, entity=>"investor"}},
                    gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Message')),
                    Sockets = connect_to_exchanges(),
                    client_handler(Sock, Enderecos, Sockets);
                false ->
                    case maps:is_key(Username, Empresas) and maps:get(Username, Empresas) == Password of
                        true ->
                            MsgR = #{res=>#{result=>true, entity=>"company"}},
                            gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Message')),
                            Sockets = connect_to_exchanges(),
                            client_handler(Sock, Enderecos, Sockets);
                        false ->
                            MsgR = #{res=>#{result=>false}},
                            gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Message'))
                    end
            end;
        false ->
            MsgR = #{res=>#{result=>false}},
            gen_tcp:send(Sock, clientProtos:encode_msg(MsgR, 'Message')),
            io:fwrite("Protocolo desconhecido\n")
    end.

connect_to_exchanges() ->
    application:start(chumak),
    {ok, Socket1} = chumak:socket(req, "exchange1"),
    chumak:bind(Socket1, tcp, "localhost", 5551),
    {ok, Socket2} = chumak:socket(req, "exchange2"),
    chumak:bind(Socket2, tcp, "localhost", 5552),
    {ok, Socket3} = chumak:socket(req, "exchange3"),
    chumak:bind(Socket3, tcp, "localhost", 5553),
    {Socket1, Socket2, Socket3}.

client_handler(Sock, Enderecos, Sockets) ->
    receive
        {tcp, Sock, Data} ->
            Msg = clientProtos:decode_msg(Data, 'Message'),
            case maps:is_key(company,Msg) of
                true -> 
                    route_to_exchange(maps:get(company,Msg), Data, Enderecos, Sockets, Sock);
                false -> 
                    io:fwrite("No company!\n"),
                    client_handler(Sock, Enderecos, Sockets)
            end;
        Data ->
            gen_tcp:send(Sock, Data),
            client_handler(Sock, Enderecos, Sockets)
    end.

route_to_exchange(Company, Data, Enderecos, Sockets, Sock) ->
    {Socket1, Socket2, Socket3} = Sockets,
    case maps:get(Company, Enderecos) of
        1 -> send_messages(Socket1, Data, Sock);
        2 -> send_messages(Socket2, Data, Sock);
        3 -> send_messages(Socket3, Data, Sock)
    end.

send_messages(Socket, Data, Sock) ->
    case chumak:send(Socket, Data) of
        ok ->
            io:format("Send message: ~p\n", [Data]);
        {error, Reason} ->
            io:format("Failed to send message: ~p, reason: ~p\n", [Data, Reason])
    end,
    case chumak:recv(Socket) of
        {ok, RecvMessage} ->
            io:format("Recv message: ~p\n", [RecvMessage]),
            gen_tcp:send(Sock, RecvMessage);
        {error, RecvReason} ->
            io:format("Failed to recv, reason: ~p\n", [RecvReason])
    end.
