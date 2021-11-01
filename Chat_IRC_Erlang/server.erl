-module(server).
-define(Puerto, 1234).
-export([start/0,fin/1,receptor/1,atencionAlCliente/1, atencionAlCliente/2]).


-export([foreach/2, strip/1, strip/2]).

%% Saca el "0" al final de un binary
strip(B) ->
  strip(B, erlang:byte_size(B) - 1).

strip(_B, -1) ->
  <<>>;
strip(B, Idx) ->
  case binary:at(B, Idx) of
    0 -> strip(B, Idx - 1);
    _ -> binary:part(B, 0, Idx + 1)
  end.

%% Recorre la lista
foreach(_, []) -> ok;
foreach(F, [H]) -> F(H);
foreach(F, [H | T]) -> F(H), foreach(F, T).

%% start: Crear un socket, y ponerse a escuchar.
start()->
    {ok, Socket} = gen_tcp:listen(?Puerto
                                 , [ binary, {active, false}]),
    clientes:iniciar(),
    spawn(?MODULE,receptor, [Socket]),
    Socket.

fin(Socket) ->
  case clientes:listaDeIds() of
    [] ->
      clientes:finalizar(),
      gen_tcp:close(Socket);
    _ ->
      io:format("Para apagar el servidor se deben desconectar todos los clientes conectados~n"),
      foreach(fun({_, SocketClient}) -> gen_tcp:send(SocketClient, <<"[SRV]: necesitamos cerrar, por favor desconectarse\n">>) end, clientes:listaDeIds())
  end,
  ok.

%% receptor: Espera a los clientes y crea nuevos actores para atender los pedidos.
%%
receptor(Socket) ->
        case gen_tcp:accept(Socket) of
            {ok, CSocket}  ->
                spawn(?MODULE, atencionAlCliente,[CSocket]);
            {error, closed} ->
                io:format("Se cerró el Servidor "),
                exit(normal);
            {error, Reason} ->
                io:format("Falló la espera del client por: ~p~n",[Reason])
        end,
        receptor(Socket).

%% Recibe al nuevo cliente.
atencionAlCliente(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Paquete} ->
      Nickname = strip(Paquete),
      clientes:nuevoNombre(Nickname, Socket),
      atencionAlCliente(Socket, Nickname);
    {error, closed} ->
      io:format("El cliente cerró la conexión~n")
  end.

% Atiende las peticiones del cliente.
atencionAlCliente(Socket, Nick) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Paquete} ->
      case comandos:comandos(Paquete) of
        {exit, _} ->
          io:format("[~p]Salida cliente~n",[Nick]),
          clientes:remover(Nick),
          gen_tcp:send(Socket, "/exit\n"),
          exit(normaly);
        {msg, error} ->
          gen_tcp:send(Socket, "error en coman MSG: Revisar el comando\n");
        {msg, NickPriv, Msg} ->
          io:format("[~p] mensaje privado para: ~p~n",[Nick, NickPriv]),
          Separador = <<": ">>,
          PaquetePrivado = <<Nick/binary, Separador/binary, Msg/binary>>,
          case clientes:quienEs(NickPriv) of
            notfound -> gen_tcp:send(Socket, "Usuario no esta encontrado\n");
            SendToID -> gen_tcp:send(SendToID, PaquetePrivado)
          end;
        {nick, NuevoNombre} ->
          io:format("[~p]Cambio de nombre~n",[Nick]),
          case clientes:cambiarNombre(string:trim(NuevoNombre, trailing, "\n\0"), Nick) of
            enuso ->
              gen_tcp:send(Socket, <<"[SRV] Nick en uso! intente con otro\n">>);
            ok ->
              gen_tcp:send(Socket, <<"[SRV] Nick actualizado\n">>),
              atencionAlCliente(Socket, NuevoNombre);
            E ->
              io:format("Error cambio de nick: ~p~n",[E]),
              gen_tcp:send(Socket, <<"[SRV] error interno\n">>)
          end;
        {commandError, Msg} ->
          io:format("[~p]Comando desconocido~n",[Nick]),
          gen_tcp:send(Socket, <<"[SRV] Comando deconocido ", Msg/binary>>);
        {all, Msg} ->
          Separador = <<": ">>,
          PaqueteNuevo = <<Nick/binary, Separador/binary, Paquete/binary>>,
          io:format("[~p] Mensaje publico: ~p ~n",[Nick, PaqueteNuevo]),
          foreach( fun({_, SocketClient}) -> gen_tcp:send(SocketClient, PaqueteNuevo) end, clientes:listaDeIds())
      end,
      atencionAlCliente(Socket, Nick);
    {error, closed} ->
      io:format("El cliente cerró la conexión~n")
   end.
