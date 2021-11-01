-module(client).
-export([start/1, preContador/3, contador/3, get/0, append/1]).

start(Nodes) ->
  register(cont, spawn(?MODULE, preContador,[0, Nodes, round(length(Nodes) / 2)])),
  on.

preContador(C, Nodes, Critical) ->
 lists:foreach(fun(X) -> monitor_node(X, true) end, Nodes),
 contador(C, Nodes, Critical).

contador(C, Nodes, Critical) ->
  receive
    {info, Pid} ->
      if
        Critical > 0 ->  Pid ! {reInfo, C, Nodes};
        true -> Pid ! error, io:format("No hay suficientes nodos conectados~n")
      end;
    {nodedown, Node} ->
      contador(C, remove(Node, Nodes), Critical-1);
    _ ->
      io:format("trash~n")
  end,
 contador(C+1, Nodes, Critical).

respuestaLedgerGet(error)-> error;
respuestaLedgerGet(N)->
  receive
    {N, getres, V} -> io:format("Recibido: ~p ~p~n",[V, N]), V;
    error -> error;
     _ -> respuestaLedgerGet(N)
  end.

respuestaContGet() ->
  receive
    {reInfo, C, Nodes} ->
     lists:foreach(fun (X) -> {distributedLedger, X} ! {get, C, self()} end, Nodes),
     C;
    error -> error;
     _ -> respuestaContGet()
  end.

get() ->
 cont ! {info, self()},
 respuestaLedgerGet(respuestaContGet()).

respuestaLedgerAppend(error)->
  error;
respuestaLedgerAppend(N)->
  receive
    {N, appendres} -> ok;
    {N, noAppendres} -> nok;
    error -> error;
     _ -> respuestaLedgerGet(N)
  end.

respuestaContApp(Elem) ->
  receive
    {reInfo, C, Nodes} ->
     lists:foreach(fun (X) -> {distributedLedger, X} ! {append, C, self(), Elem} end, Nodes),
     C;
    error -> error;
     _ -> respuestaContApp(Elem)
  end.

append(Elem) ->
  cont ! {info, self()},
  respuestaLedgerAppend(respuestaContApp(Elem)).

remove(_, []) -> [];
remove(Obj, [HEAD | TAIL]) ->
  if
    HEAD == Obj ->
      TAIL;
    true ->
      [HEAD] ++ remove(Obj, TAIL)
  end.
