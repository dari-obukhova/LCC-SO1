-module(ledger).
-export([append/2, remove/2]).
-export([start/1, stop/0, distributedLedger/3, preDistributedLedger/0]).

%%Agrego

%% list functions

%% agregar elemento a la lista
append(Elem, []) ->
  [Elem];
append(Elem, [Head | Tail]) ->
  [Head | append(Elem, Tail)].

%% eliminar elemento de la lista
remove(X, L) ->
  [Y || Y <- L, Y =/= X].


start(Nodes) ->
 atomic:start(Nodes),
 register(distributedLedger, spawn(?MODULE, preDistributedLedger, [])),
 ok.

preDistributedLedger() ->
 atomic:register(),
 distributedLedger([], [], []),
 ok.


stop() ->
 atomic:stop(),
 distributedLedger ! fin,
 unregister(distributedLedger),
 ok.

%%Gpending - lista de pendientes
%%Agregar: lista de los nodos que brinda el servicio de ledger, cantidad de nodos criticos).

distributedLedger(GPending, Pending, LedgerList) ->
  receive
    {nodedown, _} ->
      exit(0);
    fin ->
      exit(0);
    {get, C, Pid} ->
      atomic:send({deliver_get, C, Pid}),
      distributedLedger(append({Pid,C}, GPending), Pending, LedgerList);
    {deliver_get, C, Pid} ->
      Result = lists:any(fun(X) -> X == {Pid, C} end, GPending),
      if
        Result ->
          Pid ! {C, getres, LedgerList},
          distributedLedger(remove({Pid, C}, GPending), Pending, LedgerList);
        true ->
          distributedLedger(GPending, Pending, LedgerList)
       end;
    {append, C, Pid, R} ->
      atomic:send({deliver_append, C, Pid, R}),
      distributedLedger(GPending, append({C,R,Pid}, Pending), LedgerList);
    %%  Pending = append(Pending, {C,R,Pid});
    {deliver_append, C, Pid, R} ->
      Result = lists:any(fun(X) -> X == R end, LedgerList),
      if
        Result ->
          Pid ! {C, noAppendres},
          distributedLedger(GPending, Pending, LedgerList);
        true ->
          NewLedgerList = LedgerList ++ [R],
          Result2 = lists:any(fun(X) -> X == {C,R,Pid} end, Pending),
          if
            Result2 ->
              Pid ! {C, appendres},
              distributedLedger(GPending, (remove({C,R,Pid}, Pending)), NewLedgerList);
            true ->
              Pid ! {C, noAppendres},
              distributedLedger(GPending, Pending, LedgerList)
          end
      end;
      _ ->   io:format("ERROR~n"),distributedLedger(GPending, Pending, LedgerList)
  end.
