-module(print).
-export([print/0, preprintLoop/0]).
-export([print/1, preprintLoop/1]).


print(Nodo) ->
  spawn(?MODULE, preprintLoop, [Nodo]).

preprintLoop(Nodo) ->
  atomic:register(Nodo),
  printLoop().

print() ->
  spawn(?MODULE, preprintLoop, []).

preprintLoop() ->
  atomic:register(),
  printLoop().

printLoop() ->
  receive
    M ->
      io:format("print: ~p~n",[M])
  end,
  printLoop().
