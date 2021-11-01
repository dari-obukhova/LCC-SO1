-module(clientes).

%% Creación y eliminación del servicio
-export([iniciar/0, finalizar/0]).

%% Servidor
-export([loopClientes/2, loopClientes/1]).

%% Librería de Acceso
-export([nuevoNombre/2, quienEs/1, listaDeIds/0, remover/1, cambiarNombre/2]).

%% Creación y eliminación del servicio
iniciar() ->
  Clientes = spawn(?MODULE, loopClientes,[maps:new(), self()]),
  register(clientes, Clientes),
  ark().

ark()->
  receive
   servOk ->
     ok;
    servErr ->
     err
   end.


finalizar() ->
  clientes ! {finalizar, self()},
  unregister(clientes),
  ark().

%Libreria de Acceso
nuevoNombre(Nombre, Pid) ->
  clientes ! {index, Nombre, Pid, self()},
  ark().

quienEs(Nombre) ->
  clientes ! {buscar, Nombre, self()},
  receive
    {encontrado, Pid} -> Pid;
    {error, noencontrado} -> notfound
  end.

listaDeIds() ->
  clientes ! {lista, self()},
  receive
    {ok, Lista} -> Lista;
    _ -> error
  end.

remover(Nombre) ->
  clientes ! {rm, Nombre, self()},
  ark().

cambiarNombre(NuevoNombre, ViejoNombre) ->
  case quienEs(NuevoNombre) of
    notfound ->
      case quienEs(ViejoNombre) of
        notfound ->
          nolistado;
        Pid ->
          remover(ViejoNombre),
          nuevoNombre(NuevoNombre, Pid)
      end;
    _ -> enuso
  end.

%%Servidor
loopClientes(Map, Pid) ->
  Pid ! servOk,
  loopClientes(Map).

loopClientes(Map) ->
  receive
    {index, Nombre, Pid, Ask} ->
      Ask ! servOk,
      loopClientes(maps:put(Nombre, Pid, Map));
    {buscar, Nombre, Ask} ->
      case maps:find(Nombre, Map) of
        {ok, Pid} ->
          Ask ! {encontrado, Pid};
        error ->
          Ask ! {error, noencontrado}
      end,
      loopClientes(Map);
    {lista, Ask} ->
      Ask ! {ok, maps:to_list(Map)},
      loopClientes(Map);
    {rm, Nombre, Ask} ->
      Ask ! servOk,
      loopClientes(maps:remove(Nombre, Map));
    {finalizar, Ask } ->
      Ask ! servOk;
    A ->
      io:format("Se resivio un comando desconocido ~p~n", [A]),
    loopClientes(Map)
  end.
