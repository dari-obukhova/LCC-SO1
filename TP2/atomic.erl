% net_adm:ping('nodo2@alumno-virtual-machine').
% c(ledger), c(atomic), ledger:start(nodes()).
%
-module(atomic).
-include("struct.hrl").
-export([start/1,stop/0]).
-export([preDestLoop/1, destLoop/5, preSenderLoop/1, senderLoop/2, deliverLoop/1]).
-export([send/1, send/2, register/0, register/1, unRegister/0, unRegister/1]).

%Inicia el servicio de broadcast localmente.
start(Nodos) ->
    register(dest, spawn(?MODULE, preDestLoop,[Nodos])),
    register(deliver, spawn(?MODULE, deliverLoop,[[]])),
    register(sender, spawn(?MODULE, preSenderLoop,[Nodos])),
    servON.

stop() ->
    dest ! fin,       % Cierra el proceso que recibe mensajes
    deliver ! fin,    % Cierra el proceso que entrega los mensajes
    sender ! fin,     % Cierra el proceso que envia los mensajes
    unregister(dest),
    unregister(sender),
    unregister(deliver),
    serverOFF.

%% Manda el mensaje con una prioridad propuesta a senderLoop
receivePriority(Msg) ->
  receive
    {reLocalPriority, LocalPriority} ->
      sender ! #send{msg = Msg, sender = self(), priority = LocalPriority};
    _ ->
      receivePriority(Msg)
  end.

confirmSend() ->
  receive
    send -> send;
    _ -> confirmSend()
  end.

send(Msg) ->
  dest ! {localPriority, self()},
  receivePriority(Msg),
  confirmSend().

%% Manda el mensaje con una prioridad propuesta a senderLoop
%% Además se puede colocar un nodo para entrar al broadcast
send(Msg, Node) ->
  monitor_node(Node, true),
  {dest, Node} ! {localPriority, self()},
  receive
    {nodedown, Node} ->
      self() ! {nodedown, Node};
    LocalPriority ->
      {sender, Node} ! #send{msg = Msg, sender = self(), priority = LocalPriority}
  end,
  receive
    send -> send;
    {nodedown, _} -> error
  end.

%Registro de un proceso en el servicio de broadcast para recibir lo que se envie
%por el servicio de broadcast

%Registro asumiendo que te estas registrando localmente
register() ->
  deliver ! {registerP, self()},
  ok.

%Registro en otro nodo
register(Node) ->
  monitor_node(Node, true),
  {deliver, Node} ! {registerP, self()},
  ok.

%Sacar de la lista de deliver a un proceso, después de esto se dejaran de
%resivir mensaje de la lista de broadcast

%darse de baja cuando estas registrando en el mismo nodo que corre broadcast
unRegister() ->
  deliver ! {unregisterP, self()},
  ok.

%darse de baja cuando estas registrando en otro nodo
unRegister(Node) ->
  {deliver, Node} ! {unregisterP, self()},
  ok.
%%!! Después unregistrar receiveLoop !!!!

%% Recibe prioridades desde los nodos. Calcula la prioridad maxima.
%% Caso base: receiveLoop(0, Max) -> manda mensaje a los nodos con la prioridad maxima calculada.


% Recopila las respuestas de prioridades de todos los nodos de la red de broadcast
% y devuelve la mayor de estas respuestas y los nodos que pudieron responder
answer(Prop, [], List) -> {Prop, List};
answer(Prop, Nodes, List) ->
  receive
    {nodedown, Node} ->
      monitor_node(Node, false),
      answer(Prop, remove(Node, Nodes), remove(Node, List));
    {Node, Answer} ->
      T = find(Node, Nodes),
      if  T ->
          answer(max(Answer, Prop), remove(Node, Nodes), List ++ [Node]);
        true ->
          answer(Prop, Nodes,List)
      end
  end.

%%Prepara todo para poder empezar a envar mensajes
preSenderLoop(Nodes) ->
  SafeNodes = remove(node(), Nodes),
  lists:foreach(fun(X) -> monitor_node(X, true) end, SafeNodes),
  senderLoop(0, SafeNodes).

%% Manda mensajes a todos los nodos. (Tipo Broadcast)
broadcast(Msg, Nodos) ->
  lists:foreach(fun (X) -> {dest, X} ! Msg end, Nodos).

senderLoop(N, Nodes) ->
  receive
    fin ->                    % Finaliza el servicio
      lists:foreach(fun (X) -> {sender, X} ! {nodedown, node()} end, Nodes),
      lists:foreach(fun(X) -> monitor_node(X, false) end, Nodes),
      exit(success);
    {nodedown, Node} ->       % Se cae un nodo que abocado al serv. de broadcast
      monitor_node(Node, false),
      senderLoop(N, remove(Node, Nodes));
    M when is_record(M, send) ->
      broadcast(#tocheck{msg = M#send.msg,
                        sender = {node(), N},
                        priority = M#send.priority},
                Nodes),
      {Answer, Live} = answer(M#send.priority, Nodes,[]),
      Msg = #tosend{msg = M#send.msg,
                    sender = {node(), N},
                    priority = Answer},
      broadcast(Msg, Nodes),
      dest ! {localSend, Msg},
      M#send.sender ! send,
      senderLoop(N+1, Live);
    _ ->                      % Posibles casos no contemplados.
        senderLoop(N, Nodes)
    end.


posibles_perdidads([], _, List) -> List;
posibles_perdidads([Head | Tail], Node, List) ->
  case Head of
    {{Node, _}, Msg} ->
      posibles_perdidads(Tail, Node, List ++ [Msg]);
    _ -> posibles_perdidads(Tail, Node, List)
  end.

%% Recibe mensajes
%% depende del status:
%% tocheck -> agrega mensaje a la cola de prioridad, le asigna una prioridad,
%             manda esta prioridad a receiveLoop
%% tosend -> agrega mensaje a la cola de delivery para mandar después

%% Todo esto hay ue manejarlo con identificador de mensajes me imagino

%% destLoop (NPrior --> la prioridad (la última prioridad en la Cola de Prioridad + 1),
%% NDel --> núm de mensaje mandado, no decidi todavia si sire o no,
%% PQueue --> cola de prioridad
%% DQueue --> cola de delivery
%% TO --> time out

preDestLoop(Nodes) ->
  SafeNodes = remove(node(), Nodes),
  lists:foreach(fun(X) -> monitor_node(X, true) end, SafeNodes),
  destLoop(1, 1, dict:new(), dict:new(), 0).

destLoop(NPrior, NDel, PQueue, DQueue, TO) ->
  receive
    fin ->
      exit(success);
    {nodedown, Node} ->
      monitor_node(Node, false),
      case posibles_perdidads(dict:to_list(DQueue), Node, []) of
        [] ->
          destLoop(NPrior, NDel, PQueue, DQueue, TO);
        _ ->
          destLoop(NPrior, NDel, PQueue, DQueue, TO)
      end;
    {localPriority, Pid} ->               % Petición de prioridad para proponer
      Pid ! {reLocalPriority, NDel},
      destLoop(NPrior+1, NDel, PQueue, DQueue, TO);
    Data when is_record(Data, tocheck) -> % Petición de prioridad
      {Nodo, _} = Data#tocheck.sender,
      {sender, Nodo} ! {node(), NDel},
      destLoop(NPrior+1,
              NDel,
              PQueue,
              dict:append(Data#tocheck.sender, Data, DQueue),
              0);
    Data when is_record(Data, tosend) ->  % Petición de colocar en cola de envío
      destLoop(NPrior,
              NDel,
              dict:append(Data#tosend.priority, Data, PQueue),
              dict:erase(Data#tosend.sender,DQueue), %DQueue,
              0);
    {localSend, Data} ->                  % Petición de colocar en cola de envío
                                          % Local
      if
        Data#tosend.priority < NDel ->
          deliver ! [Data];
        true ->
          ok
      end,
      destLoop(NPrior,
              NDel,
              dict:append(Data#tosend.priority, Data, PQueue),
              DQueue,
              0);
    _ ->
      destLoop(NPrior, NDel, PQueue, DQueue, TO)
  after TO ->
    % Consultar si hay algo disponible para entrgar
    case dict:find(NDel, PQueue) of
      {ok, Msg} when is_list(Msg) ->
        lists:foreach(fun(X) -> deliver ! [X] end, lists:sort(Msg)),
        destLoop(NPrior, NDel+1, PQueue, DQueue, 0);
      {ok, Msg} ->  % Caso en que hay algo para entregar
        deliver ! Msg,
        destLoop(NPrior, NDel+1, PQueue, DQueue, 0);
      error ->      % No hay nada para entregar
        destLoop(NPrior, NDel, PQueue, DQueue, infinity)
    end
  end.


deliverLoop(List) ->
  receive
    fin ->
      lists:foreach(fun (X) -> X ! {nodedown, node()} end, List),
      exit(success);
    [M] when is_record(M, tosend) ->
      lists:foreach(fun(X) -> X ! M#tosend.msg end, List),
      deliverLoop(List);
    {registerP, Pid} ->
      deliverLoop([Pid] ++ List);
    {unregisterP, Pid} ->
      deliverLoop(remove(Pid, List));
    G ->
      io:format("~p~n",[G]),
      deliverLoop(List)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %Funciones aux. para listas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Nos dice si un objeto pertenece a la lista
%find : Obj x List -> true "si fue encontado" | false "NO fue encontado"
find(_, []) -> false;
find(Obj, [HEAD | TAIL]) ->
  if
    HEAD == Obj ->
      true;
    true ->
      find(Obj, TAIL)
  end.

%Funcion para remover un objeto de una lista
%remove : Obj x Lista1 -> Lista2
remove(_, []) -> [];
remove(Obj, [HEAD | TAIL]) ->
  if
    HEAD == Obj ->
      TAIL;
    true ->
      [HEAD] ++ remove(Obj, TAIL)
  end.
