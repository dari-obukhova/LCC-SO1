%% Msg para mandar a los nodos
%% Status puede ser: tosend / tocheck
-record(send,    {msg, sender, priority}).
-record(tocheck, {msg, sender, priority}).
-record(tosend,  {msg, sender, priority}).
