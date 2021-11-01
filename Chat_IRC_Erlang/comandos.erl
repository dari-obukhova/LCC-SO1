-module(comandos).
-export([comandos/1]).

comandos(<<47,101,120,105,116,10,0>>) ->
  {exit, <<47,101,120,105,116,10,0>>};
comandos(Msg) ->
  case string:split(Msg, " ") of
    [<<"/msg">>, Rest] ->
      case string:split(Rest, " ") of
        [Nick, MsgPrib] -> %= string:split(Rest, " "),
          {msg, Nick, MsgPrib};
        _ ->
          {msg, error}
      end;
    [<<"/nick">>, Rest] ->
      {nick, Rest};
    _ ->
      {all, Msg}
  end.
