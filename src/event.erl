-module(event).
-compile(export_all).
-record(state, {server, name = "", to_go = 0}).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

%% Internal function for events
init(Server, EventName, DateTime) ->
  loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

%% Function operate with list of time delays to avoid 49 days restriction
loop(S = #state{server = Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 ->
      if Next =:= [] ->
        Server ! {done, S#state.name};
      Next =/= [] ->
        loop(S#state{to_go=Next})
      end
  end.

cancel(Pid) ->
  %% Add monitor in case other process is finished
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      % flush option needed to remove 'DOWN' messages that we may arrive before demonitor
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

time_to_go(Timeout = {{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(Timeout) -
         calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0 -> ToGo;
            ToGo =< 0 -> 0
         end,
  normalize(Secs).

normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].
