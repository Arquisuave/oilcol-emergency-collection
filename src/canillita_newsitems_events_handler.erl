-module(canillita_newsitems_events_handler).

-behaviour(gen_event).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_call/2
        , code_change/3
        , handle_event/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
  {ok, []}.

handle_info(_Info, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, not_implemented, State}.

handle_event({canillita_emergencies, created, [Entity]}, State) ->
  canillita_emergencies_sse_handler:notify(Entity),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.
