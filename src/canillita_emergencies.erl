%%% @doc emergency Model
-module(canillita_emergencies).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type id() :: binary().
-type pozo() :: integer().
-type tipo() :: binary().

-opaque emergency() ::
  #{ id => id() | undefined
   , pozo => pozo()
   , tipo => tipo()
   , timestamp => calendar:datetime()
   }.

-export_type(
  [ id/0
  , pozo/0
  , tipo/0
  , emergency/0
  ]).

%% sumo_doc behaviour callbacks
-export(
  [ sumo_schema/0
  , sumo_sleep/1
  , sumo_wakeup/1
  ]).

%% sumo_rest_doc behaviour callbacks
-export(
  [ to_json/1
  , from_json/1
  , from_json/2
  , update/2
  , location/2
  ]).

%% Public API
-export([to_sse/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id, binary, [id])
    , sumo:new_field(pozo, integer, [not_null])
    , sumo:new_field(tipo, string, [not_null])
    , sumo:new_field(timestamp, datetime, [not_null])
    ]).

%% @doc Convert a newspaper from its system representation to sumo's
%%      internal one.
-spec sumo_sleep(Emergency::emergency()) -> sumo:doc().
sumo_sleep(Emergency) -> Emergency.

%% @doc Convert a newspaper from sumo's internal representation to its
%%      system one.
-spec sumo_wakeup(Emergency::sumo:doc()) -> emergency().
sumo_wakeup(Emergency) -> Emergency.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a newspaper from its system representation to json.
-spec to_json(Emergency::emergency()) -> emergency().
to_json(Emergency) ->
  #{ id => sr_json:encode_null(maps:get(id, Emergency))
   , pozo => maps:get(pozo, Emergency)
   , tipo => maps:get(tipo, Emergency)
   , timestamp => sr_json:encode_date(maps:get(timestamp, Emergency))
   }.

%% @doc Convert a newspaper from json to its system representation.
-spec from_json(Pozo::pozo(), Json::sumo_rest_doc:json()) ->
  {ok, emergency()} | {error, iodata()}.
from_json(Pozo, Json) ->
  from_json(Json#{<<"pozo">> => Pozo}).

-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, emergency()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ id => sr_json:decode_null(maps:get(<<"id">>, Json, null))
       , pozo => maps:get(<<"pozo">>, Json)
       , tipo => maps:get(<<"tipo">>, Json)
       , timestamp =>
          sr_json:decode_date(maps:get(<<"timestamp">>, Json, Now))
       }
    }
  catch
    _:{badkey, Key} -> {error, <<"missing field (Emergency): ", Key/binary>>}
  end.

-spec update(Emergency::emergency(), Json::sumo_rest_doc:json()) ->
  {ok, emergency()}.
update(Emergency, _Json) -> {ok, Emergency}.

%% @doc Specify the URL that identifies a Emergency.
-spec location(Emergency::emergency(), Path::sumo_rest_doc:path()) -> iodata().
location(#{id := EmergencyId, pozo := Pozo}, _Path) ->
  iolist_to_binary(["/emergency/", Pozo, "/", EmergencyId]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a newspaper from its system representation to SSE.
-spec to_sse(Emergency::emergency()) -> lasse_handler:event().
to_sse(Emergency) ->
  #{ id => maps:get(id, Emergency)
   , event => maps:get(tipo, Emergency)
   , data => iolist_to_binary([ maps:get(pozo, Emergency)
                              , "\n"
                              , maps:get(timestamp, Emergency)
                              ])
   }.
