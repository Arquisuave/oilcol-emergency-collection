%%% @doc NewsItems repository
-module(canillita_emergencies_repo).

-export([fetch/2, fetch_since/1]).

%% @doc Returns the newsitem that matches the given
%%      newspaper_name and id (if any).
-spec fetch( Pozo::canillita_emergencies:pozo()
           , Id::canillita_newsitems:id()
           ) -> notfound | sumo_rest_doc:entity().
fetch(Pozo, Id) ->
  Conditions = [{id, Id}, {pozo, Pozo}],
  sumo:find_one(canillita_emergencies, Conditions).

%% @doc returns all the news after the given event-id or all the news
%%      if not event-id provided.
-spec fetch_since(LastEventId::canillita_emergencies:id()  | undefined) ->
  [sumo_rest_doc:entity()].
fetch_since(undefined) ->
  fetch_all();
fetch_since(LastEventId) ->
  #{created_at := CreatedAt} = fetch(LastEventId),
  fetch_all(CreatedAt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc returns the newsitem identified with the given id.
-spec fetch(Id::canillita_emergencies:id()) -> notfound | sumo_rest_doc:entity().
fetch(Id) ->
  sumo:find(canillita_emergencies, Id).

%% @doc returns all the newsitems stored so far.
-spec fetch_all() -> [sumo_rest_doc:entity()].
fetch_all() ->
  sumo:find_all(canillita_emergencies).

%% @doc returns all elements created after the given datetime.
-spec fetch_all(CreatedAt::calendar:datetime()) -> [sumo_rest_doc:entity()].
fetch_all(CreatedAt) ->
  Conditions = [{timestamp, '>', CreatedAt}],
  sumo:find_by(canillita_emergencies, Conditions).
