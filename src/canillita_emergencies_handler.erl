%%% @doc POST /emergency/:id handler.
-module(canillita_emergencies_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , resource_exists/2
          , allowed_methods/2
          , announce_req/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_post/3
          ]
        }]).

%% Alias
-type state() :: sr_entities_handler:state().

-export([trails/0, handle_post/2]).

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Metadata =
    #{ post =>
       #{ tags => ["emergencies"]
        , description => "Creates a new emergency"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/emergency",
  Options = #{path => Path, model => canillita_emergencies, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

-spec handle_post(Req::cowboy_req:req(), State::state()) ->
  { {true, binary()} | false | halt
  , cowboy_req:req()
  , state()
  }.
handle_post(Req, State) ->
  try
    {ok, Body, Req1}      = cowboy_req:body(Req),
    Json                  = sr_json:decode(Body),
    % Checks that the given newspaper does exists
        case canillita_emergencies:from_json(Json) of
          {error, Reason} ->
            Req2 = cowboy_req:set_resp_body(sr_json:error(Reason), Req1),
            {false, Req2, State};
          {ok, Entity} ->
            handle_post(Entity, Req1, State)
        end
  catch
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request (HERE)">>), Req),
      {false, Req3, State}
  end.
