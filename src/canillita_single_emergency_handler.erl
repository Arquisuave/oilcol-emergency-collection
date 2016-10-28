%%% @doc GET|PUT|DELETE /newspapers/:id handler
-module(canillita_single_emergency_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_put/2
          , delete_resource/2
          ]
        }]).

-export([ trails/0 ]).

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Id =
    #{ name => id
     , in => path
     , description => <<"Emergency key">>
     , required => true
     , type => string
     },
  Metadata =
    #{ get =>
       #{ tags => ["emergencies"]
        , description => "Returns an emergncy"
        , produces => ["application/json"]
        , parameters => [Id]
        }
     , put =>
       #{ tags => ["emergencies"]
        , description => "Updates or creates a new emergency"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody, Id]
        }
     , delete =>
       #{ tags => ["emergencies"]
        , description => "Deletes an emergency"
        , parameters => [Id]
        }
     },
  Path = "/emergency/:id",
  Options = #{path => Path, model => canillita_emergencies, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].
