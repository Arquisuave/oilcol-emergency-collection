[ { cowboy_swagger
  , [ { global_spec
      , #{ swagger => "2.0"
         , info => #{title => "Canillita Test API"}
         }
      }
    ]
  }
, { sumo_db
  , [ {wpool_opts, [{overrun_warning, 600}]}
    , {log_queries, true}
    , {query_timeout, 30000}
    , {storage_backends, [{sumo_test_backend_pgsql,
       sumo_backend_pgsql,
       [{host, "margffoy-tuay.com"},
        {port, 5432},
        {database, "newspapers"},
        {username, "news"},
        {password, "newpassword"}]
      }]}
    , {stores, [{sumo_test_pgsql,
       sumo_store_pgsql,
       [{storage_backend, sumo_test_backend_pgsql},
        {workers, 200}]
      }]}
    , { docs
      , [ {canillita_newspapers, sumo_test_pgsql}
        , {canillita_newsitems, sumo_test_pgsql}
        ]
      }
    , {events, [{canillita_newsitems, canillita_newsitems_events_manager}]}
    ]
  }
, {canillita, []}
, { sasl
  , [ {sasl_error_logger, tty}
    , {errlog_type, all}
    ]
  }
].
