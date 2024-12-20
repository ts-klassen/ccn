%%%-------------------------------------------------------------------
%% @doc ccn public API
%% @end
%%%-------------------------------------------------------------------

-module(ccn_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ccn/health", ccn_health_handler, #{}}
          , {"/ccn/api/v1", ccn_admin_handler, #{}}
          , {"/ccn/api/v1/:arg1", ccn_admin_handler, #{}}
          , {"/ccn/api/v1/:arg1/:arg2", ccn_admin_handler, #{}}
          , {"/ccn/api/v1/:arg1/:arg2/:arg3", ccn_admin_handler, #{}}
          , {"/ccn/api/v2", ccn_api_handler, #{}}
          , {"/ccn/api/v2/:arg1", ccn_api_handler, #{}}
          , {"/ccn/api/v2/:arg1/:arg2", ccn_api_handler, #{}}
          , {"/ccn/api/v2/:arg1/:arg2/:arg3", ccn_api_handler, #{}}
          , {"/ccn/static/[...]", cowboy_static, {priv_dir, ccn, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(ccn_http_listener,
        [{port, 9080}, inet], % add 'inet6' for ipv6
        #{
            env => #{dispatch => Dispatch}
          , middlewares => [
                ccn_main_middleware
              , cowboy_router
              , cowboy_handler
            ]
        }
    ),
    ccn_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
