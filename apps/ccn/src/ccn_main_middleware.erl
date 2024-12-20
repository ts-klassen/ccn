-module(ccn_main_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env) ->
    IsIpOk = true,
    Login = case {IsIpOk, Req0} of
        {true, #{ headers := #{ <<"authorization">> := Auth }}} ->
            ccn_user:lookup_by_auth_header(Auth);
        _ ->
            none
    end,
    Req10 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req0),
    Req20 = Req10#{ ccn_login => Login },
    Req30 = remove_credentials(Req20),
    case Login of
        none ->
            {stop, unauthorized(Req30)};
        _ ->
            {ok, Req30, Env}
    end.

remove_credentials(Req=#{headers:=Headers}) ->
    Req#{headers := maps:remove(<<"authorization">>, Headers)}.

unauthorized(Req0) ->
    cowboy_req:reply(401, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
      , <<"Www-Authenticate">> => <<"Basic realm=\"Secure Area\"">>
    }, <<"<h1>401 Unauthorized</h1>\n">>, Req0).

