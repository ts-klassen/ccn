-module(ccn_api_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0=#{method:=Method, ccn_login:={value, Login}, bindings:=Bindings}, State) ->
    {ok, ReqBody, Req10} = cowboy_req:read_body(Req0),
    JSON = case ReqBody of
        <<>> -> #{};
        _ -> jsone:decode(ReqBody)
    end,
    Body = try
        jsone:encode(main(Method, Bindings, Login, JSON))
    catch
        Class:Error:Stack ->
            ccn_oplog:error(Req10, ReqBody, Class, Error, Stack),
            jsone:encode(#{success => false, reason => server_error})
    end,
    ccn_oplog:ok(Req10, ReqBody, jsone:decode(Body)),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
    }, Body, Req0),
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(403, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
    }, <<"<h1>403 Forbidden</h1>\n">>, Req0),
    io:format("~p~n", [Req]),
    {ok, Req, State}.


main(<<"POST">>, #{arg1 := <<"ccn">>, arg2 := <<"upsert">>}, User, Payload) ->
    case ccn:user_upsert(Payload, User) of
        conflict ->
            #{success => false, reason => conflict};
        ccn ->
            #{success => true, ccn => ccn}
    end;


main(<<"POST">>, #{arg1 := <<"ccn">>, arg2 := <<"lookup">>}, _, #{<<"id">>:=Id}) ->
    case ccn:lookup(Id) of
        none ->
            #{success => false, reason => not_found};
        {value, ccn} ->
            #{success => true, ccn => ccn}
    end;


main(<<"GET">>, #{arg1 := <<"ccn">>, arg2 := <<"state">>}, _, _) ->
    #{success => true, state_list => ccn_state:states()};


main(<<"POST">>, #{arg1 := <<"ccn">>, arg2 := <<"list">>}, _, #{<<"state">>:=State}) ->
    #{success => true, ccn_list => ccn_state:list(State)};


main(<<"GET">>, #{arg1 := <<"master">>, arg2 := Id}, _, _) ->
    Master = ccn_master:get(Id),
    Master#{ success => true };


main(<<"POST">>, #{arg1 := <<"master">>, arg2 := Id}, User, Payload) ->
    case ccn_master:set(Id, Payload, User) of
        conflict ->
            #{success => false, reason => conflict};
        ok ->
            #{success => true}
    end;


main(<<"POST">>, #{arg1 := <<"sheet">>, arg2 := <<"ccn">>}, _, #{<<"id">>:=Id}) ->
    case ccn_sheet:ccn_list(Id) of
        not_found ->
            #{ success => false, reason => not_found };
        ccnList ->
            #{ success => true, list => ccnList }
    end;


main(<<"GET">>, #{arg1 := <<"sheet">>, arg2 := <<"field">>}, _, _) ->
    #{ success => true, list => ccn_sheet:list() };


main(<<"POST">>, #{arg1 := <<"sheet">>, arg2 := <<"field">>}, _, #{<<"id">>:=Id}) ->
    case ccn_sheet:get_field(Id) of
        not_found ->
            #{ success => false, reason => not_found };
        Field ->
            Field#{ success => true }
    end;


main(<<"PUT">>, #{arg1 := <<"sheet">>, arg2 := <<"field">>}, User, Field) ->
    case ccn_sheet:set_field(Field, User) of
        conflict ->
            #{success => false, reason => conflict};
        Doc ->
            Doc#{success => true}
    end;


main(<<"POST">>, #{arg1 := <<"users">>, arg2 := <<"passwd">>}, #{<<"user">>:=User}, #{<<"passwd">>:=Passwd}) ->
    ccn_user:update_pw(User, Passwd, User),
    #{success => true};



main(_, _, _, _) ->
    #{ success => false, reason => clause_error }.

