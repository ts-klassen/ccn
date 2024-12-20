-module(ccn_init).

-export([
        db_setup/0
    ]).

db_setup() ->
    lists:map(fun(DB) ->
        try klsn_db:create_db(DB) catch
            error:exists -> ok
        end
    end, [ccn_user]),
    embe:init_setup(),
    klsn_db:upsert(ccn_user, {raw, <<"_design/ccn_user">>}, fun(MaybeDoc) ->
        Doc = case MaybeDoc of
            {value, Doc0} -> Doc0;
            none -> #{}
        end,
        Doc#{
            <<"views">> => #{
                <<"list">> => #{
                    <<"map">> => <<"function (doc) {emit(doc.C, {id: doc._id, is_admin: doc.is_admin, created_by: doc.created_by, created_at: doc.C});}">>
                }
            }
          , <<"language">> => <<"javascript">>
        }
    end),
    ok.

