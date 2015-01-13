%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_handle_acl).

-export([handle_acl_req/2
         ,lookup_acl_records/1, lookup_acl_records/2
        ]).

-include("kamdb.hrl").

-spec handle_acl_req(wh_json:object(), wh_proplist()) -> any().
handle_acl_req(Reqest, _Props) ->
    'true' = wapi_kamdb:acls_req_v(Reqest),
    Entity = wh_json:get_value(<<"Entity">>, Reqest),
    IncludeRealm  = wh_json:get_value(<<"With-Realm">>, Reqest),
    Payload = lookup_acl_records(Entity, IncludeRealm),
    send_response(Reqest, Payload).

-spec send_response(wh_json:object(), wh_json:objects()) -> any().
send_response(Reqest, Responses) ->
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Reqest)}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = wh_json:get_value(<<"Server-ID">>, Reqest),
    {DeviceACLs, RealmACLs} = lists:partition(fun kamdb_utils:is_device/1, Responses),
    Resp = lists:foldl(fun wh_json:merge_jobjs/2, RespStub, [make_section(DeviceACLs, <<"Device">>)
                                                             ,make_section(RealmACLs, <<"Realm">>)
                                                            ]),
    lager:debug("publishing response"),
    wapi_kamdb:publish_acls_resp(ServerID, Resp).

-spec make_section(wh_json:objects(), ne_binary()) -> wh_json:object().
-spec make_section(ne_binary(), api_object(), api_object()) -> wh_json:object().
make_section([], _) ->
    wh_json:new();
make_section([JObj], Section) ->
    Order = wh_json:get_value([<<"value">>, <<"acls">>, <<"order">>], JObj),
    CIDRs = wh_json:get_value([<<"value">>, <<"acls">>, <<"cidr">>], JObj),
    make_section(Section, Order, CIDRs).
make_section(_, Order, CIDRs)
    when Order =:= 'undefined' orelse CIDRs =:= 'undefined' ->
        wh_json:new();
make_section(Section, Order, CIDRs) ->
    wh_json:from_list([{Section, wh_json:from_list([{<<"Order">>, Order}, {<<"CIDR">>, CIDRs}])}]).

-spec lookup_acl_records(ne_binary(), boolean()) -> wh_json:objects().
lookup_acl_records(Entity, IncludeRealm) ->
    lager:debug("Handle acl request for ~s", [Entity]),
    Realm = kamdb_utils:extract_realm(Entity),
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', _} ->
            lager:debug("Found realm, try to send response"),
            run_acl_query(Entity, IncludeRealm);
        _ ->
            lager:info("Can't find realm ~s. Sending deny ACL.", [Realm]),
            make_deny_acl(Entity, IncludeRealm)
    end.

-spec lookup_acl_records(ne_binary()) -> wh_json:objects().
lookup_acl_records(Entity) ->
    lookup_acl_records(Entity, 'true').

-spec run_acl_query(ne_binary(), boolean()) -> wh_json:objects().
run_acl_query(Entity, IncludeRealm) ->
    Keys = binary:split(Entity, <<"@">>),
    ViewOpts = case Keys of
                   [User, _OnRealm] ->
                       case IncludeRealm of
                           'true' -> make_acl_view(Keys);
                           _ -> make_acl_view(User)
                       end;
                   [JustRealm] -> make_acl_view(JustRealm)
               end,
    {'ok', UserDb} = whapps_util:get_account_by_realm(kamdb_utils:extract_realm(Entity)),
    lager:debug("Looking for ~s's acls in ~s", [Entity, UserDb]),
    {'ok', Results} = couch_mgr:get_results(UserDb, <<"acls/crossbar_listing">>, ViewOpts),
    lager:debug("Found ~p records", [length(Results)]),
    Results.

-spec make_acl_view(ne_binaries() | ne_binary()) -> wh_proplist().
make_acl_view(Keys) when is_list(Keys) ->
    [{'keys', Keys}];
make_acl_view(Key) ->
    [{'key', Key}].

-spec make_deny_acl(ne_binary(), boolean()) -> wh_json:objects().
make_deny_acl(Entity, IncludeRealm) ->
    Realm = kamdb_utils:extract_realm(Entity),
    IsDevice = Realm =/= Entity,
    Type = case IsDevice of
               'true' -> <<"device">>;
               _ -> <<"account">>
           end,
    ACL = wh_json:from_list([{<<"order">>, <<"AD">>}
                             ,{<<"cidr">>, [<<"0.0.0.0/0">>]}
                            ]),
    Value = wh_json:from_list([{<<"type">>, Type}
                               ,{<<"acls">>, ACL}
                              ]),
    Record = wh_json:from_list([{<<"id">>, 'undefined'}
                                ,{<<"key">>, Entity}
                                ,{<<"value">>, Value}
                               ]),
    case IsDevice
         andalso IncludeRealm
    of
        'true' -> [Record | make_deny_acl(Realm)];
        _ -> [Record]
    end.

-spec make_deny_acl(ne_binary()) -> wh_json:objects().
make_deny_acl(Entity) ->
    make_deny_acl(Entity, 'true').
