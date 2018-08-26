%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc Handle nga911messaging api docs
%%% @end
%%%-----------------------------------------------------------------------------
-module(nga911messaging_api).

-export([handle_api_sms/1]).

-include("nga911messaging.hrl").

-type message_process_response() :: {'ok', any()}|
                                    {'error', kz_term:integers(), kz_term:binary()}.

-spec handle_api_sms(kz_json:object()) -> message_process_response().
handle_api_sms(JObj) ->
    NetworkAddress = kz_json:get_string_value(<<"network_address">>, JObj),
    FromUser = kz_json:get_string_value(<<"from_user">>, JObj),
    From = kz_json:get_string_value(<<"from">>, JObj),
    To = kz_json:get_string_value(<<"to">>, JObj),
    Body = kz_json:get_string_value(<<"body">>, JObj),
    Subject = kz_json:get_string_value(<<"subject">>, JObj),
    MediaUrls = kz_json:get_list_value(<<"media_urls">>, JObj),
    Smil = kz_json:get_string_value(<<"smil">>, JObj),
    DirectRoute = kz_json:get_boolean_value(<<"direct_route">>, JObj),
    lager:info("Received:\n network_address: ~p\n from_user: ~p from: ~p\n to: ~p\n body: ~p\n subject: ~p\n media_urls: ~p\n smil: ~p\n direct_route: ~p\n", [NetworkAddress, FromUser, From, To, Body, Subject, MediaUrls, Smil, DirectRoute]),
    handle_api_sms(NetworkAddress, FromUser, From, To, Body, Subject, MediaUrls, Smil, DirectRoute, JObj).

-spec handle_api_sms(kz_term:api_list(),
                     kz_term:api_list(),
                     kz_term:api_list(),
                     kz_term:api_list(),
                     kz_term:api_list(),
                     kz_term:api_list(),
                     kz_term:api_list(),
                     kz_term:api_list(),
                     kz_term:api_atom(),
                     kz_json:object()) -> message_process_response().
handle_api_sms('undefined', _FromUser, _From, _To, _Body, _Subject, _MediaUrls, _Smil, _DirectRoute, _JObj) ->
    {'error', 400, <<"network_address value is not defined">>};
handle_api_sms(_NetworkAddress, _FromUser, 'undefined', _To, _Body, _Subject, _MediaUrls, _Smil, _DirectRoute, _JObj) ->
    {'error', 400, <<"from value is not defined">>};
handle_api_sms(_NetworkAddress, 'undefined', _From, _To, _Body, _Subject, _MediaUrls, _Smil, _DirectRoute, _JObj) ->
    {'error', 400, <<"from_user value is not defined">>};
handle_api_sms(_NetworkAddress, _FromUser, _From, 'undefined', _Body, _Subject, _MediaUrls, _Smil, _DirectRoute, _JObj) ->
    {'error', 400, <<"to value is not defined">>};
handle_api_sms(_NetworkAddress, _FromUser, _From, _To, 'undefined', 'undefined', 'undefined', 'undefined', _DirectRoute, _JObj) ->
    {'error', 400, <<"Message is not contains any of body, subject, media_urls or smil elements">>};
handle_api_sms(_NetworkAddress, _FromUser, _From, _To, _Body, 'undefined', 'undefined', 'undefined', DirectRoute, JObj) ->
    publish_message(DirectRoute, JObj);
handle_api_sms(_NetworkAddress, _FromUser, _From, _To, 'undefined', _Subject, _MediaUrls, 'undefined', DirectRoute, JObj) ->
    publish_message(DirectRoute, JObj);
handle_api_sms(_NetworkAddress, _FromUser, _From, _To, 'undefined', _Subject, 'undefined', _Smil, DirectRoute, JObj) ->
    publish_message(DirectRoute, JObj);
handle_api_sms(_NetworkAddress, _FromUser, _From, _To, _Body, _Subject, _MediaUrls, _Smil, _DirectRoute, _JObj) ->
    {'error', 400, <<"MMS feature is not implemented">>}.

-spec publish_message(kz_term:api_atom(), kz_json:object()) -> message_process_response().
publish_message(_DirectRoute, JObj) ->
    FetchId = kz_binary:rand_hex(16),
    CallId = kz_binary:rand_hex(16),
    process_sms_api_document(FetchId, CallId, JObj).

-spec process_sms_api_document(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> message_process_response().
process_sms_api_document(FetchId, CallId, JObj) ->
    ReqResp = kz_amqp_worker:call(route_req(FetchId, CallId, JObj)
                                 ,fun kapi_route:publish_req/1
                                 ,fun kapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]),
            {'error', 503, <<"did not receive route response">>};
        {'ok', RespJObj} ->
            'true' = kapi_route:resp_v(RespJObj),
            send_route_win(FetchId, CallId, RespJObj)
    end.

-spec send_route_win(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> message_process_response().
send_route_win(FetchId, CallId, JObj) ->
    ServerQ = kz_json:get_value(<<"Server-ID">>, JObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    Win = [{<<"Msg-ID">>, FetchId}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sms api handler sending route_win to ~s", [ServerQ]),
    kz_amqp_worker:cast(Win, fun(Payload) -> kapi_route:publish_win(ServerQ, Payload) end),
    maybe_6xx_response(JObj).

-spec maybe_6xx_response(kz_json:object()) -> message_process_response().
maybe_6xx_response(JObj) ->
    case kz_json:get_value(<<"Method">>, JObj) of
        <<"error">> -> {'error', 503, <<"response from message processing application is not received">>};
        _ -> {'ok', kz_json:get_value(<<"Msg-Id">>, JObj)}
    end.

-spec route_req(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
route_req(FetchId, CallId, JObj) ->
    [{<<"Msg-ID">>, FetchId}
    ,{<<"Call-ID">>, CallId}
    ,{<<"Message-ID">>, kz_json:get_value(<<"Message-ID">>, JObj, kz_binary:rand_hex(16))}
    ,{<<"Caller-ID-Name">>, kz_json:get_value(<<"from_user">>, JObj)}
    ,{<<"Caller-ID-Number">>, kz_json:get_value(<<"from_user">>, JObj)}
    ,{<<"From">>, kz_json:get_value(<<"from">>, JObj)}
    ,{<<"To">>, kz_json:get_value(<<"to">>, JObj)}
    ,{<<"Request">>, kz_json:get_value(<<"to">>, JObj)}
    ,{<<"SMIL">>, kz_json:get_value(<<"smil">>, JObj)}
    ,{<<"Subject">>, kz_json:get_value(<<"subject">>, JObj)}
    ,{<<"MediaUrls">>, kz_json:get_value(<<"media_urls">>, JObj)}
    ,{<<"Body">>, kz_json:get_value(<<"body">>, JObj)}
    ,{<<"Custom-Channel-Vars">>, kz_json:from_list(route_req_ccvs(FetchId, JObj))}
    ,{<<"Resource-Type">>, <<"sms">>}
    ,{<<"Call-Direction">>, <<"outbound">>}
    ,{<<"From-Network-Addr">>, kz_json:get_value(<<"network_address">>, JObj)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec route_req_ccvs(kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
route_req_ccvs(FetchId, _JObj) ->
    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
      ,{<<"API-Call">>, 'true'}
      ]).
