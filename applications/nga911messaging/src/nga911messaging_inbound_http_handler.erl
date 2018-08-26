%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Handles storage proxy requests for media binaries
%%% @end
%%%-----------------------------------------------------------------------------
-module(nga911messaging_inbound_http_handler).
-behaviour(cowboy_handler).

-export([init/2
        ,terminate/3
        ]).

-include("nga911messaging.hrl").

-type handler_return() :: {'ok', cowboy_req:req(), 'ok'}.

-type reply_code() :: 200 | 400 | 401 | 404 | 405 | 413 | 503.

-spec init(cowboy_req:req(), kz_term:proplist()) -> handler_return().
init(Req, _Opts) ->
    kz_util:put_callid(kz_binary:rand_hex(16)),
    check_authn(Req, authenticate(Req)).

-spec check_authn(cowboy_req:req(), boolean()) -> handler_return().
check_authn(Req, 'true') ->
    check_validation(Req, cowboy_req:method(Req), cowboy_req:path_info(Req));
check_authn(Req, 'false') ->
    lager:info("request did not provide valid credentials"),
    {'ok', unauthorized(Req), 'ok'}.

-spec check_validation(cowboy_req:req(), kz_term:ne_binary(), kz_term:ne_binaries()) -> handler_return().
check_validation(Req, <<"POST">>, []) ->
    case cowboy_req:read_body(Req) of
        {'ok', Data, ReqWithBody} ->
            lager:info("Received body ~p", [Data]),
            try kz_json:unsafe_decode(Data) of
                JObj -> parse_request(JObj,ReqWithBody)
            catch
                _ -> reply_error(400, <<"Request is not JSON object">>, ReqWithBody)
            end;
        {'more', _Data, ReqWithBody} ->
            reply_error(413, <<"Request Entity Too Large Error">>, ReqWithBody)
    end;
check_validation(Req, <<"POST">>, _Else) ->
    Path=cowboy_req:path(Req),
    lager:info("unexpected path in request: \"~s\"", [Path]),
    reply_error(404, <<"Not Found">>, Req);
check_validation(Req, Method, _Else) ->
    lager:info("Received not supported method '~s'", [Method]),
    reply_error(405, <<"Method Not Allowed">>, Req).

-spec parse_request(kz_json:object(), cowboy_req:req()) ->  cowboy_req:req().
parse_request(JObj, Req) ->
    {NetworkAddress, _PeerPort} = cowboy_req:peer(Req),
    FromUser = kz_json:get_value(<<"from">>, JObj),
    From = case FromUser of
               'undefined' -> 'undefined';
               _ -> erlang:iolist_to_binary([FromUser, <<"@telnyx.com">>])
           end,
    ToUser = kz_json:get_value(<<"to">>, JObj),
    To = case ToUser of
               'undefined' -> 'undefined';
               _ -> erlang:iolist_to_binary([ToUser, <<"@telnyx.com">>])
           end,
    PropList = props:filter_undefined(
                 [{<<"network_address">>, kz_network_utils:iptuple_to_binary(NetworkAddress)}
                 ,{<<"from_user">>, FromUser}
                 ,{<<"from">>, From}
                 ,{<<"to">>, To}
                 ,{<<"body">>, kz_json:get_value(<<"body">>, JObj)}
                 ,{<<"subject">>, kz_json:get_value(<<"subject">>, JObj)}
                 ,{<<"smil">>, kz_json:get_value(<<"smil">>, JObj)}
                 ,{<<"media_urls">>, kz_json:get_list_value(<<"media_urls">>, JObj)}
                 ,{<<"direct_route">>, kz_json:get_value(<<"direct_route">>, JObj)}
                 ]),
    case nga911messaging_api:handle_api_sms(kz_json:from_list(PropList)) of
        {'ok', Response} ->
            {'ok', success(Response, [], Req), []};
        {'error', Code, Message} -> reply_error(Code, Message, Req)
    end.

-spec authenticate(cowboy_req:req()) -> boolean().
authenticate(Req) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"proxy_store_authenticate">>, 'true') of
        'false' -> 'true';
        'true' -> maybe_basic_authentication(Req)
    end.

-spec maybe_basic_authentication(cowboy_req:req()) -> boolean().
maybe_basic_authentication(Req) ->
    case credentials(Req) of
        {'undefined', 'undefined', Req1} ->
            lager:debug("proxy store request did not provide basic authentication", []),
            maybe_acl_authentication(Req1);
        {Username, Password, _Req1} ->
            maybe_basic_authentication(Username, Password)
    end.

-spec maybe_basic_authentication(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
maybe_basic_authentication(Username, Password) ->
    AuthUsername = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_username">>, <<>>),
    AuthPassword = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_password">>, <<>>),
    not kz_term:is_empty(AuthUsername)
        andalso not kz_term:is_empty(AuthPassword)
        andalso Username =:= AuthUsername
        andalso Password =:= AuthPassword.

-spec maybe_acl_authentication(cowboy_req:req()) -> boolean().
maybe_acl_authentication(Req) ->
    ACLs = kapps_config:get(?CONFIG_CAT, <<"proxy_store_acls">>, [<<"127.0.0.0/24">>]),
    {IpTuple, _PeerPort} = cowboy_req:peer(Req),
    Ip = kz_network_utils:iptuple_to_binary(IpTuple),
    maybe_acl_authentication(ACLs, Ip).

-spec maybe_acl_authentication(kz_term:ne_binaries(), kz_term:ne_binary()) -> boolean().
maybe_acl_authentication([], Ip) ->
    lager:debug("ip address ~s can not be authenticated via ACLs", [Ip]),
    'false';
maybe_acl_authentication([ACL|ACLs], Ip) ->
    case kz_network_utils:verify_cidr(Ip, ACL) of
        'true' ->
            lager:debug("ip address ~s was authenticated via ACLs", [Ip]),
            'true';
        'false' ->
            maybe_acl_authentication(ACLs, Ip)
    end.

-spec credentials(cowboy_req:req()) -> {kz_term:api_binary(), kz_term:api_binary(), cowboy_req:req()}.
credentials(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        'undefined' ->
            {'undefined', 'undefined', Req};
        Authorization ->
            {Username, Password} = credentials_from_header(Authorization),
            {Username, Password, Req}
    end.

-spec credentials_from_header(kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:api_binary()}.
credentials_from_header(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$\s>>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);
        _ ->
            {'undefined', 'undefined'}
    end.

-spec decoded_credentials(kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:api_binary()}.
decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [Username, Password] ->
            {Username, Password};
        _ ->
            {'undefined', 'undefined'}
    end.

-spec unauthorized(cowboy_req:req()) -> cowboy_req:req().
unauthorized(Req0) ->
    Req1 = cowboy_req:set_resp_header(<<"WWW-Authenticate">>, <<"Basic realm=\"Kazoo Messaging Endpoint\"">>, Req0),
    reply_error(401, <<"Unauthorized">>, Req1).

-spec success(kz_json:object(), kz_term:proplist(), cowboy_req:req()) -> cowboy_req:req().
success(JObj, Props, Req0) ->
    Body = io_lib:format("~s~n", [kz_json:encode(kz_json:set_value(<<"ok">>, 'true', JObj))]),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    Headers = maps:from_list([{kz_term:to_binary(H), kz_term:to_binary(V)}
                              || {H, V} <- props:get_value('headers', Props, [])
                             ]),
    lager:info("replying with 200"),
    cowboy_req:reply(200, Headers, Req1).

-spec reply_error(reply_code(), kz_term:ne_binary(), cowboy_req:req()) -> {'ok', cowboy_req:req(), 'ok'}.
reply_error(Code, Message, Req) ->
    lager:info("replying with error ~p and message \"~s\"", [Code, Message]),
    Body = erlang:iolist_to_binary([<<"{\"error\":{\"code\":">>, list_to_binary(integer_to_list(Code)), <<",\"message\":\"">>, Message, <<"\"}}\n">>]),
    ReqWithBody = cowboy_req:set_resp_body(Body, Req),
    {'ok', cowboy_req:reply(Code, ReqWithBody), 'ok'}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _) ->
    lager:debug("terminating handler: ~p", [_Reason]).
