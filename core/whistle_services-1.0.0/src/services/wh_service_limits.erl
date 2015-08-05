%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% VCCS Telecom
%%%-------------------------------------------------------------------
-module(wh_service_limits).

-export([reconcile/1, reconcile/2
         ,item_twoway/0
         ,item_inbound/0
         ,item_outbound/0
        ]).

-include("../whistle_services.hrl").

-define(CATEGORY_ID, <<"limits">>).
-define(ITEM_TWOWAY, <<"twoway_trunks">>).
-define(ITEM_INBOUND, <<"inbound_trunks">>).
-define(ITEM_OUTBOUND, <<"outbound_trunks">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, <<"limits">>) of
        {'error', _R} ->
            lager:debug("unable to get current limits in service: ~p", [_R]),
            Services;
        {'ok', JObj} -> reconcile(Services, JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services(), wh_json:object()) -> wh_services:services().
reconcile(Services, JObj) ->
    Keys = [?ITEM_TWOWAY
            ,?ITEM_INBOUND
            ,?ITEM_OUTBOUND
           ],
    lists:foldl(fun(Key, S) -> maybe_update_key(Key, JObj, S) end
                ,wh_services:reset_category(?CATEGORY_ID, Services)
                ,Keys
               ).

-spec maybe_update_key(ne_binary(), wh_json:object(), wh_services:services()) ->
                              wh_services:services().
maybe_update_key(Key, JObj, Services) ->
    case wh_json:get_integer_value(Key, JObj) of
        'undefined' ->
            lager:debug("~s isn't on request, skipping"),
            Services;
        Quantity ->
            lager:debug("updating ~s to ~p from request", [Key, Quantity]),
            wh_services:update(?CATEGORY_ID, Key, Quantity, Services)
    end.

-spec item_twoway() -> ne_binary().
-spec item_outbound() -> ne_binary().
-spec item_inbound() -> ne_binary().
item_twoway() -> ?ITEM_TWOWAY.
item_outbound() -> ?ITEM_OUTBOUND.
item_inbound() -> ?ITEM_INBOUND.
