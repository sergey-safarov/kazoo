%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_users).

-export([reconcile/1, reconcile/2
         ,reconcile_cascade/2
        ]).

-include("../whistle_services.hrl").

-define(SERVICE_CATEGORY, <<"users">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
-spec reconcile(wh_services:services(), api_binary()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['reduce'
                   ,'group'
                  ],
    case couch_mgr:get_results(AccountDb, <<"services/users">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current users in service: ~p", [_R]),
            Services;
        {'ok', []} -> wh_services:reset_category(?SERVICE_CATEGORY, Services);
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, S) ->
                                Item = wh_json:get_value(<<"key">>, JObj),
                                Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),
                                wh_services:update(?SERVICE_CATEGORY, Item, Quantity, S)
                        end
                        ,wh_services:reset_category(?SERVICE_CATEGORY, Services)
                        ,JObjs
                       )
    end.

reconcile(Services, 'undefined') -> Services;
reconcile(Services, UserType) ->
    do_reconcile(reconcile(Services)
                 ,UserType
                 ,fun wh_services:updated_quantity/3
                 ,fun wh_services:update/4
                ).

-spec reconcile_cascade(wh_services:services(), api_binary()) ->
                               wh_services:services().
reconcile_cascade(Services, 'undefined') -> Services;
reconcile_cascade(Services, UserType) ->
    do_reconcile(Services
                 ,UserType
                 ,fun wh_services:cascade_quantity/3
                 ,fun wh_services:update_cascade/4
                ).

-spec do_reconcile(wh_services:services(), ne_binary(), get_fun(), update_fun()) ->
                          wh_services:services().
do_reconcile(Services, UserType, GetFun, UpdateFun) ->
    Quantity = GetFun(?SERVICE_CATEGORY, UserType, Services),
    UpdateFun(?SERVICE_CATEGORY, UserType, Quantity+1, Services).
