%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_ips).

-export([reconcile/1, reconcile/2
         ,reconcile_cascade/2
        ]).

-include("../whistle_services.hrl").

-define(CATEGORY, <<"ips">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
-spec reconcile(wh_services:services(), api_binary() | wh_proplist()) ->
                       wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    case kz_ips:assigned(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current dedicated ips in service: ~p", [_R]),
            Services;
        {'ok', JObjs} ->
            S = wh_services:reset_category(?CATEGORY, Services),
            wh_services:update(?CATEGORY, <<"dedicated">>, length(JObjs), S)
    end.

reconcile(Services, 'undefined') -> Services;
reconcile(Services0, IpType) when is_binary(IpType) ->
    Services1 = reconcile(Services0),
    Quantity = wh_services:updated_quantity(?CATEGORY, IpType, Services1),
    wh_services:update(?CATEGORY, IpType, Quantity+1, Services1);
reconcile(Services, Props) ->
    lists:foldl(
      fun(IP, S) ->
              reconcile_foldl(IP
                              ,S
                              ,fun wh_services:updated_quantity/3
                              ,fun wh_services:update/4
                             )
      end
      ,reconcile(Services)
      ,Props
     ).

reconcile_cascade(Services, 'undefined') -> Services;
reconcile_cascade(Services, <<_/binary>> = IpType) ->
    Quantity = wh_services:cascade_quantity(?CATEGORY, IpType, Services),
    ewh_services:update_cascade(?CATEGORY, IpType, Quantity+1, Services);
reconcile_cascade(Services, Props) ->
    lists:foldl(
      fun(IP, S) ->
              reconcile_foldl(IP
                              ,S
                              ,fun wh_services:cascade_quantity/3
                              ,fun wh_services:update_cascade/4
                             )
      end
      ,reconcile(Services)
      ,Props
     ).

-type ip_info() :: {ne_binary(), integer() | ne_binary()}.

-spec reconcile_foldl(ip_info(), wh_services:services(), get_fun(), update_fun()) ->
                             wh_services:services().
reconcile_foldl({Type, Quantity}, Services, GetFun, UpdateFun) ->
    OldQuantity = GetFun(?CATEGORY, Type, Services),
    UpdateFun(?CATEGORY
              ,Type
              ,OldQuantity + wh_util:to_integer(Quantity)
              ,Services
             ).
