%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_utils).

-include("kamdb.hrl").

%% API
-export([extract_realm/1, is_device/1]).

-spec extract_realm(ne_binary()) -> ne_binary().
extract_realm(Entity) ->
    case binary:split(Entity, <<"@">>) of
        [_, OnRealm] -> OnRealm;
        [JustRealm] -> JustRealm
    end.

-spec is_device(wh_json:object()) -> boolean().
is_device(JObj) ->
    <<"device">> =:= wh_json:get_value([<<"value">>, <<"type">>], JObj).
