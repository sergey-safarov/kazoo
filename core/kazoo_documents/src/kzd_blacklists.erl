%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_blacklists).

-export([type/0]).
-export([new/0]).
-export([fetch/2, fetch/3]).
-export([action/1, action/2, set_action/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2, format_numbers/1]).
-export([numbers_name/1, numbers_name/2, set_numbers_name/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2, set_owner_id/3]).
-export([is_blacklist/1]).
-export([is_valid_owner_id/2]).
-export([is_number_blacklisted/4]).
-export([should_block_anonymous/1, should_block_anonymous/2, set_should_block_anonymous/2]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"blacklists">>).

-spec type() -> kz_term:ne_binary().
type() -> <<"blacklists">>.

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec fetch(kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
                   {'ok', doc()} |
                   kz_datamgr:data_error().
fetch(Account, BlacklistId) ->
    fetch(Account, BlacklistId, #{}).

-spec fetch(kz_term:api_ne_binary(), kz_term:api_ne_binary(), map()) ->
                   {'ok', doc()} |
                   kz_datamgr:data_error().
fetch('undefined', _BlacklistId, _Options) ->
    {'error', 'invalid_db_name'};
fetch(_Account, 'undefined', _Options) ->
    {'error', 'not_found'};
fetch(Account, BlacklistId=?NE_BINARY, Options) ->
    AccountDb = kz_util:format_account_db(Account),
    case kz_datamgr:open_cache_doc(AccountDb, BlacklistId) of
        {'error', _R}=E -> E;
        {'ok', Doc} -> maybe_filter_fetch(Doc, Options)
    end.

-spec maybe_filter_fetch(doc(), map()) ->
                                {'ok', doc()} |
                                kz_datamgr:data_error().
maybe_filter_fetch(Doc, #{<<"action">> := Action}=Options) ->
    case action(Doc) of
        Action -> maybe_filter_fetch(Doc, maps:remove(<<"action">>, Options));
        _ ->
            Value = kz_term:to_binary(Action),
            {'error', <<"blacklist 'action' value is not match ", Value/binary>>}
    end;
maybe_filter_fetch(Doc, #{<<"enabled">> := Enabled}=Options) ->
    case enabled(Doc) of
        Enabled -> maybe_filter_fetch(Doc, maps:remove(<<"enabled">>, Options));
        _ ->
            Value = kz_term:to_binary(Enabled),
            {'error', <<"blacklist 'enabled' value is not match ", Value/binary>>}
    end;
maybe_filter_fetch(Doc, #{<<"should_block_anonymous">> := ShouldBlockAnonymous}=Options) ->
    case should_block_anonymous(Doc) of
        ShouldBlockAnonymous -> maybe_filter_fetch(Doc, maps:remove(<<"should_block_anonymous">>, Options));
        _ ->
            Value = kz_term:to_binary(ShouldBlockAnonymous),
            {'error', <<"blacklist 'should_block_anonymous' value is not match ", Value/binary>>}
    end;
maybe_filter_fetch(Doc, _Options) ->
    {'ok', Doc}.

-spec action(doc()) -> binary().
action(Doc) ->
    action(Doc, <<"block">>).

-spec action(doc(), Default) -> binary() | Default.
action(Doc, Default) ->
    kz_json:get_binary_value([<<"action">>], Doc, Default).

-spec set_action(doc(), binary()) -> doc().
set_action(Doc, Action) ->
    kz_json:set_value([<<"action">>], Action, Doc).

-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, true).

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec numbers(doc()) -> kz_json:object().
numbers(Doc) ->
    numbers(Doc, kz_json:new()).

-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc, Default) ->
    kz_json:get_json_value([<<"numbers">>], Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"numbers">>], Numbers, Doc).

-spec format_numbers(kz_term:api_object()) -> kz_term:api_object().
format_numbers('undefined') ->
    'undefined';
format_numbers(Doc) ->
    case is_blacklist(Doc) of
        'true' ->
            Numbers = kz_json:get_value(<<"numbers">>, Doc),
            format_numbers(Numbers);
        'false' -> kz_json:map(fun format_number_map/2, Doc)
    end.

-spec format_number_map(kz_term:ne_binary(), kz_json:object()) ->
                               {kz_term:ne_binary(), kz_json:object()}.
format_number_map(Number, Data) ->
    {knm_converters:normalize(Number), Data}.

-spec numbers_name(doc()) -> kz_term:api_binary().
numbers_name(Doc) ->
    numbers_name(Doc, 'undefined').

-spec numbers_name(doc(), Default) -> binary() | Default.
numbers_name(Doc, Default) ->
    kz_json:get_binary_value([<<"numbers">>, <<"name">>], Doc, Default).

-spec set_numbers_name(doc(), binary()) -> doc().
set_numbers_name(Doc, NumbersName) ->
    kz_json:set_value([<<"numbers">>, <<"name">>], NumbersName, Doc).

-spec owner_id(doc()) -> kz_term:api_ne_binary().
owner_id(Doc) ->
    owner_id(Doc, 'undefined').

-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"owner_id">>], Doc, Default).

-spec set_owner_id(doc(), kz_term:ne_binary()) -> doc().
set_owner_id(Doc, 'undefined') ->
    kz_json:delete_key([<<"owner_id">>], Doc);
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value([<<"owner_id">>], OwnerId, Doc).

-spec set_owner_id(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> doc().
set_owner_id(Doc, AccountId, AccountId) ->
    set_owner_id(Doc, 'undefined', AccountId);
set_owner_id(Doc, OwnerId, AccountId) ->
    case is_valid_owner_id(OwnerId, AccountId) of
        'true' -> set_owner_id(Doc, OwnerId);
        'false' ->
            lager:notice("provided document id -s of account ~s have type not account, user or device", [OwnerId, AccountId]),
            set_owner_id(Doc, 'undefined')
    end.

-spec is_valid_owner_id(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_valid_owner_id('undefined', AccountId) ->
    %% If owner not defined, then blacklist is owned by account
    kzd_accounts:is_account(AccountId);
is_valid_owner_id(AccountId, AccountId) ->
    kzd_accounts:is_account(AccountId);
is_valid_owner_id(OwnerId, AccountId) ->
    kzd_accounts:is_account(AccountId)
        andalso (kzd_users:is_user(AccountId, OwnerId)
                 orelse kzd_devices:is_device(AccountId, OwnerId)).

-spec is_blacklist(kz_term:api_object()) -> boolean().
is_blacklist('undefined') -> 'false';
is_blacklist(Doc) ->
    kz_doc:type(Doc) =:= type().

-spec should_block_anonymous(doc()) -> kz_term:api_boolean().
should_block_anonymous(Doc) ->
    should_block_anonymous(Doc, 'undefined').

-spec should_block_anonymous(doc(), Default) -> boolean() | Default.
should_block_anonymous(Doc, Default) ->
    kz_json:get_boolean_value([<<"should_block_anonymous">>], Doc, Default).

-spec set_should_block_anonymous(doc(), boolean()) -> doc().
set_should_block_anonymous(Doc, 'undefined') ->
    kz_json:delete_key([<<"should_block_anonymous">>], Doc);
set_should_block_anonymous(Doc, ShouldBlockAnonymous) ->
    kz_json:set_value([<<"should_block_anonymous">>], ShouldBlockAnonymous, Doc).

-spec is_number_blacklisted(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary() | kz_term:api_ne_binaries(), map()) -> boolean().
is_number_blacklisted('undefined', _AccountId, _BlacklistsId, _Options) ->
    'false';
is_number_blacklisted(_Number, 'undefined', _BlacklistsId, _Options) ->
    'false';
is_number_blacklisted(Number, AccountId, BlacklistId=?NE_BINARY , Options) ->
    is_number_blacklisted(Number, AccountId, [BlacklistId] , Options);
is_number_blacklisted(_Number, _AccountId, [], _Options) ->
    'false';
is_number_blacklisted(Number, AccountId, [BlacklistId | Rest], Options) ->
    case fetch(AccountId, BlacklistId, Options) of
        {'error', Message} ->
            lager:debug("could not fetch ~s blacklist in ~s account: ~s", [BlacklistId, AccountId, Message]),
            is_number_blacklisted(Number, AccountId, Rest, Options);
        {'ok', Doc} ->
            case kz_json:get_value(Number, numbers(Doc)) of
                'undefined' -> is_number_blacklisted(Number, AccountId, Rest, Options);
                _ -> 'true'
            end
    end.
