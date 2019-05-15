%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_blacklist).

-export([lookup/3]).

-include("callflow.hrl").

-record(pattern, {actions :: kz_term:ne_binaries()
                 ,has_groups :: boolean()
                 ,names = [] :: kz_term:ne_binaries()
                 ,regex :: re:mp()
                 }).

-type pattern() :: #pattern{}.
-type patterns() :: [pattern()].
-type view_owner_key() :: kz_term:ne_binary() | 'null'.

-type lookup_ret() :: {'ok', kz_term:ne_binaries(), boolean()} | {'error', any()}.


-spec lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> lookup_ret().
lookup(Number, AccountId, OwnerId) when not is_binary(Number) ->
    lookup(kz_term:to_binary(Number), AccountId, OwnerId);
lookup(<<>>, _, _) ->
    {'error', 'invalid_number'};
lookup(Number, AccountId, OwnerId) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?BL_NUMBER_CACHE_KEY(AccountId, OwnerId, Number)) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Actions} -> {'ok', Actions, 'false'};
        {'error', 'not_found'} -> do_lookup(Number, AccountId, OwnerId)
    end.

-spec do_lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> lookup_ret().
do_lookup(Number, AccountId, OwnerId) ->
    case kzd_blacklists:fetch_number(AccountId, OwnerId, Number) of
        {'error', 'not_found'} when Number =/= ?NO_MATCH_BL ->
            lookup_patterns(Number, AccountId, OwnerId);
        {'error', 'not_found'} -> {'error', 'not_found'};
        {'ok', [JObj] } ->
            Brief = kz_json:get_value(<<"value">>, JObj),
            Actions = actions(Brief),
            cache_number(AccountId, OwnerId, Number, lists:usort(Actions))
    end.

-spec lookup_patterns(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) ->
                             {'ok', {kz_json:object(), kz_term:api_binary()}} |
                             {'error', any()}.
lookup_patterns(Number, AccountId, OwnerId) ->
    case fetch_patterns(AccountId, OwnerId) of
        {'ok', Patterns} -> lookup_blacklist_patterns(Patterns, Number, AccountId, OwnerId);
        _Error -> maybe_use_nomatch(Number, AccountId, OwnerId)
    end.

-spec fetch_patterns(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', patterns()} | {'error', 'not_found'}.
fetch_patterns(AccountId, OwnerId)->
    case kz_cache:fetch_local(?CACHE_NAME, ?BL_PATTERN_OWNERS_CACHE_KEY(AccountId)) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Owners} -> fetch_patterns(AccountId, OwnerId, lists:member(OwnerId, Owners));
        {'error', 'not_found'} ->
            lager:debug("blacklist pattern owners cache is empty, loading from database for account: ~s", [AccountId]),
            load_patterns(AccountId),
            fetch_patterns(AccountId, OwnerId)
    end.

-spec fetch_patterns(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> {'ok', patterns()} | {'error', 'not_found'}.
fetch_patterns(AccountId, 'undefined', 'false')->
    lager:debug("no blacklist patterns owned by account: ~s", [AccountId]),
    {'error', 'not_found'};
fetch_patterns(AccountId, OwnerId, 'false')->
    lager:debug("no blacklist patterns owned user with ID: ~s and account ID: ~s", [OwnerId, AccountId]),
    {'error', 'not_found'};
fetch_patterns(AccountId, OwnerId, 'true')->
    kz_cache:fetch_local(?CACHE_NAME, ?BL_PATTERN_CACHE_KEY(AccountId, OwnerId)).

-spec load_patterns(kz_term:ne_binary()) -> 'ok'.
load_patterns(AccountId) ->
    case kzd_blacklists:fetch_patterns(AccountId) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', JObjs} -> compile_patterns(AccountId, JObjs);
        {'error', _} = E -> E
    end.

-spec compile_patterns(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
compile_patterns(AccountId, JObjs) ->
    compile_patterns(AccountId, JObjs, []).

-spec compile_patterns(kz_term:ne_binary(), kz_json:objects(), kz_term:api_ne_binaries()) -> 'ok'.
compile_patterns(AccountId, [], Acc) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"blacklist">>}]}],
    Owners = lists:usort(Acc),
    kz_cache:store_local(?CACHE_NAME, ?BL_PATTERN_OWNERS_CACHE_KEY(AccountId), Owners, CacheOptions),
    'ok';
compile_patterns(AccountId, [JObj | JObjs], Acc) ->
    [Regex, ViewOwnerId] = kz_json:get_list_value(<<"key">>, JObj),
    OwnerId = parse_key(ViewOwnerId),
    Brief = kz_json:get_value(<<"value">>, JObj),
    Actions = actions(Brief),
    case re:compile(Regex) of
        {'ok', {'re_pattern', Groups, _, _, _} = MP}
          when Groups =:= 0 ->
            Pattern = #pattern{actions=Actions, regex=MP, has_groups='false'},
            cache_pattern(AccountId, OwnerId, Pattern),
            compile_patterns(AccountId, JObjs, [OwnerId | Acc]);
        {'ok', MP} ->
            {'namelist', Names} = re:inspect(MP, 'namelist'),
            Pattern = #pattern{actions=Actions, regex=MP, names=Names, has_groups='true'},
            cache_pattern(AccountId, OwnerId, Pattern),
            compile_patterns(AccountId, JObjs, [OwnerId | Acc]);
        _Err ->
            lager:debug("unexpected result compiling regular expression : ~p", [_Err]),
            compile_patterns(AccountId, JObjs, Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc CouchDB view reult key contians 'null' if lacklist owner is account and 'owner_id' is 'undefined'
%% @end
%%------------------------------------------------------------------------------
-spec parse_key(view_owner_key()) -> kz_term:api_ne_binary().
parse_key('null') ->
    'undefined';
parse_key(Owner) ->
    Owner.

%%------------------------------------------------------------------------------
%% @doc Filter patern view value for enabled blacklist and return list of blacklist actions.
%% @end
%%------------------------------------------------------------------------------
-spec actions(kz_json:objects()) -> kz_term:ne_binaries().
actions(JObjs) ->
    Enabled = lists:filter(fun(JObj1) -> kz_json:get_boolean_value(<<"enabled">>, JObj1, 'true') end, JObjs),
    lists:map(fun(JObj2) -> kz_json:get_binary_value(<<"action">>, JObj2, <<"block">>) end, Enabled).

-spec cache_pattern(kz_term:ne_binary(), kz_term:api_ne_binary(), pattern()) -> 'ok' | 'error'.
cache_pattern(AccountId, OwnerId, Pattern) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"blacklist">>}]}
                   ,{'expires', ?MILLISECONDS_IN_HOUR}
                   ],
    kz_cache:store_local(?CACHE_NAME, ?BL_PATTERN_CACHE_KEY(AccountId, OwnerId), Pattern, CacheOptions).

-spec cache_number(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) -> lookup_ret().
cache_number(AccountId, OwnerId, Number, Actions) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"blacklist">>}]}
                   ,{'expires', ?MILLISECONDS_IN_HOUR}
                   ],
    kz_cache:store_local(?CACHE_NAME, ?BL_NUMBER_CACHE_KEY(AccountId, OwnerId, Number), Actions, CacheOptions),
    case Actions of
        [] -> {'error', 'not_found'};
        _ -> {'ok', Actions, Number =:= ?NO_MATCH_BL}
    end.

-spec maybe_use_nomatch(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> lookup_ret().
maybe_use_nomatch(Number, AccountId, OwnerId) ->
    case knm_converters:is_reconcilable(Number, AccountId) of
        'true' -> try_nomatch(Number, AccountId, OwnerId);
        'false' ->
            lager:info("can't use no_match: number not all digits: ~s", [Number]),
            cache_number(AccountId, OwnerId, Number, [])
    end.

-spec try_nomatch(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> lookup_ret().
try_nomatch(Number, AccountId, OwnerId) ->
    case lookup(?NO_MATCH_BL, AccountId, OwnerId) of
        {'error', 'not_found'} ->
            cache_number(AccountId, OwnerId, Number, []);
        {'ok', _, _} = R -> R
    end.

-spec lookup_blacklist_patterns(patterns(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> lookup_ret().
lookup_blacklist_patterns(Patterns, Number, AccountId, OwnerId) ->
    case test_callflow_patterns(Patterns, Number) of
        'no_match' -> maybe_use_nomatch(Number, AccountId, OwnerId);
        {_Match, #pattern{actions=Actions}} ->
            cache_number(AccountId, OwnerId, Number, Actions)
    end.

-type test_pattern_acc() ::  {binary(), pattern() | 'undefined'}.

-spec test_callflow_patterns(patterns(), kz_term:ne_binary()) -> 'no_match' | test_pattern_acc().
test_callflow_patterns(Patterns, Number) ->
    test_callflow_patterns(Patterns, Number, {<<>>, 'undefined'}).

-spec test_callflow_patterns(patterns(), kz_term:ne_binary(), test_pattern_acc()) ->
                                    'no_match' | test_pattern_acc().
test_callflow_patterns([], _, {_, 'undefined'}) -> 'no_match';
test_callflow_patterns([], _, Result) -> Result;
test_callflow_patterns([#pattern{regex=Regex}=Pattern |T], Number, {Matched, P}=Result) ->
    case re:run(Number, Regex, match_options(Pattern)) of
        {'match', Groups} ->
            case hd(lists:sort(fun(A, B) -> byte_size(A) >= byte_size(B) end, Groups)) of
                Match when P =:= 'undefined'
                           orelse byte_size(Match) > byte_size(Matched) ->
                    test_callflow_patterns(T, Number, {Match, Pattern});
                _ -> test_callflow_patterns(T, Number, Result)
            end;
        _ ->
            test_callflow_patterns(T, Number, Result)
    end.

-spec match_options(pattern()) -> list().
match_options(#pattern{has_groups='true'}) ->
    [{'capture', 'all_but_first', 'binary'}];
match_options(_) ->
    [{'capture', 'all', 'binary'}].
