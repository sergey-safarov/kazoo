%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_blacklists).

-export([new/0]).
-export([action/1, action/2, set_action/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([numbers_name/1, numbers_name/2, set_numbers_name/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2]).
-export([patterns/1, patterns/2, set_patterns/2]).
-export([patterns_name/1, patterns_name/2, set_patterns_name/2]).
-export([should_block_anonymous/1, should_block_anonymous/2, set_should_block_anonymous/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"blacklists">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

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

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

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
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value([<<"owner_id">>], OwnerId, Doc).

-spec patterns(doc()) -> kz_json:object().
patterns(Doc) ->
    patterns(Doc, kz_json:new()).

-spec patterns(doc(), Default) -> kz_json:object() | Default.
patterns(Doc, Default) ->
    kz_json:get_json_value([<<"patterns">>], Doc, Default).

-spec set_patterns(doc(), kz_json:object()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value([<<"patterns">>], Patterns, Doc).

-spec patterns_name(doc()) -> kz_term:api_binary().
patterns_name(Doc) ->
    patterns_name(Doc, 'undefined').

-spec patterns_name(doc(), Default) -> binary() | Default.
patterns_name(Doc, Default) ->
    kz_json:get_binary_value([<<"patterns">>, <<"name">>], Doc, Default).

-spec set_patterns_name(doc(), binary()) -> doc().
set_patterns_name(Doc, PatternsName) ->
    kz_json:set_value([<<"patterns">>, <<"name">>], PatternsName, Doc).

-spec should_block_anonymous(doc()) -> kz_term:api_boolean().
should_block_anonymous(Doc) ->
    should_block_anonymous(Doc, 'undefined').

-spec should_block_anonymous(doc(), Default) -> boolean() | Default.
should_block_anonymous(Doc, Default) ->
    kz_json:get_boolean_value([<<"should_block_anonymous">>], Doc, Default).

-spec set_should_block_anonymous(doc(), boolean()) -> doc().
set_should_block_anonymous(Doc, ShouldBlockAnonymous) ->
    kz_json:set_value([<<"should_block_anonymous">>], ShouldBlockAnonymous, Doc).
