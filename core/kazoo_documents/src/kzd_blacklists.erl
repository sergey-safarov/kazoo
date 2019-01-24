%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_blacklists).

-export([type/0]).
-export([new/0]).
-export([action/1, action/2, set_action/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([numbers_name/1, numbers_name/2, set_numbers_name/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2]).
-export([is_blacklist/1]).
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
set_should_block_anonymous(Doc, ShouldBlockAnonymous) ->
    kz_json:set_value([<<"should_block_anonymous">>], ShouldBlockAnonymous, Doc).
