%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(nga911messaging_app).

-behaviour(application).

%%-include("doodle.hrl").
%%-define(ACCOUNT_CRAWLER_BINDING, <<"tasks.account_crawler">>).

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    lager:debug("Start listening for SMS/MMS messages from Telnyx carrier"),
    nga911messaging_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
%%    _ = kazoo_bindings:unbind(?ACCOUNT_CRAWLER_BINDING,
%%                              'doodle_maintenance',
%%                              'start_check_sms_by_account'),
    'ok'.
