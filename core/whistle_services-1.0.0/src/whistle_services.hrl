-ifndef(WHISTLE_SERVICES_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_log.hrl").

-include_lib("whistle_transactions/include/whistle_transactions.hrl").

-define(APP_NAME, <<"whistle_services">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(WHS_CONFIG_CAT, <<"services">>).

-define(SERVICES_CACHE, 'whistle_services_cache').

-type get_fun() :: fun((ne_binary(), ne_binary(), wh_services:services()) -> wh_services:services()).
-type update_fun() :: fun((ne_binary(), ne_binary(), integer(), wh_services:services()) ->
                                 wh_services:services()
                                     ).

-define(WHISTLE_SERVICES_HRL, 'true').
-endif.
