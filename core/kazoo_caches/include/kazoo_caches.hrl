-ifndef(KAZOO_CACHES_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(KAPPS_CONFIG_CACHE, 'kapps_config_cache').
-define(KAPPS_CALL_CACHE, 'kapps_call_cache').
-define(KAPPS_GETBY_CACHE, 'kapps_getby_cache').
-define(KAPPS_LISTENER_CACHE, 'kapps_listener_cache').
-define(EXPIRE_PERIOD, 10 * ?MILLISECONDS_IN_SECOND).
-define(EXPIRE_PERIOD_MSG, 'expire_cache_objects').

-type callback_fun() :: fun((any(), any(), 'flush' | 'erase' | 'expire') -> any()).
-type callback_funs() :: [callback_fun()].
-type origin_tuple() :: {'db', kz_term:ne_binary(), kz_term:ne_binary()} | %% {db, Database, PvtType or Id}
                        {'type', kz_term:ne_binary(), kz_term:ne_binary()} | %% {type, PvtType, Id}
                        {'db', kz_term:ne_binary()} | %% {db, Database}
                        {'db', kz_term:ne_binary(), kz_term:ne_binary() | '_'} | %% {db, Database, Type}
                        {'database', kz_term:ne_binary()} | %% {database, Database} added for notify db create/delete
                        {'type', kz_term:ne_binary()}. %% {type, PvtType}
-type origin_tuples() :: [origin_tuple()].

-record(cache_obj, {key :: any()| '_' | '$1'
                   ,value :: any() | '_' | '$1' | '$2'
                   ,expires :: timeout() | '_' | '$3'
                   ,timestamp = kz_time:now_s() :: kz_time:gregorian_seconds() | '_' | '$4'
                   ,callback :: callback_fun() | '_' | '$2' | '$3' | '$5' | 'undefined'
                   ,origin :: origin_tuple() | origin_tuples() | '$1' | '_' | 'undefined'
                   }).

-type cache_obj() :: #cache_obj{}.
-type cache_objs() :: [cache_obj()].

-record(state, {name :: atom()
               ,tab :: ets:tab()
               ,pointer_tab :: ets:tab()
               ,monitor_tab :: ets:tab()
               ,new_channel_flush = 'false' :: boolean()
               ,channel_reconnect_flush = 'false' :: boolean()
               ,new_node_flush = 'false' :: boolean()
               ,expire_node_flush = 'false' :: boolean()
               ,expire_period = ?EXPIRE_PERIOD :: timeout()
               ,expire_period_ref :: reference()
               ,props = [] :: kz_term:proplist()
               ,has_monitors = 'false' :: boolean()
               }).

-type state() :: #state{}.

-define(KAZOO_CACHES_HRL, 'true').
-endif.
