-module(kz_cache_listener).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

%% API
-export([handle_event/2]).

-include("kz_caches.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #state{tab=Tab}=State) ->
    case (V=kapi_conf:doc_update_v(JObj))
        andalso (kz_api:node(JObj) =/= kz_term:to_binary(node())
                 orelse kz_json:get_atom_value(<<"Origin-Cache">>, JObj) =/= ets:info(Tab, 'name')
                )
    of
        'true' -> handle_document_change(JObj, State);
        'false' when V -> 'ok';
        'false' -> lager:error("payload invalid for kapi_conf: ~p", [JObj])
    end,
    'ignore'.

%%%=============================================================================
%%% `gen_server' callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | timeout() | kz_term:proplist()]) -> {'ok', state()}.
init([Name, ExpirePeriod, Props]) ->
    kz_util:put_callid(Name),
    init(Name, ExpirePeriod, Props, props:get_value('origin_bindings', Props)).

-spec init(atom(), timeout(), kz_term:proplist(), kz_term:api_list()) -> {'ok', state()}.
init(Name, ExpirePeriod, Props, _Bindings) ->
    kapi_conf:declare_exchanges(),
    init(Name, ExpirePeriod, Props).

-spec init(atom(), timeout(), kz_term:proplist()) -> {'ok', state()}.
init(Name, ExpirePeriod, Props) ->
    Tab = ets:new(Name
                 ,['set', 'public', 'named_table', {'keypos', #cache_obj.key}]
                 ),
    PointerTab = ets:new(pointer_tab(Name)
                        ,['bag', 'public', {'keypos', #cache_obj.key}]
                        ),
    MonitorTab = ets:new(monitor_tab(Name)
                        ,['bag', 'public', {'keypos', #cache_obj.key}]
                        ),

    _ = case props:get_value('new_node_flush', Props) of
            'true' -> kz_nodes:notify_new();
            _ -> 'ok'
        end,
    _ = case props:get_value('expire_node_flush', Props) of
            'true' -> kz_nodes:notify_expire();
            _ -> 'ok'
        end,
    {'ok', #state{name=Name
                 ,tab=Tab
                 ,pointer_tab=PointerTab
                 ,monitor_tab=MonitorTab
                 ,new_channel_flush=props:get_value('new_channel_flush', Props)
                 ,channel_reconnect_flush=props:get_value('channel_reconnect_flush', Props)
                 ,new_node_flush=props:get_value('new_node_flush', Props)
                 ,expire_node_flush=props:get_value('expire_node_flush', Props)
                 ,expire_period=ExpirePeriod
                 ,expire_period_ref=start_expire_period_timer(ExpirePeriod)
                 ,props=Props
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('stop', _From, State) ->
    lager:debug("recv stop from ~p", [_From]),
    {'stop', 'normal', State};
handle_call(_, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{tab=Tab, pointer_tab=PTab, monitor_tab=MTab}) ->
    lager:debug("terminating ~p(~p)", [self(), Tab]),
    'true' = ets:delete(Tab),
    'true' = ets:delete(PTab),
    'true' = ets:delete(MTab),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
-spec pointer_tab(atom()) -> atom().
pointer_tab(Tab) ->
    to_tab(Tab, "_pointers").

-spec monitor_tab(atom()) -> atom().
monitor_tab(Tab) ->
    to_tab(Tab, "_monitors").

-spec to_tab(atom(), string()) -> atom().
to_tab(Tab, Suffix) ->
    kz_term:to_atom(kz_term:to_list(Tab) ++ Suffix, 'true').

-spec start_expire_period_timer(pos_integer()) -> reference().
start_expire_period_timer(ExpirePeriod) ->
    erlang:start_timer(ExpirePeriod, self(), ?EXPIRE_PERIOD_MSG).

-spec handle_document_change(kz_json:object(), state()) -> 'ok' | 'false'.
handle_document_change(JObj, State) ->
    'true' = kapi_conf:doc_update_v(JObj),
    Db = kz_json:get_value(<<"Database">>, JObj),
    Type = kz_json:get_value(<<"Type">>, JObj),
    Id = kz_json:get_value(<<"ID">>, JObj),
    Keys = handle_document_change(Db, Type, Id, State),
    Keys =/= []
        andalso lager:debug("removed ~p keys for ~s/~s/~s", [length(Keys), Db, Id, Type]).

-spec handle_document_change(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), state()) ->
                                    list().
handle_document_change(Db, <<"database">>, _Id, #state{pointer_tab=PTab}=State) ->
    MatchSpec = match_db_changed(Db),
    lists:foldl(fun(Obj, Removed) -> erase_changed(Obj, Removed, State) end
               ,[]
               ,ets:select(PTab, MatchSpec)
               );
handle_document_change(Db, Type, Id ,#state{pointer_tab=PTab}=State) ->
    MatchSpec = match_doc_changed(Db, Type, Id),
    Objects = ets:select(PTab, MatchSpec),
    lists:foldl(fun(Obj, Removed) -> erase_changed(Obj, Removed, State) end, [], Objects).

-spec match_db_changed(kz_term:ne_binary()) -> ets:match_spec().
match_db_changed(Db) ->
    [{#cache_obj{origin = {'db', Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'db', Db, '_'}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'type', <<"database">>, Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ].

-spec match_doc_changed(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> ets:match_spec().
match_doc_changed(Db, Type, Id) ->
    [{#cache_obj{origin = {'db', Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'db', Db, Type}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'db', Db, Id}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'type', Type, Id}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'type', Type}, _ = '_'}
     ,[]
     ,['$_']
     }
    ].

-spec erase_changed(cache_obj(), list(), state()) -> list().
erase_changed(#cache_obj{key=Key}, Removed, State) ->
    case lists:member(Key, Removed) of
        'true' ->
            Removed;
        'false' ->
            lager:debug("removing updated cache object ~-300p", [Key]),
            'true' = erase_changed(Key, State),
            [Key | Removed]
    end.

-spec erase_changed(any(), state()) -> 'true'.
erase_changed(Key, #state{tab=Tab
                         ,pointer_tab=PointerTab
                         ,monitor_tab=MonitorTab
                         }) ->
    maybe_exec_erase_callbacks(Tab, Key),
    maybe_remove_object(Tab, Key),
    maybe_remove_object(PointerTab, Key),
    maybe_remove_object(MonitorTab, Key).

-spec maybe_exec_erase_callbacks(ets:tab(), cache_obj() | any()) -> 'ok'.
maybe_exec_erase_callbacks(_Tab
                          ,#cache_obj{callback=Fun
                                     ,value=Value
                                     ,key=Key
                                     }
                          ) when is_function(Fun, 3) ->
    kz_util:spawn(Fun, [Key, Value, 'erase']),
    'ok';
maybe_exec_erase_callbacks(_Tab, #cache_obj{}) ->
    'ok';
maybe_exec_erase_callbacks(Tab, Key) ->
    try ets:lookup_element(Tab, Key, #cache_obj.callback) of
        Fun when is_function(Fun, 3) ->
            kz_util:spawn(fun exec_erase_callbacks/3, [Tab, Key, Fun]),
            'ok';
        _Else ->
            'ok'
    catch
        'error':'badarg' -> 'ok'
    end.

-spec exec_erase_callbacks(ets:tab(), any(), callback_fun()) -> any().
exec_erase_callbacks(Tab, Key, Fun) ->
    Value = ets:lookup_element(Tab, Key, #cache_obj.value),
    Fun(Key, Value, 'erase').

-spec maybe_remove_object(ets:tab(), cache_obj() | any()) -> 'true'.
maybe_remove_object(Tab, #cache_obj{key = Key}) ->
    maybe_remove_object(Tab, Key);
maybe_remove_object(Tab, Key) ->
    ets:delete(Tab, Key).
