%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author Sean Wysor
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_openoffice_server).

-behaviour(gen_server).

%% API
-export([start_link/0
        ,convert/2
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include_lib("kazoo_convert/include/kz_convert.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, {'local', ?MODULE}).

-define(TIMEOUT_LIFETIME, 600 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT_MESSAGE, {'$kz_openoffice_server', 'file_timeout'}).

-record(state, {queue :: list()
               ,timer_ref ::  reference()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% @doc convert an openoffice document, respecting the one document at a time
%% constraint.
%%
%% Openoffice does not permit simultanious conversions, so this gen_server is a
%% serialization mechanism for conversions from openoffice formats.
%% @end
%%------------------------------------------------------------------------------
-spec convert(kz_term:ne_binary(), map()) -> {'ok', kz_term:ne_binary()}|{'error', kz_term:ne_binary()}.
convert(Source, Options) ->
    gen_server:call(?MODULE, {'convert', Source, Options}, ?TIMEOUT_LIFETIME).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()} |
                      {'stop', any()}.
init([]) ->
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call({atom(), {'file', kz_term:ne_binary()}, map()}, kz_term:pid_ref(), state()) ->
                         kz_types:handle_call_ret_state(state()).
handle_call(stop, _From, #state{} = State) ->
    {stop, normal, ok, State};
handle_call({'convert', Source, Options}, _From, State) ->
    {'reply', openoffice_to_pdf(Source, Options), State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', TRef, ?TIMEOUT_MESSAGE}, #state{timer_ref=TRef}=State) ->
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("openoffice_server going down: ~p", [_Reason]).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
%%-spec start_timer() -> reference().
%%start_timer() ->
%%    erlang:start_timer(?TIMEOUT_LIFETIME, self(), ?TIMEOUT_MESSAGE).
%%
%%-spec stop_timer(reference()) -> integer() | boolean() | 'ok'.
%%stop_timer(Ref) ->
%%    erlang:cancel_timer(Ref).

-spec openoffice_to_pdf(kz_term:ne_binary(), map()) -> {atom(), kz_term:ne_binary()}.
openoffice_to_pdf(FromPath, #{<<"job_id">> := JobId, <<"tmp_dir">> := TmpDir}) ->
    ToPath = filename:join(TmpDir, [JobId, <<".pdf">>]),
    Command = io_lib:format(?CONVERT_OO_COMMAND, [?OPENOFFICE_SERVER, ToPath, FromPath]),
    lager:debug("converting file ~s to ~s with command: ~s", [FromPath, ToPath, Command]),
    try os:cmd(Command) of
        "success" ->
            {'ok', ToPath};
        Else ->
            lager:debug("could not convert file ~s error: ~p", [FromPath, Else]),
            kz_util:delete_file(ToPath),
            {'error', <<"failed to convert">>}
    catch
        Else ->
            lager:debug("could not convert file ~s error: ~p", [FromPath, Else]),
            kz_util:delete_file(ToPath),
            {'error', <<"error on convert">>}
    end.
