%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc Functions for interacting with the underlying system (assumes linux).
%%% @author Sean Wysor
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_os).

-export([cmd/1, cmd/2, cmd/3]).

-export([kill/1, brutally_kill/1]).

-export([run_cmd/3]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%%------------------------------------------------------------------------------
%% @doc execute system commands
%%
%% execute a system command safely with protection from unexpectedly large output or commands that
%% run forever.
%%
%% For commands which require injection of arguments, simply name the arguments in the command with
%% bash variables, add the variable names and values to a proplist and they will be injected into
%% the command via enviornment variables. This decouples the ordering from the commands, which was
%% a limitation in the customizability of the old io_lib:format/os:cmd method of running/storing
%% user customizable commands.
%%
%%    Examples:
%%        {'ok', 10} = kz_os:cmd(<<"echo -n 10">>).
%%        {'ok', 10} = kz_os:cmd(<<"echo -n $ARG">>, [{<<"ARG">>, 10}]).
%%        {'ok', 10} = kz_os:cmd(<<"echo -n $ARG">>, [{<<"ARG">>, 10}], [{<<"timeout">>, 100}]).
%%
%% For commands that do not require injection of arguments, simply use cmd/1 or specify an empty list
%% in cmd/3. cmd/2 and cmd/1 are provided when default options are fine or no args are required.
%%
%% cmd/3 permits passthrough of selected ports options as well as timeout and size thresholds.
%% `binary', `exit_status', `use_stdio', and `stderr_to_stdout' are always set as these ports options
%% are assumed in processing the command.
%%
%% <ul>
%%    <li><strong>`timeout', value</strong>The time the command will wait with no output before the process is killed.</li>
%%    <li><strong>`absolute_timeout', value</strong>The absolute time a command is permitted to run before it is killed.</li>
%%    <li><strong>`max_size'</strong>The max size of output to allow before the process is killed.</li>
%%    <li><strong>`read_mode'</strong>The readmode `{packet, 1}' is available for handling large payloads, but `stream' is used by default.</li>
%%    <li></li>
%% <ul>
%%
%% @end
%%------------------------------------------------------------------------------

-spec cmd(kz_term:ne_binary()) ->
                 {'ok', kz_term:ne_binary()}|{'error', atom(), binary()}.
cmd(Command) ->
    cmd(Command, []).

-spec cmd(kz_term:ne_binary(), kz_term:proplist()) ->
                 {'ok', kz_term:ne_binary()}|{'error', atom(), binary()}.
cmd(Command, Args) ->
    cmd(Command, Args, []).

-spec cmd(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) ->
                 {'ok', kz_term:ne_binary()}|{'error', atom(), binary()}.
cmd(Command, Args, Options) ->
    Owner = props:get_value(<<"owner">>, Options, self()),
    CmdTimeout = props:get_value(<<"absolute_timeout">>, Options, 10000),
    {Pid, Ref} =
        erlang:spawn_monitor(?MODULE
                            ,'run_cmd'
                            ,[Command
                             ,Args
                             ,props:set_value(<<"owner">>, Owner, Options)
                             ]
                            ),
        monitor_cmd(Pid, Ref, CmdTimeout, 'undefined').


monitor_cmd(Pid, Ref, CmdTimeout, Port) ->
    receive
        {'port', NewPort} ->
            monitor_cmd(Pid, Ref, CmdTimeout, NewPort);
        {'ok', _}=Ok -> Ok;
        {'error', _, _}=Error -> Error;
        {'DOWN', Ref, _, Pid, Reason} ->
            lager:debug("cmd process died unexpectedly with reason: ~p", [Reason]),
            {'error', 'died_unexpectedly', <<>>}
    after
        CmdTimeout ->
            maybe_kill_cmd(Port),
            _ = erlang:demonitor(Ref),
            _ = erlang:exit(Pid, 'timeout'),
            {'error', 'absolute_timeout', <<>>}
    end.

-spec run_cmd(kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) ->
                 'no_return'.
run_cmd(Command, Args, Options) ->
    OwnerPid = props:get_value(<<"owner">>, Options),
    OwnerRef = erlang:monitor('process', OwnerPid),
    Timeout = props:get_value(<<"timeout">>, Options, 10000),
    MaxSize = props:get_value(<<"max_size">>, Options, 100000),
    ReadMode = props:get_value(<<"read_mode">>, Options, {'line', MaxSize}),
    PortOptions = [ReadMode
                  ,'binary'
                  ,'exit_status'
                  ,'use_stdio'
                  ,'stderr_to_stdout'
                  ,{'env', opts_to_strings(Args)}
                  ],
    Port = erlang:open_port({'spawn', kz_term:to_list(Command)}, PortOptions),
    OwnerPid ! {'port', Port},
    Out = cmd_receive({Port, MaxSize, Timeout, OwnerRef}, <<>>),
    OwnerPid ! Out.

-spec cmd_receive({port(), integer(), integer(), reference()}, kz_term:binary()) ->
                      {'ok', kz_term:ne_binary()}|{'error', atom(), kz_term:ne_binary()}.
cmd_receive({Port, MaxSize, Timeout, OwnerRef}=LoopParams, Acc) ->
    receive
        {'DOWN', OwnerRef, _, _, _} when OwnerRef =/= 'ok' ->
            lager:debug("parent died - no reason to continue"),
            {'error', 'parent_died', <<>>};
		{Port, {data, {eol, Data}}} ->
            NewAcc = <<Acc/binary,Data/binary,(kz_term:to_binary("\n"))/binary>>,
            case byte_size(NewAcc) of
                Len when Len >= MaxSize ->
                    _ = maybe_kill_cmd(Port),
                    {'error', 'max_size', NewAcc};
                _ -> cmd_receive(LoopParams, NewAcc)
            end;
		{Port, {data, {noeol, Data}}} ->
            NewAcc = <<Acc/binary,Data/binary>>,
            case byte_size(NewAcc) of
                Len when Len >= MaxSize ->
                    _ = maybe_kill_cmd(Port),
                    {'error', 'max_size', NewAcc};
                _ -> cmd_receive(LoopParams, NewAcc)
            end;
		{Port, {data, Data}} ->
            NewAcc = <<Acc/binary,Data/binary>>,
            case byte_size(NewAcc) of
                Len when Len >= MaxSize ->
                    _ = maybe_kill_cmd(Port),
                    {'error', 'max_size', NewAcc};
                _ -> cmd_receive(LoopParams, NewAcc)
            end;
        {Port, {'exit_status', 0}} ->
            {'ok', Acc};
        {Port, {'exit_status', Status}} ->
            {'error', Status, Acc};
        Any ->
            lager:debug("unhandled message ~p", [Any]),
            cmd_receive(LoopParams, Acc)
    after
        Timeout ->
            _ = maybe_kill_cmd(Port),
            {'error', 'timeout', Acc}
    end.

-spec maybe_kill_cmd(kz_term:api_port()) -> 'ok'.
maybe_kill_cmd('undefined') ->
    'ok';
maybe_kill_cmd(Port) ->
    try erlang:port_info(Port, 'os_pid') of
        {'os_pid', OsPid} -> kill(OsPid)
    catch
        _ -> 'ok'
    end.

-spec kill(integer()) -> 'ok'.
kill(OsPid) ->
    lager:debug("killing pid: ~p with sigint, good day sir!", [OsPid]),
    case os:cmd(io_lib:format("kill -6 ~b", [OsPid])) of
        "" -> 'ok';
        _ ->
            lager:debug("sigint kill failed"),
            brutally_kill(OsPid)
    end.

-spec brutally_kill(integer()) -> 'ok'.
brutally_kill(OsPid) ->
    lager:debug("brutally killing ~p, I said good day!", [OsPid]),
    case os:cmd(io_lib:format("kill -9 ~b", [OsPid])) of
        "" -> 'ok';
        _ ->
            lager:debug("brutal kill failed, process returned the finger"),
            'error'
    end.

-spec opts_to_strings(kz_term:proplist()) -> kz_term:proplist().
opts_to_strings(Args) ->
    opts_to_strings(Args, []).

-spec opts_to_strings(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
opts_to_strings([{Key, Value}|Args], Acc) ->
    opts_to_strings(Args, Acc ++ [{kz_term:to_list(Key), kz_term:to_list(Value)}]);
opts_to_strings([], Acc) ->
    Acc.
