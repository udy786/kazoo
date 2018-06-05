%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fax_converter).

-export([convert/4
        ,do_openoffice_to_pdf/2
        ]).

-include_lib("kazoo_convert/include/kz_convert.hrl").

%%------------------------------------------------------------------------------
%% @doc This function will convert the `Content' from the `To' mimetype to the
%% `From' mimetype.
%%
%% Arguments Description:
%% <ul>
%% <li><strong>From:</strong> is a mimetype binary that specifies the format of
%% the Content passed in to convert.</li>
%% <li><strong>To:</strong> is a mimetype binary that specifies the format the
%% Content is to be converted.</li>
%% <li><strong>Content:</strong> content can be filepath to the source file or
%% a binary containing the contents of the file to be converted.</li>
%% </ul>
%%
%% Options Description:
%% <ul>
%% <li><strong>job_id:</strong> the unique ID of the job (like a fax job_id).
%% Used for naming the output file with the extension derived from the `To' format</li>
%% <li><strong>output_type:</strong> return the converted doc as a binary containing
%% the contensts or a path to the converted file.</li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec convert(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', filename:name()}, kz_term:proplist()) ->
                     {ok, any()} | {error, any()}.
convert(From, To, Content, Opts) ->
    lager:debug("converting document from ~s to ~s"),
    Options = maps:from_list(
                [{<<"from_format">>, From}
                ,{<<"to_format">>, To}
                ,{<<"job_id">>, props:get_value(<<"job_id">>, Opts, kz_binary:rand_hex(12))}
                 | props:delete_keys([<<"job_id">>], Opts)
                ]),
    Filename = save_file(Content, Options),
    run_convert(eval_format(From, To), To, Filename, Options).

-spec eval_format(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:list().
eval_format(<<"image/", _SubType/binary>>, ?TIFF_MIME) ->
    [fun image_to_tiff/2
    ];
eval_format(Format, Format) ->
    [];
eval_format(?TIFF_MIME, ?PDF_MIME) ->
    [fun tiff_to_pdf/2
    ];
eval_format(?PDF_MIME, ?TIFF_MIME) ->
    [fun pdf_to_tiff/2
    ];
eval_format(<<?OPENXML_MIME_PREFIX, _/binary>>, ?TIFF_MIME) ->
    [fun openoffice_to_pdf/2
    ,fun pdf_to_tiff/2
    ];
eval_format(CT, ?TIFF_MIME) when ?OPENOFFICE_COMPATIBLE(CT) ->
    [fun openoffice_to_pdf/2
    ,fun pdf_to_tiff/2
    ];
eval_format(<<?OPENOFFICE_MIME_PREFIX, _/binary>>, ?TIFF_MIME) ->
    [fun openoffice_to_pdf/2
    ,fun pdf_to_tiff/2
    ];
eval_format(<<?OPENXML_MIME_PREFIX, _/binary>>, ?PDF_MIME) ->
    [fun openoffice_to_pdf/2
    ];
eval_format(<<?OPENOFFICE_MIME_PREFIX, _/binary>>, ?PDF_MIME) ->
    [fun openoffice_to_pdf/2
    ];
eval_format(CT, ?PDF_MIME) when ?OPENOFFICE_COMPATIBLE(CT) ->
    [fun openoffice_to_pdf/2
    ];
eval_format(FromFormat, ToFormat) ->
    lager:error("invalid conversion requested from format: ~s to format: ~s", [FromFormat, ToFormat]),
    {'error', <<"invalid request to convert from format: ", FromFormat/binary, " to format: ", ToFormat/binary>>}.

-spec run_convert({atom(), kz_term:ne_binary()}, kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> kz_term:list().
run_convert({'error', _}=Error, _ToFormat, _FilePath, _Options) ->
    Error;
run_convert([Operation|Operations], ToFormat, FilePath, Options) ->
    case Operation(FilePath, Options) of
        {'ok', OutputPath} ->
            maybe_delete_previous_file(FilePath, OutputPath),
            run_convert(Operations, ToFormat, OutputPath, Options);

        Error -> Error
    end;
run_convert([], ToFormat, FilePath, Options) ->
    case validate_output(ToFormat, FilePath, Options) of
        {'ok', _} ->
            format_output(FilePath, Options);
        Error -> Error
    end.

-spec format_output(kz_term:ne_binary(), map()) ->
                           {'ok', kz_term:ne_binary()}|{'error', any()}.
format_output(FilePath, #{<<"output_type">> := 'binary'}) ->
    case file:read_file(FilePath) of
        {'ok', _}=Ok ->
            kz_util:delete_file(FilePath),
            Ok;
        Error -> Error
    end;
format_output(FilePath, #{<<"output_type">> := 'path'}) ->
    {'ok', FilePath};
format_output(FilePath, _Options) ->
    {'ok', FilePath}.

-spec maybe_delete_previous_file(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_delete_previous_file(Filename, Filename) ->
    'ok';
maybe_delete_previous_file(OldFilename, _NewFilename) ->
    kz_util:delete_file(OldFilename).

-spec save_file({'file', kz_term:ne_binary()}|kz_term:ne_binary(), map()) -> kz_term:ne_binary().
save_file({'file', FilePath}, _Options) ->
    FilePath;
save_file(Content, #{<<"tmp_dir">> := TmpDir
                    ,<<"job_id">> := JobId
                    ,<<"from_format">> := FromFormat
                    }) ->
    Ext = kz_mime:to_extension(FromFormat),
    FilePath = filename:join(TmpDir, [JobId, <<".initial.">>, Ext]),
    kz_util:write_file(FilePath, Content),
    FilePath.

-spec image_to_tiff({atom(), kz_term:ne_binary()}, map()) -> {atom(), kz_term:ne_binary()}.
image_to_tiff(FromPath, #{<<"job_id">> := JobId, <<"tmp_dir">> := TmpDir}) ->
    ToPath = filename:join(TmpDir, [JobId, <<".tiff">>]),
    run_convert_command(?CONVERT_IMAGE_COMMAND, FromPath, ToPath, TmpDir).

-spec tiff_to_pdf(kz_term:ne_binary(), map()) -> {atom(), kz_term:ne_binary()}.
tiff_to_pdf(FromPath, #{<<"job_id">> := JobId, <<"tmp_dir">> := TmpDir}) ->
    ToPath = filename:join(TmpDir, [JobId, <<".pdf">>]),
    run_convert_command(?CONVERT_TIFF_COMMAND, FromPath, ToPath, TmpDir).

-spec pdf_to_tiff(kz_term:ne_binary(), map()) -> {atom(), kz_term:ne_binary()}.
pdf_to_tiff(FromPath, #{<<"job_id">> := JobId, <<"tmp_dir">> := TmpDir}) ->
    ToPath = filename:join(TmpDir, [JobId, <<".tiff">>]),
    run_convert_command(?CONVERT_PDF_COMMAND, FromPath, ToPath, TmpDir).

-spec openoffice_to_pdf(kz_term:ne_binary(), map()) -> {atom(), kz_term:ne_binary()}.
openoffice_to_pdf(FromPath, Options) ->
    case ?ENABLE_OPENOFFICE of
        'true' ->
            case ?SERIALIZE_OPENOFFICE of
                'true' -> kz_openoffice_server:add(FromPath, Options);
                'false' -> do_openoffice_to_pdf(FromPath, Options)

            end;
        'false' ->
            {'error', <<"openoffice compatible conversion is unsupported">>}
    end.

-spec do_openoffice_to_pdf(kz_term:ne_binary(), map()) -> {atom(), kz_term:ne_binary()}.
do_openoffice_to_pdf(FromPath, #{<<"job_id">> := JobId, <<"tmp_dir">> := TmpDir}) ->
    FinalToPath = filename:join(TmpDir, [JobId, <<".pdf">>]),
    ToPath = filename:join(TmpDir, [filename:rootname(filename:basename(FromPath)), <<".pdf">>]),
    case run_convert_command(?CONVERT_OPENOFFICE_COMMAND, FromPath, FinalToPath, TmpDir) of
        {'ok', _} -> maybe_rename_tmp_file(ToPath, FinalToPath);
        Else -> Else
    end.

-spec maybe_rename_tmp_file(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', kz_term:ne_binary()} | {'error', kz_term:ne_binary()}.
maybe_rename_tmp_file(TmpPath, NewPath) ->
    case file:is_file(NewPath) of
        'true' -> {'ok', NewPath};
        'false' ->
            case file:rename(TmpPath, NewPath) of
                'ok' -> {'ok', NewPath};
                Else -> {'error', Else}
            end
    end.

-spec validate_output(kz_term:ne_binary(), kz_term:ne_binary(), map()) ->
                             {'ok', kz_term:ne_binary()} | {'error', kz_term:ne_binary()}.
validate_output(?TIFF_MIME, Filename, #{<<"tmp_dir">> := TmpDir}) ->
    OutputFile = filename:join(TmpDir, <<(kz_binary:rand_hex(16))/binary, ".pdf">>),
    case run_validate_command(?VALIDATE_TIFF_COMMAND, Filename, OutputFile, TmpDir) of
        {'ok', _}=OK ->
            kz_util:delete_file(OutputFile),
            OK;
        Error ->
            _ = file:delete(OutputFile),
            kz_util:delete_file(Filename),
            Error
    end;
validate_output(?PDF_MIME, Filename, #{<<"tmp_dir">> := TmpDir}) ->
    case run_validate_command(?VALIDATE_PDF_COMMAND, Filename, <<"">>, TmpDir) of
        {'ok', _}=OK ->
            OK;
        Error ->
            _ = file:delete(Filename),
            Error
    end;
validate_output(Mime, _FilePath, _Options) ->
    {'ok', <<"unsupported mime type", Mime/binary>>}.

-spec run_validate_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', any()} | {'ok', kz_term:ne_binary()}.
run_validate_command(Command, FromPath, ToPath, TmpDir) ->
    %Cmd = io_lib:format(Command, [FromPath, ToPath]),
    lager:debug("validating file with command: ~s", [Command]),
    run_command(Command, FromPath, ToPath, TmpDir).

-spec run_convert_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', any()} | {'ok', kz_term:ne_binary()}.
run_convert_command(Command, FromPath, ToPath, TmpDir) ->
    %Cmd = io_lib:format(Command, [FromPath, ToPath, TmpDir]),
    lager:info("converting file with command: ~s", [Command]),
    case run_command(Command, FromPath, ToPath, TmpDir) of
        {'ok', <<"success">>} ->
            {'ok', ToPath};
        {'error', Msg} ->
            lager:debug("failed to convert file with error: ~p", [Msg]),
            _ = file:delete(ToPath),
            {'error', Msg}
    end.

run_command(Command, FromPath, ToPath, TmpDir) ->
    Options = ['exit_status'
              ,'use_stdio'
              ,'stderr_to_stdout'
              ,{env, [{"FROM", kz_term:to_list(FromPath)}
                     ,{"TO", kz_term:to_list(ToPath)}
                     ,{"WORKDIR", kz_term:to_list(TmpDir)}
                     ]
               }
              ],
    Port = erlang:open_port({'spawn', Command}, Options),
    do_run_command(Port, []).

-spec do_run_command({atom(), kz_term:ne_binary()}, list()) -> {'error', any()} | {'ok', kz_term:ne_binary()}.
do_run_command(Port, Acc) ->
    Timeout = kapps_config:get_integer(?CONFIG_CAT, <<"convert_command_timeout">>, 120 * ?MILLISECONDS_IN_SECOND),
    receive
        {Port, {'data', Data}} ->
            do_run_command(Port, Acc ++ Data);
        {Port, {'exit_status', 0}} ->
            {'ok', <<"success">>};
        {Port, {'exit_status', Status}} ->
            lager:info("command exited with non-zero status: ~p output: ~p", [Status, Acc]),
            {'error', <<"command failed">>}
    after
        Timeout ->
            {'error', <<"command timeout">>}
    end.

