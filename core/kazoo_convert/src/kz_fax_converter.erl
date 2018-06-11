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
%% @doc This function converts the `Content' from the `To' mimetype to the
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
%% <li><strong>Optinos:</strong> a proplist of the converter options</li>
%% </ul>
%%
%% Options:
%% <ul>
%% <li><strong>job_id:</strong> the unique ID of the job (like a fax job_id).
%% Used for naming the output file with the extension derived from the `To' format</li>
%% <li><strong>output_type:</strong> return the converted doc as a raw `binary' containing
%% the contents of the file or `path' to receive a path to the converted file in the response.
%% The default is `path'.</li>
%% <li><strong>tmp_dir:</strong> the working directory where the conversion will take place.</li>
%% </ul>
%%
%% Cache file handling:
%% The converter (and any alternative modules) should always delete any files created in the process,
%% including the input file if the {'file', Filepath} `Content' format is specified.
%% If `output_type' is `path' the file converted file will be returned and deletion of this file will be
%% the responsiblity of the caller.
%%
%% @end
%%------------------------------------------------------------------------------
-spec convert(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', filename:name()}, kz_term:proplist()) ->
                     {'ok', any()}|{integer(), non_neg_integer()}|{'error', any()}.
convert(From, To, Content, Opts) ->
    Options = maps:from_list(
                [{<<"from_format">>, From}
                ,{<<"to_format">>, To}
                ,{<<"job_id">>, props:get_value(<<"job_id">>, Opts, kz_binary:rand_hex(12))}
                 | props:delete_keys([<<"job_id">>], Opts)
                ]),
    Filename = save_file(Content, Options),
    lager:info("converting document ~s from ~s to ~s", [Filename, From, To]),
    case run_convert(eval_format(From, To), To, Filename, Options) of
        {'ok', _}=Ok ->
            lager:info("succesfully converted file: ~s to format: ~s", [Filename, To]),
            Ok;
        {'ok', _, _}=Ok ->
            lager:info("succesfully converted file: ~s to format: ~s", [Filename, To]),
            Ok;
        {'error', Message}=Error ->
            lager:error("conversion failed with error: ~p", [Message]),
            Error
    end.

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
    {'error', <<"invalid conversion requested: ", FromFormat/binary, " to: ", ToFormat/binary>>}.

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
            format_output(ToFormat, FilePath, Options);
        Error -> Error
    end.

-spec format_output(kz_term:ne_binary(), kz_term:ne_binary(), map()) ->
                           {'ok', any()}|{integer(), non_neg_integer()}|{'error', any()}.
format_output(?TIFF_MIME, FilePath, #{<<"output_type">> := 'path', <<"count_pages">> := 'true'}) ->
    {'ok', FilePath, get_sizes(FilePath)};
format_output(?TIFF_MIME, FilePath, #{<<"output_type">> := 'binary', <<"count_pages">> := 'true'}) ->
    case file:read_file(FilePath) of
        {'ok', Content} ->
            Sizes = get_sizes(FilePath),
            kz_util:delete_file(FilePath),
            {'ok', Content, Sizes};
        Error -> Error
    end;
format_output(?TIFF_MIME, FilePath, #{<<"count_pages">> := 'true'}) ->
    {'ok', FilePath, get_sizes(FilePath)};
format_output(_FromFormat, FilePath, #{<<"output_type">> := 'binary'}) ->
    case file:read_file(FilePath) of
        {'ok', Content} ->
            kz_util:delete_file(FilePath),
            {'ok', Content};
        Error -> Error
    end;
format_output(_FromFormat, FilePath, #{<<"output_type">> := 'path'}) ->
    {'ok', FilePath};
format_output(_FromFormat, FilePath, _Options) ->
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
    FilePath = filename:join(TmpDir, [JobId, <<".">>, Ext]),
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
    TmpPath = filename:join(TmpDir, [filename:rootname(filename:basename(FromPath)), <<".pdf">>]),
    case run_convert_command(?CONVERT_OPENOFFICE_COMMAND, FromPath, FinalToPath, TmpDir) of
        {'ok', _} -> maybe_rename_tmp_file(TmpPath, FinalToPath);
        Else -> Else
    end.

-spec maybe_rename_tmp_file(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', kz_term:ne_binary()} | {'error', kz_term:ne_binary()}.
maybe_rename_tmp_file(TmpPath, NewPath) ->
    case filelib:is_file(NewPath) of
        'true' -> {'ok', NewPath};
        'false' ->
            case filelib:is_file(TmpPath) of
                'true' ->
                    kz_util:rename_file(TmpPath, NewPath),
                    {'ok', NewPath};
                'false' -> {'error', <<"no output file from conversion">>}
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
    lager:debug("validating file with command: ~s", [Command]),
    run_command(Command, FromPath, ToPath, TmpDir).

-spec run_convert_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', any()} | {'ok', kz_term:ne_binary()}.
run_convert_command(Command, FromPath, ToPath, TmpDir) ->
    lager:debug("converting file with command: ~s", [Command]),
    case run_command(Command, FromPath, ToPath, TmpDir) of
        {'ok', _} ->
            lager:debug("successfully converted file ~s", [FromPath]),
            {'ok', ToPath};
        {'error', Reason, Msg} ->
            lager:debug("failed to convert file with reason: ~p, output: ~p", [Reason, Msg]),
            _ = file:delete(ToPath),
            {'error', <<"command failed">>}
    end.

-spec run_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                         {'error', atom(), kz_term:ne_binary()}|{'ok', kz_term:ne_binary()}.
run_command(Command, FromPath, ToPath, TmpDir) ->
    kz_os:cmd(Command
             ,[{<<"FROM">>, FromPath}
              ,{<<"TO">>, ToPath}
              ,{<<"WORKDIR">>, TmpDir}
              ]
             ,[{<<"timeout">>
               ,kapps_config:get_integer(?CONFIG_CAT, <<"convert_command_timeout">>, 120 * ?MILLISECONDS_IN_SECOND)
               }]).

-spec get_sizes(kz_term:ne_binary()) -> {integer(), non_neg_integer()}.
get_sizes(Filename) ->
    NumberOfPages = case kz_os:cmd(?COUNT_PAGES_CMD, [{<<"FILE">>, Filename}], [{<<"read_mode">>, 'stream'}]) of
                        {'ok', Result} -> kz_term:to_integer(Result);
                        _ -> 0
                    end,
    FileSize = filelib:file_size(kz_term:to_list(Filename)),
    {NumberOfPages, FileSize}.




