%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_convert_maintenance).

-export([convert_fax_file/2, convert_fax_file/3
        ]).

-include("kz_convert.hrl").

-spec convert_fax_file(any(), any(), any()) ->
                     {'ok', any()} | {'error', any()}.
convert_fax_file(FromFile, ToFormat, WorkDir)
  when is_binary(FromFile),
       is_binary(ToFormat),
       is_binary(WorkDir) ->
    Options = [{"tmp_dir", WorkDir}],
    {'ok', Content} = file:read_file(FromFile),
    case format_to_mimetype(ToFormat) of
        {'ok', Type} ->
            do_convert(FromFile
                      ,kz_mime:from_filename(FromFile)
                      ,Type
                      ,Content
                      ,Options
                      );
        {'error', _} -> 'error'
    end;
convert_fax_file(FromFile, ToFormat, WorkDir) ->
    convert_fax_file(kz_term:to_binary(FromFile)
                  ,kz_term:to_binary(ToFormat)
                  ,kz_term:to_binary(WorkDir)
                  ).
-spec do_convert(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
    'ok'|'error'.
do_convert(FromFile, FromMime, ToMime, Content, Options) ->
    case kz_convert:fax(FromMime, ToMime, Content, Options) of
        { 'ok', OutputFile } ->
            io:format("Successfully converterd ~s to ~s~n", [FromFile, OutputFile]);
        { 'error', Msg } ->
            io:format("Failed to convert file ~s with error: ~s~n", [FromFile, Msg])
    end.

-spec convert_fax_file(any(), any()) ->
                     {'ok', any()} | {'error', any()}.
convert_fax_file(FromFile, ToFormat)
  when is_binary(FromFile),
       is_binary(ToFormat) ->
    convert_fax_file(FromFile, ToFormat, ?TMP_DIR);
convert_fax_file(FromFile, ToFormat) ->
    convert_fax_file(kz_term:to_binary(FromFile)
                    ,kz_term:to_binary(ToFormat)
                    ,?TMP_DIR
                    ).

-spec format_to_mimetype(kz_term:ne_binary()) ->
                     {'ok', kz_term:ne_binary()} | {'error', kz_term:ne_binary()}.
format_to_mimetype(<<"TIFF">>) -> {'ok', ?TIFF_MIME};
format_to_mimetype(<<"tiff">>) -> {'ok', ?TIFF_MIME};
format_to_mimetype(<<"pdf">>) -> {'ok', ?PDF_MIME};
format_to_mimetype(Format) ->
    io:format("invalid format requested ~s", [Format]),
    {'error', Format}.

