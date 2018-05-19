%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_convert).

-export([fax/3, fax/4
        ]).

-include("kz_convert.hrl").

%% @equiv fax(FromFormat, ToFormat, Content, [])
-spec fax(kz_tem:api_ne_binary(), kz_term:api_ne_binary(), binary()|{'file', filename:name()}) ->
                     {'ok', any()} | {'error', any()}.
fax(FromFormat, ToFormat, Content) ->
    fax(FromFormat, ToFormat, Content, []).

%%------------------------------------------------------------------------------
%% @doc This function will convert a file using the configured converter module.
%%
%% The configured converter module is loaded from system_config/kazoo_convert via
%% the parameter `fax_converter'. The default is the kz_fax_converter module.
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
%% <li><strong>`<<"job_id">>':</strong> the unique ID of the job (like a fax job_id).
%% Used for naming the output file with the extension derived from the `To' format</li>
%% <li><strong>`<<"output_type">>':</strong> return the converted doc as a binary containing
%% the contensts or a path to the converted file.</li>
%% <li><strong>`<<"tmp_dir">>':</strong> the directory where the converted files are stored.</li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec fax(kz_term:api_ne_binary(), kz_term:api_ne_binary(), binary()|{'file', filename:name()}, kz_term:proplist()) ->
                     {'ok', any()} | {'error', any()}.
fax('undefined', _ToFormat, <<>>, _Options) ->
    {'error', <<"undefined from format">>};
fax(_FromFormat, 'undefined', <<>>, _Options) ->
    {'error', <<"undefined to format">>};
fax(_FromFormat, _ToFormat, <<>>, _Options) ->
    {'error', <<"empty content">>};
fax(_FromFormat, _ToFormat, {'file', <<>>}, _Options) ->
    {'error', <<"empty filename">>};
fax(FromFormat, ToFormat, Content, Options) ->
    Conversion = kapps_config:get_ne_binary(?CONFIG_CAT, <<"fax_converter">>, <<"fax_converter">>),
    Module = convert_to_module(Conversion),
    Module:convert(FromFormat, ToFormat, Content, props:insert_value(<<"tmp_dir">>, ?TMP_DIR, Options)).

-spec convert_to_module(kz_term:ne_binary()) -> atom().
convert_to_module(Conversion) ->
    kz_term:to_atom(<<"kz_", Conversion/binary>>, 'true').

