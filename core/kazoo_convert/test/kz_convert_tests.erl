%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc Test fax conversions.
%%% @author Sean Wysor
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_convert_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_convert/include/kz_convert.hrl").

fax_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun(_) ->
             [tiff_to_pdf_binary()
             ,tiff_to_pdf_tuple()
             ,tiff_to_tiff_binary()
             ,tiff_to_tiff_tuple()
             ,pdf_to_tiff_binary()
             ,pdf_to_tiff_tuple()
             ,openoffice_to_pdf_binary()
             ,openoffice_to_pdf_tuple()
             ,openoffice_to_tiff_binary()
             ,openoffice_to_tiff_tuple()
             ]
     end
    }.

setup() ->
    LinkPid = kzd_test_fixtures:setup(),
    _ = application:start(kazoo_convert),
    LinkPid.

cleanup(LinkPid) ->
    kzd_test_fixtures:cleanup(LinkPid),
    application:stop(kazoo_convert).

read_test_file(Filename) ->
    {'ok', Content} = file:read_file(get_path_to_fixture(Filename)),
    Content.

copy_fixture_to_tmp(Filename) ->
    JobId = kz_binary:rand_hex(16),
    SrcFile = get_path_to_fixture(Filename),
    DstFile = kz_term:to_binary(["/tmp/", JobId, filename:extension(Filename) ]),
    {'ok', _} = file:copy(SrcFile, DstFile),
    DstFile.

get_path_to_fixture(Filename) ->
    case code:priv_dir('kazoo_fixturedb') of
        {'error', 'bad_name'}=Error -> Error;
        PrivPath ->
            kz_term:to_binary([PrivPath, "/media_files/", Filename])
    end.

tiff_to_pdf_binary() ->
    JobId = kz_binary:rand_hex(16),
    From = read_test_file("valid-multipage.tiff"),
    Expected = <<"/tmp/", JobId/binary, ".pdf" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(<<"image/tiff">>, <<"application/pdf">>, From, [{<<"job_id">>, JobId}]))].

tiff_to_pdf_tuple() ->
    JobId = kz_binary:rand_hex(16),
    Src = copy_fixture_to_tmp("valid-multipage.tiff"),
    Expected = <<"/tmp/", JobId/binary, ".pdf" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(<<"image/tiff">>, <<"application/pdf">>, {'file', Src}, [{<<"job_id">>, JobId}]))].

tiff_to_tiff_binary() ->
    JobId = kz_binary:rand_hex(16),
    From = read_test_file("valid.tiff"),
    Expected = <<"/tmp/", JobId/binary, ".tiff" >>,
    %% [?_assertMatch({'ok', Expected}, kz_convert:fax(<<"image/tiff">>, <<"image/tiff">>, From, [{<<"job_id">>, JobId}]))].
    {'timeout', 120, ?_assertMatch({'ok', Expected}, kz_convert:fax(<<"image/tiff">>, <<"image/tiff">>, From, [{<<"job_id">>, JobId}]))}.

tiff_to_tiff_tuple() ->
    JobId = kz_binary:rand_hex(16),
    Src = copy_fixture_to_tmp("valid.tiff"),
    Expected = <<"/tmp/", JobId/binary, ".tiff" >>,
    %% [?_assertMatch({'ok', Expected}, kz_convert:fax(<<"image/tiff">>, <<"image/tiff">>, {'file', Src}, [{<<"job_id">>, JobId}]))].
    {'timeout', 120, ?_assertMatch({'ok', Expected}, kz_convert:fax(<<"image/tiff">>, <<"image/tiff">>, {'file', Src}, [{<<"job_id">>, JobId}]))}.

pdf_to_tiff_binary() ->
    JobId = kz_binary:rand_hex(16),
    From = read_test_file("valid.pdf"),
    Expected = <<"/tmp/", JobId/binary, ".tiff" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(<<"application/pdf">>, <<"image/tiff">>, From, [{<<"job_id">>, JobId}]))].

pdf_to_tiff_tuple() ->
    JobId = kz_binary:rand_hex(16),
    Src = copy_fixture_to_tmp("valid.pdf"),
    Expected = <<"/tmp/", JobId/binary, ".tiff" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(<<"application/pdf">>, <<"image/tiff">>, {'file', Src}, [{<<"job_id">>, JobId}]))].

openoffice_to_pdf_binary() ->
    kz_openoffice_server_sup:start_link(),
    JobId = kz_binary:rand_hex(16),
    From = read_test_file("valid.docx"),
    Expected = <<"/tmp/", JobId/binary, ".pdf" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(
                                         <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>
                                         ,<<"application/pdf">>
                                         ,From
                                         ,[{<<"job_id">>, JobId}]
                                         )
                  )].

openoffice_to_pdf_tuple() ->
    JobId = kz_binary:rand_hex(16),
    Src = copy_fixture_to_tmp("valid.docx"),
    Expected = <<"/tmp/", JobId/binary, ".pdf" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(
                                                    <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>
                                                    ,<<"application/pdf">>
                                                    ,{'file', Src}
                                                    ,[{<<"job_id">>, JobId}]
                                                    )
                  )].

openoffice_to_tiff_binary() ->
    JobId = kz_binary:rand_hex(16),
    From = read_test_file("valid.docx"),
    Expected = <<"/tmp/", JobId/binary, ".tiff" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(
                                         <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>
                                         ,<<"image/tiff">>
                                         ,From
                                         ,[{<<"job_id">>, JobId}]
                                         )
                  )].

openoffice_to_tiff_tuple() ->
    JobId = kz_binary:rand_hex(16),
    Src = copy_fixture_to_tmp("valid.docx"),
    Expected = <<"/tmp/", JobId/binary, ".tiff" >>,
    [?_assertMatch({'ok', Expected}, kz_convert:fax(
                                                    <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>
                                                    ,<<"image/tiff">>
                                                    ,{'file', Src}
                                                    ,[{<<"job_id">>, JobId}]
                                                    )
                  )].
