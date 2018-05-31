-ifndef(KZ_CONVERT_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(CHUNKSIZE, 24576).
-define(APP_NAME, <<"kazoo_convert">>).
-define(APP_VERSION, <<"1.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).
-define(TIFF_MIME, <<"image/tiff">>).
-define(PDF_MIME, <<"application/pdf">>).
-define(IMAGE_MIME_PREFIX, <<"image/">>).
-define(OPENXML_MIME_PREFIX, "application/vnd.openxmlformats-officedocument.").
-define(OPENOFFICE_MIME_PREFIX, "application/vnd.oasis.opendocument.").
-define(OPENOFFICE_COMPATIBLE(CT)
       ,(CT =:= <<"application/msword">>
             orelse CT =:= <<"application/vnd.ms-excel">>
             orelse CT =:= <<"application/vnd.ms-powerpoint">>
        )).
-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o ~s ~s">>).
-define(DEFAULT_CONVERT_PDF_CMD
       ,<<"/usr/bin/gs -q "
          "-r204x98 "
          "-g1728x1078 "
          "-dNOPAUSE "
          "-dBATCH "
          "-dSAFER "
          "-sDEVICE=tiffg3 "
          "-sOutputFile=~s -- ~s"
        >>).
-define(CONVERT_IMAGE_CMD, <<"convert ~s -resample 204x98 "
                             "-units PixelsPerInch "
                             "-compress group4 "
                             "-size 1728x1078 ~s"
                           >>).
%%-define(CONVERT_OO_DOC_CMD, <<"unoconv -c ~s -f pdf -o ~s ~s">>).
-define(CONVERT_OO_DOC_CMD, <<
                             "libreoffice "
                             "--headless "
                             "--convert-to pdf ~s "
                             "--outdir ~s "
                             " 2>&1 "
                             "|egrep 'parser error|Error' && exit 1 || exit 0"
                            >>).

-define(VALIDATE_PDF_CMD,<<"gs -dNOPAUSE -dBATCH -sDEVICE=nullpage ~s">>).

-define(CONVERT_IMAGE_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_image_command">>, ?CONVERT_IMAGE_CMD)).
-define(CONVERT_OO_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_openoffice_document_command">>, ?CONVERT_OO_DOC_CMD)).
-define(CONVERT_PDF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_pdf_command">>, ?DEFAULT_CONVERT_PDF_CMD)).
-define(CONVERT_TIFF_TO_PDF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_tiff_command">>, ?TIFF_TO_PDF_CMD)).
-define(VALIDATE_PDF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"validate_pdf_command">>, ?VALIDATE_PDF_CMD)).
-define(VALIDATE_TIFF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"validate_tiff_command">>, ?TIFF_TO_PDF_CMD)).

-define(OPENOFFICE_SERVER
       ,kapps_config:get_binary(?CONFIG_CAT, <<"openoffice_server">>, <<"'socket,host=localhost,port=2002;urp;StarOffice.ComponentContext'">>)).

-define(TMP_DIR
       ,kapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>)).

-define(SHOULD_SERIALIZE_OO
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"should_serialize_openoffice">>, true)).

-define(KZ_CONVERT_HRL, 'true').
-endif.