/*
Section: Kazoo Fax Converter
Title: Kazoo Convert
Language: en-US
Version: 4.2
*/

# Kazoo Fax Converter

This module provide a consistant core interface for fax file format conversions. The module is enabled as the default fax converter, but this can be easily extended by adding other modules to the converter.

## Fax Converter Installation Considerations
When using the default converter module several system dependencies were introduced. For default conversions from PDF to TIFF, The system packages `ghostscript` must be installed, for default conversion from tiff to pdf, `libtiff-tools` must be installed. If conversion to openxml formatted documents (docx, doc, xlsx, xls) is enabled, the dependency `libreoffice` must be installed. If a dependency is not installed, the converter command will simply fail on the system and the fax converter will return an error. If the converters commands are substituted this

## Fax Converter Commands

All of the functionality for conversions was extracted from the `fax` and `teletype` apps, however the conversion commands executed did not survive the journey. Unlike the convert commands in the `fax` app, the `fax_converter` module the exit status and not the output is used to determine the success or failure of a command.

This means if you have customized your commands, you should ensure the exit status returned is correct for the result of the custom convert command.

For example, if your convert command was previously:
```bash
/usr/bin/gs -q \
     -r204x98 \
     -g1728x1078 \
     -dNOPAUSE \
     -dBATCH \
     -dSAFER \
     -sDEVICE=tiffg3 \
     -sOutputFile=~s -- ~s \
      > /dev/null 2>&1 \
      && echo -n success
```

The equivalent `fax_converter` command would be

```bash
/usr/bin/gs -q \
     -r204x98 \
     -g1728x1078 \
     -dNOPAUSE \
     -dBATCH \
     -dSAFER \
     -sDEVICE=tiffg3 \
     -sOutputFile=~s -- ~s
```

Which also means, if the converter you are using for a specific purpose is a jerk and always returns exit_status `0` (looking at you libreoffice!), you need to handle this.

Something like this could be appended to the end of the your custom command to handle this case. This searches for matches in the output and emits exit_staus 1 if those matches are found, othewise emits exit_status 0.

```
|egrep 'parser error|Error' && exit 1 || exit 0"
```

Most converters are nice about exit status, but you should definitely test your command in failure cases to ensure you don't end up sending bad faxes or notification emails.

The default parser commands are:

### Tiff Resample Command

The configuration parameter for this command is `convert_image_command`. This command is invoked when a conversion from `image/*` to `image/tiff` is requested.

Resamples a tiff into the standard format. This command is passed the from and to filenames.

```bash
convert ~s \
    -resample 204x98 \
    -units PixelsPerInch \
    -compress group4 \
    -size 1728x1078 ~s
```

## Tiff to PDF
The configuration parameter for this command is `convert_tiff_command`. This command is invoked when a conversion from `image/tiff` to `application/pdf` is requested.

this command is passed to and from filename

```bash
    tiff2pdf -o ~s ~s
```

## Pdf to Tiff

