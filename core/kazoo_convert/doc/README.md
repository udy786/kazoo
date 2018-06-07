/*
Section: Kazoo Convert
Title: Kazoo Convert
Language: en-US
Version: 4.2
*/

# Kazoo Convert *The Kazoo Core File Format Converter Library*

## Overview
The Kazoo convert provides a core set of functions for converting file formats. This app moves functionality previously scattered throughout other applications into a single core application.

This app is intended to:
1. Provides a core consistant interface for file format conversions.
1. Provide the capability of generating custom modules for conversion and enabling them via config.
1. Returns a standardized result of either the file path of the conversion output file or the file content, or standardized errors for failures to convert.

## Modules
The kazoo converter command uses modules for the converter based on the type of conversions required. This is intended to be extended to include multipe types of conversions and formats and be easily extendable by supporting selection of which modules to use via the `system_config/kazoo_convert` document.

#### Fax Converter
The fax converter module by default uses the module `fax_converter`.

For a description of how the fax_converter works, see [the fax converter documetation.](fax_converter.md)

## Configuration

#### Schema

Schema for kazoo_convert system_config

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`convert_command_timeout` | timeout in milliseconds for the convert command | `integer()` | `120000` | `false` |
`convert_image_command` | command to resample tiff images for faxing | `string()` | [see command document](fax_converter.md) | `false` |
`convert_openoffice_command` | kazoo_convert convert_openoffice_command | `string()` | [see command document](fax_converter.md) | `false` |
`convert_openoffice_document_command` | command to convert openoffice compatible documents to pdf | `string()` | [see command document](fax_converter.md) | `false` |
`convert_pdf_command` | command to convert pdf to tiff format | `string()` | [see command document](fax_converter.md) | `false` |
`convert_tiff_command` | command to convert tiff to pdf | `string()` | [see command document](fax_converter.md) | `false` |
`enable_openoffice` | enable openoffice compatible file conversions | `boolean()` | `true` | `false` |
`fax_converter` | module to load for fax conversions | `string()` | `fax_converter` | `false` |
`file_cache_path` | default working path for conversions files | `string()` | `/tmp/` | `false` |
`serialize_openoffice` | kazoo_convert serialize_openoffice | `boolean()` | `true` | `false` |
`should_serialize_openoffice` | serialize the operation of converting openoffice compatible documents | `boolean()` | `true` | `false` |
`validate_pdf_command` | command used to verify a pdf file | `string()` | `gs -dNOPAUSE -dBATCH -sDEVICE=nullpage ~s` | `false` |
`validate_tiff_command` | command used to verify a tiff file | `string()` | `tiff2pdf -o ~s ~s` | `false` |


### Sup Commands

```
sup kazoo_convert_maintenance convert_fax_file {path/to/file} {to file type}
```

This command converts a file specified in the `path/to/file` and allows conversions to the formats `pdf` and `tiff`.

```
sup kazoo_convert_maintenance convert_fax_file {path/to/file} {to file type} {work directory}
```

For batch type converters, an additional argument `work_directory` can be added to specify the directory where the output file will go.

```
sup kazoo_convert_maintenance versions_in_use
```

Used to audit the system and ensure all the converters required for the conversion operations are installed. If installed, this command attempts to display their versions.
