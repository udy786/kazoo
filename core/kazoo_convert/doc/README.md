/*
Section: Kazoo Convert
Title: Kazoo Convert
Language: en-US
Version: 4.2
*/

# Kazoo Convert *The Kazoo Core File Format Converter Library*

# Overview
The Kazoo convert provides a core set of functions for converting file formats. This app moves functionality previously scattered throughout other applications into a single core application.

This app is intended to:
1. Provides a core consistant interface for file format conversions.
1. Provide the capability of generating custom modules for conversion and enabling them via config.
1. Returns a standardized result of either the file path of the conversion output file or the file content, or standardized errors for failures to convert.

| Parameter | Description | Default |
|--- |--- |--- |

## Modules
The kazoo converter command uses modules for the converter based on the type of conversions required. This is intended to be extended to include multipe types of conversions and formats and be easily extendable by supporting selection of which modules to use via the `system_config/kazoo_convert` document.

### Fax Converter
The fax converter module by default uses the module `fax_converter`.

For a description of how the fax_converter works, see [the fax converter documetation.](fax_converter.md)
