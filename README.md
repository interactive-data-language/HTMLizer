# HTMLizer

A modern routine for parsing and converting IDL code to HTML with the option to add tooltips and links to the official documentation.

This is a modern replacement to the old extension **idl_to_html**.

## Accuracy

The overall accuracy of this colorizer is pretty good. It will have troubles if you don't follow best practices outlined in the [IDL contributing guide](https://github.com/interactive-data-language/ContribGuide).


## Usage

Here is an example that you can copy/paste into the IDL console once you have the code on IDL's search path. Note that, to speed up processing, an object was used which stores some information about the different routines in IDL. The general idea is to instantiate the object, process multiple files, and then clean up after finished.


```idl
;specify our input file
inputFile = file_which('python__define.pro')

;read in plot.pro
strings = htmlizer_read_file(inputFile)

;initialize the object
html = htmlizer()

;process some strings
coloredStrings = html.Htmlize(strings, /DOCS_LINKS, /TOOLTIPS)

;clean up
html.cleanup

;make our strings an official HTML file
coloredStrings = $
  ['<html><head><link rel="stylesheet" type="text/css" href="./idl-styles.css"></head><body>',$
  coloredStrings,$
  '</body></html>']

;set up our output file
outFile = filepath(file_basename(inputFile, '.pro') + '.html', /TMP)

;write tot disk
htmlizer_write_file, outFile, coloredStrings

;copy CSS to output directory
htmlizer_copy_css, file_dirname(outFile)

print, 'Output file : ' + outFile
```


## Licensing

Copyright (c)  Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

See LICENSE.txt for more information.