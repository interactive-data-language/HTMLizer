# HTMLizer

A modern routine for parsing and converting IDL code to HTML with the option to add tooltips and links to the official documentation.

This is a modern replacement to the old extension **idl_to_html**.


## Accuracy

The overall accuracy of highlighting the code with this is extension is pretty good. It will have troubles if you don't follow best practices outlined in the [IDL contributing guide](https://github.com/interactive-data-language/ContribGuide) and if you use goofy syntax that is not easy to read. This does perform better than the official ENVI documentation online for some cases.


## Usage

Here is an example that you can copy/paste into the IDL console once you have the code on IDL's search path. Note that, to speed up processing, an object was used which stores some information about the different routines in IDL. The general idea is to instantiate the object, process multiple files, and then clean up after finished.


```idl
;specify our input file
inputFile = file_which('python__define.pro')

;read in plot.pro
strings = htmlizer_read_file(inputFile)

;initialize the object
html = HTMLizer()

;process some strings
coloredStrings = html.HTMLize(strings, /DOCS_LINKS, /TOOLTIPS)

;clean up
html.cleanup

;make our strings an official HTML file
coloredStrings = $
  ['<html><head><link rel="stylesheet" type="text/css" href="./idl-styles.css"></head><body>',$
  coloredStrings,$
  '</body></html>']

;set up our output file
outFile = filepath(file_basename(inputFile, '.pro') + '.html', /TMP)

;write to disk
htmlizer_write_file, outFile, coloredStrings

;copy CSS to output directory
htmlizer_copy_css, file_dirname(outFile)

print, 'Output file : ' + outFile
```


## Updating the Routines

In order to update the routines you will need to do two things:

1. Run the routine `htmlizer_export_csv` which will read in the `idl_routines.sav` file and export it to `idl_routines.csv`.

2. Make changes to the `idl_routines.csv` file and then run `htmlizer_import_csv` which will then update `idl_routines.csv`

Take a look at the idl_routines.csv file for what the syntax should be for adding additional rows of information before running `htmlizer_import_csv`.


## Performance Updates

The original version of this routine takes about **4.5** seconds to process the PRO code in the main level program. In the latest version, it now takes about **0.7** seconds to process the same file which is an 85% reduction in processing time. This is because of the more efficient way that the data is being stored for tooltips and documentation links. The plus side to this is that, for larger routines, the HTMLizer will be much faster, but there is a slight overhead for smaller files.

When you first create the object it will "unpack" the idl_routines.sav file into an optimized data structure for faster access.Once this file has been created, it will keep using the SAVE file on disk. In addition to this, when colorizing many files at once, this routine also uses system variables so that only one instance of the object needs to restore the SAVE file for the object which is re-used between instances. Without the re-use, there is about a 0.2 second overhead when creating the object which, while small, can add up when you are making documentation for larger projects.


## License

© 2018 Harris Geospatial Solutions, Inc.

Licensed under MIT. See LICENSE.txt for additional details and information.
