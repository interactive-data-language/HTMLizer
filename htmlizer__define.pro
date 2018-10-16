;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
;
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
;
;  Object that colorizes IDL code with CSS classes for use in
;  web pages. Code uses regular expressions and some fancy logic behind the
;  scenes to duplicate the same syntax highlighting that you see in the IDL
;  workbench. See the main level program at the bottom of the file for an
;  example of how to use the code.
;  
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-




;+
; :Private:
; 
; :Description:
;    Procedure to replace any HTML custom characters in the strings.
;
; :Params:
;    str: in, required, type=string
;      IDL code string to process.
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro replaceHTMLChars, str
  compile_opt idl2, hidden
  
  ;replace any occurrences of the special character "&" with its HTML escape sequence:
  ;check this first so that we don't mess up our other repalcers
  ampPos = strpos(str,'&')
  if (ampPos ne -1) then begin
    str = str.replace('&', '&amp;')
  endif

  ;replace any occurrences of the special character "<" with its HTML escape sequence:
  ltPos = strpos(str,'<')
  while ltPos ne -1 do begin
    ;check to see if "<" is associated with font modifier HTML tags:
    strAfter=strmid(str,ltPos+1,4)
    if (strAfter EQ 'font') or (strAfter EQ '/fon') then begin ;HTML tags; ignore and look again:
      ltPos = strpos(str,'<',ltPos+1)
    endif else begin ;legitimate IDL minimum operator; replace with HTML escape sequence:
      str = strmid(str,0,ltPos) + '&lt;' + strmid(str,ltPos+1)
      ;look again on the current line for any other occurrences of "<":
      ltPos = strpos(str,'<',ltPos+1)
    endelse
  endwhile

  ;replace any occurrences of the special character ">" with its HTML escape sequence:
  gtPos = strpos(str,'>')
  while (gtPos ne -1) do begin
    ;check to see if ">" is associated with font modifier HTML tags:
    strBefore=strmid(str,gtPos-2,2)
    if (strBefore eq '">') then begin ;HTML tags; ignore and look again:
      gtPos = strpos(str,'>',gtPos+1)
    endif else begin ;legitimate IDL maximum operator; replace with HTML escape sequence:
      str = strmid(str,0,gtPos) + '&gt;' + strmid(str,gtPos+1)
      ;look again on the current line for any other occurrences of ">":
      gtPos = strpos(str,'>',gtPos+1)
    endelse
  endwhile

end

pro update_replacer, $
  inName, replacer, toolDatabase,$
  SEARCH_REPLACE = search_replace,$
  CONTROL = control,$
  FUNC = func,$
  PROC = proc,$
  SYSV = sysv,$
  METHOD = method,$
  TOOLTIPS = tooltips,$
  DOCS_LINKS = docs_links,$
  CUSTOM_TOOLTIPS = custom_tooltips,$
  BASELINK = baselink
  compile_opt idl2, hidden
  
  ;duplicate input name
  name = inName

  ;set our outputs to null strings
  out_link = ''
  out_tt = ''

  ;flag for being in the database or not
  inDB = 0

  ;check if we need to replace our search item or not
  if keyword_set(search_replace) then begin
    ;custom changes for printing
    if search_replace.haskey(strlowcase(name)) then begin
      dbSearch = strupcase(search_replace[strlowcase(name)])
    endif else begin
      dbSearch = strupcase(name)
    endelse
  endif else begin
    dbSearch = strupcase(name)
  endelse

  ;figure out what our search term needs to be for our database
  ;depending on the type of item the searches are a little different fromone another
  case (1) of
    ;function method
    keyword_set(method):begin
      name = '::' + dbSearch
      idx_thing = where(strpos(toolDatabase, name + '"') ne -1, count_thing)
    end

    ;system variable
    keyword_set(sysv):begin
      name = dbSearch
      idx_thing = where(strpos(toolDatabase, '"' + name) ne -1, count_thing)
    end

    ;control statements
    keyword_set(control):begin
      name = dbSearch
      idx_thing = where(strpos(toolDatabase, '"' + dbSearch + '"') ne -1, count_thing)
      if (count_thing eq 0) then begin
        idx_thing = where(strpos(toolDatabase, '"' + dbSearch + '...') ne -1, count_thing)
      endif
    end

    ;procedures
    keyword_set(procedure):begin
      ;duplaicate routiens start with the name and then Procedure afterwards (i.e. plot and plot())
      name = dbSearch + ' Procedure"'
      idx_thing = where(strpos(toolDatabase, '"' + name) ne -1, count_thing)
      if (count_thing eq 0) then begin
        idx_thing = where(strpos(toolDatabase, '"' + dbSearch + '"') ne -1, count_thing)
      endif
    end

    ;everything else, including functions
    else:begin
      name = dbSearch
      idx_thing = where(strpos(toolDatabase, '"' + dbSearch + '"') ne -1, count_thing)
    end
  endcase

  ;see if we found the entry we were searching for
  if (count_thing eq 1) then begin
    ttLine = toolDatabase[idx_thing[0]]

    ;split the line
    splitTT = strsplit(ttLine, ';', /EXTRACT)

    ;make sure we have a valid line with 6 split components
    ;just a sanity check to make sure that nothing is missing
    if (n_elements(splitTT) eq 6) then begin
      ;get the tooltip and the link (always same position)
      tt = strmid(splitTT[2], 1, strlen(splitTT[2])-2)
      link = strmid(splitTT[4], 1, strlen(splitTT[4])-2)
      inDB = 1
      useLink = baselink
    endif
  endif else begin
    ;check if we are in a custom tooltip
    if keyword_set(custom_tooltips) then begin
      if keyword_set(name) then begin
        ;remove procedure
        name = name.replace(' procedure', '')

        ;check if we are a function
        if keyword_set(func) then begin
          name += '()'
        endif
        
        ;check if we have a match
        if custom_tooltips.hasKey('tooltips') then begin
          ;extract tooltips
          tts = custom_tooltips['tooltips']
          
          ;check if we have a match
          if tts.hasKey(name) then begin
            useLink = custom_tooltips['baselink']
            tt = tts[name, 'tooltip']
            link = tts[name, 'relative-path']
            inDB = 1
          endif
        endif
      endif
    endif
  endelse

  ;check how we need to modify our strings
  if (inDB) then begin
    if keyword_set(tooltips) AND keyword_set(tt) then begin
      replace = replacer + 'idl_tt" idl_tt="' + tt + '">' + inName + "</font>"
    endif else begin
      replace = replacer + '">' + inName + '</font>'
    endelse

    if keyword_set(docs_links) AND keyword_set(link) then begin
      replace = '<a class="idl_docs_link" href="' + useLink  + link + '" target="_blank">' + replace + '</a>'
    endif

    replacer = replace
  endif else begin
    replacer = replacer + '">' + inName + '</font>'
  endelse
end


;+
; :Private:
; 
; :Description:
;    Function that splits strings into sections that we do and don't process
;
; :Params:
;    str: in, required, type=string
;      The IDL string that needs to be split
;
; :Keywords:
;    PROCESS: out, requried, type=bytarr
;      A byte array with 1/0 flags for processing or no processing.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function splitString, str, PROCESS = process, STRING_START = string_start, ALREADY_SPLIT = already_split
  compile_opt idl2, hidden
  
  ;preallocate arrays to hole the 
  parts = strarr((strlen(str)/2 + 1) > 2)
  process = bytarr((strlen(str)/2 + 1) > 2) + 1b
  string_start = bytarr((strlen(str)/2 + 1) > 2)
  string_start[0] = 1
  
  ;check if we don't want to color our line
  if (strpos(str, '!NOCOLOR') eq 0) then begin
    str = strmid(str, 9)
    replaceHTMLChars, str
    process = [0]
    string_start = [1]
    return, [str]
  endif

  ;check if we have console output
  if (strpos(str, '!CONSOLE') eq 0) then begin
    str = '      ' + strmid(str, 9)
    replaceHTMLChars, str
    process = [0]
    string_start = [1]
    return, [str]
  endif

  ;check if we have a bold line
  boldPos = strpos(str, '!BOLD')
  if (boldPos ne -1) then begin
    str = strmid(str, boldPos + 6)
    replaceHTMLChars, str
    str = '<font class="idl_bold">' + str + '</font>'
    process = [0]
    string_start = [1]
    return, [str]
  endif

  ;check if we have a gray line
  grayPos = strpos(str, '!GRAY')
  if (grayPos ne -1) then begin
    str = strmid(str, grayPos + 6)
    replaceHTMLChars, str
    str = '<font class="idl_gray">' + str + '</font>'
    process = [0]
    string_start = [1]
    return, [str]
  endif
  
  ;first, split everything into paren or not by looking for
  ;string of the form "(expression).something"
  posParen = stregex(str, '\(.*\)\.', LENGTH  = lParen)
  if (posParen eq 0) AND ~keyword_set(already_split) then begin
    ;split!
    front = strmid(str, 0, posParen + lParen - 2)
    back = strmid(str, posParen + lParen - 2)
    
    ;process front and back
    parts = [ $
      splitString(front, PROCESS = processFront, STRING_START = startFront, /ALREADY_SPLIT), $
      splitString(back, PROCESS = processBack, STRING_START = startBack, /ALREADY_SPLIT) $
      ]
      
    ;join process flags
    process = [processFront, processBack]
    
    ;set flags for if we are the start of the string or not
    string_start = [startFront*0, startBack]
  endif else begin
    ;counter for how many parts we really have
    nparts = 0

    ;get the positions of our splitters
    positions = [strpos(str, ';'), strpos(str, '"'), strpos(str, "'")]
    idx_ok = where(positions ne -1, count_ok)

    ;nothing to split
    if (count_ok eq 0) then begin
      process = [1]
      string_start = [1]
      replaceHTMLChars, str
      return, [str]
    endif else begin

      ;iterate!
      while (count_ok gt 0) do begin
        ;figure out what we are dealing with here
        case min(positions[idx_ok]) of
          ;semi-colon
          positions[0]:begin
            type = 0b
            startPos = positions[0]
            endPos = strlen(str)
          end

          ;double quote
          positions[1]:begin
            type = 1b
            startPos = positions[1]
            endPos = strpos(strmid(str, startPos+1), '"')
            if (endPos eq -1) then begin
              endPos = strlen(str)
            endif
          end

          ;single-quote
          positions[2]:begin
            type = 2b
            startPos = positions[2]
            endPos = strpos(strmid(str, startPos+1), "'")
            if (endPos eq -1) then begin
              endPos = strlen(str)
            endif
          end
        endcase

        ;save the beginning of the string
        parts[nparts] = strmid(str, 0, startPos)

        ;increment our counter
        nparts++

        ;get the mid portion of our string
        mid = strmid(str, startPos, endPos+2)

        ;replace any bad characters that might be in the strings
        replaceHTMLChars, mid

        if (type eq 1) or (type eq 2) then begin
          if (strpos(mid, "'") ne -1) then mid = mid.replace("'", '&#39;')
          if (strpos(mid, '"') ne -1) then mid = mid.replace('"', '&#34;')
        endif

        case type of
          0: type = 'class="idl_comment"'
          1: type = 'class="idl_str"'
          2: type = 'class="idl_str"'
        endcase

        ;save the part of our string that we don't want to process
        parts[nparts] =  '<font ' + type + '>' + mid + '</font>'
        process[nparts] = 0

        ;update our string component
        str = strmid(str, startpos + endPos + 2)

        ;increment our counter
        nparts ++

        ;get the positions of our splitters
        positions = [strpos(str, ';'), strpos(str, '"'), strpos(str, "'")]
        idx_ok = where(positions gt 0, count_ok)
      endwhile

      ;check if we need to save the last part of the strings
      if (strtrim(str,2) ne '') then begin
        parts[nparts] = str
        nparts++
      endif
    endelse

    foreach part, parts, i do begin
      if ~process[i] then continue
      if (i eq (nparts-1)) then break
      replaceHTMLChars, part
      parts[i] = part
    endforeach

    ;get the good parts of our arrays
    parts = parts[0:nparts-1]
    process = process[0:nparts-1]
    string_start = string_start[0:nparts-1]
  endelse
  
  ;return our parts
  return, parts
end

;+
; :Private:
; 
; :Description:
;    Procedure to split strings that are to be processed and remove the
;    HTML tags from processing.
;
; :Params:
;    strs: in, required, type=strarr
;      The array of strings that represent each part of the string.
;    process: in, required, type=bytarr
;      The array of 1/0 flags for if a section is processed or not
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro secondSplit, strs, process, string_start
  compile_opt idl2, hidden

  ;find each expression
  expressions = ['IDL&[a-z0-9_]+;', 'ENVI&[a-z0-9_]+;', '&[a-z0-9_]+;']
  nExp = 3

  foreach pFlag, process, i do begin
    if ~pFlag then continue
    str = strs[i]
    
    ;loop over each expression that we are looking for, special for IDL and ENVI prompts
    foreach exp, expressions, j do begin
      ;search with regex
      pos = stregex(str, exp, LENGTH = length)
      
      ;make sure that we found something
      if (pos ne -1) then begin
        ;extract pieces of our strings
        front = strmid(str, 0, pos)
        sub = strmid(str, pos, length)
        back = strmid(str, pos + length)

        ;check if the sub doesn't start with '&' which means we bold
        ;it for the prompts
        if (j lt (nExp-1)) then begin
          sub = '<font class="idl_bold">' + sub + '</font>'
        endif

        ;find which strings to use by excluding null strings
        add = [front, sub, back]
        addP = [1, 0, 1]
        addS = [0, 0, 0]
        idxOk = where(add ne '', countOk)
        
        ;process the string accordingly
        if (countOk gt 0) then begin
          case (1) of
            ;only one element
            (n_elements(process) eq 1): begin
              strs = add[idxOk]
              process = addP[idxOk]
              string_start = addS[idxOk]
            end
            ;first element of the array
            (i eq 0):begin
              strs = [add[idxOk], strs[(i+1)<(n_elements(process)):-1]]
              process = [addP[idxOk], process[(i+1)<(n_elements(process)):-1]]
              string_start = [addS[idxOk], string_start[(i+1)<(n_elements(process)):-1]]
            end
            ;last element of the array
            (i eq (n_elements(process)-1)): begin
              strs = [strs[0:i-1], add[idxOk]]
              process = [process[0:i-1], addP[idxOk]]
              string_start = [string_start[0:i-1], addS[idxOk]]
            end
            ;otherwise in the middle
            else:begin
              strs = [strs[0:i-1], add[idxOk], strs[(i+1)<(n_elements(process)):-1]]
              process = [process[0:i-1], addP[idxOk], process[(i+1)<(n_elements(process)):-1]]
              string_start = [string_start[0:i-1], addS[idxOk], string_start[(i+1)<(n_elements(process)):-1]]
            end
          endcase
        endif
        ;stop looping because we found something in our string and should not
        ;process anymore
        break
      endif
    endforeach
  endforeach
  
end

;+
; :Description:
;    Simple init method for our object that reads in the CSV file. Idea
;    is to read in the file once and re-use the HTMLIzer in multiple places.
;
;
; :Private:
;    CUSTOM_TOOLTIPS: in, optional, type=orderedhash, private
;      Functionality that allows for custom tooltips when creating 
;      documentation for packages built with the IDL Package Creator.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function HTMLizer::init, CUSTOM_TOOLTIPS = custom_tooltips
  compile_opt idl2, hidden

  ;set the default base link for the object
  self.BASELINK = 'https://www.harrisgeospatial.com/docs/'

  ;make sure IDL version is at least 5.5 or newer, ignore beta and development build versions
  ;of IDL because string to float conversion will fail:
  betaTest = strpos(strlowcase(!VERSION.RELEASE),'beta')
  buildTest = strpos(strlowcase(!VERSION.RELEASE),'build')
  ;check to make sure the version of IDL running is 5.4 or newer:
  if (betaTest EQ -1) and (buildTest EQ -1) then begin
    version=float(!VERSION.RELEASE)
    if (version LT 8.4) then begin
      message, 'IDL_TO_HTML is only supported in IDL version 8.4 or newer.'
    endif
  endif

  ;get the current directory
  thisdir = file_dirname(routine_filepath())

  ;check for the CSV file
  routines = thisdir + path_sep() + 'idl_routines.sav'
  if ~file_test(routines) then begin
    message, 'idl_routines.sav not found in the same directory as this file, required for tooltips or docs links!'
  endif
  
  ;restore the strings for our CSV file which has a variable called toolDatabase
  ;in it
  restore, routines

  ;parse strigns
  posTwo = strpos(toolDatabase, ';;')
  idx_change = where( strpos(toolDatabase, ';;') ne -1, count_change)
  if (count_change gt 0) then begin
    toolDatabase[idx_change] = toolDatabase[idx_change].replace(';;', '; ;')
  endif

  ;make all of the functions/procedures uppercase in our database
  foreach line, toolDatabase, idx do begin
    split = strsplit(line, ';', /EXTRACT)
    if (n_elements(split) eq 6) then begin
      up = strupcase(split[1])
      if (up ne split[1]) then begin
        split[1] = up
        toolDatabase[idx] = strjoin(split, ';')
      endif
    endif
  endforeach

  ;save our database as an object property
  self.ROUTINE_DB = ptr_new(toolDatabase)
  if keyword_set(custom_tooltips) then self.CUSTOM_DB = custom_tooltips

  ;obtain list of current System Procedures from ROUTINE_INFO:
  sysProcedures = [strlowcase(routine_info(/SYSTEM)), 'return']

  ;only keep object methods
  idx_keep = where(strpos(sysProcedures,':') eq -1, count_keep)
  if (count_keep gt 0) then begin
    sysProcedures = temporary(sysProcedures[idx_keep])
  endif

  ;obtain list of current System Functions from ROUTINE_INFO:
  sysFunctions = strlowcase(routine_info(/SYSTEM, /FUNCTIONS))

  idx_keep = where(strpos(sysFunctions,':') eq -1, count_keep)
  if (count_keep gt 0) then begin
    sysFunctions = temporary(sysFunctions[idx_keep])
  endif


  ;add modifications to our things we want to search for
  sysFunctions = [sysFunctions, 'hash', 'orderedhash', 'list', 'idltask']
  sysFunctions = temporary(sysFunctions.sort())

  ;add our program control statements
  ;do this because we need a way to delineate between procedures and these
  programControl = ['and', 'begin', 'break', 'case', 'common', 'compile_opt',$
    'continue', 'do', 'else', 'end', 'endcase', 'endelse', 'endfor', 'endforeach', $
    'endif', 'endrep', 'endswitch', 'endwhile', 'eq', 'for', 'foreach', $
    'forward_function', 'function', 'ge', 'goto', 'gt', 'if', 'inherits', $
    'le', 'lt', 'mod', 'ne', 'not', 'of', 'on_ioerror', 'or', 'pro', 'repeat',$
    'switch', 'then', 'until', 'while', 'xor']

  ;create a data structure to remember our variables
  datNames = orderedhash()
  datNames['SYSPROCEDURES']       = sysProcedures
  datNames['SYSFUNCTIONS']        = sysFunctions
  datNames['PROGRAMCONTROL']      = programControl

  ;save to object definition
  self.SEARCH_FOR = datNames

  return, 1
end

;+
; :Description:
;    Routine for cleaning up the HTMLizer object.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro HTMLizer::Cleanup
  compile_opt idl2
  obj_destroy, self
end


;+
;
; :Private:
; 
; :Description:
;    Internal routine to process a string.
;
; :Params:
;    text
;
; :Keywords:
;    CONTINUATION
;    TOOLTIPS
;    DOCS_LINKS
;    STRING_START
;    IDL_CONSOLE
;    STR_ORIG
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro htmlizer::ProcessString, text, $
  CONTINUATION = continuation,$
  TOOLTIPS = tooltips,$
  DOCS_LINKS = docs_links,$
  STRING_START = string_start,$
  IDL_CONSOLE = idl_console,$
  STR_ORIG = str_orig
  compile_opt idl2, hidden
  
  ;pre-allocate an array to hold our string contents for us
  strings = strarr(2)
  process = bytarr(2) + 1b
  
  ;base link for our tool tips
  baseLink = self.BASELINK

  ;get what we want to search for
  findThese = self.SEARCH_FOR

  ;get information from our object  
  sysProcedures = findThese['SYSPROCEDURES']
  sysFunctions = findThese['SYSFUNCTIONS']
  programControl = findThese['PROGRAMCONTROL']


  ;custom hashes tha tconatin key/value pairs with what we need to search
  ;for in our tool database for everything to work correctly with tooltips
  ;and links to the documentation center
  
  ;hash to save the custom database searches for sys procedures
  sysProSearch = hash()
  sysProSearch['print'] = 'PRINT/PRINTF'
  sysProSearch['printf'] = 'PRINT/PRINTF'

  ;hash to save the custom database searches for sys functions
  sysFuncSearch = hash()

  ;hash to save the custom database searches for lib procedures
  libProSearch = hash()

  ;hash to save the custom database searches for lib functions
  libFuncSearch = hash()

  ;hash to save the custom database searches for program controls
  progConSearch = hash()

  ;hash to save the custom database searches for system variables
  sysvSearch = hash()

  ;hash to save the custom database searches for function methods
  funcMethodSearch = hash()

  ;hash to save the custom database searches for function methods
  proMethodSearch = hash()
  
  ;hash for custom database searched with control statements
  idl_control = hash()
  
  ;control statements that a procedure can come after
  okPro = ['then', 'else', 'do']
  
  ;get our tool database
  toolDatabase = *self.ROUTINE_DB
  
  ;replace any bad characters in the substring
  replaceHTMLChars, text
  
  ;add our first string to work with
  strings[0] = text
  process[0] = 1
  nparts = 1
  
  ;check if we don't want to color our line
  if text.startswith('!NOCOLOR') then begin
    text = strmid(text, 9)
    return
  endif
  
  ;check if we have console output
  if text.startswith('!CONSOLE') then begin
    text = '      ' + strmid(text, 9)
    return
  endif
  
  ;check if we have a bold line
  if (strpos(strtrim(text,2), '!BOLD') eq 0) then begin
    text = '<font class="idl_bold">' + text.replace('!BOLD', '') + '</font>'
    return
  endif

  ;check if we have a gray line
  if (strpos(strtrim(text,2), '!GRAY') eq 0) then begin
    text = '<font class="idl_gray">' + text.replace('!GRAY', '') + '</font>'
    return
  endif
  
  ;flag if we ahve an equal sign or not
  eqStart = strpos(text, '=')
  if (eqStart ne -1) then begin
    hasEqual = 1
  endif else begin
    hasEqual = 0
  endelse
  
  ;check for executive commands
  ;check for function methods with periods
  exp = '\.[a-z_][a-z0-9_]*'
  replacer = '<font class="idl_exec '
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue

    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    ;skip if not found
    if (start eq -1) then continue

    ;get the character before only if we are not at the beginning of the string
    if (start gt 0) then begin
      if (strtrim(strmid(text, start-1, 1),2) ne '') then continue
    endif
    ;fix position
    start+=1
    length-=1

    ;get procedure method
    chars = strmid(text, start, length)

    ;refresh our replacer
    new = replacer + '">' + chars + '</font>'

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;stop processing because we can only have one xecutive command at a time
    break
  endforeach
  
  
  ;check for function methods with periods
  exp = '\.[a-z_][a-z0-9_$]*\('
  replacer = '<font class="idl_lib_func '
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue

    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    ;skip if not found
    if (start eq -1) then continue

    ;fix position
    start+=1
    length-=2

    ;get procedure method
    chars = strmid(text, start, length)

    ;refresh our replacer
    new = replacer

    update_replacer, $
      chars, new, toolDatabase,$
      SEARCH_REPLACE = funcMethodSearch,$
      /METHOD,$
      /FUNC,$
      TOOLTIPS = tooltips,$
      DOCS_LINKS = docs_links,$
      CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
      BASELINK = baselink

    ;save our updates
    if (i eq 0) then begin
      ;extract the string
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;check for function methods with dashes and greater than signs
  exp = '-&gt;[a-z_][a-z0-9_$]*\('
  replacer = '<font class="idl_lib_func '
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue

    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    ;skip if not found
    if (start eq -1) then continue

    ;fix position
    start+=5
    length-=6

    ;get procedure method
    chars = strmid(text, start, length)

    ;refresh our replacer
    new = replacer

    update_replacer, $
      chars, new, toolDatabase,$
      SEARCH_REPLACE = funcMethodSearch,$
      /METHOD,$
      /FUNC,$
      TOOLTIPS = tooltips,$
      DOCS_LINKS = docs_links,$
      CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
      BASELINK = baselink

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach
  
  ;check for procedure methods with dots
  exp = '\.[a-z_][a-z0-9_$]*'
  replacer = '<font class="idl_lib_pro '
  foreach text, strings, i do begin
    ;skip if line continuation
    if keyword_set(continuation) then continue
    
    ;only check first part of string
    if ~keyword_set(string_start) then continue
    
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') OR keyword_set(continuation) then continue
    
    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)
    
    ;skip if not found
    if (start eq -1) then continue
    
    ;get the next character
    nextChar = strmid(text,start+length,1)
    
    ;make sure we are not a property
    if (total(nextChar eq ['.']) gt 0) then begin
      ; i.e. this.that, THIS = 5, THAT = 6
      add = start+length
      start = stregex(strmid(text,start+length), exp, LENGTH = length, /FOLD_CASE)

      ;skip if not found
      if (start eq -1) then continue
      
      ;increment start
      start += add
    endif
    
    ;find the comma and equal sign
    eqStart = strpos(text, '=')
    commaStart = strpos(text, ',')

    ;make sure we are left of the equal sign if we have one
    ;and that there is a comma present after the found string
    if (eqStart ne -1) then begin
      if (commaStart eq -1) then continue
      if (eqStart le start) OR (eqStart lt commaStart) then continue
    endif
    
    ;check our comma
    if (commaStart ne -1) then begin
      if (start gt commaStart) then continue
    endif
    
    ;if IDL console and no comma present skip because we cannot
    ;delineate between variables (i.e. strucure, dict) or procedures
    if keyword_set(idl_console) AND (commaStart eq -1) then continue
    
    ;check to make sure that, if there are strings after the name that there is 
    ;a comma present. otherwise we probably have a property
    if (strtrim(strmid(text, start + length + 1),2) ne '') AND (commaStart eq -1) then continue

    ;get original starting point
    startOrig = start
    
    ;fix position
    start+=1
    length-=1

    ;get procedure method
    chars = strmid(text, start, length)

    ;check if we really have an executive command being set
    ;this will be when the string starts with '.' or when there
    ;is a space in front of the period
    if (startorig eq 0) then begin
      exec = 1
    endif else begin
      if (strmid(text, startorig-1, 1) eq ' ') then begin
        exec = 1
      endif else begin
        exec = 0
      endelse
    endelse
    
    ;check if property or a 
    if (exec) then begin
      new = '<font class="idl_exec">' + chars + '</font>'
    endif else begin
      ;refresh our replacer
      new = replacer

      update_replacer, $
        chars, new, toolDatabase,$
        SEARCH_REPLACE = proMethodSearch,$
        /METHOD,$
        TOOLTIPS = tooltips,$
        DOCS_LINKS = docs_links,$
        CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
        BASELINK = baselink
    endelse
    
    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse
    
    ;increment the number for our counter
    i+=1
  endforeach

  ;check for procedure method with dashes and greater than signs
  exp = '-&gt;[a-z_][a-z0-9_$]*'
  replacer = '<font class="idl_lib_pro '
  foreach text, strings, i do begin
    ;only check first part of string
    if ~keyword_set(string_start) then continue

    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') OR keyword_set(continuation) then continue

    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    ;skip if not found
    if (start eq -1) then continue

    ;find comma and equal pos
    eqStart = strpos(text, '=')
    commaStart = strpos(text, ',')

    ;make sure we are left of the equal sign if we have one
    ;and that there is a comma present after the found string
    if (eqStart ne -1) then begin
      if (commaStart eq -1) then continue
      if (eqStart le start) OR (eqStart lt commaStart) then continue
    endif

    ;check our comma
    if (commaStart ne -1) then begin
      if (start gt commaStart) then continue
    endif

    ;if IDL console and no comma present skip because we cannot
    ;delineate between variables or procedures
    if keyword_set(idl_console) AND (commaStart eq -1) then continue

    ;check to make sure that, if there are strings after the name that there is
    ;a comma present. otherwise we probably have a property
    if (strtrim(strmid(text, (start + length + 1) < strlen(text)),2) ne '') AND (commaStart eq -1) then continue

    ;get original starting point
    startOrig = start

    ;fix position
    start+=5
    length-=5

    ;get procedure method
    chars = strmid(text, start, length)

    ;refresh our replacer
    new = replacer

    update_replacer, $
      chars, new, toolDatabase,$
      SEARCH_REPLACE = proMethodSearch,$
      /METHOD,$
      TOOLTIPS = tooltips,$
      DOCS_LINKS = docs_links,$
      CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
      BASELINK = baselink

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;check for functions
  exp = '[a-z_][a-z0-9_$]*\('
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue
    
    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)
    
    ;skip if not found
    if (start eq -1) then continue

    ;remove parentheses
    length --

    ;get function name
    chars = strmid(text, start, length)

    ;check for internal/external
    if (total(strlowcase(chars) eq sysFunctions) gt 0) OR (strpos(strlowcase(chars), 'envi') ne -1) OR (strpos(strlowcase(chars), 'idlgr') ne -1) then begin
      new = '<font class="idl_sys_func '
      search = sysFuncSearch
    endif else begin
      new = '<font class="idl_lib_func '
      search = libFuncSearch
    endelse

    update_replacer, $
      chars, new, toolDatabase,$
      SEARCH_REPLACE = funSearch,$
      /FUNC,$
      TOOLTIPS = tooltips,$
      DOCS_LINKS = docs_links,$
      CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
      BASELINK = baselink

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;check for control statements
  exp = '[a-z][a-z0-9_]+'
  foreach text, strings, i do begin

    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue

    ;recursively search for control statements - they are harder because
    ;;we need to match words to the control statements so we much search
    ;all words in our string
    
    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)
    
    ;skip if not found
    if (start eq -1) then continue
    
    ;iterate through string
    while (start ne -1) do begin
      ;get word
      chars = strmid(text, start, length)

      ;check if special escape character for HTML with gt or lt
      charBefore = strmid(text, start-1, 1)
      if (charBefore eq '&') then begin
        chars = charBefore + chars
      endif

      ;check for match and exit if found
      if (total(strlowcase(chars) eq programControl) gt 0) then begin
        new = '<font class="idl_control '
        break
      endif
      
      ;no match, so search rest of string
      addOrig = start + length
      start = stregex(strmid(text, start + length + 1), exp, LENGTH = length)
      if (start ne -1) then begin
        start += addOrig + 1
      endif
    endwhile

    ;skip if not found
    if (start eq -1) then continue

    update_replacer, $
      chars, new, toolDatabase,$
      SEARCH_REPLACE = controlSearch,$
      /CONTROL,$
      TOOLTIPS = tooltips,$
      DOCS_LINKS = docs_links,$
      CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
      BASELINK = baselink

    ;check if we have a procedure or function being declared
    if (strlowcase(chars) eq 'pro') OR (strlowcase(chars) eq 'function') then begin
      ;check if our id is a procedure or function definition to colorize the name of the function
      ;or procedure as well
      after = strmid(text, start + length + 1)
      endPos = strpos(after, ',')
      
      ;get the item we want to color
      if (endPos eq -1) then begin
        color = after
        length += strlen(after) + 1
      endif else begin
        color = strmid(after, 0, endPos)
        length += 1 + endPos
      endelse
      
      ;update our new addition to our string and change the length of our
      ;new piece
      case strlowcase(chars) of
        'pro':begin
          new += ' <font class="idl_lib_pro">' + color + '</font>'
        end
        'function':begin
          new += ' <font class="idl_lib_func">' + color + '</font>'
        end
        else:;do nothing
      endcase
    endif

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start + length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start + length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;check for properties being set
  exp = '\.[a-z_][a-z0-9_]+'
  front = '<font class="idl_prop">'
  back = '</font>'
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue

    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    if (start eq -1) then continue
      
    ;get the next character (if we have a period next then we have a property)
    next  = strmid(text, start+length, 1)
    
    ;additional skip contidition
    if keyword_set(idl_console) AND (strpos(str_orig, '=') eq -1) AND (next ne '.') then continue

    ;fix position
    start+=1
    length-=1

    ;extract substring
    new = front + strmid(text, start, length) + back

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;check for procedures
  ;start with one letter; contains letters, numbers, underscore; ends with end of word or comma 
  exp = '[a-z_][a-z0-9_]+'
  foreach text, strings, i do begin
    ;skip line continuations
    if keyword_set(continuation) then continue
    
;    ;check to see if we have a valid control statement before our procedure call
;    if (i gt 0) then begin
;      lowFront = strlowcase(strings[i-1])
;
;      ;make sure we have control before
;      if (strpos(lowFront, '"idl_control ') ne -1) AND () then begin
;        ;extract our control statement
;        posControl = stregex(lowFront, '">[a-z_]+<\/', LENGTH = lengthControl)
;        if (posControl ne -1) then begin
;          ;extract substring
;          mid = strmid(lowFront, posControl+2,lengthControl-4)
;
;          ;make sure we have a valid control statement before
;          if (total(okPro eq mid) eq 0) then begin
;            continue
;          endif
;        endif
;      endif
;    endif
    
    ;ONLY check the first part of the string for a procedure, don't check if line continuation
;    if (i gt 0) OR ~keyword_set(string_start) then continue
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue
    
    ;search string
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    ;recursively search the string
    while (start ne -1) do begin
      ;check if special escape character for HTML with gt or lt
      ;if found, search rest of string, otherwise break
      charBefore = strmid(text, start-1, 1)
      if (charBefore eq '&') then begin
        ;no match, so search rest of string
        addOrig = start + length
        start = stregex(strmid(text, start + length + 1), exp, LENGTH = length)
        if (start ne -1) then begin
          start += addOrig + 1
          break
        endif
      endif else begin
        break
      endelse
    endwhile
    
    ;skip if not found
    if (start eq -1) then continue

    ;get procedure name
    chars = strmid(text, start, length)
    
    ;check to see if we have a valid control statement before our procedure call
    if (i gt 0) then begin
      lowFront = strlowcase(strings[i-1])

      ;make sure we have control before
      if (strpos(lowFront, '"idl_control ') ne -1) AND ~(strmid(strcompress(strmid(text, 0, start), /REMOVE_ALL),0,1, /REVERSE_OFFSET) eq ':') then begin
        ;extract our control statement
        posControl = stregex(lowFront, '">[a-z_]+<\/', LENGTH = lengthControl)
        if (posControl ne -1) then begin
          ;extract substring
          mid = strmid(lowFront, posControl+2,lengthControl-4)

          ;make sure we have a valid control statement before
          if (total(okPro eq mid) eq 0) then begin
            continue
          endif
        endif
      endif
    endif
    
    ;check for us being wihtin a function call
    posFunc = stregex(text, '\([^]]+\)', LENGTH = funcLength)
    if (posFunc ne -1) then begin
      if (start gt posFunc) AND (start lt (posFunc + funcLength)) then begin
        continue
      endif
    endif
    
    ;check if we are within the start of a function call that does not end on this line
    ;this could be robustified, but will cover most cases
    posFunc = strpos(text, '(')
    if (posFunc ne -1) then begin
      if (start gt posFunc) then begin
        continue
      endif
    endif
    
    ;get comma position in original string
    origStart = strpos(str_orig, strmid(text, start-1, length+2))
    commaPos = strpos(str_orig, ',')
    equalPos = strpos(str_orig, '=')
    
    ;check if we need to bump up the original start point
    if (start ne 0) then begin
      origStart += 1
    endif
    
    ;check for comma positions only if we are not after a control statement
    if ~(strpos(strlowcase(strings[(i-1) > 0]), '"idl_control ') ne -1) then begin
      ;make sure we are left of the equal sign if we have one
      ;and that there is a comma present after the found string 
      if (equalPos ne -1) then begin
        if (commaPos eq -1) then continue
        if (equalPos le origStart) OR (equalPos lt commaPos) then continue
      endif
  
      ;check our comma
      if (commaPos ne -1) then begin
        if (origStart gt commaPos) then continue
      endif else begin
        ;skip if not the string start without a comma present
        if ~keyword_set(string_start) then continue
        
        ;check to make sure that only spaces come before the procedure name otherwise skip
        ;only check if in line or not beginning
        if (origStart ne 0) then begin
          if (strtrim(strmid(str_orig, origStart-1, 1),2) ne '') then continue
        endif
      endelse
    endif else begin
      commaPos = strpos(text, ',')
      equalPos = strpos(text, '=')
      if (commaPos ne -1) AND (equalPos ne -1) then begin
        if (equalPos lt commaPos) then continue
      endif
      if (commaPos eq -1) AND (equalPos ne -1) then continue
    endelse

    ;check the next character
    if (total(strmid(text, start+length, 1) eq ['.', '[']) gt 0) then continue
;    if (strmid(strcompress(strmid(text, start+length), /REMOVE_ALL),0,1) ne ',') then continue

    ;if IDL console and no comma present skip because we cannot 
    ;delineate between variables or procedures
    if keyword_set(idl_console) AND (commaPos eq -1) then continue

    ;check for internal/external
    if (total(strlowcase(chars) eq sysProcedures) gt 0) then begin
      new = '<font class="idl_sys_pro '
      search = sysProSearch
    endif else begin
      new = '<font class="idl_lib_pro '
      search = libProSearch
    endelse

    update_replacer, $
      chars, new, toolDatabase,$
      SEARCH_REPLACE = search,$
      /PROC,$
      TOOLTIPS = tooltips,$
      DOCS_LINKS = docs_links,$
      CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
      BASELINK = baselink

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;check for named structure declaration
  exp = '{[a-z_][a-z0-9_]+,'
  front = '<font class="idl_bold">'
  back = '</font>'
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue

    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    if (start eq -1) then continue

    ;fix position
    start += 1
    length -= 2

    ;extract substring
    new = front + strmid(text, start, length) + back

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach
  
  ;check for structure tags
  exp = '[a-z_][^{:?\[]+:'
  front = '<font class="idl_struct_tag">'
  back = '</font>'
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue

    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    if (start eq -1) then continue

    ;fix position
    length--

    ;extract substring
    new = front + strmid(text, start, length) + back

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;check for system variables
  exp = '!{1}[a-z][a-z0-9_]+'
  replacer = '<font class="idl_sysv '
  foreach text, strings, i do begin
    ;skip conditions
    if ~process[i] OR (strtrim(text,2) eq '') then continue
    ; i.e. this.that, THIS = 5, THAT = 6
    start = stregex(text, exp, LENGTH = length, /FOLD_CASE)

    if (start eq -1) then continue

    ;get procedure method
    chars = strtrim(strmid(text, start, length),2)

    ;refresh our replacer
    new = replacer

    update_replacer, $
      chars, new, toolDatabase,$
      SEARCH_REPLACE = sysvSearch,$
      /SYSV,$
      TOOLTIPS = tooltips,$
      DOCS_LINKS = docs_links,$
      CUSTOM_TOOLTIPS = self.CUSTOM_DB,$
      BASELINK = baselink

    ;save our updates
    if (i eq 0) then begin
      strings = [strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [1, 0, 1, process[i+1:-1]]
      string_start = 0
    endif else begin
      strings = [strings[0:(i-1)>0], strmid(text, 0, start), new, strmid(text, start+length), strings[i+1:-1]]
      process = [process[0:(i-1)>0], 1, 0, 1, process[i+1:-1]]
    endelse

    ;increment the number for our counter
    i+=1
  endforeach

  ;find numbers!
  exp =   '[0-9][0-9.]*'
  front = '<font class="idl_num">'
  back =  '</font>'
  foreach text, strings, i do begin
    ;skip lines we don't want to process
    if ~process[i] then continue
    
    start = stregex(text, exp, LENGTH = length)
    while (start ne -1) do begin
      ;get char before and char after
      if (start gt 0) then begin
        before = strmid(text, start-1, 1)
      endif else begin
        before = ''
      endelse

      ;char after
      after = strmid(text, start + length, 1)

      if (strtrim(before, 2) ne '') then begin
        skip = 0
        case 1 of
          ;check for having numbers in functions/procedure names
          ;if so, then skip
          (stregex(before, '[a-z]', /FOLD_CASE) ne -1): begin
            startOrig = start
            lengthOrig = length
            start = stregex(strmid(text, startOrig+lengthOrig), exp, LENGTH = length)
            if (start ne -1) then begin
              start += startOrig + lengthOrig
            endif
            skip = 1
          end

          ;period before, so adjust position accordingly
          (before eq '.'):begin
            start--
            length++
          end

          else:;do nothing
        endcase
        if keyword_set(skip) then continue
      endif

      ;check for type declaration character
      if (strtrim(after,2) ne '') then begin
        extra = 1
        while (1) do begin
          after = strmid(text, start + length -1 + extra, 1)
          if (stregex(after, '[0-9bsulde]', /FOLD_CASE) ne -1) then begin
            length += 1
          endif else begin
            break
          endelse
        endwhile
      endif

      chars = strmid(text, start, length)
      new = front + chars + back
      numstart = strlen(new)
      stringstart = strmid(text, 0, start)
      stringend = strmid(text, start + length)
      newstart = strlen(stringstart + new)
      text = stringstart + new + stringend
      start = stregex(strmid(text, newstart), exp, LENGTH = length)
      if (start ne -1) then begin
        start += newstart
      endif
    endwhile
    
    ;save our changes
    strings[i] = text
  endforeach

  ;combine our strings again
  text = strjoin(strings)
end



;+
; :Description:
;    Function method to parse and add syntax highlighting to the provided
;    IDL code in `textArr`.
;
; :Params:
;    textArr: in, required, type=string[*]
;      Specify the strings that you want to process and colorize.
;
; :Keywords:
;    TOOLTIPS: in, optional, type=boolean, default=false
;      Set to true to have tooltips be added into the colorizing. Tooltips will
;      be added ONLY if the routine name is unique and matches a function,
;      procedure, or method that is in the CSV lookup table.
;    DOCS_LINKS: in, optional, type=boolean, default=false
;      When set, links to the documentation at harrisgeospatial.com will be added to
;      the code when functions, procedures, or methods match the CSV lookup table.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function HTMLizer::HTMLize, textArr,$
  TOOLTIPS = tooltips,$
  DOCS_LINKS = docs_links
  compile_opt idl2, hidden
  
  ;duplicate our strings that we want to process
  strOrig = textArr
  
  ;process each string in our array
  foreach str, textArr, i do begin
    ;skip empty lines
    if (strtrim(str,2) eq '') then continue
    
    ;check if we have a line continuation or not - so we don't misclassify procedures
    if (i gt 0) then begin
      if (strpos(strOrig[i-1], '$') ne -1 ) then begin
        continuation = 1
      endif else begin
        continuation = 0
      endelse
    endif else begin
      continuation = 0
    endelse
    
    ;check if we are in the IDL console or not
    if (strpos(str, 'IDL>') ne -1) OR (strpos(str, 'ENVI>') ne -1)then begin
      idl_console = 1
    endif else begin
      idl_console = 0
    endelse
    
    ;split up our line into sections that we can and cannot process
    strings = splitString(str, PROCESS = process, STRING_START = string_start)
    
    ;split based on HTML replace characters, helps greatly simplify string extraction
    secondSplit, strings, process, string_start
    
    ;build our "original" string with the pieces that will be processed
    idxJoin = where(process, countJoin)
    if (countJoin gt 0) then begin
      str_orig = strjoin(strings[idxjoin], ' ')
    endif else begin
      str_orig = ''
    endelse

    ;iterate over each good portion of the line
    foreach subStr, strings, j do begin
      ;make sure we can process our substring
      if (process[j]) then begin
        ;add color to the string
        self.processString, subStr, $
          CONTINUATION = continuation,$
          TOOLTIPS = tooltips,$
          DOCS_LINKS = docs_links,$
          STRING_START = string_start[j],$
          IDL_CONSOLE = idl_console,$
          STR_ORIG = str_orig

        ;save our changes
        strings[j] = subStr
      endif
    endforeach
    
    ;save our modifications
    textArr[i] = strjoin(strings)
  endforeach
  
  ;make a new variable for our output
  newStrings = temporary(textArr)
  
  ;return our input to the original value
  textArr = temporary(strOrig)
  
  ;return our results
  return, ['<pre class="idl_code_block">', newStrings, '</pre>']
end


;+
; :Description:
;    Simple function to read the contents of a text file.
;
; :Params:
;    file: in, required, type=string
;      Specify the text file that you want to read in.
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function htmlizer_read_file, file
  compile_opt idl2, hidden
  on_error, 2
  strings = strarr(file_lines(file))
  if ~file_test(file) then message, 'File does not exist, cannot read.'
  openr, lun, file, /GET_LUN
  readf, lun, strings
  free_lun, lun
  return, strings
end


;+
; :Description:
;    Simple procedure to write text to disk.
;
; :Params:
;    file: in, required, type=string
;      Specify the file that you want to write to disk.
;    strings: in, requried, type=string[*]
;      The strings that you want to include in the text file.
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro htmlizer_write_file, file, strings
  compile_opt idl2, hidden
  dir = file_dirname(file)
  if ~file_test(dir) then file_mkdir, dir
  openw, lun, file, /GET_LUN
  printf, lun, strings, /IMPLIED_PRINT
  free_lun, lun
end


;+
; :Description:
;    Exports the contents of the IDL SAVE file that
;    contains the routines to disk for editing.
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro htmlizer_export_csv
  compile_opt idl2, hidden
  
  ;get the current directory
  thisdir = file_dirname(routine_filepath())

  ;check for the CSV file
  routines = thisdir + path_sep() + 'idl_routines.sav'
  if ~file_test(routines) then begin
    message, 'idl_routines.sav not found in the same directory as this file, required for tooltips or docs links!'
  endif
  
  ;read in the strings
  restore, routines
  
  ;write the strings to disk
  htmlizer_write_file, thisdir + path_sep() + 'idl_routines.csv', toolDatabase
end


;+
; :Description:
;    Updates the contents of the IDL SAVE file that
;    contains the routines for tooltips and documentation
;    by reading from a file called idl_routines.csv in this
;    directory.
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro htmlizer_import_csv
  compile_opt idl2, hidden

  ;get the current directory
  thisdir = file_dirname(routine_filepath())

  ;check for the CSV file
  routines = thisdir + path_sep() + 'idl_routines.csv'
  if ~file_test(routines) then begin
    message, 'idl_routines.csv not found in the same directory as this file, required for tooltips or docs links!'
  endif
  
  ;read in the strings
  toolDatabase = htmlizer_read_file(routines)
  
  ;save them to disk with compression
  save, toolDatabase, /COMPRESS, FILE = thisdir + path_sep() + 'idl_routines.sav'
end


;+
; :Description:
;    Simple procedure that copies the CSS file for the IDL styles to a
;    user specified directory. The directory is created if it does not exist.
;
; :Params:
;    dir: in, requried, type=string
;      The output directory that you want to copy the CSS file to. Made if it
;      does not exist already.
;
;
;
; :Author: Zachary Norman - Github: [znorman-harris](https://github.com/znorman-harris)
;-
pro htmlizer_copy_css, dir
  compile_opt idl2, hidden
  on_error, 2
  
  if (dir eq !NULL) then begin
    message, 'dir not specified, requried argument!'
  endif
  
  ;get this location
  thisDir = file_dirname(routine_filepath())
  
  ;find the css file
  css = thisDir + path_sep() + 'idl-styles.css'
  
  ;make sure the file exists
  if ~file_test(css) then begin
    message, 'CSS file "idl-styles.css" not found next to this source as expected!'
  endif
  
  ;copy to the output directory
  if ~file_test(dir) then file_mkdir, dir
  file_copy, css, dir, /OVERWRITE
end

;+
; :Private:
; 
; :Description:
;    Object definition fot the HTMLizer object.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro HTMLizer__define
  compile_opt idl2, hidden
  struct = {HTMLizer, $
    inherits IDL_Object,$

    URI:'',$            ;file path to CropCenters object
    VERSION:'',$    ;flag whther or not we have the old version for the JSON file

    ;orderedhash of our names we need
    SEARCH_FOR:orderedhash(),$

    ;text file contents of our routines
    ROUTINE_DB:ptr_new(),$
    CUSTOM_DB: orderedhash(),$
      
    ;base link for content
    BASELINK:''$
  }
end

;main level program that shown an example of how you can use the routine

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

end