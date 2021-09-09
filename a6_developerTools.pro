pro commonVariable
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  filenames=FILE_SEARCH(thisPath,'*.pro', COUNT=nFound)
  
  commonName='COMMON VARI'
  commonIn=!Null
  variableName='SELIM_IQ';or functionname or procedure or anything
  varIn=!Null
  
  ;loop through all .pro files and find where COMMON
  FOR i=0, nFound-1 DO BEGIN
    OPENR, filenhet, filenames(i), /GET_LUN
    elem=''
    WHILE ~ EOF(filenhet) DO BEGIN
      READF, filenhet, elem
      IF STRMATCH(elem,'*'+commonName+'*',/FOLD_CASE) THEN commonIn=[commonIn,filenames(i)]
      IF STRMATCH(elem,'*'+variableName+'*',/FOLD_CASE) THEN varIn=[varIn,filenames(i)]
      
    ENDWHILE
    CLOSE, filenhet
    FREE_LUN, filenhet
  ENDFOR
  
  commonIn=commonIn
  
  print, 'commonIn '
  print, commonIn(UniQ(commonIn))
  print, 'varIn '
  print, varIn(uniq(varIn))
end

function nTextLines, adr
  OPENR, unit, adr, /GET_LUN
  str=''
  nn=0
  WHILE ~ EOF(unit) DO BEGIN
    READF, unit, str
    strtrimmed=STRTRIM(str,1)
    IF strtrimmed NE '' AND STRMID(strtrimmed,0,1) NE ';' THEN nn=nn+1
  ENDWHILE
  FREE_LUN, unit
  RETURN, nn
end

pro nCodeLines
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  filenames=FILE_SEARCH(thisPath,'*.pro', COUNT=nFound)

  IF filenames(0) NE '' THEN BEGIN
    nLines=0
    nFiles=N_ELEMENTS(filenames)
    FOR i=0, nFiles-1 DO BEGIN
      nLines=nLines+ nTextLines(filenames(i))
    ENDFOR
  ENDIF

  PRINT, 'Number of code lines: ', nlines
end

pro verifyStructures

  ;all imgStruct info
  ;create dummy imgStruct
  dummyImgStruct=imgStructUpdate('','')
  imgDummyTags=TAG_NAMES(dummyImgStruct)
  descImgStruct=imgStructDescTags() ;a0_functionsMini.pro
  imgDescTags=TAG_NAMES(descImgStruct)
  set_imgStructInfo, imgInfo;set_Values.pro
  arrInfo=STRUPCASE(TRANSPOSE(imgInfo[0,*]))

  notInclInfo=!Null
  notInclDesc=!Null
  FOR i=0, N_ELEMENTS(imgDummyTags)-1 DO BEGIN
    IF arrInfo.HasValue(imgDummyTags(i)) EQ 0 THEN notInclInfo=[notInclInfo,imgDummyTags(i)]
    IF imgDescTags.HasValue(imgDummyTags(i)) EQ 0 THEN notInclDesc=[notInclDesc,imgDummyTags(i)]
  ENDFOR

  IF N_ELEMENTS(notInclInfo) NE 0 THEN BEGIN
    print, 'Not in set_Values.pro set_imgStructInfo. Any of these not String-values or any specific to a few modalities only?'
    print, notInclInfo
  ENDIF ELSE print,'All imgStruct tags included in set_imgStructInfo'

  IF N_ELEMENTS(notInclDesc) NE 0 THEN BEGIN
    print, 'Not in a0_functionsMini.pro imgStructDescTags():'
    print, notInclDesc
  ENDIF ELSE print,'All imgStruct tags included in imgStructDescTags'

  ;configS structures
  set_configSinfo, configSinfo
  arrInfo=STRUPCASE(TRANSPOSE(configSinfo[0,*]))
  dummyConfigS=updateConfigS('')
  tagsConfig=TAG_NAMES(dummyConfigS.(1))

  notIncl=!Null
  FOR i=0, N_ELEMENTS(tagsConfig)-1 DO BEGIN
    IF arrInfo.HasValue(tagsConfig(i)) EQ 0 THEN notIncl=[notIncl,tagsConfig(i)]
  ENDFOR

  IF N_ELEMENTS(notIncl) NE 0 THEN BEGIN
    print, 'Not in set_Values.pro set_configSinfo. Errors might occur.'
    print, notIncl
  ENDIF ELSE print,'All configS tags included in set_configSinfo'

end

pro compareConfigFiles

  adr0=DIALOG_PICKFILE(TITLE='Select first config file.', /READ, FILTER='*.dat',/FIX_FILTER)
  IF adr0(0) NE '' THEN BEGIN
    adr1=DIALOG_PICKFILE(TITLE='Select second config file.', /READ, FILTER='*.dat',/FIX_FILTER)
    IF adr1(0) NE '' THEN BEGIN
      RESTORE, adr0
      configS0=configS & quickTemp0=quickTemp & quickTout0=quickTout & loadTemp0=loadTemp & renameTemp0=renameTemp
      RESTORE, adr1
      IF ARRAY_EQUAL(struct2xml(configS0),struct2xml(configS)) THEN print, 'configS is the same' ELSE BEGIN
        print, 'configS differ'
        stop
      ENDELSE
      IF ARRAY_EQUAL(struct2xml(quickTemp0),struct2xml(quickTemp)) THEN print, 'quickTemp is the same' ELSE BEGIN
        print, 'quickTemp differ'
        stop
      ENDELSE
      IF ARRAY_EQUAL(struct2xml(quickTout0),struct2xml(quickTout)) THEN print, 'quickTout is the same' ELSE BEGIN
        print, 'quickTout differ'
        stop
      ENDELSE
      IF ARRAY_EQUAL(struct2xml(loadTemp0),struct2xml(loadTemp)) THEN print, 'loadTemp is the same' ELSE BEGIN
        print, 'loadTemp differ'
        stop
      ENDELSE
      IF ARRAY_EQUAL(struct2xml(renameTemp0),struct2xml(renameTemp)) THEN print, 'renameTemp is the same' ELSE BEGIN
        print, 'renameTemp differ'
        stop
      ENDELSE

    ENDIF
  ENDIF

end