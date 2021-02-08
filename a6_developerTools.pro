pro verifyStructures

  ;all imgStruct info
  ;create dummy imgStruct
  dummyImgStruct=imgStructUpdate('','')
  imgDummyTags=TAG_NAMES(dummyImgStruct)
  descImgStruct=imgStructDescTags();a0_functionsMini.pro
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