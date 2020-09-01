;FUNCTIONS for Rename DICOM

function newFileName, path, folder, elemArr, tagStruct, formatsArr
  newpath=''
  
  IF folder THEN BEGIN
    Spawn, 'dir '  + '"'+path+'"' + '*'+ '/b /a-d', res; files only

    IF res(0) NE '' THEN BEGIN;find first dcm file and extract info from this header
      res=path+res(sort(res))
      nn=N_ELEMENTS(res)
      dcm=-1
      counter=0
      FOR n=0, nn-1 DO BEGIN
        dcm=QUERY_DICOM(res(n))
        IF dcm EQ 1 THEN path=res(n)
        IF dcm EQ 1 THEN BREAK ELSE counter=counter+1
      ENDFOR
    ENDIF;no files found
  ENDIF ELSE dcm=QUERY_DICOM(path)
  
  IF dcm EQ 1 THEN BEGIN
    o=obj_new('idlffdicom')
    t=o->read(path)

    nElem=N_ELEMENTS(elemArr)
    desc=TAG_NAMES(tagStruct)
    
    nameArr=!Null
    FOR ee=0, nElem-1 DO BEGIN
      ide=WHERE(desc EQ STRUPCASE(elemArr(ee)))
      ide=ide(0)
      thisTag=tagStruct.(ide)
      test=o->GetReference(thisTag(0),thisTag(1))
      IF test(0) NE -1 THEN BEGIN
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)

        stest=size(test_peker, /TNAME)
        IF stest EQ 'POINTER' THEN BEGIN
          namePart=*(test_peker[0])

          nameParts=STRSPLIT(namePart(0),'\',/EXTRACT)
          formats=STRSPLIT(STRMID(formatsArr.(ide), 1, strlen(formatsArr.(ide))-2),'\',/EXTRACT)
          nP=N_ELEMENTS(nameParts)
          IF nP EQ N_ELEMENTS(formats) AND nP GT 1 THEN BEGIN
            namePartArr=!Null
            FOR p=0, nP-1 DO BEGIN
              IF formats(p) NE '_' THEN namePartArr=[namePartArr,STRING(nameParts(p), FORMAT='('+formats(p)+')')]
            ENDFOR
            namePart=STRJOIN(namePartArr,'_')
          ENDIF ELSE namePart=STRING(nameParts(0),FORMAT=formatsArr.(ide))
          
          nameArr=[nameArr,namePart]
        ENDIF

      ENDIF
    ENDFOR

    obj_destroy,o
    
    IF N_ELEMENTS(nameArr) GT 0 THEN BEGIN
      IF folder THEN nameArr=IDL_VALIDNAME(nameArr,/CONVERT_ALL)
      nameStr=STRJOIN(nameArr,'_')
      IF folder THEN nameStr=STRJOIN(STRSPLIT(nameStr,'_',/EXTRACT),'_') ;remove all multiple and first _

      arr=STRSPLIT(path,'\',/EXTRACT)
      last=n_elements(arr)-1
      IF folder THEN newpath=STRJOIN(arr[0:last-2],'\')+'\'+nameStr+'\' ELSE BEGIN
        nameStr=STRJOIN(STRSPLIT(nameStr,'*',/EXTRACT,/PRESERVE_NULL),'X');change * to X (format code not ideal)
        nameStr=STRJOIN(STRSPLIT(nameStr,' ',/EXTRACT),'_');change space to underscore
        nameStr=STRJOIN(STRSPLIT(nameStr,'[-/\?#%&{}`<>$!:@+|=]',/EXTRACT, /REGEX),'_')
        
        newpath=STRJOIN(arr[0:last-1],'\')+'\'+nameStr+'.dcm'
      ENDELSE
    ENDIF
    
  ENDIF

  return, newpath
end

function getUniqPaths, origPaths,newPaths,pathType

  sortedOrder=sort(newPaths)
  modNewPaths=newPaths(sortedOrder)
  modOrigPaths=origPaths(sortedOrder)
  u=UNIQ(modNewPaths)
  IF N_ELEMENTS(u) NE N_ELEMENTS(modNewPaths) THEN BEGIN
    FOR i=0, N_ELEMENTS(modNewPaths)-1 DO BEGIN
      eqPaths=WHERE(modNewPaths EQ modNewPaths(i), nP)
      IF modNewPaths(i) NE '' THEN BEGIN
        IF nP GT 1 THEN BEGIN
          FOR j=0, nP-1 DO BEGIN
            IF pathType EQ 1 THEN modNewPaths(eqPaths(j))= FILE_DIRNAME(modNewPaths(eqPaths(j)))+'\'+FILE_BASENAME(modNewPaths(eqPaths(j))) + '_' + STRING(j, FORMAT='(i02)')+'\' $
            ELSE modNewPaths(eqPaths(j))= FILE_DIRNAME(modNewPaths(eqPaths(j)))+'\'+FILE_BASENAME(modNewPaths(eqPaths(j)),'.dcm')+'_'+STRING(j, FORMAT='(i03)')+'.dcm'
          ENDFOR
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
  sortedOrder=sort(modNewPaths)
  modNewPaths=modNewPaths(sortedOrder)
  modOrigPaths=modOrigPaths(sortedOrder)

  structPaths=CREATE_STRUCT('origPaths',modOrigPaths, 'newPaths',modNewPaths)
  return, structPaths
end

;edit name template - string array - delete specific items
function editTemp, inputStr, xo, yo
  
  strlist=STRJOIN(inputStr,'|')
  box=[$
    '1, BASE,, /ROW', $
    '2, LABEL, Select elements to delete', $
    '1, BASE,, /ROW', $
    '2, LIST, '+strlist+', Select elements to delete, TAG=list', $
    '1, BASE,, /ROW', $
    '0, BUTTON, OK, QUIT, TAG=OK',$
    '2, BUTTON, Cancel, QUIT, TAG=Cancel']
  res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Edit naming template', XSIZE=300, YSIZE=250, FOCUSNO=1, XOFFSET=xo+200, YOFFSET=yo+200)
  
  IF res.OK THEN BEGIN
    selIds=res.list
    IF selIds(0) NE -1 THEN BEGIN
      IF N_ELEMENTS(selIds) EQ N_ELEMENTS(inputStr) THEN outputStr='' ELSE outputStr=removeIDarr(inputStr, selIds)
    ENDIF
  ENDIF ELSE outputStr=inputStr

RETURN, outputStr
end