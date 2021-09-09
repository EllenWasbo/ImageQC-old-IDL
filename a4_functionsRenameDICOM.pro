;FUNCTIONS for Rename DICOM


;function newFileName moved to a0_functionsMini.pro

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

;edit name template in RenameDICOM - string array - delete specific items
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
  
  IF N_ELEMENTS(outputStr) EQ 0 THEN ouputStr=''

RETURN, outputStr
end