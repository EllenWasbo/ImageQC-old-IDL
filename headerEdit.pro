pro headerEdit, nam, strTab, imgNo, GROUP_LEADER = bMain

  COMMON HEAD, tblHeader, imgNmb, origTbl, tagN

  COMPILE_OPT hidden

  editbox = WIDGET_BASE(TITLE='Edit header data', GROUP_LEADER=bMain,  $
    /COLUMN, XSIZE=500, YSIZE=600, XOFFSET=150, YOFFSET=150, /MODAL)

  imgNmb=imgNo
  origTbl=strTab
  tagN=nam
  nT=N_ELEMENTS(strTab)
  ml1=WIDGET_LABEL(editbox, VALUE='', YSIZE=20)
  bTab=WIDGET_BASE(editbox, /ROW)
  
  tblHeader=WIDGET_TABLE(bTab, VALUE=TRANSPOSE(strTab), XSIZE=1, YSIZE=nT, SCR_XSIZE=400, SCR_YSIZE=500,COLUMN_WIDTHS=[300], ROW_LABELS=nam, COLUMN_LABELS=['Content'], /EDITABLE, /ALL_EVENTS)
  
  bBottom=WIDGET_BASE(editbox, /ROW)

  mlBottom0=WIDGET_LABEL(bBottom, VALUE='', XSIZE=200)
  btnPlay=WIDGET_BUTTON(bBottom,VALUE='Use', UVALUE='useEdit', XSIZE=100)
  mlBottom1=WIDGET_LABEL(bBottom, VALUE='', XSIZE=20)
  btnClose=WIDGET_BUTTON(bBottom, VALUE='Cancel', UVALUE='cancelEdit', XSIZE=100)

  WIDGET_CONTROL, editbox, /REALIZE
  XMANAGER, 'headerEdit', editbox

end

pro headerEdit_event, event

  COMMON HEAD
  COMMON VARI

  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'useEdit':BEGIN
        WIDGET_CONTROL, tblHeader, GET_VALUE=curTable
        sz=SIZE(curTable, /DIMENSIONS)
        includeArr=actualTags(TAG_NAMES(structImgs.(imgNmb)), imgStructInfo, modality)
        idsInclude=WHERE(includeArr EQ 1)
        FOR i=0, sz(1)-1 DO BEGIN
          IF curTable(i) NE origTbl(i) THEN BEGIN  
            
            oldVal=structImgs.(imgNmb).(idsInclude(i))
            typeOld=SIZE(oldVal, /TNAME)
            newVal=STRSPLIT(curTable(i), ',', /EXTRACT)
            IF typeOld NE 'STRING' THEN newVal=FLOAT(newVal)
            valid=0
            IF N_ELEMENTS(newVal) EQ N_ELEMENTS(oldVal) THEN valid=1
            IF valid EQ 0 THEN sv=DIALOG_MESSAGE('Wrong format of new value for tag '+tagN(i), DIALOG_PARENT=event.TOP) 

            If valid THEN structImgs.(imgNmb).(idsInclude(i))=newVal
          ENDIF
        ENDFOR
        WIDGET_CONTROL, event.top, /DESTROY
      END
      'cancelEdit': WIDGET_CONTROL, event.top, /DESTROY
      ELSE:
    ENDCASE
  ENDIF

end