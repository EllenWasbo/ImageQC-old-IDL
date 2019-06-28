;ImageQC - quality control of medical images
;Copyright (C) 2018  Ellen Wasbo, Stavanger University Hospital, Norway
;ellen@wasbo.no
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License version 2
;as published by the Free Software Foundation.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;setup of export format for QuickTest
pro QTexportSetup, GROUP_LEADER = mainbase, xoff, yoff

  COMMON QTSETUP, lstModality, lstTemplates, tblQTout, lstTest, lstAlt, lstCols, lstCalc, lstPer, txtDescr, qto_currMod, qto_currTemp,qto_currTest, qto_currOutp, qto_currSel
  COMMON VARI
  COMPILE_OPT hidden

  qto_currMod=0;currently selected modality
  qto_currTemp=0;currently selected template
  qto_currTest=0;currently selected test in table
  qto_currOutp=0;currently selected output in table for current test
  qto_currSel=0;currently selected row in table

  RESTORE, thisPath+'data\config.dat'

  defQTout=quickTout.(0).(0);first modality, first template = default

  QTexportSetupBox = WIDGET_BASE(TITLE='Templatemanager QuickTest output', GROUP_LEADER=mainbase,  $
    /COLUMN, XSIZE=930, YSIZE=370, XOFFSET=xoff, YOFFSET=yoff, /MODAL)

  QTsetupBoxTop=WIDGET_BASE(QTexportSetupBox, /ROW)
  ml0=WIDGET_LABEL(QTsetupBoxTop, VALUE='', xSIZE=5)

  bRgt=WIDGET_BASE(QTsetupBoxTop, /COLUMN)
  bMl=WIDGET_BASE(QTsetupBoxTop, XSIZE=30)
  bLft=WIDGET_BASE(QTsetupBoxTop, /COLUMN)

  bModality=WIDGET_BASE(bRgt, /ROW)
  lblModality=WIDGET_LABEL(bModality, VALUE='Modality ', FONT=font0,/ALIGN_LEFT)
  lstModality=WIDGET_DROPLIST(bModality, VALUE=TAG_NAMES(testVisualQTNames), UVALUE='qto_lstModality', XSIZE=70, FONT=font1)

  bTemp=WIDGET_BASE(bRgt, /COLUMN, YSIZE=170)
  lblTemp=WIDGET_LABEL(bTemp, VALUE='Templates ', FONT=font0,/ALIGN_LEFT)
  lstTemplates=WIDGET_LIST(bTemp, VALUE=TAG_NAMES(quickTout.(0)), UVALUE='qto_lstTemp', XSIZE=100, SCR_XSIZE=100, SCR_YSIZE=150, FONT=font1)
  WIDGET_CONTROL, lstTemplates, SET_LIST_SELECT=0
  bTempEdit=WIDGET_BASE(bRgt, /ROW)
  btnDupliTemp=WIDGET_BUTTON(bTempEdit, VALUE=thisPath+'images\copy.bmp' ,/BITMAP, TOOLTIP='Duplicate', UVALUE='qto_duplicate', FONT=font1)
  btnDeleteTemp=WIDGET_BUTTON(bTempEdit, VALUE=thisPath+'images\edit.bmp' ,/BITMAP, TOOLTIP='Rename', UVALUE='qto_rename', FONT=font1)
  btnDeleteTemp=WIDGET_BUTTON(bTempEdit, VALUE=thisPath+'images\delete.bmp' ,/BITMAP, TOOLTIP='Delete', UVALUE='qto_delete', FONT=font1)

  ml10= WIDGET_LABEL(bLft, VALUE='', YSIZE=29)
  lbltblQTout=WIDGET_LABEL(bLft, VALUE='Selected output template ', FONT=font0, /ALIGN_LEFT)
  tblQTout=WIDGET_TABLE(bLft, XSIZE=6, YSIZE=100, COLUMN_LABELS=['Test', 'Alternative', 'Columns', 'Calculation', 'Pr img or series','Description'], COLUMN_WIDTHS=[100,70,100,70,100,190], /NO_ROW_HEADERS, SCR_XSIZE=100*6+60, SCR_YSIZE=170, /ALL_EVENTS, FONT=font1)

  bEdit=WIDGET_BASE(bLft, /ROW)
  lstTest=WIDGET_DROPLIST(bEdit, VALUE=testVisualQTNames.(0), XSIZE=98, FONT=font1,UVALUE='qto_lstTest')
  lstAlt=WIDGET_DROPLIST(bEdit, VALUE=TAG_NAMES(tableHeaders.(0).(0)), XSIZE=68, FONT=font1,UVALUE='qto_lstAlt')
  lstCols=WIDGET_LIST(bEdit, VALUE=tableHeaders.(0).(0).(0), /MULTIPLE, SCR_XSIZE=98, SCR_YSIZE=100, FONT=font1)
  lstCalc=WIDGET_DROPLIST(bEdit, VALUE=['=', 'Min','Max','Avg','Std','Max abs'], XSIZE=68, FONT=font1)
  lstPer=WIDGET_DROPLIST(bEdit, VALUE=['Per image', 'Per series'], XSIZE=98, FONT=font1)

  bLowRgt=WIDGET_BASE(bEdit, /COLUMN)
  bEditBtns=WIDGET_BASE(bLowRgt, /ROW, YSIZE=28)
  txtDescr=WIDGET_TEXT(bEditBtns, VALUE='', XSIZE=100, SCR_XSIZE=150, YSIZE=1, SCR_YSIZE=20, FONT=font1, /EDITABLE)
  ml2=WIDGET_LABEL(bEditBtns, VALUE='', XSIZE=5)
  btnAddQTO=WIDGET_BUTTON(bEditBtns, VALUE=thisPath+'images\plus.bmp' ,/BITMAP,TOOLTIP='Add as new output', UVALUE='qto_add', FONT=font1)
  btnAddQTO=WIDGET_BUTTON(bEditBtns, VALUE=thisPath+'images\edit.bmp', /BITMAP,TOOLTIP='Overwrite selected', UVALUE='qto_overwrite', FONT=font1)
  btnDelQTO=WIDGET_BUTTON(bEditBtns, VALUE=thisPath+'images\delete.bmp' ,/BITMAP, TOOLTIP='Delete output', UVALUE='qto_deleteOutp', FONT=font1)

  ml3=WIDGET_LABEL(bLowRgt, VALUE='', YSIZE=50)

  btnAddQTO=WIDGET_BUTTON(bLowRgt, VALUE='Close window', UVALUE='qto_cancel', FONT=font1)

  qto_fillTable, 1

  WIDGET_CONTROL, QTexportSetupBox, /REALIZE
  XMANAGER, 'QTexportSetup', QTexportSetupBox

end

pro QTexportSetup_event, event

  COMMON QTSETUP
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=qt_uval

  IF N_ELEMENTS(qt_uval) GT 0 AND SIZE(qt_uval, /TNAME) EQ 'STRING' THEN BEGIN
    CASE qt_uval OF

      'qto_cancel': WIDGET_CONTROL, Event.top, /DESTROY
      'qto_lstModality': BEGIN;modality changed?
        IF WIDGET_INFO(lstModality, /DROPLIST_SELECT) NE qto_currMod THEN BEGIN
          qto_currMod=WIDGET_INFO(lstModality, /DROPLIST_SELECT)
          qto_currTemp=0
          RESTORE, thisPath+'data\config.dat'
          tempstruct=quickTout.(qto_currMod)
          WIDGET_CONTROL, lstTemplates, YSIZE=N_TAGS(tempstruct), SET_VALUE=TAG_NAMES(tempstruct), SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150

          ;update options lstTest, lstAlt, lstCols
          WIDGET_CONTROL, lstTest, SET_VALUE=testVisualQTNames.(qto_currMod), SET_DROPLIST_SELECT=0
          WIDGET_CONTROL, lstAlt, SET_VALUE=TAG_NAMES(tableHeaders.(qto_currMod).(0)), SET_DROPLIST_SELECT=0
          WIDGET_CONTROL, lstCols, SET_VALUE=tableHeaders.(qto_currMod).(0).(0), SET_LIST_SELECT=0

          qto_fillTable, 1
          
          IF qto_currMod EQ 1 OR qto_currMod EQ 2 THEN WIDGET_CONTROL, lstPer, SET_DROPLIST_SELECT=0, SENSITIVE=0 ELSE WIDGET_CONTROL, lstPer, SENSITIVE=1
        ENDIF
      END
      'qto_lstTemp': BEGIN;new temp selected?
        IF WIDGET_INFO(lstTemplates, /LIST_SELECT) NE qto_currTemp THEN BEGIN
          qto_currTemp=WIDGET_INFO(lstTemplates, /LIST_SELECT)
          WIDGET_CONTROL, tblQTout, SET_TABLE_SELECT=[0,0,0,0]
          qto_fillTable, 1
        ENDIF
      END

      'qto_duplicate': BEGIN
        ;ask for new name
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Name the new template:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Name the duplicate', XSIZE=300, YSIZE=100, FOCUSNO=0, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF ~res.Cancel THEN BEGIN
          IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not duplicate.', DIALOG_PARENT=event.top) ELSE BEGIN

            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            RESTORE, thisPath+'data\config.dat'
            currNames=TAG_NAMES(quickTout.(qto_currMod))

            IF currNames.HasValue(tempname) THEN BEGIN
              sv=DIALOG_MESSAGE('Duplication failed. A template with this name already exist.',DIALOG_PARENT=event.top)
            ENDIF ELSE BEGIN
              quickToutMod=CREATE_STRUCT(quickTout.(qto_currMod), tempname, quickTout.(qto_currMod).(WIDGET_INFO(lstTemplates, /LIST_SELECT)))
              quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

              SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
              WIDGET_CONTROL, lstTemplates, SET_VALUE=[currnames, tempname], SET_LIST_SELECT=N_ELEMENTS(currNames), SCR_YSIZE=150;added as last, set selected
              qto_currTemp=N_ELEMENTS(currNames)
            ENDELSE

          ENDELSE
        ENDIF

      END
      'qto_rename': BEGIN
        IF qto_currTemp NE 0 THEN BEGIN
          ;ask for new name
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, , LABEL_LEFT=Rename selected template:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Save',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename', XSIZE=300, YSIZE=100, FOCUSNO=0, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

          IF ~res.Cancel THEN BEGIN
            IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified.', DIALOG_PARENT=event.top) ELSE BEGIN

              tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
              RESTORE, thisPath+'data\config.dat'
              currNames=TAG_NAMES(quickTout.(qto_currMod))

              IF currNames.HasValue(tempname) THEN BEGIN
                sv=DIALOG_MESSAGE('A template with this name already exist. Renaming not possible.',DIALOG_PARENT=event.top)
              ENDIF ELSE BEGIN
                currNames(qto_currTemp)=tempname
                FOR i=0, N_ELEMENTS(currNames)-1 DO BEGIN
                  IF i EQ 0 THEN quickToutMod=CREATE_STRUCT(currNames(i),quickTout.(qto_currMod).(i)) ELSE quickToutMod=CREATE_STRUCT(quickToutMod, currNames(i), quickTout.(qto_currMod).(i))
                ENDFOR

                quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

                SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
                WIDGET_CONTROL, lstTemplates, SET_VALUE=currNames, SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150
              ENDELSE

            ENDELSE
          ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be renamed', DIALOG_PARENT=event.top)
      END
      'qto_delete': BEGIN
        IF qto_currTemp NE 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Are you sure you want to delete the selcted template', /QUESTION, DIALOG_PARENT=event.top)
          IF sv EQ 'Yes' THEN BEGIN
            RESTORE, thisPath+'data\config.dat'
            quickToutMod= removeIDstructstruct(quickTout.(qto_currMod), qto_currTemp)
            currNames=TAG_NAMES(quickToutMod)
            qto_currTemp=qto_currTemp-1

            quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)
            SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
            WIDGET_CONTROL, lstTemplates, SET_VALUE=currNames, SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150
            qto_fillTable, 1
          ENDIF

        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be delete', DIALOG_PARENT=event.top)
      END
      'qto_lstTest':BEGIN ;new test selected - update alternatives, and columns

        WIDGET_CONTROL, lstAlt, SET_VALUE=TAG_NAMES(tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT))), SET_DROPLIST_SELECT=0
        WIDGET_CONTROL, lstCols, SET_VALUE=tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT)).(0), SET_LIST_SELECT=0
      END
      'qto_lstAlt': BEGIN;new alt selected?
        WIDGET_CONTROL, lstCols, SET_VALUE=tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT)).(WIDGET_INFO(lstAlt,/DROPLIST_SELECT)), SET_LIST_SELECT=0
      END
      'qto_add':BEGIN
        IF qto_currTemp NE 0 THEN BEGIN
          testno=WIDGET_INFO(lstTest, /DROPLIST_SELECT)
          WIDGET_CONTROL, txtDescr, GET_VALUE=testDescr
          testDescr=STRUPCASE(IDL_VALIDNAME(testDescr, /CONVERT_ALL))

          RESTORE, thisPath+'data\config.dat'
          currStruct=quickTout.(qto_currMod).(qto_currTemp).(testno)
          proceed=1
          IF SIZE(currStruct, /TNAME) EQ 'STRUCT' THEN BEGIN
            currNames=TAG_NAMES(currStruct)
            IF currNames.HasValue(testDescr) THEN BEGIN
              sv=DIALOG_MESSAGE('Test-description need to be unique. Could not add to template.', DIALOG_PARENT=event.top)
              proceed=0
            ENDIF
          ENDIF
          
          IF proceed THEN BEGIN

              newStruct=CREATE_STRUCT('ALT',WIDGET_INFO(lstAlt, /DROPLIST_SELECT),'COLUMNS',WIDGET_INFO(lstCols, /LIST_SELECT),'CALC',WIDGET_INFO(lstCalc, /DROPLIST_SELECT),'PER_SERIES',WIDGET_INFO(lstPer, /DROPLIST_SELECT))
              IF SIZE(currStruct, /TNAME) EQ 'STRUCT' THEN tempstructTest=CREATE_STRUCT(currStruct, testDescr, newStruct) ELSE tempstructTest=CREATE_STRUCT(testDescr, newStruct)
              quickToutTemp=replaceStructStruct(quickTout.(qto_currMod).(qto_currTemp), tempstructTest, testno)
              quickToutMod=replaceStructStruct(quickTout.(qto_currMod), quickToutTemp, qto_currTemp)
              quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)
  
              SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
              WIDGET_CONTROL, lstTemplates, SET_VALUE=TAG_NAMES(quickTout.(qto_currMod)), SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150
              qto_fillTable, 1
           ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be changed. Duplicate to make changes.', DIALOG_PARENT=event.top)
      END
      'qto_overwrite':BEGIN
        IF qto_currTemp NE 0 THEN BEGIN

          RESTORE, thisPath+'data\config.dat'

          WIDGET_CONTROL, txtDescr, GET_VALUE=testDescr
          testDescr=STRUPCASE(IDL_VALIDNAME(testDescr, /CONVERT_ALL))
          proceed=1
          IF SIZE(quickTout.(qto_currMod).(qto_currTemp).(qto_currTest),/TNAME) EQ 'STRUCT' THEN BEGIN
            proceed=2
            currNames=TAG_NAMES(quickTout.(qto_currMod).(qto_currTemp).(qto_currTest))
            IF currNames.HasValue(testDescr) AND testDescr NE currNames(qto_currOutp) THEN BEGIN
              sv=DIALOG_MESSAGE('Test-description need to be unique. Could not overwrite template.', DIALOG_PARENT=event.top)
              proceed=0
            ENDIF
          ENDIF
          
          IF proceed GE 1 THEN BEGIN

            newStruct=CREATE_STRUCT('ALT',WIDGET_INFO(lstAlt, /DROPLIST_SELECT),'COLUMNS',WIDGET_INFO(lstCols, /LIST_SELECT),'CALC',WIDGET_INFO(lstCalc, /DROPLIST_SELECT),'PER_SERIES',WIDGET_INFO(lstPer, /DROPLIST_SELECT))
            IF proceed EQ 2 THEN BEGIN
              currTest=quickTout.(qto_currMod).(qto_currTemp).(qto_currTest)
              quickToutTempTest=replaceStructStruct(currTest, newStruct, qto_currOutp, NEW_TAG_NAME=testDescr)
            ENDIF ELSE BEGIN
              quickToutTempTest=CREATE_STRUCT(testDescr, newStruct)
            ENDELSE
            quickToutTemp=replaceStructStruct(quickTout.(qto_currMod).(qto_currTemp), quickToutTempTest, qto_currTest)
            quickToutMod=replaceStructStruct(quickTout.(qto_currMod), quickToutTemp, qto_currTemp)
            quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

            SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
            qto_fillTable, 0
          ENDIF

        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be changed. Duplicate to make changes.', DIALOG_PARENT=event.top)
      END
      'qto_deleteOutp':BEGIN
        IF qto_currTemp NE 0 THEN BEGIN

          RESTORE, thisPath+'data\config.dat'

          nActOut=N_TAGS(quickTout.(qto_currMod).(qto_currTemp).(qto_currTest))
          IF nActOut NE 1 THEN BEGIN;keep at least one

            currTest=quickTout.(qto_currMod).(qto_currTemp).(qto_currTest)
            quickToutTempTest=removeIDstructstruct(currTest, qto_currOutp)
            quickToutTemp=replaceStructStruct(quickTout.(qto_currMod).(qto_currTemp), quickToutTempTest, qto_currTest)
            quickToutMod=replaceStructStruct(quickTout.(qto_currMod), quickToutTemp, qto_currTemp)
            quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

            SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
            qto_fillTable, 1

          ENDIF ELSE sv=DIALOG_MESSAGE('At least one output for each test has to be kept.', DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be changed. Duplicate to make changes.', DIALOG_PARENT=event.top)
      END
      ELSE:
    ENDCASE
  ENDIF

  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' THEN BEGIN

    sel=WIDGET_INFO(tblQTout,/TABLE_SELECT);left,top,right,btm
    sel=sel(1);top - first row selected
    IF sel NE qto_currSel THEN BEGIN
      RESTORE, thisPath+'data\config.dat'
      ;find testNmb and outputNmb
      currTemp=quickTout.(qto_currMod).(qto_currTemp)
      nTests=N_TAGS(currTemp)
      outputs=!Null
      FOR i=0, nTests-1 DO BEGIN
        IF SIZE(currTemp.(i), /TNAME) EQ 'STRUCT' THEN outputs=[outputs, INTARR(N_TAGS(currTemp.(i)))+i] ELSE outputs=[outputs, i]
      ENDFOR
      IF sel LT N_ELEMENTS(outputs) THEN BEGIN
        qto_currTest=outputs(sel)
        actOutputs=WHERE(outputs EQ qto_currTest, nActOut)
        outputs=outputs*0
        outputs(actOutputs)=INDGEN(nActOut)
        qto_currOutp=outputs(sel)
        qto_currSel=sel
        
        qto_UpdSelections
      ENDIF
    ENDIF

  ENDIF


end

pro qto_fillTable, zero
  COMMON QTSETUP
  COMMON VARI
  COMPILE_OPT hidden

  RESTORE, thisPath+'data\config.dat'
  tempstruct=quickTout.(qto_currMod).(qto_currTemp)

  WIDGET_CONTROL, tblQTout, SET_VALUE=STRARR(6,100)

  nTests=N_TAGS(tempstruct)
  testNames=TAG_NAMES(tempstruct)
  calcStrings=['=','Min','Max','Avg','Std','Max abs']
  co=0
  FOR i=0,nTests-1 DO BEGIN; for each defined test
    IF SIZE(tempstruct.(i), /TNAME) NE 'STRUCT' THEN BEGIN
      altString='1'
      colString='All'
      calcString='='
      prString='Per image'
      descr=''
      WIDGET_CONTROL, tblQTout, SET_VALUE=[testNames(i),altString,colString,calcString,prString,descr], USE_TABLE_SELECT=[0,co,5,co]
      co=co+1
    ENDIF ELSE BEGIN
      nTouts=N_TAGS(tempstruct.(i))
      namesTouts=TAG_NAMES(tempstruct.(i))
      FOR j=0, nTouts-1 DO BEGIN; for each defined output
        testName=testNames(i)
        altString=STRING(tempstruct.(i).(j).ALT+1,FORMAT='(i0)')
        colString=STRJOIN(STRING(tempstruct.(i).(j).COLUMNS,FORMAT='(i0)'),',')
        calcString=calcStrings(tempstruct.(i).(j).CALC)
        If tempstruct.(i).(j).PER_SERIES THEN prString='Per series' ELSE prString='Per image'
        descr=namesTouts(j)
        WIDGET_CONTROL, tblQTout, SET_VALUE=[testName,altString,colString,calcString,prString,descr], USE_TABLE_SELECT=[0,co,5,co]
        co=co+1
      ENDFOR
    ENDELSE
  ENDFOR

  IF zero THEN BEGIN
    qto_currTest=0
    qto_currOutp=0
    qto_currSel=0
  ENDIF
  WIDGET_CONTROL, tblQTout, SET_TABLE_SELECT=[0,qto_currSel,0,qto_currSel]
  qto_UpdSelections
end

pro qto_UpdSelections
  COMMON QTSETUP
  COMMON VARI
  COMPILE_OPT hidden
  
  WIDGET_CONTROL, tblQTout, GET_VALUE=tabRow, USE_TABLE_SELECT=[0,qto_currSel,5,qto_currSel]
  
  WIDGET_CONTROL, lstTest, SET_DROPLIST_SELECT=qto_currTest
  WIDGET_CONTROL, lstAlt, SET_VALUE=TAG_NAMES(tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT))), SET_DROPLIST_SELECT=LONG(tabRow(1))-1
  colStrings=tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT)).(WIDGET_INFO(lstAlt,/DROPLIST_SELECT))
  IF tabRow(2) EQ 'All' THEN cols=INDGEN(N_ELEMENTS(colStrings)) ELSE cols=LONG(STRSPLIT(tabRow(2),',',/EXTRACT))
  WIDGET_CONTROL, lstCols, SET_VALUE=colStrings, SET_LIST_SELECT=cols
  WIDGET_CONTROL, lstCalc, GET_VALUE=calStrings
  WIDGET_CONTROL, lstCalc, SET_DROPLIST_SELECT=WHERE(tabRow(3) EQ calStrings)
  IF tabRow(4) EQ 'Per image' THEN WIDGET_CONTROL, lstPer, SET_DROPLIST_SELECT=0 ELSE WIDGET_CONTROL, lstPer, SET_DROPLIST_SELECT=1
  WIDGET_CONTROL, txtDescr, SET_VALUE=tabRow(5)
  
end