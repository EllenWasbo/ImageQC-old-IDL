;ImageQC - quality control of medical images
;Copyright (C) 2017  Ellen Wasbo, Stavanger University Hospital, Norway
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

pro manageQT, GROUP_LEADER = mainbase, xoff, yoff

  COMMON MQT, thisPa, QTnames, lstTemp, lstQT, lstAuto, xoff_qt, yoff_qt
  COMMON VARI
  COMPILE_OPT hidden

  thisPa=FILE_DIRNAME(ROUTINE_FILEPATH('manageQT'))+'\'
  RESTORE, thisPa+'data\config.dat'

  QTnames=TAG_NAMES(quickTemp)
  xoff_qt=xoff
  yoff_qt=yoff

  QTbox = WIDGET_BASE(TITLE='Edit/manage QuickTest templates ',  $
    /COLUMN, XSIZE=700, YSIZE=450, XOFFSET=xoff, YOFFSET=yoff,GROUP_LEADER=mainbase, /MODAL)

  ml1=WIDGET_LABEL(QTbox, VALUE='', YSIZE=10)

  bAll=WIDGET_BASE(QTbox,/ROW)
  bLft=WIDGET_BASE(bAll, /COLUMN, XSIZE=200)
  bMlLftRgt=WIDGET_LABEL(bAll, VALUE='', XSIZE=10)
  bMid=WIDGET_BASE(bAll, /COLUMN, XSIZE=170)
  bRgt=WIDGET_BASE(bAll, /COLUMN, XSIZE=300)

  ;list of templates
  lblTopLft=WIDGET_LABEL(bLft, VALUE='QuickTest  templates ', /ALIGN_LEFT, FONT=font0)

  lstTemp=WIDGET_LIST(bLft, VALUE='', XSIZE=230, YSIZE=N_ELEMENTS(QTnames), SCR_YSIZE=300, UVALUE='qt_listTemp', FONT=font1)
  bBtnsLft=WIDGET_BASE(bLft, /ROW)
  btnDelete=WIDGET_BUTTON(bBtnsLft, VALUE='delete.bmp',/BITMAP, TOOLTIP='Delete selected template', UVALUE='qt_delete')
  btnUpTemp=WIDGET_BUTTON(bBtnsLft, VALUE=thisPa+'images\switch_up.bmp',/BITMAP, UVALUE='qt_upTemp', TOOLTIP='Move template upwards in list')
  btnDownTemp=WIDGET_BUTTON(bBtnsLft, VALUE=thisPa+'images\switch_down.bmp',/BITMAP, UVALUE='qt_downTemp', TOOLTIP='Move template downwards in list')
  btnRename=WIDGET_BUTTON(bBtnsLft, VALUE='Rename', FONT=font1, UVALUE='qt_rename')

  bQTlist=WIDGET_BASE(bMid, /COLUMN)
  lblQT=WIDGET_LABEL(bQTlist, VALUE='QuickTest template ', FONT=font0)
  lstQT=WIDGET_LIST(bQTlist, XSIZE=70, SCR_XSIZE=100, YSIZE=1, SCR_YSIZE=300, FONT=font1, UVALUE='qtList');, /CONTEXT_EVENTS)
  
  bAutoList=WIDGET_BASE(bRgt, /COLUMN)
  lblAuto=WIDGET_LABEL(bAutoList, VALUE='Selected template used in automation templates: ', FONT=font0)
  lstAuto=WIDGET_LIsT(bAutoList, XSIZE=70, SCR_XSIZE=150, YSIZE=1, SCR_YSIZE=200, FONT=font1)

  ml2=WIDGET_LABEL(QTbox, VALUE='', YSIZE=20)

  bButtons=WIDGET_BASE(QTbox, /ROW)
  lblBtns0=WIDGET_LABEL(bButtons, VALUE='', XSIZE=600)
  btnCancelSett=WIDGET_BUTTON(bButtons, VALUE='Close', UVALUE='qt_cancel', FONT=font1)

  mqt_upd, 0

  WIDGET_CONTROL, QTbox, /REALIZE
  XMANAGER, 'manageQT', QTbox

end

pro manageQT_event, event

  COMMON MQT
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'qt_listTemp': mqt_upd, WIDGET_INFO(lstTemp, /LIST_SELECT)
      'qt_delete':BEGIN
        RESTORE, thisPa+'data\config.dat'

        sel=WIDGET_INFO(lstTemp, /LIST_SELECT)
        curQT=quickTemp
        QTnames=TAG_NAMES(curQT)
        
        IF N_ELEMENTS(QTnames) EQ 1 THEN BEGIN
          quickTemp=!Null
        ENDIF ELSE BEGIN
          QTnameDel=QTnames(sel)

          ;in loadTemp - warning used in automation template
          QTinUse=0
          IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
            IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
              FOR i = 0, N_ELEMENTS(loadTemp)-1 DO BEGIN
                IF STRUPCASE(loadTemp.(i).QUICKTEMP) EQ STRUPCASE(QTnameDel) THEN QTinUse=QTinUse+1
              ENDFOR
            ENDIF
          ENDIF
          proceed=1
          IF QTinUse THEN BEGIN
            sv=DIALOG_MESSAGE('Delete this QuickTest template used in '+STRING(QTinUse, FORMAT='(i0)')+' automation templates?', /QUESTION)
            IF sv EQ 'No' THEN proceed=0
          ENDIF

          IF proceed THEN BEGIN
            ss=INTARR(N_ELEMENTS(QTnames))+1
            ss(sel)=0
            remain=WHERE(ss EQ 1)
            updQT=!Null
            FOR ii=0, N_ELEMENTS(QTnames)-1 DO BEGIN
              IF remain.HasValue(ii) THEN BEGIN
                updQT=CREATE_STRUCT(updQT, QTnames(ii), curQT.(ii))
              ENDIF
            ENDFOR
            quickTemp=updQT
          ENDIF
          QTnames=TAG_NAMES(quickTemp)
        ENDELSE

        SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
        IF N_ELEMENTS(quickTemp) GT 0 THEN mqt_upd, 0 ELSE sv=DIALOG_MESSAGE('QuickTest manager closing as there are no templates left to manage.', DIALOG_PARENT=event.Top)

      END

      'qt_upTemp':BEGIN
        currSel=WIDGET_INFO(lstTemp, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(QTnames))
          newOrder=oldOrder
          newOrder[currSel-1:currSel]=REVERSE(oldOrder[currSel-1:currSel])
          RESTORE, thisPa+'data\config.dat'
          quickTemp=reorderStructStruct(quickTemp, newOrder)
          SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
          QTnames=TAG_NAMES(quickTemp)
          WIDGET_CONTROL, lstTemp, SET_VALUE=QTnames, SET_LIST_SELECT=currSel-1
          mqt_upd, currSel-1
        ENDIF
      END
      'qt_downTemp':BEGIN
        currSel=WIDGET_INFO(lstTemp, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(QTnames)-1 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(QTnames))
          newOrder=oldOrder
          newOrder[currSel:currSel+1]=REVERSE(oldOrder[currSel:currSel+1])
          RESTORE, thisPa+'data\config.dat'
          quickTemp=reorderStructStruct(quickTemp, newOrder)
          SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
          QTnames=TAG_NAMES(quickTemp)
          WIDGET_CONTROL, lstTemp, SET_VALUE=QTnames, SET_LIST_SELECT=currSel+1
          mqt_upd, currSel+1
        ENDIF
      END
      'qt_rename':BEGIN
        currSel=WIDGET_INFO(lstTemp, /LIST_SELECT)
        QTnameThis=QTnames(currSel)
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, '+QTnameThis+', LABEL_LEFT=New name:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Rename',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename template', XSIZE=300, YSIZE=100, FOCUSNO=0, XOFFSET=xoff_qt+100, YOFFSET=yoff_qt+100)

        IF ~res.Cancel AND res.newname NE '' THEN BEGIN
          tempname=IDL_VALIDNAME(res.newname, /CONVERT_ALL)
          IF QTnames.HasValue(tempname) THEN BEGIN
            sv=DIALOG_MESSAGE('Template name '+tempname+' already exist. Renaming not possible.', DIALOG_PARENT=event.Top)
          ENDIF ELSE BEGIN
            RESTORE, thisPa+'data\config.dat'

            ;replace quickTemp
            quickTemp=replaceStructStruct(quickTemp,quickTemp.(currSel), currSel, NEW_TAG_NAME=tempname)

            ;if exist in loadTemp - then change also loadTemp
            IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
              IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN          
                FOR i = 0, N_TAGS(loadTemp)-1 DO BEGIN
                  IF STRUPCASE(loadTemp.(i).QUICKTEMP) EQ STRUPCASE(QTnameThis) THEN BEGIN
                      loadTemp.(i).QUICKTEMP=tempname
                  ENDIF
                ENDFOR
              ENDIF
            ENDIF
            
            SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
            mqt_upd, currSel
          ENDELSE
        ENDIF

      END
      'qt_cancel': WIDGET_CONTROL, Event.top, /DESTROY
      ELSE:
    ENDCASE
  ENDIF

end

pro mqt_upd, listNmb
  COMMON MQT
  COMMON VARI
  COMPILE_OPT hidden

  RESTORE, thisPath+'data\config.dat'
  QTnames=TAG_NAMES(quickTemp)
  markArr=quickTemp.(listNmb)
  If markArr(0) NE -1 THEN BEGIN
    szMM=SIZE(markArr, /DIMENSIONS)
    IF N_ELEMENTS(szMM) EQ 1 THEN szMM=[szMM,1]
    strQTarr=STRARR(szMM(1))
    FOR i=0, szMM(1)-1 DO BEGIN
      FOR j=0, szMM(0)-1 DO BEGIN
        IF markArr[j,i] THEN strQTarr(i)=strQTarr(i)+STRING(j+1, FORMAT='(i0)') ELSE strQTarr(i)=strQTarr(i)+'- '
      ENDFOR
    ENDFOR
    WIDGET_CONTROL, lstQT, SET_VALUE=strQTarr
  ENDIF

  WIDGET_CONTROL, lstTemp, SET_VALUE=QTnames, SET_LIST_SELECT=listNmb
  
  autoNames=!Null
  IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
    IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
      autoAll=TAG_NAMES(loadTemp)
      FOR i = 0, N_ELEMENTS(autoAll)-1 DO BEGIN
        IF STRUPCASE(loadTemp.(i).QUICKTEMP) EQ QTnames(listNmb) THEN autoNames=[autonames, autoAll(i)]
      ENDFOR
    ENDIF
  ENDIF
  IF N_ELEMENTS(autoNames) GT 0 THEN WIDGET_CONTROL, lstAuto, SET_VALUe=autoNames ELSE WIDGET_CONTROL, lstAuto, SET_VALUe=''

end