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

pro settings, GROUP_LEADER = mainbase, xoff, yoff

  COMMON SETT, listSets, thisPa, txtDefPath, lstCT, lstX, lstNM, btnUseCurr
  COMMON VARI
  COMPILE_OPT hidden
  
  settingsbox = WIDGET_BASE(TITLE='Edit/manage parameter sets ',  $
    /COLUMN, XSIZE=700, YSIZE=450, XOFFSET=xoff, YOFFSET=yoff,GROUP_LEADER=mainbase, /MODAL)

  ml1=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=10)

  bAll=WIDGET_BASE(settingsbox,/ROW)
  bLft=WIDGET_BASE(bAll, /COLUMN, XSIZE=200)
  bMlLftRgt=WIDGET_LABEL(bAll, VALUE='', XSIZE=10)
  bRgt=WIDGET_BASE(bAll, /COLUMN, XSIZE=500)
  
  ;list of parameter sets
  lblTopLft=WIDGET_LABEL(bLft, VALUE='Parameter sets ', /ALIGN_LEFT, FONT=font0)
  thisPa=FILE_DIRNAME(ROUTINE_FILEPATH('settings'))+'\'
  listSets=WIDGET_LIST(bLft, VALUE='', XSIZE=230, YSIZE=N_ELEMENTS(setNames), SCR_YSIZE=160, UVALUE='s_listSets', FONT=font1)
  bBtnsLft=WIDGET_BASE(bLft, /ROW)
  btnSetDef=WIDGET_BUTTON(bBtnsLft, VALUE='Set default', TOOLTIP='Let this parameter set be selected when opening ImageQC', UVALUE='s_setDefault',FONT=font1)
  btnDelete=WIDGET_BUTTON(bBtnsLft, VALUE='delete.bmp',/BITMAP, TOOLTIP='Delete selected parameter set', UVALUE='s_delete')

  ;select parameterset values
  lblTopLfRgt=WIDGET_LABEL(bRgt, VALUE='Selected parameter set ', /ALIGN_LEFT, FONT=font0)
  
  mlR0=WIDGET_LABEL(bRgt, VALUE='', YSIZE=10)
  bBtnsRow=WIDGET_BASE(bRgt, /ROW)
  btnDump=WIDGET_BUTTON(bBtnsRow, VALUE='See dump of saved parameters', TOOLTIP='See dump of all parameters in this set or current values if <use current> selected', UVALUE='s_dump', FONT=font1)
  bUseCurr=WIDGET_BASE(bRgt, /NONEXCLUSIVE)
  btnUseCurr=WIDGET_BUTTON(bUseCurr, VALUE='Get parameters currently in main window', UVALUE='s_useCurr', FONT=font1)
  
  mlR1=WIDGET_LABEL(bRgt, VALUE='', YSIZE=10)
  bDefPath=WIDGET_BASE(bRgt, /ROW)
  lblDefPath=WIDGET_LABEL(bDefPath, VALUE='Default path:', /ALIGN_LEFT, FONT=font1)
  txtDefPath=WIDGET_TEXT(bDefPath, VALUE='', XSIZE=150, SCR_XSIZE=300, FONT=font1)
  btnBrowseDefPath=WIDGET_BUTTON(bDefPath, VALUE='Browse', UVALUE='s_browse', FONT=font1)
  
  mlR2=WIDGET_LABEL(bRgt, VALUE='', YSIZE=10)
  bTempHead=WIDGET_BASE(bRgt, /ROW)
  lblExpTemp=WIDGET_LABEL(bTempHead, VALUE='Output templates used with QuickTest', /ALIGN_LEFT, FONT=font1)
  btnQToutSettings=WIDGET_BUTTON(bTempHead, VALUE=thisPath+'images\gears.bmp',/BITMAP, UVALUE='manageQTout', TOOLTIP='Open templatemanager QuickTest output')
  RESTORE, thisPath+'data\config.dat'
  bTempCT=WIDGET_BASE(bRgt, /ROW)
  lblCT=WIDGET_LABEL(bTempCT, VALUE='CT', FONT=font1, XSIZE=100)
  lstCT=WIDGET_DROPLIST(bTempCT, VALUE=TAG_NAMES(quickTout.(0)), FONT=font1, XSIZE=100)
  bTempX=WIDGET_BASE(bRgt, /ROW)
  lblX=WIDGET_LABEL(bTempX, VALUE='Xray', FONT=font1, XSIZE=100)
  lstX=WIDGET_DROPLIST(bTempX, VALUE=TAG_NAMES(quickTout.(1)), FONT=font1, XSIZE=100)
  bTempNM=WIDGET_BASE(bRgt, /ROW)
  lblNM=WIDGET_LABEL(bTempNM, VALUE='NM planar', FONT=font1, XSIZE=100)
  lstNM=WIDGET_DROPLIST(bTempNM, VALUE=TAG_NAMES(quickTout.(2)), FONT=font1, XSIZE=100)
  bTempSPECT=WIDGET_BASE(bRgt, /ROW)
  lblSPECT=WIDGET_LABEL(bTempSPECT, VALUE='SPECT', FONT=font1, XSIZE=100)
  lblSPECTNA=WIDGET_LABEL(bTempSPECT, VALUE='NA', FONT=font1, XSIZE=50)
  bTempPET=WIDGET_BASE(bRgt, /ROW)
  lblPET=WIDGET_LABEL(bTempPET, VALUE='PET', FONT=font1, XSIZE=100)
  lblPETNA=WIDGET_LABEL(bTempPET, VALUE='NA', FONT=font1, XSIZE=50)
  
  mlR3=WIDGET_LABEL(bRgt, VALUE='', YSIZE=10)
  bBtnsSave=WIDGET_BASE(bRgt, /ROW)
  btnOverwrite=WIDGET_BUTTON(bBtnsSave, VALUE='Overwrite', TOOLTIP='Overwrite selected parameterset with these settings', UVALUE='s_overwrite', FONT=font1)
  btnSaveas=WIDGET_BUTTON(bBtnsSave, VALUE='Save as...', UVALUE='s_saveas', FONT=FONT1)

  ml2=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=20)

  bButtons=WIDGET_BASE(settingsbox, /ROW)
  lblBtns0=WIDGET_LABEL(bButtons, VALUE='', XSIZE=500)
  btnSetCurr=WIDGET_BUTTON(bButtons, VALUE='Use selected / Close', UVALUE='s_setCurr',FONT=font1)
  btnCancelSett=WIDGET_BUTTON(bButtons, VALUE='Cancel', UVALUE='s_cancel', FONT=font1)
  
  s_upd, selConfig-1

  WIDGET_CONTROL, settingsbox, /REALIZE
  XMANAGER, 'settings', settingsbox

end

pro settings_event, event

  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      's_setDefault':BEGIN
        RESTORE, thisPath+'data\config.dat'
        setNames=TAG_NAMES(configS)
        configS.(0)=WIDGET_INFO(listSets, /LIST_SELECT)+1
        SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPa+'data\config.dat'
        END

      's_delete':BEGIN
        RESTORE, thisPath+'data\config.dat'
        setNames=TAG_NAMES(configS)
        setNames=setNames[1:-1]
        IF N_ELEMENTS(setNames) EQ 1 THEN sv=DIALOG_MESSAGE('At least one parameter set have to be kept.', DIALOG_PARENT=event.TOP) ELSE BEGIN
          selSet=WIDGET_INFO(listSets, /LIST_SELECT)
          configS=removeIDstructstruct(configS, selSet+1)
          IF selSet+1 LT configS.(0) THEN configS.(0)=configS.(0)-1
          IF selSet+1 EQ configS.(0) THEN configS.(0)=1
          selConfig=configS.(0)
          SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPa+'data\config.dat'
          setNames=TAG_NAMES(configS)
          refreshParam, configS.(selConfig), setNames(selConfig)
          s_upd, 0
        ENDELSE
        END
      's_dump':BEGIN
        RESTORE, thisPath+'data\config.dat'
        selParams=configS.(WIDGET_INFO(listSets, /LIST_SELECT)+1)
        paramNames=TAG_NAMES(selParams)
        OPENW, outunit, thisPath+'data\dumpTemp.txt', /GET_LUN
        FOR o=0, N_TAGS(selParams)-1 DO PRINTF, outunit, paramNames(o), '  ', selParams.(o)
        FREE_LUN, outunit
        XDISPLAYFILE, thisPath+'data\dumpTemp.txt', TITLE='Dump of parameters', /MODAL
          END
      's_useCurr':BEGIN
        END
      
      's_listSets': BEGIN
        RESTORE, thisPath+'data\config.dat'
        s_upd, WIDGET_INFO(listSets, /LIST_SELECT)

      END
      's_browse': BEGIN
        newdef=DIALOG_PICKFILE(PATH=defPath, /DIRECTORY, DIALOG_PARENT=event.TOP)
        IF newdef(0) NE '' THEN WIDGET_CONTROL, txtDefPath, SET_VALUE=newdef(0)
      END
      's_overwrite':BEGIN
          sv=DIALOG_MESSAGE('Are you sure you want to overwrite the selected parameterset?', /QUESTION)
          IF sv EQ 'Yes' THEN BEGIN
          
          selSet=WIDGET_INFO(listSets, /LIST_SELECT)
          
          IF WIDGET_INFO(btnUseCurr, /BUTTON_SET) THEN saveParam, selSet+1,''
          RESTORE, thisPath+'data\config.dat'
          WIDGET_CONTROL, txtDefPath, GET_VALUE=newdef
          IF newdef(0) NE '' THEN configS.(selSet+1).defPath=newdef(0)
          namesCT=TAG_NAMES(quickTout.(0))
          namesX=TAG_NAMES(quickTout.(1))
          namesNM=TAG_NAMES(quickTout.(2))
          configS.(selSet+1).qtOutTemps=[namesCT(WIDGET_INFO(lstCT, /DROPLIST_SELECT)),namesX(WIDGET_INFO(lstX, /DROPLIST_SELECT)),namesNM(WIDGET_INFO(lstNM, /DROPLIST_SELECT)),'DEFAULT','DEFAULT']
          SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPa+'data\config.dat'
                
          s_upd, selSet
          ENDIF
      END
      
      's_saveas':BEGIN

        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Name the new parameter set:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Save as...', XSIZE=300, YSIZE=100, FOCUSNO=0, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF ~res.Cancel THEN BEGIN
          IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not save.', DIALOG_PARENT=event.top) ELSE BEGIN

            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            RESTORE, thisPath+'data\config.dat'
            tempnames=TAG_NAMES(configS)
            IF tempnames.HasValue(tempname) THEN sv=DIALOG_MESSAGE('Name already in use. Could not save.', DIALOG_PARENT=event.top) ELSE BEGIN
            
              IF WIDGET_INFO(btnUseCurr, /BUTTON_SET) THEN BEGIN
                saveParam, -1, tempname
                RESTORE, thisPath+'data\config.dat'
              ENDIF ELSE BEGIN
                configS=CREATE_STRUCT(configS, tempname, configS.(WIDGET_INFO(listSets, /LIST_SELECT)+1))
                SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
              ENDELSE
              
              ;saveNmb?       
              saveNmb=WHERE(TAG_NAMES(configS) EQ tempname)
              WIDGET_CONTROL, txtDefPath, GET_VALUE=newdef
              IF newdef(0) NE '' THEN configS.(saveNmb).defPath=newdef(0) ELSE configS.(saveNmb).defPath='C:\'
              WIDGET_CONTROL, lstCT, GET_VALUE=strCT
              WIDGET_CONTROL, lstX, GET_VALUE=strX
              WIDGET_CONTROL, lstNM, GET_VALUE=strNM
              configS.(saveNmb).qtOutTemps=[strCT(WIDGET_INFO(lstCT, /DROPLIST_SELECT)),strX(WIDGET_INFO(lstX, /DROPLIST_SELECT)),strNM(WIDGET_INFO(lstNM, /DROPLIST_SELECT)),'DEFAULT','DEFAULT']
              SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPa+'data\config.dat'
              s_upd, saveNmb-1
            ENDELSE
          ENDELSE
        ENDIF
      END
      'manageQTout':BEGIN
        sv=DIALOG_MESSAGE('Close the parameter set manager and open the QuickTest output manager?', DIALOG_PARENT=event.top, /QUESTION)
        IF sv EQ 'Yes' THEN BEGIN
          WIDGET_CONTROL, Event.top, /DESTROY
          QTexportSetup, GROUP_LEADER = evTop, xoffset+100, yoffset+100
        ENDIF
        END
      's_setCurr':BEGIN
        selConfig=WIDGET_INFO(listSets, /LIST_SELECT)+1
        RESTORE, thisPath+'data\config.dat'
        setNames=TAG_NAMES(configS)
        refreshParam, configS.(selConfig), setNames(selConfig)
        WIDGET_CONTROL, Event.top, /DESTROY
      END
      's_cancel': WIDGET_CONTROL, Event.top, /DESTROY
      ELSE:
    ENDCASE
  ENDIF

end

pro s_upd, listNmb
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden

  RESTORE, thisPath+'data\config.dat'
  WIDGET_CONTROL, txtDefPath, SET_VALUE=configS.(listNmb+1).defPath
  WIDGET_CONTROL, btnUseCurr, SET_BUTTON=0
  tempNamesSel=configS.(listNmb+1).qtOutTemps
  nmbs=INTARR(5)
  errMsg=''
  FOR i=0, N_TAGS(testVisualQTNames)-1 DO BEGIN
    tempNames=TAG_NAMES(quickTout.(i))
    ;IF tempNamesSel(i) EQ '' THEN tempNamesSel(i)='DEFAULT'
    nmb=WHERE(tempNames EQ tempNamesSel(i))
    IF nmb NE -1 THEN nmbs(i)=nmb ELSE errMsg= tempNamesSel(i) + ' '+newline
  ENDFOR
  IF errMsg NE '' THEN sv=DIALOG_MESSAGE('Output-templates used for the selected parameter set are missing. Default is used. These are missing:'+newline)
  WIDGET_CONTROL, lstCT, SET_DROPLIST_SELECT=nmbs(0)
  WIDGET_CONTROL, lstX, SET_DROPLIST_SELECT=nmbs(1)
  WIDGET_CONTROL, lstNM, SET_DROPLIST_SELECT=nmbs(2)
  setNames=TAG_NAMES(configS)
  setNames(configS.(0))=setNames(configS.(0))+' (default)'
  setNames=setNames[1:-1]
  WIDGET_CONTROL, listSets, SET_VALUE=setNAmes, SET_LIST_SELECT=listNmb
end