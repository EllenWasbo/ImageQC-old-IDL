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

  COMMON SETT, listSets, cw_action, txtName, thisPa, txtDefPath
  COMMON VARI
  COMPILE_OPT hidden

  settingsbox = WIDGET_BASE(TITLE='Edit/manage parameter sets for default settings', GROUP_LEADER=mainbase,  $
    /COLUMN, XSIZE=500, YSIZE=440, XOFFSET=xoff, YOFFSET=yoff, /MODAL)

  ml1=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=10)

  ;save new
  lblAdd=WIDGET_LABEL(settingsbox, VALUE='Save current settings to parameter set', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  bAdd=WIDGET_BASE(settingsbox, /ROW)
  btnSave=WIDGET_BUTTON(bAdd, VALUE='save.bmp', /BITMAP, UVALUE='s_saveNew')
  mla1=WIDGET_LABEL(bAdd, VALUE='', XSIZE=20)
  lblName=WIDGET_LABEL(bAdd, VALUE='New parameter set name:')
  txtName=WIDGET_TEXT(bAdd, VALUE='', /EDITABLE, XSIZE=15)
  bAddComment=WIDGET_BASE(settingsbox, /ROW)
  mla1=WIDGET_LABEL(bAddComment, VALUE='', XSIZE=200)
  lblComment=WIDGET_LABEL(bAddComment, VALUE='(leave blank to overwrite selected parameter set)')

  ml2=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=40)

  bTop=WIDGET_BASE(settingsbox, /ROW)
  bTopLft=WIDGET_BASE(bTop, XSIZE=250,/COLUMN)

  ;list of parameter sets
  lblTop=WIDGET_LABEL(bTopLft, VALUE='Parameter sets', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  thisPa=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  RESTORE, thisPa+'data\config.dat'
  setNames=TAG_NAMES(configS)
  setNames(configS.(0))=setNames(configS.(0))+' (default)'
  setNames=setNames[1:-1]
  listSets=WIDGET_LIST(bTopLft, VALUE=setNames, XSIZE=230, YSIZE=N_ELEMENTS(setNames), SCR_YSIZE=160, UVALUE='s_listSets')
  WIDGET_CONTROL, listSets, SET_LIST_SELECT=selConfig-1

  ;actions on list
  bTopRgt=WIDGET_BASE(bTop, XSIZE=230, /COLUMN)
  cw_action=CW_BGROUP(bTopRgt, ['Set as current','Set as default','Delete'], /EXCLUSIVE, SET_VALUE=0, LABEL_TOP='For selected parameter set...',UVALUE='act')
  btnUpdate=WIDGET_BUTTON(bTopRgt, VALUE='Update and close', UVALUE='s_update', XSIZE=50)

  ml3=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=20)

  lblDefPath=WIDGET_LABEL(settingsbox, VALUE='Default path for selected parameter set:', /ALIGN_LEFT)
  bDefPath=WIDGET_BASE(settingsbox, /ROW)
  txtDefPath=WIDGET_TEXT(bDefPath, VALUE=configS.(selConfig).DefPath, XSIZE=150, SCR_XSIZE=300)
  btnBrowseDefPath=WIDGET_BUTTON(bDefPath, VALUE='Browse', UVALUE='s_browse')
  btnSaveDefPath=WIDGET_BUTTON(bDefPath, VALUE='save.bmp', /BITMAP, UVALUE='s_saveNewPath')

  bButtons=WIDGET_BASE(settingsbox, /ROW)
  lblBtns0=WIDGET_LABEL(bButtons, VALUE='', XSIZE=350)
  btnCancelSett=WIDGET_BUTTON(bButtons, VALUE='Close', UVALUE='s_cancel', XSIZE=50)

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
      's_update':BEGIN

        WIDGET_CONTROL, cw_action, GET_VALUE=selAct
        selSet=WIDGET_INFO(listSets, /LIST_SELECT)
        RESTORE, thisPath+'data\config.dat'
        setNames=TAG_NAMES(configS)

        CASE selAct OF
          0: BEGIN; set selected as current
            selConfig=selSet+1
            refreshParam, configS.(selConfig), setNames(selConfig)
          END
          1: BEGIN; set selected as default
            configS.(0)=selSet+1
            SAVE, configS, FILENAME=thisPa+'data\config.dat'
            IF selConfig NE selSet+1 THEN BEGIN
              sv=DIALOG_MESSAGE('Update parameters with the new default?', /QUESTION, DIALOG_PARENT=event.TOP)
              IF sv EQ 'Yes' THEN BEGIN
                selConfig=selSet+1
                refreshParam, configS.(selConfig), setNames(selConfig)
              ENDIF
            ENDIF
          END
          2: BEGIN; delete selected set
            setNames=setNames[1:-1]
            IF N_ELEMENTS(setNames) EQ 1 THEN sv=DIALOG_MESSAGE('At least one parameter set have to be kept.', DIALOG_PARENT=event.TOP) ELSE BEGIN
              configS=removeIDstructstruct(configS, selSet+1)
              IF selSet+1 LT configS.(0) THEN configS.(0)=configS.(0)-1
              IF selSet+1 EQ configS.(0) THEN configS.(0)=1
              selConfig=configS.(0)
              SAVE, configS, FILENAME=thisPa+'data\config.dat'
              setNames=TAG_NAMES(configS)
              refreshParam, configS.(selConfig), setNames(selConfig)
            ENDELSE
          END
          ELSE:
        ENDCASE
        WIDGET_CONTROL, Event.top, /DESTROY
      END
      's_saveNew':BEGIN
        WIDGET_CONTROL, txtName, GET_VALUE=newName
        IF newName NE '' THEN saveParam, -1, newName ELSE BEGIN
          selSet=WIDGET_INFO(listSets, /LIST_SELECT)
          saveParam, selSet+1,''
        ENDELSE

        WIDGET_CONTROL, Event.top, /DESTROY
      END
      's_cancel': WIDGET_CONTROL, Event.top, /DESTROY
      's_listSets': BEGIN
        RESTORE, thisPath+'data\config.dat'
        selSet=WIDGET_INFO(listSets, /LIST_SELECT)
        WIDGET_CONTROL, txtDefPath, SET_VALUE=configS.(selSet+1).defPath
      END
      's_browse': BEGIN
        newdef=DIALOG_PICKFILE(PATH=defPath, /DIRECTORY, DIALOG_PARENT=event.TOP)
        IF newdef(0) NE '' THEN WIDGET_CONTROL, txtDefPath, SET_VALUE=newdef(0)
      END
      's_saveNewPath':BEGIN
        WIDGET_CONTROL, txtDefPath, GET_VALUE=newdef
        IF newdef(0) NE '' THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          selSet=WIDGET_INFO(listSets, /LIST_SELECT)
          configS.(selSet+1).defPath=newdef(0)
          SAVE, configS, quickTemp, loadTemp, FILENAME=thisPath+'data\config.dat'
          IF selSet+1 EQ selConfig THEN defPath=newdef(0)
        ENDIF
      END
      ELSE:
    ENDCASE
  ENDIF

end