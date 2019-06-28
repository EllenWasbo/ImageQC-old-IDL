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

;selec = selected loadtemp template from previous dialogbox
pro autoSetup, GROUP_LEADER = mainbase, selec, xoff, yoff

  COMMON AUTO, listTemp, txtName, txtBrowse, txtStatName, listSets, listQT, listElem, listSort, btnInclSub, btnOnlyLastDate,txtBrowseApp, btnMoveFiles, btnDeleteFiles,$
    selecTemp,tempnames,paramSetNames,quickTempNames, tags_imgStruct, sortElem
  COMMON VARI
  COMPILE_OPT hidden

  selecTemp=selec

  dummyImgStruct=imgStructUpdate('','')
  tags_imgStruct=TAG_NAMES(dummyImgStruct)

  tags_imgStruct=tags_imgStruct(SORT(STRUPCASE(tags_imgStruct)))

  RESTORE, thisPath+'data\config.dat'
  paramSetNames=STRUPCASE(TAG_NAMES(configS))
  paramSetNames=paramSetNames[1:-1]
  IF N_ELEMENTS(quickTemp) NE 0 THEN BEGIN
    typ=SIZE(quickTemp, /TNAME)
    IF typ EQ 'STRUCT' THEN quickTempNames=['',STRUPCASE(TAG_NAMES(quickTemp))] ELSE quickTempNames=''
  ENDIF ELSE quickTempNames=''

  tempname=''
  path=''
  statName=''
  selParam=selConfig-1
  selQT=0; (no marking = all images all quickTests)
  selLoad=0
  sortElem=''
  pathApp=''
  archiveSucc=0
  deleteFiles=0
  IF N_ELEMENTS(loadTemp) GT 0 THEN BEGIN
    IF selec EQ -1 THEN selec=0
    tempnames=TAG_NAMES(loadTemp)
    tempname=tempnames(selec)
    path=loadTemp.(selec).path
    statName=loadTemp.(selec).statName
    ;paramSet - exists still?
    paramSetName=STRUPCASE(loadTemp.(selec).paramSet)
    IF paramSetNames.HasValue(paramSetName) THEN BEGIN
      selNo=WHERE(paramSetNames EQ paramSetName)
      selParam=selNo(0)
    ENDIF ELSE selParam=selConfig-1
    ;quickTemp - exists still?
    quickTempName=STRUPCASE(loadTemp.(selec).quickTemp)
    IF quickTempNames.HasValue(quickTempName) THEN BEGIN
      selNo=WHERE(quickTempNames EQ quickTempName)
      selQT=selNo(0)
    ENDIF ELSE selQT=0
    selLoad=loadTemp.(selec).loadBy
    sortElem=loadTemp.(selec).sortBy
    pathApp=loadTemp.(selec).pathApp
    archiveSucc=loadTemp.(selec).archive
    deleteFiles=loadTemp.(selec).deleteFiles
  ENDIf ELSE tempnames=''

  autobox = WIDGET_BASE(TITLE='Edit or add automation template', GROUP_LEADER=mainbase,  $
    /ROW, XSIZE=650, YSIZE=670, XOFFSET=xoff, YOFFSET=yoff, /MODAL)
   
  ml0=WIDGET_LABEL(autobox, VALUE='', YSIZE=20)
  
  autobox1=WIDGET_BASE(autobox, /COLUMN, XSIZE=650)

  ;templates
  lblTemp=WIDGET_LABEL(autobox1, VALUE='Automation templates:', /ALIGN_LEFT, FONT=font0)
  bTemp=WIDGET_BASE(autobox1, /ROW)
  listTemp=WIDGET_LIST(bTemp, VALUE=tempNames, XSIZE=20, FONT=font1, SCR_YSIZE=150, UVALUE='listTemp')
  IF tempnames(0) NE '' THEN WIDGET_CONTROL, listTemp, SET_LIST_SELECT=selec
  bButtEndTemp=WIDGET_BASE(bTemp, /COLUMN)
  btnDelTemp=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='a_delTemp', TOOLTIP='Delete template')
  btnUpTemp=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='a_upTemp', TOOLTIP='Move template upwards in list')
  btnDownTemp=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='a_downTemp', TOOLTIP='Move template downwards in list')
  ;name
  bName=WIDGET_BASE(bTemp, /ROW, YSIZE=25)
  lblName=WIDGET_LABEL(bName, VALUE='Save (new) as:', FONT=font1)
  txtName=WIDGET_TEXT(bName, VALUE=tempname, /EDITABLE, XSIZE=15, /KBRD_FOCUS_EVENTS, FONT=font1)
  btnsSave=WIDGET_BASE(bTemp, /COLUMN)
  btnSaveAsAuto=WIDGET_BUTTON(btnsSave, VALUE='Save new', UVALUE='a_saveas', FONT=font1)
  btnOverWriteAuto=WIDGET_BUTTON(btnsSave, VALUE='Overwrite selected', UVALUE='a_overwrite', FONT=font1)
  ml3=WIDGET_LABEL(autobox1, VALUE='', YSIZE=15)
  ;path
  bBrowse=WIDGET_BASE(autobox1, /ROW)
  lblBrowse=WIDGET_LABEL(bBrowse, VALUE='Path:', FONT=font1)
  txtBrowse=WIDGET_TEXT(bBrowse, VALUE=path, XSIZE=70,/EDITABLE, FONT=font1)
  btnBrowse=WIDGET_BUTTON(bBrowse, VALUE='Browse', UVALUE='a_Browse',  FONT=font1)
  ml4=WIDGET_LABEL(autobox1, VALUE='', YSIZE=15)
  ;stationname
  bStatName=WIDGET_BASE(autobox1, /ROW)
  lblStatName=WIDGET_LABEL(bStatName, VALUE='Station name (DICOM 0008,1010):', FONT=font1)
  txtStatName=WIDGET_TEXT(bStatName, VALUE=statname, XSIZE=20,/EDITABLE, FONT=font1)
  btnStatName=WIDGET_BUTTON(bStatName, VALUE='Retrieve from DICOM file', UVALUE='a_getStatName', FONT=font1)
  ml5=WIDGET_LABEL(autobox1, VALUE='', YSIZE=15)
  
  bOpenDetails=WIDGET_BASE(autobox1, /Row)
  bSortBy=WIDGET_BASE(bOpenDetails, XSIZE=355,/COLUMN, FRAME=1)
  lblSortBy=WIDGET_LABEL(bSortBy, VALUE='Add elements from DICOM-header to sort the images by', FONT=font1)
  bSortByLists=WIDGET_BASE(bSortBy, /ROW)
  listElem=WIDGET_DROPLIST(bSortByLists, VALUE=tags_imgStruct, XSIZE=150, FONT=font1)
  bButtMid=WIDGET_BASE(bSortByLists, /COLUMN)
  btnAddElem=WIDGET_BUTTON(bButtMid, VALUE='>>', UVALUE='a_addElem', FONT=font1)
  listSort=WIDGET_LIST(bSortByLists, VALUE=sortElem, XSIZE=20, FONT=font1, SCR_YSIZE=80)
  bButtEndSort=WIDGET_BASE(bSortByLists, /COLUMN)
  btnDelElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='a_delElem', TOOLTIP='Delete selected element from sort list')
  btnUpElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='a_upElem', TOOLTIP='Move element upwards in sort list')
  btnDownElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='a_downElem', TOOLTIP='Move element downwards in sort list')

  bBtnInclSub=WIDGET_BASE(bOpenDetails, /NONEXCLUSIVE)
  btnInclSub=WIDGET_BUTTON(bBtnInclSub, VALUE='Include images in subfolders', FONT=font1)
  btnOnlyLastDate=WIDGET_BUTTON(bBtnInclSub, VALUE='Last date images only (file creation date)', FONT=font1)

  ml60=WIDGET_LABEL(autobox1, VALUe='', YSIZE=15)
  ;parameter set
  bSets=WIDGET_BASE(autobox1, /ROW, YSIZE=25)
  lblSets=WIDGET_LABEL(bSets, VALUE='Use parameter set:', XSIZE=150, FONT=font1)
  listSets=WIDGET_DROPLIST(bSets, VALUE=paramSetNames, XSIZE=180, YSIZE=N_ELEMENTS(paramSetNames), SCR_YSIZE=160, FONT=font1)
  WIDGET_CONTROL, listSets, SET_DROPLIST_SELECT=selParam

  ml6=WIDGET_LABEL(autobox1, VALUe='', YSIZE=15)
  bInfo2=WIDGET_BASE(autobox1, /ROW)
  ml7=WIDGET_LABEL(bInfo2, VALUE='', XSIZE=5)

  bQTall=WIDGET_BASE(autobox1, /ROW)
  marg=WIDGET_LABEL(bQTall, VALUe='', XSIZE=30)
  bQT2=WIDGET_BASE(bQTall,/COLUMN)
  ;quickTemp list

  bQT=WIDGET_BASE(bQT2, /ROW)
  bQTlist=WIDGET_BASE(bQT, /ROW, YSIZE=25)
  lblQT=WIDGET_LABEL(bQTlist, VALUE='Use QuickTest template:', XSIZE=150, FONT=font1)
  listQT=WIDGET_DROPLIST(bQTlist, VALUE=[quickTempNames], XSIZE=180, YSIZE=N_ELEMENTS(quickTempNames), SCR_YSIZE=160, FONT=font1)
  WIDGET_CONTROL, listQT, SET_DROPLIST_SELECT=selQT

  ;append resultfile
  bBrowseApp=WIDGET_BASE(bQT2, /ROW)
  lblBrowseApp=WIDGET_LABEL(bBrowseApp, VALUE='Append results (row, no headers) to this file:', FONT=font1)
  txtBrowseApp=WIDGET_TEXT(bBrowseApp, VALUE=pathApp, /EDITABLE, XSIZE=30, FONT=font1)
  btnBrowseApp=WIDGET_BUTTON(bBrowseApp, VALUE='Browse', UVALUE='a_BrowseApp', FONT=font1)
  btnClearApp=WIDGET_BUTTON(bBrowseApp, VALUE='Clear', UVALUE='a_ClearApp', FONT=font1)

  bMoveFiles=WIDGET_BASE(bQT2, /NONEXCLUSIVE)
  btnMoveFiles=WIDGET_BUTTON(bMoveFiles, VALUE='Move images automatically to folder named "Archived" when finished calculation.', FONT=font1)
  WIDGET_CONTROL, btnMoveFiles, SET_BUTTON=archiveSucc
  btnDeleteFiles=WIDGET_BUTTON(bMoveFiles, VALUE='Delete files not accepted (i.e. SR reports, images with no acquisition date).', FONT=font1)
  WIDGET_CONTROL, btnDeleteFiles, SET_BUTTON=deleteFiles

  ;close
  ml3=WIDGET_LABEL(autobox1, VALUE='', YSIZE=10)
  bButtons=WIDGET_BASE(autobox1, /ROW)
  lblBtns0=WIDGET_LABEL(bButtons, VALUE='', XSIZE=450)
  btnInfoAuto=WIDGET_BUTTON(bButtons, VALUE='Info', UVALUE='a_info', FONT=font1)
  btnCancelAuto=WIDGET_BUTTON(bButtons, VALUE='Close', UVALUE='a_cancel', FONT=font1)
  btnRunAuto=WIDGET_BUTTON(bButtons, VALUE='Run selected', UVALUE='a_run', FONT=font1)

  updateAuto, selecTemp

  WIDGET_CONTROL, autobox, /REALIZE
  XMANAGER, 'autoSetup', autobox

end

pro autoInfo, GROUP_LEADER = mainAI

  autoInfo_box = WIDGET_BASE(TITLE='Info automation templates', /COLUMN, $
    XSIZE=650, YSIZE=300, XOFFSET=200, YOFFSET=200, GROUP_LEADER=mainAI, /MODAL)

  bInfo0=WIDGET_BASE(autoInfo_box ,/ROW)
  bInfo=WIDGET_BASE(bInfo0, FRAME=1, /COLUMN,XSIZE=600)
  info0=WIDGET_LABEL(bInfo, VALUE='INFO:', /ALIGN_LEFT, FONT=font0)
  info1=WIDGET_LABEL(bInfo, VALUE='Edit or add template for ', /ALIGN_LEFT, FONT=font1)
  info2=WIDGET_LABEL(bInfo, VALUE='  -opening images from specified path', /ALIGN_LEFT, FONT=font1)
  info3=WIDGET_LABEL(bInfo, VALUE='  -sort by specified rules', /ALIGN_LEFT, FONT=font1)
  info4=WIDGET_LABEL(bInfo, VALUE='  -and combine with defined parameterset and QuickTest template', /ALIGN_LEFT, FONT=font1)
  ml2=WIDGET_LABEL(autoInfo_box , VALUE='', YSIZE=15)

  bInfo3=WIDGET_BASE(autoInfo_box, /COLUMN)
  lblQTinfo=WIDGET_LABEL(bInfo3, VALUE='If a QuickTest template is selected the calculation will start automatically and generate results to clipboard.', FONT=font1, /ALIGN_LEFT)
  lblQTinfo2=WIDGET_LABEL(bInfo3, VALUE='The images will be sorted on acquisition date first and the results will be generated per acquisition date found.', FONT=font1, /ALIGN_LEFT)
  lblQTinfo3=WIDGET_LABEL(bInfo3, VALUE='If a result file is specified the results will be appended to this as a row with no headers per acquisition date,', FONT=font1, /ALIGN_LEFT)
  lblQTinfo3=WIDGET_LABEL(bInfo3, VALUE='else the program will pause for each date and give the opportunity to paste the results into any file or Excel.', FONT=font1, /ALIGN_LEFT)


  WIDGET_CONTROL, autoInfo_box , /REALIZE
  XMANAGER, 'autoInfo', autoInfo_box

end

pro autoSetup_event, event

  COMMON AUTO
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=a_uval

  IF N_ELEMENTS(a_uval) GT 0 AND SIZE(a_uval, /TNAME) EQ 'STRING' THEN BEGIN
    CASE a_uval OF
      'listTemp':BEGIN
        selecTemp=WIDGET_INFO(listTemp, /LIST_SELECT)
        clearParam=0
        IF selecTemp(0) NE -1 THEN BEGIN
          IF tempnames(0) NE '' THEN updateAuto, selecTemp(0) ELSE clearParam=1
        ENDIF ELSE clearParam=1
        IF clearParam THEN updateAuto, -1
      END

      'a_delTemp':BEGIN
        currSel=WIDGET_INFO(listTemp, /LIST_SELECT)
        IF currSel GE 0 THEN BEGIN
          IF tempnames(0) NE '' THEN BEGIN
            RESTORE, thisPath+'data\config.dat'
            loadTemp=removeIDstructstruct(loadTemp, currSel)
            IF N_ELEMENTS(tempnames) EQ 1 THEN loadTemp=!Null
            SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
            IF N_ELEMENTS(tempnames) EQ 1 THEN BEGIN
              tempnames=''
              WIDGET_CONTROL, listTemp, SET_VALUE=tempnames
              updateAuto, -1
            ENDIF ELSE BEGIN
              tempnames=removeIDarr(tempnames,currSel)
              WIDGET_CONTROL, listTemp, SET_VALUE=tempnames, SET_LIST_SELECT=0
              updateAuto, 0
            ENDELSE
          ENDIF
        ENDIF
      END
      'a_upTemp':BEGIN
        currSel=WIDGET_INFO(listTemp, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(tempnames))
          newOrder=oldOrder
          newOrder[currSel-1:currSel]=REVERSE(oldOrder[currSel-1:currSel])
          RESTORE, thisPath+'data\config.dat'
          loadTemp=reorderStructStruct(loadTemp, newOrder)
          SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
          tempnames=TAG_NAMES(loadTemp)
          WIDGET_CONTROL, listTemp, SET_VALUE=tempnames, SET_LIST_SELECT=currSel-1
          updateAuto, currSel-1
        ENDIF
      END
      'a_downTemp':BEGIN
        currSel=WIDGET_INFO(listTemp, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(tempnames)-1 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(tempnames))
          newOrder=oldOrder
          newOrder[currSel:currSel+1]=REVERSE(oldOrder[currSel:currSel+1])
          RESTORE, thisPath+'data\config.dat'
          loadTemp=reorderStructStruct(loadTemp, newOrder)
          SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
          tempnames=TAG_NAMES(loadTemp)
          WIDGET_CONTROL, listTemp, SET_VALUE=tempnames, SET_LIST_SELECT=currSel+1
          updateAuto, currSel+1
        ENDIF
      END

      'a_Browse':BEGIN
        adr=dialog_pickfile(PATH=defPath, GET_PATH=defPath, /DIRECTORY, /READ, TITLE='Select folder', DIALOG_PARENT=event.top)
        IF adr(0) NE '' THEN WIDGET_CONTROL, txtBrowse, SET_VALUE=adr(0)
      END

      'a_BrowseApp':BEGIN
        adr=dialog_pickfile(PATH=defPath, GET_PATH=defPath, /READ, TITLE='Select result file to append', FILTER='*.txt', /FIX_FILTER, DIALOG_PARENT=event.top)
        IF adr(0) NE '' THEN WIDGET_CONTROL, txtBrowseApp, SET_VALUE=adr(0)
      END
      
      'a_getStatName':BEGIN
        WIDGET_CONTROL, txtBrowse, GET_VALUE=pathNow
        If pathNow EQ '' THEN pathNow=defPath
        adr=dialog_pickfile(PATH=pathNow, /READ, TITLE='Select DICOM image to retrieve the station name', DIALOG_PARENT=event.top)
        IF adr(0) NE '' THEN BEGIN
          IF FILE_BASENAME(adr) EQ 'DICOMDIR' THEN okDcm = 0 ELSE okDcm=QUERY_DICOM(adr)
          IF okDcm THEN BEGIN
            o=obj_new('idlffdicom')
            t=o->read(adr)

            ;check if directoryfile
            test=o->GetReference('0004'x,'1220'x)
            IF test(0) EQ -1 THEN BEGIN
              test=o->GetReference('0008'x,'1010'x);
              test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
              IF test(0) NE -1 THEN stationName=*(test_peker[0]) ELSE stationName=''
              WIDGET_CONTROL, txtStatName, SET_VALUE=stationName
            ENDIF ELSE sv=DIALOG_MESSAGE('Selected file is not an DICOM image file.',DIALOG_PARENT=event.top)
            
          ENDIF ELSE sv=DIALOG_MESSAGE('Selected file is not an DICOM image file.',DIALOG_PARENT=event.top)
        ENDIF
      END

      'a_ClearApp': WIDGET_CONTROL, txtBrowseApp, SET_VALUE=''

      'a_saveas':BEGIN
        WIDGET_CONTROL, txtName, GET_VALUE=newName
        IF newName NE '' THEN BEGIN
          WIDGET_CONTROL, txtBrowse, GET_VALUE=newPath
          WIDGET_CONTROL, txtBrowseApp, GET_VALUE=newPathApp
          WIDGET_CONTROL, txtStatName, GET_VALUE=newStatName
          IF newPath NE '' THEN BEGIN

            loadTempSing=CREATE_STRUCT($
              'path',newPath,$
              'statName',newStatName,$
              'loadBy',WIDGET_INFO(btnOnlyLastDate,/BUTTON_SET),$
              'includeSub',WIDGET_INFO(btnInclSub,/BUTTON_SET),$
              'sortBy', sortElem, $
              'paramSet',paramSetNames(WIDGET_INFO(listSets,/DROPLIST_SELECT)), $
              'quickTemp',quickTempNames(WIDGET_INFO(listQT,/DROPLIST_SELECT)), $
              'pathApp',newPathApp,$
              'archive',WIDGET_INFO(btnMoveFiles,/BUTTON_SET),$
              'deleteFiles',WIDGET_INFO(btnDeleteFiles,/BUTTON_SET))

            RESTORE, thisPath+'data\config.dat'
            szLT=SIZE(loadTemp, /TNAME)
            IF szLT EQ 'STRUCT' THEN alreadyNames=TAG_NAMES(loadTemp) ELSE alreadyNames=''
            IF alreadyNames(0) EQ 'EMPTY' THEN alreadyNames=''
            IF alreadyNames(0) EQ '' THEN BEGIN; new single
              loadTemp=CREATE_STRUCT(newName, loadTempSing)
              SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
              tempnames=newName
              WIDGET_CONTROL, listTemp, SET_VALUE=tempnames, SET_LIST_SELECT=0
              updateAuto,0
            ENDIF ELSE BEGIN; add
              IF alreadyNames.HasValue(newName) THEN sv=DIALOG_MESSAGE('Template name already exists. Select a new name or use Overwrite.',DIALOG_PARENT=event.top) ELSE BEGIN
                loadTemp=CREATE_STRUCT(loadTemp, newName, loadTempSing)
                SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
                tempnames=[tempnames,newName]
                WIDGET_CONTROL, listTemp, SET_VALUE=tempnames, SET_LIST_SELECT=N_ELEMENTS(tempnames)-1
                updateAuto,N_ELEMENTS(tempnames)-1
              ENDELSE
            ENDELSE
          ENDIF ELSE  sv=DIALOG_MESSAGE('Specify path.',DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('Specify template name.',DIALOG_PARENT=event.top)
      END
      'a_overwrite':BEGIN
        WIDGET_CONTROL, txtName, GET_VALUE=newName
        selecTemp=WIDGET_INFO(listTemp, /LIST_SELECT)
        IF selecTemp NE -1 THEN BEGIN
          IF newName NE '' THEN BEGIN
            WIDGET_CONTROL, txtBrowse, GET_VALUE=newPath
            IF newPath NE '' THEN BEGIN
              WIDGET_CONTROL, txtBrowseApp, GET_VALUE=newPathApp
              WIDGET_CONTROL, txtStatName, GET_VALUE=newStatName

              loadTempSing=CREATE_STRUCT($
                'path',newPath,$
                'statName', newStatName,$
                'loadBy',WIDGET_INFO(btnOnlyLastDate,/BUTTON_SET),$
                'includeSub',WIDGET_INFO(btnInclSub,/BUTTON_SET),$
                'sortBy', sortElem, $
                'paramSet',paramSetNames(WIDGET_INFO(listSets,/DROPLIST_SELECT)), $
                'quickTemp',quickTempNames(WIDGET_INFO(listQT,/DROPLIST_SELECT)),$
                'pathApp',newPathApp,$
                'archive',WIDGET_INFO(btnMoveFiles,/BUTTON_SET),$
                'deleteFiles',WIDGET_INFO(btnDeleteFiles,/BUTTON_SET))

              RESTORE, thisPath+'data\config.dat'

              loadTemp=replaceStructStruct(loadTemp, loadTempSing, selecTemp, NEW_TAG_NAME=newName)

              SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=thisPath+'data\config.dat'
              tempnames=TAG_NAMES(loadTemp)
              WIDGET_CONTROL, listTemp, SET_VALUE=tempnames, SET_LIST_SELECT=selecTemp

            ENDIF ELSE  sv=DIALOG_MESSAGE('Specify path.',DIALOG_PARENT=event.top)
          ENDIF ELSE sv=DIALOG_MESSAGE('Specify template name.',DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('No template selected to overwrite.',DIALOG_PARENT=event.top)
      END
      'a_info': autoInfo, GROUP_LEADER=Event.top
      'a_cancel': WIDGET_CONTROL, Event.top, /DESTROY
      'a_run': BEGIN
        selecTemp=WIDGET_INFO(listTemp, /LIST_SELECT)
        IF selecTemp NE -1 THEN BEGIN
          ;check if saved
          RESTORE, thisPath+'data\config.dat'
          equal=1
          selTemp=loadTemp.(selecTemp)
          WIDGET_CONTROL, txtBrowse, GET_VALUE=newPath
          WIDGET_CONTROL, txtBrowseApp, GET_VALUE=newPathApp
          IF selTemp.path NE newPath THEN equal=0 ELSE BEGIN
            IF selTemp.loadBy NE WIDGET_INFO(btnOnlyLastDate,/BUTTON_SET) THEN equal=0 ELSE BEGIN
              IF selTemp.includeSub NE WIDGET_INFO(btnInclSub,/BUTTON_SET) THEN equal=0 ELSE BEGIN
                IF ~ARRAY_EQUAL(selTemp.sortBy, sortElem) THEN equal=0 ELSE BEGIN
                  IF selTemp.paramSet NE paramSetNames(WIDGET_INFO(listSets,/DROPLIST_SELECT)) THEN equal=0 ELSE BEGIN
                    IF selTemp.quickTemp NE quickTempNames(WIDGET_INFO(listQT,/DROPLIST_SELECT)) THEN equal=0 ELSE BEGIN
                      IF selTemp.pathApp NE newPathApp THEN equal=0 ELSE BEGIN
                        IF selTemp.archive NE WIDGET_INFO(btnMoveFiles,/BUTTON_SET) THEN equal=0 ELSE BEGIN
                          IF selTemp.deleteFiles NE WIDGET_INFO(btnDeleteFiles,/BUTTON_SET) THEN equal=0
                        ENDELSE
                      ENDELSE
                    ENDELSE
                  ENDELSE
                ENDELSE
              ENDELSE
            ENDELSE
          ENDELSE
          IF equal THEN BEGIN
            WIDGET_CONTROL, Event.top, /DESTROY
            autoTempRun, selTemp 
          ENDIF ELSE sv=DIALOG_MESSAGE('Saved template and current values do not match. Save before running the template.',DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('No template selected.',DIALOG_PARENT=event.top)
      END
      'a_addElem':BEGIN
        newElem=tags_imgStruct(WIDGET_INFO(listElem,/DROPLIST_SELECT))
        IF sortElem(0) EQ '' THEN sortElem=newElem ELSE BEGIN
          IF N_ELEMENTS(sortElem) LT 9 THEN sortElem=[sortElem,newElem] ELSE sv=DIALOG_MESSAGE('Maximum 9 sorting levels.',DIALOG_PARENT=event.top)
        ENDELSE
        WIDGET_CONTROL, listSort, SET_VALUE=sortElem
      END
      'a_delElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel GE 0 THEN BEGIN
          IF N_ELEMENTS(sortElem) GT 1 THEN sortElem=removeIDarr(sortElem, currSel) ELSE sortElem=''
          WIDGET_CONTROL, listSort, SET_VALUE=sortElem
        ENDIF
      END
      'a_upElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldPrev=sortElem(currSel-1)
          newList=sortElem
          newList(currSel-1)=sortElem(currSel)
          newList(currSel)=oldPrev
          sortElem=newList
          WIDGET_CONTROL, listSort, SET_VALUE=sortElem
        ENDIF
      END
      'a_downElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(sortElem)-1 THEN BEGIN
          oldNext=sortElem(currSel+1)
          newList=sortElem
          newList(currSel+1)=sortElem(currSel)
          newList(currSel)=oldNext
          sortElem=newList
          WIDGET_CONTROL, listSort, SET_VALUE=sortElem
        ENDIF
      END
      ELSE:
    ENDCASE
  ENDIF

  ;validate structure name of template
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') OR (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
    action=0
    IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') THEN BEGIN
      IF event.enter EQ 0 THEN action=1 ; lost focus
    ENDIF
    IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
      IF event.type EQ 0 THEN action=1 ;return or enter pressed
    ENDIF
    IF action EQ 1 THEN BEGIN
      IF event.ID EQ txtName THEN BEGIN
        WIDGET_CONTROL, txtName, GET_VALUE=newName
        newerName=IDL_VALIDNAME(newName, /CONVERT_ALL)
        WIDGET_CONTROL, txtName, SET_VALUE=newerName
      ENDIF
    ENDIF
  ENDIF

end

pro updateAuto, selT
  COMMON AUTO
  COMMON VARI
  COMPILE_OPT hidden

  IF selT NE -1 THEN BEGIN
    RESTORE, thisPath+'data\config.dat'
    WIDGET_CONTROL, txtName, SET_VALUE=tempnames(selT)
    WIDGET_CONTROL, txtBrowse, SET_VALUE=loadTemp.(selT).path
    WIDGET_CONTROL, txtStatName, SET_VALUE=loadTemp.(selT).statName
    WIDGET_CONTROL, txtBrowseApp, SET_VALUE=loadTemp.(selT).pathApp
    WIDGET_CONTROL, btnOnlyLastDate, SET_BUTTON=loadTemp.(selT).loadBy
    WIDGET_CONTROL, btnInclSub, SET_BUTTON=loadTemp.(selT).includeSub
    WIDGET_CONTROL, btnMoveFiles, SET_BUTTON=loadTemp.(selT).archive
    WIDGET_CONTROL, btnDeleteFiles, SET_BUTTON=loadTemp.(selT).deleteFiles
    WIDGET_CONTROL, listSort, SET_VALUE=loadTemp.(selT).sortBy
    sortElem=loadTemp.(selT).sortBy

    ;paramSet - exists still?
    paramSetName=STRUPCASE(loadTemp.(selT).paramSet)
    IF paramSetNames.HasValue(paramSetName) THEN BEGIN
      selNo=WHERE(paramSetNames EQ paramSetName)
      selParam=selNo(0)
    ENDIF ELSE BEGIN
      selParam=selConfig-1
      sv=DIALOG_MESSAGE('Automation template saved with parametersetname no longer existing ('+loadTemp.(selT).paramSet+')', DIALOG_PARENT=evTop)
    ENDELSE
    ;quickTemp - exists still?
    quickTempName=STRUPCASE(loadTemp.(selT).quickTemp)
    IF quickTempNames.HasValue(quickTempName) THEN BEGIN
      selNo=WHERE(quickTempNames EQ quickTempName)
      selQT=selNo(0)
    ENDIF ELSE BEGIN
      selQT=0
      sv=DIALOG_MESSAGE('Automation template saved with QuickTest template no longer existing ('+loadTemp.(selT).quickTemp+')', DIALOG_PARENT=evTop)
    ENDELSE

    WIDGET_CONTROL, listSets, SET_DROPLIST_SELECT=selParam
    WIDGET_CONTROL, listQT, SET_DROPLIST_SELECT=selQT
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, txtName, SET_VALUE=''
    WIDGET_CONTROL, txtStatName, SET_VALUE=''
    WIDGET_CONTROL, txtBrowse, SET_VALUE=''
    WIDGET_CONTROL, btnInclSub, SET_BUTTON=0
    WIDGET_CONTROL, btnOnlyLastDate, SET_BUTTON=0
    WIDGET_CONTROL, listSort, SET_VALUE=''
    sortElem=''
    WIDGET_CONTROL, btnMoveFiles, SET_BUTTON=0
    WIDGET_CONTROL, txtBrowseApp, SET_VALUE=''
    WIDGET_CONTROL, listQT, SET_DROPLIST_SELECT=0
  ENDELSE

end