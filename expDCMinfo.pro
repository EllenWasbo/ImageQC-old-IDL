;ImageQC - quality control of medical images
;Copyright (C) 2020  Ellen Wasbo, Stavanger University Hospital, Norway
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

function getTableExp, tagNames, imgStru, deci
  nImg=N_TAGS(imgStru)
  nInfo=N_ELEMENTS(tagNames)
  infoTable=STRARR(nInfo,nImg+1)
  infoTable[*,0]=tagNames
  idsInclude=INTARR(nInfo)
  allTags=TAG_NAMES(imgStru.(0))
  FOR i=0, nInfo-1 DO idsInclude(i)=WHERE(allTags EQ tagNames(i))
  FOR i=0, nImg-1 DO BEGIN
    FOR j=0, nInfo-1 DO BEGIN
      ty=SIZE(imgStru.(i).(idsInclude(j)),/TNAME)
      IF ty EQ 'INT' OR ty EQ 'FLOAT' THEN BEGIN
        infoTable[j,i+1]=STRTRIM(STRING(imgStru.(i).(idsInclude(j))),1)
        IF deci EQ ',' THEN infoTable[j,i+1]=STRJOIN(STRSPLIT(infoTable[j,i+1], '.',/EXTRACT),',')
      ENDIF ELSE infoTable[j,i+1]=imgStru.(i).(idsInclude(j));already string
    ENDFOR
  ENDFOR

  return, infoTable
end

pro expDCMinfo, GROUP_LEADER = mainbase, xoff, yoff, markedImg

  COMMON EXPVAR, listTemp, listExp, listElem, expElem, tags_imgStruct, allDesc, idsInclude, txtSaveTempName
  COMMON VARI
  COMPILE_OPT hidden

  expbox = WIDGET_BASE(TITLE='Export image information from images as table',  $
    /COLUMN, XSIZE=380, YSIZE=340, XOFFSET=xoff, YOFFSET=yoff,GROUP_LEADER=mainbase, /TLB_KILL_REQUEST_EVENTS, /MODAL)

  sens=0
  IF saveOK THEN sens=1

  mlTop=WIDGET_BASE(expbox,YSIZE=10)

  lbl=WIDGET_LABEL(expbox, VALUE='Export image information.', FONT=font1, /ALIGN_LEFT, /NO_COPY)
  
  lbl=WIDGET_LABEL(expbox, VALUE='Add elements (>>) as column to export', FONT=font1, /ALIGN_LEFT, /NO_COPY)
  mlTop2=WIDGET_BASE(expbox,YSIZE=10)

  bExp=WIDGET_BASE(expbox, XSIZE=355,/COLUMN, FRAME=1)
  bExpLists=WIDGET_BASE(bExp, /ROW)
  dummyImgStruct=imgStructUpdate('','')
  descImgStruct=imgStructDescTags()
  allTags=TAG_NAMES(dummyImgStruct)
  allDesc=!Null
  FOR i=0, N_TAGS(dummyImgStruct)-1 DO allDesc=[allDesc,descImgStruct.(i)]
  allTags=allTags(SORT(STRUPCASE(allDesc)))
  allDesc=allDesc(SORT(STRUPCASE(allDesc)))
  includeArr=actualTags(allTags, imgStructInfo, modality)
  idsInclude=WHERE(includeArr EQ 1)
  tags_imgStruct=allTags;(idsInclude)
  tags_imgStructDesc=allDesc;(idsInclude)
  listElem=WIDGET_DROPLIST(bExpLists, VALUE=tags_imgStructDesc, XSIZE=150, FONT=font1)
  bButtMid=WIDGET_BASE(bExpLists, /COLUMN)
  btnAddElem=WIDGET_BUTTON(bButtMid, VALUE='>>', UVALUE='expDCM_addElem', FONT=font1)
  listExp=WIDGET_LIST(bExpLists, VALUE='', XSIZE=20, FONT=font1, SCR_YSIZE=100)
  bButtEnd=WIDGET_BASE(bExpLists, /COLUMN)
  btnDelElem=WIDGET_BUTTON(bButtEnd, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='expDCM_delElem', TOOLTIP='Delete selected element from column list')
  btnUpElem=WIDGET_BUTTON(bButtEnd, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='expDCM_upElem', TOOLTIP='Move element upwards in column list')
  btnDownElem=WIDGET_BUTTON(bButtEnd, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='expDCM_downElem', TOOLTIP='Move element downwards in column list')
  
  bListButt=WIDGET_BASE(expbox, /ROW)
  btnExpClear=WIDGET_BUTTON(bListButt, VALUE='Clear list', UVALUE='expClear', FONT=font1)
  btnExpModAct=WIDGET_BUTTON(bListButt, VALUE='Fill list with all actual for current modality', UVALUE='expFill', FONT=font1)

  mlTop5=WIDGET_BASE(expbox,YSIZE=20)

  bSelTemp=WIDGET_BASE(expbox,/ROW)
  lbl=WIDGET_LABEL(bSelTemp, VALUE='Select saved template:', /NO_COPY, FONT=font1)
  listTemp=WIDGET_DROPLIST(bSelTemp, VALUE='', UVALUE='selPattern', FONT=font1)
  bSaveTemp=WIDGET_BASE(expbox,/ROW)
  lbl=WIDGET_LABEL(bSaveTemp, VALUE='Save selections as:', /NO_COPY, FONT=font1)
  txtSaveTempName=WIDGET_TEXT(bSaveTemp, VALUE='',FONT=font1, SENSITIV=sens, /EDITABLE)
  btnSaveTemp=WIDGET_BUTTON(bSaveTemp, VALUE=thisPath+'images\plus.bmp',/BITMAP, UVALUE='savePattern', SENSITIV=sens)
  btnDelTemp=WIDGET_BUTTON(bSaveTemp, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='deletePattern', SENSITIV=sens)

  lbl=WIDGET_LABEL(expbox, VALUE='', YSIZE=10, /NO_COPY)
  bBtmButt=WIDGET_BASE(expbox, /ROW)
  lbl=WIDGET_LABEL(bBtmButt, VALUE='', XSIZE=100, /NO_COPY)
  btnCancel=WIDGET_BUTTON(bBtmButt, VALUE='Cancel',UVALUE='expCancel', FONT=font1)
 
  btnExpClipboard=WIDGET_BUTTON(bBtmButt, VALUE='Copy to clipboard', UVALUE='copyClipboard', FONT=font1)
  btnExpSaveTxt=WIDGET_BUTTON(bBtmButt, VALUE='Save to .txt', UVALUE='expSaveTxt', FONT=font1)
  
  updPatterns, 0

  WIDGET_CONTROL, expbox, /REALIZE
  XMANAGER, 'expDCMinfo', expbox

end

pro expDCMinfo_event, event

  COMMON EXPVAR
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 AND SIZE(uval, /TNAME) EQ 'STRING' THEN BEGIN

    CASE uval OF
      'expCancel': WIDGET_CONTROL, Event.top, /DESTROY
      'expClear':BEGIN
        expElem=''
        WIDGET_CONTROL, listExp, SET_VALUE=''
      END
      'expFill':BEGIN
        expElem=tags_imgStruct(idsInclude)
        WIDGET_CONTROL, listExp, SET_VALUE=expElem
        END
      'copyClipboard':BEGIN
        IF expElem(0) NE '' THEN BEGIN
          tblExp=getTableExp(expElem,structImgs, deciMark)
          CLIPBOARD.set, STRJOIN(tblExp, STRING(9B))
          WIDGET_CONTROL, Event.top, /DESTROY
        ENDIF ELSE sv=DIALOG_MESSAGE('Export list is empty. Use the >> button to push the elements to add as columns.', /INFORMATION, DIALOG_PARENT=event.Top)
      END
      'expSaveTxt':BEGIN
        IF expElem(0) NE '' THEN BEGIN
          tblExp=getTableExp(expElem,structImgs,deciMark)

          WIDGET_CONTROL, Event.top, /DESTROY

          adr=DIALOG_PICKFILE(TITLE='Save table of image information as...',/WRITE, FILTER='*.txt', /FIX_FILTER, DEFAULT_EXTENSION='.txt', DIALOG_PARENT=evTop)
          IF adr(0) NE '' THEN BEGIN
            a=STRJOIN(tblExp, STRING(9B))
            OPENW, expfile, adr,/GET_LUN
            FOR i=0, N_ELEMENTS(a)-1 DO PRINTF, expfile, a(i)
            CLOSE, expfile & FREE_LUN, expfile
          ENDIF

        ENDIF ELSE sv=DIALOG_MESSAGE('Export list is empty. Use the >> button to push the elements to add as columns.', /INFORMATION, DIALOG_PARENT=event.Top)
        END
      'selPattern':BEGIN
        currSel=WIDGET_INFO(listTemp, /DROPLIST_SELECT)
        updPatterns, currSel
        END
      'deletePattern':BEGIN
        currSel=WIDGET_INFO(listTemp, /DROPLIST_SELECT)
        IF currSel NE -1 THEN BEGIN
          RESTORE, configPath
          commonConfig=configS.(0)
          expInfoPatterns=commonConfig.EXPINFOPATTERNS
          tnames=TAG_NAMES(expInfoPatterns)
          IF N_ELEMENTS(tnames) GT 1 THEN BEGIN
            expInfoPatterns=removeIDstructstruct(expInfoPatterns,currSel)
            idExp=WHERE(TAG_NAMES(commonConfig) EQ 'EXPINFOPATTERNS')
            commonConfig=replaceStructStruct(commonConfig, expInfoPatterns, idExp)
            configS=replaceStructStruct(configS,commonConfig,0)
            SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=configPath           
            updPatterns, 0
          ENDIF ELSE sv=DIALOG_MESSAGE('Can not delete last template.', /INFORMATION, DIALOG_PARENT=event.Top)
        ENDIF
        END
      'savePattern':BEGIN
        IF saveOK THEN BEGIN
          IF expElem(0) EQ '' THEN BEGIN
            sv=DIALOG_MESSAGE('Can not save empty template.', /INFORMATION, DIALOG_PARENT=event.Top)
          ENDIF ELSE BEGIN
            RESTORE, configPath
            commonConfig=configS.(0)
            expInfoPatterns=commonConfig.EXPINFOPATTERNS
            idExp=WHERE(TAG_NAMES(commonConfig) EQ 'EXPINFOPATTERNS')
            
            WIDGET_CONTROL, txtSaveTempName, GET_VALUE=inputName
            commonConfig=configS.(0)
            expInfoPatterns=commonConfig.EXPINFOPATTERNS
            tnames=TAG_NAMES(expInfoPatterns)
            upCasetnames=STRUPCASE(tnames)
            newname=IDL_VALIDNAME(inputName(0), /CONVERT_ALL)
            saveP=1
            sel=0
            IF upCasetnames.HasValue(STRUPCASE(newname)) THEN BEGIN
              sv=DIALOG_MESSAGE('Overwrite current template named '+inputName(0)+'?' , /QUESTION, DIALOG_PARENT=event.Top)
              IF sv EQ 'Yes' THEN BEGIN
                sel=WHERE(tnames EQ newname)
                expInfoPatterns=replaceStructStruct(expInfoPatterns, expElem, sel)             
              ENDIF ELSE saveP=0
            ENDIF ELSE BEGIN
              expInfoPatterns=CREATE_STRUCT(expInfoPatterns, newname, expElem)
              sel=N_ELEMENTS(tnames)
            ENDELSE
  
            IF saveP THEN BEGIN
              commonConfig=replaceStructStruct(commonConfig, expInfoPatterns, idExp)
              configS=replaceStructStruct(configS,commonConfig,0)
              SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=configPath
              updPatterns, sel
            ENDIF
          ENDELSE
        ENDIF

      END
      'expDCM_addElem':BEGIN
        newElem=tags_imgStruct(WIDGET_INFO(listElem,/DROPLIST_SELECT))
        IF expElem(0) EQ '' THEN expElem=newElem ELSE expElem=[expElem,newElem]
        WIDGET_CONTROL, listExp, SET_VALUE=expElem
      END
      'expDCM_delElem':BEGIN
        currSel=WIDGET_INFO(listExp, /LIST_SELECT)
        IF currSel GE 0 THEN BEGIN
          IF N_ELEMENTS(expElem) GT 1 THEN BEGIN
            expElem=removeIDarr(expElem, currSel)
            WIDGET_CONTROL, listExp, SET_VALUE=expElem
          ENDIF ELSE BEGIN
            expElem=''
            WIDGET_CONTROL, listExp, SET_VALUE=''
          ENDELSE

        ENDIF
      END
      'expDCM_upElem':BEGIN
        currSel=WIDGET_INFO(listExp, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldPrev=expElem(currSel-1)
          newList=expElem
          newList(currSel-1)=expElem(currSel)
          newList(currSel)=oldPrev
          expElem=newList
          WIDGET_CONTROL, listExp, SET_VALUE=expElem
        ENDIF
      END
      'expDCM_downElem':BEGIN
        currSel=WIDGET_INFO(listExp, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(expElem)-1 THEN BEGIN
          oldNext=expElem(currSel+1)
          newList=expElem
          newList(currSel+1)=expElem(currSel)
          newList(currSel)=oldNext
          expElem=newList
          WIDGET_CONTROL, listExp, SET_VALUE=expElem
        ENDIF
      END
      ELSE:
    ENDCASE

  ENDIF

  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN WIDGET_CONTROL, event.top, /DESTROY

end

pro updPatterns, selno
  COMMON EXPVAR
  COMMON VARI
  COMPILE_OPT hidden
  ;fill lists and name
  WIDGET_CONTROL, /HOURGLASS
  IF FILE_TEST(configPath, /READ) THEN BEGIN
    RESTORE, configPath 
    
    commonConfig=configS.(0)
    expInfoPatterns=commonConfig.EXPINFOPATTERNS

    pat_names=TAG_NAMES(expInfoPatterns)
    WIDGET_CONTROL, listTemp, SET_VALUE=pat_names, SET_LIST_SELECT=selno
    
    expElem=expInfoPatterns.(selno)
    WIDGET_CONTROL, listExp, SET_VALUE=expElem
    WIDGET_CONTROL, txtSaveTempName, SET_VALUE=pat_names(selno)

    
  ENDIF ELSE sv=DIALOG_MESSAGE('Lost connection to config file '+configPath, /ERROR)
end