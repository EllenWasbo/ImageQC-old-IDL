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


pro sortImgs, GROUP_LEADER = mainbase, xoff, yoff, firstLast

  COMMON SORTVAR, firstLastSel, listTemp, listSort, listElem, ascElem, sortElem, tags_imgStruct, tempMod
  COMMON VARI
  COMPILE_OPT hidden

  firstLastSel=firstLast

  sortbox = WIDGET_BASE(TITLE='Sort selected images',  $
    /COLUMN, XSIZE=380, YSIZE=340, XOFFSET=xoff, YOFFSET=yoff,GROUP_LEADER=mainbase, /TLB_KILL_REQUEST_EVENTS, /MODAL)

  IF N_ELEMENTS(currSortElem) GT 0 THEN BEGIN
    sortElem=currSortElem
    IF N_ELEMENTS(currAscElem) GT 0 THEN ascElem=currAscElem ELSE ascElem=INTARR(N_ELEMENTS(sortElem))
  ENDIF ELSE BEGIN
    sortElem=''
    ascElem=0
  ENDELSE

  availMod=TAG_NAMES(testVisualQTNames)
  allMod=TAG_NAMES(multiOpt)
  tempMod=WHERE(availMod EQ allMod(modality))
  IF tempMod EQ -1 THEN sens=0 ELSE sens=1

  RESTORE, configPath

  mlTop=WIDGET_BASE(sortbox,YSIZE=10)
  
  lbl=WIDGET_LABEL(sortbox, VALUE='Sort images by elements from DICOM-header.', FONT=font1, /ALIGN_LEFT, /NO_COPY)
  lbl=WIDGET_LABEL(sortbox, VALUE='Add elements to list (>>) to define sort order.', FONT=font1, /ALIGN_LEFT, /NO_COPY)
  mlTop2=WIDGET_BASE(sortbox,YSIZE=10)

  bSortBy=WIDGET_BASE(sortbox, XSIZE=355,/COLUMN, FRAME=1)
  bSortByLists=WIDGET_BASE(bSortBy, /ROW)
  dummyImgStruct=imgStructUpdate('','')
  descImgStruct=imgStructDescTags()
  allTags=TAG_NAMES(dummyImgStruct)
  allDesc=!Null
  FOR i=0, N_TAGS(dummyImgStruct)-1 DO allDesc=[allDesc,descImgStruct.(i)]
  allTags=allTags(SORT(STRUPCASE(allDesc)))
  allDesc=allDesc(SORT(STRUPCASE(allDesc)))
  includeArr=actualTags(allTags, imgStructInfo, modality)
  idsInclude=WHERE(includeArr EQ 1)
  tags_imgStruct=allTags(idsInclude)
  tags_imgStructDesc=allDesc(idsInclude)
  listElem=WIDGET_DROPLIST(bSortByLists, VALUE=tags_imgStructDesc, XSIZE=150, FONT=font1)
  bButtMid=WIDGET_BASE(bSortByLists, /COLUMN)
  btnAddElem=WIDGET_BUTTON(bButtMid, VALUE='>>', UVALUE='sort_addElem', FONT=font1)
  IF sortElem(0) EQ '' THEN val='' ELSE val=ascDesc01(ascElem)+sortElem
  listSort=WIDGET_LIST(bSortByLists, VALUE=val, XSIZE=20, FONT=font1, SCR_YSIZE=100)
  bButtEndSort=WIDGET_BASE(bSortByLists, /COLUMN)
  btnDelElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='sort_delElem', TOOLTIP='Delete selected element from sort list')
  btnUpElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='sort_upElem', TOOLTIP='Move element upwards in sort list')
  btnDownElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='sort_downElem', TOOLTIP='Move element downwards in sort list')
  btnAscElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\sort.bmp',/BITMAP, UVALUE='sort_asc', TOOLTIP='Set ascending/descending option')

  mlTop5=WIDGET_BASE(sortbox,YSIZE=20)
  lbl=WIDGET_LABEL(sortbox, VALUE='Load or save sort pattern for automation templates', FONT=font1, /ALIGN_LEFT, /NO_COPY)
  lbl=WIDGET_LABEL(sortbox, VALUE='(Only templates for current modality selection is available)', FONT=font1, /ALIGN_LEFT, /NO_COPY)
  mlTop=WIDGET_BASE(sortbox,YSIZE=10)
  bLoadTemp=WIDGET_BASE(sortbox, XSIZE=355, /ROW, FRAME=1)
  listVals=''
  IF tempMod NE -1 THEN BEGIN
    IF N_ELEMENTS(loadTemp) GT 0 THEN BEGIN
      IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
        IF N_TAGS(loadTemp) GT modality THEN BEGIN
          IF SIZE(loadTemp.(modality),/TNAME) EQ 'STRUCT' THEN listVals=TAG_NAMES(loadTemp.(modality))
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  listTemp=WIDGET_DROPLIST(bLoadTemp, VALUE=listVals, XSIZE=150, FONT=font1, SENSITIV=sens)
  lbl=WIDGET_LABEL(bLoadTemp, VALUE='', XSIZE=20, /NO_COPY)
  btnLoadTemp=WIDGET_BUTTON(bLoadTemp, VALUE='Load pattern', FONT=font1, UVALUE='loadPattern', SENSITIV=sens)
  btnSaveTemp=WIDGET_BUTTON(bLoadTemp, VALUE='Save pattern', FONT=font1, UVALUE='savePattern', SENSITIV=sens)

  lbl=WIDGET_LABEL(sortbox, VALUE='', YSIZE=10, /NO_COPY)
  bBtmButt=WIDGET_BASE(sortbox, /ROW)
  lbl=WIDGET_LABEL(bBtmButt, VALUE='', XSIZE=200, /NO_COPY)
  btnCancel=WIDGET_BUTTON(bBtmButt, VALUE='Cancel',UVALUE='sortCancel', FONT=font1)
  btnSortClear=WIDGET_BUTTON(bBtmButt, VALUE='Clear', UVALUE='sortClear', FONT=font1)
  btnSortOK=WIDGET_BUTTON(bBtmButt, VALUE='Sort', UVALUE='sortOK', FONT=font1)

  WIDGET_CONTROL, sortbox, /REALIZE
  XMANAGER, 'sortImgs', sortbox

end

pro sortImgs_event, event

  COMMON SORTVAR
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 AND SIZE(uval, /TNAME) EQ 'STRING' THEN BEGIN

    CASE uval OF
      'sortCancel': WIDGET_CONTROL, Event.top, /DESTROY
      'sortClear':BEGIN
        sortElem=''
        ascElem=0
        WIDGET_CONTROL, listSort, SET_VALUE=''
        END
      'sortOK':BEGIN
        currSortElem=sortElem
        currAscElem=ascElem
        
        WIDGET_CONTROL, /HOURGLASS
        ;sort by
        IF sortElem(0) NE '' THEN BEGIN

          tnames=TAG_NAMES(structImgs.(0))
          sortNames=sortElem

          keyArr=STRARR(firstLastSel(1)-firstLastSel(0)+1,9);max 9 sorting levels

          FOR ss=0, N_ELEMENTS(sortNames)-1 DO BEGIN; for each tag to sort by
            WIDGET_CONTROL, lblProgress, SET_VALUE='Sorting images: '+STRING(ss*100./N_ELEMENTS(sortNames), FORMAT='(i0)')+' %'
            tagno=WHERE(tnames EQ STRUPCASE(sortNames(ss)))
            IF tagno(0) NE -1 THEN BEGIN
              list2sort=!Null
              FOR i=firstLastSel(0), firstLastSel(1) DO list2sort=[list2sort,structImgs.(i).(tagno(0))(0)]
              reformatNo=WHERE(STRUPCASE(imgStructInfo[0,*]) EQ STRUPCASE(sortNames(ss)))
              IF reformatNo(0) NE -1 THEN newFormat=imgStructInfo[1,reformatNo(0)] ELSE newFormat='STRING'
              IF newFormat EQ 'FLOAT' THEN newFormat='DOUBLE'
              CASE newFormat OF
                'FLOAT':list2sort=STRING(FLOAT(list2sort)-MIN(FLOAT(list2sort)), FORMAT='(f06.5)')
                'DOUBLE':list2sort=STRING(DOUBLE(list2sort)-MIN(DOUBLE(list2sort)), FORMAT='(f010.5)')
                'LONG':list2sort=STRING(LONG(list2sort)-MIN(LONG(list2sort)), FORMAT='(i010)')
                ELSE:
              ENDCASE
              ;IF ascElem(ss) THEN list2sort=REVERSE(list2sort)
              keyArr[*,ss]=list2sort
            ENDIF
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=''

          ;newSubOrder=MULTISORT(keyArr[*,0],keyArr[*,1],keyArr[*,2],keyArr[*,3],keyArr[*,4],keyArr[*,5],keyArr[*,6],keyArr[*,7],keyArr[*,8])
          newSubOrder=multiBsort(keyArr,ascElem);multiBsort function in a2_bsort.pro
          newOrder[firstLastSel(0):firstLastSel(1)]=newSubOrder+firstLastSel(0)
          ;IF ARRAY_EQUAL(newOrder,INDGEN(nImg)) EQ 0 THEN structImgs=reorderStructStruct(structImgs, newOrder)
          WIDGET_CONTROL, Event.top, /DESTROY
        ENDIF ELSE sv=DIALOG_MESSAGE('No sort pattern was selected. Use the >> button to push the elements to the pattern.', /INFORMATION, DIALOG_PARENT=event.Top)
      END
      'loadPattern':BEGIN
        RESTORE, configPath
        currSel=WIDGET_INFO(listTemp, /DROPLIST_SELECT)
        IF currSel NE -1 THEN BEGIN
          sortElem=loadTemp.(tempMod).(currSel).sortBy
          ascElem=loadTemp.(tempMod).(currSel).sortAsc
          IF N_ELEMENTS(ascElem) NE N_ELEMENTS(sortElem) THEN ascElem=INTARR(N_ELEMENTS(sortElem))
          WIDGET_CONTROL, listSort, SET_VALUE=TRANSPOSE(ascDesc01(ascElem)+sortElem)
        ENDIF
      END
      'savePattern':BEGIN
        sv=DIALOG_MESSAGE('Overwrite selected template with new sort order?',/QUESTION,DIALOG_PARENT=event.top)
        IF sv EQ 'Yes' THEN BEGIN
          RESTORE, configPath
          currSel=WIDGET_INFO(listTemp, /DROPLIST_SELECT)
          IF currSel NE -1 THEN BEGIN

            loadTempSing=CREATE_STRUCT($
              'path',loadTemp.(tempMod).(currSel).path,$
              'statName', loadTemp.(tempMod).(currSel).statName,$
              'loadBy',loadTemp.(tempMod).(currSel).loadBy,$
              'includeSub',loadTemp.(tempMod).(currSel).includeSub,$
              'sortBy', sortElem, $
              'sortAsc', ascElem,$
              'paramSet',loadTemp.(tempMod).(currSel).paramSet, $
              'quickTemp',loadTemp.(tempMod).(currSel).quickTemp,$
              'pathApp',loadTemp.(tempMod).(currSel).pathApp,$
              'archive',loadTemp.(tempMod).(currSel).archive,$
              'deleteFiles',loadTemp.(tempMod).(currSel).deleteFiles)

            loadTm=loadTemp.(tempMod)
            loadTm=replaceStructStruct(loadTm, loadTempSing, currSel)
            loadTemp=replaceStructStruct(loadTemp, loadTm, tempMod)
            SAVE, configS, quickTemp, quickTout, loadTemp, FILENAME=configPath
          ENDIF
        ENDIF

      END
      'sort_addElem':BEGIN
        newElem=tags_imgStruct(WIDGET_INFO(listElem,/DROPLIST_SELECT))
        IF sortElem(0) EQ '' THEN sortElem=newElem ELSE BEGIN
          IF N_ELEMENTS(sortElem) LT 9 THEN BEGIN
            sortElem=[sortElem,newElem]
            ascElem=[ascElem,0]
          ENDIF ELSE sv=DIALOG_MESSAGE('Maximum 9 sorting levels.',DIALOG_PARENT=event.top)
        ENDELSE
        WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
      END
      'sort_delElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel GE 0 THEN BEGIN
          IF N_ELEMENTS(sortElem) GT 1 THEN BEGIN
            sortElem=removeIDarr(sortElem, currSel)
            ascElem=removeIDarr(ascElem, currSel)
            WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
          ENDIF ELSE BEGIN
            sortElem=''
            ascElem=0
            WIDGET_CONTROL, listSort, SET_VALUE=''
          ENDELSE

        ENDIF
      END
      'sort_upElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldPrev=sortElem(currSel-1)
          newList=sortElem
          newList(currSel-1)=sortElem(currSel)
          newList(currSel)=oldPrev
          sortElem=newList
          oldPrev=ascElem(currSel-1)
          newList=ascElem
          newList(currSel-1)=ascElem(currSel)
          newList(currSel)=oldPrev
          ascElem=newList
          WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
        ENDIF
      END
      'sort_downElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(sortElem)-1 THEN BEGIN
          oldNext=sortElem(currSel+1)
          newList=sortElem
          newList(currSel+1)=sortElem(currSel)
          newList(currSel)=oldNext
          sortElem=newList
          oldNext=ascElem(currSel+1)
          newList=ascElem
          newList(currSel+1)=ascElem(currSel)
          newList(currSel)=oldNext
          ascElem=newList
          WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
        ENDIF
      END
      'sort_asc':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel(0) NE -1 THEN BEGIN
          IF N_ELEMENTS(ascElem) NE N_ELEMENTS(sortElem) THEN asc=INTARR(N_ELEMENTS(sortElem))
          FOR i=0, N_ELEMENTS(currSel)-1 DO IF ascElem(currSel(i)) EQ 0 THEN ascElem(currSel(i))=1 ELSE ascElem(currSel(i))=0
          WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
        ENDIF
      END
      ELSE:
    ENDCASE

  ENDIF
  
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN WIDGET_CONTROL, event.top, /DESTROY

end