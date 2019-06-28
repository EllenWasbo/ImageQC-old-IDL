function getSeriesList, struct
  serTab=''
  IF SIZE(struct, /TNAME) EQ 'STRUCT' THEN BEGIN
    nSer=N_TAGS(struct)
    serTab=STRARR(nSer)
    FOR i=0, nSer-1 DO serTab[i]=STRING(struct.(i).(0).acqDate, FORMAT='(i8)') +' ' + STRING(struct.(i).(0).acqTime, FORMAT='(i06)') +' ' +STRING(struct.(i).(0).seriesNmb, FORMAT='(i5)') +' ' + struct.(i).(0).seriesName
  ENDIF
  return, serTab
end

function getImgList, struct, serId
  imgTab=''
  IF SIZE(struct, /TNAME) EQ 'STRUCT' THEN BEGIN
    nImg=N_TAGS(struct.(serId))
    imgTab=STRARR(nImg)

    IF struct.(serId).(0).zpos NE -999. THEN BEGIN
      FOR i=0, nImg-1 DO imgTab[i]='imgNo ' + STRING(struct.(serId).(i).imgNo, FORMAT='(i3)') + ' zpos ' + STRING(struct.(serId).(i).zPos, FORMAT='(f0.1)')
    ENDIF ELSE FOR i=0, nImg-1 DO imgTab[i]='imgNo '+ STRING(struct.(serId).(i).imgNo, FORMAT='(3i0)')

  ENDIF
  return, imgTab
end

function getSelList, struct
  imgTab=''
  IF SIZE(struct, /TNAME) EQ 'STRUCT' THEN BEGIN
    nImg=N_TAGS(struct)
    imgTab=STRARR(nImg)

    IF struct.(0).zpos NE -999. THEN BEGIN
      FOR i=0, nImg-1 DO imgTab[i]=STRING(struct.(i).seriesNmb, FORMAT='(i5)') +' ' + struct.(i).seriesName + ' / imgNo ' + STRING(struct.(i).imgNo, FORMAT='(3i0)') + ' zpos ' + STRING(struct.(i).zPos, FORMAT='(f0.1)')
    ENDIF ELSE FOR i=0, nImg-1 DO imgTab[i]=STRING(struct.(i).seriesNmb, FORMAT='(i5)') +' ' + struct.(i).seriesName + ' / imgNo '+ STRING(struct.(i).imgNo, FORMAT='(3i0)')

  ENDIF
  return, imgTab
end

pro selectImages, markArr, QTstring, defPath

  COMMON SELIM, txtBrowse, lblProgressSelIm, structImgsSelIm, structSelected, lstSer, lstImg, lstSelImg, lstSort, $
    cw_typeSelect, txtSelNZ, txtSelNpos, txtSelCloseZ, cw_SelPos, defPathS
  COMPILE_OPT hidden

  defPathS=defPath
  structImgsSelIm=!Null
  structSelected=!Null

  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  font0="Tahoma*ITALIC*16"
  font1="Tahoma*14"
  fontTit="Tahoma*ITALIC*BOLD*18"

  selImgbox = WIDGET_BASE(TITLE='Select images to open', /COLUMN, XSIZE=1150, YSIZE=680, XOFFSET=20, YOFFSET=20)
  ml0=WIDGET_LABEL(selImgbox, VALUE='', YSIZE=20)
  topBase=WIDGET_BASE(selImgbox,/ROW)
  bBrowse = WIDGET_BASE(topBase, /ROW)
  lblBrowse = WIDGET_LABEL(bBrowse, VALUE='Selected folder:', FONT=font1)
  txtBrowse = WIDGET_TEXT(bBrowse, VALUE='', XSIZE=500, SCR_XSIZE=500, FONT=font1)
  btnBrowse = WIDGET_BUTTON(bBrowse, VALUE='Browse...', UVALUE='selectFolder', FONT=font1)
  lblBrowseTip = WIDGET_LABEL(bBrowse, VALUE='NB - local files quicker accessed than network files', FONT=font1)
  lblTopMl0=WIDGET_LABEL(topBase, VALUE='', XSIZE=20)
  lblProgressSelIm=WIDGET_LABEL(selImgbox, VALUE='', /DYNAMIC_RESIZE, XSIZE=200)

  ml1=WIDGET_LABEL(selImgbox, VALUE='', YSIZE=20)

  bLists=WIDGET_BASE(selImgbox, /ROW)
  bSerList=WIDGET_BASE(bLists, /COLUMN)
  lblSer=WIDGET_LABEL(bSerList, VALUE='Series ', FONT=fontTit)
  lblSer2=WIDGET_LABEL(bSerList, VALUE='', FONT=font1);'default sorted by series number '
  lstSer=WIDGET_LIST(bSerList, XSIZE=300, SCR_XSIZE=250, YSIZE=1, SCR_YSIZE=350, MULTIPLE=1, FONT=font1, UVALUE='serList');, /CONTEXT_EVENTS)

  bImgList=WIDGET_BASE(bLists, /COLUMN)
  lblImg=WIDGET_LABEL(bImgList, VALUE='Images in selected series ', FONT=fontTit)
  lblImg2=WIDGET_LABEL(bImgList, VALUE='images of first selected series sorted by image number', FONT=font1)
  lstImg=WIDGET_LIST(bImgList, XSIZE=280, SCR_XSIZE=340, YSIZE=1, SCR_YSIZE=350, MULTIPLE=1, FONT=font1, UVALUE='imgList');, /CONTEXT_EVENTS)

  bTransfer=WIDGET_BASE(bLists, /COLUMN)
  lblTransfer0=WIDGET_LABEL(bTransfer, VALUE='', YSIZE=100)
  lblTransfer1=WIDGET_LABEL(bTransfer, VALUE='Push', FONT=font1)
  lblTransfer2=WIDGET_LABEL(bTransfer, VALUE='selected', FONT=font1)
  btnAddSel=WIDGET_BUTTON(bTransfer, VALUE='>>', TOOLTIP='Add selected images if this series to the list of selected images', FONT=font1, UVALUE='addSel')

  bSelImgList=WIDGET_BASE(bLists, /COLUMN)
  lblSelImg=WIDGET_LABEL(bSelImgList, VALUE='Images to be opened ', FONT=fontTit)
  lblSelImg2=WIDGET_LABEL(bSelImgList, VALUE='in main window ', FONT=font1)
  lstSelImg=WIDGET_LIST(bSelImgList, XSIZE=280, SCR_XSIZE=330, YSIZE=1, SCR_YSIZE=350, MULTIPLE=1, FONT=font1, UVALUE='selImgList');, /CONTEXT_EVENTS)

  bQTlist=WIDGET_BASE(bLists, /COLUMN)
  lblQT=WIDGET_LABEL(bQTlist, VALUE='QuickTemp ', FONT=fontTit)
  lblQT2=WIDGET_LABEL(bQTlist, VALUE=QTstring, FONT=font1)
  lstQT=WIDGET_LIST(bQTlist, XSIZE=50, SCR_XSIZE=50, YSIZE=1, SCR_YSIZE=350, MULTIPLE=1, FONT=font1, UVALUE='qtList');, /CONTEXT_EVENTS)

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

  ;bottom base
  bBottom=WIDGET_BASE(selImgbox, /ROW)
  b1=WIDGET_BASE(bBottom, XSIZE=250, /COLUMN)
  lblB11=WIDGET_LABEL(b1, VALUE='Select options will affect all', FONT=font1, /ALIGN_LEFT)
  lblB12=WIDGET_LABEL(b1, VALUE='selected series.', FONT=font1,/ALIGN_LEFT)
  
  bSortBy=WIDGET_BASE(b1, /ROW)
  lblSort=WIDGET_LABEL(bSortby, VALUE='Sort by', FONT=font1, /ALIGN_LEFT)
  lstSort=WIDGET_DROPLIST(bSortby, VALUE=['Series number','Acquisition time'], XSIZE=150, FONT=font1, UVALUE='listSort')

  lblBtm1=WIDGET_LABEL(bBottom, VALUE='', XSIZE=5)
  b2=WIDGET_BASE(bBottom, /COLUMN, FRAME=1)
  
  b2_top=WIDGET_BASE(b2, /ROW)
 
  cw_typeSelect=CW_BGROUP(b2_top, ['Select all','Select the','Select the'], /EXCLUSIVE, FONT=font1, COLUMN=1, SPACE=7, YPAD=0, SET_VALUE=0, XSIZE=88, UVALUE='cw_typeSelect')
  b2_2=WIDGET_BASE(b2_top, /COLUMN)
  lblemptyall=WIDGET_LABEL(b2_2, VALUE='', YSIZE=25)
  bSelZ=WIDGET_BASE(b2_2,/ROW)
  txtSelNZ= WIDGET_TEXT(bSelZ, VALUE='1', XSIZE=5, SCR_XSIZE=25, FONT=font1, /EDITABLE)
  lblSelCloseZ=WIDGET_LABEL(bSelZ, VALUE=' images closest to z =', FONT=font1)
  txtSelCloseZ= WIDGET_TEXT(bSelZ, VALUE='0.0', XSIZE=7, SCR_XSIZE=40, FONT=font1, /EDITABLE)
  bSelPos=WIDGET_BASE(b2_2,/ROW)
  txtSelNpos= WIDGET_TEXT(bSelPos, VALUE='1', XSIZE=5, SCR_XSIZE=25, FONT=font1, /EDITABLE)
  cw_SelPos=CW_BGROUP(bSelPos, ['first','mid','last'], /EXCLUSIVE, FRAME=1, FONT=font1, ROW=1, SPACE=-2, YPAD=0, SET_VALUE=0, UVALUE='cw_selPos')
  lblSelPos=WIDGET_LABEL(bSelPos, VALUE='images', FONT=font1)

  b2_btm=WIDGET_BASE(b2, /COLUMN)
  btnTestSel=WIDGET_BUTTON(b2_btm, VALUE='Test selection rules on current series', FONT=font1, UVALUE='testSel')
  btnPushSel=WIDGET_BUTTON(b2_btm, VALUE='>> Add to list using selection rules for all selected series', FONT=font1, UVALUE='pushSel')

  b4=WIDGET_BASE(bBottom, /COLUMN)
  b4_top=WIDGET_BASE(b4,/ROW)
  lbl4t=WIDGET_LABEL(b4_top, VALUE='', XSIZE=100)
  btnDesel=WIDGET_BUTTON(b4_top, VALUE='Remove selected from list', UVALUE='removeSel', FONT=font1)
  btnClearSel=WIDGET_BUTTON(B4_top, VALUE='Clear list', UVALUE='clearSel', FONT=font1)
  
  b4ml=WIDGET_LABEL(b4, VALUE='', YSIZE=80)
  b_btm=WIDGET_BASE(b4,/ROW)
  lbl4=WIDGET_LABEL(b_btm, VALUE='', XSIZE=250)
  btnClose=WIDGET_BUTTON(b_btm, VALUE='Cancel', UVALUE='closeSelim', XSIZE=100, FONT=font1)
  btnOpen=WIDGET_BUTTON(b_btm, VALUE='Open selected', UVALUE='openSelim', XSIZE=100, FONT=font1)

  WIDGET_CONTROL, selImgbox, /REALIZE
  XMANAGER, 'selectImages', selImgbox
  DEVICE, RETAIN=2, DECOMPOSED=0

end

pro selectImages_event, event

  COMMON SELIM
  COMMON SELIM_IQ
  COMPILE_OPT hidden

  evTop=event.Top
  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'selectFolder':BEGIN
        ;search all images
        adr=DIALOG_PICKFILE(TITLE='Select the folder', PATH=defPathS, GET_PATH=defPathS, DIALOG_PARENT=evTop, /DIRECTORY)
        WIDGET_CONTROL, txtBrowse, SET_VALUE=adr(0)
        WIDGET_CONTROL, /HOURGLASS
        adr=adr(0)
        IF adr NE '' THEN BEGIN
          WIDGET_CONTROL, lblProgressSelIm, SET_VALUE='Searching for files'
          structImgsSelIm=!Null
          dirs=''
          Spawn, 'dir "'+adr(0)+'"* /b /s /a-D', adrTempTemp
          IF adrTempTemp(0) NE '' THEN BEGIN
            nFound=N_ELEMENTS(adrTempTemp)
            dcmOk=INTARR(nFound)
            dcmAdr=''
            adrTempTemp=adrTempTemp(sort(adrTempTemp))
            FOR d=0, nFound-1 DO BEGIN
              IF adrTempTemp(d) EQ 'DICOMDIR' THEN BEGIN
                dcmOk(d)=0 ; IDL crash if QUERY_DICOM on DICOMDIR - unknown reason
              ENDIF ELSE BEGIN
                ;adrTempTemp(d)=dirs(i)+adrTempTemp(d)
                dcmOk(d)=QUERY_DICOM(adrTempTemp(d))
              ENDELSE
            ENDFOR

            dcmOkId=WHERE(dcmOk EQ 1)
            IF dcmOkId(0) NE -1 THEN dcmAdr=[dcmAdr, adrTempTemp(dcmOkId)]
          ENDIF

          nFiles=N_ELEMENTS(dcmAdr)-1
          structImgsAll=CREATE_STRUCT('empty',0)
          IF nFiles GT 0 THEN BEGIN
            dcmAdr=dcmAdr[1:nFiles]

            ;load headerinfo into structure
            nFiles=n_elements(dcmAdr)
            counter=0
            errLogg=''

            FOR i=0, nFiles-1 DO BEGIN
              WIDGET_CONTROL, lblProgressSelIm, SET_VALUE='Loading file info: '+STRING(i*100./nFiles, FORMAT='(i0)')+' %'
              structNew=readImgInfo(dcmAdr(i), evTop, silent)
              IF SIZE(structNew, /TNAME) EQ 'STRUCT' THEN BEGIN
                IF counter EQ 0 THEN BEGIN
                  structImgsAll=CREATE_STRUCT('S0',structNew)
                  counter=counter+1
                ENDIF ELSE BEGIN
                  structImgsAll=CREATE_STRUCT(structImgsAll,'S'+STRING(counter,FORMAT='(i0)'),structNew)
                  counter=counter+1
                ENDELSE
              ENDIF
            ENDFOR

            ;close non image files?
            tags=tag_names(structImgsAll)
            IF tags(0) NE 'EMPTY' THEN BEGIN;any image header in memory
              nImg=N_TAGS(structImgsAll)
              imsz=!Null
              FOR i=0, nImg-1 DO imsz=[imsz,structImgsAll.(i).imagesize(0)]
              closeIds=WHERE(imsz EQ -1)
              IF closeIds(0) NE -1 THEN structImgsAll=removeIDstructstruct(structImgsAll, closeIds)
            ENDIF

            nImg=N_TAGS(structImgsAll)
            ;sort by studydatetime and seriesNmb
            IF nImg GT 0 THEN BEGIN
              
              studydatetimeArr=!Null
              FOR i=0, nImg-1 DO studydatetimeArr=[studydatetimeArr,structImgsAll.(i).studydatetime]

              studiesInList=studydatetimeArr(UNIQ(studydatetimeArr,BSORT(studydatetimeArr)))
              
              sortID=WIDGET_INFO(lstSort, /DROPLIST_SELECT)
         
              FOR d=0, N_ELEMENTS(studiesInList)-1 DO BEGIN
                imgsInStudy=WHERE(studydatetimeArr EQ studiesInList(d), nImgStudy)
                seriesArr=!Null
                
                CASE sortID OF
                  0: FOR i=0, nImgStudy-1 DO seriesArr=[seriesArr,structImgsAll.(imgsInStudy(i)).seriesTime]
                  1: FOR i=0, nImgStudy-1 DO seriesArr=[seriesArr,structImgsAll.(imgsInStudy(i)).acqTime]
                ENDCASE
                
                seriesInList=seriesArr(UNIQ(seriesArr,BSORT(LONG(seriesArr))))
                FOR s=0, N_ELEMENTS(seriesInList)-1 DO BEGIN
                  imgsInSer=WHERE(seriesArr EQ seriesInList(s), nImgSer)
                  IF imgsInSer(0) NE -1 THEN BEGIN
  
                    ;sort by imgNo
                    imgArr=!Null
                    FOR i=0, nImgSer-1 DO imgArr=[imgArr,structImgsAll.(imgsInStudy(imgsInSer(i))).imgNo]
                    imgSorted=UNIQ(imgArr,BSORT(LONG(imgArr)))
                    sortedImg=imgsInSer(imgSorted)
  
                    seriesStruct=CREATE_STRUCT('I0',structImgsAll.(imgsInStudy(sortedImg(0))))
                    IF nImgSer GT 1 THEN BEGIN
                      FOR j=1, nImgSer-1 DO seriesStruct=CREATE_STRUCT(seriesStruct,'I'+STRING(j,FORMAT='(i0)'),structImgsAll.(imgsInStudy((sortedImg(j)))))
                    ENDIF
                    IF N_ELEMENTS(structImgsSelIm) EQ 0 THEN structImgsSelIm=CREATE_STRUCT('S0', seriesStruct) ELSE BEGIN
                      already=N_TAGS(structImgsSelIm)
                      structImgsSelIm=CREATE_STRUCT(structImgsSelIm,'S'+STRING(already,FORMAT='(i0)'), seriesStruct)
                    ENDELSE
                  ENDIF
                ENDFOR
              ENDFOR
              
              
            ENDIF

            structImgsAll=!Null
            serList=getSeriesList(structImgsSelIm)
            imgList=getImgList(structImgsSelIm,0)

            IF serList(0) NE '' THEN WIDGET_CONTROL, lstSer, SET_VALUE=serList, SET_LIST_SELECT=0 ELSE WIDGET_CONTROL, lstSer, SET_VALUE='', SET_LIST_SELECT=0
            IF imgList(0) NE '' THEN WIDGET_CONTROL, lstImg, SET_VALUE=imgList, SET_LIST_SELECT=0 ELSE WIDGET_CONTROL, lstImg, SET_VALUE='', SET_LIST_SELECT=0
            WIDGET_CONTROL, lstSelImg, SET_VALUE=''
            WIDGET_CONTROL, lblProgressSelIm, SET_VALUE=''

          ENDIF ELSE BEGIN
            sv=DIALOG_MESSAGE('Found no valid DICOM files in selected folder(s)', DIALOG_PARENT=evTop)
          ENDELSE

        ENDIF;adr ''

      END
      'serList':BEGIN
        sel=WIDGET_INFO(lstSer, /LIST_SELECT)
        imgList=getImgList(structImgsSelIm,sel(0))
        IF imgList(0) NE '' THEN WIDGET_CONTROL, lstImg, SET_VALUE=imgList, SET_LIST_SELECT=0 ELSE WIDGET_CONTROL, lstImg, SET_VALUE='', SET_LIST_SELECT=0

      END
      'listSort':BEGIN
        IF N_ELEMENTS(structImgsSelIm) GT 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Lazy programmer - reload dataset (browse) to make the new sorting affect the list', DIALOG_PARENT=evTop)
        ENDIF
        END
      'testSel':BEGIN
        WIDGET_CONTROL, cw_typeSelect, GET_VALUE=type
        selSer=WIDGET_INFO(lstSer, /LIST_SELECT)
        nImg=N_TAGS(structImgsSelIm.(selSer(0)))
        
        CASE type OF
          0:BEGIN;select all
            WIDGET_CONTROL, lstImg, SET_LIST_SELECT=INDGEN(nImg)
          END
          1:BEGIN;select on zpos
            WIDGET_CONTROL, txtSelNZ, GET_VALUE=nStr
            nSel=LONG(nStr(0))
            WIDGET_CONTROL, txtSelNZ, SET_VALUE=STRING(nSel, FORMAT='(i0)')
            WIDGET_CONTROL, txtSelCloseZ, GET_VALUE=zStr
            zpos=FLOAT(zStr(0))
            WIDGET_CONTROL, txtSelCloseZ, SET_VALUE=STRING(zpos, FORMAT='(f0.1)')

            zPosAll=FLTARR(nImg)
            FOR i=0, nImg-1 DO zPosAll(i)=structImgsSelIm.(selSer(0)).(i).zPos

            diffZpos=zPosAll-zPos
            absDiffZpos=ABS(diffZpos)
            closestZ=WHERE(absDiffZpos EQ MIN(absDiffZpos))

            first=closestZ(0)-FLOOR(nSel/2)
            If first LT 0 THEN first=0
            last=first+nSel-1
            IF last GT nImg-1 THEN BEGIN
              last=nImg-1
              IF nImg GE nSel THEN first=last-nSel+1
            ENDIF
            WIDGET_CONTROL, lstImg, SET_LIST_SELECT=INDGEN(last-first+1)+first
          END
          2:BEGIN;select on pos
            WIDGET_CONTROL, txtSelNpos, GET_VALUE=nStr
            nSel=LONG(nStr(0))
            WIDGET_CONTROL, txtSelNpos, SET_VALUE=STRING(nSel, FORMAT='(i0)')

            WIDGET_CONTROL, cw_SelPos, GET_VALUE=pos
            firstLast=[0,0]
            CASE pos OF
              0: firstLast=[0,nSel-1]
              1: firstLast=[nImg/2-nSel/2,nImg/2-nSel/2+nSel-1]
              2: firstLast=[nImg-nSel,nImg-1]
            ENDCASE
            IF firstLast(0) LT 0 THEN firstLast(0)=0
            IF firstLast(1) GT nImg-1 THEN firstLast(1)=nImg-1

            WIDGET_CONTROL, lstImg, SET_LIST_SELECT=INDGEN(firstLast(1)-firstLast(0)+1)+firstLast(0)
          END
        ENDCASE
      END
      'pushSel':BEGIN
        WIDGET_CONTROL, cw_typeSelect, GET_VALUE=type
        WIDGET_CONTROL, txtSelNZ, GET_VALUE=nStr
        nSelZ=LONG(nStr(0))
        WIDGET_CONTROL, txtSelNZ, SET_VALUE=STRING(nSelZ, FORMAT='(i0)')
        WIDGET_CONTROL, txtSelCloseZ, GET_VALUE=zStr
        zpos=FLOAT(zStr(0))
        WIDGET_CONTROL, txtSelCloseZ, SET_VALUE=STRING(zpos, FORMAT='(f0.1)')
        WIDGET_CONTROL, txtSelNpos, GET_VALUE=nStr
        nSelP=LONG(nStr(0))
        WIDGET_CONTROL, txtSelNpos, SET_VALUE=STRING(nSelP, FORMAT='(i0)')
        WIDGET_CONTROL, cw_SelPos, GET_VALUE=pos
        
        selSer=WIDGET_INFO(lstSer, /LIST_SELECT)
        FOR i=0, N_ELEMENTS(selSer)-1 DO BEGIN
          nImg=N_TAGS(structImgsSelIm.(selSer(i)))
          CASE type OF
            0:BEGIN;select all
              selImgs=INDGEN(nImg)
            END
            1:BEGIN;select on zpos
              zPosAll=FLTARR(nImg)
              FOR j=0, nImg-1 DO zPosAll(j)=structImgsSelIm.(selSer(i)).(j).zPos
  
              diffZpos=zPosAll-zPos
              absDiffZpos=ABS(diffZpos)
              closestZ=WHERE(absDiffZpos EQ MIN(absDiffZpos))
  
              first=closestZ(0)-FLOOR(nSelZ/2)
              If first LT 0 THEN first=0
              last=first+nSelZ-1
              IF last GT nImg-1 THEN BEGIN
                last=nImg-1
                IF nImg GE nSelZ THEN first=last-nSelZ+1
              ENDIF
              selImgs=INDGEN(last-first+1)+first
            END
            2:BEGIN;select on pos
              firstLast=[0,0]
              CASE pos OF
                0: firstLast=[0,nSelP-1]
                1: firstLast=[nImg/2-nSelP/2,nImg/2-nSelP/2+nSelP-1]
                2: firstLast=[nImg-nSelP,nImg-1]
              ENDCASE
              IF firstLast(0) LT 0 THEN firstLast(0)=0
              IF firstLast(1) GT nImg-1 THEN firstLast(1)=nImg-1
  
              selImgs=INDGEN(firstLast(1)-firstLast(0)+1)+firstLast(0)
            END
          ENDCASE
                   
          IF selImgs(0) NE -1 THEN BEGIN
            IF SIZE(structSelected, /TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(structSelected) ELSE nAlready=0
            FOR j=0, N_ELEMENTS(selImgs)-1 DO structSelected=CREATE_STRUCT(structSelected, 'I'+STRING(j+nAlready,FORMAT='(i0)'), structImgsSelIm.(selSer(i)).(selImgs(j)))
          ENDIF
          
         ENDFOR
         
         selList=getSelList(structSelected)
         IF selList(0) NE '' THEN WIDGET_CONTROL, lstSelImg, SET_VALUE=selList ELSE WIDGET_CONTROL, lstImg, SET_VALUE=''
      END
      'addSel':BEGIN
        selSer=WIDGET_INFO(lstSer, /LIST_SELECT)
        selImgs=WIDGET_INFO(lstImg, /LIST_SELECT)
        
        IF SIZE(structSelected, /TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(structSelected) ELSE nAlready=0
        IF selSer(0) NE -1 THEN BEGIN
          selSer=selSer(0)
          IF selImgs(0) NE -1 THEN BEGIN
            FOR i=0, N_ELEMENTS(selImgs)-1 DO structSelected=CREATE_STRUCT(structSelected, 'I'+STRING(i+nAlready,FORMAT='(i0)'), structImgsSelIm.(selSer).(selImgs(i)))
          ENDIF
        ENDIF

        selList=getSelList(structSelected)
        IF selList(0) NE '' THEN WIDGET_CONTROL, lstSelImg, SET_VALUE=selList ELSE WIDGET_CONTROL, lstImg, SET_VALUE=''

      END
      'removeSel':BEGIN
        selImgs=WIDGET_INFO(lstSelImg, /LIST_SELECT)
        IF selImgs(0) NE -1 THEN BEGIN
          IF N_ELEMENTS(selImgs) EQ N_TAGS(structSelected) THEN BEGIN
            structSelected=!Null
            WIDGET_CONTROL, lstSelImg, SET_VALUE=''
          ENDIF ELSE BEGIN
            structSelected=removeIDstructstruct(structSelected, selImgs)
            selList=getSelList(structSelected)
            WIDGET_CONTROL, lstSelImg, SET_VALUE=selList
          ENDELSE 
        ENDIF
      END
      'clearSel': BEGIN
        structSelected=!Null
        WIDGET_CONTROL, lstSelImg, SET_VALUE=''
        END
      'openSelim':BEGIN
        IF SIZE(structSelected, /TNAME) EQ 'STRUCT' THEN BEGIN
          nSel=N_TAGS(structSelected)
          selAdr=STRARR(nSel)
          FOR i=0, nSel-1 DO selAdr[i]=structSelected.(i).filename
        ENDIF
        WIDGET_CONTROL, event.top, /DESTROY
      END
      'closeSelim': WIDGET_CONTROL, event.top, /DESTROY
      ELSE:
    ENDCASE
  ENDIF


end