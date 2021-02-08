pro editQuickTemp, nImages, nTests, inputMatrix, testinfo, xoff, yoff, GROUP_LEADER = bMain

  COMMON EDITQT, lbl1, lbl2, lbl3, lbl4, lbl5, lbl6, lbl7, lbl8, lbl9, lbl10,  $
    bg1, bg2, bg3, bg4, bg5, bg6, bg7, bg8, bg9, bg10,$
    bgAction, lstAction, btnPrev,btnNext, currPage, maxPage, nMaxPage, nT, nI
  COMMON EDITMULTIMARK, editedMultiMark
  COMPILE_OPT hidden

  editedMultiMark=inputMatrix
  nT=nTests
  nI=nImages
  currPage=0
  maxPage=CEIL(.1*nImages)-1
  nMaxPage=nImages MOD 10
  font1="Tahoma*14"

  editQTbox = WIDGET_BASE(TITLE='Edit QuickTest template', GROUP_LEADER=bMain,  $
    /COLUMN, XSIZE=550, YSIZE=550, XOFFSET=xoff+20, YOFFSET=yoff+20,/TLB_KILL_REQUEST_EVENTS, /MODAL)

  lbl=WIDGET_LABEL(editQTbox, VALUE='', YSIZE=5, /NO_COPY)
  lbl=WIDGET_LABEL(editQTbox, VALUE='Tests:', /ALIGN_LEFT, /NO_COPY, FONT=font1)
  FOR i=0, nTests-1 DO lbl =WIDGET_LABEL(editQTbox, VALUE=testinfo(i), /ALIGN_LEFT, /NO_COPY, FONT=font1)
  lbl=WIDGET_LABEL(editQTbox, VALUE='', YSIZE=20, /NO_COPY)
  bHeader=WIDGET_BASE(editQTbox, /ROW)
  lbl=WIDGET_LABEL(bHeader, VALUE='Active', XSIZE=43, /NO_COPY, FONT=font1)
  lbl=WIDGET_LABEL(bHeader, VALUE='Img #', XSIZE=55, /NO_COPY, FONT=font1)

  FOR i=0, nTests-1 DO lbl =WIDGET_LABEL(bHeader, VALUE=STRING(i+1,FORMAT='(i0)'), XSIZE=40, /NO_COPY, FONT=font1)

  bRows=WIDGET_BASE(editQTbox, /ROW)
  nRows=10
  bgAction=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /EXCLUSIVE, SET_VALUE=0, SPACE=-2, YPAD=0, UVALUE='bgAction')
  
  bImgNo=WIDGET_BASE(bRows, /COLUMN)
  blbl1=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl2=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl3=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl4=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl5=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl6=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl7=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl8=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl9=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  blbl10=WIDGET_BASE(bImgNo, /ROW, YSIZE=20)
  lbl1=WIDGET_LABEL(blbl1, VALUE='', XSIZE=40, FONT=font1)
  lbl2=WIDGET_LABEL(blbl2, VALUE='', XSIZE=40, FONT=font1)
  lbl3=WIDGET_LABEL(blbl3, VALUE='', XSIZE=40, FONT=font1)
  lbl4=WIDGET_LABEL(blbl4, VALUE='', XSIZE=40, FONT=font1)
  lbl5=WIDGET_LABEL(blbl5, VALUE='', XSIZE=40, FONT=font1)
  lbl6=WIDGET_LABEL(blbl6, VALUE='', XSIZE=40, FONT=font1)
  lbl7=WIDGET_LABEL(blbl7, VALUE='', XSIZE=40, FONT=font1)
  lbl8=WIDGET_LABEL(blbl8, VALUE='', XSIZE=40, FONT=font1)
  lbl9=WIDGET_LABEL(blbl9, VALUE='', XSIZE=40, FONT=font1)
  lbl10=WIDGET_LABEL(blbl10, VALUE='', XSIZE=40, FONT=font1)

  bg1=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg1', XSIZE=40)
  bg2=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg2', XSIZE=40)
  bg3=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg3', XSIZE=40)
  bg4=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg4', XSIZE=40)
  bg5=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg5', XSIZE=40)
  bg6=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg6', XSIZE=40)
  bg7=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg7', XSIZE=40)
  bg8=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg8', XSIZE=40)
  bg9=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg9', XSIZE=40)
  bg10=CW_BGROUP(bRows, STRARR(nRows), COLUMN=1, /NONEXCLUSIVE, SET_VALUE=INTARR(nRows), SPACE=-2, YPAD=0, UVALUE='bg10', XSIZE=40)
  IF nT LT 10 THEN BEGIN
    WIDGET_CONTROL, bg10, MAP=0
    IF nT LT 9 THEN BEGIN
      WIDGET_CONTROL, bg9, MAP=0
      IF nT LT 8 THEN BEGIN
        WIDGET_CONTROL, bg8, MAP=0
        IF nT LT 7 THEN BEGIN
          WIDGET_CONTROL, bg7, MAP=0
          IF nT LT 6 THEN BEGIN
            WIDGET_CONTROL, bg6, MAP=0
            IF nT LT 5 THEN BEGIN
              WIDGET_CONTROL, bg5, MAP=0
              IF nT LT 4 THEN BEGIN
                WIDGET_CONTROL, bg4, MAP=0
                IF nT LT 3 THEN BEGIN
                  WIDGET_CONTROL, bg3, MAP=0
                  IF nT LT 2 THEN WIDGET_CONTROL, bg2, MAP=0
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  bAction=WIDGET_BASE(editQTbox, /ROW)
  lbl=WIDGET_LABEL(bAction, VALUE='Action on active row:', /ALIGN_RIGHT, /NO_COPY)
  lstAction=WIDGET_DROPLIST(bAction, VALUE=['Remove active row','Insert empty before active row'], UVALUE='lstAction')
  btnAction=WIDGET_BUTTON(bAction, VALUE='Execute', UVALUE='btnAction', FONT=font1)
  lbl=WIDGET_LABEL(bAction, VALUE='', XSIZE=50)
  bPage=WIDGET_BASE(bAction,/ROW)
  btnPrev=WIDGET_BUTTON(bPage, VALUE='Prev 10', UVALUE='prev10', FONT=font1)
  btnNext=WIDGET_BUTTON(bPage, VALUE='Next 10', UVALUE='next10', FONT=font1)
  
  lbl=WIDGET_LABEL(editQTbox, VALUE='', XSIZE=50)
  bBottom=WIDGET_BASE(editQTbox, /ROW)
  lbl=WIDGET_LABEL(bBottom, VALUE='', XSIZE=20)
  btnSave=WIDGET_BUTTON(bBottom, VALUE='Save', UVALUE='saveQT', FONT=font1)
  btnCancel=WIDGET_BUTTON(bBottom, VALUE='Cancel', UVALUE='cancelQT', FONT=font1)
  lbl=WIDGET_LABEL(bBottom, VALUE='', XSIZE=50)


  newPage, 0, UPDATE=0

  loadct, 0, /SILENT
  WIDGET_CONTROL, editQTbox, /REALIZE
  XMANAGER, 'editQuickTemp', editQTbox
  DEVICE, RETAIN=2, DECOMPOSED=0

end

pro editQuickTemp_event, ev

  COMMON EDITQT
  COMMON EDITMULTIMARK

  COMPILE_OPT hidden

  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'saveQT':BEGIN
        update_matrix

        IF TOTAL(editedMultiMark) EQ 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Nothing selected. Cannot save.',DIALOG_PARENT=ev.Top)
        ENDIF ELSE WIDGET_CONTROL, ev.top, /DESTROY
      END
      'cancelQT': BEGIN
        editedMultiMark=-1
        WIDGET_CONTROL, ev.top, /DESTROY
      END
      'prev10':BEGIN
        IF currPage GT 0 THEN BEGIN
          newpage, -1
        ENDIF
      END
      'next10': BEGIN
        IF currPage LT maxPage THEN BEGIN
          newpage, 1
        ENDIF
      END
      'btnAction':BEGIN
        sel=WIDGET_INFO(lstAction, /DROPLIST_SELECT)
        proceed=1
        IF currPage EQ maxPage THEN BEGIN
          IF sel(0) GE nMaxPage THEN proceed=0
        ENDIF
        IF proceed THEN BEGIN
        CASE sel(0) OF
          0:BEGIN;remove current row
            IF nI NE 1 THEN BEGIN

              WIDGET_CONTROL, bgAction, GET_VALUE=sel
              update_matrix
              id2delete=sel(0)+currPage*10
              IF id2delete EQ 0 THEN newMatrix=editedMultiMark[*,1:nI] ELSE BEGIN
                IF id2delete EQ nI-1 THEN newMatrix=editedMultiMark[*,0:nI-2] ELSE BEGIN
                  newMatrix=INTARR(nT,nI-1)
                  newMatrix[*,0:id2delete-1]=editedMultiMark[*,0:id2delete-1]
                  newMatrix[*,id2delete:nI-2]=editedMultiMark[*,id2delete+1:nI-1]
                ENDELSE
              ENDELSE
              editedMultiMark=newMatrix
              nI=nI-1
              maxPage=nI/10
              nMaxPage=nI MOD 10
              newpage, 0, UPDATE=0
            ENDIF
          END
          1:BEGIN;insert empty before
            WIDGET_CONTROL, bgAction, GET_VALUE=sel
            update_matrix
            id2insert=sel(0)+currPage*10
            newMatrix=INTARR(nT,nI+1)
            IF id2insert EQ 0 THEN newMatrix[*,1:-1]=editedMultiMark ELSE BEGIN
              newMatrix[*,0:id2insert-1]=editedMultiMark[*,0:id2insert-1]
              newMatrix[*,id2insert+1:-1]=editedMultiMark[*,id2insert:-1]
            ENDELSE
            editedMultiMark=newMatrix
            nI=nI+1
            maxPage=nI/10
            nMaxPage=nI MOD 10
            newpage, 0, UPDATE=0
          END
          ELSE:
        ENDCASE
        ENDIF ELSE sv=DIALOG_MESSAGE('The active image selection is out of range.',DIALOG_PARENT=ev.Top)
      END
      ELSE:
    ENDCASE
  ENDIF
  
  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    update_matrix

    IF TOTAL(editedMultiMark) EQ 0 THEN BEGIN
      editedMultiMark=-1
      WIDGET_CONTROL, ev.top, /DESTROY
    ENDIF ELSE BEGIN
      sv=DIALOG_MESSAGE('Save before closing.',/QUESTION, DIALOG_PARENT=ev.Top)
      IF sv EQ 'No' THEN editedMultiMark=-1
       WIDGET_CONTROL, ev.top, /DESTROY
    ENDELSE

  ENDIF

end

;readout from screen and save to matrix editedMulitMark
pro update_matrix
  COMMON EDITQT
  COMMON EDITMULTIMARK

  arrVis=INTARR(10,10)

  ;save current setvalues to matrix
  WIDGET_CONTROL, bg1, GET_VALUE=test1
  WIDGET_CONTROL, bg2, GET_VALUE=test2
  WIDGET_CONTROL, bg3, GET_VALUE=test3
  WIDGET_CONTROL, bg4, GET_VALUE=test4
  WIDGET_CONTROL, bg5, GET_VALUE=test5
  WIDGET_CONTROL, bg6, GET_VALUE=test6
  WIDGET_CONTROL, bg7, GET_VALUE=test7
  WIDGET_CONTROL, bg8, GET_VALUE=test8
  WIDGET_CONTROL, bg9, GET_VALUE=test9
  WIDGET_CONTROL, bg10, GET_VALUE=test10
  arrVis[0,*]=test1
  arrVis[1,*]=test2
  arrVis[2,*]=test3
  arrVis[3,*]=test4
  arrVis[4,*]=test5
  arrVis[5,*]=test6
  arrVis[6,*]=test7
  arrVis[7,*]=test8
  arrVis[8,*]=test9
  arrVis[9,*]=test10

  mapLbls=INTARR(10)+1
  IF currPage EQ maxPage THEN BEGIN
    IF nMaxPage NE 10 THEN mapLbls[nMaxPage:9]=0
  ENDIF

  editedMultiMark[*,currPage*10:currPage*10+TOTAL(mapLbls)-1]=arrVis[0:nT-1,0:TOTAL(mapLbls)-1]
end

;update=1 means run update_matrix which reads the current selections and save to editedMultiMark (the output)
pro newpage, adjust, UPDATE=update
  COMMON EDITQT
  COMMON EDITMULTIMARK

  IF N_ELEMENTS(update) EQ 0 THEN update =1
  IF update THEN update_matrix

  arrVis=INTARR(10,10)
  currPage=currPage+adjust
  mapLbls=INTARR(10)+1
  IF currPage EQ maxPage THEN BEGIN
    IF nMaxPage NE 10 THEN mapLbls[nMaxPage:9]=0
  ENDIF
  arrVis[0:nT-1,0:TOTAL(mapLbls)-1]=editedMultiMark[*,currPage*10:currPage*10+TOTAL(mapLbls)-1]

  ;update img labels
  WIDGET_CONTROL, lbl1, SET_VALUE='img'+STRING(currPage*10+1, FORMAT='(i0)'), MAP=mapLbls(0)
  WIDGET_CONTROL, lbl2, SET_VALUE='img'+STRING(currPage*10+2, FORMAT='(i0)'), MAP=mapLbls(1)
  WIDGET_CONTROL, lbl3, SET_VALUE='img'+STRING(currPage*10+3, FORMAT='(i0)'), MAP=mapLbls(2)
  WIDGET_CONTROL, lbl4, SET_VALUE='img'+STRING(currPage*10+4, FORMAT='(i0)'), MAP=mapLbls(3)
  WIDGET_CONTROL, lbl5, SET_VALUE='img'+STRING(currPage*10+5, FORMAT='(i0)'), MAP=mapLbls(4)
  WIDGET_CONTROL, lbl6, SET_VALUE='img'+STRING(currPage*10+6, FORMAT='(i0)'), MAP=mapLbls(5)
  WIDGET_CONTROL, lbl7, SET_VALUE='img'+STRING(currPage*10+7, FORMAT='(i0)'), MAP=mapLbls(6)
  WIDGET_CONTROL, lbl8, SET_VALUE='img'+STRING(currPage*10+8, FORMAT='(i0)'), MAP=mapLbls(7)
  WIDGET_CONTROL, lbl9, SET_VALUE='img'+STRING(currPage*10+9, FORMAT='(i0)'), MAP=mapLbls(8)
  WIDGET_CONTROL, lbl10, SET_VALUE='img'+STRING(currPage*10+10, FORMAT='(i0)'), MAP=mapLbls(9)

  ;update selections
  WIDGET_CONTROL, bg1, SET_VALUE=arrVis[0,*]
  WIDGET_CONTROL, bg2, SET_VALUE=arrVis[1,*]
  WIDGET_CONTROL, bg3, SET_VALUE=arrVis[2,*]
  WIDGET_CONTROL, bg4, SET_VALUE=arrVis[3,*]
  WIDGET_CONTROL, bg5, SET_VALUE=arrVis[4,*]
  WIDGET_CONTROL, bg6, SET_VALUE=arrVis[5,*]
  WIDGET_CONTROL, bg7, SET_VALUE=arrVis[6,*]
  WIDGET_CONTROL, bg8, SET_VALUE=arrVis[7,*]
  WIDGET_CONTROL, bg9, SET_VALUE=arrVis[8,*]
  WIDGET_CONTROL, bg10, SET_VALUE=arrVis[9,*]

  IF currPage EQ maxPage THEN WIDGET_CONTROL, btnNext, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNext, SENSITIVE=1
  IF currPage EQ 0 THEN WIDGET_CONTROL, btnPrev, SENSITIVE=0 ELSE WIDGET_CONTROL, btnPrev, SENSITIVE=1
end