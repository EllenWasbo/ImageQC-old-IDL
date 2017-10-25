pro imgQCmovie, vol, progPath, windowLevel, colTab, GROUP_LEADER = bMain

  COMMON MOVIE, txtFrameDur, volM, drawMov, progPathM, colTabM, continDraw,frameDur, szVol, winLev, viewPlane, lblImgNo

  COMPILE_OPT hidden

  continDraw=0
  frameDur=0.1
  
  volM=vol
  progPathM=progPath
  colTabM=colTab
  szVol=SIZE(volM,/DIMENSIONS)
  winLev=windowLevel
  maxSz=max(szVol[0:1])
  viewPlane=[0.,0.,maxSz,maxSz]

  moviebox = WIDGET_BASE(TITLE='ImageQC_movie', GROUP_LEADER=bMain,  $
    /COLUMN, XSIZE=600, YSIZE=600, XOFFSET=150, YOFFSET=150, /MODAL)

  ml1=WIDGET_LABEL(moviebox, VALUE='', YSIZE=20)
  bImgNo=WIDGET_BASE(moviebox, /ROW)
  lblImgNo0=WIDGET_LABEL(bImgNo, VALUE='Image no. ')
  lblImgNo=WIDGET_LABEL(bImgNo, VALUE='0', /DYNAMIC_RESIZE)
  drawMov = WIDGET_DRAW(moviebox, XSIZE=500, YSIZE=500, GRAPHICS_LEVEL=2, RETAIN=2, SENSITIVE=0)

  bBottom=WIDGET_BASE(moviebox, /ROW)
  lblFrameDur=WIDGET_LABEL(bBottom, VALUE='Frame duration:')
  txtFrameDur=WIDGET_TEXT(bBottom, VALUE=STRING(frameDur,FORMAT='(f0.1)'),XSIZE=5, /EDITABLE)
  lblFrameDur2=WIDGET_LABEL(bBottom, VALUE='sec')
  btnPlay=WIDGET_BUTTON(bBottom,VALUE=progPath+'images\play.bmp',/BITMAP, UVALUE='playMov')
  ;btnStop=WIDGET_BUTTON(bBottom,VALUE=progPath+'images\stop.bmp',/BITMAP, UVALUE='stopMov')
  mlBottom=WIDGET_LABEL(bBottom, VALUE='', XSIZE=20)
  btnClose=WIDGET_BUTTON(bBottom, VALUE='Close window', UVALUE='closeMov', XSIZE=110)

  loadct, 0, /SILENT
  WIDGET_CONTROL, moviebox, /REALIZE
  XMANAGER, 'imgQCmovie', moviebox
  DEVICE, RETAIN=2, DECOMPOSED=0

end

pro imgQCmovie_event, event

  COMMON MOVIE

  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'playMov':BEGIN
        continDraw=1
        WIDGET_CONTROL,txtFrameDur,GET_VALUE=newFrameDur
        frameDur=FLOAT(newFrameDur(0))
        oPalette=OBJ_NEW('IDLgrPalette')
        IF colTabM EQ 0 THEN oPalette->LoadCT, 0 ELSE oPalette->LoadCT, colTabM, FILE=progPathM+'data\colorsImageQC.tbl'
        
        WIDGET_CONTROL, drawMov, GET_VALUE=iDrawMov

        FOR i=0, szVol(2)-1 DO BEGIN
          oModel=OBJ_NEW('IDLgrModel')
          oView=OBJ_NEW('IDLgrView', VIEWPLANE_RECT =viewPlane)
          tempAct=adjustWindowLevel(volM[*,*,i], winLev)
          oImage = OBJ_NEW('IDLgrImage', tempAct, PALETTE=oPalette)
          oModel->Add, oImage
          oView->Add,oModel
          iDrawMov->Draw,oView
          OBJ_DESTROY, oView & OBJ_DESTROY, oModel; & OBJ_DESTROY, oImage
          WIDGET_CONTROL, lblImgNo, SET_VALUE=STRING(i+1, FORMAT='(i0)')
          WAIT, framedur
        ENDFOR
      END
      'closeMov': WIDGET_CONTROL, event.top, /DESTROY
      ELSE:
    ENDCASE
  ENDIF

end