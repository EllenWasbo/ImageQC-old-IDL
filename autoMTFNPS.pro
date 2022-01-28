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

function getSeriesTable, struct
  serTab=''
  IF SIZE(struct, /TNAME) EQ 'STRUCT' THEN BEGIN
    nSer=N_TAGS(struct)
    IF nSer GT 1 THEN BEGIN
      serTab=STRARR(7,nSer)
      FOR i=0, nSer-1 DO serTab[*,i]=[struct.(i).(0).seriesName,STRING(struct.(i).(0).kVp, FORMAT='(i0)'),STRING(struct.(i).(0).mA,FORMAT='(i0)'),STRING(struct.(i).(0).ExpTime,FORMAT='(i0)'),struct.(i).(0).kernel,struct.(i).(0).acqDate+'/'+STRMID(struct.(i).(0).acqtime,0,6),STRING(N_TAGS(struct.(i)),FORMAT='(i0)')]
    ENDIF ELSE serTab=[struct.(0).(0).seriesName,STRING(struct.(0).(0).kVp, FORMAT='(i0)'),STRING(struct.(0).(0).mA,STRING(struct.(0).(0).ExpTime,FORMAT='(i0)'),FORMAT='(i0)'),struct.(0).(0).kernel,struct.(0).(0).acqDate+'/'+STRMID(struct.(0).(0).acqtime,0,6),STRING(N_TAGS(struct.(0)),FORMAT='(i0)')]
  ENDIF
  return, serTab
end

pro autoMTFNPS, strucParams, xoff, yoff

  COMMON MTFNPS, imgQCstruc, drawMTF, drawNPS, drawMiniMTF, drawMiniNPS, rangeWL, lblProgressMTFNPS, newline, pathMTFNPS, headersMTF, headersNPS, $
    lblInfoMTF1,lblInfoMTF2,lblInfoMTF3,lblInfoNPS1,lblInfoNPS2,lblInfoNPS3,$
    structImgsMTF,structImgsNPS, MTFres, NPSres, wtab, wtabResultMTF, wtabResultNPS, tabMTF, tabNPS, resTabMTF, resTabNPS, drawPlotMTF,drawPlotNPS,$
    imgNoMTF, imgNoNPS, firstImgNoMTF, firstImgNoNPS, $
    posROIMTF, roiMTF, roiNPS, $
    btnPrevMTF, btnNextMTF,btnPrevNPS, btnNextNPS,$
    cw_plotMTF,cw_plotMaterial,$
    txtMinRangeMTF_X,txtMaxRangeMTF_X,txtMinRangeMTF_Y,txtMaxRangeMTF_Y,txtMinRangeNPS_X,txtMaxRangeNPS_X,txtMinRangeNPS_Y,txtMaxRangeNPS_Y

  COMPILE_OPT hidden

  imgQCstruc=strucParams
  structImgsMTF=!Null
  structImgsNPS=!Null
  roiMTF=!Null
  roiNPS=!Null
  MTFres=!Null
  NPSres=!Null
  posROIMTF=!Null
  headersMTF=['dMTF 50%','dMTF 10%','dMTF 2%','gMTF 50%','gMTF 10%','gMTF 2%']
  headersNPS=['Average AUC','Average centroid']
  rangeWL=strucParams.rangeWL
  imgNoMTF=0
  imgNoNPS=0
  firstImgNoMTF=0
  firstImgNONPS=0
  pathMTFNPS=''
  maxSz=300
  viewPlane=[0.,0.,maxSz,maxSz]
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  font0="Tahoma*ITALIC*16"
  font1="Tahoma*14"
  fontTit="Tahoma*ITALIC*BOLD*18"
  if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)

  MTFNPSbox = WIDGET_BASE(TITLE='Automatic analyze of task based MTF and NPS', /COLUMN, XSIZE=1200, YSIZE=820, XOFFSET=xoff, YOFFSET=yoff)

  topBase=WIDGET_BASE(MTFNPSbox,/ROW, XSIZE=1000)
  toolbar=WIDGET_BASE(topBase,/ROW,/TOOLBAR)
  toolOpen=WIDGET_BUTTON(toolbar, VALUE=thisPath+'images\open.bmp',/BITMAP, UVALUE='openMTFNPS', TOOLTIP='Locate folder and read file information to memory')
  toolExp=WIDGET_BUTTON(toolbar, VALUE=thisPath+'images\export.bmp',/BITMAP, UVALUE='exportMTFNPS', TOOLTIP='Export results to text-files')
  lbl = WIDGET_LABEL(toolbar, VALUE='NB - local files quicker accessed than network files', FONT=font1, /NO_COPY)
  lblProgressMTFNPS=WIDGET_LABEL(topBase, VALUE='         ', /DYNAMIC_RESIZE, XSIZE=200)

  wtab=WIDGET_TAB(MTFNPSbox, XSIZE=1150, YSIZE=750, UVALUE='tabMTFNPS')
  bMTF=WIDGET_BASE(wtab, TITLE='MTF', /ROW, UVALUE='tabMTF')
  bNPS=WIDGET_BASE(wtab, TITLE='NPS', /ROW, UVALUE='tabNPS')
  bInfo=WIDGET_BASE(wtab, TITLE='Info',/COLUMN, UVALUE='tabInfo')

  ;MTF base
  bMTF2=WIDGET_BASE(bMTF, /COLUMN)
  drawMiniMTF = WIDGET_DRAW(bMTF2, XSIZE=450, YSIZE=150, GRAPHICS_LEVEL=2, RETAIN=2, SENSITIVE=1,/BUTTON_EVENTS)
  bInfoRowMTF=WIDGET_BASE(bMTF2,/ROW)
  lblInfoMTF1=WIDGET_LABEL(bInfoRowMTF, VALUE='', XSIZE=150, YSIZE=30, SCR_XSIZE=150, SCR_YSIZE=30, FONT=font1)
  lblInfoMTF2=WIDGET_LABEL(bInfoRowMTF,  VALUE='', XSIZE=150, YSIZE=30, SCR_XSIZE=150, SCR_YSIZE=30, FONT=font1)
  lblInfoMTF3=WIDGET_LABEL(bInfoRowMTF,  VALUE='', XSIZE=150, YSIZE=30, SCR_XSIZE=150, SCR_YSIZE=30, FONT=font1)
  bMTFmid=WIDGET_BASE(bMTF2, /COLUMN)
  bPrevNextMTF=WIDGET_BASE(bMTFmid, /ROW)
  lbl=WIDGET_LABEL(bPrevNextMTF, VALUE='', XSIZE=150, /NO_COPY)
  btnPrevMTF=WIDGET_BUTTON(bPrevNextMTF, VALUE=thisPath+'images\shift_left.bmp',/BITMAP,UVALUE='prevMTF',TOOLTIP='Previous image', SENSITIVE=0)
  btnNextMTF=WIDGET_BUTTON(bPrevNextMTF, VALUE=thisPath+'images\shift_right.bmp',/BITMAP,UVALUE='nextMTF',TOOLTIP='Next image',SENSITIVE=0)
  mlMTFprevnext=WIDGET_LABEL(bPrevNextMTF, VALUE='', XSIZE=10)
  btnRemThisMTF=WIDGET_BUTTON(bPrevNextMTF, VALUE=thisPath+'images\delete.bmp',/BITMAP,TOOLTIP='Remove selected image from dataset', FONT=font1, UVALUE='remThisMTF')

  drawMTF = WIDGET_DRAW(bMTF2, XSIZE=450, YSIZE=450, GRAPHICS_LEVEL=2, RETAIN=2)

  bMTF1=WIDGET_BASE(bMTF, /COLUMN)
  btabMTF=WIDGET_BASE(bMTF1, /ROW)
  tabMTF=WIDGET_TABLE(btabMTF, XSIZE=7, COLUMN_LABELS=['Series name','kV','mA','ExpTime','kernel','acqDate/Time','nImg'],ALIGNMENT=1,COLUMN_WIDTHS=[150,50,50,50,100,110,50],/NO_ROW_HEADERS, SCR_XSIZE=580, SCR_YSIZE=190, /ALL_EVENTS, FONT=font1)

  bMoveMTF=WIDGET_BASE(btabMTF,/COLUMN)
  btnRemMTF=WIDGET_BUTTON(bMoveMTF, VALUE=thisPath+'images\delete.bmp',/BITMAP,TOOLTIP='Remove selected series from dataset', FONT=font1, UVALUE='remSeriesMTF')
  btnMoveMTF2NPS=WIDGET_BUTTON(bMoveMTF, VALUE='>> NPS',TOOLTIP='Move selected series to NPS dataset', FONT=font1, UVALUE='moveSeriesMTF2NPS')

  bMTF3=WIDGET_BASE(bMTF1, /ROW)
  bButtonsMTF=WIDGET_BASE(bMTF3, /column)
  lbl=WIDGET_LABEL(bButtonsMTF, VALUE='', YSIZE=15, /NO_COPY)
  btnCalcMTF=WIDGET_BUTTON(bButtonsMTF, VALUE='Calculate MTF', UVALUE='calcMTF',FONT=font1)
  lbl=WIDGET_LABEL(bButtonsMTF, VALUE='', YSIZE=15, /NO_COPY)
  cw_plotMaterial=CW_BGROUP(bButtonsMTF, TRANSPOSE(imgQCstruc.materialTable[0,*]), /EXCLUSIVE, LABEL_TOP='Show material...', /FRAME, UVALUE='cw_material', FONT=font1, COLUMN=1, SPACE=-2, YPAD=0, SET_VALUE=0)
  cw_plotMTF=CW_BGROUP(bButtonsMTF, ['Centered xy profiles', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTF',FONT=font1, COLUMN=1, SPACE=-2, YPAD=0, SET_VALUE=3)

  wtabResultMTF=WIDGET_TAB(bMTF3, XSIZE=550, YSIZE=490)
  bTableMTF=WIDGET_BASE(wtabResultMTF, TITLE='Results table', /COLUMN, UVALUE='tabResMTF')
  bPlotResMTF=WIDGET_BASE(wtabResultMTF, TITLE='Results plot', /COLUMN, UVALUE='tabPlotMTF')

  toolbarTableMTF=WIDGET_BASE(bTableMTF,/ROW,/TOOLBAR)
  toolCopyTblMTF=WIDGET_BUTTON(toolbarTableMTF, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard', UVALUE='copyTbl')
  resTabMTF=WIDGET_TABLE(bTableMTF, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], /NO_ROW_HEADERS, SCR_XSIZE=420, SCR_YSIZE=300, /ALL_EVENTS, FONT=font1, ALIGNMENT=1)

  toolbarPlot=WIDGET_BASE(bPlotResMTF,/ROW,/TOOLBAR)
  toolCopyCurve=WIDGET_BUTTON(toolbarPlot, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy curve to clipboard', UVALUE='copyCurve')
  tooliPlot=WIDGET_BUTTON(toolbarPlot, VALUE='iPlot', TOOLTIP='Send curves to separate window with save and edit options', UVALUE='iPlot',FONT=font1)
  toolHintPlot=WIDGET_LABEL(toolbarPlot, VALUE='', /DYNAMIC_RESIZE, FONT=font1)
  bDrawPlot=WIDGET_BASE(bPlotResMTF, /ROW)
  drawPlotMTF  = WIDGET_WINDOW(bDrawPlot, XSIZE=500, YSIZE=385, GRAPHICS_LEVEL=2); hvis object graphics

  bRangeX=WIDGET_BASE(bPlotResMTF, /ROW)
  lbl = WIDGET_LABEL(bRangeX, VALUE='Horizontal axis range (lower, upper)', XSIZE=200,FONT=font1, /NO_COPY)
  txtMinRangeMTF_X = WIDGET_TEXT(bRangeX, VALUE='0', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bRangeX, VALUE=', ', XSIZE=10, FONT=font1, /NO_COPY)
  txtMaxRangeMTF_X = WIDGET_TEXT(bRangeX, VALUE='1', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  setRangeMinMaxX = WIDGET_BUTTON(bRangeX, VALUE='Set to default min/max', UVALUE='setRangeMinMaxX',FONT=font1)

  bRangeY=WIDGET_BASE(bPlotResMTF, /ROW)
  lbl = WIDGET_LABEL(bRangeY, VALUE='Vertical axis range (lower, upper)', XSIZE=200,FONT=font1, /NO_COPY)
  txtMinRangeMTF_Y = WIDGET_TEXT(bRangeY, VALUE='0', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bRangeY, VALUE=', ', XSIZE=10, FONT=font1, /NO_COPY)
  txtMaxRangeMTF_Y = WIDGET_TEXT(bRangeY, VALUE='1', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  setRangeMinMaxY = WIDGET_BUTTON(bRangeY, VALUE='Set to default min/max', UVALUE='setRangeMinMaxY',FONT=font1)

  ;NPS base
  bNPS2=WIDGET_BASE(bNPS, /COLUMN)
  drawMiniNPS = WIDGET_DRAW(bNPS2, XSIZE=450, YSIZE=150, GRAPHICS_LEVEL=2, RETAIN=2, SENSITIVE=1,/BUTTON_EVENTS)
  bInfoRowNPS=WIDGET_BASE(bNPS2,/ROW)
  lblInfoNPS1=WIDGET_LABEL(bInfoRowNPS, VALUE='', XSIZE=150, YSIZE=30, SCR_XSIZE=150, SCR_YSIZE=30, FONT=font1)
  lblInfoNPS2=WIDGET_LABEL(bInfoRowNPS,  VALUE='', XSIZE=150, YSIZE=30, SCR_XSIZE=150, SCR_YSIZE=30, FONT=font1)
  lblInfoNPS3=WIDGET_LABEL(bInfoRowNPS,  VALUE='', XSIZE=150, YSIZE=30, SCR_XSIZE=150, SCR_YSIZE=30, FONT=font1)
  bNPSmid=WIDGET_BASE(bNPS2, /COLUMN)
  bPrevNextNPS=WIDGET_BASE(bNPSmid, /ROW)
  lbl=WIDGET_LABEL(bPrevNextNPS, VALUE='', XSIZE=150, /NO_COPY)
  btnPrevNPS=WIDGET_BUTTON(bPrevNextNPS, VALUE=thisPath+'images\shift_left.bmp',/BITMAP,UVALUE='prevNPS',TOOLTIP='Previous image', SENSITIVE=0)
  btnNextNPS=WIDGET_BUTTON(bPrevNextNPS, VALUE=thisPath+'images\shift_right.bmp',/BITMAP,UVALUE='nextNPS',TOOLTIP='Next image',SENSITIVE=0)
  lbl = WIDGET_LABEL(bPrevNextNPS, VALUE='', XSIZE=10, /NO_COPY)
  btnRemThisNPS=WIDGET_BUTTON(bPrevNextNPS, VALUE=thisPath+'images\delete.bmp',/BITMAP,TOOLTIP='Remove selected image from dataset', FONT=font1, UVALUE='remThisNPS')

  drawNPS = WIDGET_DRAW(bNPS2, XSIZE=450, YSIZE=450, GRAPHICS_LEVEL=2, RETAIN=2)

  bNPS1=WIDGET_BASE(bNPS, /COLUMN)
  btabNPS=WIDGET_BASE(bNPS1, /ROW)
  tabNPS=WIDGET_TABLE(btabNPS, XSIZE=7, COLUMN_LABELS=['Series name','kV','mA','ExpTime','kernel','acqDate/Time','nImg'],ALIGNMENT=1,COLUMN_WIDTHS=[150,50,50,100,110,50],/NO_ROW_HEADERS, SCR_XSIZE=580, SCR_YSIZE=190,/ALL_EVENTS, FONT=font1)

  bMoveNPS=WIDGET_BASE(btabNPS,/COLUMN)
  btnRemNPS=WIDGET_BUTTON(bMoveNPS, VALUE=thisPath+'images\delete.bmp',/BITMAP,TOOLTIP='Remove selected series from dataset', FONT=font1, UVALUE='remSeriesNPS')
  btnMoveNPS2MTF=WIDGET_BUTTON(bMoveNPS, VALUE='>> MTF',TOOLTIP='Move selected series to NPS dataset', FONT=font1, UVALUE='moveSeriesNPS2MTF')

  bNPS3=WIDGET_BASE(bNPS1, /ROW)
  bButtonsNPS=WIDGET_BASE(bNPS3, /column)
  lbl = WIDGET_LABEL(bButtonsNPS, VALUE='', YSIZE=15, /NO_COPY)
  btnCalcNPS=WIDGET_BUTTON(bButtonsNPS, VALUE='Calculate NPS', UVALUE='calcNPS',FONT=font1)

  wtabResultNPS=WIDGET_TAB(bNPS3, XSIZE=550, YSIZE=490)
  bTableNPS=WIDGET_BASE(wtabResultNPS, TITLE='Results table', /COLUMN, UVALUE='tabResNPS')
  bPlotResNPS=WIDGET_BASE(wtabResultNPS, TITLE='Results plot', /COLUMN, UVALUE='tabPlotNPS')

  toolbarTableNPS=WIDGET_BASE(bTableNPS,/ROW,/TOOLBAR)
  toolCopyTblNPS=WIDGET_BUTTON(toolbarTableNPS, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard', UVALUE='copyTbl')
  resTabNPS=WIDGET_TABLE(bTableNPS, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], /NO_ROW_HEADERS, SCR_XSIZE=420, SCR_YSIZE=300, /ALL_EVENTS, FONT=font1, ALIGNMENT=1)

  toolbarPlotNPS=WIDGET_BASE(bPlotResNPS,/ROW,/TOOLBAR)
  toolCopyCurveNPS=WIDGET_BUTTON(toolbarPlotNPS, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy curve to clipboard', UVALUE='copyCurve')
  tooliPlotNPS=WIDGET_BUTTON(toolbarPlotNPS, VALUE='iPlot', TOOLTIP='Send curves to separate window with save and edit options', UVALUE='iPlot',FONT=font1)
  toolHintPlotNPS=WIDGET_LABEL(toolbarPlotNPS, VALUE='', /DYNAMIC_RESIZE, FONT=font1)
  bDrawPlotNPS=WIDGET_BASE(bPlotResNPS, /ROW)
  drawPlotNPS  = WIDGET_WINDOW(bDrawPlotNPS, XSIZE=500, YSIZE=385, GRAPHICS_LEVEL=2); hvis object graphics

  bRangeNPSX=WIDGET_BASE(bPlotResNPS, /ROW)
  lbl = WIDGET_LABEL(bRangeNPSX, VALUE='Horizontal axis range (lower, upper)', XSIZE=200,FONT=font1, /NO_COPY)
  txtMinRangeNPS_X = WIDGET_TEXT(bRangeNPSX, VALUE='0', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bRangeNPSX, VALUE=', ', XSIZE=10, FONT=font1, /NO_COPY)
  txtMaxRangeNPS_X = WIDGET_TEXT(bRangeNPSX, VALUE='1', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  setRangeMinMaxNPSX = WIDGET_BUTTON(bRangeNPSX, VALUE='Set to default min/max', UVALUE='setRangeMinMaxX',FONT=font1)

  bRangeNPSY=WIDGET_BASE(bPlotResNPS, /ROW)
  lbl = WIDGET_LABEL(bRangeNPSY, VALUE='Vertical axis range (lower, upper)', XSIZE=200,FONT=font1, /NO_COPY)
  txtMinRangeNPS_Y = WIDGET_TEXT(bRangeNPSY, VALUE='0', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bRangeNPSY, VALUE=', ', XSIZE=10, FONT=font1, /NO_COPY)
  txtMaxRangeNPS_Y = WIDGET_TEXT(bRangeNPSY, VALUE='1', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  setRangeMinMaxNPSY = WIDGET_BUTTON(bRangeNPSY, VALUE='Set to default min/max', UVALUE='setRangeMinMaxY',FONT=font1)
  
  ;info tab
  lbl=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='Automated MTF + NPS analyses', /ALIGN_LEFT, FONT=font0,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='Settings from the CT MTF test in the main window will be used, but method circular edge is preset. This will be combined with the material table from the CT number test', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='to calculate task based MTF (MTF for each material). For NPS the settings from the CT NPS test in the main window will be used.', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='Your image data set is supposed to have images with cylindric material inserts for MTF calculations and/or homogeneous images for NPS calculations', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='Select the folder that contain all your images.', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='Then all DICOM files of this folder will be found.', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='All images with imagetype LOCALIZER or OTHER together with non-image files will be ignored.', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='The images will be sorted by study-datetime and series number.', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='With 10 images or less per series the max HU in the first image is found and if > 500 HU it is assumed to be an MTF series, else it will be assumed to be an NPS series.', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='If there are more than 10 images in the series it is assumed that the series might contain more modules ore MTF + NPS in the same series. Then you will be asked to', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='set the z-range for MTF images and the z range for NPS images and optionally reuse this for all series of the same study. Then only the images within these z-ranges', /ALIGN_LEFT, FONT=font1,/NO_COPY)
  lbl=WIDGET_LABEL(bInfo, VALUE='will be used further.', /ALIGN_LEFT, FONT=font1,/NO_COPY)

  loadct, 0, /SILENT
  WIDGET_CONTROL, MTFNPSbox, /REALIZE
  XMANAGER, 'autoMTFNPS', MTFNPSbox
  DEVICE, RETAIN=2, DECOMPOSED=0

end

pro updateMTFNPSroi
  COMPILE_OPT hidden
  COMMON MTFNPS

  curTab=WIDGET_INFO(wtab, /TAB_CURRENT)

  IF curTab EQ 0 THEN BEGIN;MTF
    IF N_ELEMENTS(structImgsMTF) NE 0 THEN BEGIN
      tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
      serNo=tabSel(1)
      radS=imgQCstruc.MTFroisSz
      pix=structImgsMTF.(serNo).(imgNoMTF).pix
      szImg=structImgsMTF.(serNo).(imgNoNPS).imageSize
      radS=ROUND(FLOAT(radS(0))/pix(0)); assume x,y pix equal ! = normal
      IF N_ELEMENTS(posROIMTF) EQ 0 THEN BEGIN
        linTable=imgQCstruc.materialTable
        posTab=FLOAT(linTable[1:2,*])
        posTab[0,*]=ROUND(posTab[0,*]/pix(0)) & posTab[1,*]=ROUND(posTab[1,*]/pix(1))
      ENDIF ELSE BEGIN
        posTab=posROIMTF.(serNo)*pix(0)
        posTab[0,*]=posTab[0,*]-szImg(0)/2
        posTab[1,*]=posTab[1,*]-szImg(1)/2
      ENDELSE
      roiMTF=getSampleRois(szImg, [0,0,0,0], radS, posTab)
    ENDIF
  ENDIF ELSE BEGIN;NPS
    IF N_ELEMENTS(structImgsNPS) NE 0 THEN BEGIN
      tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
      serNo=tabSel(1)
      pix=structImgsNPS.(serNo).(imgNoNPS).pix
      szImg=structImgsNPS.(serNo).(imgNoNPS).imageSize
      roiNPS=getNPSrois(szImg, [0,0], imgQCstruc.NPSroiSzInt, ROUND(imgQCstruc.NPSdist/pix(0)), imgQCstruc.NPSsubNN)
    ENDIF
  ENDELSE

end

pro redrawSeries
  COMPILE_OPT hidden
  COMMON MTFNPS

  curTab=WIDGET_INFO(wtab, /TAB_CURRENT)
  IF curTab EQ 0 THEN BEGIN
    WIDGET_CONTROL, drawMiniMTF, GET_VALUE = drawid
    structImgsCurr=structImgsMTF
    tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
    firstImgNoCurr=firstImgNoMTF
    imgNoCurr=imgNoMTF
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, drawMiniNPS, GET_VALUE = drawid
    structImgsCurr=structImgsNPS
    tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
    firstImgNoCurr=firstImgNoNPS
    imgNoCurr=imgNoNPS
  ENDELSE

  IF N_ELEMENTS(structImgsCurr) NE 0 THEN BEGIN

    serNo=tabSel(1)

    nImgSer=N_TAGS(structImgsCurr.(serNo))
    thisImg1=readImg(structImgsCurr.(serNo).(firstImgNoCurr).filename, -1)
    szImg=SIZE(thisImg1, /DIMENSIONS)
    combImg=FLTARR(3*szImg(0),szImg(1))
    combImg[0:szImg(0)-1,*]=thisImg1
    IF nImgSer GE 2 THEN BEGIN
      thisImg2=readImg(structImgsCurr.(serNo).(firstImgNoCurr+1).filename, -1)
      combImg[szImg(0):2*szImg(0)-1,*]=thisImg2
      IF nImgSer GE 3 THEN BEGIN
        thisImg3=readImg(structImgsCurr.(serNo).(firstImgNoCurr+2).filename, -1)
        combImg[2*szImg(0):3*szImg(0)-1,*]=thisImg3
      ENDIF
    ENDIF

    ;display Img
    combImg=adjustWindowLevel(combImg, rangeWL)

    oModel=OBJ_NEW('IDLgrModel')
    oView=OBJ_NEW('IDLgrView', VIEWPLANE_RECT =[0.,0.,3*szImg(0),szImg(1)])

    oPaletteCT=OBJ_NEW('IDLgrPalette') & oPaletteCT->LoadCT, 0
    oImageCT = OBJ_NEW('IDLgrImage', combImg, PALETTE=oPaletteCT)
    oModel->Add, oImageCT

    IF imgNoCurr GE firstImgNoCurr AND imgNoCurr LE firstImgNoCurr+2 THEN BEGIN;show selection
      ROI=0*combImg
      marg=ROUND(.03*szImg(0))
      diff=imgNoCurr-firstImgNoCurr
      ROI[marg+diff*szImg(0):szImg(0)-marg-1+diff*szImg(0),marg:szImg(1)-marg-1]=1

      contour0=OBJ_NEW('IDLgrContour',ROI,COLOR=[255,0,0], C_VALUE=0.5, N_LEVELS=1)
      oModel->Add, contour0
    ENDIF

    imgNos=INTARR(3)-1
    zPoss=FLTARR(3)
    info1=''
    info2=''
    info3=''
    nImgCurr=N_TAGS(structImgsCurr.(serNo))
    FOR i=0,2 DO BEGIN
      IF firstImgNoCurr+i LT nImgCurr THEN BEGIN
        imgNos(i)=structImgsCurr.(serNo).(firstImgNoCurr+i).imgNo
        zPoss(i)=structImgsCurr.(serNo).(firstImgNoCurr+i).zPos
        CASE i OF 
          0: info1= 'Img no. '+STRING(imgNos(0),FORMAT='(i0)')+newline+'Z pos = '+STRING(zPoss(0),FORMAT='(f0.1)')
          1: info2= 'Img no. '+STRING(imgNos(1),FORMAT='(i0)')+newline+'Z pos = '+STRING(zPoss(1),FORMAT='(f0.1)')
          2: info3= 'Img no. '+STRING(imgNos(2),FORMAT='(i0)')+newline+'Z pos = '+STRING(zPoss(2),FORMAT='(f0.1)')
        ENDCASE
      ENDIF
    ENDFOR
    ;oTextNo = OBJ_NEW('IDLgrText', STRING(imgNos, FORMAT='(i0)'), LOCATIONS =TRANSPOSE([[[0,1,2]*szImg(0)+.05*szImg(0)],[INTARR(3)+.05*szImg(0)]]), COLOR = 255*([1,0,0]))
    ;oModel->Add, oTextNo
    IF curTab EQ 0 THEN BEGIN
      WIDGET_CONTROL, lblInfoMTF1, SET_VALUE=info1
      WIDGET_CONTROL, lblInfoMTF2, SET_VALUE=info2
      WIDGET_CONTROL, lblInfoMTF3, SET_VALUE=info3
    ENDIF ELSE BEGIN
      WIDGET_CONTROL, lblInfoNPS1, SET_VALUE=info1
      WIDGET_CONTROL, lblInfoNPS2, SET_VALUE=info2
      WIDGET_CONTROL, lblInfoNPS3, SET_VALUE=info3
    ENDELSE

    oView->Add,oModel

    drawid->Draw,oView

    OBJ_DESTROY, oView & OBJ_DESTROY, oModel & OBJ_DESTROY, oImageCT
    structImgsCurr=!Null
  ENDIF ELSE drawid.erase
end

pro redrawImgMTFNPS
  COMPILE_OPT hidden
  COMMON MTFNPS

  updateMTFNPSroi

  curTab=WIDGET_INFO(wtab, /TAB_CURRENT)
  IF curTab EQ 0 THEN BEGIN
    WIDGET_CONTROL, drawMTF, GET_VALUE = drawid
    structImgsCurr=structImgsMTF
    tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
    firstImgNoCurr=firstImgNoMTF
    imgNoCurr=imgNoMTF
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, drawNPS, GET_VALUE = drawid
    structImgsCurr=structImgsNPS
    tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
    firstImgNoCurr=firstImgNoNPS
    imgNoCurr=imgNoNPS
  ENDELSE

  IF N_ELEMENTS(structImgsCurr) NE 0 THEN BEGIN
    serNo=tabSel(1)
    thisImg=readImg(structImgsCurr.(serNo).(imgNoCurr).filename, -1)
    thisImg=adjustWindowLevel(thisImg, rangeWL)
    szImg=SIZE(thisImg, /DIMENSIONS)

    oModel=OBJ_NEW('IDLgrModel')
    oView=OBJ_NEW('IDLgrView', VIEWPLANE_RECT =[0.,0.,szImg(0),szImg(0)])

    oPaletteCT=OBJ_NEW('IDLgrPalette') & oPaletteCT->LoadCT, 0
    oImageCT = OBJ_NEW('IDLgrImage', thisImg, PALETTE=oPaletteCT)
    oModel->Add, oImageCT

    IF curTab EQ 0 THEN BEGIN;MTF ROIs
      searchROIall=TOTAL(roiMTF,3)
      contour0=OBJ_NEW('IDLgrContour',searchROIall,COLOR=255*([1,0,0]), C_VALUE=0.5, N_LEVELS=1)
      oModel->Add, Contour0

      ; material names
      linTable=imgQCstruc.materialTable
      szT=SIZE(linTable, /DIMENSIONS)
      poss=INTARR(2,szT(1))
      FOR ig=0, szT(1)-1 DO poss[*,ig]=ROUND(centroid(roiMTF[*,*,ig],0.5))
      oTextLin = OBJ_NEW('IDLgrText', TRANSPOSE(linTable[0,*]), LOCATIONS =poss, COLOR = 255*([1,0,0]))
      oModel->Add, oTextLin
    ENDIF ELSE BEGIN;NPS rois
      oPaletteROI=OBJ_NEW('IDLgrPalette')
      oPaletteROI->LoadCT, 0
      oImageROIs = OBJ_NEW('IDLgrImage', 50*total(roiNPS,3)+50 , BLEND_FUNCTION = [3, 4], ALPHA_CHANNEL=0.5, PALETTE=oPaletteROI)
      oModel->Add, oImageROIs
    ENDELSE

    oView->Add,oModel
    drawid->Draw,oView

    OBJ_DESTROY, oView & OBJ_DESTROY, oModel & OBJ_DESTROY, oImageCT
  ENDIF ELSE drawid.erase
end

pro copyResTbl, curTab
  COMMON MTFNPS
  COMPILE_OPT hidden

  resTable=''
  IF curTab EQ 0 THEN BEGIN
    IF N_ELEMENTS(MTFres) GT 0 THEN WIDGET_CONTROL, resTabMTF, GET_VALUE=resTable
    headers=headersMTF;['dMTF 50%','dMTF 10%','dMTF 2%','gMTF 50%','gMTF 10%','gMTF 2%']
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(NPSres) GT 0 THEN WIDGET_CONTROL, resTabNPS, GET_VALUE=resTable
    headers=headersNPS;['Average AUC','Average centroid']
  ENDELSE

  IF N_ELEMENTS(resTable) GT 1 THEN BEGIN
    szT=SIZE(resTable, /DIMENSIONS)
    IF imgQCstruc.deciMark EQ ',' THEN BEGIN
      IF N_ELEMENTS(szT) EQ 2 THEN BEGIN
        FOR i=0, szT(0)-1 DO BEGIN
          FOR j=0, szT(1)-1 DO BEGIN
            resTable[i,j]=STRJOIN(STRSPLIT(resTable[i,j], '.',/EXTRACT),',')
          ENDFOR
        ENDFOR
      ENDIF ELSE BEGIN
        FOR i=0, szT(0)-1 DO resTable[i]=STRJOIN(STRSPLIT(resTable[i], '.',/EXTRACT),',')
      ENDELSE
    ENDIF
    IF N_ELEMENTS(szT) EQ 1 THEN szT=[szT(0),1]
    newTable=STRARR(szT(0),szT(1)+1)
    newTable[*,1:szT(1)]=resTable
    newTable[*,0]=headers
    resTable=newTable

    CLIPBOARD.set, STRJOIN(resTable, STRING(9B))
  ENDIF
end

pro autoMTFNPS_event, event

  COMMON MTFNPS
  COMPILE_OPT hidden

  evTop=event.Top
  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'openMTFNPS':BEGIN
        ;search all images
        adr=DIALOG_PICKFILE(TITLE='Select the folder', DIALOG_PARENT=evTop, PATH=defPath, GET_PATH=selPath, /DIRECTORY)

        WIDGET_CONTROL, /HOURGLASS
        adr=adr(0)
        IF adr NE '' THEN BEGIN

          pathMTFNPS=selPath
          WIDGET_CONTROL, lblProgressMTFNPS, SET_VALUE='Searching for files'
          dirs=''
          ;Spawn, 'dir "'+adr(0)+'"* /b /s /a-D', adrTempTemp
          adrTempTemp=FILE_SEARCH(adr(0), '*')
          IF adrTempTemp(0) NE '' THEN BEGIN

            clearMTFNPS

            nFound=N_ELEMENTS(adrTempTemp)
            dcmOk=INTARR(nFound)
            dcmAdr=''
            adrTempTemp=adrTempTemp(sort(adrTempTemp))
            FOR d=0, nFound-1 DO BEGIN
              IF adrTempTemp(d) EQ 'DICOMDIR' OR FILE_TEST(adrTempTemp(d),/DIRECTORY) THEN BEGIN
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
          structImgsMTFNPS=CREATE_STRUCT('empty',0)
          IF nFiles GT 0 THEN BEGIN
            dcmAdr=dcmAdr[1:nFiles]

            ;load headerinfo into structure
            nFiles=n_elements(dcmAdr)
            counter=0
            errLogg=''

            FOR i=0, nFiles-1 DO BEGIN
              WIDGET_CONTROL, lblProgressMTFNPS, SET_VALUE='Loading file info: '+STRING(i*100./nFiles, FORMAT='(i0)')+' %'
              structNew=readImgInfo(dcmAdr(i), evTop, silent)
              IF SIZE(structNew, /TNAME) EQ 'STRUCT' THEN BEGIN
                IF counter EQ 0 THEN BEGIN
                  structImgsMTFNPS=CREATE_STRUCT('S0',structNew)
                  counter=counter+1
                ENDIF ELSE BEGIN
                  structImgsMTFNPS=CREATE_STRUCT(structImgsMTFNPS,'S'+STRING(counter,FORMAT='(i0)'),structNew)
                  counter=counter+1
                ENDELSE
              ENDIF
            ENDFOR

            ;close non image files, topograms and patient protocol
            tags=tag_names(structImgsMTFNPS)
            IF tags(0) NE 'EMPTY' THEN BEGIN;any image header in memory
              nImg=N_TAGS(structImgsMTFNPS)
              imsz=!Null
              FOR i=0, nImg-1 DO BEGIN
                imsz=[imsz,structImgsMTFNPS.(i).imagesize(0)]
                spl=STRSPLIT(structImgsMTFNPS.(i).imagetype,'\',/EXTRACT)
                IF N_ELEMENTS(spl) GE 3 THEN BEGIN
                  IF spl(2) EQ 'LOCALIZER' OR spl(2) EQ 'OTHER' THEN imsz(-1)=-1
                ENDIF
              ENDFOR
              
              closeIds=WHERE(imsz EQ -1)
              IF closeIds(0) NE -1 THEN structImgsMTFNPS=removeIDstructstruct(structImgsMTFNPS, closeIds)       
              
            ENDIF

            ;sort by study-datetime, seriesNmb and MTF/NPS
            nImg=N_TAGS(structImgsMTFNPS)
            IF nImg GT 0 THEN BEGIN

              studydatetimeArr=!Null
              FOR i=0, nImg-1 DO studydatetimeArr=[studydatetimeArr,structImgsMTFNPS.(i).studydatetime]
              studiesInList=studydatetimeArr(UNIQ(studydatetimeArr,BSORT(studydatetimeArr)))

              
              FOR d=0, N_ELEMENTS(studiesInList)-1 DO BEGIN
                imgsInStudy=WHERE(studydatetimeArr EQ studiesInList(d), nImgStudy)
                seriesArr=!Null
                FOR i=0, nImgStudy-1 DO seriesArr=[seriesArr,structImgsMTFNPS.(imgsInStudy(i)).seriesNmb]
                seriesInList=seriesArr(UNIQ(seriesArr,BSORT(LONG(seriesArr))))
                zVals=[0,0,0,0]; MTF start, stopp, NPS start, stopp
                resetZvals=0
                FOR s=0, N_ELEMENTS(seriesInList)-1 DO BEGIN
                  imgsInSer=WHERE(seriesArr EQ seriesInList(s), nImgSer)
                  
                  IF nImgSer GT 10 OR TOTAL(zVals) GT 0 THEN BEGIN; select z-range for MTF and NPS
                    IF TOTAL(zVals) EQ 0 THEN BEGIN
                      allZvals=!Null
                      FOR i=0, nImgSer -1 DO allZvals=[allZvals, structImgsMTFNPS.(imgsInStudy(imgsInSer(i))).zpos]
                      
                      sern=structImgsMTFNPS.(imgsInStudy(imgsInSer(0))).seriesName
                      kern=structImgsMTFNPS.(imgsInStudy(imgsInSer(0))).kernel
               
                      box=[$
                        '1, BASE,, /COLUMN', $
                        '0, LABEL, Series name: '+sern, $
                        '0, LABEL, Kernel: '+kern, $
                        '0, LABEL, ',$
                        '2, LABEL, Found images with z-range '+STRING(MIN(allZvals),FORMAT='(f0.1)')+'  to '+STRING(MAX(allZvals),FORMAT='(f0.1)'), $
                        '1, BASE,, /COLUMN', $
                        '0, LABEL, ',$
                        '2, LABEL, Set z-range for MTF',$
                        '1, BASE,, /ROW', $
                        '0, FLOAT, , LABEL_LEFT=min, TAG=minMTF', $
                        '0, FLOAT, , LABEL_LEFT=max, TAG=maxMTF', $
                        '2, BUTTON, All MTF, TAG=allMTF, QUIT',$
                        '1, BASE,, /COLUMN', $
                        '0, LABEL, ',$
                        '2, LABEL, Set z-range for NPS',$
                        '1, BASE,, /ROW', $
                        '0, FLOAT, , LABEL_LEFT=min, TAG=minNPS', $
                        '0, FLOAT, , LABEL_LEFT=max, TAG=maxNPS', $
                        '2, BUTTON, All NPS, TAG=allNPS, QUIT',$
                        '1, BASE,, /ROW', $
                        '0, BUTTON, Use, QUIT, TAG=OKser',$
                        '0, BUTTON, Use for all series in same study, QUIT, TAG=OKstudy',$
                        '2, BUTTON, Cancel, QUIT, TAG=cancel']
                      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Set z-ranges for MTF and NPS images', XSIZE=300, YSIZE=350, FOCUSNO=3)

                      zVals=[MIN(allZvals),MAX(allZvals),MIN(allZvals)-2,MIN(allZvals)]; = all series, and all images regarded as MTF
                      ;IF res.allMTF THEN zVals=[MIN(allZvals),MAX(allZvals),MIN(allZvals)-2,MIN(allZvals)]               
                      IF res.allNPS THEN zVals=[MIN(allZvals)-2,MIN(allZvals)-1,MIN(allZvals),MAX(allZvals)]
                      IF res.OKser THEN BEGIN
                        zVals=[res.minMTF,res.maxMTF,res.minNPS,res.maxNPS]
                        resetZvals=1
                      ENDIF
                      IF res.OKstudy THEN zVals=[res.minMTF,res.maxMTF,res.minNPS,res.maxNPS]
                      ;IF res.cancel THEN zVals=[MIN(allZvals),MAX(allZvals),MIN(allZvals)-2,MIN(allZvals)]
                      
                    ENDIF
                    
                    ;MTF images
                    ids=WHERE(allZvals GE zVals(0) AND allZvals LE zVals(1))
                    IF ids(0) NE -1 THEN BEGIN
                      seriesStruct=CREATE_STRUCT('I0',structImgsMTFNPS.(imgsInStudy(imgsInSer(ids(0)))))
                      IF N_ELEMENTS(ids) GT 1 THEN BEGIN
                        FOR j=1, N_ELEMENTS(ids)-1 DO seriesStruct=CREATE_STRUCT(seriesStruct,'I'+STRING(j,FORMAT='(i0)'),structImgsMTFNPS.(imgsInStudy(imgsInSer(ids(j)))))
                      ENDIF
                      IF N_ELEMENTS(structImgsMTF) EQ 0 THEN structImgsMTF=CREATE_STRUCT('S0', seriesStruct) ELSE BEGIN
                        already=N_TAGS(structImgsMTF)
                        structImgsMTF=CREATE_STRUCT(structImgsMTF,'S'+STRING(already,FORMAT='(i0)'), seriesStruct)
                      ENDELSE
                    ENDIF
                    
                    ;NPS images
                    ids=WHERE(allZvals GE zVals(2) AND allZvals LE zVals(3))
                    IF ids(0) NE -1 THEN BEGIN
                      seriesStruct=CREATE_STRUCT('I0',structImgsMTFNPS.(imgsInStudy(imgsInSer(ids(0)))))
                      IF N_ELEMENTS(ids) GT 1 THEN BEGIN
                        FOR j=1, N_ELEMENTS(ids)-1 DO seriesStruct=CREATE_STRUCT(seriesStruct,'I'+STRING(j,FORMAT='(i0)'),structImgsMTFNPS.(imgsInStudy(imgsInSer(ids(j)))))
                      ENDIF
                      IF N_ELEMENTS(structImgsNPS) EQ 0 THEN structImgsNPS=CREATE_STRUCT('S0', seriesStruct) ELSE BEGIN
                        already=N_TAGS(structImgsNPS)
                        structImgsNPS=CREATE_STRUCT(structImgsNPS,'S'+STRING(already,FORMAT='(i0)'), seriesStruct)
                      ENDELSE
                    ENDIF
                    
                    IF resetZvals THEN zVals=[0,0,0,0]
                    
                  ENDIF ELSE BEGIN
                    firstImg=readImg(structImgsMTFNPS.(imgsInStudy(imgsInSer(0))).filename, -1)
                    seriesStruct=CREATE_STRUCT('I0',structImgsMTFNPS.(imgsInStudy(imgsInSer(0))))
                    IF nImgSer GT 1 THEN BEGIN
                      FOR j=1, nImgSer-1 DO seriesStruct=CREATE_STRUCT(seriesStruct,'I'+STRING(j,FORMAT='(i0)'),structImgsMTFNPS.(imgsInStudy(imgsInSer(j))))
                    ENDIF
                    IF MAX(firstImg) GT 500 THEN BEGIN ;IF max HU in first image in series > 500 - assume MTF
                      IF N_ELEMENTS(structImgsMTF) EQ 0 THEN structImgsMTF=CREATE_STRUCT('S0', seriesStruct) ELSE BEGIN
                        already=N_TAGS(structImgsMTF)
                        structImgsMTF=CREATE_STRUCT(structImgsMTF,'S'+STRING(already,FORMAT='(i0)'), seriesStruct)
                      ENDELSE
                    ENDIF ELSE BEGIN
                      IF N_ELEMENTS(structImgsNPS) EQ 0 THEN structImgsNPS=CREATE_STRUCT('S0', seriesStruct) ELSE BEGIN
                        already=N_TAGS(structImgsNPS)
                        structImgsNPS=CREATE_STRUCT(structImgsNPS,'S'+STRING(already,FORMAT='(i0)'), seriesStruct)
                      ENDELSE
                    ENDELSE
                  ENDELSE

                ENDFOR
              ENDFOR
            ENDIF

            tNPS=getSeriesTable(structImgsNPS)
            tMTF=getSeriesTable(structImgsMTF)
            sNPS=SIZE(tNPS, /DIMENSIONS)
            sMTF=SIZE(tMTF, /DIMENSIONS)
            IF N_ELEMENTS(sNPS) EQ 1 THEN nNPS=1 ELSE nNPS=sNPS(1)
            IF N_ELEMENTS(sMTF) EQ 1 THEN nMTF=1 ELSE nMTF=sMTF(1)

            IF N_ELEMENTS(tNPS) NE 1 THEN WIDGET_CONTROL, tabNPS, ALIGNMENT=1, TABLE_YSIZE=nNPS, SET_VALUE=tNPS
            IF N_ELEMENTS(tMTF) NE 1 THEN WIDGET_CONTROL, tabMTF, ALIGNMENT=1, TABLE_YSIZE=nMTF, SET_VALUE=tMTF
            WIDGET_CONTROL, lblProgressMTFNPS, SET_VALUE=''

  
            IF SIZE(structImgsMTF, /TNAME) EQ 'STRUCT' THEN BEGIN
              IF N_TAGS(structImgsMTF.(0)) GT 3 THEN WIDGET_CONTROL, btnNextMTF, SENSITIVE=1 ELSE WIDGET_CONTROL, btnNextMTF, SENSITIVE=0
            ENDIF ELSE WIDGET_CONTROL, btnNextMTF, SENSITIVE=0
            IF SIZE(structImgsNPS, /TNAME) EQ 'STRUCT' THEN BEGIN
              IF N_TAGS(structImgsNPS.(0)) GT 3 THEN WIDGET_CONTROL, btnNextNPS, SENSITIVE=1 ELSE WIDGET_CONTROL, btnNextNPS, SENSITIVE=0
            ENDIF ELSE  WIDGET_CONTROL, btnNextNPS, SENSITIVE=0
            WIDGET_CONTROL, btnPrevMTF, SENSITIVE=0
            WIDGET_CONTROL, btnPrevNPS, SENSITIVE=0

            redrawSeries
            redrawImgMTFNPS

          ENDIF ELSE BEGIN
            sv=DIALOG_MESSAGE('Found no valid DICOM files in selected folder(s)', DIALOG_PARENT=evTop)
          ENDELSE

        ENDIF;adr ''

      END
      'prevMTF':BEGIN
        firstImgNoMTF=firstImgNoMTF-1
        IF firstImgNoMTF EQ 0 THEN WIDGET_CONTROL, btnPrevMTF, SENSITIVE=0
        WIDGET_CONTROL, btnNextMTF, SENSITIVE=1
        redrawSeries
      END
      'prevNPS':BEGIN
        firstImgNoNPS=firstImgNoNPS-1
        IF firstImgNoNPS EQ 0 THEN WIDGET_CONTROL, btnPrevNPS, SENSITIVE=0
        WIDGET_CONTROL, btnNextNPS, SENSITIVE=1
        redrawSeries
      END
      'nextMTF':BEGIN
        firstImgNoMTF=firstImgNoMTF+1
        tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
        serNo=tabSel(1)
        WIDGET_CONTROL, tabMTF, GET_VALUE=currTab
        nImgSer=LONG(currTab(6,serNo))
        IF firstImgNoMTF EQ nImgSer-3 THEN WIDGET_CONTROL, btnNextMTF, SENSITIVE=0
        WIDGET_CONTROL, btnPrevMTF, SENSITIVE=1
        redrawSeries
      END
      'nextNPS':BEGIN
        firstImgNoNPS=firstImgNoNPS+1
        tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
        serNo=tabSel(1)
        WIDGET_CONTROL, tabNPS, GET_VALUE=currTab
        nImgSer=LONG(currTab(6,serNo))
        IF firstImgNoNPS EQ nImgSer-3 THEN WIDGET_CONTROL, btnNextNPS, SENSITIVE=0
        WIDGET_CONTROL, btnPrevNPS, SENSITIVE=1
        redrawSeries
      END
      'remThisMTF':BEGIN
        IF N_ELEMENTS(structImgsMTF) GT 0 THEN BEGIN
          tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
          serNo=tabSel(1)
          WIDGET_CONTROL, tabMTF,GET_VALUE=currTab
          newSelect=0
          tags=TAG_NAMES(structImgsMTF)
          thisTag=tags(serNo)
          nImgSer=LONG(currTab(6,serNo))
          IF nImgSer GT 1 THEN BEGIN
            zpos=structImgsMTF.(serNo).(imgNoMTF).zpos
            thisSeries=removeIDstructstruct(structImgsMTF.(serNo), imgNoMTF)
            structImgsMTF=replaceStructStruct(structImgsMTF, thisSeries, serNo)

            ;check if any series with same acqDate, acqTime and zpos - ask if this also should be deleted
            nSer=N_TAGS(structImgsMTF)
            acqdateTime=TRANSPOSE(currTab[5,*])
            sameAcq=WHERE(acqdateTime EQ acqdateTime(serNo))
            sameZ=INTARR(nSer)-1
            IF sameAcq(0) NE -1 THEN BEGIN
              FOR i=0, nSer-1 DO BEGIN
                IF i NE serNo THEN BEGIN
                  FOR n=0, LONG(currTab(6,i))-1 DO IF structImgsMTF.(i).(n).zpos EQ zpos THEN sameZ(i)=n
                ENDIF
              ENDFOR
            ENDIF
            delSameID=WHERE(sameZ GT -1)
            IF delSameID(0) NE -1 GT 0 THEN BEGIN
              sv=DIALOG_MESSAGE('Found images in other series with same acquisition date/time and z-position. Delete these images too?', /QUESTION, DIALOG_PARENT=evTop)
              IF sv EQ 'Yes' THEN BEGIN
                FOR i=0, nSer-1 DO BEGIN
                  IF sameZ(i) NE -1 THEN BEGIN
                    nImgsThis=N_TAGS(structImgsMTF.(i))
                    IF nImgsThis GT 1 THEN BEGIN
                      thisSeries=removeIDstructstruct(structImgsMTF.(i), sameZ(i))
                      structImgsMTF=replaceStructStruct(structImgsMTF, thisSeries, i)
                    ENDIF ELSE structImgsMTF=removeIDstructstruct(structImgsMTF, i)
                  ENDIF
                ENDFOR
              ENDIF
            ENDIF

            firstImgNoMTF=0
            imgNoMTF=0
          ENDIF ELSE BEGIN
            structImgsMTF=removeIDstructstruct(structImgsMTF, serNo)
            newSelect=1
          ENDELSE

          tMTF=getSeriesTable(structImgsMTF)
          sMTF=SIZE(tMTF, /DIMENSIONS)
          IF N_ELEMENTS(sMTF) EQ 1 THEN nMTF=1 ELSE nMTF=sMTF(1)

          IF N_ELEMENTS(tMTF) NE 1 THEN BEGIN

            WIDGET_CONTROL, tabMTF, ALIGNMENT=1, TABLE_YSIZE=nMTF, SET_VALUE=tMTF
            IF newSelect THEN BEGIN
              WIDGET_CONTROL, tabMTF, SET_TABLE_SELECT=[0,0,0,0]
              nImgSer=LONG(tMTF(6,0))
            ENDIF ELSE BEGIN
              tags=TAG_NAMES(structImgsMTF)
              serNo=WHERE(tags EQ thisTag)
              WIDGET_CONTROL, tabMTF, SET_TABLE_SELECT=[0,serNo(0),0,serNo(0)]
              nImgSer=nImgSer-1
            ENDELSE
          ENDIF ELSE nImgSer=LONG(tMTF(6))

          IF firstImgNoMTF GE nImgSer-3 THEN WIDGET_CONTROL, btnNextMTF, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextMTF, SENSITIVE=1
          WIDGET_CONTROL, btnPrevMTF, SENSITIVE=0
          redrawSeries
          redrawImgMTFNPS
        ENDIF
      END
      'remThisNPS':BEGIN
        IF N_ELEMENTS(structImgsNPS) GT 0 THEN BEGIN
          tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
          serNo=tabSel(1)
          WIDGET_CONTROL, tabNPS, GET_VALUE=currTab
          newSelect=0
          tags=TAG_NAMES(structImgsNPS)
          thisTag=tags(serNo)
          nImgSer=LONG(currTab(6,serNo))
          IF nImgSer GT 1 THEN BEGIN
            zpos=structImgsNPS.(serNo).(imgNoNPS).zpos
            thisSeries=removeIDstructstruct(structImgsNPS.(serNo), imgNoNPS)
            structImgsNPS=replaceStructStruct(structImgsNPS, thisSeries, serNo)

            ;check if any series with same acqDate, acqTime and zpos - ask if this also should be deleted
            nSer=N_TAGS(structImgsNPS)
            acqdateTime=TRANSPOSE(currTab[5,*])
            sameAcq=WHERE(acqdateTime EQ acqdateTime(serNo))
            sameZ=INTARR(nSer)-1
            IF sameAcq(0) NE -1 THEN BEGIN
              FOR i=0, nSer-1 DO BEGIN
                IF i NE serNo THEN BEGIN
                  FOR n=0, LONG(currTab(6,i))-1 DO IF structImgsNPS.(i).(n).zpos EQ zpos THEN sameZ(i)=n
                ENDIF
              ENDFOR
            ENDIF
            delSameID=WHERE(sameZ GT -1)
            IF delSameID(0) NE -1 GT 0 THEN BEGIN
              sv=DIALOG_MESSAGE('Found images in other series with same acquisition date/time and z-position. Delete these images too?', /QUESTION, DIALOG_PARENT=evTop)
              IF sv EQ 'Yes' THEN BEGIN
                FOR i=0, nSer-1 DO BEGIN
                  IF sameZ(i) NE -1 THEN BEGIN
                    nImgsThis=N_TAGS(structImgsNPS.(i))
                    IF nImgsThis GT 1 THEN BEGIN
                      thisSeries=removeIDstructstruct(structImgsNPS.(i), sameZ(i))
                      structImgsNPS=replaceStructStruct(structImgsNPS, thisSeries, i)
                    ENDIF ELSE structImgsNPS=removeIDstructstruct(structImgsNPS, i)
                  ENDIF
                ENDFOR
              ENDIF
            ENDIF

            firstImgNoNPS=0
            imgNoNPS=0
          ENDIF ELSE BEGIN
            structImgsNPS=removeIDstructstruct(structImgsNPS, serNo)
            newSelect=1
          ENDELSE

          tNPS=getSeriesTable(structImgsNPS)
          sNPS=SIZE(tNPS, /DIMENSIONS)
          IF N_ELEMENTS(sNPS) EQ 1 THEN nNPS=1 ELSE nNPS=sNPS(1)

          IF N_ELEMENTS(tNPS) NE 1 THEN BEGIN

            WIDGET_CONTROL, tabNPS, ALIGNMENT=1, TABLE_YSIZE=nNPS, SET_VALUE=tNPS
            IF newSelect THEN BEGIN
              WIDGET_CONTROL, tabNPS, SET_TABLE_SELECT=[0,0,0,0]
              nImgSer=LONG(tNPS(6,0))
            ENDIF ELSE BEGIN
              tags=TAG_NAMES(structImgsNPS)
              serNo=WHERE(tags EQ thisTag)
              WIDGET_CONTROL, tabNPS, SET_TABLE_SELECT=[0,serNo(0),0,serNo(0)]
              nImgSer=nImgSer-1
            ENDELSE
          ENDIF ELSE nImgSer=LONG(tNPS(6))

          IF firstImgNoNPS GE nImgSer-3 THEN WIDGET_CONTROL, btnNextNPS, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextNPS, SENSITIVE=1
          WIDGET_CONTROL, btnPrevNPS, SENSITIVE=0
          redrawSeries
          redrawImgMTFNPS
        ENDIF
      END
      'remSeriesMTF':BEGIN
        tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
        serNo=tabSel(1)
        structImgsMTF=removeIDstructstruct(structImgsMTF, serNo)
        tMTF=getSeriesTable(structImgsMTF)
        sMTF=SIZE(tMTF, /DIMENSIONS)
        IF N_ELEMENTS(sMTF) EQ 1 THEN nMTF=1 ELSE nMTF=sMTF(1)

        IF nMTF GT 1 THEN nImgSer=LONG(tMTF(6,0)) ELSE nImgSer=LONG(tMTF(6))
        WIDGET_CONTROL, tabMTF, ALIGNMENT=1, TABLE_YSIZE=nMTF, SET_VALUE=tMTF
        WIDGET_CONTROL, tabMTF, SET_TABLE_SELECT=[0,0,0,0]

        imgNoMTF=0
        firstImgNoMTF=0

        IF firstImgNoMTF GE nImgSer-3 THEN WIDGET_CONTROL, btnNextMTF, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextMTF, SENSITIVE=1
        WIDGET_CONTROL, btnPrevMTF, SENSITIVE=0

        redrawSeries
        redrawImgMTFNPS
      END
      'remSeriesNPS':BEGIN
        tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
        serNo=tabSel(1)
        structImgsNPS=removeIDstructstruct(structImgsNPS, serNo)
        tNPS=getSeriesTable(structImgsNPS)
        sNPS=SIZE(tNPS, /DIMENSIONS)
        IF N_ELEMENTS(sNPS) EQ 1 THEN nNPS=1 ELSE nNPS=sNPS(1)

        IF nNPS GT 1 THEN nImgSer=LONG(tNPS(6,0)) ELSE nImgSer=LONG(tNPS(6))
        WIDGET_CONTROL, tabNPS, ALIGNMENT=1, TABLE_YSIZE=nNPS, SET_VALUE=tNPS
        WIDGET_CONTROL, tabNPS, SET_TABLE_SELECT=[0,0,0,0]

        imgNoNPS=0
        firstImgNoNPS=0

        IF firstImgNoNPS GE nImgSer-3 THEN WIDGET_CONTROL, btnNextNPS, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextNPS, SENSITIVE=1
        WIDGET_CONTROL, btnPrevNPS, SENSITIVE=0

        redrawSeries
        redrawImgMTFNPS
      END
      'moveSeriesMTF2NPS':BEGIN
        tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
        serNo=tabSel(1)
        serToMove=structImgsMTF.(serNo)

        ;add to NPS
        already=N_TAGS(structImgsNPS)
        structImgsNPS=CREATE_STRUCT(structImgsNPS,'S'+STRING(already,FORMAT='(i0)'), serToMove)
        tNPS=getSeriesTable(structImgsNPS)
        sNPS=SIZE(tNPS, /DIMENSIONS)
        IF N_ELEMENTS(sNPS) EQ 1 THEN nNPS=1 ELSE nNPS=sNPS(1)
        IF N_ELEMENTS(tNPS) NE 1 THEN WIDGET_CONTROL, tabNPS, ALIGNMENT=1, TABLE_YSIZE=nNPS, SET_VALUE=tNPS

        ;remove from MTF
        structImgsMTF=removeIDstructstruct(structImgsMTF, serNo)
        tMTF=getSeriesTable(structImgsMTF)
        sMTF=SIZE(tMTF, /DIMENSIONS)
        IF N_ELEMENTS(sMTF) EQ 1 THEN nMTF=1 ELSE nMTF=sMTF(1)

        IF nMTF GT 1 THEN nImgSer=LONG(tMTF(6,0)) ELSE nImgSer=LONG(tMTF(6))
        WIDGET_CONTROL, tabMTF, ALIGNMENT=1, TABLE_YSIZE=nMTF, SET_VALUE=tMTF
        WIDGET_CONTROL, tabMTF, SET_TABLE_SELECT=[0,0,0,0]

        imgNoMTF=0
        firstImgNoMTF=0

        IF firstImgNoMTF GE nImgSer-3 THEN WIDGET_CONTROL, btnNextMTF, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextMTF, SENSITIVE=1
        WIDGET_CONTROL, btnPrevMTF, SENSITIVE=0

        redrawSeries
        redrawImgMTFNPS
      END
      'moveSeriesNPS2MTF':BEGIN
        tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
        serNo=tabSel(1)
        serToMove=structImgsNPS.(serNo)

        ;add to MTF
        already=N_TAGS(structImgsMTF)
        structImgsMTF=CREATE_STRUCT(structImgsMTF,'S'+STRING(already,FORMAT='(i0)'), serToMove)
        tMTF=getSeriesTable(structImgsMTF)
        sMTF=SIZE(tMTF, /DIMENSIONS)
        IF N_ELEMENTS(sMTF) EQ 1 THEN nMTF=1 ELSE nMTF=sMTF(1)
        IF N_ELEMENTS(tMTF) NE 1 THEN WIDGET_CONTROL, tabMTF, ALIGNMENT=1, TABLE_YSIZE=nMTF, SET_VALUE=tMTF

        ;remove from NPS
        structImgsNPS=removeIDstructstruct(structImgsNPS, serNo)
        tNPS=getSeriesTable(structImgsNPS)
        sNPS=SIZE(tNPS, /DIMENSIONS)
        IF N_ELEMENTS(sNPS) EQ 1 THEN nNPS=1 ELSE nNPS=sNPS(1)

        IF nNPS GT 1 THEN nImgSer=LONG(tNPS(6,0)) ELSE nImgSer=LONG(tNPS(6))
        WIDGET_CONTROL, tabNPS, ALIGNMENT=1, TABLE_YSIZE=nNPS, SET_VALUE=tNPS
        WIDGET_CONTROL, tabNPS, SET_TABLE_SELECT=[0,0,0,0]

        imgNoNPS=0
        firstImgNoNPS=0

        IF firstImgNoNPS GE nImgSer-3 THEN WIDGET_CONTROL, btnNextNPS, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextNPS, SENSITIVE=1
        WIDGET_CONTROL, btnPrevNPS, SENSITIVE=0

        redrawSeries
        redrawImgMTFNPS
      END
      'calcMTF':BEGIN
        proceed=1
        IF imgQCstruc.MTFroisSz LT 8. THEN BEGIN
          sv=DIALOG_MESSAGE('ROI size from center < 8 mm defined for MTF (in main window). This is probably too little. Proceed?',/QUESTION)
          IF sv EQ 'No' THEN proceed=0
        ENDIF
        
        IF proceed THEN BEGIN
          MTFres=!Null
          posROIMTF=!Null
          nMaterials=N_ELEMENTS(imgQCstruc.materialTable[0,*])
  
          IF N_ELEMENTS(structImgsMTF) GT 0 THEN BEGIN
            nSer=N_TAGS(structImgsMTF)
  
            largeDiff=INTARR(nSer)
            minVal=INTARR(nSer)
            statusArr=INTARR(nMaterials,nSer)
  
            FOR i=0, nSer-1 DO BEGIN
              WIDGET_CONTROL, lblProgressMTFNPS, SET_VALUE='Calculating MTF: '+STRING(i*100./nSer, FORMAT='(i0)')+' %'
              posTab=FLOAT(imgQCstruc.materialTable[1:2,*])
              tempImg=readImg(structImgsMTF.(i).(0).filename, structImgsMTF.(i).(0).frameNo)
              pix=structImgsMTF.(i).(0).pix
              pix=pix(0)
              radS=ROUND(FLOAT(imgQCstruc.MTFroisSz/pix))
              szFirst=SIZE(tempImg, /DIMENSIONS)
              nImg=N_TAGS(structImgsMTF.(i))
  
              imgSet=FLTARR(szFirst(0),szFirst(1), nImg)
              imgSet[*,*,0]=tempImg
              IF nImg GT 1 THEN BEGIN
                FOR n=1, nImg-1 DO imgSet[*,*,n]=readImg(structImgsMTF.(i).(n).filename, structImgsMTF.(i).(n).frameNo)
              ENDIF
  
              posTab=ROUND(posTab/pix)
              ROIs=getSampleRois(szFirst, INTARR(4), radS, posTab)
  
              ;generate sum of fullmatrix
              sumImg=TOTAL(imgSet,3)/(1.*nImg)
  
              CTlinROIpos=INTARR(2,nMaterials)
  
              FOR m=0, nMaterials-1 DO BEGIN
                searchMask=ROIs[*,*,m]
                ;find center
                IMAGE_STATISTICS, sumImg, MINIMUM=mini, MAXIMUM=maxi, MASK=searchMask
                halfMax=(mini+maxi)/2
                xarr=TOTAL(searchMask,2)
                yarr=TOTAL(searchMask,1)
                xnonZero=WHERE(xarr NE 0)
                ynonZero=WHERE(yarr NE 0)
                centerPos=-1
                centerPos=ROUND(centroid(sumImg[xnonZero(0):xnonZero(0)+2*radS,ynonZero(0):ynonZero(0)+2*radS], halfmax))
                IF MIN(centerPos) EQ -1 THEN centerPos=[radS,radS]
                centerPos=centerPos+[xnonZero(0),ynonZero(0)]
                CTlinROIpos[*,m]=centerPos
              ENDFOR
              posROIMTF=CREATE_STRUCT(posROIMTF, 'P'+STRING(i, FORMAT='(i0)'), CTlinROIpos/pix(0))
  
              ; quality control
              ;avg/stddev from central 5mm of submatrix
              ;cut outer images until difference < avg stdev
              ;
              ;warning air -1024 too much?
              rad5mm=2.5/pix
              FOR m=0, nMaterials-1 DO BEGIN
                subM=imgSet[CTlinROIpos[0,m]-rad5mm:CTlinROIpos[0,m]+rad5mm,CTlinROIpos[1,m]-rad5mm:CTlinROIpos[1,m]+rad5mm,*]
                avgStdMinSub=FLTARR(3,nImg)
                FOR aa=0, nImg-1 DO avgStdMinSub[*,aa]=[MEAN(subM[*,*,aa]),STDDEV(subM[*,*,aa]), MIN(subM[*,*,aa])]
                tolDiff=MEAN(avgStdMinSub[1,*])
                diff=SHIFT(avgStdMinSub[0,*],-1)-avgStdMinSub[0,*]
                okdiff=WHERE(ABS(diff) LT tolDiff)
                IF N_ELEMENTS(okdiff) NE nImg THEN largeDiff(i)=1
                minVal(i)=MIN([minVal(i),MIN(avgStdMinSub[2,*])])
              ENDFOR
  
              MTFresThisSer=!Null
              FOR m=0, nMaterials-1 DO BEGIN
                IF CTlinROIpos[0,m]-radS LT 0 OR CTlinROIpos[1,m]-radS LT 0 OR CTlinROIpos[0,m]+radS GE szFirst(0) OR CTlinROIpos[1,m]+radS GE szFirst(1) THEN BEGIN
                  statusArr(m,i)=3
                  MTFresThisMat=-1
                ENDIF ELSE BEGIN
                  subM=imgSet[CTlinROIpos[0,m]-radS:CTlinROIpos[0,m]+radS,CTlinROIpos[1,m]-radS:CTlinROIpos[1,m]+radS,*]
                  MTFresThisMat=calculateMTF(subM, pix, [0,0], 2, -1, imgQCstruc.cutLSF, imgQCstruc.cutLSFW, imgQCstruc.cutLSFW2, imgQCstruc.sampFreq)
                  statusArr(m,i)=MTFresThisMat.status
                ENDELSE
                MTFresThisSer=CREATE_STRUCT(MTFresThisSer, 'M'+STRING(m, FORMAT='(i0)'), MTFresThisMat)
              ENDFOR
  
              MTFres=CREATE_STRUCT(MTFres,'S'+STRING(i, FORMAT='(i0)'),MTFresThisSer)
            ENDFOR
            WIDGET_CONTROL, lblProgressMTFNPS, SET_VALUE=''
            errLogg=''
            IF TOTAL(largeDiff) GT 0 THEN BEGIN
              tooLarge=WHERE(largeDiff EQ 1)+1
              stri=''
              FOR p=0, N_ELEMENTS(tooLarge)-1 DO stri=stri+STRING(tooLarge(p), FORMAT='(i0)')+', '
              stri=STRMID(stri,0,STRLEN(stri)-2)
              errLogg=errLogg+'Questionable differences between images detected for the following series (numbered from top): '+stri+newline
              errLogg=errLogg+'Consider removing images that differ from the dataset. Use the profiles or sorted pixelvalues plot to evaluate how different the HU values are.' +newline
            ENDIF
            IF MIN(minVal) LT -1000 THEN BEGIN
              errLogg=errLogg+newline+'Minimum value for at least one series is < -1000.'+newline
              errLogg=errLogg+newline+'Be aware that calculating MTF whith air as material might give erroneous results as noise lower than -1024 will be set to -1024.'+newline
              errLogg=errLogg+newline+'Use the sorted pixelvalues plot to avaluate whether there is cutting and evaluate the LSF looking for asymmetry.'+newline
            ENDIF
            IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg,DIALOG_PARENT=evTop)
          ENDIF
          updateMTFNPSresults, 1,1,0
          redrawImgMTFNPS
        ENDIF;proceed
      END
      'calcNPS':BEGIN

        NPSres=!Null

        IF N_ELEMENTS(structImgsNPS) GT 0 THEN BEGIN
          nSer=N_TAGS(structImgsNPS)

          smNPSw=0.5*imgQCstruc.NPSsmooth
          sampFreq=imgQCstruc.sampFreq

          FOR s=0, nSer-1 DO BEGIN
            WIDGET_CONTROL, lblProgressMTFNPS, SET_VALUE='Calculating NPS: '+STRING(s*100./nSer, FORMAT='(i0)')+' %'

            pix=structImgsNPS.(s).(0).pix
            thisImg=readImg(structImgsNPS.(s).(0).filename, -1)
            szImg=SIZE(thisImg, /DIMENSIONS)
            roiNPS=getNPSrois(szImg, [0,0], imgQCstruc.NPSroiSzInt, ROUND(imgQCstruc.NPSdist/pix(0)), imgQCstruc.NPSsubNN)

            nImg=N_TAGS(structImgsNPS.(s))
            NPSresThisSer=!Null
            FOR i=0, nImg-1 DO BEGIN
              tempImg=readImg(structImgsNPS.(s).(i).filename, 0)
              NPSresThisImg=calculateNPS(tempImg, roiNPS, structImgsNPS.(s).(i).pix, smNPSw, sampFreq)
              NPSresThisSer=CREATE_STRUCT(NPSresThisSer, 'I'+STRING(i, FORMAT='(i0)'), NPSresThisImg)
            ENDFOR

            NPSres=CREATE_STRUCT(NPSres, 'N'+STRING(s, FORMAT='(i0)'), NPSresThisSer)
          ENDFOR
          WIDGET_CONTROL, lblProgressMTFNPS, SET_VALUE=''
          updateMTFNPSresults, 1,1,0
        ENDIF
      END
      'cw_material':updateMTFNPSresults,1,1,0
      'cw_plotMTF':updateMTFNPSresults,1,1,0
      'copyCurve':updateMTFNPSresults, 0,0,1
      'copyTbl': copyResTbl, WIDGET_INFO(wtab, /TAB_CURRENT)
      'iPlot':updateMTFNPSresults, 0,0,2
      'setRangeMinMaxX':updateMTFNPSresults, 1,0,0
      'setRangeMinMaxY':updateMTFNPSresults, 0,1,0
      'exportMTFNPS':BEGIN
        adr=''
        IF N_ELEMENTS(MTFres) GT 0 OR N_ELEMENTS(NPSres) GT 0 THEN BEGIN
          adr=DIALOG_PICKFILE(TITLE='Select folder for saving resultfiles', PATH=pathMTFNPS, DIALOG_PARENT=evTop, /DIRECTORY)
          IF adr NE '' THEN BEGIN;test writing rights
            fi=FILE_INFO(adr)
            IF fi.WRITE EQ 0 THEN BEGIN
              sv=DIALOG_MESSAGE('You do not have writing permissions for the selected folder. Export stopped.', DIALOG_PARENT=evTop)
              adr=''
            ENDIF
          ENDIF
        ENDIF

        IF adr NE '' THEN BEGIN
          ;series info include only info actual for current modality
          includeTagsShort=STRUPCASE(['seriesName','kVp','mA','ExpTime','kernel','acqDate/time','nImgs'])
          includeTagsExt=STRUPCASE(['institution','stationName','modelName','patientName','patientID','imageType','studyDescr','protocolname','coll','sliceThick','pitch','rekonFOV','pix','CTDIvol'])

          adrList=['MTF_table','NPS_table','MTF_curves_discrete','NPS_curves','MTF_curves_gaussian']
          exArr=INTARR(4)
          FOR i=0, 3 DO BEGIN
            testResFile=FILE_INFO(adr+adrList(i)+'.txt')
            exArr=testResFile.exists
          ENDFOR
          IF TOTAL(exArr) GT 0 THEN BEGIN
            sv=DIALOG_MESSAGE('Result-files already exist in this folder. Overwrite?',/QUESTION,DIALOG_PARENT=evTop)
            IF sv EQ 'No' THEN BEGIN
              adrList=adrList+string(systime(/julian),format='(C(CYI02,CMOI02,CDI02))')+'_'+string(systime(/julian),format='(C(CHI02,CMI02,CSI02))')
            ENDIF
          ENDIF

          ;resultTables
          FOR o=0, 1 DO BEGIN
            proceed=0
            IF o EQ 0 THEN BEGIN
              IF N_ELEMENTS(MTFres) GT 0 THEN proceed=1
              structThis=structImgsMTF
              WIDGET_CONTROL, tabMTF, GET_VALUE=infoTable
            ENDIF ELSE BEGIN
              IF N_ELEMENTS(NPSres) GT 0 THEN proceed=1
              structThis=structImgsNPS
              WIDGET_CONTROL, tabNPS, GET_VALUE=infoTable
            ENDELSE

            IF proceed THEN BEGIN
              copyResTbl, o
              arrRes=CLIPBOARD.GET()

              tagsAll=TAG_NAMES(structThis.(0).(0))
              nSer=N_TAGS(structThis)
              
              arrShort=[STRJOIN(includeTagsShort, STRING(9B)),STRJOIN(infoTable, STRING(9B))]

              arrImgNo=STRARR(nSer+1)
              arrImgNo(0)='Image numbers'
              FOR s=0, nSer-1 DO BEGIN
                nImg=N_TAGS(structThis.(s))
                stri=''
                FOR j=0, nImg-1 DO BEGIN
                  stri=stri+STRING(structThis.(s).(j).imgNo, FORMAT='(i0)')+','
                ENDFOR
                stri=STRMID(stri,0,STRLEN(stri)-1)
                arrImgNo(s+1)=stri
              ENDFOR

              infoTable=STRARR(N_ELEMENTS(includeTagsExt),nSer)
              FOR s=0, nSer-1 DO BEGIN
                FOR j=0, N_ELEMENTS(includeTagsExt)-1 DO BEGIN
                  id=WHERE(tagsAll EQ includeTagsExt(j))
                  IF id(0) NE -1 THEN infoTable[j,s]=STRTRIM(STRING(structThis.(s).(0).(id)),1)
                  IF includeTagsExt(j) EQ 'COLL' THEN infoTable[j,s]=infoTable[j,s]+'|'+STRTRIM(STRING(structThis.(s).(0).(id)(1)),1)
                ENDFOR
              ENDFOR
              IF imgQCstruc.deciMark EQ ',' THEN FOREACH elem, infoTable, idx DO infoTable(idx)=STRJOIN(STRSPLIT(elem, '.',/EXTRACT),',')
              arrExt=[STRJOIN(includeTagsExt, STRING(9B)),STRJOIN(infoTable, STRING(9B))]

              output=arrRes+STRING(9B)+arrShort+STRING(9B)+arrImgNo+STRING(9B)+arrExt

              OPENW, resfile, adr+adrList(o)+'.txt', /GET_LUN
              PRINTF, resfile, output
              CLOSE, resfile & FREE_LUN, resfile
            ENDIF;proceed
          ENDFOR;o MTF=0, NPS=1

          ;curves
          mm2cm=1.;1. means no converstion to cm, 10. when convert to cm
          ;convert all curves to defined sampling frequency to facilitate d' calculation and more easily plot functionality
          
          maxFreq=2.5
          nn=ROUND(maxFreq/imgQCstruc.sampFreq)
          freq=FINDGEN(nn)*imgQCstruc.sampFreq
          FOR o=0, 2 DO BEGIN

            CASE o OF
              0: BEGIN; MTF discrete
                IF N_ELEMENTS(MTFres) GT 0 THEN BEGIN
                  nSer=N_TAGS(MTFres)
                  nMat=N_TAGS(MTFres.(0))
                  valuesPlot=!Null
                  headersPlot=!Null
                  FOR s=0, nSer-1 DO BEGIN
                    FOR m=0, nMat-1 DO BEGIN
                      IF SIZE(MTFres.(s).(m), /TNAME) EQ 'STRUCT' THEN BEGIN
                        valuesPlot=CREATE_STRUCT(valuesPlot, 'C'+STRING(s*nMat+m,FORMAT='(i0)'),INTERPOL(MTFres.(s).(m).MTFx,MTFres.(s).(m).fx,freq))
                        headersPlot=CREATE_STRUCT(headersPlot, 'C'+STRING(s*nMat+m,FORMAT='(i0)'),[structImgsMTF.(s).(0).seriesName, structImgsMTF.(s).(0).kernel,STRING(structImgsMTF.(s).(0).kVp,FORMAT='(i0)'),STRING(structImgsMTF.(s).(0).mA,FORMAT='(f0.1)'),STRING(structImgsMTF.(s).(0).ExpTime,FORMAT='(i0)'), imgQCstruc.materialTable(0,m),'MTF'])
                      ENDIF
                    ENDFOR
                  ENDFOR
                ENDIF
              END
              1:BEGIN;NPS
                IF N_ELEMENTS(NPSres) GT 0 THEN BEGIN
                  nSer=N_TAGS(NPSres)
                  valuesPlot=!Null
                  headersPlot=!Null
                  FOR s=0, nSer-1 DO BEGIN
                    nImg=N_TAGS(NPSres.(s))
                    NPStot=0
                    ftemp=0
                    FOR im=0, nImg-1 DO BEGIN
                      IF N_ELEMENTS(NPStot) EQ 1 THEN BEGIN
                        NPStot=NPSres.(s).(im).rNPS
                        ftemp=NPSres.(s).(im).dr
                      ENDIF ELSE NPStot=NPStot+NPSres.(s).(im).rNPS
                    ENDFOR
                    NPStot=NPStot/nImg
                    valuesPlot=CREATE_STRUCT(valuesPlot,'C'+STRING(s,FORMAT='(i0)'), NPStot)
                    headersPlot=CREATE_STRUCT(headersPlot, 'C'+STRING(s,FORMAT='(i0)'),[structImgsNPS.(s).(0).seriesName, structImgsNPS.(s).(0).kernel,STRING(structImgsMTF.(s).(0).kVp,FORMAT='(i0)'),STRING(structImgsMTF.(s).(0).mA,FORMAT='(f0.1)'),STRING(structImgsMTF.(s).(0).ExpTime,FORMAT='(i0)'),'','NPS'])
                  ENDFOR
                ENDIF ELSE valuesPlot=CREATE_STRUCT('C', 0)
              END
              2:BEGIN;MTF gaussian
                IF N_ELEMENTS(MTFres) GT 0 THEN BEGIN
                  nSer=N_TAGS(MTFres)
                  nMat=N_TAGS(MTFres.(0))
                  valuesPlot=!Null
                  headersPlot=!Null
                  FOR s=0, nSer-1 DO BEGIN
                    FOR m=0, nMat-1 DO BEGIN
                      IF SIZE(MTFres.(s).(m), /TNAME) EQ 'STRUCT' THEN BEGIN
                        tagMTFres=tag_names(MTFres.(s).(m))
                        IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'C'+STRING(s*nMat+m,FORMAT='(i0)'), INTERPOL(MTFres.(s).(m).gMTFx,MTFres.(s).(m).gfx,freq)) $
                        ELSE valuesPlot=CREATE_STRUCT(valuesPlot,'C'+STRING(s*nMat+m,FORMAT='(i0)'),0)
                        headersPlot=CREATE_STRUCT(headersPlot, 'C'+STRING(s*nMat+m,FORMAT='(i0)'),[structImgsMTF.(s).(0).seriesName, structImgsMTF.(s).(0).kernel,STRING(structImgsMTF.(s).(0).kVp,FORMAT='(i0)'),STRING(structImgsMTF.(s).(0).mA,FORMAT='(f0.1)'),STRING(structImgsMTF.(s).(0).ExpTime,FORMAT='(i0)'), imgQCstruc.materialTable(0,m),'MTF'])
                      ENDIF
                    ENDFOR
                  ENDFOR
                ENDIF
              END
              ELSE:
            ENDCASE

            tplots=TAG_NAMES(valuesPlot)
            nnPlots=N_ELEMENTS(tPlots)
            IF nnPlots GT 1 THEN BEGIN
              valuesArr=STRARR(nnPlots+1,N_ELEMENTS(freq))
              headArr=STRARR(nnPlots+1,7)
              topInfo=TRANSPOSE(['SeriesName','Kernel','kV','mA','ExpTime','Material','Frequency mm-1'])
              IF o EQ 1 THEN topInfo(5)=''
              headArr[0,0:6]=topInfo
                
              valuesArr[0,*]=STRING(TRANSPOSE(freq))

              FOR i=0, nnPlots-1 DO BEGIN
                    IF N_ELEMENTS(valuesPlot.(i)) GT 1 THEN valuesArr[i+1,0:N_ELEMENTS(valuesPlot.(i))-1]=STRING(TRANSPOSE(valuesPlot.(i)))
                    headArr[i+1,0:6]=TRANSPOSE(headersPlot.(i))
              ENDFOR
              IF imgQCstruc.deciMark EQ ',' THEN FOREACH elem, valuesArr, idx DO valuesArr(idx)=STRJOIN(STRSPLIT(elem, '.',/EXTRACT),',')

              output=[STRJOIN(headArr, STRING(9B)),STRJOIN(valuesArr, STRING(9B))]
              OPENW, resfile, adr+adrList(o+2)+'.txt', /GET_LUN
              PRINTF, resfile, output, FORMAT='(a0)'
              CLOSE, resfile & FREE_LUN, resfile
            ENDIF
          ENDFOR

        ENDIF;adr''
      END
      'closeAuto': BEGIN
        WIDGET_CONTROL, event.top, /DESTROY
      END
      ELSE:
    ENDCASE
  ENDIF

  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN

    IF event.release EQ 1 THEN BEGIN
      xx=event.X

      IF event.ID EQ drawMiniMTF THEN BEGIN
        IF xx LT 150 THEN imgNoMTF = firstImgNoMTF
        IF xx GT 150 AND xx LT 300 THEN imgNoMTF = firstImgNoMTF+1
        IF xx GT 300 THEN imgNoMTF = firstImgNoMTF+2
        tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
        serNo=tabSel(1)
        WIDGET_CONTROL, tabMTF, GET_VALUE=currTab
        nImgSer=LONG(currTab(6,serNo))
        IF nImgSer LT 3 THEN BEGIN
          IF imgNoMTF GT nImgSer-1 THEN imgNoMTF=nImgSer-1
        ENDIF

        redrawSeries
        redrawImgMTFNPS
      ENDIF
      
      IF event.ID EQ drawMiniNPS THEN BEGIN
        IF xx LT 150 THEN imgNoNPS = firstImgNoNPS
        IF xx GT 150 AND xx LT 300 THEN imgNoNPS = firstImgNoNPS+1
        IF xx GT 300 THEN imgNoNPS = firstImgNoNPS+2
        tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
        serNo=tabSel(1)
        WIDGET_CONTROL, tabNPS, GET_VALUE=currTab
        nImgSer=LONG(currTab(6,serNo))
        IF nImgSer LT 3 THEN BEGIN
          IF imgNoNPS GT nImgSer-1 THEN imgNoNPS=nImgSer-1
        ENDIF

        redrawSeries
        redrawImgMTFNPS
      ENDIF

    ENDIF

  ENDIF

  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' THEN BEGIN
    IF event.ID EQ tabMTF THEN BEGIN
      IF event.sel_top NE -1 THEN BEGIN
        imgNoMTF=0
        firstImgNoMTF=0
        WIDGET_CONTROL, btnPrevMTF, SENSITIVE=0
        WIDGET_CONTROL, tabMTF, GET_VALUE=currTab
        nImgSer=LONG(currTab(6,event.sel_top))
        IF nImgSer LE 3 THEN WIDGET_CONTROL, btnNextMTF, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextMTF, SENSITIVE=1
        redrawSeries
        redrawImgMTFNPS
        IF N_ELEMENTS(MTFres) NE 0 THEN BEGIN
          WIDGET_CONTROL, resTabMTF, SET_TABLE_SELECT=[0,event.sel_top,0,event.sel_top]
          updateMTFNPSresults,0,0,0
        ENDIF
      ENDIF
    ENDIF
    IF event.ID EQ tabNPS THEN BEGIN
      IF event.sel_top NE -1 THEN BEGIN
        imgNoNPS=0
        firstImgNoNPS=0
        WIDGET_CONTROL, btnPrevNPS, SENSITIVE=0
        WIDGET_CONTROL, tabNPS, GET_VALUE=currTab
        nImgSer=LONG(currTab(6,event.sel_top))
        IF nImgSer LE 3 THEN WIDGET_CONTROL, btnNextNPS, SENSITIVE=0 ELSE WIDGET_CONTROL, btnNextNPS, SENSITIVE=1

        redrawSeries
        redrawImgMTFNPS
        IF N_ELEMENTS(NPSres) NE 0 THEN BEGIN
          WIDGET_CONTROL, resTabNPS, SET_TABLE_SELECT=[0,event.sel_top,0,event.sel_top]
          updateMTFNPSresults,0,0,0
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') OR (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
    action=0
    IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') THEN BEGIN
      IF event.enter EQ 0 THEN action=1 ; lost focus
    ENDIF
    IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
      IF event.type EQ 0 THEN action=1 ;return or enter pressed
    ENDIF
    IF action EQ 1 THEN BEGIN
      CASE event.ID OF
        txtMinRangeMTF_X: BEGIN
          WIDGET_CONTROL, txtMinRangeMTF_X, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMinRangeMTF_X, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        txtMaxRangeMTF_X: BEGIN
          WIDGET_CONTROL, txtMaxRangeMTF_X, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMaxRangeMTF_X, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        txtMinRangeMTF_Y: BEGIN
          WIDGET_CONTROL, txtMinRangeMTF_Y, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMinRangeMTF_Y, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        txtMaxRangeMTF_Y: BEGIN
          WIDGET_CONTROL, txtMaxRangeMTF_Y, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMaxRangeMTF_Y, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        txtMinRangeNPS_X: BEGIN
          WIDGET_CONTROL, txtMinRangeNPS_X, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMinRangeNPS_X, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        txtMaxRangeNPS_X: BEGIN
          WIDGET_CONTROL, txtMaxRangeNPS_X, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMaxRangeNPS_X, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        txtMinRangeNPS_Y: BEGIN
          WIDGET_CONTROL, txtMinRangeNPS_Y, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMinRangeNPS_Y, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        txtMaxRangeNPS_Y: BEGIN
          WIDGET_CONTROL, txtMaxRangeNPS_Y, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMaxRangeNPS_Y, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updateMTFNPSresults, 0,0,0
        END
        ELSE:
      ENDCASE
    ENDIF
  ENDIF

  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TAB') THEN BEGIN

    selTab=WIDGET_INFO(event.ID, /TAB_CURRENT)

    CASE event.ID OF
      wtab: BEGIN
        redrawSeries
        redrawImgMTFNPS
      END
      wtabResultMTF: BEGIN
        updateMTFNPSresults,1,1,0
      END
      wtabResultNPS: BEGIN
        updateMTFNPSresults, 1,1,0
      END
      ELSE:
    ENDCASE
  ENDIF

end

pro clearMTFNPS
  COMMON MTFNPS
  COMPILE_OPT hidden
  structImgsMTF=!Null
  structImgsNPS=!Null
  MTFres=!Null
  NPSres=!Null
  roiMTF=!Null
  roiNPS=!Null
  imgNoMTF=0
  imgNoNPS=0
  firstImgNoMTF=0
  firstImgNONPS=0
  WIDGET_CONTROL, drawMTF, GET_VALUE = drawid & drawid.erase
  WIDGET_CONTROL, drawMiniMTF, GET_VALUE = drawid & drawid.erase
  WIDGET_CONTROL, drawNPS, GET_VALUE = drawid & drawid.erase
  WIDGET_CONTROL, drawMiniNPS, GET_VALUE = drawid & drawid.erase
  WIDGET_CONTROL, resTabMTF, TABLE_YSIZE=1, SET_VALUE=STRARR(7)
  WIDGET_CONTROL, resTabNPS, TABLE_YSIZE=1, SET_VALUE=STRARR(7)
end