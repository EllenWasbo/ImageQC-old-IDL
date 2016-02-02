;ImageQC - quality control of medical images
;Copyright (C) 2016  Ellen Wasbo, Stavanger University Hospital, Norway
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

pro ImageQC,  GROUP_LEADER=bMain

COMMON VARI,  $ 
    lblDir, listFiles, marked,lblProgress, activeImg, activeResImg, nFrames, ROIs,  $
    txtActive1, txtActive2, newline, $
    drawLarge, drawXY, txtMinWL, txtMaxWL, txtCenterWL, txtWidthWL, lblCursorValue, lblCursorPos,lblCursorPosMM, $  
    txtDeltaX, txtDeltaY, txtDeltaA, dxya, useDelta,$ 
    defPath, thisPath, structImgs, lastXY, lastXYreleased, mouseDown, $
    modality, analyse, analyseStringsCT, analyseStringsXray, analyseStringsNM, results, $
    resTab, wtabResult, wtabModes,wtabAnalysisCT,wtabAnalysisXray,wtabAnalysisNM, $
    drawPlot, statPlot,drawImageRes, txtMinRangeX, txtMaxRangeX, txtMinRangeY, txtMaxRangeY, rangeAcc, $
    ROIres, typeROI, typeROIX, $
    MTFres, $
    cw_typeMTF, cw_plotMTF, txtMTFroiSz, btnCutLSF, txtcutLSFW, txtcutLSFW2, $
    cw_formLSFX, cw_plotMTFX,  txtMTFroiSzX, txtMTFroiSzY, btnCutLSFX, txtcutLSFWX, $
    cw_typeMTFNM, cw_plotMTFNM, txtMTFroiSzXNM, txtMTFroiSzYNM, btnCutLSFNM, txtcutLSFWNM, MTF3dNM, $
    CTlinRes, materialData, CTlinROIs, txtLinROIrad, txtLinROIrad2,  $
    sliceThickRes, sliceThickResTab,  ramps, txtRampDist, txtRampLen, txtRampBackG, txtRampSearch, txtRampAverage,  $
    homogRes, homogROIs, txtHomogROIsz,  txtHomogROIszX, txtHomogROIdist, cw_homogNM, txtHomogROIszNM, txtHomogROIdistXNM, txtHomogROIdistYNM, $
    noiseRes, noiseROI, txtNoiseROIsz, $
    fwhmRes, dimRes, energyRes, $
    stpRes, txtStpROIsz, stpROI, txtRQA, Qvals, $
    NPSres, NPSrois, txtNPSroiSz, txtNPSsubSz, lblNPSsubSzMM, lblNPStotpix, txtNPSroiSzX, txtNPSsubSzX, lblNPSsubSzMMX, lblNPStotpixX, $
    txtNAvgSpeedNM, txtScanSpeedMedian, txtSpeedROIheight,$
    contrastRes, conROIs, txtConR1NM, txtConR2NM

  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  RESTORE, thisPath+'data\config.dat'
  defPath=config.defPath
  
  structImgs=CREATE_STRUCT('empty',0); images with attributes in memory
  activeImg=0 ; active (selected image)
  activeResImg=0 ; result-image (fx 2d NPS)
  nFrames=0; if multiframe image loaded, nFrames=nImg
  analyseStringsCT=['NONE', 'DIM', 'HOMOG', 'NOISE','MTF', 'NPS','ROI', 'CTLIN', 'SLICETHICK','FWHM']
  analyseStringsXray=['NONE', 'STP','HOMOG', 'NOISE','MTF', 'NPS','ROI']
  analyseStringsNM=['NONE','ENERGYSPEC','HOMOG','SCANSPEED','CONTRAST','MTF']
  analyse='NONE'
  modality=0; 0=CT, 1=Xray, 2=NM
  results=INTARR(9); set to 1 when analyse performed and results is available, keep analyseString and tab-order equal
  dxya=[0,0,0.0,1]; [deltax,deltay,delta,show] for positioning of center/angle. Difference from imgSz/2 in pixels. Show (last param) used for redrawCT to overplot crosshair
  CTlinROIs=0 & homogROIs=0 & noiseROI=0 & NPSrois=0 & conROIs=0; used to hold the rois for specific tests
  ramps=0; used to hold the 4 lines for slice thickness H-top,H-bottom,V1,V2
  lastXY=[-1,-1]; last mouseposition in draw window
  lastXYreleased=[-1,-1]
  mouseDown=0; If mouse pressed in draw window and still not released 1=true
  marked=-1; indexes of marked files
  if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
  
  winX=1400 &  winY=1000 ;actual winX not 1400 - adjusted down to 1250 - should be adapted to screen size (Todo)
  drawXY=500
  
  DEVICE, GET_SCREEN_SIZE=scsz
  
  IF scsz(0) LT winX-150 THEN scX=scsz(0)-50 ELSE scX=winX-150
  IF scsz(1) LT winY-50 THEN scY=scsz(1)-50 ELSE scY=winY-50
  
  bMain = WIDGET_BASE(TITLE='ImageQC', MBAR=bar, /COLUMN, XSIZE=winX, YSIZE=winY-50, XOFFSET=100, YOFFSET=100, X_SCROLL_SIZE=scX, Y_SCROLL_SIZE=scY, /TLB_KILL_REQUEST_EVENTS)
  bLarge = WIDGET_BASE(bMain, /ROW)
  bLft = WIDGET_BASE(bLarge, XSIZE=winX/2-30, YSIZE=winY-100,/COLUMN)
  bRgt = WIDGET_BASE(bLarge, XSIZE=winX/2-50, YSIZE=winY-100,/COLUMN)

  ;*****************MENU
  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  help_menu=WIDGET_BUTTON(bar, VALUe='Help', /MENU)
  ;file_menu
  btnOpen=WIDGET_BUTTON(file_menu, VALUE='Open DICOM file or series', UVALUE='open', ACCELERATOR='Ctrl+O')
  ;btnExport=WIDGET_BUTTON(file_menu, VALUE='Export results', UVALUE='export', ACCELERATOR='Ctrl+E')
  btnPref=WIDGET_BUTTON(file_menu, VALUE='Preferences..', UVALUE='pref')
  btnDefPath=WIDGET_BUTTON(file_menu, VALUE='Define default path', UVALUE='defpath', /SEPARATOR)
  btnConfig=WIDGET_BUTTON(file_menu, VALUE='Save current default values to config file', UVALUE='config')
  btnClose=WIDGET_BUTTON(file_menu, VALUE='Close all images', UVALUE='close')
  btnExit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='exit', ACCELERATOR='Ctrl+X', /SEPARATOR)
  ;help_menu
  btnInfo=WIDGET_BUTTON(help_menu, VALUE='How ImageQC works...', UVALUE='info')
  btnAbout=WIDGET_BUTTON(help_menu, VALUE='About ImageQC...',UVALUE='about')

  toolbarLft=WIDGET_BASE(bLft,/ROW,/TOOLBAR)
  toolOpen=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\open.bmp',/BITMAP, UVALUE='open', TOOLTIP='Open DICOM file(s)')
  toolOpenMultiple=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\openM.bmp',/BITMAP, UVALUE='openMulti', TOOLTIP='Open DICOM file(s) from multiple folders')
  
  toolml3=WIDGET_LABEL(toolbarLft, VALUE='', XSIZE=20)
  btnImgTop=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_top.bmp',/BITMAP, UVALUE='imgTop', TOOLTIP='Place selected image(s) at top of list')
  btnImgUp=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='imgUp', TOOLTIP='Move selected image(s) upwards in list')
  btnImgDown=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='imgDown', TOOLTIP='Move selected image(s) downwards of list')
  btnImgBottom=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_bottom.bmp',/BITMAP, UVALUE='imgBottom', TOOLTIP='Place selected image(s) at bottom of list')
  
  lblProgress = WIDGET_LABEL(toolbarLft, VALUE='', /DYNAMIC_RESIZE)
  
  ;****************** left Panel
  bInfoLoaded=WIDGET_BASE(bLft, /ROW, YSIZE=220)
  bInfoLft=WIDGET_BASE(bInfoLoaded, /COLUMN)

  ;list ++
  bList=WIDGET_BASE(bInfoLft, /ROW)
  bListLoaded=WIDGET_BASE(bList, /COLUMN)
  lblLoaded=WIDGET_LABEL(bListLoaded, VALUE='Loaded images', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  listFiles=WIDGET_LIST(bListLoaded, XSIZE=winX/2-50, SCR_XSIZE=winX/2-200, YSIZE=1, SCR_YSIZE=170, MULTIPLE=1, UVALUE='filelist')

  bMarkSelect=WIDGET_BASE(bList, /COLUMN)
  bListBtns=WIDGET_BASE(bMarkSelect, /COLUMN, /TOOLBAR)
  btnMarkSelected=WIDGET_BUTTON(bListBtns, VALUE=thisPath+'images\markSelected.bmp',/BITMAP, TOOLTIP='Mark selected', UVALUE='markSelected')
  btnUnMarkSelected=WIDGET_BUTTON(bListBtns, VALUE=thisPath+'images\unmarkSelected.bmp',/BITMAP, TOOLTIP='Remove mark from selected', UVALUE='unmarkSelected')
  btnSelectInverse=WIDGET_BUTTON(bListBtns, VALUE=thisPath+'images\selectInverse.bmp',/BITMAP, TOOLTIP='Select inverse', UVALUE='selectInverse')
  btnRemMarked=WIDGET_BUTTON(bListBtns, VALUE=thisPath+'images\selectMarked.bmp',/BITMAP, TOOLTIP='Select marked', UVALUE='selectMarked')
  btnRemove=WIDGET_BUTTON(bListBtns, VALUE=thisPath+'images\deleteSelected.bmp',/BITMAP, TOOLTIP='Close selected', UVALUE='remove')

  bPrevNext = WIDGET_BASE(bList, /COLUMN)
  mlprevnext=WIDGET_LABEL(bPrevNext, VALUE='', YSIZE=70, XSIZE=30)
  btnPrev = WIDGET_BUTTON(bPrevNext, VALUE=thisPath+'images\shift_up.bmp',/BITMAP,UVALUE='prev',TOOLTIP='Previous image in list')
  btnNext = WIDGET_BUTTON(bPrevNext, VALUE=thisPath+'images\shift_down.bmp', /BITMAP,UVALUE='next',TOOLTIP='Next image in list')

  ;above image
  ;bViz = WIDGET_BASE(bLft, /ROW)

  ;image
  bDraw = WIDGET_BASE(bLft, XSIZE=drawXY+150, YSIZE=drawXY+10, /ROW)
  drawLarge = WIDGET_DRAW(bDraw, XSIZE=drawXY, YSIZE=drawXY, KEYBOARD_EVENTS=1, /BUTTON_EVENTS, /MOTION_EVENTS, /WHEEL_EVENTS, GRAPHICS_LEVEL=2, RETAIN=2, SENSITIVE=0)
  ;rgt of image
  bDrawRgt = WIDGET_BASE(bDraw, Ysize=drawXY,XSIZE=200,/COLUMN)
  
  ;window level
  bViz = WIDGET_BASE(bDrawRgt, /COLUMN)
  lblWL = WIDGET_LABEL(bViz, VALUE='Window level', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  bWindowMinMax=WIDGET_BASE(bViz, /ROW)
  lblMinWL = WIDGET_LABEL(bWindowMinMax, VALUE='Min', SCR_XSIZE=30);, /ALIGN_RIGHT)
  txtMinWL = WIDGET_TEXT(bWindowMinMax, VALUE='-200', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  lblMl0= WIDGET_LABEL(bWindowMinMax, VALUE='Max', SCR_XSIZE=30);, /ALIGN_RIGHT)
  txtMaxWL = WIDGET_TEXT(bWindowMinMax, VALUE='200', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)

  bWindowCenterWidth=WIDGET_BASE(bViz, /ROW)
  lblCenterW=WIDGET_LABEL(bWindowCenterWidth, VALUE='Center', SCR_XSIZE=30);, /ALIGN_RIGHT)
  txtCenterWL=WIDGET_TEXT(bWindowCenterWidth, VALUE='0', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  lblWidth=WIDGET_LABEL(bWindowCenterWidth, VALUE='Width', SCR_XSIZE=30);, /ALIGN_RIGHT)
  txtWidthWL=WIDGET_TEXT(bWindowCenterWidth, VALUE='400', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)

  bWLsetto=WIDGET_BASE(bViz, /ROW)
  ;lblMl1= WIDGET_LABEL(bWLsetto, VALUE='', XSIZE=30)
  btnSetWLminmax=WIDGET_BUTTON(bWLsetto, VALUE=thisPath+'images\minmax.bmp', /BITMAP, UVALUE='WLminmax', TOOLTIP='Set Window Level to min/max in image')
  btnSetWLstdev=WIDGET_BUTTON(bWLsetto, VALUE=thisPath+'images\meanstdev.bmp', /BITMAP, UVALUE='WLmeanstdev', TOOLTIP='Set Window Level to mean+/-stdev of pixelvalues in selected image')
  
  mlRgtimg0 = WIDGET_LABEL(bDrawRgt, VALUE='', YSIZE=20)
  
  ;rgt of image - cursor 
  bCursor=WIDGET_BASE(bDrawRgt, /COLUMN)
  bCursorPos=WIDGET_BASE(bCursor, /ROW)
  lblCursorPos0=WIDGET_LABEL(bCursorPos, VALUE='Cursor pos. (pix): ')
  lblCursorPos=WIDGET_LABEL(bCursorPos, VALUE='-,-', XSIZE=70)
  bCursorPosMM=WIDGET_BASE(bCursor, /ROW)
  lblCursorPosMM0=WIDGET_LABEL(bCursorPosMM, VALUE='Cursor pos. (mm): ')
  lblCursorPosMM=WIDGET_LABEL(bCursorPosMM, VALUE='-,-', XSIZE=70)
  bCursorValue= WIDGET_BASE(bCursor, /ROW)
  lblCursorValue0=WIDGET_LABEL(bCursorValue, VALUE='Cursor value: ')
  lblCursorValue=WIDGET_LABEL(bCursorValue, VALUE='-', XSIZE=50)
  mlRgtimg = WIDGET_LABEL(bDrawRgt, VALUE='', YSIZE=20)
  
  ;rgt of image - center angle
  bCenterAngle = WIDGET_BASE(bDrawRgt, /COLUMN)
  titleCenterAngle = WIDGET_LABEL(bCenterAngle, VALUE='Correct center/rotation...')
  bDeltaX=WIDGET_BASE(bCenterAngle,/ROW)
  lblDeltaX=WIDGET_LABEL(bDeltaX, VALUE='dx')
  txtDeltaX=WIDGET_TEXT(bDeltaX, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS)
  minusDeltaX=WIDGET_BUTTON(bDeltaX, VALUE='-', UVALUE='minusDx')
  plusDeltaX=WIDGET_BUTTON(bDeltaX, VALUE='+', UVALUE='plusDx')
  bDeltaY=WIDGET_BASE(bCenterAngle,/ROW)
  lblDeltaY=WIDGET_LABEL(bDeltaY, VALUE='dy')
  txtDeltaY=WIDGET_TEXT(bDeltaY, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS)
  minusDeltaY=WIDGET_BUTTON(bDeltaY, VALUE='-', UVALUE='minusDy')
  plusDeltaY=WIDGET_BUTTON(bDeltaY, VALUE='+', UVALUE='plusDy')
  bDeltaA=WIDGET_BASE(bCenterAngle,/ROW)
  lblDeltaA=WIDGET_LABEL(bDeltaA, VALUE='da')
  txtDeltaA=WIDGET_TEXT(bDeltaA, VALUE='0.0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS)
  minusDeltaA=WIDGET_BUTTON(bDeltaA, VALUE='-', UVALUE='minusDa')
  plusDeltaA=WIDGET_BUTTON(bDeltaA, VALUE='+', UVALUE='plusDa')
  bUse=WIDGET_BASE(bCenterAngle, /NONEXCLUSIVE)
  useDelta=WIDGET_BUTTON(bUse, VALUE='Use correction', UVALUE='useDelta')
  WIDGET_CONTROL, useDelta, SET_BUTTON=1
  ;btnShowDelta=WIDGET_BUTTON(bCenterAngle, VALUE='Show center/rot', UVALUE='showDelta')
  btnShowDelta=WIDGET_BUTTON(bCenterAngle, VALUE='Get center', UVALUE='getCenter')
  btnShowDelta=WIDGET_BUTTON(bCenterAngle, VALUE='Set center', UVALUE='setCenter')

  mlRgtimg2 = WIDGET_LABEL(bDrawRgt, VALUE='', YSIZE=10)
  ;iImage toolbar
  lbliImage=WIDGET_LABEL(bDrawRgt, VALUE='Send to iImage:')
  toolBarDraw = WIDGET_BASE(bDrawRgt, /ROW, /TOOLBAR)
  ;lbliImage= WIDGET_LABEL(toolBarDraw, VALUE=' iImage:  ')
  ;btnPushImg = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\layer.bmp', /BITMAP, UVALUE='pushImg', TOOLTIP='Send image to separate window')
  btnAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\ax.bmp', /BITMAP, UVALUE='ax', TOOLTIP='Send active image to iImage window')
  btnCor = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\cor.bmp', /BITMAP, UVALUE='cor', TOOLTIP='Send coronal image found from image stack at defined senter to iImage window')
  btnSag = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sag.bmp', /BITMAP, UVALUE='sag', TOOLTIP='Send sagittal image found from image stack at defined senter to iImage window')
  lblML2=WIDGET_LABEL(toolBarDraw, VALUE='', XSIZE=5)
  btnSumAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sum.bmp', /BITMAP, UVALUE='sumax', TOOLTIP='Sum all or marked images and send to iImage window')

  bInfoLow=WIDGET_BASE(bLft, /ROW)
  txtActive1=WIDGET_TEXT(bInfoLow, XSIZE=100, YSIZE=100, VALUE='', SCR_XSIZE=320, SCR_YSIZE=130)
  txtActive2=WIDGET_TEXT(bInfoLow, XSIZE=100, YSIZE=100, VALUE='', SCR_XSIZE=250, SCR_YSIZE=130)
  toolBarInfo = WIDGET_BASE(bInfoLow, /COLUMN, /TOOLBAR)
  btnClipBoardInfo=WIDGET_BUTTON(toolBarInfo, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy these parameters for all images to clipboard in tabular format', UVALUE='copyInfo')  
  toolDump=WIDGET_BUTTON(toolbarInfo, VALUE=thisPath+'images\dump.bmp', /BITMAP, TOOLTIP='DICOM dump of active file', UVALUE='dump')

  
  ;Analysis tabs
  bAnalysis=WIDGET_BASE(bRgt, /COLUMN)
  wtabModes=WIDGET_TAB(bAnalysis, XSIZE=winX/2-50, YSIZE=250, UVALUE='tabModes')
  bCT=WIDGET_BASE(wtabModes, TITLE='CT', /COLUMN, UVALUE='tabCT')
  bX=WIDGET_BASE(wtabModes,TITLE='Xray', /COLUMN, UVALUE='tabXray')
  bNM=WIDGET_BASE(wtabModes, TITLE='NM',/COLUMN, UVALUE='tabNM')
  
  wtabAnalysisCT=WIDGET_TAB(bCT, XSIZE=winX/2-60, YSIZE=230)
  wtabAnalysisXray=WIDGET_TAB(bX, XSIZE=winX/2-60, YSIZE=230)
  wtabAnalysisNM=WIDGET_TAB(bNM, XSIZE=winX/2-60, YSIZE=230)
  
  ; *************************CT tests*****************************************************

  ;--------------- Linear dimensions DIM
  bDim=WIDGET_BASE(wtabAnalysisCT, TITLE='Dim', /COLUMN)
  lblDimInfo=WIDGET_LABEL(bDim, VALUE='Find center of rod +/- 25mm from center with a margin of 10mm and calculate distance between rods')
  bDimBtns=WIDGET_BASE(bDim, /ROW) 
  btnDim=WIDGET_BUTTON(bDimBtns, VALUE='Calculate linear dimensions', UVALUE='dim')

  ;---------------Homogeneity--------
  bHomog=WIDGET_BASE(wtabAnalysisCT, Title='Homogeneity', /COLUMN)

  bHomogSize=WIDGET_BASE(bHomog, /ROW)
  lblHomogROIsz = WIDGET_LABEL(bHomogSize, VALUE='ROI radius (mm)')
  txtHomogROIsz = WIDGET_TEXT(bHomogSize, VALUE=STRING(config.HomogROIsz,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  mlH1=WIDGET_LABEL(bHomogSize, VALUE='', XSIZE=20)
  lblHomogROIdist = WIDGET_LABEL(bHomogSize, VALUE='Radius to ROIs (mm)')
  txtHomogROIdist = WIDGET_TEXT(bHomogSize, VALUE=STRING(config.HomogROIdist,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bHomogBtns=WIDGET_BASE(bHomog, /ROW)
  btnHomogROI=WIDGET_BUTTON(bHomogBtns, VALUE='Show/update ROI', UVALUE='drawROIhomog')
  btnHomog=WIDGET_BUTTON(bHomogBtns, VALUE='Calculated homogeneity', UVALUE='homog')

  ;---------------Noise--------
  bNoise=WIDGET_BASE(wtabAnalysisCT, Title='Noise', /COLUMN)

  bNoiseROI=WIDGET_BASE(bNoise, /ROW)
  lblNoiseROIsz = WIDGET_LABEL(bNoiseROI, VALUE='ROI radius (mm)')
  txtNoiseROIsz = WIDGET_TEXT(bNoiseROI, VALUE=STRING(config.NoiseROIsz,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bNoiseBtns=WIDGET_BASE(bNoise, /ROW)
  btnNoiseROI=WIDGET_BUTTON(bNoiseBtns, VALUE='Show/update ROI', UVALUE='drawROInoise')
  btnNoise=WIDGET_BUTTON(bNoiseBtns, VALUE='Calculated noise', UVALUE='noise')

  ;----------------MTF------------------
  bMTF=WIDGET_BASE(wtabAnalysisCT, TITLE='MTF',/Column)
  bMTFsettings=WIDGET_BASE(bMTF, /ROW)
  
  bMTFlft=WIDGET_BASE(bMTFsettings,/COLUMN)
  cw_typeMTF=CW_BGROUP(bMTFlft, ['Bead','Wire','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME, SET_VALUE=config.MTFtype)

  bMTFroiSz=WIDGET_BASE(bMTFlft, /ROW)
  lblMTFroiSz=WIDGET_LABEL(bMTFroiSz, VALUE='ROI size from center (mm)')
  txtMTFroiSz=WIDGET_TEXT(bMTFroiSz, VALUE=STRING(config.MTFroiSz,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)

  bMTFrgt=WIDGET_BASE(bMTFsettings,/COLUMN)
  bCutLSF=WIDGET_BASE(bMTFrgt, /NONEXCLUSIVE, /ROW)
  btnCutLSF=WIDGET_BUTTON(bCutLSF, VALUE='Cut LSF tails', UVALUE='cutLSF')
  bCutLSFW=WIDGET_BASE(bMTFrgt, /ROW)
  lblCutLSFW=WIDGET_LABEL( bCutLSFW, VALUE='Cut LSF from halfmax (#FWHM)')
  txtCutLSFW=WIDGET_TEXT( bCutLSFW, VALUE='5', /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  bCutLSFW2=WIDGET_BASE(bMTFrgt,/ROW)
  lblCutLSFW2=WIDGET_LABEL( bCutLSFW2, VALUE='Fade out cut within (#FWHM)')
  txtCutLSFW2=WIDGET_TEXT( bCutLSFW2, VALUE='2', /EDITABLE, XSIZE=5, SCR_YSIZE=20)

  cw_plotMTF=CW_BGROUP(bMTFsettings, ['Centered xy profiles', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, SET_VALUE=config.plotMTF, UVALUE='cw_plotMTF')
  
  bMTFbtns=WIDGET_BASE(bMTF, /ROW)
  btnMTFroi=WIDGET_BUTTON(bMTFbtns, VALUE='Show/update ROIs', UVALUE='drawMTFroi')
  btnMTF=WIDGET_BUTTON(bMTFbtns, VALUE='Calculate MTF', UVALUE='MTF')

  ;------------ NPS ---------------------
  bNPS=WIDGET_BASE(wtabAnalysisCT, TITLE='NPS',/Column)
  bNPSroiSz=WIDGET_BASE(bNPS, /ROW)
  lblNPSroiSz=WIDGET_LABEL(bNPSroiSz, VALUE='ROI size (pix)')
  txtNPSroiSz=WIDGET_TEXT(bNPSroiSz, VALUE=STRING(config.NPSroiSz,FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  bNPSsubSz=WIDGET_BASE(bNPS, /ROW)
  lblNPSsubSz=WIDGET_LABEL(bNPSsubSz, VALUE='Subimage size (pix)')
  txtNPSsubSz=WIDGET_TEXT(bNPSsubSz, VALUE=STRING(config.NPSsubSz,FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS) 
  lblNPSsubSz2=WIDGET_LABEL(bNPSsubSz, VALUE=' x ROI size = ')
  lblNPSsubSzMM=WIDGET_LABEL(bNPSsubSz, VALUE='', XSIZE=20, SCR_XSIZE=20)
  lblNPSsubSz3=WIDGET_LABEL(bNPSsubSz, VALUE=' mm')
  bNPStotPix=WIDGET_BASE(bNPS, /ROW)
  lblNPStotPix0=WIDGET_LABEL(bNPStotPix, VALUE='# independent pixels/image: ')
  nn=((2*LONG(config.NPSsubSz)-1)*LONG(config.NPSroiSz))^2
  lblNPStotPix=wIDGET_LABEL(bNPStotPix, VALUE=STRING(nn, FORMAT='(i0)'), /DYNAMIC_RESIZE)
  
  bNPSbtns=WIDGET_BASE(bNPS, /ROW)
  btnNPSroi=WIDGET_BUTTON(bNPSbtns, VALUE='Show/update ROIs', UVALUE='drawNPSroi')
  btnNPS=WIDGET_BUTTON(bNPSbtns, VALUE='Calculate NPS', UVALUE='NPS')
  lblWarnMlNPS=WIDGET_LABEL(bNPS, VALUE='')
  lblWarnNPS0=WIDGET_LABEL(bNPS, VALUE='Warning: Consider test as "under construction".')
  lblWarnNPS=WIDGET_LABEL(bNPS, VALUE='     The user must verify NPS results (normalization in particular) due to programmers lack of competence. ')
  
  ;----------------User defined ROI------------
  bROI=WIDGET_BASE(wtabAnalysisCT, TITLE='ROI',/COLUMN)
  typeROI=CW_BGROUP(bROI, ['Define new','Load saved ROI (.sav)'], /EXCLUSIVE, LABEL_TOP='Define ROI and get min/max/avg/stdev', /FRAME, SET_VALUE=config.typeROI, UVALUE='typeROI')
  btnDefROI =WIDGET_BUTTON(bROI, VALUE = 'Define ROI', UVALUE='ROI')
  
  ;-------------CT linearity-------------
  bLinearity=WIDGET_BASE(wtabAnalysisCT, Title='CT Number Linearity', /ROW)
  bLinSettings=WIDGET_BASE(bLinearity, /COLUMN)
  mlLin=WIDGET_LABEL(bLinSettings, VALUE='', YSIZE=20)
  labLinearity=WIDGET_LABEL(bLinSettings, VALUE='Get CT Numbers for all loaded images')
   mlLin1=WIDGET_LABEL(bLinSettings, VALUE='', YSIZE=20)
  bLinConfig=WIDGET_BASE(bLinSettings, /ROW)
  lblSampleRad = WIDGET_LABEL(bLinConfig, VALUE='ROI radius (mm)', XSIZE=80)
  txtLinROIrad = WIDGET_TEXT(bLinConfig, VALUE=STRING(config.LinROIrad,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  lblMlSR=WIDGET_LABEL(bLinConfig, VALUE='', XSIZE=20)
  lblLargeRad = WIDGET_LABEL(bLinConfig, VALUE='Radius to ROIs (mm)', XSIZE=100)
  txtLinROIrad2 = WIDGET_TEXT(bLinConfig, VALUE=STRING(config.LinROIrad2,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  bLinButtons=WIDGET_BASE(bLinSettings, /ROW)
  btnLinRois=WIDGET_BUTTON(bLinButtons, VALUE='Show/update ROIs', UVALUE='drawLinRois')
  btnLinearity=WIDGET_BUTTON(bLinButtons, VALUE='Get CT numbers', UVALUE='Linearity')
  
  ;---------------Slice thickness--------
  bSliceThick=WIDGET_BASE(wtabAnalysisCT, Title='Slice thickness', /COLUMN)
  bRampDist=WIDGET_BASE(bSliceThick, /ROW)
  lblRampDist = WIDGET_LABEL(bRampDist, VALUE='Center to ramp distance (mm)')
  txtRampDist = WIDGET_TEXT(bRampDist, VALUE=STRING(config.RampDist,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bRampLen=WIDGET_BASE(bSliceThick, /ROW)
  lblRampLen = WIDGET_LABEL(bRampLen, VALUE='Profile length (mm)')
  txtRampLen = WIDGET_TEXT(bRampLen, VALUE=STRING(config.RampLen,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bRampBack=WIDGET_BASE(bRampLen, /ROW)
  lblBackG = WIDGET_LABEL(bRampBack, VALUE='Background from outer (mm)')
  txtRampBackG = WIDGET_TEXT(bRampBack, VALUE=STRING(config.RampBackG,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bRampSearch=WIDGET_BASE(bSliceThick, /ROW)
  lblRampSearch = WIDGET_LABEL(bRampSearch, VALUE='Search for maximum in profile')
  txtRampSearch = WIDGET_TEXT(bRampSearch, VALUE=STRING(config.RampSearch,FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  lblRampSearch2 = WIDGET_LABEL(bRampSearch, VALUE='# pix from center of ramp')
  bRampAverage=WIDGET_BASE(bSliceThick, /ROW)
  lblRampAverage = WIDGET_LABEL(bRampAverage, VALUE='Use profile from average of ')
  txtRampAverage = WIDGET_TEXT(bRampAverage, VALUE=STRING(config.RampAvg,FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  lblRampAverage2 = WIDGET_LABEL(bRampAverage, VALUE='# neighbour profiles from profile with max value')
  bSliceThickBtns=WIDGET_BASE(bSliceThick, /ROW)
  btnSliceThickRamps=WIDGET_BUTTON(bSliceThickBtns, VALUE='Show/update ramps', UVALUE='drawRamps')
  btnSliceThick=WIDGET_BUTTON(bSliceThickBtns, VALUE='Get Slice Thickness', UVALUE='SliceThick')

  ;---------------FWHM---------------- move together with MTF later
  bFwhm=WIDGET_BASE(wtabAnalysisCT, Title='FWHM', /COLUMN)
  lblFwhm=WIDGET_LABEL(bFWhm, VALUE='Code based on PSF.pro & CALCULATE_LSF_LIST.pro developed at DNR (Oslo, Norway) ')
  lblFwhm2=WIDGET_LABEL(bFWhm, VALUE=' by Arne Skretting, Wibeke Nordh'+string(248B)+'y, Alise Larsen and Kristine Eldevik')
  lblFwhmML=WIDGET_LABEL(bFWhm, VALUE='', YSIZE=20)
  lblFwhm3=WIDGET_LABEL(bFWhm, VALUE='FWHM calculated from average of 10 pixelrows.')
  lblFwhmML=WIDGET_LABEL(bFWhm, VALUE='', YSIZE=20)
  btnFwhm=WIDGET_BUTTON(bFwhm, VALUE='Calculate FWHM' , UVALUE='fwhm')
  

  ;**********************X ray tests *******************************************************
  
  ;---------------STP--------
  bSTP=WIDGET_BASE(wtabAnalysisXray, Title='STP', /COLUMN)

  bStpSettings=WIDGET_BASE(bSTP, /ROW)
  lblStpROIsz = WIDGET_LABEL(bStpSettings, VALUE='ROI radius (mm)')
  txtStpROIsz = WIDGET_TEXT(bStpSettings, VALUE=STRING(config.STProiSz,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  Qvals=[21759.,30174.,32362.,31077.,1]
  lblStpBeamQuality = WIDGET_LABEL(bStpSettings, VALUE='     Beam quality')
  ddlRQA = WIDGET_COMBOBOX(bStpSettings, VALUE=['RQA 3','RQA 5','RQA 7','RQA 9','other'], UVALUE='ddlRQA', /LIST_EVENTS)
  WIDGET_CONTROL, ddlRQA, SET_COMBOBOX_SELECT=1
  txtRQA = WIDGET_TEXT(bStpSettings, VALUE=STRING(Qvals(1),FORMAT='(i0)'), UVALUE='txtRQA', XSIZE=7, /EDITABLE)
  lblQ = WIDGET_LABEL(bStpSettings, VALUE='1/mm^2uGy')
  bStpBtns=WIDGET_BASE(bSTP, /ROW)
  btnStpROI=WIDGET_BUTTON(bStpBtns, VALUE='Show/update ROI', UVALUE='drawROIstp')
  btnStp=WIDGET_BUTTON(bStpBtns, VALUE='Find pixel values', UVALUE='STPpix')
  ;bStpProcess = WIDGET_BASE(bSTP, /ROW)
  btnImportDose=WIDGET_BUTTON(bStpBtns, VALUE='Import dose values', UVALUE='impDose')
  btnCalcSTP=WIDGET_BUTTON(bStpBtns, VALUE='Calculate STP', UVALUE='calcSTP')
  lblWarnMlSTP=WIDGET_LABEL(bSTP, VALUE='')
  lblWarnStp0=WIDGET_LABEL(bSTP, VALUE='Warning: Consider test as "under construction".')
  lblWarnStp=WIDGET_LABEL(bSTP, VALUE='     Only linear fit implemented for STP, beam quality and Qvalue might not be used correctly')

  ;---------------Homogeneity--------
  bHomogX=WIDGET_BASE(wtabAnalysisXray, Title='Homogeneity', /COLUMN)

  bHomogSizeX=WIDGET_BASE(bHomogX, /ROW)
  lblHomogROIszX = WIDGET_LABEL(bHomogSizeX, VALUE='ROI radius (mm)')
  txtHomogROIszX = WIDGET_TEXT(bHomogSizeX, VALUE=STRING(config.HomogROIszX,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bHomogBtnsX=WIDGET_BASE(bHomogX, /ROW)
  btnHomogROIX=WIDGET_BUTTON(bHomogBtnsX, VALUE='Show/update ROI', UVALUE='drawROIhomog')
  btnHomogX=WIDGET_BUTTON(bHomogBtnsX, VALUE='Calculated homogeneity', UVALUE='homog')

  ;---------------Noise--------
  bNoiseX=WIDGET_BASE(wtabAnalysisXray, Title='Noise', /COLUMN)

  bNoiseROIX=WIDGET_BASE(bNoiseX, /ROW)
  lblNoiseROIszX = WIDGET_LABEL(bNoiseROIX, VALUE='ROI 90 % of image area ')
  ;txtNoiseROIszX = WIDGET_TEXT(bNoiseROIX, VALUE=STRING(config.NoiseROIszX,FORMAT='(f0.0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bNoiseBtnsX=WIDGET_BASE(bNoiseX, /ROW)
  btnNoiseROIX=WIDGET_BUTTON(bNoiseBtnsX, VALUE='Show/update ROI', UVALUE='drawROInoise')
  btnNoiseX=WIDGET_BUTTON(bNoiseBtnsX, VALUE='Calculated noise', UVALUE='noise')

  ;----------------MTF------------------
  bMTFsettingsX=WIDGET_BASE(wtabAnalysisXray, TITLE='MTF',/COLUMN)
  bMTFX=WIDGET_BASE(bMTFsettingsX, /ROW)
  cw_formLSFX=CW_BGROUP(bMTFX, ['Exponential','Gaussian','None'], /EXCLUSIVE, LABEL_TOP='LSF fit to...', /FRAME, SET_VALUE=config.MTFtypeX) 
  bLSFfilterX=WIDGET_BASE(bMTFX, /COLUMN)
  bCutLSFX=WIDGET_BASE(bLSFfilterX, /NONEXCLUSIVE, /ROW)
  btnCutLSFX=WIDGET_BUTTON(bCutLSFX, VALUE='Cut LSF tails', UVALUE='cutLSF')
  bCutLSFWX=WIDGET_BASE(bLSFfilterX, /ROW)
  lblCutLSFWX=WIDGET_LABEL( bCutLSFWX, VALUE='Cut LSF from halfmax (#FWHM)')
  txtCutLSFWX=WIDGET_TEXT( bCutLSFWX, VALUE='5', /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  cw_plotMTFX=CW_BGROUP(bMTFX, ['Edge position', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, SET_VALUE=config.plotMTFX, UVALUE='cw_plotMTFX')
  
  bMTFroiSzX=WIDGET_BASE(bMTFsettingsX, /ROW)
  lblMTFroiSzX=WIDGET_LABEL(bMTFroiSzX, VALUE='ROI width x height (mm)')
  txtMTFroiSzX=WIDGET_TEXT(bMTFroiSzX, VALUE=STRING(config.MTFroiSzX(0),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  lblMTFx=WIDGET_LABEL(bMTFroiSzX, VALUE=' x ')
  txtMTFroiSzY=WIDGET_TEXT(bMTFroiSzX, VALUE=STRING(config.MTFroiSzX(1),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  
  bMTFbtnsX=WIDGET_BASE(bMTFsettingsX, /ROW)
  btnMTFroiX=WIDGET_BUTTON(bMTFbtnsX, VALUE='Show/update ROIs', UVALUE='drawMTFroi')
  btnMTFX=WIDGET_BUTTON(bMTFbtnsX, VALUE='Calculate MTF', UVALUE='MTFX')

  ;----------------NPS------------------
  bNPSX=WIDGET_BASE(wtabAnalysisXray, TITLE='NPS',/Column)
  bVarianceBtnsX=WIDGET_BASE(bNPSX,/ROW)
  btnVarImageX=WIDGET_BUTTON(bVarianceBtnsX, VALUE='Calculate variance image', UVALUE='varImage')
  lblVarX=WIDGET_LABEL(bVarianceBtnsX, VALUE='  to check that the variance is uniform and without major artifacts.')
  bNPSroiSzX=WIDGET_BASE(bNPSX, /ROW)
  lblNPSroiSzX=WIDGET_LABEL(bNPSroiSzX, VALUE='ROI size (pix)')
  txtNPSroiSzX=WIDGET_TEXT(bNPSroiSzX, VALUE=STRING(config.NPSroiSzX,FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  bNPSsubSzX=WIDGET_BASE(bNPSX, /ROW)
  lblNPSsubSzX=WIDGET_LABEL(bNPSsubSzX, VALUE='Subimage size (pix)')
  txtNPSsubSzX=WIDGET_TEXT(bNPSsubSzX, VALUE=STRING(config.NPSsubSzX,FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  lblNPSsubSz2X=WIDGET_LABEL(bNPSsubSzX, VALUE=' x ROI size = ')
  lblNPSsubSzMMX=WIDGET_LABEL(bNPSsubSzX, VALUE='', XSIZE=20, SCR_XSIZE=20)
  lblNPSsubSz3X=WIDGET_LABEL(bNPSsubSzX, VALUE=' mm')
  bNPStotPixX=WIDGET_BASE(bNPSX, /ROW)
  lblNPStotPix0X=WIDGET_LABEL(bNPStotPixX, VALUE='# independent pixels/image (preferrably 4 mill in total): ')
  nn=((2*LONG(config.NPSsubSzX)-1)*LONG(config.NPSroiSzX))^2
  lblNPStotPixX=wIDGET_LABEL(bNPStotPixX, VALUE=STRING(nn, FORMAT='(i0)'), /DYNAMIC_RESIZE)

  bNPSbtnsX=WIDGET_BASE(bNPSX, /ROW)
  btnNPSroiX=WIDGET_BUTTON(bNPSbtnsX, VALUE='Show/update ROIs', UVALUE='drawNPSroi')
  btnNPSX=WIDGET_BUTTON(bNPSbtnsX, VALUE='Calculate NPS', UVALUE='NPS')
  lblWarnMlNPSX=WIDGET_LABEL(bNPSX, VALUE='')
  lblWarnNPSX0=WIDGET_LABEL(bNPSX, VALUE='Warning: Consider test as "under construction".')
  lblWarnNPSX=WIDGET_LABEL(bNPSX, VALUE='     The user must verify NPS results (normalization in particular) due to programmers lack of competence. ')
  
  ;----------------User defined ROI------------
  bROIX=WIDGET_BASE(wtabAnalysisXray, TITLE='ROI',/COLUMN)
  typeROIX=CW_BGROUP(bROIX, ['Define new','Load saved ROI (.sav)'], /EXCLUSIVE, LABEL_TOP='Define ROI and get min/max/avg/stdev', /FRAME, SET_VALUE=config.typeROIX, UVALUE='typeROIX')
  btnDefROIX =WIDGET_BUTTON(bROIX, VALUE = 'Define ROI', UVALUE='ROI')

  ;***********************NM tests**********************************************************
  bEnergySpec=WIDGET_BASE(wtabAnalysisNM, TITLE='Energy spectrum', /COLUMN)
  bEnergySpecBtns=WIDGET_BASE(bEnergySpec, /ROW)
  btnLoadSpec=WIDGET_BUTTON(bEnergySpecBtns, VALUE='Load spectrum', UVALUE='loadSpectrum')
  
  ;---------------Homogeneity--------
  bHomogNM=WIDGET_BASE(wtabAnalysisNM, Title='Homogeneity', /COLUMN)

  bHomogSizeNM=WIDGET_BASE(bHomogNM, /ROW)
  lblHomogROIszNM = WIDGET_LABEL(bHomogSizeNM, VALUE='ROI radius (mm)')
  txtHomogROIszNM = WIDGET_TEXT(bHomogSizeNM, VALUE=STRING(config.HomogROIszNM,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  bHomogDistNM=WIDGET_BASE(bHomogNM, /ROW)
  lblHomogROIdistNM = WIDGET_LABEL(bHomogDistNM, VALUE='ROI distance x, y (mm)')
  txtHomogROIdistXNM = WIDGET_TEXT(bHomogDistNM, VALUE=STRING(config.HomogROIdistNM(0),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  txtHomogROIdistYNM = WIDGET_TEXT(bHomogDistNM, VALUE=STRING(config.HomogROIdistNM(1),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  cw_homogNM=CW_BGROUP(bHomogNM, ['Planar (WB)', 'SPECT'], /EXCLUSIVE, LABEL_TOP='Image type...', /FRAME, SET_VALUE=0, UVALUE='cw_homogNM')
  bHomogBtnsNM=WIDGET_BASE(bHomogNM, /ROW)
  btnHomogROINM=WIDGET_BUTTON(bHomogBtnsNM, VALUE='Show/update ROI', UVALUE='drawROIhomog')
  btnHomogNM=WIDGET_BUTTON(bHomogBtnsNM, VALUE='Calculate homogeneity', UVALUE='homog')
  
  ;-----------Scan speed------------
  bScanSpeed=WIDGET_BASE(wtabAnalysisNM, Title='Scan Speed', /COLUMN)
  bAvgSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lblAvgSpeedNM=WIDGET_LABEL(bAvgSpeedNM, VALUE='Average over ROI with width (pix)' )
  txtNAvgSpeedNM=WIDGET_TEXT(bAvgSpeedNM, VALUE=STRING(config.scanSpeedAvg, FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  bSpeedROIheight=WIDGET_BASE(bScanSpeed, /ROW)
  lblSpeedROIheight=WIDGET_LABEL(bSpeedROIheight, VALUE='ROI heigth (cm)' )
  txtSpeedROIheight=WIDGET_TEXT(bSpeedROIheight, VALUE=STRING(config.scanSpeedHeight, FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
  bMedianSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lblSpeedMedian=WIDGET_LABEL(bMedianSpeedNM, VALUE='Median filter width (pix)')
  txtScanSpeedMedian=WIDGET_TEXT(bMedianSpeedNM, VALUE=STRING(config.scanSpeedFiltW, FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
  btnPlotScanSpeed = WIDGET_BUTTON(bScanSpeed, VALUE='Plot y-profile and median filtered profile', UVALUE='plotScanSpeed')
  
  ;-------------Contrast-------------
   bContrastNM=WIDGET_BASE(wtabAnalysisNM, Title='Contrast', /COLUMN)
   bConSettingsNM=WIDGET_BASE(bContrastNM, /ROW)
   lblConR1NM = WIDGET_LABEL(bConSettingsNM, VALUE='ROI radius (mm)', XSIZE=80)
   txtConR1NM = WIDGET_TEXT(bConSettingsNM, VALUE=STRING(config.contrastRad1,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
   lblMlConRNM=WIDGET_LABEL(bConSettingsNM, VALUE='', XSIZE=20)
   lblConR2NM = WIDGET_LABEL(bConSettingsNM, VALUE='Radius to ROIs (mm)', XSIZE=100)
   txtConR2NM = WIDGET_TEXT(bConSettingsNM, VALUE=STRING(config.contrastRad2,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20)
   bConButtonsNM=WIDGET_BASE(bContrastNM, /ROW)
   btnConRoisNM=WIDGET_BUTTON(bConButtonsNM, VALUE='Show/update ROIs', UVALUE='drawConRoisNM')
   btnContrastNM=WIDGET_BUTTON(bConButtonsNM, VALUE='Calculate contrast', UVALUE='contrastNM')

   ;----------------MTF------------------
   bMTFNM=WIDGET_BASE(wtabAnalysisNM, TITLE='MTF',/Column)
   bMTFsettingsNM=WIDGET_BASE(bMTFNM, /ROW)
   bMTFlftNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)
   cw_typeMTFNM=CW_BGROUP(bMTFlftNM, ['Point','Line','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME, SET_VALUE=config.MTFtypeNM)

   bMTFroiSzNM=WIDGET_BASE(bMTFlftNM, /ROW)
   lblMTFroiSzXNM=WIDGET_LABEL(bMTFroiSzNM, VALUE='ROI width x height (mm)')
   txtMTFroiSzXNM=WIDGET_TEXT(bMTFroiSzNM, VALUE=STRING(config.MTFroiSzNM(0),FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
   lblMTFXNM=WIDGET_LABEL(bMTFroiSzNM, VALUE='x')
   txtMTFroiSzYNM=WIDGET_TEXT(bMTFroiSzNM, VALUE=STRING(config.MTFroiSzNM(1),FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20)
   bMTF3dNM=WIDGET_BASE(bMTFlftNM, /ROW, /NONEXCLUSIVE)
   MTF3dNM=WIDGET_BUTTON(bMTF3dNM, VALUE='Analyse 3d')

   bMTFrgtNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)
   bCutLSFNM=WIDGET_BASE(bMTFrgtNM, /NONEXCLUSIVE, /ROW)
   btnCutLSFNM=WIDGET_BUTTON(bCutLSFNM, VALUE='Cut LSF tails', UVALUE='cutLSF')
   bCutLSFWNM=WIDGET_BASE(bMTFrgtNM, /ROW)
   lblCutLSFWNM=WIDGET_LABEL( bCutLSFWNM, VALUE='Cut LSF from halfmax (#FWHM)')
   txtCutLSFWNM=WIDGET_TEXT( bCutLSFWNM, VALUE='5', /EDITABLE, XSIZE=3, SCR_YSIZE=20)

   cw_plotMTFNM=CW_BGROUP(bMTFsettingsNM, ['Centered xy profiles', 'Line', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, SET_VALUE=config.plotMTFNM, UVALUE='cw_plotMTFNM')

   bMTFbtnsNM=WIDGET_BASE(bMTFNM, /ROW)
   btnMTFroiNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Show/update ROIs', UVALUE='drawMTFroi')
   btnMTFNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Calculate MTF', UVALUE='MTFNM')

  ;******************************************************************************************
  ;********************* Result panel *********************************************************
  bPlot = WIDGET_BASE(bRgt, /COLUMN)
  
  wtabResult=WIDGET_TAB(bPlot, XSIZE=winX/2-50, YSIZE=480, UVALUE='tabResults')
  bTableRes=WIDGET_BASE(wtabResult, TITLE='Table of results', /COLUMN, UVALUE='tabTableRes')
  bPlotRes=WIDGET_BASE(wtabResult, TITLE='Plot results', /COLUMN, UVALUE='tabPlotRes')
  bImageRes=WIDGET_BASE(wtabResult,TITLE='Image results', /COLUMN, UVALUE='tabImageRes')
  
  ;----table-----------
  toolbarTable=WIDGET_BASE(bTableRes,/ROW,/TOOLBAR)
  ;toolExportTbl=WIDGET_BUTTON(toolbarTable, VALUE=thisPath+'images\export.bmp',/BITMAP, TOOLTIP='Export results', UVALUE='exportTbl')
  toolCopyTbl=WIDGET_BUTTON(toolbarTable, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard', UVALUE='copyTbl')

  bResults = WIDGET_BASE(bTableRes, /COLUMN)
  resTab=WIDGET_TABLE(bResults, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], /NO_ROW_HEADERS, SCR_XSIZE=winX/2-160, SCR_YSIZE=150, /ALL_EVENTS)
  
  ;----plot------------
  toolbarPlot=WIDGET_BASE(bPlotRes,/ROW,/TOOLBAR)
  toolCopyCurve=WIDGET_BUTTON(toolbarPlot, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy curve to clipboard', UVALUE='copyCurve')
  tooliPlot=WIDGET_BUTTON(toolbarPlot, VALUE='iPlot', TOOLTIP='Send curves to iPlot for further analysis', UVALUE='iPlot')
  bDrawPlot=WIDGET_BASE(bPlotRes, /ROW)
  drawPlot  = WIDGET_DRAW(bDrawPlot, XSIZE=450, YSIZE=380, RETAIN=2);GRAPHICS_LEVEL=2 hvis object graphics 
  statPlot = WIDGET_TEXT(bDrawPlot, XSIZE=50, YSIZE=10, VALUE='')
  bRangeX=WIDGET_BASE(bPlotRes, /ROW)
  lblRangeX = WIDGET_LABEL(bRangeX, VALUE='Horizontal axis range (lower, upper)', XSIZE=170)
  txtMinRangeX = WIDGET_TEXT(bRangeX, VALUE='0', /EDITABLE, XSIZE=10, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  lblMlmRx= WIDGET_LABEL(bRangeX, VALUE=', ', XSIZE=10)
  txtMaxRangeX = WIDGET_TEXT(bRangeX, VALUE='1', /EDITABLE, XSIZE=10, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  setRangeMinMaxX = WIDGET_BUTTON(bRangeX, VALUE='Set to min/max', UVALUE='setRangeMinMaxX')
  ;bRangeAcc=WIDGET_BASE(bRangeX, /NONEXCLUSIVE)
  ;rangeAcc=WIDGET_BUTTON(bRangeAcc, VALUE='Use accurate', UVALUE='rangeAcc', SCR_YSIZE=20)
  
  bRangeY=WIDGET_BASE(bPlotRes, /ROW)
  lblRangeY = WIDGET_LABEL(bRangeY, VALUE='Vertical axis range (lower, upper)', XSIZE=170)
  txtMinRangeY = WIDGET_TEXT(bRangeY, VALUE='0', /EDITABLE, XSIZE=10, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  lblMlmRy= WIDGET_LABEL(bRangeY, VALUE=', ', XSIZE=10)
  txtMaxRangeY = WIDGET_TEXT(bRangeY, VALUE='1', /EDITABLE, XSIZE=10, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS)
  setRangeMinMaxY = WIDGET_BUTTON(bRangeY, VALUE='Set to min/max', UVALUE='setRangeMinMaxY')
  
  ;----image------------
  toolbarImageRes=WIDGET_BASE(bImageRes,/ROW,/TOOLBAR)
  toolIimageRes = WIDGET_BUTTON(toolbarImageRes, VALUE=thisPath+'images\ax.bmp', /BITMAP, UVALUE='iImageRes', TOOLTIP='Send result to iImage')
  ;toolImageResCopy = WIDGET_BUTTON(toolbarImageRes, VALUE=thisPath+'images\snapshot.bmp', /BITMAP, UVALUE='snapRes', TOOLTIP='Copy image to clipboard')
  drawImageRes  = WIDGET_DRAW(bImageRes, XSIZE=450, YSIZE=450, RETAIN=2)
  
  ;bComment=WIDGET_BASE(bRgt,/COLUMN)
  ;lblComment=WIDGET_LABEL(bComment, VALUE='Comment to exported results:')
  ;txtComment= WIDGET_TEXT(bComment, VALUE='', XSIZE=350, YSIZE=5, /EDITABLE, SCR_XSIZE=winX/2-150)
  
  ;****************** BOTTOM Panel
  bDir=WIDGET_BASE(bMain,/ROW)
  lblDirectory=WIDGET_LABEL(bDir, VALUE='Full path:  ')
  lblDir=WIDGET_LABEL(bDir, VALUE='',xSIZE=winX-170, YSIZE=18, /SUNKEN_FRAME)
  
  loadct, 0, /SILENT
  WIDGET_CONTROL, bMain, /REALIZE 
  XMANAGER, 'ImageQC', bMain, /NO_BLOCK
  DEVICE, RETAIN=2, DECOMPOSED=0

end


