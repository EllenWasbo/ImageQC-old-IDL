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

pro ImageQC,  GROUP_LEADER=bMain

  COMPILE_OPT hidden
  COMMON VARI,  $
    lblDir, btnAppend, listFiles, lastList, lblLoadedN, lblProgress, lblSettings, selConfig, activeImg, activeResImg, nFrames, ROIs,  $
    marked, markedMulti, multiOpt, btnUseMulti, listSelMultiTemp, multiExpTable, $
    txtActive1, txtActive2, newline, $
    drawLarge, drawXY, coltable, btnSetColorTable, txtMinWL, txtMaxWL, txtCenterWL, txtWidthWL, lblCursorValue, lblCursorPos,lblCursorPosMM, $
    txtDeltaX, txtDeltaY, txtDeltaA, dxya, useDelta, lblDeltaO, lblDeltaOX, offxy, btnHideAnnot,$
    defPath, thisPath, structImgs, deciMark, listDeciMark, copyHeader, btnCopyHeader, headers, lastXY, lastXYreleased, mouseDown, $
    useMulti, modality, analyse, analyseStringsCT, analyseStringsXray, analyseStringsNM, analyseStringsSPECT, analyseStringsPET, results, $
    resTab, wtabResult, wtabModes,wtabAnalysisCT,wtabAnalysisXray,wtabAnalysisNM,wtabAnalysisSPECT, wtabAnalysisPET, $
    drawPlot, statPlot,drawImageRes, txtMinRangeX, txtMaxRangeX, txtMinRangeY, txtMaxRangeY, rangeAcc, $
    ROIres, typeROI, typeROIX, $
    MTFres, $
    cw_typeMTF, cw_plotMTF, txtMTFroiSz, btnCutLSF, txtcutLSFW, txtcutLSFW2,txtfreqMTF, $
    cw_formLSFX, cw_plotMTFX,  txtMTFroiSzX, txtMTFroiSzY, btnCutLSFX, txtcutLSFWX, $
    cw_typeMTFNM, cw_plotMTFNM, txtMTFroiSzXNM, txtMTFroiSzYNM, btnCutLSFNM, txtcutLSFWNM, $
    cw_typeMTFSPECT, cw_plotMTFSPECT, txtMTFroiSzSPECT, btnCutLSFSPECT, txtcutLSFWSPECT, MTF3dSPECT,$
    CTlinRes, CTlinROIs, CTlinROIpos, txtLinROIrad, txtLinROIradS, tblLin, linTabEdit, btnLinAvoidSearch, $
    sliceThickRes, sliceThickResTab,  ramps, txtRampDist, txtRampLen, txtRampBackG, txtRampSearch, txtRampAverage, cw_ramptype, cw_rampDens,  $
    homogRes, homogROIs, txtHomogROIsz, txtHomogROIszPET, txtHomogROIszX, txtHomogROIdist, txtHomogROIdistPET, $
    noiseRes, noiseROI, txtNoiseROIsz, $
    fwhmRes, dimRes, energyRes, $
    stpRes, txtStpROIsz, stpROI, txtRQA, Qvals, eiRes, $
    NPSres, NPSrois, $
    txtNPSroiSz, txtNPSroiDist, txtNPSsubNN, txtSmoothNPS, txtfreqNPS, btnNPSavg, $
    txtNPSroiSzX, txtNPSsubSzX, lblNPSsubSzMMX, lblNPStotpixX, $
    txtNAvgSpeedNM, txtScanSpeedMedian, txtSpeedROIheight,$
    txtVarImageROIsz, lblProgressVarX, $
    contrastRes, conROIs, txtConR1SPECT, txtConR2SPECT,$
    radialRes, txtRadialMedian, $
    unifRes, unifROI, $
    SNIres, SNIroi, $
    crossRes, crossROI, txtCrossROIsz, txtCrossMeasAct,txtCrossMeasActT, txtCrossMeasRest, txtCrossMeasRT, txtCrossScanAct, txtCrossScanStart,$
    txtCrossVol, txtCrossConc, txtCrossFactorPrev, txtCrossFactor,$
    rcRes, rcROIs, btnRCrev, cwRCexclude, cw_rcType

  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'

  ;always refresh structure of config file
  configS=updateConfigS(thisPath+'data\config.dat')
  quickTemp=updateQuickT(thisPath+'data\config.dat')
  IF N_ELEMENTS(quickTemp) EQ 0 THEN quickTemp=0 
  loadTemp=updateLoadT(thisPath+'data\config.dat')
  SAVE, configS, quickTemp, loadTemp, FILENAME=thisPath+'data\config.dat' 

  selConfig=configS.(0)
  config=configS.(selConfig)
  configDefault=configS.(1)
  
  configTags=TAG_NAMES(config)
  defPath=config.defPath

  structImgs=CREATE_STRUCT('empty',0); images with attributes in memory
  activeImg=0 ; active (selected image)
  activeResImg=0 ; result-image (fx 2d NPS)
  nFrames=0; if multiframe image loaded, nFrames=nImg
  analyseStringsCT=['HOMOG', 'NOISE', 'SLICETHICK', 'MTF', 'NPS','CTLIN','DIM', 'ROI', 'FWHM']
  analyseStringsXray=['STP','HOMOG','NOISE','EI','MTF','NPS','VARI','ROI']
  analyseStringsNM=['UNIF','SNI','ENERGYSPEC','SCANSPEED','MTF']
  analyseStringsSPECT=['MTF','RADIAL','CONTRAST']
  analyseStringsPET=['CROSSCALIB','HOMOG','RC']
  analyse=analyseStringsCT(0)
  modality=0; to save current modality for regretting switch and loose results
  marked=-1; indexes of marked files (-1 = all marked)
  markedMulti=-1; matrix of marked images for numbered tests (number of tests x number of images) -1 if useMuliMark is not set
  multiOpt=CREATE_STRUCT('CT',[1,2,3,4,0,0,0,0,0],'Xray',[1,2,3,4,5,0,0,0],'NM',[1,1,0,0,0],'SPECT', INTARR(3),'PET',INTARR(3)); structure of arrays corresponding to analyseStrings that have the option of being a numbered test for multimark/quicktest, number or 0 if not an option
  multiExpTable=-1
  results=INTARR(9); set to 1 when analyse performed and results is available, keep analyseString and tab-order equal
  dxya=[0,0,0.0,1]; [deltax,deltay,delta,show] for positioning of center/angle. Difference from imgSz/2 in pixels. Show (last param) used for redrawCT to overplot crosshair
  IF TOTAL(WHERE(configTags EQ 'OFFXY')) NE -1 THEN offxy=config.OFFXY ELSE offxy=configDefault.OFFXY
  CTlinROIs=0 & CTlinROIpos=0 & homogROIs=0 & noiseROI=0 & NPSrois=0 & conROIs=0 & crossROI=0; used to hold the rois for specific tests
  ramps=0; used to hold the 4 lines for slice thickness H-top,H-bottom,V1,V2
  lastXY=[-1,-1]; last mouseposition in draw window
  lastXYreleased=[-1,-1,-1]; x, y, time
  lastList=[-1,-1]; last first selected, time to control doubleclicks in fileList
  mouseDown=0; If mouse pressed in draw window and still not released 1=true
  coltable=0;  grayscale default
  deciMark=config.deciMark
  copyHeader=config.copyHeader
  if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)

  font1="Arial*15"

  winX=1400 &  winY=1000 ;actual winX not 1400 - adjusted down to 1250 - should be adapted to screen size (Todo)
  drawXY=500

  s=obj_new('idlsysmonitorinfo')
  nMon=s->GetNumberOfMonitors()
  pMon=s->GetPrimaryMonitorIndex(); wrong monitor index.... when first is second...
  rMon=s->GetRectangles()
  scsz=rMon[2:3,pMon]
  ;DEVICE, GET_SCREEN_SIZE=scsz

  IF scsz(0) LT winX THEN scX=scsz(0)-10 ELSE scX=winX
  IF scsz(1) LT winY THEN scY=scsz(1)-50 ELSE scY=winY-50

  bMain = WIDGET_BASE(TITLE='ImageQC v1.300', MBAR=bar, /COLUMN, XSIZE=winX, YSIZE=winY-60, XOFFSET=100, YOFFSET=100, X_SCROLL_SIZE=scX, Y_SCROLL_SIZE=scY, /TLB_KILL_REQUEST_EVENTS)
  bLarge = WIDGET_BASE(bMain, /ROW)
  bLft = WIDGET_BASE(bLarge, XSIZE=700, YSIZE=winY-90,/COLUMN)
  bRgt = WIDGET_BASE(bLarge, XSIZE=700, YSIZE=winY-90,/COLUMN)

  ;*****************MENU
  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  sett_menu=WIDGET_BUTTON(bar, VALUE='Settings',/MENU)
  help_menu=WIDGET_BUTTON(bar, VALUe='Help', /MENU)
  ;file_menu
  btnOpen=WIDGET_BUTTON(file_menu, VALUE='Open DICOM file or series', UVALUE='open', ACCELERATOR='Ctrl+O')
  btnSaveStruct=WIDGET_BUTTON(file_menu, VALUE='Save active file as IDL structure (.dat-file)', UVALUE='saveDat')
  btnClose=WIDGET_BUTTON(file_menu, VALUE='Close all images', UVALUE='close', /SEPARATOR)
  btnExit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='exit', ACCELERATOR='Ctrl+X')
  ;sett_menu
  btnDefPath=WIDGET_BUTTON(sett_menu, VALUE='Define default path for current parameter set', UVALUE='defpath')
  ;btnPref=WIDGET_BUTTON(sett_menu, VALUE='Set preferences on export format for current parameter set', UVALUE='pref')
  btnConfig=WIDGET_BUTTON(sett_menu, VALUE='Save current settings to current paramater set', UVALUE='saveCurrConfig')
  btnRestoreConfig=WIDGET_BUTTON(sett_menu, VALUE='Restore and replace parameter sets with backup config file', UVALUE='restoreConfig')
  btnSettingsMenu=WIDGET_BUTTON(sett_menu, VALUE='Edit/manage parameter sets',UVALUE='manageSettings', /SEPARATOR)
  
  ;help_menu
  btnInfo=WIDGET_BUTTON(help_menu, VALUE='Wiki on GitHub.com', UVALUE='info')
  btnAbout=WIDGET_BUTTON(help_menu, VALUE='About ImageQC...',UVALUE='about')

  toolbarLft=WIDGET_BASE(bLft,/ROW,/TOOLBAR)
  toolOpen=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\open.bmp',/BITMAP, UVALUE='open', TOOLTIP='Open DICOM file(s)')
  toolOpenMultiple=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\openM.bmp',/BITMAP, UVALUE='openMulti', TOOLTIP='Open DICOM file(s) from multiple folders')

  toolml3=WIDGET_LABEL(toolbarLft, VALUE='', XSIZE=20)
  btnImgTop=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_top.bmp',/BITMAP, UVALUE='imgTop', TOOLTIP='Place selected image(s) at top of list')
  btnImgUp=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='imgUp', TOOLTIP='Move selected image(s) upwards in list')
  btnImgDown=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='imgDown', TOOLTIP='Move selected image(s) downwards of list')
  btnImgBottom=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_bottom.bmp',/BITMAP, UVALUE='imgBottom', TOOLTIP='Place selected image(s) at bottom of list')
  toolml4=WIDGET_LABEL(toolbarLft, VALUE='', XSIZE=5)
  btnSort=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\sort.bmp',/BITMAP, UVALUE='sortImg', TOOLTIP='Sort selected images by...')

  lblProgress = WIDGET_LABEL(toolbarLft, VALUE=' ', XSIZE=350)

  ;****************** left Panel
  bInfoLoaded=WIDGET_BASE(bLft, /ROW, YSIZE=220)
  bInfoLft=WIDGET_BASE(bInfoLoaded, /COLUMN)

  ;list ++
  bList=WIDGET_BASE(bInfoLft, /ROW)
  bListLoaded=WIDGET_BASE(bList, /COLUMN)
  bListLoadedTitle=WIDGET_BASE(bListLoaded, /ROW)
  lblLoaded=WIDGET_LABEL(bListLoadedTitle, VALUE='Loaded images ( ', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  lblLoadedN=WIDGET_LABEL(bListLoadedTitle, VALUE='0 )', /ALIGN_LEFT, FONT="Arial*ITALIC*16", XSIZE=250)
  bAppend=WIDGET_BASE(bListLoadedTitle, /NONEXCLUSIVE)
  btnAppend=WIDGET_BUTTON(bAppend, VALUE='Append', FONT=font1, YSIZE=15, TOOLTIP='Append or replace images when opening new images')
  listActions=WIDGET_DROPLIST(bListLoadedTitle, VALUE=['Mark selected','Remove all marks','Remove mark from selected','Select inverse','Select marked','Close selected'], UVALUE='listActions')
  listFiles=WIDGET_LIST(bListLoaded, XSIZE=650, SCR_XSIZE=winX/2-100, YSIZE=1, SCR_YSIZE=160, MULTIPLE=1, UVALUE='filelist')

  bPrevNext = WIDGET_BASE(bList, /COLUMN)
  mlprevnext=WIDGET_LABEL(bPrevNext, VALUE='', YSIZE=70, XSIZE=30)
  btnPrev = WIDGET_BUTTON(bPrevNext, VALUE=thisPath+'images\shift_up.bmp',/BITMAP,UVALUE='prev',TOOLTIP='Previous image in list')
  btnNext = WIDGET_BUTTON(bPrevNext, VALUE=thisPath+'images\shift_down.bmp', /BITMAP,UVALUE='next',TOOLTIP='Next image in list')
  mlmovie=WIDGET_LABEL(bPrevNext, VALUE='', YSIZE=50)
  btnMovie=WIDGET_BUTTON(bPrevNext, VALUE=thisPath+'images\play.bmp',/BITMAP,UVALUE='movie',TOOLTIP='Show images as movie')

  ;image
  bDraw = WIDGET_BASE(bLft, XSIZE=drawXY+180, YSIZE=drawXY+10, /ROW)
  bDrawLft = WIDGET_BASE(bDraw, Ysize=drawXY,XSIZE=180,/COLUMN)
  drawLarge = WIDGET_DRAW(bDraw, XSIZE=drawXY, YSIZE=drawXY, KEYBOARD_EVENTS=1, /BUTTON_EVENTS, /MOTION_EVENTS, /WHEEL_EVENTS, GRAPHICS_LEVEL=2, RETAIN=2, SENSITIVE=0)

  ;window level
  bViz = WIDGET_BASE(bDrawLft, /COLUMN)
  bWLtit=WIDGET_BASE(bViz, /ROW)
  lblWL = WIDGET_LABEL(bWLtit, VALUE='Window level', /ALIGN_LEFT, FONT="Arial*ITALIC*16", XSIZE=100)
  btnSetWLminmax=WIDGET_BUTTON(bWLtit, VALUE=thisPath+'images\minmax.bmp', /BITMAP, UVALUE='WLminmax', TOOLTIP='Set Window Level to min/max in image')
  btnSetWLstdev=WIDGET_BUTTON(bWLtit, VALUE=thisPath+'images\meanstdev.bmp', /BITMAP, UVALUE='WLmeanstdev', TOOLTIP='Set Window Level to mean+/-stdev of pixelvalues in selected image')

  btnSetColorTable=WIDGET_BUTTON(bViz, VALUE=thisPath+'images\ctGrayScale.bmp',/BITMAP, UVALUE='colorTable', /FLAT, TOOLTIP='Change colortable')
  bWindowMinMax=WIDGET_BASE(bViz, /ROW)

  txtMinWL = WIDGET_TEXT(bWindowMinMax, VALUE='-200', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
    lblMinWL = WIDGET_LABEL(bWindowMinMax, VALUE='Min', FONT=font1,SCR_XSIZE=22);, /ALIGN_RIGHT)
  lblMl0= WIDGET_LABEL(bWindowMinMax, VALUE=' ', FONT=font1,SCR_XSIZE=3);, /ALIGN_RIGHT)
    lblMaxWL = WIDGET_LABEL(bWindowMinMax, VALUE='Max', FONT=font1,SCR_XSIZE=22);, /ALIGN_RIGHT)
  txtMaxWL = WIDGET_TEXT(bWindowMinMax, VALUE='200', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bWindowCenterWidth=WIDGET_BASE(bViz, /ROW)
  lblCenterW=WIDGET_LABEL(bWindowCenterWidth, VALUE='C, W', FONT=font1,SCR_XSIZE=45);, /ALIGN_RIGHT)
  txtCenterWL=WIDGET_TEXT(bWindowCenterWidth, VALUE='0', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lblWidth=WIDGET_LABEL(bWindowCenterWidth, VALUE=',',FONT=font1, SCR_XSIZE=3);, /ALIGN_RIGHT)
  txtWidthWL=WIDGET_TEXT(bWindowCenterWidth, VALUE='400', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  mlRgtimg0 = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=5);, FONT=font1)

  ;rgt of image - cursor
  bCursor=WIDGET_BASE(bDrawLft, /COLUMN, FRAME=1)
  lblCursor = WIDGET_LABEL(bCursor, VALUE='Cursor', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  bCursorPos=WIDGET_BASE(bCursor, /ROW )
  lblCursorPos0=WIDGET_LABEL(bCursorPos, VALUE='Position (pix): ',FONT=font1)
  lblCursorPos=WIDGET_LABEL(bCursorPos, VALUE='-,-', XSIZE=70, FONT=font1)
  bCursorPosMM=WIDGET_BASE(bCursor, /ROW)
  lblCursorPosMM0=WIDGET_LABEL(bCursorPosMM, VALUE='Position (mm): ',FONT=font1)
  lblCursorPosMM=WIDGET_LABEL(bCursorPosMM, VALUE='-,-', XSIZE=70, FONT=font1)
  bCursorValue= WIDGET_BASE(bCursor, /ROW)
  lblCursorValue0=WIDGET_LABEL(bCursorValue, VALUE='Value: ',FONT=font1)
  lblCursorValue=WIDGET_LABEL(bCursorValue, VALUE='-', XSIZE=50, FONT=font1)
  mlRgtimg = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=15, FONT=font1)

  ;rgt of image - center angle
  bCenterAngle = WIDGET_BASE(bDrawLft, /COLUMN)
  bTitCA=WIDGET_BASE(bCenterAngle, /ROW)
  titleCenterAngle = WIDGET_LABEL(bTitCA, VALUE='Center / rotation',/ALIGN_LEFT, FONT="Arial*ITALIC*16")
  btnGetDelta=WIDGET_BUTTON(bTitCA, VALUE=thisPath+'images\search.bmp',/BITMAP,FONT=font1, UVALUE='getCenter',TOOLTIP='Search for center of mass in image based on threshold value')
  btnSetDelta=WIDGET_BUTTON(bTitCA, VALUE=thisPath+'images\arrow.bmp',/BITMAP,FONT=font1, UVALUE='setCenter', TOOLTIP='Sets center to the position of the last mouseclick in image')

  bDeltaX=WIDGET_BASE(bCenterAngle,/ROW)
  lblDeltaX=WIDGET_LABEL(bDeltaX, VALUE='dx', XSIZE=20, FONT=font1)
  txtDeltaX=WIDGET_TEXT(bDeltaX, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  minusDeltaX=WIDGET_BUTTON(bDeltaX, VALUE='-', UVALUE='minusDx', FONT=font1, SCR_YSIZE=20)
  plusDeltaX=WIDGET_BUTTON(bDeltaX, VALUE='+', UVALUE='plusDx', FONT=font1, SCR_YSIZE=20)
  bDeltaY=WIDGET_BASE(bCenterAngle,/ROW)
  lblDeltaY=WIDGET_LABEL(bDeltaY, VALUE='dy', XSIZE=20,FONT=font1)
  txtDeltaY=WIDGET_TEXT(bDeltaY, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  minusDeltaY=WIDGET_BUTTON(bDeltaY, VALUE='-', UVALUE='minusDy', FONT=font1, SCR_YSIZE=20)
  plusDeltaY=WIDGET_BUTTON(bDeltaY, VALUE='+', UVALUE='plusDy', FONT=font1, SCR_YSIZE=20)
  bDeltaA=WIDGET_BASE(bCenterAngle,/ROW)
  lblDeltaA=WIDGET_LABEL(bDeltaA, VALUE='da', XSIZE=20,FONT=font1)
  txtDeltaA=WIDGET_TEXT(bDeltaA, VALUE='0.0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  minusDeltaA=WIDGET_BUTTON(bDeltaA, VALUE='-', UVALUE='minusDa', FONT=font1, SCR_YSIZE=20)
  plusDeltaA=WIDGET_BUTTON(bDeltaA, VALUE='+', UVALUE='plusDa', FONT=font1, SCR_YSIZE=20)
  bUse=WIDGET_BASE(bCenterAngle, /NONEXCLUSIVE)
  useDelta=WIDGET_BUTTON(bUse, VALUE='Use offset', FONT=font1,UVALUE='useDelta')
  WIDGET_CONTROL, useDelta, SET_BUTTON=1
  
  mlRgtimg2 = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=10, FONT=font1)

  bHide=WIDGET_BASE(bDrawLft, /ROW, /NONEXCLUSIVE)
  btnHideAnnot=WIDGET_BUTTON(bHide, VALUE='Hide annotations', UVALUE='hideAnnot',FONT=font1)

  mlRgtimg3 = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=10, FONT=font1)
  
  ;iImage toolbar
  bTitIimage=WIDGET_BASE(bDrawLft, /ROW)
  titleIimage = WIDGET_LABEL(bTitIimage, VALUE='Open in visualizer',/ALIGN_LEFT, FONT="Arial*ITALIC*16")
  toolBarDraw = WIDGET_BASE(bDrawLft, /ROW, /TOOLBAR)
  ;lbliImage=WIDGET_LABEL(toolBarDraw, VALUE='iImage:',FONT=font1)
  btnAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\ax.bmp', /BITMAP, UVALUE='ax', TOOLTIP='Send active image to iImage window')
  btnCor = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\cor.bmp', /BITMAP, UVALUE='cor', TOOLTIP='Send coronal image found from image stack at defined senter to iImage window')
  btnSag = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sag.bmp', /BITMAP, UVALUE='sag', TOOLTIP='Send sagittal image found from image stack at defined senter to iImage window')
  lblML2=WIDGET_LABEL(toolBarDraw, VALUE='', XSIZE=5, FONT=font1)
  btnSumAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sum.bmp', /BITMAP, UVALUE='sumax', TOOLTIP='Sum all or marked (X) images and send to iImage window')
  btn3dVol  = WIDGET_BUTTON(toolBarDraw, VALUE='3D', UVALUE='3d', TOOLTIP='Under construction...')

  bInfoLow=WIDGET_BASE(bLft, /ROW)
  mlinfo=WIDGET_LABEL(bInfoLow, VALUE='', XSIZE=30)
  toolBarInfo = WIDGET_BASE(bInfoLow, /COLUMN, /TOOLBAR)
  mlinfo2=WIDGET_LABEL(bInfoLow, VALUE='', XSIZE=20)
  btnClipBoardInfo=WIDGET_BUTTON(toolBarInfo, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy these parameters for all images to clipboard in tabular format', UVALUE='copyInfo')
  toolDump=WIDGET_BUTTON(toolbarInfo, VALUE=thisPath+'images\dump.bmp', /BITMAP, TOOLTIP='DICOM dump of active file', UVALUE='dump')
  toolEditHeader=WIDGET_BUTTON(toolbarInfo, VALUE=thisPath+'images\edit.bmp', /BITMAP, TOOLTIP='Edit parameters from DICOM header of active file', UVALUE='editHeader')
  txtActive1=WIDGET_TEXT(bInfoLow, XSIZE=100, YSIZE=100, VALUE='', SCR_XSIZE=300, SCR_YSIZE=150, FONT=font1)
  txtActive2=WIDGET_TEXT(bInfoLow, XSIZE=100, YSIZE=100, VALUE='', SCR_XSIZE=300, SCR_YSIZE=150, FONT=font1)
  
;################################################################## right side ###################################################
  ;Defaults
  bDefaults= WIDGET_BASE(bRgt, /ROW, /TOOLBAR)
  btnSettings=WIDGET_BUTTON(bDefaults, VALUE=thisPath+'images\gears.bmp',/BITMAP, UVALUE='manageSettings', TOOLTIP='Edit/manage parameter set for default settings')
  ttnn=TAG_NAMES(configS)
  name=ttnn(configS.(0))
  lblSettings=WIDGET_LABEL(bDefaults, VALUE=name, XSIZE=110, FONT=font1)
  mlDefault=WIDGET_LABEL(bDefaults, VALUE='', XSIZE=70)
  bExportSettings=WIDGET_BASE(bDefaults, /ROW, FRAME=1)
  lblExportSettings=WIDGET_LABEL(bExportSettings, VALUE='Export settings:   ', FONT="Arial*ITALIC*16")
  bHeaders=WIDGET_BASE(bExportSettings, /NONEXCLUSIVE)
  btnCopyHeader=WIDGET_BUTTON(bHeaders, VALUE='Include table headers', YSIZE=15, TOOLTIP='Include headers when exporting tables', FONT=font1, UVALUE='copyHeader')
  lblDeciMark=WIDGET_LABEL(bExportSettings, VALUE='Decimal mark:', FONT=font1)
  listDeciMark=WIDGET_DROPLIST(bExportSettings, VALUE=['. (period)',', (comma)'], XSIZE=100, FONT=font1, UVALUE='deciMark')
  
  ml111=WIDGET_LABEL(bRgt, VALUE='', YSIZE=10)
  
  ;QuickTest
  bQuick=WIDGET_BASE(bRgt, /COLUMN)
  bMulti=WIDGET_BASE(bQuick, /ROW, XSIZE=660, FRAME=1)
  lblMulti=WIDGET_LABEL(bMulti, VALUE='QuickTest:   ', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  ;bMulti=WIDGET_BASE(bMultiAct, /ROW)
  bUseMulti=WIDGET_BASE(bMulti, /NONEXCLUSIVE)
  btnUseMulti=WIDGET_BUTTON(bUseMulti, VALUE='Use MultiMark', YSIZE=15, UVALUE='useMulti', TOOLTIP='Mark images for specific numbered tests', FONT=font1)
  lblMlMulti1=WIDGET_LABEL(bMulti, VALUE='', XSIZE=20, FONT=font1)
  lblListMulti=WIDGET_LABEL(bMulti, VALUE='Select template:', FONT=font1)
  listSelMultiTemp=WIDGET_DROPLIST(bMulti, UVALUE='listSelMultiTemp', XSIZE=100, FONT=font1)
  bMultiTool=WIDGET_BASE(bMulti, /ROW, /TOOLBAR)
  btnSaveMultiTemp=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\save.bmp',/BITMAP, UVALUE='saveMultiTemp', TOOLTIP='Save current MultiMark as template', YSIZE=20)
  btnDelMultiTemp=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='delMultiTemp', TOOLTIP='Delete selected template', YSIZE=20)
  lblMlMulti2=WIDGET_LABEL(bMultiTool, VALUE='', XSIZE=40, FONT=font1)
  btnRunMulti=WIDGET_BUTTON(bMultiTool, VALUE='QuickTest', UVALUE='runMulti', TOOLTIP='Calculate results for all numbered tests and the corresponding marked images', YSIZE=20, FONT=font1)
  btnExpMulti=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\copy.bmp',/BITMAP, UVALUE='expMulti', TOOLTIP='Copy results to clipboard (paste into fx Excel)', YSIZE=20)
  
  ml222=WIDGET_LABEL(bRgt, VALUE='', YSIZE=10)
  
  ;Analysis tabs
  bAnalysis=WIDGET_BASE(bRgt, /COLUMN)
  wtabModes=WIDGET_TAB(bAnalysis, XSIZE=660, YSIZE=260, UVALUE='tabModes')
  bCT=WIDGET_BASE(wtabModes, TITLE='CT', /COLUMN, UVALUE='tabCT')
  bX=WIDGET_BASE(wtabModes,TITLE='Xray', /COLUMN, UVALUE='tabXray')
  bNM=WIDGET_BASE(wtabModes, TITLE='NM planar',/COLUMN, UVALUE='tabNM')
  bSPECT=WIDGET_BASE(wtabModes, TITLE='SPECT', /COLUMN, UVALUE='tabSPECT')
  bPET=WIDGET_BASE(wtabModes, TITLE='PET', /COLUMN, UVALUE='tabPET')

  wtabAnalysisCT=WIDGET_TAB(bCT, XSIZE=650, YSIZE=240)
  wtabAnalysisXray=WIDGET_TAB(bX, XSIZE=650, YSIZE=240)
  wtabAnalysisNM=WIDGET_TAB(bNM, XSIZE=650, YSIZE=240)
  wtabAnalysisSPECT=WIDGET_TAB(bSPECT, XSIZE=650, YSIZE=240)
  wtabAnalysisPET=WIDGET_TAB(bPET, XSIZE=650, YSIZE=240)

  ; *************************CT tests*****************************************************
  bHomog=WIDGET_BASE(wtabAnalysisCT, Title='1. Homogeneity', /COLUMN)
  bNoise=WIDGET_BASE(wtabAnalysisCT, Title='2. Noise', /COLUMN)
  bSliceThick=WIDGET_BASE(wtabAnalysisCT, Title='3. Slice thickness', /ROW)
  bMTF=WIDGET_BASE(wtabAnalysisCT, TITLE='4. MTF',/Column)
  bNPS=WIDGET_BASE(wtabAnalysisCT, TITLE='NPS',/Column)
  bLinearity=WIDGET_BASE(wtabAnalysisCT, Title='CT Number', /ROW)
  bDim=WIDGET_BASE(wtabAnalysisCT, TITLE='Dim', /COLUMN)
  bROI=WIDGET_BASE(wtabAnalysisCT, TITLE='ROI',/Column)
  bFwhm=WIDGET_BASE(wtabAnalysisCT, Title='FWHM', /COLUMN)

  ;--------------- Linear dimensions DIM
  lblDimInfoml0=WIDGET_LABEL(bDim, VALUE='', SCR_YSIZE=20)
  lblDimInfo=WIDGET_LABEL(bDim, VALUE='Calculate distance between rods in CatPhan.',FONT=font1)
  lblDimInfo1=WIDGET_LABEL(bDim, VALUE='Searches center of rod 25 mm (+/-) 10 mm from defined center of image.',FONT=font1)
  lblDimInfoml1=WIDGET_LABEL(bDim, VALUE='', FONT=font1)
  bDimBtns=WIDGET_BASE(bDim, /ROW)
  btnDim=WIDGET_BUTTON(bDimBtns, VALUE='Calculate linear dimensions', UVALUE='dim',FONT=font1)

  ;---------------Homogeneity--------
  lblHomogMl0=WIDGET_LABEL(bHomog, VALUE='', SCR_YSIZE=20)
  bHomogSize=WIDGET_BASE(bHomog, /ROW)
  lblHomogROIsz = WIDGET_LABEL(bHomogSize, VALUE='ROI radius (mm)',FONT=font1)
  txtHomogROIsz = WIDGET_TEXT(bHomogSize, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  mlH1=WIDGET_LABEL(bHomogSize, VALUE='', XSIZE=20, FONT=font1)
  lblHomogROIdist = WIDGET_LABEL(bHomogSize, VALUE='Radius to ROIs (mm)',FONT=font1)
  txtHomogROIdist = WIDGET_TEXT(bHomogSize, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bHomogBtns=WIDGET_BASE(bHomog, /ROW)
  btnHomog=WIDGET_BUTTON(bHomogBtns, VALUE='Calculated homogeneity', UVALUE='homog',FONT=font1)

  ;---------------Noise--------
  lblNoiseMl0=WIDGET_LABEL(bNoise, VALUE='', SCR_YSIZE=20)
  bNoiseROI=WIDGET_BASE(bNoise, /ROW)
  lblNoiseROIsz = WIDGET_LABEL(bNoiseROI, VALUE='ROI radius (mm)',FONT=font1)
  txtNoiseROIsz = WIDGET_TEXT(bNoiseROI, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bNoiseBtns=WIDGET_BASE(bNoise, /ROW)
  btnNoise=WIDGET_BUTTON(bNoiseBtns, VALUE='Calculated noise', UVALUE='noise',FONT=font1)

  ;----------------MTF------------------
  bMTFsettings=WIDGET_BASE(bMTF, /ROW)

  bMTFlft=WIDGET_BASE(bMTFsettings,/COLUMN)
  cw_typeMTF=CW_BGROUP(bMTFlft, ['Bead','Wire','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME,FONT=font1)

  bMTFrgt=WIDGET_BASE(bMTFsettings,/COLUMN)
  bCutLSF=WIDGET_BASE(bMTFrgt, /NONEXCLUSIVE, /ROW)
  btnCutLSF=WIDGET_BUTTON(bCutLSF, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFW=WIDGET_BASE(bMTFrgt, /ROW)
  lblCutLSFW=WIDGET_LABEL( bCutLSFW, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1)
  txtCutLSFW=WIDGET_TEXT( bCutLSFW, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bCutLSFW2=WIDGET_BASE(bMTFrgt,/ROW)
  lblCutLSFW2=WIDGET_LABEL( bCutLSFW2, VALUE='Fade out cut within (#FWHM)',FONT=font1)
  txtCutLSFW2=WIDGET_TEXT( bCutLSFW2, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bfreqMTF=WIDGET_BASE(bMTFrgt, /ROW)
  lblfreqMTF=WIDGET_LABEL(bfreqMTF, VALUE='Sampling frequency gaussian MTF curve (mm-1)',FONT=font1)
  txtfreqMTF=WIDGET_TEXT(bfreqMTF, VALUE='0.010', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bMTFroiSz=WIDGET_BASE(bMTFrgt, /ROW)
  lblMTFroiSz=WIDGET_LABEL(bMTFroiSz, VALUE='ROI size from center (mm)',FONT=font1)
  txtMTFroiSz=WIDGET_TEXT(bMTFroiSz, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)

  cw_plotMTF=CW_BGROUP(bMTFsettings, ['Centered xy profiles', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTF',FONT=font1)

  bOffset=WIDGET_BASE(bMTFlft, /COLUMN)
  bTitOffset=WIDGET_BASE(bOffset, /ROW)
  lblOffset=WIDGET_LABEL(bTitOffset, VALUE='Extra offset', FONT="Arial*ITALIC*16")
  getOffset=WIDGET_BUTTON(bTitOffset, VALUE=thisPath+'images\arrow.bmp',/BITMAP,UVALUE='setOffset', TOOLTIP='Sets offset corresponding to the position of the last mouseclick in image')
  bDeltaO=WIDGET_BASE(bOffset,/ROW)
  lblDeltaO_=WIDGET_LABEL(bDeltaO, VALUE='dx,dy: ', XSIZE=40, FONT=font1)
  lblDeltaO=WIDGET_LABEL(bDeltaO, VALUE=STRING(offxy(0), FORMAT='(i0)')+','+STRING(offxy(1), FORMAT='(i0)'), XSIZE=70, FONT=font1)
  ;txtDeltaXo=WIDGET_TEXT(bDeltaXo, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  ;minusDeltaXo=WIDGET_BUTTON(bDeltaXo, VALUE='-', UVALUE='minusDxo', FONT=font1, SCR_YSIZE=20)
  ;plusDeltaXo=WIDGET_BUTTON(bDeltaXo, VALUE='+', UVALUE='plusDxo', FONT=font1, SCR_YSIZE=20)
  ;bDeltaYo=WIDGET_BASE(bOffset,/ROW)
  ;lblDeltaYo=WIDGET_LABEL(bDeltaYo, VALUE='dy', XSIZE=20,FONT=font1)
  ;txtDeltaYo=WIDGET_TEXT(bDeltaYo, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  ;minusDeltaYo=WIDGET_BUTTON(bDeltaYo, VALUE='-', UVALUE='minusDyo', FONT=font1, SCR_YSIZE=20)
  ;plusDeltaYo=WIDGET_BUTTON(bDeltaYo, VALUE='+', UVALUE='plusDyo', FONT=font1, SCR_YSIZE=20)

  bMTFbtns=WIDGET_BASE(bMTFrgt, /ROW)
  btnMTF=WIDGET_BUTTON(bMTFbtns, VALUE='Calculate MTF', UVALUE='MTF',FONT=font1)

  ;------------ NPS ---------------------
  bNPSroiSz=WIDGET_BASE(bNPS, /ROW)
  lblNPSroiSz=WIDGET_LABEL(bNPSroiSz, VALUE='ROI size (pix)',FONT=font1)
  txtNPSroiSz=WIDGET_TEXT(bNPSroiSz, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSroiDist=WIDGET_BASE(bNPS, /ROW)
  lblNPSroiDist=WIDGET_LABEL(bNPSroiDist, VALUE='Radius to center of ROIs (mm)',FONT=font1)
  txtNPSroiDist=WIDGET_TEXT(bNPSroiDist, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSsubNN=WIDGET_BASE(bNPS, /ROW)
  lblNPSsubNN=WIDGET_LABEL(bNPSsubNN, VALUE='Number of ROIs',FONT=font1)
  txtNPSsubNN=WIDGET_TEXT(bNPSsubNN, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bSmoothNPS=WIDGET_BASE(bNPS, /ROW)
  lblSmoothNPS=WIDGET_LABEL(bSmoothNPS, VALUE='Smooth NPS curve by width (mm-1)',FONT=font1)
  txtSmoothNPS=WIDGET_TEXT(bSmoothNPS, VALUE='0.050', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bfreqNPS=WIDGET_BASE(bNPS, /ROW)
  lblfreqNPS=WIDGET_LABEL(bfreqNPS, VALUE='Sampling frequency NPS curve (mm-1)',FONT=font1)
  txtfreqNPS=WIDGET_TEXT(bfreqNPS, VALUE='0.010', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bNPSbtns=WIDGET_BASE(bNPS, /ROW)
  btnNPS=WIDGET_BUTTON(bNPSbtns, VALUE='Calculate NPS', UVALUE='NPS',FONT=font1)
  bNPSavg=WIDGET_BASE(bNPSbtns, /NONEXCLUSIVE, /ROW)
  btnNPSavg=WIDGET_BUTTON(bNPSavg, VALUE='Plot average', UVALUE='NPSavg',FONT=font1)

  ;----------------User defined ROI------------
  lblroiMl0=WIDGET_LABEL(bROI, VALUE='', SCR_YSIZE=20)
  typeROI=CW_BGROUP(bROI, ['Define new','Load saved ROI (.sav)'], /EXCLUSIVE, LABEL_TOP='Define ROI and get min/max/avg/stdev', /FRAME, UVALUE='typeROI',FONT=font1)
  btnDefROI =WIDGET_BUTTON(bROI, VALUE = 'Define ROI', UVALUE='ROI',FONT=font1)

  ;-------------CT numbers linearity-------------
  bLinSettings=WIDGET_BASE(bLinearity, /ROW)
  bLinLft=WIDGET_BASE(bLinSettings, /COLUMN)
  emLin=WIDGET_LABEL(bLinLft, VALUE='', YSIZE=20, FONT=font1)
  labLinearity=WIDGET_LABEL(bLinLft, VALUE='Calculate CT Numbers within ROIs',FONT=font1)
  emLin2=WIDGET_LABEL(bLinLft, VALUE='', YSIZE=20, FONT=font1)
  bLinSearchROI=WIDGET_BASE(bLinLft, /ROW)
  lblLargeRad = WIDGET_LABEL(bLinSearchROI, VALUE='Radius of search ROIs (mm)',FONT=font1)
  txtLinROIradS = WIDGET_TEXT(bLinSearchROI, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bLinSzROI=WIDGET_BASE(bLinLft, /ROW)
  lblSampleRad = WIDGET_LABEL(bLinSzROI, VALUE='ROI radius (mm)',FONT=font1)
  txtLinROIrad = WIDGET_TEXT(bLinSzROI, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bLinAvoidSearch=WIDGET_BASE(bLinlft, /NONEXCLUSIVE, /ROW)
  btnLinAvoidSearch=WIDGET_BUTTON(bLinAvoidSearch, VALUE='Avoid search and use senter of search ROI', UVALUE='linAvoidSearch',FONT=font1)
  
  bLinButtons=WIDGET_BASE(bLinLft, /ROW)
  ;btnLinRois=WIDGET_BUTTON(bLinButtons, VALUE='Show/update search ROIs', UVALUE='drawLinRois',FONT=font1)
  btnLinearity=WIDGET_BUTTON(bLinButtons, VALUE='Get CT numbers', UVALUE='Linearity',FONT=font1)
  
  bLinRgt=WIDGET_BASE(bLinSettings, /COLUMN)
  bLinTB=WIDGET_BASE(bLinRgt, /ROW, /TOOLBAR)
  btnLinImport=WIDGET_BUTTON(bLinTB, VALUE='images\importd.bmp', /BITMAP,TOOLTIP='Import table from clipboard', UVALUE='impLinTab')
  btnLinCopy=WIDGET_BUTTON(bLinTB, VALUE='images\copy.bmp', /BITMAP,TOOLTIP='Copy table to clipboard', UVALUE='copyLinTab')
  btnLinAdd=WIDGET_BUTTON(bLinTB, VALUE='images\plus.bmp', /BITMAP, TOOLTIP='Add row to table', UVALUE='addRowLinTab')
  btnLinDel=WIDGET_BUTTON(bLinTB, VALUE='images\delete.bmp', /BITMAP, TOOLTIP='Delete selected row(s) from table', UVALUE='delRowLinTab')
  btnLinCenter=WIDGET_BUTTON(bLinTB, VALUE='images\center.bmp', /BITMAP, TOOLTIP='Set position of last mouse-click as senter for this material', UVALUE='centerLinTab')
  tblLin=WIDGET_TABLE(bLinRgt, XSIZE=4, YSIZE=5, COLUMN_LABELS=['Material','ROI xpos (mm)', 'ROI ypos (mm)','Density'],COLUMN_WIDTHS=[80,90,90,60], /NO_ROW_HEADERS, SCR_XSIZE=winX/4, SCR_YSIZE=170, /ALL_EVENTS, /EDITABLE,FONT=font1)
  
  ;---------------Slice thickness--------
  lblSlMl0=WIDGET_LABEL(bSliceThick, VALUE='', SCR_YSIZE=20)
  bSliceThickLft=WIDGET_BASE(bSliceThick, /COLUMN)
  cw_ramptype=CW_BGROUP(bSliceThickLft, ['Wire ramp (CatPhan)','Beaded ramp (CatPhan)','Vertical beaded ramps (GE)'], /EXCLUSIVE, LABEL_TOP='Ramp type...', /FRAME,FONT=font1)
  cw_rampDens=CW_BGROUP(bSliceThickLft, ['higher than background', 'lower than background'], /EXCLUSIVE, LABEL_TOP='Ramp density is...', /FRAME, FONT=font1)
  bSliceThickRgt=WIDGET_BASE(bSliceThick, /COLUMN)
  bRampDist=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampDist = WIDGET_LABEL(bRampDist, VALUE='Center to ramp distance (mm)',FONT=font1)
  txtRampDist = WIDGET_TEXT(bRampDist, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lbl2RampDist = WIDGET_LABEL(bSliceThickRgt, VALUE='    (ignored for beaded ramp in CatPhan, CTP591 geometry used)',FONT=font1)
  mlLen=WIDGET_LABEL(bSliceThickRgt, VALUE='', YSIZE=10)
  bRampLen=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampLen = WIDGET_LABEL(bRampLen, VALUE='Profile length (mm)',FONT=font1)
  txtRampLen = WIDGET_TEXT(bRampLen, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bRampBack=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblBackG = WIDGET_LABEL(bRampBack, VALUE='Background from outer (mm)',FONT=font1)
  txtRampBackG = WIDGET_TEXT(bRampBack, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bRampSearch=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampSearch = WIDGET_LABEL(bRampSearch, VALUE='Search for max (or min) in profile',FONT=font1)
  txtRampSearch = WIDGET_TEXT(bRampSearch, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lblRampSearch2 = WIDGET_LABEL(bRampSearch, VALUE='# pix from center of ramp',FONT=font1)
  bRampAverage=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampAverage = WIDGET_LABEL(bRampAverage, VALUE='Average over ',FONT=font1)
  txtRampAverage = WIDGET_TEXT(bRampAverage, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lblRampAverage2 = WIDGET_LABEL(bRampAverage, VALUE='# neighbour profiles',FONT=font1)
  bSliceThickBtns=WIDGET_BASE(bSliceThickRgt, /ROW)
  ;btnSliceThickRamps=WIDGET_BUTTON(bSliceThickBtns, VALUE='Show/update ramps', UVALUE='drawRamps',FONT=font1)
  btnSliceThick=WIDGET_BUTTON(bSliceThickBtns, VALUE='Get Slice Thickness', UVALUE='SliceThick',FONT=font1)

  ;---------------FWHM---------------- move together with MTF later
  lblFWhmMl0=WIDGET_LABEL(bFWhm, VALUE='', SCR_YSIZE=20)
  lblFwhm=WIDGET_LABEL(bFWhm, VALUE='Code based on PSF.pro & CALCULATE_LSF_LIST.pro developed at DNR (Oslo, Norway) ',FONT=font1)
  lblFwhm2=WIDGET_LABEL(bFWhm, VALUE=' by Arne Skretting, Wibeke Nordh'+string(248B)+'y, Alise Larsen and Kristine Eldevik',FONT=font1)
  lblFwhmML=WIDGET_LABEL(bFWhm, VALUE='', YSIZE=20, FONT=font1)
  lblFwhm3=WIDGET_LABEL(bFWhm, VALUE='FWHM calculated from average of 10 pixelrows.',FONT=font1)
  lblFwhmML=WIDGET_LABEL(bFWhm, VALUE='', YSIZE=20, FONT=font1)
  btnFwhm=WIDGET_BUTTON(bFwhm, VALUE='Calculate FWHM' , UVALUE='fwhm',FONT=font1)


  ;**********************X ray tests *******************************************************

  bSTP=WIDGET_BASE(wtabAnalysisXray, Title='1. STP', /COLUMN)
  bHomogX=WIDGET_BASE(wtabAnalysisXray, Title='2. Homogeneity', /COLUMN)
  bNoiseX=WIDGET_BASE(wtabAnalysisXray, Title='3. Noise', /COLUMN)
  bEIX=WIDGET_BASE(wtabAnalysisXray, Title='4. EI', /COLUMN)
  bMTFsettingsX=WIDGET_BASE(wtabAnalysisXray, TITLE='5. MTF',/COLUMN)
  bNPSX=WIDGET_BASE(wtabAnalysisXray, TITLE='NPS',/Column)
  bVariX=WIDGET_BASE(wtabAnalysisXray, TITLE='Variance image',/Column)
  bROIX=WIDGET_BASE(wtabAnalysisXray, TITLE='ROI',/COLUMN)

  ;---------------STP--------
    lblstpMl0=WIDGET_LABEL(bSTP, VALUE='', SCR_YSIZE=20)
  bStpSettings=WIDGET_BASE(bSTP, /ROW)
  lblStpROIsz = WIDGET_LABEL(bStpSettings, VALUE='ROI radius (mm)',FONT=font1)
  txtStpROIsz = WIDGET_TEXT(bStpSettings, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  Qvals=[21759.,30174.,32362.,31077.,1]
  lblStpBeamQuality = WIDGET_LABEL(bStpSettings, VALUE='     Beam quality',FONT=font1)
  ddlRQA = WIDGET_COMBOBOX(bStpSettings, VALUE=['RQA 3','RQA 5','RQA 7','RQA 9','other'], UVALUE='ddlRQA', /LIST_EVENTS,FONT=font1)
  WIDGET_CONTROL, ddlRQA, SET_COMBOBOX_SELECT=1
  txtRQA = WIDGET_TEXT(bStpSettings, VALUE='', UVALUE='txtRQA', XSIZE=7, /EDITABLE, FONT=font1)
  lblQ = WIDGET_LABEL(bStpSettings, VALUE='1/mm^2uGy',FONT=font1)
  bStpBtns=WIDGET_BASE(bSTP, /ROW)
  btnStp=WIDGET_BUTTON(bStpBtns, VALUE='Find pixel values', UVALUE='STPpix',FONT=font1)
  ;bStpProcess = WIDGET_BASE(bSTP, /ROW)
  btnImportDose=WIDGET_BUTTON(bStpBtns, VALUE='Import dose values', UVALUE='impDose',FONT=font1)
  btnCalcSTP=WIDGET_BUTTON(bStpBtns, VALUE='Calculate STP', UVALUE='calcSTP',FONT=font1)
  lblWarnMlSTP=WIDGET_LABEL(bSTP, VALUE='', FONT=font1)
  lblWarnStp0=WIDGET_LABEL(bSTP, VALUE='Warning: Consider test as "under construction".',FONT=font1)
  lblWarnStp=WIDGET_LABEL(bSTP, VALUE='     Only linear fit implemented for STP, beam quality and Qvalue might not be used correctly',FONT=font1)

  ;---------------Homogeneity--------
    lblHomogXMl0=WIDGET_LABEL(bHomogX, VALUE='', SCR_YSIZE=20)
  bHomogSizeX=WIDGET_BASE(bHomogX, /ROW)
  lblHomogROIszX = WIDGET_LABEL(bHomogSizeX, VALUE='ROI radius (mm)',FONT=font1)
  txtHomogROIszX = WIDGET_TEXT(bHomogSizeX, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bHomogBtnsX=WIDGET_BASE(bHomogX, /ROW)
  btnHomogX=WIDGET_BUTTON(bHomogBtnsX, VALUE='Calculated homogeneity', UVALUE='homog',FONT=font1)

  ;---------------Noise--------
    lblNoiseXMl0=WIDGET_LABEL(bNoiseX, VALUE='', SCR_YSIZE=20)
  bNoiseROIX=WIDGET_BASE(bNoiseX, /ROW)
  lblNoiseROIszX = WIDGET_LABEL(bNoiseROIX, VALUE='ROI 90 % of image area ',FONT=font1)
  bNoiseBtnsX=WIDGET_BASE(bNoiseX, /ROW)
  btnNoiseX=WIDGET_BUTTON(bNoiseBtnsX, VALUE='Calculated noise', UVALUE='noise',FONT=font1)
  
  ;---------------EI--------
  lblEIXMl0=WIDGET_LABEL(bEIX, VALUE='', SCR_YSIZE=20)
  bbEIX=WIDGET_BASE(bEIX, /ROW)
  lblEIX = WIDGET_LABEL(bbEIX, VALUE='Extract Exposure Index from DICOM Header (tag <0018,1411>)',FONT=font1)
  bEIBtnsX=WIDGET_BASE(bEIX, /ROW)
  btnEIX=WIDGET_BUTTON(bEIBtnsX, VALUE='Get EI values', UVALUE='EI',FONT=font1)

  ;----------------MTF------------------
  bMTFX=WIDGET_BASE(bMTFsettingsX, /ROW)
  bMTFX1=WIDGET_BASE(bMTFX, /COLUMN)
  cw_formLSFX=CW_BGROUP(bMTFX1, ['Exponential','Gaussian','None'], /EXCLUSIVE, LABEL_TOP='LSF fit to...', /FRAME, FONT=font1)
  
  bOffsetX=WIDGET_BASE(bMTFX1, /COLUMN)
  bTitOffsetX=WIDGET_BASE(bOffsetX, /ROW)
  lblOffsetX=WIDGET_LABEL(bTitOffsetX, VALUE='Extra offset', FONT="Arial*ITALIC*16")
  getOffsetX=WIDGET_BUTTON(bTitOffsetX, VALUE=thisPath+'images\arrow.bmp',/BITMAP,UVALUE='setOffset', TOOLTIP='Sets offset corresponding to the position of the last mouseclick in image')
  bDeltaOX=WIDGET_BASE(bOffsetX,/ROW)
  lblDeltaOX_=WIDGET_LABEL(bDeltaOX, VALUE='dx,dy: ', XSIZE=40, FONT=font1)
  lblDeltaOX=WIDGET_LABEL(bDeltaOX, VALUE='', XSIZE=70, FONT=font1)
  
  bMTFX2=WIDGET_BASE(bMTFX, /COLUMN)
  bLSFfilterX=WIDGET_BASE(bMTFX2, /COLUMN)
  bCutLSFX=WIDGET_BASE(bLSFfilterX, /NONEXCLUSIVE, /ROW)
  btnCutLSFX=WIDGET_BUTTON(bCutLSFX, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFWX=WIDGET_BASE(bLSFfilterX, /ROW)
  lblCutLSFWX=WIDGET_LABEL( bCutLSFWX, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1)
  txtCutLSFWX=WIDGET_TEXT( bCutLSFWX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  
  bMTFroiSzX=WIDGET_BASE(bMTFX2, /ROW)
  lblMTFroiSzX=WIDGET_LABEL(bMTFroiSzX, VALUE='ROI width x height (mm)',FONT=font1)
  txtMTFroiSzX=WIDGET_TEXT(bMTFroiSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  lblMTFx=WIDGET_LABEL(bMTFroiSzX, VALUE=' x ',FONT=font1)
  txtMTFroiSzY=WIDGET_TEXT(bMTFroiSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  
  btnMTFX=WIDGET_BUTTON(bMTFX2, VALUE='Calculate MTF', UVALUE='MTFX',FONT=font1)
  
  cw_plotMTFX=CW_BGROUP(bMTFX, ['Edge position', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTFX',FONT=font1)

  ;----------------NPS------------------
  bNPSroiSzX=WIDGET_BASE(bNPSX, /ROW)
  lblNPSroiSzX=WIDGET_LABEL(bNPSroiSzX, VALUE='ROI size (pix)',FONT=font1)
  txtNPSroiSzX=WIDGET_TEXT(bNPSroiSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSsubSzX=WIDGET_BASE(bNPSX, /ROW)
  lblNPSsubSzX=WIDGET_LABEL(bNPSsubSzX, VALUE='Subimage size (pix)',FONT=font1)
  txtNPSsubSzX=WIDGET_TEXT(bNPSsubSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lblNPSsubSz2X=WIDGET_LABEL(bNPSsubSzX, VALUE=' x ROI size = ',FONT=font1)
  lblNPSsubSzMMX=WIDGET_LABEL(bNPSsubSzX, VALUE='', XSIZE=20, SCR_XSIZE=20, FONT=font1)
  lblNPSsubSz3X=WIDGET_LABEL(bNPSsubSzX, VALUE=' mm',FONT=font1)
  bNPStotPixX=WIDGET_BASE(bNPSX, /ROW)
  lblNPStotPix0X=WIDGET_LABEL(bNPStotPixX, VALUE='# independent pixels/image (preferrably 4 mill in total): ',FONT=font1)
  lblNPStotPixX=wIDGET_LABEL(bNPStotPixX, VALUE='', /DYNAMIC_RESIZE,FONT=font1)

  bNPSbtnsX=WIDGET_BASE(bNPSX, /ROW)
  btnNPSX=WIDGET_BUTTON(bNPSbtnsX, VALUE='Calculate NPS', UVALUE='NPS',FONT=font1)

  ;----------------Variance image------------------
  mlVar0=WIDGET_LABEL(bVariX,VALUE='', YSIZE=15)
  
  lblVarX=WIDGET_LABEL(bVariX, VALUE='The variance image can reveal artifacts in the image. Adjust ROI size to find artifacts of different sizes.',FONT=font1)
  mlVar1=WIDGET_LABEL(bVariX,VALUE='', YSIZE=15)
  bVariROI=WIDGET_BASE(bVariX, /ROW)
  lblVarImageROIsz=WIDGET_LABEL(bVariROI, VALUE='ROI size (mm)', FONT=font1)
  txtVarImageROIsz=WIDGET_TEXT(bVariROI, VALUE='2.0', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  mlVar2=WIDGET_LABEL(bVariX,VALUE='', YSIZE=15)
  
  bVarianceBtnsX=WIDGET_BASE(bVariX,/ROW)
  btnVarImageX=WIDGET_BUTTON(bVarianceBtnsX, VALUE='Calculate variance image of active image', UVALUE='varImage',FONT=font1)
  mlVar3=WIDGET_LABEL(bVarianceBtnsX,VALUE='', XSIZE=10)
  lblProgressVarX=WIDGET_LABEL(bVarianceBtnsX, VALUE='', /DYNAMIC_RESIZE)

  ;----------------User defined ROI------------
    lblroixMl0=WIDGET_LABEL(bROIX, VALUE='', SCR_YSIZE=20)
  typeROIX=CW_BGROUP(bROIX, ['Define new','Load saved ROI (.sav)'], /EXCLUSIVE, LABEL_TOP='Define ROI and get min/max/avg/stdev', /FRAME, UVALUE='typeROIX',FONT=font1)
  btnDefROIX =WIDGET_BUTTON(bROIX, VALUE = 'Define ROI', UVALUE='ROI',FONT=font1)

  ;***********************NM planar tests**********************************************************
  bUniformity=WIDGET_BASE(wtabAnalysisNM, Title='1. Uniformity', /COLUMN)
  bSNI=WIDGET_BASE(wtabAnalysisNM, Title='2. SNI', /COLUMN)
  bEnergySpec=WIDGET_BASE(wtabAnalysisNM, TITLE='Energy spectrum', /COLUMN)
  ;bHomogNM=WIDGET_BASE(wtabAnalysisNM, Title='Uniformity', /COLUMN)
  bScanSpeed=WIDGET_BASE(wtabAnalysisNM, Title='Scan Speed', /COLUMN)
  bMTFNM=WIDGET_BASE(wtabAnalysisNM, TITLE='MTF',/Column)
  
  ;----------------Uniformity------------------
  lblunifMl0=WIDGET_LABEL(bUniformity, VALUE='', SCR_YSIZE=20)
  lblunif2=WIDGET_LABEL(bUniformity, VALUE='Based on NEMA NU-1 2007. UFOV assumed to be 95% of signal area.',FONT=font1)
  lblunifMl2=WIDGET_LABEL(bUniformity, VALUE='', SCR_YSIZE=20)
  btnUnif=WIDGET_BUTTON(bUniformity, VALUE='Calculate Uniformity', UVALUE='uniformityNM',FONT=font1)
  
  ;----------------SNI------------------
  lblsniMl0=WIDGET_LABEL(bSNI, VALUE='', SCR_YSIZE=20)
  lblSNI=WIDGET_LABEL(bSNI, VALUE='SNI = Structured Noise Index',FONT=font1)
  lblSNI2=WIDGET_LABEL(bSNI, VALUE='Based on: Nelson et al. J Nucl Med 2014; 55:169-174',FONT=font1)
  lblsniMl2=WIDGET_LABEL(bSNI, VALUE='', SCR_YSIZE=20)
  btnSNI=WIDGET_BUTTON(bSNI, VALUE='Calculate SNI', UVALUE='SNI',FONT=font1)

  ;------------energy spectrum--------------------
  lblesMl0=WIDGET_LABEL(bEnergySpec, VALUE='', SCR_YSIZE=20)
  bEnergySpecBtns=WIDGET_BASE(bEnergySpec, /ROW)
  btnLoadSpec=WIDGET_BUTTON(bEnergySpecBtns, VALUE='Load spectrum', UVALUE='loadSpectrum',FONT=font1)

;  ;---------------Homogeneity--------
;    lblHomognmMl0=WIDGET_LABEL(bHomogNM, VALUE='', SCR_YSIZE=20)
;  bHomogNMlft=WIDGET_BASE(bHomogNM,/COLUMN)
;  bHomogSizeNM=WIDGET_BASE(bHomogNMlft, /ROW)
;  lblHomogROIszNM = WIDGET_LABEL(bHomogSizeNM, VALUE='ROI radius (mm)',FONT=font1)
;  txtHomogROIszNM = WIDGET_TEXT(bHomogSizeNM, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
;  bHomogDistNM=WIDGET_BASE(bHomogNMlft, /ROW)
;  lblHomogROIdistNM = WIDGET_LABEL(bHomogDistNM, VALUE='ROI distance x, y (mm)',FONT=font1)
;  txtHomogROIdistXNM = WIDGET_TEXT(bHomogDistNM, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
;  txtHomogROIdistYNM = WIDGET_TEXT(bHomogDistNM, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
;  cw_homogNM=CW_BGROUP(bHomogNMlft, ['Planar (WB)', 'SPECT'], /EXCLUSIVE, LABEL_TOP='Image type...', /FRAME, SET_VALUE=0, UVALUE='cw_homogNM',FONT=font1)
;  bHomogBtnsNM=WIDGET_BASE(bHomogNMlft, /ROW)
;  ;btnHomogROINM=WIDGET_BUTTON(bHomogBtnsNM, VALUE='Show/update ROI', UVALUE='drawROIhomog',FONT=font1)
;  btnHomogNM=WIDGET_BUTTON(bHomogBtnsNM, VALUE='Calculate uniformity', UVALUE='homog',FONT=font1)

  ;-----------Scan speed------------
    lblssMl0=WIDGET_LABEL(bScanSpeed, VALUE='', SCR_YSIZE=20)
  bAvgSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lblAvgSpeedNM=WIDGET_LABEL(bAvgSpeedNM, VALUE='Average over ROI with width (pix)' ,FONT=font1)
  txtNAvgSpeedNM=WIDGET_TEXT(bAvgSpeedNM, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bSpeedROIheight=WIDGET_BASE(bScanSpeed, /ROW)
  lblSpeedROIheight=WIDGET_LABEL(bSpeedROIheight, VALUE='ROI heigth (cm)' ,FONT=font1)
  txtSpeedROIheight=WIDGET_TEXT(bSpeedROIheight, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bMedianSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lblSpeedMedian=WIDGET_LABEL(bMedianSpeedNM, VALUE='Median filter width (pix)',FONT=font1)
  txtScanSpeedMedian=WIDGET_TEXT(bMedianSpeedNM, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  btnPlotScanSpeed = WIDGET_BUTTON(bScanSpeed, VALUE='Plot y-profile and median filtered profile', UVALUE='plotScanSpeed',FONT=font1)

  ;----------------MTF------------------
  bMTFsettingsNM=WIDGET_BASE(bMTFNM, /ROW)
  bMTFlftNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)
  cw_typeMTFNM=CW_BGROUP(bMTFlftNM, ['Point','Line','Edge','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME,FONT=font1)
  
  bMTFrgtNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)
  
  bMTFroiSzNM=WIDGET_BASE(bMTFrgtNM, /ROW)
  lblMTFroiSzXNM=WIDGET_LABEL(bMTFroiSzNM, VALUE='ROI width x height (mm)',FONT=font1)
  txtMTFroiSzXNM=WIDGET_TEXT(bMTFroiSzNM, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lblMTFXNM=WIDGET_LABEL(bMTFroiSzNM, VALUE='x',FONT=font1)
  txtMTFroiSzYNM=WIDGET_TEXT(bMTFroiSzNM, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  
  bCutLSFNM=WIDGET_BASE(bMTFrgtNM, /NONEXCLUSIVE, /ROW)
  btnCutLSFNM=WIDGET_BUTTON(bCutLSFNM, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFWNM=WIDGET_BASE(bMTFrgtNM, /ROW)
  lblCutLSFWNM=WIDGET_LABEL( bCutLSFWNM, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1)
  txtCutLSFWNM=WIDGET_TEXT( bCutLSFWNM, VALUE='5', /EDITABLE, XSIZE=3, SCR_YSIZE=20, FONT=font1)
  lblmlSett123=WIDGET_LABEL(bMTFrgtNM, VALUE='', YSIZE=20)

  cw_plotMTFNM=CW_BGROUP(bMTFsettingsNM, ['Centered xy profiles', 'Line', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTFNM',FONT=font1)

  bMTFbtnsNM=WIDGET_BASE(bMTFNM, /ROW)
  ;btnMTFroiNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Show/update ROIs', UVALUE='drawMTFroi',FONT=font1)
  btnMTFNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Calculate MTF', UVALUE='MTFNM',FONT=font1)

  ;***********************SPECT tests**********************************************************

  bMTF_SPECT=WIDGET_BASE(wtabAnalysisSPECT, TITLE='MTF',/Column)
  bRadialProfile=WIDGET_BASE(wtabAnalysisSPECT, Title='Radial Profile', /COLUMN)
  bContrastSPECT=WIDGET_BASE(wtabAnalysisSPECT, Title='Contrast', /COLUMN)
 
  ;----------------MTF------------------
  bMTFsettingsSPECT=WIDGET_BASE(bMTF_SPECT, /ROW)
  bMTFlftSPECT=WIDGET_BASE(bMTFsettingsSPECT,/COLUMN)
  cw_typeMTFSPECT=CW_BGROUP(bMTFlftSPECT, ['Point','Line','Edge','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME,FONT=font1)

  bMTFrgtSPECT=WIDGET_BASE(bMTFsettingsSPECT,/COLUMN)

  bMTFroiSzSPECT=WIDGET_BASE(bMTFrgtSPECT, /ROW)
  lblMTFroiSzSPECT=WIDGET_LABEL(bMTFroiSzSPECT, VALUE='ROI size (mm)',FONT=font1)
  txtMTFroiSzSPECT=WIDGET_TEXT(bMTFroiSzSPECT, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)

  bCutLSFSPECT=WIDGET_BASE(bMTFrgtSPECT, /NONEXCLUSIVE, /ROW)
  btnCutLSFSPECT=WIDGET_BUTTON(bCutLSFSPECT, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFWSPECT=WIDGET_BASE(bMTFrgtSPECT, /ROW)
  lblCutLSFWSPECT=WIDGET_LABEL( bCutLSFWSPECT, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1)
  txtCutLSFWSPECT=WIDGET_TEXT( bCutLSFWSPECT, VALUE='5', /EDITABLE, XSIZE=3, SCR_YSIZE=20, FONT=font1)
  lblmlSett123_=WIDGET_LABEL(bMTFrgtSPECT, VALUE='', YSIZE=20)
  bMTF3dSPECT=WIDGET_BASE(bMTFrgtSPECT, /COLUMN, /NONEXCLUSIVE)
  MTF3dSPECT=WIDGET_BUTTON(bMTF3dSPECT, VALUE='Analyse 3d', UVALUE='MTF3dSPECT',FONT=font1)

  cw_plotMTFSPECT=CW_BGROUP(bMTFsettingsSPECT, ['Centered xy profiles', 'Line', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTFSPECT',FONT=font1)

  bMTFbtnsSPECT=WIDGET_BASE(bMTF_SPECT, /ROW)
  ;btnMTFroiSPECT=WIDGET_BUTTON(bMTFbtnsSPECT, VALUE='Show/update ROIs', UVALUE='drawMTFroi',FONT=font1)
  btnMTFSPECT=WIDGET_BUTTON(bMTFbtnsSPECT, VALUE='Calculate MTF', UVALUE='MTFNM',FONT=font1)
  
  ;----------Radial profiles---------------
  lblrpMl0=WIDGET_LABEL(bRadialProfile, VALUE='', SCR_YSIZE=20)
  bRadialProf=WIDGET_BASE(bRadialProfile, /COLUMN)
  bMedianRadialSPECT=WIDGET_BASE(bRadialProf, /ROW)
  lblRadialMedian=WIDGET_LABEL(bMedianRadialSPECT, VALUE='Median filter width (pix)',FONT=font1)
  txtRadialMedian=WIDGET_TEXT(bMedianRadialSPECT, VALUE=STRING(5, FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1);config.radialFiltW
  btnRadialProfSPECT=WIDGET_BUTTON(bRadialProf, VALUE='Calculate radial profile', UVALUE='radialProfile',FONT=font1)

  ;-------------Contrast-------------
  lblcnmMl0=WIDGET_LABEL(bContrastSPECT, VALUE='', SCR_YSIZE=20)
  bConSettingsSPECT=WIDGET_BASE(bContrastSPECT, /ROW)
  lblConR1SPECT = WIDGET_LABEL(bConSettingsSPECT, VALUE='ROI radius (mm)', FONT=font1)
  txtConR1SPECT = WIDGET_TEXT(bConSettingsSPECT, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  lblMlConRSPECT=WIDGET_LABEL(bConSettingsSPECT, VALUE='', XSIZE=20, FONT=font1)
  lblConR2SPECT = WIDGET_LABEL(bConSettingsSPECT, VALUE='Radius to ROIs (mm)', FONT=font1)
  txtConR2SPECT = WIDGET_TEXT(bConSettingsSPECT, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bConButtonsSPECT=WIDGET_BASE(bContrastSPECT, /ROW)
  ;btnConRoisSPECT=WIDGET_BUTTON(bConButtonsSPECT, VALUE='Show/update ROIs', UVALUE='drawConRoisSPECT',FONT=font1)
  btnContrastSPECT=WIDGET_BUTTON(bConButtonsSPECT, VALUE='Calculate contrast', UVALUE='contrastSPECT',FONT=font1)

  ;***********************PET tests**********************************************************

  ;analyseStringsPET=['NONE','CROSSCALIB','HOMOG','CONTRAST','MTF']
  bCross=WIDGET_BASE(wtabAnalysisPET, TITLE='Crosscalibration', /COLUMN)
  bHomogPET=WIDGET_BASE(wtabAnalysisPET, TITLE='Uniformity', /COLUMN)
  bRC=WIDGET_BASE(wtabAnalysisPET, TITLE='RC', /COLUMN)

  ;------------Crosscalibration--------------------
  bCrossROI=WIDGET_BASE(bCross, /ROW)
  lblCrossROIsz = WIDGET_LABEL(bCrossROI, VALUE='ROI radius (mm)',FONT=font1)
  txtCrossROIsz = WIDGET_TEXT(bCrossROI, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)

  bCrossInput=WIDGET_BASE(bCross, /ROW)
  bCrossInputLft=WIDGET_BASE(bCrossInput, /COLUMN, FRAME=1, XSIZE=360)
  bCrossMeasA=WIDGET_BASE(bCrossInputLft, /ROW)
  lblCrossMeasAct = WIDGET_LABEL(bCrossMeasA, VALUE='Activity before injection (MBq)',FONT=font1)
  txtCrossMeasAct = WIDGET_TEXT(bCrossMeasA, VALUE='0.0', /EDITABLE, XSIZE=7, FONT=font1)
  lblCrossMeasActT = WIDGET_LABEL(bCrossMeasA, VALUE='  time (hh:mm)',FONT=font1)
  txtCrossMeasActT = WIDGET_TEXT(bCrossMeasA, VALUE='00:00', /EDITABLE, XSIZE=5, FONT=font1)
  bCrossMeasR=WIDGET_BASE(bCrossInputLft, /ROW)
  lblCrossMeasRest = WIDGET_LABEL(bCrossMeasR, VALUE='Rest activity after injection (MBq)',FONT=font1)
  txtCrossMeasRest = WIDGET_TEXT(bCrossMeasR, VALUE='0.0', /EDITABLE, XSIZE=5, FONT=font1)
  lblCrossMeasRT = WIDGET_LABEL(bCrossMeasR, VALUE='  time (hh:mm)',FONT=font1)
  txtCrossMeasRT = WIDGET_TEXT(bCrossMeasR, VALUE='00:00', /EDITABLE, XSIZE=5, FONT=font1)
  bCrossScanStart=WIDGET_BASE(bCrossInputLft, /ROW)
  lblCrossScanStart = WIDGET_LABEL(bCrossScanStart, VALUE='Scan started (found from first image)',FONT=font1)
  txtCrossScanStart = WIDGET_TEXT(bCrossScanStart, VALUE='', XSIZE=10, FONT=font1)
  bCrossScanAct=WIDGET_BASE(bCrossInputLft, /ROW)
  lblCrossScanAct = WIDGET_LABEL(bCrossScanAct, VALUE='Activity at start of scan (MBq)',FONT=font1)
  txtCrossScanAct = WIDGET_TEXT(bCrossScanAct, VALUE='', XSIZE=10, FONT=font1)
  bCrossInputRgt=WIDGET_BASE(bCrossInput, /COLUMN, FRAME=1, XSIZE=240)
  bCrossVol=WIDGET_BASE(bCrossInputRgt, /ROW)
  lblCrossVol = WIDGET_LABEL(bCrossVol, VALUE='Volume of container (mL)',FONT=font1)
  txtCrossVol = WIDGET_TEXT(bCrossVol, VALUE='' ,/EDITABLE,  XSIZE=10, SCR_YSIZE=20, FONT=font1)
  bCrossConc = WIDGET_BASE(bCrossInputRgt, /ROW)
  lblCrossConc = WIDGET_LABEL(bCrossConc, VALUE='Activity concentration (Bq/ml)',FONT=font1)
  txtCrossConc = WIDGET_TEXT(bCrossConc, VALUE='', XSIZE=10, FONT=font1)
  bCrossFactorPrev=WIDGET_BASE(bCrossInputRgt, /ROW)
  lblCrossFactorPrev = WIDGET_LABEL(bCrossFactorPrev, VALUE='Current calibration factor  ',FONT=font1)
  txtCrossFactorPrev = WIDGET_TEXT(bCrossFactorPrev, VALUE='1.000', XSIZE=5, SCR_YSIZE=20, /EDITABLE, FONT=font1)
  bCrossFactor=WIDGET_BASE(bCrossInputRgt, /ROW)
  lblCrossFactor = WIDGET_LABEL(bCrossFactor, VALUE='New calibration factor  ',FONT=font1)
  txtCrossFactor = WIDGET_TEXT(bCrossFactor, VALUE='', XSIZE=5, SCR_YSIZE=20, FONT=font1)

  bCrossBtm=WIDGET_BASE(bCross, /ROW)
  bCrossBtns=WIDGET_BASE(bCrossBtm, /ROW)
  ;btnCrossROI=WIDGET_BUTTON(bCrossBtns, VALUE='Show/update ROI', UVALUE='drawROIcross',FONT=font1)
  btnCross=WIDGET_BUTTON(bCrossBtns, VALUE='Get ROI values', UVALUE='cross',FONT=font1)
  btnCrossUpdate = WIDGET_BUTTON(bCrossBtns, VALUE='Calculate calibration factor', UVALUE='updateCross',FONT=font1)
  
  ;---------------Uniformity--------
    lblHomogPETMl0=WIDGET_LABEL(bHomogPET, VALUE='', SCR_YSIZE=20)
  lblInfoUniPET=WIDGET_LABEL(bHomogPET, VALUE='Calculate mean pixelvalues in a central and four peripheral ROIs and how this value varies over the slices.',FONT=font1)
  bHomogSizePET=WIDGET_BASE(bHomogPET, /ROW)
  lblHomogROIszPET = WIDGET_LABEL(bHomogSizePET, VALUE='ROI radius (mm)',FONT=font1)
  txtHomogROIszPET = WIDGET_TEXT(bHomogSizePET, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  mlH11=WIDGET_LABEL(bHomogSizePET, VALUE='', XSIZE=20, FONT=font1)
  lblHomogROIdistPET = WIDGET_LABEL(bHomogSizePET, VALUE='Radius to ROIs (mm)',FONT=font1)
  txtHomogROIdistPET = WIDGET_TEXT(bHomogSizePET, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bHomogBtnsPET=WIDGET_BASE(bHomogPET, /ROW)
  ;btnHomogROIPET=WIDGET_BUTTON(bHomogBtnsPET, VALUE='Show/update ROI', UVALUE='drawROIhomog',FONT=font1)
  btnHomogPET=WIDGET_BUTTON(bHomogBtnsPET, VALUE='Calculated uniformity', UVALUE='homog',FONT=font1)
  
  ;-------------Recovery Coefficient-------------

  lblInfo=WIDGET_LABEL(bRC, VALUE='Assuming NEMA NU2 phantom centered in lung insert.', FONT=font1)
  lblInfo2=WIDGET_LABEL(bRC, VALUE='Expecting 1 or 5 (marked) slices for the analysis. Rotation only affects the sphere ROIs.', FONT=font1)
   lblRCml0=WIDGET_LABEL(bRC, VALUE='', SCR_YSIZE=10)
;  bRCsettings=WIDGET_BASE(bRC, /ROW)
;  lblrcR1 = WIDGET_LABEL(bRCsettings, VALUE='ROI diameter (mm)', FONT=font1)
;  txtrcR1 = WIDGET_TEXT(bRCsettings, VALUE=( TOTAL(WHERE(configTags EQ 'RCRAD1')) NE -1 ? STRING(config.RCrad1,FORMAT='(f0.1)') : '37.0' ), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
;  lblMlrc=WIDGET_LABEL(bRCsettings, VALUE='', XSIZE=20, FONT=font1)
;  lblrcR2 = WIDGET_LABEL(bRCsettings, VALUE='Radius to ROIs (mm)', FONT=font1)
;  txtrcR2 = WIDGET_TEXT(bRCsettings, VALUE=( TOTAL(WHERE(configTags EQ 'RCRAD2')) NE -1 ? STRING(config.RCrad2,FORMAT='(f0.1)') : '57.2' ), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  ;bRCconcSph=WIDGET_BASE(bRC, /ROW)
  ;lblRCconcSph = WIDGET_LABEL(bRCconcSph, VALUE='Concentration in spheres at scan start (Bq/mL)', FONT=font1)
  ;txtRCconcSph = WIDGET_TEXT(bRCconcSph, VALUE='', /EDITABLE, XSIZE=8, SCR_YSIZE=20, FONT=font1)
  ;bRCconcBack=WIDGET_BASE(bRC, /ROW)
  ;lblRCconcBack = WIDGET_LABEL(bRCconcBack, VALUE='Concentration in background at scan start (Bq/mL)', FONT=font1)
  ;txtRCconcBack = WIDGET_TEXT(bRCconcBack, VALUE='', /EDITABLE, XSIZE=8, SCR_YSIZE=20, FONT=font1)
  bRCsett=WIDGET_BASE(bRC, /ROW)
  bRCsett1=WIDGET_BASE(bRCsett, /COLUMN)
  brcRev=WIDGET_BASE(bRCsett1, /ROW, /NONEXCLUSIVE)
  btnRCrev=WIDGET_BUTTON(brcRev, VALUE='Reverse order of sphere-ROIs', UVALUE='rcRev',FONT=font1)
  cw_rcType=CW_BGROUP(bRCsett1, ['Mean A50', 'Max'], /EXCLUSIVE, LABEL_TOP='Find in spheres...', /FRAME, SET_VALUE=0, UVALUE='cw_rcType',FONT=font1)
  cwRCexclude=CW_BGROUP(bRCsett, STRING(INDGEN(12)+1, FORMAT='(i0)'), COLUMN=3, SPACE=0,/NONEXCLUSIVE, LABEL_TOP='Exclude background ROI number...', UVALUE='rcBackExclude',FONT=font1)
  
  bRCButtons=WIDGET_BASE(bRC, /ROW)
  btnRC=WIDGET_BUTTON(bRCButtons, VALUE='Calculate Recovery Coefficients', UVALUE='recovCoeff',FONT=font1)

  ;******************************************************************************************
  ;********************* Result panel *********************************************************
  bPlot = WIDGET_BASE(bRgt, /COLUMN)

  wtabResult=WIDGET_TAB(bPlot, XSIZE=660, YSIZE=490, UVALUE='tabResults')
  bTableRes=WIDGET_BASE(wtabResult, TITLE='Table of results', /COLUMN, UVALUE='tabTableRes')
  bPlotRes=WIDGET_BASE(wtabResult, TITLE='Plot results', /COLUMN, UVALUE='tabPlotRes')
  bImageRes=WIDGET_BASE(wtabResult,TITLE='Image results', /COLUMN, UVALUE='tabImageRes')

  ;----table-----------
  toolbarTable=WIDGET_BASE(bTableRes,/ROW,/TOOLBAR)
  toolCopyTbl=WIDGET_BUTTON(toolbarTable, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard', UVALUE='copyTbl')

  bResults = WIDGET_BASE(bTableRes, /COLUMN)
  resTab=WIDGET_TABLE(bResults, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], /NO_ROW_HEADERS, SCR_XSIZE=650, SCR_YSIZE=300, /ALL_EVENTS, FONT=font1)
  statPlot = WIDGET_TEXT(bResults, XSIZE=50, YSIZE=10, VALUE='', FONT=font1)

  ;----plot------------
  toolbarPlot=WIDGET_BASE(bPlotRes,/ROW,/TOOLBAR)
  toolCopyCurve=WIDGET_BUTTON(toolbarPlot, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy curve to clipboard', UVALUE='copyCurve')
  tooliPlot=WIDGET_BUTTON(toolbarPlot, VALUE='iPlot', TOOLTIP='Send curves to separate window with save and edit options', UVALUE='iPlot',FONT=font1)
  bDrawPlot=WIDGET_BASE(bPlotRes, /ROW)
  ;drawPlot  = WIDGET_DRAW(bDrawPlot, XSIZE=450, YSIZE=380, RETAIN=2)
  drawPlot  = WIDGET_WINDOW(bDrawPlot, XSIZE=650, YSIZE=380, GRAPHICS_LEVEL=2); hvis object graphics
  
  bRangeX=WIDGET_BASE(bPlotRes, /ROW)
  lblRangeX = WIDGET_LABEL(bRangeX, VALUE='Horizontal axis range (lower, upper)', XSIZE=200,FONT=font1)
  txtMinRangeX = WIDGET_TEXT(bRangeX, VALUE='0', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lblMlmRx= WIDGET_LABEL(bRangeX, VALUE=', ', XSIZE=10, FONT=font1)
  txtMaxRangeX = WIDGET_TEXT(bRangeX, VALUE='1', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  setRangeMinMaxX = WIDGET_BUTTON(bRangeX, VALUE='Set to default min/max', UVALUE='setRangeMinMaxX',FONT=font1)

  bRangeY=WIDGET_BASE(bPlotRes, /ROW)
  lblRangeY = WIDGET_LABEL(bRangeY, VALUE='Vertical axis range (lower, upper)', XSIZE=200,FONT=font1)
  txtMinRangeY = WIDGET_TEXT(bRangeY, VALUE='0', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lblMlmRy= WIDGET_LABEL(bRangeY, VALUE=', ', XSIZE=10, FONT=font1)
  txtMaxRangeY = WIDGET_TEXT(bRangeY, VALUE='1', /EDITABLE, XSIZE=7, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  setRangeMinMaxY = WIDGET_BUTTON(bRangeY, VALUE='Set to default min/max', UVALUE='setRangeMinMaxY',FONT=font1)

  ;----image------------
  toolbarImageRes=WIDGET_BASE(bImageRes,/ROW,/TOOLBAR)
  toolIimageRes = WIDGET_BUTTON(toolbarImageRes, VALUE=thisPath+'images\ax.bmp', /BITMAP, UVALUE='iImageRes', TOOLTIP='Send result to iImage')
  drawImageRes  = WIDGET_DRAW(bImageRes, XSIZE=450, YSIZE=450, RETAIN=2)

  ;****************** BOTTOM Panel
  bDir=WIDGET_BASE(bMain,/ROW)
  lblDirectory=WIDGET_LABEL(bDir, VALUE='Full path:  ',FONT=font1)
  lblDir=WIDGET_LABEL(bDir, VALUE='',xSIZE=winX-170, YSIZE=18, /SUNKEN_FRAME,FONT=font1)

  refreshParam, config, ''
  fillQuickTempList, quickTemp;pro in refreshParam.pro

  loadct, 0, /SILENT
  WIDGET_CONTROL, bMain, /REALIZE
  XMANAGER, 'ImageQC', bMain, /NO_BLOCK
  DEVICE, RETAIN=2, DECOMPOSED=0
  
  WIDGET_CONTROL, drawLarge, GET_VALUE=iDrawPlot
  iDrawPlot.erase

end


