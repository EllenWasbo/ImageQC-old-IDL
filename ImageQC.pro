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

  COMMON VARI,  $
    lblDir, listFiles, lastList, lblLoadedN, lblProgress, activeImg, activeResImg, nFrames, ROIs,  $
    marked, markedMulti, multiOpt, btnUseMulti, listSelMultiTemp, multiExpTable, $
    txtActive1, txtActive2, newline, $
    drawLarge, drawXY, coltable, btnSetColorTable, txtMinWL, txtMaxWL, txtCenterWL, txtWidthWL, lblCursorValue, lblCursorPos,lblCursorPosMM, $
    txtDeltaX, txtDeltaY, txtDeltaA, dxya, useDelta, btnHideAnnot,$
    defPath, thisPath, structImgs, decimMark, copyHeader, headers, lastXY, lastXYreleased, mouseDown, $
    useMulti, modality, analyse, analyseStringsCT, analyseStringsXray, analyseStringsNM, analyseStringsPET, results, $
    resTab, wtabResult, wtabModes,wtabAnalysisCT,wtabAnalysisXray,wtabAnalysisNM, wtabAnalysisPET, $
    drawPlot, statPlot,drawImageRes, txtMinRangeX, txtMaxRangeX, txtMinRangeY, txtMaxRangeY, rangeAcc, $
    ROIres, typeROI, typeROIX, $
    MTFres, $
    cw_typeMTF, cw_plotMTF, txtMTFroiSz, btnCutLSF, txtcutLSFW, txtcutLSFW2,txtfreqMTF, $
    cw_formLSFX, cw_plotMTFX,  txtMTFroiSzX, txtMTFroiSzY, btnCutLSFX, txtcutLSFWX, $
    cw_typeMTFNM, cw_plotMTFNM, txtMTFroiSzXNM, txtMTFroiSzYNM, btnCutLSFNM, txtcutLSFWNM, MTF3dNM, MTFsumNM, $
    CTlinRes, CTlinROIs, CTlinROIpos, txtLinROIrad, txtLinROIradS, tblLin, linTabEdit, btnLinAvoidSearch, $
    sliceThickRes, sliceThickResTab,  ramps, txtRampDist, txtRampLen, txtRampBackG, txtRampSearch, txtRampAverage, cw_ramptype,  $
    homogRes, homogROIs, txtHomogROIsz, txtHomogROIszPET, txtHomogROIszX, txtHomogROIdist, txtHomogROIdistPET, cw_homogNM, txtHomogROIszNM, txtHomogROIdistXNM, txtHomogROIdistYNM, $
    noiseRes, noiseROI, txtNoiseROIsz, $
    fwhmRes, dimRes, energyRes, $
    stpRes, txtStpROIsz, stpROI, txtRQA, Qvals, eiRes, $
    NPSres, NPSrois, $
    txtNPSroiSz, txtNPSroiDist, txtNPSsubNN, txtSmoothNPS, txtfreqNPS, btnNPSavg, $
    txtNPSroiSzX, txtNPSsubSzX, lblNPSsubSzMMX, lblNPStotpixX, $
    txtNAvgSpeedNM, txtScanSpeedMedian, txtSpeedROIheight,$
    txtVarImageROIsz, lblProgressVarX, $
    contrastRes, conROIs, txtConR1NM, txtConR2NM,$
    radialRes, txtRadialMedian, $
    crossRes, crossROI, txtCrossROIsz, txtCrossMeasAct,txtCrossMeasActT, txtCrossMeasRest, txtCrossMeasRT, txtCrossScanAct, txtCrossScanStart,$
    txtCrossVol, txtCrossConc, txtCrossFactorPrev, txtCrossFactor,$
    rcRes, rcROIs, btnRCrev, cwRCexclude, cw_rcType

  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  
  RESTORE, thisPath+'data\config.dat'
  RESTORE, thisPath+'data\configDefault.dat'
  configTags=TAG_NAMES(config)
  defPath=config.defPath

  structImgs=CREATE_STRUCT('empty',0); images with attributes in memory
  activeImg=0 ; active (selected image)
  activeResImg=0 ; result-image (fx 2d NPS)
  nFrames=0; if multiframe image loaded, nFrames=nImg
  analyseStringsCT=['HOMOG', 'NOISE','CTLIN', 'SLICETHICK','DIM', 'MTF', 'NPS','ROI', 'FWHM']
  analyseStringsXray=['STP','HOMOG','NOISE','EI','MTF','NPS','VARI','ROI']
  analyseStringsNM=['ENERGYSPEC','HOMOG','SCANSPEED','CONTRAST','MTF','RADIAL']
  analyseStringsPET=['CROSSCALIB','HOMOG','RC']
  analyse=analyseStringsCT(0)
  modality=0; to save current modality for regretting switch and loose results
  marked=-1; indexes of marked files (-1 = all marked)
  markedMulti=-1; matrix of marked images for numbered tests (number of tests x number of images) -1 if useMuliMark is not set
  multiOpt=CREATE_STRUCT('CT',INTARR(9),'Xray',[1,2,3,4,5,0,0,0],'NM',INTARR(6),'PET',INTARR(3)); structure of arrays corresponding to analyseStrings that have the option of being a numbered test for multimark/quicktest, number or 0 if not an option
  multiExpTable=-1
  results=INTARR(9); set to 1 when analyse performed and results is available, keep analyseString and tab-order equal
  dxya=[0,0,0.0,1]; [deltax,deltay,delta,show] for positioning of center/angle. Difference from imgSz/2 in pixels. Show (last param) used for redrawCT to overplot crosshair
  CTlinROIs=0 & CTlinROIpos=0 & homogROIs=0 & noiseROI=0 & NPSrois=0 & conROIs=0 & crossROI=0; used to hold the rois for specific tests
  ramps=0; used to hold the 4 lines for slice thickness H-top,H-bottom,V1,V2
  lastXY=[-1,-1]; last mouseposition in draw window
  lastXYreleased=[-1,-1,-1]; x, y, time
  lastList=[-1,-1]; last first selected, time to control doubleclicks in fileList
  mouseDown=0; If mouse pressed in draw window and still not released 1=true
  coltable=0;  grayscale default
  decimMark=TOTAL(WHERE(configTags EQ 'DECIMMARK')) NE -1 ? config.decimMark : ','
  copyHeader=TOTAL(WHERE(configTags EQ 'COPYHEADER')) NE -1 ? config.copyHeader : 1
  if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)

  font1="Arial*15"

  winX=1400 &  winY=1000 ;actual winX not 1400 - adjusted down to 1250 - should be adapted to screen size (Todo)
  drawXY=500

  s=obj_new('idlsysmonitorinfo')
  nMon=s->GetNumberOfMonitors()
  pMon=s->GetPrimaryMonitorIndex()
  rMon=s->GetRectangles()
  scsz=rMon[2:3,pMon]
  ;DEVICE, GET_SCREEN_SIZE=scsz

  IF scsz(0) LT winX THEN scX=scsz(0)-10 ELSE scX=winX
  IF scsz(1) LT winY THEN scY=scsz(1)-50 ELSE scY=winY-50

  bMain = WIDGET_BASE(TITLE='ImageQC v1.200', MBAR=bar, /COLUMN, XSIZE=winX, YSIZE=winY-60, XOFFSET=100, YOFFSET=100, X_SCROLL_SIZE=scX, Y_SCROLL_SIZE=scY, /TLB_KILL_REQUEST_EVENTS)
  bLarge = WIDGET_BASE(bMain, /ROW)
  bLft = WIDGET_BASE(bLarge, XSIZE=700, YSIZE=winY-90,/COLUMN)
  bRgt = WIDGET_BASE(bLarge, XSIZE=700, YSIZE=winY-90,/COLUMN)

  ;*****************MENU
  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  help_menu=WIDGET_BUTTON(bar, VALUe='Help', /MENU)
  ;file_menu
  btnOpen=WIDGET_BUTTON(file_menu, VALUE='Open DICOM file or series', UVALUE='open', ACCELERATOR='Ctrl+O')
  btnSaveStruct=WIDGET_BUTTON(file_menu, VALUE='Save active file as IDL structure (.dat-file)', UVALUE='saveDat')
  btnPref=WIDGET_BUTTON(file_menu, VALUE='Preferences on export format..', UVALUE='pref')
  btnDefPath=WIDGET_BUTTON(file_menu, VALUE='Define default path', UVALUE='defpath', /SEPARATOR)
  btnConfig=WIDGET_BUTTON(file_menu, VALUE='Save or backup current default values to config file', UVALUE='config')
  btnRestoreConfig=WIDGET_BUTTON(file_menu, VALUE='Restore default values from backup config file', UVALUE='restoreConfig')
  btnClose=WIDGET_BUTTON(file_menu, VALUE='Close all images', UVALUE='close', /SEPARATOR)
  btnExit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='exit', ACCELERATOR='Ctrl+X')
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

  lblProgress = WIDGET_LABEL(toolbarLft, VALUE='', /DYNAMIC_RESIZE)

  ;****************** left Panel
  bInfoLoaded=WIDGET_BASE(bLft, /ROW, YSIZE=220)
  bInfoLft=WIDGET_BASE(bInfoLoaded, /COLUMN)

  ;list ++
  bList=WIDGET_BASE(bInfoLft, /ROW)
  bListLoaded=WIDGET_BASE(bList, /COLUMN)
  bListLoadedTitle=WIDGET_BASE(bListLoaded, /ROW)
  lblLoaded=WIDGET_LABEL(bListLoadedTitle, VALUE='Loaded images ( ', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  lblLoadedN=WIDGET_LABEL(bListLoadedTitle, VALUE='0 )', /ALIGN_LEFT, FONT="Arial*ITALIC*16", XSIZE=290)
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
  mlRgtimg = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=5, FONT=font1)

  ;rgt of image - center angle
  bCenterAngle = WIDGET_BASE(bDrawLft, /COLUMN)
  titleCenterAngle = WIDGET_LABEL(bCenterAngle, VALUE='Center / rotation',/ALIGN_LEFT, FONT="Arial*ITALIC*16")
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
  btnShowDelta=WIDGET_BUTTON(bCenterAngle, VALUE='Get center',FONT=font1, UVALUE='getCenter')
  btnShowDelta=WIDGET_BUTTON(bCenterAngle, VALUE='Set center',FONT=font1, UVALUE='setCenter', TOOLTIP='Sets center to the position of the last mouseclick in image')

  mlRgtimg2 = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=5, FONT=font1)
  ;iImage toolbar
  toolBarDraw = WIDGET_BASE(bDrawLft, /ROW, /TOOLBAR)
  lbliImage=WIDGET_LABEL(toolBarDraw, VALUE='iImage:',FONT=font1)
  btnAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\ax.bmp', /BITMAP, UVALUE='ax', TOOLTIP='Send active image to iImage window')
  btnCor = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\cor.bmp', /BITMAP, UVALUE='cor', TOOLTIP='Send coronal image found from image stack at defined senter to iImage window')
  btnSag = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sag.bmp', /BITMAP, UVALUE='sag', TOOLTIP='Send sagittal image found from image stack at defined senter to iImage window')
  lblML2=WIDGET_LABEL(toolBarDraw, VALUE='', XSIZE=5, FONT=font1)
  btnSumAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sum.bmp', /BITMAP, UVALUE='sumax', TOOLTIP='Sum all or marked (X) images and send to iImage window')
  btn3dVol  = WIDGET_BUTTON(toolBarDraw, VALUE='3D', UVALUE='3d', TOOLTIP='Under construction...')

  bHide=WIDGET_BASE(bDrawLft, /ROW, /NONEXCLUSIVE)
  btnHideAnnot=WIDGET_BUTTON(bHide, VALUE='Hide annotations', UVALUE='hideAnnot',FONT=font1)

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
  ;QuickTest
  bMultiAct=WIDGET_BASE(bRgt, /COLUMN, XSIZE=660, YSIZE=70, FRAME=1)
  lblMulti=WIDGET_LABEL(bMultiAct, VALUE='QuickTest', /ALIGN_LEFT, FONT="Arial*ITALIC*16")
  bMulti=WIDGET_BASE(bMultiAct, /ROW)
  bUseMulti=WIDGET_BASE(bMulti, /NONEXCLUSIVE)
  btnUseMulti=WIDGET_BUTTON(bUseMulti, VALUE='Use MultiMark', UVALUE='useMulti', TOOLTIP='Mark images for specific numbered tests', FONT=font1)
  lblMlMulti1=WIDGET_LABEL(bMulti, VALUE='', XSIZE=20)
  lblListMulti=WIDGET_LABEL(bMulti, VALUE='Select MultiMark template:', FONT=font1)
  listSelMultiTemp=WIDGET_DROPLIST(bMulti, UVALUE='listSelMultiTemp', XSIZE=100, FONT=font1)
  bMultiTool=WIDGET_BASE(bMulti, /ROW, /TOOLBAR)
  btnSaveMultiTemp=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\save.bmp',/BITMAP, UVALUE='saveMultiTemp', TOOLTIP='Save current MultiMark as template', YSIZE=20)
  btnDelMultiTemp=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='delMultiTemp', TOOLTIP='Delete selected template', YSIZE=20)
  lblMlMulti2=WIDGET_LABEL(bMultiTool, VALUE='', XSIZE=80)
  btnRunMulti=WIDGET_BUTTON(bMultiTool, VALUE='QuickTest', UVALUE='runMulti', TOOLTIP='Calculate results for all numbered tests and the corresponding marked images', YSIZE=20, FONT=font1)
  btnExpMulti=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\copy.bmp',/BITMAP, UVALUE='expMulti', TOOLTIP='Copy results to clipboard (paste into fx Excel)', YSIZE=20)
  
  tagsConfig=TAG_NAMES(config)
  IF tagsConfig.HasValue('QUICKTEMP') THEN BEGIN
    typ=SIZE(config.QUICKTEMP, /TNAME)
    IF typ EQ 'STRUCT' THEN BEGIN
      tempNames=TAG_NAMES(config.QUICKTEMP)
      WIDGET_CONTROL, listSelMultiTemp, SET_VALUE=['',tempNames], SET_DROPLIST_SELECT=0
    ENDIF
  ENDIF
  
  lblMlRgt0=WIDGET_LABEL(bRgt, VALUE='', YSIZE=20)
  
  ;Analysis tabs
  bAnalysis=WIDGET_BASE(bRgt, /COLUMN)
  wtabModes=WIDGET_TAB(bAnalysis, XSIZE=660, YSIZE=260, UVALUE='tabModes')
  bCT=WIDGET_BASE(wtabModes, TITLE='CT', /COLUMN, UVALUE='tabCT')
  bX=WIDGET_BASE(wtabModes,TITLE='Xray', /COLUMN, UVALUE='tabXray')
  bNM=WIDGET_BASE(wtabModes, TITLE='NM',/COLUMN, UVALUE='tabNM')
  bPET=WIDGET_BASE(wtabModes, TITLE='PET', /COLUMN, UVALUE='tabPET')

  wtabAnalysisCT=WIDGET_TAB(bCT, XSIZE=650, YSIZE=240)
  wtabAnalysisXray=WIDGET_TAB(bX, XSIZE=650, YSIZE=240)
  wtabAnalysisNM=WIDGET_TAB(bNM, XSIZE=650, YSIZE=240)
  wtabAnalysisPET=WIDGET_TAB(bPET, XSIZE=650, YSIZE=240)

  ; *************************CT tests*****************************************************
  bHomog=WIDGET_BASE(wtabAnalysisCT, Title='Homogeneity', /COLUMN)
  bNoise=WIDGET_BASE(wtabAnalysisCT, Title='Noise', /COLUMN)
  bLinearity=WIDGET_BASE(wtabAnalysisCT, Title='CT Number', /ROW)
  bSliceThick=WIDGET_BASE(wtabAnalysisCT, Title='Slice thickness', /ROW)
  bDim=WIDGET_BASE(wtabAnalysisCT, TITLE='Dim', /COLUMN)
  bMTF=WIDGET_BASE(wtabAnalysisCT, TITLE='MTF',/Column)
  bNPS=WIDGET_BASE(wtabAnalysisCT, TITLE='NPS',/Column)
  bROI=WIDGET_BASE(wtabAnalysisCT, TITLE='ROI',/Column)
  bFwhm=WIDGET_BASE(wtabAnalysisCT, Title='FWHM', /COLUMN)

  ;--------------- Linear dimensions DIM
  lblDimInfoml0=WIDGET_LABEL(bDim, VALUE='', SCR_YSIZE=20)
  lblDimInfo=WIDGET_LABEL(bDim, VALUE='Find center of rod +/-25 mm from center with a margin of 10 mm and calculate distance between rods',FONT=font1)
  lblDimInfoml1=WIDGET_LABEL(bDim, VALUE='', FONT=font1)
  bDimBtns=WIDGET_BASE(bDim, /ROW)
  btnDim=WIDGET_BUTTON(bDimBtns, VALUE='Calculate linear dimensions', UVALUE='dim',FONT=font1)

  ;---------------Homogeneity--------
  lblHomogMl0=WIDGET_LABEL(bHomog, VALUE='', SCR_YSIZE=20)
  bHomogSize=WIDGET_BASE(bHomog, /ROW)
  lblHomogROIsz = WIDGET_LABEL(bHomogSize, VALUE='ROI radius (mm)',FONT=font1)
  txtHomogROIsz = WIDGET_TEXT(bHomogSize, VALUE=STRING(config.HomogROIsz,FORMAT='(f0.1)') , /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  mlH1=WIDGET_LABEL(bHomogSize, VALUE='', XSIZE=20, FONT=font1)
  lblHomogROIdist = WIDGET_LABEL(bHomogSize, VALUE='Radius to ROIs (mm)',FONT=font1)
  txtHomogROIdist = WIDGET_TEXT(bHomogSize, VALUE=STRING(config.HomogROIdist,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bHomogBtns=WIDGET_BASE(bHomog, /ROW)
  btnHomog=WIDGET_BUTTON(bHomogBtns, VALUE='Calculated homogeneity', UVALUE='homog',FONT=font1)

  ;---------------Noise--------
  lblNoiseMl0=WIDGET_LABEL(bNoise, VALUE='', SCR_YSIZE=20)
  bNoiseROI=WIDGET_BASE(bNoise, /ROW)
  lblNoiseROIsz = WIDGET_LABEL(bNoiseROI, VALUE='ROI radius (mm)',FONT=font1)
  txtNoiseROIsz = WIDGET_TEXT(bNoiseROI, VALUE= STRING(config.NoiseROIsz,FORMAT='(f0.1)') , /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bNoiseBtns=WIDGET_BASE(bNoise, /ROW)
  btnNoise=WIDGET_BUTTON(bNoiseBtns, VALUE='Calculated noise', UVALUE='noise',FONT=font1)

  ;----------------MTF------------------
  bMTFsettings=WIDGET_BASE(bMTF, /ROW)

  bMTFlft=WIDGET_BASE(bMTFsettings,/COLUMN)
  cw_typeMTF=CW_BGROUP(bMTFlft, ['Bead','Wire','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME, SET_VALUE=config.MTFtype,FONT=font1)

  bMTFrgt=WIDGET_BASE(bMTFsettings,/COLUMN)
  bCutLSF=WIDGET_BASE(bMTFrgt, /NONEXCLUSIVE, /ROW)
  btnCutLSF=WIDGET_BUTTON(bCutLSF, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  WIDGET_CONTROL, btnCutLSF, SET_BUTTON=( TOTAL(WHERE(configTags EQ 'CUTLSF')) NE -1 ? config.cutLSF : 0 )
  bCutLSFW=WIDGET_BASE(bMTFrgt, /ROW)
  lblCutLSFW=WIDGET_LABEL( bCutLSFW, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1)
  txtCutLSFW=WIDGET_TEXT( bCutLSFW, VALUE=( TOTAL(WHERE(configTags EQ 'CUTLSF1')) NE -1 ? STRING(config.cutLSF1,FORMAT='(i0)') : STRING(configDefault.cutLSF1,FORMAT='(i0)') ), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bCutLSFW2=WIDGET_BASE(bMTFrgt,/ROW)
  lblCutLSFW2=WIDGET_LABEL( bCutLSFW2, VALUE='Fade out cut within (#FWHM)',FONT=font1)
  txtCutLSFW2=WIDGET_TEXT( bCutLSFW2, VALUE=( TOTAL(WHERE(configTags EQ 'CUTLSF2')) NE -1 ? STRING(config.cutLSF2,FORMAT='(i0)') : STRING(configDefault.cutLSF2,FORMAT='(i0)') ), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bfreqMTF=WIDGET_BASE(bMTFrgt, /ROW)
  lblfreqMTF=WIDGET_LABEL(bfreqMTF, VALUE='Sampling frequency gaussian MTF curve (mm-1)',FONT=font1)
  txtfreqMTF=WIDGET_TEXT(bfreqMTF, VALUE='0.010', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1);( TOTAL(WHERE(configTags EQ 'MTFFREQ')) NE -1 ? STRING(config.MTFFREQ,FORMAT='(f0.3)') : STRING(configDefault.MTFFREQ,FORMAT='(f0.3)'))
  bMTFroiSz=WIDGET_BASE(bMTFrgt, /ROW)
  lblMTFroiSz=WIDGET_LABEL(bMTFroiSz, VALUE='ROI size from center (mm)',FONT=font1)
  txtMTFroiSz=WIDGET_TEXT(bMTFroiSz, VALUE=STRING(config.MTFroiSz,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)

  cw_plotMTF=CW_BGROUP(bMTFsettings, ['Centered xy profiles', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, SET_VALUE=config.plotMTF, UVALUE='cw_plotMTF',FONT=font1)

  bMTFbtns=WIDGET_BASE(bMTF, /ROW)
  btnMTF=WIDGET_BUTTON(bMTFbtns, VALUE='Calculate MTF', UVALUE='MTF',FONT=font1)

  ;------------ NPS ---------------------
  bNPSroiSz=WIDGET_BASE(bNPS, /ROW)
  lblNPSroiSz=WIDGET_LABEL(bNPSroiSz, VALUE='ROI size (pix)',FONT=font1)
  txtNPSroiSz=WIDGET_TEXT(bNPSroiSz, VALUE=( TOTAL(WHERE(configTags EQ 'NPSROISZ')) NE -1 ? STRING(config.NPSroiSz,FORMAT='(i0)') : STRING(configDefault.NPSroiSz,FORMAT='(i0)') ), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSroiDist=WIDGET_BASE(bNPS, /ROW)
  lblNPSroiDist=WIDGET_LABEL(bNPSroiDist, VALUE='Radius to center of ROIs (mm)',FONT=font1)
  txtNPSroiDist=WIDGET_TEXT(bNPSroiDist, VALUE=( TOTAL(WHERE(configTags EQ 'NPSROIDIST')) NE -1 ? STRING(config.NPSroiDist,FORMAT='(f0.1)') : STRING(configDefault.NPSroiDist,FORMAT='(f0.1)') ), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSsubNN=WIDGET_BASE(bNPS, /ROW)
  lblNPSsubNN=WIDGET_LABEL(bNPSsubNN, VALUE='Number of ROIs',FONT=font1)
  txtNPSsubNN=WIDGET_TEXT(bNPSsubNN, VALUE=( TOTAL(WHERE(configTags EQ 'NPSSUBNN')) NE -1 ? STRING(config.NPSsubNN,FORMAT='(i0)') : STRING(configDefault.NPSsubNN,FORMAT='(i0)')), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bSmoothNPS=WIDGET_BASE(bNPS, /ROW)
  lblSmoothNPS=WIDGET_LABEL(bSmoothNPS, VALUE='Smooth NPS curve by width (mm-1)',FONT=font1)
  txtSmoothNPS=WIDGET_TEXT(bSmoothNPS, VALUE='0.050', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1);( TOTAL(WHERE(configTags EQ 'NPSSMOOTH')) NE -1 ? STRING(config.NPSsmooth,FORMAT='(f0.3)') : STRING(configDefault.NPSsmooth,FORMAT='(f0.3)'))
  bfreqNPS=WIDGET_BASE(bNPS, /ROW)
  lblfreqNPS=WIDGET_LABEL(bfreqNPS, VALUE='Sampling frequency NPS curve (mm-1)',FONT=font1)
  txtfreqNPS=WIDGET_TEXT(bfreqNPS, VALUE='0.010', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1);( TOTAL(WHERE(configTags EQ 'NPSFREQ')) NE -1 ? STRING(config.NPSFREQ,FORMAT='(f0.3)') : STRING(configDefault.NPSFREQ,FORMAT='(f0.3)'))

  bNPSbtns=WIDGET_BASE(bNPS, /ROW)
  btnNPS=WIDGET_BUTTON(bNPSbtns, VALUE='Calculate NPS', UVALUE='NPS',FONT=font1)
  bNPSavg=WIDGET_BASE(bNPSbtns, /NONEXCLUSIVE, /ROW)
  btnNPSavg=WIDGET_BUTTON(bNPSavg, VALUE='Plot average', UVALUE='NPSavg',FONT=font1)
  WIDGET_CONTROL, btnNPSavg, SET_BUTTON=( TOTAL(WHERE(configTags EQ 'NPSAVG')) NE -1 ? config.NPSavg : 1 )

  ;----------------User defined ROI------------
  lblroiMl0=WIDGET_LABEL(bROI, VALUE='', SCR_YSIZE=20)
  typeROI=CW_BGROUP(bROI, ['Define new','Load saved ROI (.sav)'], /EXCLUSIVE, LABEL_TOP='Define ROI and get min/max/avg/stdev', /FRAME, SET_VALUE=config.typeROI, UVALUE='typeROI',FONT=font1)
  btnDefROI =WIDGET_BUTTON(bROI, VALUE = 'Define ROI', UVALUE='ROI',FONT=font1)

  ;-------------CT numbers linearity-------------
  bLinSettings=WIDGET_BASE(bLinearity, /ROW)
  bLinLft=WIDGET_BASE(bLinSettings, /COLUMN)
  emLin=WIDGET_LABEL(bLinLft, VALUE='', YSIZE=20, FONT=font1)
  labLinearity=WIDGET_LABEL(bLinLft, VALUE='Calculate CT Numbers within ROIs',FONT=font1)
  emLin2=WIDGET_LABEL(bLinLft, VALUE='', YSIZE=20, FONT=font1)
  bLinSearchROI=WIDGET_BASE(bLinLft, /ROW)
  lblLargeRad = WIDGET_LABEL(bLinSearchROI, VALUE='Radius of search ROIs (mm)',FONT=font1)
  txtLinROIradS = WIDGET_TEXT(bLinSearchROI, VALUE=(TOTAL(WHERE(configTags EQ 'LINROIRADS')) NE -1 ? STRING(config.LinROIradS,FORMAT='(f0.1)') : STRING(configDefault.LinROIradS,FORMAT='(f0.1)')), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bLinSzROI=WIDGET_BASE(bLinLft, /ROW)
  lblSampleRad = WIDGET_LABEL(bLinSzROI, VALUE='ROI radius (mm)',FONT=font1)
  txtLinROIrad = WIDGET_TEXT(bLinSzROI, VALUE=STRING(config.LinROIrad,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
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
  IF TOTAL(WHERE(configTags EQ 'LINTAB')) NE -1 THEN lintabStruc=config.LinTab ELSE lintabStruc=configDefault.LinTab
  ysz=N_ELEMENTS(lintabStruc.Materials)
  fillLin=STRARR(4,ysz)
  fillLin[0,*]=TRANSPOSE(lintabStruc.Materials)
  fillLin[1,*]=STRING(TRANSPOSE(lintabStruc.posX), FORMAT='(f0.1)')
  fillLin[2,*]=STRING(TRANSPOSE(lintabStruc.posY), FORMAT='(f0.1)')
  fillLin[3,*]=STRING(TRANSPOSE(lintabStruc.RelMassD), FORMAT='(f0.3)')
  WIDGET_CONTROL, tblLin, TABLE_YSIZE=ysz, SET_VALUE=fillLin, SET_TABLE_SELECT=[-1,-1,-1,-1], SET_TABLE_VIEW=[0,0]
  
  ;---------------Slice thickness--------
  lblSlMl0=WIDGET_LABEL(bSliceThick, VALUE='', SCR_YSIZE=20)
  bSliceThickLft=WIDGET_BASE(bSliceThick, /COLUMN)
  cw_ramptype=CW_BGROUP(bSliceThickLft, ['Wire ramp','Beaded ramp'], /EXCLUSIVE, LABEL_TOP='Ramp type...', /FRAME, SET_VALUE=0,FONT=font1);, SET_VALUE=config.MTFtype)
  bSliceThickRgt=WIDGET_BASE(bSliceThick, /COLUMN)
  bRampDist=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampDist = WIDGET_LABEL(bRampDist, VALUE='Center to ramp distance (mm)',FONT=font1)
  txtRampDist = WIDGET_TEXT(bRampDist, VALUE=STRING(config.RampDist,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lbl2RampDist = WIDGET_LABEL(bRampDist, VALUE='(ignored for beaded ramp, CTP591 geometry used)',FONT=font1)
  bRampLen=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampLen = WIDGET_LABEL(bRampLen, VALUE='Profile length (mm)',FONT=font1)
  txtRampLen = WIDGET_TEXT(bRampLen, VALUE=STRING(config.RampLen,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bRampBack=WIDGET_BASE(bRampLen, /ROW)
  lblBackG = WIDGET_LABEL(bRampBack, VALUE='Background from outer (mm)',FONT=font1)
  txtRampBackG = WIDGET_TEXT(bRampBack, VALUE=STRING(config.RampBackG,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bRampSearch=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampSearch = WIDGET_LABEL(bRampSearch, VALUE='Search for maximum in profile',FONT=font1)
  txtRampSearch = WIDGET_TEXT(bRampSearch, VALUE=STRING(config.RampSearch,FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lblRampSearch2 = WIDGET_LABEL(bRampSearch, VALUE='# pix from center of ramp',FONT=font1)
  bRampAverage=WIDGET_BASE(bSliceThickRgt, /ROW)
  lblRampAverage = WIDGET_LABEL(bRampAverage, VALUE='Use profile from average of ',FONT=font1)
  txtRampAverage = WIDGET_TEXT(bRampAverage, VALUE=STRING(config.RampAvg,FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lblRampAverage2 = WIDGET_LABEL(bRampAverage, VALUE='# neighbour profiles from profile with max value',FONT=font1)
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
  txtStpROIsz = WIDGET_TEXT(bStpSettings, VALUE=STRING(config.STProiSz,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  Qvals=[21759.,30174.,32362.,31077.,1]
  lblStpBeamQuality = WIDGET_LABEL(bStpSettings, VALUE='     Beam quality',FONT=font1)
  ddlRQA = WIDGET_COMBOBOX(bStpSettings, VALUE=['RQA 3','RQA 5','RQA 7','RQA 9','other'], UVALUE='ddlRQA', /LIST_EVENTS,FONT=font1)
  WIDGET_CONTROL, ddlRQA, SET_COMBOBOX_SELECT=1
  txtRQA = WIDGET_TEXT(bStpSettings, VALUE=STRING(Qvals(1),FORMAT='(i0)'), UVALUE='txtRQA', XSIZE=7, /EDITABLE, FONT=font1)
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
  txtHomogROIszX = WIDGET_TEXT(bHomogSizeX, VALUE=STRING(config.HomogROIszX,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
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
  cw_formLSFX=CW_BGROUP(bMTFX, ['Exponential','Gaussian','None'], /EXCLUSIVE, LABEL_TOP='LSF fit to...', /FRAME, SET_VALUE=config.MTFtypeX,FONT=font1)
  bLSFfilterX=WIDGET_BASE(bMTFX, /COLUMN)
  bCutLSFX=WIDGET_BASE(bLSFfilterX, /NONEXCLUSIVE, /ROW)
  btnCutLSFX=WIDGET_BUTTON(bCutLSFX, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  WIDGET_CONTROL, btnCutLSFX, SET_BUTTON=( TOTAL(WHERE(configTags EQ 'CUTLSFX')) NE -1 ? config.cutLSF : 0 )
  bCutLSFWX=WIDGET_BASE(bLSFfilterX, /ROW)
  lblCutLSFWX=WIDGET_LABEL( bCutLSFWX, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1)
  txtCutLSFWX=WIDGET_TEXT( bCutLSFWX, VALUE=( TOTAL(WHERE(configTags EQ 'CUTLSFX1')) NE -1 ? STRING(config.cutLSFX1,FORMAT='(i0)') : STRING(configDefault.cutLSFX1,FORMAT='(i0)') ), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  cw_plotMTFX=CW_BGROUP(bMTFX, ['Edge position', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, SET_VALUE=config.plotMTFX, UVALUE='cw_plotMTFX',FONT=font1)

  bMTFroiSzX=WIDGET_BASE(bMTFsettingsX, /ROW)
  lblMTFroiSzX=WIDGET_LABEL(bMTFroiSzX, VALUE='ROI width x height (mm)',FONT=font1)
  txtMTFroiSzX=WIDGET_TEXT(bMTFroiSzX, VALUE=STRING(config.MTFroiSzX(0),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  lblMTFx=WIDGET_LABEL(bMTFroiSzX, VALUE=' x ',FONT=font1)
  txtMTFroiSzY=WIDGET_TEXT(bMTFroiSzX, VALUE=STRING(config.MTFroiSzX(1),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)

  bMTFbtnsX=WIDGET_BASE(bMTFsettingsX, /ROW)
  btnMTFX=WIDGET_BUTTON(bMTFbtnsX, VALUE='Calculate MTF', UVALUE='MTFX',FONT=font1)

  ;----------------NPS------------------
  bNPSroiSzX=WIDGET_BASE(bNPSX, /ROW)
  lblNPSroiSzX=WIDGET_LABEL(bNPSroiSzX, VALUE='ROI size (pix)',FONT=font1)
  txtNPSroiSzX=WIDGET_TEXT(bNPSroiSzX, VALUE=STRING(config.NPSroiSzX,FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSsubSzX=WIDGET_BASE(bNPSX, /ROW)
  lblNPSsubSzX=WIDGET_LABEL(bNPSsubSzX, VALUE='Subimage size (pix)',FONT=font1)
  txtNPSsubSzX=WIDGET_TEXT(bNPSsubSzX, VALUE=STRING(config.NPSsubSzX,FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lblNPSsubSz2X=WIDGET_LABEL(bNPSsubSzX, VALUE=' x ROI size = ',FONT=font1)
  lblNPSsubSzMMX=WIDGET_LABEL(bNPSsubSzX, VALUE='', XSIZE=20, SCR_XSIZE=20, FONT=font1)
  lblNPSsubSz3X=WIDGET_LABEL(bNPSsubSzX, VALUE=' mm',FONT=font1)
  bNPStotPixX=WIDGET_BASE(bNPSX, /ROW)
  lblNPStotPix0X=WIDGET_LABEL(bNPStotPixX, VALUE='# independent pixels/image (preferrably 4 mill in total): ',FONT=font1)
  nn=((2*LONG(config.NPSsubSzX)-1)*LONG(config.NPSroiSzX))^2
  lblNPStotPixX=wIDGET_LABEL(bNPStotPixX, VALUE=STRING(nn, FORMAT='(i0)'), /DYNAMIC_RESIZE,FONT=font1)

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
  typeROIX=CW_BGROUP(bROIX, ['Define new','Load saved ROI (.sav)'], /EXCLUSIVE, LABEL_TOP='Define ROI and get min/max/avg/stdev', /FRAME, SET_VALUE=config.typeROIX, UVALUE='typeROIX',FONT=font1)
  btnDefROIX =WIDGET_BUTTON(bROIX, VALUE = 'Define ROI', UVALUE='ROI',FONT=font1)

  ;***********************NM tests**********************************************************
  bEnergySpec=WIDGET_BASE(wtabAnalysisNM, TITLE='Energy spectrum', /COLUMN)
  bHomogNM=WIDGET_BASE(wtabAnalysisNM, Title='Uniformity', /COLUMN)
  bScanSpeed=WIDGET_BASE(wtabAnalysisNM, Title='Scan Speed', /COLUMN)
  bContrastNM=WIDGET_BASE(wtabAnalysisNM, Title='Contrast', /COLUMN)
  bMTFNM=WIDGET_BASE(wtabAnalysisNM, TITLE='MTF',/Column)
  bRadialProfile=WIDGET_BASE(wtabAnalysisNM, Title='Radial Profile', /COLUMN)

  ;------------energy spectrum--------------------
    lblesMl0=WIDGET_LABEL(bEnergySpec, VALUE='', SCR_YSIZE=20)
  bEnergySpecBtns=WIDGET_BASE(bEnergySpec, /ROW)
  btnLoadSpec=WIDGET_BUTTON(bEnergySpecBtns, VALUE='Load spectrum', UVALUE='loadSpectrum',FONT=font1)

  ;---------------Homogeneity--------
    lblHomognmMl0=WIDGET_LABEL(bHomogNM, VALUE='', SCR_YSIZE=20)
  bHomogNMlft=WIDGET_BASE(bHomogNM,/COLUMN)
  bHomogSizeNM=WIDGET_BASE(bHomogNMlft, /ROW)
  lblHomogROIszNM = WIDGET_LABEL(bHomogSizeNM, VALUE='ROI radius (mm)',FONT=font1)
  txtHomogROIszNM = WIDGET_TEXT(bHomogSizeNM, VALUE=STRING(config.HomogROIszNM,FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  bHomogDistNM=WIDGET_BASE(bHomogNMlft, /ROW)
  lblHomogROIdistNM = WIDGET_LABEL(bHomogDistNM, VALUE='ROI distance x, y (mm)',FONT=font1)
  txtHomogROIdistXNM = WIDGET_TEXT(bHomogDistNM, VALUE=STRING(config.HomogROIdistNM(0),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  txtHomogROIdistYNM = WIDGET_TEXT(bHomogDistNM, VALUE=STRING(config.HomogROIdistNM(1),FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  cw_homogNM=CW_BGROUP(bHomogNMlft, ['Planar (WB)', 'SPECT'], /EXCLUSIVE, LABEL_TOP='Image type...', /FRAME, SET_VALUE=0, UVALUE='cw_homogNM',FONT=font1)
  bHomogBtnsNM=WIDGET_BASE(bHomogNMlft, /ROW)
  ;btnHomogROINM=WIDGET_BUTTON(bHomogBtnsNM, VALUE='Show/update ROI', UVALUE='drawROIhomog',FONT=font1)
  btnHomogNM=WIDGET_BUTTON(bHomogBtnsNM, VALUE='Calculate uniformity', UVALUE='homog',FONT=font1)

  ;-----------Scan speed------------
    lblssMl0=WIDGET_LABEL(bScanSpeed, VALUE='', SCR_YSIZE=20)
  bAvgSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lblAvgSpeedNM=WIDGET_LABEL(bAvgSpeedNM, VALUE='Average over ROI with width (pix)' ,FONT=font1)
  txtNAvgSpeedNM=WIDGET_TEXT(bAvgSpeedNM, VALUE=STRING(config.scanSpeedAvg, FORMAT='(i0)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bSpeedROIheight=WIDGET_BASE(bScanSpeed, /ROW)
  lblSpeedROIheight=WIDGET_LABEL(bSpeedROIheight, VALUE='ROI heigth (cm)' ,FONT=font1)
  txtSpeedROIheight=WIDGET_TEXT(bSpeedROIheight, VALUE=STRING(config.scanSpeedHeight, FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bMedianSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lblSpeedMedian=WIDGET_LABEL(bMedianSpeedNM, VALUE='Median filter width (pix)',FONT=font1)
  txtScanSpeedMedian=WIDGET_TEXT(bMedianSpeedNM, VALUE=STRING(config.scanSpeedFiltW, FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  btnPlotScanSpeed = WIDGET_BUTTON(bScanSpeed, VALUE='Plot y-profile and median filtered profile', UVALUE='plotScanSpeed',FONT=font1)

  ;-------------Contrast-------------
    lblcnmMl0=WIDGET_LABEL(bContrastNM, VALUE='', SCR_YSIZE=20)
  bConSettingsNM=WIDGET_BASE(bContrastNM, /ROW)
  lblConR1NM = WIDGET_LABEL(bConSettingsNM, VALUE='ROI radius (mm)', FONT=font1)
  txtConR1NM = WIDGET_TEXT(bConSettingsNM, VALUE=STRING(config.contrastRad1,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  lblMlConRNM=WIDGET_LABEL(bConSettingsNM, VALUE='', XSIZE=20, FONT=font1)
  lblConR2NM = WIDGET_LABEL(bConSettingsNM, VALUE='Radius to ROIs (mm)', FONT=font1)
  txtConR2NM = WIDGET_TEXT(bConSettingsNM, VALUE=STRING(config.contrastRad2,FORMAT='(f0.1)'), /EDITABLE, XSIZE=5, SCR_YSIZE=20, FONT=font1)
  bConButtonsNM=WIDGET_BASE(bContrastNM, /ROW)
  ;btnConRoisNM=WIDGET_BUTTON(bConButtonsNM, VALUE='Show/update ROIs', UVALUE='drawConRoisNM',FONT=font1)
  btnContrastNM=WIDGET_BUTTON(bConButtonsNM, VALUE='Calculate contrast', UVALUE='contrastNM',FONT=font1)

  ;----------------MTF------------------
  bMTFsettingsNM=WIDGET_BASE(bMTFNM, /ROW)
  bMTFlftNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)
  cw_typeMTFNM=CW_BGROUP(bMTFlftNM, ['Point','Line','Edge','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME, SET_VALUE=config.MTFtypeNM,FONT=font1)
  
  bMTFrgtNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)
  
  bMTFroiSzNM=WIDGET_BASE(bMTFrgtNM, /ROW)
  lblMTFroiSzXNM=WIDGET_LABEL(bMTFroiSzNM, VALUE='ROI width x height (mm)',FONT=font1)
  txtMTFroiSzXNM=WIDGET_TEXT(bMTFroiSzNM, VALUE=STRING(config.MTFroiSzNM(0),FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  lblMTFXNM=WIDGET_LABEL(bMTFroiSzNM, VALUE='x',FONT=font1)
  txtMTFroiSzYNM=WIDGET_TEXT(bMTFroiSzNM, VALUE=STRING(config.MTFroiSzNM(1),FORMAT='(f0.1)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  
  bCutLSFNM=WIDGET_BASE(bMTFrgtNM, /NONEXCLUSIVE, /ROW)
  btnCutLSFNM=WIDGET_BUTTON(bCutLSFNM, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFWNM=WIDGET_BASE(bMTFrgtNM, /ROW)
  lblCutLSFWNM=WIDGET_LABEL( bCutLSFWNM, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1)
  txtCutLSFWNM=WIDGET_TEXT( bCutLSFWNM, VALUE='5', /EDITABLE, XSIZE=3, SCR_YSIZE=20, FONT=font1)
  lblmlSett123=WIDGET_LABEL(bMTFrgtNM, VALUE='', YSIZE=20)
  bMTF3dNM=WIDGET_BASE(bMTFrgtNM, /COLUMN, /NONEXCLUSIVE)
  MTF3dNM=WIDGET_BUTTON(bMTF3dNM, VALUE='Analyse 3d', UVALUE='MTF3dNM',FONT=font1)

  cw_plotMTFNM=CW_BGROUP(bMTFsettingsNM, ['Centered xy profiles', 'Line', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, SET_VALUE=config.plotMTFNM, UVALUE='cw_plotMTFNM',FONT=font1)

  bMTFbtnsNM=WIDGET_BASE(bMTFNM, /ROW)
  ;btnMTFroiNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Show/update ROIs', UVALUE='drawMTFroi',FONT=font1)
  btnMTFNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Calculate MTF', UVALUE='MTFNM',FONT=font1)

  ;----------Radial profiles---------------
    lblrpMl0=WIDGET_LABEL(bRadialProfile, VALUE='', SCR_YSIZE=20)
  bRadialProf=WIDGET_BASE(bRadialProfile, /COLUMN)
  bMedianRadialNM=WIDGET_BASE(bRadialProf, /ROW)
  lblRadialMedian=WIDGET_LABEL(bMedianRadialNM, VALUE='Median filter width (pix)',FONT=font1)
  txtRadialMedian=WIDGET_TEXT(bMedianRadialNM, VALUE=STRING(5, FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1);config.radialFiltW
  btnRadialProfNM=WIDGET_BUTTON(bRadialProf, VALUE='Calculate radial profile', UVALUE='radialProfile',FONT=font1)

  ;***********************PET tests**********************************************************

  ;analyseStringsPET=['NONE','CROSSCALIB','HOMOG','CONTRAST','MTF']
  bCross=WIDGET_BASE(wtabAnalysisPET, TITLE='Crosscalibration', /COLUMN)
  bHomogPET=WIDGET_BASE(wtabAnalysisPET, TITLE='Uniformity', /COLUMN)
  bRC=WIDGET_BASE(wtabAnalysisPET, TITLE='RC', /COLUMN)

  ;------------Crosscalibration--------------------
  bCrossROI=WIDGET_BASE(bCross, /ROW)
  lblCrossROIsz = WIDGET_LABEL(bCrossROI, VALUE='ROI radius (mm)',FONT=font1)
  txtCrossROIsz = WIDGET_TEXT(bCrossROI, VALUE= ( TOTAL(WHERE(configTags EQ 'CROSSROISZ')) NE -1 ? STRING(config.CROSSROISZ,FORMAT='(f0.1)') : STRING(configDefault.CROSSROISZ,FORMAT='(f0.1)') ) , /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)

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
  txtCrossVol = WIDGET_TEXT(bCrossVol, VALUE=( TOTAL(WHERE(configTags EQ 'CROSSVOL')) NE -1 ? STRING(config.CROSSVOL,FORMAT='(f0.1)') : STRING(configDefault.CROSSVOL,FORMAT='(f0.1)') ) ,/EDITABLE,  XSIZE=10, SCR_YSIZE=20, FONT=font1)
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
  txtHomogROIszPET = WIDGET_TEXT(bHomogSizePET, VALUE=( TOTAL(WHERE(configTags EQ 'HOMOGROISZPET')) NE -1 ? STRING(config.HOMOGROISZPET,FORMAT='(f0.1)') : STRING(configDefault.HOMOGROISZPET,FORMAT='(f0.1)') ), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
  mlH11=WIDGET_LABEL(bHomogSizePET, VALUE='', XSIZE=20, FONT=font1)
  lblHomogROIdistPET = WIDGET_LABEL(bHomogSizePET, VALUE='Radius to ROIs (mm)',FONT=font1)
  txtHomogROIdistPET = WIDGET_TEXT(bHomogSizePET, VALUE=( TOTAL(WHERE(configTags EQ 'HOMOGROIDISTPET')) NE -1 ? STRING(config.HOMOGROIDISTPET,FORMAT='(f0.1)') : STRING(configDefault.HOMOGROIDISTPET,FORMAT='(f0.1)') ), /EDITABLE, XSIZE=4, SCR_YSIZE=20, FONT=font1)
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

  loadct, 0, /SILENT
  WIDGET_CONTROL, bMain, /REALIZE
  XMANAGER, 'ImageQC', bMain, /NO_BLOCK
  DEVICE, RETAIN=2, DECOMPOSED=0
  
  WIDGET_CONTROL, drawLarge, GET_VALUE=iDrawPlot
  iDrawPlot.erase

end


