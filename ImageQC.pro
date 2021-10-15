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
;You should have received a copy of the GNU General Pugblic License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;main window GUI
pro ImageQC,  GROUP_LEADER=bMain

  COMPILE_OPT hidden
  COMMON VARI,  $
    evTop, currVersion, saveOK, configPath, tempPath, lblDir, btnAppend, switchMode, listFiles,ctmActions, lastList, lblLoadedN,lstShowFile, lblProgress0, lblProgress, lblSettings, selConfig, activeImg, activeResImg, ROIs, newOrder, currSortElem, currAscElem, $
    marked, markedMulti, autoStopFlag,multiOpt, testVisualQTNames, currentHeaderAlt, btnUseMulti, listSelMultiTemp, multiExpTable, $
    txtActive1, txtActive2, newline, pathsep, tab, xoffset, yoffset, font0, font1, fontMini,$
    drawLarge, drawXY, listFilesYsize, coltable, btnSetColorTable, txtMinWL, txtMaxWL, txtCenterWL, txtWidthWL, lblCursorValue, lblCursorPos,lblCursorPosMM, $
    zoomSlider, zoomFactor, txtDeltaX, txtDeltaY, txtDeltaA, dxya, useDelta, btnHideAnnot,$
    defPath, thisPath, structImgs, imgStructInfo,configSinfo, deciMark, listDeciMark, copyHeader, btnCopyHeader, transposeTable, btnTranspose, headers, tableHeaders,lastXY, lastXYreleased, mouseDown, lastXYright, mouseDownRight, $
    useMulti, btnIncFilename, modality, modalityName, analyse, analyseStringsAll, analyseStringsDCM, results, $
    resTab, resTabSup, wtabResult, lblResultsSup, wtabModes,wtabAnalysisCT,wtabAnalysisXray,wtabAnalysisNM,wtabAnalysisSPECT, wtabAnalysisPET,wtabAnalysisMR, $
    drawPlot, statPlot, toolHintPlot, drawImageRes, txtMinRangeX, txtMaxRangeX, txtMinRangeY, txtMaxRangeY, rangeAcc, $
    MTFres, offxyMTF, offxyMTF_X, lblDeltaO, lblDeltaOX, unitDeltaO_MTF_CT,unitDeltaO_MTF_X,$
    cw_typeMTF, cw_plotMTF, cw_tableMTF, cw_cyclMTF, txtMTFroiSz, btnCutLSF, txtcutLSFW, txtcutLSFW2,txtfreqMTF, btnSearchMaxMTF, $
    cw_formLSFX, cw_plotMTFX,  cw_tableMTFX, txtMTFroiSzX, txtMTFroiSzY, btnCutLSFX, txtcutLSFWX, $
    cw_typeMTFNM, cw_plotMTFNM, txtMTFroiSzXNM, txtMTFroiSzYNM, btnCutLSFNM, txtcutLSFWNM, $
    cw_typeMTFSPECT, cw_plotMTFSPECT, txtMTFroiSzSPECT, btnCutLSFSPECT, txtcutLSFWSPECT, MTF3dSPECT,$
    CTlinRes, CTlinROIs, CTlinROIpos, txtLinROIrad, txtLinROIradS, tblLin, linTabEdit, btnLinAvoidSearch, $
    HUwaterRes, HUwaterROI, txtHUwaterROIsz, $
    sliceThickRes, sliceThickResTab,  ramps, txtRampDist, txtRampLen, txtRampBackG, txtRampSearch, txtRampAverage, cw_ramptype, cw_rampDens,  $
    homogRes, homogROIs, txtHomogROIsz, txtHomogROIszPET, txtHomogROIszX, cw_HomogAltX, txtHomogROIdist, txtHomogROIdistPET, $
    noiseRes, noiseROI, txtNoiseROIsz, txtNoiseX ,$
    ROIres, ROIroi, typeROI, txtROIrad, txtROIx, txtROIy, txtROIa, offxyROI,lblDeltaO_ROI, unitDeltaO_ROI_CT,$
    typeROIX, txtROIXrad, txtROIXx, txtROIXy, txtROIXa, offxyROIX,lblDeltaO_ROIX, unitDeltaO_ROI_X,$
    typeROIMR, txtROIMRrad, txtROIMRx, txtROIMRy, txtROIMRa, offxyROIMR,lblDeltaO_ROIMR, unitDeltaO_ROI_MR,$
    fwhmRes, dimRes, energyRes, expRes,$
    stpRes, txtStpROIsz, stpROI, txtRQA, Qvals, $
    NPSres, NPSrois, $
    txtNPSroiSz, txtNPSroiDist, txtNPSsubNN, txtSmoothNPS, txtfreqNPS, btnNPSavg, $
    txtNPSroiSzX, txtNPSsubSzX, lblNPSsubSzMMX, lblNPStotpixX, $
    txtNAvgSpeedNM, txtScanSpeedMedian, txtSpeedROIheight,$
    timeActROI,$
    varImgRes,txtVarImageROIsz, lblProgressVarX, $
    contrastRes, conROIs, txtConR1SPECT, txtConR2SPECT,$
    radialRes, txtRadialMedian, $
    unifRes, unifROI, txtUnifAreaRatio, btnUnifCorr,cw_plotUnif,cw_imgUnif, cw_posfitUnifCorr, btnLockRadUnifCorr,txtLockRadUnifCorr,$
    SNIres, SNIsupTab, SNIroi, txtSNIAreaRatio, btnSNICorr, cw_posfitSNICorr,btnLockRadSNICorr,txtLockRadSNICorr, txtSNI_f, txtSNI_c,txtSNI_d,cw_plotSNI,cw_imgSNI, txtSmoothNPS_SNI, txtfreqNPS_SNI,$
    acqRes, barRes, barROI, txtBarROIsize,txtBar1,txtBar2,txtBar3,txtBar4,$
    crossRes, crossROI, txtCrossROIsz, txtCrossMeasAct,txtCrossMeasActT, txtCrossMeasRest, txtCrossMeasRT, txtCrossScanAct, txtCrossScanStart,$
    txtCrossVol, txtCrossConc, txtCrossFactorPrev, txtCrossFactor,$
    rcRes, rcROIs, btnRCrev, cwRCexclude, cw_rcType,$
    SNRres, SNR_ROI, txtSNR_MR_ROI, PIUres, PIU_ROI, txtPIU_MR_ROI, GeomDistRes, txtGD_MR_act,$
    ghostMRres, ghostMR_ROI, ghost_MR_optC, txtGhost_MR_ROIszC, txtGhost_MR_ROIszW, txtGhost_MR_ROIszH, txtGhost_MR_ROIszD, $
    sliceMR_ROI, slice_MR_optC, txtSlice_MR_TANA, txtSlice_MR_ROIszW, txtSlice_MR_ROIszH, txtSlice_MR_ROIszD, txtSlice_MR_ROIszD2

  !EXCEPT=0;2 to see all errors
  currVersion='2.01'
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
  xoffset=100
  yoffset=50
  retainVal=0

  if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
  if (!D.NAME eq 'WIN') then pathsep = '\' else pathsep = '/'
  tab=STRING(9B)

  analyseStringsCT=['HOMOG', 'NOISE', 'SLICETHICK', 'MTF', 'CTLIN', 'HUWATER', 'EXP','ROI','NPS','DIM', 'FWHM']
  analyseStringsXray=['STP','HOMOG','NOISE','EXP','MTF','ROI','NPS','VARI']
  analyseStringsNM=['UNIF','SNI','ACQ','BAR', 'ENERGYSPEC','SCANSPEED','MTF','GEOMMEAN'];,'TIMEACTCURVE']
  analyseStringsSPECT=['MTF','RADIAL','CONTRAST']
  analyseStringsPET=['HOMOG','CROSSCALIB','RC']
  analyseStringsMR=['DCM','SNR','PIU','GHOST','GEOMDIST','SLICETHICK','ROI']
  analyseStringsAll=CREATE_STRUCT('CT',analyseStringsCT,'Xray',analyseStringsXray,'NM',analyseStringsNM,'SPECT',analyseStringsSPECT,'PET',analyseStringsPET,'MR',analyseStringsMR)
  analyseStringsDCM=CREATE_STRUCT('CT','EXP','Xray','EXP','NM','ACQ','SPECT','','PET','','MR','DCM')

  ;options regarding QuickTest
  multiOpt=CREATE_STRUCT('CT',[1,2,3,4,5,6,7,8,0,0],'Xray',[1,2,3,4,5,6,0,0],'NM',[1,2,3,4,0,0,0],'SPECT', INTARR(3),'PET',[1,0,0],'MR',[1,2,3,4,5,6,7]); structure of arrays corresponding to analyseStrings that have the option of being a numbered test for multimark/quicktest, number or 0 if not an option

  saveOK=1; 0=blocked by other user, 1=ok, -1= blocked by writing permissions
  configPath=''
  userinfo=get_login_info()
  tempPath='C:\Users\'+userinfo.user_name+'\Documents\';'C:\temp\'
  tempPathLocateConfig=tempPath+'configImageQC_Path.txt'

  ;assume config.dat is in ImageQC/data folder. If not found there - look for configImageQC_Path.txt in C:\temp - else ask to locate or create new
  ;for testing: IF thisPath EQ 'rubbish' THEN configPath='rubbish' ELSE BEGIN;
  IF FILE_TEST(thisPath+'data\config.dat') THEN configPath=thisPath+'data\config.dat' ELSE BEGIN

    fi=FILE_INFO(tempPathLocateConfig)
    IF fi.exists THEN BEGIN
      ;read adr
      OPENR, filenhet, tempPathLocateConfig, /GET_LUN
      strPath=''
      READF, filenhet, strPath
      CLOSE, filenhet
      FREE_LUN, filenhet
      IF FILE_TEST(strPath) THEN configPath=strPath
    ENDIF ELSE BEGIN
      fi=FILE_INFO(tempPath)
      IF fi.write THEN BEGIN
        box=[$
          '1, BASE,, /COLUMN', $
          '0, LABEL, ', $
          '0, LABEL, ', $
          '0, LABEL, * Welcome to ImageQC v'+currVersion+' *,FONT=Tahoma*ITALIC*20', $
          '0, LABEL, ------------------------------------------', $
          '0, LABEL, ', $
          '0, LABEL, New to ImageQC? Initiate a new config file.,FONT=Tahoma*16', $
          '0, LABEL, ', $
          '0, LABEL, If you already have a config file for ImageQC you could,FONT=Tahoma*16', $
          '0, LABEL, restore that config file i.e. save it to ImageQC\\data\\config.dat.,FONT=Tahoma*16', $
          '0, LABEL, Or you may locate the config file saving the config path information to,FONT=Tahoma*16', $
          '0, LABEL, '+STRJOIN(STRSPLIT(tempPathLocateConfig,/EXTRACT,'\'),'\\')+',FONT=Tahoma*16', $
          '0, LABEL, Locate is helpful if you do not have writing permission to ImageQC\\data,FONT=Tahoma*16', $
          '0, LABEL, ', $
          '2, LABEL, ', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Initiate new config file, QUIT, TAG=New',$
          '0, BUTTON, Restore config file, QUIT, TAG=Restore',$
          '2, BUTTON, Locate config file, QUIT, TAG=Locate']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Welcome to ImageQC', XSIZE=435, YSIZE=300, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        restoreFailed=0
        adr=''
        IF res.Restore THEN BEGIN
          adr=DIALOG_PICKFILE(TITLE='Restore config file from', /READ, FILTER='*.dat', /FIX_FILTER, PATH='C:\')
          IF adr NE '' THEN BEGIN
            configPath=thisPath+'data\config.dat'
            fi=FILE_INFO(thisPath+'data\')
            IF fi.write THEN BEGIN
              ;copy file from adr
              IF adr NE configPath THEN BEGIN
                fi=FILE_INFO(configPath)
                IF fi.exists THEN BEGIN
                  sv=DIALOG_MESSAGE('A config file already exist (ImageQC\data\config.dat). Overwrite?',/QUESTION)
                  IF sv EQ 'Yes' THEN FILE_COPY, adr, configPath, /OVERWRITE
                ENDIF ELSE FILE_COPY, adr, configPath
              ENDIF             
            ENDIF ELSE restoreFailed=1
          ENDIF
        ENDIF; restore

        IF res.Locate OR restoreFailed THEN BEGIN
          IF res.Locate THEN adr=DIALOG_PICKFILE(TITLE='Locate config file', /READ, FILTER='*.dat', /FIX_FILTER, PATH='C:\')
          IF adr NE '' THEN BEGIN
            OPENW, pathfile, tempPathLocateConfig, /GET_LUN
            PRINTF, pathfile, adr
            CLOSE, pathfile & FREE_LUN, pathfile
            configPath=adr
            msg='When you open ImageQC next time it will find the location of the config file you specified in this text file: '+newline+tempPathLocateConfig
            IF restoreFailed THEN msg='No writing permission to ImageQC\data'+newline+msg
            sv=DIALOG_MESSAGE(msg)
          ENDIF
        ENDIF; locate
        
      ENDIF; Else no write permission, continue to configPath=''
    ENDELSE;fi.exist tempPath
  ENDELSE;data\config.dat
  
  IF configPath NE '' THEN BEGIN
    fi=FILE_INFO(configPath)
    IF fi.write EQ 0 THEN BEGIN
      sv=DIALOG_MESSAGE('You do not have permission to write to the selected config file ('+configPath+'). Saving will be blocked.')
      saveOK=-1
    ENDIF

    RESTORE, configPath

    ;update functions in a0_functionsMini.pro
    configS=updateConfigS(configPath)
    quickTemp=updateQuickT(configPath, multiOpt, TEMPPA=tempPath)
    IF N_ELEMENTS(quickTemp) EQ 0 THEN quickTemp=0
    loadTemp=updateLoadT(configPath, multiOpt, TEMPPA=tempPath)
    quickTout=updateQuickTout(configPath,analyseStringsAll)
    renameTemp=updateRenameTemp(configPath)

    IF configS.(0).SAVEBLOCKED AND saveOK NE -1 THEN BEGIN

      saveOK=0
      IF configS.(0).AUTOUNBLOCK GT 0 THEN BEGIN
        IF SIZE(configS.(0).SAVESTAMP, /TNAME) EQ 'DOUBLE' THEN BEGIN; probably not necessary to check, but...
          hoursSinceSaved=(1./60./60.)*(systime(/SECONDS)-configS.(0).SAVESTAMP)
          IF hoursSinceSaved GT configS.(0).AUTOUNBLOCK THEN saveOK=1
        ENDIF
      ENDIF
      IF saveOK EQ 0 THEN BEGIN
        msgString='Saving is blocked by another session. '
        IF configS.(0).USERNAME NE '' THEN msgString=msgString+newline+'Username: '+configS.(0).USERNAME
        IF SIZE(configS.(0).SAVESTAMP,/TNAME) EQ 'DOUBLE' THEN BEGIN
          msgString=msgString+', '+STRING((systime(/SECONDS)-configS.(0).SAVESTAMP)/60/60,FORMAT='(i0)')+' hours ago'
        ENDIF
        sv=DIALOG_MESSAGE(msgString+newline+'If this blocking is due to a software crash the blocking can be removed in Settings - find button <Reset timestamp>')
      ENDIF
    ENDIF

    IF saveOK EQ 1 THEN BEGIN
      configS.(0).SAVEBLOCKED=1;blocked for next user before this user have closed the session
      IF N_TAGS(configS.(0)) GT 2 THEN BEGIN

        configS.(0).USERNAME=userinfo.user_name
        configS.(0).SAVESTAMP=systime(/SECONDS)
      ENDIF
    ENDIF
  ENDIF ELSE BEGIN
    
    configS=updateConfigS('')
    quickTemp=updateQuickT('', TEMPPA=tempPath)
    loadTemp=updateLoadT('', TEMPPA=tempPath)
    quickTout=updateQuickTout('')
    renameTemp=updateRenameTemp('')

    fi=FILE_INFO(thisPath+'data\')
    ;for testing; IF thisPath EQ 'rubbish' THEN print, 'rubbish' ELSE BEGIN; 
    IF fi.write THEN configPath=thisPath+'data\config.dat' ELSE BEGIN
      fi=FILE_INFO(tempPath)
      IF fi.write THEN BEGIN
        configPath=tempPath+'config.dat'
        OPENW, pathfile, tempPathLocateConfig, /GET_LUN
        PRINTF, pathfile, configPath
        CLOSE, pathfile & FREE_LUN, pathfile
      ENDIF ELSE BEGIN
        sv=DIALOG_MESSAGE('No writing permissions to ..\ImageQC\data\ nor '+tempPath+'. The new config file can be saved and used in the current session.'+newline+'To use the edited config file in a later session go to settings and restore this config file for each session.',/INFORMATION)
        adr=DIALOG_PICKFILE(TITLE='Save config file', /WRITE, FILTER='*.dat', /FIX_FILTER, /OVERWRITE_PROMPT, DEFAULT_EXTENSION='.dat', PATH='C:\')
        IF adr NE '' THEN configPath=adr ; ELSE will probably crash
      ENDELSE
    ENDELSE
  ENDELSE

  IF saveOK EQ 1 THEN BEGIN
    configS.(0).saveStamp=SYSTIME(/SECONDS)
    SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=configPath
  ENDIF

  selConfig=configS.(0).defConfigNo
  config=configS.(selConfig)

  configTags=TAG_NAMES(config)
  defPath='C:\'

  testVisualQTNames=CREATE_STRUCT($
    'CT',['1. Homogeneity','2. Noise','3. Slice thickness','4. MTF','5. CT Number', '6. HU water','7. Header info','8. ROI'],$
    'Xray',['1. STP','2. Homogeneity','3. Noise','4. Header info','5. MTF','6. ROI'],$
    'NM',['1. Uniformity','2. SNI','3. Header info','4. BarPhantom'],$
    'PET', ['1. Uniformity'],$
    'MR',['1. Header info','2. SNR','3. PIU','4. Ghosting','5. GeomDist','6. Slice thickness','7. ROI'])
  
  CT_headers=CREATE_STRUCT($
    'HOMOG',CREATE_STRUCT('Alt1',['12oClock','15oClock','18oClock','21oClock','Center HU','Diff C 12','Diff C 15','Diff C 18','Diff C 21']),$
    'NOISE',CREATE_STRUCT('Alt1',['CT number (HU)','Noise=Stdev (HU)','Diff avg noise(%)', 'Avg noise (HU)']),$
    'SLICETHICK',CREATE_STRUCT($
    'WIRE_RAMP',['Nominal','H1','H2','V1','V2','Avg','Diff nominal (%)'],$
    'BEADED_RAMP',['Nominal','H1','H2','V1','V2','inner V1','inner V2'],$
    'VERTICAL_BEADED_RAMP',['Nominal','V1','V2']),$
    'MTF',CREATE_STRUCT($
    'BEAD_XY',['MTFx 50%','MTFx 10%','MTFx 2%','MTFy 50%','MTFy 10%','MTFy 2%'],$
    'WIRE_OR_CIRCULAR_EDGE',['MTF 50%','MTF 10%','MTF 2%']),$
    'CTLIN', CREATE_STRUCT('Alt1',TRANSPOSE(config.LINTAB.Materials)),$
    'HUWATER', CREATE_STRUCT('Alt1',['CT number (HU)','Noise=Stdev']),$
    'EXP',CREATE_STRUCT('Alt1',['kVp','mAs','CTDIvol','Software']),$
    'ROI',CREATE_STRUCT('Alt1',['CT number (HU)','Stdev (HU)']))
  ;updated when materialtable is updated or parameterset is updated (function updateMaterialHeaders, currTableHeaders, newMaterialHeaders)
  Xray_headers=CREATE_STRUCT($
    'STP',CREATE_STRUCT('Alt1',['Dose','Q','Mean pix','Stdev pix']),$
    'HOMOG',CREATE_STRUCT('Alt1',['Center','UpperLeft','LowerLeft','UpperRight','LowerRight','Std center', 'Std UL','Std LL','Std UR','Std LR'],$
    'Alt2',['Center','UpperLeft','LowerLeft','UpperRight','LowerRight','C - avg', 'UL - avg','LL - avg','UR - avg','LR - avg'],$
    'Alt3',['Center','UpperLeft','LowerLeft','UpperRight','LowerRight','C-avg %', 'UL-avg %','LL-avg %','UR-avg %','LR-avg %']),$
    'NOISE',CREATE_STRUCT('Alt1',['Mean pixel value','Stdev']),$
    'EXP',CREATE_STRUCT('Alt1',['kVp','mAs','EI','DAP','SDD','DetID']),$
    'MTF',CREATE_STRUCT('Alt1',['MTF @ 0.5/mm','MTF @ 1.0/mm','MTF @ 1.5/mm','MTF @ 2.0/mm','MTF @ 2.5/mm','Freq @ MTF 0.5']),$
    'ROI',CREATE_STRUCT('Alt1',['Pixel mean','Pixel stdev']))
  NM_headers=CREATE_STRUCT($
    'UNIF',CREATE_STRUCT('Alt1',['IU_UFOV %', 'DU_UFOV %', 'IU_CFOV %', 'DU_CFOV %'],$
    'AltSup',['FitX (mm from center)','FitY (mm from center)','Fit distance (mm)']),$
    'SNI',CREATE_STRUCT('Alt1',['SNI max','SNI L1','SNI L2','SNI S1','SNI S2','SNI S3','SNI S4','SNI S5','SNI S6'],$
    'AltSup',['FitX (mm from center)','FitY (mm from center)','Fit distance (mm)']),$
    'ACQ',CREATE_STRUCT('Alt1',['Total Count','Frame Duration (ms)']),$
    'BAR',CREATE_STRUCT('Alt1',['MTF @ F1','MTF @ F2','MTF @ F3','MTF @ F4','FWHM1','FWHM2','FWHM3','FWHM4']))
  PET_headers=CREATE_STRUCT($
    'HOMOG',CREATE_STRUCT('Alt1',['Center','12oClock','15oClock','18oClock','21oClock','dMean C','dMean 12','dMean 15','dMean 18','dMean 21']))
  MR_headers=CREATE_STRUCT($
    'DCM', CREATE_STRUCT('Alt1',['Imaging frequency','Receive coil','Transmit coil']),$
    'SNR', CREATE_STRUCT('Alt1',['S img 1','S img 2','S mean','stdev diff','SNR']),$
    'PIU', CREATE_STRUCT('Alt1',['min','max','PIU'],'AltSup',['x min (pix from lower left)','y min','x max', 'y max']),$
    'GHOST', CREATE_STRUCT('Alt1',['Center','top','bottom','left','right','PSG']),$
    'GEOMDIST', CREATE_STRUCT('Alt1',['width_0','width_90','width_45','width_135','GD_0','GD_90','GD_45','GD_135']),$
    'SLICETHICK', CREATE_STRUCT('Alt1',['Nominal (mm)','Measured (mm)','Diff (mm)','Diff (%)'],'AltSup',['FWHMupper (mm)','FWHMlower (mm)']),$
    'ROI', CREATE_STRUCT('Alt1',['Pixel mean','Pixel stdev']))

  tableHeaders=CREATE_STRUCT('CT',CT_headers,'Xray',Xray_headers,'NM',NM_headers,'PET',PET_headers,'MR', MR_headers)
  CT_headers=!Null & Xray_headers=!Null & NM_headers=!Null & MR_headers=!Null
  currentHeaderAlt=INTARR(9); Alt1=0, Alt2=1... set in updateTable pr test, altsup always one option and last option

  structImgs=CREATE_STRUCT('empty',0); images with attributes in memory
  activeImg=0 ; active (selected image)
  activeResImg=0 ; result-image (fx 2d NPS)

  analyse=analyseStringsAll.(0)(0)
  modality=0; to save current modality for regretting switch and lose results
  modNames=TAG_NAMES(multiOpt)
  modalityName=modNames(modality)
  marked=-1; indexes of marked files (-1 = all marked)
  markedMulti=-1; matrix of marked images for numbered tests (number of tests x number of images) -1 if useMuliMark is not set
  multiExpTable=-1
  results=INTARR(10); set to 1 when analyse performed and results is available, keep analyseString and tab-order equal
  dxya=[0,0,0.0,1]; [deltax,deltay,delta,show] for positioning of center/angle. Difference from imgSz/2 in pixels. Show (last param) used for redrawCT to overplot crosshair
  IF TOTAL(WHERE(configTags EQ 'OFFXYMTF')) NE -1 THEN offxyMTF=config.OFFXYMTF ELSE offxyMTF=[0,0]
  IF TOTAL(WHERE(configTags EQ 'OFFXYMTF_X')) NE -1 THEN offxyMTF_X=config.OFFXYMTF_X ELSE offxyMTF_X=[0,0]
  IF TOTAL(WHERE(configTags EQ 'OFFXYROI')) NE -1 THEN offxyROI=config.OFFXYROI ELSE offxyROI=[0,0]
  IF TOTAL(WHERE(configTags EQ 'OFFXYROIX')) NE -1 THEN offxyROIX=config.OFFXYROIX ELSE offxyROIX=[0,0]
  IF TOTAL(WHERE(configTags EQ 'OFFXYROIMR')) NE -1 THEN offxyROIMR=config.OFFXYROIMR ELSE offxyROIMR=[0,0]
  CTlinROIs=0 & CTlinROIpos=0 & homogROIs=0 & noiseROI=0 & NPSrois=0 & conROIs=0 & crossROI=0; used to hold the rois for specific tests
  ramps=0; used to hold the 4 lines for slice thickness H-top,H-bottom,V1,V2
  lastXY=[-1,-1] & lastXYright=[-1,-1]; last mouseposition (left/right mousebutton) in draw window
  lastXYreleased=[-1,-1,-1]; x, y, time
  lastList=[-1,-1]; last first selected, to control clicks in fileList
  mouseDown=0 & mouseDownRight=0; If mouse pressed in draw window and still not released 1=true
  coltable=0;  grayscale default
  deciMark=config.deciMark
  copyHeader=config.copyHeader

  ;tags in imgStruct that is not string or are specific to one modality [tag, typestring, modalitynumbers separated by / or empty for all
  set_imgStructInfo, imgStructInfo;in set_Values.pro

  ;tags in configS with explanations for settings.pro [tagname, description, modality (0-4) or output '-1' other '-2', not list in settings like others'-10'' analysestring or '','BOOL/INT/FLOAT/STRING']
  set_configSinfo, configSinfo;in set_Values.pro

  font0="Tahoma*ITALIC*16"
  font1="Tahoma*14"
  fontMini="Tahoma*12"

  winX=1500 &  winY=1100
  drawXY=500
  listFilesYsize=130

  s=obj_new('idlsysmonitorinfo')
  nMon=s->GetNumberOfMonitors()
  pMon=s->GetPrimaryMonitorIndex(); wrong monitor index.... when first is second...
  rMon=s->GetRectangles()
  scsz=rMon[2:3,pMon]

  IF scsz(0) LT winX THEN scX=scsz(0)-150 ELSE scX=winX-100
  IF scsz(1) LT winY-100 THEN scY=scsz(1)-150 ELSE scY=winY-150


  bMain = WIDGET_BASE(TITLE='ImageQC v'+currVersion, MBAR=bar, /COLUMN, XSIZE=winX, YSIZE=winY-60, XOFFSET=xoffset, YOFFSET=yoffset, X_SCROLL_SIZE=scX, Y_SCROLL_SIZE=scY, /TLB_KILL_REQUEST_EVENTS, /TLB_MOVE_EVENTS)
  bLarge = WIDGET_BASE(bMain, /ROW)
  bLft = WIDGET_BASE(bLarge, XSIZE=700, YSIZE=winY-190,/COLUMN)
  bRgt = WIDGET_BASE(bLarge, XSIZE=675, YSIZE=winY-190,/COLUMN)

  ;*****************MENU
  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  sett_menu=WIDGET_BUTTON(bar, VALUE='Settings',/MENU)
  help_menu=WIDGET_BUTTON(bar, VALUe='Help', /MENU)
  ;file_menu
  btn=WIDGET_BUTTON(file_menu, VALUE='Open DICOM file(s)', UVALUE='open', ACCELERATOR='Ctrl+O',/NO_COPY)
  btn=WIDGET_BUTTON(file_menu, VALUE='Open DICOM file(s) with advanced options', UVALUE='openMulti',/NO_COPY)
  btn=WIDGET_BUTTON(file_menu, VALUE='Open files using automation template', UVALUE='openAuto',/NO_COPY)
  btn=WIDGET_BUTTON(file_menu, VALUE='Read image (and header) from txt file..', UVALUE='openTxt', /SEPARATOR,/NO_COPY)
  btnSaveStruct=WIDGET_BUTTON(file_menu, VALUE='Save selected image as IDL structure (.dat-file)', UVALUE='saveDat')
  btnSelAll=WIDGET_BUTTON(file_menu, VALUE='Select all images in list', UVALUE='selectAll', ACCELERATOR='Ctrl+A', /SEPARATOR)
  btnOpenMTFNPS=WIDGET_BUTTON(file_menu, VALUE='Run MTF/NPS auto-analyse program', UVALUE='runAutoMTFNPS')
  btnRenameDICOM=WIDGET_BUTTON(file_menu, VALUE='Run RenameDICOM', UVALUE='runRenameDICOM')
  btnClose=WIDGET_BUTTON(file_menu, VALUE='Close all images', UVALUE='close', /SEPARATOR)
  btnExit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='exit', ACCELERATOR='Ctrl+X')
  ;sett_menu
  btnEditParamSets=WIDGET_BUTTON(sett_menu, VALUE='Edit/manage config file',UVALUE='manageSettings')

  ;help_menu
  btnVersion=WIDGET_BUTTON(help_menu, VALUE='Check for update on GitHub', UVALUE='updGitHub')
  btnInfo=WIDGET_BUTTON(help_menu, VALUE='Wiki on GitHub.com', UVALUE='info')
  btnAbout=WIDGET_BUTTON(help_menu, VALUE='About ImageQC...',UVALUE='about')

  btoolBar=WIDGET_BASE(bLft,/ROW)
  toolbarLft=WIDGET_BASE(btoolBar,/ROW,/TOOLBAR)
  toolOpen=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\open.bmp',/BITMAP, UVALUE='open', TOOLTIP='Open DICOM file(s)')
  toolOpenMultiple=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\openM.bmp',/BITMAP, UVALUE='openMulti', TOOLTIP='Open DICOM file(s) with advanced options')
  toolOpenAuto=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\openAuto.bmp',/BITMAP, UVALUE='openAuto', TOOLTIP='Open files using automation template')

  bAppend=WIDGET_BASE(toolbarLft, /NONEXCLUSIVE)
  btnAppend=WIDGET_BUTTON(bAppend, VALUE='Append', FONT=font1, YSIZE=15, TOOLTIP='Append or replace images when opening new images')

  lbl=WIDGET_LABEL(toolbarLft, VALUE='', XSIZE=20,/NO_COPY)
  btnImgTop=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_top.bmp',/BITMAP, UVALUE='imgTop', TOOLTIP='Place selected image(s) at top of list')
  btnImgUp=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='imgUp', TOOLTIP='Move selected image(s) upwards in list')
  btnImgDown=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='imgDown', TOOLTIP='Move selected image(s) downwards of list')
  btnImgBottom=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\switch_bottom.bmp',/BITMAP, UVALUE='imgBottom', TOOLTIP='Place selected image(s) at bottom of list')
  lbl=WIDGET_LABEL(toolbarLft, VALUE='', XSIZE=5,/NO_COPY)
  btnSort=WIDGET_BUTTON(toolbarLft, VALUE=thisPath+'images\sort.bmp',/BITMAP, UVALUE='sortImg', TOOLTIP='Sort selected images by...')

  bProgress=WIDGET_BASE(btoolbar, /COLUMN, /ALIGN_LEFT)
  lblProgress0 = WIDGET_LABEL(bProgress, VALUE=' ', XSIZE=370, YSIZE=12, /SUNKEN_FRAME, FONT=font1)
  lblProgress = WIDGET_LABEL(bProgress, VALUE=' ', XSIZE=370, YSIZE=12, /SUNKEN_FRAME, FONT=font1)

  ;****************** left Panel
  bInfoLoaded=WIDGET_BASE(bLft, /ROW, YSIZE=210)
  bInfoLft=WIDGET_BASE(bInfoLoaded, /COLUMN)

  ;list ++
  bList=WIDGET_BASE(bInfoLft, /ROW)
  bListLoaded=WIDGET_BASE(bList, /COLUMN)
  bListLoadedTitle=WIDGET_BASE(bListLoaded, /ROW)
  lblLoaded=WIDGET_LABEL(bListLoadedTitle, VALUE='Loaded images ( ', /ALIGN_LEFT, FONT=font0)
  lblLoadedN=WIDGET_LABEL(bListLoadedTitle, VALUE='0 )', /ALIGN_LEFT, FONT=font0, XSIZE=300)
  lbl=WIDGET_LABEL(bListLoadedTitle, VALUE='List with:', FONT=font1,/NO_COPY)
  modNames=TAG_NAMES(multiOpt)
  lstShowFile=WIDGET_DROPLIST(bListLoadedTitle, VALUE=['Filename','Rename template '+modNames(modality)], FONT=font1, UVALUE='lstShowFile')
  tbList=WIDGET_BASE(bListLoadedTitle, /ROW, /TOOLBAR)
  toolInfoList=WIDGET_BUTTON(tbList, VALUE=thisPath+'images\info.bmp',/BITMAP, UVALUE='infoList', TOOLTIP='Info about list display')
  listFiles=WIDGET_LIST(bListLoaded, XSIZE=650, SCR_XSIZE=winX/2-110, YSIZE=1, SCR_YSIZE=listFilesYsize, MULTIPLE=1, FONT=font1, UVALUE='filelist', /CONTEXT_EVENTS)
  ctmActions=WIDGET_BASE(listFiles, /CONTEXT_MENU)
  ctmActions0=WIDGET_BUTTON(ctmActions, VALUE='Mark selected',  FONT=font1, UVALUE='listActions', UNAME='listActions0')
  ctmActions1=WIDGET_BUTTON(ctmActions, VALUE='Remove all marks',  FONT=font1, UVALUE='listActions', UNAME='listActions1')
  ctmActions2=WIDGET_BUTTON(ctmActions, VALUE='Remove mark from selected',  FONT=font1, UVALUE='listActions', UNAME='listActions2')
  ctmActions3=WIDGET_BUTTON(ctmActions, VALUE='Select inverse',  FONT=font1, UVALUE='listActions', UNAME='listActions3')
  ctmActions4=WIDGET_BUTTON(ctmActions, VALUE='Select marked',  FONT=font1, UVALUE='listActions', UNAME='listActions4')
  ctmActions5=WIDGET_BUTTON(ctmActions, VALUE='Select imgNo .. to ..', FONT=font1, UVALUE='listActions', UNAME='listActions5')
  ctmActions6=WIDGET_BUTTON(ctmActions, VALUE='Close selected',  FONT=font1, UVALUE='listActions', UNAME='listActions6')

  bPrevNext = WIDGET_BASE(bList, /COLUMN)
  lbl=WIDGET_LABEL(bPrevNext, VALUE='', FONT=font1, YSIZE=70, /NO_COPY)
  btnPrev = WIDGET_BUTTON(bPrevNext, VALUE=thisPath+'images\shift_up.bmp',/BITMAP,UVALUE='prev',TOOLTIP='Previous image in list')
  btnNext = WIDGET_BUTTON(bPrevNext, VALUE=thisPath+'images\shift_down.bmp', /BITMAP,UVALUE='next',TOOLTIP='Next image in list')

  ;image
  bDraw = WIDGET_BASE(bLft, XSIZE=drawXY+180, YSIZE=drawXY+10, /ROW)
  bDrawLft = WIDGET_BASE(bDraw, Ysize=drawXY,XSIZE=180,/COLUMN)
  drawLarge = WIDGET_DRAW(bDraw, XSIZE=drawXY, YSIZE=drawXY, KEYBOARD_EVENTS=1, /BUTTON_EVENTS, /MOTION_EVENTS, /WHEEL_EVENTS, GRAPHICS_LEVEL=2, RETAIN=retainVal, SENSITIVE=0)

  ;window level
  bViz = WIDGET_BASE(bDrawLft, /COLUMN)
  bWLtit=WIDGET_BASE(bViz, /ROW)
  lbl = WIDGET_LABEL(bWLtit, VALUE='Window level', /ALIGN_LEFT, FONT=font0, XSIZE=80, /NO_COPY)
  btnSetWLminmax=WIDGET_BUTTON(bWLtit, VALUE=thisPath+'images\minmax.bmp', /BITMAP, UVALUE='WLminmax', TOOLTIP='Set Window Level to min/max in image')
  btnSetWLstdev=WIDGET_BUTTON(bWLtit, VALUE=thisPath+'images\meanstdev.bmp', /BITMAP, UVALUE='WLmeanstdev', TOOLTIP='Set Window Level to mean+/-stdev of pixelvalues in selected image')
  btnSetWLdcm=WIDGET_BUTTON(bWLtit, VALUE='DCM', UVALUE='WLdcm', FONT=font1, TOOLTIP='Set Window Level as specified in DICOM header for the currently active image')

  bWindowMinMax=WIDGET_BASE(bViz, /ROW)

  txtMinWL = WIDGET_TEXT(bWindowMinMax, VALUE='-200', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bWindowMinMax, VALUE='Min', FONT=font1,SCR_XSIZE=22, /NO_COPY);, /ALIGN_RIGHT)
  lbl= WIDGET_LABEL(bWindowMinMax, VALUE=' ', FONT=font1,SCR_XSIZE=3, /NO_COPY);, /ALIGN_RIGHT)
  lbl = WIDGET_LABEL(bWindowMinMax, VALUE='Max', FONT=font1,SCR_XSIZE=22, /NO_COPY);, /ALIGN_RIGHT)
  txtMaxWL = WIDGET_TEXT(bWindowMinMax, VALUE='200', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bWindowCenterWidth=WIDGET_BASE(bViz, /ROW)
  lbl=WIDGET_LABEL(bWindowCenterWidth, VALUE='C, W', FONT=font1,SCR_XSIZE=45, /NO_COPY);, /ALIGN_RIGHT)
  txtCenterWL=WIDGET_TEXT(bWindowCenterWidth, VALUE='0', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bWindowCenterWidth, VALUE=',',FONT=font1, SCR_XSIZE=3, /NO_COPY);, /ALIGN_RIGHT)
  txtWidthWL=WIDGET_TEXT(bWindowCenterWidth, VALUE='400', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  btnSetColorTable=WIDGET_BUTTON(bViz, VALUE=thisPath+'images\ctGrayScale.bmp',/BITMAP, UVALUE='colorTable', /FLAT, TOOLTIP='Change colortable')
  ;lbl = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=5, /NO_COPY);, FONT=font1)

  ;rgt of image - cursor
  bCursor=WIDGET_BASE(bDrawLft, /COLUMN, FRAME=1)
  lbl = WIDGET_LABEL(bCursor, VALUE='Cursor ', /ALIGN_LEFT, FONT=font0, /NO_COPY)
  bCursorPos=WIDGET_BASE(bCursor, /ROW )
  lbl=WIDGET_LABEL(bCursorPos, VALUE='Position (pix): ',FONT=font1, /NO_COPY)
  lblCursorPos=WIDGET_LABEL(bCursorPos, VALUE='-,-', XSIZE=70, FONT=font1)
  bCursorPosMM=WIDGET_BASE(bCursor, /ROW)
  lbl=WIDGET_LABEL(bCursorPosMM, VALUE='Position (mm): ',FONT=font1, /NO_COPY)
  lblCursorPosMM=WIDGET_LABEL(bCursorPosMM, VALUE='-,-', XSIZE=70, FONT=font1)
  bCursorValue= WIDGET_BASE(bCursor, /ROW)
  lbl=WIDGET_LABEL(bCursorValue, VALUE='Value: ',FONT=font1, /NO_COPY)
  lblCursorValue=WIDGET_LABEL(bCursorValue, VALUE='-', XSIZE=50, FONT=font1)
 
  bZoom = WIDGET_BASE(bDrawLft, /ROW)
  lbl=WIDGET_LABEL(bZoom, VALUE='Zoom: ',FONT=font1, /NO_COPY)
  zoomSlider=CW_FSLIDER(bZoom, /DRAG, MINIMUM=1., MAXIMUM=4., VALUE=1.,FORMAT='(f0.1)', UVALUE='zoom')
  
  bCenterAngle = WIDGET_BASE(bDrawLft, /COLUMN)
  bTitCA=WIDGET_BASE(bCenterAngle, /ROW)
  lbl = WIDGET_LABEL(bTitCA, VALUE='Center / rotation ',/ALIGN_LEFT, FONT=font0, /NO_COPY)
  btnGetDelta=WIDGET_BUTTON(bTitCA, VALUE=thisPath+'images\search.bmp',/BITMAP,FONT=font1, UVALUE='getCenter',TOOLTIP='Search for center of mass in image based on threshold value')
  btnSetDelta=WIDGET_BUTTON(bTitCA, VALUE=thisPath+'images\arrow.bmp',/BITMAP,FONT=font1, UVALUE='setCenter', TOOLTIP='Sets center to the position of the last mouseclick in image')

  bDeltaX=WIDGET_BASE(bCenterAngle,/ROW)
  lbl=WIDGET_LABEL(bDeltaX, VALUE='dx', XSIZE=20, FONT=font1, /NO_COPY)
  txtDeltaX=WIDGET_TEXT(bDeltaX, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  minusDeltaX=WIDGET_BUTTON(bDeltaX, VALUE='-', UVALUE='minusDx', FONT=font1, SCR_YSIZE=20)
  plusDeltaX=WIDGET_BUTTON(bDeltaX, VALUE='+', UVALUE='plusDx', FONT=font1, SCR_YSIZE=20)
  bDeltaY=WIDGET_BASE(bCenterAngle,/ROW)
  lbl=WIDGET_LABEL(bDeltaY, VALUE='dy', XSIZE=20,FONT=font1, /NO_COPY)
  txtDeltaY=WIDGET_TEXT(bDeltaY, VALUE='0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  minusDeltaY=WIDGET_BUTTON(bDeltaY, VALUE='-', UVALUE='minusDy', FONT=font1, SCR_YSIZE=20)
  plusDeltaY=WIDGET_BUTTON(bDeltaY, VALUE='+', UVALUE='plusDy', FONT=font1, SCR_YSIZE=20)
  bDeltaA=WIDGET_BASE(bCenterAngle,/ROW)
  lbl=WIDGET_LABEL(bDeltaA, VALUE='da', XSIZE=20,FONT=font1, /NO_COPY)
  txtDeltaA=WIDGET_TEXT(bDeltaA, VALUE='0.0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1, SCR_YSIZE=20)
  minusDeltaA=WIDGET_BUTTON(bDeltaA, VALUE='-', UVALUE='minusDa', FONT=font1, SCR_YSIZE=20)
  plusDeltaA=WIDGET_BUTTON(bDeltaA, VALUE='+', UVALUE='plusDa', FONT=font1, SCR_YSIZE=20)
  bUse=WIDGET_BASE(bCenterAngle, /NONEXCLUSIVE)
  useDelta=WIDGET_BUTTON(bUse, VALUE='Use offset', FONT=font1,UVALUE='useDelta')
  WIDGET_CONTROL, useDelta, SET_BUTTON=1

  ;lbl = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=5, FONT=font1, /NO_COPY)

  bHide=WIDGET_BASE(bDrawLft, /ROW, /NONEXCLUSIVE)
  btnHideAnnot=WIDGET_BUTTON(bHide, VALUE='Hide annotations', UVALUE='hideAnnot',FONT=font1)

  ;lbl = WIDGET_LABEL(bDrawLft, VALUE='', YSIZE=5, FONT=font1, /NO_COPY)

  ;iImage toolbar
  bTitIimage=WIDGET_BASE(bDrawLft, /ROW)
  titleIimage = WIDGET_LABEL(bTitIimage, VALUE='Open in visualizer ',/ALIGN_LEFT, FONT=font0)
  toolBarDraw = WIDGET_BASE(bDrawLft, /ROW, /TOOLBAR)
  btnAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\ax.bmp', /BITMAP, UVALUE='ax', TOOLTIP='Send active image to iImage window')
  btnCor = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\cor.bmp', /BITMAP, UVALUE='cor', TOOLTIP='Send coronal image found from image stack at defined center to iImage window')
  btnSag = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sag.bmp', /BITMAP, UVALUE='sag', TOOLTIP='Send sagittal image found from image stack at defined center to iImage window')
  btnSurface = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\surface.bmp', /BITMAP, UVALUE='surf', TOOLTIP='View active image as surface plot in iImage window')
  btnSumAx = WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\sum.bmp', /BITMAP, UVALUE='sumax', TOOLTIP='Sum all or marked (X) images and send to iImage window')
  btn3dVol  = WIDGET_BUTTON(toolBarDraw, VALUE='3D', UVALUE='3d', TOOLTIP='Visualize in IDL 3d Slicer3')
  btnMovie=WIDGET_BUTTON(toolBarDraw, VALUE=thisPath+'images\play.bmp',/BITMAP,UVALUE='movie',TOOLTIP='Show images as movie')

  bInfoLow=WIDGET_BASE(bLft, /ROW)
  lbl=WIDGET_LABEL(bInfoLow, VALUE='', XSIZE=30, /NO_COPY)
  toolBarInfo = WIDGET_BASE(bInfoLow, /COLUMN, /TOOLBAR)
  lbl=WIDGET_LABEL(bInfoLow, VALUE='', XSIZE=20, /NO_COPY)
  btnClipBoardInfo=WIDGET_BUTTON(toolBarInfo, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy image info (all images) to clipboard or file in tabular format', UVALUE='copyInfo')
  toolDump=WIDGET_BUTTON(toolbarInfo, VALUE=thisPath+'images\dump.bmp', /BITMAP, TOOLTIP='DICOM dump of active file', UVALUE='dump')
  toolEditHeader=WIDGET_BUTTON(toolbarInfo, VALUE=thisPath+'images\edit.bmp', /BITMAP, TOOLTIP='Edit parameters from DICOM header of active file', UVALUE='editHeader')
  txtActive1=WIDGET_TEXT(bInfoLow, XSIZE=100, YSIZE=100, VALUE='', SCR_XSIZE=300, SCR_YSIZE=150, FONT=font1)
  txtActive2=WIDGET_TEXT(bInfoLow, XSIZE=100, YSIZE=100, VALUE='', SCR_XSIZE=300, SCR_YSIZE=150, FONT=font1)

  ;################################################################## right side ###################################################
  ;Defaults
  bDefaults= WIDGET_BASE(bRgt, /ROW)
  bDefTool1=WIDGET_BASE(bDefaults, /ROW, /TOOLBAR)
  btnSettings=WIDGET_BUTTON(bDefTool1, VALUE=thisPath+'images\gears.bmp',/BITMAP, UVALUE='manageSettings', TOOLTIP='Edit/manage parameter set for default settings')
  ttnn=TAG_NAMES(configS)
  name=ttnn(configS.(0).defConfigNo)
  lblSettings=WIDGET_LABEL(bDefaults, VALUE=name, XSIZE=110, /SUNKEN_FRAME, FONT=font1)
  lbl=WIDGET_LABEL(bDefaults, VALUE='', XSIZE=10, /NO_COPY)
  bExportSettings=WIDGET_BASE(bDefaults, /ROW, FRAME=1, XSIZE=500)
  lbl=WIDGET_LABEL(bExportSettings, VALUE='Export:   ', FONT=font0, /NO_COPY)
  bHeaders=WIDGET_BASE(bExportSettings, /NONEXCLUSIVE, /ROW)
  btnCopyHeader=WIDGET_BUTTON(bHeaders, VALUE='Include headers', YSIZE=15, TOOLTIP='Include headers when exporting tables', FONT=font1, UVALUE='copyHeader')
  btnTranspose=WIDGET_BUTTON(bHeaders, VALUE='Transpose table', YSIZE=15, TOOLTIP='Transpose table when exporting (columns to rows)', FONT=font1, UVALUE='transposeTable')
  lbl=WIDGET_LABEL(bExportSettings, VALUE='Decimal mark:', FONT=font1, /NO_COPY)
  listDeciMark=WIDGET_DROPLIST(bExportSettings, VALUE=['. (period)',', (comma)'], XSIZE=100, FONT=font1, UVALUE='deciMark')

  lbl=WIDGET_LABEL(bRgt, VALUE='', YSIZE=5, /NO_COPY)
  lbl=WIDGET_LABEL(bRgt, VALUE='QuickTest:   ', /ALIGN_LEFT, FONT=font0, /NO_COPY)

  ;QuickTest
  bMulti=WIDGET_BASE(bRgt, /ROW, SCR_XSIZE=500, FRAME=1)
  bUseMulti=WIDGET_BASE(bMulti, /NONEXCLUSIVE)
  btnUseMulti=WIDGET_BUTTON(bUseMulti, VALUE='MultiMark', YSIZE=15, UVALUE='useMulti', TOOLTIP='Mark images for specific numbered tests', FONT=font1)
  lbl=WIDGET_LABEL(bMulti, VALUE='', XSIZE=10, FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bMulti, VALUE='QuickTest template:', FONT=font1, /NO_COPY)
  listSelMultiTemp=WIDGET_DROPLIST(bMulti, UVALUE='listSelMultiTemp', XSIZE=140, FONT=font1)
  bMultiTool=WIDGET_BASE(bMulti, /ROW, /TOOLBAR)
  btnAddMultiTemp=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\plus.bmp',/BITMAP, UVALUE='addMultiTemp', TOOLTIP='Save current MultiMark as new QuickTest template', YSIZE=20)
  btnSaveMultiTemp=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\save.bmp',/BITMAP, UVALUE='saveMultiTemp', TOOLTIP='Overwrite current QuickTest template', YSIZE=20)
  btnManageQT=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\gears.bmp',/BITMAP, UVALUE='manageQT', TOOLTIP='Edit/manage QuickTest templates')
  lbl=WIDGET_LABEL(bMultiTool, VALUE='', XSIZE=10, FONT=font1, /NO_COPY)
  btnRunMulti=WIDGET_BUTTON(bMultiTool, VALUE='QuickTest', UVALUE='runMulti', TOOLTIP='Calculate results for all numbered tests and the corresponding marked images', YSIZE=20, FONT=font1)
  btnExpMulti=WIDGET_BUTTON(bMultiTool, VALUE=thisPath+'images\copy.bmp',/BITMAP, UVALUE='expMulti', TOOLTIP='Copy results to clipboard (paste into fx Excel)', YSIZE=20)
  bIncFilename=WIDGET_BASE(bMulti, /NONEXCLUSIVE)
  btnIncFilename=WIDGET_BUTTON(bIncFilename, VALUE='Include filename', YSIZE=15, TOOLTIP='Include list of filenames in the QuickTest export', FONT=font1)

  lbl=WIDGET_LABEL(bRgt, VALUE='', YSIZE=5, /NO_COPY)

  ;Analysis tabs
  bAnalysis=WIDGET_BASE(bRgt, /COLUMN)
  wtabModes=WIDGET_TAB(bAnalysis, XSIZE=660, YSIZE=260, UVALUE='tabModes')
  bCT=WIDGET_BASE(wtabModes, TITLE='CT', /COLUMN, UVALUE='tabCT')
  bX=WIDGET_BASE(wtabModes,TITLE='Xray', /COLUMN, UVALUE='tabXray')
  bNM=WIDGET_BASE(wtabModes, TITLE='NM planar',/COLUMN, UVALUE='tabNM')
  bSPECT=WIDGET_BASE(wtabModes, TITLE='SPECT', /COLUMN, UVALUE='tabSPECT')
  bPET=WIDGET_BASE(wtabModes, TITLE='PET', /COLUMN, UVALUE='tabPET')
  bMR=WIDGET_BASE(wtabModes, TITLE='MR', /COLUMN, UVALUE='tabMR')

  wtabAnalysisCT=WIDGET_TAB(bCT, XSIZE=650, YSIZE=240)
  wtabAnalysisXray=WIDGET_TAB(bX, XSIZE=650, YSIZE=240)
  wtabAnalysisNM=WIDGET_TAB(bNM, XSIZE=650, YSIZE=240)
  wtabAnalysisSPECT=WIDGET_TAB(bSPECT, XSIZE=650, YSIZE=240)
  wtabAnalysisPET=WIDGET_TAB(bPET, XSIZE=650, YSIZE=240)
  wtabAnalysisMR=WIDGET_TAB(bMR, XSIZE=650, YSIZE=240)

  ; *************************CT tests*****************************************************
  bHomog=WIDGET_BASE(wtabAnalysisCT, Title='1. Homogeneity', /COLUMN)
  bNoise=WIDGET_BASE(wtabAnalysisCT, Title='2. Noise', /COLUMN)
  bSliceThick=WIDGET_BASE(wtabAnalysisCT, Title='3. Slice thickness', /ROW)
  bMTF=WIDGET_BASE(wtabAnalysisCT, TITLE='4. MTF',/Column)
  bLinearity=WIDGET_BASE(wtabAnalysisCT, Title='5. CT Number', /ROW)
  bHUwater=WIDGET_BASE(wtabAnalysisCT, Title='6. HU water', /COLUMN)
  bExp=WIDGET_BASE(wtabAnalysisCT, Title='7. Header info', /COLUMN)
  bROI=WIDGET_BASE(wtabAnalysisCT, Title='8. ROI', /COLUMN)
  bNPS=WIDGET_BASE(wtabAnalysisCT, TITLE='NPS',/Column)
  bDim=WIDGET_BASE(wtabAnalysisCT, TITLE='Dim', /COLUMN)
  bFwhm=WIDGET_BASE(wtabAnalysisCT, Title='FWHM', /COLUMN)

  ;--------------- Linear dimensions DIM
  lbl=WIDGET_LABEL(bDim, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl=WIDGET_LABEL(bDim, VALUE='Calculate distance between rods in CatPhan.',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bDim, VALUE='Searches center of rod 25 mm (+/-) 10 mm from defined center of image.',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bDim, VALUE='', FONT=font1, /NO_COPY)
  bDimBtns=WIDGET_BASE(bDim, /ROW)
  btnDim=WIDGET_BUTTON(bDimBtns, VALUE='Calculate linear dimensions', UVALUE='dim',FONT=font1)

  ;---------------Homogeneity--------
  lbl=WIDGET_LABEL(bHomog, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bHomogSize=WIDGET_BASE(bHomog, /ROW)
  lbl = WIDGET_LABEL(bHomogSize, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtHomogROIsz = WIDGET_TEXT(bHomogSize, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bHomogSize, VALUE='', XSIZE=20, FONT=font1, /NO_COPY)
  lbl = WIDGET_LABEL(bHomogSize, VALUE='Radius to ROIs (mm)',FONT=font1, /NO_COPY)
  txtHomogROIdist = WIDGET_TEXT(bHomogSize, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bHomogBtns=WIDGET_BASE(bHomog, /ROW)
  btnHomog=WIDGET_BUTTON(bHomogBtns, VALUE='Calculate homogeneity', UVALUE='homog',FONT=font1)

  ;---------------Noise--------
  lbl=WIDGET_LABEL(bNoise, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bNoiseROI=WIDGET_BASE(bNoise, /ROW)
  lbl = WIDGET_LABEL(bNoiseROI, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtNoiseROIsz = WIDGET_TEXT(bNoiseROI, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNoiseBtns=WIDGET_BASE(bNoise, /ROW)
  btnNoise=WIDGET_BUTTON(bNoiseBtns, VALUE='Calculate noise', UVALUE='noise',FONT=font1)

  ;--------------- HU water--------
  lbl = WIDGET_LABEL(bHUwater, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bHUwaterROI=WIDGET_BASE(bHUwater, /ROW)
  lbl = WIDGET_LABEL(bHUwaterROI, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtHUwaterROIsz = WIDGET_TEXT(bHUwaterROI, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bHUwaterBtns=WIDGET_BASE(bHUwater, /ROW)
  btnHUwater=WIDGET_BUTTON(bHUwaterBtns, VALUE='Calculate CT number in water', UVALUE='HUwater',FONT=font1)

  ;---------------Header info --------
  lbl=WIDGET_LABEL(bExp, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bbExp=WIDGET_BASE(bExp, /ROW)
  lbl = WIDGET_LABEL(bbExp, VALUE='Extract DICOM header info: kVp, mAs, CTDIvol and software version.',FONT=font1, /NO_COPY)
  bExpBtns=WIDGET_BASE(bExp, /ROW)
  btnExp=WIDGET_BUTTON(bExpBtns, VALUE='Get exposure values', UVALUE='headerInfoCT',FONT=font1)
  lbl = WIDGET_LABEL(bExp, VALUE='',FONT=font1, /NO_COPY)
  lbl = WIDGET_LABEL(bExp, VALUE='More DICOM elements can be added to output when using QuickTest and defining additional DICOM output.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bExp, VALUE='Define output in Setting->QuickTest Output template and then connect the output template to a parameter set.',FONT=font1, /NO_COPY, /ALIGN_LEFT)

  ;----------------User defined ROI------------
  bROIsett=WIDGET_BASE(bROI, /ROW)
  bROIlft=WIDGET_BASE(bROIsett, /COLUMN)
  ;typeROI=CW_BGROUP(bROI, ['Define new','Load saved ROI (.sav)'], /EXCLUSIVE, LABEL_TOP='Define ROI and get min/max/avg/stdev', /FRAME, UVALUE='typeROI',FONT=font1)
  typeROI=CW_BGROUP(bROIlft, ['Circular','Rectangluar','Rectangular rotated'], /EXCLUSIVE, LABEL_TOP='ROI shape', /FRAME, UVALUE='typeROI',FONT=font1)
  ;btnDefROI =WIDGET_BUTTON(bROI, VALUE = 'Define ROI', UVALUE='ROI',FONT=font1)

  bOffsetROI=WIDGET_BASE(bROIlft, /COLUMN)
  bTitOffsetROI=WIDGET_BASE(bOffsetROI, /ROW)
  lbl = WIDGET_LABEL(bTitOffsetROI, VALUE='Extra offset ', FONT=font0, /NO_COPY)
  getOffsetROI=WIDGET_BUTTON(bTitOffsetROI, VALUE=thisPath+'images\arrow.bmp',/BITMAP,UVALUE='setOffset', TOOLTIP='Sets offset corresponding to the position of the last mouseclick in image')
  bDeltaO_ROI=WIDGET_BASE(bOffsetROI,/ROW)
  lbl=WIDGET_LABEL(bDeltaO_ROI, VALUE='dx,dy: ', XSIZE=40, FONT=font1, /NO_COPY)
  lblDeltaO_ROI=WIDGET_LABEL(bDeltaO_ROI, VALUE=STRING(offxyROI(0), FORMAT='(i0)')+','+STRING(offxyROI(1), FORMAT='(i0)'), XSIZE=70, FONT=font1)
  unitDeltaO_ROI_CT=CW_BGROUP(bOffsetROI, ['pix','mm'], /EXCLUSIVE, LABEL_TOP='Unit extra offset', /FRAME, UVALUE='setOffset_unit', SPACE=-2, YPAD=0, COLUMN=2, FONT=font1)

  bROIrgt=WIDGET_BASE(bROIsett, /COLUMN)
  bROIrad=WIDGET_BASE(bROIrgt, /ROW)
  lbl = WIDGET_LABEL(bROIrad, VALUE='Radius if circular ROI (mm)',FONT=font1, /NO_COPY)
  txtROIrad = WIDGET_TEXT(bROIrad, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIx=WIDGET_BASE(bROIrgt, /ROW)
  lbl = WIDGET_LABEL(bROIx, VALUE='X size if rectangular ROI (mm)',FONT=font1, XSIZE=170, /NO_COPY)
  txtROIx = WIDGET_TEXT(bROIx, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIy=WIDGET_BASE(bROIrgt, /ROW)
  lbl = WIDGET_LABEL(bROIy, VALUE='Y size if rectangular ROI (mm)',FONT=font1, XSIZE=170, /NO_COPY)
  txtROIy = WIDGET_TEXT(bROIy, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIa=WIDGET_BASE(bROIrgt, /ROW)
  lbl = WIDGET_LABEL(bROIa, VALUE='Rotation rectangular ROI (degrees)',FONT=font1, XSIZE=195, /NO_COPY)
  txtROIa = WIDGET_TEXT(bROIa, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  btnROI=WIDGET_BUTTON(bROIrgt, VALUE='Get ROI values', UVALUE='ROI_CT',FONT=font1)

  ;----------------MTF------------------
  bMTFsettings=WIDGET_BASE(bMTF, /ROW)

  bMTFlft=WIDGET_BASE(bMTFsettings,/COLUMN)
  cw_typeMTF=CW_BGROUP(bMTFlft, ['Bead','Wire','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME,FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)

  bMTFrgt=WIDGET_BASE(bMTFsettings,/COLUMN)
  bCutLSF=WIDGET_BASE(bMTFrgt, /NONEXCLUSIVE, /ROW)
  btnCutLSF=WIDGET_BUTTON(bCutLSF, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFW=WIDGET_BASE(bMTFrgt, /ROW)
  lbl=WIDGET_LABEL( bCutLSFW, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1, /NO_COPY)
  txtCutLSFW=WIDGET_TEXT( bCutLSFW, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bCutLSFW2=WIDGET_BASE(bMTFrgt,/ROW)
  lbl=WIDGET_LABEL( bCutLSFW2, VALUE='Fade out cut within (#FWHM)',FONT=font1, /NO_COPY)
  txtCutLSFW2=WIDGET_TEXT( bCutLSFW2, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bfreqMTF=WIDGET_BASE(bMTFrgt, /ROW)
  lbl=WIDGET_LABEL(bfreqMTF, VALUE='Sampling frequency gaussian MTF curve (mm-1)',FONT=font1, /NO_COPY)
  txtfreqMTF=WIDGET_TEXT(bfreqMTF, VALUE='0.010', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bMTFroiSz=WIDGET_BASE(bMTFrgt, /ROW)
  lbl=WIDGET_LABEL(bMTFroiSz, VALUE='ROI size from center (mm)',FONT=font1, /NO_COPY)
  txtMTFroiSz=WIDGET_TEXT(bMTFroiSz, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bMTFsettings2=WIDGET_BASE(bMTFsettings, /COLUMN)
  cw_plotMTF=CW_BGROUP(bMTFsettings2, ['Centered xy profiles', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTF',FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  cw_tableMTF=CW_BGROUP(bMTFsettings2, ['Gaussian','Discrete'], /EXCLUSIVE, LABEL_TOP='Show table results from...', /FRAME, UVALUE='cw_tableMTF', FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  cw_cyclMTF=CW_BGROUP(bMTFsettings2, ['cy/mm','cy/cm'], /EXCLUSIVE, /FRAME, UVALUE='cw_cyclMTF', FONT=font1, COLUMN=2, SPACE=-2, YPAD=0)

  bOffset=WIDGET_BASE(bMTFlft, /COLUMN)
  bTitOffset=WIDGET_BASE(bOffset, /ROW)
  lbl=WIDGET_LABEL(bTitOffset, VALUE='Extra offset ', FONT=font0, /NO_COPY)
  getOffset=WIDGET_BUTTON(bTitOffset, VALUE=thisPath+'images\arrow.bmp',/BITMAP,UVALUE='setOffset', TOOLTIP='Sets offset corresponding to the position of the last mouseclick in image')
  bDeltaO=WIDGET_BASE(bOffset,/ROW)
  lbl=WIDGET_LABEL(bDeltaO, VALUE='dx,dy: ', XSIZE=40, FONT=font1, /NO_COPY)
  lblDeltaO=WIDGET_LABEL(bDeltaO, VALUE=STRING(offxyMTF(0), FORMAT='(i0)')+','+STRING(offxyMTF(1), FORMAT='(i0)'), XSIZE=70, FONT=font1)
  unitDeltaO_MTF_CT=CW_BGROUP(bOffset, ['pix','mm'], /EXCLUSIVE, LABEL_TOP='Unit extra offset', /FRAME, UVALUE='setOffset_unit', SPACE=-2, YPAD=0, COLUMN=2, FONT=font1)

  bSearchMax=WIDGET_BASE(bMTFrgt, /NONEXCLUSIVE, /ROW)
  btnSearchMaxMTF=WIDGET_BUTTON(bSearchMax, VALUE='Center ROI. (Visual ROI based on selected image only.)', UVALUE='searchMaxMTF_ROI',FONT=font1)
  lbl=WIDGET_LABEL(bMTFrgt, VALUE='Bead/wire: max in imagesum, Circular edge: optimize center.', FONT=font1, /NO_COPY)

  bMTFbtns=WIDGET_BASE(bMTFrgt, /ROW)
  btnMTF=WIDGET_BUTTON(bMTFbtns, VALUE='Calculate MTF', UVALUE='MTF',FONT=font1)

  ;------------ NPS ---------------------
  bNPSroiSz=WIDGET_BASE(bNPS, /ROW)
  lbl=WIDGET_LABEL(bNPSroiSz, VALUE='ROI size (pix)',FONT=font1, /NO_COPY)
  txtNPSroiSz=WIDGET_TEXT(bNPSroiSz, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSroiDist=WIDGET_BASE(bNPS, /ROW)
  lbl=WIDGET_LABEL(bNPSroiDist, VALUE='Radius to center of ROIs (mm)',FONT=font1, /NO_COPY)
  txtNPSroiDist=WIDGET_TEXT(bNPSroiDist, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSsubNN=WIDGET_BASE(bNPS, /ROW)
  lbl=WIDGET_LABEL(bNPSsubNN, VALUE='Number of ROIs',FONT=font1, /NO_COPY)
  txtNPSsubNN=WIDGET_TEXT(bNPSsubNN, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bSmoothNPS=WIDGET_BASE(bNPS, /ROW)
  lbl=WIDGET_LABEL(bSmoothNPS, VALUE='Smooth NPS curve by width (mm-1)',FONT=font1, /NO_COPY)
  txtSmoothNPS=WIDGET_TEXT(bSmoothNPS, VALUE='0.050', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bfreqNPS=WIDGET_BASE(bNPS, /ROW)
  lbl=WIDGET_LABEL(bfreqNPS, VALUE='Sampling frequency NPS curve (mm-1)',FONT=font1, /NO_COPY)
  txtfreqNPS=WIDGET_TEXT(bfreqNPS, VALUE='0.010', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bNPSbtns=WIDGET_BASE(bNPS, /ROW)
  btnNPS=WIDGET_BUTTON(bNPSbtns, VALUE='Calculate NPS', UVALUE='NPS',FONT=font1)
  bNPSavg=WIDGET_BASE(bNPSbtns, /NONEXCLUSIVE, /ROW)
  btnNPSavg=WIDGET_BUTTON(bNPSavg, VALUE='Plot average', UVALUE='NPSavg',FONT=font1)

  ;-------------CT numbers linearity-------------
  bLinSettings=WIDGET_BASE(bLinearity, /ROW)
  bLinLft=WIDGET_BASE(bLinSettings, /COLUMN)
  lbl=WIDGET_LABEL(bLinLft, VALUE='', YSIZE=20, FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bLinLft, VALUE='Calculate CT Numbers within ROIs',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bLinLft, VALUE='', YSIZE=20, FONT=font1, /NO_COPY)
  bLinSearchROI=WIDGET_BASE(bLinLft, /ROW)
  lbl = WIDGET_LABEL(bLinSearchROI, VALUE='Radius of search ROIs (mm)',FONT=font1, /NO_COPY)
  txtLinROIradS = WIDGET_TEXT(bLinSearchROI, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bLinSzROI=WIDGET_BASE(bLinLft, /ROW)
  lbl = WIDGET_LABEL(bLinSzROI, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtLinROIrad = WIDGET_TEXT(bLinSzROI, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bLinAvoidSearch=WIDGET_BASE(bLinlft, /NONEXCLUSIVE, /ROW)
  btnLinAvoidSearch=WIDGET_BUTTON(bLinAvoidSearch, VALUE='Avoid search and use center of search ROI', UVALUE='linAvoidSearch',FONT=font1)

  bLinButtons=WIDGET_BASE(bLinLft, /ROW)
  btnLinearity=WIDGET_BUTTON(bLinButtons, VALUE='Get CT numbers', UVALUE='Linearity',FONT=font1)

  bLinRgt=WIDGET_BASE(bLinSettings, /COLUMN)
  bLinTB=WIDGET_BASE(bLinRgt, /ROW, /TOOLBAR)
  btnLinImport=WIDGET_BUTTON(bLinTB, VALUE=thisPath+'images\importd.bmp', /BITMAP,TOOLTIP='Import table', UVALUE='impLinTab')
  btnLinCopy=WIDGET_BUTTON(bLinTB, VALUE=thisPath+'images\copy.bmp', /BITMAP,TOOLTIP='Copy table to clipboard', UVALUE='copyLinTab')
  btnLinAdd=WIDGET_BUTTON(bLinTB, VALUE=thisPath+'images\plus.bmp', /BITMAP, TOOLTIP='Add row to table', UVALUE='addRowLinTab')
  btnLinDel=WIDGET_BUTTON(bLinTB, VALUE=thisPath+'images\delete.bmp', /BITMAP, TOOLTIP='Delete selected row(s) from table', UVALUE='delRowLinTab')
  btnLinCenter=WIDGET_BUTTON(bLinTB, VALUE=thisPath+'images\center.bmp', /BITMAP, TOOLTIP='Set position of last mouse-click as center for this material', UVALUE='centerLinTab')
  tblLin=WIDGET_TABLE(bLinRgt, XSIZE=4, YSIZE=5, COLUMN_LABELS=['Material','ROI xpos (mm)', 'ROI ypos (mm)','Density'],COLUMN_WIDTHS=[80,90,90,60], /NO_ROW_HEADERS, SCR_XSIZE=winX/4, SCR_YSIZE=170, /ALL_EVENTS, /EDITABLE,FONT=font1)

  ;---------------Slice thickness--------
  lbl=WIDGET_LABEL(bSliceThick, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bSliceThickLft=WIDGET_BASE(bSliceThick, /COLUMN)
  cw_ramptype=CW_BGROUP(bSliceThickLft, ['Wire ramp (CatPhan)','Beaded ramp (CatPhan)','Vertical beaded ramps (GE)'], /EXCLUSIVE, LABEL_TOP='Ramp type...', /FRAME,FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  cw_rampDens=CW_BGROUP(bSliceThickLft, ['higher than background', 'lower than background'], /EXCLUSIVE, LABEL_TOP='Ramp density is...', /FRAME, FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  bSliceThickRgt=WIDGET_BASE(bSliceThick, /COLUMN)
  bRampDist=WIDGET_BASE(bSliceThickRgt, /ROW)
  lbl = WIDGET_LABEL(bRampDist, VALUE='Center to ramp distance (mm)',FONT=font1, /NO_COPY)
  txtRampDist = WIDGET_TEXT(bRampDist, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl2RampDist = WIDGET_LABEL(bSliceThickRgt, VALUE='    (ignored for beaded ramp in CatPhan, CTP591 geometry used)',FONT=font1)
  lbl=WIDGET_LABEL(bSliceThickRgt, VALUE='', YSIZE=10, /NO_COPY)
  bRampLen=WIDGET_BASE(bSliceThickRgt, /ROW)
  lbl = WIDGET_LABEL(bRampLen, VALUE='Profile length (mm)',FONT=font1, /NO_COPY)
  txtRampLen = WIDGET_TEXT(bRampLen, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bRampBack=WIDGET_BASE(bSliceThickRgt, /ROW)
  lbl = WIDGET_LABEL(bRampBack, VALUE='Background from outer (mm)',FONT=font1, /NO_COPY)
  txtRampBackG = WIDGET_TEXT(bRampBack, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bRampSearch=WIDGET_BASE(bSliceThickRgt, /ROW)
  lbl = WIDGET_LABEL(bRampSearch, VALUE='Search for max (or min) in profile',FONT=font1, /NO_COPY)
  txtRampSearch = WIDGET_TEXT(bRampSearch, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bRampSearch, VALUE='# pix from center of ramp',FONT=font1, /NO_COPY)
  bRampAverage=WIDGET_BASE(bSliceThickRgt, /ROW)
  lbl = WIDGET_LABEL(bRampAverage, VALUE='Average over ',FONT=font1, /NO_COPY)
  txtRampAverage = WIDGET_TEXT(bRampAverage, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bRampAverage, VALUE='# neighbour profiles',FONT=font1, /NO_COPY)
  bSliceThickBtns=WIDGET_BASE(bSliceThickRgt, /ROW)
  btnSliceThick=WIDGET_BUTTON(bSliceThickBtns, VALUE='Get Slice Thickness', UVALUE='SliceThick',FONT=font1)

  ;---------------FWHM---------------- move together with MTF later
  lbl=WIDGET_LABEL(bFWhm, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl=WIDGET_LABEL(bFWhm, VALUE='Code based on PSF.pro & CALCULATE_LSF_LIST.pro developed at DNR (Oslo, Norway) ',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bFWhm, VALUE=' by Arne Skretting, Wibeke Nordh'+string(248B)+'y, Alise Larsen and Kristine Eldevik',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bFWhm, VALUE='', YSIZE=20, FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bFWhm, VALUE='FWHM calculated from average of 10 pixelrows.',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bFWhm, VALUE='', YSIZE=20, FONT=font1, /NO_COPY)
  btnFwhm=WIDGET_BUTTON(bFwhm, VALUE='Calculate FWHM' , UVALUE='fwhm',FONT=font1)


  ;**********************X ray tests *******************************************************

  bSTP=WIDGET_BASE(wtabAnalysisXray, Title='1. STP', /COLUMN)
  bHomogX=WIDGET_BASE(wtabAnalysisXray, Title='2. Homogeneity', /COLUMN)
  bNoiseX=WIDGET_BASE(wtabAnalysisXray, Title='3. Noise', /COLUMN)
  bExpX=WIDGET_BASE(wtabAnalysisXray, Title='4. Header info', /COLUMN)
  bMTFsettingsX=WIDGET_BASE(wtabAnalysisXray, TITLE='5. MTF',/COLUMN)
  bROIX=WIDGET_BASE(wtabAnalysisXray, TITLE='6. ROI',/COLUMN)
  bNPSX=WIDGET_BASE(wtabAnalysisXray, TITLE='NPS',/Column)
  bVariX=WIDGET_BASE(wtabAnalysisXray, TITLE='Variance image',/Column)
  ;bROIX=WIDGET_BASE(wtabAnalysisXray, TITLE='ROI',/COLUMN)

  ;---------------STP--------
  lbl=WIDGET_LABEL(bSTP, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bStpSettings=WIDGET_BASE(bSTP, /ROW)
  lbl = WIDGET_LABEL(bStpSettings, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtStpROIsz = WIDGET_TEXT(bStpSettings, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  Qvals=[21759.,30174.,32362.,31077.,1]
  lbl = WIDGET_LABEL(bStpSettings, VALUE='     Beam quality',FONT=font1, /NO_COPY)
  ddlRQA = WIDGET_COMBOBOX(bStpSettings, VALUE=['RQA 3','RQA 5','RQA 7','RQA 9','other'], UVALUE='ddlRQA', /LIST_EVENTS,FONT=font1)
  WIDGET_CONTROL, ddlRQA, SET_COMBOBOX_SELECT=1
  txtRQA = WIDGET_TEXT(bStpSettings, VALUE='', UVALUE='txtRQA', XSIZE=7, /EDITABLE, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bStpSettings, VALUE='1/mm^2uGy',FONT=font1, /NO_COPY)
  bStpBtns=WIDGET_BASE(bSTP, /ROW)
  btnStp=WIDGET_BUTTON(bStpBtns, VALUE='Find pixel values', UVALUE='STPpix',FONT=font1)
  ;bStpProcess = WIDGET_BASE(bSTP, /ROW)
  btnImportDose=WIDGET_BUTTON(bStpBtns, VALUE='Import dose values', UVALUE='impDose',FONT=font1)
  btnCalcSTP=WIDGET_BUTTON(bStpBtns, VALUE='Calculate STP', UVALUE='calcSTP',FONT=font1)
  lbl=WIDGET_LABEL(bSTP, VALUE='', FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bSTP, VALUE='Warning: Consider test as "under construction" or "sleeping".',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bSTP, VALUE='     Only linear fit implemented for STP, beam quality and Qvalue might not be used correctly',FONT=font1, /NO_COPY)

  ;---------------Homogeneity--------
  lbl=WIDGET_LABEL(bHomogX, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bHomogXtop=WIDGET_BASE(bHomogX, /COLUMN)
  bHomogSizeX=WIDGET_BASE(bHomogXtop, /ROW)
  lbl = WIDGET_LABEL(bHomogSizeX, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtHomogROIszX = WIDGET_TEXT(bHomogSizeX, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  cw_HomogAltX=CW_BGROUP(bHomogXtop, ['Avg and stdev for each ROI','Avg for each ROI + difference from average of all ROIs','Avg for each ROI + % difference from average of all ROIs'], UVALUE='cw_HomogAltX', /EXCLUSIVE, LABEL_TOP='Output to table...', /FRAME, FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  bHomogBtnsX=WIDGET_BASE(bHomogX, /ROW)
  btnHomogX=WIDGET_BUTTON(bHomogBtnsX, VALUE='Calculate homogeneity', UVALUE='homog',FONT=font1)

  ;---------------Noise--------
  lbl=WIDGET_LABEL(bNoiseX, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bNoiseROIX=WIDGET_BASE(bNoiseX, /ROW)
  lbl = WIDGET_LABEL(bNoiseROIX, VALUE='ROI width and height ',FONT=font1, /NO_COPY)
  txtNoiseX  = WIDGET_TEXT(bNoiseROIX, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bNoiseROIX, VALUE=' % of image',FONT=font1, /NO_COPY)
  bNoiseBtnsX=WIDGET_BASE(bNoiseX, /ROW)
  btnNoiseX=WIDGET_BUTTON(bNoiseBtnsX, VALUE='Calculate noise', UVALUE='noise',FONT=font1)

  ;---------------Header info (prev. Exposure)--------
  lbl=WIDGET_LABEL(bExpX, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bbExpX=WIDGET_BASE(bExpX, /ROW)
  lblExpX = WIDGET_LABEL(bbExpX, VALUE='Extract DICOM header info: kVp, mAs, EI, DAP, SDD, detector ID',FONT=font1)
  bExpBtnsX=WIDGET_BASE(bExpX, /ROW)
  btnExpX=WIDGET_BUTTON(bExpBtnsX, VALUE='Get exposure values', UVALUE='EXP',FONT=font1)
  lbl = WIDGET_LABEL(bExpX, VALUE='',FONT=font1, /NO_COPY)
  lbl = WIDGET_LABEL(bExpX, VALUE='More DICOM elements can be added to output when using QuickTest and defining additional DICOM output.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bExpX, VALUE='Define output in Setting->QuickTest Output template and then connect the output template to a parameter set.',FONT=font1, /NO_COPY, /ALIGN_LEFT)

  ;----------------MTF------------------
  bMTFX=WIDGET_BASE(bMTFsettingsX, /ROW)
  bMTFX1=WIDGET_BASE(bMTFX, /COLUMN)
  cw_formLSFX=CW_BGROUP(bMTFX1, ['Exponential','Gaussian','None'], /EXCLUSIVE, LABEL_TOP='LSF fit to...', /FRAME, FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)

  bOffsetX=WIDGET_BASE(bMTFX1, /COLUMN)
  bTitOffsetX=WIDGET_BASE(bOffsetX, /ROW)
  lbl=WIDGET_LABEL(bTitOffsetX, VALUE='Extra offset ', FONT=font0, /NO_COPY)
  getOffsetX=WIDGET_BUTTON(bTitOffsetX, VALUE=thisPath+'images\arrow.bmp',/BITMAP,UVALUE='setOffset', TOOLTIP='Sets offset corresponding to the position of the last mouseclick in image')
  bDeltaOX=WIDGET_BASE(bOffsetX,/ROW)
  lbl=WIDGET_LABEL(bDeltaOX, VALUE='dx,dy: ', XSIZE=40, FONT=font1, /NO_COPY)
  lblDeltaOX=WIDGET_LABEL(bDeltaOX, VALUE=STRING(offxyMTF(0), FORMAT='(i0)')+','+STRING(offxyMTF(1), FORMAT='(i0)'), XSIZE=70, FONT=font1)
  unitDeltaO_MTF_X=CW_BGROUP(bOffsetX, ['pix','mm'], /EXCLUSIVE, LABEL_TOP='Unit extra offset', /FRAME, UVALUE='setOffset_unit', SPACE=-2, YPAD=0, COLUMN=2, FONT=font1)

  bMTFX2=WIDGET_BASE(bMTFX, /COLUMN)
  bLSFfilterX=WIDGET_BASE(bMTFX2, /COLUMN)
  bCutLSFX=WIDGET_BASE(bLSFfilterX, /NONEXCLUSIVE, /ROW)
  btnCutLSFX=WIDGET_BUTTON(bCutLSFX, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFWX=WIDGET_BASE(bLSFfilterX, /ROW)
  lbl=WIDGET_LABEL( bCutLSFWX, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1, /NO_COPY)
  txtCutLSFWX=WIDGET_TEXT( bCutLSFWX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bMTFroiSzX=WIDGET_BASE(bMTFX2, /ROW)
  lbl=WIDGET_LABEL(bMTFroiSzX, VALUE='ROI width x height (mm)',FONT=font1, /NO_COPY)
  txtMTFroiSzX=WIDGET_TEXT(bMTFroiSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bMTFroiSzX, VALUE=' x ',FONT=font1, /NO_COPY)
  txtMTFroiSzY=WIDGET_TEXT(bMTFroiSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  btnMTFX=WIDGET_BUTTON(bMTFX2, VALUE='Calculate MTF', UVALUE='MTFX',FONT=font1)
  bMTFX3=WIDGET_BASE(bMTFX, /COLUMN)
  cw_plotMTFX=CW_BGROUP(bMTFX3, ['Edge position', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTF',FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  cw_tableMTFX=CW_BGROUP(bMTFX3, ['Analytical','Discrete'], /EXCLUSIVE, LABEL_TOP='Show table results from...', /FRAME, UVALUE='cw_tableMTF', FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)

  ;----------------User defined ROI------------
  bROIXsett=WIDGET_BASE(bROIX, /ROW)
  bROIXlft=WIDGET_BASE(bROIXsett, /COLUMN)
  typeROIX=CW_BGROUP(bROIXlft, ['Circular','Rectangluar','Rectangular rotated'], /EXCLUSIVE, LABEL_TOP='ROI shape', /FRAME, UVALUE='typeROIX',FONT=font1)

  bOffsetROIX=WIDGET_BASE(bROIXlft, /COLUMN)
  bTitOffsetROIX=WIDGET_BASE(bOffsetROIX, /ROW)
  lbl = WIDGET_LABEL(bTitOffsetROIX, VALUE='Extra offset ', FONT=font0, /NO_COPY)
  getOffsetROIX=WIDGET_BUTTON(bTitOffsetROIX, VALUE=thisPath+'images\arrow.bmp',/BITMAP,UVALUE='setOffset', TOOLTIP='Sets offset corresponding to the position of the last mouseclick in image')
  bDeltaO_ROIX=WIDGET_BASE(bOffsetROIX,/ROW)
  lbl=WIDGET_LABEL(bDeltaO_ROIX, VALUE='dx,dy: ', XSIZE=40, FONT=font1, /NO_COPY)
  lblDeltaO_ROIX=WIDGET_LABEL(bDeltaO_ROIX, VALUE=STRING(offxyROIX(0), FORMAT='(i0)')+','+STRING(offxyROIX(1), FORMAT='(i0)'), XSIZE=70, FONT=font1)
  unitDeltaO_ROI_X=CW_BGROUP(bOffsetROIX, ['pix','mm'], /EXCLUSIVE, LABEL_TOP='Unit extra offset', /FRAME, UVALUE='setOffset_unit', SPACE=-2, YPAD=0, COLUMN=2, FONT=font1)

  bROIXrgt=WIDGET_BASE(bROIXsett, /COLUMN)
  bROIXrad=WIDGET_BASE(bROIXrgt, /ROW)
  lbl = WIDGET_LABEL(bROIXrad, VALUE='Radius if circular ROI (mm)',FONT=font1, /NO_COPY)
  txtROIXrad = WIDGET_TEXT(bROIXrad, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIXx=WIDGET_BASE(bROIXrgt, /ROW)
  lbl = WIDGET_LABEL(bROIXx, VALUE='X size if rectangular ROI (mm)',FONT=font1, XSIZE=170, /NO_COPY)
  txtROIXx = WIDGET_TEXT(bROIXx, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIXy=WIDGET_BASE(bROIXrgt, /ROW)
  lbl = WIDGET_LABEL(bROIXy, VALUE='Y size if rectangular ROI (mm)',FONT=font1, XSIZE=170, /NO_COPY)
  txtROIXy = WIDGET_TEXT(bROIXy, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIXa=WIDGET_BASE(bROIXrgt, /ROW)
  lbl = WIDGET_LABEL(bROIXa, VALUE='Rotation rectangular ROI (degrees)',FONT=font1, XSIZE=195, /NO_COPY)
  txtROIXa = WIDGET_TEXT(bROIXa, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  btnROIX=WIDGET_BUTTON(bROIXrgt, VALUE='Get ROI values', UVALUE='ROI_X',FONT=font1)

  ;----------------NPS------------------
  bNPSroiSzX=WIDGET_BASE(bNPSX, /ROW)
  lbl=WIDGET_LABEL(bNPSroiSzX, VALUE='ROI size (pix)',FONT=font1, /NO_COPY)
  txtNPSroiSzX=WIDGET_TEXT(bNPSroiSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bNPSsubSzX=WIDGET_BASE(bNPSX, /ROW)
  lbl=WIDGET_LABEL(bNPSsubSzX, VALUE='Subimage size (pix)',FONT=font1, /NO_COPY)
  txtNPSsubSzX=WIDGET_TEXT(bNPSsubSzX, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bNPSsubSzX, VALUE=' x ROI size = ',FONT=font1, /NO_COPY)
  lblNPSsubSzMMX=WIDGET_LABEL(bNPSsubSzX, VALUE='', XSIZE=20, SCR_XSIZE=20, FONT=font1)
  lbl=WIDGET_LABEL(bNPSsubSzX, VALUE=' mm',FONT=font1, /NO_COPY)
  bNPStotPixX=WIDGET_BASE(bNPSX, /ROW)
  lbl=WIDGET_LABEL(bNPStotPixX, VALUE='# independent pixels/image (preferrably 4 mill in total): ',FONT=font1, /NO_COPY)
  lblNPStotPixX=wIDGET_LABEL(bNPStotPixX, VALUE='', /DYNAMIC_RESIZE,FONT=font1)

  bNPSbtnsX=WIDGET_BASE(bNPSX, /ROW)
  btnNPSX=WIDGET_BUTTON(bNPSbtnsX, VALUE='Calculate NPS', UVALUE='NPS',FONT=font1)

  ;----------------Variance image------------------
  lbl=WIDGET_LABEL(bVariX,VALUE='', YSIZE=15, /NO_COPY)

  lbl=WIDGET_LABEL(bVariX, VALUE='The variance image can reveal artifacts in the image. Adjust ROI size to find artifacts of different sizes.',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bVariX,VALUE='', YSIZE=15, /NO_COPY)
  bVariROI=WIDGET_BASE(bVariX, /ROW)
  lbl=WIDGET_LABEL(bVariROI, VALUE='ROI size (mm)', FONT=font1, /NO_COPY)
  txtVarImageROIsz=WIDGET_TEXT(bVariROI, VALUE='2.0', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bVariROI, VALUE=' if less than 3 pix, 3 pix is used', FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bVariX,VALUE='', YSIZE=15, /NO_COPY)

  bVarianceBtnsX=WIDGET_BASE(bVariX,/ROW)
  btnVarImageX=WIDGET_BUTTON(bVarianceBtnsX, VALUE='Calculate variance image(s)', UVALUE='varImage',FONT=font1)
  lbl=WIDGET_LABEL(bVarianceBtnsX,VALUE='', XSIZE=10, /NO_COPY)
  lblProgressVarX=WIDGET_LABEL(bVarianceBtnsX, VALUE=' ', XSIZE=200, /SUNKEN_FRAME)

  ;***********************NM planar tests**********************************************************
  bUniformity=WIDGET_BASE(wtabAnalysisNM, Title='1. Uniformity', /COLUMN)
  bSNI=WIDGET_BASE(wtabAnalysisNM, Title='2. SNI', /COLUMN)
  bAcq=WIDGET_BASE(wtabAnalysisNM, Title='3. Header info', /COLUMN)
  bBar=WIDGET_BASE(wtabAnalysisNM, Title='4. BarPhantom', /COLUMN)
  bEnergySpec=WIDGET_BASE(wtabAnalysisNM, TITLE='Energy spectrum', /COLUMN)
  ;bHomogNM=WIDGET_BASE(wtabAnalysisNM, Title='Uniformity', /COLUMN)
  bScanSpeed=WIDGET_BASE(wtabAnalysisNM, Title='Scan Speed', /COLUMN)
  bMTFNM=WIDGET_BASE(wtabAnalysisNM, TITLE='MTF',/Column)
  bGeomMean=WIDGET_BASE(wtabAnalysisNM, TITLE='GeomMean',/Column)
  ;bTimeActCurve=WIDGET_BASE(wtabAnalysisNM, TITLE='TimeAct',/Column)

  ;----------------Uniformity------------------
  lbl=WIDGET_LABEL(bUniformity, VALUE='', SCR_YSIZE=5,/NO_COPY)
  lbl=WIDGET_LABEL(bUniformity, VALUE='Based on NEMA NU-1 2007',FONT=font1, /ALIGN_LEFT,/NO_COPY)
  lbl=WIDGET_LABEL(bUniformity, VALUE='', SCR_YSIZE=5)
  
  bUniformity_row=WIDGET_BASE(bUniformity,/ROW)
  bUniformity_lft=WIDGET_BASE(bUniformity_row,/COLUMN)

  bUnifAreaRatio=WIDGET_BASE(bUniformity_lft, /ROW)
  lbl=WIDGET_LABEL(bUnifAreaRatio, VALUE='UFOV ratio of image (nonzero part of image)', FONT=font1, /NO_COPY)
  txtUnifAreaRatio=WIDGET_TEXT(bUnifAreaRatio, VALUE='', XSIZE=5, /EDITABLE, /KBRD_FOCUS_EVENTS, FONT=font1)
  
  ;distance correction
  bUnifDistCorr=WIDGET_BASE(bUniformity_lft, /COLUMN, /FRAME)
  bSetUnifCorr=WIDGET_BASE(bUnifDistCorr, /NONEXCLUSIVE, /COLUMN, YSIZE=22)
  btnUnifCorr=WIDGET_BUTTON(bSetUnifCorr, VALUE='Correct for point-source curvature', UVALUE='unifCorrSet', FONT=font1)
  bposfitUnifCorr=WIDGET_BASE(bUnifDistCorr, /ROW)
  lbl=WIDGET_LABEL(bposfitUnifCorr, VALUE='', XSIZE=20, /NO_COPY) 
  cw_posfitUnifCorr=CW_BGROUP(bposfitUnifCorr, ['x','y'],/NONEXCLUSIVE, UVALUE='cw_posfitUnifCorr', SET_VALUE=[0,0],  LABEL_LEFT='Avoid fitting source position in', /FRAME, FONT=font1, COLUMN=2, SPACE=-2, YPAD=0)
  bradfitUnifCorr=WIDGET_BASE(bUnifDistCorr, /ROW)
  lbl=WIDGET_LABEL(bradfitUnifCorr, VALUE='', XSIZE=20, /NO_COPY)
  bLockRadUnifCorr=WIDGET_BASE(bradfitUnifCorr, /NONEXCLUSIVE, /COLUMN)
  btnLockRadUnifCorr=WIDGET_BUTTON(bLockRadUnifCorr,VALUE='Lock radius',UVALUE='unifCorrLockRad',FONT=font1)
  lbl=WIDGET_LABEL(bradfitUnifCorr, VALUE=' to ', /NO_COPY)
  txtLockRadUnifCorr=WIDGET_TEXT(bradfitUnifCorr, VALUE='', XSIZE=5, SCR_YSIZE=20, /EDITABLE, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bradfitUnifCorr, VALUE='mm', /NO_COPY)

  bUniformity_rgt=WIDGET_BASE(bUniformity_row,/COLUMN)
  cw_plotUnif=CW_BGROUP(bUniformity_rgt, ['Uniformity result for all images','Curvature fit','Curvature correction check'],/EXCLUSIVE, UVALUE='cw_plotUnif', SET_VALUE=0,  LABEL_TOP='Plot result', /FRAME, FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  cw_imgUnif=CW_BGROUP(bUniformity_rgt, ['Curvature corr. image','Processed image (pix 6.4mm, smoothed, corrected)'],/EXCLUSIVE, UVALUE='cw_imgUnif', SET_VALUE=0, LABEL_TOP='Image result', /FRAME, FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)

  btnUnif=WIDGET_BUTTON(bUniformity_lft, VALUE='Calculate Uniformity', UVALUE='uniformityNM',FONT=font1)

  ;----------------SNI------------------
  lbl=WIDGET_LABEL(bSNI, VALUE='', SCR_YSIZE=5, /NO_COPY)
  lbl=WIDGET_LABEL(bSNI, VALUE='SNI = Structured Noise Index,(J Nucl Med 2014; 55:169-174)',FONT=font1, /ALIGN_LEFT, /NO_COPY)
  lbl=WIDGET_LABEL(bSNI, VALUE='', SCR_YSIZE=5, /NO_COPY)

  bSNI2=WIDGET_BASE(bSNI,/ROW)
  bSNI_Lft=WIDGET_BASE(bSNI2, /COLUMN)

  bSNIAreaRatio=WIDGET_BASE(bSNI_Lft, /ROW)
  lbl=WIDGET_LABEL(bSNIAreaRatio, VALUE='Ratio of nonzero part of image to be analyzed', FONT=font1, /NO_COPY)
  txtSNIAreaRatio=WIDGET_TEXT(bSNIAreaRatio, VALUE='', XSIZE=5, /EDITABLE, /KBRD_FOCUS_EVENTS, FONT=font1)

  ;distance correction
  bSNIDistCorr=WIDGET_BASE(bSNI_Lft, /COLUMN, FRAME=1)
  bSetSNICorr=WIDGET_BASE(bSNIDistCorr, /NONEXCLUSIVE, /COLUMN, YSIZE=22)
  btnSNICorr=WIDGET_BUTTON(bSetSNICorr, VALUE='Correct for point-source curvature', UVALUE='SNICorrSet', FONT=font1)
  bSNICorrParam=WIDGET_BASE(bSNIDistCorr, /ROW)
  lblCP=WIDGET_LABEL(bSNICorrParam, VALUE='', XSIZE=20)
  cw_posfitSNICorr=CW_BGROUP(bSNICorrParam, ['x','y'],/NONEXCLUSIVE, UVALUE='cw_posfitSNICorr', SET_VALUE=[0,0],  LABEL_LEFT='Avoid fitting source position in', /FRAME, FONT=font1, COLUMN=2, SPACE=-2, YPAD=0)

  bradfitSNICorr=WIDGET_BASE(bSNIDistCorr, /ROW)
  lbl=WIDGET_LABEL(bradfitSNICorr, VALUE='', XSIZE=20, /NO_COPY)
  bLockRadSNICorr=WIDGET_BASE(bradfitSNICorr, /NONEXCLUSIVE, /ROW)
  btnLockRadSNICorr=WIDGET_BUTTON(bLockRadSNICorr,VALUE='Lock radius',UVALUE='SNICorrLockRad',FONT=font1)
  lbl=WIDGET_LABEL(bradfitSNICorr, VALUE=' to ', /NO_COPY)
  txtLockRadSNICorr=WIDGET_TEXT(bradfitSNICorr, VALUE='', XSIZE=5, SCR_YSIZE=20,/EDITABLE, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bradfitSNICorr, VALUE='mm', /NO_COPY)

  bSNI_Rgt=WIDGET_BASE(bSNI2,/COLUMN)
  bHumVis=WIDGET_BASE(bSNI_Rgt, /COLUMN, FRAME=1)
  lbl=WIDGET_LABEL(bHumVis, VALUE='Human visual response filter = r^f*exp(-cr^2)', FONT=font1, /NO_COPY)
  bHumVis_f=WIDGET_BASE(bHumVis,/ROW)
  lbl=WIDGET_LABEL(bHumVis_f, VALUE='f: ', FONT=font1, /NO_COPY)
  txtSNI_f=WIDGET_TEXT(bHumVis_f, VALUE='', /EDITABLE, XSIZE=4, /KBRD_FOCUS_EVENTS, FONT=font1)
  ;bHumVis_c=WIDGET_BASE(bHumVis,/ROW)
  lbl=WIDGET_LABEL(bHumVis_f, VALUE='  c: ', FONT=font1, /NO_COPY)
  txtSNI_c=WIDGET_TEXT(bHumVis_f, VALUE='', /EDITABLE, XSIZE=4, /KBRD_FOCUS_EVENTS, FONT=font1)
  ;lbl=WIDGET_LABEL(bHumVis_c, VALUE='28 if view dist = 1.5m', FONT=font1, /NO_COPY)
  ;bHumVis_d=WIDGET_BASE(bHumVis,/ROW)
  lbl=WIDGET_LABEL(bHumVis_f, VALUE='  display (mm):', FONT=font1, /NO_COPY)
  txtSNI_d=WIDGET_TEXT(bHumVis_f, VALUE='', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1)
  ;lbl=WIDGET_LABEL(bHumVis_d, VALUE='mm', FONT=font1, /NO_COPY)

  bCW_SNI=WIDGET_BASE(bSNI_Rgt,/ROW)
  cw_plotSNI=CW_BGROUP(bCW_SNI, ['SNI values','Curves to calculate SNI'], /EXCLUSIVE, UVALUE='cw_plotSNI', LABEL_TOP='Plot result', /FRAME,FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  cw_imgSNI=CW_BGROUP(bCW_SNI, ['2d NPS','Curvature corr. image'],/EXCLUSIVE, UVALUE='cw_imgSNI', SET_VALUE=0, LABEL_TOP='Image result', /FRAME, FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)

  bSmoothNPS_SNI=WIDGET_BASE(bSNI_Rgt, /ROW)
  lbl=WIDGET_LABEL(bSmoothNPS_SNI, VALUE='Smooth NPS curve (mm-1)',FONT=font1, /NO_COPY)
  txtSmoothNPS_SNI=WIDGET_TEXT(bSmoothNPS_SNI, VALUE='0.010', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bfreqNPS_SNI=WIDGET_BASE(bSNI_Rgt, /ROW)
  lbl=WIDGET_LABEL(bfreqNPS_SNI, VALUE='Sampling frequency NPS curve (mm-1)',FONT=font1,/NO_COPY)
  txtfreqNPS_SNI=WIDGET_TEXT(bfreqNPS_SNI, VALUE='0.005', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  btnSNI=WIDGET_BUTTON(bSNI_Lft, VALUE='Calculate SNI', UVALUE='SNI',FONT=font1)

  ;----------------Acquisition params----------
  lblacqMl0=WIDGET_LABEL(bAcq, VALUE='', SCR_YSIZE=20)
  lblAcq=WIDGET_LABEL(bAcq, VALUE='Retrieve acquisition parameters (frame duration (ms), total counts)',FONT=font1)

  lblacqiMl2=WIDGET_LABEL(bAcq, VALUE='', SCR_YSIZE=20)
  btnAcq=WIDGET_BUTTON(bAcq, VALUE='Get acquisition parameters', UVALUE='AcqNM',FONT=font1)
  lbl = WIDGET_LABEL(bAcq, VALUE='',FONT=font1, /NO_COPY)
  lbl = WIDGET_LABEL(bAcq, VALUE='More DICOM elements can be added to output when using QuickTest and defining additional DICOM output.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bAcq, VALUE='Define output in Setting->QuickTest Output template and then connect the output template to a parameter set.',FONT=font1, /NO_COPY, /ALIGN_LEFT)

  ;----------------Bar phantom ----------
  lbl=WIDGET_LABEL(bBar, VALUE='', SCR_YSIZE=10, /NO_COPY)
  lbl=WIDGET_LABEL(bBar, VALUE='Retrieve MTF and FWHM from quadrant bar phantom. Based on: Hander et al. Med Phys 24 (2) 1997; 327-334',FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bBar, VALUE='', SCR_YSIZE=5, /NO_COPY)
  lbl=WIDGET_LABEL(bBar, VALUE='     MTF (f=1/(2*barwidth)) = SQRT ( 2 * ROIvariance - ROImean ) / ROImean', FONT= font1, /ALIGN_LEFT, /NO_COPY)
  lbl=WIDGET_LABEL(bBar, VALUE='     FWHM = barwidth * 4/pi * SQRT(LN(2)) * SQRT(LN(1/MTF))', FONT= font1, /ALIGN_LEFT, /NO_COPY)

  lbl=WIDGET_LABEL(bBar, VALUE='', SCR_YSIZE=10, /NO_COPY)
  lbl=WIDGET_LABEL(bBar, VALUE='ROIs sorted by variance to automatically find widest and narrowest bars.', FONT= font1, /ALIGN_LEFT, /NO_COPY)
  lbl=WIDGET_LABEL(bBar, VALUE='', SCR_YSIZE=10, /NO_COPY)


  lbl=WIDGET_LABEL(bBar, VALUE='Bar widths (mm):  ', FONT= font1, /ALIGN_LEFT, /NO_COPY)
  bBarWidths=WIDGET_BASE(bBar, /ROW)
  lbl=WIDGET_LABEL(bBarWidths, VALUE='     (widest) 1:', FONT= font1, /NO_COPY)
  txtBar1=WIDGET_TEXT(bBarWidths, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bBarWidths, VALUE='  2:', FONT= font1, /NO_COPY)
  txtBar2=WIDGET_TEXT(bBarWidths, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bBarWidths, VALUE='  3:', FONT= font1, /NO_COPY)
  txtBar3=WIDGET_TEXT(bBarWidths, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bBarWidths, VALUE='  4:', FONT= font1, /NO_COPY)
  txtBar4=WIDGET_TEXT(bBarWidths, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bBarWidths, VALUE='(narrowest)', FONT= font1, /NO_COPY)

  lbl=WIDGET_LABEL(bBar, VALUE='', SCR_YSIZE=10, /NO_COPY)

  bBarNMROI=WIDGET_BASE(bBar, /ROW)
  lbl=WIDGET_LABEL(bBarNMROI, VALUE='ROI diameter (mm)' ,FONT=font1, /NO_COPY)
  txtBarROIsize=WIDGET_TEXT(bBarNMROI, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  lbl=WIDGET_LABEL(bBar, VALUE='', SCR_YSIZE=10, /NO_COPY)
  btnBar=WIDGET_BUTTON(bBar, VALUE='Get results', UVALUE='BarNM',FONT=font1)

  ;------------energy spectrum--------------------
  lbl=WIDGET_LABEL(bEnergySpec, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bEnergySpecBtns=WIDGET_BASE(bEnergySpec, /ROW)
  btnLoadSpec=WIDGET_BUTTON(bEnergySpecBtns, VALUE='Load spectrum', UVALUE='loadSpectrum',FONT=font1)

  ;-----------Scan speed------------
  lbl=WIDGET_LABEL(bScanSpeed, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bAvgSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lbl=WIDGET_LABEL(bAvgSpeedNM, VALUE='Average over ROI with width (pix)' ,FONT=font1, /NO_COPY)
  txtNAvgSpeedNM=WIDGET_TEXT(bAvgSpeedNM, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bSpeedROIheight=WIDGET_BASE(bScanSpeed, /ROW)
  lbl=WIDGET_LABEL(bSpeedROIheight, VALUE='ROI heigth (cm)' ,FONT=font1, /NO_COPY)
  txtSpeedROIheight=WIDGET_TEXT(bSpeedROIheight, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bMedianSpeedNM=WIDGET_BASE(bScanSpeed, /ROW)
  lbl=WIDGET_LABEL(bMedianSpeedNM, VALUE='Median filter width (pix)',FONT=font1, /NO_COPY)
  txtScanSpeedMedian=WIDGET_TEXT(bMedianSpeedNM, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  btnPlotScanSpeed = WIDGET_BUTTON(bScanSpeed, VALUE='Plot y-profile and median filtered profile', UVALUE='plotScanSpeed',FONT=font1)

  ;----------------MTF------------------
  bMTFsettingsNM=WIDGET_BASE(bMTFNM, /ROW)
  bMTFlftNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)
  cw_typeMTFNM=CW_BGROUP(bMTFlftNM, ['Point','Line','Edge','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME,FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)

  bMTFrgtNM=WIDGET_BASE(bMTFsettingsNM,/COLUMN)

  bMTFroiSzNM=WIDGET_BASE(bMTFrgtNM, /ROW)
  lbl=WIDGET_LABEL(bMTFroiSzNM, VALUE='ROI width x height (mm)',FONT=font1, /NO_COPY)
  txtMTFroiSzXNM=WIDGET_TEXT(bMTFroiSzNM, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bMTFroiSzNM, VALUE='x',FONT=font1, /NO_COPY)
  txtMTFroiSzYNM=WIDGET_TEXT(bMTFroiSzNM, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bCutLSFNM=WIDGET_BASE(bMTFrgtNM, /NONEXCLUSIVE, /ROW)
  btnCutLSFNM=WIDGET_BUTTON(bCutLSFNM, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1)
  bCutLSFWNM=WIDGET_BASE(bMTFrgtNM, /ROW)
  lbl=WIDGET_LABEL( bCutLSFWNM, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1, /NO_COPY)
  txtCutLSFWNM=WIDGET_TEXT( bCutLSFWNM, VALUE='5', /EDITABLE, XSIZE=3, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bMTFrgtNM, VALUE='', YSIZE=20, /NO_COPY)

  bMTFsettingsNM2=WIDGET_BASE(bMTFsettingsNM, /COLUMN)
  cw_plotMTFNM=CW_BGROUP(bMTFsettingsNM2, ['Centered xy profiles', 'Line', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTF',FONT=font1, COLUMN=1, SPACE=-4, YPAD=0)
  ;cw_tableMTFNM=CW_BGROUP(bMTFsettingsNM2, ['Gaussian','Discrete'], /EXCLUSIVE, LABEL_TOP='Show table results from...', /FRAME, UVALUE='cw_tableMTF', FONT=font1, COLUMN=1, SPACE=-4, YPAD=0)

  bMTFbtnsNM=WIDGET_BASE(bMTFrgtNM, /ROW)
  ;btnMTFroiNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Show/update ROIs', UVALUE='drawMTFroi',FONT=font1)
  btnMTFNM=WIDGET_BUTTON(bMTFbtnsNM, VALUE='Calculate MTF', UVALUE='MTFNM',FONT=font1)

  ;------------------ Geom Mean -------------
  bGeomMeanSettings=WIDGET_BASE(bGeomMean, /COLUMN)
  lbl=WIDGET_LABEL(bGeomMeanSettings, VALUE='See geometric mean for selected frame in image results or save as matrix to be opened in ImageQC.',FONT=font1, /NO_COPY)

  btnSaveROI=WIDGET_BUTTON(bGeomMeanSettings, VALUE='Save Geometric mean images as matrix (.dat)', UVALUE='saveGeomMeanMatrix', FONT=font1)

  ;  ;--------------------Time activity curve
  ;  bTimeActSettings=WIDGET_BASE(bTimeActCurve, /COLUMN)
  ;  lbl=WIDGET_LABEL(bTimeActSettings, VALUE='',FONT=font1, /NO_COPY)
  ;  lbl=WIDGET_LABEL(bTimeActSettings, VALUE='Calculate time-activity curves for the ROI.',FONT=font1, /ALIGN_LEFT, /NO_COPY)
  ;  lbl=WIDGET_LABEL(bTimeActSettings, VALUE='',FONT=font1, /NO_COPY)
  ;  bROIbtns=WIDGET_BASE(bTimeActSettings, /ROW)
  ;  btnDefineROItimeAct=WIDGET_BUTTON(bROIbtns, VALUE='Define ROI', UVALUE='defROITimeAct', XSIZE=150, FONT=font1)
  ;  btnSaveROI=WIDGET_BUTTON(bROIbtns, VALUE='Save ROI', UVALUE='saveROITimeAct', XSIZE=150, FONT=font1)
  ;  btnCalcTimeActCurve=WIDGET_BUTTON(bTimeActSettings, VALUE='Calculate time-activity curve', UVALUE='timeActCurve', FONT=font1)

  ;***********************SPECT tests**********************************************************

  bMTF_SPECT=WIDGET_BASE(wtabAnalysisSPECT, TITLE='MTF',/Column)
  bRadialProfile=WIDGET_BASE(wtabAnalysisSPECT, Title='Radial Profile', /COLUMN)
  bContrastSPECT=WIDGET_BASE(wtabAnalysisSPECT, Title='Contrast', /COLUMN)

  ;----------------MTF------------------
  bMTFsettingsSPECT=WIDGET_BASE(bMTF_SPECT, /ROW)
  bMTFlftSPECT=WIDGET_BASE(bMTFsettingsSPECT,/COLUMN)
  cw_typeMTFSPECT=CW_BGROUP(bMTFlftSPECT, ['Point','Line','Edge','Circular edge'], /EXCLUSIVE, LABEL_TOP='MTF method...', /FRAME,FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)

  bMTFrgtSPECT=WIDGET_BASE(bMTFsettingsSPECT,/COLUMN)

  bMTFroiSzSPECT=WIDGET_BASE(bMTFrgtSPECT, /ROW)
  lbl=WIDGET_LABEL(bMTFroiSzSPECT, VALUE='ROI size (mm)',FONT=font1, /NO_COPY)
  txtMTFroiSzSPECT=WIDGET_TEXT(bMTFroiSzSPECT, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bCutLSFSPECT=WIDGET_BASE(bMTFrgtSPECT, /NONEXCLUSIVE, /ROW)
  btnCutLSFSPECT=WIDGET_BUTTON(bCutLSFSPECT, VALUE='Cut LSF tails', UVALUE='cutLSF',FONT=font1, /NO_COPY)
  bCutLSFWSPECT=WIDGET_BASE(bMTFrgtSPECT, /ROW)
  lbl=WIDGET_LABEL( bCutLSFWSPECT, VALUE='Cut LSF from halfmax (#FWHM)',FONT=font1, /NO_COPY)
  txtCutLSFWSPECT=WIDGET_TEXT( bCutLSFWSPECT, VALUE='5', /EDITABLE, XSIZE=3, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bMTFrgtSPECT, VALUE='', YSIZE=20, /NO_COPY)
  bMTF3dSPECT=WIDGET_BASE(bMTFrgtSPECT, /COLUMN, /NONEXCLUSIVE)
  MTF3dSPECT=WIDGET_BUTTON(bMTF3dSPECT, VALUE='Analyse 3d', UVALUE='MTF3dSPECT',FONT=font1)

  bMTFsettingsS=WIDGET_BASE(bMTFsettingsSPECT, /COLUMN)
  cw_plotMTFSPECT=CW_BGROUP(bMTFsettingsS, ['Centered xy profiles', 'Line', 'Sorted pixelvalues', 'LSF', 'MTF'], /EXCLUSIVE, LABEL_TOP='Show plot...', /FRAME, UVALUE='cw_plotMTF',FONT=font1, COLUMN=1, SPACE=-4, YPAD=0)
  ;cw_tableMTFSPECT=CW_BGROUP(bMTFsettingsS, ['Gaussian','Discrete'], /EXCLUSIVE, LABEL_TOP='Show table results from...', /FRAME, UVALUE='cw_tableMTF', FONT=font1, COLUMN=1, SPACE=-4, YPAD=0)

  bMTFbtnsSPECT=WIDGET_BASE(bMTFrgtSPECT, /ROW)
  ;btnMTFroiSPECT=WIDGET_BUTTON(bMTFbtnsSPECT, VALUE='Show/update ROIs', UVALUE='drawMTFroi',FONT=font1)
  btnMTFSPECT=WIDGET_BUTTON(bMTFbtnsSPECT, VALUE='Calculate MTF', UVALUE='MTFNM',FONT=font1)

  ;----------Radial profiles---------------
  lbl=WIDGET_LABEL(bRadialProfile, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bRadialProf=WIDGET_BASE(bRadialProfile, /COLUMN)
  bMedianRadialSPECT=WIDGET_BASE(bRadialProf, /ROW)
  lbl=WIDGET_LABEL(bMedianRadialSPECT, VALUE='Median filter width (pix)',FONT=font1, /NO_COPY)
  txtRadialMedian=WIDGET_TEXT(bMedianRadialSPECT, VALUE=STRING(5, FORMAT='(i0)'), /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1);config.radialFiltW
  btnRadialProfSPECT=WIDGET_BUTTON(bRadialProf, VALUE='Calculate radial profile', UVALUE='radialProfile',FONT=font1)

  ;-------------Contrast-------------
  lbl=WIDGET_LABEL(bContrastSPECT, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bConSettingsSPECT=WIDGET_BASE(bContrastSPECT, /ROW)
  lbl = WIDGET_LABEL(bConSettingsSPECT, VALUE='ROI radius (mm)', FONT=font1, /NO_COPY)
  txtConR1SPECT = WIDGET_TEXT(bConSettingsSPECT, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bConSettingsSPECT, VALUE='', XSIZE=20, FONT=font1, /NO_COPY)
  lbl = WIDGET_LABEL(bConSettingsSPECT, VALUE='Radius to ROIs (mm)', FONT=font1, /NO_COPY)
  txtConR2SPECT = WIDGET_TEXT(bConSettingsSPECT, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bConButtonsSPECT=WIDGET_BASE(bContrastSPECT, /ROW)
  ;btnConRoisSPECT=WIDGET_BUTTON(bConButtonsSPECT, VALUE='Show/update ROIs', UVALUE='drawConRoisSPECT',FONT=font1)
  btnContrastSPECT=WIDGET_BUTTON(bConButtonsSPECT, VALUE='Calculate contrast', UVALUE='contrastSPECT',FONT=font1)

  ;***********************PET tests**********************************************************

  bHomogPET=WIDGET_BASE(wtabAnalysisPET, TITLE='Uniformity', /COLUMN)
  bCross=WIDGET_BASE(wtabAnalysisPET, TITLE='Crosscalibration', /COLUMN)
  bRC=WIDGET_BASE(wtabAnalysisPET, TITLE='RC', /COLUMN)

  ;---------------Uniformity--------
  lbl=WIDGET_LABEL(bHomogPET, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl=WIDGET_LABEL(bHomogPET, VALUE='Calculate mean pixelvalues in a central and four peripheral ROIs and how this value varies over the slices.',FONT=font1, /NO_COPY)
  bHomogSizePET=WIDGET_BASE(bHomogPET, /ROW)
  lbl = WIDGET_LABEL(bHomogSizePET, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtHomogROIszPET = WIDGET_TEXT(bHomogSizePET, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl=WIDGET_LABEL(bHomogSizePET, VALUE='', XSIZE=20, FONT=font1, /NO_COPY)
  lbl = WIDGET_LABEL(bHomogSizePET, VALUE='Radius to ROIs (mm)',FONT=font1, /NO_COPY)
  txtHomogROIdistPET = WIDGET_TEXT(bHomogSizePET, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bHomogBtnsPET=WIDGET_BASE(bHomogPET, /ROW)
  ;btnHomogROIPET=WIDGET_BUTTON(bHomogBtnsPET, VALUE='Show/update ROI', UVALUE='drawROIhomog',FONT=font1)
  btnHomogPET=WIDGET_BUTTON(bHomogBtnsPET, VALUE='Calculate uniformity', UVALUE='homog',FONT=font1)

  ;------------Crosscalibration--------------------
  bCrossROI=WIDGET_BASE(bCross, /ROW)
  lbl = WIDGET_LABEL(bCrossROI, VALUE='ROI radius (mm)',FONT=font1, /NO_COPY)
  txtCrossROIsz = WIDGET_TEXT(bCrossROI, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  btnCrossGetAct = WIDGET_BUTTON(bCross, VALUE='Get administered activity and time from DICOM header', UVALUE='crossGetAct', FONT=font1)

  bCrossInput=WIDGET_BASE(bCross, /ROW)
  bCrossInputLft=WIDGET_BASE(bCrossInput, /COLUMN, FRAME=1, XSIZE=360)
  bCrossMeasA=WIDGET_BASE(bCrossInputLft, /ROW)
  lbl = WIDGET_LABEL(bCrossMeasA, VALUE='Before injection (MBq)',FONT=font1, /NO_COPY)
  txtCrossMeasAct = WIDGET_TEXT(bCrossMeasA, VALUE='0.0', /EDITABLE, XSIZE=7, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bCrossMeasA, VALUE='  time (hh:mm)',FONT=font1, /NO_COPY)
  txtCrossMeasActT = WIDGET_TEXT(bCrossMeasA, VALUE='00:00', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1)
  bCrossMeasR=WIDGET_BASE(bCrossInputLft, /ROW)
  lbl = WIDGET_LABEL(bCrossMeasR, VALUE='Rest after injection (MBq)',FONT=font1, /NO_COPY)
  txtCrossMeasRest = WIDGET_TEXT(bCrossMeasR, VALUE='0.0', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bCrossMeasR, VALUE='  time (hh:mm)',FONT=font1, /NO_COPY)
  txtCrossMeasRT = WIDGET_TEXT(bCrossMeasR, VALUE='00:00', /EDITABLE, XSIZE=5, /KBRD_FOCUS_EVENTS, FONT=font1)
  bCrossScanStart=WIDGET_BASE(bCrossInputLft, /ROW)
  lbl = WIDGET_LABEL(bCrossScanStart, VALUE='Scan started (found from first image)',FONT=font1, /NO_COPY)
  txtCrossScanStart = WIDGET_TEXT(bCrossScanStart, VALUE='', XSIZE=10, FONT=font1)
  bCrossScanAct=WIDGET_BASE(bCrossInputLft, /ROW)
  lbl = WIDGET_LABEL(bCrossScanAct, VALUE='Activity at start of scan (MBq)',FONT=font1, /NO_COPY)
  txtCrossScanAct = WIDGET_TEXT(bCrossScanAct, VALUE='', XSIZE=10, FONT=font1)
  bCrossInputRgt=WIDGET_BASE(bCrossInput, /COLUMN, FRAME=1, XSIZE=240)
  bCrossVol=WIDGET_BASE(bCrossInputRgt, /ROW)
  lbl = WIDGET_LABEL(bCrossVol, VALUE='Volume of container (mL)',FONT=font1, /NO_COPY)
  txtCrossVol = WIDGET_TEXT(bCrossVol, VALUE='' ,/EDITABLE,  XSIZE=10, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bCrossConc = WIDGET_BASE(bCrossInputRgt, /ROW)
  lbl = WIDGET_LABEL(bCrossConc, VALUE='Concentration (Bq/ml)',FONT=font1, /NO_COPY)
  txtCrossConc = WIDGET_TEXT(bCrossConc, VALUE='', XSIZE=10, FONT=font1)
  bCrossFactorPrev=WIDGET_BASE(bCrossInputRgt, /ROW)
  lbl = WIDGET_LABEL(bCrossFactorPrev, VALUE='Current calibration factor  ',FONT=font1, /NO_COPY)
  txtCrossFactorPrev = WIDGET_TEXT(bCrossFactorPrev, VALUE='1.000', XSIZE=5, SCR_YSIZE=20, /EDITABLE, /KBRD_FOCUS_EVENTS, FONT=font1)
  bCrossFactor=WIDGET_BASE(bCrossInputRgt, /ROW)
  lbl = WIDGET_LABEL(bCrossFactor, VALUE='New calibration factor  ',FONT=font1, /NO_COPY)
  txtCrossFactor = WIDGET_TEXT(bCrossFactor, VALUE='', XSIZE=5, SCR_YSIZE=20, FONT=font1)

  bCrossBtm=WIDGET_BASE(bCross, /ROW)
  bCrossBtns=WIDGET_BASE(bCrossBtm, /ROW)
  ;btnCrossROI=WIDGET_BUTTON(bCrossBtns, VALUE='Show/update ROI', UVALUE='drawROIcross',FONT=font1)
  btnCross=WIDGET_BUTTON(bCrossBtns, VALUE='Get ROI values', UVALUE='cross',FONT=font1)
  btnCrossUpdate = WIDGET_BUTTON(bCrossBtns, VALUE='Calculate calibration factor', UVALUE='updateCross',FONT=font1)

  ;-------------Recovery Coefficient-------------
  lbl=WIDGET_LABEL(bRC, VALUE='Assuming NEMA NU2 phantom centered in lung insert.', FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bRC, VALUE='Expecting 1 or 5 (marked) slices for the analysis. Rotation only affects the sphere ROIs.', FONT=font1, /NO_COPY)
  lbl=WIDGET_LABEL(bRC, VALUE='', SCR_YSIZE=10, /NO_COPY)
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
  cw_rcType=CW_BGROUP(bRCsett1, ['Mean A50', 'Max'], /EXCLUSIVE, LABEL_TOP='Find in spheres...', /FRAME, SET_VALUE=0, UVALUE='cw_rcType',FONT=font1, COLUMN=1, SPACE=-2, YPAD=0)
  cwRCexclude=CW_BGROUP(bRCsett, STRING(INDGEN(12)+1, FORMAT='(i0)'), COLUMN=3, SPACE=0,/NONEXCLUSIVE, LABEL_TOP='Exclude background ROI number...', UVALUE='rcBackExclude',FONT=font1, YPAD=0)

  bRCButtons=WIDGET_BASE(bRC, /ROW)
  btnRC=WIDGET_BUTTON(bRCButtons, VALUE='Calculate Recovery Coefficients', UVALUE='recovCoeff',FONT=font1)

  ;************************ MR tests *************
  bDcmMR=WIDGET_BASE(wtabAnalysisMR, Title='1. Header info', /COLUMN)
  bSNR_MR=WIDGET_BASE(wtabAnalysisMR, Title='2. SNR', /COLUMN)
  bPIU_MR=WIDGET_BASE(wtabAnalysisMR, Title='3. PIU', /COLUMN)
  bGhost_MR=WIDGET_BASE(wtabAnalysisMR, Title='4. Ghosting', /COLUMN)
  bGD_MR=WIDGET_BASE(wtabAnalysisMR, Title='5. GeomDist', /COLUMN)
  bSlice_MR=WIDGET_BASE(wtabAnalysisMR, Title='6. Slice thickness', /COLUMN)
  bROIMR=WIDGET_BASE(wtabAnalysisMR, Title='7. ROI', /COLUMN)

  lbl=WIDGET_LABEL(bDcmMR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl=WIDGET_LABEL(bDcmMR, VALUE='Retrieve Dicom header information (imaging frequency, recieve and transmit coil)',FONT=font1, /NO_COPY)

  lbl=WIDGET_LABEL(bDcmMR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  btnDcmMR=WIDGET_BUTTON(bDcmMR, VALUE='Get values', UVALUE='dcmMR',FONT=font1)
  lbl = WIDGET_LABEL(bDcmMR, VALUE='',FONT=font1, /NO_COPY)
  lbl = WIDGET_LABEL(bDcmMR, VALUE='More DICOM elements can be added to output when using QuickTest and defining additional DICOM output.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bDcmMR, VALUE='Define output in Setting->QuickTest Output template and then connect the output template to a parameter set.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  
  ;--------------------- SNR ------------------------------------  
  lbl = WIDGET_LABEL(bSNR_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl = WIDGET_LABEL(bSNR_MR, VALUE='Based on NEMA MS-1 2008 (SNR = S mean * sqrt(2) / stdev difference image) ',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSNR_MR, VALUE='Difference image is calculated from image2-image1, image4-image3 ... If some images are marked, only marked images will be counted.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSNR_MR, VALUE='If odd number of images, the last image will be ignored.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSNR_MR, VALUE='Results in table and result image will be linked to the first of each two images.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSNR_MR, VALUE='Center and size of phantom will be found from maximum x and y profiles.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  
  lbl = WIDGET_LABEL(bSNR_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bSNR_MR_ROI=WIDGET_BASE(bSNR_MR, /ROW)
  lbl = WIDGET_LABEL(bSNR_MR_ROI, VALUE='ROI % of circular phantom ',FONT=font1, /NO_COPY)
  txtSNR_MR_ROI  = WIDGET_TEXT(bSNR_MR_ROI, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bSNR_MR_btn=WIDGET_BASE(bSNR_MR, /ROW)
  btnSNR_MR=WIDGET_BUTTON(bSNR_MR_btn, VALUE='Calculate SNR', UVALUE='SNR_MR',FONT=font1)
  
  ;--------------------- PIU ------------------------------------
  lbl = WIDGET_LABEL(bPIU_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl = WIDGET_LABEL(bPIU_MR, VALUE='Based on NEMA MS-3 2008',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bPIU_MR, VALUE='Center and size of phantom will be found from maximum x and y profiles.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bPIU_MR, VALUE='Multiple locations with the same min or same max might occur. Only one position shown.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bPIU_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bPIU_MR_ROI=WIDGET_BASE(bPIU_MR, /ROW)
  lbl = WIDGET_LABEL(bPIU_MR_ROI, VALUE='ROI % of circular phantom ',FONT=font1, /NO_COPY)
  txtPIU_MR_ROI  = WIDGET_TEXT(bPIU_MR_ROI, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bPIU_MR_btn=WIDGET_BASE(bPIU_MR, /ROW)
  btnPIU_MR=WIDGET_BUTTON(bPIU_MR_btn, VALUE='Calculate PIU', UVALUE='PIU_MR',FONT=font1)
  btnPIU_MR_setMinMax=WIDGET_BUTTON(bPIU_MR_btn, VALUE='Set window level to [min,max] for this current image', UVALUE='PIU_MR_setMinMax',FONT=font1)
  
  ;--------------------- Ghosting Ratio ------------------------------------
  lbl = WIDGET_LABEL(bGhost_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl = WIDGET_LABEL(bGhost_MR, VALUE='Based on ACR MR Quality Control Manual, 2015',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGhost_MR, VALUE='Percent Signal Ghosting (PSG) = 100 * |(left + right)-(top+bottom)| / 2*center',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGhost_MR, VALUE='If optimized, center and size of phantom will be found from maximum x and y profiles.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGhost_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  bGhost_MR_ROI1=WIDGET_BASE(bGhost_MR, /ROW)
  lbl = WIDGET_LABEL(bGhost_MR_ROI1, VALUE='Radius of central ROI (mm) ',FONT=font1, /NO_COPY)
  txtGhost_MR_ROIszC  = WIDGET_TEXT(bGhost_MR_ROI1, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bGhost_MR_ROI1, VALUE='', XSIZE=20, /NO_COPY)
  bGhost_MR_optC=WIDGET_BASE(bGhost_MR_ROI1, /ROW, /NONEXCLUSIVE)
  ghost_MR_optC=WIDGET_BUTTON(bGhost_MR_optC, VALUE='Optimize center', UVALUE='ghost_MR_optC',FONT=font1)
  bGhost_MR_ROI2=WIDGET_BASE(bGhost_MR, /ROW)
  lbl = WIDGET_LABEL(bGhost_MR_ROI2, VALUE='Width and height of outer ROI (mm) ',FONT=font1, /NO_COPY)
  txtGhost_MR_ROIszW  = WIDGET_TEXT(bGhost_MR_ROI2, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  txtGhost_MR_ROIszH  = WIDGET_TEXT(bGhost_MR_ROI2, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bGhost_MR_ROI3=WIDGET_BASE(bGhost_MR, /ROW)
  lbl = WIDGET_LABEL(bGhost_MR_ROI3, VALUE='Distance from image border (mm) ',FONT=font1, /NO_COPY)
  txtGhost_MR_ROIszD  = WIDGET_TEXT(bGhost_MR_ROI3, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bGhost_MR_btn=WIDGET_BASE(bGhost_MR, /ROW)
  btnGhost_MR=WIDGET_BUTTON(bGhost_MR_btn, VALUE='Calculate Percent Signal Ghosting', UVALUE='Ghost_MR',FONT=font1)
  
  ;--------------------- Geometric Distortion ------------------------------------
  lbl = WIDGET_LABEL(bGD_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)
  lbl = WIDGET_LABEL(bGD_MR, VALUE='Based on NEMA MS-2 2008',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGD_MR, VALUE='Geometric distortion = 100 * ABS(measured width - true width) / true width',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGD_MR, VALUE='Measured at 0, 90, 45, 135 degrees',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGD_MR, VALUE='Center and width of phantom will be found from maximum profiles.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGD_MR, VALUE='NB: Slice with grid not a good option for these measurements. Use a homogeneous slice.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bGD_MR, VALUE='', SCR_YSIZE=20, /NO_COPY)

  bGD_MR_act=WIDGET_BASE(bGD_MR, /ROW)
  lbl = WIDGET_LABEL(bGD_MR_act, VALUE='Actual width of phantom (mm) ',FONT=font1, /NO_COPY)
  txtGD_MR_act  = WIDGET_TEXT(bGD_MR_act, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bGD_MR_act, VALUE='A red circle of this width will be overlayed the image when calculated.',FONT=font1, /NO_COPY)

  bGD_MR_btn=WIDGET_BASE(bGD_MR, /ROW)
  btnGD_MR=WIDGET_BUTTON(bGD_MR_btn, VALUE='Calculate geometric distortion', UVALUE='GD_MR',FONT=font1)
  
  ;--------------------- Slice thickness MR ------------------------------------
  lbl = WIDGET_LABEL(bSlice_MR, VALUE='', SCR_YSIZE=5, /NO_COPY)
  lbl = WIDGET_LABEL(bSlice_MR, VALUE='Based on NEMA MS-5 2018 and ACR MR Quality Control Manual, 2015',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSlice_MR, VALUE='Slice thickness = tan (wedge angle) * FWHM',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSlice_MR, VALUE='Slice thickness ACR = 1/10 * harmonic mean of FWHM upper and lower = 0.2 * upper * lower / (upper + lower)',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSlice_MR, VALUE='FWHM will be calculated for the averaged profile within each ROI, max from medianfiltered profile.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSlice_MR, VALUE='If optimized, center of phantom will be found from maximum profiles.',FONT=font1, /NO_COPY, /ALIGN_LEFT)
  lbl = WIDGET_LABEL(bSlice_MR, VALUE='', SCR_YSIZE=5, /NO_COPY)

  bSlice_MR_TANA=WIDGET_BASE(bSlice_MR, /ROW)
  lbl = WIDGET_LABEL(bSlice_MR_TANA, VALUE='tan (wedge angle) ',FONT=font1, /NO_COPY)
  txtSlice_MR_TANA  = WIDGET_TEXT(bSlice_MR_TANA, VALUE='', /EDITABLE, XSIZE=5, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bSlice_MR_ROI2=WIDGET_BASE(bSlice_MR, /ROW)
  lbl = WIDGET_LABEL(bSlice_MR_ROI2, VALUE='Width and height of ROIs (mm) ',FONT=font1, /NO_COPY)
  txtSlice_MR_ROIszW  = WIDGET_TEXT(bSlice_MR_ROI2, VALUE='', /EDITABLE, XSIZE=6, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  txtSlice_MR_ROIszH  = WIDGET_TEXT(bSlice_MR_ROI2, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  lbl = WIDGET_LABEL(bSlice_MR_ROI2, VALUE='', SCR_XSIZE=20, /NO_COPY)
  bSlice_MR_optC=WIDGET_BASE(bSlice_MR_ROI2, /ROW, /NONEXCLUSIVE)
  slice_MR_optC=WIDGET_BUTTON(bSlice_MR_optC, VALUE='Optimize center', UVALUE='slice_MR_optC',FONT=font1)
  bSlice_MR_ROI3=WIDGET_BASE(bSlice_MR, /ROW)
  lbl = WIDGET_LABEL(bSlice_MR_ROI3, VALUE='Distance from center ROI upper and lower (mm) ',FONT=font1, /NO_COPY)
  txtSlice_MR_ROIszD  = WIDGET_TEXT(bSlice_MR_ROI3, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  txtSlice_MR_ROIszD2  = WIDGET_TEXT(bSlice_MR_ROI3, VALUE='', /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  bSlice_MR_btn=WIDGET_BASE(bSlice_MR, /ROW)
  btnSlice_MR=WIDGET_BUTTON(bSlice_MR_btn, VALUE='Calculate slice thickness', UVALUE='Slice_MR',FONT=font1)
  btnSlice_MR_WL=WIDGET_BUTTON(bSlice_MR_btn, VALUE='Set window level to [min,max] in ROIs', UVALUE='Slice_MR_WL',FONT=font1)
  
  ;----------------User defined ROI------------
  bROIMRsett=WIDGET_BASE(bROIMR, /ROW)
  bROIMRlft=WIDGET_BASE(bROIMRsett, /COLUMN)
  typeROIMR=CW_BGROUP(bROIMRlft, ['Circular','Rectangluar','Rectangular rotated'], /EXCLUSIVE, LABEL_TOP='ROI shape', /FRAME, UVALUE='typeROIMR',FONT=font1)

  bOffsetROIMR=WIDGET_BASE(bROIMRlft, /COLUMN)
  bTitOffsetROIMR=WIDGET_BASE(bOffsetROIMR, /ROW)
  lbl = WIDGET_LABEL(bTitOffsetROIMR, VALUE='Extra offset ', FONT=font0, /NO_COPY)
  getOffsetROIMR=WIDGET_BUTTON(bTitOffsetROIMR, VALUE=thisPath+'images\arrow.bmp',/BITMAP,UVALUE='setOffset', TOOLTIP='Sets offset corresponding to the position of the last mouseclick in image')
  bDeltaO_ROIMR=WIDGET_BASE(bOffsetROIMR,/ROW)
  lbl=WIDGET_LABEL(bDeltaO_ROIMR, VALUE='dx,dy: ', XSIZE=40, FONT=font1, /NO_COPY)
  lblDeltaO_ROIMR=WIDGET_LABEL(bDeltaO_ROIMR, VALUE=STRING(offxyROIMR(0), FORMAT='(i0)')+','+STRING(offxyROIMR(1), FORMAT='(i0)'), XSIZE=70, FONT=font1)
  unitDeltaO_ROI_MR=CW_BGROUP(bOffsetROIMR, ['pix','mm'], /EXCLUSIVE, LABEL_TOP='Unit extra offset', /FRAME, UVALUE='setOffset_unit', SPACE=-2, YPAD=0, COLUMN=2, FONT=font1)

  bROIMRrgt=WIDGET_BASE(bROIMRsett, /COLUMN)
  bROIMRrad=WIDGET_BASE(bROIMRrgt, /ROW)
  lbl = WIDGET_LABEL(bROIMRrad, VALUE='Radius if circular ROI (mm)',FONT=font1, /NO_COPY)
  txtROIMRrad = WIDGET_TEXT(bROIMRrad, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIMRx=WIDGET_BASE(bROIMRrgt, /ROW)
  lbl = WIDGET_LABEL(bROIMRx, VALUE='X size if rectangular ROI (mm)',FONT=font1, XSIZE=170, /NO_COPY)
  txtROIMRx = WIDGET_TEXT(bROIMRx, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIMRy=WIDGET_BASE(bROIMRrgt, /ROW)
  lbl = WIDGET_LABEL(bROIMRy, VALUE='Y size if rectangular ROI (mm)',FONT=font1, XSIZE=170, /NO_COPY)
  txtROIMRy = WIDGET_TEXT(bROIMRy, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)
  bROIMRa=WIDGET_BASE(bROIMRrgt, /ROW)
  lbl = WIDGET_LABEL(bROIMRa, VALUE='Rotation rectangular ROI (degrees)',FONT=font1, XSIZE=195, /NO_COPY)
  txtROIMRa = WIDGET_TEXT(bROIMRa, VALUE='' , /EDITABLE, XSIZE=4, SCR_YSIZE=20, /KBRD_FOCUS_EVENTS, FONT=font1)

  btnROIMR=WIDGET_BUTTON(bROIMRrgt, VALUE='Get ROI values', UVALUE='ROI_MR',FONT=font1)
  
  ;******************************************************************************************
  ;********************* Result panel *********************************************************
  bPlot = WIDGET_BASE(bRgt, /COLUMN)

  wtabResult=WIDGET_TAB(bPlot, XSIZE=660, YSIZE=490, UVALUE='tabResults')
  bTableRes=WIDGET_BASE(wtabResult, TITLE='Table of results', /COLUMN, UVALUE='tabTableRes')
  bPlotRes=WIDGET_BASE(wtabResult, TITLE='Plot results', /COLUMN, UVALUE='tabPlotRes')
  bImageRes=WIDGET_BASE(wtabResult,TITLE='Image results', /COLUMN, UVALUE='tabImageRes')
  bTableSup=WIDGET_BASE(wtabResult, TITLE='Supplement table', /COLUMN, UVALUE='tabTableSup')

  ;----table-----------
  toolbarTable=WIDGET_BASE(bTableRes,/ROW,/TOOLBAR)
  toolCopyTbl=WIDGET_BUTTON(toolbarTable, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard', UVALUE='copyTbl')

  bResults = WIDGET_BASE(bTableRes, /COLUMN)
  resTab=WIDGET_TABLE(bResults, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], /NO_ROW_HEADERS, SCR_XSIZE=650, SCR_YSIZE=300, /ALL_EVENTS, FONT=font1, ALIGNMENT=1)

  ;----plot------------
  toolbarPlot=WIDGET_BASE(bPlotRes,/ROW,/TOOLBAR)
  toolCopyCurve=WIDGET_BUTTON(toolbarPlot, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy curve to clipboard', UVALUE='copyCurve')
  tooliPlot=WIDGET_BUTTON(toolbarPlot, VALUE='iPlot', TOOLTIP='Send curves to separate window with save and edit options', UVALUE='iPlot',FONT=font1)
  toolHintPlot=WIDGET_LABEL(toolbarPlot, VALUE='', /DYNAMIC_RESIZE, FONT=font1)
  bDrawPlot=WIDGET_BASE(bPlotRes, /ROW)
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
  drawImageRes  = WIDGET_DRAW(bImageRes, XSIZE=450, YSIZE=450, RETAIN=retainVal)

  ;-----suplement table---------
  toolbarTableSup=WIDGET_BASE(bTableSup,/ROW,/TOOLBAR)
  toolCopyTblSup=WIDGET_BUTTON(toolbarTableSup, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard', UVALUE='copyTbl')
  lblResultsSup=WIDGET_LABEL(bTableSup, VALUE='', /DYNAMIC_RESIZE, FONT=font1)
  bResultsSup = WIDGET_BASE(bTableSup, /COLUMN)
  resTabSup=WIDGET_TABLE(bResultsSup, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], /NO_ROW_HEADERS, SCR_XSIZE=650, SCR_YSIZE=300, FONT=font1, ALIGNMENT=1)

  ;****************** BOTTOM Panel
  bDir=WIDGET_BASE(bMain,/ROW)
  lblDirectory=WIDGET_LABEL(bDir, VALUE='Full path:  ',FONT=font1)
  lblDir=WIDGET_LABEL(bDir, VALUE='',xSIZE=winX-170, YSIZE=18, /SUNKEN_FRAME,FONT=font1)

  refreshParam, config, ''
  IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN fillQuickTempList, quickTemp.(0);pro in refreshParam.pro

  loadct, 0, /SILENT
  WIDGET_CONTROL, bMain, /REALIZE
  XMANAGER, 'ImageQC', bMain, /NO_BLOCK
  DEVICE, RETAIN=retainVal, DECOMPOSED=0

  WIDGET_CONTROL, drawLarge, GET_VALUE=iDrawPlot
  iDrawPlot.erase

end


