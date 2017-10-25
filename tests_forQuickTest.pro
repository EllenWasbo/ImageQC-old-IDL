;callable procedures for use with QuickTest

pro STPpix

  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  szROI=SIZE(stpROI, /DIMENSIONS)

  resArr=FLTARR(4,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'STP',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  errStatus=0
  FOR i=0, nImg-1 DO BEGIN
    IF markedArr(i) THEN BEGIN
      ;check if same size
      tempImg=readImg(structImgs.(i).filename, 0)
      imszTemp=SIZE(tempImg, /DIMENSIONS)
      IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
        maske=stpROI
        IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanHU, STDDEV=stddevHU, MASK=maske
        resArr(2,i)=meanHU & resArr(3,i)=stddevHU
      ENDIF ELSE errstatus=1
    ENDIF
    WIDGET_CONTROL, lblProgress, SET_VALUE='Progress pixel values: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
  ENDFOR
  WIDGET_CONTROL, lblProgress, SET_VALUE=''
  If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size as the first image.',/INFORMATION)
  stpRes=CREATE_STRUCT('table',resArr)
  results(testNmb)=1

end

pro getEI

  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  resArr=FLTARR(2,nImg)-1; mAs and EI

  testNmb=getResNmb(modality,'EI',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  FOR i=0, nImg-1 DO BEGIN
    IF markedArr(i) THEN BEGIN
      resArr[0,i]=structImgs.(i).mAs
      resArr[1,i]=structImgs.(i).EI
    ENDIF
  ENDFOR
  WIDGET_CONTROL, lblProgress, SET_VALUE=''
  eiRes=resArr
  results(testNmb)=1

end

pro noise

  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  IF N_ELEMENTS(noiseROI) LE 1 THEN updateROI, ANA='NOISE' 
  szROI=SIZE(noiseROI, /DIMENSIONS)

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'NOISE',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  totNoise=0.
  errstatus=0
  FOR i=0, nImg-1 DO BEGIN
    IF markedArr(i) THEN BEGIN
      ;check if same size
      IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
      imszTemp=SIZE(tempImg, /DIMENSIONS)
      IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
        maske=noiseROI
        IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
        resArr(0,i)=meanVal
        resArr(1,i)=stddevVal
        totNoise=totNoise+stddevVal
      ENDIF ELSE errstatus=1
    ENDIF
    WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
  ENDFOR
  WIDGET_CONTROL, lblProgress, SET_VALUE=''
  If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
  avgNoise=totNoise/TOTAL(markedArr)

  noiseRes=FLTARR(4,nImg)
  noiseRes[0:1,*]=resArr
  noiseRes[3,0]=avgNoise
  noiseRes[2,*]=100.0*(resArr[1,*]-avgNoise)/avgNoise

  CASE modality OF
    0: BEGIN
      noiseRes=FLTARR(4,nImg)
      noiseRes[0:1,*]=resArr
      noiseRes[3,0]=avgNoise
      noiseRes[2,*]=100.0*(resArr[1,*]-avgNoise)/avgNoise
    END
    1: BEGIN
      noiseRes=FLTARR(2,nImg)
      noiseRes[0:1,*]=resArr
    END
    ELSE:
  ENDCASE
  results(testNmb)=1
end

pro homog
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  IF N_ELEMENTS(noiseROI) LE 1 THEN updateROI, ANA='HOMOG'
  szROI=SIZE(homogROIs, /DIMENSIONS)

  resArr=FLTARR(szROI(2)*2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'HOMOG',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  errstatus=0
  FOR i=0, nImg-1 DO BEGIN
    IF markedArr(i) THEN BEGIN
      ;check if same size
      IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
      imszTemp=SIZE(tempImg, /DIMENSIONS)
      IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
        FOR r=0, szROI(2)-1 DO BEGIN
          maske=homogROIs[*,*,r]
          IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
          resArr(r,i)=meanVal
          resArr(r+5,i)=stddevVal
        ENDFOR
      ENDIF ELSE errstatus=1
    ENDIF
    WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'

  ENDFOR
  WIDGET_CONTROL, lblProgress, SET_VALUE=''
  If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
  homogRes=resArr
  results(testNmb)=1
end

pro mtfx
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  WIDGET_CONTROL, txtMTFroiSzX, GET_VALUE=ROIszX
  WIDGET_CONTROL, txtMTFroiSzY, GET_VALUE=ROIszY
  WIDGET_CONTROL, cw_formLSFX, GET_VALUE=formLSF
  WIDGET_CONTROL, txtCutLSFWX, GET_VALUE=cutLSFW

  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  testNmb=getResNmb(modality,'MTF',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF nFrames NE 0 THEN BEGIN
    tempImg=readImg(structImgs.(0).filename, 0)
    curPix=structImgs.(0).pix(0)
  ENDIF ELSE BEGIN
    tempImg=readImg(structImgs.(first).filename, 0)
    curPix=structImgs.(first).pix(0)
  ENDELSE

  szFirst=SIZE(tempImg, /DIMENSIONS)

  ROIszMM=FLOAT([ROIszX(0),ROIszY])/2.
  ROIsz=ROIszMM/curPix
  halfSz=szFirst/2
  x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
  y1=ROUND(halfSz(1)+dxya(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(1))

  subMatrix=FLTARR(x2-x1+1,y2-y1+1)

  IF N_ELEMENTS(stpRes) EQ 0 THEN stpRes=0

  errLogg=''
  FOR i=0, nImg-1 DO BEGIN
    IF markedArr(i) THEN BEGIN
      IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
      szThis=SIZE(tempImg, /DIMENSIONS)
      IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
        submatrix[*,*]=tempImg[x1:x2,y1:y2]
        MTF=calculateMTF_xray(submatrix, curPix, dxya[0:1],stpRes, formLSF, WIDGET_INFO(btnCutLSFX, /BUTTON_SET), FLOAT(cutLSFW(0)));, WIDGET_INFO(revProcMTFX, /BUTTON_SET))
        IF MTF.errMsg NE '' THEN errLogg=errLogg+'Warning image #'+STRING(i+1, FORMAT='(i0)')+'. '+MTF.errMsg+newline
      ENDIF ELSE BEGIN
        MTF=CREATE_STRUCT('empty',0)
        errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.'+newline
      ENDELSE

    ENDIF ELSE MTF=CREATE_STRUCT('empty',0)

    IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
  ENDFOR
  IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg)
  results(testNmb)=1
end

pro testtest
  COMMON VARI
  
end