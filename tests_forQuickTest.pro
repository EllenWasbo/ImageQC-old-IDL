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

;callable procedures for use with QuickTest

pro STPpix
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  testNmb=getResNmb(modality,'STP',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  IF TOTAL(markedArr) GT 0 THEN BEGIN

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    updateROI, ANA='STP', SEL=first
    szROI=SIZE(stpROI, /DIMENSIONS)
    resArr=FLTARR(4,nImg)-1; mean, stdev all circles

    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(activeImg, /DIMENSIONS)
        szROI=SIZE(stpROI, /DIMENSIONS)
        IF ~ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN updateROI, ANA='STP', SEL=i

        maske=stpROI
        IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanHU, STDDEV=stddevHU, MASK=maske
        resArr(2,i)=meanHU & resArr(3,i)=stddevHU

      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='STP progress pixel values: '+STRING(i*100./nImg, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    stpRes=CREATE_STRUCT('table',resArr)
    results(testNmb)=1

    redrawImg,0,1
  ENDIF
end

pro getHeaderInfo; CT
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  resArr=STRARR(4,nImg); kVp,mAs, CTDIvol, software

  testNmb=getResNmb(modality,'EXP',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  IF TOTAL(markedArr) GT 0 THEN BEGIN
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        resArr[0,i]=STRING(structImgs.(i).kVp, FORMAT=formatCode(structImgs.(i).kVp))
        resArr[1,i]=STRING(structImgs.(i).mAs, FORMAT=formatCode(structImgs.(i).mAs))
        resArr[2,i]=STRING(structImgs.(i).CTDIvol, FORMAT=formatCode(structImgs.(i).CTDIvol))
        resArr[3,i]=structImgs.(i).SWversion
      ENDIF
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    expRes=resArr
    results(testNmb)=1
  ENDIF
end

pro getExposure; Xray
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  resArr=STRARR(6,nImg); kVp,mAs, EI, DAP, SDD, detector id

  testNmb=getResNmb(modality,'EXP',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  IF TOTAL(markedArr) GT 0 THEN BEGIN
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        resArr[0,i]=STRING(structImgs.(i).kVp, FORMAT=formatCode(structImgs.(i).kVp))
        resArr[1,i]=STRING(structImgs.(i).mAs, FORMAT=formatCode(structImgs.(i).mAs))
        resArr[2,i]=STRING(structImgs.(i).EI, FORMAT=formatCode(structImgs.(i).EI))
        resArr[3,i]=STRING(structImgs.(i).DAP, FORMAT=formatCode(structImgs.(i).DAP))
        resArr[4,i]=STRING(structImgs.(i).SDD, FORMAT=formatCode(structImgs.(i).SDD))
        resArr[5,i]=structImgs.(i).DETECTORID
      ENDIF
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    expRes=resArr
    results(testNmb)=1
  ENDIF
end

pro getDCM_MR; get DICOM info for MR images
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  resArr=STRARR(3,nImg)

  testNmb=getResNmb(modality,'DCM',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  IF TOTAL(markedArr) GT 0 THEN BEGIN
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        resArr[0,i]=STRING(structImgs.(i).imgFreq, FORMAT='(f0.6)')
        resArr[1,i]=structImgs.(i).recCoilName
        resArr[2,i]=structImgs.(i).traCoilName
      ENDIF
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    expRes=resArr
    results(testNmb)=1
  ENDIF
end

pro noise
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'NOISE',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    updateROI, ANA='NOISE', SEL=first
    szROI=SIZE(noiseROI, /DIMENSIONS)

    totNoise=0.
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(activeImg, /DIMENSIONS)
        szROI=SIZE(noiseROI, /DIMENSIONS)
        IF ~ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN updateROI, ANA='NOISE', SEL=i

        maske=noiseROI
        IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
        resArr(0,i)=meanVal
        resArr(1,i)=stddevVal
        ;totNoise=totNoise+stddevVal

      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Noise progress: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    ;avgNoise=totNoise/TOTAL(markedArr)

    CASE modality OF
      0: BEGIN
        noiseRes=FLTARR(4,nImg)
        noiseRes[0:1,*]=resArr

        ;find and sort by series (same seriesNmb)
        serNo=!Null
        FOR im=0, nImg-1 DO serNo=[serNo,structImgs.(im).seriesNmb]
        ;find number of series
        nActIm=N_ELEMENTS(serNo)
        uniqSerNo=uniq(serNo)
        nSeries=N_ELEMENTS(uniqSerNo)
        ;loop through series
        FOR se=0, nSeries-1 DO BEGIN
          imInSeries=WHERE(serNo EQ serNo(uniqSerNo(se)), nIm)
          serArr=markedArr*0
          serArr(imInSeries)=1
          actImg=serArr*markedArr
          ids=WHERE(actImg EQ 1, nids)
          IF ids(0) NE -1 THEN BEGIN
            noiseRes[3,ids]=TOTAL(resArr[1,ids])/nids
            noiseRes[2,ids]=100.0*(resArr[1,ids]-noiseRes[3,ids(0)])/noiseRes[3,ids(0)]
          ENDIF
        ENDFOR

        ;noiseRes[3,0]=avgNoise
        ;noiseRes[2,*]=100.0*(resArr[1,*]-avgNoise)/avgNoise
      END
      1: BEGIN
        noiseRes=FLTARR(2,nImg)
        noiseRes[0:1,*]=resArr
      END
      ELSE:
    ENDCASE
    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF
end

pro HUwater
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'HUWATER',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    updateROI, ANA='HUWATER', SEL=first
    szROI=SIZE(HUwaterROI, /DIMENSIONS)

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(activeImg, /DIMENSIONS)
        szROI=SIZE(HUwaterROI, /DIMENSIONS)
        IF ~ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN updateROI, ANA='HUWATER', SEL=i

        maske=HUwaterROI
        IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
        resArr(0,i)=meanVal
        resArr(1,i)=stddevVal
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='HU water progress: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    HUwaterRes=resArr
    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF

end

pro homog
  COMPILE_OPT hidden
  COMMON VARI

  nImg=N_TAGS(structImgs)

  testNmb=getResNmb(modality,'HOMOG',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    updateROI, ANA='HOMOG', SEL=first
    szROI=SIZE(homogROIs, /DIMENSIONS)
    resArr=FLTARR(szROI(2)*2,nImg)-1; mean, stdev all circles

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      WIDGET_CONTROL, /HOURGLASS
      IF markedArr(i) THEN BEGIN
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(activeImg, /DIMENSIONS)
        szROI=SIZE(homogROIs, /DIMENSIONS)
        updateROI, ANA='HOMOG', SEL=i;IF ~ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) OR cc EQ 0 THEN updateROI, ANA='HOMOG'

        FOR r=0, szROI(2)-1 DO BEGIN
          maske=homogROIs[*,*,r]
          IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
          resArr(r,i)=meanVal
          resArr(r+5,i)=stddevVal
        ENDFOR

      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Homogeneity progress: '+STRING(i*100./nI, FORMAT='(i0)')+' %'

    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    homogRes=resArr
    results(testNmb)=1

    redrawImg,0,1
  ENDIF
end; homog

pro slicethick
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  WIDGET_CONTROL, txtRampLen, GET_VALUE=len
  WIDGET_CONTROL, txtRampBackG, GET_VALUE=rampBackG
  WIDGET_CONTROL, txtRampSearch, GET_VALUE=nSearch
  WIDGET_CONTROL, txtRampAverage, GET_VALUE=nAvg
  WIDGET_CONTROL, txtRampDist, GET_VALUE=rampDist
  rampDistPix=FLOAT(rampDist(0))
  WIDGET_CONTROL, txtRampLen, GET_VALUE=len
  IF dxya(3) EQ 1 THEN imgCenterOffset=dxya ELSE imgCenterOffset=[0,0,0,0]
  WIDGET_CONTROL, cw_ramptype, GET_VALUE=ramptype
  WIDGET_CONTROL, cw_rampDens, GET_VALUE=rampdens

  nSearch=LONG(nSearch(0))
  nAvg=LONG(nAvg(0))

  resArr=FLTARR(7,nImg)
  testNmb=getResNmb(modality,'SLICETHICK',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    daRad=dxya(3)*dxya(2)/!radeg

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    errLogg=''
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN

        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        pix=structImgs.(i).pix
        resArr[0,i]=structImgs.(i).sliceThick

        sizeThis=SIZE(tempImg,/DIMENSIONS)
        lenPix=ROUND(FLOAT(len(0))/pix(0)); length in pixels
        nPixBackG=ROUND(rampBackG(0)/pix(0))

        nRamps=4
        sta=0
        CASE ramptype OF
          0: ramps=getRamps(sizeThis, imgCenterOffset, rampDistPix/pix(0), lenPix)
          1: BEGIN; beaded ramp CatPhan
            ramps=getRamps(sizeThis, imgCenterOffset, 45./pix(0), lenPix)
            ramps2=getRamps(sizeThis, imgCenterOffset, 25./pix(0), lenPix)
            nRamps=6
          END
          2: BEGIN; vertical beaded ramps only
            ramps=getRamps(sizeThis, imgCenterOffset, rampDistPix/pix(0), lenPix)
            sta=2
          END
          ELSE:ramps=getRamps(sizeThis, imgCenterOffset, rampDistPix/pix(0), lenPix)
        ENDCASE

        ;get line, FWHM and slice thickness
        FOR l=sta, nRamps-1 DO BEGIN
          IF l LE 3 THEN vec=getProfile(tempImg,ramps[0:1,l],ramps[2:3,l]) ELSE vec=getProfile(tempImg,ramps2[0:1,l-2],ramps2[2:3,l-2])

          IF nSearch GT 0 THEN BEGIN
            nLines=nSearch*2+1

            vecTemp=FLTARR(N_ELEMENTS(vec),nLines)
            FOR k=-nSearch, nSearch DO BEGIN
              IF l LE 1 THEN BEGIN
                vecTemp[*,k+nSearch]=getProfile(tempImg,[ramps(0,l),ramps(1,l)+k],[ramps(2,l),ramps(3,l)+k])
              ENDIF ELSE BEGIN
                IF l LE 3 THEN vecTemp[*,k+nSearch]=getProfile(tempImg,[ramps(0,l)+k,ramps(1,l)],[ramps(2,l)+k,ramps(3,l)])
                IF l GT 3 THEN vecTemp[*,k+nSearch]=getProfile(tempImg,[ramps2(0,l-2)+k,ramps2(1,l-2)],[ramps2(2,l-2)+k,ramps2(3,l-2)])
              ENDELSE
            ENDFOR
            vecSum=TOTAL(vecTemp,1)
            IF rampDens EQ 0 THEN maxminProf=WHERE(vecSum EQ max(vecSum)) ELSE maxminProf=WHERE(vecSum EQ min(vecSum))
            maxminProf=maxminProf(0)
            vec=vecTemp[*,maxminProf]
            IF nAvg GT 0 THEN BEGIN
              IF maxminProf-nAvg GE 0 AND maxminProf+nAvg LT nLines THEN vec=TOTAL(vecTemp[*,maxminProf-nAvg:maxminProf+nAvg],2)/(nAvg*2+1) $
              ELSE errLogg=errLogg+'Image '+STRING(i,FORMAT='(i0)')+', Line '+STRING(l-sta,FORMAT='(i0)')+': Found profile too close to border of search-area. Single profile with peak used (no averaging).'+newline
            ENDIF

          ENDIF

          szVec=SIZE(vec,/DIMENSIONS)
          IF nPixBackG GT szVec(0) THEN nPixBackG= szVec(0)
          ;find background
          bgVec=[vec[0:nPixBackG],vec[szVec(0)-nPixBackG:szVec(0)-1]]
          backGr=MEAN(bgVec)

          vecOrig=vec
          backGrOrig=backGr
          IF rampDens EQ 0 THEN peakVal=MAX(vec) ELSE BEGIN
            peakVal=MIN(vec)
            vec=-vec
            backGr=-backGr
          ENDELSE
          halfmax=0.5*(MAX(vec)+backGr); 0.5(max-bg)+bg = 0.5(max+bg)
          IF rampDens EQ 0 THEN halfmaxOrig=halfmax ELSE halfmaxOrig=0.5*(MIN(vecOrig)+backGrOrig)
          IF ramptype EQ 0 THEN BEGIN; wire ramp
            res=getWidthAtThreshold(vec, halfmax)
            resArr[l+1,i]=0.42*(res(0))*pix(0)/cos(daRad); sliceThickness=FWHM*0.42 according to Catphan manual
          ENDIF ELSE BEGIN; bead ramp
            ;find upper envelope curve
            derived=vec-shift(vec,1)
            subz=where(derived LT 0)-1
            dsubz=subz-shift(subz,1)
            ss=INTARR(n_elements(dsubz))
            ss(1)=1
            FOR s=2, n_elements(dsubz)-1 DO IF dsubz(s) GT 1 THEN ss(s)=1
            idxes=WHERE(ss EQ 1)
            idxmax=subz(idxes)
            vecInt=INTERPOL(vec(idxmax),idxmax, INDGEN(N_ELEMENTS(vec)));interpolate to regular stepsize
            res=getWidthAtThreshold(vecInt, halfmax)
            zinc=1.
            IF ramptype EQ 1 AND l GE 4 THEN zinc=.25;0.25mm z spacing between beads
            resArr[l+1,i]=zinc/2.0*(res(0)*pix(0)/cos(daRad)) ; 2mm axial spacing
          ENDELSE
          structTemp=CREATE_STRUCT('background',backGrOrig,'nBackGr',nPixBackG,'vector',vecOrig,'halfMax',halfMaxOrig,'firstLast',[res(1)-res(0)/2.,res(1)+res(0)/2.],'peakVal',peakVal)
          IF l EQ sta THEN lineStruct=CREATE_STRUCT('L'+STRING(l, FORMAT='(i0)'),structTemp) ELSE lineStruct=CREATE_STRUCT(lineStruct,'L'+STRING(l, FORMAT='(i0)'),structTemp)
        ENDFOR

      ENDIF ELSE lineStruct=CREATE_STRUCT('empty',0)
      IF i EQ 0 THEN sliceThickRes=CREATE_STRUCT('img0',lineStruct) ELSE sliceThickRes=CREATE_STRUCT(sliceThickRes,'img'+STRING(i,FORMAT='(i0)'),lineStruct)

      IF ramptype EQ 0 THEN BEGIN
        resArr[5,i]=MEAN(resArr[1:4,i])
        resArr[6,i]=100.0*(resArr[5,i]-resArr[0,i])/resArr[0,i]
      ENDIF

      WIDGET_CONTROL, lblProgress, SET_VALUE='Slice thickness progress: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
    errLogg=''

    sliceThickResTab=resArr
    results(testNmb)=1
  ENDIF
end;slicethick

pro ROI
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  resArr=FLTARR(2,nImg)-1; mean, stdev
  testNmb=getResNmb(modality,'ROI',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    updateROI, ANA='ROI', SEL=first
    szROI=SIZE(ROIroi, /DIMENSIONS)

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        updateROI, ANA='ROI', SEL=i

        maske=ROIroi
        IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
        resArr(0,i)=meanVal
        resArr(1,i)=stddevVal
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='ROI progress: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    ROIres=resArr

    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF
end; ROI

;CT
pro mtf
  COMPILE_OPT hidden
  COMMON VARI

  nImg=N_TAGS(structImgs)
  testNmb=getResNmb(modality,'MTF',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  nI=MIN([nImg,N_ELEMENTS(markedArr)])
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF first NE -1 THEN BEGIN
    WIDGET_CONTROL, /HOURGLASS
    WIDGET_CONTROL, cw_typeMTF, GET_VALUE=typeMTF
    WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=ROIszPix
    WIDGET_CONTROL, txtCutLSFW, GET_VALUE=cutLSFW
    WIDGET_CONTROL, txtCutLSFW2, GET_VALUE=cutLSFWf
    WIDGET_CONTROL, txtfreqMTF, GET_VALUE=sampFreq
    WIDGET_CONTROL, unitDeltaO_MTF_CT, GET_VALUE=unitOffset

    tempImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    pixFirst=structImgs.(first).pix(0)
    filtFirst=structImgs.(first).kernel

    szFirst=SIZE(tempImg, /DIMENSIONS)

    IF unitOffset THEN dxyaO=dxya[0:1]+offxyMTF/pixFirst ELSE dxyaO=dxya[0:1]+offxyMTF

    CASE typeMTF OF

      0: BEGIN; 2d bead

        ROIsz=ROUND(ROIszPix(0)/pixFirst)
        halfSz=SIZE(tempImg, /DIMENSIONS)/2
        x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz)
        y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz)

        IF x1 GE 0 AND x2 LT szFirst(0)-1 AND y1 GE 0 AND y1-(ROIsz(0)*2+1) GE 0 AND y2 LT szFirst(1)-1 AND y2-(ROIsz(0)*2+1) LT szFirst(1)-1 THEN roiOk=1 ELSE roiOk=0

        IF roiOK THEN BEGIN
          errBead=0
          errBeadClose=0
          errSize=0
          FOR i=0, nI-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              IF i GT 0 THEN BEGIN

                tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                curPix=structImgs.(i).pix(0)

              ENDIF ELSE curPix=pixFirst

              szImg=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(szImg[0:1], szFirst[0:1]) AND curPix EQ pixFirst THEN BEGIN
                IF WIDGET_INFO(btnSearchMaxMTF, /BUTTON_SET) THEN BEGIN
                  ;search for max in image
                  halfmax=0.5*(MAX(ABS(tempImg)));+MIN(tempImg))
                  centerPos=ROUND(centroid(tempImg, halfmax,0))
                  x1=centerPos(0)-ROIsz(0) & x2=centerPos(0)+ROIsz(0)
                  y1=centerPos(1)-ROIsz(0) & y2=centerPos(1)+ROIsz(0)
                  IF x1 GE 0 AND x2 LT szImg(0)-1 AND y1 GE 0 AND y1-(ROIsz(0)*2+1) GE 0 AND y2 LT szImg(1)-1 AND y2-(ROIsz(0)*2+1) LT szImg(1)-1 THEN roiOk=1 ELSE BEGIN
                    errBeadClose = errBeadClose+1
                    x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz)
                    y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz)
                  ENDELSE
                ENDIF
                submatrix=tempImg[x1:x2,y1:y2]
                backMatrix=tempImg[x1:x2,y1-(ROIsz(0)*2+1):y2-(ROIsz(0)*2+1)]

                MTF=calculateMTF(submatrix, curPix, dxyaO[0:1], typeMTF, backMatrix, WIDGET_INFO(btnCutLSF, /BUTTON_SET), FLOAT(cutLSFW(0)), FLOAT(cutLSFWf(0)), FLOAT(sampFreq(0)))
                IF MTF.status EQ 0 THEN errBead=errBead+1

              ENDIF ELSE BEGIN
                MTF=CREATE_STRUCT('empty',0)
                errSize=1
              ENDELSE
            ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
            IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
          ENDFOR
          IF errBeadClose GT 0 THEN sv=DIALOG_MESSAGE('ROI for bead too close to image border for '+STRING(errBeadClose, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. ROI center search ignored.', DIALOG_PARENT=evTop)
          IF errBead GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of bead for '+STRING(errBead, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Bead position assumed to be at the selected ROI center.', DIALOG_PARENT=evTop)
          If errSize THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
          results(testNmb)=1
        ENDIF ELSE sv=DIALOG_MESSAGE('ROIs outside image. Calculation not possible.',/INFORMATION, DIALOG_PARENT=evTop)
      END; bead

      1:BEGIN;wire
        pixFirst=structImgs.(first).pix(0)
        ROIsz=ROUND(ROIszPix(0)/pixFirst)
        halfSz=szFirst/2
        x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz)
        y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz)

        nnImg=TOTAL(markedArr)
        subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

        proceed=1
        counter=0

        IF WIDGET_INFO(btnSearchMaxMTF, /BUTTON_SET) THEN BEGIN
          ;generate sum of fullmatrix
          sumImg=tempImg*0.0
          FOR i=0, nI-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)

              szThis=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                sumImg=sumImg+tempImg
              ENDIF
            ENDIF
          ENDFOR

          IF TOTAL(sumImg) NE 0 THEN BEGIN
            halfmax=0.5*(MAX(sumImg)+MIN(sumImg))
            centerPos=ROUND(centroid(sumImg, halfmax))
            x1=centerPos(0)-ROIsz & x2=centerPos(0)+ROIsz
            y1=centerPos(1)-ROIsz & y2=centerPos(1)+ROIsz
          ENDIF
        ENDIF

        FOR i=0, nI-1 DO BEGIN
          IF markedArr(i) THEN BEGIN
            tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
            szThis=SIZE(tempImg, /DIMENSIONS)
            IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
              subM[*,*,counter]=tempImg[x1:x2,y1:y2]
              proceed=1
              counter=counter+1
            ENDIF ELSE BEGIN
              MTF=CREATE_STRUCT('empty',0)
              sv=DIALOG_MESSAGE('Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION, DIALOG_PARENT=evTop)
              proceed=0
            ENDELSE
          ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
          IF proceed EQ 0 THEN BREAK
        ENDFOR

        IF proceed THEN BEGIN
          MTFres=calculateMTF(subM, pixFirst, dxyaO[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSF, /BUTTON_SET), FLOAT(cutLSFW(0)), FLOAT(cutLSFWf(0)), FLOAT(sampFreq(0)))
          IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding location of line for one or more images. Location assumed to be at center of ROI.', DIALOG_PARENT=evTop)

          results(testNmb)=1
        ENDIF
      END; wire

      2: BEGIN ; circular edge (3d)

        tempImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
        pixFirst=structImgs.(first).pix(0)
        filtFirst=structImgs.(first).kernel

        ROIsz=ROUND(ROIszPix(0)/pixFirst)
        halfSz=szFirst/2
        x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz)
        y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz)

        nnImg=TOTAL(markedArr)
        subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

        IF WIDGET_INFO(btnSearchMaxMTF, /BUTTON_SET) THEN BEGIN
          ;generate sum of fullmatrix
          sumImg=tempImg*0.0
          FOR i=0, nI-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)

              szThis=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                sumImg=sumImg+tempImg
              ENDIF
            ENDIF
          ENDFOR

          IF TOTAL(sumImg) NE 0 THEN BEGIN
            ;firstguess: center = as defined, search for centroid within ROI size
            ;mx=MAX(sumImg, loc)
            ;ind=ARRAY_INDICES(sumImg, loc)
            ;xx1=ind(0)-ROIsz & xx2=ind(0)+ROIsz
            ;yy1=ind(1)-ROIsz & yy2=ind(1)+ROIsz
            subma=sumImg[x1:x2,y1:y2]
            centerPosSubma=ROUND(centroid(subma, MIN(subma)))
            centerPos=[x1,y1]+centerPosSubma
            x1=centerPos(0)-ROIsz & x2=centerPos(0)+ROIsz
            y1=centerPos(1)-ROIsz & y2=centerPos(1)+ROIsz
          ENDIF
        ENDIF

        filtStatus=1
        pixStatus=1
        proceed=1
        counter=0
        FOR i=0, nI-1 DO BEGIN
          IF markedArr(i) THEN BEGIN
            tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
            szThis=SIZE(tempImg, /DIMENSIONS)
            curPix=structImgs.(i).pix(0)
            curFilt=structImgs.(i).kernel
            IF curPix NE pixFirst THEN pixStatus=0
            IF curFilt NE filtFirst THEN filtStatus=0
            IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
              subM[*,*,counter]=tempImg[x1:x2,y1:y2]
              proceed=1
              counter=counter+1
            ENDIF ELSE BEGIN
              MTF=CREATE_STRUCT('empty',0)
              sv=DIALOG_MESSAGE('Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION, DIALOG_PARENT=evTop)
              proceed=0
            ENDELSE
          ENDIF
          IF proceed EQ 0 THEN BREAK
        ENDFOR

        IF proceed THEN BEGIN
          MTFres=calculateMTF(subM, pixFirst, dxyaO[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSF, /BUTTON_SET), FLOAT(cutLSFW(0)), FLOAT(cutLSFWf(0)), FLOAT(sampFreq(0)))
          IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for one or more images. Center of circle assumed to be at center of ROI.', DIALOG_PARENT=evTop)
          IF MTFres.status EQ 2 THEN sv=DIALOG_MESSAGE('Failed to fit LSF to gaussian. LSF used as is further. NB smoothed LSF!', DIALOG_PARENT=evTop)
          results(testNmb)=1
          IF filtStatus + pixStatus LT 2 THEN sv=DIALOG_MESSAGE('Pixelsize or kernel-type do not match for all images in selection.', DIALOG_PARENT=evTop)
        ENDIF
      END;circular edge 3d
      ELSE: sv=DIALOG_MESSAGE('Not implementet selected MTF type yet',/INFORMATION, DIALOG_PARENT=evTop)
    ENDCASE
  ENDIF; none selected
end

pro mtfx
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  WIDGET_CONTROL, txtMTFroiSzX, GET_VALUE=ROIszX
  WIDGET_CONTROL, txtMTFroiSzY, GET_VALUE=ROIszY
  WIDGET_CONTROL, cw_formLSFX, GET_VALUE=formLSF
  WIDGET_CONTROL, txtCutLSFWX, GET_VALUE=cutLSFW
  WIDGET_CONTROL, unitDeltaO_MTF_X, GET_VALUE=unitOffset

  nImg=N_TAGS(structImgs)
  testNmb=getResNmb(modality,'MTF',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF first NE -1 THEN BEGIN

    tempImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    curPix=structImgs.(first).pix(0)

    szFirst=SIZE(tempImg, /DIMENSIONS)
    IF unitOffset THEN dxyaO=dxya[0:1]+offxyMTF_X/curPix ELSE dxyaO=dxya[0:1]+offxyMTF_X
    ROIszMM=FLOAT([ROIszX(0),ROIszY])/2.
    ROIsz=ROIszMM/curPix
    halfSz=szFirst/2
    x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz(0))
    y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz(1))

    subMatrix=FLTARR(x2-x1+1,y2-y1+1)

    IF N_ELEMENTS(stpRes) EQ 0 THEN stpRes=0

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    errLogg=''
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        szThis=SIZE(tempImg, /DIMENSIONS)
        IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
          submatrix[*,*]=tempImg[x1:x2,y1:y2]
          MTF=calculateMTF_xray(submatrix, curPix, dxyaO[0:1],stpRes, formLSF, WIDGET_INFO(btnCutLSFX, /BUTTON_SET), FLOAT(cutLSFW(0)));, WIDGET_INFO(revProcMTFX, /BUTTON_SET))
          IF MTF.errMsg NE '' THEN errLogg=errLogg+'Warning image #'+STRING(i+1, FORMAT='(i0)')+'. '+MTF.errMsg+newline
        ENDIF ELSE BEGIN
          MTF=CREATE_STRUCT('empty',0)
          errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.'+newline
        ENDELSE

      ENDIF ELSE MTF=CREATE_STRUCT('empty',0)

      IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
    ENDFOR
    IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
    results(testNmb)=1
  ENDIF;none selected
end;mtfx

pro ctlin; CT
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  nMaterials=N_ELEMENTS(tableHeaders.CT.CTLIN.Alt1)
  resArr=FLTARR(nMaterials,nImg)-1

  testNmb=getResNmb(modality,'CTLIN',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    pix=structImgs.(first).pix
    WIDGET_CONTROL, txtLinROIrad, GET_VALUE=rad1
    WIDGET_CONTROL, txtLinROIradS, GET_VALUE=radS
    rad1=ROUND(FLOAT(rad1(0))/pix(0))
    radS=ROUND(FLOAT(radS(0))/pix(0))
    rad2=radS*2
    updateROI, ANA='CTLIN', SEL=first
    szROI=SIZE(CTlinROIs, /DIMENSIONS)
    resArr=FLTARR(szROI(2),nImg)-1;mean for all materials

    searchAvoid=WIDGET_INFO(btnLinAvoidSearch, /BUTTON_SET)
    CTlinROIpos=INTARR(2,szROI(2))
    errStatus=0
    errSearch=0
    imSum=FLTARR(szROI[0:1])
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(tempImg, /DIMENSIONS)
        IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
          imSum=imSum+tempImg
        ENDIF ELSE errStatus=1
      ENDIF;markedArr
      WIDGET_CONTROL, lblProgress, SET_VALUE='CT number progress - initializing: '+STRING(i*50./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    IF errStatus NE 1 THEN BEGIN
      imSum=imSum/TOTAL(markedArr)
      FOR r=0, szROI(2)-1 DO BEGIN
        searchMask=CTlinROIs[*,*,r]
        ;find center
        IMAGE_STATISTICS, imSum, MINIMUM=mini, MAXIMUM=maxi, MASK=searchMask
        halfMax=(mini+maxi)/2
        xarr=TOTAL(searchMask,2)
        yarr=TOTAL(searchMask,1)
        xnonZero=WHERE(xarr NE 0)
        ynonZero=WHERE(yarr NE 0)
        centerPos=-1
        IF searchAvoid EQ 0 THEN centerPos=ROUND(centroid(imSum[xnonZero(0):xnonZero(0)+rad2,ynonZero(0):ynonZero(0)+rad2], halfmax))
        IF MIN(centerPos) EQ -1 THEN BEGIN
          IF N_ELEMENTS(centerPos) EQ 2 THEN errSearch=errSearch+1
          centerPos=[radS,radS]
        ENDIF
        centerPos=centerPos+[xnonZero(0),ynonZero(0)]
        CTlinROIpos[*,r]=centerPos
      ENDFOR
      WIDGET_CONTROL, lblProgress, SET_VALUE='CT number progress - searching for center: 75 %'
      statMask=INTARR(szROI)
      FOR r=0, szROI(2)-1 DO statMask[*,*,r]=getSampleRois(szROI[0:1], [-szROI(0)/2,-szROI(1)/2,0,0], rad1, CTlinROIpos[*,r])
      WIDGET_CONTROL, lblProgress, SET_VALUE='CT number progress - calculating mean values: 90 %'
      FOR i=0, nImg-1 DO BEGIN
        IF markedArr(i) THEN BEGIN
          tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
          FOR r=0, szROI(2)-1 DO BEGIN
            thisMask=statMask[*,*,r]
            IMAGE_STATISTICS, tempImg, MEAN=meanHU, MASK=thisMask
            resArr(r,i)=meanHU
          ENDFOR
        ENDIF
      ENDFOR
    ENDIF;not errStatus
    WIDGET_CONTROL, lblProgress, SET_VALUE=' '
    If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
    IF errSearch THEN sv=DIALOG_MESSAGE('Failed searching center for one or more samples. Geometric center of search ROI is used for those.', DIALOG_PARENT=evTop)
    CTlinRes=resArr
    results(getResNmb(modality,analyse,analyseStringsAll))=1
    updateTable
    updatePlot, 1,1,0
    redrawImg,0,0
  ENDIF
end

;----------------
pro ringArt
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  resArr=FLTARR(2,nImg)-1

  testNmb=getResNmb(modality,'RING',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    WIDGET_CONTROL, txtRingMedian, GET_VALUE=medfilt
    medfilt=LONG(medfilt(0))
    WIDGET_CONTROL, txtRingSmooth, GET_VALUE=smfilt
    smfilt=LONG(smfilt(0))
    WIDGET_CONTROL, txtRingStart, GET_VALUE=startDist
    startDist=FLOAT(startDist(0))
    WIDGET_CONTROL, txtRingStop, GET_VALUE=stopDist
    stopDist=FLOAT(stopDist(0))
    WIDGET_CONTROL, cw_ringArtTrend, GET_VALUE=subType

    szMprev=[0,0]
    pixPrev=0
    sorting=!Null
    smoothSz=!Null
    ringArtProf=!Null
    ringROI=!Null
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        pix=structImgs.(i).pix(0)
        szM=SIZE(tempImg, /DIMENSIONS)
        ringROI=getROIcircle(szM, szM/2, stopDist/pix)-getROIcircle(szM, szM/2, startDist/pix)
        
        IF medfilt GT 0 THEN tempImg=MEDIAN(tempImg, medfilt)

        IF ~ARRAY_EQUAL(szMprev,szM) OR pix NE pixPrev THEN BEGIN;calculate sorting and distances
          distCenter=tempImg*0.0
          IF szM(0) MOD 2 EQ 0 THEN centerPos=0.5*szM[0:1]-0.5 ELSE centerPos=FLOOR(0.5*szM[0:1]); even numbered size - center between pixels
          FOR ii=0, szM(0)-1 DO BEGIN
            FOR jj=0, szM(1)-1 DO BEGIN
              distCenter(ii,jj)=SQRT((ii-centerPos(0))^2+(jj-centerPos(1))^2)
            ENDFOR
          ENDFOR
          sorting=SORT(distCenter)
          dists=distCenter(sorting)

          ;rebin to equally spaced resolution pix/10
          radius=MIN([centerPos(0),szM(0)-centerPos(0),centerPos(1), szM(1)-centerPos(1)])-1; maximim radius with full dataset
          newdists=FINDGEN(radius*10)*0.1*pix(0); regular x axis, cuts data at position where start to lose info due to less data in perpendicular directions
          pixNew=.1*pix(0)
          ;smooth by ~the new pix size if possible
          test=WHERE(dists*pix(0) LT max(newdists))
          smoothSz=ROUND(N_ELEMENTS(test)/N_ELEMENTS(newdists))
        ENDIF
        IF smoothSz GT 2 THEN pixVals=SMOOTH(tempImg(sorting),smoothSz) ELSE pixVals=tempImg(sorting)
        ;average over same dist close to center
        IF szM(0) MOD 2 EQ 0 THEN uEnd=21*21-1 ELSE uEnd=20*20-1;all different dists up to 7x7 matrix =0..48 when odd number sized matrix, up to 8x8 matrix when even numbered matrix     
        uniq_dists=dists(UNIQ(dists[0:uEnd]))
        radProf=FLTARR(N_ELEMENTS(uniq_dists))
        FOR r=0, N_ELEMENTS(uniq_dists)-1 DO BEGIN
          ids=WHERE(dists[0:uEnd] EQ uniq_dists(r), nIDs)
          radProf(r)=1./nIDS*TOTAL(pixVals(ids))
        ENDFOR
        distsComb=[uniq_dists,dists[uEnd+1:-1]]
        pixVals=[radProf,pixVals[uEnd+1:-1]]
        radialProf=INTERPOL(pixVals, distsComb*pix(0), newdists); linear interpolation
        IF smfilt GT 0 THEN radialProf=SMOOTH(radialProf, smfilt/pixNew, /EDGE_MIRROR)
        IF startDist GT 0 THEN BEGIN
          firstPixNo=WHERE(newdists GT startDist)
          radialProfShort=radialProf[firstPixNo(0):-1]
          newdistsShort=newdists[firstPixNo(0):-1]
          radialProf=radialProfShort
          newdists=newdistsShort
        ENDIF
        IF stopDist LT MAX(newdists) THEN BEGIN
          stopPixNo=WHERE(newdists GT stopDist)
          radialProfShort=radialProf[0:stopPixNo(0)-1]
          newdistsShort=newdists[0:stopPixNo(0)-1]
          radialProf=radialProfShort
          newdists=newdistsShort
        ENDIF
        
        yfit=!Null
        CASE subType OF
          0: a0=LINFIT(newdists, radialProf, YFIT=yfit)
          1: BEGIN
            IMAGE_STATISTICS, tempImg, MEAN=meanROI, MASK=ringROI
            yfit=radialProf*0.+meanROI
            END
        ENDCASE
          

        radialProfTrendRem=radialProf-yfit

        arr=FLTARR(N_ELEMENTS(radialProf),4)
        arr[*,0]=newdists
        arr[*,1]=radialProf
        arr[*,2]=yfit

        ringArtProf=CREATE_STRUCT(ringArtProf,'P'+STRING(i,FORMAT='(i0)'),arr)

        resArr(0,i)=MIN(radialProfTrendRem)
        resArr(1,i)=MAX(radialProfTrendRem)
        szMprev=szM
        pixPrev=pix(0)
      ENDIF ELSE ringArtProf=CREATE_STRUCT(ringArtProf,'P'+STRING(i,FORMAT='(i0)'),-1)
      WIDGET_CONTROL, lblProgress, SET_VALUE='Radial profile progress: '+STRING(i*100./TOTAL(markedArr), FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    ringArtRes=resArr
    results(testNmb)=1
  ENDIF
END
;-----------


pro uniformityNM
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)

  WIDGET_CONTROL, txtUnifAreaRatio, GET_VALUE=areaRatio
  areaRatio=FLOAT(areaRatio(0))

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'UNIF',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    resArr=FLTARR(7,nImg)-1
    errLogg=''
    adrToSave=!Null
    corrMatrix=!Null
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        sz=SIZE(tempImg, /DIMENSIONS)
        unifROI=getUnifRoi(tempImg, areaRatio)
        IF i EQ markedTemp(0) THEN BEGIN
          prevCurPix=0
          prevImgSize=sz
          corrMatrix=FLTARR(sz)+1
        ENDIF ELSE BEGIN
          prevCurPix=curPix
          prevImgSize=curImgSize
        ENDELSE
        curPix=structImgs.(i).pix(0)
        curImgSize=SIZE(tempImg, /DIMENSIONS)

        res=!Null
        ;correct for point source if at short distance
        IF WIDGET_INFO(btnUnifCorr, /BUTTON_SET) THEN BEGIN
          WIDGET_CONTROL, cw_posfitUnifCorr, GET_VALUE= corrPos
          IF WIDGET_INFO(btnLockRadUnifCorr, /BUTTON_SET) THEN WIDGET_CONTROL, txtLockRadUnifCorr, GET_VALUE=corrRadStr ELSE corrRadStr=''
          IF corrRadStr EQ '' THEN corrRad=-1 ELSE corrRad=FLOAT(corrRadStr)

          res=fitDistPointSource(tempImg, unifROI, curPix, [structImgs.(i).radius1,structImgs.(i).radius2],corrPos,corrRad)
          zeros=WHERE(tempImg EQ 0)
          tempImg=tempImg-res.corrSub
          tempImg(zeros)=0.
          resArr[4:5,i]=res.fitPos
          resArr[6,i]=res.fitDist

        ENDIF; correct?
        uR=calcUniformityNM(tempImg, unifROI, curPix,WIDGET_INFO(btnUnifCorr, /BUTTON_SET))
        IF SIZE(res, /TNAME) EQ 'STRUCT' THEN uR=CREATE_STRUCT(uR, 'fitData',res.fitData)
        resArr[0:3,i]=uR.table
        IF i EQ 0 THEN unifRes=CREATE_STRUCT('U0',uR) ELSE unifRes=CREATE_STRUCT(unifRes, 'U'+STRING(i, FORMAT='(i0)'), uR)
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Uniformity progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    If errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)

    results(testNmb)=1
    unifRes=CREATE_STRUCT('table',resArr,unifRes)
  ENDIF
end

pro sni
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)

  WIDGET_CONTROL, txtSNIAreaRatio, GET_VALUE=areaRatio
  areaRatio=FLOAT(areaRatio(0))
  WIDGET_CONTROL, txtSNI_f, GET_VALUE=f
  WIDGET_CONTROL, txtSNI_c, GET_VALUE=c
  WIDGET_CONTROL, txtSNI_d, GET_VALUE=d
  humVis_fcd=FLOAT([f(0),c(0),d(0)])
  WIDGET_CONTROL, txtSmoothNPS_SNI, GET_VALUE=sm
  WIDGET_CONTROL, txtfreqNPS_SNI, GET_VALUE=fr

  resArr=FLTARR(3,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'SNI',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    errLogg=''
    adrToSave=!Null
    corrMatrix=!Null
    nI=MIN([nImg,N_ELEMENTS(markedArr)])

    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        sz=SIZE(tempImg, /DIMENSIONS)
        SNIroi=getSNIroi(tempImg, areaRatio)

        IF i EQ markedTemp(0) THEN BEGIN
          prevCurPix=0
          prevImgSize=sz
          corrMatrix=FLTARR(sz)+1
        ENDIF ELSE BEGIN
          prevCurPix=curPix
          prevImgSize=curImgSize
        ENDELSE
        curPix=structImgs.(i).pix(0)
        curImgSize=SIZE(tempImg, /DIMENSIONS)

        ;correct for point source at distance shorter than 5 x UFOV
        IF WIDGET_INFO(btnSNICorr, /BUTTON_SET) THEN BEGIN
          WIDGET_CONTROL, cw_posfitSNICorr, GET_VALUE= corrPos
          IF WIDGET_INFO(btnLockRadSNICorr, /BUTTON_SET) THEN WIDGET_CONTROL,txtLockRadSNICorr, GET_VALUE=corrRadStr ELSE corrRadStr=''
          IF corrRadStr EQ '' THEN corrRad=-1 ELSE corrRad=FLOAT(corrRadStr)

          res=fitDistPointSource(tempImg, TOTAL(SNIroi,3), curPix, [structImgs.(i).radius1,structImgs.(i).radius2],corrPos,corrRad)

          corrMatrix=res.corrSub
          resArr[0:1,i]=res.fitPos
          resArr[2,i]=res.fitDist

        ENDIF; correct?
        IF N_ELEMENTS(SNIroi) GT 1 THEN BEGIN
          SNIthis=calculateSNI(tempImg, corrMatrix, SNIroi[*,*,0], curPix, humVis_fcd, FLOAT(sm(0)), FLOAT(fr(0)))
        ENDIF ELSE BEGIN
          SNIthis=CREATE_STRUCT('empty',0)
          errLogg=errLogg+'Image #'+STRING(i+1, FORMAT='(i0)')+' not in expected shape/signal.'+newline
        ENDELSE

      ENDIF ELSE SNIthis=CREATE_STRUCT('empty',0)

      IF i EQ 0 THEN SNIres=CREATE_STRUCT('S0',SNIthis) ELSE SNIres=CREATE_STRUCT(SNIres, 'S'+STRING(i, FORMAT='(i0)'), SNIthis)
      WIDGET_CONTROL, lblProgress, SET_VALUE='SNI progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    If errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
    SNIsupTab=resArr
    results(testNmb)=1
  ENDIF
  ;ENDIF ELSE sv=DIALOG_MESSAGE('ROI for SNI not found for active image. Not in expected shape/signal.', DIALOG_PARENT=evTop)
end

pro barNM
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)

  resArr=FLTARR(8,nImg)-1; fwhm for each quadrant
  testNmb=getResNmb(modality,'BAR',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  WIDGET_CONTROL, txtBar1, GET_VALUE=bar1
  WIDGET_CONTROL, txtBar2, GET_VALUE=bar2
  WIDGET_CONTROL, txtBar3, GET_VALUE=bar3
  WIDGET_CONTROL, txtBar4, GET_VALUE=bar4
  barw=FLOAT([bar1(0),bar2(0),bar3(0),bar4(0)])

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    errLogg=''
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN

        ;update barROI
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        sz=SIZE(tempImg, /DIMENSIONS)
        curPix=structImgs.(i).pix(0)
        updateROI, ANA='BAR', SEL=i, IMG=tempImg

        IF TOTAL(barROI) GT 4 THEN BEGIN
          resArr[0:3,i]=calculateBarNM(tempImg, barROI, curPix);MTF
          const=4.*SQRT(ALOG(2))/!pi
          resArr[4:7,i]=const*barw*SQRT(ALOG(1/resArr[0:3,i]));FWHM
        ENDIF

      ENDIF

      WIDGET_CONTROL, lblProgress, SET_VALUE='Bar phantom progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    If errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
    barRes=resArr
    results(testNmb)=1
  ENDIF

end


pro getAcqNM
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  resArr=FLTARR(2,nImg)-1; frame duration and total counts

  testNmb=getResNmb(modality,'ACQ',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  IF TOTAL(markedArr) GT 0 THEN BEGIN
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        resArr[0,i]=TOTAL(tempImg)
        resArr[1,i]=structImgs.(i).acqFrameDuration
      ENDIF
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    acqRes=resArr
    results(testNmb)=1
  ENDIF
end

pro SNR_MR
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  resArr=FLTARR(5,nImg)-1; ['S img 1','S img 2','S mean','stdev diff','SNR']
  testNmb=getResNmb(modality,'SNR',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 1 THEN BEGIN;minimum 2 images to analyse
    anaImg=WHERE(markedArr EQ 1, nAnaImg)
    firstImages=anaImg(0)
    secondImages=anaImg(1)
    IF nAnaImg GT 2 THEN BEGIN
      FOR i=2, nAnaImg-1 DO BEGIN
        IF i mod 2 EQ 0 THEN firstImages=[firstImages, anaImg(i)] ELSE secondImages=[secondImages,anaImg(i)]
      ENDFOR
    ENDIF
    nSets=N_ELEMENTS(secondImages)

    FOR i=0, nSets-1 DO BEGIN
      first=firstImages(i)
      second=secondImages(i)
      activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
      updateROI, ANA='SNR', SEL=first
      szROI=SIZE(SNR_ROI, /DIMENSIONS)
      ;check if same size
      secImg=readImg(structImgs.(second).filename, structImgs.(second).frameNo)
      szSec=SIZE(secImg, /DIMENSIONS)
      IF ~ARRAY_EQUAL(szSec, szROI) THEN BEGIN
        sv=DIALOG_MESSAGE('Image '+STRING(first,FORMAT='(i0)')+' and '+STRING(second,FORMAT='(i0)')+' assumed to be pair of images, but these have different size and a subtraction image cannot be calculated.', DIALOG_PARENT=evTop)
      ENDIF ELSE BEGIN
        maske=SNR_ROI
        IMAGE_STATISTICS, activeImg, MEAN=meanS1, MASK=maske
        resArr(0,firstImages(i))=meanS1
        IMAGE_STATISTICS, secImg, MEAN=meanS2, MASK=maske
        resArr(1,firstImages(i))=meanS2
        meanS=0.5*(meanS1+meanS2)
        resArr(2,firstImages(i))=meanS
        diffImg=activeImg-secImg
        IMAGE_STATISTICS, diffImg, STDDEV=stddev, MASK=maske
        resArr(3,firstImages(i))=stddev
        resArr(4,firstImages(i))=meanS*SQRT(2)/stddev
      ENDELSE
      WIDGET_CONTROL, lblProgress, SET_VALUE='Calculating SNR: '+STRING(i*100./nSets, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    SNRres=resArr
    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF ELSE sv=DIALOG_MESSAGE('You need to select at least two images to calculate difference image.', DIALOG_PARENT=evTop)

end

pro PIU_MR
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  resArr=FLTARR(7,nImg)-1; ['min','max','PIU','X min','Y min','X max','Y max']
  testNmb=getResNmb(modality,'PIU',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    updateROI, ANA='PIU', SEL=first
    szROI=SIZE(PIU_ROI, /DIMENSIONS)

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        updateROI, ANA='PIU', SEL=i;always update pr image

        pix=structImgs.(i).pix
        szROI=ROUND(10./pix(0));1cm^2 ROIs to evaluate
        IF szROI LT 3 THEN szROI=3
        ;code adapted from: https://www.imageeprocessing.com/2015/10/edge-detection-using-local-variance.html
        kernelSmooth=FLTARR(szROI,szROI)+(1./szROI^2)
        smoothTemp=CONVOL(activeImg,kernelSmooth,CENTER=0)

        maske=PIU_ROI
        IMAGE_STATISTICS, smoothTemp, MAXIMUM=maxVal, MINIMUM=minVal, MASK=maske
        resArr(0,i)=minVal
        resArr(1,i)=maxVal
        resArr(2,i)=100.*(1-(maxVal-minVal)/(maxVal+minVal))
        ;maskedSmoothTemp=smoothTemp*0-1000.
        maskedSmoothTemp=maske*smoothTemp
        idMin=WHERE(maskedSmoothTemp EQ minVal)
        idMax=WHERE(maskedSmoothTemp EQ maxVal)
        indMin = ARRAY_INDICES(maskedSmoothTemp, idMin(0))
        indMax = ARRAY_INDICES(maskedSmoothTemp, idMax(0))
        resArr(3,i)=indMin(0)
        resArr(4,i)=indMin(1)
        resArr(5,i)=indMax(0)
        resArr(6,i)=indMax(1)
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Calculating PIU: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    PIUres=resArr
    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF

end

pro Ghost_MR
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  resArr=FLTARR(6,nImg)-1; ['Center','top','bottom','left','right','GR']
  testNmb=getResNmb(modality,'GHOST',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    pix=structImgs.(first).pix
    updateROI, ANA='GHOST', SEL=first
    szROI=SIZE(ghostMR_ROI, /DIMENSIONS)

    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        szAct=SIZE(activeImg, /DIMENSION)
        IF pix(0) NE structImgs.(i).pix(0) OR ~ARRAY_EQUAL(szROI,szAct) THEN BEGIN
          pix=structImgs.(i).pix
          updateROI, ANA='GHOST', SEL=i;always update pr image
          szROI=SIZE(ghostMR_ROI, /DIMENSIONS)
        ENDIF

        maske=ghostMR_ROI[*,*,0]
        IMAGE_STATISTICS, activeImg, MEAN=cMean, MASK=maske
        resArr(0,i)=cMean
        maske=ghostMR_ROI[*,*,1]
        IMAGE_STATISTICS, activeImg, MEAN=bMean, MASK=maske
        resArr(1,i)=bMean
        maske=ghostMR_ROI[*,*,3]
        IMAGE_STATISTICS, activeImg, MEAN=tMean, MASK=maske
        resArr(2,i)=tMean
        maske=ghostMR_ROI[*,*,2]
        IMAGE_STATISTICS, activeImg, MEAN=lMean, MASK=maske
        resArr(3,i)=lMean
        maske=ghostMR_ROI[*,*,4]
        IMAGE_STATISTICS, activeImg, MEAN=rMean, MASK=maske
        resArr(4,i)=rMean

        resArr(5,i)=ABS(100.*0.5*((lMean+rMean)-(tMean+bMean))/cMean)

      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Calculating Percent Signal Ghosting: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    ghostMRres=resArr
    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF

end

pro GD_MR;geometric distortion
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  WIDGET_CONTROL, txtGD_MR_act, GET_VALUE=wAct
  wAct=FLOAT(wAct(0))
  nImg=N_TAGS(structImgs)

  resArr=FLTARR(10,nImg)-1; ['width_0','width_90','width_45','width_135','GD_0','GD_90','GD_45','GD_135'] + centX,centY
  testNmb=getResNmb(modality,'GEOMDIST',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        sz=SIZE(activeImg, /DIMENSION)
        pix=structImgs.(i).pix(0)

        xMax=FLTARR(sz(0))
        yMAx=FLTARR(sz(1))
        FOR c=0, sz(0)-1 DO xMax[c]=MAX(activeImg[c,*])
        FOR c=0, sz(1)-1 DO yMax[c]=MAX(activeImg[*,c])
        halfmax=0.5*(MAX(activeImg)-MIN(activeImg))
        widthX=getWidthAtThreshold(xMax, halfmax)
        widthY=getWidthAtThreshold(yMax, halfmax)
        resArr[0:1,i]=[widthX(0),widthY(0)]*pix
        resArr[8:9,i]=[widthX(1),widthY(1)]
        cent=ROUND([widthX(1),widthY(1)])

        nPixXfwhm=widthX(0)
        nPixDiag2=CEIL(nPixXfwhm/2*1.1)
        prof45=FLTARR(nPixDiag2*2+1)+MIN(activeImg)
        prof45[nPixDiag2]=activeImg[cent[0],cent[1]]
        prof135=prof45
        FOR nn=1,nPixDiag2 DO BEGIN
          IF cent[0]-nn GE 0 AND cent[1]-nn GE 0 AND cent[0]+nn LT sz(0) AND cent[1]+nn LT sz(1) THEN BEGIN
            prof45[nPixDiag2+nn]=activeImg[cent[0]+nn,cent[1]+nn]
            prof45[nPixDiag2-nn]=activeImg[cent[0]-nn,cent[1]-nn]
            prof135[nPixDiag2+nn]=activeImg[cent[0]+nn,cent[1]-nn]
            prof135[nPixDiag2-nn]=activeImg[cent[0]-nn,cent[1]+nn]
          ENDIF
        ENDFOR
        width45=getWidthAtThreshold(prof45, halfmax)
        width135=getWidthAtThreshold(prof135, halfmax)
        resArr[2:3,i]=[width45(0),width135(0)]*SQRT(2)*pix(0)
        resArr[4:7,i]=100.*ABS(resArr[0:3,i]-wAct)/wAct
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Calculating Geometric Distortion: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    GeomDistRes=resArr
    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF

end

pro Slice_MR
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS

  nImg=N_TAGS(structImgs)

  resArr=FLTARR(6,nImg)-1; ['Nominal (mm)','Measured (mm)','Diff (mm)','Diff (%)'],'AltSup',['FWHMupper','FWHMlower']
  testNmb=getResNmb(modality,'SLICETHICK',analyseStringsAll)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    anaImg=WHERE(markedArr EQ 1)
    first=anaImg(0)
    activeImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    pix=structImgs.(first).pix
    updateROI, ANA='SLICETHICK', SEL=first
    szROI=SIZE(sliceMR_ROI, /DIMENSIONS)
    lineStruct=!Null
    nI=MIN([nImg,N_ELEMENTS(markedArr)])
    sliceThickRes=!Null
    FOR i=0, nI-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        resArr[0,i]=structImgs.(i).sliceThick
        szAct=SIZE(activeImg, /DIMENSION)
        IF pix(0) NE structImgs.(i).pix(0) OR ~ARRAY_EQUAL(szROI,szAct) THEN BEGIN
          pix=structImgs.(i).pix
          updateROI, ANA='SLICETHICK', SEL=i;always update pr image
          szROI=SIZE(sliceMR_ROI, /DIMENSIONS)
        ENDIF
        ;find profiles
        temp=TOTAL(sliceMR_ROI[*,*,0],2) & temp2=WHERE(temp GT 0) & firstX=temp2(0) & lastX=temp2(-1)
        temp=TOTAL(sliceMR_ROI[*,*,0],1) & temp2=WHERE(temp GT 0) & firstY=temp2(0) & lastY=temp2(-1)
        rows=lastY-firstY+1
        subUpper=activeImg[firstX:lastX,firstY:lastY]
        temp=TOTAL(sliceMR_ROI[*,*,1],1) & temp2=WHERE(temp GT 0) & firstY=temp2(0) & lastY=temp2(-1)
        subLower=activeImg[firstX:lastX,firstY:lastY]
        nPix=lastX-firstX+1
        IF rows EQ 1 THEN profUpper = subUpper ELSE profUpper=(1./rows)*TOTAL(subUpper,2)
        IF rows EQ 1 THEN profLower = subLower ELSE profLower=(1./rows)*TOTAL(subLower,2)
        maxUpper=MAX(MEDIAN(profUpper,5))
        maxLower=MAX(MEDIAN(profLower,5))
        bg=0.5*(MEAN(subUpper[0:4,*])+MEAN(subLower[0:4,*]))
        halfmaxU=0.5*(maxUpper+bg)
        halfmaxL=0.5*(maxLower+bg)

        wU=getWidthAtThreshold(profUpper, halfmaxU)
        wL=getWidthAtThreshold(profLower, halfmaxL)

        structTemp=CREATE_STRUCT('background',bg,'maxVal',[maxUpper,maxLower],'subUpper',profUpper,'subLower',profLower,$
          'firstLast',[[wU(1)-wU(0)/2.,wU(1)+wU(0)/2.],[wL(1)-wL(0)/2.,wL(1)+wL(0)/2.]])
        sliceThickRes=CREATE_STRUCT(sliceThickRes,'L'+STRING(i, FORMAT='(i0)'),structTemp)
        fwhmU=wU(0)*pix(0)
        fwhmL=wL(0)*pix(0)
        resArr[1,i]=0.2*fwhmU*fwhmL/(fwhmU+fwhmL)
        resArr[2,i]=resArr[1,i]-resArr[0,i]
        resArr[3,i]=100.*resArr[2,i]/resArr[0,i]
        resArr[4,i]=fwhmU
        resArr[5,i]=fwhmL

      ENDIF ELSE sliceThickRes=CREATE_STRUCT(sliceThickRes,'L'+STRING(i, FORMAT='(i0)'),CREATE_STRUCT('empty',0))

      WIDGET_CONTROL, lblProgress, SET_VALUE='Slice thickness progress: '+STRING(i*100./nI, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    sliceThickRes=CREATE_STRUCT(sliceThickRes,'resArr',resArr)

    sliceThickResTab=resArr

    results(testNmb)=1
    redrawImg,0,1;active back to original selected
  ENDIF

end
;pro getPos_MR; get phantom position for MR images (geometric mass)
;  COMPILE_OPT hidden
;  COMMON VARI
;
;  WIDGET_CONTROL, /HOURGLASS
;  nImg=N_TAGS(structImgs)
;  resArr=STRARR(2,nImg)
;
;  testNmb=getResNmb(modality,'POS',analyseStringsAll)
;  markedArr=INTARR(nImg)
;  IF marked(0) EQ -1 THEN BEGIN
;    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
;  ENDIF ELSE markedArr(marked)=1
;  IF TOTAL(markedArr) GT 0 THEN BEGIN
;    nI=MIN([nImg,N_ELEMENTS(markedArr)])
;    errs=INTARR(nI)
;    FOR i=0, nI-1 DO BEGIN
;      IF markedArr(i) THEN BEGIN
;        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
;        threshold=MIN(tempImg)+0.25*(MAX(tempImg)-MIN(tempImg));25% above minimum value relative to max value in image
;        above=WHERE(tempImg GT threshold)
;        tempImg=0.*tempImg
;        tempImg(above)=threshold
;        centerThis=centroid(tempImg, .5*threshold)
;        imsz=SIZE(tempImg, /DIMENSION)
;
;          IF MIN(centerThis) EQ -1 THEN BEGIN
;            errs(i)=1
;            centerThis=0.*centerThis
;          ENDIF
;
;        resArr[*,i]=centerThis-0.5*imsz
;      ENDIF
;    ENDFOR
;    WIDGET_CONTROL, lblProgress, SET_VALUE=''
;    MRposRes=resArr
;    results(testNmb)=1
;  ENDIF
;end
