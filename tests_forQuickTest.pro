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
  szROI=SIZE(stpROI, /DIMENSIONS)

  resArr=FLTARR(4,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'STP',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  IF TOTAL(markedArr) GT 0 THEN BEGIN
<<<<<<< HEAD

    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(activeImg, /DIMENSIONS)
        szROI=SIZE(stpROI, /DIMENSIONS)
        IF ~ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN updateROI, ANA='STP'

        maske=stpROI
        IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanHU, STDDEV=stddevHU, MASK=maske
        resArr(2,i)=meanHU & resArr(3,i)=stddevHU

=======
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
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Progress pixel values: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
<<<<<<< HEAD

    stpRes=CREATE_STRUCT('table',resArr)
    results(testNmb)=1

    redrawImg,0,1
=======
    If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size as the first image.',/INFORMATION)
    stpRes=CREATE_STRUCT('table',resArr)
    results(testNmb)=1
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
  ENDIF
end

pro getEI
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  resArr=FLTARR(2,nImg)-1; mAs and EI

  testNmb=getResNmb(modality,'EI',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  IF TOTAL(markedArr) GT 0 THEN BEGIN
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        resArr[0,i]=structImgs.(i).mAs
        resArr[1,i]=structImgs.(i).EI
      ENDIF
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    eiRes=resArr
    results(testNmb)=1
  ENDIF
end

pro noise
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
<<<<<<< HEAD

  nImg=N_TAGS(structImgs)
=======
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
  IF N_ELEMENTS(noiseROI) LE 1 THEN updateROI, ANA='NOISE'
  szROI=SIZE(noiseROI, /DIMENSIONS)

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'NOISE',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN
    totNoise=0.
<<<<<<< HEAD

    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(activeImg, /DIMENSIONS)
        szROI=SIZE(noiseROI, /DIMENSIONS)
        IF ~ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN updateROI, ANA='NOISE'

          maske=noiseROI
          IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
          resArr(0,i)=meanVal
          resArr(1,i)=stddevVal
          totNoise=totNoise+stddevVal

=======
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
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
<<<<<<< HEAD

=======
    If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
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
<<<<<<< HEAD
    redrawImg,0,1;active back to original selected
=======
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
  ENDIF
end

pro homog
  COMPILE_OPT hidden
  COMMON VARI
<<<<<<< HEAD

  nImg=N_TAGS(structImgs)
=======
  WIDGET_CONTROL, /HOURGLASS
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
  IF N_ELEMENTS(homogROIs) LE 1 THEN updateROI, ANA='HOMOG'
  szROI=SIZE(homogROIs, /DIMENSIONS)

  resArr=FLTARR(szROI(2)*2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'HOMOG',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN

<<<<<<< HEAD
    FOR i=0, nImg-1 DO BEGIN
      WIDGET_CONTROL, /HOURGLASS
      IF markedArr(i) THEN BEGIN
        activeImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        imszTemp=SIZE(activeImg, /DIMENSIONS)
        szROI=SIZE(homogROIs, /DIMENSIONS)
        IF ~ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN updateROI, ANA='HOMOG'

        FOR r=0, szROI(2)-1 DO BEGIN
          maske=homogROIs[*,*,r]
          IMAGE_STATISTICS, activeImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
          resArr(r,i)=meanVal
          resArr(r+5,i)=stddevVal
        ENDFOR
      ENDIF
      WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'

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
  testNmb=getResNmb(modality,'SLICETHICK',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    daRad=dxya(3)*dxya(2)/!radeg

    errLogg=''
    FOR i=0, nImg-1 DO BEGIN
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
              ELSE errLogg=errLogg+'Image '+STRING(i,FORMAT='(i0)')+', Line '+STRING(l-sta,FORMAT='(i0)')+': Found profile to close to border of search-area. Single profile with peak used (no averaging).'+newline
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
=======
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
  ENDIF
end; homog

pro slicethick
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
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
  testNmb=getResNmb(modality,'SLICETHICK',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    daRad=dxya(3)*dxya(2)/!radeg

    errLogg=''
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN

        IF nFrames NE 0 THEN BEGIN
          tempImg=readImg(structImgs.(0).filename, i)
          pix=structImgs.(0).pix
          resArr[0,i]=structImgs.(0).sliceThick
        ENDIF ELSE BEGIN
          tempImg=readImg(structImgs.(i).filename, 0)
          pix=structImgs.(i).pix
          resArr[0,i]=structImgs.(i).sliceThick
        ENDELSE
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
              ELSE errLogg=errLogg+'Image '+STRING(i,FORMAT='(i0)')+', Line '+STRING(l-sta,FORMAT='(i0)')+': Found profile to close to border of search-area. Single profile with peak used (no averaging).'+newline
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
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c

      ENDIF ELSE lineStruct=CREATE_STRUCT('empty',0)
      IF i EQ 0 THEN sliceThickRes=CREATE_STRUCT('img0',lineStruct) ELSE sliceThickRes=CREATE_STRUCT(sliceThickRes,'img'+STRING(i,FORMAT='(i0)'),lineStruct)

      IF ramptype EQ 0 THEN BEGIN
        resArr[5,i]=MEAN(resArr[1:4,i])
        resArr[6,i]=100.0*(resArr[5,i]-resArr[0,i])/resArr[0,i]
      ENDIF

      WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
<<<<<<< HEAD
    IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
=======
    IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
    errLogg=''

    sliceThickResTab=resArr
    results(testNmb)=1
  ENDIF
end;slicethick

pro mtf
  COMPILE_OPT hidden
  COMMON VARI

<<<<<<< HEAD
  nImg=N_TAGS(structImgs)
=======
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
  testNmb=getResNmb(modality,'MTF',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF first NE -1 THEN BEGIN
    WIDGET_CONTROL, /HOURGLASS
    WIDGET_CONTROL, cw_typeMTF, GET_VALUE=typeMTF
    WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=ROIszPix
    WIDGET_CONTROL, txtCutLSFW, GET_VALUE=cutLSFW
    WIDGET_CONTROL, txtCutLSFW2, GET_VALUE=cutLSFWf
    WIDGET_CONTROL, txtfreqMTF, GET_VALUE=sampFreq

<<<<<<< HEAD
    tempImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    pixFirst=structImgs.(first).pix(0)
    filtFirst=structImgs.(first).filter

=======
    IF nFrames NE 0 THEN BEGIN
      tempImg=readImg(structImgs.(0).filename, first)
      pixFirst=structImgs.(0).pix(0)
      filtFirst=structImgs.(0).filter
    ENDIF ELSE BEGIN
      tempImg=readImg(structImgs.(first).filename, 0)
      pixFirst=structImgs.(first).pix(0)
      filtFirst=structImgs.(first).filter
    ENDELSE
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c

    szFirst=SIZE(tempImg, /DIMENSIONS)

    dxyaO=dxya[0:1]+offxy

    CASE typeMTF OF

      0: BEGIN; 2d bead

        ROIsz=ROUND(ROIszPix(0)/pixFirst)
        halfSz=SIZE(tempImg, /DIMENSIONS)/2
        x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz)
        y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz)

        IF x1 GE 0 AND x2 LT szFirst(0)-1 AND y1 GE 0 AND y1-(ROIsz(0)*2+1) GE 0 AND y2 LT szFirst(1)-1 AND y2-(ROIsz(0)*2+1) LT szFirst(1)-1 THEN roiOk=1 ELSE roiOk=0

        IF roiOK THEN BEGIN
          errBead=0
          errSize=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              IF i GT 0 THEN BEGIN
<<<<<<< HEAD

                tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                curPix=structImgs.(i).pix(0)

=======
                IF nFrames NE 0 THEN BEGIN
                  tempImg=readImg(structImgs.(0).filename, i)
                  curPix=pixFirst
                ENDIF ELSE BEGIN
                  tempImg=readImg(structImgs.(i).filename, 0)
                  curPix=structImgs.(i).pix(0)
                ENDELSE
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
              ENDIF ELSE curPix=pixFirst

              szImg=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(szImg[0:1], szFirst[0:1]) AND curPix EQ pixFirst THEN BEGIN
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
<<<<<<< HEAD
          IF errBead GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of bead for '+STRING(errBead, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Bead position assumed to be at the selected ROI center.', DIALOG_PARENT=evTop)
          If errSize THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
          results(testNmb)=1
        ENDIF ELSE sv=DIALOG_MESSAGE('ROIs outside image. Calculation not possible.',/INFORMATION, DIALOG_PARENT=evTop)
      END; bead

      1:BEGIN;wire
        pixFirst=structImgs.(first).pix(0)
=======
          IF errBead GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of bead for '+STRING(errBead, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Bead position assumed to be at the selected ROI center.')
          If errSize THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
          results(testNmb)=1
        ENDIF ELSE sv=DIALOG_MESSAGE('ROIs outside image. Calculation not possible.',/INFORMATION)
      END; bead

      1:BEGIN;wire
        IF nFrames NE 0 THEN pixFirst=structImgs.(0).pix(0) ELSE pixFirst=structImgs.(first).pix(0)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
        ROIsz=ROUND(ROIszPix(0)/pixFirst)
        halfSz=szFirst/2
        x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz)
        y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz)

        nnImg=TOTAL(markedArr)
        subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

        proceed=1
        counter=0

        FOR i=0, nImg-1 DO BEGIN
          IF markedArr(i) THEN BEGIN
<<<<<<< HEAD
            tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
=======
            IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
            szThis=SIZE(tempImg, /DIMENSIONS)
            IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
              subM[*,*,counter]=tempImg[x1:x2,y1:y2]
              proceed=1
              counter=counter+1
            ENDIF ELSE BEGIN
              MTF=CREATE_STRUCT('empty',0)
<<<<<<< HEAD
              sv=DIALOG_MESSAGE('Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION, DIALOG_PARENT=evTop)
=======
              sv=DIALOG_MESSAGE('Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
              proceed=0
            ENDELSE
          ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
          IF proceed EQ 0 THEN BREAK
        ENDFOR

        IF proceed THEN BEGIN
          MTFres=calculateMTF(subM, pixFirst, dxyaO[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSF, /BUTTON_SET), FLOAT(cutLSFW(0)), FLOAT(cutLSFWf(0)), FLOAT(sampFreq(0)))
<<<<<<< HEAD
          IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding location of line for one or more images. Location assumed to be at center of ROI.', DIALOG_PARENT=evTop)
=======
          IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding location of line for one or more images. Location assumed to be at center of ROI.')
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c

          results(testNmb)=1
        ENDIF
      END; wire

      2: BEGIN ; circular edge (3d)
<<<<<<< HEAD

        tempImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
        pixFirst=structImgs.(first).pix(0)
        filtFirst=structImgs.(first).filter
=======
        IF nFrames NE 0 THEN BEGIN
          tempImg=readImg(structImgs.(0).filename, 0)
          pixFirst=structImgs.(0).pix(0)
        ENDIF ELSE BEGIN
          tempImg=readImg(structImgs.(first).filename, 0)
          pixFirst=structImgs.(first).pix(0)
          filtFirst=structImgs.(first).filter
        ENDELSE
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c

        ROIsz=ROUND(ROIszPix(0)/pixFirst)
        halfSz=szFirst/2
        x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz)
        y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz)

        nnImg=TOTAL(markedArr)
        subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

        filtStatus=1
        pixStatus=1
        proceed=1
        counter=0
        FOR i=0, nImg-1 DO BEGIN
          IF markedArr(i) THEN BEGIN
<<<<<<< HEAD
            tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
            szThis=SIZE(tempImg, /DIMENSIONS)
            curPix=structImgs.(i).pix(0)
            curFilt=structImgs.(i).filter
            IF curPix NE pixFirst THEN pixStatus=0
            IF curFilt NE filtFirst THEN filtStatus=0
=======
            IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
            szThis=SIZE(tempImg, /DIMENSIONS)
            IF nFrames EQ 0 THEN BEGIN
              curPix=structImgs.(i).pix(0)
              curFilt=structImgs.(i).filter
              IF curPix NE pixFirst THEN pixStatus=0
              IF curFilt NE filtFirst THEN filtStatus=0
            ENDIF
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
            IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
              subM[*,*,counter]=tempImg[x1:x2,y1:y2]
              proceed=1
              counter=counter+1
            ENDIF ELSE BEGIN
              MTF=CREATE_STRUCT('empty',0)
<<<<<<< HEAD
              sv=DIALOG_MESSAGE('Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION, DIALOG_PARENT=evTop)
=======
              sv=DIALOG_MESSAGE('Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
              proceed=0
            ENDELSE
          ENDIF
          IF proceed EQ 0 THEN BREAK
        ENDFOR

        IF proceed THEN BEGIN
          MTFres=calculateMTF(subM, pixFirst, dxyaO[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSF, /BUTTON_SET), FLOAT(cutLSFW(0)), FLOAT(cutLSFWf(0)), FLOAT(sampFreq(0)))
<<<<<<< HEAD
          IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for one or more images. Center of circle assumed to be at center of ROI.', DIALOG_PARENT=evTop)
          results(testNmb)=1
          IF filtStatus + pixStatus LT 2 THEN sv=DIALOG_MESSAGE('Pixelsize or filter-type do not match for all images in selection.', DIALOG_PARENT=evTop)
        ENDIF
      END;circular edge 3d
      ELSE: sv=DIALOG_MESSAGE('Not implementet selected MTF type yet',/INFORMATION, DIALOG_PARENT=evTop)
=======
          IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for one or more images. Center of circle assumed to be at center of ROI.')
          results(testNmb)=1
          IF filtStatus + pixStatus LT 2 THEN sv=DIALOG_MESSAGE('Pixelsize or filter-type do not match for all images in selection.')
        ENDIF
      END;circular edge 3d
      ELSE: sv=DIALOG_MESSAGE('Not implementet selected MTF type yet',/INFORMATION)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
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

<<<<<<< HEAD
  nImg=N_TAGS(structImgs)
  testNmb=getResNmb(modality,'MTF',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
=======
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  testNmb=getResNmb(modality,'MTF',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF first NE -1 THEN BEGIN

    IF nFrames NE 0 THEN BEGIN
      tempImg=readImg(structImgs.(0).filename, 0)
      curPix=structImgs.(0).pix(0)
    ENDIF ELSE BEGIN
      tempImg=readImg(structImgs.(first).filename, 0)
      curPix=structImgs.(first).pix(0)
    ENDELSE

    szFirst=SIZE(tempImg, /DIMENSIONS)
    dxyaO=dxya[0:1]+offxy
    ROIszMM=FLOAT([ROIszX(0),ROIszY])/2.
    ROIsz=ROIszMM/curPix
    halfSz=szFirst/2
    x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz(0))
    y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz(1))

    subMatrix=FLTARR(x2-x1+1,y2-y1+1)

    IF N_ELEMENTS(stpRes) EQ 0 THEN stpRes=0

    errLogg=''
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
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
    IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg)
    results(testNmb)=1
  ENDIF;none selected
end;mtfx

pro uniformityNM
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  IF N_ELEMENTS(unifROI) LE 1 THEN updateROI, ANA='UNIF'
  szROI=SIZE(unifROI, /DIMENSIONS)

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'UNIF',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

<<<<<<< HEAD
  IF first NE -1 THEN BEGIN

    tempImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
    curPix=structImgs.(first).pix(0)

    szFirst=SIZE(tempImg, /DIMENSIONS)
    dxyaO=dxya[0:1]+offxy
    ROIszMM=FLOAT([ROIszX(0),ROIszY])/2.
    ROIsz=ROIszMM/curPix
    halfSz=szFirst/2
    x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxyaO(0)+ROIsz(0))
    y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz(1))

    subMatrix=FLTARR(x2-x1+1,y2-y1+1)

    IF N_ELEMENTS(stpRes) EQ 0 THEN stpRes=0

    errLogg=''
    FOR i=0, nImg-1 DO BEGIN
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

pro uniformityNM
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  IF N_ELEMENTS(unifROI) LE 1 THEN updateROI, ANA='UNIF'
  szROI=SIZE(unifROI, /DIMENSIONS)

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'UNIF',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    tempImg=readImg(structImgs.(first).filename,structImgs.(first).frameNo)
    curPix=structImgs.(first).pix(0)
=======
  IF TOTAL(markedArr) GT 0 THEN BEGIN

    IF nFrames NE 0 THEN BEGIN
      tempImg=readImg(structImgs.(0).filename, 0)
      curPix=structImgs.(0).pix(0)
    ENDIF ELSE BEGIN
      tempImg=readImg(structImgs.(first).filename, 0)
      curPix=structImgs.(first).pix(0)
    ENDELSE
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c

    resArr=FLTARR(4,nImg)-1
    errLogg=''
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
<<<<<<< HEAD
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
=======
        IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
        imszTemp=SIZE(tempImg, /DIMENSIONS)
        IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
          resArr[*,i]=calcUniformityNM(tempImg, unifROI, curPix)
        ENDIF ELSE BEGIN
<<<<<<< HEAD
          errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate uniformity separately for images with the same size.'+newline
=======
          errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate SNI separately for images with the same size.'+newline
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
        ENDELSE
      ENDIF

      WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
<<<<<<< HEAD
    If errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
=======
    If errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c

    results(testNmb)=1
    unifRes=resArr
  ENDIF
end

pro sni
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, /HOURGLASS
<<<<<<< HEAD
  nImg=N_TAGS(structImgs)
  IF N_ELEMENTS(SNIroi) LE 1 THEN updateROI, ANA='SNI'
  szROI=SIZE(SNIroi, /DIMENSIONS)

  IF N_ELEMENTS(SNIroi) GT 1 THEN BEGIN
    resArr=FLTARR(2,nImg)-1; mean, stdev all circles
    testNmb=getResNmb(modality,'SNI',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
    markedArr=INTARR(nImg)
    IF marked(0) EQ -1 THEN BEGIN
      IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
    ENDIF ELSE markedArr(marked)=1
    markedTemp=WHERE(markedArr EQ 1)
    first=markedTemp(0)

    IF TOTAL(markedArr) GT 0 THEN BEGIN

      tempImg=readImg(structImgs.(first).filename,structImgs.(first).frameNo)
      curPix=structImgs.(first).pix(0)

      errLogg=''
      FOR i=0, nImg-1 DO BEGIN
        IF markedArr(i) THEN BEGIN
          ;check if same size
          tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
          imszTemp=SIZE(tempImg, /DIMENSIONS)
          IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
            SNI=calculateSNI(tempImg, SNIroi[*,*,0], curPix)
          ENDIF ELSE BEGIN
            SNI=CREATE_STRUCT('empty',0)
            errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate SNI separately for images with the same size.'+newline
          ENDELSE
        ENDIF ELSE SNI=CREATE_STRUCT('empty',0)

        IF i EQ 0 THEN SNIres=CREATE_STRUCT('S0',SNI) ELSE SNIres=CREATE_STRUCT(SNIres, 'S'+STRING(i, FORMAT='(i0)'), SNI)
        WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
      ENDFOR
      WIDGET_CONTROL, lblProgress, SET_VALUE=''
      If errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)

      results(testNmb)=1
    ENDIF
  ENDIF ELSE sv=DIALOG_MESSAGE('ROI for SNI not found. Not in expected shape/signal.', DIALOG_PARENT=evTop)
end

pro getAcqNM
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  nImg=N_TAGS(structImgs)
  resArr=FLTARR(2,nImg)-1; frame duration and total counts

  testNmb=getResNmb(modality,'ACQ',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
=======
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  IF N_ELEMENTS(SNIroi) LE 1 THEN updateROI, ANA='SNI'
  szROI=SIZE(SNIroi, /DIMENSIONS)

  resArr=FLTARR(2,nImg)-1; mean, stdev all circles
  testNmb=getResNmb(modality,'SNI',analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT,analyseStringsPET)
>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
  markedArr=INTARR(nImg)
  IF marked(0) EQ -1 THEN BEGIN
    IF markedMulti(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr=markedMulti[testNmb,*]
  ENDIF ELSE markedArr(marked)=1
<<<<<<< HEAD
  IF TOTAL(markedArr) GT 0 THEN BEGIN
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
        resArr[0,i]=TOTAL(tempImg)
        resArr[1,i]=structImgs.(i).acqFrameDuration
      ENDIF
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    acqRes=resArr
=======
  markedTemp=WHERE(markedArr EQ 1)
  first=markedTemp(0)

  IF TOTAL(markedArr) GT 0 THEN BEGIN

    IF nFrames NE 0 THEN BEGIN
      tempImg=readImg(structImgs.(0).filename, 0)
      curPix=structImgs.(0).pix(0)
    ENDIF ELSE BEGIN
      tempImg=readImg(structImgs.(first).filename, 0)
      curPix=structImgs.(first).pix(0)
    ENDELSE

    errLogg=''
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
        imszTemp=SIZE(tempImg, /DIMENSIONS)
        IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
          SNI=calculateSNI(tempImg, SNIroi, curPix)
        ENDIF ELSE BEGIN
          SNI=CREATE_STRUCT('empty',0)
          errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate SNI separately for images with the same size.'+newline
        ENDELSE
      ENDIF ELSE SNI=CREATE_STRUCT('empty',0)

      IF i EQ 0 THEN SNIres=CREATE_STRUCT('S0',SNI) ELSE SNIres=CREATE_STRUCT(SNIres, 'S'+STRING(i, FORMAT='(i0)'), SNI)
      WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''
    If errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg)

>>>>>>> 51b538bf2a71e66c58c0bf95eec4fabbd66e127c
    results(testNmb)=1
  ENDIF
end
