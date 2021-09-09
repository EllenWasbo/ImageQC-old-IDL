  ;ImageQC - quality control of medical images
  ;Copyright (C) 2018 Ellen Wasbo, Stavanger University Hospital, Norway
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

  ;update image Results
pro updateImageRes
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
  WSET, iDrawImageRes
  TVSCL, INTARR(450,450)

  curMode=WIDGET_INFO(wTabModes, /TAB_CURRENT)

  IF results(getResNmb(curMode,analyse,analyseStringsAll)) THEN BEGIN

    rowNo=-1
    tags=TAG_NAMES(structImgs)
    IF tags(0) NE 'EMPTY' THEN BEGIN
      sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
      pix=structImgs.(sel).pix(0);IF nFrames EQ 0 THEN pix=structImgs.(sel).pix(0) ELSE pix=structImgs.(0).pix(0)
      nImg=N_TAGS(structImgs);IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
      markedArr=INTARR(nImg)
      IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
      markedTemp=WHERE(markedArr EQ 1)
      rowNo=WHERE(markedTemp EQ sel)
      rowNo=rowNo(0)
    ENDIF

    IF N_ELEMENTS(pix) GT 0 THEN nyqfr=1/(2.*pix); Nyquist frequency in mm-1

    IF rowNo(0) NE -1 OR analyse EQ 'ENERGYSPEC' THEN BEGIN

      CASE curMode OF

        ;********************************CT*************************************************
        0: BEGIN
          curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF

              'NPS': BEGIN
                IF N_TAGS(NPSres.(sel)) GT 1 THEN BEGIN
                  activeResImg=NPSres.(sel).NPS
                  tvRes=activeResImg
                  szNPS=SIZE(activeResImg, /DIMENSIONS)
                  szX=450
                  szY=ROUND(szX*(szNPS(1)*1./szNPS(0)))
                  TVSCL,congrid(adjustWindowLevel(tvRes, [0,100*max(NPSres.(sel).NPS)]), szX, szY, /INTERP)
                ENDIF
              END

              ELSE:
            ENDCASE

          ENDIF
        END

        ;- -------------------- Xray ----------------------------
        1:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF

              'NPS': BEGIN

                IF N_TAGS(NPSres.(sel)) GT 1 THEN BEGIN

                    activeResImg=NPSres.(sel).NPS
                    tvRes=activeResImg
                    tvRes[*,0]=0 & tvRes[0,*]=0;tvRes[*,128]=0 & tvRes[128,*]=0
                    szNPS=SIZE(activeResImg, /DIMENSIONS)
                    szX=450
                    szY=ROUND(szX*(szNPS(1)*1./szNPS(0)))
                    TVSCL,congrid(adjustWindowLevel(tvRes, [0,max(tvRes)]), szX, szY, /INTERP)

                ENDIF
              END
              
              'VARI': BEGIN
                
                IF SIZE(varImgRes, /TNAME) EQ 'STRUCT' THEN BEGIN
                  IF N_ELEMENTS(varImgRes.(sel)) GT 1 THEN BEGIN
                    activeResImg=varImgRes.(sel)
                    szImg=SIZE(activeResImg, /DIMENSIONS)
                    
                    IMAGE_STATISTICS, activeResImg[szImg(0)*0.25:szImg(0)*0.75,szImg(1)*0.25:szImg(1)*0.75], MEAN=meanVal, STDDEV=stddevVal, MAX=maxxVal, MIN=minnVal
                    minVal=meanVal-stddevVal;min(varianceImg[ROIrad:szImg(0)-1-ROIrad,ROIrad:szImg(1)-1-ROIrad])
                    maxVal=meanVal+stddevVal;max(varianceImg[ROIrad:szImg(0)-1-ROIrad,ROIrad:szImg(1)-1-ROIrad])
                    IF minVal LT 0 THEN BEGIN
                      minVal=minnVal & maxVal=maxxVal
                    ENDIF

                    szX=450
                    szY=ROUND(szX*(szImg(1)*1./szImg(0)))
                    tvRes=activeResImg
                    TVSCL,congrid(adjustWindowLevel(tvRes, [minVal,maxVal]), szX, szY, /INTERP)
 
                  ENDIF
                ENDIF
              END

              ELSE:
            ENDCASE

          ENDIF
        END
        ;******************************** NM *************************************************
        2:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF

              'UNIF':BEGIN
                WIDGET_CONTROL, cw_imgUnif, GET_VALUE= imgWhich

                CASE imgWhich OF
                  0: BEGIN;curvature corrected
                    IF WIDGET_INFO(btnUnifCorr, /BUTTON_SET) THEN BEGIN
                      tnames= TAG_NAMES(unifRes.(sel+1))
                      IF tnames.HasValue('CORRMATRIX') THEN BEGIN
                        activeResImg=unifRes.(sel+1).corrMatrix
;                        WIDGET_CONTROL, txtUnifDistCorr, GET_VALUE=distSource
;                        WIDGET_CONTROL, txtUnifThickCorr, GET_VALUE=detThick
;                        WIDGET_CONTROL, txtUnifAttCorr, GET_VALUE=detAtt
;                        corrM=corrDistPointSource(tempImg, FLOAT(distSource(0)), pix, FLOAT(detThick(0)), 0.1*FLOAT(detAtt(0))) ; functionsMini
;                        activeResImg=tempImg*corrM
                        szX=450
                        szImg=SIZE(activeResImg, /DIMENSIONS)
                        szY=ROUND(szX*(szImg(1)*1./szImg(0)))
                        WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
                        WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
                        rangeWL=LONG([lower,upper])
                        TVSCL,congrid(adjustWindowLevel(activeResImg, rangeWL), szX, szY, /INTERP)
                        XYOUTS, 0.05,0.05,'Curvature corrected image', CHARSIZE=1.5, COLOR=255
                      ENDIF ELSE XYOUTS, 0.05,0.05,'No correction applied', CHARSIZE=1.5, COLOR=255

                    ENDIF ELSE XYOUTS, 0.05,0.05,'No correction applied', CHARSIZE=1.5, COLOR=255
                    END
                  1: BEGIN;all processing finished
                    activeResImg=unifRes.(sel+1).matrix
                    tvRes=upscaleImg(activeResImg, 3)
                    TVSCL, tvRes
                    XYOUTS, 0.05,0.09,'View zoomed by factor 3 ', CHARSIZE=1.2, COLOR=255
                    XYOUTS, 0.05,0.05,'Pixel size 6.4mm, smoothed as specified by NEMA', CHARSIZE=1.2, COLOR=255
                    END
                  ELSE:
                ENDCASE
                              
              END
              'SNI': BEGIN
                WIDGET_CONTROL, cw_imgSNI, GET_VALUE= imgWhich
                
                CASE imgWhich OF
                  0: BEGIN;2d NPS
                      WIDGET_CONTROL, resTab, GET_VALUE=resArr
                      ;displaying filtered 2d NPS for selected region'
                      tabSel=WIDGET_INFO(resTab,/TABLE_SELECT)
                      colNo=tabSel(0)
                      maxstr=''
                      IF colNo LE 0 THEN BEGIN
                        colmax=WHERE(resArr[1:8,sel] EQ MAX(resArr[1:8,sel]))
                        ss=colmax(0)
                        maxstr=' (max)'
                      ENDIF ELSE ss=colNo-1
                      
                      ssnames=['L1','L2','S1','S2','S3','S4','S5','S6']  
                      activeResImg=SNIres.(sel).NPS_filt.(ss)
                      tvRes=activeResImg
                      szNPS=SIZE(activeResImg, /DIMENSIONS)
                      szX=450
                      szY=ROUND(szX*(szNPS(1)*1./szNPS(0)))
                      TVSCL,congrid(adjustWindowLevel(tvRes, [0,100*max(SNIres.(sel).NPS_filt.(ss))]), szX, szY, /INTERP)
                      XYOUTS, 0.05,0.05, 'NPS filtered '+ssnames(ss)+maxstr, CHARSIZE=1.5, COLOR=255
                   END
                   1: BEGIN; curvature corrected image
                     IF WIDGET_INFO(btnSNICorr, /BUTTON_SET) THEN BEGIN
                      tnames=TAG_NAMES(SNIres.(sel))
                      IF tnames.HasValue('CORRMATRIX') THEN BEGIN
;                        tempImg=readImg(structImgs.(sel).filename, structImgs.(sel).frameNo)
;                        WIDGET_CONTROL, txtSNIDistCorr, GET_VALUE=distSource
;                        WIDGET_CONTROL, txtSNIThickCorr, GET_VALUE=detThick     
;                        WIDGET_CONTROL, txtSNIAttCorr, GET_VALUE=detAtt
;                        corrM=corrDistPointSource(tempImg, FLOAT(distSource(0)), pix, FLOAT(detThick(0)), 0.1*FLOAT(detAtt(0))) ; functionsMini
;                        activeResImg=tempImg*corrM
                        activeResImg=SNIres.(sel).corrMatrix
                        szX=450
                        szImg=SIZE(activeResImg, /DIMENSIONS)
                        szY=ROUND(szX*(szImg(1)*1./szImg(0)))
                        WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
                        WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
                        rangeWL=LONG([lower,upper])
                        TVSCL,congrid(adjustWindowLevel(activeResImg, rangeWL), szX, szY, /INTERP)
                        XYOUTS, 0.05,0.05,'Curvature corrected image', CHARSIZE=1.5, COLOR=255
                      ENDIF ELSE XYOUTS, 0.05,0.05,'No correction applied', CHARSIZE=1.5, COLOR=255
                     ENDIF ELSE XYOUTS, 0.05,0.05,'No correction applied', CHARSIZE=1.5, COLOR=255
                    END
                   ELSE:
                   ENDCASE
                

              END
              'GEOMMEAN':BEGIN
                IF structImgs.(sel).nFrames GE 2 THEN BEGIN
                  img1=readImg(structImgs.(sel).filename, structImgs.(sel).frameNo)
                  IF sel LT nImg/2 THEN sel2=nImg/2+sel ELSE sel2=sel-nImg/2
                  img2=readImg(structImgs.(sel2).filename, structImgs.(sel2).frameNo)
                  img2=ROTATE(img2,5);flip left/right
  
                  activeResImg=SQRT(img1*img2)
  
                  tvRes=activeResImg
                  szGeomMean=SIZE(activeResImg, /DIMENSIONS)
                  szX=450
                  szY=ROUND(szX*(szGeomMean(1)*1./szGeomMean(0)))
                  WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
                  WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
                  rangeWL=LONG([lower,upper])
                  TVSCL,congrid(adjustWindowLevel(tvRes, rangeWL), szX, szY, /INTERP)
                  XYOUTS, 0.05,0.05, 'Geometric mean for selected frame', CHARSIZE=1.5, COLOR=255
                ENDIF
                END

              ELSE:
            ENDCASE; tests

          ENDIF
        END
        ;******************************** SPECT *************************************************
;        3:BEGIN
;
;          curTab=WIDGET_INFO(wtabAnalysisSPECT, /TAB_CURRENT)
;          IF results(curTab) EQ 1 THEN BEGIN
;
;            CASE analyse OF
;
;              ELSE:
;            ENDCASE; tests
;
;          ENDIF
;
;        END

        ;******************************** PET *************************************************
;        4:BEGIN
;          curTab=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
;          IF results(curTab) EQ 1 THEN BEGIN
;
;            CASE analyse OF
;
;
;              ELSE:
;            ENDCASE
;          ENDIF
;        END

        ;******************************** MR *************************************************
        5:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisMR, /TAB_CURRENT)
          ;IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF
              'SNR':BEGIN
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
                  
                  idIsFirst=WHERE(firstImages EQ sel)
                  IF idIsFirst(0) NE -1 AND idIsFirst(0) NE nSets THEN BEGIN
                      i=firstImages(idIsFirst(0))
                      img1=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                      szImg1=SIZE(img1, /DIMENSIONS)
                      i2=secondImages(idIsFirst(0))
                      img2=readImg(structImgs.(i2).filename, structImgs.(i2).frameNo)
                      szImg2=SIZE(img2, /DIMENSIONS)
                      IF ARRAY_EQUAL(szImg1, szImg2) THEN BEGIN
                        activeResImg=img1-img2
                        tvRes=activeResImg
                        szX=450
                        szY=ROUND(szX*(szImg1(1)*1./szImg1(0)))
                        rangeWL=LONG([MIN(activeResImg),MAX(activeResImg)])
                        TVSCL,congrid(adjustWindowLevel(tvRes, rangeWL), szX, szY, /INTERP)
                        XYOUTS, 0.05,0.05, 'img'+STRING(i+1,FORMAT='(i0)')+'-img'+STRING(i2+1,FORMAT='(i0)')+' Window Level = [min,max] of diff', CHARSIZE=1.5, COLOR=255
                      ENDIF
                  ENDIF
                ENDIF
                END

              ELSE:
            ENDCASE
          ;ENDIF
        END

        ELSE:

      ENDCASE; modes


    ENDIF;selected image not marked - no results shown

  ENDIF;no results

end