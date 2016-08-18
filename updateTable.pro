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

pro updateTable

  COMMON VARI

  nCols=-1
  IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
  markedTemp=INDGEN(nImg); all default
  IF marked(0) EQ -1 THEN nRows=nImg ELSE BEGIN
    nRows=N_ELEMENTS(marked)
    markedTemp=marked
  ENDELSE
  
  cellSelHighLight=1

  curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
  CASE curMode OF
    ;********************************CT*************************************************
    0: BEGIN
      curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
      IF results(curTab) EQ 1 THEN BEGIN

        CASE analyseStringsCT(curTab+1) OF

          'ROI': BEGIN
            nCols=4
            headers=['Min','Max','Avg','Stdev']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(ROIres[*,markedTemp(i)], FORMAT='(f0.'+nDecimals(ROIres[*,markedTemp(i)])+')')
          END

          'MTF': BEGIN
            ;table results @50,10,2%          
            multipRes=WHERE(TAG_NAMES(MTFres) EQ 'M0')
            IF multipRes(0) NE -1 THEN BEGIN
              nCols=6
              headers=['MTFx 50%','MTFx 10%','MTFx 2%','MTFy 50%','MTFy 10%','MTFy 2%']
              resArrString=STRARR(nCols,nRows)
              FOR i =0, nRows-1 DO resArrString[*,i]=STRING(MTFres.(markedTemp(i)).F50_10_2, FORMAT='(F0.2)')
            ENDIF ELSE BEGIN
              nRows=1
              nCols=3
              headers=['MTF 50%','MTF 10%','MTF 2%']
              resArrString=STRING(MTFres.F50_10_2[0:2], FORMAT='(F0.2)')
              cellSelHighLight=0
            ENDELSE
          END

          'CTLIN': BEGIN
            szTab=SIZE(CTlinRes, /DIMENSIONS)
            nCols=szTab(0) ; only mean values
            headers=[materialData.field1, 'r^2']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO BEGIN
              resArrString[*,i]=STRING(CTlinRes[*,markedTemp(i)], FORMAT='(F0.1)')
              resArrString[nCols-1,i]=STRING(CTlinRes[nCols-1,markedTemp(i)], FORMAT='(F0.4)')
            ENDFOR
          END

          'SLICETHICK': BEGIN
            nCols=7
            headers=['Nominal','H1','H2','V1','V2','Avg','Diff nominal (%)']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO BEGIN
              resArrString[0:5,i]=STRING(sliceThickResTab[0:5,markedTemp(i)], FORMAT='(f0.2)')
              resArrString[6,i]=STRING(sliceThickResTab[6,markedTemp(i)], FORMAT='(f0.1)')
            ENDFOR
          END

          'HOMOG': BEGIN
            szTab=SIZE(homogRes, /DIMENSIONS)
            nCols=szTab(0)
            headers=['Center HU','Diff 12','Diff 15','Diff 18','Diff 21','Std center', 'Std 12','Std 15','Std 18','Std 21']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO BEGIN
              resArrString[0,i]=STRING(homogRes[0,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[0,markedTemp(i)])+')')
              resArrString[1:4,i]=STRING(homogRes[1:4,markedTemp(i)]-homogRes[0,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[1:4,markedTemp(i)])+')')
              resArrString[5:9,i]=STRING(homogRes[5:9,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[5:9,markedTemp(i)])+')')
            ENDFOR
          END

          'NOISE': BEGIN
            nCols=4
            headers=['CT number (HU)','Noise=Stdev (HU)','Diff avg noise(%)', 'Avg noise (HU)']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(noiseRes[*,markedTemp(i)],FORMAT='(f0.'+nDecimals(noiseRes[*,markedTemp(i)])+')')
            IF nRows GT 1 THEN resArrString[3,1:nRows-1]=''
          END

          'FWHM': BEGIN
            nCols=3
            headers=['FWHM','FWHM_korr','mcorr']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(fwhmRes[*,markedTemp(i)], FORMAT='(f0.4)')
          END

          'DIM': BEGIN
            nCols=6
            headers=['Horisontal 1','Horisontal 2','Vertical 1','Vertical 2','Diagonal 1','Diagonal 2']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(dimRes[0:5,markedTemp(i)], FORMAT='(f0.2)')
          END

          ELSE:
        ENDCASE

      ENDIF
      END
      ;********************************Xray*************************************************
      1:BEGIN
        curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
        IF results(curTab) EQ 1 THEN BEGIN

          CASE analyseStringsXray(curTab+1) OF
            'STP':BEGIN
              nCols=4
              headers=['Dose','Q','Mean pix','Stdev pix']
              resArrString=STRARR(nCols,nRows)
              WIDGET_CONTROL, txtRQA, GET_VALUE=Qvalue
              Qvalue=LONG(Qvalue(0))
              stpRes.table[1,*]=stpRes.table[0,*]*Qvalue
              FOR j=0, nRows-1 DO BEGIN
                FOR i=0, nCols-1 DO BEGIN
                  resArrString[i,j]=STRING(stpRes.table[i,markedTemp(j)], FORMAT='(f0.'+nDecimals(stpRes.table[i,markedTemp(j)])+')')
                ENDFOR
              ENDFOR
            END
            'HOMOG':BEGIN
              szTab=SIZE(homogRes, /DIMENSIONS)
              nCols=szTab(0)
              headers=['Center','UpperLeft','LowerLeft','UpperRight','LowerRight','Std center', 'Std UL','Std LL','Std UR','Std LR']
              resArrString=STRARR(nCols,nRows)
              FOR i=0, nRows-1 DO resArrString[*,i]=STRING(homogRes[*,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[*,markedTemp(i)])+')')
            END
            'NOISE':BEGIN
              nCols=2
              headers=['Mean pixel value','Stdev']
              resArrString=STRARR(nCols,nRows)
              FOR i=0, nRows-1 DO resArrString[*,i]=STRING(noiseRes[*,markedTemp(i)], FORMAT='(f0.'+nDecimals(noiseRes[*,markedTemp(i)])+')')
            END
            'ROI':BEGIN
              nCols=4
              headers=['Min','Max','Avg','Stdev']
              resArrString=STRARR(nCols,nRows)
              FOR i=0, nRows-1 DO resArrString[*,i]=STRING(ROIres[*,markedTemp(i)], FORMAT='(f0.'+nDecimals(ROIres[*,markedTemp(i)])+')')
            END
            'MTF':BEGIN
              ;table results 0.5-2.5 lp/mm + frq @ MTF 0.5
              nCols=6
              headers=['MTF @ 0.5/mm','MTF @ 1.0/mm','MTF @ 1.5/mm','MTF @ 2.0/mm','MTF @ 2.5/mm','Freq @ MTF 0.5']
              resArrString=STRARR(nCols,nRows)
              multipRes=WHERE(TAG_NAMES(MTFres) EQ 'M0')
              IF multipRes(0) NE -1 THEN BEGIN
                FOR i =0, nRows-1 DO IF N_TAGS(MTFres.(markedTemp(i))) NE 1 THEN resArrString[*,i]=STRING(MTFres.(markedTemp(i)).lpmm, FORMAT='(F0.2)')
              ENDIF ELSE resArrString[*,0]=STRING(MTFres.lpmm, FORMAT='(F0.2)')
            END
            'NPS':
          ENDCASE
        ENDIF
      END
      ;******************************** NM *************************************************
      2:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyseStringsNM(curTab+1) OF
              'ENERGYSPEC': BEGIN
                nCols=7
                nRows=2
                cellSelHighLight=0
                headers=['Method','Max (counts)','keV at max','Stdev (keV)','FWHM','FWTM','Energy resolution']
                resArrString=STRARR(nCols,nRows)
                resArrString(0,0)='Interpolated'
                maxC=MAX(energyRes.curve[1,*])
                resArrString(1,0)=STRING(maxC, FORMAT='(i0)')
                posMax=WHERE(energyRes.curve[1,*] EQ maxC)
                resArrString(2,0)=STRING(energyRes.curve[0,posMax], FORMAT='(f0.2)')
                resArrString(3,0)='-'
                dx=TOTAL(energyRes.curve[0,posMax-10:posMax+10]-energyRes.curve[0,posMax-11:posMax+9])/21.;mean dx assume close to equispaced
                fwhm=(getWidthAtThreshold(energyRes.curve[1,*],0.5*maxC))*dx  &  fwhm=fwhm(0)
                fwtm=(getWidthAtThreshold(energyRes.curve[1,*],0.1*maxC))*dx  &  fwtm=fwtm(0)  
                resArrString(4,0)=STRING(fwhm, FORMAT='(f0.'+nDecimals(fwhm)+')')
                resArrString(5,0)=STRING(fwtm, FORMAT='(f0.'+nDecimals(fwtm)+')')
                resArrString(6,0)=STRING(100.0*fwhm/energyRes.curve[0,posMax], FORMAT='(f0.2," %")')
                
                resArrString(0,1)='Gaussian fit'
                resArrString(1,1)=STRING(energyRes.gausscoeff(0), FORMAT='(f0.'+nDecimals(energyRes.gausscoeff(0))+')')
                resArrString(2,1)=STRING(energyRes.gausscoeff(1), FORMAT='(f0.'+nDecimals(energyRes.gausscoeff(1))+')')
                resArrString(3,1)=STRING(energyRes.gausscoeff(2), FORMAT='(f0.'+nDecimals(energyRes.gausscoeff(2))+')')
                fwhm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(2))
                fwtm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(10))
                resArrString(4,1)=STRING(fwhm, FORMAT='(f0.'+nDecimals(fwhm)+')')
                resArrString(5,1)=STRING(fwtm, FORMAT='(f0.'+nDecimals(fwtm)+')')
                resArrString(6,1)=STRING(100.0*fwhm/energyRes.gausscoeff(1), FORMAT='(f0.2," %")')
                END
                
              'HOMOG':BEGIN
                  szTab=SIZE(homogRes, /DIMENSIONS)
                  WIDGET_CONTROL, cw_homogNM, GET_VALUE=typeHomogNM
                  nCols=szTab(0)+5
                  CASE typeHomogNM OF
                    0: BEGIN ;planar WB                 
                        headers=['LowerLeft','LowerRight','Center','UpperLeft','UpperRight','Std LL', 'Std LR','Std C','Std UL','Std UR','% LL','% LR','% C','% UL','% UR']
                        resArrString=STRARR(nCols,nRows)
                        FOR i=0, nRows-1 DO BEGIN
                          resArrString[0:SzTab(0)-1,i]=STRING(homogRes[*,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[*,markedTemp(i)])+')')
                          resArrString[SzTab(0):nCols-1,i]=STRING(100.*homogRes[5:9,markedTemp(i)]/homogRes[0:4,markedTemp(i)], FORMAT='(f0.1)')
                        ENDFOR
                      END
                    1: BEGIN ; spect
                      headers=['Center','12','15','18','21','Std C', 'Std 12','Std 15','Std 18','Std 21','% C', '% 12', '% 15','% 18','% 21']
                      resArrString=STRARR(nCols,nRows)
                      FOR i=0, nRows-1 DO BEGIN
                        resArrString[0:4,i]=STRING(homogRes[0:4,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[0:4,markedTemp(i)])+')')
                        resArrString[5:9,i]=STRING(homogRes[5:9,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[5:9,markedTemp(i)])+')')
                        perRms=100.*homogRes[5:9,markedTemp(i)]/homogRes[0:4,markedTemp(i)]
                        resArrString[10:14,i]=STRING(perRms, FORMAT='(f0.'+nDecimals(perRms)+')')
                      ENDFOR
                      END
                     ELSE:
                     ENDCASE
                END
              'CONTRAST':BEGIN
                  szTab=SIZE(contrastRes, /DIMENSIONS)
                  nCols=szTab(0)*2-1
                  headers=['min 1','min 2','min 3','min 4','min 5','min 6', 'Background','%Con 1','%Con 2','%Con 3','%Con 4','%Con 5','%Con 6']
                  resArrString=STRARR(nCols,nRows)
                  FOR i=0, nRows-1 DO BEGIN
                    resArrString[0:5,i]=STRING(contrastRes[0:5,markedTemp(i)], FORMAT='(f0.'+nDecimals(contrastRes[0:5,markedTemp(i)])+')')
                    resArrString[6,i]=STRING(contrastRes[6,markedTemp(i)], FORMAT='(f0.'+nDecimals(contrastRes[6,markedTemp(i)])+')')
                    contr=-(contrastRes[0:5,markedTemp(i)]-contrastRes(6,markedTemp(i)))/contrastRes(6,markedTemp(i))
                    resArrString[7:12,i]=STRING(contr, FORMAT='(f0.'+nDecimals(contr)+')')
                  ENDFOR
                END
                'MTF': BEGIN
                  ;CT..................table results @50,10,2%
;                  multipRes=WHERE(TAG_NAMES(MTFres) EQ 'M0')
;                  IF multipRes(0) NE -1 THEN BEGIN
;                    nCols=6
;                    headers=['MTFx 50%','MTFx 10%','MTFx 2%','MTFy 50%','MTFy 10%','MTFy 2%']
;                    resArrString=STRARR(nCols,nRows)
;                    FOR i =0, nRows-1 DO resArrString[*,i]=STRING(MTFres.(markedTemp(i)).F50_10_2, FORMAT='(F0.2)')
;                  ENDIF ELSE BEGIN
;                    nRows=1
;                    nCols=3
;                    headers=['MTF 50%','MTF 10%','MTF 2%']
;                    resArrString=STRING(MTFres.F50_10_2[0:2], FORMAT='(F0.2)')
;                    cellSelHighLight=0
;                  ENDELSE
                  
                  ;xray ............table results
;                  nCols=6
;                  headers=['MTF @ 0.5/mm','MTF @ 1.0/mm','MTF @ 1.5/mm','MTF @ 2.0/mm','MTF @ 2.5/mm','Freq @ MTF 0.5']
;                  resArrString=STRARR(nCols,nRows)
;                  multipRes=WHERE(TAG_NAMES(MTFres) EQ 'M0')
;                  IF multipRes(0) NE -1 THEN BEGIN
;                    FOR i =0, nRows-1 DO resArrString[*,i]=STRING(MTFres.(markedTemp(i)).lpmm, FORMAT='(F0.2)')
;                  ENDIF ELSE resArrString[*,0]=STRING(MTFres.lpmm, FORMAT='(F0.2)')
                END
              ELSE:
            ENDCASE
           ENDIF
        END

  ENDCASE

  IF nCols EQ -1 THEN BEGIN
    WIDGET_CONTROL, resTab, TABLE_XSIZE=4, TABLE_YSIZE=2, COLUMN_LABELS=['0','1','2','3'], COLUMN_WIDTHS=[100,100,100,100], SET_VALUE=STRARR(4,5), SET_TABLE_SELECT=[-1,-1,-1,-1], SET_TABLE_VIEW=[0,0]
  ENDIF ELSE BEGIN
    ;which cell should be highlighted
    IF cellSelHighLight THEN BEGIN
      markedArr=INTARR(nImg)
      tabSelect=[-1,-1,-1,-1]
      tabView=[0,0]
      IF marked(0) NE -1 THEN markedArr(marked)=1 ELSE markedArr=markedArr+1
      sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
      IF markedArr(sel) EQ 1 THEN BEGIN
        nMarked=TOTAL(markedArr)
        IF marked(0) EQ -1 THEN rowNo=sel ELSE rowNo=WHERE(marked EQ sel)
        ; select row in result-table according to file
        oldSel=WIDGET_INFO(resTab,/TABLE_SELECT)
        colNo=oldSel(0);keep column number
        tabSelect=[colNo,rowNo,colNo,rowNo]
        IF rowNo GT nMarked-5 THEN top=nMarked-5 ELSE top=rowNo
        tabView=[0,top]
      ENDIF
    ENDIF

    IF nCols GT -1 THEN $
      WIDGET_CONTROL, resTab, TABLE_XSIZE=nCols, TABLE_YSIZE=nRows, COLUMN_LABELS=headers, COLUMN_WIDTHS=INTARR(nCols)+570/nCols, SET_VALUE=resArrString, SET_TABLE_SELECT=tabSelect $
    ELSE WIDGET_CONTROL, resTab, TABLE_XSIZE=4, TABLE_YSIZE=2, COLUMN_LABELS=['0','1','2','3'], COLUMN_WIDTHS=[100,100,100,100], SET_VALUE=STRARR(4,5), SET_TABLE_SELECT=tabSelect
    WIDGET_CONTROL, resTab, SET_TABLE_VIEW=tabView
  ENDELSE


end