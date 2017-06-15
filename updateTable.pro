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

pro updateTable

  COMMON VARI

nCols=-1
  IF analyse NE 'ENERGYSPEC' THEN BEGIN
    sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)  
    IF nFrames EQ 0 THEN BEGIN
      nImg=N_TAGS(structImgs)
      pix=structImgs.(sel).pix(0)
    ENDIF ELSE BEGIN
      nImg=nFrames
      pix=structImgs.(0).pix(0)
    ENDELSE
    markedTemp=INDGEN(nImg); all default
    IF marked(0) EQ -1 THEN nRows=nImg ELSE BEGIN
      nRows=N_ELEMENTS(marked)
      markedTemp=marked
    ENDELSE
  ENDIF

  cellSelHighLight=1

  curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
  CASE curMode OF
    ;********************************CT*************************************************
    0: BEGIN
      curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
      IF results(curTab) EQ 1 THEN BEGIN

        CASE analyse OF

          'ROI': BEGIN
            nCols=4
            headers=['Min','Max','Avg','Stdev']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(ROIres[*,markedTemp(i)], FORMAT=formatCode(ROIres[*,markedTemp(i)]))
          END

          'MTF': BEGIN
            ;table results @50,10,2%
            multipRes=WHERE(TAG_NAMES(MTFres) EQ 'M0')
            IF multipRes(0) NE -1 THEN BEGIN
              nCols=6
              headers=['MTFx 50%','MTFx 10%','MTFx 2%','MTFy 50%','MTFy 10%','MTFy 2%']
              resArrString=STRARR(nCols,nRows)
              FOR i =0, nRows-1 DO resArrString[*,i]=STRING(MTFres.(markedTemp(i)).F50_10_2, FORMAT='(F0.3)')
            ENDIF ELSE BEGIN
              nRows=1
              nCols=3
              headers=['MTF 50%','MTF 10%','MTF 2%']
              resArrString=STRING(MTFres.F50_10_2[0:2], FORMAT='(F0.3)')
              cellSelHighLight=0
            ENDELSE
          END

          'CTLIN': BEGIN
            szTab=SIZE(CTlinRes, /DIMENSIONS)
            nCols=szTab(0)
            WIDGET_CONTROL, tblLin, GET_VALUE=linTable
            headers=TRANSPOSE(linTable[0,*])
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO BEGIN
              resArrString[*,i]=STRING(CTlinRes[*,markedTemp(i)], FORMAT='(F0.1)')
              ;resArrString[nCols-1,i]=STRING(CTlinRes[nCols-1,markedTemp(i)], FORMAT='(F0.4)')
            ENDFOR
          END

          'SLICETHICK': BEGIN
            nCols=7
            WIDGET_CONTROL, cw_ramptype, GET_VALUE=ramptype
            IF ramptype EQ 0 THEN headers=['Nominal','H1','H2','V1','V2','Avg','Diff nominal (%)'] ELSE headers=['Nominal','H1','H2','V1','V2','inner V1','inner V2']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO BEGIN
              resArrString[0:5,i]=STRING(sliceThickResTab[0:5,markedTemp(i)], FORMAT='(f0.2)')
              resArrString[6,i]=STRING(sliceThickResTab[6,markedTemp(i)], FORMAT='(f0.2)');f0.1? for only wired ramp??
            ENDFOR
          END

          'HOMOG': BEGIN
            szTab=SIZE(homogRes, /DIMENSIONS)
            nCols=9
            headers=['12oClock','15oClock','18oClock','21oClock','Center HU','Diff C 12','Diff C 15','Diff C 18','Diff C 21'];'Std 12','Std 15','Std 18','Std 21','Std center']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO BEGIN
              resArrString[0:3,i]=STRING(homogRes[1:4,markedTemp(i)], FORMAT=formatCode(homogRes[1:4,markedTemp(i)]))
              resArrString[4,i]=STRING(homogRes[0,markedTemp(i)], FORMAT=formatCode(homogRes[0,markedTemp(i)]))
              resArrString[5:8,i]=STRING(homogRes[1:4,markedTemp(i)]-homogRes[0,markedTemp(i)], FORMAT=formatCode(homogRes[1:4,markedTemp(i)]))
              ;resArrString[5:8,i]=STRING(homogRes[6:9,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[6:9,markedTemp(i)])+')')
              ;resArrString[9,i]=STRING(homogRes[5,markedTemp(i)], FORMAT='(f0.'+nDecimals(homogRes[5,markedTemp(i)])+')')
            ENDFOR
          END

          'NOISE': BEGIN
            nCols=4
            headers=['CT number (HU)','Noise=Stdev (HU)','Diff avg noise(%)', 'Avg noise (HU)']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(noiseRes[*,markedTemp(i)],FORMAT=formatCode(noiseRes[*,markedTemp(i)]))
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

        CASE analyse OF
          'STP':BEGIN
            nCols=4
            headers=['Dose','Q','Mean pix','Stdev pix']
            resArrString=STRARR(nCols,nRows)
            WIDGET_CONTROL, txtRQA, GET_VALUE=Qvalue
            Qvalue=LONG(Qvalue(0))
            stpRes.table[1,*]=stpRes.table[0,*]*Qvalue
            FOR j=0, nRows-1 DO BEGIN
              FOR i=0, nCols-1 DO BEGIN
                resArrString[i,j]=STRING(stpRes.table[i,markedTemp(j)], FORMAT=formatCode(stpRes.table[i,markedTemp(j)]))
              ENDFOR
            ENDFOR
          END
          'HOMOG':BEGIN
            szTab=SIZE(homogRes, /DIMENSIONS)
            nCols=szTab(0)
            headers=['Center','UpperLeft','LowerLeft','UpperRight','LowerRight','Std center', 'Std UL','Std LL','Std UR','Std LR']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(homogRes[*,markedTemp(i)], FORMAT=formatCode(homogRes[*,markedTemp(i)]))
          END
          'NOISE':BEGIN
            nCols=2
            headers=['Mean pixel value','Stdev']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(noiseRes[*,markedTemp(i)], FORMAT=formatCode(noiseRes[*,markedTemp(i)]))
          END
          'ROI':BEGIN
            nCols=4
            headers=['Min','Max','Avg','Stdev']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(ROIres[*,markedTemp(i)], FORMAT=formatCode(ROIres[*,markedTemp(i)]))
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

        CASE analyse OF
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
            IF posMax GT N_ELEMENTS(energyRes.curve[0,*])-11 THEN BEGIN
              sv=DIALOG_MESSAGE('Max point in curve to close to end of curve. Calculation not possible.')
            ENDIF ELSE BEGIN
              resArrString(2,0)=STRING(energyRes.curve[0,posMax], FORMAT='(f0.2)')
              resArrString(3,0)='-'
              dx=TOTAL(energyRes.curve[0,posMax-10:posMax+10]-energyRes.curve[0,posMax-11:posMax+9])/21.;mean dx assume close to equispaced
              fwhm=(getWidthAtThreshold(energyRes.curve[1,*],0.5*maxC))*dx  &  fwhm=fwhm(0)
              fwtm=(getWidthAtThreshold(energyRes.curve[1,*],0.1*maxC))*dx  &  fwtm=fwtm(0)
              resArrString(4,0)=STRING(fwhm, FORMAT=formatCode(fwhm))
              resArrString(5,0)=STRING(fwtm, FORMAT=formatCode(fwtm))
              resArrString(6,0)=STRING(100.0*fwhm/energyRes.curve[0,posMax], FORMAT='(f0.2," %")')

              resArrString(0,1)='Gaussian fit'
              resArrString(1,1)=STRING(energyRes.gausscoeff(0), FORMAT=formatCode(energyRes.gausscoeff(0)))
              resArrString(2,1)=STRING(energyRes.gausscoeff(1), FORMAT=formatCode(energyRes.gausscoeff(1)))
              resArrString(3,1)=STRING(energyRes.gausscoeff(2), FORMAT=formatCode(energyRes.gausscoeff(2)))
              fwhm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(2))
              fwtm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(10))
              resArrString(4,1)=STRING(fwhm, FORMAT=formatCode(fwhm))
              resArrString(5,1)=STRING(fwtm, FORMAT=formatCode(fwtm))
              resArrString(6,1)=STRING(100.0*fwhm/energyRes.gausscoeff(1), FORMAT='(f0.2," %")')
            ENDELSE
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
                  resArrString[0:SzTab(0)-1,i]=STRING(homogRes[*,markedTemp(i)], FORMAT=formatCode(homogRes[*,markedTemp(i)]))
                  resArrString[SzTab(0):nCols-1,i]=STRING(100.*homogRes[5:9,markedTemp(i)]/homogRes[0:4,markedTemp(i)], FORMAT='(f0.1)')
                ENDFOR
              END
              1: BEGIN ; spect
                headers=['Center','12','15','18','21','Std C', 'Std 12','Std 15','Std 18','Std 21','% C', '% 12', '% 15','% 18','% 21']
                resArrString=STRARR(nCols,nRows)
                FOR i=0, nRows-1 DO BEGIN
                  resArrString[0:4,i]=STRING(homogRes[0:4,markedTemp(i)], FORMAT=formatCode(homogRes[0:4,markedTemp(i)]))
                  resArrString[5:9,i]=STRING(homogRes[5:9,markedTemp(i)], FORMAT=formatCode(homogRes[5:9,markedTemp(i)]))
                  perRms=100.*homogRes[5:9,markedTemp(i)]/homogRes[0:4,markedTemp(i)]
                  resArrString[10:14,i]=STRING(perRms, FORMAT=formatCode(perRms))
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
              resArrString[0:5,i]=STRING(contrastRes[0:5,markedTemp(i)], FORMAT=formatCode(contrastRes[0:5,markedTemp(i)]))
              resArrString[6,i]=STRING(contrastRes[6,markedTemp(i)], FORMAT=formatCode(contrastRes[6,markedTemp(i)]))
              contr=-(contrastRes[0:5,markedTemp(i)]-contrastRes(6,markedTemp(i)))/contrastRes(6,markedTemp(i))
              resArrString[7:12,i]=STRING(contr, FORMAT=formatCode(contr))
            ENDFOR
          END
          'MTF': BEGIN
            tagMTFres=tag_names(MTFres)
            resforeach=WHERE(tagMTFres EQ 'M0')
            v3d=0
            IF resforeach(0) EQ -1 THEN v3d=1

            WIDGET_CONTROL, cw_typeMTFNM, GET_VALUE=typeMTF
            IF typeMTF EQ 0 THEN nCols=4 ELSE nCols=2
            IF typeMTF EQ 0 THEN headers=['FWHM x (mm)','FWTM x (mm)','FWHM y (mm)','FWTM y (mm)'] ELSE headers=['FWHM (mm)','FWTM (mm)']

            IF v3d EQ 1 THEN BEGIN
              IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
                nRows=1
                cellSelHighLight=0
                xprof=MTFres.fitLSFx
                maxval=max(xprof)
                minval=min(xprof)
                resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                resArrString=[STRING(resFWHM(0)*0.1*pix,FORMAT='(f0.3)'),STRING(resFWTM(0)*0.1*pix,FORMAT='(f0.3)')]
              ENDIF
            ENDIF ELSE BEGIN
              multipRes=WHERE(tagMTFres EQ 'M0')
              IF multipRes(0) NE -1 THEN BEGIN
                resArrString=STRARR(nCols,nRows)
                FOR i =0, nRows-1 DO BEGIN
                  tagMTFresThis=tag_names(MTFres.(markedTemp(i)))
                  IF tagMTFresThis.HasValue('FITLSFX') THEN BEGIN
                    xprof=MTFres.(markedTemp(i)).fitLSFx
                    maxval=max(xprof)
                    minval=min(xprof)
                    resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                    resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                    
                    IF typeMTF EQ 0 THEN pixFac=1. ELSE pixFac=0.1
                    resArrString[0:1,i]=[STRING(resFWHM(0)*pixFac*pix,FORMAT='(f0.3)'),STRING(resFWTM(0)*pixFac*pix,FORMAT='(f0.3)')]
                    IF typeMTF EQ 0 THEN BEGIN
                      yprof=MTFres.(markedTemp(i)).fitLSFy
                      maxval=max(yprof)
                      minval=min(yprof)
                      resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                      resFWTM=getWidthAtThreshold(yprof,(maxval-minval)/10.+minval)
                      resArrString[2:3,i]=[STRING(resFWHM(0)*pixFac*pix,FORMAT='(f0.3)'),STRING(resFWTM(0)*pixFac*pix,FORMAT='(f0.3)')]
                    ENDIF
                    
                  ENDIF
                ENDFOR
              ENDIF

            ENDELSE

          END
          ELSE:
        ENDCASE
      ENDIF
    END; NM

    ;******************************** PET *************************************************
    3:BEGIN
      curTab=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
      IF results(curTab) EQ 1 THEN BEGIN

        CASE analyse OF
          'CROSSCALIB': BEGIN
            nCols=3
            headers=['Mean pixel value','stdev','Mean of means']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO resArrString[*,i]=STRING(crossRes[*,markedTemp(i)],FORMAT=formatCode(crossRes[*,markedTemp(i)]))
            IF nRows GT 1 THEN resArrString[2,1:nRows-1]=''
          END
          'HOMOG': BEGIN
            szTab=SIZE(homogRes, /DIMENSIONS)
            nCols=10
            headers=['Center','12oClock','15oClock','18oClock','21oClock','dMean C','dMean 12','dMean 15','dMean 18','dMean 21']
            resArrString=STRARR(nCols,nRows)
            resMarked=FLTARR(szTab(0),N_ELEMENTS(markedTemp))

            FOR i=0, nRows-1 DO resMarked[*,i]=homogRes[*,markedTemp(i)]
            meanVals=TOTAL(resMarked,2)/nRows
            formatVal=formatCode(MEAN(resMarked[0:4,*]))
            formatDiff=formatCode(MEAN(resMarked[5:9,*]))
            formatDiff=STRMID(formatDiff, 0, STRLEN(formatDiff)-1)
            FOR i=0, nRows-1 DO BEGIN
              resMarked[5:9,i]=100.*(resMarked[0:4,i]-meanVals)/meanVals
              resArrString[0:4,i]=STRING(resMarked[0:4,i], FORMAT=formatVal)
              resArrString[5:9,i]=STRING(resMarked[5:9,i], FORMAT=formatDiff+', "%")')
            ENDFOR
          END
          'RC': BEGIN
            nCols=7
            WIDGET_CONTROL, cw_rcType, GET_VALUE=meanOrMax
            IF meanOrMax EQ 0 THEN BEGIN
              headers =['A50 '+STRING(INDGEN(6)+1, FORMAT='(i0)'), 'BackGround']
              resArrString=[STRING(rcRes[7:12],FORMAT=formatCode(rcRes[7:12])), STRING(rcRes[6],FORMAT=formatCode(rcRes[6]))]
            ENDIF ELSE BEGIN
              headers =['Max '+STRING(INDGEN(6)+1, FORMAT='(i0)'), 'BackGround']
              resArrString=STRING(rcRes[0:6],FORMAT=formatCode(rcRes[0:6]))
            ENDELSE
          END
          ELSE:
        ENDCASE

      ENDIF
    END; PET

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
      WIDGET_CONTROL, resTab, TABLE_XSIZE=nCols, TABLE_YSIZE=nRows, COLUMN_LABELS=headers, COLUMN_WIDTHS=INTARR(nCols)+630/nCols, SET_VALUE=resArrString, SET_TABLE_SELECT=tabSelect $
    ELSE WIDGET_CONTROL, resTab, TABLE_XSIZE=4, TABLE_YSIZE=2, COLUMN_LABELS=['0','1','2','3'], COLUMN_WIDTHS=[100,100,100,100], SET_VALUE=STRARR(4,5), SET_TABLE_SELECT=tabSelect
    WIDGET_CONTROL, resTab, SET_TABLE_VIEW=tabView
    
  ENDELSE


end