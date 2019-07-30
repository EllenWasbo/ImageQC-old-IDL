;ImageQC - quality control of medical images
;Copyright (C) 2018  Ellen Wasbo, Stavanger University Hospital, Norway
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

pro updateTableSup
  COMPILE_OPT hidden
  COMMON VARI

  nCols=-1
  IF analyse NE 'ENERGYSPEC' THEN BEGIN
    sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
    nImg=N_TAGS(structImgs)
    pix=structImgs.(sel).pix(0)
    markedArr=INTARR(nImg)
    IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
    markedTemp=WHERE(markedArr EQ 1)
    nRows=TOTAL(markedArr)
  ENDIF
  txtExplain=''

  cellSelHighLight=1

  curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
  CASE curMode OF
    ;********************************CT*************************************************
    0: BEGIN
      curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
      IF results(curTab) EQ 1 THEN BEGIN

        CASE analyse OF

          'HOMOG': BEGIN
            szTab=SIZE(homogRes, /DIMENSIONS)
            nCols=5
            headers=['Stdev 12','Stdev 15','Stdev 18','Stdev 21','Stdev Center']
            resArrString=STRARR(nCols,nRows)
            FOR i=0, nRows-1 DO BEGIN
              resArrString[0:3,i]=STRING(homogRes[6:9,markedTemp(i)], FORMAT=formatCode(homogRes[6:9,markedTemp(i)]))
              resArrString[4,i]=STRING(homogRes[5,markedTemp(i)], FORMAT=formatCode(homogRes[5,markedTemp(i)]))
            ENDFOR
          END

          'MTF': BEGIN
            resforeach=WHERE(TAG_NAMES(MTFres) EQ 'M0')
            v3d=0
            IF resforeach(0) EQ -1 THEN v3d=1

            WIDGET_CONTROL, cw_plotMTF, GET_VALUE= plotWhich

            CASE plotWhich OF
              0: BEGIN ;centered xy profile
                nCols=4
                headers=['centerPos X (mm)','centerPos Y (mm)','FWHM X (mm)','FWHM Y (mm)']
                txtExplain='FWHM of centered profiles'
                resArrString=STRARR(nCols,nRows)
                IF v3d THEN BEGIN
                  FOR j=0, nRows-1 DO BEGIN
                    IF j EQ 0 THEN resArrString[0:1,j]=STRING(pix*MTFres.centerPos, FORMAT='(f0.2)')
                    xprof=MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),j]
                    yprof=MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,j]
                    maxval=max(xprof)
                    minval=min(xprof)
                    resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                    resArrString[2,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                    maxval=max(yprof)
                    minval=min(yprof)
                    resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                    resArrString[3,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                  ENDFOR
                ENDIF ELSE BEGIN
                  FOR j=0, nRows-1 DO BEGIN
                    resArrString[0:1,j]=STRING(pix*MTFres.(markedTemp(j)).centerPos, FORMAT='(f0.2)')
                    xprof=MTFres.(markedTemp(j)).submatrixAll[*,ROUND(MTFres.(markedTemp(j)).centerPos(1)),0]
                    yprof=MTFres.(markedTemp(j)).submatrixAll[ROUND(MTFres.(markedTemp(j)).centerPos(0)),*,0]
                    maxval=max(xprof)
                    minval=min(xprof)
                    resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                    resArrString[0,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                    maxval=max(yprof)
                    minval=min(yprof)
                    resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                    resArrString[1,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                  ENDFOR
                  
                ENDELSE
              END
              3: BEGIN
                ;discrete table results @50,10,2%
                multipRes=WHERE(TAG_NAMES(MTFres) EQ 'M0')
                txtExplain='Tabulated results from discrete MTF'
                IF multipRes(0) NE -1 THEN BEGIN
                  nCols=6
                  headers=['MTFx 50%','MTFx 10%','MTFx 2%','MTFy 50%','MTFy 10%','MTFy 2%']
                  resArrString=STRARR(nCols,nRows)
                  FOR i =0, nRows-1 DO BEGIN
                    tagn=TAG_NAMES(MTFres.(markedTemp(i)))
                    IF tagn.HasValue('F50_10_2discrete') THEN resArrString[*,i]=STRING(MTFres.(markedTemp(i)).F50_10_2, FORMAT='(F0.3)')
                  ENDFOR
                ENDIF ELSE BEGIN
                  nRows=1
                  nCols=3
                  headers=['MTF 50%','MTF 10%','MTF 2%']
                  tagn=TAG_NAMES(MTFres)
                  IF tagn.HasValue('F50_10_2discrete') THEN resArrString=STRING(MTFres.F50_10_2[0:2], FORMAT='(F0.3)')
                  cellSelHighLight=0
                ENDELSE
              END
              ELSE:
            ENDCASE
          END

          'NPS': BEGIN

            v3d=WIDGET_INFO(btnNPSavg, /BUTTON_SET)
            IF v3d EQ 0 THEN BEGIN
              nCols=3
              headers=['AUC','Integral NPS','Avg variance ROIs']
              resArrString=STRARR(nRows,nCols)
              FOR j=0,nRows-1 DO BEGIN
                IF N_TAGS(NPSres.(j)) GT 1 THEN BEGIN
                  resArrString[*,j]=[STRING(NPSres.(markedTemp(j)).AUC, FORMAT=formatCode(NPSres.(markedTemp(j)).AUC)),$
                    STRING(NPSres.(markedTemp(j)).varianceIntNPS, FORMAT=formatCode(NPSres.(markedTemp(j)).varianceIntNPS)),$
                    STRING(NPSres.(markedTemp(j)).varianceImg^2, FORMAT=formatCode(NPSres.(markedTemp(j)).varianceImg^2))]
                ENDIF
              ENDFOR

            ENDIF ELSE BEGIN

              NPStot=0
              integAvg=0
              variAvg=0
              ftemp=0
              FOR i=0, nImg-1 DO BEGIN
                IF markedArr(i) THEN BEGIN
                  IF N_ELEMENTS(NPStot) EQ 1 THEN BEGIN
                    NPStot=NPSres.(i).rNPS
                    ftemp=NPSres.(i).dr
                  ENDIF ELSE NPStot=NPStot+NPSres.(i).rNPS
                  integAvg=integAvg+NPSres.(i).varianceIntNPS
                  variAvg=variAvg+NPSres.(i).varianceImg^2
                ENDIF
              ENDFOR
              NPStot=NPStot/TOTAL(markedArr)
              integAvg=integAvg/TOTAL(markedArr)
              variAvg=variAvg/TOTAL(markedArr)

              AUC=INT_TABULATED(NPSres.(sel).dr,NPStot)

              nCols=3
              nRows=1
              headers=['Avg. AUC','Avg. Integral NPS','Avg variance ROIs']
              resArrString=[STRING(AUC, FORMAT=formatCode(AUC)),$
                STRING(integAvg, FORMAT=formatCode(integAvg)),$
                STRING(variAvg, FORMAT=formatCode(variAvg))]

            ENDELSE
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
          'STP': BEGIN
            tagn=TAG_NAMES(stpRes)
            IF tagn.HasValue('A') THEN BEGIN
              nCols=3
              nRows=1
              headers=['Slope','Offset','mcorr']
              resArrString=[STRING(stpRes.a(0), FORMAT=formatCode(stpRes.a(0))),$
                STRING(stpRes.b, FORMAT=formatCode(stpRes.b)),$
                STRING(stpRes.mcorr, FORMAT=formatCode(stpRes.mcorr))]
              txtExplain='Curve fitted to y=ax+b a=slope, b=offset, mcorr=multiple linear correlation coefficient'
            ENDIF

          END

          'MTF': BEGIN

            IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
              WIDGET_CONTROL, cw_plotMTFX, GET_VALUE= plotWhich
              WIDGET_CONTROL, cw_formLSFX, GET_VALUE=formLSF

              CASE plotWhich OF

                0: BEGIN ;edge
                  nCols=1
                  headers='Edge angle'
                  FOR j=0, nRows-1 DO resArrString(j)=STRING(MTFres.(markedTemp(j)).angle,FORMAT='(f0.2)')
                  txtExplain='Angle of edge found relative to x or y axis whichever is nearest.'
                END
                ELSE:
              ENDCASE

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

          'MTF': BEGIN

            WIDGET_CONTROL, cw_plotMTFNM, GET_VALUE= plotWhich

            CASE plotWhich OF

              0: BEGIN ;centered xy profile
                nCols=4
                headers=['FWHM X (mm)','FWTM X (mm)','FWHM Y (mm)','FWTM Y (mm)']
                resArrString=STRARR(nCols,nRows)
                FOR j=0, nRows-1 DO BEGIN
                  tagsMTF=TAG_NAMES(MTFres.(markedTemp(j)))
                  IF tagsMTF.HasValue('CENTERPOS') THEN BEGIN
                    xprof=MTFres.(markedTemp(j)).submatrixAll[*,ROUND(MTFres.(markedTemp(j)).centerPos(1)),0]
                    yprof=MTFres.(markedTemp(j)).submatrixAll[ROUND(MTFres.(markedTemp(j)).centerPos(0)),*,0]
                    maxval=max(xprof)
                    minval=min(xprof)
                    resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                    resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                    resArrString[0,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                    maxval=max(yprof)
                    minval=min(yprof)
                    resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                    resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                    resArrString[1,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                  ENDIF
                ENDFOR
                txtExplain='FWHM and FWTM of profiles'
              END

              1: BEGIN ;line
                nCols=2
                headers=['Img nmb','Line angle (deg)']
                resArrString=STRARR(nCols, nRows)
                FOR j=0, nRows-1 DO BEGIN
                  resArrString[0,j]=STRING(markedTemp(j), FORMAT='(i0)')
                  tagsMTF=TAG_NAMES(MTFres.(markedTemp(j)))
                  IF tagsMTF.HasValue('ANGLE') THEN resArrString[1,j]=STRING(MTFres.(markedTemp(j)).angle,FORMAT='(f0.2)')
                ENDFOR
                txtExplain='Angle of line found relative to x or y axis whichever is nearest.'
              END

              ELSE:
            ENDCASE

          END; end test MTF

          ELSE:
        ENDCASE
      ENDIF
    END; NM

    ;******************************** SPECT *************************************************
    3:BEGIN
      curTab=WIDGET_INFO(wtabAnalysisSPECT, /TAB_CURRENT)
      IF results(curTab) EQ 1 THEN BEGIN

        CASE analyse OF
          'MTF': BEGIN
            resforeach=WHERE(TAG_NAMES(MTFres) EQ 'M0')
            v3d=0
            IF resforeach(0) EQ -1 THEN v3d=1

            WIDGET_CONTROL, cw_plotMTFSPECT, GET_VALUE= plotWhich

            CASE plotWhich OF

              0: BEGIN ;centered xy profile

                IF v3d EQ 0 THEN BEGIN

                  nCols=4
                  headers=['FWHM X (mm)','FWTM X (mm)','FWHM Y (mm)','FWTM Y (mm)']
                  resArrString=STRARR(nCols,nRows)
                  FOR j=0, nRows-1 DO BEGIN
                    tagsMTF=TAG_NAMES(MTFres.(markedTemp(j)))
                    IF tagsMTF.HasValue('CENTERPOS') THEN BEGIN
                      xprof=MTFres.(markedTemp(j)).submatrixAll[*,ROUND(MTFres.(markedTemp(j)).centerPos(1)),0]
                      yprof=MTFres.(markedTemp(j)).submatrixAll[ROUND(MTFres.(markedTemp(j)).centerPos(0)),*,0]
                      maxval=max(xprof)
                      minval=min(xprof)
                      resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                      resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                      resArrString[0,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                      maxval=max(yprof)
                      minval=min(yprof)
                      resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                      resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                      resArrString[1,j]=STRING(resFWHM(0)*pix, FORMAT='(f0.2)')
                    ENDIF
                  ENDFOR
                  txtExplain='FWHM and FWTM of profiles'
                ENDIF
              END

              1: BEGIN ;line
                IF v3d EQ 0 THEN BEGIN
                  nCols=1
                  headers='Line angle'
                  resArrString=STRARR(nRows)
                  FOR j=0, nRows-1 DO BEGIN
                    tagsMTF=TAG_NAMES(MTFres.(markedTemp(j)))
                    IF tagsMTF.HasValue('ANGLE') THEN resArrString(j)=STRING(MTFres.(markedTemp(j)).angle,FORMAT='(f0.3)')
                  ENDFOR
                  txtExplain='Angle of line found relative to x or y axis whichever is nearest.'
                ENDIF
              END

              3: BEGIN; LSF
                IF v3d NE 0 THEN BEGIN
                  tagMTFres=tag_names(MTFres)
                  IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
                    xprof=MTFres.fitLSFx
                    maxval=max(xprof)
                    minval=min(xprof)
                    resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                    resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                    nCols=2
                    nRows=1
                    headers=['FWHM','FWTM']
                    resArrString=[STRING(resFWHM(0)*0.1*pix, FORMAT='(f0.0)'),STRING(resFWTM(0)*.1*pix, FORMAT='(f0.0)')]
                    txtExplain='FWHM and FWTM of gaussian fit for smoothed LSF'
                  ENDIF
                ENDIF
            END

            ELSE:
          ENDCASE

        END; end test MTF

        ELSE:
      ENDCASE

    ENDIF
  END

  ;******************************** PET *************************************************
  4:;BEGIN
  ;        curTab=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
  ;        IF results(curTab) EQ 1 THEN BEGIN
  ;
  ;          CASE analyse OF
  ;
  ;            ELSE:
  ;          ENDCASE
  ;
  ;        ENDIF
  ;      END; PET

ENDCASE

IF nCols EQ -1 THEN BEGIN
  WIDGET_CONTROL, resTabSup, TABLE_XSIZE=4, TABLE_YSIZE=2, COLUMN_LABELS=['0','1','2','3'], COLUMN_WIDTHS=[100,100,100,100], $
    SET_VALUE=STRARR(4,5), SET_TABLE_SELECT=[-1,-1,-1,-1], SET_TABLE_VIEW=[0,0], FOREGROUND_COLOR=[0,0,0]
ENDIF ELSE BEGIN
  ;which cell should be highlighted
  IF cellSelHighLight AND nRows GT 1 THEN BEGIN
    markedArr=INTARR(nImg)
    tabSelect=[-1,-1,-1,-1]
    tabView=[0,0]
    IF marked(0) NE -1 THEN markedArr(marked)=1 ELSE markedArr=markedArr+1

    IF markedArr(sel) EQ 1 THEN BEGIN
      nMarked=TOTAL(markedArr)
      IF marked(0) EQ -1 THEN rowNo=sel ELSE rowNo=WHERE(marked EQ sel)
      ; select row in result-table according to file
      oldSel=WIDGET_INFO(resTabSup,/TABLE_SELECT)
      colNo=oldSel(0);keep column number
      tabSelect=[colNo,rowNo,colNo,rowNo]
      IF rowNo GT nMarked-5 THEN top=nMarked-5 ELSE top=rowNo
      tabView=[0,top]
    ENDIF
  ENDIF

  IF nCols GT 0 THEN BEGIN
    WIDGET_CONTROL, resTabSup, TABLE_XSIZE=nCols, TABLE_YSIZE=nRows, COLUMN_LABELS=headers, COLUMN_WIDTHS=INTARR(nCols)+630/nCols, SET_VALUE=resArrString, SET_TABLE_SELECT=tabSelect, ALIGNMENT=1
    WIDGET_CONTROL, resTabSup, FOREGROUND_COLOR=[0,0,0]
    IF markedMulti(0) NE -1 AND TOTAL(markedMulti) GT 0 AND nRows GT 1 THEN BEGIN
      testNmb=getResNmb(modality,analyse,analyseStringsAll)
      markedArrTemp=markedMulti[testNmb,*]
      FOR ii=0, nImg-1 DO BEGIN
        IF markedArrTemp(ii) EQ 0 THEN WIDGET_CONTROL, resTabSup, USE_TABLE_SELECT=[0,ii,nCols-1,ii], FOREGROUND_COLOR=[200,200,200]
      ENDFOR
    ENDIF
  ENDIF ELSE WIDGET_CONTROL, resTabSup, TABLE_XSIZE=4, TABLE_YSIZE=2, COLUMN_LABELS=['0','1','2','3'], COLUMN_WIDTHS=[100,100,100,100], SET_VALUE=STRARR(4,5), SET_TABLE_SELECT=tabSelect, FOREGROUND_COLOR=[0,0,0]
  WIDGET_CONTROL, resTabSup, SET_TABLE_VIEW=tabView

ENDELSE

WIDGET_CONTROL, lblResultsSup, SET_VALUE=txtExplain
end
