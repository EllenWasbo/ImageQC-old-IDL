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
  TVSCL, INTARR(400,400)

  curMode=WIDGET_INFO(wTabModes, /TAB_CURRENT)

  IF results(getResNmb(curMode,analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsSPECT, analyseStringsPET)) THEN BEGIN

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
                activeResImg=unifRes.(sel+1).matrix
                TVSCL, activeResImg
              END
              'SNI': BEGIN
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

        ELSE:

      ENDCASE; modes


    ENDIF;selected image not marked - no results shown

  ENDIF;no results

end