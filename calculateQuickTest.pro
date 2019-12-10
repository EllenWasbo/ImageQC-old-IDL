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


function getValString, actTable, calc

  CASE calc OF
    0: val=actTable
    1: val=MIN(actTable)
    2: val=MAX(actTable)
    3: val=MEAN(actTable)
    4: val=STDDEV(actTable)
    5: val=MAX(ABS(actTable))
    ELSE:
  ENDCASE

  addValString=STRING(val, formatCode(val))

  RETURN, addValString
end

pro calculateQuickTest
  COMPILE_OPT hidden
  COMMON VARI

  IF TOTAL(multiOpt.(modality)) GT 0 THEN BEGIN

    imgWithMark=WHERE(TOTAL(markedMulti,1) GT 0, nTested)
    IF nTested GT 0 THEN BEGIN
      RESTORE, thisPath+'data\config.dat'
      qtOutName=configS.(selConfig).qtOutTemps(modality)
      qtOutNames=TAG_NAMES(quickTout.(modality))
      tempNmb=WHERE(qtOutNames EQ qtOutName)
      IF tempNmb NE -1 THEN currQTout=quickTout.(modality).(tempNmb) ELSE BEGIN
        sv=DIALOG_MESSAGE('The output template ('+qtOutName+') assigned to this parameter set is missing. Default is used.',DIALOG_PARENT=evTop)
        currQTout=quickTout.(modality).(0)
      ENDELSE
      currTestsOut=TAG_NAMES(currQTout)

      ;top list of images, adress and date
      nImg=N_TAGS(structImgs)
      szM=SIZE(markedMulti, /DIMENSIONS)
      IF N_ELEMENTS(szM) EQ 2 THEN nI=MIN([nImg,szM(1)]) ELSE nI=1
      ;incFilenames=WIDGET_INFO(btnIncFilename, /BUTTON_SET)
      ;IF incFilenames THEN BEGIN
        multiExpTable=STRARR(2, nTested+1)
        multiExpTable[*,0]=['Date',formatDMY(structImgs.(imgWithMark(0)).acqDate)] 
        
        cc=1
        FOR im=0, nI-1 DO BEGIN
          IF TOTAL(markedMulti[*,im]) GT 0 THEN BEGIN
            multiExpTable[*,cc]=['Img'+STRING(im+1, FORMAT='(i0)'),structImgs.(im).filename]
            cc=cc+1
          ENDIF
        ENDFOR
      
      ;ENDIF ELSE multiExpTable=['Date',formatDMY(structImgs.(imgWithMark(0)).acqDate)]

      analyseStrings=analyseStringsAll.(modality)

      szMM=SIZE(markedMulti,/DIMENSIONS)
      FOR tt=0, szMM(0)-1 DO BEGIN
        markedTemp=WHERE(markedMulti[tt,*] EQ 1)
        test=tt+1
        IF markedTemp(0) NE -1 THEN BEGIN
          cc=0
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          IF sel(0) NE markedTemp(0) THEN BEGIN
            WIDGET_CONTROL, listFiles, SET_LIST_SELECT=markedTemp(0)
            redrawImg,0,1 & updateInfo
          ENDIF
          CASE modality OF
            ;***************CT***************
            0:BEGIN; 'HOMOG', 'NOISE','SLICETHICK', 'MTF'

              testPos=WHERE(currTestsOut EQ analyseStrings(test-1))
              WIDGET_CONTROL,wtabAnalysisCT, SET_TAB_CURRENT=test-1
              analyse=analyseStrings(test-1)
              CASE test OF
                1:homog
                2:noise
                3:sliceThick
                4:mtf
                5:ctlin
                6:HUwater
                7:getHeaderInfo
                ELSE:
              ENDCASE
            END

            ;***************Xray***************
            1:BEGIN; 'STP','HOMOG','NOISE','EXP','MTF'

              testPos=WHERE(currTestsOut EQ analyseStrings(test-1))
              WIDGET_CONTROL,wtabAnalysisXray, SET_TAB_CURRENT=test-1
              analyse=analyseStrings(test-1)
              CASE test OF
                1: STPpix
                2: homog
                3: noise
                4: getExposure
                5: mtfx
                ELSE:
              ENDCASE
            END
            ;***************NM planar***************
            2:BEGIN
              testPos=WHERE(currTestsOut EQ analyseStrings(test-1))
              WIDGET_CONTROL,wtabAnalysisNM, SET_TAB_CURRENT=test-1
              analyse=analyseStrings(test-1)
              CASE test OF
                1: uniformityNM
                2: SNI
                3: getAcqNM
                4: barNM
                ELSE:
              ENDCASE
            END
            ;***************SPECT***************
            3:

            ;***************PET **************
            4:
            ;***************MR **************
            5:BEGIN
              testPos=WHERE(currTestsOut EQ analyseStrings(test-1))
              WIDGET_CONTROL,wtabAnalysisMR, SET_TAB_CURRENT=test-1
              analyse=analyseStrings(test-1)
              CASE test OF
                1: getDCM_MR
                ELSE:
              ENDCASE
            END
            
            ELSE:
          ENDCASE

          updateTable; update currentHeaderAlt+get tables
          WIDGET_CONTROL, resTab, GET_VALUE=resultsTable
          resultsTableString=resultsTable
          ;resultsTable=FLOAT(resultsTable)
          
          err=0

          If results(test-1) EQ 1 THEN BEGIN

            IF SIZE(currQTout.(testPos), /TNAME) EQ 'STRUCT' THEN BEGIN
              nOutp=N_TAGS(currQTout.(testPos))
              outpNames=TAG_NAMES(currQTout.(testPos))
              res3d=0
              FOR iii=0, nOutp-1 DO BEGIN

                alt=currQTout.(testPos).(iii).ALT
                IF currentHeaderAlt(test-1) EQ alt THEN BEGIN
                  cols=currQTout.(testPos).(iii).COLUMNS                 
                  calc=currQTout.(testPos).(iii).CALC

                  szRes=SIZE(resultsTable, /DIMENSIONS)
                  IF cols(0) EQ -1 THEN cols=INDGEN(szRes(0))
                  IF MAX(cols) GT szRes(0)-1 THEN BEGIN
                    err=1
                    lowEnoughID=WHERE(cols LT szRes(0))
                    IF lowEnoughID(0) NE -1 THEN cols=cols(lowEnoughID) ELSE cols=INDGEN(szRes(0))
                  ENDIF
                  
                  IF nImg EQ 1 THEN resUse=resultsTable(cols) ELSE BEGIN
                    resUse=resultsTable[cols(0),*]
                    IF N_ELEMENTS(cols) GT 1 THEN BEGIN
                        FOR r=1, N_ELEMENTS(cols)-1 DO resUse=[resUse, resultsTable[cols(r),*]]
                    ENDIF
                    IF N_ELEMENTS(szRes) EQ 1 THEN res3d=1
                  ENDELSE
                  resUseString=resUse
                  resUse=FLOAT(resUse)

                  IF currQTout.(testPos).(iii).PER_SERIES AND calc GT 0 AND res3d EQ 0 THEN BEGIN
                    serUniq=!Null
                    FOR im=0, nImg-1 DO serUniq=[serUniq,STRING(structImgs.(im).acqNmb,FORMAT='(i0)')+' '+STRING(structImgs.(im).seriesNmb,FORMAT='(i0)')+' '+STRING(structImgs.(im).seriesTime,FORMAT='(i06)')]

                    ;find number of series
                    nActIm=N_ELEMENTS(serUniq)
                    uniqSerNo=uniq(serUniq)
                    nSeries=N_ELEMENTS(uniqSerNo)
                    ;loop through series
                    FOR se=0, nSeries-1 DO BEGIN
                      imInSeries=WHERE(serUniq EQ serUniq(uniqSerNo(se)), nIm)
                      mmThis=markedMulti[tt,*]
                      IF TOTAL(mmThis(imInSeries)) GT 0 THEN BEGIN
                        actTable=FLTARR(N_ELEMENTS(cols),nIm)
                        FOR im=0, nIm-1 DO actTable[*,im]=resUse[*,imInSeries(im)]
                        imgTxt='_Series'+STRING(serUniq(uniqSerNo(se)), FORMAT='(i0)')
                        imgHead=outpNames(iii)
                        imgVal=getValString(actTable, calc)
                        multiExpTable=[[multiExpTable],[imgHead+imgTxt,imgVal]]
                      ENDIF
                    ENDFOR

                  ENDIF ELSE BEGIN
                    ;pr image or calc EQ 0 or nImg in series=1 or 3d (one row results independent on number of images)
                    nCols=1
                    IF calc EQ 0 THEN nCols=N_ELEMENTS(cols)
                    IF res3d THEN BEGIN
                      imgTxt='_AllSelected'
                      imgHead=outpNames(iii)
                      imgVal=getValString(resUse, calc)
                      multiExpTable=[[multiExpTable],[imgHead+imgTxt,imgVal]]
                    ENDIF ELSE BEGIN
                      FOR im=0, nI-1 DO BEGIN
                        IF markedMulti(tt,im) THEN BEGIN
                          addTable=STRARR(2,nCols)
                          addTable[*,*]='_'
                          ;addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                          IF calc GT 0 THEN BEGIN;calculation=one result value, not all values directly
                            addTable(0)=outpNames(iii)+'_Img'+STRING(im+1, FORMAT='(i0)')
                            IF N_ELEMENTS(cols) EQ 1 THEN addTable(1)=getValString(resUse(im), calc) ELSE addTable(1)=getValString(resUse[*,im], calc)
                          ENDIF ELSE BEGIN
                            heads=tableHeaders.(modality).(tt).(alt)
                            IF N_ELEMENTS(cols) EQ 1 THEN BEGIN
                              addTable(0)=heads(cols)+'_Img'+STRING(im+1, FORMAT='(i0)')
                              addTable(1)=resUseString(im);STRING(resUseString(im), FORMAT=formatCode(resUse(im)))
                            ENDIF ELSE BEGIN
                              addTable[0,*]=TRANSPOSE(heads(cols))+'_Img'+STRING(im+1, FORMAT='(i0)')
                              FOR c=0, nCols-1 DO addTable[1,c]=resUseString[c,im];STRING(resUseString[c,im], FORMAT=formatCode(resUse[c,im]))
                            ENDELSE

                          ENDELSE
                          multiExpTable=[[multiExpTable],[addTable]]
                        ENDIF
                      ENDFOR
                    ENDELSE

                  ENDELSE; PER_image
                ENDIF;No specified output for current test alternative
              ENDFOR;nOutp

            ENDIF ELSE BEGIN; outp=-1
              szRes=SIZE(resultsTable, /DIMENSIONS)
              resUse=resultsTable;(cols)
              nCols=szRes(0)
              alt=currentHeaderAlt(tt)

              FOR im=0, nI-1 DO BEGIN
                IF markedMulti(tt,im) THEN BEGIN
                  addTable=STRARR(2,nCols)
                  addTable[*,*]='_'
                  ;addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                  addTable[0,*]=TRANSPOSE(tableHeaders.(modality).(tt).(alt))+'_Img'+STRING(im+1, FORMAT='(i0)')
                  FOR c=0, nCols-1 DO addTable[1,c]=resUse[c,im];STRING(resUse[c,im], FORMAT=formatCode(resUse[c,im]))
                  multiExpTable=[[multiExpTable],[addTable]]
                ENDIF
              ENDFOR

            ENDELSE; outp=-1
            
            IF err THEN sv=DIALOG_MESSAGE('Some tables have less columns now than when the template was created. Check the templatemanager and your results. CT Number test especially prone to this issue.', DIALOG_PARENT=evTop)

        ENDIF; results NE=0
        
       ENDIF; none marked
        
      ENDFOR; tests

      ;updateTable
      redrawImg, 0,0
      updatePlot,1,1,0
      WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
    ENDIF
  ENDIF ELSE sv=DIALOG_MESSAGE('QuickTest not available for this mode yet (numbered tests only).', DIALOG_PARENT=evTop)

end