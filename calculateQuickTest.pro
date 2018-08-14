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

pro calculateQuickTest
  COMPILE_OPT hidden
  COMMON VARI
  
  IF TOTAL(multiOpt.(modality)) GT 0 THEN BEGIN

    imgWithMark=WHERE(TOTAL(markedMulti,1) GT 0, nTested)
    IF nTested GT 0 THEN BEGIN

      ;top list of images, adress and date
      multiExpTable=STRARR(3, nTested+4); +4=header)

      multiExpTable[*,0]=['Date_(first_img)','_',formatDMY(structImgs.(imgWithMark(0)).acqDate)]
      multiExpTable[*,1]=['ImageNo','_','Filename']
      nImg=N_TAGS(structImgs)
      cc=2
      FOR im=0, nImg-1 DO BEGIN
        IF TOTAL(markedMulti[*,im]) GT 0 THEN BEGIN
          multiExpTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'_',structImgs.(im).filename]
          cc=cc+1
        ENDIF
      ENDFOR
      multiExpTable[*,nTested+3]=['ImageNo','Parameter','Value']

      szMM=SIZE(markedMulti,/DIMENSIONS)
      FOR tt=0, szMM(0)-1 DO BEGIN
        markedTemp=WHERE(markedMulti[tt,*] EQ 1)
        test=tt+1
        addrows=N_ELEMENTS(markedTemp)
        IF addrows NE 0 THEN BEGIN
          cc=0
          CASE modality OF
            ;***************CT***************
            0:BEGIN; 'HOMOG', 'NOISE','SLICETHICK','DIM', 'MTF'
              CASE test OF
                1:BEGIN
                  homog
                  If results(test-1) EQ 1 THEN BEGIN
                    addTable=STRARR(3,addrows)
                    ;HU in central ROI homogtest (typically water or close to water)
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Center_HU',STRING(homogRes[0,im],FORMAT='(f0.2)')]
                        cc=cc+1
                      ENDIF
                    ENDFOR
                    multiExpTable=[[multiExpTable],[addTable]]
                    ;max and min diff from center HU periferral
                    tempTable=FLTARR(4,addrows)
                    cc=0
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        tempTable[*,cc]=homogRes[1:4,im]-homogRes[0,im]
                        cc=cc+1
                      ENDIF
                    ENDFOR
                    IF TOTAL(markedMulti[tt,*]) EQ 1 THEN BEGIN
                      imgNo=WHERE(markedMulti[tt,*] EQ 1)
                      imgTxt=STRING(imgNo+1, FORMAT='(i0)')
                    ENDIF ELSE imgTxt='selection'
                    multiExpTable=[[multiExpTable],[[imgTxt,'Max_diff_from_Center_HU',STRING(MAX(tempTable), FORMAT='(f0.2)')],['selection','Min_diff_from_Center_HU',STRING(MIN(tempTable), FORMAT='(f0.2)')]]]
                  ENDIF
                END
                2:BEGIN
                  noise
                  If results(test-1) EQ 1 THEN BEGIN
                    tempTable=FLTARR(addrows)
                    cc=0
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        tempTable(cc)=noiseRes[1,im]
                        cc=cc+1
                      ENDIF
                    ENDFOR
                    IF TOTAL(markedMulti[tt,*]) EQ 1 THEN BEGIN
                      imgNo=WHERE(markedMulti[tt,*] EQ 1)
                      imgTxt=STRING(imgNo+1, FORMAT='(i0)')
                    ENDIF ELSE imgTxt='selection'
                    multiExpTable=[[multiExpTable],[imgTxt,'Max_noise', STRING(MAX(tempTable), FORMAT='(f0.2)')]]
                  ENDIF
                END
                3:BEGIN
                  sliceThick
                  If results(test-1) EQ 1 THEN BEGIN
                    addTable=STRARR(3,addrows)
                    cc=0
                    WIDGET_CONTROL, cw_ramptype, GET_VALUE=ramptype
                    CASE ramptype OF
                      0: BEGIN
                        FOR im=0, nImg-1 DO BEGIN
                          IF markedMulti(tt,im) THEN BEGIN
                            addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Avg_Slicethick',STRING(sliceThickResTab[5,im],FORMAT='(f0.2)')]
                            cc=cc+1
                          ENDIF
                        ENDFOR
                      END
                      1: BEGIN
                        FOR im=0, nImg-1 DO BEGIN
                          IF markedMulti(tt,im) THEN BEGIN
                            addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Avg_Slicethick_Outer',STRING(0.25*TOTAL(sliceThickResTab[1:4,im]),FORMAT='(f0.2)')]
                            cc=cc+1
                          ENDIF
                        ENDFOR
                        multiExpTable=[[multiExpTable],[addTable]]
                        addTable=STRARR(3,addrows)
                        cc=0
                        FOR im=0, nImg-1 DO BEGIN
                          IF markedMulti(tt,im) THEN BEGIN
                            addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Avg_Slicethick_Inner',STRING(0.5*TOTAL(sliceThickResTab[5:6,im]),FORMAT='(f0.2)')]
                            cc=cc+1
                          ENDIF
                        ENDFOR
                      END
                      2: BEGIN
                        FOR im=0, nImg-1 DO BEGIN
                          IF markedMulti(tt,im) THEN BEGIN
                            addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Avg_Slicethick',STRING(0.5*TOTAL(sliceThickResTab[3:4,im]),FORMAT='(f0.2)')]
                            cc=cc+1
                          ENDIF
                        ENDFOR
                      END
                      ELSE:
                    ENDCASE

                    multiExpTable=[[multiExpTable],[addTable]]
                  ENDIF
                END
                4:BEGIN
                  mtf
                  If results(test-1) EQ 1 THEN BEGIN
                    multipRes=WHERE(TAG_NAMES(MTFres) EQ 'M0')
                    IF multipRes(0) NE -1 THEN BEGIN
                      addTable=STRARR(3,addrows)
                      cc=0
                      FOR im=0, nImg-1 DO BEGIN
                        IF markedMulti(tt,im) THEN BEGIN
                          addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Avg_x/y_MTF_50%',STRING(0.5*(MTFres.(im).F50_10_2(0)+MTFres.(im).F50_10_2(3)),FORMAT='(f0.3)')]
                          addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Avg_x/y_MTF_10%',STRING(0.5*(MTFres.(im).F50_10_2(1)+MTFres.(im).F50_10_2(4)),FORMAT='(f0.3)')]
                          cc=cc+1
                        ENDIF
                      ENDFOR
                      multiExpTable=[[multiExpTable],[addTable]]
                    ENDIF ELSE BEGIN
                      IF TOTAL(markedMulti[tt,*]) EQ 1 THEN BEGIN
                        imgNo=WHERE(markedMulti[tt,*] EQ 1)
                        imgTxt=STRING(imgNo+1, FORMAT='(i0)')
                      ENDIF ELSE imgTxt='selection'

                      multiExpTable=[[multiExpTable],[imgTxt,'MTF_50%',STRING(MTFres.F50_10_2(0), FORMAT='(F0.3)')]]
                      multiExpTable=[[multiExpTable],['_','MTF_10%',STRING(MTFres.F50_10_2(1), FORMAT='(F0.3)')]]
                    ENDELSE
                  ENDIF
                END
                ELSE:
              ENDCASE
            END
            ;***************Xray***************
            1:BEGIN; 'STP','HOMOG','NOISE','EI','MTF'
              cc=0
              CASE test OF
                1: BEGIN
                  STPpix
                  If results(test-1) EQ 1 THEN BEGIN
                    addTable=STRARR(3,addrows)
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'Pixel_mean',STRING(STPres.table[2,im],FORMAT='(f0.2)')]
                        cc=cc+1
                      ENDIF
                    ENDFOR
                    multiExpTable=[[multiExpTable],[addTable]]
                  ENDIF
                END
                2: BEGIN
                  homog
                  If results(test-1) EQ 1 THEN BEGIN
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable=STRARR(3,10)
                        addTable[*,*]='_'
                        addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                        addTable[1,*]=TRANSPOSE(['Center','UpperLeft','LowerLeft','UpperRight','LowerRight','Std_center', 'Std_UL','Std_LL','Std_UR','Std_LR'])
                        addTable[2,*]=TRANSPOSE(STRING(homogRes[*,im], FORMAT='(f0.2)'))
                        multiExpTable=[[multiExpTable],[addTable]]
                      ENDIF
                    ENDFOR
                  ENDIF
                END
                3:  BEGIN
                  noise
                  If results(test-1) EQ 1 THEN BEGIN
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable=STRARR(3,2)
                        addTable[*,*]='_'
                        addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                        addTable[1,*]=TRANSPOSE(['Noise_mean','Noise_stdev'])
                        addTable[2,*]=TRANSPOSE(STRING(noiseRes[*,im], FORMAT='(f0.3)'))
                        multiExpTable=[[multiExpTable],[addTable]]
                      ENDIF
                    ENDFOR
                  ENDIF
                END
                4:  BEGIN
                  getEI
                  If results(test-1) EQ 1 THEN BEGIN
                    addTable=STRARR(3,addrows)
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable[*,cc]=[STRING(im+1, FORMAT='(i0)'),'EI',STRING(eiRes[1,im],FORMAT='(f0.2)')]
                        cc=cc+1
                      ENDIF
                    ENDFOR
                    multiExpTable=[[multiExpTable],[addTable]]
                  ENDIF
                END
                5: BEGIN
                  mtfx
                  If results(test-1) EQ 1 THEN BEGIN
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable=STRARR(3,6)
                        addTable[*,*]='_'
                        addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                        addTable[1,*]=TRANSPOSE(['MTF_@_0.5/mm','MTF_@_1.0/mm','MTF_@_1.5/mm','MTF_@_2.0/mm','MTF_@_2.5/mm','Freq_@_MTF_0.5'])
                        addTable[2,*]=TRANSPOSE(STRING(MTFres.(im).lpmm, FORMAT='(F0.3)'))
                        multiExpTable=[[multiExpTable],[addTable]]
                      ENDIF
                    ENDFOR
                  ENDIF
                END
                ELSE:
              ENDCASE
            END
            ;***************NM planar***************
            2:BEGIN
              cc=0
              CASE test OF
                1: BEGIN
                  uniformityNM
                  If results(test-1) EQ 1 THEN BEGIN
                    addTable=STRARR(3,addrows)
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable=STRARR(3,4)
                        addTable[*,*]='_'
                        addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                        addTable[1,*]=TRANSPOSE(['IU_UFOV', 'DU_UFOV', 'IU_CFOV', 'DU_CFOV'])
                        addTable[2,*]=TRANSPOSE(STRING(unifRes.table[*,im], FORMAT='(F0.2)'))
                        multiExpTable=[[multiExpTable],[addTable]]
                      ENDIF
                    ENDFOR
                  ENDIF
                END
                2: BEGIN
                  SNI
                  If results(test-1) EQ 1 THEN BEGIN
                    addTable=STRARR(3,addrows)
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable=STRARR(3,9)
                        addTable[*,*]='_'
                        addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                        addTable[1,*]=TRANSPOSE(['SNI max','SNI L1','SNI L2','SNI S1','SNI S2','SNI S3','SNI S4','SNI S5','SNI S6'])
                        addTable[2,*]=TRANSPOSE(STRING(SNIres.(im).snivalues, FORMAT='(F0.2)'))
                        multiExpTable=[[multiExpTable],[addTable]]
                      ENDIF
                    ENDFOR
                  ENDIF
                END
                3: BEGIN
                  getAcqNM
                  If results(test-1) EQ 1 THEN BEGIN
                    addTable=STRARR(3,addrows)
                    FOR im=0, nImg-1 DO BEGIN
                      IF markedMulti(tt,im) THEN BEGIN
                        addTable=STRARR(3,2)
                        addTable[*,*]='_'
                        addTable[0,0]=STRING(im+1, FORMAT='(i0)')
                        addTable[1,*]=TRANSPOSE(['Total_Count','Frame_Duration_(ms)'])
                        addTable[2,*]=TRANSPOSE(STRING(acqRes[*,im], FORMAT='(I0)'))
                        multiExpTable=[[multiExpTable],[addTable]]
                      ENDIF
                    ENDFOR
                  ENDIF
                END
                ELSE:
              ENDCASE
            END
            ;***************SPECT***************
            3:

            ;***************PET **************
            4:

            ELSE:
          ENDCASE


        ENDIF; addrows 0
      ENDFOR

      updateTable
      updatePlot,1,1,0
      WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
    ENDIF
  ENDIF ELSE sv=DIALOG_MESSAGE('QuickTest not available for this mode yet (numbered tests only).', DIALOG_PARENT=evTop)
  
end