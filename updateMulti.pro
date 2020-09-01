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

;update and check if possible to use MultiMark option
pro updateMulti, AUTOACTIVE=autoactive, NIMGTEMP=nImgTemp
  COMPILE_OPT hidden
  COMMON VARI

  IF N_ELEMENTS(nImgTemp) EQ 0 THEN nImgTemp=0
  IF N_ELEMENTS(autoactive) EQ 0 THEN autoactive=0

  setVal=WIDGET_INFO(btnUseMulti, /BUTTON_SET)
  IF setVal EQ 1 THEN oldsetVal=0 ELSE oldsetVal=1
  proceed=1
  IF TOTAL(results) GT 0 THEN BEGIN
    sv=DIALOG_MESSAGE('Continue and clear results?', /QUESTION, DIALOG_PARENT=evTop)
    IF sv EQ 'No' THEN BEGIN
      proceed=0
      WIDGET_CONTROL, btnUseMulti, SET_BUTTON=oldsetVal
    ENDIF
  ENDIF

  IF proceed THEN BEGIN
    clearRes
    IF setVal THEN BEGIN;multi turned on

      tags=TAG_NAMES(structImgs)
      nImg=N_TAGS(structImgs)
      IF tags(0) NE 'EMPTY' THEN BEGIN
        findOpt=WHERE(multiOpt.(modality) GT 0, nTestsOpt)
        IF nTestsOpt EQ 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Multi marking not possible (yet) for selected modality (no numbered tests)', DIALOG_PARENT=evTop)
          WIDGET_CONTROL, btnUseMulti, SET_BUTTON=0
        ENDIF ELSE BEGIN
          marked=-1
          WIDGET_CONTROL, listSelMultiTemp, GET_VALUE=multiList
          IF N_ELEMENTS(multiList) GT 0 THEN BEGIN
            sel=WIDGET_INFO(listSelMultiTemp, /DROPLIST_SELECT)
            IF sel EQ -1 THEN sel=0
            RESTORE, thisPath+'data\config.dat'
            nImg=N_TAGS(structImgs)
            testOpt=WHERE(multiOpt.(modality) GT 0, nTests)
            IF sel LE 0 THEN BEGIN
              markedMultiNew=INTARR(nTests, nImg) 
            ENDIF ELSE BEGIN
              markedMultiNew=quickTemp.(modality).(sel-1)
              szMNew=SIZE(markedMultiNew, /DIMENSIONS)
              IF N_ELEMENTS(szMNew) EQ 1 THEN szMNew=[szMNew, 1]
              nImgTemp=szMNew(1)
            ENDELSE
            szMNew=SIZE(markedMultiNew, /DIMENSIONS)
            IF N_ELEMENTS(szMNew) EQ 1 THEN szMNew=[szMNew, 1]
            IF szMNew(0) NE nTests OR szMNew(1) NE nImg THEN BEGIN
              IF autoActive THEN BEGIN
                IF szMNew(1) GT nImg THEN BEGIN
                  sv=DIALOG_MESSAGE('Warning: Template created with '+STRING(szMNew(0), FORMAT='(i0)')+' tests and '+STRING(szMNew(1), FORMAT='(i0)')+' images. Numbers do not match. Continue?', /QUESTION, DIALOG_PARENT=evTop)
                  IF sv EQ 'Yes' THEN autoStopFlag=0 ELSE autoStopFlag=1
                ENDIF    
              ENDIF ELSE BEGIN
                sv=DIALOG_MESSAGE('Warning: Template created with '+STRING(szMNew(0), FORMAT='(i0)')+' tests and '+STRING(szMNew(1), FORMAT='(i0)')+' images. Numbers do not match. Please validate template.', DIALOG_PARENT=evTop)
                autoStopFlag=0
              ENDELSE
              
              markedMultiNew=INTARR(nTests, nImg)

              markedMultiTemp=quickTemp.(modality).(sel-1)
              IF nTests GT szMNew(0) THEN markedMultiTemp=markedMultiTemp[0:szMNew(0)-1,*]
              IF nTests LT szMNew(0) THEN markedMultiTemp=markedMultiTemp[0:nTests-1,*]
              IF nImg GT szMNew(1) THEN markedMultiTemp=markedMultiTemp[*,0:szMNew(1)-1]
              IF nImg LT szMNew(1) THEN markedMultiTemp=markedMultiTemp[*,0:nImg-1]
              szMtemp=SIZE(markedMultiTemp, /DIMENSIONS)
              IF N_ELEMENTs(szMtemp) EQ 1 THEN BEGIN
                markedMultiNew[0:szMtemp(0)-1]=markedMultiTemp
              ENDIF ELSE BEGIN
                markedMultiNew[0:szMtemp(0)-1,0:szMtemp(1)-1]=markedMultiTemp
              ENDELSE

            ENDIF
            markedMulti=markedMultiNew

          ENDIF

          fileList=getListOpenFiles(structImgs,0,marked,markedMulti)
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
          WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
        ENDELSE;multimark possible
      ENDIF; tags empty

    ENDIF ELSE BEGIN;multi turned off
      clearMulti
      fileList=getListOpenFiles(structImgs,0,marked,markedMulti)
      sel=WIDGET_INFO(listFiles, /LIST_SELECT)
      oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
      WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
    ENDELSE
  ENDIF

end