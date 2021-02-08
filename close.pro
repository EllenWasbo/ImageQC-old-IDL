;ImageQC - quality control of medical images
;Copyright (C) 2019 Ellen Wasbo, Stavanger University Hospital, Norway
;ellen.wasbo@sus.no
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
pro closeImgs, imgIds
  COMPILE_OPT hidden
  COMMON VARI
  
  sel=imgIds

  IF sel(0) NE -1 THEN BEGIN
    nImg=N_TAGS(structImgs)
    selArr=INTARR(nImg) & selArr(sel)=1
    remain=WHERE(selArr EQ 0);ids to keep (oposite of selArr)
    IF marked(0) NE -1 THEN BEGIN; remove selected from marked
      markedArr=INTARR(nImg) & markedArr(marked)=1
      IF remain(0) NE -1 THEN markedArr=markedArr(remain)
      marked=WHERE(markedArr EQ 1)
    ENDIF
    IF markedMulti(0) NE -1 THEN BEGIN
      IF remain(0) NE -1 THEN BEGIN
        szMM=SIZE(markedMulti, /DIMENSIONS)
        markedMultiNew=INTARR(szMM(0),N_ELEMENTS(remain))
        FOR p=0, szMM(0)-1 DO BEGIN
          markedArr=markedMulti[p,*]
          markedMultiNew[p,*]=markedArr(remain)
        ENDFOR
        markedMulti=markedMultiNew
      ENDIF
    ENDIF

    structImgs=removeIDstructstruct(structImgs, sel)

    stillEmpty=WHERE(TAG_NAMES(structImgs) EQ 'EMPTY')
    IF stillEmpty(0) EQ -1 THEN BEGIN
      fileList=getListOpenFiles(structImgs,0,marked, markedMulti)

      WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=0, SET_LIST_TOP=0
      WIDGET_CONTROL, listFiles, SCR_YSIZE=170

      IF markedMulti(0) NE -1 THEN clearRes ELSE BEGIN; too complicated to keep results if markedMulti
        analyseStrings=analyseStringsAll.(modality)

        FOR i=0, N_ELEMENTS(results)-1 DO BEGIN
          IF results(i) THEN BEGIN
            CASE analyseStrings(i) OF
              'DIM': dimRes=dimRes[*,remain]
              'STP': clearRes, 'STP'
              'HOMOG': homogRes=homogRes[*,remain]
              'NOISE': clearRes, 'NOISE'; because avg dependent on rest - recalculation needed
              'EXP': expRes=expRes[*,remain]
              'DCM': expRes=expRes[*,remain]
              'MTF': MTFres=removeIDstructstruct(MTFres,sel)
              'NPS': NPSres=removeIDstructstruct(NPSres,sel)
              'VARI': varImgRes=removeIDstructstruct(varImgRes,sel)
              'CTLIN': CTlinres=CTlinres[*,remain]
              'HUWATER': HUwaterRes=HUwaterRes[*,remain]
              'ROI': ROIres=ROIres[*,remain]
              'SLICETHICK': BEGIN
                sliceThickRes=removeIDstructstruct(sliceThickRes,sel)
                sliceThickResTab=sliceThickResTab[*,remain]
              END
              'FWHM': fwhmRes=fwhmRes[*,remain]
              'ENERGYSPEC':
              'SCANSPEED':
              'CONTRAST': contrastRes=contrastRes[*,remain]
              'RADIAL': clearRes, 'RADIAL';less could be done, but - no time right no - fix later to keep remaining results
              'UNIF': clearRes, 'UNIF';less could be done, but - no time right no - fix later to keep remaining results
              'SNI': SNIres=removeIDstructstruct(SNIres,sel)
              'BAR': barRes=barRes[*,remain]
              'CROSSCALIB': clearRes, 'CROSSCALIB'
              'RC': clearRes, 'RC';probably less could be done, but - just in case some trouble
              'POS': clearRes, 'POS'
            ENDCASE
          ENDIF
        ENDFOR
      ENDELSE

      redrawImg,0,1
      updateInfo
      WIDGET_CONTROL, lblLoadedN, SET_VALUE=STRING(n_elements(fileList), FORMAT='(i0)')+' )'
    ENDIF ELSE clearAll

  ENDIF

end