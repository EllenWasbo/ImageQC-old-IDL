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
pro clearAll

  COMMON VARI
  WIDGET_CONTROL, drawLarge, SENSITIVE=0
  structImgs=CREATE_STRUCT('empty',0)

  ;clear img
  activeImg=INTARR(512,512)
  activeImg=0

  ;clear list
  WIDGET_CONTROL, listFiles, YSIZE=1, SCR_YSIZE=170, SET_VALUE='';, SET_LIST_SELECT=-1; empty none selected
  marked=-1

  ;clear info
  WIDGET_CONTROL, txtActive1, SET_VALUE=''
  WIDGET_CONTROL, txtActive2, SET_VALUE=''

  redrawImg,0,1
  clearRes

end

pro clearRes, analyseStr

  COMMON VARI
  WIDGET_CONTROL, resTab, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], COLUMN_LABELS=['1','2','3','4'], SET_VALUE=STRARR(4,5), SET_TABLE_VIEW=[0,0]

    analyse='NONE'
  IF N_ELEMENTS(analyseStr) EQ 0 THEN BEGIN
    dimRes=!Null
    stpRes=!Null
    homogRes=!Null
    noiseRes=!Null
    MTFres=!Null
    NPSres=!Null
    ROIres=!Null
    CTlinRes=!Null
    CTlinROIs=0 & CTlinROIpos=0
    sliceThickRes=!Null
    sliceThickResTab=!Null
    fwhmRes=!Null
    energyRes=!Null
    crossRes=!Null

    results=results*0
  ENDIF ELSE BEGIN
    CASE analyseStr OF
      'DIM': dimRes=!Null
      'STP': stpRes=!Null
      'HOMOG': homogRes=!Null
      'NOISE': noiseRes=!Null
      'MTF': MTFres=!Null
      'NPS': NPSres=!Null
      'ROI': ROIres=!Null
      'CTLIN': BEGIN
        CTlinres=!Null
        CTlinROIs=0 & CTlinROIpos=0
        END
      'SLICETHICK': BEGIN
        sliceThickRes=!Null
        sliceThickResTab=!Null
      END
      'FWHM': fwhmRes=!Null
      'ENERGYSPEC': energyRes=!Null
      'SCANSPEED':
      'CONTRAST': contrastRes=!Null
      'CROSSCALIB': crossRes=!Null
      ELSE:
    ENDCASE
    
    curTab=WIDGET_INFO(wtabModes, /TAB_CURRENT)
    CASE curTab OF
      0: analyseStrings=analyseStringsCT
      1: analyseStrings=analyseStringsXray
      2: analyseStrings=analyseStringsNM
      3: analyseStrings=analyseStringsPET
    ENDCASE

    resNo=WHERE(analyseStrings EQ analyseStr)-1
    IF resNo(0) NE -1 THEN results(resNo)=0
  ENDELSE

  redrawImg,0,0
  updateTable
  updatePlot, 0,0,0

end