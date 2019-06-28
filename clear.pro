;ImageQC - quality control of medical images
;Copyright (C) 2017 Ellen Wasbo, Stavanger University Hospital, Norway
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
  COMPILE_OPT hidden
  COMMON VARI
  WIDGET_CONTROL, drawLarge, SENSITIVE=0
  structImgs=CREATE_STRUCT('empty',0)
  activeImg=0
  WIDGET_CONTROL, drawLarge, GET_VALUE = iDrawLarge
  iDrawLarge.erase

  ;clear list
  WIDGET_CONTROL, listFiles, YSIZE=1, SCR_YSIZE=170, SET_VALUE='';, SET_LIST_SELECT=-1; empty none selected
  WIDGET_CONTROL, lblLoadedN, SET_VALUE='0 )'
  marked=-1
  markedMulti=-1

  ;clear info
  WIDGET_CONTROL, txtActive1, SET_VALUE=''
  WIDGET_CONTROL, txtActive2, SET_VALUE=''
  WIDGET_CONTROL, lblDir, SET_VALUE=''

  clearRes
  clearMulti
end

pro clearRes, analyseStr
  COMPILE_OPT hidden
  COMMON VARI
  IF TOTAL(results) GT 0 THEN BEGIN
    WIDGET_CONTROL, resTab, XSIZE=4, YSIZE=5, COLUMN_WIDTHS=[100,100,100,100], COLUMN_LABELS=['1','2','3','4'], SET_VALUE=STRARR(4,5), SET_TABLE_VIEW=[0,0]
  
    curTab=WIDGET_INFO(wtabModes, /TAB_CURRENT)
    analyseStrings=analyseStringsAll.(curTab)
  
    IF N_ELEMENTS(analyseStr) EQ 0 THEN BEGIN
      dimRes=!Null
      stpRes=!Null
      homogRes=!Null
      noiseRes=!Null
      expRes=!Null
      MTFres=!Null
      NPSres=!Null
      ;ROIres=!Null
      CTlinRes=!Null
      HUwaterRes=!Null
      sliceThickRes=!Null
      sliceThickResTab=!Null
      fwhmRes=!Null
      unifRes=!Null
      SNIres=!Null
      barRes=!Null
      energyRes=!Null
      crossRes=!Null
      rcRes=!Null
      radialRes=!Null
      
      results=results*0
      currentHeaderAlt=currentHeaderAlt*0
    ENDIF ELSE BEGIN
      CASE analyseStr OF
        'DIM': dimRes=!Null
        'STP': stpRes=!Null
        'HOMOG': homogRes=!Null
        'NOISE': noiseRes=!Null
        'EXP': expRes=!Null
        'MTF': MTFres=!Null
        'NPS': NPSres=!Null
        ;'ROI': ROIres=!Null
        'CTLIN':CTlinres=!Null
        'HUWATER': HUwaterRes=!Null
        'SLICETHICK': BEGIN
          sliceThickRes=!Null
          sliceThickResTab=!Null
        END
        'FWHM': fwhmRes=!Null
        'UNIF': unifRes=!Null
        'SNI': SNIres=!Null
        'BAR': barRes=!Null
        'ENERGYSPEC': energyRes=!Null
        'SCANSPEED':
        'CONTRAST': contrastRes=!Null
        'CROSSCALIB': crossRes=!Null
        'RC': rcRes=!Null
        'RADIAL': radialRes=!Null
        ELSE:
      ENDCASE
  
      resNo=WHERE(analyseStrings EQ analyse)
      IF resNo(0) NE -1 THEN BEGIN
        results(resNo)=0
        currentHeaderAlt(resNo)=0
      ENDIF
    ENDELSE
  
    WIDGET_CONTROL, resTab, TABLE_XSIZE=4, TABLE_YSIZE=2, COLUMN_LABELS=['0','1','2','3'], COLUMN_WIDTHS=[100,100,100,100], SET_VALUE=STRARR(4,5), SET_TABLE_SELECT=[-1,-1,-1,-1], SET_TABLE_VIEW=[0,0]
    
    WIDGET_CONTROL, drawPlot, GET_VALUE = iDrawPlot
    iDrawPlot.SELECT & iDrawPlot.erase
    
    WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
    WSET, iDrawImageRes
    TVSCL, INTARR(550,550)
  ENDIF
end

pro clearMulti
  COMPILE_OPT hidden
  COMMON VARI
  markedMulti=-1
  marked=-1
  WIDGET_CONTROL, btnUseMulti, SET_BUTTON=0
  WIDGET_CONTROL, listSelMultiTemp, SET_DROPLIST_SELECT=0
  multiExpTable=-1
end
