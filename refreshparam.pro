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

;fill quickTemp list
;qt=quickTemp structure
;SELECT_NAME = string of the template
pro fillQuickTempList, qT, SELECT_NAME=select_name
  COMPILE_OPT hidden
  COMMON VARI
  IF N_ELEMENTS(qT) NE 0 THEN BEGIN
    typ=SIZE(qT, /TNAME)
    IF typ EQ 'STRUCT' THEN BEGIN
      tempNames=TAG_NAMES(qT)
      selno=0
      IF N_ELEMENTS(select_name) NE 0 THEN BEGIN
        tagNo=WHERE(STRUPCASE(tempNames) EQ STRUPCASE(select_name))
        IF tagNo(0) NE -1 THEN selno=tagNo
        WIDGET_CONTROL, listSelMultiTemp, SET_DROPLIST_SELECT=selno+1
      ENDIF ELSE BEGIN;fill new
        WIDGET_CONTROL, listSelMultiTemp, SET_VALUE=['',tempNames]
        clearMulti
        marked=-1
        WIDGET_CONTROL, btnUseMulti, SET_BUTTON=0
      ENDELSE
    ENDIF

    tags=TAG_NAMES(structImgs)
    IF tags(0) NE 'EMPTY' THEN BEGIN
      fileList=getListOpenFiles(structImgs,0,marked,markedMulti)
      sel=WIDGET_INFO(listFiles, /LIST_SELECT)
      oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
      WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
    ENDIF
  ENDIF
  
end

pro setQuickTempName,  SELECT_NAME=select_name
  COMPILE_OPT hidden
  COMMON VARI
end

;if paramSetName NE '' Then update also shown name
pro refreshParam, paramSet, paramSetName
  COMPILE_OPT hidden
  COMMON VARI

  clearRes

  IF paramSetName NE '' THEN WIDGET_CONTROL, lblSettings, SET_VALUE=paramSetName

  defPath=paramSet.defPath
  decimMark=paramSet.deciMark
  CASE deciMark OF
    '.':  WIDGET_CONTROL, listDeciMark, SET_DROPLIST_SELECT=0
    ',':  WIDGET_CONTROL, listDeciMark, SET_DROPLIST_SELECT=1
    ELSE:
  ENDCASE
  
  copyHeader=paramSet.copyHeader
  WIDGET_CONTROL, btnCopyHeader, SET_BUTTON=paramSet.COPYHEADER
  transposeTable=paramSet.transposeTable
  WIDGET_CONTROL, btnTranspose, SET_BUTTON=paramSet.TRANSPOSETABLE
  
  WIDGET_CONTROL, btnAppend, SET_BUTTON=paramSet.APPEND
  ;offxy both CT and Xray
  strOff=STRING(paramSet.OFFXY(0), FORMAT='(i0)')+','+STRING(paramSet.OFFXY(1), FORMAT='(i0)')
  WIDGET_CONTROL, lblDeltaO, SET_VALUE=strOff
  WIDGET_CONTROL, lblDeltaOX, SET_VALUE=strOff
  offxy=paramSet.offxy
  ;CT tests
  WIDGET_CONTROL, cw_typeMTF, SET_VALUE=paramSet.MTFTYPE
  WIDGET_CONTROL, cw_plotMTF, SET_VALUE=paramSet.PLOTMTF
  WIDGET_CONTROL, txtMTFroiSz, SET_VALUE=STRING(paramSet.MTFROISZ, FORMAT='(f0.1)')
  WIDGET_CONTROL, btnCutLSF, SET_BUTTON=paramSet.CUTLSF
  WIDGET_CONTROL, txtCutLSFW, SET_VALUE=STRING(paramSet.CUTLSF1, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtCutLSFW2, SET_VALUE=STRING(paramSet.CUTLSF2, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtLinROIrad, SET_VALUE=STRING(paramSet.LINROIRAD, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtLinROIradS, SET_VALUE=STRING(paramSet.LINROIRADS, FORMAT='(f0.1)')
  ysz=N_ELEMENTS(paramSet.LINTAB.Materials)
  fillLin=STRARR(4,ysz)
  fillLin[0,*]=TRANSPOSE(paramSet.LINTAB.Materials)
  fillLin[1,*]=STRING(TRANSPOSE(paramSet.LINTAB.posX), FORMAT='(f0.1)')
  fillLin[2,*]=STRING(TRANSPOSE(paramSet.LINTAB.posY), FORMAT='(f0.1)')
  fillLin[3,*]=STRING(TRANSPOSE(paramSet.LINTAB.RelMassD), FORMAT='(f0.3)')
  WIDGET_CONTROL, tblLin, TABLE_YSIZE=ysz, SET_VALUE=fillLin, SET_TABLE_SELECT=[-1,-1,-1,-1], SET_TABLE_VIEW=[0,0]
  WIDGET_CONTROL, txtRampDist, SET_VALUE=STRING(paramSet.RAMPDIST, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtRampLen, SET_VALUE=STRING(paramSet.RAMPLEN, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtRampBackG, SET_VALUE=STRING(paramSet.RAMPBACKG, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtRampSearch, SET_VALUE=STRING(paramSet.RAMPSEARCH, FORMAT='(i0)')
  WIDGET_CONTROL, txtRampAverage, SET_VALUE=STRING(paramSet.RAMPAVG, FORMAT='(i0)')
  WIDGET_CONTROL, cw_ramptype, SET_VALUE=paramSet.RAMPTYPE
  WIDGET_CONTROL, cw_rampDens, SET_VALUE=paramSet.RAMPDENS
  WIDGET_CONTROL, txtHomogROIsz, SET_VALUE=STRING(paramSet.HOMOGROISZ, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtHomogROIdist, SET_VALUE=STRING(paramSet.HOMOGROIDIST, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtNoiseROIsz, SET_VALUE=STRING(paramSet.NOISEROISZ, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtNPSroiSz, SET_VALUE=STRING(paramSet.NPSROISZ, FORMAT='(i0)')
  WIDGET_CONTROL, txtNPSroiDist, SET_VALUE=STRING(paramSet.NPSROIDIST, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtNPSsubNN, SET_VALUE=STRING(paramSet.NPSSUBNN, FORMAT='(i0)')
  ;Xray tests
  WIDGET_CONTROL, txtStpROIsz, SET_VALUE=STRING(paramSet.STPROISZ, FORMAT='(f0.1)')
  WIDGET_CONTROL, cw_formLSFX, SET_VALUE=paramSet.MTFtypeX
  WIDGET_CONTROL, cw_plotMTFX, SET_VALUE=paramSet.plotMTFX
  WIDGET_CONTROL, txtCutLSFWX, SET_VALUE=STRING(paramSet.cutLSFX1, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtMTFroiSzX, SET_VALUE=STRING(paramSet.MTFroiSzX(0), FORMAT='(f0.1)')
  WIDGET_CONTROL, txtMTFroiSzY, SET_VALUE=STRING(paramSet.MTFroiSzX(1), FORMAT='(f0.1)')
  WIDGET_CONTROL, btnCutLSFX, SET_BUTTON=paramSet.CUTLSFX
  WIDGET_CONTROL, txtHomogROIszX, SET_VALUE=STRING(paramSet.HOMOGROISZX, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtNPSroiSzX, SET_VALUE=STRING(paramSet.NPSROISZX, FORMAT='(i0)')
  WIDGET_CONTROL, txtNPSsubSzX, SET_VALUE=STRING(paramSet.NPSSUBSZX, FORMAT='(i0)')
  WIDGET_CONTROL, btnNPSavg, SET_BUTTON=paramSet.NPSAVG
  nn=((2*LONG(paramSet.NPSsubSzX)-1)*LONG(paramSet.NPSroiSzX))^2
  WIDGET_CONTROL, lblNPStotPixX, SET_VALUE=STRING(nn, FORMAT='(i0)')
  ;NM tests
  WIDGET_CONTROL, cw_typeMTFNM, SET_VALUE=paramSet.MTFtypeNM
  WIDGET_CONTROL, cw_plotMTFNM, SET_VALUE=paramSet.plotMTFNM
  WIDGET_CONTROL, txtMTFroiSzXNM, SET_VALUE=STRING(paramSet.MTFroiSzNM(0), FORMAT='(f0.1)')
  WIDGET_CONTROL, txtMTFroiSzYNM, SET_VALUE=STRING(paramSet.MTFroiSzNM(1), FORMAT='(f0.1)')
  WIDGET_CONTROL, txtNAvgSpeedNM, SET_VALUE=STRING(paramSet.scanSpeedAvg, FORMAT='(i0)')
  WIDGET_CONTROL, txtSpeedROIheight, SET_VALUE=STRING(paramSet.scanSpeedHeight, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtScanSpeedMedian, SET_VALUE=STRING(paramSet.scanSpeedFiltW, FORMAT='(i0)')
  ;SPECT tests
  WIDGET_CONTROL, cw_typeMTFSPECT, SET_VALUE=paramSet.MTFtypeSPECT
  WIDGET_CONTROL, cw_plotMTFSPECT, SET_VALUE=paramSet.plotMTFSPECT
  WIDGET_CONTROL, txtMTFroiSzSPECT, SET_VALUE=STRING(paramSet.MTFroiSzSPECT, FORMAT='(f0.1)')
  WIDGET_CONTROL, MTF3dSPECT, SET_BUTTON=paramSet.MTF3dSPECT
  WIDGET_CONTROL, txtConR1SPECT, SET_VALUE=STRING(paramSet.contrastRad1, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtConR2SPECT, SET_VALUE=STRING(paramSet.contrastRad2, FORMAT='(f0.1)')
  ;PET tests
  WIDGET_CONTROL, txtCrossROIsz, SET_VALUE=STRING(paramSet.crossROIsz, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtCrossVol, SET_VALUE=STRING(paramSet.crossVol, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtHomogROIszPET, SET_VALUE=STRING(paramSet.HomogROIszPET, FORMAT='(f0.1)')
  WIDGET_CONTROL, txtHomogROIdistPET, SET_VALUE=STRING(paramSet.HomogROIdistPET, FORMAT='(f0.1)')

end
