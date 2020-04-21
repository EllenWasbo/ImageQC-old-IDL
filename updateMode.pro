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

pro updateMode

  COMPILE_OPT hidden
  COMMON VARI

  selTab=WIDGET_INFO(wtabModes, /TAB_CURRENT)

  CASE selTab OF
    0: selTest=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
    1: selTest=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
    2: selTest=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
    3: selTest=WIDGET_INFO(wtabAnalysisSPECT, /TAB_CURRENT)
    4: selTest=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
    5: selTest=WIDGET_INFO(wtabAnalysisMR, /TAB_CURRENT)
  ENDCASE
  analyseStrings=analyseStringsAll.(selTab)
  analyse=analyseStrings(selTest)

  RESTORE, thisPath+'data\config.dat'; getting the quickTemp-structure
  IF N_ELEMENTS(quickTemp) NE 0 THEN BEGIN
    IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
      IF SIZE(quickTemp.(modality), /TNAME) EQ 'STRUCT' THEN fillQuickTempList, quickTemp.(modality) ELSE fillQuickTempList, -1
    ENDIF ELSE fillQuickTempList, -1
  ENDIF ELSE fillQuickTempList, -1

end