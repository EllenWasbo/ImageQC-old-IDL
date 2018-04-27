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
    0: BEGIN
      selTest=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
      analyse=analyseStringsCT(selTest)
    END
    1:BEGIN
      selTest=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
      analyse=analyseStringsXray(selTest)
    END
    2:BEGIN
      selTest=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
      analyse=analyseStringsNM(selTest)
    END
    3:BEGIN
      selTest=WIDGET_INFO(wtabAnalysisSPECT, /TAB_CURRENT)
      analyse=analyseStringsSPECT(selTest)
    END
    4:BEGIN
      selTest=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
      analyse=analyseStringsPET(selTest)
    END
  ENDCASE

end