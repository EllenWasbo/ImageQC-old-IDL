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

;update header-info and full path display
pro updateInfo
  COMPILE_OPT hidden
  COMMON VARI
  
  
  sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
  IF sel NE -1 THEN BEGIN
    tempStruct=structImgs.(sel);IF nFrames EQ 0 THEN tempStruct=structImgs.(sel) ELSE tempStruct=structImgs.(0)
    tempImg=activeImg
    imSz=SIZE(tempImg,/DIMENSIONS)
    tab=STRING(9B)

    CASE modality OF
      0: BEGIN
        infoString1=$
          ['Date' +(tempStruct.acqDate NE '' ? ('OfAcq:'+tab+ tempStruct.acqDate) : ('OfImage:'+tab+ tempStruct.imgDate)), $
          'Institution:'+tab+ tempStruct.Institution, $
          'ModelName:'+tab+ tempStruct.ModelName, $
          'PatientName:'+tab+ tempStruct.PatientName, $
          'PatientID:'+tab+ tab+tempStruct.PatientID, $
          'SeriesName:'+tab+ tempStruct.seriesName, $
          'ImageType:'+tab+ tempStruct.imageType, $
          'ExposureModType:'+tab+tempStruct.ExModType, $
          'Kernel:'+tab+tempStruct.kernel]

        infoString2=$
          ['SliceThick:'+tab+ (tempStruct.SliceThick NE -1 ? string(tempStruct.SliceThick, format='(f0.2)') : '-'), $
          'Pixelsize:'+tab+tab+ (tempStruct.pix[0] NE -1 ? string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)')  : '-'), $
          'Imagesize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'),$
          'CollWidth:'+tab+(tempStruct.coll[0] NE -1 ? string(tempStruct.coll[0],format='(f0.2)') : '-')+' | '+(tempStruct.coll[1] NE -1 ? string(tempStruct.coll[1],format='(f0.2)') : '-'), $
          'kVp:'+tab+(tempStruct.kVp NE -1 ? STRING(tempStruct.kVp, format='(f0.2)') : '-'), $
          'mAs:'+tab+(tempStruct.mAs NE -1 ? STRING(tempStruct.mAs, format=formatCode(tempStruct.mAs)) : '-'), $
          'ExpTime (ms):'+tab+(tempStruct.ExpTime NE -1 ? STRING(tempStruct.ExpTime, format=formatCode(tempStruct.ExpTime)) : '-'), $
          'Pitch:'+tab+(tempStruct.pitch NE -1 ? STRING(tempStruct.pitch, format=formatCode(tempStruct.pitch)) : '-'), $
          'CTDIvol:'+tab+(tempStruct.CTDIvol(0) NE -1 ? (N_ELEMENTS(tempStruct.CTDIvol) EQ 1 ? STRING(tempStruct.CTDIvol, format=formatCode(tempStruct.CTDIvol)): 'err') : '-')]
      END
      1: BEGIN
        infoString1=$
          ['Date' +(tempStruct.acqDate NE '' ? ('/TimeOfAcq:'+tab+ tempStruct.acqDate + ' | '+tempStruct.acqTime) : ('OfImage:'+tab+ tempStruct.imgDate)), $
          'Institution:'+tab+ tempStruct.Institution, $
          'ModelName:'+tab+ tempStruct.ModelName, $
          'Detector ID:'+tab+tempStruct.DetectorID, $
          'PatientName:'+tab+ tempStruct.PatientName, $
          'PatientID:'+tab+ tab+tempStruct.PatientID, $
          'Modality:'+tab+tab+tempStruct.modality, $
          'Presentation type:'+tab+tempStruct.presType,$
          'SeriesNmb / ImageNmb:'+tab+ string(tempStruct.seriesNmb, format='(i0)')+' / '+STRING(tempStruct.imgNo, format='(i0)'), $
          'Protocol name:'+tab+ tempStruct.protocolName]

        infoString2=$
          ['Pixelsize:'+tab+ tab+string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)'),$
          'Imagesize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'),$
          'kVp:'+tab+tab+(tempStruct.kVp NE -1 ? STRING(tempStruct.kVp, format='(f0.2)') : '-'), $
          'mA / mAs:'+tab+tab+(tempStruct.mA NE -1 ? STRING(tempStruct.mA, format=formatCode(tempStruct.mA)) : '-') + $
          ' / '+(tempStruct.mAs NE -1 ? STRING(tempStruct.mAs, format=formatCode(tempStruct.mAs)) : '-'), $
          'ExpTime(ms):' + tab + (tempStruct.ExpTime NE -1 ? STRING(tempStruct.ExpTime, format=formatCode(tempStruct.ExpTime)) : '-'), $     
          'EI:'+tab+tab+(tempStruct.EI NE -1 ? STRING(tempStruct.EI, format=formatCode(tempStruct.EI)) : '-'), $
          'DAP (dGycm2):'+tab+(tempStruct.DAP NE -1 ? STRING(tempStruct.DAP, format=formatCode(tempStruct.DAP)) : '-'), $
          'ExposureModType:'+tab+tempStruct.ExModType,$
          'Filter:'+tab+tab+tempStruct.filterAddOn]
      END
      2:BEGIN
        infoString1=$
          ['Date' +(tempStruct.acqDate NE '' ? ('OfAcq:'+tab+ tempStruct.acqDate) : ('OfImage:'+tab+ tempStruct.imgDate)), $
          'Institution:'+tab+ tempStruct.Institution, $
          'StationName:'+tab+ tempStruct.StationName, $
          'PatientName:'+tab+ tempStruct.PatientName, $
          'PatientID:'+tab+ tab+tempStruct.PatientID, $
          'Modality:'+tab+tab+tempStruct.modality, $
          'ImageType:'+tab+ tempStruct.imageType, $
          'Acquisition time:'+tab+ tempStruct.acqTime, $
          'SeriesName:'+tab+ tempStruct.seriesName, $
          'Study Description:'+tab+ tempStruct.studyDescr]

        infoString2=$
          ['Pixelsize:'+tab+tab+ string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)'), $
          'Imagesize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'), $
          'Collimator:'+tab+tempStruct.collType,$
          'Termination condition:'+tab+tempStruct.acqTerminationCond,$
          'Frame duration:'+tab+string(tempStruct.acqFrameDuration, format='(i0)'),$
          'Radius det 1:'+tab+(tempStruct.radius1(0) NE -1 ? (N_ELEMENTS(tempStruct.radius1) GT 1 ? '['+string(min(tempStruct.radius1), format='(f0.1)')+'.. '+string(max(tempStruct.radius1), format='(f0.1)') + ']' : string(tempStruct.radius1(0), format='(f0.1)')) : '-'), $
          'Radius det 2:'+tab+(tempStruct.radius2(0) NE -1 ? (N_ELEMENTS(tempStruct.radius2) GT 1 ? '['+string(min(tempStruct.radius2), format='(f0.1)')+'.. '+string(max(tempStruct.radius2), format='(f0.1)') + ']' : string(tempStruct.radius2(0), format='(f0.1)')) : '-'), $
          'Energy Window:'+tab+tempStruct.EWindowName,$
          'Zoom factor:'+tab+tempStruct.zoomFactor]
      END
      3:BEGIN
        infoString1=$
          ['Date' +(tempStruct.acqDate NE '' ? ('OfAcq:'+tab+ tempStruct.acqDate) : ('OfImage:'+tab+ tempStruct.imgDate)), $
          'Institution:'+tab+ tempStruct.Institution, $
          'StationName:'+tab+ tempStruct.StationName, $
          'PatientName:'+tab+ tempStruct.PatientName, $
          'PatientID:'+tab+ tab+tempStruct.PatientID, $
          'Modality:'+tab+tab+tempStruct.modality, $
          'ImageType:'+tab+ tempStruct.imageType, $
          'Acquisition time:'+tab+ tempStruct.acqTime, $
          'SeriesName:'+tab+ tempStruct.seriesName, $
          'Study Description:'+tab+ tempStruct.studyDescr]

        infoString2=$
          ['Pixelsize:'+tab+tab+ string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)'), $
          'Imagesize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'), $
          'SliceThick:'+tab+ (tempStruct.SliceThick NE -1 ? string(tempStruct.SliceThick, format='(f0.2)') : '-'), $
          'Collimator:'+tab+tempStruct.collType,$
          'Radius det 1:'+tab+(tempStruct.radius1(0) NE -1 ? (N_ELEMENTS(tempStruct.radius1) GT 1 ? '['+string(min(tempStruct.radius1), format='(f0.1)')+'.. '+string(max(tempStruct.radius1), format='(f0.1)') + ']' : string(tempStruct.radius1(0), format='(f0.1)')) : '-'), $
          'Radius det 2:'+tab+(tempStruct.radius2(0) NE -1 ? (N_ELEMENTS(tempStruct.radius2) GT 1 ? '['+string(min(tempStruct.radius2), format='(f0.1)')+'.. '+string(max(tempStruct.radius2), format='(f0.1)') + ']' : string(tempStruct.radius2(0), format='(f0.1)')) : '-'), $
          'Energy Window:'+tab+tempStruct.EWindowName,$
          'Zoom factor:'+tab+tempStruct.zoomFactor, $
          'Recon:'+tab+tempStruct.kernel]
      END
      4: BEGIN
        infoString1=$
          ['Date' +(tempStruct.acqDate NE '' ? ('OfAcq:'+tab+ tempStruct.acqDate) : ('OfImage:'+tab+ tempStruct.imgDate)), $
          'Acquisition time:'+tab+ tempStruct.acqTime, $
          'ModelName:'+tab+ tempStruct.ModelName, $
          'PatientName:'+tab+ tempStruct.PatientName, $
          'PatientID:'+tab+ tab+tempStruct.PatientID, $
          'Modality:'+tab+tab+tempStruct.modality, $
          'SeriesName:'+tab+ tempStruct.seriesName, $
          'PatientWeight:'+tab+tempStruct.patientWeight,$
          'Inj.act.(Bq) /time: '+ tab+tempStruct.admDose +' / '+STRING(tempStruct.admDoseTime, FORMAT='(a06)')]

        infoString2=$
          ['PixelSize:'+tab+tab+ string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)'), $
          'ImageSize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'), $
          'SliceThick:'+tab+ (tempStruct.SliceThick NE -1 ? string(tempStruct.SliceThick, format='(f0.2)') : '-'), $
          'Radiopharmaca: '+tab+tempStruct.radiopharmaca, $
          'Units:'+tab+tab+tempStruct.units, $
          'Att corr: '+tab+tempStruct.attCorrMethod,$
          'Recon: '+tab+tempStruct.reconMethod+'/'+tempStruct.kernel,$
          'Scatter corr: '+tab+tempStruct.scaCorrMethod,$
          'Scatter fraction: '+tab+tempStruct.scatterFrac]
      END
      ELSE:
    ENDCASE

    WIDGET_CONTROL, txtActive1, SET_VALUE=infoString1
    WIDGET_CONTROL, txtActive2, SET_VALUE=infoString2
    WIDGET_CONTROL, lblDir, SET_VALUE=tempStruct.filename

    IF TOTAL(results) GT 0 THEN BEGIN
      updateTable
      IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 1 THEN updatePlot, 0,0,0
      IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 2 THEN updateImageRes
      IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 3 THEN updateTableSup
      
    ENDIF
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, txtActive1, SET_VALUE=''
    WIDGET_CONTROL, txtActive2, SET_VALUE=''
    WIDGET_CONTROL, lblDir, SET_VALUE=''
  ENDELSE

end