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

;update Plot and image Results (fx 2d NPS)
pro updatePlot, setRangeMinMaxX, setRangeMinMaxY, copyCurve

  COMMON VARI

  WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
  WSET, iDrawImageRes
  TVSCL, INTARR(550,550)

  WIDGET_CONTROL, drawPlot, GET_VALUE = iDrawPlot
  WSET, idrawPlot
  IF analyse NE 'NONE' THEN BEGIN

    valuesPlot=CREATE_STRUCT('empty',0); structure of plot values to be copied to clipboard (copyCurve = 1) or sent to iPlot (copyCurve =2), x/y pairs of vectors, tagname 'COPYXXX...' means that the vector (x values) is not included when copyCurve=1
    statTxt='';text to show in plot statistics (left of plot window)
    WIDGET_CONTROL, statPlot, SET_VALUE=statTxt

    rowNo=-1
    tags=TAG_NAMES(structImgs)
    IF tags(0) NE 'EMPTY' THEN BEGIN
      sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
      IF nFrames EQ 0 THEN pix=structImgs.(sel).pix(0) ELSE pix=structImgs.(0).pix(0)
      IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
      markedArr=INTARR(nImg)
      IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
      markedTemp=WHERE(markedArr EQ 1)
      rowNo=WHERE(markedTemp EQ sel)
      rowNo=rowNo(0)
    ENDIF

    IF N_ELEMENTS(pix) GT 0 THEN nyqfr=1/(2.*pix); Nyquist frequency in mm-1

    IF rowNo(0) NE -1 OR analyse EQ 'ENERGYSPEC' THEN BEGIN

      WIDGET_CONTROL, txtMinRangeX, GET_VALUE=rangeX1
      WIDGET_CONTROL, txtMaxRangeX, GET_VALUE=rangeX2
      WIDGET_CONTROL, txtMinRangeY, GET_VALUE=rangeY1
      WIDGET_CONTROL, txtMaxRangeY, GET_VALUE=rangeY2
      rangeX=FLOAT([rangeX1(0),rangeX2(0)])
      rangeY=FLOAT([rangeY1(0),rangeY2(0)])

      curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
      CASE curMode OF

        ;********************************CT*************************************************
        0: BEGIN
          curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyseStringsCT(curTab+1) OF

              'HOMOG':BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                resArr=FLOAT(resArr[0:4,*])
                colNo=5
                zPosMarked=getZposMarked(structImgs, markedTemp)

                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[min(resArr),max(resArr)]

                valCenter=transpose(resArr[0,*])
                val12=transpose(resArr[1,*])
                val15=transpose(resArr[2,*])
                val18=transpose(resArr[3,*])
                val21=transpose(resArr[4,*])
                valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Center', valCenter, 'COPY0zPos',zPosMarked, 'at12', val12, 'COPY1zPos',zPosMarked, 'at15', val15, 'COPY2zPos',zPosMarked, 'at18', val18, 'COPY3zPos',zPosMarked, 'at21', val21)
                PLOT, zPosMarked, valCenter, XTITLE='zPos (mm)', YTITLE='Mean pixel value in ROI' , TITLE='Mean in ROI for all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, COLOR=255
                LOADCT, 12, /SILENT
                OPLOT, zPosMarked, val12, COLOR=200;red
                OPLOT, zPosMarked, val15, COLOR=100;blue
                OPLOT, zPosMarked, val18, COLOR=40;green
                OPLOT, zPosMarked, val21, COLOR=90;cyan
                loadct, 0, /SILENT
                OPLOT, rangeX, [5,5], LINESTYLE=1, COLOR=255
                OPLOT, rangeX, [-5,-5], LINESTYLE=1, COLOR=255
              END

              'MTF': BEGIN
                resforeach=WHERE(TAG_NAMES(MTFres) EQ 'M0')
                v3d=0
                IF resforeach(0) EQ -1 THEN v3d=1

                WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=ROIsz
                ROIsz=ROUND(ROIsz(0)/pix)
                halfSz=SIZE(activeImg, /DIMENSIONS)/2
                x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
                y1=ROUND(halfSz(1)+dxya(1)-ROIsz(0)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(0))

                WIDGET_CONTROL, cw_plotMTF, GET_VALUE= plotWhich

                CASE plotWhich OF

                  0: BEGIN ;centered xy profile

                    IF v3d THEN BEGIN

                      valuesPlot=CREATE_STRUCT('x',MTFres.cdx,'Xprof',MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0],'y', MTFres.cdy, 'Yprof', transpose(MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0]))

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.cdx),max(MTFres.cdx)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.submatrixAll),max(MTFres.submatrixAll)]

                      szM=size(MTFres.submatrixAll,/DIMENSIONS)
                      plot, MTFres.cdx,MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0],TITLE='Profile x and y centered', linestyle=0, XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                      oplot, MTFres.cdy,MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0], linestyle=1
                      IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                        LOADCT, 13, /SILENT
                        FOR i = 1, szM(2)-1 DO BEGIN
                          oplot, MTFres.cdx, MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], LINESTYLE=0, COLOR=i*40
                          oplot,  MTFres.cdy, MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i], LINESTYLE=1, COLOR=i*40
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.cdx, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y', MTFres.cdy,'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                        ENDFOR
                        LOADCT, 0, /SILENT
                      ENDIF
                    ENDIF ELSE BEGIN
                      valuesPlot=CREATE_STRUCT('x',MTFres.(sel).cdx, 'Xprof', MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0], 'y',MTFres.(sel).cdy, 'Yprof', MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0])

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).cdx),max(MTFres.(sel).cdx)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).submatrixAll),max(MTFres.(sel).submatrixAll)]

                      szM=size(MTFres.(sel).submatrixAll,/DIMENSIONS)
                      xprof=MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0]
                      yprof=MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0]
                      plot, MTFres.(sel).cdx,xprof,TITLE='Profile x and y centered', XRANGE=rangeX, YRANGE=rangeY, linestyle=0, XSTYLE=1, YSTYLE=1
                      oplot, MTFres.(sel).cdy,yprof, linestyle=1
                      ;                      IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                      ;                        LOADCT, 13, /SILENT
                      ;                        FOR i = 1, szM(2)-1 DO BEGIN
                      ;                          oplot, MTFres.(sel).cdx, MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),i], LINESTYLE=0, COLOR=i*40
                      ;                          oplot,  MTFres.(sel).cdy, MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,i], LINESTYLE=1, COLOR=i*40
                      ;                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.(sel).cdx, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.(sel).submatrixAll[*,ROUND(MTFres.centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y', MTFres.(sel).cdy,'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.(sel).submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                      ;                        ENDFOR
                      ;                        LOADCT, 0, /SILENT
                      ;                      ENDIF

                      maxval=max(xprof)
                      minval=min(xprof)
                      resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                      statTxt=['FWHM X: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')]
                      maxval=max(yprof)
                      minval=min(yprof)
                      resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                      statTxt=[[statTxt],['FWHM Y: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')]]
                    ENDELSE
                  END
                  1: BEGIN ;sorted pixelvalues, interpolated and smoothed
                    IF v3d THEN BEGIN
                      valuesPlot=CREATE_STRUCT('distance from center', MTFres.newdists,'Interpolated pixel values',MTFres.pixValsInterp,'COPY000distance from center', MTFres.newdists,'Smoothed pixelvalues',MTFres.pixValsSmooth)

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.distspix0),max(MTFres.distspix0)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.pixValSort),max(MTFres.pixValSort)]

                      plot, MTFres.distspix0, MTFres.pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                      szM=size(MTFres.submatrixAll,/DIMENSIONS)
                      IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                        FOR b=1, szM(2)-1 DO oplot,  MTFres.distspix0, MTFres.pixValSort[*,b], PSYM=3
                      ENDIF
                      LOADCT, 13, /SILENT
                      oplot, MTFres.newdists, MTFres.pixValsInterp, COLOR=255
                      oplot, MTFres.newdists, MTFres.pixValsSmooth, COLOR=150
                      LOADCT, 0, /SILENT

                    ENDIF ELSE BEGIN
                      TVSCL, INTARR(550,550)
                      XYOUTS, -4,.5, 'Sorted pixelvalues not available for 2d bead method.', CHARSIZE=1.2
                    ENDELSE

                  END
                  2: BEGIN; LSF
                    IF v3d EQ 0 THEN BEGIN
                      IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
                        valuesPlot=CREATE_STRUCT('xpos mm',MTFres.(sel).dx, 'LSFx',MTFres.(sel).LSFx)

                        IF setRangeMinMaxX THEN rangeX=[min([MTFres.(sel).dx,MTFres.(sel).dy]),max([MTFres.(sel).dx,MTFres.(sel).dy])]
                        IF setRangeMinMaxY THEN rangeY=[min([MTFres.(sel).LSFx,MTFres.(sel).LSFy]),max([MTFres.(sel).LSFx,MTFres.(sel).LSFy])]
                        tagMTFres=tag_names(MTFres.(sel))
                        IF tagMTFres.hasValue('FITLSFX') THEN BEGIN
                          PLOT, MTFres.(sel).dx,MTFres.(sel).fitLSFx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY001xpos mm',MTFres.(sel).dx, 'Fit to smoothed LSFx', MTFres.(sel).fitLSFx)
                        ENDIF ELSE PLOT, MTFres.(sel).dx,MTFres.(sel).dx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                        LOADCT, 13, /SILENT
                        IF tagMTFres.hasValue('SMLSFX') THEN BEGIN
                          oplot, MTFres.(sel).dx, MTFres.(sel).smLSFx, COLOR=150
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY002xpos mm',MTFres.(sel).dx, 'Smoothed LSFx', MTFres.(sel).smLSFx)
                        ENDIF
                        LOADCT, 0, /SILENT
                        valuesPlot=CREATE_STRUCT(valuesPlot, 'ypos mm', MTFres.(sel).dy, 'LSFy', MTFres.(sel).LSFy)
                        IF tagMTFres.hasValue('FITLSFY') THEN BEGIN
                          OPLOT, MTFres.(sel).dy,MTFres.(sel).fitLSFy,linestyle=1
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY001ypos mm',MTFres.(sel).dy, 'Fit to smoothed LSFy', MTFres.(sel).fitLSFy)
                        ENDIF
                        LOADCT, 13, /SILENT
                        IF tagMTFres.hasValue('SMLSFY') THEN BEGIN
                          oplot, MTFres.(sel).dy, MTFres.(sel).smLSFy, COLOR=150, linestyle=1
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY002ypos mm',MTFres.(sel).dy, 'Smoothed LSFy', MTFres.(sel).smLSFy)
                        ENDIF
                        oplot, MTFres.(sel).dx, MTFres.(sel).LSFx, COLOR=255
                        oplot, MTFres.(sel).dy, MTFres.(sel).LSFy, COLOR=255, linestyle=1
                        LOADCT, 0, /SILENT
                      ENDIF ELSE TVSCL, INTARR(550,550)
                    ENDIF ELSE BEGIN
                      valuesPlot=CREATE_STRUCT('pos mm', MTFres.dx, 'LSF', MTFres.LSFx)
                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.dx),max(MTFres.dx)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.LSFx),max(MTFres.LSFx)]
                      tagMTFres=tag_names(MTFres)
                      IF tagMTFres.hasValue('FITLSFX') THEN BEGIN
                        PLOT, MTFres.dx,MTFres.fitLSFx, linestyle=0, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                        valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY001pos mm',MTFres.dx, 'Fit to smoothed LSF', MTFres.fitLSFx)
                      ENDIF ELSE PLOT, MTFres.dx,MTFres.dx, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                      LOADCT, 13, /SILENT
                      IF tagMTFres.hasValue('SMLSFX') THEN BEGIN
                        OPLOT, MTFres.dx,MTFres.smLSFx, COLOR=150, linestyle=0
                        valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY002pos mm',MTFres.dx, 'Smoothed LSF', MTFres.smLSFx)
                      ENDIF
                      OPLOT, MTFres.dx,MTFres.LSFx, COLOR=255, linestyle=0
                      LOADCT, 0, /SILENT
                    ENDELSE
                  END
                  3: BEGIN ;MTF

                    mm2cm=1.;1. means no converstion to cm, 10. when convert to cm
                    IF v3d EQ 0 THEN BEGIN
                      IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN

                        tagMTFres=tag_names(MTFres.(sel))
                        valuesPlot=CREATE_STRUCT('discrete du mm_1',MTFres.(sel).fx*mm2cm, 'discrete MTFu',MTFres.(sel).MTFx)
                        IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian du mm_1',MTFres.(sel).gfx*mm2cm,'gaussian MTFu',MTFres.(sel).gMTFx)
                        IF tagMTFres.hasValue('FY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'discrete dv mm_1',MTFres.(sel).fy*mm2cm, 'discrete MTFv',MTFres.(sel).MTFy)
                        IF tagMTFres.hasValue('GFY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian dv mm_1',MTFres.(sel).gfy*mm2cm,'gaussian MTFv',MTFres.(sel).gMTFy)

                        IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]*mm2cm
                        IF setRangeMinMaxY THEN rangeY=[min([MTFres.(sel).MTFx,MTFres.(sel).MTFy]),max([MTFres.(sel).MTFx,MTFres.(sel).MTFy])]
                        PLOT, MTFres.(sel).fx*mm2cm,MTFres.(sel).MTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='MTF gaussian (red) and discrete (white) x and y (dotted)', XSTYLE=1, YSTYLE=1
                        OPLOT, MTFres.(sel).fy*mm2cm,MTFres.(sel).MTFy,linestyle=1
                        loadct, 13, /SILENT
                        IF tagMTFres.hasValue('GFX') THEN OPLOT, MTFres.(sel).gfx*mm2cm,MTFres.(sel).gMTFx,linestyle=0
                        IF tagMTFres.hasValue('GFY') THEN OPLOT, MTFres.(sel).gfy*mm2cm,MTFres.(sel).gMTFy,linestyle=1
                        OPLOT, [0,MTFres.(sel).F50_10_2(0)*mm2cm],[.5,.5],linestyle=0
                        OPLOT, [0,MTFres.(sel).F50_10_2(1)*mm2cm],[.1,.1],linestyle=0
                        OPLOT, [0,MTFres.(sel).F50_10_2(2)*mm2cm],[.02,.02],linestyle=0
                        OPLOT, [0,MTFres.(sel).F50_10_2(3)*mm2cm],[.5,.5],linestyle=1
                        OPLOT, [0,MTFres.(sel).F50_10_2(4)*mm2cm],[.1,.1],linestyle=1
                        OPLOT, [0,MTFres.(sel).F50_10_2(5)*mm2cm],[.02,.02],linestyle=1
                        loadct, 0, /SILENT
                        OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0
                      ENDIF ELSE TVSCL, INTARR(550,550)
                    ENDIF ELSE BEGIN

                      tagMTFres=tag_names(MTFres)
                      valuesPlot=CREATE_STRUCT('discrete frequency mm_1',MTFres.fx*mm2cm, 'discrete MTF',MTFres.MTFx)
                      IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian frequency mm_1',MTFres.gfx*mm2cm,'gaussian MTF',MTFres.gMTFx)

                      IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]*mm2cm
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.MTFx),max(MTFres.MTFx)]
                      PLOT, MTFres.fx*mm2cm,MTFres.MTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, TITLE='MTF gaussian and discrete (dotted)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                      IF tagMTFres.hasValue('GFX') THEN OPLOT, MTFres.gfx*mm2cm,MTFres.gMTFx, linestyle=0
                      OPLOT, MTFres.fx*mm2cm, MTFres.MTFx, LINESTYLE=1
                      OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0
                      OPLOT, [0,MTFres.F50_10_2(0)*mm2cm],[.5,.5],linestyle=0
                      OPLOT, [0,MTFres.F50_10_2(1)*mm2cm],[.1,.1],linestyle=0
                      OPLOT, [0,MTFres.F50_10_2(2)*mm2cm],[.02,.02],linestyle=0
                    ENDELSE
                  END
                  ELSE: TVSCL, INTARR(550,550)
                ENDCASE
              END

              'NPS': BEGIN
                IF N_TAGS(NPSres.(sel)) GT 1 THEN BEGIN
                  v3d=WIDGET_INFO(btnNPSavg, /BUTTON_SET)
                  IF v3d EQ 0 THEN BEGIN
                    IF setRangeMinMaxX THEN rangeX=[0,max(NPSres.(sel).dr)]
                    IF setRangeMinMaxY THEN rangeY=[0,max(NPSres.(sel).rNPS)]
                    PLOT, NPSres.(sel).dr,NPSres.(sel).rNPS, XTITLE='spatial frequency (mm-1)',YTITLE='NPS',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='NPS', XSTYLE=1, YSTYLE=1
                    valuesPlot=CREATE_STRUCT('frequency mm_1',NPSres.(sel).dr,'NPS',NPSres.(sel).rNPS)
                    statTxt=[['AUC: '+STRING(NPSres.(sel).AUC, FORMAT='(f0.'+nDecimals(NPSres.(sel).AUC)+')')],$
                      ['Integral NPS: '+STRING(NPSres.(sel).varianceIntNPS, FORMAT='(f0.'+nDecimals(NPSres.(sel).varianceIntNPS)+')')],$
                      ['Avg var ROIs: '+STRING(NPSres.(sel).varianceImg^2, FORMAT='(f0.'+nDecimals(NPSres.(sel).varianceImg^2)+')')]]
                  ENDIF ELSE BEGIN

                    NPStot=0
                    ftemp=0
                    FOR i=0, nImg-1 DO BEGIN
                      IF markedArr(i) THEN BEGIN
                        IF N_ELEMENTS(NPStot) EQ 1 THEN BEGIN
                          NPStot=NPSres.(i).rNPS
                          ftemp=NPSres.(i).dr
                        ENDIF ELSE NPStot=NPStot+NPSres.(i).rNPS
                      ENDIF
                    ENDFOR
                    NPStot=NPStot/TOTAL(markedArr)
                    IF setRangeMinMaxX THEN rangeX=[0,max(NPSres.(sel).dr)]
                    IF setRangeMinMaxY THEN rangeY=[0,max(NPStot)]
                    valuesPlot=CREATE_STRUCT('f',ftemp, 'NPStot', NPStot)
                    plot,NPSres.(sel).dr,NPStot,XTITLE='spatial frequency (mm-1)',YTITLE='NPS',TITLE='NPS for each image and average', linestyle=0, THICK=2, XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                    AUC=INT_TABULATED(NPSres.(sel).dr,NPStot)
                    statTxt=[['AUC for average: '+STRING(AUC, FORMAT='(f0.'+nDecimals(AUC)+')')],$
                      ['Integral NPS: '+STRING(NPSres.(sel).varianceIntNPS, FORMAT='(f0.'+nDecimals(NPSres.(sel).varianceIntNPS)+')')],$
                      ['Avg variance ROIs: '+STRING(NPSres.(sel).varianceImg^2, FORMAT='(f0.'+nDecimals(NPSres.(sel).varianceImg^2)+')')]]
                    IF TOTAL(markedArr) GT 1 THEN BEGIN
                      LOADCT, 13, /SILENT
                      FOR i=0, nImg-1 DO BEGIN
                        IF markedArr(i) THEN BEGIN
                          oplot, NPSres.(i).dr, NPSres.(i).rNPS, LINESTYLE=0, COLOR=i*40
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'f',NPSres.(i).dr, 'NPS'+STRING(i,FORMAT='(i0)'), NPSres.(i).rNPS)
                        ENDIF
                      ENDFOR
                      LOADCT, 0, /SILENT
                    ENDIF
                    oplot, NPSres.(sel).dr,NPStot,linestyle=0, THICK=2
                  ENDELSE

                  OPLOT, [nyqfr,nyqfr],[0,1],linestyle=1,thick=1.

                  WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
                  WSET, iDrawImageRes
                  activeResImg=NPSres.(sel).NPS
                  tvRes=activeResImg
                  szNPS=SIZE(activeResImg, /DIMENSIONS)
                  szX=450
                  szY=ROUND(szX*(szNPS(1)*1./szNPS(0)))
                  TVSCL,congrid(adjustWindowLevel(tvRes, [0,100*max(NPSres.(sel).NPS)]), szX, szY, /INTERP)

                ENDIF ELSE TVSCL, INTARR(550,550)
              END

              'CTLIN': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                szTab=SIZE(resArr, /DIMENSIONS)
                IF setRangeMinMaxX THEN rangeX=[min(FLOAT(resArr[*,rowNo])),max(FLOAT(resArr[*,rowNo]))]
                WIDGET_CONTROL, tblLin, GET_VALUE=linTable
                densit=TRANSPOSE(FLOAT(linTable[3,*]))
                IF setRangeMinMaxY THEN rangeY=[min(densit),max(densit)]
                PLOT, FLOAT(resArr[0:szTab(0)-1,rowNo]), densit, PSYM=4,  XTITLE='CT number (HU)', YTITLE='Density' , TITLE='CT linearity for first image', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=2, YSTYLE=2
                a0=REGRESS(resArr[0:szTab(0)-1,rowNo],densit, YFIT=yfit)
                OPLOT, resArr[0:szTab(0)-1,rowNo], yfit, LINESTYLE=0
              END

              'SLICETHICK': BEGIN
                tabSel=WIDGET_INFO(resTab,/TABLE_SELECT)
                colNo=tabSel(0)
                loadct, 12, /SILENT
                colors=[200,50,100,255,130,80]
                WIDGET_CONTROL, cw_ramptype, GET_VALUE=ramptype

                first=0
                IF ramptype EQ 0 THEN last=3 ELSE last=5
                IF colNo GT 0 AND ( colNo LT 5 AND ramptype EQ 0 ) THEN BEGIN
                  first=colNo-1
                  last=first
                ENDIF

                lines=['H1','H2','V1','V2','iV1','iV2']
                FOR line=first,last DO BEGIN
                  vect=sliceThickRes.(sel).(line).vector
                  nPixVec=N_ELEMENTS(vect)
                  IF line EQ first THEN valuesPlot=CREATE_STRUCT('pix',INDGEN(nPixVec),lines(line), vect) ELSE valuesPlot=CREATE_STRUCT(valuesPlot,'COPY'+STRING(line,FORMAT='(i03)')+'pix',INDGEN(nPixVec), lines(line), vect)

                  backGround=sliceThickRes.(sel).(line).background
                  lenVec=N_ELEMENTS(vect)
                  halfMax=sliceThickRes.(sel).(line).halfMax
                  nPixBackG=sliceThickRes.(sel).(line).nBackGr

                  IF setRangeMinMaxX THEN rangeX=[0,lenVec]
                  IF setRangeMinMaxY THEN rangeY=[min(vect),max(vect)]
                  IF line EQ first THEN PLOT, vect,TITLE='Vectors to find slice thickness', YTITLE='CT value (HU)', XTITLE='n pix', COLOR=255, LINESTYLE=0, XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                  OPLOT, vect, COLOR=colors(line), LINESTYLE=0
                  OPLOT, INDGEN(nPixBackG), FLTARR(nPixBackG)+backGround, COLOR=colors(line), LINESTYLE=1
                  OPLOT, INDGEN(nPixBackG)+lenVec-nPixBackG, FLTARR(nPixBackG)+backGround, COLOR=colors(line), LINESTYLE=1
                  OPLOT, sliceThickRes.(sel).(line).firstLast,[halfMax,halfMax], COLOR=colors(line), LINESTYLE=1
                  OPLOT, [0,lenVec],[sliceThickRes.(sel).(line).maxVal,sliceThickRes.(sel).(line).maxVal], COLOR=colors(line), LINESTYLE=1
                ENDFOR
                loadct, 0, /SILENT
              END

              'NOISE': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                zPosMarked=getZposMarked(structImgs, markedTemp)
                yValues = FLOAT(resArr[1,*])
                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[min(yValues),max(yValues)]
                PLOT, zPosMarked, yValues,  XTITLE='zPos (mm)', YTITLE='Noise = stdev (HU)' , TITLE='Noise for all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Noise', yValues)
              END

              ELSE:TVSCL, INTARR(550,550)
            ENDCASE


          ENDIF ELSE TVSCL, INTARR(550,550)
        END

        ;- -------------------- Xray ----------------------------
        1:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyseStringsXray(curTab+1) OF

              'STP': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                xValues = FLOAT(resArr[1,*])
                yValues = FLOAT(resArr[2,*])
                IF setRangeMinMaxX THEN rangeX=[0, max(xValues)*1.1]
                IF setRangeMinMaxY THEN rangeY=[0,max(yValues)*1.1]
                PLOT, xValues, yValues,  XTITLE='Dose', YTITLE='Pixel Value' , TITLE='Signal Transfer Properties', XRANGE=rangeX, YRANGE=rangeY, PSYM=2, XSTYLE=1, YSTYLE=1
                tagn=TAG_NAMES(stpRes)
                ain=WHERE(tagn EQ 'A')
                IF ain(0) NE -1 THEN BEGIN
                  yfit=xValues*stpRes.a(0)+stpRes.b
                  OPLOT, xValues,yfit, LINESTYLE=1
                  statTxt=[$
                    ['slope  = '+STRING(stpRes.a(0), FORMAT='(f0.'+nDecimals(stpRes.a(0))+')')],$
                    ['offset = '+STRING(stpRes.b, FORMAT='(f0.'+nDecimals(stpRes.b)+')')],$
                    ['mcorr  = '+STRING(stpRes.mcorr, FORMAT='(f0.'+nDecimals(stpRes.mcorr)+')')]]
                ENDIF
              END

              'NOISE': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                tags=TAG_NAMES(structImgs)
                nImg=N_ELEMENTS(tags)
                imgNo=markedTemp
                yValues = FLOAT(resArr[1,*])
                IF setRangeMinMaxX THEN rangeX=[min(imgNo),max(imgNo)]
                IF setRangeMinMaxY THEN rangeY=[min(yValues),max(yValues)]
                PLOT, imgNo, yValues,  XTITLE='imgNo', YTITLE='Noise = stdev' , TITLE='Noise for all images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                valuesPlot=CREATE_STRUCT('image number', imgNo, 'Noise', yValues)
              END

              'MTF': BEGIN

                IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
                  WIDGET_CONTROL, cw_plotMTFX, GET_VALUE= plotWhich
                  WIDGET_CONTROL, cw_formLSFX, GET_VALUE=formLSF

                  CASE plotWhich OF

                    0: BEGIN ;edge
                      plot, MTFres.(sel).edgePos,  MTFres.(sel).edgeRow, PSYM=3, XTITLE='Position of edge (pix i ROI)', YTITLE='Profile number', TITLE='Edgeposition for each row in ROI with interpolation', XSTYLE=1, YSTYLE=1
                      oplot,  MTFres.(sel).edgeFitx, MTFres.(sel).edgeFity
                      statTxt=['Angle = '+STRING(MTFres.(sel).angle,FORMAT='(f0.2)')]
                      valuesPlot=CREATE_STRUCT('position pix', MTFres.(sel).edgePos, 'Row or column number ', MTFres.(sel).edgeRow, 'fitted position pix', MTFres.(sel).edgeFitx, 'fitted row_col', MTFres.(sel).edgeFity)
                    END
                    1: BEGIN ;sorted pixelvalues, interpolated (and smoothed)

                      valuesPlot=CREATE_STRUCT('distance from edge', MTFres.(sel).newdists, 'Interpolated pixel values', MTFres.(sel).pixValsInterp, 'COPY000distance from edge', MTFres.(sel).newdists,'Smoothed pixelvalues',MTFres.(sel).pixValsSmooth)

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).distspix0),max(MTFres.(sel).distspix0)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).pixValSort),max(MTFres.(sel).pixValSort)]

                      plot, MTFres.(sel).distspix0, MTFres.(sel).pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                      LOADCT, 13, /SILENT
                      oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsInterp, COLOR=255
                      oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsSmooth, COLOR=150
                      LOADCT, 0, /SILENT

                    END
                    2: BEGIN; LSF

                      valuesPlot=CREATE_STRUCT('pos mm',MTFres.(sel).newdists, 'LSF', MTFres.(sel).dLSF)
                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).newdists),max(MTFres.(sel).newdists)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).dLSF),max(MTFres.(sel).dLSF)]

                      CASE formLSF OF
                        0: tit='LSF for discrete MTF (red), with fitted exponetial (white)'
                        1: tit='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)'
                        2: tit='LSF for discrete MTF'
                        ELSE:
                      ENDCASE
                      PLOT, MTFres.(sel).newdists,MTFres.(sel).dLSF, XTITLE='position (mm)',TITLE=tit, XRANGE=rangeX, YRANGE=rangeY, /NODATA, XSTYLE=1, YSTYLE=1
                      LOADCT, 13, /SILENT
                      OPLOT, MTFres.(sel).newdists,MTFres.(sel).dLSF, COLOR=255, linestyle=0
                      IF formLSF EQ 1 THEN BEGIN
                        OPLOT, MTFres.(sel).d,MTFres.(sel).smLSF, COLOR=150, linestyle=0
                        valuesPlot=CREATE_STRUCT(valuesPlot,'pos mm smoothed', MTFres.(sel).d, 'smoothed LSF', MTFres.(sel).smLSF)
                      ENDIF
                      LOADCT, 0, /SILENT
                      IF formLSF NE 2 THEN BEGIN
                        OPLOT, MTFres.(sel).d,MTFres.(sel).gLSF, linestyle=0
                        valuesPlot=CREATE_STRUCT(valuesPlot,'pos mm fitted', MTFres.(sel).d, 'fitted LSF', MTFres.(sel).gLSF)
                      ENDIF

                    END
                    3: BEGIN ;MTF

                      tagMTFres=tag_names(MTFres.(sel))
                      IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]
                      IF setRangeMinMaxY THEN rangeY=[0,max([MTFres.(sel).dMTF])]

                      CASE formLSF OF
                        0: textStr='exponetial'
                        1: textStr='gaussian'
                        2: textStr=''
                        ELSE:
                      ENDCASE
                      IF formLSF EQ 2 THEN BEGIN
                        valuesPlot=CREATE_STRUCT('frequency mm_1',MTFres.(sel).dx,'MTF',MTFres.(sel).dMTF)
                        tit=''
                      ENDIF ELSE BEGIN
                        valuesPlot=CREATE_STRUCT('discrete du mm_1',MTFres.(sel).dx,'discrete MTF',MTFres.(sel).dMTF)
                        IF tagMTFres.HasValue('GMTF') THEN valuesPlot=CREATE_STRUCT(valuesPlot, textStr+' du mm_1',MTFres.(sel).gx,textStr+' MTF',MTFres.(sel).gMTF)
                        tit='MTF '+textStr+' (red) and discrete (white)'
                      ENDELSE

                      PLOT, MTFres.(sel).dx,MTFres.(sel).dMTF, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE=tit, XSTYLE=1, YSTYLE=1

                      IF formLSF NE 2 AND tagMTFres.HasValue('GMTF') THEN BEGIN
                        loadct, 13, /SILENT
                        OPLOT, MTFres.(sel).gx,MTFres.(sel).gMTF,linestyle=0
                      ENDIF
                      lim=[0.5,1.,1.5,2.,2.5]
                      FOR i=0,4 DO OPLOT, [lim(i),lim(i)],[0,MTFres.(sel).lpmm(i)],linestyle=0
                      OPLOT, [0,MTFres.(sel).lpmm(5)],[0.5,0.5], linestyle=0
                      loadct, 0, /SILENT

                      OPLOT, [nyqfr,nyqfr],[0,1],linestyle=0,thick=2.

                      sincRect=SIN(!pi*pix*MTFres.(sel).dx)/(!pi*pix*MTFres.(sel).dx)
                      sincRect(0)=1.
                      OPLOT, MTFres.(sel).dx, sincRect, linestyle=1

                    END
                    ELSE: TVSCL, INTARR(550,550)
                  ENDCASE

                ENDIF
              END

              'NPS': BEGIN

                IF N_TAGS(NPSres.(sel)) GT 1 THEN BEGIN

                  IF setRangeMinMaxX THEN rangeX=[0,1./(2*pix)]
                  IF setRangeMinMaxY THEN rangeY=[0,max([NPSres.(sel).uNPS,NPSres.(sel).vNPS])]/(NPSres.(sel).largeAreaSignal^2)
                  PLOT, NPSres.(sel).du,NPSres.(sel).uNPS/(NPSres.(sel).largeAreaSignal^2), XTITLE='spatial frequency (mm-1)',YTITLE='NNPS',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='NNPS', XSTYLE=1, YSTYLE=1
                  OPLOT, NPSres.(sel).du,NPSres.(sel).vNPS/(NPSres.(sel).largeAreaSignal^2),linestyle=1
                  OPLOT, [nyqfr,nyqfr],[0,1],linestyle=1,thick=1.

                  WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
                  WSET, iDrawImageRes
                  activeResImg=NPSres.(sel).NPS
                  tvRes=activeResImg
                  tvRes[*,128]=0 & tvRes[128,*]=0
                  szNPS=SIZE(activeResImg, /DIMENSIONS)
                  szX=450
                  szY=ROUND(szX*(szNPS(1)*1./szNPS(0)))
                  TVSCL,congrid(adjustWindowLevel(tvRes, [0,max(tvRes)]), szX, szY, /INTERP)
                  valuesPlot=CREATE_STRUCT('frequency mm_1',NPSres.(sel).du,'uNNPS', NPSres.(sel).uNPS/(NPSres.(sel).largeAreaSignal^2), 'COPY000frequency mm_1',NPSres.(sel).du, 'vNNPS', NPSres.(sel).vNPS/(NPSres.(sel).largeAreaSignal^2))
                      
                ENDIF ELSE TVSCL, INTARR(550,550)
              END

              ELSE:TVSCL, INTARR(550,550)
            ENDCASE

          ENDIF ELSE TVSCL, INTARR(550,550)
        END
        ;******************************** NM *************************************************
        2:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyseStringsNM(curTab+1) OF
              'ENERGYSPEC': BEGIN
                fwhm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(2))
                fwtm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(10))

                IF setRangeMinMaxX THEN rangeX=[-fwtm,fwtm]+energyRes.gausscoeff(1)
                IF setRangeMinMaxY THEN rangeY=[0,energyRes.gausscoeff(0)]

                PLOT, energyRes.curve[0,*],energyRes.curve[1,*], XTITLE='Energy (keV)',YTITLE='Counts',PSYM=3, TITLE='Energy spectrum and gaussian fit', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                LOADCT, 13, /SILENT
                gfit=calcGauss(energyRes.curve[0,*],energyRes.gausscoeff(2),energyRes.gausscoeff(0),energyRes.gausscoeff(1))
                OPLOT, energyRes.curve[0,*], gfit
                OPLOT, [energyRes.gausscoeff(1)-fwhm*0.5,energyRes.gausscoeff(1)-fwhm*0.5],[0,energyRes.gausscoeff(0)],linestyle=1,thick=1.
                OPLOT, [energyRes.gausscoeff(1)+fwhm*0.5,energyRes.gausscoeff(1)+fwhm*0.5],[0,energyRes.gausscoeff(0)],linestyle=1,thick=1.
                OPLOT, [energyRes.gausscoeff(1)-fwtm*0.5,energyRes.gausscoeff(1)-fwtm*0.5],[0,energyRes.gausscoeff(0)],linestyle=2,thick=1.
                OPLOT, [energyRes.gausscoeff(1)+fwtm*0.5,energyRes.gausscoeff(1)+fwtm*0.5],[0,energyRes.gausscoeff(0)],linestyle=2,thick=1.
                LOADCT, 0, /SILENT

                valuesPlot=CREATE_STRUCT('Energy keV',energyRes.curve,'Counts', energyRes.curve[1,*], 'Gaussian fit', gfit)

              END
              'SCANSPEED': BEGIN
                WIDGET_CONTROL, txtScanSpeedMedian, GET_VALUE=val
                medfilt=LONG(val(0))
                WIDGET_CONTROL, txtSpeedROIheight, GET_VALUE=val
                height=FLOAT(val(0))*10./pix
                WIDGET_CONTROL, txtNAvgSpeedNM, GET_VALUE=val
                nnn=LONG(val(0))
                nnn=nnn/2
                IF nnn LT 0 THEN nnn=0
                sz=SIZE(activeImg, /DIMENSIONS)
                xpos=ROUND(sz(0)/2+dxya(0))
                xvals=pix*(INDGEN(sz(1))-sz(1)/2+dxya(1))
                IF dxya(1)+sz(1)/2 GE height/2 THEN first=dxya(1)+sz(1)/2-height/2 ELSE first=0
                IF dxya(1)+sz(1)/2+height/2 LT sz(1)-1 THEN last=dxya(1)+sz(1)/2+height/2 ELSE last=sz(1)-1
                xvals=xvals[first:last]
                yvals=TOTAL(activeImg[xpos-nnn:xpos+nnn,*],1)/(nnn*2+1)
                yvals=yvals[first:last]
                meanVal=MEAN(yvals)
                IF setRangeMinMaxX THEN rangeX=[min(xvals),max(xvals)]
                IF setRangeMinMaxY THEN rangeY=meanVal*[0.9,1.1]
                PLOT, xvals, yvals, XTITLE='Position (mm)', YTITLE='Signal', TITLE='Y profile of active image', XRANGE=rangeX, Yrange=rangeY, XSTYLE=1, YSTYLE=1
                LOADCT, 13, /SILENT
                OPLOT, xvals, MEDIAN(yvals,medfilt)
                OPLOT, rangeX, [meanVal,meanVal]*1.05, LINESTYLE=0, COLOR=128
                OPLOT, rangeX, [meanVal,meanVal]*0.95, LINESTYLE=0, COLOR=128
                LOADCT, 0, /SILENT
                valuesPlot=CREATE_STRUCT('Position mm',xvals, 'Averaged signal', yvals, 'Median filtered',MEDIAN(yvals,medfilt))
              END
              'CONTRAST': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                szTab=SIZE(resArr, /DIMENSIONS)
                IF setRangeMinMaxX THEN rangeX=[1,6]
                IF setRangeMinMaxY THEN rangeY=[0.,1.]
                PLOT,INDGEN(6)+1, FLOAT(resArr[7:12,rowNo]), PSYM=4,  XTITLE='ROI number', YTITLE='Contrast' , XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
              END
              'RADIAL': BEGIN
                WIDGET_CONTROL, txtRadialMedian, GET_VALUE=val
                medfilt=LONG(val(0))
                xvals=FINDGEN(N_ELEMENTS(radialRes[*,sel]))*0.1*pix; interpolated to 0.1*pix
                yvals=radialRes[*,sel]
                IF setRangeMinMaxX THEN rangeX=[min(xvals),max(xvals)]
                IF setRangeMinMaxY THEN rangeY=[min(yvals),max(yvals)]
                PLOT, xvals, yvals, XTITLE='Radial distance from center (mm)', YTITLE='Signal', TITLE='Radial profile of active image, interpolated to 0.1 pix + medianfiltered', XRANGE=rangeX, Yrange=rangeY, XSTYLE=1, YSTYLE=1
                LOADCT, 13, /SILENT
                OPLOT, xvals, MEDIAN(yvals,medfilt)
                LOADCT, 0, /SILENT
                valuesPlot=CREATE_STRUCT('Position mm',xvals, 'Signal', yvals, 'Median filtered',MEDIAN(yvals,medfilt))
              END
              'MTF': BEGIN
                resforeach=WHERE(TAG_NAMES(MTFres) EQ 'M0')
                v3d=0
                IF resforeach(0) EQ -1 THEN v3d=1

                WIDGET_CONTROL, cw_plotMTFNM, GET_VALUE= plotWhich

                CASE plotWhich OF

                  0: BEGIN ;centered xy profile
                    IF v3d THEN tagsMTF=TAG_NAMES(MTFres) ELSE tagsMTF=TAG_NAMES(MTFres.(sel))
                    profIn=WHERE(tagsMTF EQ 'CDX')
                    IF profIn(0) NE -1 THEN BEGIN
                      IF v3d THEN BEGIN
                        valuesPlot=CREATE_STRUCT('x', MTFres.cdx,'Xprof1', MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0] , 'y', MTFres.cdy, 'Yprof1', MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0])

                        szM=size(MTFres.submatrixAll,/DIMENSIONS)
                        plot, MTFres.cdx,MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0],TITLE='Profile x and y centered', YRANGE=[min(MTFres.submatrixAll),max(MTFres.submatrixAll)], linestyle=0, XSTYLE=1, YSTYLE=1
                        oplot, MTFres.cdy,MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0], linestyle=1
                        IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                          LOADCT, 13, /SILENT
                          FOR i = 1, szM(2)-1 DO BEGIN
                            oplot, MTFres.cdx, MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], LINESTYLE=0, COLOR=i*40
                            oplot,  MTFres.cdy, MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i], LINESTYLE=1, COLOR=i*40
                            valuesPlot=CREATE_STRUCT(valuesPlot,'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.cdx,'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y',MTFres.cdy,'Yprof'+STRING(i,FORMAT='(i0)'),MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                          ENDFOR
                          LOADCT, 0, /SILENT
                        ENDIF
                      ENDIF ELSE BEGIN
                        valuesPlot=CREATE_STRUCT('x', MTFres.(sel).cdx,'Xprof1', MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0] , 'y', MTFres.(sel).cdy, 'Yprof1', MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0])

                        szM=size(MTFres.(sel).submatrixAll,/DIMENSIONS)
                        xprof=MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0]
                        yprof=MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0]
                        plot, MTFres.(sel).cdx,xprof,TITLE='Profile x and y centered', YRANGE=[min(MTFres.(sel).submatrixAll),max(MTFres.(sel).submatrixAll)], linestyle=0, XSTYLE=1, YSTYLE=1
                        oplot, MTFres.(sel).cdy,MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0], linestyle=1

                        maxval=max(xprof)
                        minval=min(xprof)
                        resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                        resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                        statTxt=[['Statistics X profile:'],['FWHM: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*pix, FORMAT='(f0.0)')]]
                        maxval=max(yprof)
                        minval=min(yprof)
                        resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                        resFWTM=getWidthAtThreshold(yprof,(maxval-minval)/10.+minval)
                        statTxt=[[statTxt],[''],['Statistics Y profile:'],['FWHM: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*pix, FORMAT='(f0.0)')]]
                      ENDELSE
                    ENDIF ELSE BEGIN
                      TVSCL, INTARR(550,550)
                      XYOUTS, -4,.5, 'N/A for current method.', CHARSIZE=1.2
                    ENDELSE
                  END

                  1: BEGIN ;line
                    IF v3d THEN BEGIN
                      TVSCL, INTARR(550,550)
                      XYOUTS, -4,.5, 'N/A for current method.', CHARSIZE=1.2
                    ENDIF ELSE BEGIN
                      tagsMTF=TAG_NAMES(MTFres.(sel))
                      edgeIn=WHERE(tagsMTF EQ 'EDGEPOS')
                      IF edgeIn(0) NE -1 THEN BEGIN
                        plot, MTFres.(sel).edgePos,  MTFres.(sel).edgeRow, PSYM=3, XTITLE='Position of line (pix i ROI)', YTITLE='Profile number', TITLE='Lineposition for each row in ROI with interpolation.', XSTYLE=1, YSTYLE=1
                        oplot,  MTFres.(sel).edgeFitx, MTFres.(sel).edgeFity
                        statTxt=['Angle: '+STRING(MTFres.(sel).angle,FORMAT='(f0.3)') ]
                        valuesPlot=CREATE_STRUCT('position pix', MTFres.(sel).edgePos, 'Row or column number ', MTFres.(sel).edgeRow, 'fitted position pix', MTFres.(sel).edgeFitx, 'fitted row_col', MTFres.(sel).edgeFity)
                      ENDIF ELSE BEGIN
                        TVSCL, INTARR(550,550)
                        XYOUTS, -4,.5, 'N/A for current method.', CHARSIZE=1.2
                      ENDELSE
                    ENDELSE
                  END
                  2: BEGIN ;sorted pixelvalues, interpolated and smoothed
                    IF v3d THEN BEGIN

                      valuesPlot=CREATE_STRUCT('distance from center', MTFres.newdists, 'Interpolated pixel values', MTFres.pixValsInterp, 'Smoothed pixelvalues', MTFres.pixValsSmooth)

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.distspix0),max(MTFres.distspix0)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.pixValSort),max(MTFres.pixValSort)]

                      plot, MTFres.distspix0, MTFres.pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                      szPS=size(MTFres.pixValSort,/DIMENSIONS)
                      IF N_ELEMENTS(szPS) EQ 2 THEN BEGIN
                        FOR b=1, szPS(2)-1 DO oplot,  MTFres.distspix0, MTFres.pixValSort[*,b], PSYM=3
                      ENDIF
                      LOADCT, 13, /SILENT
                      oplot, MTFres.newdists, MTFres.pixValsInterp, COLOR=255
                      oplot, MTFres.newdists, MTFres.pixValsSmooth, COLOR=150
                      LOADCT, 0, /SILENT

                    ENDIF ELSE BEGIN
                      tagsMTF=TAG_NAMES(MTFres.(sel))
                      IF tagsMTF.HasValue('DISTSPIX0') THEN BEGIN
                        valuesPlot=CREATE_STRUCT('distance from center', MTFres.(sel).newdists,'Interpolated pixel values', MTFres.(sel).pixValsInterp,'Smoothed pixelvalues', MTFres.(sel).pixValsSmooth)

                        IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).distspix0),max(MTFres.(sel).distspix0)]
                        IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).pixValSort),max(MTFres.(sel).pixValSort)]

                        plot, MTFres.(sel).distspix0, MTFres.(sel).pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                        szM=size(MTFres.(sel).submatrixAll,/DIMENSIONS)
                        IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                          FOR b=1, szM(2)-1 DO oplot,  MTFres.(sel).distspix0, MTFres.(sel).pixValSort[*,b], PSYM=3
                        ENDIF
                        LOADCT, 13, /SILENT
                        oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsInterp, COLOR=255
                        oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsSmooth, COLOR=150
                        LOADCT, 0, /SILENT
                      ENDIF ELSE BEGIN
                        TVSCL, INTARR(550,550)
                        XYOUTS, -4,.5, 'Sorted pixelvalues not available for point method.', CHARSIZE=1.2
                      ENDELSE
                    ENDELSE

                  END
                  3: BEGIN; LSF
                    IF v3d EQ 0 THEN BEGIN
                      tagMTFres=tag_names(MTFres.(sel))
                      IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
                        IF N_ELEMENTS(MTFres.(sel).dy) EQ 1 THEN BEGIN
                          valuesPlot=CREATE_STRUCT('pos mm',MTFres.(sel).dx,'LSF',MTFres.(sel).LSFx)
                          IF tagMTFres.HasValue('SMLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000pos mm',MTFres.(sel).dx,'Smoothed LSF', MTFres.(sel).smLSFx)
                          IF tagMTFres.HasValue('FITLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001pos mm',MTFres.(sel).dx, 'Fitted LSF', MTFres.(sel).fitLSFx)
                          IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).dx),max(MTFres.(sel).dx)]
                          IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).LSFx),max(MTFres.(sel).LSFx)]
                        ENDIF ELSE BEGIN
                          valuesPlot=CREATE_STRUCT('xpos mm', MTFres.(sel).dx, 'LSFx', MTFres.(sel).LSFx)
                          IF tagMTFres.HasValue('SMLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000xpos mm',MTFres.(sel).dx,'Smoothed LSFX', MTFres.(sel).smLSFx)
                          IF tagMTFres.HasValue('FITLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001xpos mm',MTFres.(sel).dx, 'Fitted LSFX', MTFres.(sel).fitLSFx)
                          valuesPlot=CREATE_STRUCT(valuesPlot,'ypos mm', MTFres.(sel).dy, 'LSFy', MTFres.(sel).LSFy)
                          IF tagMTFres.HasValue('SMLSFY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000ypos mm',MTFres.(sel).dy,'Smoothed LSFY', MTFres.(sel).smLSFy)
                          IF tagMTFres.HasValue('FITLSFY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001ypos mm',MTFres.(sel).dy, 'Fitted LSFY', MTFres.(sel).fitLSFy)
                          IF setRangeMinMaxX THEN rangeX=[min([MTFres.(sel).dx,MTFres.(sel).dy]),max([MTFres.(sel).dx,MTFres.(sel).dy])]
                          IF setRangeMinMaxY THEN rangeY=[min([MTFres.(sel).LSFx,MTFres.(sel).LSFy]),max([MTFres.(sel).LSFx,MTFres.(sel).LSFy])]
                        ENDELSE

                        IF tagMTFres.HasValue('FITLSFX') THEN PLOT, MTFres.(sel).dx,MTFres.(sel).fitLSFx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1 $
                        ELSE PLOT, MTFres.(sel).dx,MTFres.(sel).LSFx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.HasValue('FITLSFY') THEN OPLOT, MTFres.(sel).dy,MTFres.(sel).fitLSFy,linestyle=1
                        LOADCT, 13, /SILENT
                        IF tagMTFres.HasValue('SMLSFX') THEN oplot, MTFres.(sel).dx, MTFres.(sel).smLSFx, COLOR=150
                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.HasValue('SMLSFY') THEN oplot, MTFres.(sel).dy, MTFres.(sel).smLSFy, COLOR=150, linestyle=1
                        oplot, MTFres.(sel).dx, MTFres.(sel).LSFx, COLOR=255
                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN oplot, MTFres.(sel).dy, MTFres.(sel).LSFy, COLOR=255, linestyle=1
                        LOADCT, 0, /SILENT

                        IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
                          xprof=MTFres.(sel).fitLSFx
                          maxval=max(xprof)
                          minval=min(xprof)
                          resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                          resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                          statTxt=[['Gaussian fit of smoothed LSF:'],['FWHM: '+STRING(resFWHM(0)*0.1*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*0.1*pix, FORMAT='(f0.0)')]]
                        ENDIF
                        ;                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN BEGIN
                        ;                          maxval=max(yprof)
                        ;                          minval=min(yprof)
                        ;                          resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                        ;                          resFWTM=getWidthAtThreshold(yprof,(maxval-minval)/10.+minval)
                        ;                          statTxt=[[statTxt],[''],['Y profile:'],['FWHM: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*pix, FORMAT='(f0.0)')]]
                        ;                        ENDIF
                      ENDIF ELSE TVSCL, INTARR(550,550)
                    ENDIF ELSE BEGIN

                      ;;;;;; TODO ;;;;;;;;;;;; add y if option for line in 3d?
                      tagMTFres=tag_names(MTFres)
                      valuesPlot=CREATE_STRUCT('pos mm',MTFres.dx,'LSF',MTFres.LSFx)
                      IF tagMTFres.HasValue('SMLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000pos mm',MTFres.dx,'Smoothed LSF', MTFres.smLSFx)
                      IF tagMTFres.HasValue('FITLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001pos mm',MTFres.dx, 'Fitted LSF', MTFres.fitLSFx)
                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.dx),max(MTFres.dx)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.LSFx),max(MTFres.LSFx)]

                      IF tagMTFres.HasValue('FITLSFX') THEN PLOT, MTFres.dx,MTFres.fitLSFx, linestyle=0, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1 $
                      ELSE PLOT, MTFres.dx,MTFres.LSFx, linestyle=0, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                      LOADCT, 13, /SILENT
                      IF tagMTFres.HasValue('SMLSFX') THEN OPLOT, MTFres.dx,MTFres.smLSFx, COLOR=150, linestyle=0
                      OPLOT, MTFres.dx,MTFres.LSFx, COLOR=255, linestyle=0
                      LOADCT, 0, /SILENT

                      IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
                        xprof=MTFres.fitLSFx
                        maxval=max(xprof)
                        minval=min(xprof)
                        resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                        resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                        statTxt=[['Gaussian fit of smoothed LSF:'],['FWHM: '+STRING(resFWHM(0)*0.1*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*.1*pix, FORMAT='(f0.0)')]]
                      ENDIF
                    ENDELSE
                  END
                  4: BEGIN ;MTF

                    mm2cm=1.;1. means no converstion to cm, 10. when convert to cm
                    IF v3d EQ 0 THEN BEGIN
                      IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
                        tagMTFres=tag_names(MTFres.(sel))
                        IF N_ELEMENTS(MTFres.(sel).dy) LE 1 THEN BEGIN
                          IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).MTFx),max(MTFres.(sel).MTFx)]
                          valuesPlot=CREATE_STRUCT('discrete du mm_1',MTFres.(sel).fx*mm2cm, 'discrete MTFu',MTFres.(sel).MTFx)
                          IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian du mm_1',MTFres.(sel).gfx*mm2cm,'gaussian MTFu',MTFres.(sel).gMTFx)
                        ENDIF ELSE BEGIN
                          IF setRangeMinMaxY THEN rangeY=[min([MTFres.(sel).MTFx,MTFres.(sel).MTFy]),max([MTFres.(sel).MTFx,MTFres.(sel).MTFy])]
                          valuesPlot=CREATE_STRUCT('discrete du mm_1',MTFres.(sel).fx*mm2cm, 'discrete MTFu',MTFres.(sel).MTFx)
                          IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian du mm_1',MTFres.(sel).gfx*mm2cm,'gaussian MTFu',MTFres.(sel).gMTFx)
                          IF tagMTFres.hasValue('FY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'discrete dv mm_1',MTFres.(sel).fy*mm2cm, 'discrete MTFv',MTFres.(sel).MTFy)
                          IF tagMTFres.hasValue('GFY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian dv mm_1',MTFres.(sel).gfy*mm2cm,'gaussian MTFv',MTFres.(sel).gMTFy)
                        ENDELSE

                        IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]*mm2cm

                        PLOT, MTFres.(sel).fx*mm2cm,MTFres.(sel).MTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='MTF gaussian (red) and discrete (white) x and y (dotted)', XSTYLE=1, YSTYLE=1
                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN OPLOT, MTFres.(sel).fy*mm2cm,MTFres.(sel).MTFy,linestyle=1
                        loadct, 13, /SILENT
                        IF tagMTFres.hasValue('GFX') THEN OPLOT, MTFres.(sel).gfx*mm2cm,MTFres.(sel).gMTFx,linestyle=0
                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.hasValue('GFY') THEN OPLOT, MTFres.(sel).gfy*mm2cm,MTFres.(sel).gMTFy,linestyle=1

                        loadct, 0, /SILENT
                        OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0,thick=2.
                      ENDIF ELSE TVSCL, INTARR(550,550)
                    ENDIF ELSE BEGIN
                      tagMTFres=tag_names(MTFres)
                      valuesPlot=CREATE_STRUCT('discrete frequency mm_1',MTFres.fx*mm2cm, 'discrete MTF',MTFres.MTFx)
                      IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian frequency mm_1',MTFres.gfx*mm2cm,'gaussian MTF',MTFres.gMTFx)

                      IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]*mm2cm
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.gMTFx),max(MTFres.gMTFx)]
                      IF tagMTFres.hasValue('GFX') THEN PLOT, MTFres.gfx*mm2cm,MTFres.gMTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, TITLE='MTF gaussian and discrete (dotted)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1 $
                      ELSE PLOT, MTFres.gfx*mm2cm,MTFres.gMTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, TITLE='MTF (discrete)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                      OPLOT, MTFres.fx*mm2cm, MTFres.MTFx, LINESTYLE=1
                      OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0,thick=2.
                      OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=1,thick=2.
                    ENDELSE
                  END; show MTF
                  ELSE: TVSCL, INTARR(550,550)
                ENDCASE; MTF test
              END

              ELSE:TVSCL, INTARR(550,550)
            ENDCASE; tests

          ENDIF ELSE TVSCL, INTARR(550,550)
        END
        
        ;******************************** PET *************************************************
        3:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyseStringsPET(curTab+1) OF
              
              'CROSSCALIB': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                zPosMarked=getZposMarked(structImgs, markedTemp)
                yValues = FLOAT(resArr[0,*])
                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[min(yValues),max(yValues)]
                PLOT, zPosMarked, yValues,  XTITLE='zPos (mm)', YTITLE='Activity concentration (Bq/mL)' , TITLE='Activity concentration for all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Activity concentration', yValues)
                END
                
              'HOMOG':BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                resArr=FLOAT(resArr[5:9,*])
                colNo=5
                zPosMarked=getZposMarked(structImgs, markedTemp)
                
                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[-10,10]
                       
                valCenter=transpose(resArr[0,*])
                val12=transpose(resArr[1,*])
                val15=transpose(resArr[2,*])
                val18=transpose(resArr[3,*])
                val21=transpose(resArr[4,*])
                valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Center', valCenter, 'COPY0zPos',zPosMarked, 'at12', val12, 'COPY1zPos',zPosMarked, 'at15', val15, 'COPY2zPos',zPosMarked, 'at18', val18, 'COPY3zPos',zPosMarked, 'at21', val21)
                PLOT, zPosMarked, valCenter, XTITLE='zPos (mm)', YTITLE='Difference (%)' , TITLE='Difference from mean of all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, COLOR=255
                LOADCT, 12, /SILENT
                OPLOT, zPosMarked, val12, COLOR=200;red
                OPLOT, zPosMarked, val15, COLOR=100;blue
                OPLOT, zPosMarked, val18, COLOR=40;green
                OPLOT, zPosMarked, val21, COLOR=90;cyan
                loadct, 0, /SILENT
                OPLOT, rangeX, [5,5], LINESTYLE=1, COLOR=255
                OPLOT, rangeX, [-5,-5], LINESTYLE=1, COLOR=255
                END
              
              ELSE:
            ENDCASE
          ENDIF ELSE TVSCL, INTARR(550,550)
        END
        
        ELSE:
        
      ENDCASE; modes


      IF setRangeMinMaxX THEN BEGIN
        WIDGET_CONTROL, txtMinRangeX, SET_VALUE=STRING(rangeX(0),FORMAT='(f0.'+nDecimals(rangeX(0))+')')
        WIDGET_CONTROL, txtMaxRangeX, SET_VALUE=STRING(rangeX(1),FORMAT='(f0.'+nDecimals(rangeX(1))+')')
      ENDIF
      IF setRangeMinMaxY THEN BEGIN
        WIDGET_CONTROL, txtMinRangeY, SET_VALUE=STRING(rangeY(0),FORMAT='(f0.'+nDecimals(rangeY(0))+')')
        WIDGET_CONTROL, txtMaxRangeY, SET_VALUE=STRING(rangeY(1),FORMAT='(f0.'+nDecimals(rangeY(1))+')')
      ENDIF

      IF copyCurve GE 1 THEN BEGIN
        plotNames=TAG_NAMES(valuesPlot)
        nnNames=N_ELEMENTS(plotNames)
        IF nnNames GT 1 THEN BEGIN
          nVals=INTARR(nnNames)
          copyVals=INTARR(nnNames)
          FOR i=0, nnNames-1 DO BEGIN
            nVals(i)=N_ELEMENTS(valuesPlot.(i))
            IF STRMID(plotNames(i),0,4) EQ 'COPY' THEN copyVals(i)=1
          ENDFOR

          IF copyCurve EQ 1 THEN BEGIN
            valuesArr=STRARR(nnNames-TOTAL(copyVals),MAX(nVals))
            headArr=STRARR(nnNames-TOTAL(copyVals))
            counter=0
            FOR i=0, nnNames-1 DO BEGIN
              IF copyVals(i) NE 1 THEN BEGIN
                tempArr=STRTRIM(STRING(TRANSPOSE(valuesPlot.(i))),1)
                FOREACH elem, tempArr, idx DO tempArr(idx)=STRJOIN(STRSPLIT(elem, '.',/EXTRACT),',')
                valuesArr[counter,0:nVals(i)-1]=tempArr
                headArr(counter)=plotNames(i)
                counter=counter+1
              ENDIF
            ENDFOR

            CLIPBOARD.set, [STRJOIN(headArr, STRING(9B)),STRJOIN(valuesArr, STRING(9B))]
          ENDIF
          IF copyCurve EQ 2 THEN BEGIN
            IF N_TAGS(valuesPlot) GT 1 THEN BEGIN
              counter=0
              FOR i=0, nnNames-1 DO BEGIN
                IF copyVals(i) THEN plotNames(i)=STRMID(plotNames(i),7,STRLEN(plotNames(i)))
                IF i mod 2 THEN BEGIN
                  IF i EQ 1 THEN iPlot, valuesPlot.(i-1), valuesPlot.(i), NAME=plotNames(i), /INSERT_LEGEND $
                  ELSE iPlot, valuesPlot.(i-1), valuesPlot.(i), NAME=plotNames(i), LINESTYLE=counter mod 7, /OVERPLOT, /INSERT_LEGEND
                  counter=counter+1
                ENDIF
              ENDFOR
            ENDIF
          ENDIF
        ENDIF ELSE BEGIN
          IF copyCurve EQ 1 THEN sv=DIALOG_MESSAGE('No values to copy to clipboard.')
          IF copyCurve EQ 2 THEN sv=DIALOG_MESSAGE('No plot values to display.')
        ENDELSE
      ENDIF

      WIDGET_CONTROL, statPlot, SET_VALUE=statTxt

    ENDIF ELSE TVSCL, INTARR(550,550)

  ENDIF ELSE TVSCL, INTARR(550,550);selected image not marked - no results shown

end