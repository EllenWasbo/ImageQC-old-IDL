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

;update Plot and image Results
;setRangeMinMaxX / setRangeMinMaxY = 1:set new default values, 0:keep ranges as already set
;optionSet 0 = nothing extra
;optionSet 1 = copy data to clipboard (paste into Excel or txt)
;optionSet 2 = send plot to separate window with toolbar for editing /saving to file
;optionSet 3 = set min/max only, no plot
pro updatePlot, setRangeMinMaxX, setRangeMinMaxY, optionSet

  COMMON VARI

  WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
  WSET, iDrawImageRes
  TVSCL, INTARR(550,550)

  WIDGET_CONTROL, drawPlot, GET_VALUE = iDrawPlot
  iDrawPlot.SELECT
  
  curMode=WIDGET_INFO(wTabModes, /TAB_CURRENT)
  statTxt='';text to show in plot statistics (left of plot window)
  WIDGET_CONTROL, statPlot, SET_VALUE=statTxt

  IF results(getResNmb(curMode,analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM,analyseStringsPET)) THEN BEGIN

    valuesPlot=CREATE_STRUCT('empty',0); structure of plot values to be copied to clipboard (optionSet = 1) or sent to iPlot (optionSet =2), x/y pairs of vectors, tagname 'COPYXXX...' means that the vector (x values) is not included when optionSet=1

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

      IF optionSet EQ 2 THEN currWin=0 ELSE BEGIN
        currWin=1
        iDrawPlot.ERASE
      ENDELSE
      foName='Arial'
      foSize=8
      resPlotMargin=[0.15,0.1,0.25,0.1]; or [left, bottom, right, top]
      legPos=[1.0,0.9]

      WIDGET_CONTROL, txtMinRangeX, GET_VALUE=rangeX1
      WIDGET_CONTROL, txtMaxRangeX, GET_VALUE=rangeX2
      WIDGET_CONTROL, txtMinRangeY, GET_VALUE=rangeY1
      WIDGET_CONTROL, txtMaxRangeY, GET_VALUE=rangeY2
      rangeX=FLOAT([rangeX1(0),rangeX2(0)])
      rangeY=FLOAT([rangeY1(0),rangeY2(0)])

      CASE curMode OF

        ;********************************CT*************************************************
        0: BEGIN
          curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF

              'HOMOG':BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                resArr=FLOAT(resArr[0:4,*])
                colNo=5
                zPosMarked=getZposMarked(structImgs, markedTemp)

                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[-10,10]

                IF optionSet NE 3 THEN BEGIN
                  val12=transpose(resArr[0,*]-resArr[4,*])
                  val15=transpose(resArr[1,*]-resArr[4,*])
                  val18=transpose(resArr[2,*]-resArr[4,*])
                  val21=transpose(resArr[3,*]-resArr[4,*])
                  ;valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'at12', val12, 'COPY0zPos',zPosMarked, 'at15', val15, 'COPY1zPos',zPosMarked, 'at18', val18, 'COPY2zPos',zPosMarked, 'at21', val21)
                  valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'at12', val12, 'at15', val15, 'at18', val18, 'at21', val21)
                  ;PLOT, zPosMarked, val12, XTITLE='zPos (mm)', YTITLE='Difference from center ROI' , TITLE='Difference from center ROI for all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, COLOR=255, /NODATA
                  ;LOADCT, 12, /SILENT
                  ;OPLOT, zPosMarked, val12, COLOR=200;red
                  ;OPLOT, zPosMarked, val15, COLOR=100;blue
                  ;OPLOT, zPosMarked, val18, COLOR=40;green
                  ;OPLOT, zPosMarked, val21, COLOR=90;cyan
                  ;loadct, 0, /SILENT
                  ;OPLOT, rangeX, [4,4], LINESTYLE=1, COLOR=255
                  ;OPLOT, rangeX, [-4,-4], LINESTYLE=1, COLOR=255
                  resPlot=OBJARR(6)
                  resPlot[0]=PLOT(zPosMarked, val12, '-r', NAME='at 12', XTITLE='zPos (mm)', YTITLE='Difference from center ROI' , TITLE='Difference from center ROI for all (marked) images', $
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  resPlot[0].refresh, /DISABLE
                  resPlot[1]=PLOT(zPosMarked, val15, '-b',  NAME='at 15', /OVERPLOT)
                  resPlot[2]=PLOT(zPosMarked, val18, '-g',  NAME='at 18', /OVERPLOT)
                  resPlot[3]=PLOT(zPosMarked, val21, '-c',  NAME='at 21', /OVERPLOT)
                  resPlot[4]=PLOT(rangeX, [4,4],':', NAME='tolerance 4 HU', /OVERPLOT)
                  resPlot[5]=PLOT(rangeX, [-4,-4],':', /OVERPLOT)
                  resPlotLeg=LEGEND(TARGET=resPlot[0:4], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                  resPlot[0].refresh
                ENDIF
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

                      IF optionSet NE 3 THEN BEGIN
                        szM=size(MTFres.submatrixAll,/DIMENSIONS)
                        nObj=1 & IF N_ELEMENTS(szM) EQ 3 THEN nObj=szM(2)
                        resPlotX=OBJARR(nObj) & resPlotY=OBJARR(nObj)
                        resPlotX[0]=PLOT(MTFres.cdx,MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0], '-', NAME='x', XTITLE='x or y pos (mm)', YTITLE='Pixel value' , TITLE='Centered x and y profile for all (marked) images', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        resPlotX[0].refresh, /DISABLE
                        resPlotY[0]=PLOT(MTFres.cdy,MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0], '--',  NAME='y', /OVERPLOT)
                        IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                          FOR i = 1, szM(2)-1 DO BEGIN
                            resPlotX[i]=PLOT(MTFres.cdx, MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], '-', COLOR=[i*40,i*40+30, i*40+70], /OVERPLOT)
                            resPlotY[i]=PLOT(MTFres.cdy, MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i], '--', COLOR=[i*40,i*40+30, i*40+70], /OVERPLOT)
                            valuesPlot=CREATE_STRUCT(valuesPlot, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], $
                              'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                            ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.cdx, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y', MTFres.cdy,'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                          ENDFOR
                        ENDIF
                        resPlotLeg=LEGEND(TARGET=[resPlotX[0],resPlotY[0]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                        resPlotX[0].refresh

                        ;                      plot, MTFres.cdx,MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0],TITLE='Profile x and y centered', linestyle=0, XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                        ;                      oplot, MTFres.cdy,MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0], linestyle=1
                        ;                      IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                        ;                        LOADCT, 13, /SILENT
                        ;                        FOR i = 1, szM(2)-1 DO BEGIN
                        ;                          oplot, MTFres.cdx, MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], LINESTYLE=0, COLOR=i*40
                        ;                          oplot,  MTFres.cdy, MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i], LINESTYLE=1, COLOR=i*40
                        ;                          valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.cdx, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y', MTFres.cdy,'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                        ;                        ENDFOR
                        ;                        LOADCT, 0, /SILENT
                        ;                      ENDIF
                      ENDIF
                    ENDIF ELSE BEGIN
                      valuesPlot=CREATE_STRUCT('x',MTFres.(sel).cdx, 'Xprof', MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0], 'y',MTFres.(sel).cdy, 'Yprof', MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0])

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).cdx),max(MTFres.(sel).cdx)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).submatrixAll),max(MTFres.(sel).submatrixAll)]

                      IF optionSet NE 3 THEN BEGIN
                        ;szM=size(MTFres.(sel).submatrixAll,/DIMENSIONS)
                        xprof=MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0]
                        yprof=MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0]

                        resPlotX=PLOT(MTFres.(sel).cdx,xprof, '-', NAME='x', XTITLE='x or y pos (mm)', YTITLE='Pixel value' , TITLE='Centered x and y profile', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        resPlotY=PLOT(MTFres.(sel).cdy,yprof, '--',  NAME='y', /OVERPLOT)
                        resPlotLeg=LEGEND(TARGET=[resPlotX[0],resPlotY[0]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)

                        ;                      plot, MTFres.(sel).cdx,xprof,TITLE='Profile x and y centered', XRANGE=rangeX, YRANGE=rangeY, linestyle=0, XSTYLE=1, YSTYLE=1
                        ;                      oplot, MTFres.(sel).cdy,yprof, linestyle=1

                        maxval=max(xprof)
                        minval=min(xprof)
                        resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                        statTxt=['FWHM X: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')]
                        maxval=max(yprof)
                        minval=min(yprof)
                        resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                        statTxt=[[statTxt],['FWHM Y: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')]]
                      ENDIF
                    ENDELSE
                  END
                  1: BEGIN ;sorted pixelvalues, interpolated and smoothed
                    IF v3d THEN BEGIN
                      valuesPlot=CREATE_STRUCT('distance from center', MTFres.newdists,'Interpolated pixel values',MTFres.pixValsInterp,'Smoothed pixelvalues',MTFres.pixValsSmooth)
                      ;valuesPlot=CREATE_STRUCT('distance from center', MTFres.newdists,'Interpolated pixel values',MTFres.pixValsInterp,'COPY000distance from center', MTFres.newdists,'Smoothed pixelvalues',MTFres.pixValsSmooth)

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.distspix0),max(MTFres.distspix0)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.pixValSort),max(MTFres.pixValSort)]

                      IF optionSet NE 3 THEN BEGIN
                        ;plot, MTFres.distspix0, MTFres.pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                        szM=size(MTFres.submatrixAll,/DIMENSIONS)
                        nObj=1 & IF N_ELEMENTS(szM) EQ 3 THEN nObj=szM(2)
                        resPlot=objarr(nObj)
                        resPlot[0]=PLOT(MTFres.distspix0, MTFres.pixValSort, '', SYMBOL='.', NAME='Sorted pixel values', TITLE='Pixel values sorted by distance to center', XTITLE='Distance from center (mm)', YTITLE='Pixel value', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        resPlot[0].refresh, /DISABLE
                        IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                          FOR b=1, szM(2)-1 DO resPlot[b]=PLOT(MTFres.distspix0, MTFres.pixValSort[*,b], '', SYMBOL='.', /OVERPLOT)
                        ENDIF
                        plotInterp=PLOT(MTFres.newdists, MTFres.pixValsInterp, '-r', NAME='Interpolated', /OVERPLOT)
                        plotSmooth=PLOT(MTFres.newdists, MTFres.pixValsSmooth, '-b', NAME='Smoothed', /OVERPLOT)
                        resLeg=LEGEND(TARGET=[resPlot[0],plotInterp,plotSmooth], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                        resPlot[0].refresh

                        ;LOADCT, 13, /SILENT
                        ;oplot, MTFres.newdists, MTFres.pixValsInterp, COLOR=255
                        ;oplot, MTFres.newdists, MTFres.pixValsSmooth, COLOR=150
                        ;LOADCT, 0, /SILENT
                      ENDIF

                    ENDIF ELSE BEGIN
                      t=TEXT(0.1, 0.2,'Sorted pixelvalues not available for 2d bead method.')
                    ENDELSE

                  END
                  2: BEGIN; LSF
                    IF v3d EQ 0 THEN BEGIN
                      IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
                        valuesPlot=CREATE_STRUCT('xpos mm',MTFres.(sel).dx, 'LSFx',MTFres.(sel).LSFx)

                        IF setRangeMinMaxX THEN rangeX=[min([MTFres.(sel).dx,MTFres.(sel).dy]),max([MTFres.(sel).dx,MTFres.(sel).dy])]
                        IF setRangeMinMaxY THEN rangeY=[min([MTFres.(sel).LSFx,MTFres.(sel).LSFy]),max([MTFres.(sel).LSFx,MTFres.(sel).LSFy])]

                        IF optionSet NE 3 THEN BEGIN
                          tagMTFres=tag_names(MTFres.(sel))
                          resPlot=objarr(6)
                          IF tagMTFres.hasValue('FITLSFX') THEN BEGIN
                            resPlot[0]=PLOT(MTFres.(sel).dx,MTFres.(sel).fitLSFx,'-', NAME='Gaussian fit LSF x', XTITLE='position (mm)', TITLE='Line Spread Function',$
                              XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                            ;PLOT, MTFres.(sel).dx,MTFres.(sel).fitLSFx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                            ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY001xpos mm',MTFres.(sel).dx, 'Fit to smoothed LSFx', MTFres.(sel).fitLSFx)
                            valuesPlot=CREATE_STRUCT(valuesPlot, 'Gaussian fit LSFx', MTFres.(sel).fitLSFx)
                          ENDIF ELSE BEGIN
                            resPlot[0]=PLOT(MTFres.(sel).dx,MTFres.(sel).dx,'-', XTITLE='position (mm)', TITLE='Line Spread Function', /NODATA,$
                              XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                            ;PLOT, MTFres.(sel).dx,MTFres.(sel).dx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                          ENDELSE
                          resPlot[0].refresh, /DISABLE
                          ;LOADCT, 13, /SILENT
                          ;IF tagMTFres.hasValue('SMLSFX') THEN BEGIN ;smFac set to 0 - smoothing not active
                          ;oplot, MTFres.(sel).dx, MTFres.(sel).smLSFx, COLOR=150
                          ;  resPlot[1]=PLOT(MTFres.(sel).dx, MTFres.(sel).smLSFx, '-b', NAME='Smoothed LSF x', /OVERPLOT)
                          ;  valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY002xpos mm', MTFres.(sel).dx, 'Smoothed LSFx', MTFres.(sel).smLSFx)
                          ;ENDIF
                          ;LOADCT, 0, /SILENT
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'ypos mm', MTFres.(sel).dy, 'LSFy', MTFres.(sel).LSFy)
                          IF tagMTFres.hasValue('FITLSFY') THEN BEGIN
                            resPlot[2]=PLOT(MTFres.(sel).dy,MTFres.(sel).fitLSFy, '--', NAME='Gaussian fit LSF y', /OVERPLOT)
                            ;OPLOT, MTFres.(sel).dy,MTFres.(sel).fitLSFy,linestyle=1
                            ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY001ypos mm',MTFres.(sel).dy, 'Fit to smoothed LSFy', MTFres.(sel).fitLSFy)
                            valuesPlot=CREATE_STRUCT(valuesPlot, 'Gaussian fit LSFy', MTFres.(sel).fitLSFy)
                          ENDIF
                          ;LOADCT, 13, /SILENT
                          ;IF tagMTFres.hasValue('SMLSFY') THEN BEGIN;smFac set to 0 - smoothing not active
                          ;  resPlot[3]=PLOT(MTFres.(sel).dy, MTFres.(sel).smLSFy, '--b', NAME='Smoothed LSF y', /OVERPLOT)
                          ;oplot, MTFres.(sel).dy, MTFres.(sel).smLSFy, COLOR=150, linestyle=1
                          ; valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY002ypos mm',MTFres.(sel).dy, 'Smoothed LSFy', MTFres.(sel).smLSFy)
                          ;ENDIF
                          resPlot[4]=PLOT(MTFres.(sel).dx, MTFres.(sel).LSFx, '-r', NAME='LSF x', /OVERPLOT)
                          resPlot[5]=PLOT(MTFres.(sel).dy, MTFres.(sel).LSFy, '--r', NAME='LSF y', /OVERPLOT)
                          resLeg=LEGEND(TARGET=[resPlot[4:5],resPlot[0],resPlot[2]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                          resPlot[0].refresh
                          ;oplot, MTFres.(sel).dx, MTFres.(sel).LSFx, COLOR=255
                          ;oplot, MTFres.(sel).dy, MTFres.(sel).LSFy, COLOR=255, linestyle=1
                          ;LOADCT, 0, /SILENT
                        ENDIF
                      ENDIF ELSE iDrawPlot.Erase
                    ENDIF ELSE BEGIN
                      valuesPlot=CREATE_STRUCT('pos mm', MTFres.dx, 'LSF', MTFres.LSFx)
                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.dx),max(MTFres.dx)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.LSFx),max(MTFres.LSFx)]

                      IF optionSet NE 3 THEN BEGIN
                        tagMTFres=tag_names(MTFres)
                        resPlot=objarr(3)
                        IF tagMTFres.hasValue('FITLSFX') THEN BEGIN
                          resPlot[0]=PLOT(MTFres.dx,MTFres.fitLSFx, '-', NAME='Fitted gaussian',XTITLE='position (mm)',TITLE='Line Spread Function',$
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          ;PLOT, MTFres.dx,MTFres.fitLSFx, linestyle=0, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                          ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY001pos mm',MTFres.dx, 'Fit to smoothed LSF', MTFres.fitLSFx)
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'Fit to smoothed LSF', MTFres.fitLSFx)
                        ENDIF ELSE BEGIN
                          resPlot[0]=PLOT(MTFres.dx,MTFres.dx, XTITLE='position (mm)',TITLE='Line Spread Function', /NODATA, $
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          ;PLOT, MTFres.dx,MTFres.dx, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                        ENDELSE
                        ;LOADCT, 13, /SILENT
                        IF tagMTFres.hasValue('SMLSFX') THEN BEGIN
                          resPlot[1]=PLOT( MTFres.dx,MTFres.smLSFx, '-b', NAME='Smoothed LSF', /OVERPLOT)
                          ;OPLOT, MTFres.dx,MTFres.smLSFx, COLOR=150, linestyle=0
                          ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY002pos mm',MTFres.dx, 'Smoothed LSF', MTFres.smLSFx)
                          valuesPlot=CREATE_STRUCT(valuesPlot, 'Smoothed LSF', MTFres.smLSFx)
                        ENDIF
                        resPlot[2]=PLOT( MTFres.dx,MTFres.LSFx, '-r', NAME='Interpolated LSF', /OVERPLOT)
                        resLeg=LEGEND(TARGET=[resPlot[2],resPlot[1],resPlot[0]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                        ;OPLOT, MTFres.dx,MTFres.LSFx, COLOR=255, linestyle=0
                        ;LOADCT, 0, /SILENT
                      ENDIF
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

                        IF optionSet NE 3 THEN BEGIN
                          resPlot=objarr(11)
                          resPlot[0]=PLOT(MTFres.(sel).fx*mm2cm,MTFres.(sel).MTFx, '-r', NAME='Discrete MTF x', XTITLE='frequency (mm-1)',YTITLE='MTF', TITLE='Modulation Transfer Function',$
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          resPlot[0].refresh, /DISABLE
                          resPlot[1]=PLOT(MTFres.(sel).fy*mm2cm,MTFres.(sel).MTFy,'--r', NAME='Discrete MTF y', /OVERPLOT)
                          ;PLOT, MTFres.(sel).fx*mm2cm,MTFres.(sel).MTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='MTF gaussian (red) and discrete (white) x and y (dotted)', XSTYLE=1, YSTYLE=1
                          ;OPLOT, MTFres.(sel).fy*mm2cm,MTFres.(sel).MTFy,linestyle=1
                          ;loadct, 13, /SILENT
                          nleg=1
                          IF tagMTFres.hasValue('GFX') THEN BEGIN
                            resPlot[2]=PLOT(MTFres.(sel).gfx*mm2cm,MTFres.(sel).gMTFx, '-', NAME='Gaussian MTF x', /OVERPLOT);OPLOT, MTFres.(sel).gfx*mm2cm,MTFres.(sel).gMTFx,linestyle=0
                            nleg=nleg+1
                          ENDIF
                          IF tagMTFres.hasValue('GFY') THEN BEGIN
                            resPlot[3]=PLOT(MTFres.(sel).gfy*mm2cm,MTFres.(sel).gMTFy, '--', NAME='Gaussian MTF y', /OVERPLOT);OPLOT, MTFres.(sel).gfy*mm2cm,MTFres.(sel).gMTFy,linestyle=1
                            nleg=nleg+1
                          ENDIF
                          legPlot=LEGEND(TARGET=resPlot[0:nleg], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                          resPlot[4]=PLOT([0,MTFres.(sel).F50_10_2(0)*mm2cm],[.5,.5],'-r',/OVERPLOT)
                          resPlot[5]=PLOT([0,MTFres.(sel).F50_10_2(1)*mm2cm],[.1,.1],'-r',/OVERPLOT)
                          resPlot[6]=PLOT([0,MTFres.(sel).F50_10_2(2)*mm2cm],[.02,.02],'-r',/OVERPLOT)
                          resPlot[7]=PLOT([0,MTFres.(sel).F50_10_2(3)*mm2cm],[.5,.5],'--r',/OVERPLOT)
                          resPlot[8]=PLOT([0,MTFres.(sel).F50_10_2(4)*mm2cm],[.1,.1],'--r',/OVERPLOT)
                          resPlot[9]=PLOT([0,MTFres.(sel).F50_10_2(5)*mm2cm],[.02,.02],'--r',/OVERPLOT)
                          resPlot[10]=PLOT([nyqfr,nyqfr]*mm2cm,[0,1],'-',/OVERPLOT)
                          nqTxt=TEXT(nyqfr*.95,.5,'NQf',/DATA)
                          resPlot[0].refresh
                          ;                        OPLOT, [0,MTFres.(sel).F50_10_2(0)*mm2cm],[.5,.5],linestyle=0
                          ;                        OPLOT, [0,MTFres.(sel).F50_10_2(1)*mm2cm],[.1,.1],linestyle=0
                          ;                        OPLOT, [0,MTFres.(sel).F50_10_2(2)*mm2cm],[.02,.02],linestyle=0
                          ;                        OPLOT, [0,MTFres.(sel).F50_10_2(3)*mm2cm],[.5,.5],linestyle=1
                          ;                        OPLOT, [0,MTFres.(sel).F50_10_2(4)*mm2cm],[.1,.1],linestyle=1
                          ;                        OPLOT, [0,MTFres.(sel).F50_10_2(5)*mm2cm],[.02,.02],linestyle=1
                          ;                        loadct, 0, /SILENT
                          ;                        OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0
                        ENDIF
                      ENDIF ELSE iDrawPlot.erase
                    ENDIF ELSE BEGIN

                      tagMTFres=tag_names(MTFres)
                      valuesPlot=CREATE_STRUCT('discrete frequency mm_1',MTFres.fx*mm2cm, 'discrete MTF',MTFres.MTFx)
                      IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian frequency mm_1',MTFres.gfx*mm2cm,'gaussian MTF',MTFres.gMTFx)

                      IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]*mm2cm
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.MTFx),max(MTFres.MTFx)]
                      IF optionSet NE 3 THEN BEGIN
                        resPlot=objarr(7)
                        resPlot[0]=PLOT(MTFres.fx*mm2cm,MTFres.MTFx,XTITLE='frequency (mm-1)',YTITLE='MTF',TITLE='Modulation Transfer Function',/NODATA,$
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        ;PLOT, MTFres.fx*mm2cm,MTFres.MTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, TITLE='MTF gaussian and discrete (dotted)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                        nLeg=1
                        resPlot[0].refresh, /DISABLE
                        IF tagMTFres.hasValue('GFX') THEN BEGIN
                          ;OPLOT, MTFres.gfx*mm2cm,MTFres.gMTFx, linestyle=0
                          resPlot[2]=PLOT(MTFres.gfx*mm2cm,MTFres.gMTFx, '-',NAME='Gaussian MTF',/OVERPLOT)
                          nLeg=2
                        ENDIF
                        resPlot[1]=PLOT(MTFres.fx*mm2cm, MTFres.MTFx, '-r',NAME='Discrete MTF',/OVERPLOT)
                        legPlot=LEGEND(TARGET=resPlot[1:nleg], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                        resPlot[3]=PLOT([0,MTFres.F50_10_2(0)*mm2cm],[.5,.5],'-',/OVERPLOT)
                        resPlot[4]=PLOT([0,MTFres.F50_10_2(1)*mm2cm],[.1,.1],'-',/OVERPLOT)
                        resPlot[5]=PLOT([0,MTFres.F50_10_2(2)*mm2cm],[.02,.02],'-',/OVERPLOT)
                        resPlot[6]=PLOT([nyqfr,nyqfr]*mm2cm,[0,1],'-',/OVERPLOT)
                        nqTxt=TEXT(nyqfr*.95,.5,'NQf',/DATA)
                        resPlot[0].refresh
                        ;                      OPLOT, MTFres.fx*mm2cm, MTFres.MTFx, LINESTYLE=1
                        ;                      OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0
                        ;                      OPLOT, [0,MTFres.F50_10_2(0)*mm2cm],[.5,.5],linestyle=0
                        ;                      OPLOT, [0,MTFres.F50_10_2(1)*mm2cm],[.1,.1],linestyle=0
                        ;                      OPLOT, [0,MTFres.F50_10_2(2)*mm2cm],[.02,.02],linestyle=0
                      ENDIF
                    ENDELSE
                  END
                  ELSE: iDrawPlot.erase
                ENDCASE
              END

              'NPS': BEGIN
                IF N_TAGS(NPSres.(sel)) GT 1 THEN BEGIN
                  v3d=WIDGET_INFO(btnNPSavg, /BUTTON_SET)
                  mm2cm=1.;1. means no converstion to cm, 10. when convert to cm
                  IF v3d EQ 0 THEN BEGIN
                    IF setRangeMinMaxX THEN rangeX=[0,max(NPSres.(sel).dr)]
                    IF setRangeMinMaxY THEN rangeY=[0,max(NPSres.(sel).rNPS)]
                    IF optionSet NE 3 THEN BEGIN
                      resPlot=PLOT(NPSres.(sel).dr,NPSres.(sel).rNPS, '-', XTITLE='spatial frequency (mm-1)',YTITLE='NPS', TITLE='Noise Power Spectrum', $
                        XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                      ;PLOT, NPSres.(sel).dr,NPSres.(sel).rNPS, XTITLE='spatial frequency (mm-1)',YTITLE='NPS',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='NPS', XSTYLE=1, YSTYLE=1
                      valuesPlot=CREATE_STRUCT('frequency mm_1',NPSres.(sel).dr,'NPS',NPSres.(sel).rNPS)
                      statTxt=[['AUC: '+STRING(NPSres.(sel).AUC, FORMAT=formatCode(NPSres.(sel).AUC))],$
                        ['Integral NPS: '+STRING(NPSres.(sel).varianceIntNPS, FORMAT=formatCode(NPSres.(sel).varianceIntNPS))],$
                        ['Avg var ROIs: '+STRING(NPSres.(sel).varianceImg^2, FORMAT=formatCode(NPSres.(sel).varianceImg^2))]]
                    ENDIF
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
                    IF optionSet NE 3 THEN BEGIN
                      valuesPlot=CREATE_STRUCT('f',ftemp, 'NPStot', NPStot)
                      resPlot=PLOT(NPSres.(sel).dr,NPStot, '-', XTITLE='spatial frequency (mm-1)',YTITLE='NPS', TITLE='Noise Power Spectrum', /NODATA,$
                        XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                      resPlot.refresh, /DISABLE
                      ;plot,NPSres.(sel).dr,NPStot,XTITLE='spatial frequency (mm-1)',YTITLE='NPS',TITLE='NPS for each image and average', linestyle=0, THICK=2, XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                      AUC=INT_TABULATED(NPSres.(sel).dr,NPStot)
                      statTxt=[['AUC for average: '+STRING(AUC, FORMAT=formatCode(AUC))],$
                        ['Integral NPS: '+STRING(NPSres.(sel).varianceIntNPS, FORMAT=formatCode(NPSres.(sel).varianceIntNPS))],$
                        ['Avg variance ROIs: '+STRING(NPSres.(sel).varianceImg^2, FORMAT=formatCode(NPSres.(sel).varianceImg^2))]]
                      IF TOTAL(markedArr) GT 1 THEN BEGIN
                        ;LOADCT, 13, /SILENT
                        resPlotImg=objarr(nImg)
                        FOR i=0, nImg-1 DO BEGIN
                          IF markedArr(i) THEN BEGIN
                            resPlotImg[i]=PLOT(NPSres.(i).dr, NPSres.(i).rNPS, '-', NAME='Image '+STRING(i, FORMAT='(i0)'), COLOR=[i*10,i*50,i*100],/OVERPLOT)
                            ;oplot, NPSres.(i).dr, NPSres.(i).rNPS, LINESTYLE=0, COLOR=i*40
                            ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'f',NPSres.(i).dr, 'NPS'+STRING(i,FORMAT='(i0)'), NPSres.(i).rNPS)
                            valuesPlot=CREATE_STRUCT(valuesPlot, 'NPS'+STRING(i,FORMAT='(i0)'), NPSres.(i).rNPS)
                          ENDIF
                        ENDFOR
                        ;LOADCT, 0, /SILENT
                      ENDIF
                      resPlotAvg=PLOT(NPSres.(sel).dr,NPStot,'-2',NAME='NPS average',/OVERPLOT);oplot, NPSres.(sel).dr,NPStot,linestyle=0, THICK=2
                      IF TOTAL(markedArr) GT 1 THEN resLeg=LEGEND(TARGET=[resPlotImg,resPlotAvg], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos) $
                      ELSE resLeg=LEGEND(TARGET=resPlotAvg, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                      resPlot.refresh
                    ENDIF
                  ENDELSE
                  IF optionSet NE 3 THEN BEGIN
                    nqPlot=PLOT([nyqfr,nyqfr]*mm2cm,[0,rangeY(1)],'-',/OVERPLOT)
                    nqTxt=TEXT(nyqfr*.95*mm2cm,rangeY(1)/2,'NQf',/DATA)
                    ;OPLOT, [nyqfr,nyqfr],[0,1],linestyle=1,thick=1.
                  ENDIF
                  WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
                  WSET, iDrawImageRes
                  activeResImg=NPSres.(sel).NPS
                  tvRes=activeResImg
                  szNPS=SIZE(activeResImg, /DIMENSIONS)
                  szX=450
                  szY=ROUND(szX*(szNPS(1)*1./szNPS(0)))
                  TVSCL,congrid(adjustWindowLevel(tvRes, [0,100*max(NPSres.(sel).NPS)]), szX, szY, /INTERP)

                ENDIF ELSE iDrawPlot.erase;TVSCL, INTARR(550,550)
              END

              'CTLIN': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                szTab=SIZE(resArr, /DIMENSIONS)
                IF setRangeMinMaxX THEN rangeX=1.1*[min(FLOAT(resArr[*,rowNo])),max(FLOAT(resArr[*,rowNo]))]
                WIDGET_CONTROL, tblLin, GET_VALUE=linTable
                densit=TRANSPOSE(FLOAT(linTable[3,*]))
                IF setRangeMinMaxY THEN rangeY=[min(densit),max(densit)*1.1]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=PLOT(FLOAT(resArr[0:szTab(0)-1,rowNo]), densit, '', SYMBOL='D', NAME='Mean CT numbers', XTITLE='CT number (HU)', YTITLE='Density' , TITLE='CT linearity for selected image',$
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  ;PLOT, FLOAT(resArr[0:szTab(0)-1,rowNo]), densit, PSYM=4,  XTITLE='CT number (HU)', YTITLE='Density' , TITLE='CT linearity for first image', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=2, YSTYLE=2
                  a0=REGRESS(resArr[0:szTab(0)-1,rowNo],densit, YFIT=yfit, MCORRELATION=mcorr)
                  resPlotFit=PLOT(FLOAT(resArr[0:szTab(0)-1,rowNo]), TRANSPOSE(yfit),'-', NAME='Linear fit, mCorr= '+STRING(mcorr, FORMAT='(f0.4)'), /OVERPLOT)
                  resLeg=LEGEND(TARGET=resPlotFit, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                  ;OPLOT, resArr[0:szTab(0)-1,rowNo], yfit, LINESTYLE=0
                ENDIF
              END

              'SLICETHICK': BEGIN
                tabSel=WIDGET_INFO(resTab,/TABLE_SELECT)
                colNo=tabSel(0)
                ;loadct, 12, /SILENT
                ;colors=[200,50,100,255,130,80]
                colors=255*[[0,0,0],[0,1,0],[0,0,1],[1,0,0],[1,0,1],[0,1,1]];k,g,b,1.50,101,011
                WIDGET_CONTROL, cw_ramptype, GET_VALUE=ramptype

                first=0
                IF ramptype EQ 0 THEN last=3 ELSE last=5
                IF colNo GT 0 AND ( colNo LT 5 AND ramptype EQ 0 ) THEN BEGIN
                  first=colNo-1
                  last=first
                ENDIF

                lines=['H1','H2','V1','V2','iV1','iV2']

                vect=sliceThickRes.(sel).(first).vector
                IF setRangeMinMaxX THEN rangeX=[0,N_ELEMENTS(vect)]
                IF setRangeMinMaxY THEN rangeY=[min(vect),max(vect)*1.1]
                IF optionSet NE 3 THEN BEGIN

                  FOR line=first,last DO BEGIN
                    vect=sliceThickRes.(sel).(line).vector
                    nPixVec=N_ELEMENTS(vect)
                    IF line EQ first THEN valuesPlot=CREATE_STRUCT('pix',INDGEN(nPixVec),lines(line), vect) ELSE valuesPlot=CREATE_STRUCT(valuesPlot, lines(line), vect)
                    ;valuesPlot=CREATE_STRUCT('pix',INDGEN(nPixVec),lines(line), vect) ELSE valuesPlot=CREATE_STRUCT(valuesPlot,'COPY'+STRING(line,FORMAT='(i03)')+'pix',INDGEN(nPixVec), lines(line), vect)

                    backGround=sliceThickRes.(sel).(line).background
                    lenVec=N_ELEMENTS(vect)
                    halfMax=sliceThickRes.(sel).(line).halfMax
                    nPixBackG=sliceThickRes.(sel).(line).nBackGr

                    IF line EQ first THEN BEGIN
                      resPlot=objarr(6)
                      resPlot[0]=PLOT(vect, TITLE='Vectors to find slice thickness', YTITLE='CT value (HU)', XTITLE='n pix', /NODATA,$
                        XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                      ;PLOT, vect,TITLE='Vectors to find slice thickness', YTITLE='CT value (HU)', XTITLE='n pix', COLOR=255, LINESTYLE=0, XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                    ENDIF
                    resPlot[1]=PLOT(vect, '-', NAME='Profile', COLOR=colors[*,line], /OVERPLOT)
                    resPlot[2]=PLOT(INDGEN(nPixBackG), FLTARR(nPixBackG)+backGround, '-2', NAME='Background', COLOR=colors[*,line], /OVERPLOT)
                    resPlot[3]=PLOT(INDGEN(nPixBackG)+lenVec-nPixBackG, FLTARR(nPixBackG)+backGround,'-2', COLOR=colors[*,line], /OVERPLOT)
                    resPlot[4]=PLOT(sliceThickRes.(sel).(line).firstLast,[halfMax,halfMax], '-.', NAME='Half Max', COLOR=colors[*,line], /OVERPLOT)
                    resPlot[5]=PLOT([0,lenVec],[sliceThickRes.(sel).(line).maxVal,sliceThickRes.(sel).(line).maxVal], ':', NAME='Max', COLOR=colors[*,line], /OVERPLOT)
                    IF line EQ first THEN resLeg=LEGEND(TARGET=[resPlot[1:2],resPlot[4:5]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                    ;                  OPLOT, vect, COLOR=colors(line), LINESTYLE=0
                    ;                  OPLOT, INDGEN(nPixBackG), FLTARR(nPixBackG)+backGround, COLOR=colors(line), LINESTYLE=1
                    ;                  OPLOT, INDGEN(nPixBackG)+lenVec-nPixBackG, FLTARR(nPixBackG)+backGround, COLOR=colors(line), LINESTYLE=1
                    ;                  OPLOT, sliceThickRes.(sel).(line).firstLast,[halfMax,halfMax], COLOR=colors(line), LINESTYLE=1
                    ;                  OPLOT, [0,lenVec],[sliceThickRes.(sel).(line).maxVal,sliceThickRes.(sel).(line).maxVal], COLOR=colors(line), LINESTYLE=1
                  ENDFOR
                  ;loadct, 0, /SILENT
                ENDIF
              END

              'NOISE': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                zPosMarked=getZposMarked(structImgs, markedTemp)
                yValues = FLOAT(resArr[1,*])
                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[min(yValues)*.95,max(yValues)*1.05]
                IF optionSet NE 3 THEN BEGIN
                  ;PLOT, zPosMarked, yValues,  XTITLE='zPos (mm)', YTITLE='Noise = stdev (HU)' , TITLE='Noise for all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1

                  resPlot=plot(zPosMarked, yValues, XTITLE='zPos (mm)', YTITLE='Noise = stdev (HU)' , TITLE='Noise for all (marked) images', $
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Noise', yValues)
                ENDIF
              END

              ELSE:iDrawPlot.erase
            ENDCASE


          ENDIF ELSE iDrawPlot.erase
        END

        ;- -------------------- Xray ----------------------------
        1:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF

              'STP': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                xValues = FLOAT(resArr[1,*])
                yValues = FLOAT(resArr[2,*])
                IF setRangeMinMaxX THEN rangeX=[0, max(xValues)*1.1]
                IF setRangeMinMaxY THEN rangeY=[0,max(yValues)*1.1]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=plot(xValues, yValues,  '', SYMBOL='D', XTITLE='Flux', YTITLE='Pixel Value' , TITLE='Signal Transfer Properties', $
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  ;PLOT, xValues, yValues,  XTITLE='Dose', YTITLE='Pixel Value' , TITLE='Signal Transfer Properties', XRANGE=rangeX, YRANGE=rangeY, PSYM=2, XSTYLE=1, YSTYLE=1
                  tagn=TAG_NAMES(stpRes)
                  ain=WHERE(tagn EQ 'A')
                  IF ain(0) NE -1 THEN BEGIN
                    yfit=xValues*stpRes.a(0)+stpRes.b
                    resPlotFit=PLOT(xValues,yfit, '-', /OVERPLOT)
                    ;OPLOT, xValues,yfit, LINESTYLE=1
                    statTxt=[$
                      ['slope  = '+STRING(stpRes.a(0), FORMAT=formatCode(stpRes.a(0)))],$
                      ['offset = '+STRING(stpRes.b, FORMAT=formatCode(stpRes.b))],$
                      ['mcorr  = '+STRING(stpRes.mcorr, FORMAT=formatCode(stpRes.mcorr))]]
                  ENDIF
                ENDIF
              END

              'HOMOG':t=TEXT(0.1, 0.2,'No plot available for this test')

              'NOISE': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                tags=TAG_NAMES(structImgs)
                nImg=N_ELEMENTS(tags)
                imgNo=markedTemp
                yValues = FLOAT(resArr[1,*])
                IF setRangeMinMaxX THEN rangeX=[min(imgNo),max(imgNo)+1]
                IF setRangeMinMaxY THEN rangeY=[min(yValues)*.95,max(yValues)*1.05]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=barplot(imgNo, yValues, XTITLE='imgNo', YTITLE='Noise = stdev' , TITLE='Noise for all images', $
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  ;PLOT, imgNo, yValues,  XTITLE='imgNo', YTITLE='Noise = stdev' , TITLE='Noise for all images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                  valuesPlot=CREATE_STRUCT('image number', imgNo, 'Noise', yValues)
                ENDIF
              END

              'MTF': BEGIN

                IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
                  WIDGET_CONTROL, cw_plotMTFX, GET_VALUE= plotWhich
                  WIDGET_CONTROL, cw_formLSFX, GET_VALUE=formLSF

                  CASE plotWhich OF

                    0: BEGIN ;edge
                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).edgePos),max(MTFres.(sel).edgePos)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).edgeRow),max(MTFres.(sel).edgeRow)]
                      IF optionSet NE 3 THEN BEGIN
                        resPlot=plot(MTFres.(sel).edgePos,  MTFres.(sel).edgeRow, '', SYMBOL='.',  XTITLE='Position of edge (pix i ROI)', YTITLE='Profile number', TITLE='Edgeposition for each row in ROI with interpolation', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=2, YSTYLE=2, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        resPlotFit=PLOT(MTFres.(sel).edgeFitx, MTFres.(sel).edgeFity,'-', /OVERPLOT)
                        ;plot, MTFres.(sel).edgePos,  MTFres.(sel).edgeRow, PSYM=3, XTITLE='Position of edge (pix i ROI)', YTITLE='Profile number', TITLE='Edgeposition for each row in ROI with interpolation', XSTYLE=1, YSTYLE=1
                        ;oplot,  MTFres.(sel).edgeFitx, MTFres.(sel).edgeFity
                        statTxt=['Angle = '+STRING(MTFres.(sel).angle,FORMAT='(f0.2)')]
                        valuesPlot=CREATE_STRUCT('position pix', MTFres.(sel).edgePos, 'Row or column number ', MTFres.(sel).edgeRow, 'fitted position pix', MTFres.(sel).edgeFitx, 'fitted row_col', MTFres.(sel).edgeFity)
                      ENDIF
                    END
                    1: BEGIN ;sorted pixelvalues, interpolated (and smoothed)

                      valuesPlot=CREATE_STRUCT('distance from edge', MTFres.(sel).newdists, 'Interpolated pixel values', MTFres.(sel).pixValsInterp, 'Smoothed pixelvalues',MTFres.(sel).pixValsSmooth)
                      ;valuesPlot=CREATE_STRUCT('distance from edge', MTFres.(sel).newdists, 'Interpolated pixel values', MTFres.(sel).pixValsInterp, 'COPY000distance from edge', MTFres.(sel).newdists,'Smoothed pixelvalues',MTFres.(sel).pixValsSmooth)

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).distspix0),max(MTFres.(sel).distspix0)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).pixValSort),max(MTFres.(sel).pixValSort)]
                      IF optionSet NE 3 THEN BEGIN
                        resPlot=plot(MTFres.(sel).distspix0, MTFres.(sel).pixValSort, '', SYMBOL='.', NAME='Pixel values', XTITLE='Distance from edge (mm)', YTITLE='Pixel value', TITLE='Sorted pixel values', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        resPlotInterp=PLOT(MTFres.(sel).newdists, MTFres.(sel).pixValsInterp,'-r', NAME='Interpolated', /OVERPLOT)
                        resPlotSmooth=PLOT(MTFres.(sel).newdists, MTFres.(sel).pixValsSmooth,'-b', NAME='Smoothed', /OVERPLOT)
                        resLeg=LEGEND(TARGET=[resPlot,resPlotInterp,resPlotSmooth], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                        ;                      plot, MTFres.(sel).distspix0, MTFres.(sel).pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                        ;                      LOADCT, 13, /SILENT
                        ;                      oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsInterp, COLOR=255
                        ;                      oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsSmooth, COLOR=150
                        ;                      LOADCT, 0, /SILENT
                      ENDIF
                    END
                    2: BEGIN; LSF

                      valuesPlot=CREATE_STRUCT('pos mm',MTFres.(sel).newdists, 'LSF', MTFres.(sel).dLSF)
                      IF setRangeMinMaxX THEN BEGIN
                        rangeX=[min(MTFres.(sel).newdists),max(MTFres.(sel).newdists)]
                        IF TOTAL(MTFres.(sel).dLSF[0:10]) EQ 0 AND TOTAL(MTFres.(sel).dLSF[-10:-1]) EQ 0 THEN BEGIN
                          nonZero=WHERE(MTFres.(sel).dLSF NE 0)
                          rangeX=1.1*[MTFres.(sel).newdists(nonZero(0)),MTFres.(sel).newdists(nonZero(-1))]
                        ENDIF
                      ENDIF
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).dLSF),max(MTFres.(sel).dLSF)]
                      IF optionSet NE 3 THEN BEGIN
                        ;                      CASE formLSF OF
                        ;                        0: tit='LSF for discrete MTF (red), with fitted exponetial (white)'
                        ;                        1: tit='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)'
                        ;                        2: tit='LSF for discrete MTF'
                        ;                        ELSE:
                        ;                      ENDCASE
                        resPlot=objarr(3)
                        resPlot[0]=plot(MTFres.(sel).newdists,MTFres.(sel).dLSF, '-r', NAME='LSF from interpolated values', XTITLE='position (mm)',TITLE='Line Spread Function',$
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        ;                      PLOT, MTFres.(sel).newdists,MTFres.(sel).dLSF, XTITLE='position (mm)',TITLE=tit, XRANGE=rangeX, YRANGE=rangeY, /NODATA, XSTYLE=1, YSTYLE=1
                        ;                      LOADCT, 13, /SILENT
                        ;                      OPLOT, MTFres.(sel).newdists,MTFres.(sel).dLSF, COLOR=255, linestyle=0
                        tar=resPlot(0)
                        IF formLSF EQ 1 THEN BEGIN
                          resPlot[2]=PLOT(MTFres.(sel).d,MTFres.(sel).smLSF, '-b', NAME='LSF from smoothed values', /OVERPLOT)
                          ;OPLOT, MTFres.(sel).d,MTFres.(sel).smLSF, COLOR=150, linestyle=0
                          valuesPlot=CREATE_STRUCT(valuesPlot,'pos mm smoothed', MTFres.(sel).d, 'smoothed LSF', MTFres.(sel).smLSF)
                          tar=[tar, resPlot(2)]
                        ENDIF
                        ;LOADCT, 0, /SILENT
                        IF formLSF NE 2 THEN BEGIN
                          resPlot[1]=PLOT(MTFres.(sel).d,MTFres.(sel).gLSF,'-',NAME='LSF fit to smoothed',/OVERPLOT)
                          ;OPLOT, MTFres.(sel).d,MTFres.(sel).gLSF, linestyle=0
                          valuesPlot=CREATE_STRUCT(valuesPlot,'pos mm fitted', MTFres.(sel).d, 'fitted LSF', MTFres.(sel).gLSF)
                          tar=[tar, resPlot(1)]
                        ENDIF
                        resLeg=LEGEND(TARGET=tar, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                      ENDIF
                    END
                    3: BEGIN ;MTF

                      tagMTFres=tag_names(MTFres.(sel))
                      IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]
                      IF setRangeMinMaxY THEN rangeY=[0,max([MTFres.(sel).dMTF])]
                      IF optionSet NE 3 THEN BEGIN
                        CASE formLSF OF
                          0: textStr='exponetial'
                          1: textStr='gaussian'
                          2: textStr=''
                          ELSE:
                        ENDCASE
                        IF formLSF EQ 2 THEN BEGIN
                          valuesPlot=CREATE_STRUCT('frequency mm_1',MTFres.(sel).dx,'MTF',MTFres.(sel).dMTF)
                          ;tit=''
                        ENDIF ELSE BEGIN
                          valuesPlot=CREATE_STRUCT('discrete du mm_1',MTFres.(sel).dx,'discrete MTF',MTFres.(sel).dMTF)
                          IF tagMTFres.HasValue('GMTF') THEN valuesPlot=CREATE_STRUCT(valuesPlot, textStr+' du mm_1',MTFres.(sel).gx,textStr+' MTF',MTFres.(sel).gMTF)
                          ;tit='MTF '+textStr+' (red) and discrete (white)'
                        ENDELSE

                        resPlot=objarr(10)
                        resPlot[0]=PLOT(MTFres.(sel).dx,MTFres.(sel).dMTF, '-r', NAME='MTF from interpolated', XTITLE='frequency (mm-1)',YTITLE='MTF', TITLE='Modulation Transfer Function', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        ;PLOT, MTFres.(sel).dx,MTFres.(sel).dMTF, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE=tit, XSTYLE=1, YSTYLE=1

                        IF formLSF NE 2 AND tagMTFres.HasValue('GMTF') THEN BEGIN
                          resPlot[1]=PLOT(MTFres.(sel).gx,MTFres.(sel).gMTF,'-', NAME='MTF from fitted', /OVERPLOT)
                          ;loadct, 13, /SILENT
                          ;OPLOT, MTFres.(sel).gx,MTFres.(sel).gMTF,linestyle=0
                        ENDIF
                        lim=[0.5,1.,1.5,2.,2.5]
                        FOR i=0,4 DO resPlot[i+2]=PLOT([lim(i),lim(i)],[0,MTFres.(sel).lpmm(i)],'-',/OVERPLOT);OPLOT, [lim(i),lim(i)],[0,MTFres.(sel).lpmm(i)],linestyle=0
                        resPlot[7]=PLOT([0,MTFres.(sel).lpmm(5)],[0.5,0.5],'-',/OVERPLOT);OPLOT, [0,MTFres.(sel).lpmm(5)],[0.5,0.5], linestyle=0
                        ;loadct, 0, /SILENT
                        resPlot[8]=PLOT([nyqfr,nyqfr],[0,1],'-',/OVERPLOT);OPLOT, [nyqfr,nyqfr],[0,1],linestyle=0,thick=2.
                        nqTxt=TEXT(nyqfr*.95,.5,'NQf',/DATA)
                        sincRect=SIN(!pi*pix*MTFres.(sel).dx)/(!pi*pix*MTFres.(sel).dx)
                        sincRect(0)=1.
                        resPlot[9]=PLOT(MTFres.(sel).dx, sincRect,'--',NAME='Aperture MTF (pix.size)',/OVERPLOT);OPLOT, MTFres.(sel).dx, sincRect, linestyle=1
                        resLeg=LEGEND(TARGET=[resPlot[0:1],resPlot[9]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                      ENDIF
                    END
                    ELSE: iDrawPlot.erase
                  ENDCASE

                ENDIF
              END

              'NPS': BEGIN

                IF N_TAGS(NPSres.(sel)) GT 1 THEN BEGIN

                  IF setRangeMinMaxX THEN rangeX=[0,1./(2*pix)]
                  IF setRangeMinMaxY THEN rangeY=[0,max([NPSres.(sel).uNPS,NPSres.(sel).vNPS])]/(NPSres.(sel).largeAreaSignal^2)
                  IF optionSet NE 3 THEN BEGIN
                    resPlotu=PLOT(NPSres.(sel).du,NPSres.(sel).uNPS/(NPSres.(sel).largeAreaSignal^2), '-',NAME='NPS u', XTITLE='spatial frequency (mm-1)',YTITLE='NNPS', TITLE='NNPS', $
                      XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                    resPlotv=PLOT(NPSres.(sel).du,NPSres.(sel).vNPS/(NPSres.(sel).largeAreaSignal^2), '--', NAME='NPS v', /OVERPLOT)
                    nqPlot=PLOT([nyqfr,nyqfr],[0,rangeY(1)], '-2', /OVERPLOT)
                    resLeg=LEGEND(TARGET=[resPlotu,resPlotv], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                    ;PLOT, NPSres.(sel).du,NPSres.(sel).uNPS/(NPSres.(sel).largeAreaSignal^2), XTITLE='spatial frequency (mm-1)',YTITLE='NNPS',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='NNPS', XSTYLE=1, YSTYLE=1
                    ;OPLOT, NPSres.(sel).du,NPSres.(sel).vNPS/(NPSres.(sel).largeAreaSignal^2),linestyle=1
                    ;OPLOT, [nyqfr,nyqfr],[0,1],linestyle=1,thick=1.

                    WIDGET_CONTROL, drawImageRes, GET_VALUE = iDrawImageRes
                    WSET, iDrawImageRes
                    activeResImg=NPSres.(sel).NPS
                    tvRes=activeResImg
                    tvRes[*,128]=0 & tvRes[128,*]=0
                    szNPS=SIZE(activeResImg, /DIMENSIONS)
                    szX=450
                    szY=ROUND(szX*(szNPS(1)*1./szNPS(0)))
                    TVSCL,congrid(adjustWindowLevel(tvRes, [0,max(tvRes)]), szX, szY, /INTERP)
                    ;valuesPlot=CREATE_STRUCT('frequency mm_1',NPSres.(sel).du,'uNNPS', NPSres.(sel).uNPS/(NPSres.(sel).largeAreaSignal^2), 'COPY000frequency mm_1',NPSres.(sel).du, 'vNNPS', NPSres.(sel).vNPS/(NPSres.(sel).largeAreaSignal^2))
                    valuesPlot=CREATE_STRUCT('frequency mm_1',NPSres.(sel).du,'uNNPS', NPSres.(sel).uNPS/(NPSres.(sel).largeAreaSignal^2), 'vNNPS', NPSres.(sel).vNPS/(NPSres.(sel).largeAreaSignal^2))
                  ENDIF
                ENDIF ELSE iDrawPlot.erase
              END

              ELSE:iDrawPlot.erase
            ENDCASE

          ENDIF ELSE iDrawPlot.erase
        END
        ;******************************** NM *************************************************
        2:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF
              'ENERGYSPEC': BEGIN
                fwhm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(2))
                fwtm=energyRes.gausscoeff(2)*2*SQRT(2*ALOG(10))

                IF setRangeMinMaxX THEN rangeX=[-fwtm,fwtm]+energyRes.gausscoeff(1)
                IF setRangeMinMaxY THEN rangeY=[0,energyRes.gausscoeff(0)]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=PLOT(energyRes.curve[0,*],energyRes.curve[1,*], '', SYMBOL='.', NAME='Values from file', XTITLE='Energy (keV)',YTITLE='Counts',TITLE='Energy spectrum and gaussian fit',$
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  ;PLOT, energyRes.curve[0,*],energyRes.curve[1,*], XTITLE='Energy (keV)',YTITLE='Counts',PSYM=3, TITLE='Energy spectrum and gaussian fit', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                  ;LOADCT, 13, /SILENT
                  gfit=calcGauss(energyRes.curve[0,*],energyRes.gausscoeff(2),energyRes.gausscoeff(0),energyRes.gausscoeff(1))
                  ;OPLOT, energyRes.curve[0,*], gfit
                  ;OPLOT, [energyRes.gausscoeff(1)-fwhm*0.5,energyRes.gausscoeff(1)-fwhm*0.5],[0,energyRes.gausscoeff(0)],linestyle=1,thick=1.
                  ;OPLOT, [energyRes.gausscoeff(1)+fwhm*0.5,energyRes.gausscoeff(1)+fwhm*0.5],[0,energyRes.gausscoeff(0)],linestyle=1,thick=1.
                  ;OPLOT, [energyRes.gausscoeff(1)-fwtm*0.5,energyRes.gausscoeff(1)-fwtm*0.5],[0,energyRes.gausscoeff(0)],linestyle=2,thick=1.
                  ;OPLOT, [energyRes.gausscoeff(1)+fwtm*0.5,energyRes.gausscoeff(1)+fwtm*0.5],[0,energyRes.gausscoeff(0)],linestyle=2,thick=1.
                  ;LOADCT, 0, /SILENT
                  gfitPlot=PLOT(energyRes.curve[0,*], gfit, '-', NAME='Gaussian fit', /OVERPLOT)
                  fwhmPlot=PLOT([-fwhm,fwhm]*.5+energyRes.gausscoeff(1),[.5,.5]*energyRes.gausscoeff(0), '-r', NAME='FWHM', /OVERPLOT)
                  fwtmPlot=PLOT([-fwtm,fwtm]*.5+energyRes.gausscoeff(1),[.1,.1]*energyRes.gausscoeff(0), '--r', NAME='FWTM', /OVERPLOT)
                  resLeg=LEGEND(TARGET=[resPlot, gfitPlot, fwhmPlot, fwtmPlot], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)

                  valuesPlot=CREATE_STRUCT('Energy keV',energyRes.curve,'Counts', energyRes.curve[1,*], 'Gaussian fit', gfit)
                ENDIF
              END
              
              'HOMOG':t=TEXT(0.1, 0.2,'No plot available for this test')
              
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
                IF setRangeMinMaxY THEN rangeY=meanVal*[0.5,1.5]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=objarr(4)
                  resPlot(0)=PLOT(xvals, yvals, '-', NAME='Mean values', XTITLE='Position (mm)',YTITLE='Signal',TITLE='Y profile of active image',$
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  resPlot(1)=PLOT(xvals, MEDIAN(yvals,medfilt), '-r', NAME='Median filtered mean values', /OVERPLOT)
                  resPlot(2)=PLOT(rangeX, [meanVal,meanVal]*1.05, '--', NAME='Tolerance +', /OVERPLOT)
                  resPlot(3)=PLOT(rangeX, [meanVal,meanVal]*0.95, '--', NAME='Tolerance -', /OVERPLOT)
                  resLeg=LEGEND(TARGET=resPlot[0:1], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                  ;PLOT, xvals, yvals, XTITLE='Position (mm)', YTITLE='Signal', TITLE='Y profile of active image', XRANGE=rangeX, Yrange=rangeY, XSTYLE=1, YSTYLE=1
                  ;LOADCT, 13, /SILENT
                  ;OPLOT, xvals, MEDIAN(yvals,medfilt)
                  ;OPLOT, rangeX, [meanVal,meanVal]*1.05, LINESTYLE=0, COLOR=128
                  ;OPLOT, rangeX, [meanVal,meanVal]*0.95, LINESTYLE=0, COLOR=128
                  ;LOADCT, 0, /SILENT
                  valuesPlot=CREATE_STRUCT('Position mm',xvals, 'Averaged signal', yvals, 'Median filtered',MEDIAN(yvals,medfilt))
                ENDIF
              END
              'CONTRAST': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                szTab=SIZE(resArr, /DIMENSIONS)
                IF setRangeMinMaxX THEN rangeX=[1,6]
                IF setRangeMinMaxY THEN rangeY=[0.,1.]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=PLOT(INDGEN(6)+1, FLOAT(resArr[7:12,rowNo]),'', SYMBOL='D',XTITLE='ROI number', YTITLE='Contrast' , $
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  ;PLOT,INDGEN(6)+1, FLOAT(resArr[7:12,rowNo]), PSYM=4,  XTITLE='ROI number', YTITLE='Contrast' , XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                ENDIF
              END
              'RADIAL': BEGIN
                WIDGET_CONTROL, txtRadialMedian, GET_VALUE=val
                medfilt=LONG(val(0))
                xvals=FINDGEN(N_ELEMENTS(radialRes[*,sel]))*0.1*pix; interpolated to 0.1*pix
                yvals=radialRes[*,sel]
                IF setRangeMinMaxX THEN rangeX=[min(xvals),max(xvals)]
                IF setRangeMinMaxY THEN rangeY=[min(yvals),max(yvals)]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=objarr(2)
                  resPlot(0)=PLOT(xvals, yvals, '-', NAME='interpolated to 0.1 pix', XTITLE='Radial distance from center (mm)', YTITLE='Signal', TITLE='Radial profile of active image',$
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  resPlot(1)=PLOT(xvals, MEDIAN(yvals,medfilt),'-r', NAME='medianfiltered', /OVERPLOT)
                  resLeg=LEGEND(TARGET=resPlot[0:1], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                  ;                PLOT, xvals, yvals, XTITLE='Radial distance from center (mm)', YTITLE='Signal', TITLE='Radial profile of active image, interpolated to 0.1 pix + medianfiltered', XRANGE=rangeX, Yrange=rangeY, XSTYLE=1, YSTYLE=1
                  ;                LOADCT, 13, /SILENT
                  ;                OPLOT, xvals, MEDIAN(yvals,medfilt)
                  ;                LOADCT, 0, /SILENT
                  valuesPlot=CREATE_STRUCT('Position mm',xvals, 'Signal', yvals, 'Median filtered',MEDIAN(yvals,medfilt))
                ENDIF
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
                        IF setRangeMinMaxX THEN rangeX=[min(MTFres.cdx),max(MTFres.cdx)]
                        IF setRangeMinMaxY THEN rangeY=[min(MTFres.submatrixAll),max(MTFres.submatrixAll)]
                        IF optionSet NE 3 THEN BEGIN
                          szM=size(MTFres.submatrixAll,/DIMENSIONS)

                          nObj=1 & IF N_ELEMENTS(szM) EQ 3 THEN nObj=szM(2)
                          resPlotX=OBJARR(nObj) & resPlotY=OBJARR(nObj)
                          resPlotX[0]=PLOT(MTFres.cdx,MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0], '-', NAME='x', XTITLE='x or y pos (mm)', YTITLE='Pixel value' , TITLE='Centered x and y profile for all (marked) images', $
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          resPlotX[0].refresh, /DISABLE
                          resPlotY[0]=PLOT(MTFres.cdy,MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0], '--',  NAME='y', /OVERPLOT)
                          IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                            FOR i = 1, szM(2)-1 DO BEGIN
                              resPlotX[i]=PLOT(MTFres.cdx, MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], '-', COLOR=[i*40,i*40+30, i*40+70], /OVERPLOT)
                              resPlotY[i]=PLOT(MTFres.cdy, MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i], '--', COLOR=[i*40,i*40+30, i*40+70], /OVERPLOT)
                              valuesPlot=CREATE_STRUCT(valuesPlot, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], $
                                'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                              ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.cdx, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y', MTFres.cdy,'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                            ENDFOR
                          ENDIF
                          resPlotLeg=LEGEND(TARGET=[resPlotX[0],resPlotY[0]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                          resPlotX[0].refresh

                          ;                        plot, MTFres.cdx,MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),0],TITLE='Profile x and y centered', YRANGE=[min(MTFres.submatrixAll),max(MTFres.submatrixAll)], linestyle=0, XSTYLE=1, YSTYLE=1
                          ;                        oplot, MTFres.cdy,MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,0], linestyle=1
                          ;                        IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                          ;                          LOADCT, 13, /SILENT
                          ;                          FOR i = 1, szM(2)-1 DO BEGIN
                          ;                            oplot, MTFres.cdx, MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], LINESTYLE=0, COLOR=i*40
                          ;                            oplot,  MTFres.cdy, MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i], LINESTYLE=1, COLOR=i*40
                          ;                            valuesPlot=CREATE_STRUCT(valuesPlot,'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.cdx,'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.submatrixAll[*,ROUND(MTFres.centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y',MTFres.cdy,'Yprof'+STRING(i,FORMAT='(i0)'),MTFres.submatrixAll[ROUND(MTFres.centerPos(0)),*,i])
                          ;                          ENDFOR
                          ;                          LOADCT, 0, /SILENT
                          ;                        ENDIF
                        ENDIF
                      ENDIF ELSE BEGIN
                        valuesPlot=CREATE_STRUCT('x', MTFres.(sel).cdx,'Xprof1', MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0] , 'y', MTFres.(sel).cdy, 'Yprof1', MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0])
                        IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).cdx),max(MTFres.(sel).cdx)]
                        IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).submatrixAll),max(MTFres.(sel).submatrixAll)]
                        IF optionSet NE 3 THEN BEGIN
                          szM=size(MTFres.(sel).submatrixAll,/DIMENSIONS)
                          xprof=MTFres.(sel).submatrixAll[*,ROUND(MTFres.(sel).centerPos(1)),0]
                          yprof=MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0]
                          resPlotX=PLOT(MTFres.(sel).cdx,xprof, '-', NAME='x', XTITLE='x or y pos (mm)', YTITLE='Pixel value' , TITLE='Centered x and y profile for selected image', $
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          resPlotY=PLOT(MTFres.(sel).cdy, yprof, '--', NAME='y',/OVERPLOT)
                          resPlotLeg=LEGEND(TARGET=[resPlotX,resPlotY], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                          ;plot, MTFres.(sel).cdx,xprof,TITLE='Profile x and y centered', YRANGE=[min(MTFres.(sel).submatrixAll),max(MTFres.(sel).submatrixAll)], linestyle=0, XSTYLE=1, YSTYLE=1
                          ;oplot, MTFres.(sel).cdy,MTFres.(sel).submatrixAll[ROUND(MTFres.(sel).centerPos(0)),*,0], linestyle=1

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
                        ENDIF
                      ENDELSE
                    ENDIF ELSE BEGIN
                      iDrawPlot.erase
                      t=TEXT(0.1, 0.2,'N/A for current method.')
                      ;                      TVSCL, INTARR(550,550)
                      ;                      XYOUTS, -4,.5, 'N/A for current method.', CHARSIZE=1.2
                    ENDELSE
                  END

                  1: BEGIN ;line
                    IF v3d THEN BEGIN
                      iDrawPlot.erase
                      t=TEXT(0.1, 0.2,'N/A for current method.')
                      ;                      TVSCL, INTARR(550,550)
                      ;                      XYOUTS, -4,.5, 'N/A for current method.', CHARSIZE=1.2
                    ENDIF ELSE BEGIN
                      tagsMTF=TAG_NAMES(MTFres.(sel))
                      edgeIn=WHERE(tagsMTF EQ 'EDGEPOS')
                      IF edgeIn(0) NE -1 THEN BEGIN
                        IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).edgePos),max(MTFres.(sel).edgePos)]
                        IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).edgeRow),max(MTFres.(sel).edgeRow)]
                        IF optionSet NE 3 THEN BEGIN
                          resPlot=PLOT(MTFres.(sel).edgePos,  MTFres.(sel).edgeRow, '', SYMBOL='D', NAME='Lineposition for each row in ROI', XTITLE='Position of line (pix i ROI)', YTITLE='Profile number', TITLE='Lineposition',$
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          resPlotFit=PLOT(MTFres.(sel).edgeFitx, MTFres.(sel).edgeFity, '-', NAME='Linear fit', /OVERPLOT)
                          resPlotLeg=LEGEND(TARGET=[resPlot,resPlotFit], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                          ;plot, MTFres.(sel).edgePos,  MTFres.(sel).edgeRow, PSYM=3, XTITLE='Position of line (pix i ROI)', YTITLE='Profile number', TITLE='Lineposition for each row in ROI with interpolation.', XSTYLE=1, YSTYLE=1
                          ;oplot,  MTFres.(sel).edgeFitx, MTFres.(sel).edgeFity
                          statTxt=['Angle: '+STRING(MTFres.(sel).angle,FORMAT='(f0.3)') ]
                          valuesPlot=CREATE_STRUCT('position pix', MTFres.(sel).edgePos, 'Row or column number ', MTFres.(sel).edgeRow, 'fitted position pix', MTFres.(sel).edgeFitx, 'fitted row_col', MTFres.(sel).edgeFity)
                        ENDIF
                      ENDIF ELSE BEGIN
                        iDrawPlot.erase
                        t=TEXT(0.1, 0.2,'N/A for current method.')
                        ;                        TVSCL, INTARR(550,550)
                        ;                        XYOUTS, -4,.5, 'N/A for current method.', CHARSIZE=1.2
                      ENDELSE
                    ENDELSE
                  END
                  2: BEGIN ;sorted pixelvalues, interpolated and smoothed
                    IF v3d THEN BEGIN

                      valuesPlot=CREATE_STRUCT('distance from center', MTFres.newdists, 'Interpolated pixel values', MTFres.pixValsInterp)
                      tagsMTF=TAG_NAMES(MTFres)
                      IF tagsMTF.HasValue('PIXVALSSMOOTH') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Smoothed pixelvalues', MTFres.(sel).pixValsSmooth)

                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.distspix0),max(MTFres.distspix0)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.pixValSort),max(MTFres.pixValSort)]
                      IF optionSet NE 3 THEN BEGIN
                        szPS=size(MTFres.pixValSort,/DIMENSIONS)
                        nObj=1 & IF N_ELEMENTS(szPS) EQ 3 THEN nObj=szPS(2)
                        resPlot=objarr(nObj)

                        resPlot[0]=PLOT(MTFres.distspix0, MTFres.pixValSort, '', SYMBOL='.', NAME='Sorted pixel values', TITLE='Pixel values sorted by distance to center', XTITLE='Distance from center (mm)', YTITLE='Pixel value', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        resPlot[0].refresh, /DISABLE
                        IF N_ELEMENTS(szPS) EQ 3 THEN BEGIN
                          FOR b=1, szPS(2)-1 DO resPlot[b]=PLOT(MTFres.distspix0, MTFres.pixValSort[*,b], '', SYMBOL='.', /OVERPLOT)
                        ENDIF
                        plotInterp=PLOT(MTFres.newdists, MTFres.pixValsInterp, '-r', NAME='Interpolated', /OVERPLOT)
                        tar=[resPlot[0],plotInterp]
                        IF tagsMTF.HasValue('PIXVALSSMOOTH') THEN BEGIN
                          plotSmooth=PLOT(MTFres.newdists, MTFres.pixValsSmooth, '-b', NAME='Smoothed', /OVERPLOT)
                          tar=[tar, plotSmooth]
                        ENDIF
                        resLeg=LEGEND(TARGET=tar, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                        resPlot[0].refresh
                        ;                      plot, MTFres.distspix0, MTFres.pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                        ;
                        ;                      IF N_ELEMENTS(szPS) EQ 3 THEN BEGIN
                        ;                        FOR b=1, szPS(2)-1 DO oplot,  MTFres.distspix0, MTFres.pixValSort[*,b], PSYM=3
                        ;                      ENDIF
                        ;                      LOADCT, 13, /SILENT
                        ;                      oplot, MTFres.newdists, MTFres.pixValsInterp, COLOR=255
                        ;                      oplot, MTFres.newdists, MTFres.pixValsSmooth, COLOR=150
                        ;                      LOADCT, 0, /SILENT
                      ENDIF
                    ENDIF ELSE BEGIN
                      tagsMTF=TAG_NAMES(MTFres.(sel))
                      IF tagsMTF.HasValue('DISTSPIX0') THEN BEGIN
                        valuesPlot=CREATE_STRUCT('distance from center', MTFres.(sel).newdists,'Interpolated pixel values', MTFres.(sel).pixValsInterp)
                        tagsMTF=TAG_NAMES(MTFres.(sel))
                        IF tagsMTF.HasValue('PIXVALSSMOOTH') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Smoothed pixelvalues', MTFres.(sel).pixValsSmooth)

                        IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).distspix0),max(MTFres.(sel).distspix0)]
                        IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).pixValSort),max(MTFres.(sel).pixValSort)]
                        IF optionSet NE 3 THEN BEGIN
                          szPS=size(MTFres.(sel).pixValSort,/DIMENSIONS)
                          nObj=1 & IF N_ELEMENTS(szPS) EQ 3 THEN nObj=szPS(2)
                          resPlot=objarr(nObj)

                          resPlot[0]=PLOT(MTFres.(sel).distspix0, MTFres.(sel).pixValSort, '', SYMBOL='.', NAME='Sorted pixel values', TITLE='Pixel values sorted by distance to center', XTITLE='Distance from center (mm)', YTITLE='Pixel value', $
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          resPlot[0].refresh, /DISABLE
                          IF N_ELEMENTS(szPS) EQ 3 THEN BEGIN
                            FOR b=1, szPS(2)-1 DO resPlot[b]=PLOT(MTFres.(sel).distspix0, MTFres.(sel).pixValSort[*,b], '', SYMBOL='.', /OVERPLOT)
                          ENDIF
                          plotInterp=PLOT(MTFres.(sel).newdists, MTFres.(sel).pixValsInterp, '-r', NAME='Interpolated', /OVERPLOT)
                          tar=[resPlot[0],plotInterp]
                          IF tagsMTF.HasValue('PIXVALSSMOOTH') THEN BEGIN
                            plotSmooth=PLOT(MTFres.(sel).newdists, MTFres.(sel).pixValsSmooth, '-b', NAME='Smoothed', /OVERPLOT)
                            tar=[tar, plotSmooth]
                          ENDIF
                          resLeg=LEGEND(TARGET=tar, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                          resPlot[0].refresh

                          ;                        plot, MTFres.(sel).distspix0, MTFres.(sel).pixValSort, PSYM=3, TITLE='Sorted pixel values, interpolated (red) and smoothed (green)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1
                          ;                        szM=size(MTFres.(sel).submatrixAll,/DIMENSIONS)
                          ;                        IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                          ;                          FOR b=1, szM(2)-1 DO oplot,  MTFres.(sel).distspix0, MTFres.(sel).pixValSort[*,b], PSYM=3
                          ;                        ENDIF
                          ;                        LOADCT, 13, /SILENT
                          ;                        oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsInterp, COLOR=255
                          ;                        oplot, MTFres.(sel).newdists, MTFres.(sel).pixValsSmooth, COLOR=150
                          ;                        LOADCT, 0, /SILENT
                        ENDIF
                      ENDIF ELSE BEGIN
                        iDrawPlot.erase
                        t=TEXT(0.1, 0.2,'Sorted pixelvalues not available for point method.')
                        ;                        TVSCL, INTARR(550,550)
                        ;                        XYOUTS, -4,.5, 'Sorted pixelvalues not available for point method.', CHARSIZE=1.2
                      ENDELSE
                    ENDELSE

                  END
                  3: BEGIN; LSF
                    IF v3d EQ 0 THEN BEGIN
                      tagMTFres=tag_names(MTFres.(sel))
                      IF N_TAGS(MTFres.(sel)) GT 1 THEN BEGIN
                        IF N_ELEMENTS(MTFres.(sel).dy) EQ 1 THEN BEGIN
                          valuesPlot=CREATE_STRUCT('pos mm',MTFres.(sel).dx,'LSF',MTFres.(sel).LSFx)
                          IF tagMTFres.HasValue('SMLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Smoothed LSF', MTFres.(sel).smLSFx);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000pos mm',MTFres.(sel).dx,'Smoothed LSF', MTFres.(sel).smLSFx)
                          IF tagMTFres.HasValue('FITLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Fitted LSF', MTFres.(sel).fitLSFx);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001pos mm',MTFres.(sel).dx, 'Fitted LSF', MTFres.(sel).fitLSFx)
                          IF setRangeMinMaxX THEN rangeX=[min(MTFres.(sel).dx),max(MTFres.(sel).dx)]
                          IF setRangeMinMaxY THEN rangeY=[min(MTFres.(sel).LSFx),max(MTFres.(sel).LSFx)]
                        ENDIF ELSE BEGIN
                          valuesPlot=CREATE_STRUCT('xpos mm', MTFres.(sel).dx, 'LSFx', MTFres.(sel).LSFx)
                          IF tagMTFres.HasValue('SMLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Smoothed LSFX', MTFres.(sel).smLSFx);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000xpos mm',MTFres.(sel).dx,'Smoothed LSFX', MTFres.(sel).smLSFx)
                          IF tagMTFres.HasValue('FITLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Fitted LSFX', MTFres.(sel).fitLSFx);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001xpos mm',MTFres.(sel).dx, 'Fitted LSFX', MTFres.(sel).fitLSFx)
                          valuesPlot=CREATE_STRUCT(valuesPlot,'ypos mm', MTFres.(sel).dy, 'LSFy', MTFres.(sel).LSFy)
                          IF tagMTFres.HasValue('SMLSFY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Smoothed LSFY', MTFres.(sel).smLSFy);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000ypos mm',MTFres.(sel).dy,'Smoothed LSFY', MTFres.(sel).smLSFy)
                          IF tagMTFres.HasValue('FITLSFY') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Fitted LSFY', MTFres.(sel).fitLSFy);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001ypos mm',MTFres.(sel).dy, 'Fitted LSFY', MTFres.(sel).fitLSFy)
                          IF setRangeMinMaxX THEN rangeX=[min([MTFres.(sel).dx,MTFres.(sel).dy]),max([MTFres.(sel).dx,MTFres.(sel).dy])]
                          IF setRangeMinMaxY THEN rangeY=[min([MTFres.(sel).LSFx,MTFres.(sel).LSFy]),max([MTFres.(sel).LSFx,MTFres.(sel).LSFy])]
                        ENDELSE
                        IF optionSet NE 3 THEN BEGIN
                          resPlotX=objarr(3) & resPlotY=objarr(3)
                          resPlotX(0)=PLOT(MTFres.(sel).dx,MTFres.(sel).LSFx, '-r', NAME='LSF x', XTITLE='position (mm)', TITLE='Line Spread Function', $
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          tar=resPlotX(0)
                          IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN BEGIN
                            resPlotY(0)=PLOT(MTFres.(sel).dy, MTFres.(sel).LSFy, '--r', NAME='LSF y', /OVERPLOT)
                            tar=[tar, resPlotY(0)]
                          ENDIF
                          IF tagMTFres.HasValue('SMLSFX') THEN BEGIN
                            resPlotX(1)=PLOT(MTFres.(sel).dx, MTFres.(sel).smLSFx, '-b', NAME='Smoothed LSF x', /OVERPLOT)
                            tar=[tar, resPlotX(1)]
                          ENDIF
                          IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.HasValue('SMLSFY') THEN BEGIN
                            resPlotY(1)=PLOT(MTFres.(sel).dy, MTFres.(sel).smLSFy, '--b', NAME='Smoothed LSF y', /OVERPLOT)
                            tar=[tar, resPlotY(1)]
                          ENDIF
                          IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
                            resPlotX(2)=PLOT(MTFres.(sel).dx,MTFres.(sel).fitLSFx, '-', NAME='Fitted LSF x', /OVERPLOT)
                            tar=[tar, resPlotX(2)]
                          ENDIF
                          IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.HasValue('FITLSFY') THEN BEGIN
                            resPlotY(2)=PLOT(MTFres.(sel).dy,MTFres.(sel).fitLSFy,'--', NAME='Fitted LSF y', /OVERPLOT)
                            tar=[tar, resPlotY(2)]
                          ENDIF
                          resLeg=LEGEND(TARGET=tar, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)

                          ;                        IF tagMTFres.HasValue('FITLSFX') THEN PLOT, MTFres.(sel).dx,MTFres.(sel).fitLSFx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1 $
                          ;                        ELSE PLOT, MTFres.(sel).dx,MTFres.(sel).LSFx, linestyle=0, XTITLE='position (mm)', TITLE='LSF in x/y(dotted) dir, green=smoothed, white=fit to smoothed', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                          ;                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.HasValue('FITLSFY') THEN OPLOT, MTFres.(sel).dy,MTFres.(sel).fitLSFy,linestyle=1
                          ;                        LOADCT, 13, /SILENT
                          ;                        IF tagMTFres.HasValue('SMLSFX') THEN oplot, MTFres.(sel).dx, MTFres.(sel).smLSFx, COLOR=150
                          ;                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.HasValue('SMLSFY') THEN oplot, MTFres.(sel).dy, MTFres.(sel).smLSFy, COLOR=150, linestyle=1
                          ;                        oplot, MTFres.(sel).dx, MTFres.(sel).LSFx, COLOR=255
                          ;                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN oplot, MTFres.(sel).dy, MTFres.(sel).LSFy, COLOR=255, linestyle=1
                          ;                        LOADCT, 0, /SILENT

;                          IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
;                            xprof=MTFres.(sel).fitLSFx
;                            maxval=max(xprof)
;                            minval=min(xprof)
;                            resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
;                            resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
;                            statTxt=[['Gaussian fit of smoothed LSF:'],['FWHM: '+STRING(resFWHM(0)*0.1*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*0.1*pix, FORMAT='(f0.0)')]]
;                          ENDIF
                          ;                        IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN BEGIN
                          ;                          maxval=max(yprof)
                          ;                          minval=min(yprof)
                          ;                          resFWHM=getWidthAtThreshold(yprof,(maxval+minval)/2.)
                          ;                          resFWTM=getWidthAtThreshold(yprof,(maxval-minval)/10.+minval)
                          ;                          statTxt=[[statTxt],[''],['Y profile:'],['FWHM: '+STRING(resFWHM(0)*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*pix, FORMAT='(f0.0)')]]
                          ;                        ENDIF
                        ENDIF
                      ENDIF ELSE iDrawPlot.erase;TVSCL, INTARR(550,550)
                    ENDIF ELSE BEGIN

                      ;;;;;; TODO ;;;;;;;;;;;; add y if option for line in 3d?
                      tagMTFres=tag_names(MTFres)
                      valuesPlot=CREATE_STRUCT('pos mm',MTFres.dx,'LSF',MTFres.LSFx)
                      IF tagMTFres.HasValue('SMLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'Smoothed LSF', MTFres.smLSFx);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY000pos mm',MTFres.dx,'Smoothed LSF', MTFres.smLSFx)
                      IF tagMTFres.HasValue('FITLSFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot, 'Fitted LSF', MTFres.fitLSFx);valuesPlot=CREATE_STRUCT(valuesPlot,'COPY001pos mm',MTFres.dx, 'Fitted LSF', MTFres.fitLSFx)
                      IF setRangeMinMaxX THEN rangeX=[min(MTFres.dx),max(MTFres.dx)]
                      IF setRangeMinMaxY THEN rangeY=[min(MTFres.LSFx),max(MTFres.LSFx)]
                      IF optionSet NE 3 THEN BEGIN
                        resPlot=objarr(3)
                        resPlot(0)=PLOT(MTFres.dx,MTFres.LSFx, '-r', NAME='LSF from interpolation', XTITLE='position (mm)',TITLE='Line Spread Function', $
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        tar=resPlot(0)
                        IF tagMTFres.HasValue('SMLSFX') THEN BEGIN
                          resPlot(1)=PLOT(MTFres.dx,MTFres.smLSFx, '-b', NAME='Smoothed LSF', /OVERPLOT)
                          tar=[tar, resPlot(1)]
                        ENDIF
                        IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
                          resPlot(2)=PLOT(MTFres.dx,MTFres.fitLSFx, '-', NAME='Fitted LSF', /OVERPLOT)
                          tar=[tar, resPlot(2)]
                        ENDIF
                        resLeg=LEGEND(TARGET=tar, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)

                        ;                      IF tagMTFres.HasValue('FITLSFX') THEN PLOT, MTFres.dx,MTFres.fitLSFx, linestyle=0, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1 $
                        ;                      ELSE PLOT, MTFres.dx,MTFres.LSFx, linestyle=0, XTITLE='position (mm)',TITLE='LSF for discrete MTF (red), smoothed LSF (green) with fitted gaussian (white)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                        ;                      LOADCT, 13, /SILENT
                        ;                      IF tagMTFres.HasValue('SMLSFX') THEN OPLOT, MTFres.dx,MTFres.smLSFx, COLOR=150, linestyle=0
                        ;                      OPLOT, MTFres.dx,MTFres.LSFx, COLOR=255, linestyle=0
                        ;                      LOADCT, 0, /SILENT

                        IF tagMTFres.HasValue('FITLSFX') THEN BEGIN
                          xprof=MTFres.fitLSFx
                          maxval=max(xprof)
                          minval=min(xprof)
                          resFWHM=getWidthAtThreshold(xprof,(maxval+minval)/2.)
                          resFWTM=getWidthAtThreshold(xprof,(maxval-minval)/10.+minval)
                          statTxt=[['Gaussian fit of smoothed LSF:'],['FWHM: '+STRING(resFWHM(0)*0.1*pix, FORMAT='(f0.0)')],['FWTM: '+STRING(resFWTM(0)*.1*pix, FORMAT='(f0.0)')]]
                        ENDIF
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
                        IF optionSet NE 3 THEN BEGIN
                          IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN xName='MTF x' ELSE xName='MTF'
                          resPlot=PLOT(MTFres.(sel).fx*mm2cm,MTFres.(sel).MTFx, '-r', NAME=xName, XTITLE='frequency (mm-1)',YTITLE='MTF', TITLE='Modulation Transfer Function', $
                            XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                          tar=resPlot
                          IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN BEGIN
                            resPloty=PLOT(MTFres.(sel).fy*mm2cm,MTFres.(sel).MTFy,'--r', NAME='MTF y', /OVERPLOT)
                            tar=[tar, resPloty]
                          ENDIF
                          IF tagMTFres.hasValue('GFX') THEN BEGIN
                            IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN xName='Gaussian MTF x' ELSE xName='Gaussian MTF'
                            resPlotGx=PLOT(MTFres.(sel).gfx*mm2cm,MTFres.(sel).gMTFx,'-', NAME=xName, /OVERPLOT)
                            tar=[tar, resPlotGx]
                          ENDIF
                          IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.hasValue('GFY') THEN BEGIN
                            resPlotGy=PLOT(MTFres.(sel).gfy*mm2cm,MTFres.(sel).gMTFy,'--', NAME='Gaussian MTF y', /OVERPLOT)
                            tar=[tar, resPlotGy]
                          ENDIF
                          resLeg=LEGEND(TARGET=tar, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                          nqPlot=PLOT([nyqfr,nyqfr]*mm2cm,[0,rangeY(1)],'-',/OVERPLOT)
                          nqTxt=TEXT(nyqfr*.95*mm2cm,rangeY(1)/2,'NQf',/DATA)
                          ;PLOT, MTFres.(sel).fx*mm2cm,MTFres.(sel).MTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, XRANGE=rangeX, YRANGE=rangeY, TITLE='MTF gaussian (red) and discrete (white) x and y (dotted)', XSTYLE=1, YSTYLE=1
                          ;IF N_ELEMENTS(MTFres.(sel).dy) GT 1 THEN OPLOT, MTFres.(sel).fy*mm2cm,MTFres.(sel).MTFy,linestyle=1
                          ;loadct, 13, /SILENT
                          ;IF tagMTFres.hasValue('GFX') THEN OPLOT, MTFres.(sel).gfx*mm2cm,MTFres.(sel).gMTFx,linestyle=0
                          ;IF N_ELEMENTS(MTFres.(sel).dy) GT 1 AND tagMTFres.hasValue('GFY') THEN OPLOT, MTFres.(sel).gfy*mm2cm,MTFres.(sel).gMTFy,linestyle=1
                          ;loadct, 0, /SILENT
                          ;OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0,thick=2.
                        ENDIF
                      ENDIF ELSE iDrawPlot.erase;TVSCL, INTARR(550,550)
                    ENDIF ELSE BEGIN
                      tagMTFres=tag_names(MTFres)
                      valuesPlot=CREATE_STRUCT('discrete frequency mm_1',MTFres.fx*mm2cm, 'discrete MTF',MTFres.MTFx)
                      IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian frequency mm_1',MTFres.gfx*mm2cm,'gaussian MTF',MTFres.gMTFx)

                      IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]*mm2cm
                      IF setRangeMinMaxY THEN BEGIN
                        IF tagMTFres.hasValue('GMTFX') THEN rangeY=[min(MTFres.gMTFx),max(MTFres.gMTFx)] ELSE rangeY=[0, 1]
                      ENDIF
                      IF optionSet NE 3 THEN BEGIN
                        resPlot=PLOT(MTFres.fx*mm2cm, MTFres.MTFx,'-r',NAME='MTF',XTITLE='frequency (mm-1)',YTITLE='MTF',TITLE='Modulation Transfer Function',$
                          XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                        IF tagMTFres.hasValue('GMTFX') THEN BEGIN
                          resPlotG=PLOT(MTFres.gfx*mm2cm,MTFres.gMTFx, '-', NAME='Gaussian MTF', /OVERPLOT)
                          resLeg=LEGEND(TARGET=[resPlot, resPlotG], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                        ENDIF
                        nqPlot=PLOT([nyqfr,nyqfr]*mm2cm,[0,rangeY(1)],'-',/OVERPLOT)
                        nqTxt=TEXT(nyqfr*.95*mm2cm,rangeY(1)/2,'NQf',/DATA)
                        ;IF tagMTFres.hasValue('GMTFX') THEN PLOT, MTFres.gfx*mm2cm,MTFres.gMTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, TITLE='MTF gaussian and discrete (dotted)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1 $
                        ;ELSE PLOT, MTFres.fx*mm2cm,MTFres.MTFx, XTITLE='frequency (mm-1)',YTITLE='MTF',linestyle=0, TITLE='MTF (discrete)', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, /NODATA
                        ;OPLOT, MTFres.fx*mm2cm, MTFres.MTFx, LINESTYLE=1
                        ;OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=0,thick=2.
                        ;OPLOT, [nyqfr,nyqfr]*mm2cm,[0,1],linestyle=1,thick=2.
                      ENDIF
                    ENDELSE
                  END; show MTF
                  ELSE: iDrawPlot.erase
                ENDCASE; MTF test
              END

              ELSE:iDrawPlot.erase
            ENDCASE; tests

          ENDIF ELSE iDrawPlot.erase
        END

        ;******************************** PET *************************************************
        3:BEGIN
          curTab=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
          IF results(curTab) EQ 1 THEN BEGIN

            CASE analyse OF

              'CROSSCALIB': BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                zPosMarked=getZposMarked(structImgs, markedTemp)
                yValues = FLOAT(resArr[0,*])
                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[mean(yValues)*0.9,mean(yValues)*1.1]
                IF optionSet NE 3 THEN BEGIN
                  resPlot=PLOT(zPosMarked, yValues,'-r',XTITLE='zPos (mm)', YTITLE='Activity concentration (Bq/mL)' , TITLE='Activity concentration for all (marked) images',$
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  ;PLOT, zPosMarked, yValues,  XTITLE='zPos (mm)', YTITLE='Activity concentration (Bq/mL)' , TITLE='Activity concentration for all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, POSITION=[0.2,0.15,0.95,0.95]
                  valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Activity concentration', yValues)
                ENDIF
              END

              'HOMOG':BEGIN
                WIDGET_CONTROL, resTab, GET_VALUE=resArr
                resArr=FLOAT(resArr[5:9,*])
                colNo=5
                zPosMarked=getZposMarked(structImgs, markedTemp)

                IF setRangeMinMaxX THEN rangeX=[min(zPosMarked),max(zPosMarked)]
                IF setRangeMinMaxY THEN rangeY=[-10,10]
                IF optionSet NE 3 THEN BEGIN
                  valCenter=transpose(resArr[0,*])
                  val12=transpose(resArr[1,*])
                  val15=transpose(resArr[2,*])
                  val18=transpose(resArr[3,*])
                  val21=transpose(resArr[4,*])
                  valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Center', valCenter, 'at12', val12, 'at15', val15,'at18', val18, 'at21', val21)
                  ;valuesPlot=CREATE_STRUCT('zPos', zPosMarked, 'Center', valCenter, 'COPY0zPos',zPosMarked, 'at12', val12, 'COPY1zPos',zPosMarked, 'at15', val15, 'COPY2zPos',zPosMarked, 'at18', val18, 'COPY3zPos',zPosMarked, 'at21', val21)
                  resPlot=OBJARR(6)
                  resPlot[0]=PLOT(zPosMarked, val12, '-r', NAME='at 12', XTITLE='zPos (mm)', YTITLE='Difference (%)' , TITLE='Difference from mean of all (marked) images', $
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  resPlot[0].refresh, /DISABLE
                  resPlot[1]=PLOT(zPosMarked, val15, '-b',  NAME='at 15', /OVERPLOT)
                  resPlot[2]=PLOT(zPosMarked, val18, '-g',  NAME='at 18', /OVERPLOT)
                  resPlot[3]=PLOT(zPosMarked, val21, '-c',  NAME='at 21', /OVERPLOT)
                  resPlot[4]=PLOT(rangeX, [5,5],':', NAME='tolerance 5%', /OVERPLOT)
                  resPlot[5]=PLOT(rangeX, [-5,-5],':', /OVERPLOT)
                  resPlotLeg=LEGEND(TARGET=resPlot[0:4], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                  resPlot[0].refresh
                  ;                PLOT, zPosMarked, valCenter, XTITLE='zPos (mm)', YTITLE='Difference (%)' , TITLE='Difference from mean of all (marked) images', XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, COLOR=255
                  ;                LOADCT, 12, /SILENT
                  ;                OPLOT, zPosMarked, val12, COLOR=200;red
                  ;                OPLOT, zPosMarked, val15, COLOR=100;blue
                  ;                OPLOT, zPosMarked, val18, COLOR=40;green
                  ;                OPLOT, zPosMarked, val21, COLOR=90;cyan
                  ;                loadct, 0, /SILENT
                  ;                OPLOT, rangeX, [5,5], LINESTYLE=1, COLOR=255
                  ;                OPLOT, rangeX, [-5,-5], LINESTYLE=1, COLOR=255
                ENDIF
              END

              ELSE:
            ENDCASE
          ENDIF ELSE iDrawPlot.erase;TVSCL, INTARR(550,550)
        END

        ELSE:

      ENDCASE; modes


      IF setRangeMinMaxX THEN BEGIN
        WIDGET_CONTROL, txtMinRangeX, SET_VALUE=STRING(rangeX(0),FORMAT=formatCode(rangeX(0)))
        WIDGET_CONTROL, txtMaxRangeX, SET_VALUE=STRING(rangeX(1),FORMAT=formatCode(rangeX(1)))
      ENDIF
      IF setRangeMinMaxY THEN BEGIN
        WIDGET_CONTROL, txtMinRangeY, SET_VALUE=STRING(rangeY(0),FORMAT=formatCode(rangeY(0)))
        WIDGET_CONTROL, txtMaxRangeY, SET_VALUE=STRING(rangeY(1),FORMAT=formatCode(rangeY(1)))
      ENDIF

      IF optionSet GE 1 THEN BEGIN
        plotNames=TAG_NAMES(valuesPlot)
        nnNames=N_ELEMENTS(plotNames)
        IF nnNames GT 1 THEN BEGIN
          nVals=INTARR(nnNames)
          copyVals=INTARR(nnNames)
          FOR i=0, nnNames-1 DO BEGIN
            nVals(i)=N_ELEMENTS(valuesPlot.(i))
            IF STRMID(plotNames(i),0,4) EQ 'COPY' THEN copyVals(i)=1
          ENDFOR

          IF optionSet EQ 1 THEN BEGIN
            valuesArr=STRARR(nnNames-TOTAL(copyVals),MAX(nVals))
            headArr=STRARR(nnNames-TOTAL(copyVals))
            counter=0
            FOR i=0, nnNames-1 DO BEGIN
              IF copyVals(i) NE 1 THEN BEGIN
                tempArr=STRTRIM(STRING(TRANSPOSE(valuesPlot.(i))),1)
                IF decimMark EQ ',' THEN FOREACH elem, tempArr, idx DO tempArr(idx)=STRJOIN(STRSPLIT(elem, '.',/EXTRACT),',')
                valuesArr[counter,0:nVals(i)-1]=tempArr
                headArr(counter)=plotNames(i)
                counter=counter+1
              ENDIF
            ENDFOR

            CLIPBOARD.set, [STRJOIN(headArr, STRING(9B)),STRJOIN(valuesArr, STRING(9B))]
          ENDIF
          ;          IF optionSet EQ 2 THEN BEGIN
          ;            IF N_TAGS(valuesPlot) GT 1 THEN BEGIN
          ;              counter=0
          ;              FOR i=0, nnNames-1 DO BEGIN
          ;                IF copyVals(i) THEN plotNames(i)=STRMID(plotNames(i),7,STRLEN(plotNames(i)))
          ;                IF i mod 2 THEN BEGIN
          ;                  IF i EQ 1 THEN iPlot, valuesPlot.(i-1), valuesPlot.(i), NAME=plotNames(i), /INSERT_LEGEND $
          ;                  ELSE iPlot, valuesPlot.(i-1), valuesPlot.(i), NAME=plotNames(i), LINESTYLE=counter mod 7, /OVERPLOT, /INSERT_LEGEND
          ;                  counter=counter+1
          ;                ENDIF
          ;              ENDFOR
          ;            ENDIF
          ;          ENDIF
        ENDIF ELSE BEGIN
          IF optionSet EQ 1 THEN sv=DIALOG_MESSAGE('No values to copy to clipboard.')
          ;IF optionSet EQ 2 THEN sv=DIALOG_MESSAGE('No plot values to display.')
        ENDELSE
      ENDIF

      WIDGET_CONTROL, statPlot, SET_VALUE=statTxt

    ENDIF ELSE iDrawPlot.ERASE

  ENDIF ELSE iDrawPlot.ERASE ;selected image not marked - no results shown

end