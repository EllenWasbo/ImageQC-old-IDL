;ImageQC - quality control of medical images
;Copyright (C) 2019 Ellen Wasbo, Stavanger University Hospital, Norway
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
pro updateMTFNPSresults, setRangeMinMaxX, setRangeMinMaxY, optionSet
  COMPILE_OPT hidden
  COMMON MTFNPS

  valuesPlot=CREATE_STRUCT('empty',0); structure of plot values to be copied to clipboard (optionSet = 1) or sent to iPlot (optionSet =2), x/y pairs of vectors, tagname 'COPYXXX...' means that the vector (x values) is not included when optionSet=1

  MTForNPS=WIDGET_INFO(wtab, /TAB_CURRENT)
  tabOrPlot=0
  IF MTForNPS EQ 0 THEN tabOrPlot=WIDGET_INFO(wtabResultMTF, /TAB_CURRENT) ELSE tabOrPlot=WIDGET_INFO(wtabResultNPS, /TAB_CURRENT)

  CASE tabOrPlot OF
    0: BEGIN; table results*********************************************
      CASE MTForNPS OF
        0: BEGIN ;MTF
          ;Alternative table: Gaussian fit parameters

          tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
          serNo=tabSel(1)
          WIDGET_CONTROL, cw_plotMaterial, GET_VALUE=matNo
          nRows=N_TAGS(structImgsMTF)
          nCols=6
          headers=headersMTF;['dMTF 50%','dMTF 10%','dMTF 2%','gMTF 50%','gMTF 10%','gMTF 2%']
          resArrString=STRARR(N_ELEMENTS(headers),nRows)
          FOR i=0, nRows-1 DO BEGIN
            resArrString[0:2,i]=STRING(MTFres.(i).(matNo).F50_10_2[0:2], FORMAT='(F0.3)')
            resArrString[3:5,i]=STRING(MTFres.(i).(matNo).F50_10_2DISCRETE[0:2], FORMAT='(F0.3)')
          ENDFOR

          WIDGET_CONTROL, resTabMTF, TABLE_XSIZE=nCols, TABLE_YSIZE=nRows, COLUMN_LABELS=headers, COLUMN_WIDTHS=INTARR(nCols)+420/nCols, SET_VALUE=resArrString, ALIGNMENT=1
          WIDGET_CONTROL, resTabMTF, SET_TABLE_SELECT=[0,serNo,0,serNo]

        END
        1: BEGIN ;NPS
          tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
          serNo=tabSel(1)
          nRows=N_TAGS(structImgsNPS)
          nCols=2
          headers=headersNPS;['Average AUC','Average centroid']
          resArrString=STRARR(N_ELEMENTS(headers),nRows)
          
          NPStot=0
          FOR s=0, nRows-1 DO BEGIN
            nImg=N_TAGS(structImgsNPS.(s))
            FOR i=0, nImg-1 DO BEGIN
              IF N_ELEMENTS(NPStot) EQ 1 THEN BEGIN
                NPStot=NPSres.(s).(i).rNPS
                ftemp=NPSres.(s).(i).dr
              ENDIF ELSE NPStot=NPStot+NPSres.(s).(i).rNPS
            ENDFOR
            NPStot=NPStot/nImg
            AUC=INT_TABULATED(ftemp,NPStot)
            centr=INT_TABULATED(ftemp,ftemp*NPStot)/AUC
            
            resArrString[0,s]=STRING(AUC, FORMAT='(F0.3)')
            resArrString[1,s]=STRING(centr, FORMAT='(F0.3)')
          ENDFOR

          WIDGET_CONTROL, resTabNPS, TABLE_XSIZE=nCols, TABLE_YSIZE=nRows, COLUMN_LABELS=headers, COLUMN_WIDTHS=INTARR(nCols)+420/nCols, SET_VALUE=resArrString, ALIGNMENT=1
          WIDGET_CONTROL, resTabNPS, SET_TABLE_SELECT=[0,serNo,0,serNo]
        END
        ELSE:
      ENDCASE;MTForNPS


    END; table results


    1: BEGIN ; plot results*********************************************

      IF MTForNPS EQ 0 THEN WIDGET_CONTROL, drawPlotMTF, GET_VALUE = iDrawPlot ELSE WIDGET_CONTROL, drawPlotNPS, GET_VALUE = iDrawPlot
      iDrawPlot.SELECT

      IF optionSet EQ 2 THEN currWin=0 ELSE BEGIN
        currWin=1
        iDrawPlot.ERASE
      ENDELSE
      foName='Arial'
      foSize=8
      resPlotMargin=[0.15,0.1,0.25,0.1]; or [left, bottom, right, top]
      legPos=[1.0,0.9]

      IF MTForNPS EQ 0 THEN BEGIN
        WIDGET_CONTROL, txtMinRangeMTF_X, GET_VALUE=rangeX1
        WIDGET_CONTROL, txtMaxRangeMTF_X, GET_VALUE=rangeX2
        WIDGET_CONTROL, txtMinRangeMTF_Y, GET_VALUE=rangeY1
        WIDGET_CONTROL, txtMaxRangeMTF_Y, GET_VALUE=rangeY2
      ENDIF ELSE BEGIN
        WIDGET_CONTROL, txtMinRangeNPS_X, GET_VALUE=rangeX1
        WIDGET_CONTROL, txtMaxRangeNPS_X, GET_VALUE=rangeX2
        WIDGET_CONTROL, txtMinRangeNPS_Y, GET_VALUE=rangeY1
        WIDGET_CONTROL, txtMaxRangeNPS_Y, GET_VALUE=rangeY2
      ENDELSE
      rangeX=FLOAT([rangeX1(0),rangeX2(0)])
      rangeY=FLOAT([rangeY1(0),rangeY2(0)])

      CASE MTForNPS OF
        0: BEGIN ;MTF

          tabSel=WIDGET_INFO(tabMTF,/TABLE_SELECT)
          serNo=tabSel(1)
          pix=structImgsMTF.(serNo).(0).pix
          pix=pix(0)
          nyqfr=1/(2.*pix); Nyquist frequency in mm-1
          nImg=N_TAGS(structImgsMTF.(serNo))

          ROIsz=ROUND(imgQCstruc.MTFroisSz(0)/pix)
          halfSz=structImgsMTF.(serNo).(0).imageSize / 2
          x1=ROUND(halfSz(0)-ROIsz) & x2=ROUND(halfSz(0)+ROIsz)
          y1=ROUND(halfSz(1)-ROIsz) & y2=ROUND(halfSz(1)+ROIsz)

          WIDGET_CONTROL, cw_plotMTF, GET_VALUE= plotType
          WIDGET_CONTROL, cw_plotMaterial, GET_VALUE=plotMaterial

          s=serNo
          m=plotMaterial
          CASE plotType OF

            0: BEGIN ;centered xy profile

              valuesPlot=CREATE_STRUCT('x',MTFres.(s).(m).cdx,'Xprof',MTFres.(s).(m).submatrixAll[*,ROUND(MTFres.(s).(m).centerPos(1)),0],'y', MTFres.(s).(m).cdy, 'Yprof', transpose(MTFres.(s).(m).submatrixAll[ROUND(MTFres.(s).(m).centerPos(0)),*,0]))

              IF setRangeMinMaxX THEN rangeX=[min(MTFres.(s).(m).cdx),max(MTFres.(s).(m).cdx)]
              IF setRangeMinMaxY THEN rangeY=[min(MTFres.(s).(m).submatrixAll),max(MTFres.(s).(m).submatrixAll)]

              IF optionSet NE 3 THEN BEGIN
                szM=size(MTFres.(s).(m).submatrixAll,/DIMENSIONS)
                nObj=1 & IF N_ELEMENTS(szM) EQ 3 THEN nObj=szM(2)
                resPlotX=OBJARR(nObj) & resPlotY=OBJARR(nObj)
                resPlotX[0]=PLOT(MTFres.(s).(m).cdx,MTFres.(s).(m).submatrixAll[*,ROUND(MTFres.(s).(m).centerPos(1)),0], '-', NAME='x', XTITLE='x or y pos (mm)', YTITLE='Pixel value' , TITLE='Centered x and y profile for all (marked) images', $
                  XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                resPlotX[0].refresh, /DISABLE
                resPlotY[0]=PLOT(MTFres.(s).(m).cdy,MTFres.(s).(m).submatrixAll[ROUND(MTFres.(s).(m).centerPos(0)),*,0], '--',  NAME='y', /OVERPLOT)
                IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
                  FOR i = 1, szM(2)-1 DO BEGIN
                    resPlotX[i]=PLOT(MTFres.(s).(m).cdx, MTFres.(s).(m).submatrixAll[*,ROUND(MTFres.(s).(m).centerPos(1)),i], '-', COLOR=[i*40,i*40+30, i*40+70], /OVERPLOT)
                    resPlotY[i]=PLOT(MTFres.(s).(m).cdy, MTFres.(s).(m).submatrixAll[ROUND(MTFres.(s).(m).centerPos(0)),*,i], '--', COLOR=[i*40,i*40+30, i*40+70], /OVERPLOT)
                    valuesPlot=CREATE_STRUCT(valuesPlot, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.(s).(m).submatrixAll[*,ROUND(MTFres.(s).(m).centerPos(1)),i], $
                      'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.(s).(m).submatrixAll[ROUND(MTFres.(s).(m).centerPos(0)),*,i])
                    ;valuesPlot=CREATE_STRUCT(valuesPlot, 'COPY'+STRING(i,FORMAT='(i03)')+'x',MTFres.(s).(m).cdx, 'Xprof'+STRING(i,FORMAT='(i0)'), MTFres.(s).(m).submatrixAll[*,ROUND(MTFres.(s).(m).centerPos(1)),i], 'COPY'+STRING(i,FORMAT='(i03)')+'y', MTFres.(s).(m).cdy,'Yprof'+STRING(i,FORMAT='(i0)'), MTFres.(s).(m).submatrixAll[ROUND(MTFres.(s).(m).centerPos(0)),*,i])
                  ENDFOR
                ENDIF
                resPlotLeg=LEGEND(TARGET=[resPlotX[0],resPlotY[0]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                resPlotX[0].refresh

              ENDIF
            END
            1: BEGIN ;sorted pixelvalues, interpolated and smoothed

              valuesPlot=CREATE_STRUCT('distance from center', MTFres.(s).(m).newdists,'Interpolated pixel values',MTFres.(s).(m).pixValsInterp)
              MTFtags=TAG_NAMES(MTFres.(s).(m))
              IF MTFtags.HasValue('PIXVALSSMOOTH') THEN valuesPlot=CREATE_STRUCT(valuesPlot, 'Smoothed pixelvalues',MTFres.(s).(m).pixValsSmooth)

              IF setRangeMinMaxX THEN rangeX=[min(MTFres.(s).(m).distspix0),max(MTFres.(s).(m).distspix0)]
              IF setRangeMinMaxY THEN rangeY=[min(MTFres.(s).(m).pixValSort),max(MTFres.(s).(m).pixValSort)]

              IF optionSet NE 3 THEN BEGIN
                szM=size(MTFres.(s).(m).submatrixAll,/DIMENSIONS)
                nObj=1 & IF N_ELEMENTS(szM) EQ 3 THEN nObj=szM(2)
                resPlot=PLOT(MTFres.(s).(m).distspix0, MTFres.(s).(m).pixValSort, '', SYMBOL='.', NAME='Sorted pixel values', TITLE='Pixel values sorted by distance to center', XTITLE='Distance from center (mm)', YTITLE='Pixel value', $
                  XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                plotInterp=PLOT(MTFres.(s).(m).newdists, MTFres.(s).(m).pixValsInterp, '-r', NAME='Interpolated', /OVERPLOT)
                tars=[resPlot,plotInterp]
                IF MTFtags.HasValue('PIXVALSSMOOTH') THEN BEGIN
                  plotSmooth=PLOT(MTFres.(s).(m).newdists, MTFres.(s).(m).pixValsSmooth, '-b', NAME='Smoothed', /OVERPLOT)
                  tars=[tars, plotSmooth]
                ENDIF
                resLeg=LEGEND(TARGET=tars, FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
              ENDIF


            END
            2: BEGIN; LSF

              valuesPlot=CREATE_STRUCT('pos mm', MTFres.(s).(m).dx, 'LSF', MTFres.(s).(m).LSFx)
              IF setRangeMinMaxX THEN rangeX=[min(MTFres.(s).(m).dx),max(MTFres.(s).(m).dx)]
              IF setRangeMinMaxY THEN rangeY=[min(MTFres.(s).(m).LSFx),max(MTFres.(s).(m).LSFx)]

              IF optionSet NE 3 THEN BEGIN
                tagMTFres=tag_names(MTFres.(s).(m))
                resPlot=objarr(3)
                IF tagMTFres.hasValue('FITLSFX') THEN BEGIN
                  resPlot[0]=PLOT(MTFres.(s).(m).dx,MTFres.(s).(m).fitLSFx, '-', NAME='Fitted gaussian',XTITLE='position (mm)',TITLE='Line Spread Function',$
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                  valuesPlot=CREATE_STRUCT(valuesPlot, 'Fit to smoothed LSF', MTFres.(s).(m).fitLSFx)
                ENDIF ELSE BEGIN
                  resPlot[0]=PLOT(MTFres.(s).(m).dx,MTFres.(s).(m).dx, XTITLE='position (mm)',TITLE='Line Spread Function', /NODATA, $
                    XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                ENDELSE
                IF tagMTFres.hasValue('SMLSFX') THEN BEGIN
                  IF N_ELEMENTS(MTFres.(s).(m).smLSFx) GT 1 THEN BEGIN
                    resPlot[1]=PLOT( MTFres.(s).(m).dx,MTFres.(s).(m).smLSFx, '-b', NAME='Smoothed LSF', /OVERPLOT)
                    valuesPlot=CREATE_STRUCT(valuesPlot, 'Smoothed LSF', MTFres.(s).(m).smLSFx)
                  ENDIF
                ENDIF
                resPlot[2]=PLOT( MTFres.(s).(m).dx,MTFres.(s).(m).LSFx, '-r', NAME='Interpolated LSF', /OVERPLOT)
                resLeg=LEGEND(TARGET=[resPlot[2],resPlot[1],resPlot[0]], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
              ENDIF

            END
            3: BEGIN ;MTF

              mm2cm=1.;1. means no converstion to cm, 10. when convert to cm


              tagMTFres=tag_names(MTFres.(s).(m))
              valuesPlot=CREATE_STRUCT('discrete frequency mm_1',MTFres.(s).(m).fx*mm2cm, 'discrete MTF',MTFres.(s).(m).MTFx)
              IF tagMTFres.hasValue('GFX') THEN valuesPlot=CREATE_STRUCT(valuesPlot,'gaussian frequency mm_1',MTFres.(s).(m).gfx*mm2cm,'gaussian MTF',MTFres.(s).(m).gMTFx)

              IF setRangeMinMaxX THEN rangeX=[0,nyqfr*1.1]*mm2cm
              IF setRangeMinMaxY THEN rangeY=[min(MTFres.(s).(m).MTFx),max(MTFres.(s).(m).MTFx)]
              IF optionSet NE 3 THEN BEGIN
                resPlot=objarr(7)
                resPlot[0]=PLOT(MTFres.(s).(m).fx*mm2cm,MTFres.(s).(m).MTFx,XTITLE='frequency (mm-1)',YTITLE='MTF',TITLE='Modulation Transfer Function',/NODATA,$
                  XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
                nLeg=1
                resPlot[0].refresh, /DISABLE
                IF tagMTFres.hasValue('GFX') THEN BEGIN
                  resPlot[2]=PLOT(MTFres.(s).(m).gfx*mm2cm,MTFres.(s).(m).gMTFx, '-',NAME='Gaussian MTF',/OVERPLOT)
                  nLeg=2
                ENDIF
                resPlot[1]=PLOT(MTFres.(s).(m).fx*mm2cm, MTFres.(s).(m).MTFx, '-r',NAME='Discrete MTF',/OVERPLOT)
                legPlot=LEGEND(TARGET=resPlot[1:nleg], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
                resPlot[3]=PLOT([0,MTFres.(s).(m).F50_10_2(0)*mm2cm],[.5,.5],'-',/OVERPLOT)
                resPlot[4]=PLOT([0,MTFres.(s).(m).F50_10_2(1)*mm2cm],[.1,.1],'-',/OVERPLOT)
                resPlot[5]=PLOT([0,MTFres.(s).(m).F50_10_2(2)*mm2cm],[.02,.02],'-',/OVERPLOT)
                resPlot[6]=PLOT([nyqfr,nyqfr]*mm2cm,[0,1],'-',/OVERPLOT)
                nqTxt=TEXT(nyqfr*.95,.5,'NQf',/DATA)
                resPlot[0].refresh
              ENDIF

            END
            ELSE: iDrawPlot.erase
          ENDCASE

          ;*******************
          IF setRangeMinMaxX THEN BEGIN
            WIDGET_CONTROL, txtMinRangeMTF_X, SET_VALUE=STRING(rangeX(0),FORMAT=formatCode(rangeX(0)))
            WIDGET_CONTROL, txtMaxRangeMTF_X, SET_VALUE=STRING(rangeX(1),FORMAT=formatCode(rangeX(1)))
          ENDIF
          IF setRangeMinMaxY THEN BEGIN
            WIDGET_CONTROL, txtMinRangeMTF_Y, SET_VALUE=STRING(rangeY(0),FORMAT=formatCode(rangeY(0)))
            WIDGET_CONTROL, txtMaxRangeMTF_Y, SET_VALUE=STRING(rangeY(1),FORMAT=formatCode(rangeY(1)))
          ENDIF
        END
        1: BEGIN ;NPS
          ;****************
          IF N_ELEMENTS(NPSres) GT 0 THEN BEGIN

            tabSel=WIDGET_INFO(tabNPS,/TABLE_SELECT)
            serNo=tabSel(1)

            mm2cm=1.;1. means no converstion to cm, 10. when convert to cm
            NPStot=0
            ftemp=0
            nImg=N_TAGS(structImgsNPS.(serNo))
            FOR i=0, nImg-1 DO BEGIN
              IF N_ELEMENTS(NPStot) EQ 1 THEN BEGIN
                NPStot=NPSres.(serNo).(i).rNPS
                ftemp=NPSres.(serNo).(i).dr
              ENDIF ELSE NPStot=NPStot+NPSres.(serNo).(i).rNPS
            ENDFOR
            NPStot=NPStot/nImg

            IF setRangeMinMaxX THEN rangeX=[0,max(NPSres.(serNo).(0).dr)]
            IF setRangeMinMaxY THEN rangeY=[0,max(NPStot)]
            IF optionSet NE 3 THEN BEGIN
              valuesPlot=CREATE_STRUCT('f',ftemp, 'NPStot', NPStot)
              resPlot=PLOT(NPSres.(serNo).(0).dr,NPStot, '-', XTITLE='spatial frequency (mm-1)',YTITLE='NPS', TITLE='Noise Power Spectrum', /NODATA,$
                XRANGE=rangeX, YRANGE=rangeY, XSTYLE=1, YSTYLE=1, MARGIN=resPlotMargin, FONT_NAME=foName, FONT_SIZE=foSize, CURRENT=currWin)
              resPlot.refresh, /DISABLE

              resPlotImg=objarr(nImg)
              FOR i=0, nImg-1 DO BEGIN
                resPlotImg[i]=PLOT(NPSres.(serNo).(i).dr, NPSres.(serNo).(i).rNPS, '-', NAME='Image '+STRING(i, FORMAT='(i0)'), COLOR=[i*10,i*50,i*100],/OVERPLOT)
                valuesPlot=CREATE_STRUCT(valuesPlot, 'NPS'+STRING(i,FORMAT='(i0)'), NPSres.(serNo).(i).rNPS)
              ENDFOR
            ENDIF
            resPlotAvg=PLOT(NPSres.(serNo).(0).dr,NPStot,'-2',NAME='NPS average',/OVERPLOT)
            resLeg=LEGEND(TARGET=[resPlotImg,resPlotAvg], FONT_NAME=foName, FONT_SIZE=foSize, POSITION=legPos)
            resPlot.refresh
            IF optionSet NE 3 THEN BEGIN
              pix=structImgsNPS.(serNo).(0).pix
              pix=pix(0)
              nyqfr=1/(2.*pix); Nyquist frequency in mm-1
              nqPlot=PLOT([nyqfr,nyqfr]*mm2cm,[0,rangeY(1)],'-',/OVERPLOT)
              nqTxt=TEXT(nyqfr*.95*mm2cm,rangeY(1)/2,'NQf',/DATA)
            ENDIF
          ENDIF
          IF setRangeMinMaxX THEN BEGIN
            WIDGET_CONTROL, txtMinRangeNPS_X, SET_VALUE=STRING(rangeX(0),FORMAT=formatCode(rangeX(0)))
            WIDGET_CONTROL, txtMaxRangeNPS_X, SET_VALUE=STRING(rangeX(1),FORMAT=formatCode(rangeX(1)))
          ENDIF
          IF setRangeMinMaxY THEN BEGIN
            WIDGET_CONTROL, txtMinRangeNPS_Y, SET_VALUE=STRING(rangeY(0),FORMAT=formatCode(rangeY(0)))
            WIDGET_CONTROL, txtMaxRangeNPS_Y, SET_VALUE=STRING(rangeY(1),FORMAT=formatCode(rangeY(1)))
          ENDIF
      END;NPS

      ELSE:
    ENDCASE;MTF or NPS
    ;****************

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
              IF imgQCstruc.deciMark EQ ',' THEN FOREACH elem, tempArr, idx DO tempArr(idx)=STRJOIN(STRSPLIT(elem, '.',/EXTRACT),',')
              valuesArr[counter,0:nVals(i)-1]=tempArr
              headArr(counter)=plotNames(i)
              counter=counter+1
            ENDIF
          ENDFOR

          CLIPBOARD.set, [STRJOIN(headArr, STRING(9B)),STRJOIN(valuesArr, STRING(9B))]
        ENDIF

      ENDIF ELSE BEGIN
        IF optionSet EQ 1 THEN sv=DIALOG_MESSAGE('No values to copy to clipboard.', DIALOG_PARENT=evTop)
      ENDELSE
    ENDIF
  END
  ELSE:
ENDCASE;plot


end
