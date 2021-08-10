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

;update ROI
pro updateROI, Ana=ana, SEL=presel, IMG=preImg

  COMMON VARI

  IF N_ELEMENTS(ana) EQ 0 THEN ana=analyse

  WIDGET_CONTROL, /HOURGLASS

  tags=TAG_NAMES(structImgs)
  IF tags(0) NE 'EMPTY' THEN BEGIN

    IF N_ELEMENTS(presel) EQ 0 THEN BEGIN
      sel=WIDGET_INFO(listFiles, /LIST_SELECT)
      sel=sel(0)
    ENDIF ELSE sel=presel

    IF N_ELEMENTS(preImg) EQ 0 THEN tempImg=activeImg ELSE tempImg=preImg
    szImg=SIZE(tempImg,/DIMENSIONS)
    pix=structImgs.(sel).pix;IF nFrames EQ 0 THEN pix=structImgs.(sel).pix ELSE pix=structImgs.(0).pix

    imgCenterOffset=[0,0,0,0]
    IF dxya(3) EQ 1 THEN imgCenterOffset=dxya
    center=szImg/2+imgCenterOffset[0:1]

    CASE ana OF

      'STP': BEGIN
        WIDGET_CONTROL, txtStpROIsz, GET_VALUE=ROIsz
        ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
        stpROI=getROIcircle(szImg, center, ROIsz) ;in a1_getROIs.pro
      END

      'HOMOG': BEGIN

        CASE modality OF
          0: BEGIN
            WIDGET_CONTROL, txtHomogROIsz, GET_VALUE=ROIsz
            ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
            WIDGET_CONTROL,  txtHomogROIdist, GET_VALUE=ROIdist
            ROIdist=ROUND(FLOAT(ROIdist(0))/pix(0)) ; assume x,y pix equal ! = normal
          END
          1:BEGIN
            WIDGET_CONTROL, txtHomogROIszX, GET_VALUE=ROIsz
            ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
            ROIdist=-1
          END

          4:BEGIN
            WIDGET_CONTROL, txtHomogROIszPET, GET_VALUE=ROIsz
            ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
            WIDGET_CONTROL,  txtHomogROIdistPET, GET_VALUE=ROIdist
            ROIdist=ROUND(FLOAT(ROIdist(0))/pix(0)) ; assume x,y pix equal ! = normal
          END
          ELSE:
        ENDCASE

        homogROIs=getHomogRois(szImg, imgCenterOffset, ROIsz, ROIdist, modality);in a1_getROIs.pro
      END; homog

      'NOISE': BEGIN

        CASE modality OF
          0:BEGIN
            WIDGET_CONTROL, txtNoiseROIsz, GET_VALUE=ROIsz
            ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
            noiseROI=getROIcircle(szImg, center, ROIsz);in a1_getROIs.pro
          END
          1:BEGIN
            noiseROI=INTARR(szImg)
            WIDGET_CONTROL, txtNoiseX, GET_VALUE=ROIperc
            p1=0.5*(1.-0.01*LONG(ROIperc(0)))
            p2=1.-p1
            noiseROI[p1*szImg(0):p2*(szImg(0)-1),p1*szImg(1):p2*(szImg(1)-1)]=1
          END
        ENDCASE
      END

      'HUWATER': BEGIN
        WIDGET_CONTROL, txtHUwaterROIsz, GET_VALUE=ROIsz
        ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
        HUwaterROI=getROIcircle(szImg, center, ROIsz);in a1_getROIs.pro
      END

      'ROI': BEGIN
        CASE modality OF
          0: BEGIN
            WIDGET_CONTROL, unitDeltaO_ROI_CT, GET_VALUE=offxyROI_unit
            IF offxyROI_unit THEN centerOff=center+offxyROI/pix ELSE centerOff=center+offxyROI
            WIDGET_CONTROL, typeROI, GET_VALUE= ROItype
          END
          1: BEGIN
            WIDGET_CONTROL, unitDeltaO_ROI_X, GET_VALUE=offxyROIX_unit
            IF offxyROIX_unit THEN centerOff=center+offxyROI/pix ELSE centerOff=center+offxyROI
            WIDGET_CONTROL, typeROIX, GET_VALUE= ROItype
          END
          ELSE:
        ENDCASE
        CASE ROItype OF
          0:BEGIN;circular
            CASE modality OF
              0: WIDGET_CONTROL, txtROIrad, GET_VALUE=ROIrad
              1: WIDGET_CONTROL, txtROIXrad, GET_VALUE=ROIrad
            ENDCASE
            ROIsz=ROUND(FLOAT(ROIrad(0))/pix(0)) ; assume x,y pix equal ! = normal
            ROIroi=getROIcircle(szImg, centerOff, ROIsz);in a1_getROIs.pro
          END
          1:BEGIN
            CASE modality OF
              0: BEGIN
                WIDGET_CONTROL, txtROIx, GET_VALUE=ROIx
                WIDGET_CONTROL, txtROIy, GET_VALUE=ROIy
              END
              1: BEGIN
                WIDGET_CONTROL, txtROIXx, GET_VALUE=ROIx
                WIDGET_CONTROL, txtROIXy, GET_VALUE=ROIy
              END
            ENDCASE
            ddx=FLOOR(0.5*FLOAT(ROIx(0))/pix(0))
            ddy=FLOOR(0.5*FLOAT(ROIy(0))/pix(1))
            ROIroi=INTARR(szImg)
            x1=MAX([0, centerOff(0)-ddx])
            x2=MIN([centerOff(0)+ddx, szImg(0)-1])
            y1=MAX([0, centerOff(1)-ddy])
            y2=MIN([centerOff(1)+ddy, szImg(1)-1])
            ROIroi[x1:x2,y1:y2]=1
          END
          2:BEGIN
            CASE modality OF
              0: BEGIN
                WIDGET_CONTROL, txtROIx, GET_VALUE=ROIx
                WIDGET_CONTROL, txtROIy, GET_VALUE=ROIy
                WIDGET_CONTROL, txtROIa, GET_VALUE=ROIa
              END
              1: BEGIN
                WIDGET_CONTROL, txtROIXx, GET_VALUE=ROIx
                WIDGET_CONTROL, txtROIXy, GET_VALUE=ROIy
                WIDGET_CONTROL, txtROIXa, GET_VALUE=ROIa
              END
            ENDCASE

            ddx=FLOOR(0.5*FLOAT(ROIx(0))/pix(0))
            ddy=FLOOR(0.5*FLOAT(ROIy(0))/pix(1))
            miniROI=INTARR(ddx*4+1,ddy*4+1)
            miniROI[ddx:ddx*3,ddy:ddy*3]=1
            miniROI=ROUND(ROT(miniROI, FLOAT(ROIa(0))))
            ROIroi=INTARR(szImg)
            x1=MAX([0, centerOff(0)-2*ddx]) & mx1=x1-(centerOff(0)-2*ddx)
            x2=MIN([centerOff(0)+2*ddx, szImg(0)-1]) & mx2=ddx*4-(centerOff(0)+2*ddx-x2)
            y1=MAX([0, centerOff(1)-2*ddy]) & my1=y1-(centerOff(1)-2*ddy)
            y2=MIN([centerOff(1)+2*ddy, szImg(1)-1]) & my2=ddy*4-(centerOff(1)+2*ddy-y2)
            ROIroi[x1:x2,y1:y2]=miniROI[mx1:mx2,my1:my2]

          END
        ENDCASE
      END

      'MTF': BEGIN
        dxya(3)=1 ; center option has to be used
        WIDGET_CONTROL, useDelta, SET_BUTTON=1
        dxya(2)=0; no rotation allowed
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')
      END

      'NPS': BEGIN
        dxya(3)=1 ; center option has to be used
        WIDGET_CONTROL, useDelta, SET_BUTTON=1
        dxya(2)=0; no rotation allowed
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')

        proceed=1
        CASE modality OF
          0: BEGIN
            WIDGET_CONTROL, txtNPSroiSz, GET_VALUE=ROIsz
            WIDGET_CONTROL, txtNPSroiDist, GET_VALUE=ROIdist
            WIDGET_CONTROL, txtNPSsubNN, GET_VALUE=subNN
            ROIsz=LONG(ROIsz(0)) & ROIdist=FLOAT(ROIdist(0)) & subNN=LONG(subNN(0))
            NPSrois=getNPSrois(SIZE(tempImg,/DIMENSIONS), dxya[0:1], ROIsz, ROUND(ROIdist/pix(0)), subNN)
            IF max(NPSrois) NE 1 THEN proceed=0
          END
          1: BEGIN
            WIDGET_CONTROL, txtNPSroiSzX, GET_VALUE=ROIsz
            WIDGET_CONTROL, txtNPSsubSzX, GET_VALUE=subSz
            ROIsz=LONG(ROIsz(0)) & subSz=LONG(subSz(0))
            subSzMM=pix(0)*ROIsz*subSz
            WIDGET_CONTROL, lblNPSsubSzMMX, SET_VALUE=STRING(subSzMM, FORMAT='(f0.1)')
          END
          ELSE:

        ENDCASE

      END

      'CTLIN': BEGIN
        WIDGET_CONTROL, txtLinROIradS, GET_VALUE=radS
        WIDGET_CONTROL, tblLin, GET_VALUE=linTable
        radS=ROUND(FLOAT(radS(0))/pix(0)); assume x,y pix equal ! = normal
        posTab=FLOAT(linTable[1:2,*])
        posTab[0,*]=ROUND(posTab[0,*]/pix(0)) & posTab[1,*]=ROUND(posTab[1,*]/pix(1))
        CTlinROIs=getSampleRois(szImg, imgCenterOffset, radS, posTab) ;in a1_getROIs.pro
        ;IF max(CTlinROIs) EQ 1 THEN ana='CTLIN' ELSE ana='NONE'
      END

      'SNI': BEGIN
        WIDGET_CONTROL, txtSNIAreaRatio, GET_VALUE=rat
        SNIroi=getSNIroi(tempImg, FLOAT(rat)) ;in a1_getROIs.pro
      END

      'UNIF': BEGIN
        WIDGET_CONTROL, txtUnifAreaRatio, GET_VALUE=rat
        unifROI=getUnifRoi(tempImg, FLOAT(rat)) ;in a1_getROIs.pro
      END

      'BAR': BEGIN
        WIDGET_CONTROL, txtBarROIsize, GET_VALUE=barROIsizeMM
        barROI=getBarROIs(tempImg, imgCenterOffset, FLOAT(barROIsizeMM(0))/pix(0)); assume x,y pix equal ! = normal ;in a1_getROIs.pro
      END

      'CONTRAST': BEGIN
        WIDGET_CONTROL, txtConR1SPECT, GET_VALUE=rad1
        WIDGET_CONTROL, txtConR2SPECT, GET_VALUE=rad2
        rad1=ROUND(FLOAT(rad1(0))/pix(0)) & rad2=ROUND(FLOAT(rad2(0))/pix(0)); assume x,y pix equal ! = normal
        conROIs=getConNMRois(szImg, imgCenterOffset, rad1,rad2)
      END

      'CROSSCALIB': BEGIN
        WIDGET_CONTROL, txtCrossROIsz, GET_VALUE=ROIsz
        ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
        crossROI=getROIcircle(szImg, center, ROIsz)
      END

      'RC': BEGIN
        rad1=37.0
        rad2=57.2
        rad1=ROUND(FLOAT(rad1(0)/2)/pix(0)) & rad2=ROUND(FLOAT(rad2(0))/pix(0)); assume x,y pix equal ! = normal
        rcROIsSph=getConNMRois(szImg, imgCenterOffset, rad1,rad2)
        rcROIback=getRCbackRois(szImg, imgCenterOffset, rad1, pix(0))
        rcROIs=INTARR(szImg(0), szImg(1), 6+12)
        rev=WIDGET_INFO(btnRCrev, /BUTTON_SET)
        IF rev THEN BEGIN
          rcROIs[*,*,0:5]=rcROIsSph[*,*,0:5]
        ENDIF ELSE BEGIN
          FOR i=0,5 DO rcROIs[*,*,i]=rcROIsSph[*,*,5-i]
        ENDELSE
        WIDGET_CONTROL,cwRCexclude, GET_VALUE=back
        exBack=ABS(back-1)
        FOR i=0, 11 DO rcROIback[*,*,i]=rcROIback[*,*,i]*exBack(i)
        rcROIs[*,*,6:17]=rcROIback
      END
      ELSE:
    ENDCASE; ana

  ENDIF ELSE BEGIN;no images loaded
    CTlinROIs=0 & CTlinROIpos=0 & homogROIs=0 & noiseROI=0 & HUwaterROI=0 & NPSrois=0 & conROIs=0 & crossROI=0 & rcROIs=0 & barROI=0 & unifROI=0 & SNIroi=0
  ENDELSE

end
