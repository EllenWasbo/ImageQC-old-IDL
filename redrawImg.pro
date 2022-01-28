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

pro redrawImg, viewpl, newActive
  COMPILE_OPT hidden
  COMMON VARI

  WIDGET_CONTROL, /HOURGLASS
  WIDGET_CONTROL, drawLarge, GET_VALUE = iDrawLarge
  WIDGET_CONTROL, zoomSlider, GET_VALUE=zoomFactor
  oModel=OBJ_NEW('IDLgrModel')

  tags=TAG_NAMES(structImgs)
  sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
  IF tags(0) NE 'EMPTY' AND sel NE -1 THEN BEGIN

    curMode=WIDGET_INFO(wTabModes, /TAB_CURRENT)
    sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)

    IF N_ELEMENTS(activeImg) EQ 1 THEN newActive=1

    tempStruct=structImgs.(sel)
    IF newActive THEN activeImg=readImg(tempStruct.filename, tempStruct.frameNo)
    updateROI, ANA=analyse

    nImg=N_TAGS(structImgs)
    sizeAct=SIZE(activeImg, /DIMENSIONS)
    IF N_ELEMENTS(sizeAct) EQ 2 THEN BEGIN
    tempAct=activeImg
    
    markedArr=INTARR(nImg)
    IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1

    maxSz=max(sizeAct[0:1])
    IF N_ELEMENTS(viewpl) EQ 1 THEN BEGIN
      ;viewpl=[0.,0.,maxSz,maxSz];[0.,0.,sizeAct[0:1]]
      center=sizeAct/2
      out=ROUND(maxSz/2./zoomFactor)
      IF zoomFactor GT 1. THEN center=center+dxya[0:1]       
      viewpl=[center-out,maxSz/zoomFactor,maxSz/zoomFactor];x,y,width,height
    ENDIF
    oView=OBJ_NEW('IDLgrView', VIEWPLANE_RECT =viewpl)

    imgCenterOffset=[0,0,0,0]
    IF dxya(3) EQ 1 THEN imgCenterOffset=dxya

    hideAnnot=WIDGET_INFO(btnHideAnnot, /BUTTON_SET)
    annot=1 & IF hideAnnot THEN annot=0

    ;display Img
    WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
    WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
    rangeWL=LONG([lower,upper])
    tempAct=adjustWindowLevel(tempAct, rangeWL)

    oPaletteCT=OBJ_NEW('IDLgrPalette')
    IF colTable EQ 0 THEN oPaletteCT->LoadCT, 0 ELSE oPaletteCT->LoadCT, coltable, FILE=thisPath+'data\colorsImageQC.tbl'
    oPaletteROI=OBJ_NEW('IDLgrPalette')
    oPaletteROI->LoadCT, 0
    oImageCT = OBJ_NEW('IDLgrImage', tempAct, PALETTE=oPaletteCT)
    oModel->Add, oImageCT

    mmCT=tempStruct.pix*sizeAct

    IF annot THEN BEGIN

      ;fileList=getListOpenFiles(structImgs,0,marked, markedMulti)
      IF tempStruct.zpos(0) NE -999. AND tempStruct.sliceThick GT 0. THEN textZpos='z = '+ STRING(tempStruct.zpos,FORMAT='(f0.3)') ELSE textZpos = ''

      oTextZ = OBJ_NEW('IDLgrText', textZpos, LOCATIONS = [2,10], COLOR = 255*([1,0,0]));[2,20] when above oTextAdr
      oModel->Add, oTextZ

      lineX= OBJ_NEW('IDLgrPolyline', COLOR = 255*([1,0,0]), LINESTYLE=1)
      lineY= OBJ_NEW('IDLgrPolyline', COLOR = 255*([1,0,0]), LINESTYLE=1)

;      IF analyse EQ 'ROI' THEN BEGIN; ROI
;        RESTORE, thisPath + 'data\thisROI.sav'
;        oModel->Add, thisROI
;      ENDIF

      analyseArr=['HOMOG', 'NOISE','HUWATER','ROI','STP','UNIF','SNI','BAR','CONTRAST','CROSSCALIB','RC','SNR','PIU','GHOST']
      IF analyseArr.HasValue(analyse) THEN BEGIN
        CASE analyse OF
          'HOMOG': ROIs=homogROIs
          'NOISE': ROIs=noiseROI
          'HUWATER': ROIs=HUwaterROI
          'ROI': ROIs=ROIroi
          'CROSSCALIB': ROIs=crossROI
          'STP': ROIs=stpROI
          'UNIF': ROIs=unifROI
          'SNI': ROIs=SNIroi
          'BAR': ROIs=barROI
          'CONTRAST': ROIs=conROIs
          'RC': ROIs=rcROIs
          'SNR': ROIs=SNR_ROI
          'PIU': ROIs=PIU_ROI
          'GHOST': ROIs=ghostMR_ROI
          ELSE:
        ENDCASE
        
        szROIs=SIZE(ROIs, /DIMENSIONS)

        IF N_ELEMENTS(ROIs) GT 1 THEN BEGIN
          colors=INTARR(3,9)
          IF analyse EQ 'HOMOG' OR analyse EQ 'SNI' OR analyse EQ 'BAR' THEN BEGIN
            colors[*,0]= [255,255,0]
            colors[*,1]= [255,0,0]
            colors[*,2]= [0,0,255]
            colors[*,3]= [0,255,0]
            colors[*,4]= [0,255,255]
            colors[*,5]= [255,0,255]
            colors[*,6]= [0,0,0]
            colors[*,7]= [255,255,255]
            colors[*,8]= [125,125,255]
          ENDIF ELSE colors[0,*]=255
          IF analyse NE 'SNI' OR analyse EQ 'ROI' OR analyse EQ 'GHOST' THEN BEGIN
            contour0=OBJ_NEW('IDLgrContour',ROIs[*,*,0],COLOR=colors[*,0], C_VALUE=0.5, N_LEVELS=1)
            oModel->Add, Contour0
          ENDIF

          IF analyse EQ 'HOMOG' OR analyse EQ 'CONTRAST' OR analyse EQ 'RC' OR analyse EQ 'SNI' OR analyse EQ 'BAR' OR analyse EQ 'GHOST' THEN BEGIN
            contour1=OBJ_NEW('IDLgrContour',ROIs[*,*,1],COLOR=colors[*,1], C_VALUE=0.5, N_LEVELS=1) & oModel->Add, Contour1
            contour2=OBJ_NEW('IDLgrContour',ROIs[*,*,2],COLOR=colors[*,2], C_VALUE=0.5, N_LEVELS=1) & oModel->Add, Contour2
            contour3=OBJ_NEW('IDLgrContour',ROIs[*,*,3],COLOR=colors[*,3], C_VALUE=0.5, N_LEVELS=1) & oModel->Add, Contour3
            IF szROIs(2) GT 4 THEN BEGIN
              contour4=OBJ_NEW('IDLgrContour',ROIs[*,*,4],COLOR=colors[*,4], C_VALUE=0.5, N_LEVELS=1) & oModel->Add, Contour4
            ENDIF
          ENDIF
          
          IF analyse EQ 'HOMOG' AND modality EQ 1 THEN BEGIN
            poss=INTARR(2,4)
            FOR ir=0, 3 DO poss[*,ir]=ROUND(centroid(ROIs[*,*,ir+1],0.5))
            poss[1,*]=poss[1,*]-7
            oText1 = OBJ_NEW('IDLgrText', ['1'], LOCATIONS =poss[*,0], COLOR = colors[*,1], ALIGNMENT=0.5)
            oText2 = OBJ_NEW('IDLgrText', ['2'], LOCATIONS =poss[*,1], COLOR = colors[*,2], ALIGNMENT=0.5)
            oText3 = OBJ_NEW('IDLgrText', ['3'], LOCATIONS =poss[*,2], COLOR = colors[*,3], ALIGNMENT=0.5)
            oText4 = OBJ_NEW('IDLgrText', ['4'], LOCATIONS =poss[*,3], COLOR = colors[*,4], ALIGNMENT=0.5)
            oModel->Add, oText1 & oModel->Add, oText2 & oModel->Add, oText3 & oModel->Add, oText4  
          ENDIF
          
          IF analyse EQ 'SNI' THEN BEGIN
            ;named ROIs
            temp=TOTAL(ROIs[*,*,0],2)
            temp2=WHERE(temp GT 0)
            firstX=temp2(0)
            lastX=temp2(-1)
            temp=TOTAL(ROIs[*,*,0],1)
            temp2=WHERE(temp GT 0)
            firstY=temp2(0)
            lastY=temp2(-1)
            
            poss=INTARR(2,8)
            poss[*,0]=[firstX+5,firstY+5]
            poss[*,1]=[lastX-5,firstY+5]
            FOR ir=2, 7 DO poss[*,ir]=ROUND(centroid(ROIs[*,*,ir+1],0.5))
            oTextL1= OBJ_NEW('IDLgrText', 'L1', LOCATIONS=poss[*,0], COLOR=colors[*,1])
            oTextL2= OBJ_NEW('IDLgrText', 'L2', LOCATIONS=poss[*,1], COLOR=colors[*,2], ALIGNMENT=1)
            oTextS1= OBJ_NEW('IDLgrText', 'S1', LOCATIONS=poss[*,2], COLOR=colors[*,3], ALIGNMENT=0.5)
            oTextS2= OBJ_NEW('IDLgrText', 'S2', LOCATIONS=poss[*,3], COLOR=colors[*,4], ALIGNMENT=0.5)
            oTextS = OBJ_NEW('IDLgrText', ['S3','S4','S5','S6'], LOCATIONS =poss[*,4:7], COLOR = colors[*,0], ALIGNMENT=0.5)
            oModel->Add, oTextL1 & oModel->Add, oTextL2 & oModel->Add, oTextS1 & oModel->Add, oTextS2 & oModel->Add, oTextS
          ENDIF

          IF analyse EQ 'BAR' THEN BEGIN
            ;numbered ROIs
            poss=INTARR(2,4)
            FOR ig=0, 3 DO poss[*,ig]=ROUND(centroid(ROIs[*,*,ig],0.5))
            labels=['1 lowest freq','2','3','4 highest freq']
            oText = OBJ_NEW('IDLgrText', labels, LOCATIONS =poss, COLOR = 255*([1,0,0]))
            oModel->Add, oText
          ENDIF

          IF analyse EQ 'CONTRAST' OR analyse EQ 'RC' THEN BEGIN
            contour5=OBJ_NEW('IDLgrContour',ROIs[*,*,5],COLOR=colors[*,0], C_VALUE=0.5, N_LEVELS=1)
            oModel->Add, Contour5

            ;numbered ROIs
            poss=INTARR(2,6)
            FOR ig=0, 5 DO poss[*,ig]=ROUND(centroid(ROIs[*,*,ig],0.5))
            oText = OBJ_NEW('IDLgrText', STRING(INDGEN(6)+1,FORMAT='(i0)'), LOCATIONS =poss, COLOR = 255*([1,0,0]))
            oModel->Add, oText

            bgcol=255*([0,0,1])
            contour6=OBJ_NEW('IDLgrContour',ROIs[*,*,6],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
            oModel->Add, Contour6
            IF analyse EQ 'RC' THEN BEGIN;background
              contourb1=OBJ_NEW('IDLgrContour',ROIs[*,*,6+1],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb2=OBJ_NEW('IDLgrContour',ROIs[*,*,6+2],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb3=OBJ_NEW('IDLgrContour',ROIs[*,*,6+3],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb4=OBJ_NEW('IDLgrContour',ROIs[*,*,6+4],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb5=OBJ_NEW('IDLgrContour',ROIs[*,*,6+5],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb6=OBJ_NEW('IDLgrContour',ROIs[*,*,6+6],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb7=OBJ_NEW('IDLgrContour',ROIs[*,*,6+7],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb8=OBJ_NEW('IDLgrContour',ROIs[*,*,6+8],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb9=OBJ_NEW('IDLgrContour',ROIs[*,*,6+9],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb10=OBJ_NEW('IDLgrContour',ROIs[*,*,6+10],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              contourb11=OBJ_NEW('IDLgrContour',ROIs[*,*,6+11],COLOR=bgcol, C_VALUE=0.5, N_LEVELS=1)
              oModel->Add, Contourb1 & oModel->Add, Contourb2 & oModel->Add, Contourb3 & oModel->Add, Contourb4 & oModel->Add, Contourb5 & oModel->Add, Contourb6
              oModel->Add, Contourb7 & oModel->Add, Contourb8 & oModel->Add, Contourb9 &  oModel->Add, Contourb10 & oModel->Add, Contourb11
            ENDIF

          ENDIF
        ENDIF
      ENDIF

      IF analyse EQ 'CTLIN' AND N_ELEMENTS(CTlinROIs) NE 1 THEN BEGIN
        ;search ROIs
        szROIs=SIZE(CTlinROIs, /DIMENSIONS)
        IF N_ELEMENTS(szROIs) GT 2 THEN searchROIall=TOTAL(CTlinROIs,3) ELSE  searchROIall=CTlinROIs
        searchROIall=TOTAL(CTlinROIs,3)
        contour0=OBJ_NEW('IDLgrContour',searchROIall,COLOR=255*([1,0,0]), C_VALUE=0.5, N_LEVELS=1)
        oModel->Add, Contour0

        ;actual calculation ROIs
        IF N_ELEMENTS(CTlinROIpos) GT 1 THEN BEGIN
          WIDGET_CONTROL, txtLinROIrad, GET_VALUE=rad1
          rad1=ROUND(FLOAT(rad1(0))/tempStruct.pix(0))
          statROIs=getSampleRois(sizeAct, [-sizeAct(0)/2,-sizeAct(1)/2,0,0], rad1, CTlinROIpos)
          statROIall=TOTAL(statROIs,3)
          contour1=OBJ_NEW('IDLgrContour',statROIall,COLOR=255*([0,0,1]), C_VALUE=0.5, N_LEVELS=1)
          oModel->Add, Contour1
        ENDIF

        ; material names
        WIDGET_CONTROL, tblLin, GET_VALUE=linTable
        szT=SIZE(linTable, /DIMENSIONS)
        poss=INTARR(2,szT(1))
        FOR ig=0, szT(1)-1 DO poss[*,ig]=ROUND(centroid(CTlinROIs[*,*,ig],0.5))
        oTextLin = OBJ_NEW('IDLgrText', TRANSPOSE(linTable[0,*]), LOCATIONS =poss, COLOR = 255*([1,0,0]))
        oModel->Add, oTextLin
      ENDIF

      IF analyse EQ 'SLICETHICK' THEN BEGIN; slice thickness ramps
        CASE modality OF
          0: BEGIN;CT
            WIDGET_CONTROL, txtRampDist, GET_VALUE=rampDist
            rampDist=ROUND(FLOAT(rampDist(0))/tempStruct.pix(0)) ; assume x,y pix equal ! = normal
            WIDGET_CONTROL, txtRampLen, GET_VALUE=len
            len=ROUND(FLOAT(len(0))/tempStruct.pix(0)) ; assume x,y pix equal ! = normal
            WIDGET_CONTROL, cw_ramptype, GET_VALUE=ramptype

            Case ramptype OF
              0: ramps=getRamps(sizeAct, imgCenterOffset, rampDist, len)
              1: BEGIN
                ramps=getRamps(sizeAct, imgCenterOffset, 45./tempStruct.pix(0), len)
                ramps2=getRamps(sizeAct, imgCenterOffset, 25./tempStruct.pix(0), len)
              END
              2: BEGIN
                ramps=getRamps(sizeAct, imgCenterOffset, rampDist, len)
                ramps[*,0:1]=0
              END
            Endcase

            WIDGET_CONTROL, txtRampSearch, GET_VALUE=nRamps
            nRamps=LONG(nRamps(0))

            IF TOTAL(ramps[*,0:1]) NE 0 THEN BEGIN
              H1=OBJ_NEW('IDLgrPolyline', [ramps[0,0],ramps[2,0]],[ramps[1,0]+nRamps,ramps[3,0]+nRamps], COLOR = 255*([0,0,0]), LINESTYLE=0)
              H1_1=OBJ_NEW('IDLgrPolyline', [ramps[0,0],ramps[2,0]],[ramps[1,0]-nRamps,ramps[3,0]-nRamps], COLOR = 255*([0,0,0]), LINESTYLE=0)
              H2=OBJ_NEW('IDLgrPolyline', [ramps[0,1],ramps[2,1]],[ramps[1,1]+nRamps,ramps[3,1]+nRamps], COLOR = 255*([0,1,0]), LINESTYLE=0)
              H2_1=OBJ_NEW('IDLgrPolyline', [ramps[0,1],ramps[2,1]],[ramps[1,1]-nRamps,ramps[3,1]-nRamps], COLOR = 255*([0,1,0]), LINESTYLE=0)
              oModel->Add, H1 & oModel->Add, H1_1 & oModel->Add, H2 & oModel->Add, H2_1
            ENDIF
            V1=OBJ_NEW('IDLgrPolyline', [ramps[0,2]+nRamps,ramps[2,2]+nRamps],[ramps[1,2],ramps[3,2]], COLOR = 255*([0,0,1]), LINESTYLE=0)
            V1_1=OBJ_NEW('IDLgrPolyline', [ramps[0,2]-nRamps,ramps[2,2]-nRamps],[ramps[1,2],ramps[3,2]], COLOR = 255*([0,0,1]), LINESTYLE=0)
            V2=OBJ_NEW('IDLgrPolyline', [ramps[0,3]+nRamps,ramps[2,3]+nRamps],[ramps[1,3],ramps[3,3]], COLOR = 255*([1,0,0]), LINESTYLE=0)
            V2_1=OBJ_NEW('IDLgrPolyline', [ramps[0,3]-nRamps,ramps[2,3]-nRamps],[ramps[1,3],ramps[3,3]], COLOR = 255*([1,0,0]), LINESTYLE=0)
            oModel->Add, V1 & oModel->Add, V1_1 & oModel->Add, V2 & oModel->Add, V2_1
            IF  ramptype EQ 1 THEN BEGIN
              V3=OBJ_NEW('IDLgrPolyline', [ramps2[0,2]+nRamps,ramps2[2,2]+nRamps],[ramps2[1,2],ramps2[3,2]], COLOR = 255*([1,0,1]), LINESTYLE=0)
              V3_1=OBJ_NEW('IDLgrPolyline', [ramps2[0,2]-nRamps,ramps2[2,2]-nRamps],[ramps2[1,2],ramps2[3,2]], COLOR = 255*([1,0,1]), LINESTYLE=0)
              V4=OBJ_NEW('IDLgrPolyline', [ramps2[0,3]+nRamps,ramps2[2,3]+nRamps],[ramps2[1,3],ramps2[3,3]], COLOR = 255*([0,1,1]), LINESTYLE=0)
              V4_1=OBJ_NEW('IDLgrPolyline', [ramps2[0,3]-nRamps,ramps2[2,3]-nRamps],[ramps2[1,3],ramps2[3,3]], COLOR = 255*([0,1,1]), LINESTYLE=0)
              oModel->Add, V3 & oModel->Add, V3_1 & oModel->Add, V4 & oModel->Add, V4_1
            ENDIF
            END
          5: BEGIN; MR
            contour0=OBJ_NEW('IDLgrContour',sliceMR_ROI[*,*,0],COLOR=255*([1,0,0]), C_VALUE=0.5, N_LEVELS=1)
            contour1=OBJ_NEW('IDLgrContour',sliceMR_ROI[*,*,1],COLOR=255*([1,0,0]), C_VALUE=0.5, N_LEVELS=1)
            oModel->Add, Contour0 & oModel->Add, Contour1
            END
          ELSE:
        ENDCASE
        
      ENDIF

      IF analyse EQ 'DIM' AND N_ELEMENTS(dimRes) GT 1 AND markedArr(sel) EQ 1 THEN BEGIN; linear dimensions - rod positions
        posX=dimRes[6:9, sel]+sizeAct(0)/2+imgCenterOffset(0)
        posY=dimRes[10:13, sel]+sizeAct(1)/2+imgCenterOffset(1)
        dimLine=OBJ_NEW('IDLgrPolyline', [[posX(0),posY(0)],[posX(1),posY(1)],[posX(2),posY(2)],[posX(3),posY(3)],[posX(0),posY(0)]], COLOR = 255*([1,0,0]), /DOUBLE, LINESTYLE=0)
        oModel->Add, dimLine ;& oModel->Add, H2 & oModel->Add, V1 & oModel->Add, V2 & oModel->Add, D1 & oModel->Add, D2
      ENDIF

      IF analyse EQ 'FWHM' THEN BEGIN
        center=sizeAct/2+imgCenterOffset[0:1]
        centerB=center
        centerB(1)=center(1)+ROUND(sizeAct(1)/4)
        fwhmLine=OBJ_NEW('IDLgrPolyline', [[center(0)-15,center(1)-5],[center(0)-15,center(1)+4],[center(0)+15,center(1)+4],[center(0)+15,center(1)-5],[center(0)-15,center(1)-5]], COLOR = 255*([1,0,0]), /DOUBLE, LINESTYLE=0)
        fwhmBackLine=OBJ_NEW('IDLgrPolyline', [[centerB(0)-15,centerB(1)-5],[centerB(0)-15,centerB(1)+4],[centerB(0)+15,centerB(1)+4],[centerB(0)+15,centerB(1)-5],[centerB(0)-15,centerB(1)-5]], COLOR = 255*([1,0,0]), /DOUBLE, LINESTYLE=0)
        oModel->Add, fwhmLine & oModel->Add, fwhmBackLine
        oTextBG = OBJ_NEW('IDLgrText', 'Background', LOCATIONS = [centerB(0)+20,centerB(1)], COLOR = 255*([1,0,0]))
        oModel->Add, oTextBG
      ENDIF

      IF analyse EQ 'VARI' THEN BEGIN
        center=sizeAct/2+imgCenterOffset[0:1]
        WIDGET_CONTROL, txtVarImageROIsz, GET_VALUE=ROIszMM
        ROIrad=ROUND((FLOAT(ROIszMM(0))/2)/tempstruct.pix(0))
        roiLine=OBJ_NEW('IDLgrPolyline', [[center(0)-ROIrad,center(1)-ROIrad],[center(0)-ROIrad,center(1)+ROIrad],[center(0)+ROIrad,center(1)+ROIrad],$
          [center(0)+ROIrad,center(1)-ROIrad],[center(0)-ROIrad,center(1)-ROIrad]], COLOR = 255*([1,0,0]), /DOUBLE, LINESTYLE=0)
        oModel->Add, roiLine
      ENDIF

      IF analyse EQ 'SCANSPEED' THEN BEGIN
        WIDGET_CONTROL, txtNAvgSpeedNM, GET_VALUE=val
        val=LONG(val(0))
        val=val/2-1
        IF val LT 0 THEN val=0
        WIDGET_CONTROL, txtSpeedROIheight, GET_VALUE=valh
        height=FLOAT(valh(0))*10./tempstruct.pix(1)
        IF dxya(1)+sizeAct(1)/2 GE height/2 THEN first=dxya(1)+sizeAct(1)/2-height/2 ELSE first=0
        IF dxya(1)+sizeAct(1)/2+height/2 LT sizeAct(1)-1 THEN last=dxya(1)+sizeAct(1)/2+height/2 ELSE last=sizeAct(1)-1
        posX=sizeAct(0)/2+dxya(0)
        speedLine=OBJ_NEW('IDLgrPolyline', [[posX-val,first],[posX-val,last],[posX+val,last],[posX+val,first],[posX-val,first]], COLOR = 255*([1,0,0]), /DOUBLE, LINESTYLE=0)
        oModel->Add, speedLine
      ENDIF
      
      halfSz=sizeAct/2

      IF analyse EQ 'RING' THEN BEGIN; ring artifact analysis - crosshair at image center and circular ring with radius equal to ringStop
        lineX2= OBJ_NEW('IDLgrPolyline', COLOR = 255*([1,0,0]), LINESTYLE=0)
        lineY2= OBJ_NEW('IDLgrPolyline', COLOR = 255*([1,0,0]), LINESTYLE=0)
        lineX2->SetProperty, DATA=[[0,halfSz(1)],[sizeAct(0),halfSz(1)]]
        lineY2->SetProperty, DATA=[[halfSz(0),0],[halfSz(0),sizeAct(1)]]
        oModel->Add, lineX2 & oModel->Add, lineY2
        WIDGET_CONTROL, txtRingStart, GET_VALUE=ringStart
        WIDGET_CONTROL, txtRingStop, GET_VALUE=ringStop
        circStartStop=getROIcircle(sizeAct, halfSz, FLOAT(ringStop(0))/tempStruct.pix(0))-getROIcircle(sizeAct, halfSz, FLOAT(ringStart(0))/tempStruct.pix(0))
        contour0=OBJ_NEW('IDLgrContour',circStartStop,COLOR=255*([1,0,0]), C_VALUE=0.5, N_LEVELS=1)
        oModel->Add, Contour0
      ENDIF

   
      IF dxya(3) EQ 1 THEN BEGIN
        tana=TAN(dxya(2)*!DtoR)
        dy1=tana*(halfSz(0)+dxya(0))
        dy2=tana*(halfSz(0)-dxya(0))
        dx1=tana*(halfSz(1)+dxya(1))
        dx2=tana*(halfSz(1)-dxya(1))
        lineX->SetProperty, DATA=[[0,halfSz(1)+dxya(1)+dy1],[sizeAct(0),halfSz(1)+dxya(1)-dy2]]
        lineY->SetProperty, DATA=[[halfSz(0)+dxya(0)-dx1,0],[halfSz(0)+dxya(0)+dx2,sizeAct(1)]]
        IF analyse EQ 'MTF' THEN BEGIN; dxya(2) = 0 & dxya(3)=1
          
          CASE curMode OF

            0: BEGIN;CT
              WIDGET_CONTROL, unitDeltaO_MTF_CT, GET_VALUE=unitOffset
              IF unitOffset THEN dxyaO=dxya[0:1]+offxyMTF/tempStruct.pix ELSE dxyaO=dxya[0:1]+offxyMTF
              WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=ROIsz
              ROIsz=ROUND(ROIsz(0)/tempStruct.pix)
              WIDGET_CONTROL, cw_typeMTF, GET_VALUE=typeMTF
              IF WIDGET_INFO(btnSearchMaxMTF, /BUTTON_SET) AND ROIsz(0) GT 0 THEN BEGIN
                ;search for max in image
                CASE typeMTF OF
                  0: BEGIN;bead
                    halfmax=0.5*(MAX(ABS(activeImg)));+MIN(activeImg))
                    centerPos=ROUND(centroid(activeImg, halfmax, 0))
                    END
                  1:BEGIN;wire
                    halfmax=0.5*(MAX(activeImg)+MIN(activeImg))
                    centerPos=ROUND(centroid(activeImg, halfmax))
                    END
                  2:BEGIN;circular edge
                    ;firstguess: center = as defined, search for centroid within ROI size
                    ;mx=MAX(activeImg, loc)
                    ;ind=ARRAY_INDICES(activeImg, loc)
                    ;xx1=ind(0)-ROIsz(0) & xx2=ind(0)+ROIsz(0)
                    ;IF xx1 LT 0 THEN xx1=0 & IF xx2 GT sizeAct(0)-1 THEN xx2=sizeAct(0)-1
                    ;yy1=ind(1)-ROIsz(0) & yy2=ind(1)+ROIsz(0)
                    ;IF yy1 LT 0 THEN yy1=0 & IF yy2 GT sizeAct(1)-1 THEN yy2=sizeAct(1)-1
                    x1=ROUND(halfSz(0)+dxyaO(0)-ROIsz(0)) & x2=ROUNd(halfSz(0)+dxyaO(0)+ROIsz(0))
                    y1=ROUND(halfSz(1)+dxyaO(1)-ROIsz(0)) & y2=ROUND(halfSz(1)+dxyaO(1)+ROIsz(0))
                    IF x1 GE 0 AND y1 GE 0 AND x2 LT sizeAct(0) AND y2 LT sizeAct(1) THEN BEGIN
                      subma=activeImg[x1:x2,y1:y2]
                      centerPosSubma=ROUND(centroid(subma, MIN(subma)))
                      centerPos=[x1,y1]+centerPosSubma
                    ENDIF ELSE centerPos=[-1,-1]
                  END
                  ELSE:
                ENDCASE         
                x1=centerPos(0)-ROIsz(0) & x2=centerPos(0)+ROIsz(0)
                y1=centerPos(1)-ROIsz(0) & y2=centerPos(1)+ROIsz(0)
              ENDIF ELSE BEGIN
                x1=halfSz(0)+dxyaO(0)-ROIsz(0) & x2=halfSz(0)+dxyaO(0)+ROIsz(0)
                y1=halfSz(1)+dxyaO(1)-ROIsz(0) & y2=halfSz(1)+dxyaO(1)+ROIsz(0)
              ENDELSE
              lineROI = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI
              
              IF typeMTF EQ 0 THEN BEGIN
                y1=y1-(ROIsz(0)*2+1) & y2=y2-(ROIsz(0)*2+1)
                lineROI2 = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
                oModel->Add, lineROI2
                oTextBG = OBJ_NEW('IDLgrText', 'Background', LOCATIONS = [x1,y1], COLOR = 255*([1,0,0]))
                oModel->Add, oTextBG
              ENDIF
            END
            1: BEGIN;xray
              WIDGET_CONTROL, unitDeltaO_MTF_X, GET_VALUE=unitOffset
              IF unitOffset THEN dxyaO=dxya[0:1]+offxyMTF_X/tempStruct.pix ELSE dxyaO=dxya[0:1]+offxyMTF_X
              WIDGET_CONTROL, txtMTFroiSzX, GET_VALUE=ROIszX
              WIDGET_CONTROL, txtMTFroiSzY, GET_VALUE=ROIszY
              ROIsz=ROUND([ROIszX(0),ROIszY(0)]/(2.*tempStruct.pix))
              x1=halfSz(0)+dxyaO(0)-ROIsz(0) & x2=halfSz(0)+dxyaO(0)+ROIsz(0)
              y1=halfSz(1)+dxyaO(1)-ROIsz(1) & y2=halfSz(1)+dxyaO(1)+ROIsz(1)
              lineROI = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI
            END
            2: BEGIN;NM planar
              WIDGET_CONTROL, txtMTFroiSzXNM, GET_VALUE=ROIszX
              WIDGET_CONTROL, txtMTFroiSzYNM, GET_VALUE=ROIszY
              ROIsz=ROUND([ROIszX(0),ROIszY(0)]/(2.*tempStruct.pix))
              x1=halfSz(0)+dxya(0)-ROIsz(0) & x2=halfSz(0)+dxya(0)+ROIsz(0)
              y1=halfSz(1)+dxya(1)-ROIsz(1) & y2=halfSz(1)+dxya(1)+ROIsz(1)
              lineROI = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI
            END
            3: BEGIN;SPECT
              WIDGET_CONTROL, txtMTFroiSzSPECT, GET_VALUE=ROIsz
              ROIsz=ROUND(ROIsz(0)/(2.*tempStruct.pix))
              x1=halfSz(0)+dxya(0)-ROIsz(0) & x2=halfSz(0)+dxya(0)+ROIsz(0)
              y1=halfSz(1)+dxya(1)-ROIsz(0) & y2=halfSz(1)+dxya(1)+ROIsz(0)
              lineROI = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI
            END
            ELSE:
          ENDCASE
        ENDIF
        IF analyse EQ 'NPS' THEN BEGIN
          CASE curMode OF
            0: BEGIN; CT
              IF N_ELEMENTS(NPSrois) NE 1 THEN BEGIN
                oImageROIs = OBJ_NEW('IDLgrImage', 50*total(NPSrois,3)+50 , BLEND_FUNCTION = [3, 4], ALPHA_CHANNEL=0.5, PALETTE=oPaletteROI)
                oModel->Add, oImageROIs
              ENDIF
            END
            1: BEGIN; xray
              WIDGET_CONTROL, txtNPSroiSzX, GET_VALUE=ROIsz
              WIDGET_CONTROL, txtNPSsubSzX, GET_VALUE=subSz
              ROIsz=LONG(ROIsz(0)) & subSz=LONG(subSz(0))
              x1=halfSz(0)+dxya(0)-ROIsz*subSz*0.5 & x2=halfSz(0)+dxya(0)+ROIsz*subSz*0.5
              y1=halfSz(1)+dxya(1)-ROIsz*subSz*0.5 & y2=halfSz(1)+dxya(1)+ROIsz*subSz*0.5
              lineROI = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI
              x1=halfSz(0)+dxya(0)-ROIsz*0.5 & x2=halfSz(0)+dxya(0)+ROIsz*0.5
              y1=halfSz(1)+dxya(1)-ROIsz*0.5 & y2=halfSz(1)+dxya(1)+ROIsz*0.5
              lineROI2 = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI2
              x1=halfSz(0)+dxya(0) & x2=halfSz(0)+dxya(0)+ROIsz
              y1=halfSz(1)+dxya(1) & y2=halfSz(1)+dxya(1)+ROIsz
              lineROI3= OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=1)
              oModel->Add, lineROI3
            END
            2:
            ELSE:
          ENDCASE

        ENDIF
        
        IF analyse EQ 'PIU' THEN BEGIN
            IF N_ELEMENTS(PIUres) GT 0 THEN BEGIN
              szROI2=ROUND(5./tempStruct.pix(0));1cm^2 ROIs to evaluate
              pos=PIUres[3:4,sel]
              x1=pos(0)-szROI2 & x2=pos(0)+szROI2
              y1=pos(1)-szROI2 & y2=pos(1)+szROI2
              lineROI = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI
              oTextL1= OBJ_NEW('IDLgrText', 'min', LOCATIONS=[x2+2,y2], COLOR = 255*([1,0,0]))
              oModel->Add, oTextL1
              pos=PIUres[5:6,sel]
              x1=pos(0)-szROI2 & x2=pos(0)+szROI2
              y1=pos(1)-szROI2 & y2=pos(1)+szROI2
              lineROI2 = OBJ_NEW('IDLgrPolyline', [[x1,y1],[x2,y1],[x2,y2],[x1,y2],[x1,y1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, lineROI2
              oTextL2= OBJ_NEW('IDLgrText', 'max', LOCATIONS=[x2+2,y2], COLOR = 255*([1,0,0]))
              oModel->Add, oTextL2
            ENDIF
        ENDIF
        
        IF analyse EQ 'GEOMDIST' THEN BEGIN
          IF N_ELEMENTS(GeomDistRes) GT 0 THEN BEGIN
            cent=GeomDistRes[8:9,sel]
            IF ~ARRAY_EQUAL([-1.,-1.],cent) THEN BEGIN
              V1=OBJ_NEW('IDLgrPolyline', [[0,cent(1)],[sizeAct(0)-1,cent(1)]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              V1_1=OBJ_NEW('IDLgrPolyline',[[cent(0),0],[cent(0),sizeAct(1)-1]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              nPixXfwhm=GeomDistRes[0,sel]/tempStruct.pix(0)
              nPixDiag2=CEIL(nPixXfwhm/2*1.1)
              V2=OBJ_NEW('IDLgrPolyline',[[cent(0)-nPixDiag2,cent(1)-nPixDiag2],[cent(0)+nPixDiag2,cent(1)+nPixDiag2]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              V2_1=OBJ_NEW('IDLgrPolyline',[[cent(0)-nPixDiag2,cent(1)+nPixDiag2],[cent(0)+nPixDiag2,cent(1)-nPixDiag2]], COLOR = 255*([1,0,0]), LINESTYLE=0)
              oModel->Add, V1 & oModel->Add, V1_1 & oModel->Add, V2 & oModel->Add, V2_1
              
              ;add true size circle
              WIDGET_CONTROL, txtGD_MR_act, GET_VALUE=wAct
              ROIsz=0.5*(FLOAT(wAct(0))/tempStruct.pix(0))
              actCircle=getROIcircle(sizeAct, cent+1, ROIsz);cent+1 should be cent, but by visual inspection..... Todo...
              contour0=OBJ_NEW('IDLgrContour',actCircle[*,*],COLOR=255*([1,0,0]), C_VALUE=0.5, N_LEVELS=1)
              oModel->Add, Contour0
            ENDIF
          ENDIF
        ENDIF
        
      ENDIF ELSE BEGIN
        lineX->SetProperty, DATA=[[0,halfSz(1)],[sizeAct(0)-1,halfSz(1)]]
        lineY->SetProperty, DATA=[[halfSz(0),0],[halfSz(0),sizeAct(1)-1]]
      ENDELSE

      oModel->Add, lineX & oModel->Add, lineY
    ENDIF;annot

    oView->Add,oModel

    iDrawLarge->Draw,oView

    OBJ_DESTROY, oPaletteCT
    IF annot THEN BEGIN
      OBJ_DESTROY, oTextZ & OBJ_DESTROY,lineX & OBJ_DESTROY,lineY

      IF OBJ_VALID(contour0) THEN BEGIN
        OBJ_DESTROY, contour0
        IF OBJ_VALID(contour1) THEN OBJ_DESTROY, contour1
        IF OBJ_VALID(contour2) THEN OBJ_DESTROY, contour2
        IF OBJ_VALID(contour3) THEN OBJ_DESTROY, contour3
        IF OBJ_VALID(contour4) THEN OBJ_DESTROY, contour4
        IF OBJ_VALID(contour5) THEN OBJ_DESTROY, contour5
        IF OBJ_VALID(contour6) THEN OBJ_DESTROY, contour6
        IF OBJ_VALID(contour7) THEN OBJ_DESTROY, contour7
        IF OBJ_VALID(contour8) THEN OBJ_DESTROY, contour8
      ENDIF

      IF OBJ_VALID(lineROI) THEN BEGIN;2?
        OBJ_DESTROY, lineROI
        IF OBJ_VALID(lineROI2) THEN BEGIN
          OBJ_DESTROY, lineROI2
          IF OBJ_VALID(lineROI3) THEN OBJ_DESTROY, lineROI3
        ENDIF
      ENDIF

      IF OBJ_VALID(V1) THEN BEGIN
        OBJ_DESTROY, V1 &  OBJ_DESTROY, V1_1 & OBJ_DESTROY, V2 & OBJ_DESTROY, V2_1
        IF OBJ_VALID(H1) THEN BEGIN
          OBJ_DESTROY, H1 & OBJ_DESTROY, H1_1 & OBJ_DESTROY, H2 & OBJ_DESTROY, H2_1
        ENDIF
        IF OBJ_VALID(V3) THEN BEGIN
          OBJ_DESTROY, V3 &  OBJ_DESTROY, V3_1 & OBJ_DESTROY, V4 & OBJ_DESTROY, V4_1
        ENDIF
      ENDIF

      IF OBJ_VALID(oTextL1) THEN BEGIN
        OBJ_DESTROY, oTextL1 & OBJ_DESTROY, oTextL2 
        IF OBJ_VALID(oTextS1) THEN BEGIN
          OBJ_DESTROY, oTextS1 & OBJ_DESTROY, oTextS2 & OBJ_DESTROY, oTextS
        ENDIF
      ENDIF

      IF OBJ_VALID(dimLine) THEN OBJ_DESTROY, dimLine
      IF OBJ_VALID(fwhmLine) THEN OBJ_DESTROY, fwhmLine
      IF OBJ_VALID(fwhmBackLine) THEN OBJ_DESTROY, fwhmBackLine
      IF OBJ_VALID(speedLine) THEN OBJ_DESTROY, speedLine
      IF OBJ_VALID(roiLine) THEN OBJ_DESTROY, roiLine
      IF OBJ_VALID(posLineX) THEN BEGIN
        OBJ_DESTROY, posLineX & OBJ_DESTROY, posLineY
      ENDIF
    ENDIF;annot
    OBJ_DESTROY, oView & OBJ_DESTROY, oModel & OBJ_DESTROY, oImageCT
    ENDIF ELSE iDrawLarge.erase;N_ELEMENTS(sizeAct) NE 2 - rgb?
  ENDIF ELSE iDrawLarge.erase

end
