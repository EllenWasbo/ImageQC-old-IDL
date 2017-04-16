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

;Placing rois for homogeneity 5 circles (center + 4 outer)
;
;imgSize = size of active image
;imgCenterOffset = shift from the default imSz/2 (same as dxya if in use) [dx, dy, dAngle, (not used here)]
;ROIsZ = radius of the circles (pix)
;ROIdist = radius from center of image to center of circles (pix)

function getHomogRois, imgSize, imgCenterOffset, ROIsz, ROIdist, mode

  roiAll=INTARR(imgSize(0),imgSize(1),5)

  center=imgSize/2+imgCenterOffset[0:1]
  centers=INTARR(2,5); centerpositions x,y for all circles
  
  CASE mode OF
    1:BEGIN ; Xray
      dx=imgSize(0)/4
      dy=imgSize(1)/4
      centers[0:1,0]=center
      centers[0:1,1]=center+[-dx,dy];upper left
      centers[0:1,2]=center+[-dx,-dy];lower left
      centers[0:1,3]=center+[dx,dy];upper right
      centers[0:1,4]=center+[dx,-dy];lower right
      END
    2:BEGIN ; NM
        dx=ROIdist(0)
        IF N_ELEMENTS(ROIdist) EQ 2 THEN BEGIN ; planar wb
          dy=ROIdist(1)       
          centers[0:1,0]=center+[-dx,-dy];lower left
          centers[0:1,1]=center+[dx,-dy];lower right
          centers[0:1,2]=center
          centers[0:1,3]=center+[-dx,dy];upper left
          centers[0:1,4]=center+[dx,dy];upper right
        ENDIF ELSE BEGIN
          daRad=imgCenterOffset(2)/!radeg
          dd=ROIdist*[cos(daRad),sin(daRad)]

          centers[0:1,0]=center
          centers[0:1,1]=center+[dd(1),dd(0)];12 o'clock
          centers[0:1,2]=center+[dd(0),-dd(1)];15 o'clock
          centers[0:1,3]=center+[-dd(1),-dd(0)];18 o'clock
          centers[0:1,4]=center+[-dd(0),dd(1)];21 o'clock
        ENDELSE
      END
    ELSE: BEGIN ;CT/PET
        daRad=imgCenterOffset(2)/!radeg
        dd=ROIdist*[cos(daRad),sin(daRad)]

        centers[0:1,0]=center
        centers[0:1,1]=center+[dd(1),dd(0)];12 o'clock
        centers[0:1,2]=center+[dd(0),-dd(1)];15 o'clock
        centers[0:1,3]=center+[-dd(1),-dd(0)];18 o'clock
        centers[0:1,4]=center+[-dd(0),dd(1)];21 o'clock
      END
  ENDCASE


  FOR i= 0,4 DO roiAll[*,*,i]=getROIcircle(imgSize, centers[0:1,i], ROIsz)

  return, roiAll
end

;Placing ramps for slice thickness 4 lines
;
;imgSize = size of active CT image
;offset = shift from the default imSz/2 (same as dxya if in use) [dx, dy, dAngle, (0)] in pix or deg
;dist = distance from center to ramps (in pix)
;len = length of ramps (pix)

;return lines= INTARR(4,4) X1, Y1, X2, Y2 for each of the lines H-top,H-bottom,V1,V2

function getRamps, imgSize, offset, dista, len

  lines=INTARR(4,4)
  daRad=offset(2)/!radeg
  len2=ROUND(0.5*len)
  imSz2=ROUND(0.5*imgSize)

  ;first x1x2y1y2
  IF offset(2) EQ 0 THEN BEGIN
    ;H1
    lines[0:1,0]=imSz2(0)+offset(0)+[-1,1]*len2
    lines[2:3,0]=imSz2(1)+offset(1)+[1,1]*dista
    ;H2
    lines[0:1,1]=imSz2(0)+offset(0)+[-1,1]*len2
    lines[2:3,1]=imSz2(1)+offset(1)-[1,1]*dista
    ;V1
    lines[0:1,2]=imSz2(0)+offset(0)-[1,1]*dista
    lines[2:3,2]=imSz2(1)+offset(1)+[-1,1]*len2
    ;V2
    lines[0:1,3]=imSz2(0)+offset(0)+[1,1]*dista
    lines[2:3,3]=imSz2(1)+offset(1)+[-1,1]*len2
  ENDIF ELSE BEGIN
    ;H1
    lines[0:1,0]=imSz2(0)+offset(0)+dista*sin(daRad)+[-1,1]*len2*cos(daRad)
    lines[2:3,0]=imSz2(1)+offset(1)+dista*cos(daRad)+[1,-1]*len2*sin(daRad)
    ;H2
    lines[0:1,1]=imSz2(0)+offset(0)-dista*sin(daRad)+[-1,1]*len2*cos(daRad)
    lines[2:3,1]=imSz2(1)+offset(1)-dista*cos(daRad)+[1,-1]*len2*sin(daRad)
    ;V1
    lines[0:1,2]=imSz2(0)+offset(0)-dista*cos(daRad)+[-1,1]*len2*sin(daRad)
    lines[2:3,2]=imSz2(1)+offset(1)+dista*sin(daRad)+[-1,1]*len2*cos(daRad)
    ;V2
    lines[0:1,3]=imSz2(0)+offset(0)+dista*cos(daRad)+[-1,1]*len2*sin(daRad)
    lines[2:3,3]=imSz2(1)+offset(1)-dista*sin(daRad)+[-1,1]*len2*cos(daRad)
  ENDELSE

  lines2=INTARR(4,4)
  lines2=lines
  lines2[1,*]=lines[2,*]
  lines2[2,*]=lines[1,*]

  return, ROUND(lines2)

end

;Placing rois for CT number linearity
;
;imgSize = size of active CT image
;imgCenter = shift from the default imSz/2 (same as dxya if in use) [dx, dy, dAngle, (0)]
;radSample = radius of the samples (circles)
;posTable = x and y (pix) positions for all materials relative to imgCenter

function getSampleRois, imgSize, imgCenter, radSample, posTable

  szTab=SIZE(posTable, /DIMENSIONS)
  IF N_ELEMENTS(szTab) EQ 1 THEN szTab=[szTab, 1]
  roiAll=INTARR(imgSize(0),imgSize(1),szTab(1))
  daRad=-imgCenter(2)/!radeg
  FOR gg=0, szTab(1)-1 DO BEGIN
    pos=posTable[*,gg]
    IF imgCenter(2) NE 0 THEN BEGIN
      posx=pos(0)*cos(daRad)-pos(1)*sin(daRad)
      posy=pos(0)*sin(daRad)+pos(1)*cos(daRad)
      pos=[posx,posy]
    ENDIF
    roiAll[*,*,gg]=getROIcircle(imgSize, imgSize/2+imgCenter[0:1]+pos, radSample)
  ENDFOR

  return, roiAll
end

;Placing rois for NM contrast 6 circles
;
;imgSize = size of active image
;imgCenter = shift from the default imSz/2 (same as dxya if in use) [dx, dy, dAngle, (0)]
;radSample = radius of the circles
;radSample2 = radius from center of image to center of circles

function getConNMRois, imgSize, imgCenter, radSample, radSample2

  circle=getROIcircle([radSample*2+1,radSample*2+1], [radSample,radSample], radSample)

  ;position the circles
  radSampleLarge=radSample2+radSample
  roi0=INTARR(radSampleLarge*2+1,radSampleLarge*2+1)
  roi0[radSampleLarge-radSample:radSampleLarge+radSample, radSampleLarge*2-radSample*2:radSampleLarge*2]=circle
  roi0=ROUND(ROT(FLOAT(roi0), imgCenter(2), Missing=0, cubic=-0.5)); add dAngle
  roi60=ROUND(ROT(FLOAT(roi0),60, Missing=0, cubic=-0.5))
  roi120=ROUND(ROT(FLOAT(roi0),120, Missing=0, cubic=-0.5))
  roi180=ROUND(ROT(FLOAT(roi0),180, Missing=0, cubic=-0.5))
  roi240=ROUND(ROT(FLOAT(roi0),240, Missing=0, cubic=-0.5))
  roi300=ROUND(ROT(FLOAT(roi0),300, Missing=0, cubic=-0.5))
  background=INTARR(radSampleLarge*2+1,radSampleLarge*2+1)
  background[radSampleLarge-radSample:radSampleLarge+radSample, radSampleLarge-radSample:radSampleLarge+radSample]=circle

  ;position circles on image
  roiAll=INTARR(imgSize(0),imgSize(1),7)
  x1=imgSize(0)/2+imgCenter(0)-radSampleLarge
  x2=imgSize(0)/2+imgCenter(0)+radSampleLarge
  y1=imgSize(1)/2+imgCenter(1)-radSampleLarge
  y2=imgSize(1)/2+imgCenter(1)+radSampleLarge
  roiAll[x1:x2,y1:y2,0]=roi0
  roiAll[x1:x2,y1:y2,1]=roi60
  roiAll[x1:x2,y1:y2,2]=roi120
  roiAll[x1:x2,y1:y2,3]=roi180
  roiAll[x1:x2,y1:y2,4]=roi240
  roiAll[x1:x2,y1:y2,5]=roi300
  roiAll[x1:x2,y1:y2,6]=background

  return, roiAll
end

function getNPSrois, imgSize, imgCenter, ROIsz, ROIdistPix, subNN
  ;first ROI at 12o'clock, rest +dAngle 
  dAngle=2.*!pi/subNN
  centerposX=0
  centerposY=1
  FOR i=1, subNN-1 DO BEGIN
    centerposX=[centerposX, SIN(i*dAngle)]
    centerposY=[centerposY, COS(i*dAngle)]
  ENDFOR
  
  CC=[imgSize(0)/2+imgCenter(0),imgSize(1)/2+imgCenter(1)]
  
  centerposX=ROUND(ROIdistPix*centerposX+CC(0))
  centerposY=ROUND(ROIdistPix*centerposY+CC(1))

  ROIsz2=ROIsz/2

  roiAll=-1
  IF CC(0)+ROIdistPix+ROIsz2 LT imgSize(0) AND CC(0)-ROIdistPix-ROIsz2 GE 0 AND CC(1)+ROIdistPix+ROIsz2 LT imgSize(1) AND CC(1)-ROIdistPix-ROIsz2 GE 0 THEN BEGIN
    roiAll=INTARR(imgSize(0), imgSize(1),subNN)
    FOR i = 0, subNN-1 DO roiAll[centerposX(i)-ROIsz2:centerposX(i)+ROIsz2,centerposY(i)-ROIsz2:centerposY(i)+ROIsz2,i]=1
  ENDIF ELSE sv=DIALOG_MESSAGE('ROIs fall outside image due to defined center or radius.')

  return, roiAll
end