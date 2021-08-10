
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

;calculated uniformity according to NEMA NU-1 2007
function calcUniformityNM, imgMatrix, roiMatrix, pix, corrFlag

  szI=SIZE(imgMatrix,/DIMENSIONS)

  ;congrid to 6.4+/-30% mm pixels
  minPix=6.4*0.7 & maxPix=6.4*1.3
  scaleFactors=[2,4,8,16,32]
  possPix=pix(0)*scaleFactors
  selPix=WHERE(possPix GE minPix)
  selFact=scaleFactors(selPix(0))
  pixRes=pix(0)*selFact
  newImgMatrix=sum2x2pix(imgMatrix, selPix(0))
  newROImatrix=sum2x2pix(roiMatrix, selPix(0))
  
  outSide=WHERE(newROImatrix LE selFact);minimum 50% inside UFOV to be included
  newROImatrix[*,*]=1
  newROImatrix(outSide)=0
  
  ;corners of UFOV
  temp=TOTAL(newROImatrix,2)
  temp2=WHERE(temp GT 0)
  firstXu=temp2(0)
  lastXu=temp2(-1)
  temp=TOTAL(newROImatrix,1)
  temp2=WHERE(temp GT 0)
  firstYu=temp2(0)
  lastYu=temp2(-1)
  
  ;corners of CFOV 75% of UFOV
  firstXc=firstXu+0.25*(lastXu-firstXu)/2
  lastXc=lastXu-0.25*(lastXu-firstXu)/2
  firstYc=firstYu+0.25*(lastYu-firstYu)/2
  lastYc=lastYu-0.25*(lastYu-firstYu)/2
  
  ;smoothing 9 point filter [1,2,1][2,4,2][1,2,1]
  kernel=[[1,2,1],[2,4,2],[1,2,1]]
  smoothedImg=CONVOL(newImgMatrix, kernel/TOTAL(kernel), /CENTER)
  
  UFOVmatrix=smoothedImg[firstXu:lastXu, firstYu:lastYu]
  CFOVmatrix=smoothedImg[firstXc:lastXc, firstYc:lastYc]
  
  ;differential uniformity
  szUFOV=SIZE(UFOVmatrix, /DIMENSIONS)
  
  DU_UFOV=0.
  FOR i=0, szUFOV(0)-1 DO BEGIN;columns
    FOR j=0, szUFOV(1)-5 DO BEGIN
      sub=UFOVmatrix[i,j:j+4]
      temp=100.*(MAX(sub)-MIN(sub))/(MAX(sub)+MIN(sub))
      IF temp GT DU_UFOV THEN DU_UFOV=temp 
    ENDFOR
  ENDFOR
  FOR i=0, szUFOV(0)-5 DO BEGIN;rows
    FOR j=0, szUFOV(1)-1 DO BEGIN
      sub=UFOVmatrix[i:i+4,j]
      temp=100.*(MAX(sub)-MIN(sub))/(MAX(sub)+MIN(sub))
      IF temp GT DU_UFOV THEN DU_UFOV=temp
    ENDFOR
  ENDFOR
  
  szCFOV=SIZE(CFOVmatrix, /DIMENSIONS)
  DU_CFOV=0.
  FOR i=0, szCFOV(0)-1 DO BEGIN;columns
    FOR j=0, szCFOV(1)-5 DO BEGIN
      sub=CFOVmatrix[i,j:j+4]
      temp=100.*(MAX(sub)-MIN(sub))/(MAX(sub)+MIN(sub))
      IF temp GT DU_CFOV THEN DU_CFOV=temp
    ENDFOR
  ENDFOR
  FOR i=0, szCFOV(0)-5 DO BEGIN;rows
    FOR j=0, szCFOV(1)-1 DO BEGIN
      sub=CFOVmatrix[i:i+4,j]
      temp=100.*(MAX(sub)-MIN(sub))/(MAX(sub)+MIN(sub))
      IF temp GT DU_CFOV THEN DU_CFOV=temp
    ENDFOR
  ENDFOR
  
  ;integral uniformity
  IU_UFOV=100.*(MAX(UFOVmatrix)-MIN(UFOVmatrix))/(MAX(UFOVmatrix)+MIN(UFOVmatrix))
  IU_CFOV=100.*(MAX(CFOVmatrix)-MIN(CFOVmatrix))/(MAX(CFOVmatrix)+MIN(CFOVmatrix))

  resU=CREATE_STRUCT('matrix',smoothedImg, 'table', [IU_UFOV, DU_UFOV, IU_CFOV, DU_CFOV])
  IF corrFlag THEN resU=CREATE_STRUCT(resU,'corrMatrix',imgMatrix)

return, resU
end