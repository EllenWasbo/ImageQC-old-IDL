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

;generate human visual response filter
;ROIsz should be a even number
; Response function based on equation in Nelson et al, J Nucl Med 2014;55:169-174
function generateHumVisFilter, ROIsz, pixSize
  
  ;V(r)=r^1.3*exp(-cr^2)
  ;c adjusted to have V(r) max @ 4 cy/degree
  ;V'(r)=1.3r^0.3-2cr^2.3)*exp(-cr^2)
  ;maxCyDeg=4.
  ;viewingDistMM=1500.;viewing distance in millimeter
  ;maxCyMM=maxCyDeg/(viewingDistMM*TAN(1./!RADEG))
  
  c=28;c=28 with viewing distance 1.5m (as Nelson et al), c=12 with viewing distance 1.0m
  displaySize=65.;mm on screen (height of ROI) - Nelson et al uses 65mm
  roiHeight=ROIsz*pixSize; mm for real in image
  s=ROIsz/2
  r=(1./displaySize)*FINDGEN(s)
  Vr=r^1.3*exp(-c*r^2)
  
  ;quadrant
  corn=FLTARR(s,s)
  FOR i=1,s-1 DO BEGIN;starting from 1, not 0 to ensure 0 in midle 
    FOR j=1,s-1 DO BEGIN
      avst=sqrt((i+0.5)^2+(j+0.5)^2)
      top=CEIL(avst)
      btm=FLOOR(avst)
      IF top LT s THEN BEGIN
        diff=avst-btm
        corn(i,j)=Vr(btm)-((Vr(btm)-Vr(top))*diff)
      ENDIF
    ENDFOR
  ENDFOR
  
  humVisFilter=fltarr(2*s,2*s)

  humVisFilter[0:s-1,0:s-1]=ROTATE(corn,2)
  humVisFilter[0:s-1,s:2*s-1]=ROTATE(corn,5)
  humVisFilter[s:2*s-1,s:2*s-1]=corn
  humVisFilter[s:2*s-1,0:s-1]=ROTATE(corn,7)
  
  humVisFilter=humVisFilter/MAX(humVisFilter); max=1

  return, humVisFilter
end


;Calculate Structured Noise Index (SNI) based on Nelson et al, J Nucl Med 2014;55:169-174
function calculateSNI, noiseImg, SNIroi, pix
  szI=SIZE(noiseImg,/DIMENSIONS)

  temp=TOTAL(SNIroi,2)
  temp2=WHERE(temp GT 0)
  firstX=temp2(0)
  lastX=temp2(-1)
  temp=TOTAL(SNIroi,1)
  temp2=WHERE(temp GT 0)
  firstY=temp2(0)
  lastY=temp2(-1)

  ;assuming landscape image (X broader than Y and ROIsz even for both large and small ROI)
  largeDim=lastY-firstY+1
  lastY=firstY+largeDim-1
  smallDim=largeDim/2

  ;large ROIs (2)
  subL=FLTARR(largeDim,largeDim,2)
  subL[*,*,0]=noiseImg[firstX:firstX+largeDim-1,firstY:firstY+largeDim-1]
  subL[*,*,1]=noiseImg[lastX-largeDim+1:lastX,firstY:firstY+largeDim-1]

  ;small ROIs (6)
  subS=FLTARR(smallDim, smallDim, 6)
  mid=(lastX+firstX)/2
  firstM=mid-(smallDim/2-1)
  subS[*,*,0]=noiseImg[firstX:firstX+smallDim-1,lastY-smallDim+1:lastY];upper lft
  subS[*,*,1]=noiseImg[firstM:firstM+smallDim-1,lastY-smallDim+1:lastY];upper mid
  subS[*,*,2]=noiseImg[lastX-smallDim+1:lastX,lastY-smallDim+1:lastY];upper rgt
  subS[*,*,3]=noiseImg[firstX:firstX+smallDim-1,firstY:firstY+smallDim-1];lower lft
  subS[*,*,4]=noiseImg[firstM:firstM+smallDim-1,firstY:firstY+smallDim-1];lower mid
  subS[*,*,5]=noiseImg[lastX-smallDim+1:lastX,firstY:firstY+smallDim-1];lower rgt

  ;****2d fourier of each ROI***
  NPS_L=FLTARR(largeDim,largeDim,2)
  NPS_S=FLTARR(smallDim, smallDim, 6)
  NPS_L_filt=NPS_L
  NPS_S_filt=NPS_S
  ;filter with human visual response filter (also removing peak on very low ferquency - trendremoval not needed)
  humVisFilterLarge=generateHumVisFilter(largeDim,pix)
  humVisFilterSmall=generateHumVisFilter(smallDim,pix)
  SNIvalues=FLTARR(9);max,L1,L2,S1..6
  FOR i = 0, 1 DO BEGIN
    subM=subL[*,*,i]
    subtr=SFIT(subM, 2);subtract 2nd order 2d polynomial fit - to remove trend
    temp=FFT(subM-MEAN(subM),/CENTER);FFT(subM-subtr,/CENTER); or without polynomial fit: FFT(subM-MEAN(subM),/CENTER)
    NPS_L[*,*,i]=largeDim^2*pix^2*(REAL_PART(temp)^2+IMAGINARY(temp)^2)
    ;remove quantum noise
    NPS_Lstruc=NPS_L[*,*,i]-MEAN(subM)*pix^2;Meancount=variance=pixNPS^2*Total(NPS) where pixNPS=1./(ROIsz*pix), Total(NPS)=NPSvalue*ROIsz^2, NPSvalue=Meancount/(pixNPS^2*ROIsz^2)=MeanCount*pix^2
    ;filter with human visual response filter
    NPS_L_filt[*,*,i]=NPS_L[*,*,i]*humVisFilterLarge
    NPS_Lstruc_filt=NPS_Lstruc*humVisFilterLarge
    SNIvalues(i+1)=TOTAL(NPS_Lstruc_filt)/TOTAL(NPS_L_filt[*,*,i])
  ENDFOR

  FOR i = 0, 5 DO BEGIN
    subM=subS[*,*,i]
    subtr=SFIT(subM, 2);subtract 2nd order 2d polynomial fit - to remove trend
    temp=FFT(subM-MEAN(subM),/CENTER);FFT(subM-subtr,/CENTER); or without polynomial fit: FFT(subM-MEAN(subM),/CENTER)
    NPS_S[*,*,i]=smallDim^2*pix^2*(REAL_PART(temp)^2+IMAGINARY(temp)^2)
    ;remove quantum noise
    NPS_Sstruc=NPS_S[*,*,i]-MEAN(subM)*pix^2;Meancount=variance=pixNPS^2*Total(NPS) where pixNPS=1./(ROIsz*pix), Total(NPS)=NPSvalue*ROIsz^2, NPSvalue=Meancount/(pixNPS^2*ROIsz^2)=MeanCount*pix^2
    ;filter with human visual response filter
    NPS_S_filt[*,*,i]=NPS_S[*,*,i]*humVisFilterSmall
    NPS_Sstruc_filt=NPS_Sstruc*humVisFilterSmall
    SNIvalues(i+3)=TOTAL(NPS_Sstruc_filt)/TOTAL(NPS_S_filt[*,*,i])
  ENDFOR

  SNIvalues(0)=MAX(SNIvalues)

  NPS_filt=CREATE_STRUCT('L1',NPS_L_filt[*,*,0],'L2',NPS_L_filt[*,*,1],'S1',NPS_S_filt[*,*,0],'S2',NPS_S_filt[*,*,1],'S3',NPS_S_filt[*,*,2],'S4',NPS_S_filt[*,*,3],'S5',NPS_S_filt[*,*,4],'S6',NPS_S_filt[*,*,5])
  
  SNIstruc=CREATE_STRUCT('SNIvalues',SNIvalues, 'NPS_filt', NPS_filt)
  return, SNIstruc
end
