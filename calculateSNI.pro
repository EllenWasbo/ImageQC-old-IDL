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
function generateHumVisFilter, ROIsz, pixSize, fcd;, c, displaySize, f
  
  ;V(r)=r^1.3*exp(-cr^2)
  ;c adjusted to have V(r) max @ 4 cy/degree
  ;V'(r)=1.3r^0.3-2cr^2.3)*exp(-cr^2)
  ;maxCyDeg=4.
  ;viewingDistMM=1500.;viewing distance in millimeter
  ;maxCyMM=maxCyDeg/(viewingDistMM*TAN(1./!RADEG))
  
  c=fcd(1);c=28 with viewing distance 1.5m (as Nelson et al), c=12 with viewing distance 1.0m
  displaySize=fcd(2);mm on screen (height of ROI) - Nelson et al uses 65mm
  f=fcd(0)
  roiHeight=ROIsz*pixSize; mm for real in image
  s=ROIsz/2
  r=(1./displaySize)*FINDGEN(s)
  Vr=r^f*exp(-c*r^2)
  
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

  return, CREATE_STRUCT('filt2d',humVisFilter,'curve',CREATE_STRUCT('r',r,'V',Vr/(MAX(Vr))))
end

function get_dists_sorting, dim
  ;radial binning
  distCenter=FLTARR(dim,dim)
  centerPos=(1.0*dim)/2-0.5
  FOR i=0, dim-1 DO BEGIN
    FOR j=0, dim-1 DO BEGIN
      distCenter(i,j)=SQRT((i-centerPos)^2+(j-centerPos)^2)
    ENDFOR
  ENDFOR
  sorting=SORT(distCenter)
  dists=distCenter(sorting)
  
  return, CREATE_STRUCT('sorting',sorting,'dists',dists)
end

function get_rNPS, NPS_2d, ROIsz, pix, dists, sorting, smoothWidth, sampFreq
  
  NPSvals=NPS_2d(sorting)
  unity=1./(ROIsz*pix(0))
  width=smoothWidth/unity

  ;avg over unique dists
;  uu=uniq(dists)
;  nvals=N_ELEMENTS(uu)
;  NPSvalsU=FLTARR(nvals)
;  NPSvalsU(0)=MEAN(NPSvals[0:uu(0)])
;  FOR i=1, nvals-1 DO NPSvalsU(i)=MEAN(NPSvals[uu(i-1):uu(i)])
;  newdists=dists[UNIQ(dists, SORT(dists))];keep only uniq dists
  
  ;need to speed up code and avg over close to unique dists (1/10 of the width regarded as small enough) 
  tenthWidths=ROUND((10./width)*dists)
  u=uniq(tenthWidths)
  nvals=N_ELEMENTS(u)
  valsU=FLTARR(nvals)
  valsU(0)=MEAN(NPSvals[0:u(0)])
  FOR i=1, nvals-1 DO valsU(i)=MEAN(NPSvals[u(i-1):u(i)])
  nd=tenthWidths[UNIQ(tenthWidths, SORT(tenthWidths))];keep only uniq dists
  newdists=width/10.*nd

  ;smooth irregularly sampled data within given width
  NPSvalsU=smoothIrreg(newdists,valsU, width);smoothIrreg in a0_functionsMini.pro

  ;regular sampling
  sampRelUnity=sampFreq/unity
  newdistsReg=FINDGEN(ROUND(max(newdists)/sampRelUnity))*sampRelUnity
  NPSvalsInterp=INTERPOL(NPSvalsU, newdists, newdistsReg); linear interpolation
  nn=N_ELEMENTS(NPSValsInterp)

  dr=(FINDGEN(nn))*(sampRelUnity/(ROIsz*pix(0)))
  
  return, CREATE_STRUCT('dr',dr,'rNPS',NPSvalsInterp)
end


;Calculate Structured Noise Index (SNI) based on Nelson et al, J Nucl Med 2014;55:169-174
function calculateSNI, noiseImg, corrMat, SNIroi, pix, fcd, smoothWidth, sampFreq
  
  szI=SIZE(noiseImg,/DIMENSIONS)
  
  IF N_ELEMENTS(corrMat) NE 0 THEN BEGIN
    zeros=WHERE(noiseImg EQ 0)
    noiseImgCorr=noiseImg-corrMat
    noiseImgCorr(zeros)=0. 
  ENDIF ELSE noiseImgCorr=noiseImg

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

  ;totROI=noiseImg[firstX:lastX,firstY:lastY]

  ;large ROIs (2)
  subL=FLTARR(largeDim,largeDim,2)
  subL[*,*,0]=noiseImgCorr[firstX:firstX+largeDim-1,firstY:firstY+largeDim-1]
  subL[*,*,1]=noiseImgCorr[lastX-largeDim+1:lastX,firstY:firstY+largeDim-1]
  
  quantNoiseL=FLTARR(2)
;  IF N_ELEMENTS(corrMat) NE 0 THEN BEGIN
  quantNoiseL(0)=MEAN(noiseImg[firstX:firstX+largeDim-1,firstY:firstY+largeDim-1])
  quantNoiseL(1)=MEAN(noiseImg[lastX-largeDim+1:lastX,firstY:firstY+largeDim-1])
;  ENDIF

  ;small ROIs (6)
  subS=FLTARR(smallDim, smallDim, 6)
  mid=(lastX+firstX)/2
  firstM=mid-(smallDim/2-1)
  subS[*,*,0]=noiseImgCorr[firstX:firstX+smallDim-1,lastY-smallDim+1:lastY];upper lft
  subS[*,*,1]=noiseImgCorr[firstM:firstM+smallDim-1,lastY-smallDim+1:lastY];upper mid
  subS[*,*,2]=noiseImgCorr[lastX-smallDim+1:lastX,lastY-smallDim+1:lastY];upper rgt
  subS[*,*,3]=noiseImgCorr[firstX:firstX+smallDim-1,firstY:firstY+smallDim-1];lower lft
  subS[*,*,4]=noiseImgCorr[firstM:firstM+smallDim-1,firstY:firstY+smallDim-1];lower mid
  subS[*,*,5]=noiseImgCorr[lastX-smallDim+1:lastX,firstY:firstY+smallDim-1];lower rgt

  quantNoiseS=FLTARR(6)
  quantNoiseS(0)=MEAN(noiseImg[firstX:firstX+smallDim-1,lastY-smallDim+1:lastY])
  quantNoiseS(1)=MEAN(noiseImg[firstM:firstM+smallDim-1,lastY-smallDim+1:lastY])
  quantNoiseS(2)=MEAN(noiseImg[lastX-smallDim+1:lastX,lastY-smallDim+1:lastY])
  quantNoiseS(3)=MEAN(noiseImg[firstX:firstX+smallDim-1,firstY:firstY+smallDim-1])
  quantNoiseS(4)=MEAN(noiseImg[firstM:firstM+smallDim-1,firstY:firstY+smallDim-1])
  quantNoiseS(5)=MEAN(noiseImg[lastX-smallDim+1:lastX,firstY:firstY+smallDim-1])

  ;****2d fourier of each ROI***
  NPS_L=FLTARR(largeDim,largeDim,2)
  NPS_S=FLTARR(smallDim, smallDim, 6)
  NPS_L_filt=NPS_L
  NPS_S_filt=NPS_S
  rNPS_L=!Null;radial NPS large ROIs
  rNPS_S=!Null;radial NPS small ROIs
  ;filter with human visual response filter (also removing peak on very low ferquency - trendremoval not needed)
  large=generateHumVisFilter(largeDim,pix, fcd)
  small=generateHumVisFilter(smallDim,pix, fcd)
  humVisFilterLarge=large.filt2d
  humVisFilterSmall=small.filt2d
  humVisFiltCurve=CREATE_STRUCT('L',large.curve,'S',small.curve)
  
  SNIvalues=FLTARR(9);max,L1,L2,S1..6
  
  FOR i = 0, 1 DO BEGIN
    subM=subL[*,*,i]
    temp=FFT(subM-MEAN(subM),/CENTER)
    NPS_L[*,*,i]=largeDim^2*pix^2*(REAL_PART(temp)^2+IMAGINARY(temp)^2)
    ;remove quantum noise to be left with structural noise
    NPS_Lstruc=NPS_L[*,*,i]-quantNoiseL(i)*pix^2;Meancount=variance=pixNPS^2*Total(NPS) where pixNPS=1./(ROIsz*pix), Total(NPS)=NPSvalue*ROIsz^2, NPSvalue=Meancount/(pixNPS^2*ROIsz^2)=MeanCount*pix^2
    ;filter with human visual response filter
    NPS_L_filt[*,*,i]=NPS_L[*,*,i]*humVisFilterLarge
    NPS_Lstruc_filt=NPS_Lstruc*humVisFilterLarge
    SNIvalues(i+1)=TOTAL(NPS_Lstruc_filt)/TOTAL(NPS_L_filt[*,*,i])
    
  ENDFOR

  FOR i = 0, 5 DO BEGIN
    subM=subS[*,*,i]
    temp=FFT(subM-MEAN(subM),/CENTER)
    NPS_S[*,*,i]=smallDim^2*pix^2*(REAL_PART(temp)^2+IMAGINARY(temp)^2)
    ;remove quantum noise
    NPS_Sstruc=NPS_S[*,*,i]-quantNoiseS(i)*pix^2;Meancount=variance=pixNPS^2*Total(NPS) where pixNPS=1./(ROIsz*pix), Total(NPS)=NPSvalue*ROIsz^2, NPSvalue=Meancount/(pixNPS^2*ROIsz^2)=MeanCount*pix^2
    ;filter with human visual response filter
    NPS_S_filt[*,*,i]=NPS_S[*,*,i]*humVisFilterSmall
    NPS_Sstruc_filt=NPS_Sstruc*humVisFilterSmall
    SNIvalues(i+3)=TOTAL(NPS_Sstruc_filt)/TOTAL(NPS_S_filt[*,*,i])
  ENDFOR

  SNIvalues(0)=MAX(SNIvalues)

  NPS_filt=CREATE_STRUCT('L1',NPS_L_filt[*,*,0],'L2',NPS_L_filt[*,*,1],'S1',NPS_S_filt[*,*,0],'S2',NPS_S_filt[*,*,1],'S3',NPS_S_filt[*,*,2],'S4',NPS_S_filt[*,*,3],'S5',NPS_S_filt[*,*,4],'S6',NPS_S_filt[*,*,5])
  
  ds=get_dists_sorting(largeDim)
  rNPS_L1=get_rNPS(NPS_L[*,*,0], largeDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  rNPS_L2=get_rNPS(NPS_L[*,*,1], largeDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  ds=get_dists_sorting(smallDim)
  rNPS_S1=get_rNPS(NPS_S[*,*,0], smallDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  rNPS_S2=get_rNPS(NPS_S[*,*,1], smallDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  rNPS_S3=get_rNPS(NPS_S[*,*,2], smallDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  rNPS_S4=get_rNPS(NPS_S[*,*,3], smallDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  rNPS_S5=get_rNPS(NPS_S[*,*,4], smallDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  rNPS_S6=get_rNPS(NPS_S[*,*,5], smallDim, pix, ds.dists, ds.sorting, smoothWidth, sampFreq)
  
  rNPS = CREATE_STRUCT('L1',rNPS_L1,'L2',rNPS_L2,'S1',rNPS_S1,'S2',rNPS_S2,'S3',rNPS_S3,'S4',rNPS_S4,'S5',rNPS_S5,'S6',rNPS_S6)
  
  SNIstruc=CREATE_STRUCT('SNIvalues',SNIvalues, 'NPS_filt', NPS_filt, 'rNPS', rNPS, 'estQuantNoiseL1',quantNoiseL(0)*pix^2,'humVisFiltCurve',humVisFiltCurve)
  IF N_ELEMENTS(corrMat) NE 0 THEN SNIstruc=CREATE_STRUCT(SNIstruc,'corrMatrix',noiseImgCorr)
  return, SNIstruc
end
