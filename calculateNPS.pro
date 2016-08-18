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
function calculateNPS, noiseImg, NPSrois, pix, smoothWidth, sampFreq
  ;based on ICRU87
  
  szI=SIZE(noiseImg,/DIMENSIONS)
  szN=SIZE(NPSrois,/DIMENSIONS)
  nROIs=szN(2)
  
  temp=TOTAL(NPSrois[*,*,0],2)
  temp2=WHERE(temp GT 0, ROIsz)
  
  iSub=FLTARR(ROIsz,ROIsz,nROIs)
  NPS=FLTARR(ROIsz,ROIsz)
  thisSub=NPS
  FOR i=0, nROIs-1 DO BEGIN
    thisROI=NPSrois[*,*,i]
    idx=WHERE(thisROI EQ 1)
    thisSub[*,*]=noiseImg(idx)
    subtr=SFIT(thisSub, 2);subtract 2nd order 2d polynomial fit - to remove trend
    iSub[*,*,i]=thisSub-subtr
  ENDFOR

  ;2d fourier of each ROI
  stdevImg=0
  FOR i = 0, nROIs-1 DO BEGIN
      ;temp=FFT(zeroPadd3(iSub[*,*,i]),/CENTER)
      temp=FFT(iSub[*,*,i],/CENTER)
      stdevImg=stdevImg+STDEV(iSub[*,*,i])
      NPSthis=REAL_PART(temp)^2+IMAGINARY(temp)^2
      NPS=NPS+NPSthis
  ENDFOR
  stdevImg=stdevImg/nROIs

  NPS=ROIsz^2*((pix(0)*pix(1))/nROIs)*NPS
  ;NPS=ROIsz^2*(1./nROIs)*NPS

  ;radial binning
  distCenter=NPS*0.0
  centerPos=(1.0*ROIsz)/2-0.5
  FOR i=0, ROIsz-1 DO BEGIN
    FOR j=0, ROIsz-1 DO BEGIN
      distCenter(i,j)=SQRT((i-centerPos)^2+(j-centerPos)^2)
    ENDFOR
  ENDFOR
  sorting=SORT(distCenter)
  dists=distCenter(sorting)
  NPSvals=NPS(sorting)
  
  ;avg over unique dists
  uu=uniq(dists)
  nvals=N_ELEMENTS(uu)
  NPSvalsU=FLTARR(nvals)
  NPSvalsU(0)=MEAN(NPSvals[0:uu(0)])
  FOR i=1, nvals-1 DO NPSvalsU(i)=MEAN(NPSvals[uu(i-1):uu(i)])
  newdists=dists[UNIQ(dists, SORT(dists))];keep only uniq dists
  
  ;smooth irregularly sampled data within given width
  unity=1./(ROIsz*pix(0))
  width=smoothWidth/unity
  NPSvalsU=smoothIrreg(newdists,NPSvalsU, width)
  
  ;regular sampling
  sampRelUnity=sampFreq/unity
  newdistsReg=FINDGEN(ROUND(max(newdists)/sampRelUnity))*sampRelUnity
  NPSvalsInterp=INTERPOL(NPSvalsU, newdists, newdistsReg); linear interpolation
  nn=N_ELEMENTS(NPSValsInterp)

  dr=(FINDGEN(nn))*(sampRelUnity/(ROIsz*pix(0)))

  AUC=INT_TABULATED(dr,NPSvalsInterp)
  NPS2dintegral=TOTAL(NPS)*unity^2

  NPSstruct=CREATE_STRUCT('dr',dr,'rNPS',NPSvalsInterp,'NPS',NPS, 'AUC', AUC, 'varianceIntNPS',NPS2dintegral,'varianceImg',stdevImg)

  return, NPSstruct
end