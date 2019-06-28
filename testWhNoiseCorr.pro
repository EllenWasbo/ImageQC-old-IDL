pro testWhNoise

szImg=128
rand=RANDOMN(seed, szImg, szImg)
N=100.
noiseimg=rand*sqrt(N);+N
temp=FFT(noiseimg, /CENTER)
NPS=REAL_PART(temp)^2+IMAGINARY(temp)^2
NPS=szImg^2*NPS

;radial binning
distCenter=NPS*0.0
centerPos=(1.0*szImg)/2-0.5
FOR i=0, szImg-1 DO BEGIN
  FOR j=0, szImg-1 DO BEGIN
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
unity=1./szImg
width=0.05/unity
NPSvalsU=smoothIrreg(newdists,NPSvalsU, width)

;regular sampling
sampRelUnity=0.01/unity
newdistsReg=FINDGEN(ROUND(max(newdists)/sampRelUnity))*sampRelUnity
NPSvalsInterp=INTERPOL(NPSvalsU, newdists, newdistsReg); linear interpolation
nn=N_ELEMENTS(NPSValsInterp)

dr=(FINDGEN(nn))*(sampRelUnity/szImg)

p=plot(dr, NPSValsInterp, 'r')


corrM=corrDistPointSource(rand, 100., 1., 0,0)
noiseImg2=rand*sqrt(N*corrM);+N*corrM

temp=FFT(noiseimg2, /CENTER)
NPS2=REAL_PART(temp)^2+IMAGINARY(temp)^2
NPS2=szImg^2*NPS2
NPSvals2=NPS2(sorting)

;avg over unique dists
NPSvalsU2=FLTARR(nvals)
NPSvalsU2(0)=MEAN(NPSvals2[0:uu(0)])
FOR i=1, nvals-1 DO NPSvalsU2(i)=MEAN(NPSvals2[uu(i-1):uu(i)])

;smooth irregularly sampled data within given width
NPSvalsU2=smoothIrreg(newdists,NPSvalsU2, width)

;regular sampling
NPSvalsInterp2=INTERPOL(NPSvalsU2, newdists, newdistsReg); linear interpolation
nn=N_ELEMENTS(NPSValsInterp2)

p=plot(dr, NPSValsInterp2, 'b', /OVERPLOT)

stop
ROIrad=5

tempImg=noiseImg2
varianceImg=0.0*tempImg
;varianceimg
FOR i=ROIrad, szImg-1-ROIrad DO BEGIN
  FOR j=ROIrad, szImg-1-ROIrad DO BEGIN
    IMAGE_STATISTICS, tempImg[i-ROIrad:i+ROIrad,j-ROIrad:j+ROIrad], VARIANCE=var
    varianceImg(i,j)=var
  ENDFOR
ENDFOR

stop
end