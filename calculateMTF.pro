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

function calculateMTF, submatrix, pix, center, typeMTF, backmatrix, cutLSF, cutW, cutF, sampFreq
;typeMTF 0 = method from JAppl Clin Med Phys, Vol 14. No4, 2013, pp216..
;typeMTF 2 = extract edge from circular edge 
fitWidthFactor=0.;how much of the profile to include in curvefit: factor*FWHM from center
factorPad=3;x input size when zeropadding

IF N_ELEMENTS(pix) EQ 1 THEN pix=[pix,pix]; no guarantee that non-isotropic pixels will work troughout the code, not tested

MTFstruct=CREATE_STRUCT('empty',0)

szM=size(submatrix,/DIMENSIONS)

CASE typeMTF OF

0: BEGIN

  ;for testing:
  ;subMatrixAll=generateTestMatrix()
  ;halfmat=68; 137 0,2pix
  ;pix=[0.201,0.201]
  ;radicut=ROUND(3./pix(0))
  ;print, 'ROI radicut #pix: ',radicut
  ;submatrix=subMatrixAll[halfmat-radicut:halfmat+radicut,halfmat-radicut:halfmat+radicut,0]
  ;backMatrix=subMatrixAll[0:radicut*2,0:radicut*2,0]  
  ;window, 1, TITLE='profile x/y' & wset,1
  ;plot, submatrix[*,radicut]
  ;oplot, submatrix[radicut,*], linestyle=1
  ;tvscl, submatrix 
  
  MTFstruct=CREATE_STRUCT('subMatrixAll',submatrix)
  LSFx=1.0*TOTAL(submatrix,2)
  LSFy=1.0*TOTAL(submatrix,1)
  background=MEAN(1.0*TOTAL(backMatrix,2))
  
  halfmax=0.5*(MAX(submatrix)+MIN(submatrix))
  centerPos=centroid(submatrix, halfmax) 
  IF MIN(centerPos) EQ -1 THEN BEGIN
    status=0
    centerPos=0.5*szM[0:1]
  ENDIF ELSE status=1
  ddx=(FINDGEN(szM(0))-centerPos(0))*pix(0)
  ddy=(FINDGEN(szM(1))-centerPos(1))*pix(1)
  MTFstruct=CREATE_STRUCT(MTFstruct,'centerPos', centerPos,'cdx',ddx,'cdy',ddy)
  
  LSFx=LSFx-background
  LSFx=LSFx/TOTAL(LSFx,1)
  LSFy=LSFy-background
  LSFy=LSFy/TOTAL(LSFy,1)
 
  halfsz=szM/2
  
  ;gaussian MTF
  smFac=0;0.3
  sigmaF=smFac/pix(0); =beadsz/pix
  IF sigmaF NE 0 THEN BEGIN
    smoothFilter=gaussFilter(sigmaF,szM(0))
    smLSFx=CONVOL(LSFx,smoothFilter,/CENTER,/EDGE_TRUNCATE)
    smLSFy=CONVOL(LSFy,smoothFilter,/CENTER,/EDGE_TRUNCATE)
  ENDIF ELSE BEGIN
    smLSFx=LSFx & smLSFy=LSFy
  ENDELSE
  resX=getGaussFit(ddx,smLSFx,pix(0),fitWidthFactor)
  resY=getGaussFit(ddy,smLSFy,pix(1),fitWidthFactor)
  fitLSFx=resX.yfit & fitLSFy=resY.yfit
  
  IF N_ELEMENTS(fitLSFx) NE 1 THEN gMTFx=getMTFgauss(resX.A, sigmaF*pix(0))
  IF N_ELEMENTS(fitLSFy) NE 1 THEN gMTFy=getMTFgauss(resY.A, sigmaF*pix(1))

  szPadded=factorPad*szM(0)

  IF cutLSF THEN BEGIN
    nn=N_ELEMENTS(LSFx)
    smdLSF=SMOOTH(LSFx,3)
    smdLSF=smdLSF/max(smdLSF)
    over05=WHERE(smdLSF GT 0.5, nover05)
    pp1=over05(0) & pp2=over05(nover05-1)
    ppFWHM=pp2-pp1
    first=ROUND(pp1-cutW*ppFWHM)
    last=ROUND(pp2+cutW*ppFWHM)
    IF first GT 0 THEN LSFx[0:first]=0
    IF last LT nn-1 THEN LSFx[last:nn-1]=0

    nn=N_ELEMENTS(LSFy)
    smdLSF=SMOOTH(LSFy,3)
    smdLSF=smdLSF/max(smdLSF)
    over05=WHERE(smdLSF GT 0.5, nover05)
    pp1=over05(0) & pp2=over05(nover05-1)
    ppFWHM=pp2-pp1
    first=ROUND(pp1-cutW*ppFWHM)
    last=ROUND(pp2+cutW*ppFWHM)
    IF first GT 0 THEN LSFy[0:first]=0
    IF last LT nn-1 THEN LSFy[last:nn-1]=0
  ENDIF
  
  MTFx=FFTvector(LSFx, factorPad)
  Nx=N_ELEMENTS(MTFx)
  fx=FINDGEN(Nx)*(1./(szPadded*pix(0)))
  
  MTFy=FFTvector(LSFy, factorPad)
  Ny=N_ELEMENTS(MTFy)
  fy=FINDGEN(Ny)*(1./(szPadded*pix(1)))

END

1: BEGIN; wire (3d)

  ;for testing:
  ;pix=[0.5,0.5]
  ;submatrix=submatrix[10:40,10:40,*]
  
  szM=size(submatrix,/DIMENSIONS)
  
  background=MEAN(subMatrix[0,0,*]);MEAN([MEAN(submatrix[*,0,*]),MEAN(submatrix[*,szM(1)-1,*]),MEAN(submatrix[0,*,*]),MEAN(submatrix[szM(0)-1,*,*])])
  IF N_ELEMENTS(szM) EQ 2 THEN BEGIN; duplicate image
    submatrix2=FLTARR(szM(0),szM(1),2)
    submatrix2[*,*,0]=submatrix
    submatrix2[*,*,1]=submatrix
    submatrix=submatrix2
    background=MEAN(subMatrix[0:1,0:1,0])
    szM=[szM, 2]
  ENDIF

    MTFstruct=CREATE_STRUCT('subMatrixAll',submatrix)

    distArrX=FLTARR(szM(0),szM(2))
    distArrY=FLTARR(szM(1),szM(2))
    LSFarrX=FLTARR(szM(0),szM(2))
    LSFarrY=FLTARR(szM(1),szM(2))
    FOR i=0, szM(2)-1 DO BEGIN
      LSFx=1.0*TOTAL(submatrix[*,*,i],2)
      LSFy=1.0*TOTAL(submatrix[*,*,i],1)
      ;IF N_ELEMENTS(backMatrix) GT 1 THEN background=MEAN(1.0*TOTAL(backMatrix,2)) ELSE background=backMatrix

      halfmax=0.5*[MAX(submatrix[*,*,i])+MIN(submatrix[*,*,i])]
      centerPos=centroid(submatrix[*,*,i], halfmax)
      IF MIN(centerPos) EQ -1 THEN BEGIN
        status=0
        centerPos=0.5*szM[0:1]
      ENDIF ELSE status=1
      distArrX[*,i]=(FINDGEN(szM(0))-centerPos(0));*pix(0)
      distArrY[*,i]=(FINDGEN(szM(1))-centerPos(1));*pix(1)

      LSFx=LSFx-background*szM(1)
      LSFarrX[*,i]=LSFx/TOTAL(LSFx)
      LSFy=LSFy-background*szM(0)
      LSFarrY[*,i]=LSFy/TOTAL(LSFy)
    ENDFOR

    ;sort LSF values by distance to center
    distArr=[[distArrX],[distArrY]]
    LSFarr=[[LSFarrX],[LSFarrY]]
    sorting=SORT(distArr)
    dists=distArr(sorting)
    lsfSortVals=LSFarr(sorting)
    pixVals=lsfSortVals

    ;rebin to equally spaced resolution pix/10
    pix=pix(0) & pixNew=.1*pix
    ;ddists=SHIFT(dists,-1)-dists
    ;ddists=ddists[1:N_ELEMENTS(dists)-2]

    newdists=(FINDGEN((MAX(dists)-MIN(dists)+1)*10)+(MIN(dists)-0.5)*10)*pixNew
    nn=N_ELEMENTS(newdists)

    ;smooth by ~the new pix size if possible
    test=WHERE(dists*pix(0) LT max(newdists))
    smoothSz=ROUND(N_ELEMENTS(test)/N_ELEMENTS(newdists))
    If smoothSz GT 2 THEN pixVals=SMOOTH(pixVals,smoothSz)
    lsfSortValsInterp=INTERPOL(pixVals, dists*pix, newdists);smooth over pix/10 before interpolate
    lsfSortValsInterp[0:9]=lsfSortValsInterp(10) & lsfSortValsInterp[nn-10:nn-1]=lsfSortValsInterp(nn-11)
    pixValsInterp=lsfSortValsInterp

    angle=-1

  ;discrete MTF
  dLSF=pixValsInterp
  szPadded=nn
  dLSF=dLSF/MAX(dLSF)

  IF cutLSF THEN BEGIN
    smdLSF=SMOOTH(dLSF,5)
    smdLSF=smdLSF/max(smdLSF)
    over05=WHERE(smdLSF GT 0.5, nover05)
    pp1=over05(0) & pp2=over05(nover05-1)
    ppFWHM=pp2-pp1
    first=ROUND(pp1-cutW*ppFWHM)
    last=ROUND(pp2+cutW*ppFWHM)
    IF first GT 0 THEN dLSF[0:first]=0
    IF last LT (nn-1) THEN dLSF[last:nn-1]=0
  ENDIF

  dMTFcomplex=FFT(dLSF,/CENTER)
  dMTF=szPadded*SQRT(REAL_PART(dMTFcomplex)^2+IMAGINARY(dMTFcomplex)^2); modulus of Fouriertransform * size of submatrix (divided by 1/N during FFT)
  dMTF=dMTF[szPadded/2:szPadded-1]
  dMTF=dMTF/dMTF(0)
  fx=FINDGEN(N_ELEMENTS(dMTF))*(1./(szPadded*pixNew))

  ;smooth with gaussian
  sigmaF=0 ; used to be 3, if sigmaF=5 , FWHM ~9 newpix = ~ 1 original pix
  If sigmaF NE 0 THEN BEGIN
    IF nn*.5 EQ nn/2 THEN odd=0 ELSE odd=1
    nnn=nn/2+odd
    xf=FINDGEN(nnn)
    yf=EXP(-0.5*xf^2/sigmaF^2)
    filter=[reverse(yf[1:nnn-1]),yf]
    IF odd EQ 0 THEN filter=[0,filter]
    filter=filter/TOTAL(filter)
    nonZeros=WHERE(filter NE 0.)
    filter=filter(nonZeros)
    pixValsSmooth=CONVOL(pixValsInterp,filter,/CENTER,/EDGE_TRUNCATE)
  ENDIF ELSE pixValsSmooth=pixValsInterp

  lsf=pixValsSmooth
  LSF=LSF/MAX(LSF)

  MTFstruct=CREATE_STRUCT(MTFstruct,'distspix0',dists*pix(0),'pixValSort',pixVals,'newdists',newdists,'pixValsInterp',pixValsInterp, 'angle',angle)
  IF sigmaF NE 0 THEN MTFstruct=CREATE_STRUCT(MTFstruct,'pixValsSmooth',pixValsSmooth)

  X = newdists
  Y = lsf
  ;weights = 1.0/Y
  weights= FLTARR(nn)+1.
  res=getWidthAtThreshold(Y,max(lsf)/2)
  IF res(0) NE -1 THEN BEGIN
    FWHM1=res(0)*pixNew
    center=res(1)*pixNew
    ;X=(FINDGEN(N_ELEMENTS(X)))*pixNew-center;(FINDGEN(N_ELEMENTS(X))+0.5)*pixNew-center ; tatt bort +0.5 ift korreksjon getWidhtAtThreshold....center korrigert med -0.5
    ;X=X-center
    lsfSmTemp=lsf
    sigma1=FWHM1/(2*SQRT(2*ALOG(2)))
    vec=lsfSmTemp

    res2=getWidthAtThreshold(vec,min(vec)/2)
    IF res2(0) NE -1 THEN BEGIN
      FWHM2=res2(0)*pixNew
      sigma2=FWHM2/(2*SQRT(2*ALOG(2)))
      IF sigma2 LT sigma1 THEN sigma2=2.*sigma1
    ENDIF ELSE sigma2=2.*sigma1

    ss1=0
    ss2=nn-1

    A = [max(lsfSmTemp[ss1:ss2])-min(lsfSmTemp[ss1:ss2]),1.5*min(lsfSmTemp[ss1:ss2]),sigma1, sigma2];first guess parameters for curvefit gaussFitAdd2
    yfit = CURVEFIT(X[ss1:ss2], Y[ss1:ss2], weights[ss1:ss2], A, FUNCTION_NAME='gaussFitAdd2', ITER=iter, CHISQ=chisq, TOL=1.0*10^(-4));, ITMAX=100)

    IF A(1) GT A(0) THEN BEGIN
      ;resort such that highest amp first
      newA=[A(1),A(0),A(3),A(2)]
      A=newA
    ENDIF
    A(2)=ABS(A(2)) & A(3)=ABS(A(3))

    IF ABS(A[3]) GT 10.*A[2] OR A[3] LT 0 THEN BEGIN; retry with single gaussfit - allow double gauss with both terms positive
      ;IF A(1) GT 0 OR ABS(A[3]) GT 10.*A[2] THEN BEGIN; retry with single gaussfit - double is for sharp filters
      yfit=gaussfit(X[ss1:ss2], lsf[ss1:ss2], A, ESTIMATES=[max(lsfSmTemp[ss1:ss2]),0,sigma1], NTERMS=3)
    ENDIF

    IF sigmaF NE 0 THEN smLSFx=Y[ss1:ss2] ELSE smLSFx=-1
    fitLSFx=yfit
    ddx=X[ss1:ss2]

  ENDIF ELSE BEGIN
    ss1=0
    ss2=nn-1
    LSFx=lsf
    ddx=newdists
    svar=DIALOG_MESSAGE('Failed to fit LSF to gaussian. LSF used as is further. NB smoothed LSF!',/ERROR)
  ENDELSE

  ;gauss to gauss continuous version:
  ;http://www.cse.yorku.ca/~kosta/CompVis_Notes/fourier_transform_Gaussian.pdf
  nVals=CEIL(3./sampFreq)
  kvals=FINDGEN(nVals)*sampFreq*2*!pi
  IF N_ELEMENTS(A) GT 0 THEN BEGIN
    ;kvals=FINDGEN(200)*0.05/A(2);sample 20 steps from 0 to 1 stdv MTF curve A0 (stdev=1/A(2))
    Fgu0=calcGauss(kvals, 1/A(2),A(0)*A(2),0)
    IF N_ELEMENTS(A) EQ 4 THEN Fgu1=calcGauss(kvals, 1/A(3),A(1)*A(3),0) ELSE Fgu1=0.
    If sigmaF NE 0 THEN Ffilter=calcGauss(kvals,1./(sigmaF*pixNew),1.0,0) ELSE Ffilter=1.
    gMTFx=(Fgu0+Fgu1)/Ffilter
    gfx=kvals/(2*!pi)
    gMTFx=gMTFx/gMTFx(0)
  ENDIF

  LSFx=dLSF
  MTFx=dMTF

  LSFy=-1
  ddy=-1
  MTFy=-1
  fy=-1

END


2: BEGIN
  ;method from Richard et al: Towards task-based assessment of CT performance, Med Phys 39(7) 2012
  ;sort all pixels regarding distance to centerposition
  subMatrixAll=submatrix
  
  ;for testing:
  ;subMatrixAll=generateTestMatrix()
  ;pix=[0.5,0.5]
  ;szM=size(submatrixAll,/DIMENSIONS)
  
  submatrix=submatrixAll[*,*,0]

  halfMax=(max(submatrixAll)+min(submatrixAll))/2; assume same halfmax for all
  status=0
  IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
    centerPosTemp=FLTARR(2,szM(2))
    FOR b=0, szM(2)-1 DO BEGIN
      tempMatrix=submatrixAll[*,*,b]
      centerPosTemp[*,b]=centroid(tempMatrix, halfmax)
    ENDFOR
    IF MIN(centerPosTemp) EQ -1 THEN BEGIN
      centerPos=0.5*szM[0:1]
    ENDIF ELSE BEGIN
      centerPos=TOTAL(centerPosTemp,2)*1./szM(2);mean centerPos
      status=1
    ENDELSE
  ENDIF ELSE BEGIN
    centerPos=centroid(submatrix, halfmax)
    IF MIN(centerPos) EQ -1 THEN BEGIN
      centerPos=0.5*szM[0:1]
    ENDIF ELSE status=1
  ENDELSE
 
  ddx=(FINDGEN(szM(0))-centerPos(0))*pix(0)
  ddy=(FINDGEN(szM(1))-centerPos(1))*pix(1)
  
  distCenter=submatrix*0.0
  FOR i=0, szM(0)-1 DO BEGIN
     FOR j=0, szM(1)-1 DO BEGIN
        distCenter(i,j)=SQRT((i-centerPos(0))^2+(j-centerPos(1))^2)
     ENDFOR
  ENDFOR
  sorting=SORT(distCenter)
  dists=distCenter(sorting)
  
  IF N_ELEMENTS(szM) EQ 3 THEN BEGIN
    
    pixValSort=FLTARR(N_ELEMENTS(submatrix),szM(2))
    subTemp=subMatrixAll[*,*,0]
    pixValSort[*,0]=subTemp(sorting)
    
    FOR b=1, szM(2)-1 DO BEGIN
      subTemp=subMatrixAll[*,*,b]
      pixValSort[*,b]=subTemp(sorting)
    ENDFOR
    
    pixVals=TOTAL(pixValSort,2)/szM(2); average of several slices
  ENDIF ELSE BEGIN
    pixVals=submatrixAll(sorting)
    pixValSort=pixVals
  ENDELSE
  
  ;rebin to equally spaced resolution pix/10
  radius=MIN([centerPos(0),szM(0)-centerPos(0),centerPos(1), szM(1)-centerPos(1)])-1; maximim radius with full dataset
  newdists=FINDGEN(radius*10)*0.1*pix(0); regular x axis, cuts data at position where start to loose info due to less data in perpendicular directions
  pixNew=.1*pix(0)
  ;smooth by ~the new pix size if possible
  test=WHERE(dists*pix(0) LT max(newdists))
  smoothSz=ROUND(N_ELEMENTS(test)/N_ELEMENTS(newdists))
  If smoothSz GT 2 THEN pixVals=SMOOTH(pixVals,smoothSz)

  pixValsInterp=INTERPOL(pixVals, dists*pix(0), newdists); linear interpolation
  IF N_ELEMENTS(pixValsInterp) GT 10 THEN pixValsInterp[0:9]=pixValsInterp(10);don't trust first 10 values (first original pix)
  nn=N_ELEMENTS(pixValsInterp)

  ;smooth with gaussian
  sigmaF=5 ; if sigmaF=5 , FWHM ~9 newpix = ~ 1 original pix
  If sigmaF NE 0 THEN BEGIN
    IF nn*.5 EQ nn/2 THEN odd=0 ELSE odd=1
    nnn=nn/2+odd
    xf=FINDGEN(nnn)
    yf=EXP(-0.5*xf^2/sigmaF^2)
    filter=[reverse(yf[1:nnn-1]),yf]
    IF odd EQ 0 THEN filter=[0,filter]
    filter=filter/TOTAL(filter)
    nonZeros=WHERE(filter NE 0.)
    filter=filter(nonZeros)
    pixValsSmooth=CONVOL(pixValsInterp,filter,/CENTER,/EDGE_TRUNCATE)
  ENDIF ELSE pixValsSmooth=pixValsInterp

  lsf=SHIFT(pixValsSmooth,-1)-pixValsSmooth; derivated edge
  lsf(0)=lsf(1) & lsf(nn-1)=lsf(nn-2) ; to avoid extremas
  
  lsfRaw=SHIFT(pixValsInterp,-1)-pixValsInterp
  lsfRaw(0)=lsfRaw(1) & lsfRaw(nn-1)=lsfRaw(nn-2)

  IF abs(min(lsf)) GT abs(max(lsf)) THEN BEGIN
    lsf=-lsf ;to have positive gauss
    lsfRaw=-lsfRaw
  ENDIF

  MTFstruct=CREATE_STRUCT('distspix0',dists*pix(0),'pixValSort',pixValSort,'pixValsSmooth',pixValsSmooth,'newdists',newdists,'pixValsInterp',pixValsInterp, $
     'centerPos', centerPos,'cdx',ddx,'cdy',ddy, 'subMatrixAll',subMatrixAll)
  
  X = newdists
  Y = lsf
  ;weights = 1.0/Y
  weights= FLTARR(nn)+1.
  res=getWidthAtThreshold(Y,max(lsf)/2)
  IF res(0) NE -1 THEN BEGIN
    FWHM1=res(0)*pixNew
    center=res(1)*pixNew

    lsfSmTemp=lsf
    sigma1=FWHM1/(2*SQRT(2*ALOG(2)))
    
    vec=lsfSmTemp

    res2=getWidthAtThreshold(vec,min(vec)/2)
    IF res2(0) NE -1 THEN BEGIN
      FWHM2=res2(0)*pixNew
      sigma2=FWHM2/(2*SQRT(2*ALOG(2)))
      IF sigma2 LT sigma1 THEN sigma2=2.*sigma1
    ENDIF ELSE sigma2=2.*sigma1
    
    xCenter=X-center

    IF fitWidthFactor EQ 0 THEN ss1=0 ELSE ss1=ROUND(center/pixNew)-ROUND(FWHM1/pixNew)*fitWidthFactor
    IF fitWidthFactor EQ 0 THEN ss2=nn-1 ELSE ss2=ROUND(center/pixNew)+ROUND(FWHM1/pixNew)*fitWidthFactor
    IF ss1 LT 0 THEN ss1=0
    IF ss2 GT nn-1 THEN ss2=nn-1
    A = [max(lsfSmTemp[ss1:ss2])-min(lsfSmTemp[ss1:ss2]),1.5*min(lsfSmTemp[ss1:ss2]),sigma1, sigma2];first guess parameters for curvefit gaussFitAdd2
  
    yfit = CURVEFIT(xCenter[ss1:ss2], Y[ss1:ss2], weights[ss1:ss2], A, FUNCTION_NAME='gaussFitAdd2', ITER=iter, CHISQ=chisq);, TOL=.00001*(1.0*10^(-3)));, ITMAX=100)

    IF A(1) GT A(0) THEN BEGIN
      ;resort such that highest amp first
      newA=[A(1),A(0),A(3),A(2)]
      A=newA
    ENDIF
    A(2)=ABS(A(2)) & A(3)=ABS(A(3))

    IF ABS(A[3]) GT 10.*A[2] THEN BEGIN; retry with single gaussfit
    ;IF A(1) GT 0 OR ABS(A[3]) GT 10.*A[2] THEN BEGIN; retry with single gaussfit - double is for sharp filters
      yfit=gaussfit(xCenter[ss1:ss2], lsf[ss1:ss2], A, ESTIMATES=[max(lsfSmTemp[ss1:ss2]),0,sigma1], NTERMS=3)
    ENDIF

    LSFx=yfit
    ddx=newdists[ss1:ss2]
  ENDIF ELSE BEGIN
    ss1=0
    ss2=nn-1
    LSFx=lsf
    ddx=newdists
    status=2
    ;svar=DIALOG_MESSAGE('Failed to fit LSF to gaussian. LSF used as is further. NB smoothed LSF!',/ERROR)
  ENDELSE
  
  IF N_ELEMENTS(res) EQ 2 THEN ddx= newdists-res(1)*pixNew ELSE ddx=newdists

  ;gauss to gauss continuous version:
  ;http://www.cse.yorku.ca/~kosta/CompVis_Notes/fourier_transform_Gaussian.pdf
  nVals=CEIL(3./sampFreq); sample up to max frequency; TODO set max freq user editable
  kvals=FINDGEN(nVals)*sampFreq*2*!pi;sample 20 steps from 0 to 1 stdv MTF curve A0 (stdev=1/A(2))
  IF N_ELEMENTS(A) GT 0 THEN BEGIN
    Fgu0=calcGauss(kvals, 1/A(2),A(0)*A(2),0)
    IF N_ELEMENTS(A) EQ 4 THEN Fgu1=calcGauss(kvals, 1/A(3),A(1)*A(3),0) ELSE Fgu1=0.
    If sigmaF NE 0 THEN Ffilter=calcGauss(kvals,1./(sigmaF*pixNew),1.0,0) ELSE Ffilter=1.
    gMTFx=(Fgu0+Fgu1)/Ffilter
    gfx=kvals/(2*!pi)

    gMTFx=gMTFx/gMTFx(0)
  ENDIF

  smLSFx=lsf
  fitLSFx=LSFx
  LSFx=lsfRaw
  
  szPadded=2*((factorPad*N_ELEMENTS(lsfRaw))/2);assure even number
  halfsz=N_ELEMENTS(LSFx)/2
  IF cutLSF THEN BEGIN
    IF N_ELEMENTS(yfit) NE 0 THEN BEGIN
      ppFWHM=2*A(2)/pixNew
      maxp=WHERE(fitLSFx EQ max(fitLSFx))
      pp1=maxp(0)-ppFWHM/2 & pp2=maxp(0)+ppFWHM/2
    ENDIF ELSE BEGIN
      smdLSF=SMOOTH(LSFx,5)
      smdLSF=smdLSF/max(smdLSF)
      over05=WHERE(smdLSF GT 0.5, nover05)
      pp1=over05(0) & pp2=over05(nover05-1)
      ppFWHM=pp2-pp1
    ENDELSE
    first=ROUND(pp1-cutW*ppFWHM)
    firstF=ROUND(first-cutF*ppFWHM)
    last=ROUND(pp2+cutW*ppFWHM)
    lastF=ROUND(last+cutF*ppFWHM)
    IF lastF-firstF GT 0 THEN factorArr=INTARR(lastF-firstF)+1
    IF cutF NE 0 THEN BEGIN
      IF first-firstF GT 0 THEN BEGIN
        grad=INDGEN(first-firstF)
        factorArr[0:first-firstF-1]=grad
        factorArr[last-firstF:lastF-firstF-1]=REVERSE(grad)
        factorArr[first-firstF:last-firstF-1]=first-firstF
        factorArr=1.0/(MAX(factorArr))*factorArr
      ENDIF
    ENDIF
    
    IF N_ELEMENTS(factorArr) GT 0 THEN BEGIN
      factorArr2=LSFx*0.
      IF firstF GE 0 AND lastF LE (nn-1) THEN BEGIN
         factorArr2[firstF:lastF-1]=factorArr
         LSFx=LSFx*factorArr2
      ENDIF ELSE BEGIN
        ;sv=DIALOG_MESSAGE('Cut of LSF close to border of data. Not implemented yet... LSF preserved.')
        A1=0 & B1=N_ELEMENTS(factorArr)-1
        A2=firstF & B2=lastF-1
        IF firstF LT 0 THEN BEGIN
          A2=0
          A1=ABS(firstF)
        ENDIF
        IF lastF GE nn THEN BEGIN
          B2=nn-1
          B1=B1-(lastF-(nn-1))+1
        ENDIF
        factorArr2[A2:B2]=factorArr[A1:B1]
        LSFx=LSFx*factorArr2
      ENDELSE
    ENDIF

  ENDIF
  
  ;discrete MTF
  nullPadd=FLTARR(szPadded)
  nullPadd[szPadded/2-halfsz(0):szPadded/2-halfsz(0)+N_ELEMENTS(lsfRaw)-1]=LSFx;lsfRaw
  lsfRaw=nullPadd
  MTFxComplex=FFT(lsfRaw,/CENTER)
  MTFx=szPadded*SQRT(REAL_PART(MTFxComplex)^2+IMAGINARY(MTFxComplex)^2); modulus of Fouriertransform * size of submatrix (divided by 1/N during FFT)
 
  MTFx=MTFx[szPadded/2:szPadded-1]
  MTFx=MTFx/MTFx(0) 

  fx=FINDGEN(N_ELEMENTS(MTFx))*(1./(szPadded*0.1*pix(0))); 0.1 x pix, mm to cm = 0.01
  
  LSFy=-1
  ddy=-1
  MTFy=-1
  fy=-1
END

ELSE:

ENDCASE

MTFstruct=CREATE_STRUCT(MTFstruct, 'LSFx',LSFx,'dx',ddx,'LSFy',LSFy,'dy',ddy)

MTFstruct=CREATE_STRUCT(MTFstruct, 'MTFx',MTFx, 'fx',fx,'MTFy',MTFy,'fy',fy)
szg=SIZE(gMTFx, /TNAME)
IF szg EQ 'STRUCT' THEN MTFstruct=CREATE_STRUCT(MTFstruct, 'smLSFx',smLSFx,'fitLSFx',fitLSFx,'gfx', gMTFx.k,'gMTFx',gMTFx.MTF)
IF szg EQ 'FLOAT' THEN MTFstruct=CREATE_STRUCT(MTFstruct, 'smLSFx',smLSFx,'fitLSFx',fitLSFx,'gfx', gfx,'gMTFx',gMTFx)
szgy=SIZE(gMTFy, /TNAME)
IF szgy EQ 'STRUCT' THEN MTFstruct=CREATE_STRUCT(MTFstruct, 'smLSFy',smLSFy,'fitLSFy',fitLSFy,'gfy', gMTFy.k,'gMTFy',gMTFy.MTF)


;calculating 50,10,2% (gaussian if exists)
tagMTFres=TAG_NAMES(MTFstruct)
IF tagMTFres.hasValue('GMTFX') THEN MTF=CREATE_STRUCT('fx',MTFstruct.gfx,'MTFx',MTFstruct.gMTFx) ELSE MTF=CREATE_STRUCT('fx',fx,'MTFx',MTFx)
CASE typeMTF OF
  0: BEGIN
    IF szgy EQ 'STRUCT' THEN MTF=CREATE_STRUCT(MTF,'fy',MTFstruct.gfy,'MTFy',MTFstruct.gMTFy) ELSE MTF=CREATE_STRUCT(MTF,'fy',fy,'MTFy',MTFy)
    END
  ELSE: MTF=CREATE_STRUCT(MTF,'fy',0,'MTFy',0)
ENDCASE

lim=[.5,.1,.02]
res=FLTARR(6)-1
FOR i=0, 2 DO BEGIN
  pos=WHERE(MTF.MTFx LT lim(i))
  IF pos(0) NE -1 THEN res(i)=getInterpX(lim(i),MTF.fx(pos(0)-1),MTF.fx(pos(0)),MTF.MTFx(pos(0)-1),MTF.MTFx(pos(0)))
  IF N_ELEMENTS(MTF.MTFy) GT 1 THEN BEGIN   
    pos=WHERE(MTF.MTFy LT lim(i))
    IF pos(0) NE -1 THEN res(i+3)=getInterpX(lim(i),MTF.fy(pos(0)-1),MTF.fy(pos(0)),MTF.MTFy(pos(0)-1),MTF.MTFy(pos(0)))
  ENDIF
ENDFOR
res2=FLTARR(6)-1
FOR i=0, 2 DO BEGIN
  pos=WHERE(MTFx LT lim(i))
  IF pos(0) NE -1 THEN res2(i)=getInterpX(lim(i),fx(pos(0)-1),fx(pos(0)),MTFx(pos(0)-1),MTFx(pos(0)))
  IF N_ELEMENTS(MTFy) GT 1 THEN BEGIN
    pos=WHERE(MTFy LT lim(i))
    IF pos(0) NE -1 THEN res2(i+3)=getInterpX(lim(i),fy(pos(0)-1),fy(pos(0)),MTFy(pos(0)-1),MTFy(pos(0)))
  ENDIF
ENDFOR

MTFstruct=CREATE_STRUCT(MTFstruct, 'F50_10_2', res, 'F50_10_2discrete',res2,'status', status )

return, MTFstruct
end