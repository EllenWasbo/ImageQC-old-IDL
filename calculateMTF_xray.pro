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
function calculateMTF_xray, subMatrix, pix, center, stp, formLSF, cutLSF, cutW;, reviewProcess

  subM=subMatrix
  ;IF useFilter THEN subM=MEDIAN(subM, filterW)
  errMsg=''

  IF SIZE(stpRes, /TNAME) EQ 'STRUCT' THEN subM=linearizeSTP(subM,stpRes)

  ;for testing:
  ;subM=generateTestMatrixX()
  ;pix=.1*501./351.*[1.,1.]
  ;stop

  szM=SIZE(subM,/DIMENSIONS)

  ;get 4 mean values to find direction and halfmax
  x1=MEAN(subM[0:2,*])
  x2=MEAN(subM[szM(0)-3:szM(0)-1,*])
  y1=MEAN(subM[*,0:2])
  y2=MEAN(subM[*,szM(1)-3:szM(1)-1])
  meanvals=[[x1,x2],[y1,y2]]
  diff=abs([x2-x1, y2-y1])
  direction=WHERE(diff EQ max(diff)); direction of MTF x = 0 or y = 1
  halfmax=MEAN(meanvals[*,direction])

  IF direction EQ 1 THEN BEGIN
    subM=ROTATE(subM, 1)
    szM=REVERSE(szM)
  ENDIF

  edgePos=FLTARR(szM(1))
  FOR i=0, szM(1)-1 DO edgePos(i)=findPosTreshold(SMOOTH(subM[*,i],3), halfmax)

  notFoundEdge=WHERE(edgePos EQ -1, nNotFound)
  IF nNotFound GT 0 THEN errMsg=errMsg+'Edge position not found for full ROI. This part is attempted to be ignored, but carefully inspect edge-fit to verify this.'
  foundEdge=WHERE(edgePos GE 0, nFound)
  distTemp=FINDGEN(szM(1))

  ;linear fit of edge positions
  res=LINFIT(edgePos(foundEdge), distTemp(foundEdge), YFIT=yfit)
  slope=res(1) & interc=res(0)
  minx=MIN(edgePos(foundEdge))
  maxx=MAX(edgePos(foundEdge))
  xx=[minx,maxx]
  yy=slope*xx+interc

  MTFstruct=CREATE_STRUCT('edgePos',edgePos,'edgeRow',FINDGEN(szM(1)),'edgeFitx',xx,'edgeFity',yy)

  angle=(180/!PI)*ATAN((xx(1)-xx(0))/(yy(1)-yy(0))) ; *pix(0)/pix(1) if pix not isotropic

  IF ABS(angle) GT 8 OR ABS(angle) LT 2 THEN errMsg=errMsg+'Slanted edge recommended to be 2-8 degrees rotated. This edge is found to be ' + STRING(ABS(angle), FORMAT='(f0.1)') +' degrees. '

  ;sort pixels by distance normal to edge
  distArr=FLTARR(szM(0),szM(1))

  ;FOR i=0, szM(1)-1 DO distArr[*,i]=FINDGEN(szM(0))-(((i)-interc)/slope);-COS(!Pi/180*angle)*(((i)-interc)/slope)
  FOR i=0, szM(1)-1 DO distArr[*,i]=(FINDGEN(szM(0))-(((i)-interc)/slope))*COS(!Pi*angle/180)

  sorting=SORT(distArr)
  pixVals=subM(sorting)
  dists=distArr(sorting)

  ;rebin to equally spaced resolution pix/10
  pix=pix(direction) & pix=pix(0)
  pixNew=.1*pix
  newdists=(FINDGEN((MAX(distArr)-MIN(distArr)+1)*10+1)+(MIN(distArr)-0.5)*10)*pixNew
  nn=N_ELEMENTS(newdists)

  wSm=FLOOR(N_ELEMENTS(pixVals)/N_ELEMENTS(newdists));*2
  smPixVals=SMOOTH(pixVals, wSm)
  smPixValsInterp=INTERPOL(smPixVals, dists*pix, newdists);smooth before interpolate
  smPixValsInterp[0:9]=smPixValsInterp(10) & smPixValsInterp[nn-10:nn-1]=smPixValsInterp(nn-11)

  pixValsInterp=smPixValsInterp
  ;pixValsInterp=INTERPOL(pixVals, dists*pix, newdists); linear interpolation
  ;pixValsInterp[0:9]=pixValsInterp(10);don't trust first 10 values (first original pix)
  ;nn=N_ELEMENTS(pixValsInterp)
  ;pixValsInterp[nn-10:nn-1]=pixValsInterp(nn-11)

  ;discrete MTF
  ;dLSF=SHIFT(smPixValsInterp,-1)-smPixValsInterp; derivated edge
  dLSF=ESFtoLSF(smPixValsInterp)
  szPadded=nn
  dLSF[0:4]=dLSF(5) & dLSF[nn-5:nn-1]=dLSF(nn-6) ; to avoid extremas at outer end
  ;nullPadd=FLTARR(szPadded) & nullPadd[szPadded/2-halfsz(0):szPadded/2-halfsz(0)+szM(0)-1]=dLSF
  ;dLSF=nullPadd
  ;dLSF=SMOOTH(dLSF,1)
  IF ABS(MIN(dLSF)) GT ABS(MAX(dLSF)) THEN dLSF=-dLSF
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

  dx=FINDGEN(N_ELEMENTS(dMTF))*(1./(szPadded*pixNew))

  IF formLSF EQ 1 THEN BEGIN
    ;smooth with gaussian
    sigmaF=3 ; if sigmaF=5 , FWHM ~9 newpix = ~ 1 original pix
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
  ENDIF ELSE pixValsSmooth=smPixValsInterp

  lsf=ESFtoLSF(pixValsSmooth);SHIFT(pixValsSmooth,-1)-pixValsSmooth; derivated edge

  lsf[0:4]=lsf(5) & lsf[nn-5:nn-1]=lsf(nn-6) ; to avoid extremas at outer end
  ;lsfShift=lsf
  IF abs(min(lsf)) GT abs(max(lsf)) THEN lsf=-lsf ;to have positive top
  LSF=LSF/MAX(LSF)

  MTFstruct=CREATE_STRUCT(MTFstruct,'distspix0',dists*pix(0),'pixValSort',pixVals,'pixValsSmooth',pixValsSmooth,'newdists',newdists,'pixValsInterp',pixValsInterp, 'angle',angle)
  X = newdists
  Y = lsf
  ;weights = 1.0/Y
  weights= FLTARR(nn)+1.
  res=getWidthAtThreshold(Y,max(lsf)/2)
  IF res(0) NE -1 THEN BEGIN
    FWHM1=res(0)*pixNew
    center=res(1)*pixNew
    X=(FINDGEN(N_ELEMENTS(X)))*pixNew-center;(FINDGEN(N_ELEMENTS(X))+0.5)*pixNew-center ; tatt bort +0.5 ift korreksjon getWidhtAtThreshold....center korrigert med -0.5
    lsfSmTemp=lsf
    sigma1=FWHM1/(2*SQRT(2*ALOG(2)))

    vec=lsfSmTemp

    ;IF fitWidthFactor EQ 0 THEN ss1=0 ELSE ss1=ROUND(center/pixNew)-ROUND(FWHM1/pixNew)*fitWidthFactor
    ;IF fitWidthFactor EQ 0 THEN ss2=nn-1 ELSE ss2=ROUND(center/pixNew)+ROUND(FWHM1/pixNew)*fitWidthFactor
    ;IF ss1 LT 0 THEN
    ss1=0
    ;IF ss2 GT nn-1 THEN
    ss2=nn-1
    
    CASE formLSF OF 
    
    0: BEGIN ;exp
      A = [1.1*max(lsfSmTemp[ss1:ss2]),2.*ALOG(2)/FWHM1]
      yfit = CURVEFIT(X[ss1:ss2], Y[ss1:ss2], weights[ss1:ss2], A, FUNCTION_NAME='expFit', ITER=iter, CHISQ=chisq, TOL=1.0*10^(-4))
    END
    1: BEGIN
      A = [max(lsfSmTemp[ss1:ss2])/2,max(lsfSmTemp[ss1:ss2])/2,sigma1, sigma1];first guess parameters for curvefit gaussFitAdd2
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
    END
    ELSE:
    ENDCASE

    smLSF=Y[ss1:ss2]
    LSFx=yfit
    ddx=X[ss1:ss2]
    ;stop
  ENDIF ELSE BEGIN
    ss1=0
    ss2=nn-1
    LSFx=lsf
    ddx=newdists
    errMsg=errMsg+'Failed to fit LSF. LSF used as is further. NB smoothed LSF!'
  ENDELSE


;  tit='LSF from edge interpolated (red), smoothed (green), fitted to smoothed (white)'
;  window,3, TITLE=tit
;  IF N_ELEMENTS(res) EQ 2 THEN xs= X ELSE xs=newdists
;  LOADCT, 0, /SILENT
;  IF res(0) NE -1 THEN plot, xs, lsf, XRANGE=res(0)*pixNew*5*[-1,1] ELSE plot, xs, lsf
;  LOADCT, 13, /SILENT
;  IF res(0) NE -1 THEN oplot, xs, lsf, COLOR=150
;  oplot, newdists, dLSF, COLOR=255
;  LOADCT, 0, /SILENT
;  IF N_ELEMENTS(LSFx) GT 0 THEN oplot, ddx, LSFx
;
;  sv=DIALOG_MESSAGE('Accept current fit (Y) or adjust parameters manually (N)?',/QUESTION)
;  IF sv EQ 'No' THEN BEGIN
;    tryAgain=1
;    WHILE tryAgain DO BEGIN
;      box=[$
;        '1, BASE,, /COLUMN', $
;        '0, LABEL, Adjust parameters', $
;        '1, BASE,, /ROW, ',$
;        '0, FLOAT, '+STRING(A(0))+', TAG=A0', $
;        '2, FLOAT, '+STRING(A(2))+', TAG=A2', $
;        '1, BASE,, /ROW', $
;        '0, BUTTON, OK, QUIT, TAG=OK',$
;        '2, BUTTON, Cancel, QUIT']
;      manAdj=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Adjust curvefit', XSIZE=300, YSIZE=300, FOCUSNO=3)
;
;      IF manAdj.OK THEN BEGIN
;        IF res(0) NE -1 THEN plot, xs, lsf, XRANGE=res(0)*pixNew*5*[-1,1] ELSE plot, xs, lsf
;        LOADCT, 13, /SILENT
;        IF N_ELEMENTS(yfit) GT 0 THEN oplot, ddx, LSFx, COLOR=255
;        yAdj=calcGauss(X[ss1:ss2], FLOAT(manAdj.A2),FLOAT(manAdj.A0),0)
;        oplot, ddx, yAdj, COLOR=150
;        LOADCT, 0, /SILENT
;
;        sv=DIALOG_MESSAGE('Accept adjusted fit green (Y) or adjust further (N)?',/QUESTION)
;        IF sv EQ 'Yes' THEN BEGIN
;          tryAgain=0
;          LSFx=yAdj
;        ENDIF
;      ENDIF ELSE tryAgain=0
;    ENDWHILE
;  ENDIF

CASE formLSF OF 
0: BEGIN;exp
  kvals=FINDGEN(200)*0.05/(pix(0)*2.*!pi)
  k0=A[1]/(2.*!pi)
  gMTF=(A[0]/!pi)*k0/(kvals^2+k0^2) ; http://mathworld.wolfram.com/FourierTransformExponentialFunction.html
  gx=kvals;/(2.*!pi)
  gMTF=gMTF/gMTF(0)
  gLSF=LSFx
  END
1: BEGIN

  ;gauss to gauss continuous version:
  ;http://www.cse.yorku.ca/~kosta/CompVis_Notes/fourier_transform_Gaussian.pdf

  kvals=FINDGEN(200)*0.05/A(2);sample 20 steps from 0 to 1 stdv MTF curve A0 (stdev=1/A(2))
  Fgu0=calcGauss(kvals, 1/A(2),A(0)*A(2),0)
  IF N_ELEMENTS(A) EQ 4 THEN Fgu1=calcGauss(kvals, 1/A(3),A(1)*A(3),0) ELSE Fgu1=0.
  If sigmaF NE 0 THEN Ffilter=calcGauss(kvals,1./(sigmaF*pixNew),1.0,0) ELSE Ffilter=1.
  gMTF=(Fgu0+Fgu1)/Ffilter
  gx=kvals/(2*!pi)
  gMTF=gMTF/gMTF(0)
  gLSF=LSFx
END
ELSE:
ENDCASE

MTFstruct=CREATE_STRUCT(MTFstruct,'interpX', newdists, 'dLSF', dLSF) 
IF formLSF NE 2 THEN MTFstruct=CREATE_STRUCT(MTFstruct,'smLSF',smLSF, 'gLSF',gLSF,'d',ddx)
  
  ;calculate lp 0.5, 1, 1.5, 2, 2.5 and frequence at MTF 0.5
  lim=[.5,1.,1.5,2.0,2.5]
  res=FLTARR(6)-1
  FOR i=0, 4 DO BEGIN
     IF formLSF EQ 2 THEN pos=WHERE(dx GT lim(i)) ELSE pos=WHERE(gx GT lim(i))
    IF pos(0) NE -1 THEN BEGIN
      IF formLSF EQ 2 THEN res(i)=getInterpX(lim(i),dMTF(pos(0)-1),dMTF(pos(0)),dx(pos(0)-1),dx(pos(0))) ELSE res(i)=getInterpX(lim(i),gMTF(pos(0)-1),gMTF(pos(0)),gx(pos(0)-1),gx(pos(0)))
    ENDIF ELSE BEGIN
      res(i)=-1
    ENDELSE
  ENDFOR  
  IF formLSF EQ 2 THEN pos=WHERE(dMTF LT 0.5) ELSE pos=WHERE(gMTF LT 0.5)
  IF pos(0) NE -1 THEN BEGIN
    IF formLSF EQ 2 THEN res(5)=getInterpX(0.5,dx(pos(0)-1),dx(pos(0)),dMTF(pos(0)-1),dMTF(pos(0))) ELSE res(5)=getInterpX(0.5,gx(pos(0)-1),gx(pos(0)),gMTF(pos(0)-1),gMTF(pos(0))) 
  ENDIF ELSE res(5)=-1
  
  MTFstruct=CREATE_STRUCT(MTFstruct, 'dx',dx,'dMTF',dMTF, 'lpmm', res, 'slope', slope, 'interc', interc, 'direction',direction,'errMsg',errMsg)
  IF formLSF NE 2 THEN MTFstruct=CREATE_STRUCT(MTFstruct, 'gMTF',gMTF, 'gx',gx)

  return, MTFstruct
end