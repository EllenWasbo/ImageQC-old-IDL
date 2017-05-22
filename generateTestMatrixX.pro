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

function generateTestmatrixX

  szM=[501,501];pix = 0.1
  s=250
  ;d2=101
  
  angle=20.
  rangle=angle*!pi/180
  upper=2400
  lower=750
  noise=0
  rota=0; 1=90deg
  
  nImg=1
  noiseFactor=20.

  matrix=FLTARR(szM)+lower
  matrix[0:s,*]=upper

  ;define psf amplitude/sigma
  A1=250.
  sigma1=2.
  A2=0.
  sigma2=1.
  A=[A1,A2,sigma1,sigma2]

  ;generate psf matrix
  X=FINDGEN(szM(0))
  F=calcGauss(X,A[2],A[0],0);+calcGauss(X,A[3],A[1],0); sum of two gaussfunctions, both with mean=0

  corn=dblarr(s,s)
  FOR i=0,s-1 DO BEGIN
    FOR j=0,s-1 DO BEGIN
      avst=sqrt(long(i)^2+long(j)^2)
      top=CEIL(avst)
      btm=FLOOR(avst)
      IF top LT s THEN BEGIN
        diff=avst-btm
        IF diff EQ 0 THEN corn(i,j)=F(ROUND(avst)) ELSE corn(i,j)=F(btm)-((F(btm)-F(top))*diff); interpolate
      ENDIF
    ENDFOR
  ENDFOR

  kernel=fltarr(2*s-1,2*s-1)
  kernel[0:s-1,0:s-1]=ROTATE(corn,2)
  kernel[0:s-1,s-1:2*s-2]=ROTATE(corn,5)
  kernel[s-1:2*s-2,s-1:2*s-2]=corn
  kernel[s-1:2*s-2,0:s-1]=ROTATE(corn,7)

  kernelTemp=FLTARR(s*2+1,s*2+1)
  kernelTemp[1:s*2-1,1:s*2-1]=kernel
  kernel=kernelTemp
  kernel=kernel/TOTAL(kernel)

  lsfKern=TOTAL(kernel,2)


  ;convolve submatrix with psf
  matrixConvol=0.0*matrix
  matrixConvol[*,*]=SHIFT(FFT(FFT(matrix[*,*])*FFT(kernel),/INVERSE),s,s)*((s*2.+1)^2); *N^2


  matrix=ROT(matrixConvol, angle, CUBIC=-0.5)

  ;resize to actual pixelsize
  ;matrixScaledDown1= CONGRID(matrixConvol, szM(0)/10, szM(1)/10,/Center,cubic=-0.5) ; pix=1
  ;matrixScaledDown=matrixScaledDown1[200:300,200:300]
  matrixScaledDown=matrix[200:300,200:300]

  ;add noise
  noiseMatrix=noiseFactor*randomn(seed,101,101)
  matrix=matrixScaledDown+noiseMatrix
  
  ;MTF from not degraded lsf
  ;IF d2 EQ 351 THEN BEGIN
    gg=getMTFgauss(A[0:2],0)
    ;pix=.1*501./351.
    CLIPBOARD.set, STRJOIN(TRANSPOSE(gg.k), STRING(9B))
    stop
;    fx=FINDGEN(szM(0)/2)*(1./(szM(0)*pix));resol = 1/NT + mm to cm
;    MTFkComplex=FFT(lsfKern,/CENTER)
;    MTFk=szM(0)*SQRT(REAL_PART(MTFkComplex)^2+IMAGINARY(MTFkComplex)^2); modulus of Fouriertransform * size of submatrix (divided by 1/N during FFT)
;    MTFk=MTFk[szM(0)/2:szM(0)-1]
;    MTFk=MTFk/max(MTFk)
    ;stop
  ;ENDIF
  
  
  return, matrix

end

