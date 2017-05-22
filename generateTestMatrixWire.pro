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

function generateTestMatrixWire

s=275
d2=55;137;pix 0,201; 55 for pix 0,5
A=[700.0,-40,1.4*10,3.*10];[700.0,-40,0.7*10,2.0*10];[600.,0.,.7*10,2.*10];
radFraction=0.01; .01=bead, .6 brukt til disk forsok
outVal=100
inVal=9000;-1000
nImg1=5;start with 45 deg rotation
nImg2=nImg1*10;interpolate to less steap angle
noiseFactor=.5;200;50

;center=[s,s+5];[s+5,s+9,s+12,s+19]
center=[s,s]

matrix=FLTARR(s*2+1,s*2+1,nImg1)+outVal

FOR i=0,nImg1-1 DO matrix[s+i,s+i,i]=inVal
matrix2=CONGRID(matrix, s*2+1, s*2+1, nImg2, /INTERP)

X=FINDGEN(s); pixstr/10
F=calcGauss(X,A[2],A[0],0)+calcGauss(X,A[3],A[1],0); sum of two gaussfunctions, both with mean=0

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

matrixConvol=0.0*matrix2
FOR i=0,nImg2-1 DO matrixConvol[*,*,i]=SHIFT(FFT(FFT(matrix2[*,*,i])*FFT(kernel),/INVERSE),s,s)*((s*2.+1)^2); *N^2

matrixScaledDown= CONGRID(matrixConvol, d2,d2,nImg2,/Center,cubic=-0.5)

noiseMatrix=noiseFactor*randomn(seed,d2,d2,nImg2)
matrixOut=matrixScaledDown+noiseMatrix

;MTF from not degraded lsf
IF d2 EQ 551 THEN BEGIN
pix=0.05

szPadded=275*2-1

fx=FINDGEN(szPadded/2)*(1./(szPadded*0.1*pix));resol = 1/NT + mm to cm
lsfk=lsfKern[1:549]
MTFkComplex=FFT(LSFk,/CENTER)
MTFk=szPadded*SQRT(REAL_PART(MTFkComplex)^2+IMAGINARY(MTFkComplex)^2); modulus of Fouriertransform * size of submatrix (divided by 1/N during FFT)
MTFk=MTFk[szPadded/2:szPadded-1]
MTFk=MTFk/MTFk(0)
stop

ENDIF

return, matrixOut
end