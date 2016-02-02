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
function calculateNPS, subMatrix, ROIsz, nSub, pix, stp, imgNo
  ;Code based on:
  ; IEC 62220-1 (2003) & IPEM 32

  subM=subMatrix
  

  IF SIZE(stp, /TNAME) EQ 'STRUCT' THEN BEGIN
    subM=linearizeSTP(subM,stp)
    ;Qval=stp.table[1,imgNo] 
  ENDIF; ELSE stop; set STP : subM=(subM-17.2)/107.

  szM=SIZE(subM,/DIMENSIONS)
  IMAGE_STATISTICS, subM, MEAN=meanVal, STDDEV=stddevVal
  ;subtract 2nd order 2d polynomial fit - to remove trend 
  subtr=SFIT(subM, 2)
  subM=subM-subtr;+meanVal?
  
  nROIs=(nSub*2-1)^2
  ;NPS=FLTARR(3*ROIsz,3*ROIsz)
  NPS=FLTARR(ROIsz,ROIsz)
  
  ;2d fourier of each ROI
  FOR i = 0, nSub*2-2 DO BEGIN
    FOR j = 0, nSub*2-2 DO BEGIN
      x1=0.5*i*ROIsz & x2=(0.5*i+1)*ROIsz-1
      y1=0.5*j*ROIsz & y2=(0.5*j+1)*ROIsz-1
      ;temp=FFT(zeroPadd3(subM[x1:x2,y1:y2]),/CENTER)
      temp=FFT(subM[x1:x2,y1:y2],/CENTER)
      NPSthis=REAL_PART(temp)^2+IMAGINARY(temp)^2
      NPS=NPS+NPSthis
    ENDFOR
  ENDFOR

  NPS=ROIsz^2*((pix(0)*pix(1))/nROIs)*NPS

  nRows=7
;  uNPS=NPS[*,ROIsz+ROIsz/2-nRows:ROIsz+ROIsz/2-1]+NPS[*,ROIsz+ROIsz/2+1:ROIsz+ROIsz/2+nRows]
;  szuNPS=SIZE(uNPS, /DIMENSIONS)
;  uNPS=TOTAL(uNPS,2)/szuNPS(1)
;  vNPS=NPS[ROIsz+ROIsz/2-nRows:ROIsz+ROIsz/2-1,*]+NPS[ROIsz+ROIsz/2+1:ROIsz+ROIsz/2+nRows,*]
;  vNPS=TOTAL(vNPS,1)/szuNPS(1)
;
;  uNPS=uNPS[ROIsz+ROIsz/2+1:3*ROIsz-1]
;  vNPS=vNPS[ROIsz+ROIsz/2+1:3*ROIsz-1]
;
;  du=(FINDGEN(N_ELEMENTS(uNPS))+1)*(1./(3*ROIsz*pix(0)))

  uNPS=[[NPS[*,ROIsz/2-nRows:ROIsz/2-1]],[NPS[*,ROIsz/2+1:ROIsz/2+nRows]]]
  szuNPS=SIZE(uNPS, /DIMENSIONS)
  uNPS=TOTAL(uNPS,2)/szuNPS(1)
  vNPS=[NPS[ROIsz/2-nRows:ROIsz/2-1,*],NPS[ROIsz/2+1:ROIsz/2+nRows,*]]
  vNPS=TOTAL(vNPS,1)/szuNPS(1)

  uNPS=uNPS[ROIsz/2+1:ROIsz-1]
  vNPS=vNPS[ROIsz/2+1:ROIsz-1]

  du=(FINDGEN(N_ELEMENTS(uNPS))+1)*(1./(ROIsz*pix(0)))
  
  uAUC=INT_TABULATED(du,uNPS)
  vAUC=INT_TABULATED(du,vNPS)
  
  
  NPSstruct=CREATE_STRUCT('du',du,'uNPS',uNPS,'vNPS',vNPS,'NPS',NPS, 'largeAreaSignal', meanVal, 'AUC', [uAUC,vAUC], 'variance',stddevVal^2)

  print, 'uAUC', uAUC
  print, 'vAUC', vAUC
  print, 'variance', stddevVal^2

  return, NPSstruct
end