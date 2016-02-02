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

;fit to sum of two gaussfunctions, both with mean=0
PRO gaussFitAdd2, X, A, F, pder
  ;X=X-u
  gu0=EXP(-.5*(X/A[2])^2)
  gu1=EXP(-.5*(X/A[3])^2)
  F=A[0]*gu0+A[1]*gu1

  IF N_PARAMS() GE 4 THEN BEGIN 
    dfdA0=gu0
    dfdA1=gu1
    dfdA2=A[0]*gu0*(X^2)/(A[2]^3)
    dfdA3=A[1]*gu1*(X^2)/(A[3]^3)
    
    pder=[[dfdA0],[dfdA1],[dfdA2],[dfdA3]]
  ENDIF

END

;fit to exponential e^(-Ax)
PRO expFit, X, A, F, pder
  F=A[0]*EXP(-A[1]*SQRT(X^2))
  
  IF N_PARAMS() GE 4 THEN BEGIN
    dfdA0=EXP(-A[1]*SQRT(X^2))
    dfdA1=-A[1]*A[0]*EXP(-A[1]*SQRT(X^2))
    pder=[[dfdA0],[dfdA1]]
  ENDIF
  
END