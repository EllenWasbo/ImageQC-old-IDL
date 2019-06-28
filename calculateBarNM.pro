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


;Calculate resolution from bar-phantom (gamma camera)
function calculateBarNM, barImg, barROI, pix
  szI=SIZE(barImg,/DIMENSIONS)

  barMTF=FLTARR(4)

  FOR i=0,3 DO BEGIN
    thisMask=barROI[*,*,i]
    IMAGE_STATISTICS, barImg, MASK=thisMask, MEAN=meanROI, VARIANCE=varROI
    barMTF(i)=SQRT(2*(varROI-meanROI))/meanROI
  ENDFOR

  return, barMTF
end
