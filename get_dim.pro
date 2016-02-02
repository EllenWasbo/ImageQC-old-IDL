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

function get_center_rod, subimg

  filtimg=MEDIAN(subimg,3)
  minimg=MIN(filtimg)
  maximg=MAX(filtimg)
  halfmax=0.5*(minimg+maximg)
  center=centroid(subimg, halfmax)

  return, center
end

;find rods in module CTP404
function get_dim, img, imgCenter, pix

margin=10; radius in mm

rodCenter=FLTARR(2,4); clockwise starting with upper left
mShort=ROUND((25-margin)/pix(0))
mLong=ROUND((25+margin)/pix(0))
imSz=SIZE(img, /DIMENSIONS)
IF imgCenter(0)-mLong GE 0 AND imgCenter(0)+mLong LT imSz(0) AND imgCenter(1)-mLong GE 0 AND imgCenter(1)+mLong LT imSz(1) THEN BEGIN
  rodCenter[*,0]=get_center_rod(img[imgCenter(0)-mLong:imgCenter(0)-mShort,imgCenter(1)+mShort:imgCenter(1)+mLong]);upper left
  rodCenter[*,1]=get_center_rod(img[imgCenter(0)+mShort:imgCenter(0)+mLong,imgCenter(1)+mShort:imgCenter(1)+mLong]);upper right
  rodCenter[*,2]=get_center_rod(img[imgCenter(0)+mShort:imgCenter(0)+mLong,imgCenter(1)-mLong:imgCenter(1)-mShort]);lower right
  rodCenter[*,3]=get_center_rod(img[imgCenter(0)-mLong:imgCenter(0)-mShort,imgCenter(1)-mLong:imgCenter(1)-mShort]); lower left
ENDIF ELSE rodCenter=rodCenter-1

IF MIN(rodCenter) EQ -1 THEN status=0 ELSE status=1

rodCenter[0,0]=(-mLong+rodCenter[0,0])*pix(0)
rodCenter[1,0]=(mShort+rodCenter[1,0])*pix(0)

rodCenter[0,1]=(mShort+rodCenter[0,1])*pix(0)
rodCenter[1,1]=(mShort+rodCenter[1,1])*pix(0)

rodCenter[0,2]=(mShort+rodCenter[0,2])*pix(0)
rodCenter[1,2]=(-mLong+rodCenter[1,2])*pix(0)

rodCenter[0,3]=(-mLong+rodCenter[0,3])*pix(0)
rodCenter[1,3]=(-mLong+rodCenter[1,3])*pix(0)

resArr=FLTARR(6)
resArr=[SQRT((rodCenter[0,1]-rodCenter[0,0])^2+(rodCenter[1,1]-rodCenter[1,0])^2),$
      SQRT((rodCenter[0,2]-rodCenter[0,3])^2+(rodCenter[1,2]-rodCenter[1,3])^2),$
      SQRT((rodCenter[0,3]-rodCenter[0,0])^2+(rodCenter[1,3]-rodCenter[1,0])^2),$
      SQRT((rodCenter[0,1]-rodCenter[0,2])^2+(rodCenter[1,1]-rodCenter[1,2])^2),$
      SQRT((rodCenter[0,2]-rodCenter[0,0])^2+(rodCenter[1,2]-rodCenter[1,0])^2),$
      SQRT((rodCenter[0,1]-rodCenter[0,3])^2+(rodCenter[1,1]-rodCenter[1,3])^2)]

return, CREATE_STRUCT('dists',resArr,'centers',rodCenter, 'status', status)

end