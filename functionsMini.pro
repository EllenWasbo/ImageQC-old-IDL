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

; return rescaled image within windowLevel (range) to display with tvscl
function adjustWindowLevel, arr, range

  high=WHERE(arr GT range(1)) & low=WHERE(arr LT range(0))
  IF high(0) NE -1 THEN arr(high)=range(1) & IF low(0) NE -1 THEN arr(low)=range(0)
  adjarr=(arr-range(0))*255.0/(range(1)-range(0))

  return, adjArr
end

;return image matrix scaled with slope and intercept
function readImg, adr, frame
  o=obj_new('idlffdicom')
  t=o->read(adr)
  
  ;image with correct values and rotation
  test=o->GetReference('0028'x,'1052'x)
  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
  IF test(0) NE -1 THEN intercept=FLOAT(*(test_peker[0])) ELSE intercept=0
  
  test=o->GetReference('0028'x,'1053'x)
  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
  IF test(0) NE -1 THEN slope=FLOAT(*(test_peker[0])) ELSE slope=1.
  
  test=o->GetReference('0018'x,'5100'x)
  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
  ori=0
  IF test(0) NE -1 THEN BEGIN
    temporient=STRTRIM(STRING(*(test_peker[0])),2)
    IF temporient EQ 'FFS' THEN ori=1
  ENDIF
  
  ;multiframe?
  multiframe=0
  test=o->GetReference('0028'x,'0008'x)
  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
  IF test(0) NE -1 THEN BEGIN 
    IF *(test_peker[0]) NE 1 THEN multiframe=1 
  ENDIF
  
  test=o->GetReference('7FE0'x,'0010'x)
  IF multiframe EQ 0 THEN BEGIN
    ;CT image is last image (icon images first)
    test_peker=o->GetValue(REFERENCE=test[N_ELEMENTS(test)-1],/NO_COPY)
    matrix=FLOAT(*(test_peker[0]))
  ENDIF ELSE BEGIN
    ;multiframe
    test_peker=o->GetValue(REFERENCE=test[frame],/NO_COPY)
    matrix=FLOAT(*(test_peker[0]))
  ENDELSE
  
  matrix=REVERSE(matrix,2)*slope + intercept
  IF ori EQ 1 THEN matrix=REVERSE(matrix)
  
  OBJ_DESTROY,o & PTR_FREE, test_peker
  return, matrix
end

;return max position [x,y] in array after medianfilter (width 5)
function findMedianMax, array
  array=MEDIAN(array, 5)
  pos=WHERE(array EQ MAX(array))
  posXY=ARRAY_INDICES(array, pos(0))
  return, posXY
end

function linearizeSTP, matrix, STP
  ;add other STP forms when ready....
  linMatrix=(matrix-STP.b)/STP.a(0)
  return, linMatrix
end

;adjust to resonable number of decimals
;function nDecimals, arr
;  nDec=8
;  maxa=MAX(ABS(arr))
;
;  IF maxa LT 0.0001 THEN nDec=7
;  IF maxa LT 0.001 THEN nDec=6
;  IF maxa LT 0.01 THEN nDec=5
;  IF maxa LT 0.1 THEN nDec=4
;  IF maxa LE 1 THEN nDec=3
;  IF maxa GT 1 THEN nDec=3
;  IF maxa GT 10 THEN nDec=2
;  IF maxa GT 100 THEN nDec=1
;
;  return, STRING(nDec, FORMAT='(i0)')
;end

;adjust to resonable number of decimals
function formatCode, arr
  maxa=MAX(ABS(arr))

  IF maxa LE 1 THEN BEGIN
    strInner='f0.3'
    IF maxa LT 0.1 THEN BEGIN
      strInner='f0.4'
      IF maxa LT 0.01 THEN BEGIN
        strInner='f0.5'
        IF maxa LT 0.001 THEN BEGIN
          strInner='g0.2'
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF maxa GT 1 THEN strInner='f0.3'
  IF maxa GT 10 THEN strInner='f0.2'
  IF maxa GT 100 THEN strInner='f0.1'
  IF maxa GT 99999 THEN strInner='g0.2'
  strFormatCode='('+strInner+')'

  return, strFormatCode
end

;smoothes irregularly sampled Y within +/- w
function smoothIrreg, X, Y, w
  IF w GT 0 THEN BEGIN
    nY=N_ELEMENTS(Y)
    smoothedY=0.0*Y
    FOR i=0, nY-1 DO BEGIN
      tempX=ABS(X-X(i))
      idx=WHERE(tempX LE w, nidx)
      fac=w-tempX(idx)
      fac=fac/TOTAL(fac)
      smoothedY(i)=TOTAL(fac*Y(idx))
    ENDFOR
  ENDIF ELSE smoothedY=Y
  return, smoothedY
end

;assure . not , for float-inputs
function comma2pointFloat, txt
  txt=STRJOIN(STRSPLIT(txt, ',',/EXTRACT),'.')
  return, txt
end

;centroid function not accurate enough, use centroid first and optimize center with this after
;centerProfile assume all positive values and values to be centered is the maximum values
;centerProfile also assume relatively smooth profiles
function centerProfile, vec

  sHalf= -1
  ;center=sHalf
  ;centerval=TOTAL(vec[sHalf(0)-1:sHalf(0)+1])/3.
  ;outerVal=(vec(0)+vec(N_ELEMENTS(vec)-1))/2
  ;IF centerval LT outerVal THEN vec= max(vec)-vec ;invert array
  
  nVec=N_ELEMENTS(vec)
  posMax=WHERE(vec EQ MAX(vec))
  posMax=ROUND(MEAN(posMax))
  IF posMax NE 0 AND posMax NE nVec-1 THEN centerVal=MEAN(vec[posMax-1:posMax+1]) ELSE centerVal=MAX(vec)
  nOut=CEIL(0.05*nVec)
  outerVal=0.5*(MEAN(vec[0:nOut])+MEAN(vec[nVec-nOut-1:nVec-1]))
  treshold=0.5*(centerVal+outerVal)
  above=WHERE(vec GT treshold, nn)
  ;remove noise even more
  abTemp=FLTARR(nVec)
  abTemp(above)=1.
  abTemp=SMOOTH(abTemp,3, /EDGE_MIRROR)
  above=WHERE(abTemp EQ 1., nn)

  If above(0) NE -1 THEN BEGIN
    first=above(0)
    last=above(nn-1)
    IF first GE 1 AND last LE N_ELEMENTS(vec)-2 THEN BEGIN
      dy=treshold-vec(first-1)
      IF vec(first) NE vec(first-1) THEN dx=dy/(vec(first)-vec(first-1)) ELSE dx=0.
      x1=first-1+dx
      
      dy=vec(last)-treshold
      IF vec(last) NE vec(last+1) THEN dx=dy/(vec(last)-vec(last+1)) ELSE dx=0.
      x2=last+dx
      sHalf=(x1+x2)/2
      IF sHalf GT nVec-1 THEN sHalf=-1
    ENDIF
  ENDIF
  
  RETURN, sHalf
end

;return [x,y] position for center of mass
;From: http://www.idlcoyote.com/tip_examples/centroid.pro
;added treshold and invert if object lower intensity than outer
function Centroid, array, treshold

  s = Size(array, /Dimensions)
  sHalf= s/2
  ;centerval=TOTAL(array[sHalf(0)-1:sHalf(0)+1,sHalf(1)-1:sHalf(1)+1])/9.
  outerVal=MEAN(array[*,0])
  IF abs(outerVal- min(array)) GT abs(outerVal- max(array)) THEN array= max(array)-array ;invert array

  lower=WHERE(array LT treshold)
  array=array-MIN(array);starting at zero
  arrTemp=array
  IF lower(0) NE -1 THEN arrTemp(lower)=0.
  
  ;totalMass = Total(arrTemp)
  
  ;IF totalMass GT 0 THEN BEGIN
  ;  x = Total( Total(arrTemp, 2) * Indgen(s[0]) ) / totalMass
  ;  y = Total( Total(arrTemp, 1) * Indgen(s[1]) ) / totalMass
  ;ENDIF ELSE BEGIN
  ;  x=sHalf[0]
  ;  y=sHalf[1]
  ;ENDELSE
  x=centerProfile(SMOOTH(TOTAL(arrTemp,2),3,/EDGE_MIRROR))
  y=centerProfile(SMOOTH(TOTAL(arrTemp,1),3,/EDGE_MIRROR))

  ;filter array and average of 3 neighbour profiles to remove noise
  ;filterw=CEIL(0.1*s(0))
  arrayFilt=SMOOTH(array,5,/EDGE_MIRROR)
  yNo=ROUND(y)
  xNo=ROUND(x)
  IF yNo GE 1 AND yNo LE s(1)-2 THEN vecX=TOTAL(arrayFilt[*,yNo-1:yNo+1],2)*1./3 ELSE vecX=arrayFilt[*,yNo]
  IF xNo GE 1 AND xNo LE s(0)-2 THEN vecY=TOTAL(arrayFilt[xNo-1:xNo+1,*],1)*1./3 ELSE vecY=arrayFilt[xNo,*]

  optimX=centerProfile(vecX)
  optimY=centerProfile(vecY)

  RETURN, [optimX, optimY]
end

;find position of treshold value position
function findPosTreshold, vec, treshold
  lenVec=N_ELEMENTS(vec)
  pos=-1
  above=WHERE(vec GT treshold, nn)
  If above(0) NE -1 AND nn NE lenVec THEN BEGIN
    rgt=above(0)
    IF rgt EQ 0 THEN rgt=above(nn-1)+1
    lft=rgt-1

    IF lft GT 0 AND lft LT lenVec AND rgt GT 0 AND rgt LT lenVec THEN BEGIN
      dy=treshold-vec(lft)
      dx=dy/(vec(rgt)-vec(lft))

      pos=lft+dx
    ENDIF
  ENDIF

  return, pos
end

;find x for given y where linear line is given by two points (left/right)
function getInterpX, Y,x1,x2,y1,y2
  w=(Y-y2)/(y1-y2)
  X=w*x1+(1-w)*x2
  
  RETURN, X
end

;find the vector from image given start and end position [x,y]
;modified part of IDL function:profile - without selection by mouse part
function getProfile, imagein, start, ende
  s=size(imagein)
  sx = s[1] & sy=s[2]
  dx = float(ende(0)-start(0))       ;delta x
  dy = float(ende(1)-start(1))
  n = abs(dx) > abs(dy)
  r = fltarr(n+1)
  
  if abs(dx) gt abs(dy) then begin
    if ende(0) ge start(0) then s=1 else s=-1
    sy = (ende(1)-start(1))/abs(dx)
  endif else begin
    if ende(1) ge start(1) then sy=1 else sy=-1
    s = (ende(0)-start(0))/abs(dy)
  endelse
  
  xx = long(findgen(n+1l)*s+start(0))    ;X values, make into longwords.
  yy = long(findgen(n+1l)*sy+start(1))   ;Y values
  
  return,imagein[long(yy)*sx + xx]
end

;get circular mask within array of size arrSz
;center of circle [x,y] pix
function getROIcircle, arrSz, center, radius

  arr=SHIFT(DIST(arrSz(0),arrSz(1)),center(0),center(1))
  in=WHERE(arr LE radius)
  circle=INTARR(arrSz(0), arrSz(1))
  circle(in)=1
  
  return, circle
end

;remove ids from structure of structures
function removeIDstructstruct, struct, ids
  structNew=CREATE_STRUCT('EMPTY',0)
  counter=0
  ntags=N_TAGS(struct)
  tagname=TAG_NAMES(struct)
  FOR i=0, ntags-1 DO BEGIN
    inSel=WHERE(ids EQ i)
    IF inSel(0) EQ -1 THEN BEGIN
      stillEmpty=WHERE(TAG_NAMES(structNew) EQ 'EMPTY')
      IF stillEmpty(0) EQ -1 THEN structNew=CREATE_STRUCT(structNew,tagname(counter),struct.(i)) ELSE structNew=CREATE_STRUCT(tagname(0),struct.(i))
      counter=counter+1
    ENDIF
  ENDFOR
  return, structNew
end

;reorder ids in structure of structures
function reorderStructStruct, struct, newOrder  
  ntags=N_TAGS(struct)
  tagname=TAG_NAMES(struct)
  structNew=CREATE_STRUCT(tagname(newOrder(0)),struct.(newOrder(0)))
  FOR i=1, ntags-1 DO structNew=CREATE_STRUCT(structNew,tagname(newOrder(i)),struct.(newOrder(i))) 
  return, structNew
end

;find the width of a profile at specified threshold value using interpolation. Return width and centerpos of profile at threshold
function getWidthAtThreshold, vec, threshold
  width=-1
  center=-1
  
  aboveInd=WHERE(vec GT threshold)
  belowInd=WHERE(vec LT threshold)
  
  IF aboveInd(0) GE 1 THEN above=aboveInd ELSE above=belowInd  
  nn=N_ELEMENTS(above)

  If above(0) GE 1 THEN BEGIN
    first=above(0)
    last=above(nn-1)
    IF last LT N_ELEMENTS(vec)-1 THEN BEGIN
    
      dy=vec(first)-threshold;dy=treshold-vec(first-1)
      dx=dy/(vec(first)-vec(first-1))
      x1=first-dx;x1=first+dx
      
      dy=vec(last)-threshold
      dx=dy/(vec(last)-vec(last+1))
      x2=last+dx
      center=(x1+x2)/2
      width=x2-x1
    ENDIF
  ENDIF

  return, [width,center]

end

function ESFtoLSF, esfVec
  n=N_ELEMENTS(esfVec)
  lsfVec=esfVec*0.
  FOR i=1, n-2 DO BEGIN
    lsfVec(i)=esfVec(i+1)-esfVec(i-1)
  ENDFOR
  return, lsfVec
end

;calculate y(x) given x,sigma,A
function calcGauss, xvals, stdev, amp, meanval
  yvals=amp*EXP(-.5*(xvals-meanval)^2/stdev^2)
  return, yvals
end

;fit, X,Y to gaussian, allow for visual verification and manual edit, X assumed to be centered already
function getGaussFit, X, Y, pix, fitWidthFactor; assume X already centered
  nn=N_ELEMENTS(X)
  weights= FLTARR(nn)+1.
  res=getWidthAtThreshold(Y,max(Y)/2)
  
  ss1=0
  yfit=-1
  A=-1
  IF res(0) NE -1 THEN BEGIN
    FWHM1=res(0)*pix
    center=ABS(X(0))
    sigma1=FWHM1/(2*SQRT(2*ALOG(2)))
    
    res2=getWidthAtThreshold(Y,min(Y)/2) 
    IF res2(0) NE -1 THEN BEGIN
      FWHM2=res2(0)*pix
      sigma2=FWHM2/(2*SQRT(2*ALOG(2)))
      IF sigma2 LT sigma1 THEN sigma2=2.*sigma1
    ENDIF ELSE sigma2=2.*sigma1

    IF fitWidthFactor EQ 0 THEN ss1=0 ELSE ss1=ROUND(center/pix)-ROUND(FWHM1/pix)*fitWidthFactor
    IF fitWidthFactor EQ 0 THEN ss2=nn-1 ELSE ss2=ROUND(center/pix)+ROUND(FWHM1/pix)*fitWidthFactor
    IF ss1 LT 0 THEN ss1=0
    IF ss2 GT nn-1 THEN ss2=nn-1
    A = [max(Y[ss1:ss2])-min(Y[ss1:ss2]),1.5*min(Y[ss1:ss2]),sigma1, sigma2];first guess parameters for curvefit gaussFitAdd2
  
    yfit = CURVEFIT(X[ss1:ss2], Y[ss1:ss2], weights[ss1:ss2], A, FUNCTION_NAME='gaussFitAdd2', ITER=iter, CHISQ=chisq);, TOL=.00001*(1.0*10^(-3)));, ITMAX=100)

    IF A(1) GT A(0) THEN BEGIN
      ;resort such that highest amp first
      newA=[A(1),A(0),A(3),A(2)]
      A=newA
    ENDIF

    IF ABS(A[3]) GT 10.*A[2] OR A[3] LT 0 THEN BEGIN; retry with single gaussfit - allow double gauss with both terms positive
    ;IF A(1) GT 0 OR ABS(A[3]) GT 10.*A[2] THEN BEGIN; retry with single gaussfit - double is for sharp filters
      yfit=gaussfit(X[ss1:ss2], Y[ss1:ss2], A, ESTIMATES=[max(Y[ss1:ss2]),0,sigma1], NTERMS=3)
      A(1)=0
    ENDIF

   ENDIF; res(0)=-1
   retStruct=CREATE_STRUCT('yfit',yfit,'A',A,'startpos',ss1)
    
   return, retStruct
end

  ;gauss to gauss continuous version:
  ;http://www.cse.yorku.ca/~kosta/CompVis_Notes/fourier_transform_Gaussian.pdf
function getMTFgauss, A, sigmaF
  nSteps=200;sample 20 steps from 0 to 1 stdv MTF curve A0 (stdev=1/A(2))
  kvals=FINDGEN(nSteps)*(10./nSteps)/A(2)
  Fgu0=calcGauss(kvals, 1/A(2),A(0)*A(2),0)
  IF N_ELEMENTS(A) EQ 4 THEN Fgu1=calcGauss(kvals, 1/A(3),A(1)*A(3),0) ELSE Fgu1=0.
  If sigmaF NE 0 THEN Ffilter=calcGauss(kvals,1./(sigmaF),1.0,0) ELSE Ffilter=1.
  MTF=(Fgu0+Fgu1)/Ffilter
  k=kvals/(2*!pi)

  MTF=MTF/MTF(0)
  
  gradient=SHIFT(MTF,-1)-MTF
  ng=N_ELEMENTS(gradient)-2
  gradient=gradient[0: ng];pop last
  IF gradient(ng-1) GT 0 THEN BEGIN
    negVal=WHERE(gradient LT 0, nNeg)
    unFilt=Fgu0+Fgu1
    unFilt=unFilt/unFilt(0)
    MTF[nNeg:nSteps-1]=unFilt[nNeg:nSteps-1]
  ENDIF
  retStruct=CREATE_STRUCT('k',k,'MTF',MTF)
  return, retStruct
end

; return list of filenames for open files
; struc = structure of structures from readCT.pro
; full = 0 for only parentfolder\filename, =1 for full path
; marked = array of indexes for marked files, -1 means none is marked
;   full=1 returns only marked, full=0 returns all and set an X on the marked
function getListOpenFiles, struc, full, marked

  nn=N_TAGS(struc)
  markedArr=INTARR(nn)
  IF marked(0) NE -1 THEN markedArr(marked)=1 ELSE markedArr=markedArr+1
  IF full EQ 1 THEN BEGIN
    fileList=STRARR(TOTAL(markedArr))
    counter=0
    FOR i=0, nn-1 DO BEGIN
      IF markedArr(i) EQ 1 THEN BEGIN
        fileList(counter)=struc.(i).filename
        counter=counter+1
      ENDIF
    ENDFOR
  ENDIF ELSE BEGIN
    fileList=STRARR(nn)
    FOR i=0, nn-1 DO BEGIN
      IF markedArr(i) AND marked(0) NE -1 THEN add='X ' ELSE add='   '
      t=STRSPLIT(struc.(i).filename,'\',/EXTRACT)
      nSplit=N_ELeMENTS(t)
      
      fileList(i)=add+STRJOIN(t[nSplit-2:nSplit-1],'\')
      
    ENDFOR
  ENDELSE
  
  return, fileList
end

;similar to getListOpenFiles only with one multiframe file
function getListFrames, struc, marked
  nn=struc.nFrames
  markedArr=INTARR(nn)
  IF marked(0) NE -1 THEN markedArr(marked)=1 ELSE markedArr=markedArr+1
  imgList=STRARR(nn)
  
  o=obj_new('idlffdicom')
  t=o->read(struc.filename)
  test=o->GetReference('0020'x,'0032'x)
  
  strAdd='Img number '
  angles=0
  IF N_ELEMENTS(test) EQ nn THEN BEGIN
     strAdd='Img pos ' 
  ENDIF ELSE BEGIN
    test=o->GetReference('0054'x,'0090'x); angular view vector
    IF test(0) NE -1 THEN BEGIN 
      test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
      angleVec=*(test_peker[0])
      angles=STRSPLIT(angleVec, '\', /EXTRACT)
      strAdd='Angle '
    ENDIF
  ENDELSE
      
    FOR i=0, nn-1 DO BEGIN
      IF markedArr(i) AND marked(0) NE -1 THEN add='X ' ELSE add='   '
      CASE strAdd OF
        'Img pos ': BEGIN
          test_peker=o->GetValue(REFERENCE=test[i],/NO_COPY)
          imgpos=*(test_peker[0])
          END
          'Angle ': imgpos= angles(i)
          ELSE: imgpos = STRING(i, FORMAT='(i0)')
      ENDCASE
      
      imgList(i)=add+strAdd+imgpos    
    ENDFOR


  OBJ_DESTROY,o 
  stest=size(test_peker, /TNAME)
  IF stest EQ 'POINTER' THEN PTR_FREE, test_peker
  return, imgList
end

;calculate filter given sigma and size of filter
function gaussFilter, sigmaF, nn
  filter=-1
  If sigmaF NE 0 THEN BEGIN
    IF nn*.5 EQ nn/2 THEN odd=0 ELSE odd=1
    nnn=nn/2-odd
    xf=FINDGEN(nnn)
    yf=EXP(-0.5*xf^2/sigmaF^2)
    filter=[reverse(yf[1:nnn-1]),yf]
    IF odd EQ 0 THEN filter=[0,filter]
    filter=filter/TOTAL(filter)
    nonZeros=WHERE(filter NE 0.)
    filter=filter(nonZeros)
  Endif
   return, filter
end

function getResNmb, tabNmb, stringElem, stringArr0, stringArr1, stringArr2, stringArr3
  CASE tabNmb OF
    0: actStrings=stringArr0
    1: actStrings=stringArr1
    2: actStrings=stringArr2
    3: actStrings=stringArr3
  ENDCASE
  i=WHERE(actStrings EQ stringElem)
  resNmb=i
return, resNmb
end

function FFTvector, vec, padfactor
  szV=N_ELEMENTS(vec)
  szPadded=2*((padfactor*szV)/2);assure even number
  halfsz=szV/2
  nullPadd=FLTARR(szPadded)
  nullPadd[szPadded/2-halfsz:szPadded/2-halfsz+szV-1]=vec
  vecPadd=nullPadd
  fvecComplex=FFT(vecPadd,/CENTER)
  fvec=szPadded*SQRT(REAL_PART(fvecComplex)^2+IMAGINARY(fvecComplex)^2); modulus of Fouriertransform * size of submatrix (divided by 1/N during FFT)
  fvec=fvec[szPadded/2:szPadded-1]
  fvec=fvec/fvec(0)
  return, fvec
end

function zeroPadd3, matrix

  sz=SIZE(matrix, /DIMENSIONS)
  padded=FLTARR(3*sz(0),3*sz(1))
  padded[sz(0):sz(0)*2-1,sz(1):sz(1)*2-1]=matrix

  return, padded
end

function getZposMarked, struc, markedTemp
  nFrames=0
  IF struc.(0).nFrames GT 1 THEN BEGIN
    nImg=struc.(0).nFrames
    nFrames=nImg
  ENDIF ELSE nImg=N_ELEMENTS(TAG_NAMES(struc))
  zPos=FLTARR(nImg)
  IF nFrames EQ 0 THEN BEGIN
    FOR i = 0, nImg -1 DO zPos(i)=struc.(i).zpos
  ENDIF ELSE BEGIN
    zPos=struc.(0).zPos
  ENDELSE
  zPosMarked=zPos(markedTemp)
  return, zPosMarked
end