function Bsort, Array, Asort, INFO=info, REVERSE = rev
  ;+
  ; NAME:
  ;       BSORT
  ; PURPOSE:
  ;       Function to sort data into ascending order, like a simple bubble sort.
  ; EXPLANATION:
  ;       Original subscript order is maintained when values are equal (stable sort).
  ;       (This differs from the IDL SORT routine alone, which may rearrange
  ;       order for equal values)
  ;
  ;       A faster algorithm (radix sort) for numeric data is described at
  ;http://www.harrisgeospatial.com/Support/SelfHelpTools/HelpArticles/HelpArticles-Detail/TabId/2718/ArtMID/10220/ArticleID/16724/An-LSD-radix-sort-algorithm-in-IDL.aspx
  ;       and available at
  ;       https://github.com/mgalloy/mglib/blob/master/src/analysis/mg_sort.pro
  ; CALLING SEQUENCE:
  ;       result = bsort( array, [ asort, /INFO, /REVERSE ] )
  ;
  ; INPUT:
  ;       Array - array to be sorted
  ;
  ; OUTPUT:
  ;       result - sort subscripts are returned as function value
  ;
  ; OPTIONAL OUTPUT:
  ;       Asort - sorted array
  ;
  ; OPTIONAL KEYWORD INPUTS:
  ;       /REVERSE - if this keyword is set, and non-zero, then data is sorted
  ;                 in descending order instead of ascending order.
  ;       /INFO = optional keyword to cause brief message about # equal values.
  ;
  ; HISTORY
  ;       written by F. Varosi Oct.90:
  ;       uses WHERE to find equal clumps, instead of looping with IF ( EQ ).
  ;       compatible with string arrays, test for degenerate array
  ;       20-MAY-1991     JKF/ACC via T AKE- return indexes if the array to
  ;                       be sorted has all equal values.
  ;       Aug - 91  Added  REVERSE keyword   W. Landsman
  ;       Always return type LONG    W. Landsman     August 1994
  ;       Converted to IDL V5.0   W. Landsman   September 1997
  ;-
  N = N_elements( Array )
  if N lt 1 then begin
    print,'Input to BSORT must be an array'
    return, [0L]
  endif

  if N lt 2 then begin
    asort = array       ;MDM added 24-Sep-91
    return,[0L]    ;Only 1 element
  end
  ;
  ; sort array (in descending order if REVERSE keyword specified )
  ;
  subs = sort( Array )
  if keyword_set( REV ) then subs = rotate(subs,5)
  Asort = Array[subs]
  ;
  ; now sort subscripts into ascending order
  ; when more than one Asort has same value
  ;
  weq = where( (shift( Asort, -1 ) eq Asort) , Neq )

  if keyword_set( info ) then $
    message, strtrim( Neq, 2 ) + " equal values Located",/CON,/INF

  if (Neq EQ n) then return,lindgen(n) ;Array is degenerate equal values

  if (Neq GT 0) then begin

    if (Neq GT 1) then begin              ;find clumps of equality

      wclump = where( (shift( weq, -1 ) - weq) GT 1, Nclump )
      Nclump++

    endif else Nclump = 1

    if (Nclump LE 1) then begin
      Clump_Beg = 0
      Clump_End = Neq-1
    endif else begin
      Clump_Beg = [0,wclump+1]
      Clump_End = [wclump,Neq-1]
    endelse

    weq_Beg = weq[ Clump_Beg ]              ;subscript ranges
    weq_End = weq[ Clump_End ] + 1          ; of Asort equalities.

    if keyword_set( info ) then message, strtrim( Nclump, 2 ) + $
      " clumps of equal values Located",/CON,/INF

    for ic = 0L, Nclump-1 do begin          ;sort each clump.

      subic = subs[ weq_Beg[ic] : weq_End[ic] ]
      subs[ weq_Beg[ic] ] = subic[ sort( subic ) ]
    endfor

    if N_params() GE 2 then Asort = Array[subs]     ;resort array.
  endif

  return, subs
end

;added by Ellen Wasbø 2020 to sort on multilevel
;keyArr = STRING array with each row representing the strings to sort on each level
;descKey = INTARR with one flag pr row in keyArr, 0 = asc, 1 = desc
function multiBsort, keyArr, descKey
  szKey=SIZE(keyArr,/DIMENSIONS)
  order=-1
  IF szKey(0) NE 0 THEN BEGIN
    IF N_ELEMENTS(szKey) EQ 1 THEN szKey=[szKey,1]
    nKeys=szKey(1)
    ;check if empty keys
    actKeys=!Null
    FOR k=0,nKeys-1 DO BEGIN
      idNotEmpty=WHERE(keyArr[*,k] NE '')
      IF idNotEmpty(0) NE -1 THEN actKeys=[actKeys,k]
    ENDFOR
    nKeys=N_ELEMENTS(actKeys)
    thisKeys=keyArr[*,0]
    order=bsort(thisKeys,REVERSE=descKey(0))
    IF nKeys GT 1 THEN BEGIN
      vals=thisKeys(uniq(thisKeys,order))
      FOR k=1, nKeys-1 DO BEGIN     
        IF N_ELEMENTS(prevKeys) EQ 0 THEN prevKeys=thisKeys ELSE prevKeys=prevKeys+'_'+thisKeys
        thisKeys=keyArr[*,k]
        vals=prevKeys(uniq(prevKeys,order));ordered keys by now
        counter=0
        FOR v=0, N_ELEMENTS(vals)-1 DO BEGIN;for each uniq value combination from previous keys
          ids=WHERE(prevKeys EQ vals(v),nEqVals)
          IF N_ELEMENTS(ids) GT 1 THEN BEGIN
            actKeys=thisKeys(ids)
            orderAct=order[counter:counter+nEqVals-1]
            IF k LT N_ELEMENTS(descKey) THEN rev=descKey(k) ELSE rev=0
            subOrder=bsort(actKeys,REVERSE=rev)
            order[counter:counter+nEqVals-1]=orderAct(subOrder)
            counter=counter+nEqVals
          ENDIF ELSE counter=counter+1
        ENDFOR
      ENDFOR
    ENDIF
  ENDIF
  return, order
end