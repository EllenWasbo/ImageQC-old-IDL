;ImageQC based on getSiemensQC - extraction of figures from Siemens QC reports (CT constancy/daily)
;Copyright (C) 2021  Ellen Wasbo, Stavanger University Hospital, Norway
;ellen@wasbo.no
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License version 2
;as published by the Free Software Foundation.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See thef
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

function formatDDMMYYYY, str
  strSpl=STRSPLIT(str,'-',/EXTRACT)
  strDMY=str
  IF N_ELEMENTS(strSpl) EQ 3 THEN BEGIN
    IF STRLEN(strSpl(0)) EQ 4 THEN BEGIN
      strDMY=strSpl(2)+'.'+strSpl(1)+'.'+strSpl(0)
    ENDIF
  ENDIF
  return, strDMY
end

function getTypeAndLanuage, clipres, configGetSiemensQC

  ;detect language and type of report
  nLangu=N_TAGS(configGetSiemensQC.CT)
  langu=0
  daily=0
  const=0
  typeRep=-1
  typeStr=''

  symbiaT=WHERE(STRMATCH(clipres[0:1], '*Symbia T*', /FOLD_CASE) EQ 1); or old Syngo versions?
  symbiaIntevo=WHERE(STRMATCH(clipres[0:1], '*Symbia Intevo*', /FOLD_CASE) EQ 1)
  IF symbiaIntevo(0) EQ -1 THEN symbiaIntevo=0 ELSE symbiaIntevo=1

  IF symbiaT (0) NE -1 THEN BEGIN;inconsistent language
    symbiaT=1
    dailyStr=STRJOIN(STRSPLIT(configGetSiemensQC.CT.(0)(0),' ',/EXTRACT),'*')
    daily=WHERE(STRMATCH(clipres, '*'+dailyStr+'*', /FOLD_CASE) EQ 1)
    IF daily(0) NE -1 THEN BEGIN
      typeRep=0
      typeStr=dailyStr
    ENDIF ELSE BEGIN
      constStr=STRJOIN(STRSPLIT(configGetSiemensQC.CT.(0)(1),' ',/EXTRACT),'*')
      const=WHERE(STRMATCH(clipres, '*'+constStr+'*', /FOLD_CASE) EQ 1)
      IF const(0) NE -1 THEN BEGIN
        typeRep=1
        typeStr=constStr
      ENDIF
    ENDELSE
    FOR i=0, nLangu-1 DO BEGIN
      rowno=WHERE(STRMATCH(clipres, configGetSiemensQC.CT.(i)(10)+'*', /FOLD_CASE) EQ 1)
      IF rowno NE -1 THEN BEGIN
        langu=i
        BREAK
      ENDIF
    ENDFOR

  ENDIF ELSE BEGIN
    symbiaT=0
    FOR i=0, nLangu-1 DO BEGIN
      dailyStr=STRJOIN(STRSPLIT(configGetSiemensQC.CT.(i)(0),' ',/EXTRACT),'*')
      daily=WHERE(STRMATCH(clipres, '*'+dailyStr+'*', /FOLD_CASE) EQ 1)
      IF daily(0) NE -1 THEN BEGIN
        langu=i
        typeRep=0
        typeStr=dailyStr
        BREAK
      ENDIF
      constStr=STRJOIN(STRSPLIT(configGetSiemensQC.CT.(i)(1),' ',/EXTRACT),'*')
      const=WHERE(STRMATCH(clipres, '*'+constStr+'*', /FOLD_CASE) EQ 1)
      IF const(0) NE -1 THEN BEGIN
        langu=i
        typeRep=1
        typeStr=constStr
        BREAK
      ENDIF
    ENDFOR
  ENDELSE

  return, CREATE_STRUCT('languageNo',langu,'reportType',typeStr, 'reportTypeNo', typeRep, 'symbiaT',symbiaT,'symbiaIntevo',symbiaIntevo)
end

function readCTserial, clipres, configGetSiemensQC
  serialString=''
  
  typeAndLanguage=getTypeAndLanuage(clipres, configGetSiemensQC)

  IF typeAndLanguage.REPORTTYPENO EQ -1 THEN errMsg='No results in file or unexpected content or language.' ELSE BEGIN 
    ;serial Number
    langu=typeAndLanguage.LANGUAGENO
    rowno=WHERE(STRMATCH(clipres, configGetSiemensQC.CT.(langu)(12)+'*', /FOLD_CASE) EQ 1)
    serialString=STRTRIM(STRMID(clipres(rowno(0)),STRLEN(configGetSiemensQC.CT.(langu)(12))),2)
  ENDELSE
  return, serialString
end

function readCTconstancy, clipres, configGetSiemensQC
  errMsg=''
  ;tolerance
  ;HU water +/-4 from ref, but never above +/-4
  ;diff +/-2 from ref
  ;noise +/-10% from ref
  ;slice +/- 1mm from ref, but never more than +/-1mm from nominal
  ;MTF B30f 3,02-3,70 / 5,17-6,31
  ;MTF H30s 2,84-3,47 / 5,13-6,27
  ;MTF H70h 10,24-12,52 / 12,60-15,4

  strArrRes=STRARR(26+16);16 extra for double tube

  IF N_ELEMENTS(clipres) GT 1 THEN BEGIN

    nShortRes=15
    shortres=STRARR(N_ELEMENTS(clipres))
    shortres5=shortres
    FOR i=0, N_ELEMENTS(clipres)-1 DO BEGIN
      shortres(i)=STRMID(clipres(i), 0, nShortRes)
      shortres5(i)=STRMID(clipres(i), 0, 5)
    ENDFOR
    resVectST=FLTARR(4)-1000.; slice thickness min/max head + body
    resVectWA=FLTARR(4)-1000.; HU water min/max head + body
    resVectHO=FLTARR(2)-1000.; max diff center head + body
    resVectNO=FLTARR(2)-1000.; max noise head + body
    resVectMTF=FLTARR(8)-1000.; 50% 10% typical head + mean typical body + sharpest

    ;double tube
    resVectSTdbl=FLTARR(4)-1000.; slice thickness min/max body tube A / tube B
    resVectWAdbl=FLTARR(4)-1000.; HU water min/max body tube A / tube B
    resVectHOdbl=FLTARR(2)-1000.; max diff center body tube A / tube B
    resVectNOdbl=FLTARR(2)-1000.; max noise body tube A / tube B
    resVectMTFdbl=FLTARR(4)-1000.; 50% 10% typical body smooth tube A / tube B

    ;detect language and type of report
    typeAndLanguage=getTypeAndLanuage(clipres, configGetSiemensQC)
    langu=typeAndLanguage.LANGUAGENO
    typeStr=typeAndLanguage.REPORTTYPE
    symbiaT=typeAndLanguage.SYMBIAT
    symbiaIntevo=typeAndLanguage.SYMBIAINTEVO

    IF typeAndLanguage.REPORTTYPENO EQ -1 THEN errMsg='No results in file or unexpected content or language.' ELSE BEGIN

      date=STRSPLIT(clipres(2),' ',/EXTRACT)
      IF symbiaT THEN date=STRSPLIT(clipres(3),' ',/EXTRACT)
      date=formatDDMMYYYY(date(0)); ensure format = dd.mm.yyyy
      strArrRes(0)=date

      ;tester
      rowno=WHERE(STRMATCH(clipres, configGetSiemensQC.CT.(langu)(10)+'*', /FOLD_CASE) EQ 1)
      strArrRes(1)=STRMID(clipres(rowno(0)),STRLEN(configGetSiemensQC.CT.(langu)(10)))
      ;product Name
      rowno=WHERE(STRMATCH(clipres, configGetSiemensQC.CT.(langu)(11)+'*', /FOLD_CASE) EQ 1)
      strArrRes(2)=STRMID(clipres(rowno(0)),STRLEN(configGetSiemensQC.CT.(langu)(11)))
      ;serial Number
      rowno=WHERE(STRMATCH(clipres, configGetSiemensQC.CT.(langu)(12)+'*', /FOLD_CASE) EQ 1)
      strArrRes(3)=STRTRIM(STRMID(clipres(rowno(0)),STRLEN(configGetSiemensQC.CT.(langu)(12))),2)
      ;tube assembly A and B serialnumber
      rowno=WHERE(STRMATCH(clipres, configGetSiemensQC.CT.(langu)(13)+'*', /FOLD_CASE) EQ 1)
      IF N_ELEMENTS(rowno) EQ 2 THEN BEGIN
        IF rowno(1) EQ rowno(0)+1 THEN BEGIN;as newer files where table with three columns and both tubes in table even though only one tube
          IF clipres(rowno(0)+2) EQ configGetSiemensQC.CT.(langu)(12) THEN BEGIN;seen at least for Drive
            strArrRes(4)=clipres(rowno(0)+3)
            strArrRes(5)=clipres(rowno(0)+4)
          ENDIF ELSE BEGIN
            strArrRes(4)=clipres(rowno(0)+13)
            strArrRes(5)=clipres(rowno(1)+13)
          ENDELSE
        ENDIF
      ENDIF ELSE BEGIN
        IF rowno(0) NE -1 AND N_ELEMENTS(rowno) EQ 1 THEN BEGIN

          rownoVal=WHERE(STRMATCH(clipres[rowno(0)+1:rowno(0)+2], configGetSiemensQC.CT.(langu)(15)+'*', /FOLD_CASE) EQ 1);symbia some files
          IF rownoVal(0) NE -1 THEN BEGIN
            strs=STRSPLIT(clipres(rowno(0)+1+rownoVal(0)),' ',/EXTRACT)
            IF N_ELEMENTS(strs) GE 2 THEN strArrRes(4)=strs(1)
          ENDIF ELSE BEGIN
            rownoSerial=WHERE(STRMATCH(clipres[rowno(0):-1], configGetSiemensQC.CT.(langu)(12)+'*', /FOLD_CASE) EQ 1)
            IF rownoSerial(0) NE -1 THEN BEGIN
              strs=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
              IF symbiaIntevo EQ 1 THEN strArrRes(4)=clipres(rowno(0)+rownoSerial(0)+4) ELSE BEGIN
                IF N_ELEMENTS(strs) EQ 5 THEN strArrRes(4)=strs(3) ELSE strArrRes(4)=clipres(rowno(0)+rownoSerial(0)+4)
              ENDELSE
            ENDIF
          ENDELSE
        ENDIF
      ENDELSE

      IF symbiaT THEN languTemp=0 ELSE languTemp=langu
      rownoST=WHERE(STRMATCH(clipres, '*'+typeStr+'*'+configGetSiemensQC.CT.(languTemp)(2)+'*', /FOLD_CASE) EQ 1) ;'Quality *','Slice'
      rownoHO=WHERE(STRMATCH(clipres, '*'+typeStr+'*'+configGetSiemensQC.CT.(languTemp)(3)+'*', /FOLD_CASE) EQ 1) ;'Quality *','Homogeneity'
      rownoNO=WHERE(STRMATCH(clipres, '*'+typeStr+'*'+configGetSiemensQC.CT.(languTemp)(4)+'*', /FOLD_CASE) EQ 1) ;'Quality *','Noise'
      rownoMTF=WHERE(STRMATCH(clipres, '*'+typeStr+'*MTF*', /FOLD_CASE) EQ 1) ;'Quality Constancy','MTF'
      IF N_ELEMENTS(rownoMTF) EQ 1 AND rownoMTF(0) NE -1 THEN rownoMTF=[-1, rownoMTF(0)]

      sliceStr=STRMID(configGetSiemensQC.CT.(langu)(2), 0, 5)
      tolStr=STRMID(configGetSiemensQC.CT.(langu)(5), 0, 5)


      IF symbiaT EQ 0 THEN BEGIN

        IF symbiaIntevo EQ 0 THEN BEGIN

          strTest=[configGetSiemensQC.CT.(langu)(6),configGetSiemensQC.CT.(langu)(7), configGetSiemensQC.CT.(langu)(9)]; Test: typical head / body / sharpest

          ;slice thickness

          IF N_ELEMENTS(rownoST) EQ 2 THEN BEGIN
            nextTest=WHERE(STRMATCH(clipres[rownoST(1):-1], configGetSiemensQC.CT.(langu)(14),/FOLD_CASE) EQ 1)
            IF N_ELEMENTS(nextTest) GT 1 THEN nextTest=nextTest(1) ELSE nextTest=-1

            FOR a=0, 1 DO BEGIN
              rowno=WHERE(shortres[rownoST(1):-1] EQ STRMID(strTest(a), 0, nShortRes))
              IF rowno(0) NE -1 THEN BEGIN

                IF nextTest GT -1 THEN BEGIN
                  actRowNos=WHERE(rowno LT nextTest)
                  rowno=rowno(actRowNos)
                ENDIF

                rowno=rowno+rownoST(1)

                FOR tt=0, N_ELEMENTS(rowno)-1 DO BEGIN
                  ;number of Images in test
                  rownoNimg=WHERE(STRMATCH(clipres[rowno(tt):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
                  strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(tt)),' ',/EXTRACT)
                  nImg=LONG(strNimg(-1))

                  addSlice=WHERE(shortres5[rowno(tt):-1] EQ sliceStr)
                  addSlice=addSlice[1:-1];first one is Nominal slice thickness
                  rownoTol=WHERE(shortres5[rowno(tt):-1] EQ tolStr)
                  rownoTest=WHERE(shortres5[rowno(tt):-1] EQ STRMID(strTest(0), 0, 5))
                  IF addSlice(0) NE -1 THEN BEGIN
                    IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                      actLines=WHERE(addslice LT rownoTol(0),nImg)
                      res=FLTARR(nImg)
                      res2=res
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+addSlice(actLines(i))),' ',/EXTRACT)
                        IF tt EQ 0 THEN res(i)=FLOAT(clipSplit(-2)) ELSE BEGIN
                          res(i)=FLOAT(clipSplit(-4))
                          res2(i)=FLOAT(clipSplit(-2))
                        ENDELSE
                      ENDFOR
                    ENDIF ELSE BEGIN
                      IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                        actLines=WHERE(addslice LT rownoTest(1))
                        actLines=addSlice(actLines)+2
                      ENDIF ELSE actLines=addSlice+2
                      nImg=N_ELEMENTS(actLines)
                      res=FLTARR(nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        res(i)=FLOAT(clipSplit(0))
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        res2=FLTARR(nImg)
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2),' ',/EXTRACT)
                          res2(i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDIF
                    ENDELSE

                    IF tt EQ 0 THEN resVectST[0+a*2:1+a*2]=[MIN(res),MAX(res)] ELSE resVectSTdbl=[MIN(res),MAX(res),MIN(res2),MAX(res2)]
                  ENDIF
                ENDFOR
              ENDIF
            ENDFOR
            ;structRes=CREATE_STRUCT(structRes, 'ST', resVectST)
          ENDIF

          ;homogenity / water
          IF N_ELEMENTS(rownoHO) EQ 2 THEN BEGIN
            nextTest=WHERE(STRMATCH(clipres[rownoHO(1):-1], configGetSiemensQC.CT.(langu)(14),/FOLD_CASE) EQ 1)
            IF N_ELEMENTS(nextTest) GT 1 THEN nextTest=nextTest(1) ELSE nextTest=-1
            ;min/max HU water
            FOR a=0, 1 DO BEGIN
              rowno=WHERE(shortres[rownoHO(1):-1] EQ STRMID(strTest(a), 0, nShortRes))
              IF rowno(0) NE -1 THEN BEGIN

                IF nextTest GT -1 THEN BEGIN
                  actRowNos=WHERE(rowno LT nextTest)
                  rowno=rowno(actRowNos)
                ENDIF

                rowno=rowno+rownoHO(1)

                FOR tt=0, N_ELEMENTS(rowno)-1 DO BEGIN
                  ;number of Images in test
                  rownoNimg=WHERE(STRMATCH(clipres[rowno(tt):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
                  IF rownoNimg(0) GT 10 THEN BEGIN
                    ;assume not found, probably "Number of images" split into two lines
                    stxt=STRSPLIT(configGetSiemensQC.CT.(langu)(8),' ',/EXTRACT)
                    sfirst=STRJOIN(stxt[0:-2],' ')
                    rownoNimg=WHERE(STRMATCH(clipres[rowno(tt):-1], sfirst+'*',/FOLD_CASE) EQ 1)
                    IF rownoNimg(0) LT 10 THEN BEGIN
                      strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(tt)+1),' ',/EXTRACT)
                      nImg=LONG(strNimg(-1))
                    ENDIF ELSE nImg=1 ;todo - smarter than 1... does this ever happen? warning?
                  ENDIF ELSE BEGIN
                    strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(tt)),' ',/EXTRACT)
                    nImg=LONG(strNimg(-1))
                  ENDELSE

                  addSlice=WHERE(shortres5[rowno(tt):-1] EQ sliceStr)
                  addSlice=addSlice[1:-1];first one is Nominal slice thickness
                  rownoTol=WHERE(shortres5[rowno(tt):-1] EQ tolStr)

                  IF addSlice(0) NE -1 THEN BEGIN
                    ;min/max HU water
                    tolEnd=0
                    IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                      tolEnd=1
                      actLines=WHERE(addslice LT rownoTol(0),nImg)
                      res=FLTARR(nImg)
                      res2=res
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+addSlice(actLines(i))),' ',/EXTRACT)
                        IF tt EQ 0 THEN res(i)=FLOAT(clipSplit(-2)) ELSE BEGIN
                          res(i)=FLOAT(clipSplit(-4))
                          res2(i)=FLOAT(clipSplit(-2))
                        ENDELSE
                      ENDFOR
                    ENDIF ELSE BEGIN

                      diff=addSlice- SHIFT(addSlice,-1)
                      eqnn=WHERE(diff[0:nImg] EQ diff(0), nEQ)
                      nImg=nEQ+1
                      actLines=addSlice[0:nImg-1]+2
                      res=FLTARR(nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        res(i)=FLOAT(clipSplit(0))
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        res2=FLTARR(nImg)
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2),' ',/EXTRACT)
                          res2(i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDIF
                    ENDELSE
                    IF tt EQ 0 THEN resVectWA[0+a*2:1+a*2]=[MIN(res),MAX(res)] ELSE resVectWAdbl=[MIN(res),MAX(res),MIN(res2),MAX(res2)]

                    ;max inhomogeneity
                    actLines=addslice[nImg:2*nImg-1]
                    res=FLTARR(5,nImg)
                    res2=res
                    IF tolEnd THEN BEGIN
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        FOR d=0, 4 DO BEGIN
                          res(d,i)=FLOAT(clipSplit(-2*(5-d)))
                        ENDFOR
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        actLines=addslice[2*nImg:3*nImg-1]
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                          FOR d=0, 4 DO BEGIN
                            res2(d,i)=FLOAT(clipSplit(-2*(5-d)))
                          ENDFOR
                        ENDFOR
                      ENDIF
                    ENDIF ELSE BEGIN
                      FOR i=0, nImg-1 DO BEGIN
                        FOR d=1, 5 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2*d),' ',/EXTRACT)
                          res(d-1,i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        actLines=addslice[2*nImg:3*nImg-1]
                        FOR i=0, nImg-1 DO BEGIN
                          FOR d=1, 5 DO BEGIN
                            clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2*d),' ',/EXTRACT)
                            res2(d-1,i)=FLOAT(clipSplit(0))
                          ENDFOR
                        ENDFOR
                      ENDIF
                    ENDELSE
                    IF tt EQ 0 THEN resVectHO(a)=MAX(ABS(res[1:4,*])) ELSE resVectHOdbl=[MAX(ABS(res[1:4,*])),MAX(ABS(res2[1:4,*]))]
                  ENDIF
                ENDFOR;tt
              ENDIF
            ENDFOR
            ;structRes=CREATE_STRUCT(structRes, 'WA', resVectWA, 'HO', resVectHO)

          ENDIF

          ;noise
          IF N_ELEMENTS(rownoNO) EQ 2 THEN BEGIN
            nextTest=WHERE(STRMATCH(clipres[rownoNo(1):-1], configGetSiemensQC.CT.(langu)(14),/FOLD_CASE) EQ 1)
            IF N_ELEMENTS(nextTest) GT 1 THEN nextTest=nextTest(1) ELSE nextTest=-1
            FOR a=0, 1 DO BEGIN
              rowno=WHERE(shortres[rownoNO(1):-1] EQ STRMID(strTest(a), 0, nShortRes))

              IF rowno(0) NE -1 THEN BEGIN

                IF nextTest GT -1 THEN BEGIN
                  actRowNos=WHERE(rowno LT nextTest)
                  rowno=rowno(actRowNos)
                ENDIF

                rowno=rowno+rownoNO(1)

                FOR tt=0, N_ELEMENTS(rowno)-1 DO BEGIN
                  ;number of Images in test
                  rownoNimg=WHERE(STRMATCH(clipres[rowno(tt):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
                  strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(tt)),' ',/EXTRACT)
                  nImg=LONG(strNimg(-1))

                  addSlice=WHERE(shortres5[rowno(tt):-1] EQ sliceStr)
                  addSlice=addSlice[1:-1];first one is Nominal slice thickness
                  rownoTol=WHERE(shortres5[rowno(tt):-1] EQ tolStr)
                  rownoTest=WHERE(shortres5[rowno(tt):-1] EQ STRMID(strTest(0), 0, 5))
                  IF addSlice(0) NE -1 THEN BEGIN
                    IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                      actLines=WHERE(addslice LT rownoTol(0),nImg)
                      res=FLTARR(nImg)
                      res2=res
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+addSlice(actLines(i))),' ',/EXTRACT)
                        IF tt EQ 0 THEN res(i)=FLOAT(clipSplit(-2)) ELSE BEGIN
                          res(i)=FLOAT(clipSplit(-4))
                          res2(i)=FLOAT(clipSplit(-2))
                        ENDELSE
                      ENDFOR
                    ENDIF ELSE BEGIN
                      IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                        actLines=WHERE(addslice LT rownoTest(1))
                        actLines=addSlice(actLines)+2
                      ENDIF ELSE actLines=addSlice+2
                      nImg=N_ELEMENTS(actLines)
                      res=FLTARR(nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        res(i)=FLOAT(clipSplit(0))
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        res2=FLTARR(nImg)
                        actLines=actLines+2
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                          res2(i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDIF
                    ENDELSE
                    IF tt EQ 0 THEN resVectNO(a)=MAX(res) ELSE resVectNOdbl=[MAX(res),MAX(res2)]
                  ENDIF
                ENDFOR

              ENDIF
            ENDFOR
            ;structRes=CREATE_STRUCT(structRes, 'NO', resVectNO)
          ENDIF

          ;MTF
          IF N_ELEMENTS(rownoMTF) EQ 2 THEN BEGIN
            nextTest=WHERE(STRMATCH(clipres[rownoMTF(1):-1], configGetSiemensQC.CT.(langu)(14),/FOLD_CASE) EQ 1)
            IF N_ELEMENTS(nextTest) GT 1 THEN nextTest=nextTest(1) ELSE nextTest=-1

            strTestResort=[strTest(1),strTest(0),strTest(2)]
            FOR a=0, 2 DO BEGIN
              rowno=WHERE(shortres[rownoMTF(1):-1] EQ STRMID(strTestResort(a), 0, nShortRes))
              IF rowno(0) NE -1 THEN BEGIN
                IF nextTest NE -1 THEN BEGIN
                  actRowNos=WHERE(rowno LT nextTest)
                  rowno=rowno(actRowNos)
                ENDIF

                ccc=0
                IF a NE 1 AND N_ELEMENTS(rowno) EQ 2 THEN ccc=1; ccc=1 for sharpest (a=2) with UHR or body (a=0) with double tube
                rowno=rowno+rownoMTF(1)
                FOR cc=0, ccc DO BEGIN

                  IF cc EQ 1 THEN rowno=rowno(1)
                  ;number of Images in test
                  rownoNimg=WHERE(STRMATCH(clipres[rowno(0):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
                  strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(0)),' ',/EXTRACT)
                  nImg=LONG(strNimg(-1))

                  addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)
                  addSlice=addSlice[1:-1];first one is Nominal slice thickness
                  rownoTol=WHERE(shortres5[rowno(0):-1] EQ tolStr)
                  rownoTest=WHERE(shortres5[rowno(0):-1] EQ STRMID(strTest(0), 0, 5))
                  IF addSlice(0) NE -1 THEN BEGIN
                    IF N_ELEMENTS(STRSPLIT(clipres(rowno(0)+addslice(0)),' ',/EXTRACT)) GT 2 THEN BEGIN; tolerance at end
                      IF N_ELEMENTS(rownoTest) GE 2 THEN actLines=WHERE(addslice LT rownoTest(1)) ELSE actLines=WHERE(addslice LT rownoTol(0))
                      nImg=N_ELEMENTS(actLines)
                      res=FLTARR(2,nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(actLines(i))),' ',/EXTRACT)
                        res(0,i)=FLOAT(clipSplit(-4))
                        res(1,i)=FLOAT(clipSplit(-2))
                      ENDFOR
                    ENDIF ELSE BEGIN
                      IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                        actLines=WHERE(addslice LT rownoTest(1))
                        actLines=addSlice(actLines)+2
                      ENDIF ELSE actLines=addSlice+2
                      nImg=N_ELEMENTS(actLines)
                      res=FLTARR(2,nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)),' ',/EXTRACT)
                        res(0,i)=FLOAT(clipSplit(0))
                        clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)+2),' ',/EXTRACT)
                        res(1,i)=FLOAT(clipSplit(0))
                      ENDFOR
                    ENDELSE

                    IF a EQ 0 AND cc EQ 1 THEN BEGIN;dbl tube results
                      szRes=SIZE(res, /DIMENSIONS)
                      IF szRes(1) GT 2 THEN BEGIN
                        resVectMTFdbl=[MEAN(res[0,0:szRes(1)/2-1]),MEAN(res[1,0:szRes(1)/2-1]),MEAN(res[0,szRes(1)/2:szRes(1)-1]),MEAN(res[1,szRes(1)/2:szRes(1)-1])]
                      ENDIF ELSE  resVectMTFdbl=[res[*,0],res[*,1]]
                    ENDIF ELSE resVectMTF[0+(a+cc)*2:1+(a+cc)*2]=[MEAN(res[0,*]),MEAN(res[1,*])]
                  ENDIF
                ENDFOR

              ENDIF
            ENDFOR
            ;structRes=CREATE_STRUCT(structRes, 'MTF', resVectMTF)

          ENDIF

          IF MIN(resVectWA) NE -1000. THEN strArrRes[6:6+3]=STRING(resVectWA, FORMAT='(f0.2)')
          IF MIN(resVectHO) NE -1000. THEN strArrRes[10:10+1]=STRING(resVectHO, FORMAT='(f0.2)')
          IF MIN(resVectNO) NE -1000. THEN strArrRes[12:12+1]=STRING(resVectNO, FORMAT='(f0.2)')
          IF MIN(resVectST) NE -1000. THEN strArrRes[14:14+3]=STRING(resVectST, FORMAT='(f0.2)')
          IF MAX(resVectMTF[0:5]) GT 0. THEN BEGIN
            newStr=STRING(resVectMTF, FORMAT='(f0.2)')
            IF MIN(resVectMTF) LE 0 THEN BEGIN
              less=WHERE(resVectMTF LE 0)
              newStr(less)=''
            ENDIF
            strArrRes[18:18+7]=newStr
          ENDIF
          IF MIN(resVectWAdbl) NE -1000. THEN strArrRes[26:26+3]=STRING(resVectWAdbl, FORMAT='(f0.2)')
          IF MIN(resVectHOdbl) NE -1000. THEN strArrRes[30:30+1]=STRING(resVectHOdbl, FORMAT='(f0.2)')
          IF MIN(resVectNOdbl) NE -1000. THEN strArrRes[32:32+1]=STRING(resVectNOdbl, FORMAT='(f0.2)')
          IF MIN(resVectSTdbl) NE -1000. THEN strArrRes[34:34+3]=STRING(resVectSTdbl, FORMAT='(f0.2)')
          IF MAX(resVectMTFdbl[0:3]) GT 0. THEN BEGIN
            newStr=STRING(resVectMTFdbl, FORMAT='(f0.2)')
            IF MIN(resVectMTFdbl) LE 0 THEN BEGIN
              less=WHERE(resVectMTFdbl LE 0)
              newStr(less)=''
            ENDIF
            strArrRes[38:38+3]=newStr
          ENDIF
        ENDIF ELSE BEGIN;symbia Intevo


          strTest='Test:'; TODO: if other language where this not valid -add something to langu

          ;slice thickness
          IF N_ELEMENTS(rownoST) EQ 1 THEN BEGIN

            rownoNow=WHERE(shortres[rownoST:-1] EQ STRMID(strTest, 0, nShortRes))
            IF N_ELEMENTS(rownoNow) EQ 2 THEN BEGIN
              FOR a=0, 1 DO BEGIN;head first, then body
                rowno=rownoNow(a)+rownoST
                FOR tt=0, N_ELEMENTS(rowno)-1 DO BEGIN
                  ;number of Images in test
                  rownoNimg=WHERE(STRMATCH(clipres[rowno(tt):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
                  strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(tt)),' ',/EXTRACT)
                  nImg=LONG(strNimg(-1))

                  addSlice=WHERE(shortres5[rowno(tt):-1] EQ sliceStr)
                  addSlice=addSlice[1:-1];first one is Nominal slice thickness
                  rownoTol=WHERE(shortres5[rowno(tt):-1] EQ tolStr)
                  rownoTest=WHERE(shortres5[rowno(tt):-1] EQ STRMID(strTest(0), 0, 5))
                  IF addSlice(0) NE -1 THEN BEGIN
                    IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                      actLines=WHERE(addslice LT rownoTol(0),nImg)
                      res=FLTARR(nImg)
                      res2=res
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+addSlice(actLines(i))),' ',/EXTRACT)
                        IF tt EQ 0 THEN res(i)=FLOAT(clipSplit(-2)) ELSE BEGIN
                          res(i)=FLOAT(clipSplit(-4))
                          res2(i)=FLOAT(clipSplit(-2))
                        ENDELSE
                      ENDFOR
                    ENDIF ELSE BEGIN
                      IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                        actLines=WHERE(addslice LT rownoTest(1))
                        actLines=addSlice(actLines)+2
                      ENDIF ELSE actLines=addSlice+2
                      nImg=N_ELEMENTS(actLines)
                      res=FLTARR(nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        res(i)=FLOAT(clipSplit(0))
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        res2=FLTARR(nImg)
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2),' ',/EXTRACT)
                          res2(i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDIF
                    ENDELSE

                    IF tt EQ 0 THEN resVectST[0+a*2:1+a*2]=[MIN(res),MAX(res)] ELSE resVectSTdbl=[MIN(res),MAX(res),MIN(res2),MAX(res2)]
                  ENDIF
                ENDFOR
              ENDFOR
            ENDIF
            ;structRes=CREATE_STRUCT(structRes, 'ST', resVectST)
          ENDIF

          ;homogenity / water - 6 tests, focus on test 3 (head full coll, 130 kV) and 6 (bbody full coll, 130 kV)
          IF N_ELEMENTS(rownoHO) EQ 1 THEN BEGIN

            ;min/max HU water
            rownoNow=WHERE(shortres[rownoHO:-1] EQ STRMID(strTest, 0, nShortRes))
            IF N_ELEMENTS(rownoNow) EQ 6 THEN BEGIN
              testno=[2,5]
              FOR a=0, 1 DO BEGIN
                rowno=rownoNow(testno(a))+rownoHO

                FOR tt=0, N_ELEMENTS(rowno)-1 DO BEGIN
                  ;number of Images in test
                  rownoNimg=WHERE(STRMATCH(clipres[rowno(tt):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
                  strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(tt)),' ',/EXTRACT)
                  nImg=LONG(strNimg(-1))

                  addSlice=WHERE(shortres5[rowno(tt):-1] EQ sliceStr)
                  addSlice=addSlice[1:-1];first one is Nominal slice thickness
                  rownoTol=WHERE(shortres5[rowno(tt):-1] EQ tolStr)
                  IF addSlice(0) NE -1 THEN BEGIN
                    ;min/max HU water
                    tolEnd=0
                    IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                      tolEnd=1
                      actLines=WHERE(addslice LT rownoTol(0),nImg)
                      res=FLTARR(nImg)
                      res2=res
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+addSlice(actLines(i))),' ',/EXTRACT)
                        IF tt EQ 0 THEN res(i)=FLOAT(clipSplit(-2)) ELSE BEGIN
                          res(i)=FLOAT(clipSplit(-4))
                          res2(i)=FLOAT(clipSplit(-2))
                        ENDELSE
                      ENDFOR
                    ENDIF ELSE BEGIN

                      diff=addSlice- SHIFT(addSlice,-1)
                      eqnn=WHERE(diff[0:nImg] EQ diff(0), nEQ)
                      nImg=nEQ+1
                      actLines=addSlice[0:nImg-1]+2
                      res=FLTARR(nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        res(i)=FLOAT(clipSplit(0))
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        res2=FLTARR(nImg)
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2),' ',/EXTRACT)
                          res2(i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDIF
                    ENDELSE
                    IF tt EQ 0 THEN resVectWA[0+a*2:1+a*2]=[MIN(res),MAX(res)] ELSE resVectWAdbl=[MIN(res),MAX(res),MIN(res2),MAX(res2)]

                    ;max inhomogeneity
                    actLines=addslice[nImg:2*nImg-1]
                    res=FLTARR(5,nImg)
                    res2=res
                    IF tolEnd THEN BEGIN
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        FOR d=0, 4 DO BEGIN
                          res(d,i)=FLOAT(clipSplit(-2*(5-d)))
                        ENDFOR
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        actLines=addslice[2*nImg:3*nImg-1]
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                          FOR d=0, 4 DO BEGIN
                            res2(d,i)=FLOAT(clipSplit(-2*(5-d)))
                          ENDFOR
                        ENDFOR
                      ENDIF
                    ENDIF ELSE BEGIN
                      FOR i=0, nImg-1 DO BEGIN
                        FOR d=1, 5 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2*d),' ',/EXTRACT)
                          res(d-1,i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        actLines=addslice[2*nImg:3*nImg-1]
                        FOR i=0, nImg-1 DO BEGIN
                          FOR d=1, 5 DO BEGIN
                            clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)+2*d),' ',/EXTRACT)
                            res2(d-1,i)=FLOAT(clipSplit(0))
                          ENDFOR
                        ENDFOR
                      ENDIF
                    ENDELSE
                    IF tt EQ 0 THEN resVectHO(a)=MAX(ABS(res[1:4,*])) ELSE resVectHOdbl=[MAX(ABS(res[1:4,*])),MAX(ABS(res2[1:4,*]))]
                  ENDIF
                ENDFOR;tt
              ENDFOR
              ;structRes=CREATE_STRUCT(structRes, 'WA', resVectWA, 'HO', resVectHO)
            ENDIF
          ENDIF

          ;noise - seven tests - only check two last (130 kV body, head full coll)
          IF N_ELEMENTS(rownoNO) EQ 1 THEN BEGIN
            rownoNow=WHERE(shortres[rownoNO:-1] EQ STRMID(strTest, 0, nShortRes))
            IF N_ELEMENTS(rownoNow) EQ 7 THEN BEGIN

              testNo=[6,5]
              FOR a=0, 1 DO BEGIN
                rowno=rownoNow(testno(a))+rownoNO

                FOR tt=0, N_ELEMENTS(rowno)-1 DO BEGIN
                  ;number of Images in test
                  rownoNimg=WHERE(STRMATCH(clipres[rowno(tt):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
                  strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(tt)),' ',/EXTRACT)
                  nImg=LONG(strNimg(-1))

                  addSlice=WHERE(shortres5[rowno(tt):-1] EQ sliceStr)
                  addSlice=addSlice[1:-1];first one is Nominal slice thickness
                  rownoTol=WHERE(shortres5[rowno(tt):-1] EQ tolStr)
                  rownoTest=WHERE(shortres5[rowno(tt):-1] EQ STRMID(strTest(0), 0, 5))
                  IF addSlice(0) NE -1 THEN BEGIN
                    IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                      actLines=WHERE(addslice LT rownoTol(0),nImg)
                      res=FLTARR(nImg)
                      res2=res
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+addSlice(actLines(i))),' ',/EXTRACT)
                        IF tt EQ 0 THEN res(i)=FLOAT(clipSplit(-2)) ELSE BEGIN
                          res(i)=FLOAT(clipSplit(-4))
                          res2(i)=FLOAT(clipSplit(-2))
                        ENDELSE
                      ENDFOR
                    ENDIF ELSE BEGIN
                      IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                        actLines=WHERE(addslice LT rownoTest(1))
                        actLines=addSlice(actLines)+2
                      ENDIF ELSE actLines=addSlice+2
                      nImg=N_ELEMENTS(actLines)
                      res=FLTARR(nImg)
                      FOR i=0, nImg-1 DO BEGIN
                        clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                        res(i)=FLOAT(clipSplit(0))
                      ENDFOR
                      IF tt EQ 1 THEN BEGIN
                        res2=FLTARR(nImg)
                        actLines=actLines+2
                        FOR i=0, nImg-1 DO BEGIN
                          clipSplit=STRSPLIT(clipres(rowno(tt)+actLines(i)),' ',/EXTRACT)
                          res2(i)=FLOAT(clipSplit(0))
                        ENDFOR
                      ENDIF
                    ENDELSE
                    IF tt EQ 0 THEN resVectNO(a)=MAX(res) ELSE resVectNOdbl=[MAX(res),MAX(res2)]
                  ENDIF
                ENDFOR

              ENDFOR
            ENDIF
            ;structRes=CREATE_STRUCT(structRes, 'NO', resVectNO)
          ENDIF

          ;MTF 5 tests, 3 first (130 kV, body, head U90)
          IF N_ELEMENTS(rownoMTF) EQ 2 THEN BEGIN

            testNo=[0,1,2]
            ;assuming B41, H31, U90s
            ;'MTF50 B smooth','MTF10 B smooth','MTF50 H smooth','MTF10 H smooth','MTF50 H sharp','MTF10 H sharp','MTF50 UHR','MTF10 UHR'
            cc=[0,1,3]
            rownoNow=WHERE(shortres[rownoMTF(1):-1] EQ STRMID(strTest, 0, nShortRes))
            FOR a=0, 2 DO BEGIN
              rowno=rownoNow(testNo(a))+rownoMTF(1)

              ;number of Images in test
              rownoNimg=WHERE(STRMATCH(clipres[rowno(0):-1], configGetSiemensQC.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
              strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(0)),' ',/EXTRACT)
              nImg=LONG(strNimg(-1))

              addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)
              addSlice=addSlice[1:-1];first one is Nominal slice thickness
              rownoTol=WHERE(shortres5[rowno(0):-1] EQ tolStr)
              rownoTest=WHERE(shortres5[rowno(0):-1] EQ STRMID(strTest(0), 0, 5))
              IF addSlice(0) NE -1 THEN BEGIN
                IF N_ELEMENTS(STRSPLIT(clipres(rowno(0)+addslice(0)),' ',/EXTRACT)) GT 2 THEN BEGIN; tolerance at end
                  IF N_ELEMENTS(rownoTest) GE 2 THEN actLines=WHERE(addslice LT rownoTest(1)) ELSE actLines=WHERE(addslice LT rownoTol(0))
                  nImg=N_ELEMENTS(actLines)
                  res=FLTARR(2,nImg)
                  FOR i=0, nImg-1 DO BEGIN
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(actLines(i))),' ',/EXTRACT)
                    res(0,i)=FLOAT(clipSplit(-4))
                    res(1,i)=FLOAT(clipSplit(-2))
                  ENDFOR
                ENDIF ELSE BEGIN
                  IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                    actLines=WHERE(addslice LT rownoTest(1))
                    actLines=addSlice(actLines)+2
                  ENDIF ELSE actLines=addSlice+2
                  nImg=N_ELEMENTS(actLines)
                  res=FLTARR(2,nImg)
                  FOR i=0, nImg-1 DO BEGIN
                    clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)),' ',/EXTRACT)
                    res(0,i)=FLOAT(clipSplit(0))
                    clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)+2),' ',/EXTRACT)
                    res(1,i)=FLOAT(clipSplit(0))
                  ENDFOR
                ENDELSE
              ENDIF

              resVectMTF[cc(a)*2:1+cc(a)*2]=[MEAN(res[0,*]),MEAN(res[1,*])]
            ENDFOR


          ENDIF

          IF MIN(resVectWA) NE -1000. THEN strArrRes[6:6+3]=STRING(resVectWA, FORMAT='(f0.2)')
          IF MIN(resVectHO) NE -1000. THEN strArrRes[10:10+1]=STRING(resVectHO, FORMAT='(f0.2)')
          IF MIN(resVectNO) NE -1000. THEN strArrRes[12:12+1]=STRING(resVectNO, FORMAT='(f0.2)')
          IF MIN(resVectST) NE -1000. THEN strArrRes[14:14+3]=STRING(resVectST, FORMAT='(f0.2)')
          IF MAX(resVectMTF[0:5]) GT 0. THEN BEGIN
            newStr=STRING(resVectMTF, FORMAT='(f0.2)')
            IF MIN(resVectMTF) LE 0 THEN BEGIN
              less=WHERE(resVectMTF LE 0)
              newStr(less)=''
            ENDIF
            strArrRes[18:18+7]=newStr
          ENDIF
        ENDELSE

      ENDIF ELSE BEGIN;Symbia T2

        ;slice thickness
        IF N_ELEMENTS(rownoST) EQ 2 THEN BEGIN
          resVectST=FLTARR(5)-1000.
          ;assuming 1mm, 1.5mm, 2.5mm, 4mm, 5mm

          rownoRes=WHERE(STRMATCH(shortres[rownoST(1):-1], configGetSiemensQC.CT.(langu)(16), /FOLD_CASE) EQ 1);Result

          nextTest=WHERE(STRMATCH(clipres[rownoST(1):-1], configGetSiemensQC.CT.(langu)(14),/FOLD_CASE) EQ 1);Description
          IF N_ELEMENTS(nextTest) GT 1 THEN nextTest=nextTest(1) ELSE nextTest=-1

          IF nextTest NE -1 THEN BEGIN
            actRes=WHERE(rownoRes LT nextTest)
            rownoRes=rownoRes(actRes)
          ENDIF

          res=FLTARR(2,5)
          IF N_ELEMENTS(rownoRes) GE 10 THEN BEGIN;result twice before each
            FOR st=0, 4 DO BEGIN
              rowno=rownoST(1)+rownoRes(st*2)
              addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)

              IF addSlice(0) NE -1 THEN BEGIN
                addSlice=addSlice[0:1];two slices for T2
                FOR i=0, 1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)),' ',/EXTRACT)
                  IF N_ELEMENTS(clipSplit) EQ 3 THEN BEGIN; tolerance at end
                    res(i,st)=FLOAT(clipSplit(-2))
                  ENDIF
                  IF N_ELEMENTS(clipSplit) EQ 1 THEN BEGIN; file from ~2017+, tolerance in between
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+2),' ',/EXTRACT)
                    res(i,st)=FLOAT(clipSplit(0))
                  ENDIF
                ENDFOR
                resVectST[st]=MEAN(res[*,st])
              ENDIF

            ENDFOR
          ENDIF
        ENDIF

        ;homogeneity/water
        IF N_ELEMENTS(rownoHO) GE 2 THEN BEGIN
          ;assuming 110kV, 130kV
          rownoRes=WHERE(STRMATCH(shortres[rownoHO(1):-1], configGetSiemensQC.CT.(langu)(16), /FOLD_CASE) EQ 1);Result
          IF rownoRes(1) EQ rownoRes(0)+1 THEN rownoRes=[rownoRes(0),rownoRes(2)]
          res=FLTARR(5,4); center,diff3,6,9,12 2 slices twice

          IF N_ELEMENTS(rownoRes) GE 2 THEN BEGIN
            FOR te=0, 1 DO BEGIN
              rowno=rownoHO(1)+rownoRes(te)
              addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)

              IF addSlice(0) NE -1 THEN BEGIN
                addSlice=addSlice[0:1];two slices for T2
                FOR i=0, 1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)),' ',/EXTRACT)
                  IF N_ELEMENTS(clipSplit) EQ 11 THEN BEGIN; tolerance at end
                    res(0,2*te+i)=FLOAT(clipSplit(1))
                    res(1,2*te+i)=FLOAT(clipSplit(3))
                    res(2,2*te+i)=FLOAT(clipSplit(5))
                    res(3,2*te+i)=FLOAT(clipSplit(7))
                    res(4,2*te+i)=FLOAT(clipSplit(9))
                  ENDIF
                  IF N_ELEMENTS(clipSplit) EQ 1 THEN BEGIN; file from ~2017+, tolerance in between
                    addEx=0
                    IF clipres(rowno(0)-1) EQ configGetSiemensQC.CT.(langu)(17) THEN addEx=1 ;Test Result ;syngo CT 2007E
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+2+addEx),' ',/EXTRACT)
                    res(0,2*te+i)=FLOAT(clipSplit(0))
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+4+addEx),' ',/EXTRACT)
                    res(1,2*te+i)=FLOAT(clipSplit(0))
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+6+addEx),' ',/EXTRACT)
                    res(2,2*te+i)=FLOAT(clipSplit(0))
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+8+addEx),' ',/EXTRACT)
                    res(3,2*te+i)=FLOAT(clipSplit(0))
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+10+addEx),' ',/EXTRACT)
                    res(4,2*te+i)=FLOAT(clipSplit(0))
                  ENDIF
                ENDFOR
                resVectWA[te*2:te*2+1]=[MIN(res[0,te*2:te*2+1]),MAX(res[0,te*2:te*2+1])]
                resVectHO(te)=MAX(ABS(res[1:4,te*2:te*2+1]))
              ENDIF

            ENDFOR
          ENDIF
        ENDIF

        ;noise
        IF N_ELEMENTS(rownoNO) GE 2 THEN BEGIN
          resVectNO=FLTARR(3)-1000. ;assuming 80, 110, 130kV

          rownoRes=WHERE(STRMATCH(shortres[rownoNO(1):-1], '*Sigma*', /FOLD_CASE) EQ 1)

          IF N_ELEMENTS(rownoRes) EQ 3 THEN BEGIN;result twice before each
            FOR i=0, 2 DO BEGIN
              rowno=rownoNO(1)+rownoRes(i)+6
              clipSplit=STRSPLIT(clipres(rowno(0)),'HU',/EXTRACT)
              resVectNO(i)=FLOAT(clipSplit(0))
            ENDFOR
          ENDIF

          IF N_ELEMENTS(rownoRes) EQ 6 THEN BEGIN;tolerance before each
            FOR i=0, 2 DO BEGIN
              rowno=rownoNO(1)+rownoRes(i*2+1)+6
              clipSplit=STRSPLIT(clipres(rowno(0)),'HU',/EXTRACT)
              resVectNO(i)=FLOAT(clipSplit(0))
            ENDFOR
          ENDIF
        ENDIF

        ;MTF
        IF N_ELEMENTS(rownoMTF) GE 2 THEN BEGIN

          nextTest=WHERE(STRMATCH(clipres[rownoMTF(1):-1], configGetSiemensQC.CT.(langu)(14),/FOLD_CASE) EQ 1)
          IF N_ELEMENTS(nextTest) GT 1 THEN nextTest=nextTest(1) ELSE nextTest=-1

          ;assuming U90s, H41s, B31s
          ;'MTF50 B smooth','MTF10 B smooth','MTF50 H smooth','MTF10 H smooth','MTF50 H sharp','MTF10 H sharp','MTF50 UHR','MTF10 UHR'
          cc=[2,1,0]
          rownoRes=WHERE(STRMATCH(shortres[rownoMTF(1):-1], configGetSiemensQC.CT.(langu)(16)+'*', /FOLD_CASE) EQ 1);Result*

          IF nextTest NE -1 THEN BEGIN
            actRes=WHERE(rownoRes LT nextTest)
            rownoRes=rownoRes(actRes)
          ENDIF

          res=FLTARR(2,2)
          IF N_ELEMENTS(rownoRes) EQ 3 THEN BEGIN
            resVectMTF=FLTARR(6)
            FOR a=0, 2 DO BEGIN
              rowno=rownoMTF(1)+rownoRes(a)
              addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)

              IF addSlice(0) NE -1 THEN BEGIN
                addSlice=addSlice[0:1];two slices for T2
                FOR i=0, 1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)),' ',/EXTRACT)
                  IF N_ELEMENTS(clipSplit) EQ 7 THEN BEGIN; tolerance at end, lp/cm noted for each
                    res(0,i)=FLOAT(clipSplit(1))
                    res(1,i)=FLOAT(clipSplit(3))
                  ENDIF
                  IF N_ELEMENTS(clipSplit) EQ 5 THEN BEGIN; old filetype without lp/cm noted for each
                    res(0,i)=FLOAT(clipSplit(-3))
                    res(1,i)=FLOAT(clipSplit(-2))
                  ENDIF
                  IF N_ELEMENTS(clipSplit) EQ 1 THEN BEGIN; file from ~2017+, tolerance in between
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+2),' ',/EXTRACT)
                    res(0,i)=FLOAT(clipSplit(0))
                    clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(i)+4),' ',/EXTRACT)
                    res(1,i)=FLOAT(clipSplit(0))
                  ENDIF

                ENDFOR
              ENDIF

              resVectMTF[cc(a)*2:1+cc(a)*2]=[MEAN(res[0,*]),MEAN(res[1,*])]
            ENDFOR

          ENDIF

        ENDIF;MTF

        IF MIN(resVectWA) NE -1000. THEN strArrRes[5:5+3]=STRING(resVectWA, FORMAT='(f0.2)')
        IF MIN(resVectHO) NE -1000. THEN strArrRes[9:9+1]=STRING(resVectHO, FORMAT='(f0.2)')
        IF MIN(resVectNO) NE -1000. THEN strArrRes[11:11+2]=STRING(resVectNO, FORMAT='(f0.2)')
        IF MIN(resVectST) NE -1000. THEN strArrRes[14:14+4]=STRING(resVectST, FORMAT='(f0.2)')
        IF MAX(resVectMTF[0:5]) GT 0. THEN BEGIN
          newStr=STRING(resVectMTF, FORMAT='(f0.2)')
          IF MIN(resVectMTF) LE 0 THEN BEGIN
            less=WHERE(resVectMTF LE 0)
            newStr(less)=''
          ENDIF
          strArrRes[19:19+5]=newStr
        ENDIF
        strArrRes=strArrRes[0:24]

      ENDELSE; symbia t2

    ENDELSE;language
  ENDIF; N_ELEMENTS(clipres)>1

  res=CREATE_STRUCT('strArrRes',strArrRes, 'errMsg', errMsg)

  return, res

end