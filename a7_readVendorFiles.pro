;different read from vendor file functions
;Copyright (C) 2017  Ellen Wasbo, Stavanger University Hospital, Norway
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

;Siemens PET CT Daily QC
function readPETdailyQC, clipres, configGetSiemensQC
  errMsg=''

  shortres=STRARR(N_ELEMENTS(clipres))
  FOR i=0, N_ELEMENTS(clipres)-1 DO shortres(i)=STRMID(clipres(i), 0, 9)
  resVect=FLTARR(9)

  IF clipres(0) EQ 'System Quality Report' THEN BEGIN
    ;date
    rowno=WHERE(shortres EQ 'Scan Date')
    date=STRSPLIT(clipres(rowno),',',/EXTRACT)
    dateMD=STRSPLIT(date(0),' ',/EXTRACT)
    IF N_ELEMENTS(dateMD) GE 4 THEN BEGIN
      day=STRING(LONG(STRTRIM(dateMD(3),2)),FORMAT='(i02)')
      month=STRTRIM(dateMD(2),2)
      monthNmb=WHERE(month EQ configGetSiemensQC.months)
      IF monthNmb(0) NE -1 THEN month=STRING(LONg(monthNmb(0))+1, FORMAT='(i02)')
      year=STRTRIM(date(1),2)
      date=day+'.'+month+'.'+year
    ENDIF ELSE date=''

    rowno=WHERE(shortres EQ 'Partial s')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      IF clipSplit(-1) EQ 'true' THEN part='X' ELSE part=''
    ENDIF ELSE part=''
    rowno=WHERE(shortres EQ 'Full setu')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      IF clipSplit(-1) EQ 'true' THEN full='X' ELSE full=''
    ENDIF ELSE full=''
    rowno=WHERE(shortres EQ 'Time Alig')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      IF clipSplit(-1) EQ 'true' THEN timA='X' ELSE timA=''
    ENDIF ELSE timA=''
    rowno=WHERE(shortres EQ 'ICS Name ')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      icsName=clipSplit(-1)
    ENDIF ELSE icsName=''
    rowno=WHERE(shortres EQ 'Calibrati')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      calib=FLOAT(clipSplit(-1))
    ENDIF ELSE calib=-1


    ;Block Noise 3 [crystal] 0 [crystal] 0 Blocks
    rowno=WHERE(shortres EQ 'Block Noi')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      IF clipSplit(6) NE '0' THEN errMsg=[errMsg,clipSplit(6)+' block(s) with noise out of range.']
    ENDIF

    ;Block Efficiency 120 [%] 80 [%] 0 Blocks
    rowno=WHERE(shortres EQ 'Block Eff')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      IF clipSplit(6) NE '0' THEN errMsg=[errMsg,clipSplit(6)+' block(s) with efficiency out of range.']
    ENDIF

    ;Randoms 115 [%] 85 [%] 103.8 [%] Passed
    rowno=WHERE(shortres EQ 'Randoms 1')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      measuredRandoms=FLOAT(clipSplit(-3))
    ENDIF ELSE measuredRandoms=-1
    resVect(0)=measuredRandoms

    ;Scanner Efficiency 47.32 [cps/Bq/cc] 25.48 [cps/Bq/cc] 37.7 [cps/Bq/cc] Passed
    ;or newer
    ;Scanner Efficiency 47.3 [(count/
    ;sec)/(Bq/cc)]
    ;25.5 [(count/
    ;sec)/(Bq/cc)]
    ;36.1 [(count/
    ;sec)/(Bq/cc)] Passed
    rowno=WHERE(shortres EQ 'Scanner E')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      IF N_ELEMENTS(clipSplit) EQ 4 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno(0)+4),' ',/EXTRACT)
        scannerEff=FLOAT(clipSplit(0))
      ENDIF ELSE scannerEff=FLOAT(clipSplit(-3))
    ENDIF ELSE scannerEff=-1
    resVect(1)=scannerEff

    ;Scatter Ratio 35.2 [%] 28.8 [%] 30.7 [%] Passed
    rowno=WHERE(shortres EQ 'Scatter R')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
      scatterRat=FLOAT(clipSplit(-3))
    ENDIF ELSE scatterRat=-1
    resVect(2)=scatterRat

    ;      Scanner efficiency
    ;      correction factor
    ;      (ECF)
    ;      4e+007 [Bq*s/
    ;      ECAT counts]
    ;      2e+007 [Bq*s/
    ;      ECAT counts]
    ;      3.147e+007
    ;      [Bq*s/ECAT
    ;      counts]
    ;      Passed
    rowno=WHERE(shortres EQ '(ECF)')
    IF rowno(0) NE -1 THEN ECF=FLOAT(clipres(rowno(0)+5)) ELSE ECF=-1
    resVect(3)=ECF

    ;      Image Plane
    ;      Efficiency 5 [%] -5 [%] 0 Planes
    ;      out of range Passed
    rowno=WHERE(shortres EQ 'Image Pla')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+1),' ',/EXTRACT)
      IF clipSplit(5) NE '0' THEN errMsg=[errMsg,clipSplit(5)+' image plane(s) with efficiency out of range.']
    ENDIF

    ;      Block Timing
    ;      Offset 0.5 [bin] 0 [bin] 0 Blocks
    ;      out of range Passed
    rowno=WHERE(shortres EQ 'Block Tim')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+1),' ',/EXTRACT)
      IF clipSplit(-2) NE '0' THEN errMsg=[errMsg,clipSplit(-2)+' block(s) with timing offset out of range.']
      IF N_ELEMENTS(rowno) EQ 2 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno(1)+1),' ',/EXTRACT)
        IF clipSplit(-2) NE '0' THEN errMsg=[errMsg,clipSplit(-2)+' block(s) with  timing width out of range.']
      ENDIF
    ENDIF ELSE BEGIN
      rowno=WHERE(shortres EQ 'Block Ave')
      IF rowno(0) NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno(0)+3),' ',/EXTRACT)
        IF clipSplit(4) NE '0' THEN errMsg=[errMsg,clipSplit(4)+' block(s) with timing offset out of range.']
        IF N_ELEMENTS(rowno) EQ 2 THEN BEGIN
          clipSplit=STRSPLIT(clipres(rowno(1)+1),' ',/EXTRACT)
          IF clipSplit(6) NE '0' THEN errMsg=[errMsg,clipSplit(6)+' block(s) with timing width out of range.']
        ENDIF
      ENDIF
    ENDELSE

    ;      Time Alignment
    ;      Residual 3 [mm] 0 [mm] 1.17 [mm] Passed
    ;      Time Alignment Fit
    ;      (x / y) 2 [mm] 0 [mm] 0.38 [mm] /
    ;      0.41 [mm] Passed
    rowno=WHERE(shortres EQ 'Time Alig')
    IF N_ELEMENTS(rowno) GE 3 THEN BEGIN
      IF rowno(1) NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno(1)+1),' ',/EXTRACT)
        timeAlignResid=FLOAT(clipSplit(-3))
      ENDIF ELSE timeAlignResid=-1
      resVect(4)=timeAlignResid
      IF rowno(2) NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno(2)+1),' ',/EXTRACT)
        timeAlignFitx=FLOAT(clipSplit(-3))
        clipSplit=STRSPLIT(clipres(rowno(2)+2),' ',/EXTRACT)
        timeAlignFity=FLOAT(clipSplit(0))
        timeAlignFit=[timeAlignFitx,timeAlignFity]
      ENDIF ELSE timeAlignFit=[-1,-1]
      resVect[5:6]=timeAlignFit
    ENDIF

    ;      Phantom position:
    ;      Axis Value [mm]
    ;      X -0.2
    ;      Y -1.9
    phantomPos=[0,0]
    rowno=WHERE(shortres EQ 'Axis Valu'); older versions
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+1),' ',/EXTRACT)
      phantomPosx=FLOAT(clipSplit(1))
      clipSplit=STRSPLIT(clipres(rowno(0)+2),' ',/EXTRACT)
      phantomPosy=FLOAT(clipSplit(1))
      phantomPos=[phantomPosx,phantomPosy]
    ENDIF ELSE BEGIN; from VG80a / VG76B at least (possibly older too)
      rowno=WHERE(shortres EQ 'Phantom P')
      IF rowno(0) NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno(1)+1),' ',/EXTRACT)
        IF N_ELEMENTS(clipSplit) EQ 9 THEN BEGIN
          phantomPosx=FLOAT(clipSplit(-3))
          clipSplit=STRSPLIT(clipres(rowno(1)+3),' ',/EXTRACT)
          phantomPosy=FLOAT(clipSplit(-3))
          phantomPos=[phantomPosx,phantomPosy]
        ENDIF
      ENDIF
    ENDELSE
    resVect[7:8]=phantomPos


    resArr=[date, icsName, part, full, timA, STRING(calib),STRING(resVect)]
    
    IF TOTAL(resVect[4:6]) EQ -3 OR TOTAL(resVect[4:6]) EQ 0 THEN BEGIN;time alignment residual and fit no longer included in newer reports
      resArr[-5:-3]='-'
    ENDIF

  ENDIF ELSE resArr=STRARR(14)

  res=CREATE_STRUCT('strArrRes',resArr, 'errMsg', errMsg)

  return, res
end;Siemens PET CT Daily QC

;siemens PET_MR QC...xml files from Daily QC
function readPETdailyQC_xml, filen
  errMsg=''
  
  OPENR, filenhet, filen, /GET_LUN
  elem=''
  clipres=!Null
  WHILE ~ EOF(filenhet) DO BEGIN
    READF, filenhet, elem
    clipres=[clipres,STRTRIM(elem,2)];remove leading and trailing space
  ENDWHILE
  CLOSE, filenhet
  FREE_LUN, filenhet
  
  shortres=STRARR(N_ELEMENTS(clipres))
  FOR i=0, N_ELEMENTS(clipres)-1 DO shortres(i)=STRMID(clipres(i), 0, 9)

  resVect=FLTARR(4)

  IF clipres(2) EQ '<SystemQualityReport>' THEN BEGIN
    ;date       
    rowno=WHERE(shortres EQ '<bScandat')
    date=''
    IF rowno(0) NE -1 THEN BEGIN
      rownoDate=WHERE(shortres[rowno(0):rowno(0)+10] EQ '<bddmmyy>')
      IF rownoDate(0) NE -1 THEN BEGIN
        date=STRSPLIT(clipres(rowno(0)+rownoDate(0)),'><',/EXTRACT)
        IF N_ELEMENTS(date) EQ 3 THEN date=date(1) ELSE date=''
      ENDIF
    ENDIF

    rowno=WHERE(shortres EQ '<eCalibra')
    calib=-1
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)),'><',/EXTRACT)
      IF N_ELEMENTS(clipSplit) EQ 3 THEN calib=FLOAT(clipSplit(1))
    ENDIF ELSE calib=-1

    ;Block Noise (#blocks)
    rowno=WHERE(shortres EQ '<aBlockNo')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+10),'><',/EXTRACT)
      IF LONG(clipSplit(1)) GT 3 THEN errMsg=[errMsg,'Block noise out of range.']
    ENDIF

    ;Block Efficiency (#blocks)
    rowno=WHERE(shortres EQ '<bBlockEf')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+10),'><',/EXTRACT)
      IF clipSplit(1) NE '0' THEN errMsg=[errMsg,'Block efficiency out of range.']
    ENDIF

    ;Randoms %
    rowno=WHERE(shortres EQ '<cMeasure')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+10),'><',/EXTRACT)
      measuredRandoms=FLOAT(clipSplit(1))
    ENDIF ELSE measuredRandoms=-1
    resVect(0)=measuredRandoms

    ;Scanner Efficiency
    rowno=WHERE(shortres EQ '<dScanner')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+10),'><',/EXTRACT)
      scannerEff=FLOAT(clipSplit(1))
    ENDIF ELSE scannerEff=-1
    resVect(1)=scannerEff

    ;Scatter Ratio
    rowno=WHERE(shortres EQ '<eScatter')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+10),'><',/EXTRACT)
      scatterRat=FLOAT(clipSplit(1))
    ENDIF ELSE scatterRat=-1
    resVect(2)=scatterRat

    ;ECF
    rowno=WHERE(shortres EQ '<fECF>')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+10),'><',/EXTRACT)
      ECF=FLOAT(clipSplit(1)) 
    ENDIF ELSE ECF=-1
    resVect(3)=ECF

    ;Image PlaneEfficiency
    rowno=WHERE(shortres EQ '<gPlaneEf')
    IF rowno(0) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(0)+10),'><',/EXTRACT)
      IF clipSplit(1) NE '0' THEN errMsg=[errMsg,'Image plane efficiency out of range.']
    ENDIF

    resArr=[date, STRING(calib),STRING(resVect)]
    ;headers=['Date','CalibrationFactor','MeasuredRandoms','ScannerEfficiency','ScatterRatio','ECF']

  ENDIF ELSE resArr=STRARR(6)

  res=CREATE_STRUCT('strArrRes',resArr, 'errMsg', errMsg)

  return, res
end

;Read central frequency and transmit gain from Philips MR ACR results and PIQT test
function readMR_PDF, clipres
  headers=''
  errMsg=''
  resArr=STRARR(9)

  months=['January','February','March','April','May','June','July','August','September','October','November','December']
  shortMonths=months
  FOR i=0, N_ELEMENTS(months)-1 DO shortMonths[i]=STRMID(months[i], 0, 3)

  shortres=STRARR(N_ELEMENTS(clipres))
  FOR i=0, N_ELEMENTS(clipres)-1 DO shortres[i]=STRMID(clipres[i], 0, 9)

  CASE clipres(0) OF
    'ACR Results': BEGIN; weekly or triennial ACR Test from Philips MR
      headers=['Date', 'Test_Type','Central_freq','Transmit_Gain']

      date=STRSPLIT(clipres[3],',',/EXTRACT); acq date
      dateMD=STRSPLIT(date[1],' ',/EXTRACT)
      IF N_ELEMENTS(dateMD) EQ 2 THEN BEGIN
        day=STRING(LONG(dateMD[1]),FORMAT='(i02)')
        month=STRTRIM(dateMD[0])
        monthNmb=WHERE(month EQ months)
        IF monthNmb[0] NE -1 THEN month=STRING(LONG(monthNmb[0])+1, FORMAT='(i02)')
        dateY=STRSPLIT(date[2],' ',/EXTRACT)
        resArr[0]=day+'.'+month+'.'+dateY(0)
      ENDIF

      rowno=WHERE(shortres EQ 'Test Type')
      IF rowno[0] NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno[0]),':',/EXTRACT)
        resArr[1]=STRTRIM(clipSplit[-1],2)
      ENDIF

      rowno=WHERE(shortres EQ 'Center fr')
      IF rowno[0] NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno[0]),' ',/EXTRACT)
        resArr[2]=clipSplit[-1]
      ENDIF

      rowno=WHERE(shortres EQ 'Transmit ')
      IF rowno[0] NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno[0]),' ',/EXTRACT)
        resArr[3]=clipSplit[-1]
      ENDIF

    END
    'Flood Field Uniformity':BEGIN;PIQT from Philips MR

      headers=['Date', 'Scan_Name1','Scan_Name2','RF_Factor1','RF_Factor2','Central_freq1','Central_freq1']

      date=STRSPLIT(clipres[1],',',/EXTRACT)
      dateMD=STRSPLIT(date[1],' ',/EXTRACT)
      IF N_ELEMENTS(dateMD) GE 4 THEN BEGIN
        day=STRING(LONG(dateMD[0]),FORMAT='(i02)')
        month=STRTRIM(dateMD[1])
        monthNmb=WHERE(month EQ shortMonths)
        IF monthNmb[0] NE -1 THEN month=STRING(LONG(monthNmb[0])+1, FORMAT='(i02)')
        resArr[0]=day+'.'+month+'.'+dateMD[2]
      ENDIF

      rowno=WHERE(shortres EQ 'Scan_Name')
      IF rowno[0] NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno[0]),' ',/EXTRACT)
        IF N_ELEMENTS(clipSplit) EQ 3 THEN resArr[1:2]=clipSplit[1:2]
      ENDIF

      rowno=WHERE(shortres EQ 'RF_Factor')
      IF rowno[0] NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno[0]),' ',/EXTRACT)
        IF N_ELEMENTS(clipSplit) EQ 3 THEN resArr[3:4]=clipSplit[1:2]
      ENDIF

      rowno=WHERE(shortres EQ 'Central_f')
      IF rowno[0] NE -1 THEN BEGIN
        clipSplit=STRSPLIT(clipres(rowno[0]),' ',/EXTRACT)
        IF N_ELEMENTS(clipSplit) EQ 3 THEN resArr[5:6]=clipSplit[1:2]
      ENDIF

    END
    ELSE: errMsg='No results in file or unexpected content or language.'
  ENDCASE


  res=CREATE_STRUCT('strArrRes',resArr, 'headers', headers, 'errMsg', errMsg)

  return, res
end

;QAP report from GE XR656 (conventional xray)
;files assumed to be .txt files and at least one file, msg back to autoTempRun
pro readQAPreport, adrTempTemp, outputFile, msg, decimark

  ;exampleContent:
  ;
  ;IMAGE QUALITY TEST RESULTS                Mon Nov 14 07:54:37 CET 2016
  ;                                          Detector Serial No.: UA46409-4
  ;
  ;---------------------------------------------------------------------
  ;TEST                          MEASUREMENT     LSL      USL     STATUS
  ;---------------------------------------------------------------------
  ;OVERALL RESULT                                                   FAIL
  ;No. Of Bad Pixels                      1.00       -      30              PASS
  ;Global Brightness Non Uniformity       4.28       -      30              PASS
  ;Local Brightness Non Uniformity        0.92       -       5              PASS
  ;SNR Non Uniformity                    23.95       -      60              PASS
  ;Spatial MTF at 0.5 lp/mm              85.99      65       -              PASS
  ;Spatial MTF at 1.0 lp/mm              67.07      48       -              PASS
  ;Spatial MTF at 1.5 lp/mm              49.33      31       -              PASS
  ;Spatial MTF at 2.0 lp/mm              34.77      21       -              PASS
  ;Spatial MTF at 2.5 lp/mm              23.59      15       -              PASS

  ;what to read from it:
  ;
  ;headers=
  ;0['Date',
  ;1'Detector serial',
  ;2'Stand',
  ;3'Overall
  ;4'Bad pixels',
  ;5'Global brightness non uniformity',
  ;6'Local brightness non uniformity',
  ;7'SNR non uniformity',$
  ;8-12'MTF at 0.5 lp/mm','1.0','1.5','2.0','2.5']

  monthsStr=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  headers=['Date','Detector serial','Stand','Overall result',$
    'Bad pixels','Global brightness non uniformity','Local brightness non uniformity','SNR non uniformity',$
    'MTF at 0.5 lp/mm','1.0','1.5','2.0','2.5']

  nFiles=N_ELEMENTS(adrTempTemp)
  readFiles=INTARR(nFiles)

  testResFile=FILE_INFO(outputFile)

  strArrRes=STRARR(13,nFiles)

  FOR q=0, nFiles-1 DO BEGIN

    fi=FILE_INFO(adrTempTemp(q))
    IF fi.read THEN BEGIN
      OPENR, lun, adrTempTemp(q), /GET_LUN
      arr=''
      line=''
      WHILE NOT EOF(lun) DO BEGIN
        READF, lun, line
        arr=[arr,line]
      ENDWHILE
      FREE_LUN,lun

      IF N_ELEMENTS(arr) GT 6 THEN BEGIN
        arr=arr[1:N_ELEMENTS(arr)-1]
        readFiles(q)=1
        ;date
        dateStr=STRSPLIT(arr(0),' ',/EXTRACT)
        year=dateStr(-1)
        date=dateStr(-4)
        month=STRMID(dateStr(-5),0,3)
        monthNmb=WHERE(month EQ monthsStr)
        IF monthNmb(0) NE -1 THEN BEGIN
          month=STRING(LONg(monthNmb(0))+1, FORMAT='(i02)')
          date=date+'.'+month+'.'+year
        ENDIF ELSE date='XX.XX.XXXX'
        strArrRes[0,q]=date

        ;detector
        detStr=STRSPLIT(arr(1),' ',/EXTRACT)
        detector=detStr(-1)
        strArrRes[1,q]=detector

        ;stand
        adrSplit=STRSPLIT(adrTempTemp(q), PATH_SEP(), /EXTRACT)
        strArrRes[2,q]=adrSplit(-2)

        ;overall
        overStr=STRSPLIT(arr(6),' ',/EXTRACT)
        strArrRes[3,q]=overStr(-1)

        IF N_ELEMENTS(arr) GT 8 THEN BEGIN
          nn=8
          IF N_ELEMENTS(arr) EQ 12 THEN nn=3
          FOR i =0, nn DO BEGIN
            ss=STRSPLIT(arr(7+i),' ',/EXTRACT)
            strArrRes[4+i,q]=ss(-4)
            IF i EQ 4 THEN headers(4+i)=STRJOIN(ss[1:4],' ')
            IF i GT 4 THEN headers(4+i)=ss(3)
          ENDFOR
        ENDIF

      ENDIF;N arr < 6

    ENDIF;file not read

  ENDFOR

  idRead=WHERE(readFiles EQ 1, nRead)

  IF nRead GT 0 THEN BEGIN
    adrTempTemp=adrTempTemp(idRead)
    strArrRead=STRARR(13,nRead)
    FOR r=0, nRead-1 DO strArrRead[*,r]=strArrRes[*,idRead(r)]

    ;decimal mark not .?
    IF decimark EQ ',' THEN BEGIN
      FOR i=0, nRead-1 DO BEGIN
        FOR j=4, 12 DO BEGIN
          strArrRead[j,i]=STRJOIN(STRSPLIT(strArrRead[j,i], '.',/EXTRACT),',')
        ENDFOR
      ENDFOR
    ENDIF

    IF testResFile.write EQ 0 OR (testResFile.write AND testResFile.SIZE EQ 0) THEN strArrRead=[[headers],[strArrRead]];include header to clipboard

    CLIPBOARD.set, STRJOIN(strArrRead, STRING(9B))
    IF testResFile.exists EQ 0 THEN BEGIN;create file
      create=1
      Catch, Error_status
      IF Error_status NE 0 THEN BEGIN
        create=0
        CATCH, /CANCEL
      ENDIF
      IF create THEN BEGIN
        OPENW, resfile, outputFile, /APPEND, /GET_LUN
        CLOSE, resfile & FREE_LUN, resfile
      ENDIF
    ENDIF
    testResFile=FILE_INFO(outputFile)
    IF testResFile.write THEN BEGIN
      OPENW, resfile, outputFile, /APPEND, /GET_LUN
      PRINTF, resfile, CLIPBOARD.GET()
      CLOSE, resfile & FREE_LUN, resfile
      msg='written'
    ENDIF ELSE msg='clipboard'

  ENDIF ELSE msg='None of the '+STRING(nFiles, FORMAT='(i0)')+' files could be read or did not have the expected content'

end