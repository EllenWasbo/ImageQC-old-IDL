;readQAP - extraction of figures to table from GE QAP reports
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
;
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