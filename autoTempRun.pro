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

;small functions and procedures for reuse when reading txt and pdf files to extract data

;array (vector of strings) in, replace decimal mark if dec other than content. Dec '' is do not change
;write to file or
function writeAutoTxtRes, strArrInput, dec
  strLine=''
  FOR i=1, N_ELEMENTS(strArrInput) -1 DO BEGIN;from 1 to avoid changing . in date
    CASE dec OF
      ',': strArrInput(i)=STRJOIN(STRSPLIT(strArrInput(i), '.',/EXTRACT),',')
      '.': strArrInput(i)=STRJOIN(STRSPLIT(strArrInput(i), ',',/EXTRACT),'.')
      ELSE:
    ENDCASE
  ENDFOR

  strLine=STRJOIN(strArrInput, STRING(9B))
  return, strLine
end

pro writeResToFile, headL, resL, wpath, inputPath, toClipb, errArr, nn, nAdr, archivePaths, nnWritten, parent
  resFileExist=0
  testResFile=FILE_INFO(wpath)
  IF wpath NE '' THEN IF testResFile.exists THEN resFileExist=1
  IF resFileExist THEN BEGIN
    IF testResFile.SIZE EQ 0 THEN BEGIN
      CLIPBOARD.set, headL + resL
    ENDIF ELSE CLIPBOARD.set, resL

    writeToFile=1
    IF toClipb THEN BEGIN
      sv=DIALOG_MESSAGE('You have selected specific files to extract data from. Results exist in clipboard (without headers) until closing this dialog. Append to resultfile for selected template?',/QUESTION, DIALOG_PARENT=parent)
      IF sv EQ 'No' THEN writeToFile=0
    ENDIF
    IF writeToFile THEN BEGIN
      IF FILE_TEST(wpath, /WRITE) THEN BEGIN
        OPENW, resfile, wpath, /APPEND, /GET_LUN
        IF nn EQ 0 THEN PRINTF, resfile, CLIPBOARD.GET() ELSE PRINTF, resfile, resL
        CLOSE, resfile & FREE_LUN, resfile
        archivePaths=[archivePaths, inputPath]
        nnWritten=nnWritten+1
      ENDIF ELSE errArr(nn)=errArr(nn)+' Failed to write results to file. Writingprotection?'
    ENDIF
  ENDIF ELSE BEGIN
    CLIPBOARD.set, headL + resL
    sv=DIALOG_MESSAGE('The result file do not exist. Find the results in clipboard until closing this dialog. Continue reading rest of files?', /QUESTION, DIALOG_PARENT=parent)
    IF sv EQ 'No' THEN nn=nAdr
  ENDELSE
end
;--------------- sort files by last modified date ---------------------
function sortFilesByDate, inputPaths
  sortedFiles=''
  IF SIZE(inputPaths, /TNAME) EQ 'STRING' THEN BEGIN
    dateList=!Null
    FOR i=0, N_ELEMENTS(inputPaths)-1 DO BEGIN
      mtime=FILE_MODTIME(inputPaths(i))
      dateList=[dateList,mtime]
    ENDFOR
    sortedFiles=inputPaths(SORT(dateList))
  ENDIF
  return, sortedFiles
end

;--------------- copy PDF to clipboard using Excel macro - allowing to wait for Excel and Acrobat Reader to open and close before code proceeds (and miss the clipboard)
function PDF2clipboard, inputPath, imQCpath, waitS
  clipboard.set, inputPath; send input Path to Excel macro
  SPAWN, 'start Excel.exe "'+imQCPath+'data\convertPDFtoTXT.xlsm" /e', /HIDE
  WAIT, waitS(0);give time to open Excel/Acrobat Reader
  FOR w=0, 9 DO BEGIN; wait 1 sec for up to 10 sec to see if clipboard changed
    pdfContent=clipboard.get()
    IF pdfContent(0) EQ inputPath OR pdfContent(0) EQ '' THEN WAIT,1 ELSE BREAK
  ENDFOR
  pdfContent=clipboard.get()
  IF pdfContent(0) EQ inputPath OR pdfContent(0) EQ '' THEN pdfContent=''

  return, pdfContent
end

;archive the files while running Automation template
function archiveFiles, inputPaths, archiveParentPath
  nFailed=0
  ;archive
  sep=PATH_SEP()
  IF STRMID(archiveParentPath(0), 0, /REVERSE_OFFSET) NE sep THEN archiveParentPath=archiveParentPath(0)+sep
  archivePath=archiveParentPath+'Archive\'
  fi=FILE_INFO(archivePath)
  IF fi.exists EQ 0 THEN FILE_MKDIR, archivePath
  FOR i=0, N_ELEMENTS(inputPaths)-1 DO BEGIN
    newFiName=archivePath+FILE_BASENAME(inputPaths(i))
    fin=FILE_INFO(newFiName)
    IF fin.exists EQ 0 AND fi.write EQ 1 AND FILE_TEST(inputPaths(i), /WRITE) THEN file_move, inputPaths(i), newFiName ELSE nFailed=nFailed+1
  ENDFOR

  return, nFailed

end

pro autoTempRun, thisTemp, thisModality, LOOP=loop, PICKFILES=pickfiles, TEMPNAME=tempname, TESTLOG=testLog, AUTOPAUSE=autopause

  COMPILE_OPT hidden
  COMMON VARI

  IF N_ELEMENTS(testLog) EQ 0 THEN testlog=tempname+':' ELSE testLog=testLog+newline+tempname+':'
  RESTORE, configPath

  IF N_ELEMENTS(pickfiles) EQ 0 THEN pickfiles=0
  IF N_ELEMENTS(loop) EQ 0 THEN loop2=0 ELSE loop2=loop; loop=1 sent form autoOpen to indicate that this is not the last template to run, loop=-1 = last run (no loop defined = no looping)
  ;loop2 is the local viriable of this to indicate if the pro was called with loop option or not and save loop2 to loop at end to send back indication to stop the loop or not
  ;autoStopFlag indicate that something happened that stopped the looping - COMMON VARI variable also used by updateMulti to indicate that the automation should be stopped

  ;set parameterset
  configName=thisTemp.paramSet
  IF thisTemp.alternative EQ '' THEN BEGIN
    tnamesConfigS=TAG_NAMES(configS)
    tagno=WHERE(STRUPCASE(tnamesConfigS) EQ STRUPCASE(configName))
    IF tagno(0) NE -1 THEN BEGIN
      selConfig=tagno(0)
      refreshParam, configS.(tagno(0)), configName
    ENDIF ELSE BEGIN
      sv=DIALOG_MESSAGE('Did not find the saved parameterset-name ('+configName+') in list of parametersets.',DIALOG_PARENT=evTop)
      testLog=testLog+newline+tab+'Did not find the saved parameterset-name ('+configName+') in list of parametersets.'
    ENDELSE
  ENDIF

  deciMarkDefault=configS.(configS.(0).DEFCONFIGNO).DECIMARK

  ;find images in path
  IF thisTemp.path NE '' OR pickfiles THEN BEGIN
    WIDGET_CONTROL, /HOURGLASS
    WIDGET_CONTROL, btnAppend, SET_BUTTON=0
    clearAll
    adr=thisTemp.path
    adrTempTemp=''

    ;ensure path ending with separator
    sep=PATH_SEP()
    IF STRMID(adr(0), 0, /REVERSE_OFFSET) NE sep THEN adr=adr(0)+sep

    IF pickfiles THEN BEGIN
      adrTempTemp=DIALOG_PICKFILE(TITLE='Select the files', PATH=adr, /READ, /Multiple_files, DIALOG_PARENT=evTop)
    ENDIF ELSE BEGIN
      IF thisTemp.alternative EQ '' THEN BEGIN
        ;IF thisTemp.includeSub THEN Spawn, 'dir '  + '"'+adr(0)+'"' + '*'+ '/b /s', adrTempTemp ELSE BEGIN
        Spawn, 'dir '  + '"'+adr(0)+'"' + '*'+ '/b /a-D', adrTempTemp
        adrTempTemp=adr(0)+adrTempTemp
        ;ENDELSE
      ENDIF ELSE BEGIN

        altType=STRMID(thisTemp.alternative,0,3)
        CASE altType OF
          'PDF': BEGIN
            Spawn, 'dir '  + '"'+adr(0)+'"' + '*.pdf'+ '/b /a-D', adrTempTemp
            IF adrTempTemp(0) NE '' THEN BEGIN
              adrTempTemp=adr(0)+adrTempTemp
            ENDIF
          END
          'XML':BEGIN
            Spawn, 'dir '  + '"'+adr(0)+'"' + '*.xml'+ '/b /a-D', adrTempTemp
            IF adrTempTemp(0) NE '' THEN BEGIN
              adrTempTemp=adr(0)+adrTempTemp
            ENDIF
          END
          ELSE:
        ENDCASE

        CASE thisTemp.alternative OF
          'GE_QAP': adrTempTemp=findNewGE_QAP_files(adr(0));findNewGE_QAP_files defined in a0_functionsMini.pro
          ELSE:
        ENDCASE

      ENDELSE
    ENDELSE

    IF adrTempTemp(0) NE '' THEN BEGIN

      IF thisTemp.alternative EQ '' THEN BEGIN
        dcmAdr=!Null
        delAdr=!Null
        nFound=N_ELEMENTS(adrTempTemp)
        dcmOk=INTARR(nFound)

        WIDGET_CONTROL, lblProgress, SET_VALUE='Checking for DICOM files for template '+tempname
        FOR d=0, nFound-1 DO BEGIN

          IF FILE_BASENAME(adrTempTemp(d)) EQ 'DICOMDIR' THEN BEGIN
            dcmOk(d)=0 ; IDL crash if QUERY_DICOM on DICOMDIR - unknown reason
          ENDIF ELSE BEGIN
            fi=FILE_INFO(adrTempTemp(d))
            IF fi.directory NE 1 AND fi.exists THEN dcmOk(d)=QUERY_DICOM(adrTempTemp(d))
          ENDELSE

        ENDFOR

        dcmOkId=WHERE(dcmOk EQ 1)
        dcmNotOkId=WHERE(dcmOk EQ 0)
        IF dcmNotOkID(0) NE -1 THEN delAdr=adrTempTemp(dcmNotOkId)
        IF dcmOkId(0) NE -1 THEN dcmAdr=adrTempTemp(dcmOkId)

        nFiles=N_ELEMENTS(dcmAdr)
        IF nFiles EQ 0 THEN BEGIN
          IF loop2 EQ 0 THEN sv=DIALOG_MESSAGE('Found no valid DICOM files in '+adr(0), DIALOG_PARENT=evTop) ELSE testLog=testLog+newline+tab+'Found no valid DICOM files in '+adr(0)
          WIDGET_CONTROL, lblProgress, SET_VALUE=' '
        ENDIF ELSE BEGIN
          origDcmAdr=dcmAdr
          WIDGET_CONTROL, /HOURGLASS
          openFiles, dcmAdr, SILENT=1

          WIDGET_CONTROL, /HOURGLASS
          nImg=N_TAGS(structImgs)
          tnames=TAG_NAMES(structImgs)

          ;delete files with no image info?
          accAdr=!Null
          IF tnames(0) NE 'EMPTY' THEN FOR i=0, nImg-1 DO accAdr=[accAdr, structImgs.(i).filename]

          ;picked files check that it is the correct station name according to the template
          stopAnalysis=0
          IF pickFiles THEN BEGIN
            arrStatName=!Null
            IF tnames(0) NE 'EMPTY' THEN FOR i=0, nImg-1 DO arrStatName=[arrStatName, structImgs.(i).stationName]
            arrStatNameUniq=arrStatName(UNIQ(arrStatName))
            oneName=1
            IF N_ELEMENTS(arrStatNameUniq) GT 1 THEN oneName=0
            IF arrStatNameUniq.HasValue(thisTemp.statName) OR thisTemp.statName EQ '' THEN BEGIN
              IF oneName EQ 0 THEN BEGIN
                sv=DIALOG_MESSAGE('The data set contain files with another station name than defined in template.'+newline+'Data set: '+ STRJOIN(arrStatNameUniq,'/') +newline+'Template: '+thisTemp.statName+newline+newline+'Continue analysis?',/QUESTION,DIALOG_PARENT=evTop)
                IF sv EQ 'No' THEN stopAnalysis=1
              ENDIF
            ENDIF ELSE BEGIN
              sv=DIALOG_MESSAGE('Mismatch between station name in data set and station name in template.'+newline+'Data set: '+ arrStatNameUniq +newline+'Template: '+thisTemp.statName+newline+newline+'Continue analysis?',/QUESTION,DIALOG_PARENT=evTop)
              IF sv EQ 'No' THEN stopAnalysis=1
            ENDELSE
          ENDIF

          IF stopAnalysis THEN BEGIN
            tnames='EMPTY'
            clearAll
          ENDIF

          ;**************************************************************************************
          IF tnames(0) NE 'EMPTY' AND stopAnalysis NE 1 THEN BEGIN; only non-image files in folder

            FOR i=0, N_ELEMENTS(origDcmAdr)-1 DO BEGIN
              okAdr=WHERE(origDcmAdr(i) EQ accAdr)
              IF okAdr(0) EQ -1 THEN delAdr=[delAdr, origDcmAdr(i)]
            ENDFOR

            ;sort by
            IF thisTemp.sortBy(0) NE '' THEN BEGIN

              tnames=TAG_NAMES(structImgs.(0))
              sortNames=thisTemp.sortBy
              descArr=thisTemp.sortAsc
              IF N_ELEMENTS(descArr) GT N_ELEMENTS(sortNames) THEN sortNames=sortNames[0:N_ELEMENTS(descArr)-1];from error in between version 1.74,1.75

              keyArr=STRARR(nIMG,9);max 9 sorting levels

              FOR ss=0, N_ELEMENTS(sortNames)-1 DO BEGIN; for each tag to sort by
                WIDGET_CONTROL, lblProgress, SET_VALUE='Sorting images: '+STRING(ss*100./N_ELEMENTS(sortNames), FORMAT='(i0)')+' %'
                tagno=WHERE(tnames EQ STRUPCASE(thisTemp.sortBy(ss)))
                IF tagno(0) NE -1 THEN BEGIN
                  list2sort=!Null
                  FOR i=0, nImg-1 DO list2sort=[list2sort,structImgs.(i).(tagno(0))(0)]
                  reformatNo=WHERE(STRUPCASE(imgStructInfo[0,*]) EQ STRUPCASE(thisTemp.sortBy(ss)))
                  IF reformatNo(0) NE -1 THEN newFormat=imgStructInfo[1,reformatNo(0)] ELSE newFormat='STRING'
                  IF newFormat EQ 'FLOAT' THEN newFormat='DOUBLE'
                  CASE newFormat OF
                    'FLOAT':list2sort=STRING(FLOAT(list2sort)-MIN(FLOAT(list2sort)), FORMAT='(f06.5)')
                    'DOUBLE':list2sort=STRING(DOUBLE(list2sort)-MIN(DOUBLE(list2sort)), FORMAT='(f010.5)')
                    'LONG':list2sort=STRING(LONG(list2sort)-MIN(LONG(list2sort)), FORMAT='(i010)')
                    ELSE:
                  ENDCASE
                  keyArr[*,ss]=list2sort
                ENDIF
              ENDFOR
              WIDGET_CONTROL, lblProgress, SET_VALUE=''

              ;newOrder=MULTISORT(keyArr[*,0],keyArr[*,1],keyArr[*,2],keyArr[*,3],keyArr[*,4],keyArr[*,5],keyArr[*,6],keyArr[*,7],keyArr[*,8])
              newOrder=multiBsort(keyArr,descArr)

              IF ARRAY_EQUAL(newOrder,INDGEN(nImg)) EQ 0 THEN structImgs=reorderStructStruct(structImgs, newOrder)
            ENDIF;sortby

            ;sort on acqdate
            firstImg=0
            lastImg=nImg-1
            list2sort=!Null
            FOR i=0, nImg-1 DO list2sort=[list2sort,structImgs.(i).acqDate]
            newOrder=BSORT(list2sort);bsort stable sort - not rearranging if not necessary
            IF ARRAY_EQUAL(newOrder,INDGEN(nImg)) EQ 0 THEN structImgs=reorderStructStruct(structImgs, newOrder)
            dates=list2sort(newOrder)
            dateList=dates(UNIQ(dates))

            structImgsAll=structImgs
            structImgs=CREATE_STRUCT('empty',0)

            ;set quickTemp
            qtName=thisTemp.quickTemp
            qtNumb=-1
            IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
              IF SIZE(quickTemp.(thisModality), /TNAME) EQ 'STRUCT' THEN fillQuickTempList, quickTemp.(thisModality) ELSE fillQuickTempList, -1
            ENDIF ELSE fillQuickTempList, -1

            ;justOpenAndStop=0

            modality=thisModality
            IF qtName EQ '' THEN BEGIN

              IF N_ELEMENTS(loop) EQ 0 THEN looptemp=-1 ELSE looptemp=loop
              IF loop2 AND looptemp NE -1 THEN BEGIN
                sv=DIALOG_MESSAGE('Missing QuickTest template in '+tempname+'. Skip to next template?',/QUESTION,DIALOG_PARENT=evTop)
                IF sv EQ 'No' THEN BEGIN
                  loop2=0
                  ;justOpenAndStop=1
                ENDIF
              ENDIF ELSE BEGIN
                sv=DIALOG_MESSAGE('Missing QuickTest template in '+tempname+'.',DIALOG_PARENT=evTop)
                ;justOpenAndStop=1
              ENDELSE
              fillQuickTempList, quickTemp.(thisModality)
              autoStopFlag=1
              testLog=testLog+newline+tab+'No QuickTest template defined.'
            ENDIF ELSE BEGIN
              IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
                IF SIZE(quickTemp.(thisModality), /TNAME) EQ 'STRUCT' THEN BEGIN
                  tnamesQT=TAG_NAMES(quickTemp.(thisModality))
                  tagno=WHERE(STRUPCASE(tnamesQT) EQ STRUPCASE(qtName))
                  qtNumb=tagno(0)
                  IF tagno(0) NE -1 THEN BEGIN
                    fillQuickTempList, quickTemp.(thisModality), SELECT_NAME=qtName;pro in refreshParam.pro
                    WIDGET_CONTROL, btnUseMulti, SET_BUTTON=1
                  ENDIF ELSE BEGIN
                    fillQuickTempList, quickTemp.(thisModality)
                    sv=DIALOG_MESSAGE('QuickTest template named '+ qtName+ ' defined in '+tempname+'. This QuickTest template cannot be found anymore for current modality. Skip to next template?',/QUESTION,DIALOG_PARENT=evTop)
                    testLog=testLog+newline+tab+'QuickTest template '+ qtName+ ' missing.'
                    IF sv EQ 'No' THEN BEGIN
                      loop2=0
                      ;justOpenAndStop=1
                    ENDIF
                    autoStopFlag=1
                  ENDELSE
                ENDIF
              ENDIF
            ENDELSE

            IF loop2 EQ 1 AND autoStopFlag EQ 1 THEN skipToNext=1 ELSE skipToNext=0

            ;----------------------------------------------------------------------------------------
            ;-------------------------------continue to open files------------------------------------
            IF skipToNext EQ 0 THEN BEGIN

              oldCopyHeader=copyHeader
              oldTransposeTable=transposeTable
              ;infoLogg1='' & infoLogg2=''
              writtenToFile=0
              notMove=-1

              IF autoStopFlag AND loop2 EQ 0 THEN BEGIN
                endLoop=0
                idCont=WHERE(dateList NE '')
                IF idCont(0) NE -1 THEN dateList=dateList(idCont(0))
              ENDIF ELSE endLoop=N_ELEMENTS(dateList)-1

              writeOK=-1
              archiveOK=-1
              WIDGET_CONTROL, lblProgress0, GET_VALUE=currStat
              FOR q=0, endLoop DO BEGIN

                IF datelist(q) NE '' THEN BEGIN
                  IF N_ELEMENTS(dateList) NE 1 THEN BEGIN
                    notIds=WHERE(dates NE dateList(q))
                    WIDGET_CONTROL, lblProgress0, SET_VALUE=currStat+' Analysing date '+STRING(q+1, FORMAT='(i0)')+' / ' + STRING(N_ELEMENTS(dateList), FORMAT='(i0)')
                    allIds=INDGEN(nImg)
                    remIds=allIds(notIds)
                    structImgs=removeIDstructstruct(structImgsAll,remIds)
                  ENDIF ELSE BEGIN
                    structImgs=structImgsAll
                    structImgsAll=!Null
                  ENDELSE

                  activeImg=readImg(structImgs.(0).filename, structImgs.(0).frameNo)
                  fileList=getListOpenFiles(structImgs,0,marked, markedMulti)
                  WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=0, SET_LIST_TOP=0
                  WIDGET_CONTROL, listFiles, SCR_YSIZE=listFilesYsize
                  WIDGET_CONTROL, lblLoadedN, SET_VALUE=STRING(n_elements(fileList), FORMAT='(i0)')+' )'
                  clearRes
                  updateInfo

                  IF autoStopFlag EQ 0 THEN BEGIN
                    nImgTemp=0
                    updateMulti, AUTOACTIVE=1, NIMGTEMP=nImgTemp;can set autoStopFlag to 1 if numbers do not match

                    CASE autoStopFlag OF
                      1:BEGIN;stop
                        loop2=0
                        endLoop=0
                      END
                      2: BEGIN;skip to next template
                        autoStopFlag=0
                        endLoop=0
                        skipToNext=1
                      END
                      ELSE:
                    ENDCASE

                    tags=TAG_NAMES(structImgs)
                    IF tags(0) NE 'EMPTY' AND autoStopFlag EQ 0 AND skipToNext EQ 0 THEN BEGIN
                      calculateQuickTest
                      ;*****************************'
                      ;********************************
                      ;**********************************
                      IF TOTAL(results) GT 0 THEN BEGIN
                        testLog=testLog+newline+tab+datelist(q)+': Found '+STRING(n_elements(fileList), FORMAT='(i0)')+' images.'
                        IF nImgTemp NE n_elements(fileList) THEN testLog=testLog+' Minimum '+STRING(nImgTemp,FORMAT='(i0)')+' images expected.'
                        errFile=0
                        ;append resultfile?
                        IF thisTemp.pathApp NE '' THEN BEGIN
                          transposeTable=1

                          ;file exists?
                          testResFile=FILE_INFO(thisTemp.pathApp)
                          IF testResFile.exists THEN BEGIN
                            IF testResFile.SIZE EQ 0 THEN copyHeader=1 ELSE copyHeader=0
                            exportMulti
                            IF pickfiles THEN BEGIN
                              IF writeOK EQ -1 THEN BEGIN; ask only for first date if more than one
                                sv=DIALOG_MESSAGE('You have selected specific files to analyse. Results exist in clipboard. Append to resultfile for selected template?',/QUESTION, DIALOG_PARENT=evTop)
                                IF sv EQ 'No' THEN writeOK=0 ELSE writeOK=1
                              ENDIF
                            ENDIF ELSE writeOK=1
                            IF writeOK THEN BEGIN
                              OPENW, resfile, thisTemp.pathApp, /APPEND, /GET_LUN
                              PRINTF, resfile, CLIPBOARD.GET()
                              CLOSE, resfile & FREE_LUN, resfile
                              writtenToFile=1
                            ENDIF
                            ;IF infoLogg1 EQ '' THEN infoLogg1='Written results to file';commas not working wiht cw_form... for dates: '+structImgs.(0).acqDate ELSE infoLogg1=infoLogg1+', '+structImgs.(0).acqDate
                          ENDIF ELSE BEGIN
                            errFile=1
                            exportMulti;to clipboard
                          ENDELSE

                        ENDIF ELSE exportMulti;to clipboard

                        IF writtenToFile EQ 0 THEN BEGIN
                          sv='Yes'
                          IF errFile EQ 1 THEN sv=DIALOG_MESSAGE('Could not find file '+thisTemp.pathApp+newline+'. Current results can be found in clipboard. Paste these manually to a result file and then press Yes to continue or No to stop.', /QUESTION,DIALOG_PARENT=evTop)
                          IF thisTemp.pathApp EQ '' THEN sv=DIALOG_MESSAGE('No path for results specified. Current results can be found in clipboard. Paste these manually to a result file and then press Yes to continue or No to stop.', /QUESTION,DIALOG_PARENT=evTop)
                          IF sv EQ 'No' THEN BEGIN
                            loop2=0
                            autoStopFlag=1
                          ENDIF
                        ENDIF ELSE testLog=testLog+newline+ tab+tab+'Results written to file.'

                        IF thisTemp.deleteFilesEnd AND autoStopFlag EQ 0 THEN BEGIN
                          imgWithMark=WHERE(TOTAL(markedMulti,1) GT 0)
                          IF N_ELEMENTS(tags) GT imgWithMark(-1)+1 AND pickFiles EQ 0 THEN BEGIN
                            delAdr=!Null
                            dd=imgWithMark(-1)+1
                            selList=!Null
                            WHILE dd LT N_ELEMENTS(tags) DO BEGIN
                              delAdr=[delAdr,structImgs.(dd).filename]
                              selList=[selList,dd]
                              dd=dd+1
                            ENDWHILE
                            IF N_ELEMENTS(delAdr) GT 0 THEN BEGIN
                              FILE_DELETE, delAdr, /QUIET, /RECYCLE
                              closeImgs, selList
                              ;infoLogg2=infoLogg2+newline+STRING(N_ELEMENTS(delAdr),FORMAT='(i0)')+' files exceeding last image analysed moved to recycle bin'
                              testLog=testLog+newline+ tab+tab+STRING(N_ELEMENTS(delAdr),FORMAT='(i0)')+' files exceeding last image deleted.'
                            ENDIF
                          ENDIF
                        ENDIF;deleteFilesEnd

                        ;move files?
                        IF thisTemp.archive AND autoStopFlag EQ 0 THEN BEGIN
                          IF pickfiles THEN BEGIN
                            IF archiveOK EQ -1 THEN BEGIN
                              sv=DIALOG_MESSAGE('You have selected specific files to analyse. Move files to archive?',/QUESTION, DIALOG_PARENT=evTop)
                              IF sv EQ 'No' THEN archiveOK=0 ELSE archiveOK=1
                            ENDIF
                          ENDIF ELSE archiveOK=1
                          IF archiveOK THEN BEGIN
                            IF N_TAGS(structImgs) EQ 1 THEN archivePath=FILE_DIRNAME(structImgs.(0).filename)+'\Archive\' $
                            ELSE archivePath=FILE_DIRNAME(structImgs.(0).filename)+'\Archive\'+datelist(q)+'\'
                            fi=FILE_INFO(archivePath)
                            IF fi.exists EQ 0 THEN FILE_MKDIR, archivePath
                            FOR i=0, N_TAGS(structImgs)-1 DO BEGIN
                              IF N_TAGS(sturctImgs) EQ 1 THEN newFiName=archivePath+datelist(q)+'_'+FILE_BASENAME(structImgs.(i).filename) ELSE newFiName=archivePath+FILE_BASENAME(structImgs.(i).filename)
                              fi=FILE_INFO(newFiName)
                              IF fi.exists EQ 0 THEN file_move, structImgs.(i).filename, newFiName
                              structImgs.(i).filename=newFiName
                            ENDFOR

                            ;IF infoLogg2 EQ '' THEN infoLogg2='Moved files to "Archive" in '+STRJOIN(STRSPLIT(thisTemp.path,','),'')+newline;remove comma to not create error with CW_FORM later
                            testLog=testLog+newline+ tab+tab+'Moved files to Archive.'
                          ENDIF
                          fileList=getListOpenFiles(structImgs,0,marked, markedMulti);update with new path
                          WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=0, SET_LIST_TOP=0
                          WIDGET_CONTROL, listFiles, SCR_YSIZE=listFilesYsize
                          WIDGET_CONTROL, lblLoadedN, SET_VALUE=STRING(n_elements(fileList), FORMAT='(i0)')+' )'
                        ENDIF;move files to 'Archive'

                      ENDIF; results GT 0
                      ;***********************
                      ;******************
                      ;********************
                    ENDIF;images to work with for this date (autostop =0 and found images and skipToNext not triggered by updateMulti)
                  ENDIF;autostop flag before multi-tested

                ENDIF ELSE BEGIN; datelist(q) NE '', e.g. not aquired images like 'Patient Protocol' overview
                  noAcqIds=WHERE(dates EQ '')
                  FOR a=0, N_ELEMENTS(noAcqIds)-1 DO delAdr=[delAdr,structImgsAll.(a).filename]
                ENDELSE

              ENDFOR;datelist

              ;IF writtenToFile EQ 1 THEN infoLogg='Written results to file'+newline+infoLogg2 ELSE infoLogg=infoLogg2
              IF skipToNext EQ 0 THEN BEGIN;after updateMulti

                IF thisTemp.deleteFiles THEN BEGIN
                  IF delAdr NE !Null AND pickFiles EQ 0 THEN BEGIN
                    FILE_DELETE, delAdr, /QUIET;, /RECYCLE to recycle-bin?
                    ;infoLogg2=infoLogg2+'Deleted files with no image data. '+newline
                    testLog=testLog+newline+ tab+'Deleted '+STRING(N_ELEMENTS(delAdr),FORMAT='(i0)')+' files with no image data.'
                  ENDIF
                ENDIF
                structImgsAll=!Null
                copyHeader=oldCopyHeader
                transposeTable=oldTransposeTable

                tags=TAG_NAMES(structImgs)
                IF tags(0) NE 'EMPTY' THEN BEGIN
                  redrawImg, 0,1
                  updateInfo;calling updateTable
                  updatePlot,1,1,0
                  WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
                ENDIF ELSE clearAll
                ;IF infoLogg NE '' THEN BEGIN
                IF N_ELEMENTS(loop) NE 0 THEN BEGIN
                  IF autopause THEN BEGIN
                    IF loop2 EQ 1 THEN BEGIN
                      box=[$
                        '1, BASE,, /ROW', $
                        '2, LABEL, ', $
                        '1, BASE,, /ROW', $
                        '0, BUTTON, Continue to next template?, QUIT, TAG=Cont',$
                        '2, BUTTON, Stop, QUIT, TAG=Cancel']
                      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Continue?', XSIZE=300, YSIZE=100, FOCUSNO=3, XOFFSET=xoffset+200, YOFFSET=yoffset+200)
                      IF res.Cancel THEN loop2=0
                    ENDIF
                  ENDIF
                ENDIF; ELSE sv=DIALOG_MESSAGE(infoLogg, /INFORMATION, DIALOG_PARENT=evTop)
                ;ENDIF;infologg
              ENDIF;--skiptonext after updateMulti


            ENDIF;-------------------skiptonext
            ;----------------------------
          ENDIF;dcm with image content GT 0, and not stopped analysis
          ;*******************************************************
          ;ENDIF;dcmadr not empty after loadby
        ENDELSE; valid dicom
      ENDIF ELSE BEGIN ;alternative NE ''
        ;adrTempTemp to evaluate
        nAdr=N_ELEMENTS(adrTempTemp)
        errArr='' & nCols=!Null
        IF adrTempTemp(0) NE '' THEN BEGIN
          errArr=STRARR(nAdr) & nCols=INTARR(nAdr)
        ENDIF
        waitSec=configS.(1).WAIT
        nnWritten=0
        archivePaths=!Null

        PDF_failed_txt='Failed to read PDF content for file '+newline+'Troubleshoot:'+newline+$
          '-Make sure to close Acrobat Reader before starting the process.'+newline+'-Check content of the PDF file.'+newline+$
          '-Consider increasing the number of seconds to wait for PDF input (User settings - Automation templates).'+newline+$
          '-Open Excel and hold SHIFT while opening \ImageQC\data\convertPDFtoTXT.xlsm to verify Acrobat reader .exe path or macro activation.'

        CASE thisTemp.alternative OF
          'PDF_SiemensConstancy': BEGIN
            WIDGET_CONTROL, /HOURGLASS
            configGetSiemensQC=updConfigGetSiemensQC()
            IF adrTempTemp(0) NE '' THEN BEGIN
              adrTempTemp=sortFilesByDate(adrTempTemp);sortFilesByDate defined at top of this file

              headerStruct=configGetSiemensQC.headers
              headerStruct=removeIDstructstruct(headerStruct,0);remove PET header
              nColsCTtype=INTARR(3)
              FOR nn=0, N_TAGS(headerStruct)-1 DO nColsCTtype(nn)=N_ELEMENTS(headerStruct.(nn))

              FOR nn=0, nAdr-1 DO BEGIN
                IF adrTempTemp(nn) NE '' THEN BEGIN
                  WIDGET_CONTROL, lblProgress, SET_VALUE='Getting data from file '+STRING(nn+1, FORMAT='(i0)')+'/'+STRING(nAdr, FORMAT='(i0)')
                  IF nn GT 0 THEN WAIT, waitSec(1);give time to close Excel and Acrobat reader avoiding writingprotection error
                  pdfContent=PDF2clipboard(adrTempTemp(nn), thisPath, waitSec); PDF2clipboard defined at top of this file
                  IF pdfContent(0) NE '' THEN BEGIN
                    resu=readCTconstancy(pdfContent, configGetSiemensQC)
                    resLine=writeAutoTxtRes(resu.strArrRes, deciMarkDefault); writeAutoTxtRes defined at top of this file
                    nCols(nn)=N_ELEMENTS(resu.strArrRes)
                    errArr(nn)=STRJOIN(resu.errMsg,newline)
                    IF thisTemp.statName EQ '' THEN errArr(nn)='CT serial number not defined in template. Please verify results.'
                    IF resu.strArrRes(3) NE thisTemp.statName AND thisTemp.statName NE '' THEN errArr(nn)='CT serial number do not fit what is defined in template. This file will be left as is.' ELSE BEGIN
                      idHead=WHERE(nColsCTtype EQ nCols(nn))
                      IF idHead(0) NE -1 THEN headLine=STRJOIN(headerStruct.(idHead(0)),STRING(9B)) + newline ELSE headLine=!Null
                      writeResToFile, headLine, resLine, thisTemp.pathApp, adrTempTemp(nn), pickfiles, errArr, nn, nAdr, archivePaths, nnWritten, evTop; writeResToFile defined at top of this file
                    ENDELSE
                  ENDIF ELSE BEGIN
                    errArr(nn)='Failed to read PDF content to clipboard.'
                    IF nn LT nAdr-1 THEN BEGIN
                      sv=DIALOG_MESSAGE(adrTempTemp(nn)+newline+PDF_failed_txt+newline+newline+' Continue reading rest of files?',/QUESTION, DIALOG_PARENT=evTop)
                      IF sv EQ 'No' THEN nn=nAdr
                    ENDIF
                  ENDELSE
                ENDIF
              ENDFOR
              WIDGET_CONTROL, lblProgress, SET_VALUE=''

            ENDIF
          END
          'PDF_SiemensPETdailyQC':BEGIN
            WIDGET_CONTROL, /HOURGLASS
            configGetSiemensQC=updConfigGetSiemensQC()
            IF adrTempTemp(0) NE '' THEN BEGIN
              adrTempTemp=sortFilesByDate(adrTempTemp);sortFilesByDate defined at top of this file

              headersPET=configGetSiemensQC.headers.(0)
              headLine=STRJOIN(headersPET,STRING(9B)) + newline

              FOR nn=0, nAdr-1 DO BEGIN
                IF adrTempTemp(nn) NE '' THEN BEGIN
                  WIDGET_CONTROL, lblProgress, SET_VALUE='Getting data from file '+STRING(nn+1, FORMAT='(i0)')+'/'+STRING(nAdr, FORMAT='(i0)')
                  IF nn GT 0 THEN WAIT, waitSec(1);give time to close Excel and Acrobat reader avoiding writingprotection error
                  pdfContent=PDF2clipboard(adrTempTemp(nn), thisPath, waitSec); PDF2clipboard defined at top of this file
                  IF pdfContent(0) NE '' THEN BEGIN
                    resu=readPETdailyQC(pdfContent, configGetSiemensQC)
                    resLine=writeAutoTxtRes(resu.strArrRes, deciMarkDefault); writeAutoTxtRes defined at top of this file
                    errArr(nn)=STRJOIN(resu.errMsg,newline)
                    IF thisTemp.statName EQ '' THEN errArr(nn)='CT serial number not defined in template. Please verify results.'
                    IF resu.strArrRes(1) NE thisTemp.statName AND thisTemp.statName NE '' THEN errArr(nn)='ICS name do not fit what is defined in template. This file will be left as is.' ELSE BEGIN
                      writeResToFile, headLine, resLine, thisTemp.pathApp, adrTempTemp(nn), pickfiles, errArr, nn, nAdr, archivePaths, nnWritten, evTop; writeResToFile defined at top of this file
                    ENDELSE
                  ENDIF ELSE BEGIN
                    errArr(nn)='Failed to read PDF content to clipboard.'
                    IF nn LT nAdr-1 THEN BEGIN
                      sv=DIALOG_MESSAGE(adrTempTemp(nn)+newline+PDF_failed_txt+newline+newline+' Continue reading rest of files?',/QUESTION, DIALOG_PARENT=evTop)
                      IF sv EQ 'No' THEN nn=nAdr
                    ENDIF
                  ENDELSE
                ENDIF
              ENDFOR
              WIDGET_CONTROL, lblProgress, SET_VALUE=''

            ENDIF
          END
          'XML_SiemensPETdailyQC':BEGIN
            WIDGET_CONTROL, /HOURGLASS
            IF adrTempTemp(0) NE '' THEN BEGIN
              adrTempTemp=sortFilesByDate(adrTempTemp);sortFilesByDate defined at top of this file
              heads=['Date','CalibrationFactor','MeasuredRandoms','ScannerEfficiency','ScatterRatio','ECF']
              headLine=STRJOIN(heads,STRING(9B)) + newline

              FOR nn=0, nAdr-1 DO BEGIN
                IF adrTempTemp(nn) NE '' THEN BEGIN
                  WIDGET_CONTROL, lblProgress, SET_VALUE='Getting data from file '+STRING(nn+1, FORMAT='(i0)')+'/'+STRING(nAdr, FORMAT='(i0)')
                  resu=readPETdailyQC_xml(adrTempTemp(nn))
                  resLine=writeAutoTxtRes(resu.strArrRes, deciMarkDefault); writeAutoTxtRes defined at top of this file
                  errArr(nn)=STRJOIN(resu.errMsg,newline)
                  writeResToFile, headLine, resLine, thisTemp.pathApp, adrTempTemp(nn), pickfiles, errArr, nn, nAdr, archivePaths, nnWritten, evTop; writeResToFile defined at top of this file
                ENDIF
              ENDFOR
              WIDGET_CONTROL, lblProgress, SET_VALUE=''

            ENDIF
          END

          'PDF_MR':BEGIN
            WIDGET_CONTROL, /HOURGLASS
            IF adrTempTemp(0) NE '' THEN BEGIN
              adrTempTemp=sortFilesByDate(adrTempTemp);sortFilesByDate defined at top of this file

              FOR nn=0, nAdr-1 DO BEGIN
                IF adrTempTemp(nn) NE '' THEN BEGIN
                  WIDGET_CONTROL, lblProgress, SET_VALUE='Getting data from file '+STRING(nn+1, FORMAT='(i0)')+'/'+STRING(nAdr, FORMAT='(i0)')
                  IF nn GT 0 THEN WAIT, waitSec(1);give time to close Excel and Acrobat reader avoiding writingprotection error
                  pdfContent=PDF2clipboard(adrTempTemp(nn), thisPath, waitSec); PDF2clipboard defined at top of this file
                  IF pdfContent(0) NE '' THEN BEGIN
                    resu=readMR_PDF(pdfContent)
                    resLine=writeAutoTxtRes(resu.strArrRes, deciMarkDefault); writeAutoTxtRes defined at top of this file
                    errArr(nn)=STRJOIN(resu.errMsg,newline)
                    headLine=STRJOIN(resu.headers,STRING(9B)) + newline
                    writeResToFile, headLine, resLine, thisTemp.pathApp, adrTempTemp(nn), pickfiles, errArr, nn, nAdr, archivePaths, nnWritten, evTop; writeResToFile defined at top of this file
                  ENDIF ELSE BEGIN
                    errArr(nn)='Failed to read PDF content to clipboard.'
                    IF nn LT nAdr-1 THEN BEGIN
                      sv=DIALOG_MESSAGE(adrTempTemp(nn)+newline+PDF_failed_txt+newline+newline+' Continue reading rest of files?',/QUESTION, DIALOG_PARENT=evTop)
                      IF sv EQ 'No' THEN nn=nAdr
                    ENDIF
                  ENDELSE
                ENDIF
              ENDFOR
              WIDGET_CONTROL, lblProgress, SET_VALUE=''

            ENDIF
          END
          ;########################################### GE QAP ################################################33
          'GE_QAP': BEGIN
            msgThis=''
            WIDGET_CONTROL, /HOURGLASS
            readQAPreport, adrTempTemp, thisTemp.pathApp, msgThis, deciMarkDefault

            CASE msgThis OF
              'clipboard': BEGIN
                sv=DIALOG_MESSAGE('Output file not defined or no writing permission. Paste current results for '+tempname+' from clipboard and then press OK to continue.',/INFORMATION, DIALOG_PARENT=evTop)
                IF loop2 NE 0 THEN testLog=testLog+newline+tab+tab+'Output file not defined or no writing permissions.'
              END
              'written': BEGIN;archive
                IF loop2 NE 0 THEN testLog=testLog+newline+tab+tab+'Results written to file.'
                archiveOK=1
                IF pickfiles THEN BEGIN
                  sv=DIALOG_MESSAGE('You have selected specific files to analyse. Move files to archive?',/QUESTION, DIALOG_PARENT=evTop)
                  IF sv EQ 'No' THEN archiveOK=0
                ENDIF
                IF archiveOK THEN BEGIN
                  archivePath=thisTemp.path+'\Archive\'
                  fi=FILE_INFO(archivePath)
                  IF fi.exists EQ 0 THEN FILE_MKDIR, archivePath
                  FOR i=0, N_ELEMENTS(adrTempTemp)-1 DO BEGIN
                    adrSplit=STRSPLIT(adrTempTemp(i), PATH_SEP(), /EXTRACT)

                    fif=FILE_INFO(archivePath+adrSplit(-2))
                    IF fif.exists EQ 0 THEN FILE_MKDIR, archivePath+adrSplit(-2)

                    newFiName=archivePath+adrSplit(-2)+PATH_SEP()+adrSplit(-1)
                    fi=FILE_INFO(newFiName)
                    IF fi.exists EQ 0 THEN file_move, adrTempTemp(i), newFiName
                  ENDFOR
                  testLog=testLog+newline+ tab+tab+'Moved files to Archive.'
                ENDIF
              END;written
              ELSE:
            ENDCASE

            archivePaths=!Null; to avoid re-Archive in code below

          END; GE_QAP
          ELSE:
        ENDCASE;alternative

        ids=WHERE(errArr NE '')
        IF ids(0) NE -1 THEN BEGIN
          FOR nn=0, N_ELEMENTS(ids)-1 DO BEGIN
            testLog=testLog+newline+ tab+tab+FILE_BASENAME(adrTempTemp(ids(nn)))
            testLog=testLog+newline+ tab+tab+tab+errArr(ids(nn))
          ENDFOR
        ENDIF

        IF nnWritten GT 0 THEN BEGIN
          testLog=testLog+newline+ tab+tab+'Results written to file.'
          nF=archiveFiles(archivePaths, thisTemp.path); function archiveFiles  defined at top of this file
          IF nF EQ 0 THEN testLog=testLog+newline+ tab+tab+'Moved files to Archive.' ELSE BEGIN
            IF nF EQ N_ELEMENTS(archivePaths) THEN testLog=testLog+newline+ tab+tab+'Failed to move files to Archive.' ELSE testLog=testLog+newline+ tab+tab+'Failed to move some of the files to Archive.'
          ENDELSE
        ENDIF

        IF N_ELEMENTS(loop) NE 0 THEN BEGIN
          IF autopause THEN BEGIN
            IF loop2 EQ 1 THEN BEGIN
              box=[$
                '1, BASE,, /ROW', $
                '2, LABEL, ', $
                '1, BASE,, /ROW', $
                '0, BUTTON, Continue to next template?, QUIT, TAG=Cont',$
                '2, BUTTON, Stop, QUIT, TAG=Cancel']
              res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Continue?', XSIZE=300, YSIZE=100, FOCUSNO=3, XOFFSET=xoffset+200, YOFFSET=yoffset+200)
              IF res.Cancel THEN loop2=0
            ENDIF
          ENDIF
        ENDIF

      ENDELSE

    ENDIF ELSE testLog=testLog+newline+tab+'Found no files in selected folder. (NB special characteres in file-path might not work well.)';adrTempTemp empty
  ENDIF ELSE testLog=testLog+newline+tab+'No path defined in template. Analysis not possible.';path empty + pickfiles none

  loop=loop2
  return
end