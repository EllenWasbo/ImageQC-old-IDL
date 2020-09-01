;ImageQC - quality control of medical images
;Copyright (C) 2020  Ellen Wasbo, Stavanger University Hospital, Norway
;ellen@wasbo.no
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License version 2
;as published by the Free Software Foundation
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

pro autoOpen, GROUP_LEADER = mainbase, xoff, yoff

  COMMON AUTOVAR, listAuto, modArr,tempnames,tempIDarr,statnames,loadAdr,lblAutoProgress
  COMMON VARI
  COMPILE_OPT hidden

  tempnames=!Null
  modArr=!Null
  tempIDarr=!Null
  statNames=!Null
  loadAdr=!Null

  autobox = WIDGET_BASE(TITLE='Open files with automation template',  $
    /COLUMN, XSIZE=430, YSIZE=460, XOFFSET=xoff, YOFFSET=yoff,GROUP_LEADER=mainbase, /TLB_KILL_REQUEST_EVENTS, /MODAL)

  bAutoToolbar=WIDGET_BASE(autobox,/ROW,/TOOLBAR)
  btnRefresh=WIDGET_BUTTON(bAutoToolbar, VALUE=thisPath+'images\refresh.bmp',/BITMAP, UVALUE='au_refresh', TOOLTIP='Refresh to count new files')
  btnImport=WIDGET_BUTTON(bAutoToolbar, VALUE=thisPath+'images\importf.bmp',/BITMAP, UVALUE='au_import', TOOLTIP='Sort images from select folder to paths defined in templates based on station-name from DICOM header. The files will be renamed to better describe the content.')
  btnSettings=WIDGET_BUTTON(bAutoToolbar, VALUE=thisPath+'images\gears.bmp',/BITMAP, UVALUE='au_settings', TOOLTIP='Go to settings to edit the automation templates.')
  btnSettings=WIDGET_BUTTON(bAutoToolbar, VALUE=thisPath+'images\document.bmp',/BITMAP, UVALUE='au_openRes', TOOLTIP='Open the results file for the selected template.')

  lblMLProgress=WIDGET_LABEL(bAutoToolbar, VALUE=' ', XSIZE=20, FONT=font1)
  lblAutoProgress=WIDGET_LABEL(bAutoToolbar, VALUE=' ', XSIZE=280, FONT=font1, /DYNAMIC_RESIZE)
  btnPutAllInOne=WIDGET_BUTTON(bAutoToolbar, VALUE=thisPath+'images\undo.bmp',/BITMAP,UVALUE='putAllinOne',TOOLTIP='Move files out of the Archive to re-run the analysis', FONT=font1)


  mlTop=WIDGET_BASE(autobox,YSIZE=10)

  lblAuto2=WIDGET_LABEL(autobox, VALUE='Refresh to show number of files waiting (number of files not in Archive yet).', FONT=font1, /ALIGN_LEFT)

  mlTop2=WIDGET_BASE(autobox,YSIZE=20)

  bAutoMid=WIDGET_BASE(autobox, /ROW)
  bAutoLft=WIDGET_BASE(bAutoMid, /COLUMN)
  lblAuto=WIDGET_LABEL(bAutoLft, VALUE='Automation templates', FONT=font0, /ALIGN_LEFT)
  listAuto=WIDGET_LIST(bAutoLft, VALUE='', XSIZE=20, MULTIPLE=1, FONT=font1, SCR_XSIZE=200, SCR_YSIZE=300)

  lblMlMid=WIDGET_LABEL(bAutoLft, VALUE='', YSIZE=10)
  bAutoRgt=WIDGET_BASE(bAutoMid, /COLUMN)

  bRgtButt=WIDGET_BASE(bAutoRgt, /COLUMN)
  mlRowButt=WIDGET_LABEL(bRgtButt, VALUE='', XSIZE=200)
  btnRunSelected=WIDGET_BUTTON(bRgtButt, VALUE='Run selected',UVALUE='autoRunSel',TOOLTIP='Run selected templates', FONT=font1)
  btnRunAll=WIDGET_BUTTON(bRgtButt, VALUE='Run All',UVALUE='autoRunAll', TOOLTIP='Run all templates',FONT=font1)
  btnRunPickFiles=WIDGET_BUTTON(bRgtButt, VALUE='Run for selected files...', UVALUE='autoRunPicked',TOOLTIP='Run selected template on specified files',  FONT=font1)

  bBtmButt=WIDGET_BASE(autobox, /ROW)
  mlRowButt=WIDGET_LABEL(bBtmButt, VALUE='', XSIZE=350)
  btnAutoCancel=WIDGET_BUTTON(bBtmButt, VALUE='Cancel', UVALUE='autoCancel', FONT=font1)

  upd_AutoList,0

  WIDGET_CONTROL, autobox, /REALIZE
  XMANAGER, 'autoOpen', autobox

end

pro autoOpen_event, event

  COMMON AUTOVAR
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  proceed=0

  IF N_ELEMENTS(uval) GT 0 AND SIZE(uval, /TNAME) EQ 'STRING' THEN BEGIN

    CASE uval OF
      'autoCancel': WIDGET_CONTROL, Event.top, /DESTROY
      'autoRunSel': IF N_ELEMENTS(tempNames) GT 0 THEN proceed=1 ELSE sv=DIALOG_MESSAGE('No template defined yet. Go to settings to define a template.', /INFORMATION, DIALOG_PARENT=evTop)
      'autoRunAll': IF N_ELEMENTS(tempNames) GT 0 THEN proceed=2 ELSE sv=DIALOG_MESSAGE('No template defined yet. Go to settings to define a template.', /INFORMATION, DIALOG_PARENT=evTop)
      'autoRunPicked': BEGIN
        IF N_ELEMENTS(tempNames) GT 0 THEN BEGIN
          selTemp=WIDGET_INFO(listAuto, /LIST_SELECT)
          IF N_ELEMENTS(selTemp) NE 1 THEN BEGIN
            sv=DIALOG_MESSAGE('Select one template (only).', /INFORMATION, DIALOG_PARENT=evTop)
          ENDIF ELSE proceed=3
        ENDIF ELSE sv=DIALOG_MESSAGE('No template defined yet. Go to settings to define a template.', /INFORMATION, DIALOG_PARENT=evTop)
      END
      'au_refresh': upd_AutoList,1
      'au_import': BEGIN
        RESTORE, thisPath+'data\config.dat'

        IF N_ELEMENTS(loadTemp) NE 0 AND N_ELEMENTS(statnames) EQ 0 THEN BEGIN
          modnames=TAG_NAMES(multiOpt)
          FOR m=0, N_TAGS(multiOpt)-1 DO BEGIN
            IF SIZE(loadTemp.(m), /TNAME) EQ 'STRUCT' THEN BEGIN
              namesThis=TAG_NAMES(loadTemp.(m))
              FOR i=0, N_ELEMENTS(namesThis)-1 DO BEGIN
                statNames=[statNames, loadTemp.(m).(i).statName]
                loadAdr=[loadAdr, loadTemp.(m).(i).path]
              ENDFOR
            ENDIF
          ENDFOR
        ENDIF

        defImpPath=configS.(1).AUTOIMPORTPATH
        adr=DIALOG_PICKFILE(TITLE='Select folder with images to be moved to template-defined paths', DIALOG_PARENT=event.Top, /DIRECTORY, PATH=defImpPath)
        IF adr NE '' THEN BEGIN
          Spawn, 'dir '  + '"'+adr(0)+'"* /b /s /a-D', adrTempTemp
          IF adrTempTemp(0) NE '' THEN BEGIN
            WIDGET_CONTROL, /HOURGLASS
            nn=N_ELEMENTS(adrTempTemp)
            dcm=INTARR(nn)
            
            sv=DIALOG_MESSAGE('Found '+STRING(nn, FORMAT='(i0)')+' files in the selected folder. Continue to rename and move DICOM files?', /QUESTION, DIALOG_PARENT=event.Top)
            IF sv EQ 'Yes' THEN BEGIN
              WIDGET_CONTROL, /HOURGLASS
              countNoTemp=0
              errF=0
              nAlr=0
              renames=''
              nNoImg=0
              statNameFound=!Null
              FOR n=0, nn-1 DO BEGIN
                IF FILE_BASENAME(adrTempTemp(n)) EQ 'DICOMDIR' THEN dcm(n)=0 ELSE dcm(n)=QUERY_DICOM(adrTempTemp(n))
                IF dcm(n) NE 0 THEN BEGIN
                  WIDGET_CONTROL, lblAutoProgress, SET_VALUE='Moving and renaming file '+STRING(n, FORMAT='(i0)')+' / '+STRING(nn, FORMAT='(i0)')
                  o=obj_new('idlffdicom')
                  t=o->read(adrTempTemp(n))

                  test=o->GetReference('0008'x,'0060'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN modalityStr=*(test_peker[0]) ELSE modalityStr='?'

                  test=o->GetReference('0008'x,'0023'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN contentDate=*(test_peker[0]) ELSE contentDate=''
                  
                  test=o->GetReference('0008'x,'0033'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN contentTime=STRING(*(test_peker[0]), FORMAT='(a6)') ELSE contentTime=''

                  test=o->GetReference('0008'x,'103E'x);series description
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN serDesc=*(test_peker[0]) ELSE serDesc='unknown'

                  ;filename: statname_PatID_date_time_protocol
                  test=o->GetReference('0008'x,'1010'x);
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN stationName=*(test_peker[0]) ELSE stationName='noStationName'

                  test=o->GetReference('0010'x,'0020'x);
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN patid=*(test_peker[0]) ELSE patid='noPatID'

                  test=o->GetReference('0008'x,'0022'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN acqDate=*(test_peker[0]) ELSE acqDate=''

                  test=o->GetReference('0008'x,'0032'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN acqTime=*(test_peker[0]) ELSE acqTime=''
                  IF acqTime NE '' THEN acqTime=acqTime.substring(0,5)

                  test=o->GetReference('0018'x,'1030'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN BEGIN
                    protocolName=*(test_peker[0])
                  ENDIF ELSE BEGIN
                    test=o->GetReference('0008'x,'1030'x);study description
                    test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                    IF test(0) NE -1 THEN protocolName=*(test_peker[0]) ELSE protocolName='noProtName'
                  ENDELSE

                  test=o->GetReference('0020'x,'0011'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN seriesNmb=STRING(*(test_peker[0]), FORMAT='(i0)') ELSE seriesNmb='noSerNmb'

                  test=o->GetReference('0020'x,'0013'x)
                  test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
                  IF test(0) NE -1 THEN imgNo=STRING(*(test_peker[0]), FORMAT='(i0)') ELSE imgNo='noImgNmb'

                  IF acqdate NE '' AND acqtime NE '' AND modalityStr NE 'SR' THEN BEGIN
                    basename=stationName+'_'+patid+'_'
                    basename=basename+acqDate+'_'+acqTime+'_'+protocolName+'_'+seriesNmb+'_'+imgNo
                    basename=IDL_VALIDNAME(basename.compress(),/CONVERT_ALL)

                    IF N_ELEMENTS(statNames) GT 0 THEN tempID=WHERE(stationName EQ statNames) ELSE tempID=-1

                    IF tempID(0) NE -1 THEN BEGIN
                      IF loadAdr(tempID(0)) NE '' THEN BEGIN
                        newAdr=loadAdr(tempID(0))
                        statNameFound=[statNameFound, stationName]
                      ENDIF ELSE BEGIN
                        newAdr=adr(0)
                        countNoTemp=countNoTemp+1
                      ENDELSE
                    ENDIF ELSE BEGIN
                      newAdr=adr(0)
                      countNoTemp=countNoTemp+1
                    ENDELSE
                    ;ensure path ending with separator
                    IF STRMID(newAdr(0), 0, /REVERSE_OFFSET) NE PATH_SEP() THEN newAdr=newAdr(0)+PATH_SEP()

                  ENDIF ELSE BEGIN
                    nNoImg=nNoImg+1; acqdate and acqtime not empty (not regarded as image)

                    basename=stationName+'_'+patid+'_'+contentDate+'_'+contentTime+'_'+serDesc
                    newAdr=adr(0);do not move, just rename
                  ENDELSE

                  IF adrTempTemp(n) NE newAdr(0)+basename+'.dcm' THEN BEGIN;already renamed like this
                    resu=FILE_TEST(newAdr(0),/DIRECTORY)
                    IF resu THEN BEGIN
                      IF renames.HasValue(newAdr(0)+basename) THEN BEGIN
                        wh=WHERE(renames EQ newAdr(0)+basename, nNames)
                        wh=WHERE(renames EQ newAdr(0)+STRMID(basename,0,STRLEN(basename)-3), nExtra)
                        nNames=nNames+nExtra
                        basenameN=basename+'_'+STRING(nNames, FORMAT='(i02)')
                      ENDIF ELSE basenameN=basename
                      IF  adrTempTemp(n) NE newAdr(0)+basenameN+'.dcm' THEN BEGIN
                        fiexist=FILE_TEST(newAdr(0)+basenameN+'.dcm')
                        IF fiexist THEN BEGIN
                          IF basenameN NE basename THEN file_move, adrTempTemp(n), adr(0)+basenameN+'.dcm'
                          nAlr=nAlr+1
                        ENDIF ELSE BEGIN
                          file_move, adrTempTemp(n), newAdr(0)+basenameN+'.dcm'
                          renames=[renames,newAdr(0)+basename]
                        ENDELSE
                      ENDIF
                    ENDIF ELSE errF=errF+1
                  ENDIF
                ENDIF; dcm?
              ENDFOR; n files found

              IF TOTAL(dcm) EQ 0 THEN BEGIN
                sv=DIALOG_MESSAGE('Found no valid DICOM files.', DIALOG_PARENT=event.Top)
              ENDIF ELSE BEGIN
                ;delete empty folders?
                sv=DIALOG_MESSAGE('Delete empty folders?',/QUESTION, DIALOG_PARENT=event.Top)
                IF sv EQ 'Yes' THEN BEGIN
                  WIDGET_CONTROL, /HOURGLASS
                  Spawn, 'dir '  + '"'+adr(0)+'"' + '*'+ '/b /s /aD', dirNames
                  
                  IF dirNames(0) NE '' THEN BEGIN
                    ;sort longest path first
                    strLenArr=!Null
                    FOR i=0, N_ELEMENTS(dirNames)-1 DO strLenArr=[strLenArr, STRLEN(dirNames(i))]
                    sortOrder=REVERSE(SORT(strLenArr))
                    dirNames=dirNames(sortOrder)
                    FOR i=0, N_ELEMENTS(dirNames)-1 DO BEGIN
                      fi=FILE_INFO(dirNames(i))
                      IF fi.size EQ 0 THEN BEGIN
                        WIDGET_CONTROL, lblAutoProgress, SET_VALUE='Deleting empty subfolder '+STRING(i+1,FORMAT='(i0)')
                        FILE_DELETE, dirNames(i), /QUIET
                      ENDIF
                    ENDFOR
                  ENDIF
                ENDIF
                
                IF N_ELEMENTS(statNameFound) GT 0 THEN BEGIN
                  allNames=statNameFound(UNIQ(statNameFound))
                  txtRes='Found and moved these images:'+newline
                  FOR n=0, N_ELEMENTS(allNames)-1 DO BEGIN
                    ss=WHERE(allNames(n) EQ statNameFound, nn)
                    txtRes=txtRes+'   '+allNames(n)+' ('+STRING(nn, FORMAT='(i0)')+')'+newline         
                  ENDFOR
                  sv=DIALOG_MESSAGE(txtRes, DIALOG_PARENT=event.Top)
                ENDIF
  
                IF errF THEN  sv = DIALOG_MESSAGE(STRING(errF, FORMAT='(i0)')+ ' file(s) not moved as the path defined in template for the specified station name could not be reached.', DIALOG_PARENT=event.Top)
  
                IF countNoTemp GT 0 THEN BEGIN
                  sv = DIALOG_MESSAGE(STRING(countNoTemp, FORMAT='(i0)')+ ' file(s) not moved, just renamed and placed directly in the selected folder. No corresponding automation template for the staion name or target path not defined.', DIALOG_PARENT=event.Top)
                ENDIF
                
                IF nAlr GT 0 THEN BEGIN
                  sv = DIALOG_MESSAGE(STRING(nAlr, FORMAT='(i0)')+ ' file(s) not moved, just renamed and placed directly in the selected folder. File with same filename already exist in target folder.', DIALOG_PARENT=event.Top)
                ENDIF
  
                IF nNoImg GT 0 THEN BEGIN
                  sv = DIALOG_MESSAGE(STRING(nNoImg, FORMAT='(i0)')+ ' file(s) had modality SR or no acquisition date/time. These are not regarded as image files and are left in selected folder with name (station-name_PatientID_seriesDescription).', DIALOG_PARENT=event.Top)
                ENDIF
              ENDELSE
            ENDIF; Yes - continue
            WIDGET_CONTROL, lblAutoProgress, SET_VALUE=''
          ENDIF ELSE sv=DIALOG_MESSAGE('Found no files in the selected folder.', DIALOG_PARENT=event.Top)

        ENDIf;adr=''
      END
      'au_settings': BEGIN
        WIDGET_CONTROL, Event.top, /DESTROY
        settings, GROUP_LEADER=evTop, xoffset+100, yoffset+100, 'AUTOSETUP'
      END
      'au_openRes':BEGIN
        RESTORE, thisPath+'data\config.dat'
        IF N_ELEMENTS(loadTemp) GT 0 THEN BEGIN
          selTemp=WIDGET_INFO(listAuto, /LIST_SELECT)
          IF N_ELEMENTS(selTemp) GT 1 THEN sv=DIALOG_MESSAGE('Select only one template.', /INFORMATION, DIALOG_PARENT=event.Top) ELSE BEGIN
            resAdr=loadTemp.(modArr(selTemp(0))).(tempIDarr(selTemp(0))).pathApp
            IF resAdr EQ '' THEN sv=DIALOG_MESSAGE('No result file specified for the selected template.', /INFORMATION, DIALOG_PARENT=event.Top) ELSE BEGIN
              IF FILE_TEST(resAdr) THEN BEGIN
                Case !version.os_family of
                  'Windows': SPAWN, 'notepad.exe '+resAdr, /NOSHELL, /NOWAIT
                  Else: if (!version.os_name eq 'Mac OS X') then SPAWN, 'open '+resAdr
                Endcase
              ENDIF ELSE sv=DIALOG_MESSAGE('File not found '+resAdr, /INFORMATION, DIALOG_PARENT=event.Top)
            ENDELSE
          ENDELSE
        ENDIF ELSE sv=DIALOG_MESSAGE('No template defined yet. Go to settings to define a template.', /INFORMATION, DIALOG_PARENT=evTop)
      END
      'putAllinOne':BEGIN
        IF N_ELEMENTS(tempNames) GT 0 THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          selTemp=WIDGET_INFO(listAuto, /LIST_SELECT)
          notMoved=0
          moved=0
          FOR t = 0, N_ELEMENTS(selTemp)-1 DO BEGIN

            adr=loadTemp.(modArr(selTemp(t))).(tempIDarr(selTemp(t))).path
            adrArc=adr+'Archive\'
            sv=DIALOG_MESSAGE('Move files in '+adrArc+' to parent folder?', /QUESTION, DIALOG_PARENT=evTop)
            IF sv EQ 'Yes' THEN BEGIN
              WIDGET_CONTROL, /HOURGLASS
              WIDGET_CONTROL, lblAutoProgress, SET_VALUE='Searching for files in Archive'
              Spawn, 'dir '  + '"'+adrArc+'"' + '*'+ '/b /s /a-D', res
              IF res(0) NE '' THEN BEGIN
                origPaths=res
                Spawn, 'dir '  + '"'+adrArc+'"' + '*'+ '/b /s /aD', res
                IF res(0) NE '' THEN dirNames=res ELSE dirNames=!Null

                nFiles=N_ELEMENTS(origPaths)
                IF nFiles NE 0 THEN BEGIN
                  newPaths=origPaths & newPaths[*]=''
                  ;dateArr=!Null
                  FOR i=0, nFiles-1 DO BEGIN
                    pathSplit=STRSPLIT(origPaths(i),'\',/EXTRACT)
                    IF pathSplit(-2) NE 'Archive' THEN BEGIN; not directly under Archive - prefix filename = subfoldername
                      ;already same prefix?
                      IF pathSplit(-2) EQ STRMID(FILE_BASENAME(origPaths(i)),0,STRLEN(pathSplit(-2))) THEN pref='' ELSE pref=pathSplit(-2)+'_'
                    ENDIF ELSE pref=''
                    newName=adr(0)+ pref +FILE_BASENAME(origPaths(i))
                    alr=WHERE(newPaths EQ newName, nEq)
                    IF alr(0) NE -1 THEN newName=STRMID(newName,0,STRLEN(newName)-4)+'_'+STRING(nEq,FORMAT='(i3)')+'.dcm'
                    newPaths(i)=newName
                    ;dateArr=[dateArr,FILE_MODTIME(origPaths(i))]
                  ENDFOR

                  strNfiles='/'+STRING(nFiles,FORMAT='(i0)')
                  FOR i=0, nFiles-1 DO BEGIN
                    IF origPaths(i) NE '' THEN BEGIN
                      fi=FILE_INFO(newPaths(i))
                      IF fi.exists THEN notMoved=notMoved+1 ELSE BEGIN
                        file_move, origPaths(i), newPaths(i)
                        moved=moved+1
                        WIDGET_CONTROL, lblAutoProgress, SET_VALUE='Moving file '+STRING(i+1,FORMAT='(i0)')+strNfiles
                      ENDELSE
                    ENDIF
                  ENDFOR
                  WIDGET_CONTROL, lblAutoProgress, SET_VALUE=''

                  ;if folder empty - delete
                  strNfolders='/'+STRING(N_ELEMENTS(dirNames),FORMAT='(i0)')
                  IF N_ELEMENTS(dirNames) NE 0 THEN BEGIN
                    FOR i=0, N_ELEMENTS(dirNames)-1 DO BEGIN
                      fi=FILE_INFO(dirNames(i))
                      IF fi.size EQ 0 THEN BEGIN
                        WIDGET_CONTROL, lblAutoProgress, SET_VALUE='Deleting empty subfolder '+STRING(i+1,FORMAT='(i0)')+strNfolders
                        FILE_DELETE, dirNames(i)
                      ENDIF
                    ENDFOR
                  ENDIF
                  WIDGET_CONTROL, lblAutoProgress, SET_VALUE=''
                ENDIF
              ENDIF ELSE sv=DIALOG_MESSAGE('Found no files in the subfolderers of the Archive in '+adr(0), /INFORMATION, DIALOG_PARENT=evTop)
            ENDIF
          ENDFOR
          IF notMoved THEN sv=DIALOG_MESSAGE(STRING(notMoved, FORMAT='(i0)')+' files were not moved as there was a conflict with the filename already existing.', /INFORMATION, DIALOG_PARENT=evTop)
          sv=DIALOG_MESSAGE(STRING(moved, FORMAT='(i0)')+' files from Archive are now moved to the parent folder.', /INFORMATION, DIALOG_PARENT=evTop)
        ENDIF ELSE sv=DIALOG_MESSAGE('No template defined yet. Go to settings to define a template.', /INFORMATION, DIALOG_PARENT=evTop)
      END
      ELSE:
    ENDCASE

  ENDIF

  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN WIDGET_CONTROL, event.top, /DESTROY

  IF proceed GT 0 THEN BEGIN
    autoStopFlag=0
    selTemp=WIDGET_INFO(listAuto, /LIST_SELECT)
    WIDGET_CONTROL, event.top, /DESTROY
    WIDGET_CONTROL, /HOURGLASS
    RESTORE, thisPath+'data\config.dat'
    ;tempIDnotFoundImg=!Null
    testLog=''
    CASE proceed of
      1: BEGIN; run selected
        loop=1
        FOR i=0, N_ELEMENTS(selTemp)-1 DO BEGIN
          thisTempName=tempnames(selTemp(i))
          IF i EQ N_ELEMENTS(selTemp)-1 THEN BEGIN; last selected
            loop=-1
            autoTempRun, loadTemp.(modArr(selTemp(i))).(tempIDarr(selTemp(i))), modArr(selTemp(i)), LOOP=loop, TEMPNAME=thisTempName, TESTLOG=testLog
          ENDIF ELSE autoTempRun, loadTemp.(modArr(selTemp(i))).(tempIDarr(selTemp(i))), modArr(selTemp(i)), LOOP=loop, TEMPNAME=thisTempName, TESTLOG=testLog

          If loop NE 1 THEN break
        ENDFOR
      END
      2: BEGIN;run all
        loop=1
        FOR i=0, N_ELEMENTS(tempnames)-1 DO BEGIN
          thisTempName=tempnames(i)
          IF i EQ N_ELEMENTS(tempnames)-1 THEN BEGIN
            loop=-1
            autoTempRun, loadTemp.(modArr(i)).(tempIDarr(i)), modArr(i), LOOP=loop, TEMPNAME=thisTempName, TESTLOG=testLog
          ENDIF ELSE autoTempRun, loadTemp.(modArr(i)).(tempIDarr(i)), modArr(i), LOOP=loop, TEMPNAME=thisTempName, TESTLOG=testLog

          If loop NE 1 THEN break
        ENDFOR
      END
      3: BEGIN
        thisTempName=tempnames(selTemp(0))
        autoTempRun, loadTemp.(modArr(selTemp(0))).(tempIDarr(selTemp(0))), modArr(selTemp(0)), PICKFILES=1, TEMPNAME=thisTempName, TESTLOG=testLog
      END

      ELSE:
    ENDCASE
    
    IF testLog NE '' THEN BEGIN
      OPENW, logfile, thisPath+'data\autoLog.txt', /GET_LUN
      PRINTF, logfile, testLog
      CLOSE, logfile & FREE_LUN, logfile
      XDISPLAYFILE, thisPath+'data\autoLog.txt', TITLE='Analyse log', /MODAL, DONE_BUTTON='Close', WIDTH=100, GROUP=evtop
    ENDIF
  ENDIF

end

pro upd_AutoList, countFiles

  COMMON AUTOVAR
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, /HOURGLASS
  RESTORE, thisPath+'data\config.dat'

  tempnames=!Null
  modArr=!Null
  tempIDarr=!Null
  statNames=!Null
  loadAdr=!Null

  IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
    modnames=TAG_NAMES(multiOpt)
    errLogg=''
    breakFlag=0
    FOR m=0, N_TAGS(multiOpt)-1 DO BEGIN
      IF SIZE(loadTemp.(m), /TNAME) EQ 'STRUCT' THEN BEGIN
        namesThis=modNames(m)+' / '+ TAG_NAMES(loadTemp.(m))
        IF countFiles THEN BEGIN
          WIDGET_CONTROL, lblAutoProgress, SET_VALUE='Searching for new files in ' + modNames(m)
          ;search for new files (outside Archive)
          FOR i=0, N_ELEMENTS(namesThis)-1 DO BEGIN
            statNames=[statNames, loadTemp.(m).(i).statName]
            adr=loadTemp.(m).(i).path
            loadAdr=[loadAdr, adr]
            adrTempTemp=''
            ;ensure path ending with separator
            IF adr(0) NE '' THEN BEGIN
              IF STRMID(adr(0), 0, /REVERSE_OFFSET) NE pathsep THEN adr=adr(0)+pathsep
              ft=FILE_TEST(adr, /DIRECTORY)
              IF ft THEN BEGIN
                Spawn, 'dir '  + '"'+adr(0)+'"' + '*'+ '/b /a-D', adrTempTemp
                IF adrTempTemp(0) NE '' THEN namesThis(i)=namesThis(i)+' ('+STRING(N_ELEMENTS(adrTempTemp),FORMAT='(i0)')+')'
              ENDIF ELSE BEGIN
                IF errLogg EQ '' THEN sv=DIALOG_MESSAGE('Path not found '+adr+newline+newline+'Continue searching for files on other templates?', /QUESTION, DIALOG_PARENT=event.Top)
                errLogg=errLogg+'Path not found '+adr+newline
                IF sv EQ 'No' THEN BEGIN
                  breakFlag=1
                  errLogg=''
                  BREAK
                ENDIF
              ENDELSE
            ENDIF
          ENDFOR
          IF breakFlag THEN BREAK
        ENDIF
        tempnames=[tempnames, namesThis]
        modArr=[modArr,INTARR(N_ELEMENTS(namesThis))+m]
        tempIDarr=[tempIDarr,INDGEN(N_ELEMENTS(namesThis))]
      ENDIF
    ENDFOR
    IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=event.Top)
  ENDIF
  WIDGET_CONTROL, lblAutoProgress, SET_VALUE=''
  WIDGET_CONTROL, listAuto, SET_VALUE=tempnames, SET_LIST_SELECT=0

end
