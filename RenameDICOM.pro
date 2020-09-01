;RenameDICOM - part of ImageQC - quality control of medical images
;Copyright (C) 2017  Ellen Wasbo, Stavanger University Hospital, Norway
;ellen@wasbo.no
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License version 2
;as published by the Free Software Foundation
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See thef
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

pro RenameDICOM, GROUP_LEADER = mainbase, xoff, yoff, inputAdr

  COMMON VAR_RD, tblAdr, txtCat, txtFormat, lstTags, lstTemplate, btnRename,lblStatus, origPaths, newPaths, pathType, $
    catTemplate, fileTemplate, catTemp, fileTemp, selTag, selTemp, tagDesc, formatDefault,formatCurr,defCatOrFile, btnPutAllinOne, inputAdrRD
  COMMON VARI
  COMPILE_OPT hidden

  origPaths=inputAdr
  inputAdrRD=inputAdr
  newPaths=''
  IF origPaths(0) NE '' THEN pathType=2 ELSE pathType=0; 1=subdirectories, 2=DICOM-files

  RESTORE, thisPath+'data\config.dat'
  tagDesc=TAG_NAMES(renameTemp.tags)
  nT=N_TAGS(renameTemp.tags)
  formatDefault=STRARR(nT)
  FOR i=0, nT-1 DO formatDefault(i)=renameTemp.tagFormats.(i)
  formatCurr=formatDefault
  selTemp=0

  bMain = WIDGET_BASE(TITLE='RenameDICOM - define set of dicom tags to be used for renaming of dicom files', /COLUMN, XSIZE=900, YSIZE=860, XOFFSET=xoff, YOFFSET=yoff,/TLB_KILL_REQUEST_EVENTS, GROUP_LEADER=mainbase,/MODAL)

  ;Name templates

  bhead=WIDGET_BASE(bMain, /ROW, YSIZE=30)
  lblInfoName=WIDGET_LABEL(bHead,VALUE='Build or select naming template ', FONT="Arial*Bold*18")
  bNameConstruct=WIDGET_BASE(bMAin, /COLUMN, FRAME=1, XSIZE=880, YSIZE=220)

  bNC2=WIDGET_BASE(bNameConstruct, /ROW)
  bTags=WIDGET_BASE(bNC2, /COLUMN)
  lblTags=WIDGET_LABEL(bTags, VALUE='Tag list ', FONT=font0, /ALIGN_LEFT)
  lstTags=WIDGET_LIST(bTags, VALUE=tagDesc, UVALUE='lstTags', SCR_YSIZE=170, FONT=font1)
  bPush=WIDGET_BASE(bNC2,/COLUMN)
  lblPush=WIDGET_LABEL(bPush, VALUE='', YSIZE=60)
  btnPush=WIDGET_BUTTON(bPush, VALUE='>>', TOOLTIP='Add tag to name template', UVALUE='addTag', FONT=font1)

  bNCcol=WIDGET_BASE(bNC2, /COLUMN)
  bTemplate=WIDGET_BASE(bNCcol, /ROW)
  lblTemplate=WIDGET_LABEL(bTemplate, VALUE='Select template:', FONT=font1)
  lstTemplate=WIDGET_DROPLIST(bTemplate, VALUE='', UVALUE='lstTemplate', XSIZE=100, FONT=font1)
  btnSaveTemplate=WIDGET_BUTTON(bTemplate, VALUE=thisPath+'images\save.bmp', /BITMAP, TOOLTIP='Save current Name formats as template', UVALUE='saveTemp')
  btnDelTemplate=WIDGET_BUTTON(bTemplate, VALUE=thisPath+'images\delete.bmp', /BITMAP, TOOLTIP='Delete selected template', UVALUE='delTemp')
  btnSettRD=WIDGET_BUTTON(bTemplate, VALUE=thisPath+'images\gears.bmp', /BITMAP, TOOLTIP='RenameDICOM settings and template manager', UVALUE='settingsRD')

  lblInfoName=WIDGET_LABEL(bNCcol,VALUE='Double-click on element in the list to add element to the name template.', /ALIGN_LEFT, FONT=font1)

  bNCrow=WIDGET_BASE(bNCcol, /ROW)
  defCatOrFile=CW_BGROUP(bNCrow, ['Subfolder name','File name'], UVALUE='defCatOrFile', SET_VALUE=0, /COLUMN, /EXCLUSIVE, /RETURN_INDEX, FONT=font1)
  bStrings=WIDGET_BASE(bNCrow, /COLUMN)
  bStringsCat=WIDGET_BASE(bStrings, /ROW)
  bStringsFile=WIDGET_BASE(bStrings, /ROW)

  catTemplate=WIDGET_TEXT(bStringsCat, VALUE='', SCR_XSIZE=450, XSIZE=200, FONT=font1)
  fileTemplate=WIDGET_TEXT(bStringsFile, VALUE='', SCR_XSIZE=450, XSIZE=200, FONT=font1)
  btnCatEmpty=WIDGET_BUTTON(bStringsCat, VALUE='<<', UVALUE='catPop', TOOLTIP='Remove last element from name format', FONT=font1)
  btnCatEdit=WIDGET_BUTTON(bStringsCat, VALUE=thisPath+'images\edit.bmp', /BITMAP, UVALUE='catEdit', TOOLTIP='Remove specific elements from name template')
  btnFileEmpty=WIDGET_BUTTON(bStringsFile, VALUE='<<', UVALUE='filePop', TOOLTIP='Remove last element from name format', FONT=font1)
  btnFileEdit=WIDGET_BUTTON(bStringsFile, VALUE=thisPath+'images\edit.bmp', /BITMAP, UVALUE='fileEdit', TOOLTIP='Remove specific elements from name template')

  mlF=WIDGET_LABEL(bNCcol, VALUE='')

  ;Format coding
  bFormat=WIDGET_BASE(bNCcol, /ROW)
  lblFormat=WIDGET_LABEL(bFormat, VALUE='Format code for selected tag and template:', FONT=font1)
  txtFormat=WIDGET_TEXT(bFormat, VALUE='',/EDITABLE, XSIZE=20, FONT=font1)
  btnFormat=WIDGET_BUTTON(bFormat, VALUE='Apply', UVALUE='applyFormat', FONT=font1)
  lblFormatInfo=WIDGET_LABEL(bNCcol, VALUE='Text: a<#letters, 0=all>, Integer: i<#digits>, Float: f<#digits>.<#decimals>', /ALIGN_LEFT, FONT=font1)
  lblFormatInfo2=WIDGET_LABEL(bNCcol, VALUE='See settings for more on format codes and testing format codes.', /ALIGN_LEFT, FONT=font1)


  lblMlm=WIDGET_LABEL(bMain, VALUE='', YSIZE=20)

  ;browse
  bBrowse=WIDGET_BASE(bMain, /ROW, XSIZE=500, SCR_XSIZE=450)
  lblCat=WIDGET_LABEL(bBrowse, VALUE='Selected folder: ', FONT="Arial*Bold*18")
  txtCat=WIDGET_TEXT(bBrowse, XSIZE=100)
  btnBrowse=WIDGET_BUTTON(bBrowse, VALUE='Browse',UVALUE='browse', XSIZE=50, FONT=font1)
  btnClear=WIDGET_BUTTON(bBrowse, VALUE='Clear',TOOLTIP='Use files open in ImageQC (if any)',UVALUE='clear', XSIZE=50, FONT=font1)
  lblInfoSel=WIDGET_LABEL(bMain,VALUE='If no path specified then files open in ImageQC will be the files subject to renaming.',FONT=font1)

  bFileActions=WIDGET_BASE(bMain, /ROW, XSIZE=300)
  btnPutAllinOne=WIDGET_BUTTON(bFileActions, VALUE='Move all files in subfolders to selected folder', UVALUE='putAllinOne', FONT=font1, XSIZE=300)
  btnPutSeriesInFolders=WIDGET_BUTTON(bFileActions, VALUE='Sort files into subfolders of same seriesnumber', UVALUE='putSeriesFolder', TOOLTIP='Put all files with same seriesnumber into folder named <seriesnumber>', FONT=font1, XSIZE=300)
  ;btnResetToInput=WIDGET_BUTTON(bFileActions, VALUE='Use paths from open files in ImageQC',UVALUE='resetInput', TOOLTIP='Work on the files already open in the main window (if any)', FONT=font1, XSIZE=300)

  lblMlm2=WIDGET_LABEL(bMain, VALUE='', YSIZE=20)

  ;table
  bTable=WIDGET_BASE(bMain, /ROW)
  mlmTbl=WIDGET_LABEL(bTable, VALUE='', XSIZE=20)
  rownames=['Original name', 'Suggested name']
  displayStrArr=STRARR(2,200)
  IF origPaths(0) NE '' THEN displayStrArr[0,0:MIN([200,N_ELEMENTS(origPaths)])-1]=origPaths
  tblAdr = WIDGET_TABLE(bTable, VALUE=displayStrArr, SCR_XSIZE=700, XSIZE=2, YSIZE=200, SCR_YSIZE=400, /NO_ROW_HEADERS, column_widths=[350,350], column_labels=rownames, ALIGNMENT=1)
  lblMl3=WIDGET_LABEL(bTable, VALUE='', XSIZE=20)
  bSide=WIDGET_BASE(bTable, /COLUMN)
  lblS=WIDGET_LABEL(bSide, VALUe='', YSIZE=20)
  btnViewFirstCat=WIDGET_BUTTON(bSide, VALUE='Test 10 first', UVALUE='firstFolders', FONT=font1)
  btnUpdateName = WIDGET_BUTTON(bSide, VALUE='Generate names', UVALUE='update', FONT=font1)
  btnRename = WIDGET_BUTTON(bSide, VALUE='Rename', SENSITIVE=0, XSIZE=80, UVALUE='rename', FONT=font1)
  mlSide=WIDGET_LABEL(bSide, VALUE='', YSIZE=290)
  btnClose=WIDGET_BUTTON(bSide, VALUE='Close/Cancel', XSIZE=80, UVALUE='cancel', FONT=font1)

  bBottom=WIDGET_BASE(bMain, /row, XSIZE=950)
  lblStatus = WIDGET_LABEL(bBottom, VALUE='Status:', SCR_XSIZE=700, XSIZE=700, FRAME=1, /DYNAMIC_RESIZE, FONT=font1)

  updTemp, 0, 1;sel 0, update lstTemplate 1

  WIDGET_CONTROL, bMain, /REALIZE
  XMANAGER, 'RenameDICOM', bMain
end


pro RenameDICOM_event, event

  COMMON VAR_RD
  COMMON VARI; to change filenames of already opened files
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF

      'cancel': WIDGET_CONTROL, event.top, /DESTROY
      'settingsRD': BEGIN
        WIDGET_CONTROL, Event.top, /DESTROY
        settings, GROUP_LEADER=evTop, xoffset+100, yoffset+100, 'RENAMEDICOM'
      END
      'lstTags': BEGIN
        IF event.clicks EQ 2 THEN addTag
        f=formatCurr.(event.index)
        WIDGET_CONTROL, txtFormat, SET_VALUE=STRMID(f, 1, strlen(f)-2)
      END
      'addTag':addTag
      'lstTemplate': updTemp, WIDGET_INFO(lstTemplate, /DROPLIST_SELECT),0
      'saveTemp': BEGIN
        IF saveOK THEN BEGIN

          newTemp=1
          selNo=WIDGET_INFO(lstTemplate, /DROPLIST_SELECT)

          box=[$
            '1, BASE,, /ROW', $
            '2, LABEL, Overwrite selected template or create new?', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Overwrite, QUIT, TAG=Overwrite',$
            '2, BUTTON, New, QUIT, TAG=New']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Owerwrite or new?', XSIZE=260, YSIZE=100, FOCUSNO=1)

          IF res.Overwrite THEN BEGIN
            newTemp=0
            updTemp=CREATE_STRUCT('cat',catTemp,'file',fileTemp,'formats',formatCurr)
            temps=replaceStructStruct(renameTemp.temp,updTemp,selNo)
            renameTemp=replaceStructStruct(renameTemp,temps,2)
            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          ENDIF

          IF newTemp THEN BEGIN
            box=[$
              '1, BASE,, /ROW', $
              '2, TEXT, , LABEL_LEFT=New template name:, WIDTH=12, TAG=tempname', $
              '1, BASE,, /ROW', $
              '0, BUTTON, Cancel, QUIT, TAG=Cancel',$
              '2, BUTTON, Save, QUIT, TAG=Save']
            res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Owerwrite or new?', XSIZE=200, YSIZE=100, FOCUSNO=1)
            IF res.Save THEN BEGIN
              IF res.tempname NE '' THEN BEGIN
                RESTORE, thisPath+'data\config.dat'
                tempname=STRUPCASE(IDL_VALIDNAME(res.tempname, /CONVERT_ALL))
                alrNames=TAG_NAMES(renameTemp.temp)
                IF alrNames.HasValue(tempname) THEN BEGIN
                  sv=DIALOG_MESSAGE('Template name '+tempname+' already exist.', DIALOG_PARENT=event.Top)
                ENDIF ELSE BEGIN
                  newT=CREATE_STRUCT('cat',catTemp,'file',fileTemp,'formats',formatCurr)
                  temps=CREATE_STRUCT(renameTemp.temp,tempname,newT)
                  renameTemp=replaceStructStruct(renameTemp,temps,2)
                  SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                  updTemp, selTemp+1, 1
                ENDELSE
              ENDIF ELSE sv=DIALOG_MESSAGE('No new name entered. Could not save template.', DIALOG_PARENT=event.top)
            ENDIF
          ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('Save blocked by another user session. Go to settings and remove blocking if this is due to a previous program crash.', DIALOG_PARENT=event.Top)
      END
      'delTemp': BEGIN
        IF saveOK THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          nT=N_TAGS(renameTemp.temp)
          IF nT GT 1 THEN BEGIN
            temps=removeIdStructStruct(renameTemp.temp,selTemp)
            renameTemp=replaceStructStruct(renameTemp,temps,2)
            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            updTemp, 0, 1
          ENDIF ELSE sv=DIALOG_MESSAGE('At least one template is required to be kept.', DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('Save blocked by another user session. Go to settings and remove blocking if this is due to a previous program crash.', DIALOG_PARENT=event.Top)
      END
      'catPop': BEGIN
        ;remove last element
        n=N_ELEMENTS(catTemp)
        IF n GT 1 THEN BEGIN
          catTemp=catTemp[0:n-2]
          WIDGET_CONTROL, catTemplate, SET_VALUE=STRUPCASE(STRJOIN(catTemp,'  |  '))
        ENDIF ELSE BEGIN
          catTemp=!NULL
          WIDGET_CONTROL, catTemplate, SET_VALUE=''
        ENDELSE
      END
      'catEdit':BEGIN
        n=N_ELEMENTS(catTemp)
        IF n GT 1 THEN catTemp=editTemp(catTemp,xoffset,yoffset) ELSE sv=DIALOG_MESSAGE('Can not edit one element only.', DIALOG_PARENT=event.top)
        IF N_ELEMENTS(catTemp) GT 0 AND catTemp(0) NE '' THEN WIDGET_CONTROL, catTemplate, SET_VALUE=STRUPCASE(STRJOIN(catTemp,'  |  ')) ELSE WIDGET_CONTROL, catTemplate, SET_VALUE=''
      END
      'filePop': BEGIN
        ;remove last element
        n=N_ELEMENTS(fileTemp)
        IF n GT 1 THEN BEGIN
          fileTemp=fileTemp[0:n-2]
          WIDGET_CONTROL, fileTemplate, SET_VALUE=STRUPCASE(STRJOIN(fileTemp,'  |  '))
        ENDIF ELSE BEGIN
          fileTemp=!NULL
          WIDGET_CONTROL, fileTemplate, SET_VALUE=''
        ENDELSE
      END
      'fileEdit':BEGIN
        n=N_ELEMENTS(fileTemp)
        IF n GT 1 THEN fileTemp=editTemp(fileTemp,xoffset,yoffset) ELSE sv=DIALOG_MESSAGE('Can not edit one element only.', DIALOG_PARENT=event.top)
        IF N_ELEMENTS(fileTemp) GT 0 AND fileTemp(0) NE '' THEN WIDGET_CONTROL, fileTemplate, SET_VALUE=STRUPCASE(STRJOIN(fileTemp,'  |  ')) ELSE WIDGET_CONTROL, fileTemplate, SET_VALUE=''
      END
      'browse':BEGIN
        WIDGET_CONTROL, lblStatus, SET_VALUE=''
        adr=dialog_pickfile(PATH=defPath, GET_PATH=defPath,/DIRECTORY, /READ, TITLE='Select folder whith DICOM files or subfolders')
        IF adr(0) NE '' THEN BEGIN
          WIDGET_CONTROL, txtCat, SET_VALUE=adr(0)
          WIDGET_CONTROL, tblAdr, SET_VALUE=STRARR(2,200)
          origPaths=''
        ENDIF
      END
      'clear':clearRD
      ;'resetInput':clearRD
      'putAllinOne':BEGIN
        WIDGET_CONTROL, /HOURGLASS
        WIDGET_CONTROL, txtCat, GET_VALUE=adr
        proceed=1
        adrDir=''
        askEmptyDir=!Null
        IF adr(0) NE '' THEN BEGIN
          Spawn, 'dir '  + '"'+adr+'"' + '*'+ '/b /s', res; files only
          origPaths=res(sort(res))
          adrDir=adr(0)
        ENDIF ELSE BEGIN
          IF inputAdrRD(0) NE '' THEN BEGIN
            adrSel=dialog_pickfile(PATH=defPath, GET_PATH=defPath,/DIRECTORY, /READ, TITLE='Select the target folder for the files')
            IF adrSel(0) EQ '' THEN proceed=0 ELSE BEGIN
              fi=FILE_INFO(adrSel(0))
              IF fi.write EQ 1 THEN adrDir=adrSel(0) ELSE BEGIN
                proceed=0
                sv=DIALOG_MESSAGE('You are not allowed to write files to this path:'+adrSel(0), DIALOG_PARENT=event.top)
              ENDELSE
            ENDELSE
          ENDIF
        ENDELSE
        IF origPaths(0) NE '' AND proceed THEN BEGIN
          nnn=N_ELEMENTS(origPaths)
          ;find all dicom files
          FOR i =0,nnn-1 DO BEGIN
            resFI=FILE_INFO(origPaths(i))
            IF resFI.DIRECTORY EQ 1 THEN origPaths(i)='' ELSE BEGIN

              ;IF STRMID(origPaths(i),2,/REVERSE_OFFSET) NE 'dcm' THEN origPaths(i)=''
              IF FILE_BASENAME(origPaths(i)) EQ 'DICOMDIR' THEN dcm=0 ELSE dcm=QUERY_DICOM(origPaths(i))
              IF dcm EQ 0 THEN BEGIN
                origPaths(i)=''
              ENDIF ELSE BEGIN
                IF FILE_BASENAME(origPaths(i)) EQ 'DICOMDIR' THEN origPaths(i)=''
              ENDELSE
            ENDELSE
          ENDFOR

          notEmpty=WHERE(origPaths NE '')
          origPaths2=origPaths(notEmpty)
          origPaths=origPaths2
          nFiles=N_ELEMENTS(origPaths)
          newPaths=origPaths & newPaths[*]=''

          for i=0, nFiles-1 do begin
            filen=FILE_BASENAME(origPaths(i))
            tempname=adrDir + filen
            newPaths(i)=tempname
          endfor

          modPaths=getUniqPaths(origPaths,newPaths,2)
          origPaths=modPaths.origPaths & newPaths=modPaths.newPaths

          for i=0, nFiles-1 do BEGIN
            IF origPaths(i) NE '' AND origPaths(i) NE newPaths(i) THEN BEGIN
              WIDGET_CONTROL, lblStatus, SET_VALUE='Moving file '+STRING(i,FORMAT='(i0)')+'/'+  STRING(nFiles,FORMAT='(i0)')
              file_move, origPaths(i), newPaths(i) 
              askEmptyDir=[askEmptyDir, FILE_DIRNAME(origPaths(i))]
              IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN structImgs.(i).filename=newPaths(i)
            ENDIF
          endfor

          alreadyName=WHERE(newPaths EQ tempname)
          IF alreadyName(0) NE -1 THEN tempname=STRMID(tempname,0,STRLEN(tempname)-4)+'_1.dcm'

          IF N_ELEMENTS(origPaths) EQ 1 AND origPaths(0) EQ '' THEN BEGIN
            sv=DIALOG_MESSAGE('No valid DICOM files found.', DIALOG_PARENT=event.top)
          ENDIF ELSE BEGIN
            sv=DIALOG_MESSAGE('All DICOM files in the subfolders ('+STRING(nFiles, FORMAT='(i0)')+') are now moved to the selected folder. Delete empty folders?',/QUESTION, DIALOG_PARENT=event.Top)
            IF sv EQ 'Yes' THEN BEGIN
              WIDGET_CONTROL, /HOURGLASS            
              ;sort longest path first
              strLenArr=!Null
              FOR i=0, N_ELEMENTS(askEmptyDir)-1 DO strLenArr=[strLenArr, STRLEN(askEmptyDir(i))]
              strLenArr2str=STRING(strLenArr,FORMAT='(i010)')
              newOrder=multiBsort([[strLenArr2str],[askEmptyDir]],[1,0]);longest adr first (subfolders first) then alphabetically on address to make uniq work
              askEmptyDir=askEmptyDir(newOrder)
              askEmptyDir=askEmptyDir(UNIQ(askEmptyDir))
              FOR i=0, N_ELEMENTS(askEmptyDir)-1 DO BEGIN
                fi=FILE_INFO(askEmptyDir(i))
                IF fi.size EQ 0 THEN BEGIN
                  WIDGET_CONTROL, lblStatus, SET_VALUE='Deleting empty subfolder '+STRING(i+1,FORMAT='(i0)')
                  FILE_DELETE, askEmptyDir(i), /QUIET
                ENDIF
              ENDFOR
            ENDIF
          ENDELSE
          WIDGET_CONTROL, lblStatus, SET_VALUE=''
          IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN BEGIN
            origPaths=newPaths
            inputAdrRD=newPaths
            displayStrArr=STRARR(2,200)
            pathType=2
            IF origPaths(0) NE '' THEN displayStrArr[0,0:MIN([200,N_ELEMENTS(origPaths)])-1]=origPaths
            WIDGET_CONTROL, tblAdr, SET_VALUE=displayStrArr
          ENDIF
        ENDIF; no adr
      END
      'putSeriesFolder': BEGIN
        WIDGET_CONTROL, /HOURGLASS
        WIDGET_CONTROL, txtCat, GET_VALUE=adr
        proceed=1
        adrDir=''
        IF adr(0) NE '' THEN BEGIN
          Spawn, 'dir '  + '"'+adr(0)+'\"' + '*'+ '/b /s', res; both files and directories
          origPaths=res(sort(res))
          adrDir=adr(0)
        ENDIF ELSE BEGIN
          IF inputAdrRD(0) NE '' THEN BEGIN
            ;same base filename?
            dirName=FILE_DIRNAME(inputAdrRD)
            uqDir=UNIQ(dirName)
            IF N_ELEMENTS(uqDir) NE 1 THEN BEGIN
              proceed=0
              sv=DIALOG_MESSAGE('The selected files do not have the same base folder. This operation can only be done to files in the same folder.', DIALOG_PARENT=event.top)
            ENDIF ELSE adrDir=dirName(0)+'\'
          ENDIF
        ENDELSE
        IF origPaths(0) NE '' AND proceed THEN BEGIN
          nnn=N_ELEMENTS(origPaths)
          ;find all dicom files
          FOR i =0,nnn-1 DO BEGIN
            IF FILE_BASENAME(origPaths(i)) EQ 'DICOMDIR' THEN dcm = 0 ELSE BEGIN
              resFI=FILE_INFO(origPaths(i))
              IF resFI.DIRECTORY EQ 1 THEN dcm=0 ELSE dcm=QUERY_DICOM(origPaths(i))
            ENDELSE
            IF dcm EQ 0 THEN origPaths(i)=''
          ENDFOR

          notEmpty=WHERE(origPaths NE '')
          origPaths2=origPaths(notEmpty)
          origPaths=origPaths2
          nFiles=N_ELEMENTS(origPaths)
          newPaths=origPaths & newPaths[*]=''

          subFs=''

          for i=0, nFiles-1 do begin
            WIDGET_CONTROL, lblStatus, SET_VALUE='Preparing files for subfolders... '+STRING(i+1,FORMAT='(i0)')+' / '+STRING(nFiles, FORMAT='(i0)')
            o=obj_new('idlffdicom')
            t=o->read(origPaths(i))

            test=o->GetReference('0020'x,'0011'x)
            test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
            seriesNmb=LONG(*(test_peker[0]))

            subFolder=STRING(seriesNmb, FORMAT='(i04)')+'\'
            IF ~subFs.HasValue(subFolder) THEN FILE_MKDIR, adrDir+ subFolder
            tempname=adrDir+ subFolder + FILE_BASENAME(origPaths(i))
            newPaths(i)=tempname
          endfor

          for i=0, nFiles-1 do BEGIN
            IF origPaths(i) NE '' AND origPaths(i) NE newPaths(i) THEN BEGIN
              file_move, origPaths(i), newPaths(i)
              IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN structImgs.(i).filename=newPaths(i)
            ENDIF
          endfor

          sv=DIALOG_MESSAGE('All DICOM files in the selected folder are now moved to subfolders named by the seriesnumber.', DIALOG_PARENT=event.top)
          displayArr=STRARR(2,200)
          IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN BEGIN
            origPaths=newPaths
            inputAdrRD=newPaths
            IF origPaths(0) NE '' THEN displayArr[0,0:MIN([200,N_ELEMENTS(origPaths)])-1]=origPaths
          ENDIF
          WIDGET_CONTROL, tblAdr, SET_VALUE=displayArr
          WIDGET_CONTROL, lblStatus, SET_VALUE=''
        ENDIF
      END
      'firstFolders':update=10
      'update':update=1
      'rename':rename=1
      'applyFormat': BEGIN
        WIDGET_CONTROL, txtFormat, GET_VALUE=newFormat
        sel=WIDGET_INFO(lstTags, /LIST_SELECT)
        IF sel(0) NE -1 AND newFormat(0) NE '' THEN BEGIN
          a='0'
          Catch, Error_status
          IF Error_status NE 0 THEN BEGIN
            sv=DIALOG_MESSAGE('Format code not valid. Set to a0.',/ERROR, DIALOG_PARENT=event.top)
            newFormat='a0'
            CATCH, /CANCEL
          ENDIF
          b=string(a,FORMAT='('+newFormat(0)+')');cause error?

          formatCurr.(sel)='('+newFormat(0)+')'
          WIDGET_CONTROL, txtFormat, GET_VALUE=newFormat(0)
        ENDIF
      END

      Else:
    ENDCASE
  ENDIF; uval defined

  ;******************* Exit program ***********************
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    WIDGET_CONTROL, event.top, /DESTROY
  ENDIF

  ;******************* Finding suggested names ***********************
  IF N_ELEMENTS(update) GT 0 THEN BEGIN
    IF update GE 1 THEN BEGIN; find dicom files within path in txtCat field if defined else use inputAdr
      WIDGET_CONTROL, /HOURGLASS
      WIDGET_CONTROL, txtCat, GET_VALUE=adr
      RESTORE, thisPath+'data\config.dat'
      res=''
      IF adr(0) NE '' THEN BEGIN; find dicom files within path in txtCat field
        Spawn, 'dir '  + '"'+adr(0)+'"' + '*'+ '/b /ad', res; directories only
        origPaths=adr(0)+res(sort(res))
        delimPos=STRPOS(origPaths,'\', /REVERSE_SEARCH)
        IF delimPos(0) NE 0 THEN origPaths=origPaths+'\'

        pathType=1; default assume catalogues not single files
        IF res(0) NE '' THEN BEGIN

          IF N_ELEMENTS(catTemp) GT 0 THEN BEGIN
            newPaths=origPaths & newPaths[*]=''

            IF update EQ 10 THEN ncats=MIN([10,n_elements(origPaths)]) ELSE ncats=n_elements(origPaths)
            displayTable=STRARR(2,200)
            IF ncats LT 200 THEN displayTable[0,0:ncats-1]=FILE_BASENAME(origPaths[0:ncats-1]) ELSE BEGIN
              displayTable[0,*]=FILE_BASENAME(origPaths[0:199])
              sv=DIALOG_message('More than 200 subfolders. Only first 200 will appear in list.', DIALOG_PARENT=event.top)
            ENDELSE
            WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable

            for i=0, ncats-1 do begin
              newPaths(i)=newFileName(origPaths(i), 1, catTemp, renameTemp.tags, formatCurr)
              IF i LT 200 THEN displayTable(1,i)=FILE_BASENAME(newPaths(i)) ELSE displayTable[1,*]=FILE_BASENAME(newPaths[0:199])
              WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable
              WIDGET_CONTROL, lblStatus, SET_VALUE='Finding names from DICOM content - folder: '+displayTable(0,i)
            endfor
            empt=WHERE(newPaths EQ '')
            IF empt(0) NE -1 AND N_ELEMENTS(empt) EQ N_ELEMENTS(newPaths) THEN BEGIN
              pathType=2
              WIDGET_CONTROL, lblStatus, SET_VALUE='Found no DICOM content in subfolder, searching for files of parent folder.'
            ENDIF ELSE BEGIN
              If update EQ 1 THEN WIDGET_CONTROL, btnRename, SENSITIVE=1
              WIDGET_CONTROL, lblStatus, SET_VALUE=''
            ENDELSE
          ENDIF ELSE sv=DIALOG_MESSAGE('Name template cannot be empty.', DIALOG_PARENT=event.top)
        ENDIF
      ENDIF

      IF res(0) EQ '' OR pathType EQ 2 THEN BEGIN; no catalogues (or only catalogues with no DICOM content) search for files
        pathType=2

        IF N_ELEMENTS(fileTemp) GT 0 THEN BEGIN
          IF adr(0) NE '' THEN BEGIN; find dicom files within path in txtCat field
            Spawn, 'dir '  + '"'+adr(0)+'\"' + '*'+ '/b /a-d', res
            IF res(0) EQ '' THEN origPaths='' ELSE origPaths=adr(0)+res(sort(res))
          ENDIF

          IF origPaths(0) NE '' THEN BEGIN
            IF update EQ 10 THEN nFiles=MIN([10,N_ELEMENTS(origPaths)]) ELSE nFiles=N_ELEMENTS(origPaths)
            newPaths=origPaths & newPaths[*]=''
            displayTable=STRARR(2,200)
            IF nFiles LT 200 THEN displayTable[0,0:nFiles-1]=FILE_BASENAME(origPaths[0:nFiles-1]) ELSE BEGIN
              displayTable[0,0:199]=FILE_BASENAME(origPaths[0:199])
              sv=DIALOG_message('More than 200 files. Only first 200 will appear in list.', DIALOG_PARENT=event.top)
            ENDELSE
            WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable

            for i=0, nFiles-1 do begin
              newPaths(i)=newFileName(origPaths(i), 0, fileTemp, renameTemp.tags, formatCurr)
              IF i LT 200 THEN displayTable(1,i)=FILE_BASENAME(newPaths(i)) ELSE displayTable[1,*]=FILE_BASENAME(newPaths[0:199])
              WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable
              WIDGET_CONTROL, lblStatus, SET_VALUE='Finding names from DICOM content. Filenumber '+STRING(i,FORMAT='(i0)')+' / '+STRING(nFiles, FORMAT='(i0)')
            endfor
            IF update EQ 1 THEN WIDGET_CONTROL, btnRename, SENSITIVE=1
            WIDGET_CONTROL, lblStatus, SET_VALUE=''
          ENDIF ELSE sv=DIALOG_MESSAGE('Found no images to rename. Specify folder or open files in main window first. NB: Cannot handle special charachters in the path.', DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('Filename template cannot be empty.', DIALOG_PARENT=event.top)
      ENDIF
    ENDIF
    update=0
  ENDIF

  ;******************* Perform rename of catalogues or files ? ***********************
  IF N_ELEMENTS(rename) NE 0 THEN BEGIN
    IF rename EQ 1 THEN BEGIN
      WIDGET_CONTROL, txtCat, GET_VALUE=adr
      ;empty?
      NotEmp=WHERE(newPaths NE '')
      newPaths=newPaths(NotEmp)
      origPaths=origPaths(NotEmp)

      ;unique names?
      modPaths=getUniqPaths(origPaths,newPaths,pathType)
      origPaths=modPaths.origPaths & newPaths=modPaths.newPaths

      u=UNIQ(newPaths)

      antEndret=0
      IF N_ELEMENTS(u) EQ N_ELEMENTS(newPaths) THEN BEGIN

        nP= n_elements(newPaths)
        for i=0, nP-1 do begin
          IF newPaths(i) NE '' AND newPaths(i) NE origPaths(i) THEN BEGIN
            file_move, origPaths(i), newPaths(i)
            IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN structImgs.(i).filename=newPaths(i)
            antEndret=antEndret+1
          ENDIF
          WIDGET_CONTROL, lblStatus, SET_VALUE='Renaming... '+STRING(i,FORMAT='(i0)')+' / '+STRING(nP, FORMAT='(i0)')
        endfor
        WIDGET_CONTROL, btnRename, SENSITIVE=0
        WIDGET_CONTROL, lblStatus, SET_VALUE='Finished renaming'
        WIDGET_CONTROL, tblAdr, SET_VALUE=STRARR(2,200)

        ; also rename files of directories?
        IF pathType EQ 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Also rename files?',/QUESTION, DIALOG_PARENT=event.top)
          IF sv EQ 'Yes' THEN BEGIN
            WIDGET_CONTROL, /HOURGLASS
            pathType=2
            dirNames=newPaths
            nDirs=n_elements(dirNames)
            FOR i=0, nDirs-1 DO BEGIN
              Spawn, 'dir '  + '"'+dirNames(i)+'"' + '*'+ '/b /a-d', res; files only,may not work in standalone version (makeRT)
              origPaths=dirNames(i)+res(sort(res))
              IF origPaths(0) NE '' THEN BEGIN
                nFiles=N_ELEMENTS(origPaths)
                WIDGET_CONTROL, lblStatus, SET_VALUE='Renaming '+STRING(nFiles,FORMAT='(i0)')+' files in directory '+STRING(i+1, FORMAT='(i0)')+'/'+STRING(nDirs, FORMAT='(i0)')
                newPaths=origPaths & newPaths[*]=''
                RESTORE, thisPath+'data\config.dat'
                for j=0, nFiles-1 do newPaths(j)=newFileName(origPaths(j), 0, fileTemp, renameTemp.tags, formatCurr)
                modPaths=getUniqPaths(origPaths,newPaths,pathType)
                origPaths=modPaths.origPaths & newPaths=modPaths.newPaths
                u=UNIQ(newPaths)
                IF N_ELEMENTS(u) EQ N_ELEMENTS(newPaths) THEN BEGIN

                  for k=0, n_elements(newPaths)-1 do begin
                    IF newPaths(k) NE '' AND origPaths(k) NE newPaths(k) THEN file_move, origPaths(k), newPaths(k)
                  endfor
                ENDIF ELSE BEGIN
                  sv=DIALOG_MESSAGE('Names are not unique using the selected name template. Only first unique file is renamed.', DIALOG_PARENT=event.top)
                  origPathsMod=origPaths(u)
                  newPathsMod=newPaths(u)
                  for k=0, n_elements(newPathsMod)-1 do begin
                    IF newPathsMod(k) NE '' AND newPaths(i) NE origPaths(i) THEN file_move, origPathsMod(k), newPathsMod(k)
                  endfor
                ENDELSE
              ENDIF
            ENDFOR
          ENDIF
        ENDIF

        IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN WIDGET_CONTROL, event.top, /DESTROY ELSE WIDGET_CONTROL, lblStatus, SET_VALUE='Finished renaming'
        newPaths[*]='' & origPaths[*]=''
        pathType=0
      ENDIF ELSE BEGIN
        sv=DIALOG_MESSAGE('Names are not unique using the selected name template. Only first unique file is renamed.', DIALOG_PARENT=event.top)
        origPathsMod=origPaths(u)
        newPathsMod=newPaths(u)
        for k=0, n_elements(newPathsMod)-1 do begin
          IF newPathsMod(k) NE '' AND newPathsMod(k) NE origPathsMod(k) THEN BEGIN
            file_move, origPathsMod(k), newPathsMod(k)
            IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN structImgs.(u(k)).filename=newPathsMod(k)
          ENDIF
        endfor
        IF inputAdrRD(0) NE '' AND adr(0) EQ '' THEN WIDGET_CONTROL, event.top, /DESTROY ELSE WIDGET_CONTROL, lblStatus, SET_VALUE='Finished renaming'
        newPaths[*]='' & origPaths[*]=''
        pathType=0
      ENDELSE
    ENDIF
    rename=0
    IF adr NE '' AND inputAdrRD(0) NE '' THEN sv=DIALOG_MESSAGE('Files of selected folder have been renamed while files are open in ImageQC. If the open files are the same that were renamed these will cause errors. Reopen these files to refresh the filepath.', DIALOG_PARENT=event.top)
  ENDIF
end

pro updTemp, sel, refreshTempList
  COMMON VARI
  COMMON VAR_RD
  COMPILE_OPT hidden

  IF selTemp NE sel OR refreshTempList THEN BEGIN;new selection
    RESTORE, thisPath+'data\config.dat'
    formatCurr=renameTemp.temp.(sel).formats
    catTemp=renameTemp.temp.(sel).cat
    fileTemp=renameTemp.temp.(sel).file
    WIDGET_CONTROL, catTemplate, SET_VALUE=STRUPCASE(STRJOIN(catTemp,'  |  '))
    WIDGET_CONTROL, fileTemplate, SET_VALUE=STRUPCASE(STRJOIN(fileTemp,'  |  '))
    IF refreshTempList THEN WIDGET_CONTROL, lstTemplate, SET_VALUE=TAG_NAMES(renameTemp.temp), SET_DROPLIST_SELECT=sel
    selTemp=sel
  ENDIF

  selTag=WIDGET_INFO(lstTags, /LIST_SELECT)
  IF selTag NE -1 THEN BEGIN
    f=renameTemp.temp.(sel).formats.(selTag)
    WIDGET_CONTROL, txtFormat, SET_VALUE=STRMID(f, 1, strlen(f)-2)
  ENDIF ELSE WIDGET_CONTROL, txtFormat, SET_VALUE=''

end

pro addTag
  COMMON VAR_RD
  COMPILE_OPT hidden

  selT=WIDGET_INFO(lstTags, /LIST_SELECT)
  selT=selT(0)
  WIDGET_CONTROL, defCatOrFile, GET_VALUE=defa
  ;add element to numbered list and element to string
  IF defa EQ 0 THEN BEGIN; folder
    IF N_ELEMENTS(catTemp) GT 0 THEN BEGIN
      IF catTemp.HasValue(tagDesc(selT)) THEN sv=DIALOG_MESSAGE('This element is already included.', DIALOG_PARENT=event.top) ELSE BEGIN
        catTemp=[catTemp,tagDesc(selT)]
        WIDGET_CONTROL, catTemplate, SET_VALUE=STRUPCASE(STRJOIN(catTemp,'  |  '))
      ENDELSE
    ENDIF ELSE BEGIN
      catTemp=tagDesc(selT)
      WIDGET_CONTROL, catTemplate, SET_VALUE=STRUPCASE(catTemp)
    ENDELSE
  ENDIF ELSE BEGIN; file
    IF N_ELEMENTS(fileTemp) GT 0 THEN BEGIN
      IF fileTemp.HasValue(tagDesc(selT)) THEN sv=DIALOG_MESSAGE('This element is already included.', DIALOG_PARENT=event.top) ELSE BEGIN
        fileTemp=[fileTemp,tagDesc(selT)]
        WIDGET_CONTROL, fileTemplate, SET_VALUE=STRUPCASE(STRJOIN(fileTemp,'  |  '))
      ENDELSE
    ENDIF ELSE BEGIN
      fileTemp=tagDesc(selT)
      WIDGET_CONTROL, fileTemplate, SET_VALUE=STRUPCASE(fileTemp)
    ENDELSE
  ENDELSE
end

pro clearRD
  COMMON VAR_RD
  COMPILE_OPT hidden
  WIDGET_CONTROL, lblStatus, SET_VALUE=''
  WIDGET_CONTROL, txtCat, SET_VALUE=''
  origPaths=inputAdrRD
  displayStrArr=STRARR(2,200)
  pathType=2
  IF origPaths(0) NE '' THEN displayStrArr[0,0:MIN([200,N_ELEMENTS(origPaths)])-1]=origPaths
  WIDGET_CONTROL, tblAdr, SET_VALUE=displayStrArr
end
