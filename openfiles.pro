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

pro openFiles, adrFilesToOpen, SILENT=silent, FRAMENOS=frameNos
  COMPILE_OPT hidden
  COMMON VARI

  IF adrFilesToOpen(0) NE '' THEN BEGIN
     WIDGET_CONTROL, /HOURGLASS

    ;defPath=FILE_DIRNAME(adrFilesToOpen(0))
    tagNames=TAG_NAMES(structImgs)
    oldSel=WIDGET_INFO(listFiles, /LIST_SELECT)  & oldSel=oldSel(0)
    newSel=oldSel

    app=WIDGET_INFO(btnAppend, /BUTTON_SET)

    nFiles=n_elements(adrFilesToOpen)
    counter=0
    tnSt=TAG_NAMES(structImgs)
    IF tnSt(0) EQ 'EMPTY' THEN app=0
    IF app EQ 0 THEN newSel=0 ELSE BEGIN
      app=N_TAGS(structImgs)
      ;avoid conflicting structure names after closing images
      tnSt=TAG_NAMES(structImgs)
      FOR i=0, app-1 DO tnSt(i)=STRMID(tnSt(i),1,STRLEN(tnSt(i))-1)
      tnSt=LONG(tnSt)
      counter=MAX(tnSt)
    ENDELSE
    
    ;read image info from dicom header
    errLogg=''
    
    IF N_ELEMENTS(silent) EQ 0 THEN silent = 0

    FOR i=0, nFiles-1 DO BEGIN

      WIDGET_CONTROL, lblProgress, SET_VALUE='Loading file info: '+STRING(i*100./nFiles, FORMAT='(i0)')+' %'
     
      structNew=readImgInfo(adrFilesToOpen(i), evTop, silent)
         
      IF SIZE(structNew, /TNAME) EQ 'STRUCT' THEN BEGIN
        tagn=TAG_NAMES(structNew)
        multiFrame=1
        IF tagn.HasValue('FILENAME') THEN multiFrame=0

        IF counter EQ 0 AND app EQ 0 THEN BEGIN
          IF multiFrame THEN BEGIN          
            IF N_ELEMENTS(frameNos) GT 0 THEN BEGIN;coming from selectImages with specified frameNos
              structImgs=CREATE_STRUCT('S0',structNew.(frameNos(i)))
              counter=counter+1
            ENDIF ELSE BEGIN
              structImgs=CREATE_STRUCT('S0',structNew.(0))
              counter=1
              FOR mf=1, structNew.(0).nFrames-1 DO BEGIN
                structImgs=CREATE_STRUCT(structImgs,'S'+STRING(counter,FORMAT='(i0)'),structNew.(mf))
                counter=counter+1
              ENDFOR
            ENDELSE
          ENDIF ELSE BEGIN
            structImgs=CREATE_STRUCT('S0',structNew)
            counter=counter+1
          ENDELSE
          
        ENDIF ELSE BEGIN
          
          IF multiFrame THEN BEGIN
            IF N_ELEMENTS(frameNos) GT 0 THEN BEGIN;coming from selectImages with specified frameNos
              structImgs=CREATE_STRUCT(structImgs,'S'+STRING(counter+app,FORMAT='(i0)'),structNew.(frameNos(i)))
              counter=counter+1         
            ENDIF ELSE BEGIN
              FOR mf=0, structNew.(0).nFrames-1 DO BEGIN
                structImgs=CREATE_STRUCT(structImgs,'S'+STRING(counter+app,FORMAT='(i0)'),structNew.(mf))
                counter=counter+1
              ENDFOR
            ENDELSE
          ENDIF ELSE BEGIN
            structImgs=CREATE_STRUCT(structImgs,'S'+STRING(counter+app,FORMAT='(i0)'),structNew)
            counter=counter+1
          ENDELSE
        ENDELSE
        ;ENDIF
      ENDIF
    ENDFOR
    
    tags=tag_names(structImgs)
    
    IF tags(0) NE 'EMPTY' THEN BEGIN;any image header in memory
      nImg=N_TAGS(structImgs)
      
      ;close non image files?
      imsz=!Null
      FOR i=0, nImg-1 DO imsz=[imsz,structImgs.(i).imagesize(0)]
      closeIds=WHERE(imsz EQ -1)
      IF closeIds(0) NE -1 THEN BEGIN
        
        IF silent THEN sv='Yes' ELSE sv=DIALOG_MESSAGE(STRING(N_ELEMENTS(closeIds),FORMAT='(i0)')+' of '+STRING(nImg,FORMAT='(i0)')+ ' files contain no image data. These files may cause crashes. Close these files now?', DIALOG_PARENT=evTop, /QUESTION)
        IF sv EQ 'Yes' THEN BEGIN
          closeImgs, closeIds
          tags=tag_names(structImgs)
          IF tags(0) NE 'EMPTY' THEN nImg=N_TAGS(structImgs)
          IF app NE 0 THEN app=app-N_ELEMENTS(closeIds)
        ENDIF
      ENDIF   

      IF tags(0) NE 'EMPTY' THEN BEGIN;after closing files
        IF TOTAL(results) GT 0 AND counter GT 0 THEN BEGIN
          IF app EQ 0 THEN BEGIN
            clearMulti
          ENDIF ELSE BEGIN
            IF marked(0) EQ -1 THEN BEGIN
              marked=INDGEN(app);there is results, but no file was marked and the new files is appended = mark the files already there
            ENDIF
            clearRes
            
          ENDELSE
        ENDIF ELSE marked=-1
        IF markedMulti(0) NE -1 THEN BEGIN
          szMM=SIZE(markedMulti, /DIMENSIONS)
          markedMultiNew=INTARR(szMM(0),szMM(1)+app)
          markedMultiNew[*,0:szMM(1)-1]=markedMulti
          markedMulti=markedMultiNew
          sv=DIALOG_MESSAGE('Turn off and on MultiMark if you want to update preselection to template.', DIALOG_PARENT=evTop)
        ENDIF
        
        ;warning non-quadratic or missing pixsize
        pix0=!Null & pix1=!Null
        FOR i=0, nImg-1 DO pix0=[pix0,structImgs.(i).pix(0)]
        FOR i=0, nImg-1 DO pix1=[pix1,structImgs.(i).pix(1)]
        IF ~ARRAY_EQUAL(pix0,pix1) THEN errLogg=errLogg+'Software not verified to handle non-quadratic pixels. Images found with non-quadratic pixels. First value might be used for both directions.'+newline
        minOne=WHERE(pix0 EQ -1.,nminOne)
        IF minOne(0) NE -1 THEN errLogg=errLogg+'Missing pixel size from DICOM header for '+STRING(nminOne, FORMAT='(i0)')+' images. Edit pixel size manually to avoid corrupted results. Find button for this in the toolbar in the bottom left corner.'+newline
  
        IF app EQ 0 THEN BEGIN
          wCenter=LONG(structImgs.(0).wCenter)
          wWidth=LONG(structImgs.(0).wWidth)
          If wWidth EQ -1 THEN BEGIN
            wWidth=400 & wCenter=0
          ENDIF
          minmax=[wCenter-wWidth/2,wCenter+wWidth/2]
          WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(wCenter,FORMAT='(i0)')
          WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(wWidth,FORMAT='(i0)')
          WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minmax(0),FORMAT='(i0)')
          WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(minmax(1),FORMAT='(i0)')
          activeImg=0
          moda=structImgs.(0).modality
          newModa=modality
          CASE moda OF
            'CT': newModa=0
            'DX': newModa=1
            'CR': newModa=1
            'DR': newModa=1
            'RG': newModa=1
            'XA': newModa=1
            'MG': newModa=1
            'NM': newModa=2
            'ST': newModa=3
            'PT': newModa=4
            'MR': newModa=5
            ELSE:
          ENDCASE
          IF newModa EQ 2 THEN BEGIN ;SPECT?
            IF structImgs.(0).sliceThick GT 0. THEN newModa=3
          ENDIF
          IF newModa NE modality THEN BEGIN
            modality=newModa
            modNames=TAG_NAMES(multiOpt)
            modalityName=modNames(modality)
            clearRes
            clearMulti
            ;updateMode; later
            ;analyseStrings=analyseStringsAll.(modality); included in updateMode
            ;analyse=analyseStrings(0);included in updateMode
          ENDIF
        ENDIF
  
        IF TOTAL(results) GT 0 AND counter GT 0 AND app EQ 0 THEN clearRes
  
        IF structImgs.(0).nFrames GT 1 THEN BEGIN
          infoFile=FILE_INFO(structImgs.(0).filename)
          IF infoFile.size GT 15000000 THEN sv=DIALOG_MESSAGE('Warning: large files (>15MB) loaded. Consider storing the files locally first to ensure smooth workflow.', DIALOG_PARENT=evTop)
        ENDIF
        
        fileList=getListOpenFiles(structImgs,0,marked, markedMulti);temporary without RenameDICOM temp option to speed up - updateMode calls fillQuickTemplist where this happens
  
        WIDGET_CONTROL, lblProgress, SET_VALUE=' '
  
        IF N_ELEMENTS(silent) EQ 0 THEN silent = 0
        IF silent EQ 0 THEN BEGIN
          WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=newSel
          WIDGET_CONTROL, listFiles, SCR_YSIZE=listFilesYsize
          WIDGET_CONTROL, lblLoadedN, SET_VALUE=STRING(n_elements(fileList), FORMAT='(i0)')+' )'
        ENDIF
        activeImg=readImg(structImgs.(app).filename, structImgs.(app).frameNo)
        
        WIDGET_CONTROL, wtabModes, SET_TAB_CURRENT=modality;include redrawImg, updateROI, updateInfo, but as this is new image and more need it still
        updateMode
        switchMode='No'
  
        WIDGET_CONTROL, drawLarge, SENSITIVE=1
  
        adrFilesToOpen=''
      ENDIF ELSE WIDGET_CONTROL, drawLarge, SENSITIVE=0
    ENDIF ELSE WIDGET_CONTROL, drawLarge, SENSITIVE=0

    IF errLogg NE '' AND silent EQ 0 THEN sv=DIALOG_MESSAGE(errLogg ,/ERROR, DIALOG_PARENT=evTop)
    errLogg=''
    WIDGET_CONTROL, lblProgress, SET_VALUE=' '

  ENDIF
end