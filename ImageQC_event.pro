;ImageQC - quality control of medical images
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

;main window event handeling
pro ImageQC_event, ev
  COMPILE_OPT hidden
  COMMON VARI

  ;local variables in event
  COMMON LocEv, structImgsAll
  COMMON SELIM_IQ, selAdr; passing selected adresses from selectImages_event.pro to ImageQC_event.pro

  evTop=ev.Top
  ;******************* UVALUE **********************
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 AND SIZE(uval, /TNAME) EQ 'STRING' THEN BEGIN

    tags=TAG_NAMES(structImgs)

    CASE uval OF

      'exit': BEGIN
        IF saveOK EQ 1 THEN BEGIN
          IF FILE_TEST(configPath, /READ) THEN RESTORE, configPath ELSE sv=DIALOG_MESSAGE('Lost connection to config file '+configPath, /ERROR)
          configS.(0).saveBlocked=0
          SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=configPath
        ENDIF
        WIDGET_CONTROL, ev.top, /DESTROY
      END
      'info': BEGIN
        url='https://github.com/EllenWasbo/ImageQC/wiki'
        Case !version.os_family of
          'Windows': SPAWN, 'start '+url
          Else: if (!version.os_name eq 'Mac OS X') then SPAWN, 'open '+url
        Endcase
      END
      'updGitHub': BEGIN
        sv=DIALOG_MESSAGE('Your version of Image QC is ' + currVersion + '. You will be sent to the version-page for ImageQC on GitHub to check whether this is the latest version.', /INFORMATION, DIALOG_PARENT=evTop)
        url='https://github.com/EllenWasbo/ImageQC/wiki/Versions'
        Case !version.os_family of
          'Windows': SPAWN, 'start '+url
          Else: if (!version.os_name eq 'Mac OS X') then SPAWN, 'open '+url
        Endcase
      END
      'about': imageqc_about, GROUP_LEADER=ev.top
      'close': clearAll

      'deciMark': BEGIN
        CASE WIDGET_INFO(listDeciMark, /DROPLIST_SELECT) OF
          0: deciMark='.'
          1: deciMark=','
          ELSE:
        ENDCASE
      END
      'copyHeader': copyHeader=WIDGET_INFO(btnCopyHeader, /BUTTON_SET)
      'transposeTable': transposeTable=WIDGET_INFO(btnTranspose, /BUTTON_SET)

      ;********settings menu***************

      'manageSettings': BEGIN
        oldSelConfig=selConfig
        prevMarkedMulti=markedMulti
        prevMarked=marked
        settings, GROUP_LEADER=ev.TOP, xoffset+100, yoffset+100, 'PARAM'
        IF N_ELEMENTS(prevMarkedMulti) GT 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Restore the marking of images you had before going to Settings?',/QUESTION,DIALOG_PARENT=evTop)
          IF sv EQ 'Yes' THEN BEGIN
            marked=prevMarked
            markedMulti=prevMarkedMulti
            WIDGET_CONTROL, btnUseMulti, SET_BUTTON=1
            IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
            fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
            WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
          ENDIF
        ENDIF
        IF selConfig NE oldSelConfig THEN BEGIN
          clearRes
          redrawImg, 0,0
        ENDIF
      END
      'manageLoadTemp': BEGIN
        prevMarkedMulti=markedMulti
        prevMarked=marked
        settings, GROUP_LEADER=ev.Top, xoffset+100, yoffset+100, 'AUTOSETUP'
        IF N_ELEMENTS(prevMarkedMulti) GT 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Restore the marking of images you had before going to Settings?',/QUESTION,DIALOG_PARENT=evTop)
          IF sv EQ 'Yes' THEN BEGIN
            marked=prevMarked
            markedMulti=prevMarkedMulti
            WIDGET_CONTROL, btnUseMulti, SET_BUTTON=1
            IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
            fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
            WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
          ENDIF
        ENDIF
      END
      'manageQTexp': BEGIN
        prevMarkedMulti=markedMulti
        prevMarked=marked
        settings, GROUP_LEADER = ev.Top, xoffset+100, yoffset+100, 'QTOUT'
        IF N_ELEMENTS(prevMarkedMulti) GT 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Restore the marking of images you had before going to Settings?',/QUESTION,DIALOG_PARENT=evTop)
          IF sv EQ 'Yes' THEN BEGIN
            marked=prevMarked
            markedMulti=prevMarkedMulti
            WIDGET_CONTROL, btnUseMulti, SET_BUTTON=1
            IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
            fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
            WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
          ENDIF
        ENDIF
      END

      ;-----button open files------------generate list of adresses to open---------------------------------------------

      'open':BEGIN
        adrFilesToOpen='--'

        IF adrFilesToOpen(0) EQ '--' THEN BEGIN
          adrFilesToOpen=DIALOG_PICKFILE(TITLE='Select DICOM or .dat file(s) to open.', /READ, /Multiple_files, PATH=defPath, GET_PATH=defPath, DIALOG_PARENT=evTop)
          WIDGET_CONTROL, lblProgress, SET_VALUE='Preparing to load new files...'
          WIDGET_CONTROL, /HOURGLASS
          openFiles, adrFilesToOpen(SORT(adrFilesToOpen))
          redrawImg,0,1
          updateInfo
          WIDGET_CONTROL, lblProgress, SET_VALUE=''
        ENDIF
      END

      'openTxt':BEGIN
        adrFilesToOpen='--'

        IF adrFilesToOpen(0) EQ '--' THEN BEGIN
          adrFilesToOpen=DIALOG_PICKFILE(TITLE='Select .txt files with tabulated imagedata (txt export from ImageJ).', /READ, FILTER='*.txt', /FIX_FILTER, PATH=defPath, GET_PATH=defPath, DIALOG_PARENT=evTop)
          WIDGET_CONTROL, lblProgress, SET_VALUE='Preparing to load new files...'
          WIDGET_CONTROL, /HOURGLASS
          openFiles, adrFilesToOpen;(SORT(adrFilesToOpen))
          redrawImg,0,1
          updateInfo
          WIDGET_CONTROL, lblProgress, SET_VALUE=''
        ENDIF
      END

      'openMulti':BEGIN

        sel=WIDGET_INFO(listSelMultiTemp, /DROPLIST_SELECT)
        IF FILE_TEST(configPath, /READ) THEN RESTORE, configPath ELSE sv=DIALOG_MESSAGE('Lost connection to config file '+configPath, /ERROR); getting the quickTemp-structure
        qTexist=1
        IF SIZE(quickTemp, /TNAME) EQ 'INT' THEN qTexist=0 ELSE BEGIN
          IF SIZE(quickTemp.(modality), /TNAME) EQ 'INT'THEN qTexist=0
        ENDELSE
        IF sel EQ 0 OR qTexist EQ 0 THEN BEGIN
          QTtempArr=-1
          QTname='<none selected>'
        ENDIF ELSE BEGIN
          QTtempArr=quickTemp.(modality).(sel-1)
          tempnames=TAG_NAMES(quickTemp.(modality))
          QTname=tempnames(sel-1)
        ENDELSE

        selAdr=''
        selectImages, QTtempArr, QTname, defPath

        IF selAdr(0) NE '' THEN BEGIN
          openFiles, selAdr
          redrawImg,0,1
          updateInfo
        ENDIF
      END
      'openAuto': BEGIN
        WIDGET_CONTROL, lblProgress, SET_VALUE=''
        autoOpen, GROUP_LEADER = evTop, xoffset+200, yoffset+200
      END; openAuto
      'runAutoMTFNPS': BEGIN
        WIDGET_CONTROL,txtfreqMTF,GET_VALUE=sampFreq
        WIDGET_CONTROL, txtCutLSFW, GET_VALUE=cutLSFw
        WIDGET_CONTROL, txtCutLSFW2, GET_VALUE=cutLSFw2
        WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=MTFroiSz
        WIDGET_CONTROL, tblLin, GET_VALUE=matTab
        WIDGET_CONTROL, txtNPSroiSz, GET_VALUE=NPSroiSzInt
        WIDGET_CONTROL, txtNPSroiDist, GET_VALUE=NPSdist
        WIDGET_CONTROL, txtNPSsubNN, GET_VALUE=NPSsubNN
        WIDGET_CONTROL, txtSmoothNPS, GET_VALUE=NPSsmooth
        WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
        WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
        sParams=CREATE_STRUCT('SampFreq',FLOAT(sampFreq(0)),'cutLSF',WIDGET_INFO(btnCutLSF,/BUTTON_SET),'cutLSFw', FLOAT(cutLSFw(0)),'cutLSFw2',FLOAT(cutLSFw2(0)),'MTFroisSz',FLOAT(MTFroiSz(0)),$
          'materialTable',matTab,'NPSroiSzInt',LONG(NPSroiSzInt(0)),'NPSdist', FLOAT(NPSdist(0)), 'NPSsubNN', LONG(NPSsubNN(0)),'NPSsmooth',FLOAT(NPSsmooth(0)),'rangeWL',LONG([lower,upper]),'decimark',decimark)
        clearAll
        autoMTFNPS, sParams, xoffset+100, yoffset+100
      END
      'runRenameDICOM': BEGIN
        proceed=1
        IF saveOK EQ 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Warning: Saving changes to config file is blocked by another user session. Go to settings and remove blocking if this is due to a previous program crash. Continue to RenameDICOM with template changes blocked?',/Question, DIALOG_PARENT=evTop)
          IF sv EQ 'No' THEN proceed=0
        ENDIF
        IF proceed THEN BEGIN
          alreadyOpenAdr=''
          IF tags(0) NE 'EMPTY' THEN alreadyOpenAdr=getListOpenFiles(structImgs,1,-1,-1)
          RenameDICOM, GROUP_LEADER=ev.Top, xoffset+200, yoffset+200, alreadyOpenAdr
          IF alreadyOpenAdr(0) NE '' THEN BEGIN
            IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
            fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
            WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel, SET_LIST_TOP=oldTop
          ENDIF
        ENDIF
      END
      'saveDat': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          IF N_ELEMENTS(sel) NE 1 THEN BEGIN
            sv=DIALOG_MESSAGE('Select one image only.', DIALOG_PARENT=evTop)
          ENDIF ELSE BEGIN

            imageQCmatrix=CREATE_STRUCT(structImgs.(sel),'matrix', readImg(structImgs.(sel).filename, structImgs.(sel).frameNo))

            adr=DIALOG_PICKFILE(PATH=defPath, GET_PATH=defPath, TITLE='Save active file as IDL structure (.dat-file)',/WRITE, FILTER='*.dat', /FIX_FILTER, DEFAULT_EXTENSION='.dat', DIALOG_PARENT=evTop)

            IF adr NE '' THEN BEGIN
              ;check folder for write permissions
              fi=FILE_INFO(FILE_DIRNAME(adr))
              IF fi.write THEN BEGIN
                imageQCmatrix.filename=adr; changed when opened so that renaming/moving file is possible
                SAVE, imageQCmatrix, FILENAME=adr
              ENDIF ELSE sv=DIALOG_MESSAGE('You do not have writing permissions for the selected folder.',/ERROR)
            ENDIF
          ENDELSE
        ENDIF
      END;saveDat
      
      ;----------------------zoom----------------------------------------
      'zoom': BEGIN
        redrawImg,0,0
        END
      ;-----button DICOM dump to text-file-----------------------------------------------------------------------------------------------------------
      'dump':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          sel=sel(0)
          obj = OBJ_NEW( 'IDLffDICOM' )
          var = obj->Read(structImgs.(sel).filename)
          tit=structImgs.(sel).filename
          pa=thisPath+'data\dumpTemp.txt'
          fi=FILE_INFO(thisPath+'data\')
          IF fi.write EQ 0 THEN BEGIN
            fi=FILE_INFO(tempPath)
            IF fi.write THEN pa=tempPath+'dumpTemp.txt' ELSE BEGIN
              pa=DIALOG_PICKFILE(TITLE='Select a txt-file to dump the DICOM header into.',/WRITE, FILTER='*.txt', /FIX_FILTER, DEFAULT_EXTENSION='.txt', DIALOG_PARENT=evTop)
              IF pa NE '' THEN BEGIN
                fi=FILE_INFO(FILE_DIRNAME(pa))
                IF fi.write EQ 0 THEN BEGIN
                  sv=DIALOG_MESSAGE('You do not have write permissions for the selected folder.',/ERROR)
                  pa=''
                ENDIF
              ENDIF
            ENDELSE
          ENDIF
          IF pa NE '' THEN BEGIN
            obj->DumpElements, pa
            sv=DIALOG_MESSAGE('DICOM dump saved in file '+pa+newline+'Make sure no sensitive data is left there.')
            XDISPLAYFILE, pa, TITLE=tit
          ENDIF
        ENDIF
      END

      ;------edit header info --------------
      'editHeader':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          tn=TAG_NAMES(structImgs.(sel(0)))

          includeArr=actualTags(tn, imgStructInfo, modality)
          idsInclude=WHERE(includeArr EQ 1)

          nInfo=TOTAL(includeArr)
          tagTable=STRARR(nInfo)
          FOR i=0, N_ELEMENTS(idsInclude)-1 DO BEGIN
            tagTable(i)=STRJOIN(STRING(structImgs.(sel(0)).(idsInclude(i)), FORMAT='(a0)'),', ')
          ENDFOR
          headerEdit, tn(idsInclude), tagTable, sel(0), GROUP_LEADER=ev.Top
          updateInfo & redrawImg, 0, 0
          clearRes
        ENDIF
      END

      ;-----move up/down in list ------------------------------------------------------------------------------------------------------------------
      ;moveSelected set? see close to end of this code-file for the actions
      'imgTop': moveSelected=0
      'imgUp': moveSelected=1
      'imgDown': moveSelected=2
      'imgBottom': moveSelected=3

      'sortImg':  moveSelected=4

      ;-----select/mark/remove------------------------------------------------------------------------------------------------------------------
      'listActions':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN

          sel=LONG(STRMID(WIDGET_INFO(ev.ID, /UNAME),0,1,/REVERSE_OFFSET))

          CASE sel OF
            0: BEGIN;Mark selected
              proceed=1
              IF TOTAL(results) GT 0 THEN BEGIN;
                sv=DIALOG_MESSAGE('Continue and lose results?',/QUESTION, DIALOG_PARENT=evTop)
                IF sv EQ 'No' THEN proceed=0
              ENDIF
              IF proceed THEN BEGIN
                sel=WIDGET_INFO(listFiles, /LIST_SELECT)
                testNmb=getResNmb(modality,analyse,analyseStringsAll)
                IF WIDGET_INFO(btnUseMulti, /BUTTON_SET) THEN BEGIN
                  markedArr=markedMulti[testNmb,*]
                  markedArr(sel)=1
                  IF multiOpt.(modality)(testNmb) NE 0 THEN markedMulti[testNmb,*]=markedArr ELSE sv=DIALOG_MESSAGE('Selected test not availble for MultiMark (only numbered tests).', DIALOG_PARENT=evTop)
                ENDIF ELSE BEGIN
                  marked=[marked,sel]
                  marked=marked(SORT(marked))
                  marked=marked(UNIQ(marked))
                  nMark=N_ELEMENTS(marked)
                  IF marked(0) EQ -1 THEN marked=marked[1:nMark-1]; remove the first -1
                ENDELSE

                IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
                fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
                nSel=N_ELEMENTS(sel)
                oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
                WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(nSel-1), SET_LIST_TOP=oldTop
                IF TOTAL(results) GT 0 THEN clearRes
                redrawImg,0,1 & updateInfo
              ENDIF
            END

            1: BEGIN;Remove all marks
              proceed=1
              IF TOTAL(results) GT 0 THEN BEGIN;
                sv=DIALOG_MESSAGE('Continue and lose results?',/QUESTION, DIALOG_PARENT=evTop)
                IF sv EQ 'No' THEN proceed=0
              ENDIF
              IF proceed THEN BEGIN
                clearRes
                nImg=N_TAGS(structImgs)
                marked=-1
                IF markedMulti(0) NE -1 THEN markedMulti=INTARR(N_ELEMENTS(multiOpt.(modality)),nImg)

                IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
                fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
                sel=WIDGET_INFO(listFiles, /LIST_SELECT)
                oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
                WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel, SET_LIST_TOP=oldTop
              ENDIF
            END

            2: BEGIN;Remove mark from selected
              MTFv3d=0
              IF results(getResNmb(modality,'MTF',analyseStringsAll)) THEN BEGIN
                resforeach=WHERE(TAG_NAMES(MTFres) EQ 'M0')
                IF resforeach(0) EQ -1 THEN MTFv3d=1
              ENDIF

              proceed=1
              IF markedMulti(0) NE -1 AND TOTAL(results) GT 0 THEN BEGIN
                sv=DIALOG_MESSAGE('Continue and lose results?',/QUESTION, DIALOG_PARENT=evTop)
                IF sv EQ 'No' THEN proceed=0 ELSE clearRes
              ENDIF

              IF markedMulti(0) EQ -1 THEN BEGIN
                IF MTFv3d OR N_ELEMENTS(noiseRes) GT 1 OR N_ELEMENTS(stpRes) GT 0 OR N_ELEMENTS(crossRes) GT 0 OR N_ELEMENTS(rcRes) GT 0 OR N_ELEMENTS(SNRres) THEN BEGIN
                  sv=DIALOG_MESSAGE('Continue and lose results?',/QUESTION, DIALOG_PARENT=evTop)
                  IF sv EQ 'No' THEN proceed=0 ELSE BEGIN
                    ;clear all results where total result depend on single-images
                    IF MTFv3d THEN clearRes, 'MTF'
                    IF N_ELEMENTS(noiseRes) GT 1 THEN clearRes, 'NOISE'
                    IF N_ELEMENTS(stpRes) GT 0 THEN clearRes, 'STP'
                    IF N_ELEMENTS(crossRes) GT 0 THEN clearRes, 'CROSSCALIB'
                    IF N_ELEMENTS(rcRes) GT 0 THEN clearRes, 'RC'
                    IF N_ELEMENTS(SNRres) GT 0 THEN clearRes, 'SNR'
                  ENDELSE
                ENDIF
              ENDIF

              IF proceed THEN BEGIN
                sel=WIDGET_INFO(listFiles, /LIST_SELECT)
                nImg=N_TAGS(structImgs)
                newSel=-1
                IF markedMulti(0) EQ -1 THEN BEGIN
                  markedArr=INTARR(nImg)
                  IF marked(0) NE -1 THEN markedArr(marked)=1 ELSE markedArr=markedArr+1
                  markedArr(sel)=0
                  marked=WHERE(markedArr EQ 1)
                  unmarked=WHERE(markedArr EQ 0)
                  newSel=marked(0)
                ENDIF ELSE BEGIN
                  testNmb=getResNmb(modality,analyse,analyseStringsAll)
                  markedArr=markedMulti[testNmb,*]
                  markedArr(sel)=0
                  IF multiOpt.(modality)(testNmb) NE 0 THEN markedMulti[testNmb,*]=markedArr
                  mm=WHERE(markedArr EQ 1)
                  newSel=mm(0)
                ENDELSE

                IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
                fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)

                If newSel EQ -1 THEN newSel=0
                oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
                WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=newSel, SET_LIST_TOP=oldTop ; YSIZE=n_elements(fileList),
                IF marked(0) EQ -1 THEN clearRes
                redrawImg,0,1 & updateInfo
              ENDIF
            END

            3: BEGIN;Select inverse
              nImg=N_TAGS(structImgs)
              sel=WIDGET_INFO(listFiles, /LIST_SELECT)
              IF sel(0) EQ -1 THEN BEGIN;mark all
                newSel=INDGEN(nImg)
              ENDIF ELSE BEGIN
                selArr=INTARR(nImg)
                selArr(sel)=1
                newSel=WHERE(selArr EQ 0)
              ENDELSE

              oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
              WIDGET_CONTROL, listFiles, SET_LIST_SELECT=newSel, SET_LIST_TOP=oldTop
              redrawImg,0,1 & updateInfo
            END

            4: BEGIN;Select marked
              oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
              IF markedMulti(0) EQ -1 THEN BEGIN
                IF marked(0) NE -1 THEN BEGIN
                  WIDGET_CONTROL, listFiles, SET_LIST_SELECT=marked, SET_LIST_TOP=oldTop
                  redrawImg,0,1 & updateInfo
                ENDIF
              ENDIF ELSE BEGIN
                testNmb=getResNmb(modality,analyse,analyseStringsAll)
                mm=WHERE(markedMulti[testNmb,*] EQ 1)
                IF mm(0) NE -1 THEN BEGIN
                  WIDGET_CONTROL, listFiles, SET_LIST_SELECT=mm, SET_LIST_TOP=oldTop
                  redrawImg,0,1 & updateInfo
                ENDIF
              ENDELSE
            END

            5: BEGIN; Select imgNo .. to ..DIM
              nImg=N_TAGS(structImgs)
              box=[$
                '1, BASE,, /COLUMN', $
                '2, LABEL, Select images with DICOM image number', $
                '1, BASE,, /ROW', $
                '0, INTEGER, 0, TAG=imgLow, SET_VALUE=0', $
                '0, LABEL, to', $
                '2, INTEGER, 0, TAG=imgHigh, SET_VALUE=0', $
                '1, BASE,, /ROW', $
                '0, BUTTON, Cancel, QUIT',$
                '2, BUTTON, OK, QUIT, TAG=OK']
              res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select range of DICOM image numbers', XSIZE=300, YSIZE=150, FOCUSNO=3, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

              IF res.OK THEN BEGIN
                imgNmb=[res.imgLow, res.imgHigh]
                listImgNmb=INTARR(nImg)
                FOR i=0, nImg-1 DO listImgNmb(i)=structImgs.(i).imgNo
                imgNmb=[MIN(imgNmb),MAX(imgNmb)]
                listImgNmbSel=INTARR(nImg)
                geMin=WHERE(listImgNmb GE imgNmb(0))
                IF geMin(0) NE -1 THEN listImgNmbSel(geMin)=1
                leMax=WHERE(listImgNmb LE imgNmb(1))
                IF leMax(0) NE -1 THEN listImgNmbSel(leMax)=listImgNmbSel(leMax)+1
                newSel=WHERE(listImgNmbSel EQ 2)
                IF newSel(0) NE -1 THEN BEGIN
                  oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
                  WIDGET_CONTROL, listFiles, SET_LIST_SELECT=newSel, SET_LIST_TOP=oldTop
                  redrawImg,0,1 & updateInfo
                ENDIF ELSE sv=DIALOG_MESSAGE('Found no images in the selected range. Selection not changed.', /INFORMATION, DIALOG_PARENT=evTop)
              ENDIF
            END

            6: BEGIN;Close selected
              sel=WIDGET_INFO(listFiles, /LIST_SELECT)
              closeImgs, sel
            END
            ELSE:
          ENDCASE

        ENDIF;tags not empty
      END

      'selectAll':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          ;TODO
          nImg=N_TAGS(structImgs)
          oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
          WIDGET_CONTROL, listFiles, SET_LIST_SELECT=INDGEN(nImg), SET_LIST_TOP=0
          redrawImg,0,1 & updateInfo
        ENDIF
      END

      ;-----button prev/next image------------------------------------------------------------------------------------------------------------------
      'next'  : BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
          nImg=N_TAGS(structImgs)
          IF sel LT nImg-1 THEN BEGIN
            WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel+1
            redrawImg,0,1 & updateInfo
          ENDIF
        ENDIF
      END

      'prev':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          IF sel GT 0 THEN BEGIN
            WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel-1
            redrawImg,0,1 & updateInfo
          ENDIF
        ENDIF
      END

      'movie': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS
          nImg=N_TAGS(structImgs)
          firstImg=readImg(structImgs.(0).filename, structImgs.(0).frameNo)

          szFirst=SIZE(firstImg, /DIMENSIONS)
          volTemp=FLTARR(szFirst(0),szFirst(1), nImg)
          center=szFirst/2+dxya[0:1]

          FOR i=0, nImg-1 DO BEGIN
            tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
            szTemp=SIZE(tempImg, /DIMENSIONS)
            IF ARRAY_EQUAL(szTemp,szFirst) THEN BEGIN
              volTemp[*,*,i]=tempImg
            ENDIF ELSE BEGIN
              sv=DIALOG_MESSAGE('Image number ' + STRING(i, FORMAT='(i0)')+' do not have the same size as the first image. Movie can not be generated.', DIALOG_PARENT=evTop)
              volTemp=-1
              BREAK
            ENDELSE
          ENDFOR

          IF N_ELEMENTS(volTemp) GT 1 AND nImg GT 1 THEN BEGIN
            WIDGET_CONTROL, txtMinWL, GET_VALUE=minWinLev
            WIDGET_CONTROL, txtMaxWL, GET_VALUE=maxWinLev
            winlev=FLOAT([minWinLev,maxWinLev])
            imgQCmovie, volTemp, thisPath, winlev, colTable, GROUP_LEADER=ev.Top
          ENDIF
        ENDIF
      END

      ;---- if new selection in filelist
      'filelist' : BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          redrawImg, 0,1 & updateInfo
        ENDIF
      END

      'lstShowFile':BEGIN
        IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
        fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)
        oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
        WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
      END

      'infoList': BEGIN
        sv=DIALOG_MESSAGE('Right-click in list to find mark/selection options.'+newline+newline+$
          'The images will be listed with their filename + closest subfolder '+newline+$
          'or using a RenameDICOM file template (see user settings) with name equal to the'+newline+$
          'current modality selected.'+newline+$
          'Using a RenameDICOM template will be a bit slower than showing the filename so'+newline+$
          'during running automation templates only the filename option will be used.'+newline+newline+$
          'If the file have multiple frames these will be listed per frame.',/INFORMATION, DIALOG_PARENT=evTop)
      END

      ;------button set min/max WL ----------------------------------------------------------------------------------------------------------------
      'WLminmax': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          minWL=min(activeImg)
          maxWL=max(activeImg)
          WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
          ;reset center/width
          centerWL=(minWL+maxWL)/2
          widthWL=maxWL-minWL
          WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
          redrawImg,0,0
        ENDIF
      END

      'WLmeanstdev': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IMAGE_STATISTICS, activeImg, MEAN=mean, STDDEV=stdev
          minWL=mean-stdev
          maxWL=mean+stdev
          WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
          ;reset center/width
          centerWL=(minWL+maxWL)/2
          widthWL=maxWL-minWL
          WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
          redrawImg,0,0
        ENDIF
      END

      'WLdcm': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          centerWL=structImgs.(sel(0)).wCenter
          widthWL=structImgs.(sel(0)).wWidth
          minWL=centerWL-widthWL/2
          maxWL=centerWL+widthWL/2
          WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
          redrawImg,0,0
        ENDIF
      END

      'colorTable':BEGIN
        box=[$
          '1, BASE,, /COLUMN', $
          '0, LABEL, Select colortable', $
          '0, LABEL, ',$
          '2, LIST, Grayscale B-W|Grayscale W-B|Hot Iron|Hot Metal Blue|PET colors, TAG=ctab', $
          '1, BASE,, /ROW', $
          '0, BUTTON, OK, QUIT, TAG=OK',$
          '2, BUTTON, Cancel, QUIT']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select colortable', XSIZE=300, YSIZE=300, FOCUSNO=3, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.OK THEN BEGIN
          IF N_ELEMENTS(res.ctab) NE 0 THEN BEGIN
            colSel=res.ctab(0)
            oldColtable=coltable
            CASE colSel OF
              0:BEGIN
                coltable=0
                WIDGET_CONTROL, btnSetColorTable, SET_VALUE=thisPath+'images\ctGrayScale.bmp',/BITMAP
              END

              1:BEGIN
                coltable=78
                WIDGET_CONTROL, btnSetColorTable, SET_VALUE=thisPath+'images\ctGrayScaleInv.bmp',/BITMAP
              END

              2:BEGIN
                coltable=75
                WIDGET_CONTROL, btnSetColorTable, SET_VALUE=thisPath+'images\ctHotIron.bmp',/BITMAP
              END

              3:BEGIN
                coltable=76
                WIDGET_CONTROL, btnSetColorTable, SET_VALUE=thisPath+'images\ctHotMetalBlue.bmp',/BITMAP
              END
              4:BEGIN
                coltable=77
                WIDGET_CONTROL, btnSetColorTable, SET_VALUE=thisPath+'images\ctPETcolors.bmp',/BITMAP
              END
              ELSE:
            ENDCASE
            IF coltable NE oldColtable THEN redrawImg, 0,0
          ENDIF
        ENDIF
      END

      ;-----buttons for delta positioning/centering-------------------------------------------------------------------------------------------------------------
      'useDelta': BEGIN
        IF dxya(3) EQ 0 THEN dxya(3)=1 ELSE dxya(3)=0
        clearRes
        updateROI
        redrawImg, 0,0
      END

      'getCenter': BEGIN
        tempImg=activeImg
        imsz=SIZE(tempImg, /DIMENSIONS)

        box=[$
          '1, BASE,, /COLUMN', $
          '0, LABEL, Threshold (HU) for object to center', $
          '0, LABEL, ',$
          '2, INTEGER, 200, LABEL_LEFT=Pixel value, TAG=lim', $
          '1, BASE,, /ROW', $
          '0, BUTTON, OK, QUIT, TAG=OK',$
          '2, BUTTON, Cancel, QUIT']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Set threshold for object to center', XSIZE=200, YSIZE=150, FOCUSNO=3, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.OK THEN BEGIN

          centerTemp=centroid(tempImg, res.lim)

          IF min(centerTemp) LT 0 OR max(centerTemp) GE min(imsz) THEN BEGIN
            centerTemp=imsz/2
            sv=DIALOG_MESSAGE('Centering failed.',/INFORMATION, DIALOG_PARENT=evTop)
          ENDIF

          dxya[0:1]=centerTemp-imsz/2
          WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
          WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
          dxya(3)=1
          IF TOTAL(results) GT 0 THEN clearRes
          updateROI
          redrawImg,0,0
        ENDIF
      END
      'setCenter':BEGIN;set to last position clicked in image
        tempImg=activeImg
        imsz=SIZE(tempImg, /DIMENSIONS)
        IF lastXYreleased(0) EQ -1 THEN centerTemp = imsz/2 ELSE centerTemp=lastXYreleased;*max(imsz)/drawXY
        dxya[0:1]=centerTemp-imsz/2
        WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
        WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
        dxya(3)=1
        IF TOTAL(results) GT 0 THEN clearRes
        updateROI
        redrawImg, 0,0
      END
      'setOffset':BEGIN;set to last position clicked in image to offset
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
        IF sel NE -1 THEN BEGIN
          tempImg=activeImg
          imsz=SIZE(tempImg, /DIMENSIONS)
          IF lastXYreleased(0) EQ -1 THEN centerTemp = imsz/2 ELSE centerTemp=lastXYreleased;*max(imsz)/drawXY
          thisOff=centerTemp-imsz/2-dxya[0:1]
          thisOff_mm=thisOff*structImgs.(sel).pix
          CASE modality OF
            0:BEGIN;CT
              CASE analyse of
                'MTF': BEGIN
                  WIDGET_CONTROL, unitDeltaO_MTF_CT, GET_VALUE=mm
                  IF mm THEN offxyMTF=thisOff_mm ELSE offxyMTF=thisOff
                  strOff=STRING(offxyMTF(0), FORMAT='(i0)')+','+STRING(offxyMTF(1), FORMAT='(i0)')
                  WIDGET_CONTROL, lblDeltaO, SET_VALUE=strOff
                END
                'ROI': BEGIN
                  WIDGET_CONTROL, unitDeltaO_ROI_CT, GET_VALUE=mm
                  IF mm THEN offxyROI=thisOff_mm ELSE offxyROI=thisOff
                  strOff=STRING(offxyROI(0), FORMAT='(i0)')+','+STRING(offxyROI(1), FORMAT='(i0)')
                  WIDGET_CONTROL, lblDeltaO_ROI, SET_VALUE=strOff
                END
                ELSE:
              ENDCASE
            END
            1:BEGIN;Xray
              CASE analyse OF
                'MTF': BEGIN
                  WIDGET_CONTROL, unitDeltaO_MTF_X, GET_VALUE=mm
                  IF mm THEN offxyMTF_X=thisOff_mm ELSE offxyMTF_X=thisOff
                  strOff=STRING(offxyMTF_X(0), FORMAT='(i0)')+','+STRING(offxyMTF_X(1), FORMAT='(i0)')
                  WIDGET_CONTROL, lblDeltaOX, SET_VALUE=strOff
                END
                'ROI': BEGIN
                  WIDGET_CONTROL, unitDeltaO_ROI_X, GET_VALUE=mm
                  IF mm THEN offxyROIX=thisOff_mm ELSE offxyROIX=thisOff
                  strOff=STRING(offxyROIX(0), FORMAT='(i0)')+','+STRING(offxyROIX(1), FORMAT='(i0)')
                  WIDGET_CONTROL, lblDeltaO_ROIX, SET_VALUE=strOff
                END
                ELSE:
              ENDCASE
            END
            5:BEGIN;MR
              CASE analyse OF
                'ROI': BEGIN
                  WIDGET_CONTROL, unitDeltaO_ROI_MR, GET_VALUE=mm
                  IF mm THEN offxyROIMR=thisOff_mm ELSE offxyROIMR=thisOff
                  strOff=STRING(offxyROIMR(0), FORMAT='(i0)')+','+STRING(offxyROIMR(1), FORMAT='(i0)')
                  WIDGET_CONTROL, lblDeltaO_ROIMR, SET_VALUE=strOff
                END
                ELSE:
              ENDCASE
            END
            ELSE:
          ENDCASE

          testNmb=getResNmb(modality,analyse,analyseStringsAll)
          IF results(testNmb) GT 0 THEN clearRes, analyse
          updateROI
          redrawImg, 0,0
        ENDIF
      END
      'setOffset_unit':BEGIN; recalculate text of label to mm
        IF ev.SELECT THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          IF sel NE -1 THEN BEGIN

            CASE modality OF
              0:BEGIN;CT
                CASE analyse of
                  'MTF': BEGIN
                    WIDGET_CONTROL, unitDeltaO_MTF_CT, GET_VALUE=unit
                    IF unit THEN offxyMTF=offxyMTF*structImgs.(sel).pix ELSE offxyMTF=offxyMTF/structImgs.(sel).pix
                    strOff=STRING(offxyMTF(0), FORMAT='(i0)')+','+STRING(offxyMTF(1), FORMAT='(i0)')
                    WIDGET_CONTROL, lblDeltaO, SET_VALUE=strOff
                  END
                  'ROI': BEGIN
                    WIDGET_CONTROL, unitDeltaO_ROI_CT, GET_VALUE=unit
                    IF unit THEN offxyROI=offxyROI*structImgs.(sel).pix ELSE offxyROI=offxyROI/structImgs.(sel).pix
                    strOff=STRING(offxyROI(0), FORMAT='(i0)')+','+STRING(offxyROI(1), FORMAT='(i0)')
                    WIDGET_CONTROL, lblDeltaO_ROI, SET_VALUE=strOff
                  END
                  ELSE:
                ENDCASE
              END
              1:BEGIN;Xray
                CASE analyse OF
                  'MTF': BEGIN
                    WIDGET_CONTROL, unitDeltaO_MTF_X, GET_VALUE=unit
                    IF unit THEN offxyMTF_X=offxyMTF_X*structImgs.(sel).pix ELSE offxyMTF_X=offxyMTF_X/structImgs.(sel).pix
                    strOff=STRING(offxyMTF_X(0), FORMAT='(i0)')+','+STRING(offxyMTF_X(1), FORMAT='(i0)')
                    WIDGET_CONTROL, lblDeltaOX, SET_VALUE=strOff
                  END
                  'ROI': BEGIN
                    WIDGET_CONTROL, unitDeltaO_ROI_X, GET_VALUE=unit
                    IF unit THEN offxyROIX=offxyROIX*structImgs.(sel).pix ELSE offxyROIX=offxyROIX/structImgs.(sel).pix
                    strOff=STRING(offxyROIX(0), FORMAT='(i0)')+','+STRING(offxyROIX(1), FORMAT='(i0)')
                    WIDGET_CONTROL, lblDeltaO_ROIX, SET_VALUE=strOff
                  END
                  ELSE:
                ENDCASE
              END
              5: BEGIN; MR
                CASE analyse OF
                  'ROI': BEGIN
                    WIDGET_CONTROL, unitDeltaO_ROI_MR, GET_VALUE=unit
                    IF unit THEN offxyROIMR=offxyROIMR*structImgs.(sel).pix ELSE offxyROIMR=offxyROIMR/structImgs.(sel).pix
                    strOff=STRING(offxyROIMR(0), FORMAT='(i0)')+','+STRING(offxyROIMR(1), FORMAT='(i0)')
                    WIDGET_CONTROL, lblDeltaO_ROIMR, SET_VALUE=strOff
                  END
                  ELSE:
                ENDCASE
                END
              ELSE:
            ENDCASE

            testNmb=getResNmb(modality,analyse,analyseStringsAll)
            IF results(testNmb) GT 0 THEN clearRes, analyse
            updateROI
            redrawImg, 0,0
          ENDIF;sel ne -1
        ENDIF; select
      END
      'minusDx': BEGIN
        dxya(0)=dxya(0)-1
        WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
        dxya(3)=1 & IF TOTAL(results) GT 0 THEN clearRes & updateROI & redrawImg,0,0
      END
      'plusDx': BEGIN
        dxya(0)=dxya(0)+1
        WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
        dxya(3)=1 & IF TOTAL(results) GT 0 THEN clearRes & updateROI & redrawImg,0,0
      END
      'minusDy': BEGIN
        dxya(1)=dxya(1)-1
        WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
        dxya(3)=1 & IF TOTAL(results) GT 0 THEN clearRes & updateROI & redrawImg,0,0
      END
      'plusDy': BEGIN
        dxya(1)=dxya(1)+1
        WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
        dxya(3)=1 & IF TOTAL(results) GT 0 THEN clearRes & updateROI & redrawImg,0,0
      END
      'minusDa': BEGIN
        dxya(2)=dxya(2)-0.1
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')
        dxya(3)=1 & IF TOTAL(results) GT 0 THEN clearRes & updateROI & redrawImg,0,0
      END
      'plusDa': BEGIN
        dxya(2)=dxya(2)+0.1
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')
        dxya(3)=1 & IF TOTAL(results) GT 0 THEN clearRes & updateROI & redrawImg,0,0
      END
      'hideAnnot': redrawImg, 0,0

      'listSelMultiTemp':BEGIN
        IF markedMulti(0) NE -1 THEN updateMulti
      END
      'addMultiTemp':BEGIN
        IF markedMulti(0) NE -1 THEN BEGIN

          IF saveOK EQ 1 THEN BEGIN
            saveChange=0
            testOpt=WHERE(multiOpt.(modality) GT 0, nTests)

            IF FILE_TEST(configPath, /READ) THEN RESTORE, configPath ELSE sv=DIALOG_MESSAGE('Lost connection to config file '+configPath, /ERROR)
            exTempNames=''
            IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
              IF SIZE(quickTemp.(modality), /TNAME) EQ 'STRUCT' THEN exTempNames=TAG_NAMES(quickTemp.(modality))
            ENDIF ELSE BEGIN
              quickTemp=!Null
              ms=TAG_NAMES(multiOpt)
              FOR i=0, N_TAGS(multiOpt)-1 DO quickTemp=CREATE_STRUCT(quickTemp, ms(i), -1)
            ENDELSE

            box=[$
              '1, BASE,, /ROW', $
              '2,  TEXT, , LABEL_LEFT=Name the template:, WIDTH=12, TAG=newname,', $
              '1, BASE,, /ROW', $
              '0, BUTTON, Save, QUIT, TAG=Save',$
              '2, BUTTON, Cancel, QUIT, TAG=Cancel']
            res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Save template to config file', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

            newQTmod=!Null
            IF ~res.Cancel THEN BEGIN
              IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not save template.', DIALOG_PARENT=evTop) ELSE BEGIN

                tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
                saveChange=1
                oldQTmod=quickTemp.(modality)
                IF SIZE(oldQTmod, /TNAME) EQ 'STRUCT' THEN BEGIN
                  IF exTempNames.HasValue(tempname) THEN BEGIN
                    sv=DIALOG_MESSAGE('A template with this name already exist. Choose another name. Rename can be done in user settings manager.',DIALOG_PARENT=evTop)
                    saveChange=0
                  ENDIF ELSE newQTmod=CREATE_STRUCT(oldQTmod, tempname, markedMulti)
                ENDIF ELSE newQTmod=CREATE_STRUCT(tempname, markedMulti)

              ENDELSE
            ENDIF

            IF saveChange THEN BEGIN
              If N_ELEMENTS(newQTmod) EQ 0 THEN newQTmod=-1
              quickTemp=replaceStructStruct(quickTemp, newQTmod, modality)
              SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=configPath

              WIDGET_CONTROL, listSelMultiTemp, GET_VALUE=multiList
              multiList=[multiList, tempname]
              WIDGET_CONTROL, listSelMultiTemp, SET_VALUE=multiList, SET_DROPLIST_SELECT=N_ELEMENTS(multiList)-1
            ENDIF
          ENDIF ELSE sv=DIALOG_MESSAGE('Save blocked by another user session. Go to settings and remove blocking if this is due to a previous program crash.', DIALOG_PARENT=evTop)

        ENDIF ELSE sv=DIALOG_MESSAGE('No multiMark to save.', DIALOG_PARENT=evTop)
      END
      'saveMultiTemp':BEGIN
        IF markedMulti(0) NE -1 THEN BEGIN
          IF saveOK EQ 1 THEN BEGIN
            saveChange=0
            sel=WIDGET_INFO(listSelMultiTemp, /DROPLIST_SELECT)
            IF sel NE 0 THEN BEGIN
              testOpt=WHERE(multiOpt.(modality) GT 0, nTests)
              szMM=SIZE(markedMulti, /DIMENSIONS)

              IF szMM(0) EQ nTests THEN BEGIN; no new tests available since last save
                markedMultiTemp=markedMulti[0:nTests-1,*]
              ENDIF ELSE BEGIN
                nImg=N_TAGS(structImgs)
                markedMultiTemp=INTARR(nTests, nImg)
                IF N_ELEMENTS(szMM) EQ 1 THEN markedMultiTemp[0:szMM(0)-1]=markedMulti ELSE markedMultiTemp[0:nTests-1,0:szMM(1)-1]=markedMulti[0:nTests-1,0:szMM(1)-1]
              ENDELSE

              IF FILE_TEST(configPath, /READ) THEN RESTORE, configPath ELSE sv=DIALOG_MESSAGE('Lost connection to config file '+configPath, /ERROR)
              szQT=SIZE(quickTemp.(modality), /TNAME)
              IF szQT EQ 'STRUCT' THEN exTempNames=TAG_NAMES(quickTemp.(modality)) ELSE exTempNames=''
              IF sel(0) GT 0 THEN BEGIN; overwrite
                IF szQT EQ 'STRUCT' THEN BEGIN
                  sv=DIALOG_MESSAGE('Are you sure you want to overwrite this template?',DIALOG_PARENT=evTop, /QUESTION)
                  IF sv EQ 'Yes' THEN BEGIN
                    oldQTmod=quickTemp.(modality)
                    newQTmod=replaceStructStruct(oldQTmod, markedMultiTemp, sel(0)-1)
                    quickTemp=replaceStructStruct(quickTemp, newQTmod, modality)
                    saveChange=1
                  ENDIF
                ENDIF
              ENDIF

              IF saveChange THEN SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=configPath
            ENDIF ELSE sv=DIALOG_MESSAGE('Now selected template to overwrite. Use the + button to add a new template.',DIALOG_PARENT=evTop)
          ENDIF ELSE sv=DIALOG_MESSAGE('Save blocked by another user session. Go to settings and remove blocking if this is due to a previous program crash.', DIALOG_PARENT=evTop)
        ENDIF ELSE sv=DIALOG_MESSAGE('No multiMark to save.', DIALOG_PARENT=evTop)
      END

      'manageQT': BEGIN
        prevMarkedMulti=markedMulti
        prevMarked=marked
        settings, GROUP_LEADER=ev.TOP, xoffset+200, yoffset+200, 'QTSETUP'
        IF N_ELEMENTS(prevMarkedMulti) GT 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Restore the marking of images you had before going to Settings?',/QUESTION,DIALOG_PARENT=evTop)
          IF sv EQ 'Yes' THEN BEGIN
            marked=prevMarked
            markedMulti=prevMarkedMulti
            WIDGET_CONTROL, btnUseMulti, SET_BUTTON=1
            IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
            fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
            WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(N_ELEMENTS(sel)-1), SET_LIST_TOP=oldTop
          ENDIF
        ENDIF
      END

      ;***************************************************************************************************************
      ;******************************** TESTS *******************************************************************************
      ;***************************************************************************************************************

      ;---- Quick Test--------------------------------------------------------------------------------------------------
      'runMulti': BEGIN
        calculateQuickTest
        updatePlot,1,1,0
      END

      'expMulti': exportMulti

      'manageQTout': QTexportSetup, GROUP_LEADER = ev.TOP, xoffset+100, yoffset+100

      ;---- tab CT/Dim--------------------------------------------------------------------------------------------------

      'dim': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          tempImg=activeImg
          szImg=SIZE(tempImg,/DIMENSIONS)

          pix=structImgs.(sel).pix
          nImg=N_ELEMENTS(tags)

          imgCenterOffset=[0,0,0,0]
          IF dxya(3) EQ 1 THEN imgCenterOffset=dxya
          center=szImg/2+imgCenterOffset[0:1]

          resArr=FLTARR(6+8,nImg); Horizontal 1, 2 , vertical 1, 2, (difference from nominal 50mm), diagonal 1, 2 + x/y (#pix) for 4 rods (UR, UL, LL, LR)
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errStatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
              pix=structImgs.(i).pix

              res=get_dim(tempImg, center, pix(0))
              IF res.status EQ 1 THEN BEGIN
                resArr[0:5,i]=res.dists
                resArr[6:9,i]=TRANSPOSE(res.centers[0,*]/pix(0));x positions of rods
                resArr[10:13,i]=TRANSPOSE(res.centers[1,*]/pix(1));y positions of rods
              ENDIF ELSE BEGIN
                resArr[0:13,i]=-1
                errStatus=errStatus+1
              ENDELSE
            ENDIF

            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=' '
          IF errStatus GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of rods for '+STRING(errStatus, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Values set to -1.', DIALOG_PARENT=evTop)
          results(getResNmb(modality,'DIM',analyseStringsAll))=1
          dimRes=resArr
          updateTable
          WIDGET_CONTROL, drawPlot, GET_VALUE=iDrawPlot & iDrawPlot.erase
          redrawImg,0,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF; empty
      END

      ;----analyse tab STP--------------------------------------------------------------------------------------------------
      'ddlRQA': BEGIN
        WIDGET_CONTROL, txtRQA, SET_VALUE=STRING(Qvals(ev.index), FORMAT='(i0)')
        clearRes
      END

      'STPpix': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          STPpix; tests_forQuickTest.pro
          updateTable
          WIDGET_CONTROL, drawPlot, GET_VALUE=iDrawPlot & iDrawPlot.erase;updatePlot, 0,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0

        ENDIF
      END

      'impDose': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF results(getResNmb(modality,'STP',analyseStringsAll)) THEN BEGIN
            nImg=N_TAGS(structImgs)
            markedArr=INTARR(nImg)
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1

            box=[$
              '1, BASE,, /COLUMN', $
              '2, BUTTON, .txt file|clipboard, EXCLUSIVE, LABEL_TOP=Import from..., COLUMN, SET_VALUE=1, TAG=from', $
              '1, BASE,, /ROW', $
              '0, BUTTON, OK, QUIT, TAG=OK',$
              '2, BUTTON, Cancel, QUIT']
            res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select how to import doses', XSIZE=300, YSIZE=200, FOCUSNO=3, XOFFSET=xoffset+400, YOFFSET=yoffset+200)

            IF res.OK THEN BEGIN
              CASE res.from OF

                0: BEGIN ;txt file
                  OPENR, filenhet, DIALOG_PICKFILE(DIALOG_PARENT=evTop, PATH=defPath), /GET_LUN
                  elem=''
                  doses=[0.]
                  WHILE ~ EOF(filenhet) DO BEGIN
                    READF, filenhet, elem
                    doses=[doses,FLOAT(STRJOIN(STRSPLIT(elem,',',/EXTRACT),'.'))]
                  ENDWHILE
                  CLOSE, filenhet
                  FREE_LUN, filenhet
                  IF N_ELEMENTS(doses) GT 1 THEN doses=doses[1:N_elements(doses)-1]
                END

                1: BEGIN; clipboard
                  clipres=CLIPBOARD.GET()
                  nElem=N_ELEMENTS(clipRes)
                  IF nElem GT 0 THEN BEGIN
                    dottedRes=STRARR(nElem)
                    FOR i=0, nElem-1 DO dottedRes(i)=STRJOIN(STRSPLIT(clipres(i),',',/EXTRACT),'.')
                    notEmpt=WHERE(dottedRes NE '')
                    IF notEmpt(0) NE -1 THEN doses = FLOAT(dottedRes(notEmpt))
                  ENDIF
                END
              ENDCASE

              IF N_ELEMENTS(doses) EQ TOTAL(markedArr) THEN BEGIN
                WIDGET_CONTROL, txtRQA, GET_VALUE=Qvalue
                Qvalue=LONG(Qvalue(0))

                cc=0
                FOR i=0, nImg-1 DO BEGIN
                  IF markedArr(i) THEN BEGIN
                    stpRes.table[0,i]=doses(cc)
                    stpRes.table[1,i]=doses(cc)*Qvalue
                    cc=cc+1
                  ENDIF
                ENDFOR

                updateTable
                updatePlot, 1,1,3
                WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
              ENDIF ELSE sv=DIALOG_MESSAGE('Mismatch between number of pixelvalues (N= '+STRING(TOTAL(markedArr), FORMAT='(i0)')+' and imported doses '+STRING(N_ELEMENTS(doses),FORMAT='(i0)')+'.', DIALOG_PARENT=evTop)
            ENDIF;res.ok
          ENDIF ELSE sv=DIALOG_MESSAGE('Get pixel values first.', DIALOG_PARENT=evTop)
        ENDIF
      END

      'calcSTP': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF results(getResNmb(modality,'STP',analyseStringsAll)) THEN BEGIN
            xvals=stpRes.table[1,*]
            yvals=stpRes.table[2,*]
            IF marked(0) NE -1 THEN BEGIN
              Qs=xvals(marked)
              pixvals=yvals(marked)
            ENDIF ELSE BEGIN
              Qs=TRANSPOSE(xvals)
              pixvals=TRANSPOSE(yvals)
            ENDELSE

            a0=REGRESS(Qs, pixvals, MCORRELATION=mcorr, CONST=const)
            stpRes=CREATE_STRUCT('table',stpRes.table, 'a', a0, 'b', const,'mcorr',mcorr)

            updateTable
            updatePlot, 0,0,0
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
          ENDIF ELSE sv=DIALOG_MESSAGE('Get pixel values first.', DIALOG_PARENT=evTop)
        ENDIF
      END

      ;----analyse tab Homogeneity--------------------------------------------------------------------------------------------------
      'homog': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          homog; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,3
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
          IF modality EQ 3 THEN BEGIN
            updatePlot, 1,1,0
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1; PET showing slice-to-slice variation as plot rather than table
          ENDIF
        ENDIF
      END

      'cw_HomogAltX': BEGIN
        IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 0 THEN updateTable
        ENDIF
      END
      ;----analyse tab Noise--------------------------------------------------------------------------------------------------
      'noise': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          noise; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          IF modality EQ 0 THEN WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1 ELSE WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      ;----analyse tab HU water--------------------------------------------------------------------------------------------------
      'HUwater': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          HUwater; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      ;----analyse tab ROI--------------------------------------------------------------------------------------------------
      'ROI_CT': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          ROI; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      'typeROI':  BEGIN
        IF ev.SELECT EQ 1 AND tags(0) NE 'EMPTY' THEN BEGIN
          redrawImg, 0,0
        ENDIF
      END

      'ROI_X': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          ROI; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      'typeROIX':  BEGIN
        IF ev.SELECT EQ 1 AND tags(0) NE 'EMPTY' THEN BEGIN
          redrawImg, 0,0
        ENDIF
      END

      'ROI_MR': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          ROI; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      'typeROIMR':  BEGIN
        IF ev.SELECT EQ 1 AND tags(0) NE 'EMPTY' THEN BEGIN
          redrawImg, 0,0
        ENDIF
      END

      ;-----analyse-tab header info
      'headerInfoCT':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          getHeaderInfo; tests_forQuickTest.pro
          updateTable
          updatePlot,1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      'EXP':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          getExposure; tests_forQuickTest.pro
          updateTable
          updatePlot,1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      'dcmMR':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          getDCM_MR; tests_forQuickTest.pro
          updateTable
          updatePlot,1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

;      'posMR':BEGIN
;        IF tags(0) NE 'EMPTY' THEN BEGIN
;          getPos_MR; tests_forQuickTest.pro
;          updateTable
;          updatePlot,1,1,0
;          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
;        ENDIF
;      END

      ;-----analyse-tab MTF --------------------------------------------------------------------------------------------------------------------
      'MTF': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          mtf; tests_forQuickTest.pro
          updateTable
          updatePlot,1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
        ENDIF; empty
      END

      'searchMaxMTF_ROI': BEGIN
        IF analyse EQ 'MTF' AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN clearRes, 'MTF'
        redrawImg,0,0
      END

      'cw_plotMTF':  BEGIN
        IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          updatePlot, 1,1,0;IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 1 THEN updatePlot, 1,1,0
          IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 3 THEN updateTableSup
        ENDIF
      END

      'cw_tableMTF':  BEGIN
        IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 0 THEN updateTable
        ENDIF
      END

      'cw_cyclMTF':  BEGIN
        IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          updatePlot, 1,1,0
          IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 0 THEN updateTable
        ENDIF
      END

      'MTFX': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          mtfx; tests_forQuickTest.pro
          updateTable
          updatePlot,1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
        ENDIF; empty
      END

      'MTF3dSPECT': BEGIN
        IF analyse EQ 'MTF' AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN clearRes, 'MTF'
      END

      'MTFNM': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS
          IF modality EQ 2 THEN BEGIN; NM planar
            WIDGET_CONTROL, txtMTFroiSzXNM, GET_VALUE=ROIszX
            WIDGET_CONTROL, txtMTFroiSzYNM, GET_VALUE=ROIszY
            WIDGET_CONTROL, cw_typeMTFNM, GET_VALUE=typeMTF
            WIDGET_CONTROL, txtCutLSFWNM, GET_VALUE=cutLSFW
            cutLSF=WIDGET_INFO(btnCutLSFNM, /BUTTON_SET)
            v3d=0
          ENDIF ELSE BEGIN;
            WIDGET_CONTROL, txtMTFroiSzSPECT, GET_VALUE=ROIszX
            ROIszY=ROIszX
            WIDGET_CONTROL, cw_typeMTFSPECT, GET_VALUE=typeMTF
            WIDGET_CONTROL, txtCutLSFWSPECT, GET_VALUE=cutLSFW
            cutLSF=WIDGET_INFO(btnCutLSFSPECT, /BUTTON_SET)
            v3d=WIDGET_INFO(MTF3dSPECT, /BUTTON_SET)
          ENDELSE

          nImg=N_TAGS(structImgs)
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          markedTemp=WHERE(markedArr EQ 1)
          first=markedTemp(0)
          tempImg=readImg(structImgs.(first).filename, structImgs.(first).frameNo)
          curPix=structImgs.(first).pix(0)

          szFirst=SIZE(tempImg, /DIMENSIONS)
          ROIszMM=FLOAT([ROIszX(0),ROIszY(0)])/2.
          ROIsz=ROIszMM/curPix
          halfSz=szFirst/2
          x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
          y1=ROUND(halfSz(1)+dxya(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(1))
          IF v3d THEN  y2 =y1+(x2-x1)

          CASE typeMTF OF

            0: BEGIN; point
              errStatus=0
              errSize=0
              errLogg=''
              FOR i=0, nImg-1 DO BEGIN
                IF markedArr(i) THEN BEGIN
                  ;check if same size
                  IF i GT 0 THEN BEGIN
                    tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                  ENDIF
                  szImg=SIZE(tempImg, /DIMENSIONS)
                  IF ARRAY_EQUAL(szImg[0:1], szFirst[0:1]) THEN BEGIN

                    curPix=structImgs.(i).pix(0)
                    ROIszMM=FLOAT([ROIszX(0),ROIszY(0)])/2.
                    ROIsz=ROIszMM/curPix
                    halfSz=szFirst/2
                    x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
                    y1=ROUND(halfSz(1)+dxya(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(1))

                    submatrix=tempImg[x1:x2,y1:y2]
                    backMatrix=0

                    MTF=calculateMTF_NM(submatrix, curPix, dxya[0:1], typeMTF, backMatrix, cutLSF, FLOAT(cutLSFW(0)), v3d)
                    IF MTF.errMsg NE '' THEN errLogg=errLogg+'Warning image #'+STRING(i+1, FORMAT='(i0)')+'. '+MTF.errMsg+newline
                    IF MTF.status EQ 0 THEN errStatus=errStatus+1
                  ENDIF ELSE BEGIN
                    MTF=CREATE_STRUCT('empty',0)
                    errSize=1
                  ENDELSE
                ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
                IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
              ENDFOR
              IF errStatus GT 0 THEN BEGIN
                errLogg='Problem finding center of pointsource for '+STRING(errStatus, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Point position assumed to be at the selected ROI center.'+newline+errLogg
                IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
              ENDIF
              If errSize THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
              results(getResNmb(modality,analyse,analyseStringsAll))=1
              updateTable
              updatePlot, 1,1,0
              WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
            END

            1: BEGIN; line (in plane or z dir)

              ;IF v3d THEN y2=y1+x2-x1;ensure quadratic for v3d
              nnImg=TOTAL(markedArr)
              subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

              proceed=1
              counter=0

              errLogg=''
              FOR i=0, nImg-1 DO BEGIN
                IF markedArr(i) THEN BEGIN
                  tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                  szThis=SIZE(tempImg, /DIMENSIONS)
                  IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                    subM[*,*,counter]=tempImg[x1:x2,y1:y2]
                    proceed=1
                    counter=counter+1
                  ENDIF ELSE BEGIN
                    MTF=CREATE_STRUCT('empty',0)
                    errLogg=errLogg+'Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.'+newline
                    proceed=0
                  ENDELSE
                ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
                IF proceed EQ 0 THEN BREAK
              ENDFOR
              IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)

              IF proceed THEN BEGIN

                errLogg=''
                IF v3d THEN BEGIN; line in z dir
                  MTFres=calculateMTF_NM(subM, curPix, dxya[0:1], typeMTF, -1, cutLSF, FLOAT(cutLSFW(0)), v3d)
                  IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding location of line for one or more images. Location assumed to be at center of ROI.', DIALOG_PARENT=evTop)
                ENDIF ELSE BEGIN; line in plane
                  counter=0
                  FOR i=0, nImg-1 DO BEGIN
                    IF markedArr(i) THEN BEGIN
                      MTF=calculateMTF_NM(subM[*,*,counter], curPix, dxya[0:1], typeMTF, -1, cutLSF, FLOAT(cutLSFW(0)), v3d)
                      IF MTF.errMsg NE '' THEN errLogg=errLogg+'Warning image #'+STRING(i+1, FORMAT='(i0)')+'. '+MTF.errMsg+newline
                      counter=counter+1
                    ENDIF ELSE MTF=CREATE_STRUCT('empty',0)

                    IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
                  ENDFOR
                ENDELSE

                IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
                results(getResNmb(modality,analyse,analyseStringsAll))=1
                updateTable
                updatePlot,1,1,0
                WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
              ENDIF
            END

            2: BEGIN ;straight edge
              szFirst=SIZE(tempImg, /DIMENSIONS)

              subMatrix=FLTARR(x2-x1+1,y2-y1+1)

              errLogg=''
              FOR i=0, nImg-1 DO BEGIN
                IF markedArr(i) THEN BEGIN
                  tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                  szThis=SIZE(tempImg, /DIMENSIONS)
                  IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                    submatrix[*,*]=tempImg[x1:x2,y1:y2]
                    MTF=calculateMTF_NM(submatrix, curPix, dxya[0:1], typeMTF, -1,cutLSF, FLOAT(cutLSFW(0)), v3d)
                    IF MTF.errMsg NE '' THEN errLogg=errLogg+'Warning image #'+STRING(i+1, FORMAT='(i0)')+'. '+MTF.errMsg+newline
                  ENDIF ELSE BEGIN
                    MTF=CREATE_STRUCT('empty',0)
                    errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.'+newline
                  ENDELSE

                ENDIF ELSE MTF=CREATE_STRUCT('empty',0)

                IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
              ENDFOR
              IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
              results(getResNmb(modality,analyse,analyseStringsAll))=1
              updateTable
              updatePlot,1,1,0
              WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1


            END

            3: BEGIN ; circular edge (2d or 3d)

              nnImg=TOTAL(markedArr)
              subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

              proceed=1
              counter=0
              errLogg=''
              FOR i=0, nImg-1 DO BEGIN
                IF markedArr(i) THEN BEGIN
                  tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                  szThis=SIZE(tempImg, /DIMENSIONS)
                  IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                    subM[*,*,counter]=tempImg[x1:x2,y1:y2]
                    proceed=1
                    counter=counter+1
                  ENDIF ELSE BEGIN
                    MTF=CREATE_STRUCT('empty',0)
                    errLogg=errLogg+'Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.'+newline
                    proceed=0
                  ENDELSE
                ENDIF
                IF proceed EQ 0 THEN BREAK
              ENDFOR
              IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)

              IF proceed THEN BEGIN
                IF v3d THEN BEGIN
                  MTFres=calculateMTF_NM(subM, curPix, dxya[0:1], typeMTF, -1, cutLSF, FLOAT(cutLSFW(0)), v3d)
                  IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for one or more images. Center of circle assumed to be at center of ROI.', DIALOG_PARENT=evTop)
                ENDIF ELSE BEGIN
                  counter=0
                  errStatus=0
                  FOR i=0, nImg-1 DO BEGIN
                    IF markedArr(i) THEN BEGIN
                      MTF=calculateMTF_NM(subM[*,*,counter], curPix, dxya[0:1], typeMTF, -1, cutLSF, FLOAT(cutLSFW(0)), v3d)
                      IF MTF.status EQ 0 THEN errStatus=errStatus+1
                      counter=counter+1
                    ENDIF ELSE MTF=CREATE_STRUCT('empty',0)

                    IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
                  ENDFOR
                  IF errStatus GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for '+STRING(errStatus, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Circle center assumed to be at the selected ROI center.', DIALOG_PARENT=evTop)
                ENDELSE

                results(getResNmb(modality,analyse,analyseStringsAll))=1
                updateTable
                updatePlot,1,1,0
                WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
              ENDIF
            END
            ELSE: sv=DIALOG_MESSAGE('Not implementet selected MTF type yet',/INFORMATION, DIALOG_PARENT=evTop)
          ENDCASE
        ENDIF; empty
      END

      ;-----analyse-tab NPS --------------------------------------------------------------------------------------------------------------------
      'NPS': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS

          nImg=N_TAGS(structImgs)
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          markedTemp=WHERE(markedArr EQ 1)
          first=markedTemp(0)
          tempImg=readImg(structImgs.(first).filename, 0)
          szFirst=SIZE(tempImg, /DIMENSIONS)
          nnImg=TOTAL(markedArr)

          CASE modality OF
            0:BEGIN
              WIDGET_CONTROL, txtSmoothNPS, GET_VALUE=smNPS
              WIDGET_CONTROL, txtFreqNPS, GET_VALUE=sampFreq
              smNPSw=0.5*FLOAT(smNPS(0))
              sampFreq=FLOAT(sampFreq(0))
            END
            1:BEGIN
              WIDGET_CONTROL, txtNPSroiSzX, GET_VALUE=ROIsz
              WIDGET_CONTROL, txtNPSsubSzX, GET_VALUE=nsubSz
              ROIsz=LONG(ROIsz(0)) & nsubSz=LONG(nsubSz(0))
              subSz=ROIsz*nsubSz
              halfSz=szFirst/2
              x1=ROUND(halfSz(0)+dxya(0)-subSz/2) & x2=ROUND(halfSz(0)+dxya(0)+subSz/2)
              y1=ROUND(halfSz(1)+dxya(1)-subSz/2) & y2=ROUND(halfSz(1)+dxya(1)+subSz/2)
              subMatrix=FLTARR(x2-x1+1,y2-y1+1);,nnImg)
              IF N_ELEMENTS(stpRes) EQ 0 THEN stpRes=0
            END
            ELSE:
          ENDCASE

          proceed=1
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              tempImg=readImg(structImgs.(i).filename, 0)
              szThis=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                CASE modality OF
                  0:
                  1: submatrix[*,*]=tempImg[x1:x2,y1:y2]
                  ELSE:
                ENDCASE
                proceed=1
              ENDIF ELSE BEGIN
                NPS=CREATE_STRUCT('empty',0)
                sv=DIALOG_MESSAGE('Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate NPS separately for images with the same size.',/INFORMATION, DIALOG_PARENT=evTop)
                proceed=0
              ENDELSE
            ENDIF ELSE BEGIN
              proceed=0
              NPS=CREATE_STRUCT('empty',0)
            ENDELSE
            IF proceed EQ 1 THEN BEGIN
              CASE modality OF
                0: NPS=calculateNPS(tempImg, NPSrois, structImgs.(first).pix, smNPSw, sampFreq)
                1: NPS=calculateNPS_xray(submatrix, ROIsz, nsubSz, structImgs.(first).pix, stpRes, i)
                ELSE:
              ENDCASE
            ENDIF
            IF i EQ 0 THEN NPSres=CREATE_STRUCT('N0',NPS) ELSE NPSres=CREATE_STRUCT(NPSres, 'N'+STRING(i, FORMAT='(i0)'), NPS)
          ENDFOR

          results(getResNmb(modality,analyse,analyseStringsAll))=1
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
        ENDIF; empty
      END

      ;------------------- Variance image-------------------
      'varImage': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS

          nImg=N_TAGS(structImgs)
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          markedTemp=WHERE(markedArr EQ 1)
          nnImg=TOTAL(markedArr)

          sel=WIDGET_INFO(listFiles, /LIST_SELECT)

          ;first uneditable defaults: 2x2mm (IPEM, recommended 10x10 pix, for xray 2x2 mm, mammo 0.7x0.7mm)
          WIDGET_CONTROL, txtVarImageROIsz, GET_VALUE=ROIszMM

          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              pix=structImgs.(i).pix
              szROI=ROUND(ROIszMM(0)/pix(0))
              IF szROI LT 3 THEN szROI=3
              tempImg=readImg(structImgs.(i).filename, 0)
              tempImg=DOUBLE(tempImg);^2 might need that to not get to limit of floating point numbers
              ;code adapted from: https://www.imageeprocessing.com/2015/10/edge-detection-using-local-variance.html
              kernelSmooth=FLTARR(szROI,szROI)+(1./szROI^2)
              mu=CONVOL(tempImg,kernelSmooth,CENTER=0)
              ii=CONVOL(tempImg^2,kernelSmooth,CENTER=0)
              varianceImg=ii-mu^2
            ENDIF ELSE varianceImg=0
            IF i EQ 0 THEN varImgRes=CREATE_STRUCT('V0',varianceImg) ELSE varImgRes=CREATE_STRUCT(varImgRes, 'V'+STRING(i, FORMAT='(i0)'), varianceImg)
          ENDFOR

          results(getResNmb(modality,analyse,analyseStringsAll))=1
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=2
          ;updateImageRes

        ENDIF; no images
      END

      ;-----analyse tab CT number Linearity-----------------------------------------------------------------------------------------
      'Linearity': BEGIN; exctract results

        IF tags(0) NE 'EMPTY' THEN BEGIN
          ctlin; tests_forQuickTest.pro

          updateTable
          updatePlot, 1,1,0
          redrawImg,0,0
        ENDIF
      END

      'linAvoidSearch': BEGIN
        clearRes, 'CTLIN'
        CTlinROIpos=0
        redrawImg, 0,0
      END

      'impLinTab': BEGIN

        box=[$
          '1, BASE,, /COLUMN', $
          '2, BUTTON, predefined Catphan setups|clipboard, EXCLUSIVE, LABEL_TOP=Import table from..., COLUMN, SET_VALUE=1, TAG=from', $
          '1, BASE,, /ROW', $
          '0, BUTTON, OK, QUIT, TAG=OK',$
          '2, BUTTON, Cancel, QUIT']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select how to import the table', XSIZE=300, YSIZE=200, FOCUSNO=3, XOFFSET=xoffset+400, YOFFSET=yoffset+200)

        IF res.OK THEN BEGIN
          clipres=''
          nElem=0
          IF res.from EQ 1 THEN BEGIN
            clipres=CLIPBOARD.GET()
            nElem=N_ELEMENTS(clipRes)
            IF clipres(-1) EQ '' THEN BEGIN
              clipRes=clipRes[0:nElem-2]
              nElem=nElem-1
            ENDIF
          ENDIF ELSE BEGIN
            adr=DIALOG_PICKFILE(TITLE='Load table from .txt file with Catphan standard setup',/READ, FILTER='*.txt', /FIX_FILTER, PATH=thisPath+'data\CatphanSensitometry\', DIALOG_PARENT=evTop)
            IF adr(0) NE '' THEN BEGIN
              OPENR, filenhet, adr(0), /GET_LUN
              elem=''
              WHILE ~ EOF(filenhet) DO BEGIN
                READF, filenhet, elem
                clipres=[clipres,elem]
              ENDWHILE
              CLOSE, filenhet
              FREE_LUN, filenhet
              IF N_ELEMENTS(clipres) GT 1 THEN clipres=clipres[1:N_elements(clipres)-1]
              nElem=N_ELEMENTS(clipres)
            ENDIF
          ENDELSE

          IF nElem LE 8 THEN BEGIN
            IF nElem GT 0 THEN BEGIN
              newTab=STRARR(4,nElem)
              test=STRSPLIT(clipres(0),STRING(9B),/EXTRACT)
              IF N_ELEMENTS(test) EQ 4 THEN BEGIN
                FOR i=0, nElem-1 DO newTab[*,i]=STRSPLIT(STRJOIN(STRSPLIT(clipres(i),',',/EXTRACT),'.'),STRING(9B),/EXTRACT)
                floatings=FLOAT(newTab[1:3,*])
                newTab[1:2,*]=STRING(floatings[0:1,*], FORMAT='(f0.1)')
                newTab[3,*]=STRING(floatings[2,*], FORMAT='(f0.3)')
                WIDGET_CONTROL, tblLin, SET_VALUE=newTab,  TABLE_YSIZE=nElem, SET_TABLE_SELECT=[-1,-1,-1,-1], SET_TABLE_VIEW=[0,0]
                tableHeaders=updateMaterialHeaders(tableHeaders, TRANSPOSE(newTab[0,*]))
                clearRes, 'CTLIN'
                redrawImg, 0,0
              ENDIF ELSE sv=DIALOG_MESSAGE('Expecting four columns to paste from clipboard (tested using Excel).', DIALOG_PARENT=evTop)

            ENDIF ELSE sv=DIALOG_MESSAGE('Nothing in clipboard or file to import', DIALOG_PARENT=evTop)
          ENDIF ELSE sv=DIALOG_MESSAGE('Maximum 8 rows possible. Sorry - too messy result-table with more than 8 materials.', DIALOG_PARENT=evTop)
        ENDIF
      END

      'copyLinTab': BEGIN
        WIDGET_CONTROL, tblLin, GET_VALUE=resTable
        szT=SIZE(resTable, /DIMENSIONS)
        IF N_ELEMENTS(szT) EQ 2 THEN BEGIN
          FOR i=0, szT(0)-1 DO BEGIN
            FOR j=0, szT(1)-1 DO BEGIN
              resTable[i,j]=STRJOIN(STRSPLIT(resTable[i,j], '.',/EXTRACT),',')
            ENDFOR
          ENDFOR
        ENDIF ELSE BEGIN
          FOR i=0, szT(0)-1 DO resTable[i]=STRJOIN(STRSPLIT(resTable[i], '.',/EXTRACT),',')
        ENDELSE
        CLIPBOARD.set, STRJOIN(resTable, STRING(9B))
      END

      'addRowLinTab':BEGIN
        WIDGET_CONTROL, tblLin, GET_VALUE=oldTab
        szOld=SIZE(oldTab, /DIMENSIONS)
        IF szOld(1) LT 8 THEN BEGIN
          newTab=STRARR(szOld(0), szOld(1)+1)
          newTab[*,0:szOld(1)-1]=oldTab
          newTab[*,szOld(1)]=['<new>','0','0','0']
          WIDGET_CONTROL, tblLin, SET_VALUE=newTab, TABLE_YSIZE=szOld(1)+1, SET_TABLE_VIEW=[0,MAX([szOld(1)-5,0])]
          tableHeaders=updateMaterialHeaders(tableHeaders, TRANSPOSE(newTab[0,*]))
          redrawImg,0,0
        ENDIF ELSE sv=DIALOG_MESSAGE('Maximum 8 ROIs possible. Sorry - lazy programmer.', DIALOG_PARENT=evTop)
      END

      'delRowLinTab':BEGIN
        sel=WIDGET_INFO(tblLin, /TABLE_SELECT)
        IF MAX(sel) NE -1 THEN BEGIN;any selected?
          sv=DIALOG_MESSAGE('Delete selected row(s)?',/QUESTION, DIALOG_PARENT=evTop)
          IF sv EQ 'Yes' THEN BEGIN
            WIDGET_CONTROL, tblLin, GET_VALUE=oldTab
            szOld=SIZE(oldTab, /DIMENSIONS)
            nDel=sel(3)-sel(1)+1; btm-top+1
            IF nDel EQ szOld(1) THEN newTab=STRARR(4) ELSE BEGIN
              newTab=STRARR(4, szOld(1)-nDel)
              counter=0
              FOR i=0, szOld(1)-1 DO BEGIN
                IF i LT sel(1) OR i GT sel(3) THEN BEGIN
                  newTab[*,counter]=oldTab[*,i]
                  counter=counter+1
                ENDIF
              ENDFOR
            ENDELSE
            szNew=SIZE(newTab, /DIMENSIONS)
            WIDGET_CONTROL, tblLin, SET_VALUE=newTab, TABLE_YSIZE=szNew(1), SET_TABLE_VIEW=[0,0]
            tableHeaders=updateMaterialHeaders(tableHeaders, TRANSPOSE(newTab[0,*]))
            clearRes, 'CTLIN'
            redrawImg,0,0
          ENDIF
        ENDIF
      END

      'centerLinTab': BEGIN; set last mouse-click in image to center of selected material
        selT=WIDGET_INFO(tblLin, /TABLE_SELECT)
        IF MAX(selT) NE -1 THEN BEGIN;any selected?
          selRow=selT(1)
          tempImg=activeImg
          imsz=SIZE(tempImg, /DIMENSIONS)
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          pix=structImgs.(sel).pix
          IF lastXYreleased(0) EQ -1 THEN posNew = imsz/2 ELSE posNew=lastXYreleased;*max(imsz)/drawXY
          posNew=(posNew-dxya[0:1]-imsz/2)*pix; relative to defined centerposition
          ;update table
          WIDGET_CONTROL, tblLin, GET_VALUE=oldTab
          oldTab[1:2,selRow]=STRING(posNew, FORMAT='(f0.1)')
          WIDGET_CONTROL, tblLin, SET_VALUE=oldTab
          clearRes, 'CTLIN'
          redrawImg,0,0
        ENDIF ELSE sv=DIALOG_MESSAGE('No row in table selected.', DIALOG_PARENT=evTop)
      END

      ;----analyse tab Slice Thickness--------------------------------------------------------------------------------------------------
      'SliceThick': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          slicethick; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,3
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      ;----analyse tab FWHM--------------------------------------------------------------------------------------------------
      'fwhm': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS
          ;analyse='FWHM'
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          tempImg=activeImg
          szImg=SIZE(tempImg,/DIMENSIONS)

          pix=structImgs.(sel).pix
          nImg=N_ELEMENTS(tags)

          imgCenterOffset=[0,0,0,0]
          IF dxya(3) EQ 1 THEN imgCenterOffset=dxya
          center=szImg/2+imgCenterOffset[0:1]

          resArr=FLTARR(3,nImg); mean, stdev all circles
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errStatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
              imszTemp=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(imszTemp[0:1], szImg[0:1]) THEN BEGIN
                res=get_fwhm(tempImg, center, pix(0))
                IF N_ELEMENTS(res) GT 1 THEN resArr[*,i]=res ELSE resArr[*,i]=-1
              ENDIF ELSE errStatus=1
            ENDIF
            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=' '
          If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
          results(getResNmb(modality,analyse,analyseStringsAll))=1
          fwhmRes=resArr
          updateTable
          WIDGET_CONTROL, drawPlot, GET_VALUE=iDrawPlot & iDrawPlot.erase
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF; empty
      END

      ; NM tests
      'uniformityNM': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          uniformityNM; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END
      'unifCorrSet': BEGIN
        IF results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Calculate uniformity to update with new setting.', DIALOG_PARENT=evTop)
          clearRes, 'UNIF'
        ENDIF
      END
      'cw_posfitUnifCorr':BEGIN
        IF results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Calculate uniformity to update with new setting.', DIALOG_PARENT=evTop)
          clearRes, 'UNIF'
        ENDIF
      END
      'unifCorrLockRad':BEGIN
        IF results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Calculate uniformity to update with new setting.', DIALOG_PARENT=evTop)
          clearRes, 'UNIF'
        ENDIF
        IF WIDGET_INFO(btnLockRadUnifCorr, /BUTTON_SET) THEN BEGIN;turned on - if empty fill
          WIDGET_CONTROL, txtLockRadUnifCorr, GET_VALUE=currRadStr
          IF currRadStr EQ '' THEN BEGIN
            nImg=N_TAGS(structImgs)
            IF nImg GT 0 THEN radDef=MEAN([structImgs.(0).radius1,structImgs.(0).radius2]) ELSE radDef=100.
            WIDGET_CONTROL, txtLockRadUnifCorr, SET_VALUE=STRING(radDef, FORMAT='(f0.1)')
          ENDIF
        ENDIF
      END

      'SNICorrSet': BEGIN
        IF results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Calculate uniformity to update with new setting.', DIALOG_PARENT=evTop)
          clearRes, 'SNI'
        ENDIF
      END
      'cw_posfitSNICorr': BEGIN
        IF results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Calculate uniformity to update with new setting.', DIALOG_PARENT=evTop)
          clearRes, 'SNI'
        ENDIF
      END
      'SNICorrLockRad': BEGIN
        IF results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          sv=DIALOG_MESSAGE('Calculate uniformity to update with new setting.', DIALOG_PARENT=evTop)
          clearRes, 'SNI'
        ENDIF
        IF WIDGET_INFO(btnLockRadSNICorr, /BUTTON_SET) THEN BEGIN;turned on - if empty fill
          WIDGET_CONTROL, txtLockRadSNICorr, GET_VALUE=currRadStr
          IF currRadStr EQ '' THEN BEGIN
            nImg=N_TAGS(structImgs)
            IF nImg GT 0 THEN radDef=MEAN([structImgs.(0).radius1,structImgs.(0).radius2]) ELSE radDef=100.
            WIDGET_CONTROL, txtLockRadSNICorr, SET_VALUE=STRING(radDef, FORMAT='(f0.1)')
          ENDIF
        ENDIF
      END

      'cw_plotUnif':BEGIN
        IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 1 THEN BEGIN
          IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN updatePlot, 1,1,0
        ENDIF
      END

      'cw_imgUnif':BEGIN
        IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 2 THEN BEGIN
          IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN updateImageRes
        ENDIF
      END

      'SNI': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          SNI; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      'cw_plotSNI':BEGIN
        IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN BEGIN
          IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 1 THEN updatePlot, 1,1,0
        ENDIF
      END

      'cw_imgSNI':BEGIN
        IF WIDGET_INFO(wTabResult, /TAB_CURRENT) EQ 2 THEN BEGIN
          IF ev.SELECT EQ 1 AND results(getResNmb(modality,analyse,analyseStringsAll)) EQ 1 THEN updateImageRes
        ENDIF
      END

      'BarNM': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          barNM; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      'AcqNM': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          getAcqNM; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END

      ;-------- NM energy spectrum ------------------
      'loadSpectrum': BEGIN
        adr=DIALOG_PICKFILE(TITLE='Open .txt file with spectrum data',/READ, FILTER='*.txt', /FIX_FILTER, PATH=defPath, GET_PATH=defPath, DIALOG_PARENT=evTop)
        IF adr NE '' THEN BEGIN
          ;get data
          OPENR, filenhet, adr, /GET_LUN
          elem=''
          READF, filenhet, elem; first line no data
          elems=[0.0,0.0]
          WHILE ~ EOF(filenhet) DO BEGIN
            READF, filenhet, elem
            elem=STRJOIN(STRSPLIT(elem,',',/EXTRACT),'.'); change , to .
            elem=FLOAT(STRSPLIT(elem, STRING(9B), /EXTRACT))
            IF N_ELEMENTS(elem) LT 2 THEN BEGIN
              sv=DIALOG_MESSAGE('File not in expected format: Tabular separated, 2 or 3 columns', DIALOG_PARENT=evTop)
              BREAK
            ENDIF
            IF N_ELEMENTS(elem) EQ 2 THEN elems=[[elems],[elem[0:1]]] ELSE elems=[[elems],[elem[1:2]]]
            elem=''
          ENDWHILE
          CLOSE, filenhet
          FREE_LUN, filenhet

          IF MAX(elems) GT 0 THEN BEGIN
            ;plot/analyse data
            gfit=gaussfit(elems[0,*],elems[1,*], coeff, NTERMS=3)
            energyRes=CREATE_STRUCT('gaussCoeff',coeff,'curve',elems[0:1,*])
            ;analyse='ENERGYSPEC'
            results(getResNmb(modality,analyse,analyseStringsAll))=1
            updateTable
            updatePlot, 1,1,3
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
          ENDIF ELSE BEGIN
            sv=DIALOG_MESSAGE('Found no values from the selected file.', DIALOG_PARENT=evTop)
            clearRes, 'ENERGYSPEC'
          ENDELSE

        ENDIF;adr ne ''
      END

      'radialProfile': BEGIN

        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS

          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          pix=structImgs.(sel).pix
          distCenter=activeImg*0.0
          szM=SIZE(activeImg, /DIMENSIONS)
          centerPos=0.5*szM[0:1]+dxya[0:1]*dxya(3)
          FOR i=0, szM(0)-1 DO BEGIN
            FOR j=0, szM(1)-1 DO BEGIN
              distCenter(i,j)=SQRT((i-centerPos(0))^2+(j-centerPos(1))^2)
            ENDFOR
          ENDFOR
          sorting=SORT(distCenter)
          dists=distCenter(sorting)

          nImg=N_ELEMENTS(tags)
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1

          ;rebin to equally spaced resolution pix/10
          radius=MIN([centerPos(0),szM(0)-centerPos(0),centerPos(1), szM(1)-centerPos(1)])-1; maximim radius with full dataset
          newdists=FINDGEN(radius*10)*0.1*pix(0); regular x axis, cuts data at position where start to lose info due to less data in perpendicular directions
          pixNew=.1*pix(0)
          ;smooth by ~the new pix size if possible
          test=WHERE(dists*pix(0) LT max(newdists))
          smoothSz=ROUND(N_ELEMENTS(test)/N_ELEMENTS(newdists))

          radialRes=FLTARR(N_ELEMENTS(newdists), nImg)
          errStatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
              imszTemp=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(imszTemp[0:1], szM[0:1]) THEN BEGIN
                If smoothSz GT 2 THEN pixVals=SMOOTH(tempImg(sorting),smoothSz) ELSE pixVals=tempImg(sorting)
                radialRes[*,i]=INTERPOL(pixVals, dists*pix(0), newdists); linear interpolation
                radialRes[0:9,i]=radialRes[10,i];don't trust first 10 values (first original pix)
              ENDIF ELSE errStatus=1
            ENDIF
            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'

          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=' '
          If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
          ;analyse='RADIAL'
          results(getResNmb(modality,'RADIAL',analyseStringsAll))=1
          updateTable
          updatePlot, 1,1,0
          ;redrawImg, 0,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
        ENDIF
      END

      'plotScanSpeed':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS
          results(getResNmb(modality,'SCANSPEED',analyseStringsAll))=1
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
        ENDIF
      END

      'saveGeomMeanMatrix':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          sel2=sel
          IF structImgs.(sel).nFrames GE 2 THEN BEGIN
            WIDGET_CONTROL, /HOURGLASS
            ;assume first half of images is AP and secound half is PA
            nImg=N_TAGS(structImgs)

            ;TODO
            ;detectorVector=!Null
            ;nDetec=1
            ;FOR i=0, nImg-1 DO detectorVector=[detectorVector,structImgs.(i).detectorVector
            ;IF N_ELEMENTS(detectorVector) EQ nImg THEN BEGIN
            ; idfirst=WHERE(detectorVector EQ detectorVector(0), nEqual)
            ;IF nEqual EQ nImg/2 THEN BEGIN
            ; nDetec=2
            ;idLast=
            ;ENDIF
            ;ENDIF

            sv=DIALOG_MESSAGE('All frames (Yes) or just this frame-set (No)?',/QUESTION, DIALOG_PARENT=evTop)
            IF sv EQ 'No' THEN BEGIN
              img1=readImg(structImgs.(sel).filename, structImgs.(sel).frameNo)
              ; IF nDetec EQ 1 THEN BEGIN; detectorVector not known assume first half is first detector
              IF sel LT nImg/2 THEN sel2=nImg/2+sel ELSE sel2=sel-nImg/2
              ;ENDIF ELSE BEGIN
              ;
              ;ENDELSE
              img2=readImg(structImgs.(sel2).filename, structImgs.(sel2).frameNo)
              img2=ROTATE(img2,5);flip left/right

              geomMeanImg=SQRT(img1*img2)
              imageQCmatrix=CREATE_STRUCT(structImgs.(sel),'matrix', geomMeanImg)
              imageQCmatrix.SERIESNAME=imageQCmatrix.SERIESNAME+'_GEOMMEAN_by_imageQC'
              adr=DIALOG_PICKFILE(PATH=defPath, GET_PATH=defPath, TITLE='Save geometric mean image as IDL structure (.dat-file)',/WRITE, FILTER='*.dat', /FIX_FILTER, DEFAULT_EXTENSION='.dat', DIALOG_PARENT=evTop)
              IF adr NE '' THEN BEGIN
                ;check folder for write permissions
                fi=FILE_INFO(FILE_DIRNAME(adr))
                IF fi.write THEN BEGIN
                  imageQCmatrix.filename=adr; changed when opened so that renaming/moving file is possible
                  SAVE, imageQCmatrix, FILENAME=adr
                ENDIF ELSE sv=DIALOG_MESSAGE('You do not have writing permissions for the selected folder.',/ERROR, DIALOG_PARENT=evTop)
              ENDIF
            ENDIF ELSE BEGIN
              adr=DIALOG_PICKFILE(PATH=defPath, GET_PATH=defPath, TITLE='Save geometric mean image as IDL structure (.dat-files) - each frame will get suffix frameXXX',/WRITE, FILTER='*.dat', /FIX_FILTER, DEFAULT_EXTENSION='.dat', DIALOG_PARENT=evTop)
              IF adr NE '' THEN BEGIN
                ;check folder for write permissions
                fi=FILE_INFO(FILE_DIRNAME(adr))
                IF fi.write THEN BEGIN
                  sv=DIALOG_MESSAGE('Orientation as detector 1 (Yes) or 2 (No)?',/QUESTION, DIALOG_PARENT=evTop)
                  IF sv EQ 'Yes' THEN BEGIN
                    i1=0
                    i2=nImg/2
                  ENDIF ELSE BEGIN
                    i1=nImg/2
                    i2=0
                  ENDELSE

                  FOR i=0, nImg/2-1 DO BEGIN
                    img1=readImg(structImgs.(i+i1).filename, structImgs.(i+i1).frameNo)
                    nImg=N_TAGS(structImgs)
                    img2=readImg(structImgs.(i+i2).filename, structImgs.(i+i2).frameNo)
                    img2=ROTATE(img2,5);flip left/right

                    geomMeanImg=SQRT(img1*img2)
                    imageQCmatrix=CREATE_STRUCT(structImgs.(sel),'matrix', geomMeanImg)
                    imageQCmatrix.SERIESNAME=imageQCmatrix.SERIESNAME+'_GEOMMEAN_by_imageQC'
                    adrThis=adr.Insert('_frame'+STRING(i+1,FORMAT='(i03)'), -4)
                    imageQCmatrix.filename=adrThis; changed when opened so that renaming/moving file is possible
                    imageQCmatrix.frameNo=i+1
                    SAVE, imageQCmatrix, FILENAME=adrThis
                  ENDFOR
                ENDIF ELSE sv=DIALOG_MESSAGE('You do not have writing permissions for the selected folder.',/ERROR, DIALOG_PARENT=evTop)
              ENDIF

            ENDELSE


          ENDIF ELSE sv=DIALOG_MESSAGE('Geometric mean expect corresponding frames. Number of frames has to be at least two (per se - ',/ERROR, DIALOG_PARENT=evTop)
        ENDIF
      END

      ;      'defROITimeAct':BEGIN
      ;        IF tags(0) NE 'EMPTY' THEN BEGIN
      ;          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
      ;          tempStruct=structImgs.(sel)
      ;
      ;          WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
      ;          WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
      ;          rangeWL=LONG([lower,upper])
      ;          tvRes=readImg(tempStruct.filename, tempStruct.frameNo)
      ;          sv=DIALOG_MESSAGE('Draw on or more ROIs with the tool. Close the popup (for each ROI) and close the ROI tool. Then continue in ImageQC with the defined ROI.'+newline+'The ROI can also be saved and reopened with the same tool later on.', DIALOG_PARENT=evTop)
      ;          XROI, adjustWindowLevel(tvRes, rangeWL), /MODAL, GROUP=evTop, TITLE='Define ROI', REGIONS_OUT=definedROIs
      ;          timeActROI=0
      ;          IF ISA(definedROIs) THEN BEGIN
      ;            szImg=SIZE(tvRes, /DIMENSIONS)
      ;            nROIs=N_ELEMENTS(definedROIs)
      ;            timeActROI=INTARR(tvRes(0), tvRes(1), nROIs)
      ;            FOR i=0, nROIs-1 DO timeActROI[*,*,i]=definedROIs(0) -> ComputeMask(Dimensions=Size(tvRes, /Dimensions), Mask_Rule=2)
      ;          ENDIF
      ;        ENDIF
      ;      END
      ;      'timeActCurve':BEGIN
      ;        IF N_ELEMENTS(timeActROI) GT 1 THEN BEGIN
      ;          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
      ;          ;img same size as roi?
      ;          ;all frames same detector (detectorVector) ?
      ;          ;read img ( + oppsing image, flip and geom mean)
      ;          ;read timepoint
      ;          ;timeActResult
      ;
      ;        ENDIF ELSE sv=DIALOG_MESSAGE('ROI not defined.', DIALOG_PARENT=evTop)
      ;      END

      ;-----analyse tab contrast-----------------------------------------------------------------------------------------
      'contrastSPECT': BEGIN; exctract results
        IF tags(0) NE 'EMPTY' THEN BEGIN
          ;IF analyse NE 'CONTRAST' THEN sv=DIALOG_MESSAGE('Show ROIs first to verify size and positions.',/INFORMATION) ELSE BEGIN

          WIDGET_CONTROL, /HOURGLASS
          nImg=N_ELEMENTS(tags)
          szROI=SIZE(conROIs, /DIMENSIONS)

          resArr=FLTARR(szROI(2),nImg);minVals for all ROIs + background

          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errStatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
              imszTemp=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                FOR r=0, szROI(2)-1 DO BEGIN
                  maske=conROIs[*,*,r]
                  IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanVal, MINIMUM=minVal, MASK=maske
                  resArr(r,i)=minVal
                  IF r EQ szROI(2)-1 THEN resArr(r,i)=meanVal
                ENDFOR
              ENDIF ELSE errStatus=1
            ENDIF;markedArr
            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=' '
          If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
          contrastRes=resArr
          results(getResNmb(modality,analyse,analyseStringsAll))=1
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1

        ENDIF
      END

      ;----analyse tab Crosscalibration--------------------------------------------------------------------------------------------------
      'crossGetAct': BEGIN
        ;get administered activity and time from DICOM header
        IF tags(0) NE 'EMPTY' THEN BEGIN
          admA=structImgs.(0).admDose
          admT=structImgs.(0).admDoseTime
          IF admA NE '-' THEN BEGIN
            admA=FLOAT(admA)/(1e+6)
            WIDGET_CONTROL, txtCrossMeasAct, SET_VALUE=STRING(admA, FORMAT=formatCode(admA))
            admT=STRING(admT, FORMAT='(i06)')
            admHour=STRMID(admT,0,2)
            admMin=STRMID(admT,2,2)
            WIDGET_CONTROL, txtCrossMeasActT, SET_VALUE=admHour+':'+admMin
          ENDIF ELSE sv=DIALOG_MESSAGE('Found no administered activity in the DICOM header of the first image.')
        ENDIF ELSE sv=DIALOG_MESSAGE('No image loaded yet.')

      END
      'cross': BEGIN
        ;get ROI values
        IF tags(0) NE 'EMPTY' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS
          nImg=N_ELEMENTS(tags)
          szROI=SIZE(crossROI, /DIMENSIONS)
          IF structImgs.(0).units NE 'BQML' THEN sv=DIALOG_MESSAGE('Expected unit is BQML. Actual unit is '+structImgs.(0).units, DIALOG_PARENT=evTop)
          totMean=0.
          resArr=FLTARR(3, nImg)-1
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errstatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
              imszTemp=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                maske=crossROI
                IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
                totMean=totMean+meanVal
                resArr[0,i]=meanVal
                resArr[1,i]=stddevVal
              ENDIF ELSE errstatus=1
            ENDIF
            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=' '
          If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
          meanOfmean=totMean/TOTAL(markedArr)
          resArr(2,0)=meanOfmean

          WIDGET_CONTROL, txtCrossConc, GET_VALUE=actConc
          WIDGET_CONTROL, txtCrossFactorPrev, GET_VALUE=oldCCCF
          corrFactor=FLOAT(oldCCCF)*FLOAT(actConc(0))/meanOfmean
          WIDGET_CONTROL, txtCrossFactor, SET_VALUE=STRING(corrFactor, FORMAT='(f0.3)')
          crossRes=resArr

          ;get scanstart from DICOM
          scanTmin=-1
          IF tags(0) NE 'EMPTY' THEN BEGIN
            scanT=structImgs.(0).acqTime
            scanT=STRING(scanT, FORMAT='(i06)')
            scanHour=STRMID(scanT,0,2)
            scanMin=STRMID(scanT,2,2)
            scanTmin=LONG(scanHour)*60+LONG(scanMin); hh*24 + mm
            WIDGET_CONTROL, txtCrossScanStart, SET_VALUE=scanHour+':'+scanMin
          ENDIF ELSE WIDGET_CONTROL, txtCrossScanStart, SET_VALUE='-'

          results(getResNmb(modality,analyse,analyseStringsAll))=1
          updateTable
          updatePlot, 1,1,3
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END;cross

      'updateCross': BEGIN

        errMsg=''
        ;calculate activity in phantom at start of scan based on measured activities and time stamps
        WIDGET_CONTROL, txtCrossMeasAct, GET_VALUE=act
        act=FLOAT(act(0))
        WIDGET_CONTROL, txtCrossMeasRest, GET_VALUE=rest
        rest=FLOAT(rest(0))
        WIDGET_CONTROL, txtCrossMeasActT, GET_VALUE=actT
        actTmin=LONG(STRMID(actT,0,2))*60+LONG(STRMID(actT,3,2)); hh*24 + mm
        WIDGET_CONTROL, txtCrossMeasRT, GET_VALUE=restT
        restTmin=LONG(STRMID(restT,0,2))*60+LONG(STRMID(restT,3,2)); hh*24 + mm
        IF restTmin LT actTmin AND rest GT 0.0 THEN errMsg=errMsg+'ERROR: Time of measuring rest-activity should be later than time of measuring activity.'+newline

        ;get scanstart from DICOM
        scanTmin=-1
        IF tags(0) NE 'EMPTY' THEN BEGIN
          scanT=structImgs.(0).acqTime
          scanT=STRING(scanT, FORMAT='(i06)')
          scanHour=STRMID(scanT,0,2)
          scanMin=STRMID(scanT,2,2)
          scanTmin=LONG(scanHour)*60+LONG(scanMin); hh*24 + mm
          WIDGET_CONTROL, txtCrossScanStart, SET_VALUE=scanHour+':'+scanMin
          IF scanTmin LT restTmin THEN errMsg=errMsg+'ERROR: Time of scan should be later than time of injected activity measurements.'+newline
        ENDIF ELSE WIDGET_CONTROL, txtCrossScanStart, SET_VALUE='-'

        IF scanTmin GT 0 THEN BEGIN
          ;calculate activity at start of scan
          tHalf=110;minutes halftime F-18
          actAtScanT=act*EXP(-ALOG(2)*(scanTmin-actTmin)/tHalf)
          restAtScanT=rest*EXP(-ALOG(2)*(scanTmin-restTmin)/tHalf)
          scanAct=actAtScanT-restAtScanT
          WIDGET_CONTROL, txtCrossScanAct, SET_VALUE=STRING(scanAct, FORMAT='(f0.3)')

          ;calculate concentration (Bq/mL)
          WIDGET_CONTROL, txtCrossVol, GET_VALUE=vol
          conc=scanAct*10.^6/FLOAT(vol(0))
          WIDGET_CONTROL, txtCrossConc, SET_VALUE=STRING(conc, FORMAT=formatCode(conc))

          ;calculate calibration factor if
          IF N_ELEMENTS(crossRes) GT 1 THEN BEGIN
            WIDGET_CONTROL, txtCrossFactorPrev, GET_VALUE=oldCCCF
            WIDGET_CONTROL, txtCrossFactor, SET_VALUE=STRING(FLOAT(oldCCCF)*conc/crossRes(2,0), FORMAT='(f0.3)')
          ENDIF ELSE WIDGET_CONTROL, txtCrossFactor, SET_VALUE='-'

        ENDIF

        IF errMsg NE '' THEN sv=DIALOG_MESSAGE(errMsg, DIALOG_PARENT=evTop)
        errMsg=''
      END;updateCross

      'rcRev': BEGIN
        clearRes, 'RC'
        redrawImg, 0,0
      END
      'rcBackExclude':BEGIN
        IF ev.SELECT THEN BEGIN
          clearRes, 'RC'
          redrawImg, 0,0
        ENDIF
      END
      'cw_rcType': IF ev.SELECT THEN updateTable
      'recovCoeff': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN

          WIDGET_CONTROL, /HOURGLASS
          nImg=N_ELEMENTS(tags)

          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1

          tttt=[1,5]
          IF tttt.HasValue(TOTAL(markedArr)) THEN BEGIN
            szROI=SIZE(rcROIs, /DIMENSIONS)
            resArr=FLTARR(7+6);values for all 6 ROIs + background

            errStatus=0
            resArrTemp=FLTARR(7+6,nImg)

            ;max & background
            FOR i=0, nImg-1 DO BEGIN
              IF markedArr(i) THEN BEGIN
                ;check if same size
                tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                imszTemp=SIZE(tempImg, /DIMENSIONS)
                IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                  bg=FLTARR(12)
                  FOR bb=0,11 DO BEGIN
                    maske=rcROIs[*,*,bb+6]
                    IF total(maske) NE 0 THEN BEGIN
                      IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanVal, MASK=maske
                      bg(bb)=meanVal
                    ENDIF ELSE bg(bb)=-1
                  ENDFOR
                  usedBg=WHERE(bg NE -1)
                  IF usedBg(0) NE -1 THEN resArrTemp(6,i)=MEAN(bg(usedBg))
                  FOR r=0, 5 DO BEGIN
                    maske=rcROIs[*,*,r]
                    IMAGE_STATISTICS, tempImg, COUNT=nPix, MAXIMUM=maxVal, MASK=maske
                    resArrTemp(r,i)=maxVal

                  ENDFOR
                ENDIF ELSE errStatus=1
              ENDIF;markedArr
              WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./TOTAL(markedArr), FORMAT='(i0)')+' %'
            ENDFOR
            FOR r=0,5 DO resArr(r)=MAX(resArrTemp[r,*])

            markedTemp=WHERE(markedArr EQ 1)
            resArr(6)=MEAN(resArrTemp[6,markedTemp])

            ;A50
            valStruc=CREATE_STRUCT('emty',0)
            FOR r=0, 5 DO BEGIN
              maske=rcROIs[*,*,r]
              valstemp=0
              FOR i=0, nImg-1 DO BEGIN
                IF markedArr(i) THEN BEGIN
                  ;check if same size
                  tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
                  imszTemp=SIZE(tempImg, /DIMENSIONS)
                  IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                    v50=0.5*(resArr(r)+resArr(6)); mean of max and background
                    inMask=WHERE(maske EQ 1)
                    valsInMask=tempImg(inMask)
                    A50=WHERE(valsInMask GT v50)
                    IF A50(0) NE -1 THEN valstemp=[valstemp, valsInMask(A50)]
                  ENDIF
                ENDIF
              ENDFOR
              resArr(7+r)=MEAN(valstemp)
            ENDFOR

            WIDGET_CONTROL, lblProgress, SET_VALUE=' '
            If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION, DIALOG_PARENT=evTop)
            rcRes=resArr
            results(getResNmb(modality,analyse,analyseStringsAll))=1
            updateTable
            updatePlot, 1,1,0
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
          ENDIF ELSE sv=DIALOG_MESSAGE('Expecting 1 or 5 (marked) images for the analysis', DIALOG_PARENT=evTop)
        ENDIF;tags emtpy
      END;recovCoeff

      ;----analyse SNR MR--------------------------------------------------------------------------------------------------
      'SNR_MR':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          SNR_MR; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,3
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END
      ;----analyse PUI MR--------------------------------------------------------------------------------------------------
      'PUI_MR':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          PUI_MR; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,3
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END
      'PUI_MR_setMinMax':BEGIN
        resValid=0
        IF N_ELEMENTS(PUIres) GT 0 THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          minWL=PUIres[0,sel]
          maxWL=PUIres[1,sel]
          IF minWL NE maxWL AND minWL NE -1 THEN BEGIN
            resValid=1
            WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
            WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
            ;reset center/width
            centerWL=(minWL+maxWL)/2
            widthWL=maxWL-minWL
            WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
            WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
            redrawImg,0,0
          ENDIF
        ENDIF
        IF resValid EQ 0 THEN BEGIN
          sv=DIALOG_MESSAGE('No valid PUI results for selected image. Window level could not be set based on the results.',/INFORMATION, DIALOG_PARENT=evTop)
        ENDIF
      END
      ;----analyse Ghosting Ratio MR--------------------------------------------------------------------------------------------------
      'Ghost_MR':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          Ghost_MR; tests_forQuickTest.pro
          updateTable
          updatePlot, 1,1,3
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF
      END
      'ghost_MR_optC':BEGIN
        clearRes, 'GHOST' & redrawImg,0,0
        END
        
        ;----analyse Geometric Distortion MR--------------------------------------------------------------------------------------------------
        'GD_MR':BEGIN
          IF tags(0) NE 'EMPTY' THEN BEGIN
            GD_MR; tests_forQuickTest.pro
            updateTable
            updatePlot, 1,1,3
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
          ENDIF
        END
        
        ;----analyse Slice thickness MR--------------------------------------------------------------------------------------------------
        'Slice_MR':BEGIN
          IF tags(0) NE 'EMPTY' THEN BEGIN
            Slice_MR; tests_forQuickTest.pro
            updateTable
            updatePlot, 1,1,3
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
          ENDIF
        END
        'slice_MR_optC':BEGIN
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
      'Slice_MR_WL':BEGIN
        resValid=0
        IF N_ELEMENTS(sliceMR_ROI) GT 0 THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          
          maske=sliceMR_ROI[*,*,0]
          IMAGE_STATISTICS, activeImg, MINIMUM=min1, MAXIMUM=max1, MASK=maske
          maske=sliceMR_ROI[*,*,1]
          IMAGE_STATISTICS, activeImg, MINIMUM=min2, MAXIMUM=max2, MASK=maske
          
          minWL=MIN([min1,min2])
          maxWL=MAX([max1,max2])
          IF minWL NE maxWL AND minWL NE -1 THEN BEGIN
            resValid=1
            WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
            WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
            ;reset center/width
            centerWL=(minWL+maxWL)/2
            widthWL=maxWL-minWL
            WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
            WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
            redrawImg,0,0
          ENDIF
        ENDIF
        IF resValid EQ 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Could not find the ROIs. Window level could not be set.',/INFORMATION, DIALOG_PARENT=evTop)
        ENDIF
      END

      ;**************************************************** Copy to clipboard *******************************************

      'copyInfo':BEGIN
        tags=TAG_NAMES(structImgs)
        IF tags(0) NE 'EMPTY' THEN BEGIN

          ;include only info actual for current modality
          tagnam=TAG_NAMES(structImgs.(0))
          includeArr=actualTags(tagnam, imgStructInfo, modality)
          idsInclude=WHERE(includeArr EQ 1)

          nImg=N_TAGS(structImgs)
          nInfo=TOTAL(includeArr)
          infoTable=STRARR(nInfo,nImg+1)
          infoTable[*,0]=tagnam(idsInclude)
          FOR i=0, nImg-1 DO BEGIN
            FOR j=0, nInfo-1 DO BEGIN
              infoTable[j,i+1]=STRJOIN(STRING(structImgs.(i).(idsInclude(j))),' | ')
            ENDFOR
          ENDFOR
          sv=DIALOG_MESSAGE('Save to file? (No = copy to clipboard)', /QUESTION, DIALOG_PARENT=evTop)
          IF sv EQ 'Yes' THEN BEGIN
            adr=DIALOG_PICKFILE(TITLE='Save as...',/WRITE, FILTER='*.txt', /FIX_FILTER, DEFAULT_EXTENSION='.txt', DIALOG_PARENT=evTop)
            IF adr(0) NE '' THEN BEGIN
              a=STRJOIN(infoTable, STRING(9B))
              OPENW, expfile, adr,/GET_LUN
              FOR i=0, N_ELEMENTS(a)-1 DO PRINTF, expfile, a(i)
              CLOSE, expfile & FREE_LUN, expfile
            ENDIF
          ENDIF ELSE BEGIN
            CLIPBOARD.set, STRJOIN(infoTable, STRING(9B))
          ENDELSE
        ENDIF
      END

      'copyTbl': BEGIN
        CASE modality OF
          0: curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
          1: curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
          2: curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
          3: curTab=WIDGET_INFO(wtabAnalysisSPECT, /TAB_CURRENT)
          4: curTab=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
          5: curTab=WIDGET_INFO(wtabAnalysisMR, /TAB_CURRENT)
          ELSE:curTab=-1
        ENDCASE

        IF results(curTab) EQ 1 THEN BEGIN

          IF WIDGET_INFO(wtabResult, /TAB_CURRENT) EQ 0 THEN BEGIN
            WIDGET_CONTROL, resTab, GET_VALUE=resTable;, /USE_TABLE_SELECT
          ENDIF ELSE WIDGET_CONTROL, resTabSup, GET_VALUE=resTable
          szT=SIZE(resTable, /DIMENSIONS)
          IF deciMark EQ ',' THEN BEGIN
            IF N_ELEMENTS(szT) EQ 2 THEN BEGIN
              FOR i=0, szT(0)-1 DO BEGIN
                FOR j=0, szT(1)-1 DO BEGIN
                  resTable[i,j]=STRJOIN(STRSPLIT(resTable[i,j], '.',/EXTRACT),',')
                ENDFOR
              ENDFOR
            ENDIF ELSE BEGIN
              FOR i=0, szT(0)-1 DO resTable[i]=STRJOIN(STRSPLIT(resTable[i], '.',/EXTRACT),',')
            ENDELSE
          ENDIF
          IF copyHeader THEN BEGIN
            IF N_ELEMENTS(szT) EQ 1 THEN szT=[szT(0),1]
            newTable=STRARR(szT(0),szT(1)+1)
            newTable[*,1:szT(1)]=resTable
            newTable[*,0]=headers
            resTable=newTable
          ENDIF
          CLIPBOARD.set, STRJOIN(resTable, STRING(9B))
        ENDIF
      END

      'copyCurve': updatePlot, 0,0,1

      ;**************************************************** Push data to iImage/iPlot *******************************************

      'iPlot': updatePlot, 0,0,2

      'ax':BEGIN
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)
        pix=structImgs.(sel).pix
        iImage, activeImg, TITLE='Axial image', ASPECT_RATIO=pix(1)/pix(0)
      END

      'surf': su=SURFACE(activeImg)

      'sumax': BEGIN
        nImg=N_ELEMENTS(tags)
        pix=structImgs.(0).pix
        markedArr=INTARR(nImg)
        IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
        szAct=SIZE(activeImg, /DIMENSIONS)
        sumArr=FLTARR(szAct(0),szAct(1))
        errStatus=0
        FOR i=0, nImg-1 DO BEGIN
          IF markedArr(i) THEN BEGIN
            ;check if same size
            tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
            imszTemp=SIZE(tempImg, /DIMENSIONS)
            IF ARRAY_EQUAL(imszTemp[0:1], szAct[0:1]) THEN sumArr=sumArr+tempImg ELSE errStatus=1
          ENDIF;markedArr
        ENDFOR
        If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Sum of images restricted to those with same size.',/INFORMATION, DIALOG_PARENT=evTop)
        iImage, sumArr, TITLE='Sum of axial images', ASPECT_RATIO=pix(1)/pix(0)
      END

      'iImageRes': IF N_ELEMENTS(activeResImg) GT 1 THEN iImage, activeResImg

      'cor':BEGIN
        WIDGET_CONTROL, /HOURGLASS

        nImg=N_TAGS(structImgs)
        firstImg=readImg(structImgs.(0).filename, structImgs.(0).frameNo)

        szFirst=SIZE(firstImg, /DIMENSIONS)
        corTemp=FLTARR(szFirst(0), nImg)
        center=szFirst/2+dxya[0:1]

        FOR i=0, nImg-1 DO BEGIN
          tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
          szTemp=SIZE(tempImg, /DIMENSIONS)
          IF ARRAY_EQUAL(szTemp,szFirst) THEN BEGIN
            corTemp[*,i]=tempImg[*,center(1)]
          ENDIF ELSE BEGIN
            sv=DIALOG_MESSAGE('Image number ' + STRING(i, FORMAT='(i0)')+' do not have the same size as the first image. Coronal image can not be generated.', DIALOG_PARENT=evTop)
            corTemp=-1
            BREAK
          ENDELSE
        ENDFOR

        IF N_ELEMENTS(corTemp) GT 1 AND nImg GT 1 THEN BEGIN
          IF structImgs.(0).sliceThick EQ -1 THEN asR=0 ELSE asR=structImgs.(0).sliceThick/structImgs.(0).pix(0)
          iImage, corTemp, TITLE='Coronal image', ASPECT_RATIO=asR
        ENDIF

      END

      'sag':BEGIN
        WIDGET_CONTROL, /HOURGLASS
        nImg=N_TAGS(structImgs)
        firstImg=readImg(structImgs.(0).filename, structImgs.(0).frameNo)

        szFirst=SIZE(firstImg, /DIMENSIONS)
        sagTemp=FLTARR(szFirst(1), nImg)
        center=szFirst/2+dxya[0:1]

        FOR i=0, nImg-1 DO BEGIN
          tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
          szTemp=SIZE(tempImg, /DIMENSIONS)
          IF ARRAY_EQUAL(szTemp,szFirst) THEN BEGIN
            sagTemp[*,i]=tempImg[center(0),*]
          ENDIF ELSE BEGIN
            sv=DIALOG_MESSAGE('Image number ' + STRING(i, FORMAT='(i0)')+' do not have the same size as the first image. Sagittal image can not be generated.', DIALOG_PARENT=evTop)
            sagTemp=-1
            BREAK
          ENDELSE
        ENDFOR

        IF N_ELEMENTS(sagTemp) GT 1 AND nImg GT 1 THEN BEGIN
          IF structImgs.(0).sliceThick EQ -1 THEN asR=0 ELSE asR=structImgs.(0).sliceThick/structImgs.(0).pix(1)
          iImage, sagTemp, TITLE='Sagittal image', ASPECT_RATIO=asR
        ENDIF


      END

      '3d':BEGIN
        WIDGET_CONTROL, /HOURGLASS
        nImg=N_TAGS(structImgs)
        firstImg=readImg(structImgs.(0).filename,structImgs.(0).frameNo)

        szFirst=SIZE(firstImg, /DIMENSIONS)
        volTemp=FLTARR(szFirst(0),szFirst(1), nImg)
        center=szFirst/2+dxya[0:1]

        FOR i=0, nImg-1 DO BEGIN
          tempImg=readImg(structImgs.(i).filename, structImgs.(i).frameNo)
          szTemp=SIZE(tempImg, /DIMENSIONS)
          IF ARRAY_EQUAL(szTemp,szFirst) THEN BEGIN
            volTemp[*,*,i]=tempImg
          ENDIF ELSE BEGIN
            sv=DIALOG_MESSAGE('Image number ' + STRING(i, FORMAT='(i0)')+' do not have the same size as the first image. Sagittal image can not be generated.', DIALOG_PARENT=evTop)
            volTemp=-1
            BREAK
          ENDELSE
        ENDFOR

        IF N_ELEMENTS(volTemp) GT 1 AND nImg GT 1 THEN BEGIN
          IF structImgs.(0).sliceThick EQ -1 THEN asR=0 ELSE asR=structImgs.(0).sliceThick/structImgs.(0).pix(1)
          pVol=PTR_NEW(volTemp)
          slicer3, pVol, DATA_NAMES='volume_data'
        ENDIF
      END

      ;***********************************************************************************************

      'setRangeMinMaxX':updatePlot, 1,0,0
      'setRangeMinMaxY':updatePlot, 0,1,0
      'cutLSF': clearRes, 'MTF'
      ELSE:
    ENDCASE; uvalue
  ENDIF

  ;********************************************* Radiobutton changed ***********************************************************
  IF ev.ID EQ cw_typeMTF OR ev.ID EQ cw_formLSFX OR ev.ID EQ cw_typeMTFNM OR ev.ID EQ cw_typeMTFSPECT THEN clearRes, 'MTF'
  IF ev.ID EQ cw_typeMTF OR ev.ID EQ cw_typeMTFNM OR ev.ID EQ cw_typeMTFSPECT THEN redrawImg, 0,0
  IF ev.ID EQ cw_rampType THEN BEGIN
    clearRes, 'SLICETHICK'
    redrawImg, 0,0
  ENDIF
  ;------ use MultiMark
  IF ev.ID EQ btnUseMulti THEN updateMulti

  ;********************************************* Textfield changed **************************************************************
  IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') OR (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
    action=0
    IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') THEN BEGIN
      IF ev.enter EQ 0 THEN action=1 ; lost focus
    ENDIF
    IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
      IF ev.type EQ 0 THEN action=1 ;return or enter pressed
    ENDIF
    IF action EQ 1 THEN BEGIN
      CASE ev.ID OF
        txtDeltaX: BEGIN
          WIDGET_CONTROL, txtDeltaX, GET_VALUE=dx
          dx=LONG(dx(0))
          WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dx, FORMAT='(i0)')
          dxya(0)=dx & dxya(3)=1
          IF TOTAL(results) GT 0 THEN clearRes
          redrawImg,0,0
        END
        txtDeltaY: BEGIN
          WIDGET_CONTROL, txtDeltaY, GET_VALUE=dy
          dy=LONG(dy(0))
          WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dy, FORMAT='(i0)')
          dxya(1)=dy & dxya(3)=1
          IF TOTAL(results) GT 0 THEN clearRes
          redrawImg,0,0
        END
        txtDeltaA: BEGIN
          WIDGET_CONTROL, txtDeltaA, GET_VALUE=da
          da=FLOAT(comma2pointFloat(da(0)))
          WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(da, FORMAT='(f0.1)')
          WIDGET_CONTROL, txtDeltaA, GET_VALUE=da
          da=FLOAT(da(0))
          dxya(2)=da & dxya(3)=1
          IF TOTAL(results) GT 0 THEN clearRes
          redrawImg,0,0
        END
        txtMinWL: BEGIN
          WIDGET_CONTROL, txtMinWL, GET_VALUE=minWL
          minWL=LONG(minWL(0))
          WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
          ;reset center/width
          WIDGET_CONTROL, txtMaxWL, GET_VALUE=maxWL
          maxWL=LONG(maxWL(0))
          centerWL=(minWL+maxWL)/2
          widthWL=maxWL-minWL
          WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
          redrawImg,0,0
        END
        txtMaxWL: BEGIN
          WIDGET_CONTROL, txtMaxWL, GET_VALUE=maxWL
          maxWL=LONG(maxWL(0))
          WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
          ;reset center/width
          WIDGET_CONTROL, txtMinWL, GET_VALUE=minWL
          minWL=LONG(minWL(0))
          centerWL=(minWL+maxWL)/2
          widthWL=maxWL-minWL
          WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
          redrawImg,0,0
        END
        txtCenterWL: BEGIN
          WIDGET_CONTROL, txtCenterWL, GET_VALUE=centerWL
          centerWL=LONG(centerWL(0))
          WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
          ;reset min/max
          WIDGET_CONTROL, txtWidthWL, GET_VALUE=widthWL
          widthWL=LONG(widthWL(0))
          maxWL=centerWL+widthWL/2
          minWL=centerWL-widthWL/2
          WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
          redrawImg,0,0
        END
        txtWidthWL: BEGIN
          WIDGET_CONTROL, txtWidthWL, GET_VALUE=widthWL
          widthWL=LONG(widthWL(0))
          WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')
          ;reset min/max
          WIDGET_CONTROL, txtCenterWL, GET_VALUE=centerWL
          centerWL=LONG(centerWL(0))
          maxWL=centerWL+widthWL/2
          minWL=centerWL-widthWL/2
          WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(maxWL, FORMAT='(i0)')
          WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minWL, FORMAT='(i0)')
          redrawImg,0,0
        END
        txtMinRangeX: BEGIN
          WIDGET_CONTROL, txtMinRangeX, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMinRangeX, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updatePlot, 0,0,0
        END
        txtMaxRangeX: BEGIN
          WIDGET_CONTROL, txtMaxRangeX, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMaxRangeX, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updatePlot, 0,0,0
        END
        txtMinRangeY: BEGIN
          WIDGET_CONTROL, txtMinRangeY, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMinRangeY, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updatePlot, 0,0,0
        END
        txtMaxRangeY: BEGIN
          WIDGET_CONTROL, txtMaxRangeY, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtMaxRangeY, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          updatePlot, 0,0,0
        END

        txtMTFroiSz:BEGIN
          WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtMTFroiSz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF' & redrawImg,0,0
        END
        txtcutLSFW:BEGIN
          WIDGET_CONTROL, txtcutLSFW, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtcutLSFW, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF'
        END
        txtcutLSFW2:BEGIN
          WIDGET_CONTROL, txtcutLSFW2, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtcutLSFW2, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF'
        END
        txtMTFroiSzX:BEGIN
          WIDGET_CONTROL, txtMTFroiSzX, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtMTFroiSzX, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF' & redrawImg,0,0
        END
        txtMTFroiSzY:BEGIN
          WIDGET_CONTROL, txtMTFroiSzY, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtMTFroiSzY, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF' & redrawImg,0,0
        END
        txtcutLSFWX:BEGIN
          WIDGET_CONTROL, txtcutLSFWX, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtcutLSFWX, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF'
        END
        txtMTFroiSzXNM:BEGIN
          WIDGET_CONTROL, txtMTFroiSzXNM, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtMTFroiSzXNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF' & redrawImg,0,0
        END
        txtMTFroiSzYNM:BEGIN
          WIDGET_CONTROL, txtMTFroiSzYNM, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtMTFroiSzYNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF' & redrawImg,0,0
        END
        txtMTFroiSzSPECT:BEGIN
          WIDGET_CONTROL, txtMTFroiSzSPECT, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtMTFroiSzSPECT, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF' & redrawImg,0,0
        END
        txtcutLSFWNM:BEGIN
          WIDGET_CONTROL, txtcutLSFWNM, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtcutLSFWNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF'
        END
        txtcutLSFWSPECT:BEGIN
          WIDGET_CONTROL, txtcutLSFWSPECT, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtcutLSFWSPECT, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'MTF'
        END

        txtLinROIrad:BEGIN
          WIDGET_CONTROL, txtLinROIrad, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtLinROIrad, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'CTLIN' & redrawImg,0,0
        END
        txtLinROIradS:BEGIN
          WIDGET_CONTROL, txtLinROIradS, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtLinROIradS, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'CTLIN' & redrawImg,0,0
        END

        txtRampDist:BEGIN
          WIDGET_CONTROL, txtRampDist, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtRampDist, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        txtRampLen:BEGIN
          WIDGET_CONTROL, txtRampLen, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtRampLen, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        txtRampBackG:BEGIN
          WIDGET_CONTROL, txtRampBackG, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtRampBackG, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SLICETHICK'
        END
        txtRampSearch:BEGIN
          WIDGET_CONTROL, txtRampSearch, GET_VALUE=val
          val=ABS(LONG(val(0)))
          WIDGET_CONTROL, txtRampSearch, SET_VALUE=STRING(val, FORMAT='(i0)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        txtRampAverage:BEGIN
          WIDGET_CONTROL, txtRampAverage, GET_VALUE=val
          val=ABS(LONG(val(0)))
          WIDGET_CONTROL, txtRampAverage, SET_VALUE=STRING(val, FORMAT='(i0)')
          clearRes, 'SLICETHICK'
        END

        txtRQA: BEGIN
          WIDGET_CONTROL, txtRQA, GET_VALUE=val
          Qvalue=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtRQA, SET_VALUE=STRING(Qvalue, FORMAT='(f0.1)')
          WIDGET_CONTROL, ddlRQA, SET_COMBOBOX_SELECT=4
          Qvals(4)=Qvalue
          clearRes; all results potenially based on STP results
        END
        txtStpROIsz:BEGIN
          WIDGET_CONTROL, txtStpROIsz, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtStpROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes & redrawImg,0,0; all results potenially based on STP results
        END

        txtHomogROIsz:BEGIN
          WIDGET_CONTROL, txtHomogROIsz, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtHomogROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'HOMOG' & redrawImg,0,0
        END
        txtHomogROIszX:BEGIN
          WIDGET_CONTROL, txtHomogROIszX, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtHomogROIszX, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'HOMOG' & redrawImg,0,0
        END
        txtHomogROIdist:BEGIN
          WIDGET_CONTROL, txtHomogROIdist, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtHomogROIdist, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'HOMOG' & redrawImg,0,0
        END
        txtHomogROIszPET:BEGIN
          WIDGET_CONTROL, txtHomogROIszPET, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtHomogROIszPET, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'HOMOG' & redrawImg,0,0
        END
        txtHomogROIdistPET:BEGIN
          WIDGET_CONTROL, txtHomogROIdistPET, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtHomogROIdistPET, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'HOMOG' & redrawImg,0,0
        END
        txtNoiseROIsz:BEGIN
          WIDGET_CONTROL, txtNoiseROIsz, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtNoiseROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'NOISE' & redrawImg,0,0
        END
        txtNoiseX:BEGIN
          WIDGET_CONTROL, txtNoiseX, GET_VALUE=val
          val=ABS(LONG(comma2pointFloat(val(0))))
          IF val GT 100 THEN val = 100
          IF val LT 2 THEN val =2
          WIDGET_CONTROL, txtNoiseX, SET_VALUE=STRING(val, FORMAT='(i0)')
          clearRes, 'NOISE' & redrawImg,0,0
        END
        txtHUwaterROIsz:BEGIN
          WIDGET_CONTROL, txtHUwaterROIsz, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtHUwaterROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'HUWATER' & redrawImg,0,0
        END

        txtROIrad:BEGIN
          WIDGET_CONTROL, txtROIrad, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIrad, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIx:BEGIN
          WIDGET_CONTROL, txtROIx, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIx, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIy:BEGIN
          WIDGET_CONTROL, txtROIy, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIy, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIa:BEGIN
          WIDGET_CONTROL, txtROIa, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIa, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END

        txtROIXrad:BEGIN
          WIDGET_CONTROL, txtROIXrad, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIXrad, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIXx:BEGIN
          WIDGET_CONTROL, txtROIXx, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIXx, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIXy:BEGIN
          WIDGET_CONTROL, txtROIXy, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIXy, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIXa:BEGIN
          WIDGET_CONTROL, txtROIXa, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIXa, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        
        txtROIMRrad:BEGIN
          WIDGET_CONTROL, txtROIMRrad, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIMRrad, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIMRx:BEGIN
          WIDGET_CONTROL, txtROIMRx, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIMRx, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIMRy:BEGIN
          WIDGET_CONTROL, txtROIMRy, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIMRy, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END
        txtROIMRa:BEGIN
          WIDGET_CONTROL, txtROIMRa, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtROIMRa, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'ROI' & redrawImg,0,0
        END

        txtNPSroiSz: BEGIN
          WIDGET_CONTROL, txtNPSroiSz, GET_VALUE=val
          val=LONG(val(0))
          IF val LE 0 THEN val=1
          WIDGET_CONTROL, txtNPSroiSz, SET_VALUE=STRING(val, FORMAT='(i0)')
          clearRes, 'NPS' & redrawImg,0,0
        END
        txtNPSroiDist: BEGIN
          WIDGET_CONTROL, txtNPSroiDist, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val EQ 0 THEN val=1.
          WIDGET_CONTROL, txtNPSroiDist, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'NPS' & redrawImg,0,0
        END
        txtNPSsubNN: BEGIN
          WIDGET_CONTROL, txtNPSsubNN, GET_VALUE=val
          val=LONG(val(0))
          IF val LE 1 THEN val=2
          WIDGET_CONTROL, txtNPSsubNN, SET_VALUE=STRING(val, FORMAT='(i0)')
          clearRes, 'NPS' & redrawImg,0,0
        END
        txtSmoothNPS: BEGIN
          WIDGET_CONTROL, txtSmoothNPS, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSmoothNPS, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          clearRes, 'NPS'
        END
        txtVarImageROIsz: BEGIN
          WIDGET_CONTROL, txtVarImageROIsz, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtVarImageROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          redrawImg, 0,0
        END
        txtNPSroiSzX: BEGIN
          WIDGET_CONTROL, txtNPSroiSzX, GET_VALUE=val
          val=LONG(val(0))
          IF val LT 22 THEN val=22
          WIDGET_CONTROL, txtNPSroiSzX, SET_VALUE=STRING(val, FORMAT='(i0)')
          WIDGET_CONTROL, txtNPSsubSzX, GET_VALUE=valS
          valS=LONG(valS(0))
          nPix=((valS*2-1)*val)^2
          WIDGET_CONTROL, lblNPStotPixX, SET_VALUE=STRING(nPix, FORMAT='(i0)')
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          IF sel(0) NE -1 THEN BEGIN
            curPix=structImgs.(sel(0)).pix
            subSzMM=curPix(0)*val*valS
            WIDGET_CONTROL, lblNPSsubSzMMX, SET_VALUE=STRING(subSzMM, FORMAT='(f0.1)')
          ENDIF ELSE WIDGET_CONTROL, lblNPSsubSzMMX, SET_VALUE=' '
          clearRes, 'NPS' & redrawImg,0,0
        END
        txtNPSsubSzX: BEGIN
          WIDGET_CONTROL, txtNPSsubSzX, GET_VALUE=val
          val=LONG(val(0))
          WIDGET_CONTROL, txtNPSsubSzX, SET_VALUE=STRING(val, FORMAT='(i0)')
          WIDGET_CONTROL, txtNPSroiSzX, GET_VALUE=valR
          nPix=((val*2-1)*LONG(valR(0)))^2
          WIDGET_CONTROL, lblNPStotPixX, SET_VALUE=STRING(nPix, FORMAT='(i0)')
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          IF sel(0) NE -1 THEN BEGIN
            curPix=structImgs.(sel(0)).pix
            subSzMM=curPix(0)*valR*val
            WIDGET_CONTROL, lblNPSsubSzMMX, SET_VALUE=STRING(subSzMM, FORMAT='(f0.1)')
          ENDIF ELSE WIDGET_CONTROL, lblNPSsubSzMMX, SET_VALUE=' '
          clearRes, 'NPS' & redrawImg,0,0
        END
        txtUnifAreaRatio: BEGIN
          WIDGET_CONTROL, txtUnifAreaRatio, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val GT 1. THEN val=1.
          WIDGET_CONTROL, txtUnifAreaRatio, SET_VALUE=STRING(val, FORMAT='(f0.2)')
          clearRes, 'UNIF' & redrawImg,0,0
        END
        txtLockRadUnifCorr: BEGIN
          WIDGET_CONTROL, txtLockRadUnifCorr, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val LT 1. THEN val=1.
          WIDGET_CONTROL, txtLockRadUnifCorr, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'UNIF' & redrawImg,0,0
        END
        txtSNIAreaRatio: BEGIN
          WIDGET_CONTROL, txtSNIAreaRatio, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val GT 1. THEN val=1.
          WIDGET_CONTROL, txtSNIAreaRatio, SET_VALUE=STRING(val, FORMAT='(f0.2)')
          clearRes, 'SNI' & redrawImg,0,0
        END
        txtLockRadSNICorr: BEGIN
          WIDGET_CONTROL, txtLockRadSNICorr, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val LT 1. THEN val=1.
          WIDGET_CONTROL, txtLockRadSNICorr, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SNI' & redrawImg,0,0
        END
        txtSNI_f: BEGIN
          WIDGET_CONTROL, txtSNI_f, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSNI_f, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SNI'
        END
        txtSNI_c: BEGIN
          WIDGET_CONTROL, txtSNI_c, GET_VALUE=val
          val=ABS(LONG(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSNI_c, SET_VALUE=STRING(val, FORMAT='(i0)')
          clearRes, 'SNI'
        END
        txtSNI_d: BEGIN
          WIDGET_CONTROL, txtSNI_d, GET_VALUE=val
          val=ABS(LONG(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSNI_d, SET_VALUE=STRING(val, FORMAT='(i0)')
          clearRes, 'SNI'
        END
        txtSmoothNPS_SNI: BEGIN
          WIDGET_CONTROL, txtSmoothNPS_SNI, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val EQ 0 THEN val=0.001
          WIDGET_CONTROL, txtSmoothNPS_SNI, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          clearRes, 'SNI'
        END
        txtfreqNPS_SNI: BEGIN
          WIDGET_CONTROL, txtfreqNPS_SNI, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val EQ 0 THEN val=0.001
          WIDGET_CONTROL, txtfreqNPS_SNI, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          clearRes, 'SNI'
        END
        txtBarROIsize: BEGIN
          WIDGET_CONTROL, txtBarROIsize, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtBarROIsize, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'BAR'
        END
        txtBar1: BEGIN
          WIDGET_CONTROL, txtBar1, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtBar1, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'BAR'
        END
        txtBar2: BEGIN
          WIDGET_CONTROL, txtBar2, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtBar2, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'BAR'
        END
        txtBar3: BEGIN
          WIDGET_CONTROL, txtBar3, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtBar3, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'BAR'
        END
        txtBar4: BEGIN
          WIDGET_CONTROL, txtBar4, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtBar4, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'BAR'
        END
        txtNAvgSpeedNM: BEGIN
          WIDGET_CONTROL, txtNAvgSpeedNM, GET_VALUE=val
          val=LONG(val(0))
          IF val/2 EQ (val*1.0)/2 THEN val=val-1; assure odd number
          IF val LT 1 THEN val=1
          WIDGET_CONTROL, txtNAvgSpeedNM, SET_VALUE=STRING(val, FORMAT='(i0)')
          updatePlot, 0,0,0 & redrawImg,0,0
        END
        txtSpeedROIheight: BEGIN
          WIDGET_CONTROL, txtSpeedROIheight, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSpeedROIheight, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          updatePlot,0,0,0  & redrawImg,0,0
        END
        txtScanSpeedMedian: BEGIN
          WIDGET_CONTROL, txtScanSpeedMedian, GET_VALUE=val
          val=LONG(val(0))
          IF val/2 EQ (val*1.0)/2 THEN val=val-1; assure odd number
          IF val LT 1 THEN val=1
          WIDGET_CONTROL, txtScanSpeedMedian, SET_VALUE=STRING(val, FORMAT='(i0)')
          updatePlot, 0,0,0
        END
        txtRadialMedian: BEGIN
          WIDGET_CONTROL, txtRadialMedian, GET_VALUE=val
          val=LONG(val(0))
          IF val/2 EQ (val*1.0)/2 THEN val=val-1; assure odd number
          IF val LT 1 THEN val=1
          WIDGET_CONTROL, txtRadialMedian, SET_VALUE=STRING(val, FORMAT='(i0)')
          updatePlot, 0,0,0
        END
        txtConR1SPECT: BEGIN
          WIDGET_CONTROL, txtConR1SPECT, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtConR1SPECT, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'CONTRAST' & redrawImg,0,0
        END
        txtConR2SPECT: BEGIN
          WIDGET_CONTROL, txtConR2SPECT, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtConR2SPECT, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'CONTRAST' & redrawImg,0,0
        END
        txtCrossROIsz: BEGIN
          WIDGET_CONTROL, txtCrossROIsz, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtCrossROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'CROSS' & redrawImg,0,0
        END
        txtCrossMeasAct: BEGIN
          WIDGET_CONTROL, txtCrossMeasAct, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtCrossMeasAct, SET_VALUE=STRING(val, FORMAT=formatCode(val))
          WIDGET_CONTROL, txtCrossScanAct, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossConc, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossFactor, SET_VALUE='-'
        END
        txtCrossMeasActT: BEGIN
          WIDGET_CONTROL, txtCrossMeasActT, GET_VALUE=val
          IF STRLEN(val) NE 5 THEN val='00:00' ELSE BEGIN
            hourVal=LONG(STRMID(val, 0,2))
            minVal=LONG(STRMID(val, 3,2))
            IF hourVal GT 24 OR hourVal LT 0 THEN hourVal=0
            IF minVal GT 59 OR hourVal LT 0 THEN minVal=0
            val=STRING(hourVal, FORMAT='(i02)')+':'+STRING(minVal, FORMAT='(i02)')
          ENDELSE
          WIDGET_CONTROL, txtCrossMeasActT, SET_VALUE=val
          WIDGET_CONTROL, txtCrossScanAct, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossConc, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossFactor, SET_VALUE='-'
        END
        txtCrossMeasRest: BEGIN
          WIDGET_CONTROL, txtCrossMeasRest, GET_VALUE=valR
          valR=ABS(FLOAT(comma2pointFloat(valR(0))))
          WIDGET_CONTROL, txtCrossMeasRest, SET_VALUE=STRING(valR, FORMAT=formatCode(valR))
          WIDGET_CONTROL, txtCrossScanAct, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossConc, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossFactor, SET_VALUE='-'
        END
        txtCrossMeasRT:BEGIN
          WIDGET_CONTROL, txtCrossMeasRT, GET_VALUE=val
          IF STRLEN(val) NE 5 THEN val='00:00' ELSE BEGIN
            hourVal=LONG(STRMID(val, 0,2))
            minVal=LONG(STRMID(val, 3,2))
            IF hourVal GT 24 OR hourVal LT 0 THEN hourVal=0
            IF minVal GT 59 OR hourVal LT 0 THEN minVal=0
            val=STRING(hourVal, FORMAT='(i02)')+':'+STRING(minVal, FORMAT='(i02)')
          ENDELSE
          WIDGET_CONTROL, txtCrossMeasRT, SET_VALUE=val
          WIDGET_CONTROL, txtCrossScanAct, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossConc, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossFactor, SET_VALUE='-'
        END
        txtCrossVol: BEGIN
          WIDGET_CONTROL, txtCrossVol, GET_VALUE=valV
          valV=ABS(FLOAT(comma2pointFloat(valV(0))))
          WIDGET_CONTROL, txtCrossVol, SET_VALUE=STRING(valV, FORMAT=formatCode(valV))
          WIDGET_CONTROL, txtCrossConc, SET_VALUE='-'
          WIDGET_CONTROL, txtCrossFactor, SET_VALUE='-'
        END
        txtCrossFactorPrev: BEGIN
          WIDGET_CONTROL, txtCrossFactorPrev, GET_VALUE=valV
          valV=ABS(FLOAT(comma2pointFloat(valV(0))))
          WIDGET_CONTROL, txtCrossFactorPrev, SET_VALUE=STRING(valV, FORMAT='(f0.3)')
          WIDGET_CONTROL, txtCrossFactor, SET_VALUE='-'
        END
        txtSNR_MR_ROI: BEGIN
          WIDGET_CONTROL, txtSNR_MR_ROI, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val LT 1. THEN val=1.
          IF val GT 100. THEN val=100.
          WIDGET_CONTROL, txtSNR_MR_ROI, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SNR' & redrawImg,0,0
        END
        txtPUI_MR_ROI: BEGIN
          WIDGET_CONTROL, txtPUI_MR_ROI, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val LT 1. THEN val=1.
          IF val GT 100. THEN val=100.
          WIDGET_CONTROL, txtPUI_MR_ROI, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'PUI' & redrawImg,0,0
        END
        txtGhost_MR_ROIszC: BEGIN
          WIDGET_CONTROL, txtGhost_MR_ROIszC, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtGhost_MR_ROIszC, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'GHOST' & redrawImg,0,0
        END
        txtGhost_MR_ROIszW: BEGIN
          WIDGET_CONTROL, txtGhost_MR_ROIszW, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtGhost_MR_ROIszW, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'GHOST' & redrawImg,0,0
        END
        txtGhost_MR_ROIszH: BEGIN
          WIDGET_CONTROL, txtGhost_MR_ROIszH, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtGhost_MR_ROIszH, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'GHOST' & redrawImg,0,0
        END
        txtGhost_MR_ROIszD: BEGIN
          WIDGET_CONTROL, txtGhost_MR_ROIszD, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtGhost_MR_ROIszD, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'GHOST' & redrawImg,0,0
        END
        txtGD_MR_act: BEGIN
          WIDGET_CONTROL, txtGD_MR_act, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtGD_MR_act, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'GEOMDIST' & redrawImg,0,0
        END
        txtSlice_MR_TANA: BEGIN
          WIDGET_CONTROL, txtSlice_MR_TANA, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSlice_MR_TANA, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        txtSlice_MR_ROIszW: BEGIN
          WIDGET_CONTROL, txtSlice_MR_ROIszW, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSlice_MR_ROIszW, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        txtSlice_MR_ROIszH: BEGIN
          WIDGET_CONTROL, txtSlice_MR_ROIszH, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtSlice_MR_ROIszH, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        txtSlice_MR_ROIszD: BEGIN
          WIDGET_CONTROL, txtSlice_MR_ROIszD, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtSlice_MR_ROIszD, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        txtSlice_MR_ROIszD2: BEGIN
          WIDGET_CONTROL, txtSlice_MR_ROIszD2, GET_VALUE=val
          val=FLOAT(comma2pointFloat(val(0)))
          WIDGET_CONTROL, txtSlice_MR_ROIszD2, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          clearRes, 'SLICETHICK' & redrawImg,0,0
        END
        ELSE:
      ENDCASE
    ENDIF
  ENDIF

  ;***************************************WIDGET_TABLE events***************************************************************
  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' THEN BEGIN
    CASE modality OF
      0:curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
      1:curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
      2:curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
      3:curTab=WIDGET_INFO(wtabAnalysisSPECT, /TAB_CURRENT)
      4:curTab=WIDGET_INFO(wtabAnalysisPET, /TAB_CURRENT)
      5:curTab=WIDGET_INFO(wtabAnalysisMR, /TAB_CURRENT)
    ENDCASE

    IF ev.ID EQ resTab AND results(curTab) EQ 1 THEN BEGIN
      active=1
      IF curTab EQ 2 THEN BEGIN
        curTest=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
        IF curTest EQ getResNmb(2,'ENERGYSPEC',analyseStringsAll) THEN active=0
      ENDIF
      IF active THEN BEGIN
        tabSel=WIDGET_INFO(resTab,/TABLE_SELECT)
        rowNo=tabSel(1)
        IF rowNo NE -1 THEN BEGIN
          IF marked(0) NE -1 THEN sel=marked(rowNo) ELSE sel=rowNo
          WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel
          redrawImg,0,1 & updateInfo
          updatePlot, 0,0,0
        ENDIF
      ENDIF
    ENDIF

    IF ev.ID EQ tblLin THEN BEGIN
      IF linTabEdit EQ !Null THEN linTabEdit=0
      IF ev.type EQ 9 AND linTabEdit NE 0 THEN linTabEdit=1; cell deselected when change has occured
    ENDIF

  ENDIF

  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CH' THEN BEGIN
    IF ev.ID EQ tblLin THEN BEGIN
      IF linTabEdit EQ !Null THEN linTabEdit=0
      CASE ev.type OF
        0: BEGIN; singel character
          linTabEdit=2
          IF ev.ch EQ 13 THEN BEGIN
            linTabEdit=1 ; enter
            WIDGET_CONTROL, tblLin, GET_VALUE=currTab
            szt=SIZE(currTab, /DIMENSIONS)
            newTab=currTab
            IF N_ELEMENTS(szt) GT 1 THEN BEGIN
              nDec=['','1','1','3']
              FOR i=0,szt(1)-1 DO BEGIN
                FOR p=1,3 DO newTab[p,i]=STRING(FLOAT(STRJOIN(STRSPLIT(currTab[p,i], ',',/EXTRACT),'.')),FORMAT='(f0.'+nDec(p)+')')
              ENDFOR
            ENDIF
            WIDGET_CONTROL, tblLin, SET_VALUE=newTab
            tableHeaders=updateMaterialHeaders(tableHeaders, TRANSPOSE(newTab[0,*]))
          ENDIF
        END
        1: linTabEdit=2; multiple characters
        2: linTabEdit=2; delete text
        ELSE:
      ENDCASE

    ENDIF
  ENDIF

  ; ************************************** WIDGET_DRAW events **************************************************************
  IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN

    IF N_ELEMENTS(activeImg) GT 1 THEN BEGIN
      imgSz=SIZE(activeImg, /DIMENSIONS)
      maxSz=max(imgSz[0:1])
      WIDGET_CONTROL, zoomSlider, GET_VALUE=zoomFactor
      scaleF=maxSz/(drawXY*zoomFactor)
      IF zoomFactor GT 1 THEN corn0=imgSz/2+dxya[0:1]-ROUND(maxSz/2./zoomFactor) ELSE corn0=imgSz/2-ROUND(maxSz/2./zoomFactor)
      evX=ev.X*scaleF+corn0(0) & evY = ev.Y*scaleF+corn0(1);pixno in image

      IF evX LT 0 THEN evX=0
      IF evY LT 0 THEN evY=0
      IF evX GT imgSz(0)-1 THEN evX=imgSz(0)-1
      IF evY GT imgSz(1)-1 THEN evY=imgSz(1)-1
      curVal=activeImg(evX,evY)
      WIDGET_CONTROL, lblCursorValue, SET_VALUE=STRING(curVal, FORMAT=formatCode(curVal))
      xx=evX-imgSz(0)/2-dxya(0) & yy=evY-imgSz(1)/2-dxya(1)
      WIDGET_CONTROL, lblCursorPos, SET_VALUE=STRING(xx, FORMAT='(i0)')+','+STRING(yy, FORMAT='(i0)')

      sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
      IF sel NE -1 THEN BEGIN
        pix=structImgs.(sel).pix
        WIDGET_CONTROL, lblCursorPosMM, SET_VALUE=STRING(xx*pix(0), FORMAT='(f0.1)')+','+STRING(yy*pix(1), FORMAT='(f0.1)')
      ENDIF

      IF (ev.release EQ 1 AND ev.type LE 1) OR mouseDown EQ 1 THEN BEGIN
        diffXY=[evX,evY]-lastXY;diffXY=[ev.X,ev.Y]-lastXY
        WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
        WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
        newLower=LONG(lower)+diffXY(1)-diffXY(0)
        newUpper=LONG(upper)+diffXY(1)+diffXY(0)
        IF newLower GT newUpper-1 THEN newLower=newUpper-1
        WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(newLower,FORMAT='(i0)')
        WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(newUpper,FORMAT='(i0)')
        ;reset center/width
        centerWL=(newLower+newUpper)/2
        widthWL=newUpper-newLower
        WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
        WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')

        lastXY=[evX,evY];[ev.X,ev.Y]
        IF ev.release EQ 1 THEN BEGIN
          nClick=1
          IF lastXYreleased(2) NE -1 THEN timediff=  SYSTIME(/SECONDS)-  lastXYreleased(2) ELSE timediff=-1
          IF timediff NE -1 THEN BEGIN
            IF timediff LT 0.35 THEN nClick=2
          ENDIF
          lastXYreleased=[lastXY,SYSTIME(/SECONDS)]
          lastXY=[-1,-1]
          mouseDown=0

          IF nClick EQ 2 THEN BEGIN ;set senter on doubleclick
            tempImg=activeImg
            imsz=SIZE(tempImg, /DIMENSIONS)
            IF lastXYreleased(0) EQ -1 THEN centerTemp = imsz/2 ELSE centerTemp=lastXYreleased;*max(imsz)/drawXY
            dxya[0:1]=centerTemp-imsz/2
            WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
            WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
            dxya(3)=1 & IF TOTAL(results) GT 0 THEN clearRes
            redrawImg, 0,0
          ENDIF
        ENDIF
        redrawImg,0,0
      ENDIF

      IF ev.release EQ 4 AND ev.type EQ 1 AND mouseDownRight EQ 1 THEN BEGIN
        WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
        WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
        xes=[xx, lastXYright(0)]
        yes=[yy, lastXYright(1)]
        xes=xes(SORT(xes))+imgSz(0)/2+dxya(0) & yes=yes(SORT(yes))+imgSz(1)/2+dxya(1)

        subImg=activeImg[xes(0):xes(1),yes(0):yes(1)]
        newLower=ROUND(MIN(subImg))
        newUpper=ROUND(MAX(subImg))

        WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(newLower,FORMAT='(i0)')
        WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(newUpper,FORMAT='(i0)')
        ;reset center/width
        centerWL=(newLower+newUpper)/2
        widthWL=newUpper-newLower
        WIDGET_CONTROL, txtCenterWL, SET_VALUE=STRING(centerWL, FORMAT='(i0)')
        WIDGET_CONTROL, txtWidthWL, SET_VALUE=STRING(widthWL, FORMAT='(i0)')

        IF ev.release EQ 4 THEN BEGIN
          lastXYright=[-1,-1]
          mouseDownRight=0
          redrawImg,0,0
        ENDIF
      ENDIF

      IF ev.press EQ 1 AND ev.type LE 1 THEN BEGIN
        lastXY=[evX,evY];[ev.X,ev.Y]
        mouseDown = 1
      ENDIF

      IF ev.press EQ 4 AND ev.type EQ 0 AND mouseDownRight EQ 0 THEN BEGIN;right mouse press
        lastXYright=[xx,yy]
        mouseDownRight = 1
      ENDIF

      IF ev.type EQ 7 THEN BEGIN ; wheel events
        di=ev.clicks
        tags=TAG_NAMES(structImgs)
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          nImg=N_TAGS(structImgs)
          IF sel-di LT nImg AND sel-di GE 0 THEN BEGIN
            WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel-di
            redrawImg,0,1 & updateInfo
          ENDIF
        ENDIF
      ENDIF

      IF (ev.key EQ 6 OR ev.key EQ 8 OR ev.key EQ 10) AND ev.release THEN BEGIN ; next image (arrow right or down or PageDown)
        sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
        nImg=N_TAGS(structImgs)
        IF sel LT nImg-1 THEN BEGIN
          WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel+1
          redrawImg,0,1 & updateInfo
        ENDIF
      ENDIF

      IF (ev.key EQ 5 OR ev.key EQ 7 OR ev.key EQ 9) AND ev.release THEN BEGIN ; prev image (arrow lft or up or PageUp)
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
        IF sel GT 0 THEN BEGIN
          WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel-1
          redrawImg,0,1 & updateInfo
        ENDIF
      ENDIF

    ENDIF

  ENDIF
  ;-------------------- WIDGET_TAB events-----------------------------------
  ;New tab mode/analysis/result type selected
  IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TAB') THEN BEGIN

    tags=TAG_NAMES(structImgs)
    IF tags(0) NE 'EMPTY' THEN loadedImg=1 ELSE loadedImg=0
    selTab=WIDGET_INFO(ev.ID, /TAB_CURRENT)

    CASE ev.ID OF
      wtabModes: BEGIN

        IF N_ELEMENTS(switchMode) EQ 0 THEN BEGIN
          sv='Yes'
          IF TOTAL(results) GT 0 THEN sv=DIALOG_MESSAGE('Switch test-mode and lose current results?', /QUESTION, DIALOG_PARENT=evTop)
          IF sv EQ 'Yes' THEN BEGIN
            modality=selTab
            clearRes
            clearMulti
            updateMode
          ENDIF ELSE BEGIN
            switchMode='No'
            WIDGET_CONTROL, wtabModes, SET_TAB_CURRENT=modality
          ENDELSE
        ENDIF ELSE switchMode=!Null

        redrawImg, 0,0
        updateInfo
      END
      wtabAnalysisCT: BEGIN
        analyse=analyseStringsAll.CT[selTab]
        IF loadedImg THEN redrawImg, 0,0
      END
      wtabAnalysisXray: BEGIN
        analyse=analyseStringsAll.Xray[selTab]
        IF loadedImg THEN redrawImg, 0,0
      END
      wtabAnalysisNM: BEGIN
        analyse=analyseStringsAll.NM[selTab]
        IF loadedImg THEN redrawImg, 0,0
        IF analyse EQ 'GEOMMEAN' THEN BEGIN
          results(getResNmb(modality,'GEOMMEAN',analyseStringsAll))=1
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=2
        ENDIF
      END
      wtabAnalysisSPECT: BEGIN
        analyse=analyseStringsAll.SPECT[selTab]
        IF loadedImg THEN redrawImg, 0,0
      END
      wtabAnalysisPET: BEGIN
        analyse=analyseStringsAll.PET[selTab]
        IF loadedImg THEN redrawImg, 0,0
      END
      wtabAnalysisMR: BEGIN
        analyse=analyseStringsAll.MR[selTab]
        IF loadedImg THEN redrawImg, 0,0
      END
      wtabResult:BEGIN
        ;IF TOTAL(results) GT 0 THEN BEGIN
        CASE selTab OF
          0: updateTable
          1: updatePlot, 0,0,0
          2: updateImageRes
          3: updateTableSup
          ELSE:
        ENDCASE
        ;ENDIF
      END

      ELSE:

    ENDCASE

    IF loadedImg THEN BEGIN;IF ev.ID NE wtabResult AND loadedImg THEN BEGIN
      IF ev.ID NE wtabResult THEN BEGIN
        updateTable
        IF WIDGET_INFO(wtabResult, /TAB_CURRENT) EQ 1 THEN updatePlot, 1,1,0 ELSE updatePlot, 1,1,3
      ENDIF; ELSE updatePlot, 0,0,0
    ENDIF
  ENDIF

  ;************** Move selected **********************
  IF N_ELEMENTS(moveSelected) GT 0 THEN BEGIN
    IF moveSelected GE 0 THEN BEGIN
      sel=WIDGET_INFO(listFiles, /LIST_SELECT)
      newFirstSel=sel(0)
      last=N_ELEMENTS(sel)-1

      IF sel(0) NE -1 THEN BEGIN
        nImg=N_TAGS(structImgs)
        oldOrder=INDGEN(nImg)

        IF moveSelected NE 4 THEN BEGIN
          invSel=oldOrder
          invSel(sel)=-1
          invSel=invSel(SORT(invSel))
          invSel=invSel(UNIQ(invSel))
          IF invSel(0) EQ -1 THEN invSel=invSel[1: N_ELEMENTS(invSel)-1]
        ENDIF
        CASE moveSelected OF
          0: BEGIN;top
            newOrder=[sel,invSel]
            newFirstSel=0
          END
          1: BEGIN;up
            newOrder=oldOrder
            IF sel(0) GT 0 THEN BEGIN
              newOrder[sel(0)-1:sel(last)-1]=oldOrder[sel(0)-1:sel(last)-1]+1
              newOrder(sel(last))=oldOrder(sel(0)-1)
              newFirstSel=sel-1
            ENDIF
          END
          2:BEGIN;down
            newOrder=oldOrder
            IF sel(last) NE nImg-1 THEN BEGIN
              newOrder[sel(0)+1:sel(last)+1]=oldOrder[sel(0)+1:sel(last)+1]-1
              newOrder(sel(0))=oldOrder(sel(last)+1)
              newFirstSel=sel+1
            ENDIF
          END
          3: BEGIN;bottom
            newOrder=[invSel,sel]
            newFirstSel=N_ELEMENTS(invSel)
          END
          4: BEGIN; sort by...
            ;check that selection is in a group
            nSel=N_ELEMENTS(sel)
            newOrder=oldOrder
            IF nSel LE 1 THEN BEGIN
              sel=INDGEN(nImg)
              nSel=nImg
            ENDIF
            diff=sel-shift(sel,-1)
            ff=uniq(diff)
            IF diff(0) EQ -1 AND ff(0) EQ nSel-2 THEN BEGIN
              sortImgs, GROUP_LEADER=ev.top, xoffset+200, yoffset+200, [sel(0),sel(-1)]
              newFirstSel=0
            ENDIF ELSE sv=DIALOG_MESSAGE('Selection have to be continuous for this option.', DIALOG_PARENT=evTop)

          END
          ELSE:newOrder=oldOrder
        ENDCASE

        IF ARRAY_EQUAL(newOrder,oldOrder) EQ 0 THEN BEGIN

          proceed=1
          IF TOTAL(results) GT 0 THEN BEGIN
            sv=DIALOG_MESSAGE('Continue and clear results?', /QUESTION, DIALOG_PARENT=evTop)
            IF sv EQ 'No' THEN proceed=0
          ENDIF
          If proceed THEN BEGIN
            structImgs=reorderStructStruct(structImgs, newOrder)
            IF marked(0) NE -1 THEN BEGIN
              markedArr=INTARR(nImg)
              markedArr(marked)=1
              markedArr=markedArr(newOrder)
              marked=WHERE(markedArr EQ 1)
            ENDIF
            IF markedMulti(0) NE -1 THEN BEGIN
              szMM=SIZE(markedMulti, /DIMENSIONS)
              FOR p=0, szMM(0)-1 DO BEGIN
                markedArr=markedMulti[p,*]
                markedMulti[p,*]=markedArr(newOrder)
              ENDFOR
            ENDIF
            IF WIDGET_INFO(lstShowFile, /DROPLIST_SELECT) EQ 0 THEN RDtemp='' ELSE RDtemp=modalityName
            fileList=getListOpenFiles(structImgs,0,marked,markedMulti,RENAMEDICOM=RDtemp, CONFIGPATH=configPath, PARENT=evTop)
            WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=newFirstSel, SET_LIST_TOP=0
            WIDGET_CONTROL, listFiles, SCR_YSIZE=170
            clearRes
            updateInfo
          ENDIF
        ENDIF;newOrder proceed
      ENDIF;selected exists
      ;ENDIF ELSE sv=DIALOG_MESSAGE('Not possible for multiframe images', DIALOG_PARENT=evTop)
      moveSelected = -1
    ENDIF
  ENDIF

  ;* **************** Edited CT linearity table - delete ROIs and results for update  ******************************
  IF N_ELEMENTS(linTabEdit) GT 0 THEN BEGIN
    IF linTabEdit EQ 1 THEN BEGIN ;pressed enter in editable cell or cell desel when linTabEdit=2
      clearRes, 'CTLIN'
      redrawImg, 0,0
      linTabEdit=0
    ENDIF
  ENDIF

  ;******************** Context MENU ***************************
  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_CONTEXT' THEN BEGIN
    WIDGET_DISPLAYCONTEXTMENU, ev.ID, ev.X, ev.Y, ctmActions
  ENDIF


  ;******************* Exit program ***********************
  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    IF saveOK EQ 1 THEN BEGIN
      IF FILE_TEST(configPath, /READ) THEN RESTORE, configPath ELSE sv=DIALOG_MESSAGE('Lost connection to config file '+configPath, /ERROR)
      configS.(0).saveBlocked=0
      SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=configPath
    ENDIF
    WIDGET_CONTROL, ev.top, /DESTROY
  ENDIF

  ;******************* Move on screen ***************
  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TLB_MOVE' THEN BEGIN
    xoffset=ev.x
    yoffset=ev.y
  ENDIF
end

;##################### about ImageQC ######################################

pro ImageQC_about, GROUP_LEADER = mainB

  COMPILE_OPT hidden
  COMMON VARI

  about_box = WIDGET_BASE(TITLE='About ImageQC', /COLUMN, $
    XSIZE=350, YSIZE=200, XOFFSET=200, YOFFSET=200, GROUP_LEADER=mainB, /MODAL)

  infoe=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE=' ')
  info0=WIDGET_LABEL(about_box, /ALIGN_CENTER, VALUE='ImageQC v'+currVersion, FONT="Arial*ITALIC*24")
  info1=WIDGET_LABEL(about_box, /ALIGN_CENTER, VALUE='Quality Control for medical imaging', FONT="Arial*ITALIC*16")
  info2=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='---------------------------------')
  info3=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='Implemented with IDL v 8.7')
  info9=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='')
  info12=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='Ellen Wasb'+string(248B)+' 2020 (ellen.wasbo@sus.no)')
  info13=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='Stavanger University Hospital, Norway')

  WIDGET_CONTROL, about_box, /REALIZE
  XMANAGER, 'ImageQC_about', about_box

end

