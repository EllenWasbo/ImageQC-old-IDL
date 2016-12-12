;ImageQC - quality control of medical images
;Copyright (C) 2016  Ellen Wasbo, Stavanger University Hospital, Norway
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

pro ImageQC_event, ev

  COMMON VARI

  ;local variables in event
  COMMON LocEv, switchMode

  ;******************* UVALUE ***********************
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 AND SIZE(uval, /TNAME) EQ 'STRING' THEN BEGIN

    tags=TAG_NAMES(structImgs)

    CASE uval OF

      'exit': WIDGET_CONTROL, ev.top, /DESTROY
      'info': BEGIN
        url='https://github.com/EllenWasbo/ImageQC/wiki'
        Case !version.os_family of
          'Windows': SPAWN, 'start '+url
          Else: if (!version.os_name eq 'Mac OS X') then SPAWN, 'open '+url
        Endcase
      END
      'about': imageqc_about, GROUP_LEADER=ev.top
      'close': clearAll

      ;---- new modal Preferences ---------------------------------------------------------------------------------------------------------
      'pref': BEGIN
        ;punktum eller komma ved copy to clipboard, copyTbl ta med headers?
        ;defpath innbakt i denne
        sv=DIALOG_MESSAGE('Not implemented yet')
      END

      ;---- define new default Path---------------------------------------------------------------------------------------------------------
      'defpath':BEGIN
        newdef=DIALOG_PICKFILE(PATH=defPath, /DIRECTORY)
        IF newdef(0) NE '' THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          config.defPath=newdef(0)
          defPath=newdef(0)
          SAVE, config, FILENAME=thisPath+'data\config.dat'
        ENDIF
      END
      ;-----save current default parameters to config file
      'config': BEGIN
        ;CT tests
        WIDGET_CONTROL, cw_typeMTF, GET_VALUE=typeMTF
        WIDGET_CONTROL, cw_plotMTF, GET_VALUE= plotWhich
        WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=MTFroiSz
        WIDGET_CONTROL, txtCutLSFW, GET_VALUE=LSFcut1
        WIDGET_CONTROL, txtCutLSFW2, GET_VALUE=LSFcut2
        WIDGET_CONTROL, txtLinROIrad, GET_VALUE=rad1
        WIDGET_CONTROL, txtLinROIrad2, GET_VALUE=rad2
        WIDGET_CONTROL, txtRampDist, GET_VALUE=rampDist
        WIDGET_CONTROL, txtRampLen, GET_VALUE=rampLen
        WIDGET_CONTROL, txtRampBackG, GET_VALUE=rampBackG
        WIDGET_CONTROL, txtRampSearch, GET_VALUE=rampSearch
        WIDGET_CONTROL, txtRampAverage, GET_VALUE=rampAvg
        WIDGET_CONTROL, txtHomogROIsz, GET_VALUE=homogROIsz
        WIDGET_CONTROL, txtHomogROIdist, GET_VALUE=homogROIdist
        WIDGET_CONTROL, txtNoiseROIsz, GET_VALUE=noiseROIsz
        WIDGET_CONTROL, txtNPSroiSz, GET_VALUE=NPSroiSz
        WIDGET_CONTROL, txtNPSroiDist, GET_VALUE=NPSroiDist
        WIDGET_CONTROL, txtNPSsubNN, GET_VALUE=NPSsubNN
        ;Xray tests
        WIDGET_CONTROL, txtStpROIsz, GET_VALUE=STProiSz
        WIDGET_CONTROL, cw_formLSFX, GET_VALUE=typeMTFX
        WIDGET_CONTROL, cw_plotMTFX, GET_VALUE= plotWhichX
        WIDGET_CONTROL, txtMTFroiSzX, GET_VALUE=MTFroiSzX
        WIDGET_CONTROL, txtMTFroiSzY, GET_VALUE=MTFroiSzY
        WIDGET_CONTROL, txtHomogROIszX, GET_VALUE=homogROIszX
        WIDGET_CONTROL, txtNPSroiSzX, GET_VALUE=NPSroiSzX
        WIDGET_CONTROL, txtNPSsubSzX, GET_VALUE=NPSsubSzX
        ;NM tests
        WIDGET_CONTROL, txtHomogROIszNM, GET_VALUE=homogROIszNM
        WIDGET_CONTROL, txtHomogROIdistXNM, GET_VALUE=homogROIdistXNM
        WIDGET_CONTROL, txtHomogROIdistYNM, GET_VALUE=homogROIdistYNM
        WIDGET_CONTROL, cw_typeMTFNM, GET_VALUE=typeMTFNM
        WIDGET_CONTROL, cw_plotMTFNM, GET_VALUE= plotWhichNM
        WIDGET_CONTROL, txtMTFroiSzXNM, GET_VALUE=MTFroiSzXNM
        WIDGET_CONTROL, txtMTFroiSzYNM, GET_VALUE=MTFroiSzYNM
        WIDGET_CONTROL, txtNAvgSpeedNM, GET_VALUE=scanSpeedAvg
        WIDGET_CONTROL, txtSpeedROIheight, GET_VALUE=scanSpeedHeight
        WIDGET_CONTROL, txtScanSpeedMedian, GET_VALUE=scanSpeedFiltW
        WIDGET_CONTROL, txtConR1NM, GET_VALUE=contrastRad1
        WIDGET_CONTROL, txtConR2NM, GET_VALUE=contrastRad2

        config=CREATE_STRUCT('defPath',defPath,$
          'typeROI',WIDGET_INFO(typeROI, /BUTTON_SET),'typeROIX',WIDGET_INFO(typeROIX, /BUTTON_SET),$
          'MTFtype',typeMTF,'MTFtypeX', typeMTFX, 'MTFtypeNM', typeMTFNM, 'plotMTF',plotWhich,'plotMTFX',plotWhichX,'plotMTFNM', plotWhichNM,'MTFroiSz',FLOAT(MTFroiSz(0)),'MTFroiSzX',[FLOAT(MTFroiSzX(0)),FLOAT(MTFroiSzY(0))],'MTFroiSzNM',[FLOAT(MTFroiSzXNM(0)),FLOAT(MTFroiSzYNM(0))],$
          'cutLSF',WIDGET_INFO(btnCutLSF,/BUTTON_SET),'cutLSF1',LONG(LSFcut1),'cutLSF2',LONG(LSFcut2),$
          'LinROIrad',FLOAT(rad1(0)),'LinROIrad2',FLOAT(rad2(0)),$
          'RampDist',FLOAT(rampDist(0)),'RampLen',FLOAT(rampLen(0)),'RampBackG',FLOAT(rampBackG(0)),'RampSearch',LONG(RampSearch(0)),'RampAvg',LONG(rampAvg(0)),$
          'HomogROIsz',FLOAT(homogROIsz(0)), 'HomogROIszX',FLOAT(homogROIszX(0)),'HomogROIdist',FLOAT(homogROIdist(0)), 'HomogROIszNM',FLOAT(homogROIszNM(0)),'HomogROIdistNM',[FLOAT(homogROIdistXNM(0)),FLOAT(homogROIdistYNM(0))],$
          'NoiseROIsz',FLOAT(noiseROIsz(0)), $
          'NPSroiSz', LONG(NPSroiSz(0)), 'NPSroiDist', FLOAT(NPSroiDist(0)),'NPSsubNN', LONG(NPSsubNN(0)), 'NPSroiSzX', LONG(NPSroiSzX(0)), 'NPSsubSzX', LONG(NPSsubSzX(0)), 'NPSavg',WIDGET_INFO(btnNPSavg, /BUTTON_SET),$
          'STProiSz', FLOAT(STProiSz(0)), $
          'scanSpeedAvg',LONG(scanSpeedAvg(0)), 'scanSpeedHeight', FLOAT(scanSpeedHeight(0)), 'scanSpeedFiltW', LONG(scanSpeedFiltW(0)), $
          'contrastRad1', FLOAT(contrastRad1(0)), 'contrastRad2', FLOAT(contrastRad2(0)) )

        SAVE, config, FILENAME=thisPath+'data\config.dat'
      END

      ;-----button open files------------generate list of adresses to open (adrFilesToOpen), later in code checked whether this list is empty or not----------------------------------------------
      'open':BEGIN
        adrFilesToOpen=DIALOG_PICKFILE(TITLE='Select DICOM CT file(s) to open', /READ, /Multiple_files, PATH=defPath)
        adrFilesToOpen=adrFilesToOpen(SORT(adrFilesToOpen))
        ;rest performed later in code where adrFilesToOpen content checked
      END

      'openMulti':BEGIN
        adr=DIALOG_PICKFILE(TITLE='Select the parent-folder of the subfolders (NB slow search -keep number of folders/content low)', /READ, /DIRECTORY, PATH=defPath)
        WIDGET_CONTROL, /HOURGLASS
        IF adr(0) NE '' THEN BEGIN
          Spawn, 'dir '  + '"'+adr(0)+'"' + '*'+ '/b /aD', dirs
          dirs=dirs(sort(dirs))

          IF dirs(0) NE '' THEN BEGIN
            dirs=adr(0)+dirs

            names=file_basename(dirs)
            box=[$
              '1, BASE,, /COLUMN', $
              '0, LABEL, Select folders to open', $
              '0, LABEL, ',$
              '2, LIST, ' + STRJOIN(names,'|') + ', TAG=folders', $
              '1, BASE,, /ROW', $
              '0, BUTTON, OK, QUIT, TAG=OK',$
              '2, BUTTON, Cancel, QUIT']
            res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select folders to open', XSIZE=300, YSIZE=300, FOCUSNO=3)

            IF res.OK THEN BEGIN
              IF N_ELEMENTS(res.folders) NE 0 THEN BEGIN
                dirs=dirs(res.folders)+'\'
              ENDIF ELSE dirs=''
            ENDIF ELSE dirs=''
          ENDIF ELSE BEGIN
            dirs=adr(0)
          ENDELSE
          WIDGET_CONTROL, /HOURGLASS

          IF dirs(0) NE '' THEN BEGIN
            dcmAdr=''
            FOR i= 0, N_ELEMENTS(dirs)-1 DO BEGIN
              Spawn, 'dir '  + '"'+dirs(i)+'"' + '*'+ '/b /a-D', adrTempTemp
              nFound=N_ELEMENTS(adrTempTemp)
              dcmOk=INTARR(nFound)
              IF adrTempTemp(0) NE '' THEN BEGIN
                adrTempTemp=adrTempTemp(sort(adrTempTemp))
                FOR d=0, nFound-1 DO BEGIN
                  IF adrTempTemp(d) EQ 'DICOMDIR' THEN BEGIN
                    dcmOk(d)=0 ; IDL crash if QUERY_DICOM on DICOMDIR - unknown reason
                  ENDIF ELSE BEGIN
                    adrTempTemp(d)=dirs(i)+adrTempTemp(d)
                    dcmOk(d)=QUERY_DICOM(adrTempTemp(d))
                  ENDELSE
                  WIDGET_CONTROL, lblProgress, SET_VALUE='Checking for Dicom files i directory '+STRING(i, FORMAT='(i0)')+': '+STRING(d*100./nFound, FORMAT='(i0)')+' %'
                ENDFOR

                dcmOkId=WHERE(dcmOk EQ 1)
                IF dcmOkId(0) NE -1 THEN dcmAdr=[dcmAdr, adrTempTemp(dcmOkId)]
              ENDIF
            ENDFOR
            nFiles=N_ELEMENTS(dcmAdr)-1
            IF nFiles GT 0 THEN BEGIN
              dcmAdr=dcmAdr[1:nFiles]
              adrFilesToOpen=dcmAdr
            ENDIF ELSE BEGIN
              sv=DIALOG_MESSAGE('Found no valid DICOM files in selected folder(s)')
              WIDGET_CONTROL, lblProgress, SET_VALUE=''
            ENDELSE
            
          ENDIF;dirs
        ENDIF;adr
      END
      ;-----button DICOM dump to text-file-----------------------------------------------------------------------------------------------------------
      'dump':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)
          obj = OBJ_NEW( 'IDLffDICOM' )
          IF nFrames EQ 0 THEN BEGIN
            var = obj->Read(structImgs.(sel).filename)
            tit=structImgs.(sel).filename
          ENDIF ELSE BEGIN 
            var = obj->Read(structImgs.(0).filename)
            tit=structImgs.(0).filename
          ENDELSE
          obj->DumpElements, thisPath+'data\dumpTemp.txt'
          XDISPLAYFILE, thisPath+'data\dumpTemp.txt', TITLE=tit
        ENDIF
      END

      ;-----move up/down in list ------------------------------------------------------------------------------------------------------------------
      'imgTop': moveSelected=0
      'imgUp': moveSelected=1
      'imgDown': moveSelected=2
      'imgBottom': moveSelected=3

      ;-----select/mark/remove------------------------------------------------------------------------------------------------------------------

      'remove': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF nFrames EQ 0 THEN BEGIN
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            IF sel(0) NE -1 THEN BEGIN
              nImg=N_TAGS(structImgs)
              selArr=INTARR(nImg) & selArr(sel)=1
              remain=WHERE(selArr EQ 0);ids to keep (oposite of selArr)
              IF marked(0) NE -1 THEN BEGIN; remove selected from marked
                markedArr=INTARR(nImg) & markedArr(marked)=1
                IF remain(0) NE -1 THEN markedArr=markedArr(remain)
                marked=WHERE(markedArr EQ 1)
              ENDIF

              structImgs=removeIDstructstruct(structImgs, sel)

              stillEmpty=WHERE(TAG_NAMES(structImgs) EQ 'EMPTY')
              IF stillEmpty(0) EQ -1 THEN BEGIN
                fileList=getListOpenFiles(structImgs,0,marked)

                WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=0, SET_LIST_TOP=0
                WIDGET_CONTROL, listFiles, SCR_YSIZE=170
                redrawImg,0,1

                curTab=WIDGET_INFO(wtabModes, /TAB_CURRENT)
                CASE curTab OF
                  0: analyseStrings=analyseStringsCT
                  1: analyseStrings=analyseStringsXray
                  2: analyseStrings=analyseStringsNM
                ENDCASE

                FOR i=0, N_ELEMENTS(results)-1 DO BEGIN
                  IF results(i) THEN BEGIN
                    CASE analyseStrings(i+1) OF
                      'DIM': dimRes=dimRes[*,remain]
                      'STP': BEGIN
                        stpRes=!Null
                        analyse='NONE'
                        END
                      'HOMOG': homogRes=homogRes[*,remain]
                      'NOISE': BEGIN
                        noise=!Null; because avg dependent on rest - recalculation needed
                        analyse='NONE'
                        END
                      'MTF': MTFres=removeIDstructstruct(MTFres,sel)
                      'NPS': NPSres=removeIDstructstruct(NPSres,sel)
                      'ROI': ROIres=ROIres[*,remain]
                      'CTLIN': CTlinres=CTlinres[*,remain]
                      'SLICETHICK': BEGIN
                        sliceThickRes=removeIDstructstruct(sliceThickRes,sel)
                        sliceThickResTab=sliceThickResTab[*,remain]
                      END
                      'FWHM': fwhmRes=fwhmRes[*,remain]
                      'ENERGYSPEC':
                      'SCANSPEED':
                      'CONTRAST': contrastRes=contrastRes[*,remain]
                      ELSE:
                    ENDCASE
                  ENDIF
                ENDFOR

                updateInfo=1
                updateTable
                updatePlot, 0,0,0
                WIDGET_CONTROL, lblLoadedN, SET_VALUE=STRING(n_elements(fileList), FORMAT='(i0)')+' )'
              ENDIF ELSE clearAll

            ENDIF
          ENDIF ELSE sv=DIALOG_MESSAGE('Open file is multiframe. Removing single frames not possible.',/INFORMATION)
        ENDIF;empty list
      END

      'unmarkSelected': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN

          MTFv3d=0
          IF results(getResNmb(modality,'MTF',analyseStringsCT,analyseStringsXray,analyseStringsNM)) THEN BEGIN
            resforeach=WHERE(TAG_NAMES(MTFres) EQ 'M0')
            IF resforeach(0) EQ -1 THEN MTFv3d=1
          ENDIF

          proceed=1
          IF MTFv3d OR N_ELEMENTS(noiseRes) GT 1 OR N_ELEMENTS(stpRes) GT 0 THEN BEGIN
            sv=DIALOG_MESSAGE('Continue and loose results?',/QUESTION)
            IF sv EQ 'No' THEN proceed=0 ELSE BEGIN
              IF MTFv3d THEN clearRes, 'MTF'
              IF N_ELEMENTS(noiseRes) GT 1 THEN clearRes, 'NOISE'
              IF N_ELEMENTS(stpRes) GT 0 THEN clearRes, 'STP'
            ENDELSE
          ENDIF

          IF proceed THEN BEGIN
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
            markedArr=INTARR(nImg)
            IF marked(0) NE -1 THEN markedArr(marked)=1 ELSE markedArr=markedArr+1
            markedArr(sel)=0
            marked=WHERE(markedArr EQ 1)
            unmarked=WHERE(markedArr EQ 0)

            IF nFrames EQ 0 THEN fileList=getListOpenFiles(structImgs,0,marked) ELSE fileList=getListFrames(structImgs.(0),marked)

            newSel=marked(0)
            If newSel EQ -1 THEN newSel=0
            oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
            WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=newSel, SET_LIST_TOP=oldTop ; YSIZE=n_elements(fileList),
            IF marked(0) EQ -1 THEN clearRes
            redrawImg,0,1 & updateInfo=1
          ENDIF
        ENDIF
      END

      'markSelected':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          
          proceed=1
          IF TOTAL(results) GT 0 THEN BEGIN;
            sv=DIALOG_MESSAGE('Continue and loose results?',/QUESTION)
            IF sv EQ 'No' THEN proceed=0
          ENDIF
          IF proceed THEN BEGIN
            sel=WIDGET_INFO(listFiles, /LIST_SELECT)
            oldMarked=marked
            marked=[marked,sel]
            marked=marked(SORT(marked))
            marked=marked(UNIQ(marked))
            nMark=N_ELEMENTS(marked)
            IF marked(0) EQ -1 THEN marked=marked[1:nMark-1]; remove the first -1
            IF nFrames EQ 0 THEN fileList=getListOpenFiles(structImgs,0,marked) ELSE fileList=getListFrames(structImgs.(0),marked)
            nSel=N_ELEMENTS(sel)
            oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
            WIDGET_CONTROL, listFiles, SET_VALUE=fileList, SET_LIST_SELECT=sel(nSel-1), SET_LIST_TOP=oldTop
            IF TOTAL(results) GT 0 THEN clearRes
            redrawImg,0,1 & updateInfo=1
          ENDIF
        ENDIF
      END
      'selectInverse':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN

          IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
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
          redrawImg,0,1 & updateInfo=1

        ENDIF; tagsempty
      END
      'selectMarked':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          oldTop=WIDGET_INFO(listFiles, /LIST_TOP)
          IF marked(0) NE -1 THEN BEGIN
            WIDGET_CONTROL, listFiles, SET_LIST_SELECT=marked, SET_LIST_TOP=oldTop
            redrawImg,0,1 & updateInfo=1
          ENDIF
        ENDIF
      END

      ;-----button prev/next image------------------------------------------------------------------------------------------------------------------
      'next'  : BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
          IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
          IF sel LT nImg-1 THEN BEGIN
            WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel+1
            redrawImg,0,1 & updateInfo=1
          ENDIF
        ENDIF
      END

      'prev':BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          IF sel GT 0 THEN BEGIN
            WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel-1
            redrawImg,0,1 & updateInfo=1
          ENDIF
        ENDIF
      END

      ;---- if new selection in filelist
      'filelist' : BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          redrawImg, 0,1 & updateInfo=1
          updateTable
          updatePlot, 0,0,0
        ENDIF
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

      ;-----buttons for delta positioning/centering-------------------------------------------------------------------------------------------------------------
      'useDelta': BEGIN
        IF dxya(3) EQ 0 THEN dxya(3)=1 ELSE dxya(3)=0
        clearRes
      END
      'showDelta': BEGIN
        redrawImg,0,0
      END
      'getCenter': BEGIN
        tempImg=activeImg
        imsz=SIZE(tempImg, /DIMENSIONS)

        box=[$
          '1, BASE,, /COLUMN', $
          '0, LABEL, Treshold (HU) for object to center', $
          '0, LABEL, ',$
          '2, INTEGER, 200, LABEL_LEFT=Pixel value, TAG=lim', $
          '1, BASE,, /ROW', $
          '0, BUTTON, OK, QUIT, TAG=OK',$
          '2, BUTTON, Cancel, QUIT']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Set treshold for object to center', XSIZE=200, YSIZE=150, FOCUSNO=3)

        IF res.OK THEN BEGIN

          centerTemp=centroid(tempImg, res.lim)

          IF min(centerTemp) LT 0 OR max(centerTemp) GE min(imsz) THEN BEGIN
            centerTemp=imsz/2
            sv=DIALOG_MESSAGE('Centering failed.',/INFORMATION)
          ENDIF

          dxya[0:1]=centerTemp-imsz/2
          WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
          WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
          dxya(3)=1 & redrawImg,0,0
        ENDIF
      END
      'setCenter':BEGIN;set to last position clicked in image
        tempImg=activeImg
        imsz=SIZE(tempImg, /DIMENSIONS)
        IF lastXYreleased(0) EQ -1 THEN centerTemp = imsz/2 ELSE centerTemp=lastXYreleased*max(imsz)/drawXY
        dxya[0:1]=centerTemp-imsz/2
        WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
        WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
        dxya(3)=1 & redrawImg,0,0
      END
      'minusDx': BEGIN
        dxya(0)=dxya(0)-1
        WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
        dxya(3)=1 & redrawImg,0,0
      END
      'plusDx': BEGIN
        dxya(0)=dxya(0)+1
        WIDGET_CONTROL, txtDeltaX, SET_VALUE=STRING(dxya(0), FORMAT='(i0)')
        dxya(3)=1 & redrawImg,0,0
      END
      'minusDy': BEGIN
        dxya(1)=dxya(1)-1
        WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
        dxya(3)=1 & redrawImg,0,0
      END
      'plusDy': BEGIN
        dxya(1)=dxya(1)+1
        WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dxya(1), FORMAT='(i0)')
        dxya(3)=1 & redrawImg,0,0
      END
      'minusDa': BEGIN
        dxya(2)=dxya(2)-0.1
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')
        dxya(3)=1 & redrawImg,0,0
      END
      'plusDa': BEGIN
        dxya(2)=dxya(2)+0.1
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')
        dxya(3)=1 & redrawImg,0,0
      END

      ;***************************************************************************************************************
      ;******************************** TESTS *******************************************************************************
      ;***************************************************************************************************************
      ;---- tab CT/Dim--------------------------------------------------------------------------------------------------

      'dim': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          analyse='DIM'
          WIDGET_CONTROL, /HOURGLASS
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          tempImg=activeImg
          szImg=SIZE(tempImg,/DIMENSIONS)

          IF nFrames EQ 0 THEN BEGIN
            pix=structImgs.(sel).pix
            nImg=N_ELEMENTS(tags)
          ENDIF ELSE nImg=nFrames

          imgCenterOffset=[0,0,0,0]
          IF dxya(3) EQ 1 THEN imgCenterOffset=dxya
          center=szImg/2+imgCenterOffset[0:1]

          resArr=FLTARR(6+8,nImg); Horizontal 1, 2 , vertical 1, 2, (difference from nominal 50mm), diagonal 1, 2 + x/y (#pix) for 4 rods (UR, UL, LL, LR)
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errStatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              IF nFrames NE 0 THEN BEGIN
                tempImg=readImg(structImgs.(0).filename, i)
              ENDIF ELSE BEGIN
                tempImg=readImg(structImgs.(i).filename, 0)
                pix=structImgs.(i).pix
              ENDELSE

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
          WIDGET_CONTROL, lblProgress, SET_VALUE=''
          IF errStatus GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of rods for '+STRING(errStatus, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Values set to -1.')
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'DIM',analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
          dimRes=resArr
          updateTable
          updatePlot, 0,0,0
          redrawImg,0,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF; empty
      END

      ;----analyse tab STP--------------------------------------------------------------------------------------------------
      'ddlRQA': BEGIN
        WIDGET_CONTROL, txtRQA, SET_VALUE=STRING(Qvals(ev.index), FORMAT='(i0)')
        clearRes
      END

      'drawROIstp': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          analyse='STP'
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          tempImg=activeImg
          szImg=SIZE(tempImg,/DIMENSIONS)
          pix=structImgs.(sel).pix

          WIDGET_CONTROL, txtStpROIsz, GET_VALUE=ROIsz
          ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal

          imgCenterOffset=[0,0,0,0]
          IF dxya(3) EQ 1 THEN imgCenterOffset=dxya
          center=szImg/2+imgCenterOffset[0:1]

          stpROI=getROIcircle(szImg, center, ROIsz)

          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'STP',analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          stpRes=CREATE_STRUCT('EMPTY',0)
          updateTable
          updatePlot, 0,0,0
          redrawImg,0,0
        ENDIF; empty
      END

      'STPpix': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF analyse NE 'STP' THEN sv=DIALOG_MESSAGE('Show ROIs first to verify size and positions.',/INFORMATION) ELSE BEGIN

            WIDGET_CONTROL, /HOURGLASS
            nImg=N_ELEMENTS(tags)
            szROI=SIZE(stpROI, /DIMENSIONS)

            resArr=FLTARR(4,nImg)-1; mean, stdev all circles
            markedArr=INTARR(nImg)
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
            errStatus=0
            FOR i=0, nImg-1 DO BEGIN
              IF markedArr(i) THEN BEGIN
                ;check if same size
                tempImg=readImg(structImgs.(i).filename, 0)
                imszTemp=SIZE(tempImg, /DIMENSIONS)
                IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                  maske=stpROI
                  IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanHU, STDDEV=stddevHU, MASK=maske
                  resArr(2,i)=meanHU & resArr(3,i)=stddevHU
                ENDIF ELSE errstatus=1
              ENDIF
              WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
            ENDFOR
            WIDGET_CONTROL, lblProgress, SET_VALUE=''
            If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size as the first image.',/INFORMATION)
            stpRes=CREATE_STRUCT('table',resArr)
            results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'STP',analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
            updateTable
            updatePlot, 0,1,0
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
          ENDELSE
        ENDIF
      END

      'impDose': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'STP',analyseStringsCT,analyseStringsXray,analyseStringsNM)) THEN BEGIN
            nImg=N_ELEMENTS(tags)
            markedArr=INTARR(nImg)
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1

            box=[$
              '1, BASE,, /COLUMN', $
              '2, BUTTON, .txt file|clipboard, EXCLUSIVE, LABEL_TOP=Import from..., COLUMN, SET_VALUE=1, TAG=from', $
              '1, BASE,, /ROW', $
              '0, BUTTON, OK, QUIT, TAG=OK',$
              '2, BUTTON, Cancel, QUIT']
            res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select how to import doses', XSIZE=300, YSIZE=200, FOCUSNO=3)

            IF res.OK THEN BEGIN
              CASE res.from OF

                0: BEGIN ;txt file
                  OPENR, filenhet, DIALOG_PICKFILE(), /GET_LUN
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
                updatePlot, 1,0,0
                WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
              ENDIF ELSE sv=DIALOG_MESSAGE('Mismatch between number of pixelvalues (N= '+STRING(TOTAL(markedArr), FORMAT='(i0)')+' and imported doses '+STRING(N_ELEMENTS(doses),FORMAT='(i0)')+'.')
            ENDIF;res.ok
          ENDIF ELSE sv=DIALOG_MESSAGE('Get pixel values first.')
        ENDIF
      END

      'calcSTP': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'STP',analyseStringsCT,analyseStringsXray,analyseStringsNM)) THEN BEGIN
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
          ENDIF ELSE sv=DIALOG_MESSAGE('Get pixel values first.')
        ENDIF
      END

      ;----analyse tab Homogeneity--------------------------------------------------------------------------------------------------
      'drawROIhomog': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          analyse='HOMOG'
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          tempImg=activeImg
          szImg=SIZE(tempImg,/DIMENSIONS)
          IF nFrames EQ 0 THEN pix=structImgs.(sel).pix ELSE pix=structImgs.(0).pix

          CASE modality OF
            0: BEGIN
              WIDGET_CONTROL, txtHomogROIsz, GET_VALUE=ROIsz
              ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
              WIDGET_CONTROL,  txtHomogROIdist, GET_VALUE=ROIdist
              ROIdist=ROUND(FLOAT(ROIdist(0))/pix(0)) ; assume x,y pix equal ! = normal
            END
            1:BEGIN
              WIDGET_CONTROL, txtHomogROIszX, GET_VALUE=ROIsz
              ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
              ROIdist=-1
            END
            2:BEGIN
              WIDGET_CONTROL, cw_homogNM, GET_VALUE=typeHomogNM
              WIDGET_CONTROL, txtHomogROIszNM, GET_VALUE=ROIsz
              ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal
              WIDGET_CONTROL,  txtHomogROIdistXNM, GET_VALUE=ROIdistX
              CASE typeHomogNM OF
                0: BEGIN; planar WB
                  WIDGET_CONTROL,  txtHomogROIdistYNM, GET_VALUE=ROIdistY
                  ROIdist=ROUND([FLOAT(ROIdistX(0))/pix(0),FLOAT(ROIdistY(0))/pix(1)])
                END
                1: BEGIN
                  ROIdist=ROUND(FLOAT(ROIdistX(0))/pix(0)) ; assume x,y pix equal ! = normal
                END
                ELSE:
              ENDCASE
            END
          ENDCASE

          imgCenterOffset=[0,0,0,0]
          IF dxya(3) EQ 1 THEN imgCenterOffset=dxya

          homogROIs=getHomogRois(szImg, imgCenterOffset, ROIsz, ROIdist, modality)

          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          homogRes=0
          updateTable
          updatePlot, 0,0,0

          redrawImg,0,0
        ENDIF; empty
      END

      'homog': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF analyse NE 'HOMOG' THEN sv=DIALOG_MESSAGE('Show ROIs first to verify size and positions.',/INFORMATION) ELSE BEGIN

            WIDGET_CONTROL, /HOURGLASS
            IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames
            szROI=SIZE(homogROIs, /DIMENSIONS)

            resArr=FLTARR(szROI(2)*2,nImg)-1; mean, stdev all circles
            markedArr=INTARR(nImg)
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1

            errstatus=0
            FOR i=0, nImg-1 DO BEGIN
              IF markedArr(i) THEN BEGIN
                ;check if same size
                IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
                imszTemp=SIZE(tempImg, /DIMENSIONS)
                IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                  FOR r=0, szROI(2)-1 DO BEGIN
                    maske=homogROIs[*,*,r]
                    IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
                    IF r GT 0 THEN resArr(r,i)=meanVal-resArr(0,i) ELSE resArr(r,i)=meanVal
                    resArr(r,i)=meanVal
                    resArr(r+5,i)=stddevVal
                  ENDFOR
                ENDIF ELSE errstatus=1
              ENDIF
              WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'

            ENDFOR
            WIDGET_CONTROL, lblProgress, SET_VALUE=''
            If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
            homogRes=resArr
            results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
            updateTable
            updatePlot, 1,1,0
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
          ENDELSE
        ENDIF
      END

      'cw_homogNM':  BEGIN
        IF analyse EQ 'HOMOG' AND results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM)) EQ 1 THEN BEGIN
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'HOMOG',analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          homogRes=!Null
          analyse='NONE'
          redrawImg,0,0
          updateTable
          updatePlot, 0,0,0
        ENDIF
        ;set new values from selected mode defaults
        RESTORE, thisPath+'data\config.dat'
        WIDGET_CONTROL, cw_homogNM, GET_VALUE=typeHomogNM
        CASE typeHomogNM OF
          0: BEGIN ;planar WB
            WIDGET_CONTROL, txtHomogROIszNM, SET_VALUE=STRING(config.HomogROIszNM,FORMAT='(f0.1)')
            WIDGET_CONTROL, txtHomogROIdistXNM, SET_VALUE=STRING(config.HomogROIdistNM(0),FORMAT='(f0.1)')
            WIDGET_CONTROL, txtHomogROIdistYNM, SET_VALUE=STRING(config.HomogROIdistNM(1),FORMAT='(f0.1)'), SENSITIVE=1
          END
          1: BEGIN ; SPECT
            WIDGET_CONTROL, txtHomogROIszNM, SET_VALUE='15.0'
            WIDGET_CONTROL, txtHomogROIdistXNM, SET_VALUE='55.0'
            WIDGET_CONTROL, txtHomogROIdistYNM, SET_VALUE='55.0', SENSITIVE=0
          END
          ELSE:
        ENDCASE
      END
      
      
      ;----analyse tab Noise--------------------------------------------------------------------------------------------------
      'drawROInoise': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          analyse='NOISE'
          sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
          tempImg=activeImg
          szImg=SIZE(tempImg,/DIMENSIONS)
          IF nFrames EQ 0 THEN pix=structImgs.(sel).pix ELSE pix=structImgs.(0).pix

          curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
          CASE curMode OF
            ;------CT
            0:BEGIN
              WIDGET_CONTROL, txtNoiseROIsz, GET_VALUE=ROIsz
              ROIsz=ROUND(FLOAT(ROIsz(0))/pix(0)) ; assume x,y pix equal ! = normal

              imgCenterOffset=[0,0,0,0]
              IF dxya(3) EQ 1 THEN imgCenterOffset=dxya
              center=szImg/2+imgCenterOffset[0:1]

              noiseROI=getROIcircle(szImg, center, ROIsz)
            END
            ;-------Xray
            1:BEGIN
              noiseROI=INTARR(szImg)
              noiseROI[0.05*szImg(0):0.95*szImg(0),0.05*szImg(1):0.95*szImg(1)]=1
            END
            2:
          ENDCASE
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          noiseRes=0
          updateTable
          updatePlot, 0,0,0
          redrawImg,0,0
        ENDIF; empty
      END

      'noise': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          IF analyse NE 'NOISE' THEN sv=DIALOG_MESSAGE('Show ROIs first to verify size and positions.',/INFORMATION) ELSE BEGIN

            WIDGET_CONTROL, /HOURGLASS
            IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames
            szROI=SIZE(noiseROI, /DIMENSIONS)

            resArr=FLTARR(2,nImg)-1; mean, stdev all circles
            markedArr=INTARR(nImg)
            totNoise=0.
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
            errstatus=0
            FOR i=0, nImg-1 DO BEGIN
              IF markedArr(i) THEN BEGIN
                ;check if same size
                IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
                imszTemp=SIZE(tempImg, /DIMENSIONS)
                IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                  maske=noiseROI
                  IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanVal, STDDEV=stddevVal, MASK=maske
                  resArr(0,i)=meanVal
                  resArr(1,i)=stddevVal
                  totNoise=totNoise+stddevVal
                ENDIF ELSE errstatus=1
              ENDIF
              WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
            ENDFOR
            WIDGET_CONTROL, lblProgress, SET_VALUE=''
            If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
            avgNoise=totNoise/TOTAL(markedArr)

            noiseRes=FLTARR(4,nImg)
            noiseRes[0:1,*]=resArr
            noiseRes[3,0]=avgNoise
            noiseRes[2,*]=100.0*(resArr[1,*]-avgNoise)/avgNoise

            curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
            CASE curMode OF
              0: BEGIN
                noiseRes=FLTARR(4,nImg)
                noiseRes[0:1,*]=resArr
                noiseRes[3,0]=avgNoise
                noiseRes[2,*]=100.0*(resArr[1,*]-avgNoise)/avgNoise
              END
              1: BEGIN
                noiseRes=FLTARR(2,nImg)
                noiseRes[0:1,*]=resArr
              END
              2:
            ENDCASE
            results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
            updateTable
            updatePlot, 1,1,0
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
          ENDELSE
        ENDIF
      END

      ;-----analyse-tab MTF --------------------------------------------------------------------------------------------------------------------
      'drawMTFroi': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN
          tempImg=activeImg
          dxya(3)=1 ; center option has to be used
          WIDGET_CONTROL, useDelta, SET_BUTTON=1
          dxya(2)=0; no rotation allowed
          WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')

          analyse = 'MTF'
          curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          MTFres=CREATE_STRUCT('empty',0)
          updateTable
          updatePlot, 0,0,0
          redrawImg,0,0
        ENDIF
      END

      'MTF': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN

          IF analyse NE 'MTF' THEN sv=DIALOG_MESSAGE('Show ROI first to verify size and position.',/INFORMATION) ELSE BEGIN

            WIDGET_CONTROL, /HOURGLASS
            WIDGET_CONTROL, cw_typeMTF, GET_VALUE=typeMTF
            WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=ROIszPix
            WIDGET_CONTROL, txtCutLSFW, GET_VALUE=cutLSFW
            WIDGET_CONTROL, txtCutLSFW2, GET_VALUE=cutLSFWf
            WIDGET_CONTROL, txtfreqMTF, GET_VALUE=sampFreq

            IF nFrames NE 0 THEN nImg=nFrames ELSE nImg=N_ELEMENTS(tags)
            markedArr=INTARR(nImg)
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
            markedTemp=WHERE(markedArr EQ 1)
            first=markedTemp(0)
            IF nFrames NE 0 THEN BEGIN
              tempImg=readImg(structImgs.(0).filename, first) 
              pixFirst=structImgs.(0).pix(0)
              filtFirst=structImgs.(0).filter
            ENDIF ELSE BEGIN
              tempImg=readImg(structImgs.(first).filename, 0)
              pixFirst=structImgs.(first).pix(0)
              filtFirst=structImgs.(first).filter
            ENDELSE

            szFirst=SIZE(tempImg, /DIMENSIONS)

            CASE typeMTF OF

              0: BEGIN; 2d bead

                ROIsz=ROUND(ROIszPix(0)/pixFirst)
                halfSz=SIZE(tempImg, /DIMENSIONS)/2
                x1=ROUND(halfSz(0)+dxya(0)-ROIsz) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz)
                y1=ROUND(halfSz(1)+dxya(1)-ROIsz) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz)

                IF x1 GE 0 AND x2 LT szFirst(0)-1 AND y1 GE 0 AND y1-(ROIsz(0)*2+1) GE 0 AND y2 LT szFirst(1)-1 AND y2-(ROIsz(0)*2+1) LT szFirst(1)-1 THEN roiOk=1 ELSE roiOk=0

                IF roiOK THEN BEGIN
                  errBead=0
                  errSize=0
                  FOR i=0, nImg-1 DO BEGIN
                    IF markedArr(i) THEN BEGIN
                      ;check if same size
                      IF i GT 0 THEN BEGIN
                        IF nFrames NE 0 THEN BEGIN
                          tempImg=readImg(structImgs.(0).filename, i)
                          curPix=pixFirst
                        ENDIF ELSE BEGIN
                          tempImg=readImg(structImgs.(i).filename, 0)
                          curPix=structImgs.(i).pix(0)
                        ENDELSE
                      ENDIF ELSE curPix=pixFirst
     
                      szImg=SIZE(tempImg, /DIMENSIONS)
                      IF ARRAY_EQUAL(szImg[0:1], szFirst[0:1]) AND curPix EQ pixFirst THEN BEGIN               
                        submatrix=tempImg[x1:x2,y1:y2]
                        backMatrix=tempImg[x1:x2,y1-(ROIsz(0)*2+1):y2-(ROIsz(0)*2+1)]
  
                        MTF=calculateMTF(submatrix, curPix, dxya[0:1], typeMTF, backMatrix, WIDGET_INFO(btnCutLSF, /BUTTON_SET), FLOAT(cutLSFW(0)), FLOAT(cutLSFWf(0)), FLOAT(sampFreq(0)))
                        IF MTF.status EQ 0 THEN errBead=errBead+1 
                      ENDIF ELSE BEGIN
                        MTF=CREATE_STRUCT('empty',0)
                        errSize=1
                      ENDELSE
                    ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
                    IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
                  ENDFOR
                  IF errBead GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of bead for '+STRING(errBead, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Bead position assumed to be at the selected ROI center.')
                  If errSize THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
                  results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
                  updateTable
                  updatePlot, 1,1,0
                ENDIF ELSE sv=DIALOG_MESSAGE('ROIs outside image. Calculation not possible.',/INFORMATION)
              END

              2: BEGIN ; circular edge (3d)
                  IF nFrames NE 0 THEN BEGIN
                    tempImg=readImg(structImgs.(0).filename, 0)
                    pixFirst=structImgs.(0).pix(0)
                  ENDIF ELSE BEGIN
                    tempImg=readImg(structImgs.(first).filename, 0)
                    pixFirst=structImgs.(first).pix(0)
                    filtFirst=structImgs.(first).filter
                  ENDELSE

                ROIsz=ROUND(ROIszPix(0)/pixFirst)
                halfSz=szFirst/2
                x1=ROUND(halfSz(0)+dxya(0)-ROIsz) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz)
                y1=ROUND(halfSz(1)+dxya(1)-ROIsz) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz)

                nnImg=TOTAL(markedArr)
                subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

                filtStatus=1
                pixStatus=1
                proceed=1
                counter=0
                FOR i=0, nImg-1 DO BEGIN
                  IF markedArr(i) THEN BEGIN
                    IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
                    szThis=SIZE(tempImg, /DIMENSIONS)
                    IF nFrames EQ 0 THEN BEGIN
                      curPix=structImgs.(i).pix(0)
                      curFilt=structImgs.(i).filter
                      IF curPix NE pixFirst THEN pixStatus=0
                      IF curFilt NE filtFirst THEN filtStatus=0
                    ENDIF
                    IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                      subM[*,*,counter]=tempImg[x1:x2,y1:y2]
                      proceed=1
                      counter=counter+1
                    ENDIF ELSE BEGIN
                      MTF=CREATE_STRUCT('empty',0)
                      sv=DIALOG_MESSAGE('Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION)
                      proceed=0
                    ENDELSE
                  ENDIF
                  IF proceed EQ 0 THEN BREAK
                ENDFOR

                IF proceed THEN BEGIN
                  MTFres=calculateMTF(subM, pixFirst, dxya[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSF, /BUTTON_SET), FLOAT(cutLSFW(0)), FLOAT(cutLSFWf(0)), FLOAT(sampFreq(0)))
                  IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for one or more images. Center of circle assumed to be at center of ROI.')
                  results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
                  updateTable
                  updatePlot,1,1,0
                  WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
                  IF filtStatus + pixStatus LT 2 THEN sv=DIALOG_MESSAGE('Pixelsize or filter-type do not match for all images in selection.')
                ENDIF
              END
              ELSE: sv=DIALOG_MESSAGE('Not implementet selected MTF type yet',/INFORMATION)
            ENDCASE

          ENDELSE; analyse=MTF
        ENDIF; empty
      END

      'cw_plotMTF':  BEGIN
        IF analyse EQ 'MTF' AND results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM)) EQ 1 THEN BEGIN
          updatePlot, 1,1,0
        ENDIF
      END

      'MTFX': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN

          IF analyse NE 'MTF' THEN sv=DIALOG_MESSAGE('Show ROI first to verify size and position.',/INFORMATION) ELSE BEGIN

            WIDGET_CONTROL, /HOURGLASS
            WIDGET_CONTROL, txtMTFroiSzX, GET_VALUE=ROIszX
            WIDGET_CONTROL, txtMTFroiSzY, GET_VALUE=ROIszY
            WIDGET_CONTROL, cw_formLSFX, GET_VALUE=formLSF
            WIDGET_CONTROL, txtCutLSFWX, GET_VALUE=cutLSFW

            nImg=N_ELEMENTS(tags)
            markedArr=INTARR(nImg)
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
            markedTemp=WHERE(markedArr EQ 1)
            first=markedTemp(0)
            tempImg=readImg(structImgs.(first).filename, 0)

            szFirst=SIZE(tempImg, /DIMENSIONS)
            curPix=structImgs.(first).pix
            ROIszMM=FLOAT([ROIszX(0),ROIszY])/2.
            ROIsz=ROIszMM/curPix
            halfSz=szFirst/2
            x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
            y1=ROUND(halfSz(1)+dxya(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(1))

            subMatrix=FLTARR(x2-x1+1,y2-y1+1)

            IF N_ELEMENTS(stpRes) EQ 0 THEN stpRes=0

            errLogg=''
            FOR i=0, nImg-1 DO BEGIN
              IF markedArr(i) THEN BEGIN
                tempImg=readImg(structImgs.(i).filename, 0)
                szThis=SIZE(tempImg, /DIMENSIONS)
                IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                  submatrix[*,*]=tempImg[x1:x2,y1:y2]
                  MTF=calculateMTF_xray(submatrix, curPix, dxya[0:1],stpRes, formLSF, WIDGET_INFO(btnCutLSFX, /BUTTON_SET), FLOAT(cutLSFW(0)));, WIDGET_INFO(revProcMTFX, /BUTTON_SET))
                  IF MTF.errMsg NE '' THEN errLogg=errLogg+'Warning image #'+STRING(i+1, FORMAT='(i0)')+'. '+MTF.errMsg+newline
                ENDIF ELSE BEGIN
                  MTF=CREATE_STRUCT('empty',0)
                  errLogg=errLogg+'Image size for image #'+STRING(i+1, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.'+newline
                ENDELSE
                
              ENDIF ELSE MTF=CREATE_STRUCT('empty',0)

              IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
            ENDFOR
            IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg)
            results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
            updateTable
            updatePlot,1,1,0
            WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
          ENDELSE; analyse=MTF
        ENDIF; empty
      END

      'cw_plotMTFX':  BEGIN
        IF analyse EQ 'MTF' AND results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM)) EQ 1 THEN BEGIN
          updatePlot, 1,1,0
        ENDIF
      END

      'MTFNM': BEGIN
        IF tags(0) NE 'EMPTY' THEN BEGIN

          IF analyse NE 'MTF' THEN sv=DIALOG_MESSAGE('Show ROI first to verify size and position.',/INFORMATION) ELSE BEGIN

            WIDGET_CONTROL, /HOURGLASS
            WIDGET_CONTROL, txtMTFroiSzXNM, GET_VALUE=ROIszX
            WIDGET_CONTROL, txtMTFroiSzYNM, GET_VALUE=ROIszY
            WIDGET_CONTROL, cw_typeMTFNM, GET_VALUE=typeMTF
            WIDGET_CONTROL, txtCutLSFWNM, GET_VALUE=cutLSFW

            v3d=WIDGET_INFO(MTF3dNM, /BUTTON_SET)

            IF nFrames NE 0 THEN nImg=nFrames ELSE nImg=N_ELEMENTS(tags)
            markedArr=INTARR(nImg)
            IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
            markedTemp=WHERE(markedArr EQ 1)
            first=markedTemp(0)
            IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, first) ELSE tempImg=readImg(structImgs.(first).filename, 0)

            szFirst=SIZE(tempImg, /DIMENSIONS)

            CASE typeMTF OF

              0: BEGIN; point
                errStatus=0
                errSize=0
                FOR i=0, nImg-1 DO BEGIN
                  IF markedArr(i) THEN BEGIN
                    ;check if same size
                    IF i GT 0 THEN BEGIN
                      IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
                    ENDIF
                    szImg=SIZE(tempImg, /DIMENSIONS)
                    IF ARRAY_EQUAL(szImg[0:1], szFirst[0:1]) THEN BEGIN

                      IF nFrames EQ 0 THEN curPix=structImgs.(i).pix(0) ELSE curPix=structImgs.(0).pix(0)
                      ROIszMM=FLOAT([ROIszX(0),ROIszY(0)])/2.
                      ROIsz=ROIszMM/curPix
                      halfSz=szFirst/2
                      x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
                      y1=ROUND(halfSz(1)+dxya(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(1))

                      submatrix=tempImg[x1:x2,y1:y2]
                      backMatrix=0

                      MTF=calculateMTF_NM(submatrix, curPix, dxya[0:1], typeMTF, backMatrix, WIDGET_INFO(btnCutLSFNM, /BUTTON_SET), FLOAT(cutLSFW(0)), v3d)
                      IF MTF.status EQ 0 THEN errStatus=errStatus+1 
                    ENDIF ELSE BEGIN
                      MTF=CREATE_STRUCT('empty',0)
                      errSize=1
                    ENDELSE
                  ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
                  IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
                ENDFOR
                IF errStatus GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of pointsource for '+STRING(errStatus, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Point position assumed to be at the selected ROI center.')
                If errSize THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
                results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
                updateTable
                updatePlot, 1,1,0
              END

              1: BEGIN; line (in plane or z dir)

                IF nFrames NE 0 THEN curPix=structImgs.(0).pix(0) ELSE curPix=structImgs.(first).pix(0)
                ROIszMM=FLOAT([ROIszX(0),ROIszY(0)])/2.
                ROIsz=ROIszMM/curPix
                halfSz=szFirst/2
                x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
                y1=ROUND(halfSz(1)+dxya(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(1))

                nnImg=TOTAL(markedArr)
                subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)
                
                proceed=1
                counter=0
                FOR i=0, nImg-1 DO BEGIN
                  IF markedArr(i) THEN BEGIN
                    IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
                    szThis=SIZE(tempImg, /DIMENSIONS)
                    IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                      subM[*,*,counter]=tempImg[x1:x2,y1:y2]
                      proceed=1
                      counter=counter+1
                    ENDIF ELSE BEGIN
                      MTF=CREATE_STRUCT('empty',0)
                      sv=DIALOG_MESSAGE('Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION)
                      proceed=0
                    ENDELSE
                  ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
                  IF proceed EQ 0 THEN BREAK
                ENDFOR

                IF proceed THEN BEGIN
                  IF v3d THEN BEGIN; line in z dir
                    MTFres=calculateMTF_NM(subM, curPix, dxya[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSFNM, /BUTTON_SET), FLOAT(cutLSFW(0)), v3d)
                    IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding location of line for one or more images. Location assumed to be at center of ROI.')
                  ENDIF ELSE BEGIN; line in plane
                    counter=0
                    FOR i=0, nImg-1 DO BEGIN
                      IF markedArr(i) THEN BEGIN
                        MTF=calculateMTF_NM(subM[*,*,counter], curPix, dxya[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSFNM, /BUTTON_SET), FLOAT(cutLSFW(0)), v3d)
                        counter=counter+1
                      ENDIF ELSE MTF=CREATE_STRUCT('empty',0)

                      IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
                    ENDFOR
                    
                  ENDELSE

                  results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
                  updateTable
                  updatePlot,1,1,0
                  WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
                ENDIF
              END

              2: BEGIN ; circular edge (2d or 3d)
                IF nFrames NE 0 THEN curPix=structImgs.(0).pix(0) ELSE curPix=structImgs.(first).pix(0)
                ROIszMM=FLOAT([ROIszX(0),ROIszY(0)])/2.
                ROIsz=ROIszMM/curPix
                halfSz=szFirst/2
                x1=ROUND(halfSz(0)+dxya(0)-ROIsz(0)) & x2=ROUND(halfSz(0)+dxya(0)+ROIsz(0))
                y1=ROUND(halfSz(1)+dxya(1)-ROIsz(1)) & y2=ROUND(halfSz(1)+dxya(1)+ROIsz(1))

                nnImg=TOTAL(markedArr)
                subM=FLTARR(x2-x1+1,y2-y1+1,nnImg)

                proceed=1
                counter=0
                FOR i=0, nImg-1 DO BEGIN
                  IF markedArr(i) THEN BEGIN
                    IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
                    szThis=SIZE(tempImg, /DIMENSIONS)
                    IF ARRAY_EQUAL(szThis[0:1], szFirst[0:1]) THEN BEGIN
                      subM[*,*,counter]=tempImg[x1:x2,y1:y2]
                      proceed=1
                      counter=counter+1
                    ENDIF ELSE BEGIN
                      MTF=CREATE_STRUCT('empty',0)
                      sv=DIALOG_MESSAGE('Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate MTF separately for images with the same size.',/INFORMATION)
                      proceed=0
                    ENDELSE
                  ENDIF
                  IF proceed EQ 0 THEN BREAK
                ENDFOR

              IF proceed THEN BEGIN
                IF v3d THEN BEGIN
                  MTFres=calculateMTF_NM(subM, curPix, dxya[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSFNM, /BUTTON_SET), FLOAT(cutLSFW(0)), v3d)
                  IF MTFres.status EQ 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for one or more images. Center of circle assumed to be at center of ROI.')
                ENDIF ELSE BEGIN
                  counter=0
                  errStatus=0
                  FOR i=0, nImg-1 DO BEGIN
                    IF markedArr(i) THEN BEGIN
                      MTF=calculateMTF_NM(subM[*,*,counter], curPix, dxya[0:1], typeMTF, -1, WIDGET_INFO(btnCutLSFNM, /BUTTON_SET), FLOAT(cutLSFW(0)), v3d)
                      IF MTF.status EQ 0 THEN errStatus=errStatus+1
                      counter=counter+1
                    ENDIF ELSE MTF=CREATE_STRUCT('empty',0)
                    
                    IF i EQ 0 THEN MTFres=CREATE_STRUCT('M0',MTF) ELSE MTFres=CREATE_STRUCT(MTFres, 'M'+STRING(i, FORMAT='(i0)'), MTF)
                  ENDFOR
                  IF errStatus GT 0 THEN sv=DIALOG_MESSAGE('Problem finding center of circle for '+STRING(errStatus, FORMAT='(i0)') +' of '+STRING(TOTAL(markedArr), FORMAT='(i0)') + ' images. Circle center assumed to be at the selected ROI center.')
                ENDELSE

                results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
                updateTable
                updatePlot,1,1,0
                WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
              ENDIF
            END
            ELSE: sv=DIALOG_MESSAGE('Not implementet selected MTF type yet',/INFORMATION)
          ENDCASE
        ENDELSE; analyse=MTF
      ENDIF; empty
    END

    'cw_plotMTFNM':  BEGIN
      IF analyse EQ 'MTF' AND results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM)) EQ 1 THEN BEGIN
        updatePlot, 1,1,0
      ENDIF
    END
    ;-----analyse-tab NPS --------------------------------------------------------------------------------------------------------------------

    'varImage': BEGIN
      IF N_ELEMENTS(activeImg) GT 1 THEN BEGIN
        WIDGET_CONTROL, /HOURGLASS
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
        tempImg=activeImg
        szImg=SIZE(tempImg,/DIMENSIONS)
        pix=structImgs.(sel).pix

        ;ROI size should be user-editable
        ;first uneditable defaults: 2x2mm (IPEM, recommended 10x10 pix, for xray 2x2 mm, mammo 0.7x0.7mm)

        ;traverse image with ROI and calculate variance
        ROIrad=5; npix radius of ROI
        varianceImg=FLTARR(szImg)

        FOR i=ROIrad, szImg(0)-1-ROIrad DO BEGIN
          FOR j=ROIrad, szImg(1)-1-ROIrad DO BEGIN
            IMAGE_STATISTICS, tempImg[i-ROIrad:i+ROIrad,j-ROIrad:j+ROIrad], VARIANCE=var
            varianceImg(i,j)=var
          ENDFOR
        ENDFOR

        ;show image
        ;evaluate example IPEM 32, page 82
        IMAGE_STATISTICS, varianceImg[szImg(0)*0.25:szImg(0)*0.75,szImg(1)*0.25:szImg(1)*0.75], MEAN=meanVal, STDDEV=stddevVal, MAX=maxxVal, MIN=minnVal
        minVal=meanVal-stddevVal;min(varianceImg[ROIrad:szImg(0)-1-ROIrad,ROIrad:szImg(1)-1-ROIrad])
        maxVal=meanVal+stddevVal;max(varianceImg[ROIrad:szImg(0)-1-ROIrad,ROIrad:szImg(1)-1-ROIrad])
        IF minVal LT 0 THEN BEGIN
          minVal=minnVal & maxVal=maxxVal
        ENDIF
        szX=drawXY
        szY=ROUND(szX*(szImg(1)*1./szImg(0)))
        IF nFrames EQ 0 THEN fileList=getListOpenFiles(structImgs,0,marked) ELSE fileList=getListFrames(structImgs.(0), marked)
          
        im=IMAGE(adjustWindowLevel(varianceImg, [minVal,maxVal]), WINDOW_TITLE='Variance image from file: '+STRMID(fileList(sel),2) )
      ENDIF ELSE sv=DIALOG_MESSAGE('No active image')
    END

    'drawNPSroi': BEGIN
      IF tags(0) NE 'EMPTY' THEN BEGIN
        tempImg=activeImg
        dxya(3)=1 ; center option has to be used
        WIDGET_CONTROL, useDelta, SET_BUTTON=1
        dxya(2)=0; no rotation allowed
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(dxya(2), FORMAT='(f0.1)')
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)
        curPix=structImgs.(sel(0)).pix
        proceed=1

        curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
        CASE curMode OF
          0: BEGIN
            WIDGET_CONTROL, txtNPSroiSz, GET_VALUE=ROIsz
            WIDGET_CONTROL, txtNPSroiDist, GET_VALUE=ROIdist
            WIDGET_CONTROL, txtNPSsubNN, GET_VALUE=subNN
            ROIsz=LONG(ROIsz(0)) & ROIdist=FLOAT(ROIdist(0)) & subNN=LONG(subNN(0))
            NPSrois=getNPSrois(SIZE(tempImg,/DIMENSIONS), dxya[0:1], ROIsz, ROUND(ROIdist/curPix(0)), subNN)
            IF max(NPSrois) NE 1 THEN proceed=0
          END
          1: BEGIN
            WIDGET_CONTROL, txtNPSroiSzX, GET_VALUE=ROIsz
            WIDGET_CONTROL, txtNPSsubSzX, GET_VALUE=subSz
            ROIsz=LONG(ROIsz(0)) & subSz=LONG(subSz(0))
            subSzMM=curPix(0)*ROIsz*subSz
            WIDGET_CONTROL, lblNPSsubSzMMX, SET_VALUE=STRING(subSzMM, FORMAT='(f0.1)')
          END
          ELSE:

        ENDCASE

        IF proceed THEN BEGIN
          analyse = 'NPS'
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          NPSres=CREATE_STRUCT('empty',0)
          updateTable
          updatePlot, 0,0,0
          redrawImg,0,0
        ENDIF ELSE analyse='NONE'
      ENDIF
    END

    'NPS': BEGIN
      IF tags(0) NE 'EMPTY' THEN BEGIN

        IF analyse NE 'NPS' THEN sv=DIALOG_MESSAGE('Show ROI first to verify size and position.',/INFORMATION) ELSE BEGIN

          WIDGET_CONTROL, /HOURGLASS

          nImg=N_ELEMENTS(tags)
          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          markedTemp=WHERE(markedArr EQ 1)
          first=markedTemp(0)
          tempImg=readImg(structImgs.(first).filename, 0)
          szFirst=SIZE(tempImg, /DIMENSIONS)
          nnImg=TOTAL(markedArr)

          curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
          CASE curMode OF
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
                CASE curMode OF
                  0: 
                  1: submatrix[*,*]=tempImg[x1:x2,y1:y2]
                ELSE:
                ENDCASE
                proceed=1
              ENDIF ELSE BEGIN
                NPS=CREATE_STRUCT('empty',0)
                sv=DIALOG_MESSAGE('Image size for image #'+STRING(i, FORMAT='(i0)')+' do not match first image. Calculate NPS separately for images with the same size.',/INFORMATION)
                proceed=0
              ENDELSE
            ENDIF
            IF proceed EQ 1 THEN BEGIN
              CASE curMode OF
                0: NPS=calculateNPS(tempImg, NPSrois, structImgs.(first).pix, smNPSw, sampFreq)
                1: NPS=calculateNPS_xray(submatrix, ROIsz, nsubSz, structImgs.(first).pix, stpRes, i)
              ELSE:
              ENDCASE
            ENDIF
            IF i EQ 0 THEN NPSres=CREATE_STRUCT('N0',NPS) ELSE NPSres=CREATE_STRUCT(NPSres, 'N'+STRING(i, FORMAT='(i0)'), NPS)
          ENDFOR

          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
          updateTable
          updatePlot, 1,1,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
        ENDELSE; analyse=NPS
      ENDIF; empty
    END

    ;--------------------------- ROI ---------------------------------------------------

    'ROI':BEGIN
      IF tags(0) NE 'EMPTY' THEN BEGIN
        analyse='ROI'
        tempImg=activeImg

        curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
        CASE curMode OF
          0: troi=WIDGET_INFO(typeROI, /BUTTON_SET)
          1: troi=WIDGET_INFO(typeROIX, /BUTTON_SET)
          2:
        ENDCASE

        CASE troi OF
          0: BEGIN; new
            WIDGET_CONTROL, txtMinWL, GET_VALUE=lower
            WIDGET_CONTROL, txtMaxWL, GET_VALUE=upper
            XROI, tempImg, REGIONS_OUT = thisROI, /BLOCK
            IF N_ELEMENTS(thisROI) GT 1 THEN BEGIN
              thisROI=thisROI(0)
              sv=DIALOG_MESSAGE('Only implemented for one ROI at the time in this version. Results for first ROI shown.', /INFORMATION)
            ENDIF

            IF OBJ_VALID(thisROI) THEN thisROI -> SetProperty, COLOR = [255,0,0], THICK = 2
          END
          1: BEGIN ;load
            adr = DIALOG_PICKFILE(TITLE='Load ROI', /READ, FILTER='*.sav', /FIX_FILTER, PATH=defPath)
            IF adr NE '' THEN BEGIN
              RESTORE, FILENAME=adr, RESTORED_OBJECTS=loadedObj
              IF ISA(loadedObj,'IDLgrROI') THEN thisROI=loadedObj ELSE sv=DIALOG_MESSAGE('Found no valid ROIs in the selected file.')
              OBJ_DESTROY, loadedObj
            ENDIF
          END
          ELSE:
        ENDCASE

        IF OBJ_VALID(thisROI) THEN BEGIN
          SAVE, thisROI, FILENAME=thisPath+'data\thisROI.sav'
          imsz=SIZE(tempImg, /DIMENSIONS)
          maskResult = thisROI -> ComputeMask(DIMENSIONS = imsz)

          IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames
          resArr=FLTARR(4,nImg)
          WIDGET_CONTROL, /HOURGLASS
          markedArr=INTARR(nIMG)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
              imszTemp=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(imszTemp[0:1], imsz[0:1]) THEN BEGIN
                IMAGE_STATISTICS, tempImg, MASK = maskResult, MINIMUM=minval, MAXIMUM=maxval, MEAN=avgval, STDDEV=stdval
                resArr[*,i]=[minval,maxval,avgval,stdval]
              ENDIF
            ENDIF
            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=''

          roiRes=resArr
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
          updateTable
          updatePlot, 0,0,0
          redrawImg,0,0
          WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
        ENDIF; thisROI valid

      ENDIF; empty
    END
    ;-----analyse tab CT number Linearity-----------------------------------------------------------------------------------------
    'drawLinRois': BEGIN; visual proof of ROIs
      IF tags(0) NE 'EMPTY' THEN BEGIN
        analyse='CTLIN'
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
        IF nFrames EQ 0 THEN pix=structImgs.(sel).pix ELSE pix=structImgs.(0).pix
        tempImg=activeImg
        szImg=SIZE(tempImg,/DIMENSIONS)

        WIDGET_CONTROL, txtLinROIrad, GET_VALUE=rad1
        WIDGET_CONTROL, txtLinROIrad2, GET_VALUE=rad2
        rad1=ROUND(FLOAT(rad1(0))/pix(0)) & rad2=ROUND(FLOAT(rad2(0))/pix(0)); assume x,y pix equal ! = normal

        imgCenterOffset=[0,0,0,0]
        IF dxya(3) EQ 1 THEN imgCenterOffset=dxya

        CTlinROIs=getSampleRois(szImg, imgCenterOffset, rad1,rad2)
        IF max(CTlinROIs) EQ 1 THEN BEGIN
          redrawImg,0,0
  
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          CTlinres=0
          updateTable
          updatePlot, 0,0,0
        ENDIF ELSE analyse='NONE'
      ENDIF; empty
    END

    'Linearity': BEGIN; exctract results
      IF tags(0) NE 'EMPTY' THEN BEGIN
        IF analyse NE 'CTLIN' THEN sv=DIALOG_MESSAGE('Show ROIs first to verify size and positions.',/INFORMATION) ELSE BEGIN

          WIDGET_CONTROL, /HOURGLASS
          IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames
          szROI=SIZE(CTlinROIs, /DIMENSIONS)

          resArr=FLTARR(szROI(2)+1,nImg)-1;mean+stddev for all and #pix
          materialData=read_csv(thisPath+'data\CTlinearity.csv')
          sorting=materialData.field6

          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errStatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
              imszTemp=SIZE(tempImg, /DIMENSIONS)
              IF ARRAY_EQUAL(imszTemp[0:1], szROI[0:1]) THEN BEGIN
                FOR r=0, szROI(2)-1 DO BEGIN
                  sr=sorting(r)
                  maske=CTlinROIs[*,*,sr]
                  IMAGE_STATISTICS, tempImg, COUNT=nPix, MEAN=meanHU, STDDEV=stddevHU, MASK=maske
                  resArr(r,i)=meanHU
                ENDFOR
                a0=REGRESS(resArr[0:szROI(2)-1,i],materialData.field5, MCORRELATION=mcorr)
                resArr(szROI(2),i)=mcorr
              ENDIF ELSE errStatus=1
            ENDIF;markedArr
            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=''
          If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
          CTlinRes=resArr
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
          updateTable
          updatePlot, 1,1,0
          redrawImg,0,0 & updateInfo=1
        ENDELSE
      ENDIF
    END

    ;----analyse tab Slice Thickness--------------------------------------------------------------------------------------------------
    'drawRamps': BEGIN; visual proof of lines over ramps
      IF tags(0) NE 'EMPTY' THEN BEGIN
        analyse='SLICETHICK';to activate drawing of ramps in redrawImg

        results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
        sliceThickResTab=0
        sliceThickRes=CREATE_STRUCT('empty',0)
        updateTable
        updatePlot, 0,0,0
        redrawImg,0,0
      ENDIF; empty
    END

    'SliceThick': BEGIN; extract results
      IF tags(0) NE 'EMPTY' THEN BEGIN
        IF analyse NE 'SLICETHICK' THEN sv=DIALOG_MESSAGE('Show ramps first to verify size and positions.',/INFORMATION) ELSE BEGIN

          WIDGET_CONTROL, /HOURGLASS
          IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames

          WIDGET_CONTROL, txtRampLen, GET_VALUE=len
          WIDGET_CONTROL, txtRampBackG, GET_VALUE=rampBackG
          WIDGET_CONTROL, txtRampSearch, GET_VALUE=nSearch
          WIDGET_CONTROL, txtRampAverage, GET_VALUE=nAvg
          WIDGET_CONTROL, txtRampDist, GET_VALUE=rampDist
          rampDistPix=FLOAT(rampDist(0))
          WIDGET_CONTROL, txtRampLen, GET_VALUE=len
          ;lenPix=FLOAT(len(0))         
          IF dxya(3) EQ 1 THEN imgCenterOffset=dxya ELSE imgCenterOffset=[0,0,0,0]
          WIDGET_CONTROL, cw_ramptype, GET_VALUE=ramptype
          
          nSearch=LONG(nSearch(0))
          nAvg=LONG(nAvg(0))

          resArr=FLTARR(7,nImg)
          daRad=dxya(3)*dxya(2)/!radeg

          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errLogg=''
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN

              IF nFrames NE 0 THEN BEGIN
                tempImg=readImg(structImgs.(0).filename, i)
                pix=structImgs.(0).pix
                resArr[0,i]=structImgs.(0).sliceThick
              ENDIF ELSE BEGIN
                tempImg=readImg(structImgs.(i).filename, 0)
                pix=structImgs.(i).pix
                resArr[0,i]=structImgs.(i).sliceThick
              ENDELSE
              sizeThis=SIZE(tempImg,/DIMENSIONS)
              lenPix=ROUND(FLOAT(len(0))/pix(0)); length in pixels
              nPixBackG=ROUND(rampBackG(0)/pix(0))

              nRamps=4
              CASE ramptype OF
                0: ramps=getRamps(sizeThis, imgCenterOffset, rampDistPix/pix(0), lenPix)
                1: BEGIN; beaded ramp
                    ramps=getRamps(sizeThis, imgCenterOffset, 45./pix(0), lenPix)
                    ramps2=getRamps(sizeThis, imgCenterOffset, 25./pix(0), lenPix)
                    nRamps=6
                  END
              ELSE:ramps=getRamps(sizeThis, imgCenterOffset, rampDistPix/pix(0), lenPix)
              ENDCASE

              ;get line, FWHM and slice thickness              
              FOR l=0, nRamps-1 DO BEGIN 

                IF l LE 3 THEN vec=getProfile(tempImg,ramps[0:1,l],ramps[2:3,l]) ELSE vec=getProfile(tempImg,ramps2[0:1,l-2],ramps2[2:3,l-2])

                IF nSearch GT 0 THEN BEGIN
                  nLines=nSearch*2+1
                  
                  vecTemp=FLTARR(N_ELEMENTS(vec),nLines)
                  FOR k=-nSearch, nSearch DO BEGIN
                    IF l LE 1 THEN BEGIN
                      vecTemp[*,k+nSearch]=getProfile(tempImg,[ramps(0,l),ramps(1,l)+k],[ramps(2,l),ramps(3,l)+k])
                    ENDIF ELSE BEGIN
                      IF l LE 3 THEN vecTemp[*,k+nSearch]=getProfile(tempImg,[ramps(0,l)+k,ramps(1,l)],[ramps(2,l)+k,ramps(3,l)])
                      IF l GT 3 THEN vecTemp[*,k+nSearch]=getProfile(tempImg,[ramps2(0,l-2)+k,ramps2(1,l-2)],[ramps2(2,l-2)+k,ramps2(3,l-2)])
                    ENDELSE
                  ENDFOR
                  vecSum=TOTAL(vecTemp,1)
                  maxProf=WHERE(vecSum EQ max(vecSum))
                  maxProf=maxProf(0)
                  vec=vecTemp[*,maxProf]
                  IF nAvg GT 0 THEN BEGIN
                    IF maxProf-nAvg GE 0 AND maxProf+nAvg LT nLines THEN vec=TOTAL(vecTemp[*,maxProf-nAvg:maxProf+nAvg],2)/(nAvg*2+1) $
                    ELSE errLogg=errLogg+'Image '+STRING(i,FORMAT='(i0)')+', Line '+STRING(l,FORMAT='(i0)')+': Max profile to close to border of search-area. Single profile with max used (no averaging).'+newline
                  ENDIF
                  
                ENDIF
                
                szVec=SIZE(vec,/DIMENSIONS)
                IF nPixBackG GT szVec(0) THEN nPixBackG= szVec(0)
                ;find background
                bgVec=[vec[0:nPixBackG],vec[szVec(0)-nPixBackG:szVec(0)-1]]
                backGr=MEAN(bgVec)

                halfmax=0.5*(MAX(vec)+backGr); 0.5(max-bg)+bg = 0.5(max+bg)
                CASE ramptype OF
                0: BEGIN; wire ramp
                  res=getWidthAtThreshold(vec, halfmax)
                  resArr[l+1,i]=0.42*(res(0))*pix(0)/cos(daRad); sliceThickness=FWHM*0.42 according to Catphan manual
                  END  
                1: BEGIN; bead ramp
                  ;find upper envelope curve
                  derived=vec-shift(vec,1)
                  subz=where(derived LT 0)-1
                  dsubz=subz-shift(subz,1)
                  ss=INTARR(n_elements(dsubz))
                  ss(1)=1
                  FOR s=2, n_elements(dsubz)-1 DO IF dsubz(s) GT 1 THEN ss(s)=1
                  idxes=WHERE(ss EQ 1)
                  idxmax=subz(idxes)
                  vecInt=INTERPOL(vec(idxmax),idxmax, INDGEN(N_ELEMENTS(vec)));interpolate to regular stepsize
                  res=getWidthAtThreshold(vecInt, halfmax)
                  IF l LE 3 THEN zinc=1. ELSE zinc=.25;1 or 0.25mm z spacing between beads
                  resArr[l+1,i]=zinc/2.0*(res(0)*pix(0)/cos(daRad)) ; 2mm axial spacing
                  END
                ELSE:
                ENDCASE
                structTemp=CREATE_STRUCT('background',backGr,'nBackGr',nPixBackG,'vector',vec,'halfMax',halfMax,'firstLast',[res(1)-res(0)/2.,res(1)+res(0)/2.],'maxVal',MAX(vec))
                IF l EQ 0 THEN lineStruct=CREATE_STRUCT('L0',structTemp) ELSE lineStruct=CREATE_STRUCT(lineStruct,'L'+STRING(l, FORMAT='(i0)'),structTemp)
              ENDFOR
              
            ENDIF ELSE lineStruct=CREATE_STRUCT('empty',0)
            IF i EQ 0 THEN sliceThickRes=CREATE_STRUCT('img0',lineStruct) ELSE sliceThickRes=CREATE_STRUCT(sliceThickRes,'img'+STRING(i,FORMAT='(i0)'),lineStruct)

            IF ramptype EQ 0 THEN BEGIN
              resArr[5,i]=MEAN(resArr[1:4,i])
              resArr[6,i]=100.0*(resArr[5,i]-resArr[0,i])/resArr[0,i]
            ENDIF

            WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
          ENDFOR
          WIDGET_CONTROL, lblProgress, SET_VALUE=''
          IF errLogg NE '' THEN sv=DIALOG_MESSAGE(errLogg)
          errLogg=''
  
          sliceThickResTab=resArr
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
          updateTable
          updatePlot,1,1,0
        ENDELSE
      ENDIF
    END


    ;----analyse tab FWHM--------------------------------------------------------------------------------------------------
    'fwhm': BEGIN
      IF tags(0) NE 'EMPTY' THEN BEGIN
        WIDGET_CONTROL, /HOURGLASS
        analyse='FWHM'
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
        tempImg=activeImg
        szImg=SIZE(tempImg,/DIMENSIONS)

        IF nFrames EQ 0 THEN BEGIN
          pix=structImgs.(sel).pix
          nImg=N_ELEMENTS(tags)
        ENDIF ELSE BEGIN
          pix=structImgs.(0).pix
          nImg=nFrames
        ENDELSE
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
            IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
            imszTemp=SIZE(tempImg, /DIMENSIONS)
            IF ARRAY_EQUAL(imszTemp[0:1], szImg[0:1]) THEN BEGIN
              res=get_fwhm(tempImg, center, pix(0))
              IF N_ELEMENTS(res) GT 1 THEN resArr[*,i]=res ELSE resArr[*,i]=-1
            ENDIF ELSE errStatus=1
          ENDIF
          WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'
        ENDFOR
        WIDGET_CONTROL, lblProgress, SET_VALUE=''
        If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
        results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
        fwhmRes=resArr
        updateTable
        updatePlot, 1,1,0
        redrawImg, 0,0
        WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=0
      ENDIF; empty
    END

    ;-------- NM energy spectrum ------------------
    'loadSpectrum': BEGIN
      adr=DIALOG_PICKFILE(TITLE='Open .txt file with spectrum data',/READ, FILTER='*.txt', /FIX_FILTER, PATH=defPath)
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
          IF N_ELEMENTS(elem) NE 3 THEN BEGIN
            sv=DIALOG_MESSAGE('File not in expected format: Tabular separated, 3 columns')
            BREAK
          ENDIF
          elems=[[elems],[elem[1:2]]]
          elem=''
        ENDWHILE
        CLOSE, filenhet
        FREE_LUN, filenhet

        IF MAX(elems) GT 0 THEN BEGIN
          ;plot/analyse data
          gfit=gaussfit(elems[0,*],elems[1,*], coeff, NTERMS=3)
          energyRes=CREATE_STRUCT('gaussCoeff',coeff,'curve',elems[0:1,*])
          analyse='ENERGYSPEC'
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
          updateTable
          updatePlot, 1,1,0
        ENDIF ELSE BEGIN
          sv=DIALOG_MESSAGE('Found no values from the selected file.')
          analyse='NONE'
          energyRes=!Null
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'ENERGYSPEC',analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
          updateTable
          updatePlot, 0,0,0
        ENDELSE

      ENDIF;adr ne ''
    END
    
    'radialProfile': BEGIN

      IF tags(0) NE 'EMPTY' THEN BEGIN
        WIDGET_CONTROL, /HOURGLASS
        
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
        IF nFrames EQ 0 THEN pix=structImgs.(sel).pix ELSE pix=structImgs.(0).pix
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

        IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames
        markedArr=INTARR(nImg)
        IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1

        ;rebin to equally spaced resolution pix/10
        radius=MIN([centerPos(0),szM(0)-centerPos(0),centerPos(1), szM(1)-centerPos(1)])-1; maximim radius with full dataset
        newdists=FINDGEN(radius*10)*0.1*pix(0); regular x axis, cuts data at position where start to loose info due to less data in perpendicular directions
        pixNew=.1*pix(0)
        ;smooth by ~the new pix size if possible
        test=WHERE(dists*pix(0) LT max(newdists))
        smoothSz=ROUND(N_ELEMENTS(test)/N_ELEMENTS(newdists))     

        radialRes=FLTARR(N_ELEMENTS(newdists), nImg)
        errStatus=0
        FOR i=0, nImg-1 DO BEGIN
          IF markedArr(i) THEN BEGIN
            ;check if same size
            IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
            imszTemp=SIZE(tempImg, /DIMENSIONS)
            IF ARRAY_EQUAL(imszTemp[0:1], szM[0:1]) THEN BEGIN
              If smoothSz GT 2 THEN pixVals=SMOOTH(tempImg(sorting),smoothSz) ELSE pixVals=tempImg(sorting)
              radialRes[*,i]=INTERPOL(pixVals, dists*pix(0), newdists); linear interpolation
              radialRes[0:9,i]=radialRes[10,i];don't trust first 10 values (first original pix)
            ENDIF ELSE errStatus=1
          ENDIF
          WIDGET_CONTROL, lblProgress, SET_VALUE='Progress: '+STRING(i*100./nIMG, FORMAT='(i0)')+' %'

        ENDFOR
        WIDGET_CONTROL, lblProgress, SET_VALUE=''
        If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
        analyse='RADIAL'
        results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'RADIAL',analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
        updateTable
        updatePlot, 1,1,0
        redrawImg, 0,0
        WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
      ENDIF

    END

    
    'plotScanSpeed':BEGIN
      IF tags(0) NE 'EMPTY' THEN BEGIN
        WIDGET_CONTROL, /HOURGLASS
        analyse='SCANSPEED'
        results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),'SCANSPEED',analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
        updateTable
        updatePlot, 1,1,0
        redrawImg, 0,0
        WIDGET_CONTROL, wtabResult, SET_TAB_CURRENT=1
      ENDIF
    END


    ;-----analyse tab contrast-----------------------------------------------------------------------------------------
    'drawConRoisNM': BEGIN; visual proof of ROIs
      IF tags(0) NE 'EMPTY' THEN BEGIN
        analyse='CONTRAST'
        sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
        IF nFrames EQ 0 THEN pix=structImgs.(sel).pix ELSE pix=structImgs.(0).pix
        tempImg=activeImg
        szImg=SIZE(tempImg,/DIMENSIONS)

        WIDGET_CONTROL, txtConR1NM, GET_VALUE=rad1
        WIDGET_CONTROL, txtConR2NM, GET_VALUE=rad2
        rad1=ROUND(FLOAT(rad1(0))/pix(0)) & rad2=ROUND(FLOAT(rad2(0))/pix(0)); assume x,y pix equal ! = normal

        imgCenterOffset=[0,0,0,0]
        IF dxya(3) EQ 1 THEN imgCenterOffset=dxya

        conROIs=getConNMRois(szImg, imgCenterOffset, rad1,rad2)
        redrawImg,0,0

        results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=0
        contrastRes=0
        updateTable
        updatePlot, 0,0,0
      ENDIF; empty
    END

    'contrastNM': BEGIN; exctract results
      IF tags(0) NE 'EMPTY' THEN BEGIN
        IF analyse NE 'CONTRAST' THEN sv=DIALOG_MESSAGE('Show ROIs first to verify size and positions.',/INFORMATION) ELSE BEGIN

          WIDGET_CONTROL, /HOURGLASS
          IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames
          szROI=SIZE(conROIs, /DIMENSIONS)

          resArr=FLTARR(szROI(2),nImg);minVals for all ROIs + background

          markedArr=INTARR(nImg)
          IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
          errStatus=0
          FOR i=0, nImg-1 DO BEGIN
            IF markedArr(i) THEN BEGIN
              ;check if same size
              IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
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
          WIDGET_CONTROL, lblProgress, SET_VALUE=''
          If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Results restricted to images with same size.',/INFORMATION)
          contrastRes=resArr
          results(getResNmb(WIDGET_INFO(wtabModes, /TAB_CURRENT),analyse,analyseStringsCT,analyseStringsXray,analyseStringsNM))=1
          updateTable
          updatePlot, 1,1,0
          redrawImg,0,0 & updateInfo=1
        ENDELSE
      ENDIF
    END
    
    ;**************************************************** Copy to clipboard *******************************************

  'copyInfo':BEGIN
    tags=TAG_NAMES(structImgs)
    IF tags(0) NE 'EMPTY' THEN BEGIN
      nImg=N_TAGS(structImgs)
      nInfo=N_TAGS(structImgs.(0))
      infoTable=STRARR(nInfo,nImg+1)
      infoTable[*,0]=TAG_NAMES(structImgs.(0));column headers
      FOR i=0, nImg-1 DO BEGIN
        FOR j=0, nInfo-1 DO BEGIN
          infoTable[j,i+1]=STRING(structImgs.(i).(j))
        ENDFOR
      ENDFOR
      CLIPBOARD.set, STRJOIN(infoTable, STRING(9B))
    ENDIF
  END

  'copyTbl': BEGIN
    curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
    CASE curMode OF
      0: curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
      1: curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
      2: curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
      ELSE:curTab=-1
    ENDCASE

    IF results(curTab) EQ 1 THEN BEGIN
      WIDGET_CONTROL, resTab, GET_VALUE=resTable;, /USE_TABLE_SELECT
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
    ENDIF
  END

  'copyCurve': updatePlot, 0,0,1
  
  ;**************************************************** Push data to iImage/iPlot *******************************************

  'iPlot': updatePlot, 0,0,2

  'ax':BEGIN
    sel=WIDGET_INFO(listFiles, /LIST_SELECT)
    IF nFrames NE 0 THEN pix=structImgs.(0).pix ELSE pix=structImgs.(sel).pix
    iImage, activeImg, TITLE='Axial image', ASPECT_RATIO=pix(1)/pix(0)
  END

  'sumax': BEGIN
    IF nFrames EQ 0 THEN nImg=N_ELEMENTS(tags) ELSE nImg=nFrames
    pix=structImgs.(0).pix
    markedArr=INTARR(nImg)
    IF marked(0) EQ -1 THEN markedArr=markedArr+1 ELSE markedArr(marked)=1
    szAct=SIZE(activeImg, /DIMENSIONS)
    sumArr=FLTARR(szAct(0),szAct(1))
    errStatus=0
    FOR i=0, nImg-1 DO BEGIN
      IF markedArr(i) THEN BEGIN
        ;check if same size
        IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
        imszTemp=SIZE(tempImg, /DIMENSIONS)
        IF ARRAY_EQUAL(imszTemp[0:1], szAct[0:1]) THEN sumArr=sumArr+tempImg ELSE errStatus=1
      ENDIF;markedArr
    ENDFOR
    If errstatus THEN sv=DIALOG_MESSAGE('The images have different size. Sum of images restricted to those with same size.',/INFORMATION)
    iImage, sumArr, TITLE='Sum of axial images', ASPECT_RATIO=pix(1)/pix(0)
  END

  'iImageRes': IF N_ELEMENTS(activeResImg) GT 1 THEN iImage, activeResImg

  'cor':BEGIN
    WIDGET_CONTROL, /HOURGLASS
    IF nFrames EQ 0 THEN BEGIN
      nImg=N_TAGS(structImgs)
      firstImg=readImg(structImgs.(0).filename, 0)
    ENDIF ELSE BEGIN
      nImg=nFrames
      firstImg=readImg(structImgs.(0).filename, 0)
    ENDELSE

    szFirst=SIZE(firstImg, /DIMENSIONS)
    corTemp=FLTARR(szFirst(0), nImg)
    center=szFirst/2+dxya[0:1]

    FOR i=0, nImg-1 DO BEGIN
      IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
      szTemp=SIZE(firstImg, /DIMENSIONS)
      IF ARRAY_EQUAL(szTemp,szFirst) THEN BEGIN
        corTemp[*,i]=tempImg[*,center(1)]
      ENDIF ELSE BEGIN
        sv=DIALOG_MESSAGE('Image number ' + STRING(i, FORMAT='(i0)')+' do not have the same size as the first image. Coronal image can not be generated.')
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
    IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=nFrames
    IF nFrames NE 0 THEN firstImg=readImg(structImgs.(0).filename, 0) ELSE firstImg=readImg(structImgs.(0).filename, 0)

    szFirst=SIZE(firstImg, /DIMENSIONS)
    sagTemp=FLTARR(szFirst(1), nImg)
    center=szFirst/2+dxya[0:1]

    FOR i=0, nImg-1 DO BEGIN
      IF nFrames NE 0 THEN tempImg=readImg(structImgs.(0).filename, i) ELSE tempImg=readImg(structImgs.(i).filename, 0)
      szTemp=SIZE(firstImg, /DIMENSIONS)
      IF ARRAY_EQUAL(szTemp,szFirst) THEN BEGIN
        sagTemp[*,i]=tempImg[center(0),*]
      ENDIF ELSE BEGIN
        sv=DIALOG_MESSAGE('Image number ' + STRING(i, FORMAT='(i0)')+' do not have the same size as the first image. Sagittal image can not be generated.')
        sagTemp=-1
        BREAK
      ENDELSE
    ENDFOR

    IF N_ELEMENTS(sagTemp) GT 1 AND nImg GT 1 THEN BEGIN
      IF structImgs.(0).sliceThick EQ -1 THEN asR=0 ELSE asR=structImgs.(0).sliceThick/structImgs.(0).pix(1)
      iImage, sagTemp, TITLE='Sagittal image', ASPECT_RATIO=asR
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
IF ev.ID EQ cw_typeMTF OR ev.ID EQ cw_formLSFX OR ev.ID EQ cw_typeMTFNM THEN clearRes, 'MTF'

;********************************************* Textfield changed **************************************************************
; WindowLevel, dx,dy,da
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
        redrawImg,0,0
      END
      txtDeltaY: BEGIN
        WIDGET_CONTROL, txtDeltaY, GET_VALUE=dy
        dy=LONG(dy(0))
        WIDGET_CONTROL, txtDeltaY, SET_VALUE=STRING(dy, FORMAT='(i0)')
        dxya(1)=dy & dxya(3)=1
        redrawImg,0,0
      END
      txtDeltaA: BEGIN
        WIDGET_CONTROL, txtDeltaA, GET_VALUE=da
        da=FLOAT(comma2pointFloat(da(0)))
        WIDGET_CONTROL, txtDeltaA, SET_VALUE=STRING(da, FORMAT='(f0.1)')
        WIDGET_CONTROL, txtDeltaA, GET_VALUE=da
        da=FLOAT(da(0))
        dxya(2)=da & dxya(3)=1
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
        WIDGET_CONTROL, txtMinRangeX, SET_VALUE=STRING(val, FORMAT='(f0.'+nDecimals(val)+')')
        updatePlot, 0,0,0
      END
      txtMaxRangeX: BEGIN
        WIDGET_CONTROL, txtMaxRangeX, GET_VALUE=val
        val=FLOAT(comma2pointFloat(val(0)))
        WIDGET_CONTROL, txtMaxRangeX, SET_VALUE=STRING(val, FORMAT='(f0.'+nDecimals(val)+')')
        updatePlot, 0,0,0
      END
      txtMinRangeY: BEGIN
        WIDGET_CONTROL, txtMinRangeY, GET_VALUE=val
        val=FLOAT(comma2pointFloat(val(0)))
        WIDGET_CONTROL, txtMinRangeY, SET_VALUE=STRING(val, FORMAT='(f0.'+nDecimals(val)+')')
        updatePlot, 0,0,0
      END
      txtMaxRangeY: BEGIN
        WIDGET_CONTROL, txtMaxRangeY, GET_VALUE=val
        val=FLOAT(comma2pointFloat(val(0)))
        WIDGET_CONTROL, txtMaxRangeY, SET_VALUE=STRING(val, FORMAT='(f0.'+nDecimals(val)+')')
        updatePlot, 0,0,0
      END
      
      txtMTFroiSz:BEGIN
        WIDGET_CONTROL, txtMTFroiSz, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtMTFroiSz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'MTF'
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
        clearRes, 'MTF'
      END
      txtMTFroiSzY:BEGIN
        WIDGET_CONTROL, txtMTFroiSzY, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtMTFroiSzY, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'MTF'
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
        clearRes, 'MTF'
      END
      txtMTFroiSzYNM:BEGIN
        WIDGET_CONTROL, txtMTFroiSzYNM, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtMTFroiSzYNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'MTF'
      END
      txtcutLSFWNM:BEGIN
        WIDGET_CONTROL, txtcutLSFWNM, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtcutLSFWNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'MTF'
      END

      txtLinROIrad:BEGIN
        WIDGET_CONTROL, txtLinROIrad, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtLinROIrad, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'CTLIN'
      END
      txtLinROIrad2:BEGIN
        WIDGET_CONTROL, txtLinROIrad2, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtLinROIrad2, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'CTLIN'
      END
      
      txtRampDist:BEGIN
        WIDGET_CONTROL, txtRampDist, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtRampDist, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'SLICETHICK'
      END
      txtRampLen:BEGIN
        WIDGET_CONTROL, txtRampLen, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtRampLen, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'SLICETHICK'
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
        clearRes, 'SLICETHICK'
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
        clearRes; all results potenially based on STP results
      END
      
      txtHomogROIsz:BEGIN
        WIDGET_CONTROL, txtHomogROIsz, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtHomogROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'HOMOG
      END
      txtHomogROIszX:BEGIN
        WIDGET_CONTROL, txtHomogROIszX, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtHomogROIszX, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'HOMOG
      END
      txtHomogROIdist:BEGIN
        WIDGET_CONTROL, txtHomogROIdist, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtHomogROIdist, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'HOMOG
      END
      txtHomogROIszNM:BEGIN
        WIDGET_CONTROL, txtHomogROIszNM, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtHomogROIszNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'HOMOG
      END
      txtHomogROIdistXNM:BEGIN
        WIDGET_CONTROL, txtHomogROIdistXNM, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtHomogROIdistXNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'HOMOG
      END
      txtHomogROIdistYNM:BEGIN
        WIDGET_CONTROL, txtHomogROIdistYNM, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtHomogROIdistYNM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'HOMOG
      END
      txtNoiseROIsz:BEGIN
        WIDGET_CONTROL, txtNoiseROIsz, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtNoiseROIsz, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'NOISE'
      END
   
      txtNPSroiSz: BEGIN
        WIDGET_CONTROL, txtNPSroiSz, GET_VALUE=val
        val=LONG(val(0))
        IF val LE 0 THEN val=1
        WIDGET_CONTROL, txtNPSroiSz, SET_VALUE=STRING(val, FORMAT='(i0)')
        clearRes, 'NPS'
      END
      txtNPSroiDist: BEGIN
        WIDGET_CONTROL, txtNPSroiDist, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        IF val EQ 0 THEN val=1.
        WIDGET_CONTROL, txtNPSroiDist, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'NPS'
      END
      txtNPSsubNN: BEGIN
        WIDGET_CONTROL, txtNPSsubNN, GET_VALUE=val
        val=LONG(val(0))
        IF val LE 0 THEN val=1
        WIDGET_CONTROL, txtNPSsubNN, SET_VALUE=STRING(val, FORMAT='(i0)')
        clearRes, 'NPS'
      END
      txtSmoothNPS: BEGIN
        WIDGET_CONTROL, txtSmoothNPS, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtSmoothNPS, SET_VALUE=STRING(val, FORMAT='(f0.3)')
        clearRes, 'NPS'
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
        clearRes, 'NPS'
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
        clearRes, 'NPS'
      END    
       
      txtNAvgSpeedNM: BEGIN
        WIDGET_CONTROL, txtNAvgSpeedNM, GET_VALUE=val
        val=LONG(val(0))
        IF val/2 EQ (val*1.0)/2 THEN val=val-1; assure odd number
        IF val LT 1 THEN val=1
        WIDGET_CONTROL, txtNAvgSpeedNM, SET_VALUE=STRING(val, FORMAT='(i0)')
        IF analyse EQ 'SCANSPEED' THEN updatePlot, 0,0,0
      END
      txtSpeedROIheight: BEGIN
        WIDGET_CONTROL, txtSpeedROIheight, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtSpeedROIheight, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        IF analyse EQ 'SCANSPEED' THEN updatePlot,0,0,0
        END
      txtScanSpeedMedian: BEGIN
        WIDGET_CONTROL, txtScanSpeedMedian, GET_VALUE=val
        val=LONG(val(0))
        IF val/2 EQ (val*1.0)/2 THEN val=val-1; assure odd number
        IF val LT 1 THEN val=1
        WIDGET_CONTROL, txtScanSpeedMedian, SET_VALUE=STRING(val, FORMAT='(i0)')
        IF analyse EQ 'SCANSPEED' THEN updatePlot, 0,0,0
      END
      txtRadialMedian: BEGIN
        WIDGET_CONTROL, txtRadialMedian, GET_VALUE=val
        val=LONG(val(0))
        IF val/2 EQ (val*1.0)/2 THEN val=val-1; assure odd number
        IF val LT 1 THEN val=1
        WIDGET_CONTROL, txtRadialMedian, SET_VALUE=STRING(val, FORMAT='(i0)')
        IF analyse EQ 'RADIAL' THEN updatePlot, 0,0,0
      END
      txtConR1NM: BEGIN
        WIDGET_CONTROL, txtConR1NM, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtConR1NM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'CONTRAST'
      END
      txtConR2NM: BEGIN
        WIDGET_CONTROL, txtConR2NM, GET_VALUE=val
        val=ABS(FLOAT(comma2pointFloat(val(0))))
        WIDGET_CONTROL, txtConR2NM, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        clearRes, 'CONTRAST'
      END

      ELSE:
    ENDCASE
  ENDIF
ENDIF

;***************************************WIDGET_TABLE events***************************************************************
IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' THEN BEGIN
  curMode=WIDGET_INFO(wtabModes, /TAB_CURRENT)
  CASE curMode OF
    0:curTab=WIDGET_INFO(wtabAnalysisCT, /TAB_CURRENT)
    1:curTab=WIDGET_INFO(wtabAnalysisXray, /TAB_CURRENT)
    2:curTab=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
  ENDCASE

  IF ev.ID EQ resTab AND results(curTab) EQ 1 THEN BEGIN
    active=1
    IF curTab EQ 2 THEN BEGIN
      curTest=WIDGET_INFO(wtabAnalysisNM, /TAB_CURRENT)
      IF curTest EQ getResNmb(2,'ENERGYSPEC','','',analyseStringsNM) THEN active=0
    ENDIF
    IF active THEN BEGIN
      tabSel=WIDGET_INFO(resTab,/TABLE_SELECT)
      rowNo=tabSel(1)
      IF marked(0) NE -1 THEN sel=marked(rowNo) ELSE sel=rowNo
      WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel
      redrawImg,0,1 & updateInfo=1
      updateTable
      updatePlot, 0,0,0
      ;IF analyseStrings(curTab+1) EQ 'STP' THEN WIDGET_CONTROL, resTab, EDITABLE=1, USE_TABLE_SELECT=[0,0,0,tabSel(1)] ELSE WIDGET_CONTROL, resTab, EDITABLE=0
     ENDIF
  ENDIF
ENDIF

;IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CH' THEN BEGIN
;  editab=WIDGET_INFO(resTab, /TABLE_EDITABLE)
;  IF max(editab) EQ 1 THEN BEGIN
;    change=0
;    IF ev.type EQ 0 THEN BEGIN;single character
;      IF ev.ch EQ 13 THEN change=1 ; enter
;    ENDIF
;    IF change THEN BEGIN ;pressed enter in editable cell
;      WIDGET_CONTROL, resTab, GET_VALUE=tableTemp
;      stpRes.table=FLOAT(tableTemp)
;      results(0)=1
;      updateTable
;      updatePlot, 1,0,0
;    ENDIF
;  ENDIF
;ENDIF

; ************************************** WIDGET_DRAW events **************************************************************
IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN

  imgSz=SIZE(activeImg, /DIMENSIONS)
  maxSz=max(imgSz[0:1])
  xx=ev.X*maxSz/drawXY & yy = ev.Y*maxSz/drawXY

  IF xx LT 0 THEN xx=0
  IF yy LT 0 THEN yy=0
  IF xx GT imgSz(0)-1 THEN xx=imgSz(0)-1
  IF yy GT imgSz(1)-1 THEN yy=imgSz(1)-1
  curVal=activeImg(xx,yy)
  WIDGET_CONTROL, lblCursorValue, SET_VALUE=STRING(curVal, FORMAT='(f0.'+nDecimals(curVal)+')')
  xx=xx-imgSz(0)/2-dxya(0) & yy=yy-imgSz(1)/2-dxya(1)
  WIDGET_CONTROL, lblCursorPos, SET_VALUE=STRING(xx, FORMAT='(i0)')+','+STRING(yy, FORMAT='(i0)')

  sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
  IF sel NE -1 THEN BEGIN
    IF nFrames EQ 0 THEN pix=structImgs.(sel).pix ELSE pix=structImgs.(0).pix
    WIDGET_CONTROL, lblCursorPosMM, SET_VALUE=STRING(xx*pix(0), FORMAT='(f0.1)')+','+STRING(yy*pix(1), FORMAT='(f0.1)')
  ENDIF

  IF (ev.release EQ 1 AND ev.type LE 1) OR mouseDown EQ 1 THEN BEGIN
    diffXY=[ev.X,ev.Y]-lastXY
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

    lastXY=[ev.X,ev.Y]
    IF ev.release EQ 1 THEN BEGIN
      lastXYreleased=lastXY
      lastXY=[-1,-1]
      mouseDown=0
    ENDIF
    redrawImg,0,0
  ENDIF

  IF ev.press EQ 1 AND ev.type LE 1 THEN BEGIN
    lastXY=[ev.X,ev.Y]
    mouseDown = 1
  ENDIF

  IF ev.type EQ 7 THEN BEGIN ; wheel events
    di=ev.clicks
    tags=TAG_NAMES(structImgs)
    IF tags(0) NE 'EMPTY' THEN BEGIN
      sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
      IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=structImgs.(0).nFrames
      IF sel-di LT nImg AND sel-di GE 0 THEN BEGIN
        WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel-di
        redrawImg,0,1 & updateInfo=1
      ENDIF
    ENDIF
  ENDIF

  IF (ev.key EQ 6 OR ev.key EQ 8 OR ev.key EQ 10) AND ev.release THEN BEGIN ; next image (arrow right or down or PageDown)
    sel=WIDGET_INFO(listFiles, /LIST_SELECT) & sel=sel(0)
    IF nFrames EQ 0 THEN nImg=N_TAGS(structImgs) ELSE nImg=structImgs.(0).nFrames
    IF sel LT nImg-1 THEN BEGIN
      WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel+1
      redrawImg,0,1 & updateInfo=1
      updateTable
      updatePlot, 0,0,0
    ENDIF
  ENDIF

  IF (ev.key EQ 5 OR ev.key EQ 7 OR ev.key EQ 9) AND ev.release THEN BEGIN ; prev image (arrow lft or up or PageUp)
    sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
    IF sel GT 0 THEN BEGIN
      WIDGET_CONTROL, listFiles, SET_LIST_SELECT=sel-1
      redrawImg,0,1 & updateInfo=1
      updateTable
      updatePlot, 0,0,0
    ENDIF
  ENDIF

ENDIF
;-------------------- WIDGET_TAB events-----------------------------------
;New analysis type selected
IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TAB') THEN BEGIN

  tags=TAG_NAMES(structImgs)
  IF tags(0) NE 'EMPTY' THEN loadedImg=1 ELSE loadedImg=0
  selTab=WIDGET_INFO(ev.ID, /TAB_CURRENT)

  CASE ev.ID OF
    wtabModes: BEGIN
      IF TOTAL(results) GT 0 THEN BEGIN

        IF N_ELEMENTS(switchMode) EQ 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Switch test-mode and loose current results?', /QUESTION)
          IF sv EQ 'Yes' THEN BEGIN
            modality=selTab
            clearRes
          ENDIF ELSE BEGIN
            switchMode='No'
            WIDGET_CONTROL, wtabModes, SET_TAB_CURRENT=modality
          ENDELSE
        ENDIF ELSE switchMode=!Null
      ENDIF ELSE modality=selTab
    END
    wtabAnalysisCT: BEGIN
      IF loadedImg THEN BEGIN
        IF results(selTab) EQ 1 THEN analyse=analyseStringsCT(selTab+1) ELSE analyse=analyseStringsCT(0)
      ENDIF
    END
    wtabAnalysisXray: BEGIN
      IF loadedImg THEN BEGIN
        IF results(selTab) EQ 1 THEN analyse=analyseStringsXray(selTab+1) ELSE analyse=analyseStringsXray(0)
      ENDIF
    END
    wtabAnalysisNM: BEGIN
      IF loadedImg THEN BEGIN
        IF results(selTab) EQ 1 THEN analyse=analyseStringsNM(selTab+1) ELSE analyse=analyseStringsNM(0)
      ENDIF
    END
    ELSE:
  ENDCASE

  IF ev.ID NE wtabResult THEN BEGIN
    updateInfo=1
    redrawImg,0,0
    updateTable
    updatePlot, 1,1,0
  ENDIF
ENDIF

;***************** Open files ***********************
IF N_ELEMENTS(adrFilesToOpen) GT 0 THEN BEGIN
  IF adrFilesToOpen(0) NE '' THEN BEGIN
    defPath=FILE_DIRNAME(adrFilesToOpen(0))
    tagNames=TAG_NAMES(structImgs)
    oldSel=WIDGET_INFO(listFiles, /LIST_SELECT)  & oldSel=oldSel(0)
    newSel=oldSel
    app=0 ; append
    IF tagNames(0) NE 'EMPTY' AND nFrames EQ 0 THEN BEGIN 
      box=[$
        '1, BASE,, /ROW', $
        '2, LABEL, Keep loaded files?', $
        '1, BASE,, /ROW', $
        '0, BUTTON, Append, QUIT, TAG=Append',$
        '2, BUTTON, Replace, QUIT, TAG=Replace']
      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Append or replace', XSIZE=200, YSIZE=100, FOCUSNO=1)

      IF res.Append THEN app=N_TAGS(structImgs) 
    ENDIF
    WIDGET_CONTROL, /HOURGLASS
    IF app EQ 0 THEN newSel=0

    nFiles=n_elements(adrFilesToOpen)
    counter=0
    FOR i=0, nFiles-1 DO BEGIN
      WIDGET_CONTROL, lblProgress, SET_VALUE='Loading file info: '+STRING(i*100./nFiles, FORMAT='(i0)')+' %'
      structCT=readImgInfo(adrFilesToOpen(i))
      include=1
      IF SIZE(structCT, /TNAME) NE 'INT' THEN BEGIN
        IF structCT.nFrames GT 1 THEN BEGIN
          IF nFiles GT 1 OR app NE 0 THEN BEGIN
            sv=DIALOG_MESSAGE('Multiframe only possible to open as single file. File ignored.',/ERROR)
            include=0
          ENDIF
        ENDIF

        IF include THEN BEGIN
          IF counter EQ 0 AND app EQ 0 THEN BEGIN
            structImgs=CREATE_STRUCT('S0',structCT)
          ENDIF ELSE structImgs=CREATE_STRUCT(structImgs,'S'+STRING(counter+app,FORMAT='(i0)'),structCT)

          counter=counter+1
        ENDIF
      ENDIF
    ENDFOR
    WIDGET_CONTROL, lblProgress, SET_VALUE=''

    tags=tag_names(structImgs)
    IF tags(0) NE 'EMPTY' THEN BEGIN

      IF TOTAL(results) GT 0 AND counter GT 0 THEN BEGIN
        IF app EQ 0 THEN BEGIN
          marked=-1
        ENDIF ELSE BEGIN
          IF marked(0) EQ -1 THEN BEGIN
            marked=INDGEN(app);there is results, but no file was marked and the new files is appended = mark the files already there
          ENDIF
        ENDELSE
      ENDIF ELSE marked=-1

      IF structImgs.(0).nFrames GT 1 THEN BEGIN
        fileList=getListFrames(structImgs.(0),marked)
        nFrames=structImgs.(0).nFrames
        WIDGET_CONTROL, lblProgress, SET_VALUE=''
        infoFile=FILE_INFO(structImgs.(0).filename)
        IF infoFile.size GT 10000000 THEN sv=DIALOG_MESSAGE('Warning: large file (>10MB). Consider storing the file locally first to ensure smooth workflow.')
      ENDIF ELSE BEGIN
        fileList=getListOpenFiles(structImgs,0,marked)
        nFrames=0
      ENDELSE

      WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=newSel
      WIDGET_CONTROL, listFiles, SCR_YSIZE=170
      IF nFrames NE 0 THEN activeImg=readImg(structImgs.(0).filename, 0) ELSE activeImg=readImg(structImgs.(app).filename)
      WIDGET_CONTROL, lblLoadedN, SET_VALUE=STRING(n_elements(fileList), FORMAT='(i0)')+' )'

      IF app EQ 0 THEN BEGIN
        wCenter=structImgs.(0).wCenter
        wWidth=structImgs.(0).wWidth
        IF wCenter NE -1 AND wWidth NE -1 THEN minmax=[wCenter-wWidth/2,wCenter+wWidth/2] ELSE minmax=[-200,200]
        WIDGET_CONTROL, txtMinWL, SET_VALUE=STRING(minmax(0),FORMAT='(i0)')
        WIDGET_CONTROL, txtMaxWL, SET_VALUE=STRING(minmax(1),FORMAT='(i0)')
      ENDIF

      IF TOTAL(results) GT 0 AND counter GT 0 AND app EQ 0 THEN clearRes

      WIDGET_CONTROL, drawLarge, SENSITIVE=1
      redrawImg,0,1
      updateInfo=1
      adrFilesToOpen=''
    ENDIF ELSE WIDGET_CONTROL, drawLarge, SENSITIVE=0

  ENDIF
ENDIF

;************** Move selected **********************
IF N_ELEMENTS(moveSelected) GT 0 THEN BEGIN
  IF moveSelected GE 0 THEN BEGIN
    IF nFrames EQ 0 THEN BEGIN
      sel=WIDGET_INFO(listFiles, /LIST_SELECT)
      newFirstSel=sel(0)
      last=N_ELEMENTS(sel)-1
      
      IF sel(0) NE -1 THEN BEGIN
        nImg=N_TAGS(structImgs)
        oldOrder=INDGEN(nImg)

        invSel=oldOrder
        invSel(sel)=-1
        invSel=invSel(SORT(invSel))
        invSel=invSel(UNIQ(invSel))
        IF invSel(0) EQ -1 THEN invSel=invSel[1: N_ELEMENTS(invSel)-1]
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
          ELSE:newOrder=oldOrder
        ENDCASE

        IF ARRAY_EQUAL(newOrder,oldOrder) EQ 0 THEN BEGIN

          proceed=1
          IF TOTAL(results) GT 0 THEN BEGIN
            sv=DIALOG_MESSAGE('Continue and clear results?', /QUESTION)
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
            fileList=getListOpenFiles(structImgs,0,marked)
            WIDGET_CONTROL, listFiles, YSIZE=n_elements(fileList), SET_VALUE=fileList, SET_LIST_SELECT=newFirstSel, SET_LIST_TOP=0
            WIDGET_CONTROL, listFiles, SCR_YSIZE=170
            clearRes
            updateInfo=1
          ENDIF
        ENDIF;newOrder for real
      ENDIF;selected exists
    ENDIF ELSE sv=DIALOG_MESSAGE('Not possible for multiframe images')
    moveSelected = -1
  ENDIF
ENDIF


; ************************************** update info on active image **************************************************************
IF N_ELEMENTS(updateInfo) GT 0 THEN BEGIN
  IF updateInfo THEN BEGIN
    sel=WIDGET_INFO(listFiles, /LIST_SELECT)  & sel=sel(0)
    IF sel NE -1 THEN BEGIN
      IF nFrames EQ 0 THEN tempStruct=structImgs.(sel) ELSE tempStruct=structImgs.(0)
      tempImg=activeImg
      imSz=SIZE(tempImg,/DIMENSIONS)
      tab=STRING(9B)

      CASE modality OF
        0: BEGIN
          infoString1=$
            ['StudyDate:'+tab+ tempStruct.StudyDate, $
            'Institution:'+tab+ tempStruct.Institution, $
            'ModelName:'+tab+ tempStruct.ModelName, $
            'PatientName:'+tab+ tempStruct.PatientName, $
            'PatientID:'+tab+ tab+tempStruct.PatientID, $
            'SeriesName:'+tab+ tempStruct.seriesName, $
            'ImageType:'+tab+ tempStruct.imageType, $
            'Filter:'+tab+tempStruct.filter, $
            'ExposureModType:'+tab+tempStruct.ExModType]

          infoString2=$
            ['SliceThick:'+tab+ (tempStruct.SliceThick NE -1 ? string(tempStruct.SliceThick, format='(f0.2)') : '-'), $
            'PixelSize:'+tab+tab+ (tempStruct.pix[0] NE -1 ? string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)')  : '-'), $
            'ImageSize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'),$
            'CollWidth:'+tab+(tempStruct.coll[0] NE -1 ? string(tempStruct.coll[0],format='(f0.2)') : '-')+' | '+(tempStruct.coll[1] NE -1 ? string(tempStruct.coll[1],format='(f0.2)') : '-'), $
            'kVp:'+tab+(tempStruct.kVp NE -1 ? STRING(tempStruct.kVp, format='(f0.2)') : '-'), $
            'mAs:'+tab+(tempStruct.mAs NE -1 ? STRING(tempStruct.mAs, format='(f0.2)') : '-'), $
            'ExpTime:'+tab+(tempStruct.time NE -1 ? STRING(tempStruct.time, format='(f0.2)') : '-'), $
            'Pitch:'+tab+(tempStruct.pitch NE -1 ? STRING(tempStruct.pitch, format='(f0.2)') : '-'), $
            'CTDIvol:'+tab+(tempStruct.CTDIvol(0) NE -1 ? (N_ELEMENTS(tempStruct.CTDIvol) EQ 1 ? STRING(tempStruct.CTDIvol, format='(f0.2)'): 'err') : '-')]
        END
        1: BEGIN
          infoString1=$
            ['StudyDate:'+tab+ tempStruct.StudyDate, $
            'Institution:'+tab+ tempStruct.Institution, $
            'ModelName:'+tab+ tempStruct.ModelName, $
            'PatientName:'+tab+ tempStruct.PatientName, $
            'PatientID:'+tab+ tab+tempStruct.PatientID, $
            'Modality:'+tab+tab+tempStruct.modality, $
            'Presentation type:'+tab+tempStruct.presType,$
            'SeriesNmb:'+tab+ string(tempStruct.seriesNmb, format='(i0)'), $
            'Acquisition time:'+tab+ (tempStruct.acqTime NE -1 ? string(tempStruct.acqTime, format='(i0)'): '-'), $
            'Protocol name:'+tab+ tempStruct.protocolName]

          infoString2=$
            ['Image no.:'+tab+STRING(tempStruct.imgNo, format='(i0)'),$
            'PixelSize:'+tab+tab+ string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)'), $
            'ImageSize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'),$
            'kVp:'+tab+(tempStruct.kVp NE -1 ? STRING(tempStruct.kVp, format='(f0.2)') : '-'), $
            'mA:'+tab+(tempStruct.mA NE -1 ? STRING(tempStruct.mA, format='(f0.2)') : '-'), $
            'mAs:'+tab+(tempStruct.mAs NE -1 ? STRING(tempStruct.mAs, format='(f0.2)') : '-'), $
            'ExpTime:'+tab+(tempStruct.time NE -1 ? STRING(tempStruct.time, format='(f0.2)') : '-'), $
            'EI:'+tab+tab+(tempStruct.EI NE -1 ? STRING(tempStruct.EI, format='(f0.2)') : '-'), $
            'Sensitivity:'+tab+(tempStruct.sensitivity NE -1 ? STRING(tempStruct.sensitivity, format='(f0.2)') : '-'), $
            'DAP:'+tab+(tempStruct.DAP NE -1 ? STRING(tempStruct.DAP, format='(f0.2)') : '-')]
        END
        2:BEGIN
          infoString1=$
            ['StudyDate:'+tab+ tempStruct.StudyDate, $
            'Institution:'+tab+ tempStruct.Institution, $
            'StationName:'+tab+ tempStruct.StationName, $
            'PatientName:'+tab+ tempStruct.PatientName, $
            'PatientID:'+tab+ tab+tempStruct.PatientID, $
            'Modality:'+tab+tab+tempStruct.modality, $
            'Acquisition time:'+tab+ (tempStruct.acqTime NE -1 ? string(tempStruct.acqTime, format='(i0)'): '-'), $
            'SeriesName:'+tab+ tempStruct.seriesName, $
            'Study Description:'+tab+ tempStruct.studyDescr]

          infoString2=$
            ['PixelSize:'+tab+tab+ string(tempStruct.pix[0],format='(f0.2)')+', '+string(tempStruct.pix[1],format='(f0.2)'), $
            'ImageSize:'+tab+string(imSz[0],format='(i0)')+', '+string(imSz[1],format='(i0)'), $
            'SliceThick:'+tab+ (tempStruct.SliceThick NE -1 ? string(tempStruct.SliceThick, format='(f0.2)') : '-'), $
            'Collimator:'+tab+tempStruct.collType,$
            'Radius det 1:'+tab+(tempStruct.radius1(0) NE -1 ? (N_ELEMENTS(tempStruct.radius1) GT 1 ? '['+string(min(tempStruct.radius1), format='(f0.1)')+'.. '+string(max(tempStruct.radius1), format='(f0.1)') + ']' : string(tempStruct.radius1(0), format='(f0.1)')) : '-'), $
            'Radius det 2:'+tab+(tempStruct.radius2(0) NE -1 ? (N_ELEMENTS(tempStruct.radius2) GT 1 ? '['+string(min(tempStruct.radius2), format='(f0.1)')+'.. '+string(max(tempStruct.radius2), format='(f0.1)') + ']' : string(tempStruct.radius2(0), format='(f0.1)')) : '-'), $
            'Energy Window:'+tab+tempStruct.EWindowName,$
            'Zoom factor:'+tab+tempStruct.zoomFactor, $
            'Filter:'+tab+tempStruct.filter]
        END
        ELSE:
      ENDCASE

      WIDGET_CONTROL, txtActive1, SET_VALUE=infoString1
      WIDGET_CONTROL, txtActive2, SET_VALUE=infoString2
      WIDGET_CONTROL, lblDir, SET_VALUE=tempStruct.filename

      IF TOTAL(results) GT 0 THEN BEGIN
        updateTable
        updatePlot, 0,0,0
      ENDIF
    ENDIF
    updateInfo=0
  ENDIF
ENDIF

;******************* Exit program ***********************
IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  WIDGET_CONTROL, ev.top, /DESTROY
ENDIF

end

;##################### about ImageQC ######################################

pro ImageQC_about, GROUP_LEADER = mainB

  about_box = WIDGET_BASE(TITLE='About ImageQC', /COLUMN, $
    XSIZE=350, YSIZE=200, XOFFSET=200, YOFFSET=200, GROUP_LEADER=mainB, /MODAL)

  infoe=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE=' ')
  info0=WIDGET_LABEL(about_box, /ALIGN_CENTER, VALUE='ImageQC v1.0', FONT="Arial*ITALIC*24")
  info1=WIDGET_LABEL(about_box, /ALIGN_CENTER, VALUE='Quality Control for medical imaging', FONT="Arial*ITALIC*16")
  info2=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='---------------------------------')
  info3=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='Implemented with IDL v 8.4+')
  info9=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='')
  info12=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='Ellen Wasb'+string(248B)+' 2016 (ellen@wasbo.net)')
  info13=WIDGET_LABEL(about_box, /ALIGN_CENTER,VALUE='Stavanger University Hospital, Norway')

  WIDGET_CONTROL, about_box, /REALIZE
  XMANAGER, 'ImageQC_about', about_box

end

