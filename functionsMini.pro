;ImageQC - quality control of medical images
;Copyright (C) 2017  Ellen Wasbo, Stavanger University Hospital, Norway
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

; ***************** editing arrays and structures (adding/deleting) *****************************

;remove ids from 1d-array; works no good with 1 element string.... not getting set to !null
function removeIDarr, arr, id
  newArr=arr
  IF id EQ 0 THEN BEGIN
    IF N_ELEMENTS(arr) GT 1 THEN newArr=arr[1:N_ELEMENTS(arr)-1] ELSE newArr=!null
  ENDIF
  IF id EQ N_ELEMENTS(arr)-1 THEN newArr=arr[0:N_ELEMENTS(arr)-2]
  IF id GT 0 AND id LT N_ELEMENTS(arr)-1 THEN newArr=[arr[0:id-1],arr[id+1:N_ELEMENTS(arr)-1]]
  return, newArr
end

;remove ids from structure of structures
function removeIDstructstruct, struct, ids
  structNew=CREATE_STRUCT('EMPTY',0)
  counter=0
  ntags=N_TAGS(struct)
  tagname=TAG_NAMES(struct)
  FOR i=0, ntags-1 DO BEGIN
    inSel=WHERE(ids EQ i)
    IF inSel(0) EQ -1 THEN BEGIN
      stillEmpty=WHERE(TAG_NAMES(structNew) EQ 'EMPTY')
      IF stillEmpty(0) EQ -1 THEN structNew=CREATE_STRUCT(structNew,tagname(i),struct.(i)) ELSE structNew=CREATE_STRUCT(tagname(i),struct.(i))
    ENDIF
  ENDFOR
  return, structNew
end

;reorder ids in structure of structures
function reorderStructStruct, struct, newOrder
  ntags=N_TAGS(struct)
  tagname=TAG_NAMES(struct)
  structNew=CREATE_STRUCT(tagname(newOrder(0)),struct.(newOrder(0)))
  FOR i=1, ntags-1 DO structNew=CREATE_STRUCT(structNew,tagname(newOrder(i)),struct.(newOrder(i)))
  return, structNew
end

;replace numbered structure in structure of structures
;numb = id to replace
function replaceStructStruct, fullStruct, newSubStruct, numb, NEW_TAG_NAME=new_tag_name
  structNew=CREATE_STRUCT('EMPTY',0)
  counter=0
  ntags=N_TAGS(fullStruct)
  tagname=TAG_NAMES(fullStruct)
  FOR i=0, ntags-1 DO BEGIN
    stillEmpty=WHERE(TAG_NAMES(structNew) EQ 'EMPTY')
    IF i NE numb THEN BEGIN
      IF stillEmpty(0) EQ -1 THEN structNew=CREATE_STRUCT(structNew,tagname(counter),fullStruct.(i)) ELSE structNew=CREATE_STRUCT(tagname(0),fullStruct.(i))
      counter=counter+1
    ENDIF ELSE BEGIN
      IF N_ELEMENTS(new_tag_name) GT 0 THEN tname=new_tag_name ELSE tname=tagname(counter)
      IF stillEmpty(0) EQ -1 THEN structNew=CREATE_STRUCT(structNew,tname,newSubStruct) ELSE structNew=CREATE_STRUCT(tname,newSubStruct)
      counter=counter+1
    ENDELSE
  ENDFOR
  return, structNew
end

;rename all tags of a structure
;assume N_TAGS structIn equals number of newnames
function renameTagsStruct, structIn, new_tag_names
  structNew=!Null
  counter=0
  ntags=N_TAGS(structIn)
  FOR i=0, ntags-1 DO structNew=CREATE_STRUCT(structNew,new_tag_names(i),structIn.(i))
  return, structNew
end

function updateMaterialHeaders, currTableHeaders, newMaterialHeaders
  newTableHeaders=currTableHeaders
  modNames=TAG_NAMES(currTableHeaders)
  idCT=WHERE(modNames EQ 'CT')
  IF idCT(0) NE -1 THEN BEGIN
    IF N_ELEMENTS(newMaterialHeaders) GT 1 THEN BEGIN
      szHeaders=SIZE(newMaterialHeaders, /DIMENSIONS)
      IF szHeaders(0) EQ 1 THEN newMaterialHeaders=TRANSPOSE(newMaterialHeaders)
    ENDIF
    testNames=TAG_NAMES(currTableHeaders.CT)
    idTest=WHERE(testNames EQ 'CTLIN')
    IF idTest(0) NE -1 THEN BEGIN
      newCTLINStruct=CREATE_STRUCT('Alt1',newMaterialHeaders)
      newCTstruct=replaceStructStruct(currTableHeaders.CT, newCTLINStruct, idTest)
      newTableHeaders=replaceStructStruct(currTableHeaders, newCTstruct, idCT)
    ENDIF
  ENDIF

  return, newTableHeaders
end

;change structure tags from array to one-element-values. Values are set to the numbered value
function structArr2elem, struct, taglist2scalar, valueNmb
  ntags=N_TAGS(struct)
  tagname=TAG_NAMES(struct)
  IF taglist2scalar.HasValue(tagname(0)) THEN val=struct.(0)[valueNmb] ELSE val=struct.(0)
  structNew=CREATE_STRUCT(tagname(0),val)
  FOR i=1, ntags-1 DO BEGIN
    IF taglist2scalar.HasValue(tagname(i)) THEN BEGIN
      IF N_ELEMENTS(struct.(i)) GT valueNmb THEN val=struct.(i)[valueNmb] ELSE val=struct.(i)
    ENDIF ELSE val=struct.(i)
    structNew=CREATE_STRUCT(structNew,tagname(i),val)
  ENDFOR
  return, structNew
end

;************* config updates *****************************

function updateConfigS, file

  ;default values if missing:
  materials=['Teflon','Delrin','Acrylic','Water','Polystyrene','LDPE','PMP','Air']
  relMassD=[2.16,1.41,1.18,1.,1.05,0.92,0.83,0.]
  posX=[-28.,-58.,-28.,0.,28.,58.,28.,0.]
  posY=[-50.,0.,50.,58.,50.,0.,-50.,-58.]
  lintab=CREATE_STRUCT('materials', materials, 'relMassD', relMassD, 'posX', posX, 'posY', posY)
  configDefault=CREATE_STRUCT($

    'copyHeader', 0, $
    'transposeTable', 0, $
    'deciMark',',', $
    'includeFilename', 0, $
    'append',0,$
    'autoImportPath','',$
    'qtOutTemps', ['DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT'], $
    'MTFtype',2,'MTFtypeX',1,'MTFtypeNM',1,'MTFtypeSPECT',1, $
    'plotMTF',3,'plotMTFX', 3, 'plotMTFNM',4,'plotMTFSPECT',4, 'tableMTF',0,'cyclMTF',0,'tableMTFX', 0, $
    'MTFroiSz',11.0,'MTFroiSzX',[20.,50.],'MTFroiSzNM',[20.,20.],'MTFroiSzSPECT',30.,'MTF3dSPECT',1, $
    'cutLSF',1,'cutLSF1',3,'cutLSF2',1, 'cutLSFX', 1, 'cutLSFX1', 3, 'offxy', [0,0], $
    'searchMaxMTF_ROI',0,$
    'LinROIrad',3.,'LinROIradS',11., 'LinTab',lintab, $
    'RampDist',38.,'RampLen',60.,'RampBackG',5.,'RampSearch',5,'RampAvg',1,'RampType',0,'RampDens',0,$
    'HomogROIsz',10., 'HomogROIszX',10., 'HomogROIszPET', 10.,'HomogROIdist',55.,'HomogROIdistPET',55.,$
    'NoiseROIsz',55., 'HUwaterROIsz', 55.,$
    'NPSroiSz', 50, 'NPSroiDist', 50., 'NPSsubNN', 20, 'NPSroiSzX', 256, 'NPSsubSzX', 5, 'NPSavg', 1, $
    'STProiSz', 11.3, $
    'unifAreaRatio', 0.95,'SNIAreaRatio', 0.9,'unifCorr',0,'SNIcorr',0,'distCorr',385.0,'attCoeff',2.2,'detThick',9.5,$
    'barROIsz',50.0,'barWidths',[6.4,4.8,4.0,3.2],$
    'ScanSpeedAvg', 25, 'ScanSpeedHeight', 100., 'ScanSpeedFiltW', 15, $
    'ContrastRad1', 20., 'ContrastRad2', 58.,$
    'CrossROIsz', 60., 'CrossVol', 0.0)
  configSdefault=CREATE_STRUCT('defConfigNo',1,'configDefault',configDefault)

  newConfigS=-1

  IF file EQ '' THEN newConfigS=configSdefault ELSE BEGIN
    ;find existing values and paste into new configS structure
    RESTORE, file
    errCounter=0
    IF N_ELEMENTS(config) NE 0 THEN oldConfigS=CREATE_STRUCT('defConfigNo',1,'configDefault',config) ELSE errCounter=1
    IF N_ELEMENTS(configS) NE 0 THEN oldConfigS=configS ELSE errCounter=errCounter+1
    IF errCounter NE 2 THEN BEGIN
      ;copy values into newest version config structure
      defaultTags=TAG_NAMES(configDefault)
      restoreTagsS=TAG_NAMES(oldConfigS)
      newConfigS=CREATE_STRUCT('defConfigNo',oldConfigS.(0))
      FOR i=1, N_ELEMENTS(restoreTagsS)-1 DO BEGIN;for each parameterset
        oldTags=TAG_NAMES(oldConfigS.(i))
        configTemp=!Null;CREATE_STRUCT('DECIMARK',oldConfigS.(i).DECIMARK)
        FOR j=0, N_ELEMENTS(defaultTags)-1 DO BEGIN;for each parameter in parameterset
          IF oldTags.HasValue(defaultTags(j)) THEN BEGIN
            ;copy tag content
            ff=WHERE(oldTags EQ defaultTags(j))
            IF defaultTags(j) EQ 'QTOUTTEMPS' THEN BEGIN
              nModOld=N_ELEMENTS(oldConfigS.(i).(ff))
              nModNew=N_ELEMENTS(configDefault.(j))
              IF nModOld LT nModNew THEN BEGIN  
                modQtOutTemps=configDefault.(j)
                modQtOutTemps[0:nModOld-1]=oldConfigS.(i).(ff)
                configTemp=CREATE_STRUCT(configTemp, defaultTags(j), modQtOutTemps)
              ENDIF ELSE configTemp=CREATE_STRUCT(configTemp, defaultTags(j), oldConfigS.(i).(ff))
            ENDIF ELSE configTemp=CREATE_STRUCT(configTemp, defaultTags(j), oldConfigS.(i).(ff))
          ENDIF ELSE BEGIN
            ;paste default content
            configTemp=CREATE_STRUCT(configTemp, defaultTags(j),configDefault.(j))
          ENDELSE
        ENDFOR
        newConfigS=CREATE_STRUCT(newConfigS,restoreTagsS(i),configTemp)
      ENDFOR

    ENDIF ELSE sv=DIALOG_MESSAGE('Found no valid config structure.', DIALOG_PARENT=0)

  ENDELSE;file ''

  return, newConfigS
end

function updateQuickT, file, mOpt
  IF file EQ '' THEN quickT=!Null ELSE BEGIN
    ;find existing values and paste into new quicktemp structure
    RESTORE, file
    ;securing older versions
    IF N_ELEMENTS(config) NE 0 THEN BEGIN
      oldTags=TAG_NAMES(config)
      If oldTags.HasValue('QUICKTEMP') THEN quickT=config.QUICKTEMP
    ENDIF
    IF N_ELEMENTS(configS) NE 0 THEN BEGIN
      oldTags=TAG_NAMES(configS.(1))
      If oldTags.HasValue('QUICKTEMP') THEN quickT=configS.(1).QUICKTEMP
    ENDIF
    IF N_ELEMENTS(quickTemp) NE 0 THEN quickT=quickTemp ELSE quickT=!Null
    IF SIZE(quickT, /TNAME) EQ 'STRUCT' THEN BEGIN
      ;mOpt= Struct 'CT',[1,2,3,4,5,6,7,0,0,0],'Xray',[1,2,3,4,5,0,0],'NM',[1,1,1,1,0,0,0],'SPECT', INTARR(3),'PET',INTARR(3),'MR',INTARR(1))
      ms=TAG_NAMES(mOpt)
      tnamQT=TAG_NAMES(quickT)
      IF ~ARRAY_EQUAL(ms, tnamQT) THEN BEGIN
        quickTm=quickT
        quickT=!Null
        FOR m=0, N_TAGS(mOpt)-1 DO BEGIN
          quickTthisMod=!Null
          IF TOTAL(mOpt.(m)) NE 0 THEN BEGIN; QuickTest option exist for this modality
            names=TAG_NAMES(quickTm)
            box=[$
              '1, BASE,, /COLUMN', $
              '0, LABEL, QuickTest templates now sorted into modality types', $
              '0, LABEL, Select templates for '+ms(m), $
              '0, LABEL, Use Ctrl or Shift to select multiple', $
              '0, LABEL, ',$
              '2, LIST, ' + STRJOIN(names,'|') + ', TAG=templates', $
              '1, BASE,, /ROW', $
              '2, BUTTON, OK, QUIT, TAG=OK']
            res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select QuickTest templates assosiated with '+ms(m), XSIZE=300, YSIZE=300, FOCUSNO=3, XOFFSET=200, YOFFSET=200)

            IF N_ELEMENTS(res.templates) NE 0 THEN BEGIN
              FOR i=0, N_ELEMENTS(res.templates)-1 DO quickTthisMod=CREATE_STRUCT(quickTthisMod, names(res.templates(i)),quickTm.(res.templates(i)))
              quickTm=removeIDstructstruct(quickTm, res.templates)
            ENDIF
          ENDIF
          IF N_ELEMENTS(quickTthisMod) EQ 0 THEN quickTthisMod=-1
          quickT=CREATE_STRUCT(quickT, ms(m),quickTthisMod)
        ENDFOR
        IF SIZE(quickTm,/TNAME) EQ 'STRUCT' THEN BEGIN
          t=TAG_NAMES(quickTm)
          IF t(0) NE 'EMPTY' THEN BEGIN
            sv=DIALOG_MESSAGE('Not all QuickTest templates were selected in this process. Those templates will be lost. Create a backup file to perform this process once again?', /QUESTION)
            IF sv EQ 'Yes' THEN BEGIN
              adr=DIALOG_PICKFILE(TITLE='Backup old config file', /WRITE, FILTER='*.dat', /FIX_FILTER, /OVERWRITE_PROMPT, DEFAULT_EXTENSION='.dat', PATH='C:\')
              IF adr(0) NE '' THEN FILE_COPY, file, adr
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      ;ensure correct size of arrays (columns equal number of tests available
      ce=0
      FOR m=0, N_TAGS(mOpt)-1 DO BEGIN
        IF SIZE(quickT.(m),/TNAME) EQ 'STRUCT' THEN BEGIN
          idTest=WHERE(mOpt.(m) NE 0)
          names=TAG_NAMES(quickT.(m))
          IF idTest(0) NE -1 THEN BEGIN
            nTests=N_ELEMENTS(idTest)
            newQuickTmod=!Null
            FOR i=0, N_TAGS(quickT.(m))-1 DO BEGIN
              sz=SIZE(quickT.(m).(i), /DIMENSIONS)
              IF sz(0) NE nTests THEN BEGIN
                newArr=INTARR(nTests,sz(1))
                IF sz(0) LT nTests THEN newArr[0:sz(0)-1,*]=quickT.(m).(i) ELSE BEGIN
                  oldArr=quickT.(m).(i)
                  newArr=oldArr[0:nTests-1,*]
                ENDELSE
                ce=ce+1
              ENDIF ELSE newArr=quickT.(m).(i)
              newQuickTmod=CREATE_STRUCT(newQuickTmod,names(i),newArr)
            ENDFOR
            quickT=replaceStructStruct(quickT, newQuickTmod, m)
          ENDIF
        ENDIF
      ENDFOR
      IF ce GT 0 THEN BEGIN
        sv=DIALOG_MESSAGE('Number of tests saved in QuickTest templates do not correspond to the available tests of this version. Control QuickTest template to verify consistency.', /INFORMATION)
      ENDIF
    ENDIF
  ENDELSE
  
  return, quickT
end

function updateQuickTout, file, analyseStrAll

  ;default values if missing:
  defCT=CREATE_STRUCT($
    'HOMOG',CREATE_STRUCT($
    'Center_HU',CREATE_STRUCT('ALT',0,'COLUMNS',4,'CALC',0,'PER_SERIES',0),$
    'Min_diff_from_Center_HU',CREATE_STRUCT('ALT',0,'COLUMNS',[5,6,7,8],'CALC',1,'PER_SERIES',1),$
    'Max_diff_from_Center_HU',CREATE_STRUCT('ALT',0,'COLUMNS',[5,6,7,8],'CALC',2,'PER_SERIES',1)),$
    'NOISE',CREATE_STRUCT('Max_noise',CREATE_STRUCT('ALT',0,'COLUMNS',1,'CALC',2,'PER_SERIES',1)),$
    'SLICETHICK',CREATE_STRUCT($
    'Avg_Slicethick',CREATE_STRUCT('ALT',0,'COLUMNS',5,'CALC',0,'PER_SERIES',0),$
    'Avg_Slicethick_inner',CREATE_STRUCT('ALT',1,'COLUMNS',[1,2,3,4],'CALC',3,'PER_SERIES',0),$
    'Avg_Slicethick_outer',CREATE_STRUCT('ALT',1,'COLUMNS',[5,6],'CALC',3,'PER_SERIES',0),$
    'Avg_SlicethickGE',CREATE_STRUCT('ALT',2,'COLUMNS',[1,2],'CALC',3,'PER_SERIES',0)),$
    'MTF',CREATE_STRUCT($
    'Avg_xy_MTF50',CREATE_STRUCT('ALT',0,'COLUMNS',[0,3],'CALC',3,'PER_SERIES',0),$
    'Avg_xy_MTF10',CREATE_STRUCT('ALT',0,'COLUMNS',[1,4],'CALC',3,'PER_SERIES',0),$
    'MTF50',CREATE_STRUCT('ALT',1,'COLUMNS',[0],'CALC',0,'PER_SERIES',1),$
    'MTF10',CREATE_STRUCT('ALT',1,'COLUMNS',[1],'CALC',0,'PER_SERIES',1)),$
    'CTLIN',-1,'HUWATER',-1,'EXP',-1)
  defXray=CREATE_STRUCT($
    'STP', CREATE_STRUCT('Pixel_mean',CREATE_STRUCT('ALT',0,'COLUMNS',2,'CALC',0,'PER_SERIES',0)),$
    'HOMOG',-1,'NOISE',-1,'EXP',-1,'MTF',-1)
  defNM=CREATE_STRUCT('UNIF', -1,'SNI',-1,'ACQ',-1,'BAR',-1)
  defMR=CREATE_STRUCT('DCM', -1)
  quickToutDefault=CREATE_STRUCT('CT',CREATE_STRUCT('DEFAULT',defCT),'Xray',CREATE_STRUCT('DEFAULT',defXray),'NM',CREATE_STRUCT('DEFAULT',defNM),'MR',CREATE_STRUCT('DEFAULT',defMR))

  IF file EQ '' THEN quickTout=quickToutDefault ELSE BEGIN
    ;find existing values and paste into new configS structure
    RESTORE, file
    errCounter=0
    IF N_ELEMENTS(quickTout) NE 0 THEN BEGIN
      oldQuickTout=quickTout
      quickTout=CREATE_STRUCT('popit',0)

      ;copy values into newest versions structure
      quickToutThisVersion=CREATE_STRUCT('ALT',0,'COLUMNS',0,'CALC',0,'PER_SERIES',0)
      tagNewest=TAG_NAMES(quickToutThisVersion)

      modesCurr=TAG_NAMES(quickToutDefault)
      modesOld=TAG_NAMES(oldQuickTout)
      FOR i=0, N_ELEMENTS(modesCurr)-1 DO BEGIN;for each modality in current version

        IF modesOld.HasValue(modesCurr(i)) THEN BEGIN ;modality defined in old version
          ;update tags
          ;add new tests to template with default values

          ii=WHERE(modesOld EQ modesCurr(i))
          oldTempsThisMode=TAG_NAMES(oldQuickTout.(ii))
          IF oldTempsThisMode(0) NE 'EMPTY' THEN BEGIN

            allTestsCurr=TAG_NAMES(quickToutDefault.(i).DEFAULT)
            nTestsCurr=N_ELEMENTS(allTestsCurr)
            allTestsOld=TAG_NAMES(oldQuickTout.(ii).DEFAULT)

            missingTestsInOld=INTARR(nTestsCurr)
            FOR a=0, nTestsCurr-1 DO IF ~allTestsOld.HasValue(allTestsCurr(a)) THEN missingTestsInOld(a)=1

            tempNames=TAG_NAMES(oldQuickTout.(ii))
            tempsThisMode=CREATE_STRUCT('popit',0)
            FOR j=0, N_ELEMENTS(tempNames)-1 DO BEGIN; for each template

              thisTemp=CREATE_STRUCT('popit',0)

              nTests=N_TAGS(oldQuickTout.(ii).(j))
              FOR k=0, N_ELEMENTS(allTestsOld)-1 DO BEGIN; for each defined test

                IF SIZE(oldQuickTout.(ii).(j).(k), /TNAME) EQ 'STRUCT' THEN BEGIN
                  outputsThisTest=CREATE_STRUCT('popit',0)
                  outPutNames=TAG_NAMES(oldQuickTout.(ii).(j).(k))
                  FOR l=0, N_ELEMENTS(outPutNames)-1 DO BEGIN; for each defined output
                    ;IF SIZE(oldQuickTout.(ii).(j).(k).(l), /TNAME) EQ 'STRUCT' THEN BEGIN
                    oldTags=TAG_NAMES(oldQuickTout.(ii).(j).(k).(l))

                    paramsThis=CREATE_STRUCT('popit',0)

                    IF ~ARRAY_EQUAL(oldTags, tagNewest) THEN BEGIN
                      FOR m=0, N_ELEMENTS(tagNewest)-1 DO BEGIN
                        IF oldTags.HasValue(tagNewest(m)) THEN BEGIN
                          ;copy tag content
                          ff=WHERE(oldTags EQ tagNewest(m))
                          paramsThis=CREATE_STRUCT(paramsThis, tagNewest(m), oldQuickTout.(ii).(j).(k).(l).(ff))
                        ENDIF ELSE paramsThis=CREATE_STRUCT(paramsThis, tagNewest(j),quickToutThisVersion.(m))
                      ENDFOR

                      paramsThis=removeIDstructstruct(paramsThis, 0);pop off first dummy element

                    ENDIF ELSE paramsThis=oldQuickTout.(ii).(j).(k).(l)
                    outputsThisTest=CREATE_STRUCT(outputsThisTest,outPutNames(l), paramsThis)

                  ENDFOR
                  outputsThisTest=removeIDstructstruct(outputsThisTest, 0);pop off first dummy element
                  thisTemp=CREATE_STRUCT(thisTemp,allTestsOld(k), outputsThisTest)
                ENDIF ELSE thisTemp=CREATE_STRUCT(thisTemp,allTestsOld(k),-1)
              ENDFOR
              IF TOTAL(missingTestsInOld) GT 0 THEN BEGIN
                testNo=WHERE(missingTestsInOld EQ 1)
                FOR aa=0, TOTAL(missingTestsInOld)-1 DO thisTemp=CREATE_STRUCT(thisTemp, allTestsCurr(testNo(aa)), quickToutDefault.(i).DEFAULT.(testno(aa)))
              ENDIF

              thisTemp=removeIDstructstruct(thisTemp, 0);pop off first dummy element
              ;reorder tests according to current order of QuickTests
              tagsThis=TAG_NAMES(thisTemp)
              IF ~ARRAY_EQUAL(allTestsCurr, tagsThis) THEN BEGIN
                nnn=N_ELEMENTS(allTestsCurr)
                newOrdTests=INTARR(nnn)
                FOR aaa=0, nnn-1 DO BEGIN
                  samme=WHERE(tagsThis EQ allTestsCurr(aaa))
                  newOrdTests(aaa)=samme(0)
                ENDFOR
                thisTemp=reorderStructStruct(thisTemp, newOrdTests)
              ENDIF

              tempsThisMode=CREATE_STRUCT(tempsThisMode, tempNames(j), thisTemp)
            ENDFOR

            tempsThisMode=removeIDstructstruct(tempsThisMode, 0);pop off first dummy element
            quickTout=CREATE_STRUCT(quickTout, modesCurr(i), tempsThisMode)

          ENDIF ELSE quickTout=CREATE_STRUCT(quickTout, modesCurr(i), quickToutDefault.(i))
        ENDIF ELSE quickTout=CREATE_STRUCT(quickTout, modesCurr(i), quickToutDefault.(i))
      ENDFOR

      quickTout=removeIDstructstruct(quickTout, 0);pop off first dummy element
    ENDIF ELSE quickTout=quickToutDefault

  ENDELSE
  return, quickTout
end

function updateLoadT, file, mOpt
  IF file EQ '' THEN loadT=!Null ELSE BEGIN
    ;find existing values and paste into new loadTemp structure (no doing anything yet as this is first version with this)
    RESTORE, file
    IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
      ;update with modalities from older versions
      IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
        tempsExist=TAG_NAMES(loadTemp)
        loadT=loadTemp
        IF ~ARRAY_EQUAL(tempsExist[0:2],['CT','XRAY','NM']) THEN BEGIN
          ;mOpt= Struct 'CT',[1,2,3,4,5,6,7,0,0,0],'Xray',[1,2,3,4,5,0,0],'NM',[1,1,1,1,0,0,0],'SPECT', INTARR(3),'PET',INTARR(3),'MR',INTARR(1))
          ms=TAG_NAMES(mOpt)
          loadTm=loadTemp
          loadT=!Null
          FOR m=0, N_TAGS(mOpt)-1 DO BEGIN
            loadTthisMod=!Null
            IF TOTAL(mOpt.(m)) NE 0 THEN BEGIN; QuickTest option exist for this modality
              names=TAG_NAMES(loadTm)
              box=[$
                '1, BASE,, /COLUMN', $
                '0, LABEL, Automation templates now sorted into modality types', $
                '0, LABEL, Select templates for '+ms(m), $
                '0, LABEL, Use Ctrl or Shift to select multiple', $
                '0, LABEL, ',$
                '2, LIST, ' + STRJOIN(names,'|') + ', TAG=templates', $
                '1, BASE,, /ROW', $
                '2, BUTTON, OK, QUIT, TAG=OK']
              res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select Automation templates assosiated with '+ms(m), XSIZE=300, YSIZE=300, FOCUSNO=3, XOFFSET=200, YOFFSET=200)

              IF N_ELEMENTS(res.templates) NE 0 THEN BEGIN
                FOR i=0, N_ELEMENTS(res.templates)-1 DO loadTthisMod=CREATE_STRUCT(loadTthisMod, names(res.templates(i)),loadTm.(res.templates(i)))
                loadTm=removeIDstructstruct(loadTm, res.templates)
              ENDIF
            ENDIF
            IF N_ELEMENTS(loadTthisMod) EQ 0 THEN loadTthisMod=-1
            loadT=CREATE_STRUCT(loadT, ms(m),loadTthisMod)
          ENDFOR
          IF SIZE(loadTm,/TNAME) EQ 'STRUCT' THEN BEGIN
            t=TAG_NAMES(loadTm)
            IF t(0) NE 'EMPTY' THEN BEGIN
              sv=DIALOG_MESSAGE('Not all automation templates were selected in this process. Those templates will be lost. Create a backup file to perform this process once again?', /QUESTION)
              IF sv EQ 'Yes' THEN BEGIN
                adr=DIALOG_PICKFILE(TITLE='Backup old config file', /WRITE, FILTER='*.dat', /FIX_FILTER, /OVERWRITE_PROMPT, DEFAULT_EXTENSION='.dat', PATH='C:\')
                IF adr(0) NE '' THEN FILE_COPY, file, adr
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      ;securing older versions tags
      IF N_ELEMENTS(loadT) GT 0 THEN BEGIN
        ;path - folder close to where the images should be found
        ;loadBy - choise - 0 = load all images in specified path
        ;sortBy - STRARR with structure tags in image structure to sort images by
        ;sortAsc - 0/1 ARR where sort order defined 0=ascending, 1=descending , one element=0 (from old template versions) means all ascending
        ;paramSet - name of paramSet to link to or '' if default
        ;quickTemp - name of quickTemp to link to or '' to default (all selected)
        ;pathApp- path to append results if successfully calculated
        loadTthisVersion=CREATE_STRUCT($
          'path','',$
          'statName','',$
          'loadBy',0,$
          'includeSub',0,$
          'sortBy', '', $
          'sortAsc',0, $
          'paramSet','', $
          'quickTemp','',$
          'pathApp','',$
          'archive',0,$
          'deleteFiles',0,$
          'deleteFilesEnd',0)
        loadTsetDef=CREATE_STRUCT('loadTempDefault',loadTthisVersion)
        tagNewest=TAG_NAMES(loadTthisVersion)

        ;copy values into newest version structure
        FOR m=0, N_TAGS(mOpt)-1 DO BEGIN
          IF SIZE(loadT.(m), /TNAME) EQ 'STRUCT' THEN BEGIN
            tempsExist=TAG_NAMES(loadT.(m))
            FOR i=0, N_ELEMENTS(tempsExist)-1 DO BEGIN;for each set
              oldTags=TAG_NAMES(loadT.(m).(i))
              loadNew=CREATE_STRUCT('PATH',loadT.(m).(i).PATH)
              FOR j=1, N_ELEMENTS(tagNewest)-1 DO BEGIN;for each parameter in set
                IF oldTags.HasValue(tagNewest(j)) THEN BEGIN
                  ;copy tag content
                  ff=WHERE(oldTags EQ tagNewest(j))
                  loadNew=CREATE_STRUCT(loadNew, tagNewest(j), loadT.(m).(i).(ff))
                ENDIF ELSE BEGIN
                  ;paste default content
                  loadNew=CREATE_STRUCT(loadNew, tagNewest(j),loadTsetDef.(0).(j))
                ENDELSE
              ENDFOR
              IF i EQ 0 THEN loadTm=CREATE_STRUCT(tempsExist(i),loadNew) ELSE loadTm=CREATE_STRUCT(loadTm, tempsExist(i),loadNew)
            ENDFOR
            loadT=replaceStructStruct(loadT, loadTm, m)
           ENDIF
          ENDFOR
        ENDIF ELSE loadT=!Null

      ENDIF ELSE loadT=!Null
    ENDELSE
    return, loadT
  end

  ;previously saved .dat-file might miss some newly introduced parameters. Update to avoid crashes and to update current Path (.filename) if replaced since created
  ;called by struc='',pathNow ='' gives default, empty structure to be able to find list of available tags
function imgStructUpdate, struc, pathNow
  updatedStruct=-1
  szStru=SIZE(struc, /TNAME)
  IF szStru EQ 'STRUCT' THEN tnLoaded=TAG_NAMES(struc) ELSE tnLoaded=''
  IF tnLoaded.HasValue('FILENAME') OR tnLoaded(0) EQ '' THEN BEGIN
    currStruct=CREATE_STRUCT('filename',pathNow,'studydatetime','','acqDate', '', 'imgDate', '', 'institution','','modality', '', 'modelName','','stationName','','SWversion','','detectorID','',$
      'patientName','', 'patientID', '', 'patientWeight', '-', 'imageType','','presType','','studyDescr','','seriesName','', 'protocolname', '',$
      'seriesNmb',-1,'seriesTime','','seriesUID','','acqNmb',-1, 'acqtime','','sliceThick',-1., 'pix', [-1.,-1.],'imageSize',[-1,-1],'kVp',-1.,'FOV',-1.,'rekonFOV',-1.,'mA',-1.,'mAs',-1.,'ExpTime',-1.,'coll',[-1.,-1.],'pitch',-1.,$
      'ExModType','','CTDIvol',-1.,'DAP',-1.,'EI',-1.,'sensitivity',-1.,'sdd',-1.,'filterAddOn','-','kernel','-',$
      'zpos', -999., 'imgNo',-1,'nFrames',0,'wCenter',-1,'wWidth',-1,$
      'collType','-','nEWindows',-1,'EWindowName','-','zoomFactor','-','radius1',-1.,'radius2',-1.,'angle',-999.,'acqFrameDuration',-1.,'acqTerminationCond','-',$
      'units','-','radiopharmaca','-','admDose','-','admDoseTime','-','reconMethod','-','attCorrMethod','-','scaCorrMethod','-', 'scatterFrac','-',$
      'imgFreq',-1.,'MRacqType','-','MRscanSeq','-','MRseqVariant','-','TR',-1.,'TE',-1.,'NSA',-1.,'flipAng',-1.,'spaceSlice',-1.,$
      'frameNo', -1)

    tnCurr=TAG_NAMES(currStruct)
    updatedStruct=CREATE_STRUCT('filename',pathNow)

    FOR i=1, N_ELEMENTS(tnCurr)-1 DO BEGIN
      IF tnLoaded.HasValue(tnCurr(i)) THEN BEGIN
        pos=WHERE(tnLoaded EQ tnCurr(i))
        updatedStruct=CREATE_STRUCT(updatedStruct, tnCurr(i), struc.(pos(0)))
      ENDIF ELSE updatedStruct=CREATE_STRUCT(updatedStruct, tnCurr(i), currStruct.(i))
    ENDFOR

  ENDIF

  return, updatedStruct
end

function imgStructDescTags
  ;keep same order as imgStructUpdate! (until code for this is at place)
  imgStructDesc=CREATE_STRUCT('filename','File Name','studydatetime','Study DateTime','acqDate', 'Acquisition Date', 'imgDate', 'Image Creation Date', $
    'institution','Institution','modality', 'Modality', 'modelName','Equipment Model Name','stationName','Station Name','SWversion','Software version',$
    'detectorID','Detector ID',$
    'patientName','Patient Name', 'patientID', 'Patient ID', 'patientWeight', 'Patient Weight', 'imageType','Image Type','presType','Presentation type',$
    'studyDescr','Study Description','seriesName','Series Name', 'protocolname', 'Protocol Name',$
    'seriesNmb','Series Number','seriesTime','Series Time','seriesUID','Series UID','acqNmb','Acquisition Number', 'acqtime','Acquisition Time',$
    'sliceThick','Slice Thickness', 'pix', 'Pixel size','imageSize','Image Size','kVp','kVp','FOV','FOV','rekonFOV','Reconstruction FOV','mA','mA','mAs','mAs',$
    'ExpTime','Exposure Time','coll','Collimation','pitch','Pitch',$
    'ExModType','Exposure Modulation Type','CTDIvol','CTDI vol','DAP','DAP','EI','Exposure Index','sensitivity','Sensitivity','sdd','Source Detector Distance',$
    'filterAddOn','Filter AddOn','kernel','Reconstruction kernel',$
    'zpos', 'Z position', 'imgNo','Image Number','nFrames','Number Of Frames','wCenter','Window Center','wWidth','Window Width',$
    'collType','Collimator Type','nEWindows','Number of Energy Windows','EWindowName','Energy Window Name','zoomFactor','Zoom Factor','radius1','Radius Detector 1','radius2','Radius Detector 2',$
    'angle','Image Angle','acqFrameDuration','Acquisition Frame Duration','acqTerminationCond','Acquisition Termination Condition',$
    'units','Units For Pixel Values','radiopharmaca','Radiopharmaca','admDose','Administered Activity (MBq)','admDoseTime','Administered Activity Time',$
    'reconMethod','Reconstruction Method','attCorrMethod','Attenuation Correction Method','scaCorrMethod','Scatter Correction Method', 'scatterFrac','Scatter Fraction',$
    'imgFreq','Imaging Frequency (MHz)','MRacqType','MR Acquisition Type','MRscanSeq','MR Scanning Sequence','MRseqVariant','MR Sequence Variant',$
    'TR','TR (Repetition Time)','TE','TE (Echo Time)','NSA','Number Of Averages','flipAng','Flip Angle','spaceSlice','Spacing Between Slices',$
    'frameNo', 'Frame Number')
  return, imgStructDesc
end

; *************** display images *********************************

; return rescaled image within windowLevel (range) to display with tvscl
function adjustWindowLevel, arr, range

  high=WHERE(arr GT range(1)) & low=WHERE(arr LT range(0))
  IF high(0) NE -1 THEN arr(high)=range(1) & IF low(0) NE -1 THEN arr(low)=range(0)
  adjarr=(arr-range(0))*255.0/(range(1)-range(0))

  return, adjArr
end

;return image matrix scaled with slope and intercept
;frame -1 means single-frame image else frame starting on 1
function readImg, adr, frame
  WIDGET_CONTROL, /HOURGLASS
  IF FILE_TEST(adr) THEN BEGIN
    qd=QUERY_DICOM(adr)
    IF qd THEN BEGIN
      o=obj_new('idlffdicom')
      t=o->read(adr)

      ;image with correct values and rotation
      test=o->GetReference('0028'x,'1052'x)
      test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
      IF test(0) NE -1 THEN intercept=FLOAT(*(test_peker[0])) ELSE intercept=0

      test=o->GetReference('0028'x,'1053'x)
      test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
      IF test(0) NE -1 THEN slope=FLOAT(*(test_peker[0])) ELSE slope=1.

      test=o->GetReference('0018'x,'5100'x)
      test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
      ori=0
      IF test(0) NE -1 THEN BEGIN
        temporient=STRTRIM(STRING(*(test_peker[0])),2)
        IF temporient EQ 'FFS' THEN ori=1
      ENDIF

      ;multiframe?
      test=o->GetReference('7FE0'x,'0010'x)
      IF test(0) NE -1 THEN BEGIN
        IF frame EQ -1 THEN BEGIN
          ;real image is last image (icon images first)
          test_peker=o->GetValue(REFERENCE=test[N_ELEMENTS(test)-1],/NO_COPY)
          matrix=FLOAT(*(test_peker[0]))
        ENDIF ELSE BEGIN
          ;multiframe
          test_peker=o->GetValue(REFERENCE=test[frame-1],/NO_COPY)
          matrix=FLOAT(*(test_peker[0]))
        ENDELSE

        matrix=REVERSE(matrix,2)*slope + intercept
        IF ori EQ 1 THEN matrix=REVERSE(matrix)
        PTR_FREE, test_peker
      ENDIF ELSE BEGIN
        matrix=INTARR(100,100)
        ;sv=DIALOG_MESSAGE('File do not contain image data. Program might crash. Try closing the file.'+adr, DIALOG_PARENT=0)
      ENDELSE

      OBJ_DESTROY,o
    ENDIF ELSE BEGIN; 'dat file
      RESTORE, adr
      matrix=imageQCmatrix.matrix;IF imageQCmatrix.nFrames GT 0 THEN matrix=imageQCmatrix.matrix[*,*,frame] ELSE matrix=imageQCmatrix.matrix
      imageQCmatrix=!null
    ENDELSE
  ENDIF ELSE BEGIN
    sv=DIALOG_MESSAGE('File no longer exists. Renamed or removed. Program might work instable with this file in the list. Try closing the file.'+adr, DIALOG_PARENT=0)
    matrix=INTARR(100,100)
  ENDELSE
  return, matrix
end


;**************** formating output and GUI stuff ***********************************

function ascDesc01, arr
  str=['(ASC) ','(DES) '] 
  newArr=STRARR(N_ELEMENTS(arr))+str(0)
  desc=WHERE(arr EQ 1)
  IF desc(0) NE -1 THEN newArr(desc)=str(1)
  RETURN, newArr
end

;adjust to resonable number of decimals
function formatCode, arr
  IF SIZE(arr, /TNAME) EQ 'STRING' THEN strFormatCode='(a0)' ELSE BEGIN
    maxa=MAX(ABS(arr))
    strInner='f0'
    IF maxa LE 1 THEN BEGIN
      strInner='f0.3'
      IF maxa LT 0.1 THEN BEGIN
        strInner='f0.4'
        IF maxa LT 0.01 THEN BEGIN
          strInner='f0.5'
          IF maxa LT 0.001 THEN BEGIN
            strInner='g0.5'
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF maxa GT 1 THEN strInner='f0.3'
    IF maxa GT 10 THEN strInner='f0.2'
    IF maxa GT 100 THEN strInner='f0.1'
    IF maxa GT 99999 THEN strInner='i0'
    strFormatCode='('+strInner+')'
  ENDELSE
  return, strFormatCode
end

;formatstring according to input type
function formatCodeType, val, type
  nVal=N_ELEMENTS(val)
  CASE type Of
    'BOOL':IF val EQ 0 THEN strFormatCode='("false ", i0)' ELSE strFormatCode='("true ", i0)'
    'STRING':strFormatCode='(a0)'
    'INT':BEGIN
      IF nVal GT 1 THEN BEGIN
        strFormatCode='('+STRING(nVal-1, FORMAT='(i0)')+'(i0," / "),i0)'
      ENDIF ELSE strFormatCode='(i0)'
    END
    'FLOAT':BEGIN
      IF nVal GT 1 THEN BEGIN
        strFormatCode='('+STRING(nVal-1, FORMAT='(i0)')+'(f0.1," / "),f0.1)'
      ENDIF ELSE strFormatCode=formatCode(val)
    END
    ELSE:
  ENDCASE

  return, strFormatCode
end

;assure . not , for float-inputs
function comma2pointFloat, txt
  txt=STRJOIN(STRSPLIT(txt, ',',/EXTRACT),'.')
  return, txt
end

;day month year format
function formatDMY, str
  IF STRLEN(str) EQ 8 THEN BEGIN
    strDMY=STRMID(str, 6, 2)+'.'+STRMID(str, 4, 2)+'.'+STRMID(str, 0, 4)
  ENDIF ELSE strDMY=str
  return, strDMY
end

function DMYtoYMD, str
  IF STRLEN(str) EQ 10 THEN BEGIN
    strSpl=STRSPLIT(str,'.',/EXTRACT)
    IF N_ELEMENTS(strSpl) EQ 3 THEN strYMD=strSpl(2)+'.'+strSpl(1)+'.'+strSpl(0) ELSE strYMD=str
  ENDIF ELSE strYMD=str
  return, strYMD
end

; return list of filenames for open files
; struc = structure of structures from readCT.pro
; full = 0 for only parentfolder\filename, =1 for full path
; marked = array of indexes for marked files, -1 means none is marked
;   full=1 returns only marked, full=0 returns all and set an X on the marked
; mMulti = multiMark array, -1 means no multimarking (X)
function getListOpenFiles, struc, full, marked, mMulti

  nn=N_TAGS(struc)
  markedArr=INTARR(nn)
  szMM=SIZE(mMulti, /DIMENSIONS)
  IF N_ELEMENTS(szMM) EQ 1 THEN szMM=[szMM,1]
  IF marked(0) NE -1 THEN markedArr(marked)=1 ELSE markedArr=markedArr+1
  IF full EQ 1 THEN BEGIN
    fileList=STRARR(TOTAL(markedArr))
    counter=0
    FOR i=0, nn-1 DO BEGIN
      IF markedArr(i) EQ 1 THEN BEGIN
        fileList(counter)=struc.(i).filename
        counter=counter+1
      ENDIF
    ENDFOR
  ENDIF ELSE BEGIN
    fileList=STRARR(nn)

    FOR i=0, nn-1 DO BEGIN
      add='   '
      IF mMulti(0) EQ -1 THEN BEGIN
        IF markedArr(i) AND marked(0) NE -1 THEN add='X ' ELSE add='   '
      ENDIF ELSE IF mMulti(0) NE -1 THEN BEGIN
        add=''
        IF i LT szMM(1) THEN BEGIN
          FOR j=0, szMM(0)-1 DO BEGIN
            IF mMulti[j,i] THEN add=add+STRING(j+1, FORMAT='(i0)') ELSE add=add+'  '
          ENDFOR
        ENDIF ELSE add=STRING(STRARR(szMM(0)), FORMAT='('+STRING(szMM(0), FORMAT='(i0)')+'(a2))')
        add=add+'   '
      ENDIF
      t=STRSPLIT(struc.(i).filename,'\',/EXTRACT)
      nSplit=N_ELeMENTS(t)

      endStr=''
      IF struc.(i).nFrames GT 1 THEN BEGIN
        IF struc.(i).zpos NE -999. AND struc.(i).slicethick GT 0. THEN endStr='  zpos '+STRING(struc.(i).zpos,FORMAT='(f0.3)')
        IF struc.(i).angle NE -999. THEN endStr='  angle '+STRING(struc.(i).zpos,FORMAT='(f0.3)')
        IF endStr EQ '' THEN endStr='  frame '+STRING(struc.(i).frameNo,FORMAT='(i0)')
      ENDIF

      fileList(i)=add+STRJOIN(t[nSplit-2:nSplit-1],'\')+endStr

    ENDFOR
  ENDELSE

  return, fileList
end

;get test-number based on current modality tab
function getResNmb, tabNmb, stringElem, allStrings
  actStrings=allStrings.(tabNmb)
  i=WHERE(actStrings EQ stringElem)
  resNmb=i
  return, resNmb
end

;get zpos of all marked images
function getZposMarked, struc, markedTemp
  nFrames=0
  IF struc.(0).nFrames GT 1 THEN BEGIN
    nImg=struc.(0).nFrames
    nFrames=nImg
  ENDIF ELSE nImg=N_ELEMENTS(TAG_NAMES(struc))
  zPos=FLTARR(nImg)
  IF nFrames EQ 0 THEN BEGIN
    FOR i = 0, nImg -1 DO zPos(i)=struc.(i).zpos
  ENDIF ELSE BEGIN
    zPos=struc.(0).zPos
  ENDELSE
  zPosMarked=zPos(markedTemp)
  return, zPosMarked
end

;return array (0/1) whether the tag in imgStruct is actual for the current modality
;arr_tagn - found tagnames in imgStruct for this version
;arr_isinfo - imgStructInfo array see ImageQC.pro
;moda - current modality
function actualTags, arr_tagn, arr_isinfo, moda
  actArr=INTARR(N_ELEMENTS(arr_tagn))+1;all actual as default
  tagsMentioned=STRUPCASE(arr_isinfo[0,*])
  tagsModality=arr_isinfo[2,*]
  FOR t=0, N_ELEMENTS(arr_tagn)-1 DO BEGIN
    pos=WHERE(tagsMentioned EQ arr_tagn(t))
    IF pos(0) NE -1 THEN BEGIN
      IF tagsModality(pos(0)) NE '' THEN BEGIN
        modpos=STRPOS(tagsModality(pos(0)),STRING(moda, FORMAT='(i0)'))
        IF modpos(0) EQ -1 THEN actArr(t)=0; did not find current modality-number in string defining actual modalities
      ENDIF
    ENDIF
  ENDFOR
  return, actArr
end

;********** finding center and widths **********************

;centroid function not accurate enough, use centroid first and optimize center with this after
;centerProfile assume all positive values and values to be centered is the maximum values
;centerProfile also assume relatively smooth profiles
function centerProfile, vec

  sHalf= -1
  ;center=sHalf
  ;centerval=TOTAL(vec[sHalf(0)-1:sHalf(0)+1])/3.
  ;outerVal=(vec(0)+vec(N_ELEMENTS(vec)-1))/2
  ;IF centerval LT outerVal THEN vec= max(vec)-vec ;invert array

  nVec=N_ELEMENTS(vec)
  posMax=WHERE(vec EQ MAX(vec))
  posMax=ROUND(MEAN(posMax))
  IF posMax NE 0 AND posMax NE nVec-1 THEN centerVal=MEAN(vec[posMax-1:posMax+1]) ELSE centerVal=MAX(vec)
  nOut=CEIL(0.05*nVec)
  outerVal=0.5*(MEAN(vec[0:nOut])+MEAN(vec[nVec-nOut-1:nVec-1]))
  treshold=0.5*(centerVal+outerVal)
  above=WHERE(vec GT treshold, nn)
  ;remove noise even more
  abTemp=FLTARR(nVec)
  abTemp(above)=1.
  abTemp=SMOOTH(abTemp,3, /EDGE_MIRROR)
  above=WHERE(abTemp EQ 1., nn)

  If above(0) NE -1 THEN BEGIN
    first=above(0)-1;remove noise even more smooths outer edge too -1
    last=above(nn-1)+1;remove noise even more smooths outer edge too +1

    IF first EQ -1 OR first EQ 0 THEN first=1
    IF first GE 1 AND last LE N_ELEMENTS(vec)-2 THEN BEGIN

      dy=treshold-vec(first-1)
      IF vec(first) NE vec(first-1) THEN dx=dy/(vec(first)-vec(first-1)) ELSE dx=0.
      x1=first-1+dx

      dy=vec(last)-treshold
      IF vec(last) NE vec(last+1) THEN dx=dy/(vec(last)-vec(last+1)) ELSE dx=0.
      x2=last+dx
      sHalf=(x1+x2)/2
      IF sHalf GT nVec-1 THEN sHalf=-1
      IF sHalf LT 0 THEN sHalf=-1
    ENDIF
  ENDIF

  RETURN, sHalf
end

;return [x,y] position for center of mass
;From: http://www.idlcoyote.com/tip_examples/centroid.pro
;added treshold and invert if object lower intensity than outer
function Centroid, array, treshold, allowInvert
  array2=array;avoid problems when array changes activeImg as input
  s = Size(array2, /Dimensions)
  sHalf= s/2

  IF N_ELEMENTS(allowInvert) EQ 0 THEN allowInvert = 1
  IF allowInvert THEN BEGIN
    outerVal=MEAN(array2[*,0])
    IF abs(outerVal- min(array2)) GT abs(outerVal- max(array2)) THEN array2= max(array2)-array2 ;invert array
  ENDIF
  
  lower=WHERE(array2 LT treshold)
  array2=array2-MIN(array2);starting at zero
  arrTemp=array2
  IF lower(0) NE -1 THEN arrTemp(lower)=0.

  x=centerProfile(SMOOTH(TOTAL(arrTemp,2),3,/EDGE_MIRROR))
  y=centerProfile(SMOOTH(TOTAL(arrTemp,1),3,/EDGE_MIRROR))

  ;filter array and average of 3 neighbour profiles to remove noise
  ;filterw=CEIL(0.1*s(0))
  arrayFilt=SMOOTH(array2,5,/EDGE_MIRROR)
  yNo=ROUND(y)
  xNo=ROUND(x)
  IF yNo GE 1 AND yNo LE s(1)-2 THEN vecX=TOTAL(arrayFilt[*,yNo-1:yNo+1],2)*1./3 ELSE vecX=arrayFilt[*,yNo]
  IF xNo GE 1 AND xNo LE s(0)-2 THEN vecY=TOTAL(arrayFilt[xNo-1:xNo+1,*],1)*1./3 ELSE vecY=arrayFilt[xNo,*]

  optimX=centerProfile(vecX)
  optimY=centerProfile(vecY)

  RETURN, [optimX, optimY]
end

;find position of treshold value
function findPosTreshold, vec, treshold
  lenVec=N_ELEMENTS(vec)
  pos=-1
  above=WHERE(vec GT treshold, nn)
  If above(0) NE -1 AND nn NE lenVec THEN BEGIN
    rgt=above(0)
    IF rgt EQ 0 THEN rgt=above(nn-1)+1
    lft=rgt-1

    IF lft GT 0 AND lft LT lenVec AND rgt GT 0 AND rgt LT lenVec THEN BEGIN
      dy=treshold-vec(lft)
      dx=dy/(vec(rgt)-vec(lft))

      pos=lft+dx
    ENDIF
  ENDIF

  return, pos
end

;find the width of a profile at specified threshold value using interpolation. Return width and centerpos of profile at threshold
function getWidthAtThreshold, vec, threshold
  width=-1
  center=-1

  aboveInd=WHERE(vec GT threshold)
  belowInd=WHERE(vec LT threshold)

  IF aboveInd(0) GE 1 THEN above=aboveInd ELSE above=belowInd
  nn=N_ELEMENTS(above)

  If above(0) GE 1 THEN BEGIN
    first=above(0)
    last=above(nn-1)
    IF last LT N_ELEMENTS(vec)-1 THEN BEGIN

      dy=vec(first)-threshold;dy=treshold-vec(first-1)
      dx=dy/(vec(first)-vec(first-1))
      x1=first-dx;x1=first+dx

      dy=vec(last)-threshold
      dx=dy/(vec(last)-vec(last+1))
      x2=last+dx
      center=(x1+x2)/2
      width=x2-x1
    ENDIF
  ENDIF

  return, [width,center]

end

;find x for given y where linear line is given by two points (left/right)
function getInterpX, Y,x1,x2,y1,y2
  w=(Y-y2)/(y1-y2)
  X=w*x1+(1-w)*x2

  RETURN, X
end

;smoothes irregularly sampled Y within +/- w
function smoothIrreg, X, Y, w
  IF w GT 0 THEN BEGIN
    nY=N_ELEMENTS(Y)
    smoothedY=0.0*Y
    FOR i=0, nY-1 DO BEGIN
      tempX=ABS(X-X(i))
      idx=WHERE(tempX LE w, nidx)
      fac=w-tempX(idx)
      fac=fac/TOTAL(fac)
      smoothedY(i)=TOTAL(fac*Y(idx))
    ENDFOR
  ENDIF ELSE smoothedY=Y
  return, smoothedY
end

;find the vector from image given start and end position [x,y]
;modified part of IDL function:profile - without selection by mouse part
function getProfile, imagein, start, ende
  s=size(imagein)
  sx = s[1] & sy=s[2]
  dx = float(ende(0)-start(0))       ;delta x
  dy = float(ende(1)-start(1))
  n = abs(dx) > abs(dy)
  r = fltarr(n+1)

  if abs(dx) gt abs(dy) then begin
    if ende(0) ge start(0) then s=1 else s=-1
    sy = (ende(1)-start(1))/abs(dx)
  endif else begin
    if ende(1) ge start(1) then sy=1 else sy=-1
    s = (ende(0)-start(0))/abs(dy)
  endelse

  xx = long(findgen(n+1l)*s+start(0))    ;X values, make into longwords.
  yy = long(findgen(n+1l)*sy+start(1))   ;Y values

  return,imagein[long(yy)*sx + xx]
end

;get circular mask within array of size arrSz
;center of circle [x,y] pix
function getROIcircle, arrSz, center, radius

  arr=SHIFT(DIST(arrSz(0),arrSz(1)),center(0),center(1))
  in=WHERE(arr LE radius)
  circle=INTARR(arrSz(0), arrSz(1))
  circle(in)=1

  return, circle
end



;*********** fourier and gauss stuff ********************
function ESFtoLSF, esfVec
  n=N_ELEMENTS(esfVec)
  lsfVec=esfVec*0.
  FOR i=1, n-2 DO BEGIN
    lsfVec(i)=esfVec(i+1)-esfVec(i-1)
  ENDFOR
  return, lsfVec
end

;calculate y(x) given x,sigma,A
function calcGauss, xvals, stdev, amp, meanval
  yvals=amp*EXP(-.5*(xvals-meanval)^2/stdev^2)
  return, yvals
end

;fit, X,Y to gaussian, allow for visual verification and manual edit, X assumed to be centered already
function getGaussFit, X, Y, pix, fitWidthFactor; assume X already centered
  nn=N_ELEMENTS(X)
  weights= FLTARR(nn)+1.
  res=getWidthAtThreshold(Y,max(Y)/2)

  ss1=0
  yfit=-1
  A=-1
  IF res(0) NE -1 THEN BEGIN
    FWHM1=res(0)*pix
    center=ABS(X(0))
    sigma1=FWHM1/(2*SQRT(2*ALOG(2)))

    res2=getWidthAtThreshold(Y,min(Y)/2)
    IF res2(0) NE -1 THEN BEGIN
      FWHM2=res2(0)*pix
      sigma2=FWHM2/(2*SQRT(2*ALOG(2)))
      IF sigma2 LT sigma1 THEN sigma2=2.*sigma1
    ENDIF ELSE sigma2=2.*sigma1

    IF fitWidthFactor EQ 0 THEN ss1=0 ELSE ss1=ROUND(center/pix)-ROUND(FWHM1/pix)*fitWidthFactor
    IF fitWidthFactor EQ 0 THEN ss2=nn-1 ELSE ss2=ROUND(center/pix)+ROUND(FWHM1/pix)*fitWidthFactor
    IF ss1 LT 0 THEN ss1=0
    IF ss2 GT nn-1 THEN ss2=nn-1
    A = [max(Y[ss1:ss2])-min(Y[ss1:ss2]),1.5*min(Y[ss1:ss2]),sigma1, sigma2];first guess parameters for curvefit gaussFitAdd2

    yfit = CURVEFIT(X[ss1:ss2], Y[ss1:ss2], weights[ss1:ss2], A, FUNCTION_NAME='gaussFitAdd2', ITER=iter, CHISQ=chisq);, TOL=.00001*(1.0*10^(-3)));, ITMAX=100)

    IF A(1) GT A(0) THEN BEGIN
      ;resort such that highest amp first
      newA=[A(1),A(0),A(3),A(2)]
      A=newA
    ENDIF

    IF ABS(A[3]) GT 10.*A[2] OR A[3] LT 0 THEN BEGIN; retry with single gaussfit - allow double gauss with both terms positive
      ;IF A(1) GT 0 OR ABS(A[3]) GT 10.*A[2] THEN BEGIN; retry with single gaussfit - double is for sharp filters
      yfit=gaussfit(X[ss1:ss2], Y[ss1:ss2], A, ESTIMATES=[max(Y[ss1:ss2]),0,sigma1], NTERMS=3)
      A(1)=0
    ENDIF

  ENDIF; res(0)=-1
  retStruct=CREATE_STRUCT('yfit',yfit,'A',A,'startpos',ss1)

  return, retStruct
end

;gauss to gauss continuous version:
;http://www.cse.yorku.ca/~kosta/CompVis_Notes/fourier_transform_Gaussian.pdf
function getMTFgauss, A, sigmaF
  nSteps=200;sample 20 steps from 0 to 1 stdv MTF curve A0 (stdev=1/A(2))
  kvals=FINDGEN(nSteps)*(10./nSteps)/A(2)
  Fgu0=calcGauss(kvals, 1/A(2),A(0)*A(2),0)
  IF N_ELEMENTS(A) EQ 4 THEN Fgu1=calcGauss(kvals, 1/A(3),A(1)*A(3),0) ELSE Fgu1=0.
  If sigmaF NE 0 THEN Ffilter=calcGauss(kvals,1./(sigmaF),1.0,0) ELSE Ffilter=1.
  MTF=(Fgu0+Fgu1)/Ffilter
  k=kvals/(2*!pi)

  MTF=MTF/MTF(0)

  gradient=SHIFT(MTF,-1)-MTF
  ng=N_ELEMENTS(gradient)-2
  gradient=gradient[0: ng];pop last
  IF gradient(ng-1) GT 0 THEN BEGIN
    negVal=WHERE(gradient LT 0, nNeg)
    unFilt=Fgu0+Fgu1
    unFilt=unFilt/unFilt(0)
    MTF[nNeg:nSteps-1]=unFilt[nNeg:nSteps-1]
  ENDIF
  retStruct=CREATE_STRUCT('k',k,'MTF',MTF)
  return, retStruct
end

;calculate filter given sigma and size of filter
function gaussFilter, sigmaF, nn
  filter=-1
  If sigmaF NE 0 THEN BEGIN
    IF nn*.5 EQ nn/2 THEN odd=0 ELSE odd=1
    nnn=nn/2-odd
    xf=FINDGEN(nnn)
    yf=EXP(-0.5*xf^2/sigmaF^2)
    filter=[reverse(yf[1:nnn-1]),yf]
    IF odd EQ 0 THEN filter=[0,filter]
    filter=filter/TOTAL(filter)
    nonZeros=WHERE(filter NE 0.)
    filter=filter(nonZeros)
  Endif
  return, filter
end

;fft of vector with optional zero-padding
function FFTvector, vec, padfactor
  szV=N_ELEMENTS(vec)
  szPadded=2*((padfactor*szV)/2);assure even number
  halfsz=szV/2
  nullPadd=FLTARR(szPadded)
  nullPadd[szPadded/2-halfsz:szPadded/2-halfsz+szV-1]=vec
  vecPadd=nullPadd
  fvecComplex=FFT(vecPadd,/CENTER)
  fvec=szPadded*SQRT(REAL_PART(fvecComplex)^2+IMAGINARY(fvecComplex)^2); modulus of Fouriertransform * size of submatrix (divided by 1/N during FFT)
  fvec=fvec[szPadded/2:szPadded-1]
  fvec=fvec/fvec(0)
  return, fvec
end

;zero Padd matrix to 3xn size
function zeroPadd3, matrix

  sz=SIZE(matrix, /DIMENSIONS)
  padded=FLTARR(3*sz(0),3*sz(1))
  padded[sz(0):sz(0)*2-1,sz(1):sz(1)*2-1]=matrix

  return, padded
end

;***************** all others ***********************

;correction matrix based on input image with inverse square law when point source flooding planar detektor
function corrDistPointSource, img, sid, pix, thick, attcoeff
  ;center of imager
  szImg=SIZE(img, /DIMENSIONS)
  distCenter=FLTARR(szImg/2);pixel distance from center
  FOR i=0, szImg(0)/2-1 DO BEGIN
    FOR j=0, szImg(1)/2-1 DO distCenter(i,j)=SQRT((i+0.5)^2+(j+0.5)^2)
  ENDFOR
  distCenter=pix*distCenter

  ;equation A6 from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3966082/#x0 (doi:  10.1118/1.3125642)
  ;corrMatrix = f central relativ to f periferral excluding N
  ds=distCenter^2+sid^2
  fCentral =  1. / (sid^2)
  IF thick GT 0. AND attcoeff GT 0. THEN BEGIN
    euT=EXP(-attcoeff*thick)
    fPerif = ((1-euT^(SQRT(ds)/sid))/(1-euT))*(sid/(ds^1.5))
  ENDIF ELSE fPerif=(sid/(ds^1.5))

  corrMatrix=img*0.0
  corrQuad=fCentral/fPerif
  corrMatrix[0:szImg(0)/2-1,0:szImg(1)/2-1]=ROTATE(corrQuad,2)
  corrMatrix[0:szImg(0)/2-1,szImg(1)/2:szImg(1)-1]=ROTATE(corrQuad,5)
  corrMatrix[szImg(0)/2:szImg(0)-1,0:szImg(1)/2-1]=ROTATE(corrQuad,7)
  corrMatrix[szImg(0)/2:szImg(0)-1,szImg(1)/2:szImg(1)-1]=corrQuad

  return, corrMatrix
end

;combine 2x2 pixels to get matrix size from 2^n to 2^(n-1)
function sum2x2pix, img, repeats
  ;if image cannot be divided by 2 - right and top pixels missed
  szImg=SIZE(img, /DIMENSIONS)
  downscaledImg=FLTARR(szImg/2)
  imgToSum=img
  FOR r=0, repeats DO BEGIN
    FOR i=0, szImg(0)/2-1 DO BEGIN
      FOR j=0, szImg(1)/2-1 DO BEGIN
        downScaledImg[i,j]=TOTAL(imgToSum[2*i:2*i+1,2*j:2*j+1])
      ENDFOR
    ENDFOR
    imgToSum=downScaledImg
    szImg=szImg/2
    IF r NE repeats THEN downscaledImg=FLTARR(szImg/2)
  ENDFOR
  return, downscaledImg
end

;return max position [x,y] in array after medianfilter (width 5)
function findMedianMax, array
  array=MEDIAN(array, 5)
  pos=WHERE(array EQ MAX(array))
  posXY=ARRAY_INDICES(array, pos(0))
  return, posXY
end

function linearizeSTP, matrix, STP
  ;add other STP forms when ready....
  linMatrix=(matrix-STP.b)/STP.a(0)
  return, linMatrix
end

