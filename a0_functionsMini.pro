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
  nNewOrder=N_ELEMENTS(newOrder)
  tagname=TAG_NAMES(struct)
  structNew=CREATE_STRUCT(tagname(newOrder(0)),struct.(newOrder(0)))
  FOR i=1, nNewOrder-1 DO structNew=CREATE_STRUCT(structNew,tagname(newOrder(i)),struct.(newOrder(i)))
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

function updConfigGetSiemensQC

  PETstrings=CREATE_STRUCT($
    'English',['Scan Date:','Partial Setup','Full setup','Time Alignment','Calibration','Block Noise','Block Efficiency','Randoms','Scanner Efficiency','Scatter Ratio','Image Plane','Block Timing','Phantom position','true'])
  CTstrings=CREATE_STRUCT($
    'English',['Quality Daily','Quality*Constancy','Slice','Homogeneity','Noise','Tolerance','Test: Typical head','Test: Typical body','Number of images','Test: Sharpest mode','Tester name','Product Name','Serial Number','Tube Asse','Description','Value','Result','Test Result'],$
    'Norsk',['Kvalitet daglig','Kvalitets*konstans','Snitt','Homogenitet','St?y','Toleranse','Test: Typisk hode','Test: Typisk kropp','Antall bilder','Test: Skarpeste modus','Kontroll?rnavn','Produktnavn','Serienummer','R?renhet','Beskrivelse','Verdi','Resultat','Test Resultat'])
  months=['January','February','March','April','May','June','July','August','September','October','November','December']

  headersPET=['Date','ICS Name','Partial','Full',$
    'Time Align','Calib Factor','Measured Randoms','Scanner Efficiency',$
    'Scatter Ratio','ECF','Time Alignment Residual', 'Time Alignment fit x','Time Alignment fit y',$
    'Phantom Pos x', 'Phantom Pos y']
  headersCT=['Date','Tester name','Product Name','Serial Number','Serial Tube A', 'Serial Tube B',$
    'HUwater head min','HUwater head max','HUwater body min','HUwater body max',$
    'Diff head max(abs)','Diff body max(abs)',$
    'Noise head max','Noise body max',$
    'Slice head min','Slice head max','Slice body min','Slice body max',$
    'MTF50 B smooth','MTF10 B smooth','MTF50 H smooth','MTF10 H smooth','MTF50 H sharp','MTF10 H sharp','MTF50 UHR','MTF10 UHR',$
    'HUwater dblA min','HUwater dblA max','HUwater dblB min','HUwater dblB max',$
    'Diff dblA max(abs)','Diff dblB max(abs)',$
    'Noise dblA max','Noise dblB max',$
    'Slice dblA min','Slice dblA max','Slice dblB min','Slice dblB max',$
    'MTF50 dblA smooth','MTF10 dblA smooth','MTF50 dblB smooth','MTF10 dblB smooth']
  headersCT_T2=['Date','Tester name','Product Name','Serial Number','Tube ID',$
    'HUwater 110kV min','HUwater 110kV max','HUwater 130kV min','HUwater 130kV max',$
    'Diff 110kV max(abs)','Diff 130kV max(abs)',$
    'Noise 80kV','Noise 110kV','Noise 130kV',$
    'Slice 1mm','Slice 1.5mm','Slice 2.5mm','Slice 4mm','Slice 5mm',$
    'MTF50 B31s','MTF10 B31s','MTF50 H41s','MTF10 H41s','MTF50 U90s','MTF10 U90s']
  headersCT_Intevo=['Date','Tester name','Product Name','Serial Number','Tube ID',$
    'HUwater head min','HUwater head max','HUwater body min','HUwater body max',$
    'Diff head max(abs)','Diff body max(abs)',$
    'Noise head max','Noise body max',$
    'Slice head min','Slice head max','Slice body min','Slice body max',$
    'MTF50 B smooth','MTF10 B smooth','MTF50 H smooth','MTF10 H smooth','MTF50 UHR','MTF10 UHR']
  headers=CREATE_STRUCT('PET',headersPET,'CT', headersCT,'CT_T2', headersCT_T2, 'CT_Intevo', headersCT_Intevo)

  cGSQC=CREATE_STRUCT('PET',PETstrings,'CT',CTstrings,'months',months, 'headers',headers)
  return, cGSQC
end

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
    'autoImportPath','','autoContinue',0,'wait',[5,2],$
    'qtOutTemps', ['DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT'], $
    'MTFtype',2,'MTFtypeX',1,'MTFtypeNM',1,'MTFtypeSPECT',1, $
    'plotMTF',3,'plotMTFX', 3, 'plotMTFNM',4,'plotMTFSPECT',4, 'tableMTF',0,'cyclMTF',0,'tableMTFX', 0, $
    'MTFroiSz',11.0,'MTFroiSzX',[20.,50.],'MTFroiSzNM',[20.,20.],'MTFroiSzSPECT',30.,'MTF3dSPECT',1, $
    'cutLSF',1,'cutLSF1',3,'cutLSF2',1, 'cutLSFX', 1, 'cutLSFX1', 3, 'offxyMTF', [0,0],'offxyMTF_X', [0,0],'offxyMTF_unit', 0, 'offxyMTF_X_unit', 0,$
    'searchMaxMTF_ROI',0,$
    'LinROIrad',3.,'LinROIradS',11., 'LinTab',lintab, $
    'RampDist',38.,'RampLen',60.,'RampBackG',5.,'RampSearch',5,'RampAvg',1,'RampType',0,'RampDens',0,$
    'HomogROIsz',10., 'HomogROIszX',10., 'altHomogX', 0, 'HomogROIszPET', 10.,'HomogROIdist',55.,'HomogROIdistPET',55.,$
    'NoiseROIsz',55., 'NoiseXpercent',90, 'HUwaterROIsz', 55.,$
    'typeROI',0,'ROIrad',5.,'ROIx',10.,'ROIy',10.,'ROIa',0.,'offxyROI', [0,0], 'offxyROI_unit', 0,$
    'typeROIX',0,'ROIXrad',5.,'ROIXx',10.,'ROIXy',10.,'ROIXa',0.,'offxyROIX', [0,0], 'offxyROIX_unit', 0,$
    'typeROIMR',0,'ROIMRrad',5.,'ROIMRx',10.,'ROIMRy',10.,'ROIMRa',0.,'offxyROIMR', [0,0], 'offxyROIMR_unit', 0,$
    'NPSroiSz', 50, 'NPSroiDist', 50., 'NPSsubNN', 20, 'NPSroiSzX', 256, 'NPSsubSzX', 5, 'NPSavg', 1, $
    'STProiSz', 11.3, $
    'unifAreaRatio', 0.95,'SNIAreaRatio', 0.9,'unifCorr',0,'unifCorrPos',[0,0],'unifCorrRad',-1.,'SNIcorr',0,'SNIcorrPos',[0,0],'SNIcorrRad',-1.,$
    'SNI_fcd',[1.3,28.,65.],'plotSNI',1,$
    'barROIsz',50.0,'barWidths',[6.4,4.8,4.0,3.2],$
    'ScanSpeedAvg', 25, 'ScanSpeedHeight', 100., 'ScanSpeedFiltW', 15, $
    'ContrastRad1', 20., 'ContrastRad2', 58.,$
    'CrossROIsz', 60., 'CrossVol', 0.0,$
    'SNR_MR_ROI', 75., 'PIU_MR_ROI', 75., 'Ghost_MR_ROI', [80.,40.,10.,10.,1.],'GD_MR_act', 190.,'Slice_MR_ROI',[0.1,100.,3.,-2.5,2.5,1.])
  expInfoPatterns=CREATE_STRUCT('mAs_profile',['ZPOS','MAS'])

  userinfo=get_login_info()
  commonConfig=CREATE_STRUCT('defConfigNo',1,'saveBlocked',1,'saveStamp',systime(/SECONDS),'username',userinfo.user_name,'autoUnBlock',8,'expInfoPatterns',expInfoPatterns);NB if changed check on remBlock in settings.pro
  configSdefault=CREATE_STRUCT('commonConfig',commonConfig,'configDefault',configDefault)

  newConfigS=-1

  IF file EQ '' THEN newConfigS=configSdefault ELSE BEGIN
    ;find existing values and paste into new configS structure
    RESTORE, file
    errCounter=0
    IF N_ELEMENTS(config) NE 0 THEN oldConfigS=CREATE_STRUCT('commonConfig',commonConfig,'configDefault',config) ELSE errCounter=1; very old config file (before configS)
    IF N_ELEMENTS(configS) NE 0 THEN oldConfigS=configS ELSE errCounter=errCounter+1
    IF errCounter NE 2 THEN BEGIN
      ;copy values into newest version config structure

      ;commonConfig (configS.(0)
      currCommonTags=TAG_NAMES(commonConfig)
      oldCommon=commonConfig
      IF SIZE(oldConfigS.(0),/TNAME) NE 'STRUCT' THEN BEGIN
        oldCommon.(0)=oldConfigS.(0);defConfigNo from older versions
        oldCommonTags=currCommonTags
      ENDIF ELSE BEGIN
        oldCommonTags=TAG_NAMES(oldConfigS.(0))

        FOR i=0, N_ELEMENTS(currCommonTags)-1 DO BEGIN
          IF oldCommonTags.HasValue(currCommonTags(i)) THEN BEGIN
            iOld=WHERE(oldCommonTags EQ currCommonTags(i))
            IF iOld NE -1 THEN BEGIN
              IF currCommonTags(i) EQ 'SAVESTAMP' THEN BEGIN
                IF SIZE(oldConfigS.(0).(iOld), /TNAME) EQ 'STRING' THEN BEGIN
                  sv=DIALOG_MESSAGE('Config file of older version where timestamp saved as string. For your information (before it is lost): This file was last saved '+oldConfigS.(0).(iOld))
                ENDIF
              ENDIF
              IF SIZE(oldCommon.(i), /TNAME) EQ 'STRUCT' THEN BEGIN
                oldCommon=replaceStructStruct(oldCommon, oldConfigS.(0).(iOld), i)
              ENDIF ELSE oldCommon.(i)=oldConfigS.(0).(iOld)
            ENDIF
          ENDIF
        ENDFOR
      ENDELSE
      newConfigS=CREATE_STRUCT('commonConfig',oldCommon)

      ;the rest configS.(1+)
      defaultTags=TAG_NAMES(configDefault)
      restoreTagsS=TAG_NAMES(oldConfigS)
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

        ;convert old tags to new tags
        IF oldTags.HasValue('OFFXY') THEN BEGIN; offxy for MTF split to CT and Xray and more added hence naming different
          ff=WHERE(oldTags EQ 'OFFXY')
          configTemp.OFFXYMTF=oldConfigS.(i).(ff)
          configTemp.OFFXYMTF_X=oldConfigS.(i).(ff)
        ENDIF

        newConfigS=CREATE_STRUCT(newConfigS,restoreTagsS(i),configTemp)
      ENDFOR

    ENDIF ELSE sv=DIALOG_MESSAGE('Found no valid config structure.', DIALOG_PARENT=0)

  ENDELSE;file ''

  return, newConfigS
end

function updateQuickT, file, mOpt, TEMPPA=temppa
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
      ;mOpt= Struct 'CT',[1,2,3,4,5,6,7,0,0,0],'Xray',[1,2,3,4,5,6,0,0],'NM',[1,1,1,1,0,0,0],'SPECT', INTARR(3),'PET',INTARR(3),'MR',INTARR(1))
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
              IF adr(0) NE '' THEN BEGIN
                fi=FILE_INFO(FILE_DIRNAME(adr))
                IF fi.write THEN FILE_COPY, file, adr ELSE BEGIN
                  IF N_ELEMENTS(temppa) GT 0 THEN BEGIN
                    adr=temppa+FILE_BASENAME(adr)+'.dat'
                    fi=FILE_INFO(temppa)
                    IF fi.write THEN BEGIN
                      FILE_COPY, file, adr
                      sv=DIALOG_MESSAGE('Failed to save backup in selected folder. Backup can be found in '+temppa+' instead.')
                    ENDIF ELSE sv=DIALOG_MESSAGE('Failed to save backup. No write permission on selected folder nor in '+temppa+'.',/ERROR)
                  ENDIF ELSE sv=DIALOG_MESSAGE('Failed to save backup in selected folder. No write permission.',/ERROR)
                ENDELSE
              ENDIF
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
                IF N_ELEMENTS(sz) EQ 1 THEN sz=[sz,1]
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
    'CTLIN',-1,'HUWATER',-1,'EXP',-1,'ROI',-1)
  defXray=CREATE_STRUCT($
    'STP', CREATE_STRUCT('Pixel_mean',CREATE_STRUCT('ALT',0,'COLUMNS',2,'CALC',0,'PER_SERIES',0)),$
    'HOMOG',-1,'NOISE',-1,'EXP',-1,'MTF',-1,'ROI',-1)
  defNM=CREATE_STRUCT('UNIF', -1,'SNI',-1,'ACQ',-1,'BAR',-1)
  defPET=CREATE_STRUCT('HOMOG', -1)
  defMR=CREATE_STRUCT('DCM', -1,'SNR',-1,'PIU',-1,'GHOST',-1,'GEOMDIST',-1,'SLICETHICK',-1,'ROI',-1)
  quickToutDefault=CREATE_STRUCT('CT',CREATE_STRUCT('DEFAULT',defCT),'Xray',CREATE_STRUCT('DEFAULT',defXray),'NM',CREATE_STRUCT('DEFAULT',defNM),'PET',CREATE_STRUCT('DEFAULT',defPET),'MR',CREATE_STRUCT('DEFAULT',defMR))

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
      quickToutThisDCM=CREATE_STRUCT('ALT',-1,'TAGS',-1,'TAGFORMATS',-1);alt -1 means additional dicom tags to include for tests EXP,DCM,ACQ
      tagNewestDCM=TAG_NAMES(quickToutThisDCM)

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

              ;nTests=N_TAGS(oldQuickTout.(ii).(j))
              FOR k=0, N_ELEMENTS(allTestsOld)-1 DO BEGIN; for each defined test

                IF SIZE(oldQuickTout.(ii).(j).(k), /TNAME) EQ 'STRUCT' THEN BEGIN
                  outputsThisTest=CREATE_STRUCT('popit',0)
                  outPutNames=TAG_NAMES(oldQuickTout.(ii).(j).(k))
                  FOR l=0, N_ELEMENTS(outPutNames)-1 DO BEGIN; for each defined output
                    ;IF SIZE(oldQuickTout.(ii).(j).(k).(l), /TNAME) EQ 'STRUCT' THEN BEGIN
                    oldTags=TAG_NAMES(oldQuickTout.(ii).(j).(k).(l))

                    paramsThis=CREATE_STRUCT('popit',0)

                    IF oldQuickTout.(ii).(j).(k).(l).(0) EQ -1 THEN BEGIN ; alt=-1
                      IF ~ARRAY_EQUAL(oldTags, tagNewestDCM) THEN BEGIN
                        FOR m=0, N_ELEMENTS(tagNewestDCM)-1 DO BEGIN
                          IF oldTags.HasValue(tagNewestDCM(m)) THEN BEGIN
                            ;copy tag content
                            ff=WHERE(oldTags EQ tagNewestDCM(m))
                            paramsThis=CREATE_STRUCT(paramsThis, tagNewestDCM(m), oldQuickTout.(ii).(j).(k).(l).(ff))
                          ENDIF ELSE paramsThis=CREATE_STRUCT(paramsThis, tagNewestDCM(j),quickToutThisDCM.(m))
                        ENDFOR

                        paramsThis=removeIDstructstruct(paramsThis, 0);pop off first dummy element

                      ENDIF ELSE paramsThis=oldQuickTout.(ii).(j).(k).(l)
                    ENDIF ELSE BEGIN
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
                    ENDELSE
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

function updateLoadT, file, mOpt, TEMPPA=temppa
  IF file EQ '' THEN loadT=!Null ELSE BEGIN
    ;find existing values and paste into new loadTemp structure (no doing anything yet as this is first version with this)
    RESTORE, file
    IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
      ;update with modalities from older versions
      IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
        tempsExist=TAG_NAMES(loadTemp)
        loadT=loadTemp
        IF ~ARRAY_EQUAL(tempsExist[0:2],['CT','XRAY','NM']) THEN BEGIN;at least three first match = correct type of structure
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
                IF adr(0) NE '' THEN BEGIN
                  fi=FILE_INFO(FILE_DIRNAME(adr))
                  IF fi.write THEN FILE_COPY, file, adr ELSE BEGIN
                    adr=temppa+FILE_BASENAME(adr)+'.dat'
                    fi=FILE_INFO(temppa)
                    IF fi.write THEN BEGIN
                      FILE_COPY, file, adr
                      sv=DIALOG_MESSAGE('Failed to save backup in selected folder. Backup can be found in '+temppa+' instead.')
                    ENDIF ELSE sv=DIALOG_MESSAGE('Failed to save backup. No write permission on selected folder.',/ERROR)
                  ENDELSE
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      ;securing older versions tags
      IF N_ELEMENTS(loadT) GT 0 THEN BEGIN
        ;path - folder where the images should be found
        ;statName - station name as defined in DICOM (or empty) - to automatically recognize and sort images into their corresponding folders
        ;sortBy - STRARR with structure tags in image structure to sort images by
        ;sortAsc - 0/1 ARR where sort order defined 0=ascending, 1=descending , one element=0 (from old template versions) means all ascending
        ;paramSet - name of paramSet to link to or '' if default
        ;quickTemp - name of quickTemp to link to or '' to default (all selected)
        ;pathApp- path to append results if successfully calculated
        ;archive = 0 if no archiving, 1=archive images in folder Archive after analysis
        ;deleteFiles = 1 if files with no image data should be deleted
        ;deleteFilesEnd = 1 if files exceeding the number of images defined in the corresponding quickTemp should be deleted before archiving (keep those necessary for rerunning the analysis)
        loadTthisVersion=CREATE_STRUCT($
          'path','',$
          'statName','',$
          'dcmCrit',['0000','0000',''],$
          'sortBy', '', $
          'sortAsc',0, $
          'paramSet','', $
          'quickTemp','',$
          'pathApp','',$
          'archive',0,$
          'deleteFiles',0,$
          'deleteFilesEnd',0,$
          'alternative','')
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

function updateRenameTemp, file

  ;default values if missing:
  tags=CREATE_STRUCT('AcqDate', ['0008'x,'0022'x],$
    'AcqTime',['0008'x,'0032'x], $
    'AcqNumber',['0020'x,'0012'x], $
    'SeriesNumber',['0020'x,'0011'x], $
    'SeriesDescription', ['0008'x,'103E'x],$
    'ProtocolName',['0018'x,'1030'x],$
    'ImageNumber',['0020'x,'0013'x],$
    'Modality',['0008'x,'0060'x],$
    'PatientID',['0010'x,'0020'x],$
    'kVp',['0018'x,'0060'x],$
    'mAs', ['0018'x,'1152'x],$
    'SliceThickness', ['0018'x,'0050'x],$
    'Kernel', ['0018'x,'1210'x],$
    'SliceZposition',['0020'x,'1041'x])
  tagFormats=CREATE_STRUCT('AcqDate', '(i0)',$
    'AcqTime','(i06)', $
    'AcqNumber','(i03)', $
    'SeriesNumber','(i03)', $
    'SeriesDescription', '(a0)',$
    'ProtocolName','(a0)',$
    'ImageNumber','(i03)',$
    'Modality', '(a0)',$
    'PatientID', '(a0)',$
    'kVp', '(i03)',$
    'mAs',  '(i03)',$
    'SliceThickness', '(f0.1)',$
    'Kernel',  '(a0)',$
    'SliceZposition', '(f0.1)')
  defTemp=CREATE_STRUCT('cat',['Modality','AcqDate'],'file',['AcqTime','ImageNumber'],'formats',tagFormats)
  temp=CREATE_STRUCT('DEFAULT',defTemp)
  renameTempDef=CREATE_STRUCT('tags',tags,'tagformats',tagFormats,'temp',temp)

  newRenameTemp=-1

  IF file EQ '' THEN newRenameTemp=renameTempDef ELSE BEGIN
    ;find existing values and paste into new structure
    RESTORE, file
    ;errCounter=0
    IF N_ELEMENTS(renameTemp) NE 0 THEN newrenameTemp=renameTemp ELSE newRenameTemp=renameTempDef
    ;changes for later versions
  ENDELSE;file ''

  return, newRenameTemp
end

;tagdescr - verified tag string
;groupElem = ['xxxx','xxxx'] stringarray 4 digit group and element of DICOM tag or finished as UINT
;formatStr ex 'a0' format string without () unverified
function addTagRenameTemp, oldRenameTemp, tagdescr, groupElem, formatStr
  warnings=''

  IF SIZE(groupElem, /TNAME) EQ 'STRING' THEN BEGIN
    GrEl=[UINT(0),UINT(0)]
    READS, groupElem, GrEl, FORMAT='(z)'
    ;el=UINT(0)
    ;READS, groupElem(1), el, FORMAT='(z)'
  ENDIF ELSE GrEl=groupElem

  newtags=CREATE_STRUCT(oldRenameTemp.tags,tagdescr,GrEl)

  ;verify formatStr or set to a0
  a='0'
  Catch, Error_status
  IF Error_status NE 0 THEN BEGIN
    warnings=[warnings,'Format code not valid. Set to a0.']
    formatStr='a0'
    CATCH, /CANCEL
  ENDIF
  b=string(a,FORMAT='('+formatStr+')');cause error?
  formatStr='('+formatStr+')'

  newtagformats=CREATE_STRUCT(oldRenameTemp.tagformats,tagdescr,formatStr)

  ;update all templates with new tag in temp.(i).formats
  updTemps=oldRenameTemp.temp
  nTemp=N_TAGS(oldRenameTemp.temp)
  FOR t=0, nTemp-1 DO BEGIN
    newformats=CREATE_STRUCT(oldRenameTemp.temp.(t).formats,tagdescr,formatStr)
    newtemp=replaceStructStruct(oldRenameTemp.temp.(t),newformats,2)
    updTemps=replaceStructStruct(updTemps,newtemp,t)
  ENDFOR

  newRenameTemp=CREATE_STRUCT('tags', newtags, 'tagformats', newtagformats, 'temp', updTemps)

  return, CREATE_STRUCT('renameTemp',newRenameTemp,'warnings',warnings)
end

function changeTagRenameTemp, oldRenameTemp, oldtagdescr, newtagdescr, groupElem, oldFormatStr, formatStr, changeThisOnly,thisNumb
  warnings=''

  oldtagdescr=STRUPCASE(oldtagdescr)
  newtagdescr=STRUPCASE(newtagdescr)

  tagNames=TAG_NAMES(oldRenameTemp.tags)
  id2change=WHERE(tagNames EQ oldtagdescr)

  IF id2change(0) NE -1 THEN BEGIN
    nTemp=N_TAGS(oldRenameTemp.temp)

    IF SIZE(groupElem, /TNAME) EQ 'STRING' THEN BEGIN
      GrEl=[UINT(0),UINT(0)]
      READS, groupElem, GrEl, FORMAT='(z)'
    ENDIF ELSE GrEl=groupElem

    oldGrEl=oldRenameTemp.tags.(id2change)
    IF ~ARRAY_EQUAL(GrEl,oldGrEl) THEN BEGIN
      ;tag in Use?
      rentempnames=TAG_NAMES(oldRenameTemp.temp)
      inUse=''
      FOR t=0, nTemp-1 DO BEGIN
        matchcat=WHERE(STRUPCASE(oldRenameTemp.temp.(t).cat) EQ oldtagdescr)
        matchfile=WHERE(STRUPCASE(oldRenameTemp.temp.(t).file) EQ oldtagdescr)
        IF matchcat(0) NE -1 OR matchfile(0) NE -1 THEN BEGIN
          inUse=[inUse,rentempnames(t)]
        ENDIF
      ENDFOR

      IF N_ELEMENTS(inUse) GT 1 THEN BEGIN
        if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
        warnings=[warnings,'Tag in use for the following templates:'+STRJOIN(inUse,newline)+newline+'Changing tag number will affect all these templates.']
      ENDIF

    ENDIF

    newtags=replaceStructStruct(oldRenameTemp.tags,GrEl,id2change(0),NEW_TAG_NAME=newtagdescr)

    ;verify formatStr or set to a0
    a='0'
    Catch, Error_status
    IF Error_status NE 0 THEN BEGIN
      warnings=[warnings,'Format code not valid. Set to a0.']
      formatStr='a0'
      CATCH, /CANCEL
    ENDIF
    b=string(a,FORMAT='('+formatStr+')');cause error?

    formatStr='('+formatStr+')'


    ;update default formatstring if change to all, else just change tagname (
    IF changeThisOnly THEN formatStrDef=oldRenameTemp.tagformats.(id2change(0)) ELSE formatStrDef=formatStr
    newtagformats=replaceStructStruct(oldRenameTemp.tagformats,formatStrDef,id2change(0),NEW_TAG_NAME=newtagdescr)

    updTemps=oldRenameTemp.temp
    FOR t=0, nTemp-1 DO BEGIN
      ;change format and tag on format
      IF changeThisOnly EQ 0 THEN formatCodeT=formatStrDef ELSE BEGIN
        formatCodeT=oldRenameTemp.temp.(t).formats.(id2change)
        IF t EQ thisNumb(0) THEN formatCodeT=formatStr
      ENDELSE
      newformats=replaceStructStruct(oldRenameTemp.temp.(t).formats,formatCodeT,id2change,NEW_TAG_NAME=newtagdescr)
      newtemp=replaceStructStruct(oldRenameTemp.temp.(t),newformats,2)

      ;update tagname in templatestringarrays if name changed
      IF oldtagdescr NE newtagdescr THEN BEGIN
        catTags=STRUPCASE(oldRenameTemp.temp.(t).cat)
        IF catTags.HasValue(oldtagdescr) THEN BEGIN
          strId=WHERE(catTags EQ oldtagdescr)
          newtemp.cat(strId(0))=newtagdescr
        ENDIF
        fileTags=STRUPCASE(oldRenameTemp.temp.(t).file)
        IF fileTags.HasValue(oldtagdescr) THEN BEGIN
          strId=WHERE(fileTags EQ oldtagdescr)
          newtemp.file(strId(0))=newtagdescr
        ENDIF
      ENDIF

      updTemps=replaceStructStruct(updTemps,newtemp,t)

    ENDFOR

    IF thisNumb(0) EQ -1 AND changeThisOnly THEN warnings=[warnings,'No template selected. Format code did not change.']

    newRenameTemp=CREATE_STRUCT('tags', newtags, 'tagformats', newtagformats, 'temp', updTemps)
  ENDIF ELSE BEGIN
    warnings=[warnings,'An error occured when editing. No change applied.']
    newRenameTemp=oldRenameTemp
  ENDELSE

  return, CREATE_STRUCT('renameTemp',newRenameTemp,'warnings',warnings)
end

;with .ALT = -1 similar structure to renametemp with .tags and .tagformats
;groupElem = ['xxxx','xxxx'] stringarray 4 digit group and element of DICOM tag or finished as UINT
;formatStr ex 'a0' format string without () unverified
function addTagOutputTempMinOne, oldMinOneTemp, tagdescr, groupElem, formatStr
  warnings=''

  IF SIZE(groupElem, /TNAME) EQ 'STRING' THEN BEGIN
    GrEl=[UINT(0),UINT(0)]
    READS, groupElem, GrEl, FORMAT='(z)'
  ENDIF ELSE GrEl=groupElem

  IF SIZE(oldMinOneTemp, /TNAME) EQ 'STRUCT' THEN newtags=CREATE_STRUCT(oldMinOneTemp.tags,tagdescr,GrEl) ELSE newtags=CREATE_STRUCT(tagdescr,GrEl)

  ;verify formatStr or set to a0
  a='0'
  Catch, Error_status
  IF Error_status NE 0 THEN BEGIN
    warnings=[warnings,'Format code not valid. Set to a0.']
    formatStr='a0'
    CATCH, /CANCEL
  ENDIF
  b=string(a,FORMAT='('+formatStr+')');cause error?
  formatStr='('+formatStr+')'

  IF SIZE(oldMinOneTemp, /TNAME) EQ 'STRUCT' THEN newtagformats=CREATE_STRUCT(oldMinOneTemp.tagformats,tagdescr,formatStr) ELSE newtagformats=CREATE_STRUCT(tagdescr,formatStr)

  newTemp=CREATE_STRUCT('ALT', -1, 'tags', newtags, 'tagformats', newtagformats)

  return, CREATE_STRUCT('newTemp',newTemp,'warnings',warnings)
end

;with .ALT = -1 similar structure to renametemp with .tags and .tagformats
;groupElem = ['xxxx','xxxx'] stringarray 4 digit group and element of DICOM tag or finished as UINT
function changeTagOutputTempMinOne, oldMinOneTemp, tagdescr, groupElem, formatStr, nmbChange
  warnings=''

  IF SIZE(groupElem, /TNAME) EQ 'STRING' THEN BEGIN
    GrEl=[UINT(0),UINT(0)]
    READS, groupElem, GrEl, FORMAT='(z)'
  ENDIF ELSE GrEl=groupElem

  oldStruct=oldMinOneTemp.tags.(nmbChange)
  names=TAG_NAMES(oldMinOneTemp.tags)
  oldDesc=names(nmbChange)

  IF tagdescr NE oldDesc THEN BEGIN
    idEq=WHERE(names EQ tagdescr)
    IF idEq(0) NE -1 THEN BEGIN
      tagdescr=oldDesc
      warnings=[warnings,'Tagname already exist. Could not change name.']
    ENDIF
  ENDIF

  newtags=replaceStructStruct(oldMinOneTemp.tags, GrEl, nmbChange, NEW_TAG_NAME=tagdescr)

  ;verify formatStr or set to a0
  a='0'
  Catch, Error_status
  IF Error_status NE 0 THEN BEGIN
    warnings=[warnings,'Format code not valid. Set to a0.']
    formatStr='a0'
    CATCH, /CANCEL
  ENDIF
  b=string(a,FORMAT='('+formatStr+')');cause error?
  formatStr='('+formatStr+')'

  newtagformats=replaceStructStruct(oldMinOneTemp.tagformats,formatStr,nmbChange, NEW_TAG_NAME=tagdescr)

  newTemp=CREATE_STRUCT('ALT', -1, 'tags', newtags, 'tagformats', newtagformats)

  return, CREATE_STRUCT('newTemp',newTemp,'warnings',warnings)
end

;for settings.pro when setting extra DICOM output with Alt=-1
function getOutNumbMinOne, tempStructIn, testname
  outputNmbMin1=-1
  expTestNo=-1
  IF SIZE(tempStructIn, /TNAME) EQ 'STRUCT' THEN BEGIN
    availTests=TAG_NAMES(tempStructIn)
    expTestNo=WHERE(availTests EQ testname); find test number where DICOM output is
    IF expTestNo(0) NE -1 THEN BEGIN
      IF SIZE(tempStructIn.(expTestNo(0)), /TNAME) EQ 'STRUCT' THEN BEGIN ; test could have -1 only if none defined
        outputNames=TAG_NAMES(tempStructIn.(expTestNo(0)))
        FOR i=0, N_ELEMENTS(outputNames)-1 DO BEGIN
          IF tempStructIn.(expTestNo(0)).(i).ALT EQ -1 THEN BEGIN
            outputNmbMin1=i
            BREAK
          ENDIF
        ENDFOR
      ENDIF
    ENDIF
  ENDIF
  RETURN, [expTestNo(0),outputNmbMin1]
end

;previously saved .dat-file might miss some newly introduced parameters. Update to avoid crashes and to update current Path (.filename) if replaced since created
;called by struc='',pathNow ='' gives default, empty structure to be able to find list of available tags
function imgStructUpdate, struc, pathNow
  updatedStruct=-1
  szStru=SIZE(struc, /TNAME)
  IF szStru EQ 'STRUCT' THEN tnLoaded=TAG_NAMES(struc) ELSE tnLoaded=''
  IF tnLoaded.HasValue('FILENAME') OR tnLoaded(0) EQ '' THEN BEGIN
    currStruct=CREATE_STRUCT('filename',pathNow,'studydatetime','','acqDate', '', 'imgDate', '', 'institution','','manufacturer','','modality', '', 'modelName','','stationName','','SWversion','','detectorID','',$
      'patientName','', 'patientID', '', 'patientWeight', '-', 'imageType','','presType','','studyDescr','','seriesName','', 'protocolname', '',$
      'seriesNmb',-1,'seriesTime','','seriesUID','','acqNmb',-1, 'acqtime','','sliceThick',-1., 'pix', [-1.,-1.],'imageSize',[-1,-1],'kVp',-1.,'FOV',-1.,'rekonFOV',-1.,'mA',-1.,'mAs',-1.,'ExpTime',-1.,'coll',[-1.,-1.],'pitch',-1.,$
      'ExModType','','CTDIvol',-1.,'focalSpotSz',-1.,'DAP',-1.,'EI',-1.,'sensitivity',-1.,'sdd',-1.,'filterAddOn','-','kernel','-',$
      'zpos', -999., 'imgNo',-1,'nFrames',0,'wCenter',-1,'wWidth',-1,$
      'collType','-','nEWindows',-1,'EWindowName','-','zoomFactor','-','radius1',-1.,'radius2',-1.,'detectorVector','-','angle',-999.,'acqFrameDuration',-1.,'acqTerminationCond','-',$
      'units','-','radiopharmaca','-','admDose','-','admDoseTime','-','reconMethod','-','attCorrMethod','-','scaCorrMethod','-', 'scatterFrac','-',$
      'imgFreq',-1.,'MRacqType','-','MRscanSeq','-','MRseqVariant','-','TR',-1.,'TE',-1.,'NSA',-1.,'flipAng',-1.,'spaceSlice',-1.,'recCoilName','-','traCoilName','-',$
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
    'institution','Institution','manufacturer','Manufacturer','modality', 'Modality', 'modelName','Equipment Model Name','stationName','Station Name','SWversion','Software version',$
    'detectorID','Detector ID',$
    'patientName','Patient Name', 'patientID', 'Patient ID', 'patientWeight', 'Patient Weight', 'imageType','Image Type','presType','Presentation type',$
    'studyDescr','Study Description','seriesName','Series Name', 'protocolname', 'Protocol Name',$
    'seriesNmb','Series Number','seriesTime','Series Time','seriesUID','Series UID','acqNmb','Acquisition Number', 'acqtime','Acquisition Time',$
    'sliceThick','Slice Thickness', 'pix', 'Pixel size','imageSize','Image Size','kVp','kVp','FOV','FOV','rekonFOV','Reconstruction FOV','mA','mA','mAs','mAs',$
    'ExpTime','Exposure Time','coll','Collimation','pitch','Pitch',$
    'ExModType','Exposure Modulation Type','CTDIvol','CTDI vol','focalSpotSz','Focal Spot size (mm)','DAP','DAP','EI','Exposure Index','sensitivity','Sensitivity','sdd','Source Detector Distance',$
    'filterAddOn','Filter AddOn','kernel','Reconstruction kernel',$
    'zpos', 'Z position', 'imgNo','Image Number','nFrames','Number Of Frames','wCenter','Window Center','wWidth','Window Width',$
    'collType','Collimator Type','nEWindows','Number of Energy Windows','EWindowName','Energy Window Name','zoomFactor','Zoom Factor','radius1','Radius Detector 1','radius2','Radius Detector 2','detectorVector','Detector Number',$
    'angle','Image Angle','acqFrameDuration','Acquisition Frame Duration','acqTerminationCond','Acquisition Termination Condition',$
    'units','Units For Pixel Values','radiopharmaca','Radiopharmaca','admDose','Administered Activity (MBq)','admDoseTime','Administered Activity Time',$
    'reconMethod','Reconstruction Method','attCorrMethod','Attenuation Correction Method','scaCorrMethod','Scatter Correction Method', 'scatterFrac','Scatter Fraction',$
    'imgFreq','Imaging Frequency (MHz)','MRacqType','MR Acquisition Type','MRscanSeq','MR Scanning Sequence','MRseqVariant','MR Sequence Variant',$
    'TR','TR (Repetition Time)','TE','TE (Echo Time)','NSA','Number Of Averages','flipAng','Flip Angle','spaceSlice','Spacing Between Slices','recCoilName','Receive Coil Name','traCoilName','Transmit Coil Name',$
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
    'BOOL': IF val EQ 0 THEN strFormatCode='("false ", i0)' ELSE strFormatCode='("true ", i0)'
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

;For RenameDICOM
;path = file or folder address
;foler = 1 if path is a folder or 0 if path is a file, -1 if file and output is strarray, not filename
;elemArr = string array with elements to pick from tagStruct
;tagStruct = structure with dicom elements [group, element]
;formatsArr = string array with format strings without () e.g. ['a0','f0.3']
function newFileName, path, folder, elemArr, tagStruct, formatsArr
  newpath='_'

  IF folder EQ 1 THEN BEGIN
    Spawn, 'dir '  + '"'+path+'"' + '*'+ '/b /a-d', res; files only

    IF res(0) NE '' THEN BEGIN;find first dcm file and extract info from this header
      res=path+res(sort(res))
      nn=N_ELEMENTS(res)
      dcm=-1
      counter=0
      FOR n=0, nn-1 DO BEGIN
        dcm=QUERY_DICOM(res(n))
        IF dcm EQ 1 THEN path=res(n)
        IF dcm EQ 1 THEN BREAK ELSE counter=counter+1
      ENDFOR
    ENDIF ELSE dcm=-1;no files found
  ENDIF ELSE dcm=QUERY_DICOM(path)

  IF dcm EQ 1 THEN BEGIN
    o=obj_new('idlffdicom')
    t=o->read(path)
    nameArr=!Null

    test=o->GetReference('0008'x,'0060'x)
    test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
    modality=*(test_peker[0])
    IF modality EQ 'SR' THEN BEGIN
      nameArr='RDSR'
    ENDIF ELSE BEGIN
      nElem=N_ELEMENTS(elemArr)
      desc=TAG_NAMES(tagStruct)
      
      FOR ee=0, nElem-1 DO BEGIN
        ide=WHERE(desc EQ STRUPCASE(elemArr(ee)))
        ide=ide(0)
        thisTag=tagStruct.(ide)
        test=o->GetReference(thisTag(0),thisTag(1))
        notFound=1
        IF test(0) NE -1 THEN BEGIN
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
  
          stest=size(test_peker, /TNAME)
          IF stest EQ 'POINTER' THEN BEGIN
            namePart=*(test_peker[0])
  
            nameParts=STRSPLIT(namePart(0),'\',/EXTRACT)
            formats=STRSPLIT(STRMID(formatsArr.(ide), 1, strlen(formatsArr.(ide))-2),'\',/EXTRACT)
            nP=N_ELEMENTS(nameParts)
            IF nP EQ N_ELEMENTS(formats) AND nP GT 1 THEN BEGIN
              namePartArr=!Null
              FOR p=0, nP-1 DO BEGIN
                IF formats(p) NE '_' THEN namePartArr=[namePartArr,STRING(nameParts(p), FORMAT='('+formats(p)+')')]
              ENDFOR
              namePart=STRJOIN(namePartArr,'_')
            ENDIF ELSE namePart=STRING(nameParts(0),FORMAT=formatsArr.(ide))
  
            nameArr=[nameArr,namePart]
            notFound=0
          ENDIF
  
        ENDIF
        IF notFound and folder EQ -1 THEN nameArr=[nameArr,'- not found -']
      ENDFOR
    ENDELSE

    obj_destroy,o

    IF N_ELEMENTS(nameArr) GT 0 THEN BEGIN
      IF folder EQ 1 THEN nameArr=IDL_VALIDNAME(nameArr,/CONVERT_ALL)
      IF folder EQ -1 THEN BEGIN;array of file elements out if folder =-1
        newpath=nameArr
        FOREACH elem, newpath, idx DO newpath(idx)=elem.replace('*','X')
      ENDIF ELSE BEGIN
        nameStr=STRJOIN(nameArr,'_')
        IF folder EQ 1 THEN nameStr=STRJOIN(STRSPLIT(nameStr,'_',/EXTRACT),'_') ;remove all multiple and first _

        arr=STRSPLIT(path,'\',/EXTRACT)
        last=n_elements(arr)-1
        IF folder EQ 1 THEN newpath=STRJOIN(arr[0:last-2],'\')+'\'+nameStr+'\' ELSE BEGIN
          nameStr=nameStr.replace('*','X');change * to X (format code not ideal return *)
          nameStr=nameStr.replace(' ','_')
          nameStr=STRJOIN(STRSPLIT(nameStr,'[-/\?#%&{}`<>$!:@+|=]',/EXTRACT, /REGEX),'_')

          newpath=STRJOIN(arr[0:last-1],'\')+'\'+nameStr+'.dcm'
        ENDELSE
      ENDELSE
    ENDIF
    IF newpath EQ '' THEN newpath='_'
  ENDIF

  return, newpath
end

; return list of filenames for open files
; struc = structure of structures from readCT.pro
; full = 0 for only parentfolder\filename, =1 for full path
; marked = array of indexes for marked files, -1 means none is marked
;   full=1 returns only marked, full=0 returns all and set an X on the marked
; mMulti = multiMark array, -1 means no multimarking (only X)
function getListOpenFiles, struc, full, marked, mMulti, RENAMEDICOM=RDname, CONFIGPATH=cPath, PARENT=parent
  tags=TAG_NAMES(struc)
  fileList=''
  IF tags(0) NE 'EMPTY' THEN BEGIN
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

      RD=0
      currRDtemp=!Null
      RDtags=!Null
      IF N_ELEMENTS(RDname) NE 0 THEN BEGIN
        IF RDname NE '' THEN BEGIN
          IF FILE_TEST(cPath, /READ) THEN BEGIN
            RESTORE, cPath
            currNames=TAG_NAMES(renameTemp.(2))
            IF currNames.HasValue(RDname) THEN BEGIN
              RD=1
              idTemp=WHERE(currNames EQ RDname)
              currRDtemp=renameTemp.(2).(idTemp(0))
              RDtags=renameTemp.tags
            ENDIF ELSE BEGIN
              sv=DIALOG_MESSAGE('Could not fine RenameDICOM template called '+RDname+'. Consider creating this template within File->RenameDICOM.', /INFORMATION, DIALOG_PARENT=parent)
            ENDELSE
          ENDIF
        ENDIF
      ENDIF

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

        midStr=''
        IF RD EQ 0 THEN BEGIN;show filename
          splitAdr=STRSPLIT(struc.(i).filename,'\',/EXTRACT)
          nSplit=N_ELeMENTS(splitAdr)
          midStr=STRJOIN(splitAdr[nSplit-2:nSplit-1],'\')
        ENDIF ELSE BEGIN;show RenameDICOM string for input template name
          formatCurr=currRDtemp.formats
          fileTemp=currRDtemp.file
          IF fileTemp(0) NE '' THEN midStr=newFileName(struc.(i).filename, 0, fileTemp, RDtags, formatCurr) ELSE midStr=''
          IF midStr EQ '' THEN BEGIN
            splitAdr=STRSPLIT(struc.(i).filename,'\',/EXTRACT)
            nSplit=N_ELeMENTS(splitAdr)
            midStr=STRJOIN(splitAdr[nSplit-2:nSplit-1],'\')
          ENDIF ELSE BEGIN
            midStr=STRSPLIT(midStr,'\',/EXTRACT)
            midStr=STRMID(midStr[-1],0,STRLEN(midStr[-1])-4);remove folder and .dcm
          ENDELSE
        ENDELSE

        endStr=''
        IF struc.(i).nFrames GT 1 THEN BEGIN
          IF struc.(i).zpos[0] NE -999. AND struc.(i).slicethick GT 0. THEN endStr='  zpos '+STRING(struc.(i).zpos,FORMAT='(f0.3)')
          IF struc.(i).angle[0] NE -999. THEN endStr='  angle '+STRING(struc.(i).zpos,FORMAT='(f0.3)')
          IF endStr EQ '' THEN endStr='  frame '+STRING(struc.(i).frameNo,FORMAT='(i0)')
        ENDIF

        fileList(i)=add+midStr+endStr

      ENDFOR
    ENDELSE
  ENDIF
  return, fileList
end;getListOpenFilse

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
;arr_isinfo - imgStructInfo array see set_Values.pro
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

;imgPath - path for image to be read
;tagStruct .alt = -1, .tags = structure with uint [group, elem], .tagformats = structure with formatcodes corresponding to defined .tags
function getFormatedDICOMtags, imgPath, tagStruct
  strTags='_'
  proceed=1
  IF SIZE(tagStruct, /TNAME) EQ 'STRUCT' THEN BEGIN
    IF ~ARRAY_EQUAL(TAG_NAMES(tagStruct),['ALT','TAGS','TAGFORMATS']) THEN proceed=0
  ENDIF
  proceed=QUERY_DICOM(imgPath);readable DICOM?

  IF proceed THEN BEGIN
    o=obj_new('idlffdicom')
    t=o->read(imgpath)
    ntags=N_TAGS(tagStruct.TAGS)
    strTags=STRARR(ntags)
    FOR i=0, ntags-1 DO BEGIN
      thisTag=tagStruct.TAGS.(i)
      test=o->GetReference(thisTag(0),thisTag(1))
      IF test(0) NE -1 THEN BEGIN
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)

        stest=size(test_peker, /TNAME)
        IF stest EQ 'POINTER' THEN BEGIN
          strPart=*(test_peker[0])

          strParts=STRSPLIT(strPart(0),'\',/EXTRACT)
          formats=STRSPLIT(STRMID(tagStruct.TAGFORMATS.(i), 1, strlen(tagStruct.TAGFORMATS.(i))-2),'\',/EXTRACT)
          nP=N_ELEMENTS(strParts)
          IF nP EQ N_ELEMENTS(formats) AND nP GT 1 THEN BEGIN
            strPartArr=!Null
            FOR p=0, nP-1 DO BEGIN
              IF formats(p) NE '_' THEN strPartArr=[strPartArr,STRING(strParts(p), FORMAT='('+formats(p)+')')]
            ENDFOR
            strPart=STRJOIN(strPartArr,'_')
          ENDIF ELSE strPart=STRING(strParts(0),FORMAT=tagStruct.TAGFORMATS.(i))
          IF strPart EQ '' THEN strPart='_'
          strTags(i)=strPart
        ENDIF
      ENDIF
    ENDFOR
  ENDIF;proceed
  return, strTags
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

;img = 2d array with values to fit
;roiMatrix = 2d array with nonzero where part of img should be fit
;pix = pixelsize in mm
function fitDistPointSource, img, roiMatrix, pix, radi, posfit, radfit

  ;find ROI part
  temp=TOTAL(roiMatrix,2)
  temp2=WHERE(temp GT 0)
  firstX=temp2(0)
  lastX=temp2(-1)
  temp=TOTAL(roiMatrix,1)
  temp2=WHERE(temp GT 0)
  firstY=temp2(0)
  lastY=temp2(-1)

  imgSz=SIZE(img,/DIMENSIONS)

  ;estimate centerposition (max) in image?
  IF posfit(0) EQ 0 THEN BEGIN;x dir
    profX=TOTAL(img[firstX:lastX,firstY:lastY],2)
    xx=FINDGEN(lastX-firstX+1)
    cc=POLY_FIT(xx,profX,2)
    topPos=-.5*cc(1)/cc(2); x=-b/2a
    fitX=topPos+firstX+0.5;zero starting at border
  ENDIF ELSE fitX=imgSz(0)/2

  IF posfit(1) EQ 0 THEN BEGIN;y dir
    profY=TOTAL(img[firstX:lastX,firstY:lastY],1)
    xx=FINDGEN(lastY-firstY+1)
    cc=POLY_FIT(xx,profY,2)
    topPos=-.5*cc(1)/cc(2); x=-b/2a)
    fitY=topPos+firstY+0.5
  ENDIF ELSE fitY=imgSz(1)/2

  fitPos=ROUND([fitX,fitY])
  centerVal=MEAN(img[fitPos(0)-3:fitPos(0)+3,fitPos(1)-3:fitPos(1)+3]);mean of center pixel and its neighbours
  fitPos=[fitX,fitY]

  ;calculate distance from center in image plane within ROI, sort pixels by distance and fit
  roiSz=[lastX-firstX+1, lastY-firstY+1]
  distCenter=FLTARR(imgSz);pixel distance from center

  FOR i=0, imgSz(0)-1 DO BEGIN
    FOR j=0, imgSz(1)-1  DO distCenter(i,j)=SQRT(((i+0.5)-fitPos(0))^2+((j+0.5)-fitPos(1))^2)
  ENDFOR
  distCenter=pix*distCenter
  distsInROI=distCenter[firstX:lastX,firstY:lastY]
  sortDist=SORT(distsInROI)
  dists=distsInROI(sortDist)
  smImg=SMOOTH(img,3)
  imgInROI=smImg[firstX:lastX,firstY:lastY];smooth before interpolating and fitting data
  sortImg=imgInROI(sortDist)
  newDists=FINDGEN((lastX-firstX+1)/2)*pix;resample to pixelsize
  newSortImg=INTERPOL(sortImg,dists,newDists)
  newSortImg(0)=newSortImg(1);avoid Inf

  A=[centerVal*(MEAN(radi)^2),MEAN(radi)];firstGuess I0, R0
  fita=[1,1]
  IF radfit GT 0 THEN BEGIN
    A=[centerVal*(MEAN(radfit)^2),MEAN(radfit)];firstGuess I0, R0
    fita=[1,0]
  ENDIF

  ;equation A6 from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3966082/#x0 (doi:  10.1118/1.3125642)
  yfit = CURVEFIT(newDists, newSortImg, newDists*0+1., A, FITA=fita, FUNCTION_NAME='pointSourceFit', ITER=iter, CHISQ=chisq);pointSourceFit in a3_fitFunctions.pro

  fitData=CREATE_STRUCT('x',newDists,'y',newSortImg,'yfit',yfit)

  ;corr=(distCenter^2+A[1]^2)^(1.5)
  ;corr=(1./min(corr))*corr; matrix to multiply by

  fitM=A(0)*A(1)/[(distCenter^2+A[1]^2)^(1.5)]
  corrSub=fitM-yfit(0);matrix to subtract
  corr=(1./max(fitM))*fitM
  corr=1./corr; matrix to multiply by

  fitDist=A[1]
  fitPos=pix*(fitPos-imgSz/2);mm from center

  return, CREATE_STRUCT('corr',corr, 'corrSub',corrSub,'fitDist',fitDist, 'fitPos', fitPos,'fitData',fitData)
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

;upscale image to view each pixel as nxn pixels
function upscaleImg, img, nZoom
  szImg=SIZE(img, /DIMENSIONS)
  nZoom=LONG(nZoom)
  upscaledImg=FLTARR(szImg*nZoom)
  FOR i=0, szImg(0)-1 DO BEGIN
    FOR j=0, szImg(1)-1 DO BEGIN
      upscaledImg[i*nZoom:i*nZoom+nZoom-1,j*nZoom:j*nZoom+nZoom-1]=img(i,j)
    ENDFOR
  ENDFOR
  return, upscaledImg
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

function get_center_rod, subimg

  filtimg=MEDIAN(subimg,3)
  minimg=MIN(filtimg)
  maximg=MAX(filtimg)
  halfmax=0.5*(minimg+maximg)
  center=centroid(subimg, halfmax)

  return, center
end

;find rods in module CTP404
function get_dim, img, imgCenter, pix

  margin=10; radius in mm

  rodCenter=FLTARR(2,4); clockwise starting with upper left
  mShort=ROUND((25-margin)/pix(0))
  mLong=ROUND((25+margin)/pix(0))
  imSz=SIZE(img, /DIMENSIONS)
  IF imgCenter(0)-mLong GE 0 AND imgCenter(0)+mLong LT imSz(0) AND imgCenter(1)-mLong GE 0 AND imgCenter(1)+mLong LT imSz(1) THEN BEGIN
    rodCenter[*,0]=get_center_rod(img[imgCenter(0)-mLong:imgCenter(0)-mShort,imgCenter(1)+mShort:imgCenter(1)+mLong]);upper left
    rodCenter[*,1]=get_center_rod(img[imgCenter(0)+mShort:imgCenter(0)+mLong,imgCenter(1)+mShort:imgCenter(1)+mLong]);upper right
    rodCenter[*,2]=get_center_rod(img[imgCenter(0)+mShort:imgCenter(0)+mLong,imgCenter(1)-mLong:imgCenter(1)-mShort]);lower right
    rodCenter[*,3]=get_center_rod(img[imgCenter(0)-mLong:imgCenter(0)-mShort,imgCenter(1)-mLong:imgCenter(1)-mShort]); lower left
  ENDIF ELSE rodCenter=rodCenter-1

  IF MIN(rodCenter) EQ -1 THEN status=0 ELSE status=1

  rodCenter[0,0]=(-mLong+rodCenter[0,0])*pix(0)
  rodCenter[1,0]=(mShort+rodCenter[1,0])*pix(0)

  rodCenter[0,1]=(mShort+rodCenter[0,1])*pix(0)
  rodCenter[1,1]=(mShort+rodCenter[1,1])*pix(0)

  rodCenter[0,2]=(mShort+rodCenter[0,2])*pix(0)
  rodCenter[1,2]=(-mLong+rodCenter[1,2])*pix(0)

  rodCenter[0,3]=(-mLong+rodCenter[0,3])*pix(0)
  rodCenter[1,3]=(-mLong+rodCenter[1,3])*pix(0)

  resArr=FLTARR(6)
  resArr=[SQRT((rodCenter[0,1]-rodCenter[0,0])^2+(rodCenter[1,1]-rodCenter[1,0])^2),$
    SQRT((rodCenter[0,2]-rodCenter[0,3])^2+(rodCenter[1,2]-rodCenter[1,3])^2),$
    SQRT((rodCenter[0,3]-rodCenter[0,0])^2+(rodCenter[1,3]-rodCenter[1,0])^2),$
    SQRT((rodCenter[0,1]-rodCenter[0,2])^2+(rodCenter[1,1]-rodCenter[1,2])^2),$
    SQRT((rodCenter[0,2]-rodCenter[0,0])^2+(rodCenter[1,2]-rodCenter[1,0])^2),$
    SQRT((rodCenter[0,1]-rodCenter[0,3])^2+(rodCenter[1,1]-rodCenter[1,3])^2)]

  return, CREATE_STRUCT('dists',resArr,'centers',rodCenter, 'status', status)

end

;from psf.pro & CALCULATE_LSF_LIST.pro developed at DNR (Oslo, Norway) by Arne Skretting, Wibeke Nordhoy, Alise Larsen and Kristine Eldevik
;modified by Ellen Wasbo 2015 (Stavanger University Hospital, Norway)
;modification related to user interface to incorporate code into ImageQC

function get_fwhm, imageIn, center, pixel

  szImg=SIZE(imageIn, /DIMENSIONS)

  ;modified from psf.pro & CALCULATE_LSF_LIST.pro
  region    = imageIn[center(0)-15:center(0)+15, center(1)-5:center(1)+4]             ; omr�det rundt kulen, 31 piksler i x-retning, 10 piksler i y-retning
  unitv   = REPLICATE(1.,10)                      ; array, 10 elementer med verdien 1
  profile   = region # unitv                      ; array, 31 elementer med summen av de 10 y-verdiene for hver piksel i x-retning

  centerB=center
  centerB(1)=center(1)+ROUND(szImg(1)/4)

  regionB   = imageIn[centerB(0)-15:centerB(0)+15, centerB(1)-5:centerB(1)+4]          ; bakgrunn med samme form og st�rrelse som signalet
  backgroundv = regionB # unitv                       ; summerer bakgrunnen slik som signalet
  meanbackgr  = MEAN(backgroundv)                     ; finner gjennomsnittlig verdi av bakgrunnen
  background  = REPLICATE(meanbackgr, N_ELEMENTS(profile))        ; 1d-array med gjennomsnittlig bakgrunnsverdi i alle 31 elementene
  profileC  = profile - background                    ; profilen etter at bakgrunnen er subtrahert

  maxvalue  = MAX(profileC, address)                  ; finner maks-verdien i profilen
  profileN  = profileC / maxvalue                     ; normaliserer profilen til maxverdi av signalet

  profile2  = profileN > 0                        ; fjerner elementene i profilen med verdi lavere enn 0
  test    = MAX(profile2, pos)
  grad    = profile2 - SHIFT(profile2, 1)
  nupper    = pos
  nlower    = pos

  FOR n = pos+1, N_ELEMENTS(profile2)-1   DO BEGIN
    IF grad[n] GT 0.  THEN GOTO, upperset
    nupper  = n
  ENDFOR

  upperset:
  FOR m = 1, pos-1  DO BEGIN
    IF grad[pos-m] LT 0.  THEN GOTO, lowerset
    nlower  = pos - m
  ENDFOR

  lowerset:
  psf_pixel = profile2[nlower:nupper]                 ; reell PSF i bildet
  npoints   = nupper - nlower + 1                   ; antall piksler i PSF
  cum     = FLTARR(npoints)                     ; array for kumulativ funksjon
  cum[0]    = psf_pixel[0]

  FOR n=1, npoints-1  DO cum[n] = cum[n-1] + psf_pixel[n]         ; kumulativ funksjon, der hvert element = summen av tidligere elementer

  back      = cum[0]                      ; navngir f�rste element i kumulativ funksjon
  max_cum     = MAX(cum, index)                 ; finner maksverdien og dens plassering i kumulativ funksjon
  relative    = FLOAT(cum) / max_cum                ; normaliserer funksjonen i forhold til maksverdien
  normal      = relative[0:index]                 ; normalisert funksjon opp til maksverdien

  x = FLTARR(100)
  FOR i=0, index-1  DO BEGIN
    x[i] = GAUSS_CVF(normal[i])                  ; finner 'cut-off value' for hvert element
  ENDFOR

  utvalg      = WHERE(ABS(x) LT 1.9 AND x NE 0.0)         ; finner indeksene der |x|<1.9 og ikke 0
  IF N_ELEMENTS(utvalg) GT 2 THEN BEGIN ; probably higher than 2 needed - adjust if crashes
    y = x[utvalg]
    psf_ny      = psf_pixel[utvalg]
    normal_utvalg = normal[utvalg]

    y = -y
    test      = MAX(y, maxpos)

    no        = N_ELEMENTS(utvalg)
    xx        = FLTARR(1, no)
    xx(0,*)     = FINDGEN(no)

    a0 = REGRESS(xx, y, YFIT=yfit, CONST=const, MCORRELATION=mcorr)
    scale = 1 / a0[0]                         ; scale er et standardavvik

    sigma_korr = SQRT((scale^2)-((0.28^2)/5.))              ; korrigerer for endelig kulest�rrelse

    FWHM      = 2 * SQRT(2. * ALOG(2)) * scale * pixel
    FWHM_korr   = 2 * SQRT(2. * ALOG(2)) * sigma_korr * pixel

    res=[FWHM, FWHM_korr, mcorr]


  ENDIF ELSE res=-1

  return, res
end

;-----------------search for new GE_QAP files excluding folder Archive and including other subfolders-----------
function findNewGE_QAP_files, inputPath
  filesFound=''
  sep=PATH_SEP()
  Spawn, 'dir '  + '"'+inputPath(0)+'"' + ' /b /a:D', dirTempTemp
  IF dirTempTemp(0) NE '' THEN BEGIN
    FOR dd=0, N_ELEMENTS(dirTempTemp)-1 DO BEGIN
      IF dirTempTemp(dd) NE 'Archive' THEN BEGIN
        Spawn, 'dir '  + '"'+inputPath(0)+dirTempTemp(dd)+sep+'"' + '*.txt'+ ' /b', adrThisSub
        IF adrThisSub(0) NE '' THEN BEGIN
          filesFound=[filesFound,inputPath(0)+dirTempTemp(dd)+sep+adrThisSub]
        ENDIF
      ENDIF
    ENDFOR
    Spawn, 'dir '  + '"'+inputPath(0)+'"' + '*.txt'+ ' /b', adrDirect
    IF adrDirect(0) NE '' THEN BEGIN
      filesFound=[filesFound,inputPath(0)+adrDirect]
    ENDIF
    IF N_ELEMENTS(filesFound) GT 1 THEN filesFound=filesFound[1:-1];remove first empty ''
  ENDIF
  return, filesFound
end
