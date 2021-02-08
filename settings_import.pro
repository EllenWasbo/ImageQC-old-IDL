;procedures for importing templates from another config-path. Used in settings.pro

;import parameter set(s)
pro import_s, source_path, target_path, xo, yo, parent, NAMED_SETS=named_sets
  impConfigS=updateConfigS(source_path)

  IF SIZE(impConfigS, /TNAME) EQ 'STRUCT' THEN BEGIN
    names=TAG_NAMES(impConfigS)
    names=names[1:-1]

    IDs2import=!Null
    IF N_ELEMENTS(named_sets) GT 0 THEN BEGIN
      FOR i=0, N_ELEMENTS(named_sets)-1 DO BEGIN
        id=WHERE(names EQ named_sets(i))
        IF id(0) NE -1 THEN IDs2import=[IDs2import, id(0)]
      ENDFOR
    ENDIF ELSE BEGIN
      box=[$
        '1, BASE,, /COLUMN', $
        '0, LABEL, Parameter sets found in selected config file', $
        '0, LABEL, Select set(s) to import', $
        '0, LABEL, ',$
        '2, LIST, ' + STRJOIN(names,'|') + ', TAG=templates', $
        '1, BASE,, /ROW', $
        '0, BUTTON, Cancel, QUIT, TAG=Cancel',$
        '2, BUTTON, OK, QUIT, TAG=OK']
      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select parameter sets to import', XSIZE=300, YSIZE=300, FOCUSNO=3, XOFFSET=xo+250, YOFFSET=yo+250)

      IF res.OK THEN BEGIN
        IF N_ELEMENTS(res.templates) NE 0 THEN IDs2import=res.templates
      ENDIF
    ENDELSE

    IF N_ELEMENTS(IDs2import) GT 0 THEN BEGIN
      RESTORE, target_path
      currNames=TAG_NAMES(configS)
      updSet=0

      FOR i=0, N_ELEMENTS(IDs2import)-1 DO BEGIN
        newname=names(IDs2import(i))
        IF currNames.HasValue(newname) THEN BEGIN
          ;ask for new name or replace
          box=[$
            '1, BASE,, /COLUMN', $
            '0, LABEL, Parameter set '+newname+' already exist', $
            '2,  TEXT, , LABEL_LEFT=Rename the imported parameter set:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, OK, QUIT, TAG=Save',$
            '0, BUTTON, Overwrite, QUIT, TAG=Overwrite',$
            '2, BUTTON, Cancel/Ignore, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename the imported parameter set', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xo+250, YOFFSET=yo+250)

          IF res.Cancel THEN newname=''
          IF res.Save THEN BEGIN
            IF res.newname NE '' THEN newname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL)) ELSE newname=''
          ENDIF
          IF res.Overwrite THEN BEGIN
            idReplace=WHERE(currNames EQ newname)
            idSource=WHERE(names EQ newname)+1

            ;check output-templates connected
            ots=impConfigS.(impConfigS.(idSource)).QTOUTTEMPS
            def_ots=WHERE(ots NE 'DEFAULT')
            IF def_ots(0) NE -1 THEN BEGIN
              newline = string([13B, 10B])
              sv=DIALOG_MESSAGE('The parameter set '+newname+' have links to these QuickTest Output templates '+newline+STRJOIN(ots,newline)+newline+'These will all be set to DEFAULT. Consider importing output templates and reconnect the paramater set.', /INFORMATION, DIALOG_PARENT=parent)
              FOREACH elem, impConfigS.(idSource).QTOUTTEMPS, o DO impConfigS.(idSource).QTOUTTEMPS(o)='DEFAULT'
            ENDIF

            configS=replaceStructStruct(configS, impConfigS.(idSource), idReplace)
            updSet=1
            newname=''
          ENDIF
        ENDIF
        IF newname NE '' THEN BEGIN
          IF currNames.HasValue(newname) THEN BEGIN
            sv=DIALOG_MESSAGE('Import failed as parameter set '+newname+' already exist.', DIALOG_PARENT=parent)
          ENDIF ELSE BEGIN
            ;check output-templates connected
            ots=impConfigS.(IDs2import(i)+1).QTOUTTEMPS
            def_ots=WHERE(ots NE 'DEFAULT')
            IF def_ots(0) NE -1 THEN BEGIN
              newline = string([13B, 10B])
              sv=DIALOG_MESSAGE('The parameter set '+newname+' have links to these QuickTest Output templates '+newline+STRJOIN(ots,newline)+newline+'These will all be set to DEFAULT. Consider importing output templates and reconnect the paramater set.', /INFORMATION, DIALOG_PARENT=parent)
              FOREACH elem, impConfigS.(IDs2import(i)+1).QTOUTTEMPS, o DO impConfigS.(IDs2import(i)+1).QTOUTTEMPS(o)='DEFAULT'
            ENDIF
            configS=CREATE_STRUCT(configS, newname, impConfigS.(IDs2import(i)+1))
            updSet=1
          ENDELSE
        ENDIF
      ENDFOR

      IF updSet EQ 1 THEN SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=target_path
    ENDIF
  ENDIF ELSE sv=DIALOG_MESSAGE('File '+string([13B, 10B])+source_path+string([13B, 10B])+' do not contain parameter sets as expected.', DIALOG_PARENT=parent)
end

;import QuickTest template(s)
pro import_qt, source_path, target_path, mOpt, modSel, xo, yo, parent, NAMED_TEMPS=named_temps
  impquickTemp=updateQuickT(source_path, mOpt)
  IF SIZE(impquickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
    modNames=TAG_NAMES(impquickTemp)
    curimpQT=impquickTemp.(modSel)
    names=TAG_NAMES(curimpQT)

    IDs2import=!Null
    IF N_ELEMENTS(named_temps) GT 0 THEN BEGIN
      FOR i=0, N_ELEMENTS(named_temps)-1 DO BEGIN
        id=WHERE(names EQ named_temps(i))
        IF id(0) NE -1 THEN IDs2import=[IDs2import, id(0)]
      ENDFOR
    ENDIF ELSE BEGIN
      box=[$
        '1, BASE,, /COLUMN', $
        '0, LABEL, Templates found in selected config file for '+modNames(modSel), $
        '0, LABEL, Select template(s) to import', $
        '0, LABEL, ',$
        '2, LIST, ' + STRJOIN(names,'|') + ', TAG=templates', $
        '1, BASE,, /ROW', $
        '0, BUTTON, Cancel, QUIT, TAG=Cancel',$
        '2, BUTTON, OK, QUIT, TAG=OK']
      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select template(s) to import', XSIZE=300, YSIZE=300, FOCUSNO=3, XOFFSET=xo+250, YOFFSET=yo+250)

      IF res.OK THEN BEGIN
        IF N_ELEMENTS(res.templates) NE 0 THEN IDs2import=res.templates
      ENDIF
    ENDELSE

    IF N_ELEMENTS(IDs2import) GT 0 THEN BEGIN
      quickTemp=!Null
      RESTORE, target_path
      curQT=!Null
      currNames=!Null
      updCurQT=0
      IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
        IF SIZE(quickTemp.(modSel), /TNAME) EQ 'STRUCT' THEN BEGIN
          curQT=quickTemp.(modSel)
          currNames=TAG_NAMES(curQT)
        ENDIF
      ENDIF ELSE quickTemp=!Null

      FOR i=0, N_ELEMENTS(IDs2import)-1 DO BEGIN
        newname=names(IDs2import(i))
        IF N_ELEMENTS(currNames) GT 0 THEN BEGIN
          IF currNames.HasValue(newname) THEN BEGIN
            ;ask for new name

            box=[$
              '1, BASE,, /COLUMN', $
              '0, LABEL, Template '+newname+' already exist', $
              '2,  TEXT, , LABEL_LEFT=Rename the imported template:, WIDTH=12, TAG=newname,', $
              '1, BASE,, /ROW', $
              '0, BUTTON, OK, QUIT, TAG=Save',$
              '0, BUTTON, Overwrite, QUIT, TAG=Overwrite',$
              '2, BUTTON, Cancel/Ignore, QUIT, TAG=Cancel']
            res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename the imported template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xo+250, YOFFSET=yo+250)

            IF res.Cancel THEN newname=''
            IF res.Save THEN BEGIN
              IF res.newname NE '' THEN newname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL)) ELSE newname=''
            ENDIF
            IF res.Overwrite THEN BEGIN
              idReplace=WHERE(currNames EQ newname)
              idSource=WHERE(names EQ newname)
              curQT=replaceStructStruct(curQT, curimpQT.(idSource), idReplace)
              updCurQT=1
              newname=''
            ENDIF
          ENDIF
        ENDIF
        IF newname NE '' THEN BEGIN
          curQT=CREATE_STRUCT(curQT, newname, curimpQT.(IDs2import(i)))
          updCurQT=1
        ENDIF

      ENDFOR

      IF updCurQT THEN BEGIN
        IF N_ELEMENTS(quickTemp) EQ 0 THEN BEGIN
          ms=TAG_NAMES(mOpt)
          FOR i=0, N_TAGS(mOpt)-1 DO quickTemp=CREATE_STRUCT(quickTemp, ms(i), -1)
        ENDIF
        quickTemp=replaceStructStruct(quickTemp, curQT, modSel)
        SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=target_path
      ENDIF

    ENDIF
  ENDIF

end

;import QuickTest Output template(s)
pro import_qto, source_path, target_path, analyseStrA, modNmb, xo, yo, parent
  impquickTout=updateQuickTout(source_path, analyseStrA)
  modNames=TAG_NAMES(impquickTout)
  curimpQTO=impquickTout.(modNmb)
  names=TAG_NAMES(curimpQTO)

  box=[$
    '1, BASE,, /COLUMN', $
    '0, LABEL, Templates found in selected config file for '+modNames(modNmb), $
    '0, LABEL, Select template(s) to import', $
    '0, LABEL, ',$
    '2, LIST, ' + STRJOIN(names,'|') + ', TAG=templates', $
    '1, BASE,, /ROW', $
    '0, BUTTON, Cancel, QUIT, TAG=Cancel',$
    '2, BUTTON, OK, QUIT, TAG=OK']
  res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select template(s) to import', XSIZE=300, YSIZE=300, FOCUSNO=3, XOFFSET=xo+250, YOFFSET=yo+250)

  IF res.OK THEN BEGIN
    IF N_ELEMENTS(res.templates) NE 0 THEN BEGIN
      RESTORE, target_path
      curQTO=quickTout.(modNmb)
      currNames=TAG_NAMES(curQTO)

      updcurQTO=0
      IDs2import=res.templates

      FOR i=0, N_ELEMENTS(IDs2import)-1 DO BEGIN
        newname=names(IDs2import(i))
        IF currNames.HasValue(newname) THEN BEGIN
          ;ask for new name

          box=[$
            '1, BASE,, /COLUMN', $
            '0, LABEL, Template '+newname+' already exist', $
            '2,  TEXT, , LABEL_LEFT=Rename the imported template:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, OK, QUIT, TAG=Save',$
            '0, BUTTON, Overwrite, QUIT, TAG=Overwrite',$
            '2, BUTTON, Cancel/Ignore, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename the imported template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xo+250, YOFFSET=yo+250)

          IF res.Cancel THEN newname=''
          IF res.Save THEN BEGIN
            IF res.newname NE '' THEN newname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL)) ELSE newname=''
          ENDIF
          IF res.Overwrite THEN BEGIN
            idReplace=WHERE(currNames EQ newname)
            idSource=IDs2import(i)
            curQTO=replaceStructStruct(curQTO, curimpQTO.(idSource), idReplace)
            updcurQTO=1
            newname=''
          ENDIF
        ENDIF
        IF newname NE '' THEN BEGIN
          curQTO=CREATE_STRUCT(curQTO, newname, curimpQTO.(IDs2import(i)))
          updcurQTO=1
        ENDIF
      ENDFOR

      IF updcurQTO THEN BEGIN
        quickTout=replaceStructStruct(quickTout, curQTO, modNmb)
        SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=target_path
      ENDIF
    ENDIF

  ENDIF

end

;import automation template (loadTemp)
pro import_a, source_path, target_path, mOpt, modSel, xo, yo, parent
  imploadTemp=updateLoadT(source_path, mOpt)
  IF SIZE(imploadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
    modNames=TAG_NAMES(imploadTemp)
    IF SIZE(imploadTemp.(modSel), /TNAME) EQ 'STRUCT' THEN BEGIN

      modNames=TAG_NAMES(imploadTemp)
      curimpLT=imploadTemp.(modSel)
      names=TAG_NAMES(curimpLT)

      box=[$
        '1, BASE,, /COLUMN', $
        '0, LABEL, Templates found in selected config file for '+modNames(modSel), $
        '0, LABEL, Select template(s) to import', $
        '0, LABEL, ',$
        '2, LIST, ' + STRJOIN(names,'|') + ', TAG=templates', $
        '1, BASE,, /ROW', $
        '0, BUTTON, Cancel, QUIT, TAG=Cancel',$
        '2, BUTTON, OK, QUIT, TAG=OK']
      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Select template(s) to import', XSIZE=300, YSIZE=300, FOCUSNO=3, XOFFSET=xo+250, YOFFSET=yo+250)

      IF res.OK THEN BEGIN
        IF N_ELEMENTS(res.templates) NE 0 THEN BEGIN
          loadTemp=!Null
          RESTORE, target_path
          curLT=!Null
          currNames=!Null
          IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
            IF SIZE(loadTemp.(modSel), /TNAME) EQ 'STRUCT' THEN BEGIN
              curLT=loadTemp.(modSel)
              currNames=TAG_NAMES(curLT)
            ENDIF
          ENDIF ELSE loadTemp=!Null
          IDs2import=res.templates
          updIDs=!Null
          FOR i=0, N_ELEMENTS(IDs2import)-1 DO BEGIN
            newname=names(IDs2import(i))
            IF N_ELEMENTS(currNames) GT 0 THEN BEGIN
              IF currNames.HasValue(newname) THEN BEGIN
                ;ask for new name

                box=[$
                  '1, BASE,, /COLUMN', $
                  '0, LABEL, Template '+newname+' already exist', $
                  '2,  TEXT, , LABEL_LEFT=Rename the imported template:, WIDTH=12, TAG=newname,', $
                  '1, BASE,, /ROW', $
                  '0, BUTTON, OK, QUIT, TAG=Save',$
                  '0, BUTTON, Overwrite, QUIT, TAG=Overwrite',$
                  '2, BUTTON, Cancel/Ignore, QUIT, TAG=Cancel']
                res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename the imported template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xo+250, YOFFSET=yo+250)

                IF res.Cancel THEN newname=''
                IF res.Save THEN BEGIN
                  IF res.newname NE '' THEN newname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL)) ELSE newname=''
                ENDIF
                IF res.Overwrite THEN BEGIN
                  idReplace=WHERE(currNames EQ newname)
                  idSource=WHERE(names EQ newname)
                  curLT=replaceStructStruct(curLT, curimpLT.(idSource), idReplace)
                  updIDs=[updIDs, idReplace]
                  newname=''
                ENDIF
              ENDIF
            ENDIF

            IF newname NE '' THEN BEGIN
              curLT=CREATE_STRUCT(curLT, newname, curimpLT.(IDs2import(i)))
              updIDs=[updIDs, N_TAGS(curLT)-1]
            ENDIF
          ENDFOR

          IF N_ELEMENTS(loadTemp) EQ 0 THEN BEGIN
            ms=TAG_NAMES(mOpt)
            FOR i=0, N_TAGS(mOpt)-1 DO loadTemp=CREATE_STRUCT(loadTemp, ms(i), -1)
          ENDIF

          IF N_ELEMENTS(curLT) NE 0 THEN loadTemp=replaceStructStruct(loadTemp, curLT, modSel)

          missingParamSets=!Null
          IDmissingParamSets=!Null
          missingQuickTemp=!Null
          IDmissingQuickTemp=!Null
          importMissing=0
          IF N_ELEMENTS(updIDs) GT 0 THEN BEGIN
            ;check if parameterset-name, QuickTest name  exist in current config file
            paramSetNames=STRUPCASE(TAG_NAMES(configS))
            paramSetNames=paramSetNames[1:-1]
            QuickTempNames=!Null
            IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
              IF SIZE(quickTemp.(modSel), /TNAME) EQ 'STRUCT' THEN QuickTempNames=TAG_NAMES(quickTemp.(modSel))
            ENDIF

            FOR i=0, N_ELEMENTS(updIDs)-1 DO BEGIN
              thisLT=loadTemp.(modSel).(updIDs(i))
              IF ~paramSetNames.HasValue(thisLT.PARAMSET) THEN BEGIN
                missingParamSets=[missingParamSets,thisLT.PARAMSET]
                IDmissingParamSets=[IDmissingParamSets,updIDs(i)]
              ENDIF
              IF N_ELEMENTS(QuickTempNames) GT 0 THEN BEGIN
                IF ~QuickTempNames.HasValue(thisLT.QUICKTEMP) THEN BEGIN
                  missingQuickTemp=[missingQuickTemp,thisLT.QUICKTEMP]
                  IDmissingQuickTemp=[IDmissingQuickTemp,updIDs(i)]
                ENDIF
              ENDIF ELSE BEGIN
                missingQuickTemp=[missingQuickTemp,thisLT.QUICKTEMP]
                IDmissingQuickTemp=[IDmissingQuickTemp,updIDs(i)]
              ENDELSE
            ENDFOR
            IF N_ELEMENTS(missingParamSets) + N_ELEMENTS(missingQuickTemp) GT 0 THEN BEGIN
              newline = string([13B, 10B])
              msg='The imported automation templates are linked to parameter sets and/or QuickTest templates that cannot be found in the current config file.'+newline+' Import the missing parameter sets and QuickTest templates?'
              msg2=newline+''
              sv=DIALOG_MESSAGE(msg, /QUESTION, DIALOG_PARENT=parent)
              IF sv EQ 'Yes' THEN importMissing = 1
            ENDIF
          ENDIF

          IF importMissing THEN BEGIN
            SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=target_path
            IF N_ELEMENTS(missingParamSets) GT 0 THEN BEGIN
              missingParamSets=missingParamSets(SORT(missingParamSets))
              missingParamSets=missingParamSets(UNIQ(missingParamSets))
              import_s, source_path, target_path, xo, yo, parent, NAMED_SETS=missingParamSets
            ENDIF
            IF N_ELEMENTS(missingQuickTemp) GT 0 THEN BEGIN
              missingQuickTemp=missingQuickTemp(SORT(missingQuickTemp))
              missingQuickTemp=missingQuickTemp(UNIQ(missingQuickTemp))
              import_qt, source_path, target_path, mOpt, modSel, xo, yo, parent, NAMED_TEMPS=missingQuickTemp
            ENDIF
          ENDIF ELSE BEGIN
            IF N_ELEMENTS(missingParamSets) GT 0 THEN BEGIN
              FOR i=0, N_ELEMENTS(IDmissingParamSets)-1 DO loadTemp.(modSel).(IDmissingParamSets(i)).PARAMSET=''
            ENDIF
            IF N_ELEMENTS(missingQuickTemp) GT 0 THEN BEGIN
              FOR i=0, N_ELEMENTS(IDmissingQuickTemp)-1 DO loadTemp.(modSel).(IDmissingQuickTemp(i)).QUICKTEMP=''
            ENDIF
            SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=target_path
            IF N_ELEMENTS(missingParamSets) + N_ELEMENTS(missingQuickTemp) GT 0 THEN sv=DIALOG_MESSAGE('Imported automation templates with link to missing parameter sets or missing QuickTest templates are now unlinked', /INFORMATION, DIALOG_PARENT=parent)
          ENDELSE

        ENDIF
      ENDIF
    ENDIF ELSE sv=DIALOG_MESSAGE('Found no automation templates for modality '+modName(modSel), /INFORMATION, DIALOG_PARENT=parent)
  ENDIF ELSE sv=DIALOG_MESSAGE('Found no automation templates in the selected file.', /INFORMATION, DIALOG_PARENT=parent)
end
;
;pro import_rdt, source_path, target_path, xo, yo
;
;end