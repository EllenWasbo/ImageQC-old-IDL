pro SAVEIF, saveOK, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=configPath

  fi=FILE_INFO(configPath)
  IF fi.write EQ 0 THEN BEGIN
    sv=DIALOG_MESSAGE('You have lost writing-permission to the config file. Saving will be blocked.')
    saveOK=-1
  ENDIF ELSE BEGIN
    configS.(0).saveStamp=SYSTIME(/SECONDS)
    SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=configPath
  ENDELSE
end

;on restore
;IF FILE_TEST(configPath, /READ) THEN RESTORE, configPath ELSE sv=DIALOG_MESSAGE('Lost connection to config file '+configPath, /ERROR)
