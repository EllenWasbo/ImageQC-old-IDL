pro MakeExeImageQC
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\' 
  exePath='I:\Felles\Straalevern\Kvalitetskontroll\IDL_programmer\Exe\'
  MAKE_RT, 'ImageQC', exePath, /OVERWRITE, SAVEFILE=thisPath+'imageqc.sav', /VM, /WIN32
end

;;  update configDefault
;configS=updateConfigS('')
;thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\' 
;SAVE, configS, FILENAME=thisPath+'data\config.dat'
;SAVE, configS, FILENAME=thisPath+'data\configDefault.dat'

;----------------------
;
;
;ImageQC.ini (change show to false)
;[DIALOG]
;Show=FALSE
;BackColor=&H6B1F29
;Caption=IDL Virtual Machine Application
;Picture=.\splash.bmp
;DefaultAction=.\IDL71\bin\bin.x86\idlrt.exe -vm=ImageQC.sav
;
;[BUTTON1]
;Show=True
;Caption=TestObjGraphics
;Action=.\IDL71\bin\bin.x86\idlrt.exe -vm=ImageQC.sav
;
;[BUTTON2]
;Show=True
;Caption=Exit
;Action=Exit
;
;[BUTTON3]
;Show=False
;Caption=
;Action=
;
;[BUTTON4]
;Show=False
;Caption=
;Action=
;-----------------------------
;autorun.inf
;[autorun]
;open = ImageQC.exe
;icon= ImageQC.ico


