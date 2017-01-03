pro MakeExeImageQC
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\' 
  exePath='I:\Felles\Straalevern\Kvalitetskontroll\IDL_programmer\Exe\'
  MAKE_RT, 'ImageQC', exePath, /OVERWRITE, SAVEFILE=thisPath+'ImageQC.sav', /VM, /WIN32
end
;create new default config
;thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
;materials=['Teflon','Delrin','Acrylic','Water','Polystyrene','LDPE','PMP','Air']
;relMassD=[2.16,1.41,1.18,1.,1.05,0.92,0.83,0.]
;posX=[-28.,-58.,-28.,0.,28.,58.,28.,0.]
;posY=[-50.,0.,50.,58.,50.,0.,-50.,-58.]
;lintab=CREATE_STRUCT('materials', materials, 'relMassD', relMassD, 'posX', posX, 'posY', posY)
;configDefault=CREATE_STRUCT('defPath','C:\',$ 
;  'typeROI',0,'typeROIX',0,$
;  'MTFtype',2,'MTFtypeX',1,'MTFtypeNM',1,'plotMTF',3,'plotMTFX', 3, 'plotMTFNM',4,'MTFroiSz',11.0,'MTFroiSzX',[20.,50.],'MTFroiSzNM',[20.,20.], $
;  'cutLSF',0,'cutLSF1',3,'cutLSF2',1,$
;  'LinROIrad',3.,'LinROIradS',11., 'LinTab',lintab, $
; 'RampDist',38.,'RampLen',60.,'RampBackG',5.,'RampSearch',5,'RampAvg',1,$
; 'HomogROIsz',10., 'HomogROIszX',10., 'HomogROIszPET', 10.,'HomogROIdist',55.,'HomogROIdistPET',55.,'HomogROIszNM',25.,'HomogROIdistNM',[100.,200.],$
;  'NoiseROIsz',55., $
;  'NPSroiSz', 50, 'NPSroiDist', 50., 'NPSsubNN', 20, 'NPSroiSzX', 256, 'NPSsubSzX', 5, 'NPSavg', 1, $
;  'STProiSz', 11.3, 'ScanSpeedAvg', 25, 'ScanSpeedHeight', 100., 'ScanSpeedFiltW', 15, 'ContrastRad1', 20., 'ContrastRad2', 58.,$
;  'CrossROIsz', 55., 'CrossVol', 0.0)
;SAVE, configDefault, FILENAME=thisPath+'data\configDefault.dat'

;config=CREATE_STRUCT('defPath',defPath,$
;----------------------
;
;test config
;adrExe=dialog_pickfile(/READ,TITLE='Config file in EXE folder', PATH='I:\Felles\Straalevern\Kvalitetskontroll\IDL_programmer\Exe\ImageQC\data\')
;RESTORE, adrExe
;configExe=config
;help, configExe, /STRUCTURE
;adr=dialog_pickfile(/READ,TITLE='Config in progress folder', PATH='I:\Felles\Straalevern\Kvalitetskontroll\IDL_programmer\ImageQC\data\')
;RESTORE, adr
;help, config, /STRUCTURE
;stop
;
;configTemp=config
;
;IKKE OPPDATERT!!!!
;config=CREATE_STRUCT('defPath','I:\Felles\Straalevern\Kvalitetskontroll\CT\Gjennomfort QA CT\',$
;  'typeROI',configTemp.typeROI,'typeROIX',configTemp.typeROIX,$
;  'MTFtype',configTemp.MTFtype,'MTFtypeX',configTemp.MTFtypeX,'MTFtypeNM',configTemp.MTFtypeNM,'plotMTF',configTemp.plotMTF,'MTFroiSz',configTemp.MTFroiSz,$
;  'MTFroiSzX',configTemp.MTFroiSzX,$
;  'LinROIrad',configTemp.LinROIrad,'LinROIrad2',configTemp.LinROIrad2,$
;  'RampDist',configTemp.RampDist,'RampLen',configTemp.RampLen,'RampBackG',configTemp.RampBackG,'RampSearch',configTemp.RampSearch,'RampAvg',configTemp.RampAvg, $
;  'HomogROIsz',configTemp.HomogROIsz, 'HomogROIszX',configTemp.HomogROIszX,'HomogROIdist',configTemp.HomogROIdist,'HomogROIszNM',configTemp.HomogROIszNM,'HomogROIdistNM',configTemp.HomogROIdistNM,$
;  'NoiseROIsz',configTemp.NoiseROIsz, $
;  'NPSroiSz', configTemp.NPSroiSz, 'NPSsubSz', configTemp.NPSsubSz, 'NPSroiSzX', configTemp.NPSroiSzX, 'NPSsubSzX', configTemp.NPSsubSzX, $
;  'STProiSz', configTemp.STProiSz )
;SAVE, config, FILENAME=adr
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


