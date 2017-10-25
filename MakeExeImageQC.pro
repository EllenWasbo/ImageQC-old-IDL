pro MakeExeImageQC
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\' 
  exePath='I:\Felles\Straalevern\Kvalitetskontroll\IDL_programmer\Exe\'
  MAKE_RT, 'ImageQC', exePath, /OVERWRITE, SAVEFILE=thisPath+'imageqc.sav', /VM, /WIN32
end
;create new default config
;thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('ImageQC'))+'\'
;materials=['Teflon','Delrin','Acrylic','Water','Polystyrene','LDPE','PMP','Air']
;relMassD=[2.16,1.41,1.18,1.,1.05,0.92,0.83,0.]
;posX=[-28.,-58.,-28.,0.,28.,58.,28.,0.]
;posY=[-50.,0.,50.,58.,50.,0.,-50.,-58.]
;lintab=CREATE_STRUCT('materials', materials, 'relMassD', relMassD, 'posX', posX, 'posY', posY)
;configDefault=CREATE_STRUCT('defPath','C:\','deciMark',',', 'copyHeader', 0, $ 
;  'typeROI',0,'typeROIX',0,$
;  'MTFtype',2,'MTFtypeX',1,'MTFtypeNM',1,'plotMTF',3,'plotMTFX', 3, 'plotMTFNM',4,'MTFroiSz',11.0,'MTFroiSzX',[20.,50.],'MTFroiSzNM',[20.,20.], $
;  'cutLSF',1,'cutLSF1',3,'cutLSF2',1, 'cutLSFX', 1, 'cutLSFX1', 3, $
;  'LinROIrad',3.,'LinROIradS',11., 'LinTab',lintab, $
; 'RampDist',38.,'RampLen',60.,'RampBackG',5.,'RampSearch',5,'RampAvg',1,$
; 'HomogROIsz',10., 'HomogROIszX',10., 'HomogROIszPET', 10.,'HomogROIdist',55.,'HomogROIdistPET',55.,'HomogROIszNM',25.,'HomogROIdistNM',[100.,200.],$
;  'NoiseROIsz',55., $
;  'NPSroiSz', 50, 'NPSroiDist', 50., 'NPSsubNN', 20, 'NPSroiSzX', 256, 'NPSsubSzX', 5, 'NPSavg', 1, $
;  'STProiSz', 11.3, 'ScanSpeedAvg', 25, 'ScanSpeedHeight', 100., 'ScanSpeedFiltW', 15, 'ContrastRad1', 20., 'ContrastRad2', 58.,$
;  'CrossROIsz', 60., 'CrossVol', 0.0)
;SAVE, configDefault, FILENAME=thisPath+'data\configDefault.dat'

;config=CREATE_STRUCT('defPath',defPath,$
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


