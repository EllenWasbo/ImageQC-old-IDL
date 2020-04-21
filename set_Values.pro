;tags in imgStruct that is not string or are specific to one modality [tag, typestring, modalitynumbers separated by / or empty for all
pro set_imgStructInfo, iSi
  iSi=[['seriesNmb','LONG',''],$
    ['acqNmb','LONG',''],$
    ['imgNo','LONG',''],$
    ['detectorID','STRING','1'],$
    ['nFrames','LONG',''],['frameNo','LONG',''],$
    ['wCenter','FLOAT',''],['wWidth','FLOAT',''],$
    ['zoomFactor','FLOAT',''],$
    ['sliceThick','FLOAT','0/3/4/5'], $
    ['pix', 'FLOAT',''],['imageSize','LONG',''],$
    ['FOV','FLOAT',''],['rekonFOV','FLOAT','0/3/4/5'],$
    ['zpos', 'FLOAT','0/3/4/5'], $
    ['reconMethod','STRING','2/3/4'],$
    ['kernel','STRING','0/3/4'],$
    ['kVp','FLOAT','0/1'],['mA','FLOAT','0/1'],['mAs','FLOAT','0/1'],['ExpTime','FLOAT','0/1'],$
    ['filterAddOn','STRING','0/1'],$
    ['coll','FLOAT','0'],['pitch','FLOAT','0'],$
    ['ExModType','STRING','0'],$
    ['CTDIvol','FLOAT','0'],['DAP','FLOAT','1'],$
    ['EI','FLOAT','1'],['sensitivity','FLOAT','1'],$
    ['sdd','FLOAT','1'],$
    ['collType','STRING','2/3/4'],$
    ['nEWindows','LONG','2/3/4'],['EWindowName','STRING','2/3/4'],$
    ['radius1','FLOAT','2/3'],['radius2','FLOAT','2/3'],$
    ['angle','FLOAT','2/3/4'],$
    ['acqFrameDuration','DOUBLE','2/3/4'],['acqTerminationCond','STRING','2/3/4'],['radiopharmaca','STRING','2/3/4'],['admDose','FLOAT','2/3/4'],['admDoseTime','STRING','2/3/4'],$
    ['attCorrMethod','STRING','2/3/4'],['scaCorrMethod','STRING','2/3/4'],['scatterFrac','FLOAT','2/3/4'],$
    ['imgFreq','FLOAT','5'],['MRacqType','STRING','5'],['MRscanSeq','STRING','5'],['MRseqVariant','STRING','5'],['TR','LONG','5'],['TE','LONG','5'],['NSA','LONG','5'],['flipAng','LONG','5'],['spaceSlice','FLOAT','5']]

  RETURN
end

;tags in configS with explanations for settings.pro [tagname, description, modality (0-4) or output '-1' other '-2', not list in settings like others'-10'' analysestring or '','BOOL/INT/FLOAT/STRING']
pro set_configSinfo, cSi
  cSi=[['deciMark','Decimal mark','-1','','STRING'], $
    ['copyHeader', 'Include headers', '-1','','BOOL'], $
    ['includeFilename', 'Include filename','-1','','BOOL'], $
    ['transposeTable', 'Transpose table','-1','','BOOL'], $
    ['append','Append when opening files','-2','','BOOL'],$
    ['autoImportPath','Default path for automation import','-10','','STRING'],$
    ['qtOutTemps', 'QuickTest Output templates', '-10','','STRING'], $
    ['MTFtype','MTF method (0..2)','0','MTF','INT'],$
    ['MTFtypeX','LSF fit to.. (0..2)','1','MTF','INT'],$
    ['MTFtypeNM','MTF method (0..3)','2','MTF','INT'],$
    ['MTFtypeSPECT','MTF method (0..3)','3','MTF','INT'],$
    ['plotMTF','Plot selection (0..3)','0','MTF','INT'],$
    ['plotMTFX', 'Plot selection (0..3)','1','MTF','INT'],$
    ['plotMTFNM','Plot selection (0..4)','2','MTF','INT'],$
    ['plotMTFSPECT','Plot selection (0..4)','3','MTF','INT'],$
    ['tableMTF','Table gaussion or discrete','0','MTF','INT'],$
    ['cyclMTF','cy/mm or cy/cm','0','MTF','INT'],$
    ['tableMTFX', 'Table gaussion or discrete','1','MTF','INT'],$
    ['MTFroiSz','ROI size (radius)','0','MTF','FLOAT'],$
    ['MTFroiSzX','ROI size x,y','1','MTF','FLOAT'],$
    ['MTFroiSzNM','ROI size x,y','2','MTF','FLOAT'],$
    ['MTFroiSzSPECT','ROI size','3','MTF','FLOAT'],$
    ['MTF3dSPECT','Analyse 3d','3','MTF','BOOL'],$
    ['cutLSF','Cut LSF tails','0','MTF','BOOL'],$
    ['cutLSF1','Cut LSF from halfmax #FWHM','0','MTF','FLOAT'],$
    ['cutLSF2','Fade out cut within #FWHM','0','MTF','FLOAT'],$
    ['cutLSFX', 'Cut LSF tails','1','MTF','BOOL'],$
    ['cutLSFX1', 'Cut LSF from halfmax #FWHM','1','MTF','FLOAT'],$
    ['offxy', 'Extra offset MTF ROI dx,dy','0/1','MTF','INT'], $
    ['searchMaxMTF_ROI','Center ROI by max in image','0','MTF','BOOL'],$
    ['LinROIradS','Search ROI radius','0','CTLIN','FLOAT'],$
    ['LinROIrad','ROI radius','0','CTLIN','FLOAT'],$
    ['LinTab','Material,position,density table','-10','CTLIN','STRUCT'],$
    ['RampType','Ramp type (0..2)','0','SLICETHICK','INT'],$
    ['RampDens','Ramp density (0..1)','0','SLICETHICK','INT'],$
    ['RampDist','Center to ramp distance','0','SLICETHICK','FLOAT'],$
    ['RampLen','Profile length','0','SLICETHICK','FLOAT'],$
    ['RampBackG','Background from outer','0','SLICETHICK','FLOAT'],$
    ['RampSearch','Search for max in profile','0','SLICETHICK','INT'],$
    ['RampAvg','Average over neighbour profiles','0','SLICETHICK','INT'],$
    ['HomogROIsz','ROI radius','0','HOMOG','FLOAT'],$
    ['HomogROIszX','ROI radius','1','HOMOG','FLOAT'],$
    ['HomogROIszPET', 'ROI radius','4','HOMOG','FLOAT'],$
    ['HomogROIdist','ROI distance from center','0','HOMOG','FLOAT'],$
    ['HomogROIdistPET','ROI distance from center','4','HOMOG','FLOAT'],$
    ['NoiseROIsz','ROI radius','0','NOISE','FLOAT'],$
    ['HUwaterROIsz','ROI radius','0','HUWATER','FLOAT'],$
    ['NPSroiSz', 'ROI size','0','NPS','INT'],$
    ['NPSroiDist','ROI distance from center','0','NPS','FLOAT'],$
    ['NPSsubNN', 'Number of ROIs', '0','NPS','INT'],$
    ['NPSavg', 'Plot average','0','NPS','BOOL'], $
    ['NPSroiSzX', 'ROI size','1','NPS','INT'],$
    ['NPSsubSzX', 'Subimage size','1', 'NPS','INT'],$
    ['STProiSz', 'ROI radius','1','STP','FLOAT'],$
    ['unifAreaRatio', 'UFOV ratio','2','UNIF','FLOAT'],$
    ['SNIAreaRatio','Ratio of image to be analyzed','2','SNI','FLOAT'],$
    ['unifCorr','Correct for point-source','2','UNIF','BOOL'],$
    ['SNIcorr','Correct for point-source','2','SNI','BOOL'],$
    ['distCorr','Source-detector distance','2','UNIF/SNI','FLOAT'],$
    ['detThick','Detector thickness','2','UNIF/SNI','FLOAT'],$
    ['attCoeff','Attenuation coeff detector','2','UNIF/SNI','FLOAT'],$
    ['barWidths','Bar widths','2','BAR','FLOAT'],$
    ['barROIsz','ROI diameter','2','BAR','FLOAT'],$
    ['ScanSpeedAvg', 'Average over ROI width','2','SCANSPEED','INT'],$
    ['ScanSpeedHeight', 'ROI heigth','2','SCANSPEED','FLOAT'],$
    ['ScanSpeedFiltW', 'Median filter width','2','SCANSPEED','INT'],$
    ['ContrastRad1', 'ROI radius','3','CONTRAST','FLOAT'],$
    ['ContrastRad2','ROI distance from center','3','CONTRAST','FLOAT'],$
    ['CrossROIsz', 'ROI radius','4','CROSSCALIB','FLOAT'],$
    ['CrossVol', 'Volume of container','4','CROSSCALIB','FLOAT']]

  RETURN
end