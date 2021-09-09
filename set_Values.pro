;tags in imgStruct that is not string or are specific to one modality [tag, typestring, modalitynumbers separated by / or empty for all, DICOM tag xxxx xxxx (or empty if too complicated or prevent showing
pro set_imgStructInfo, iSi
  iSi=[['modality','STRING','','0008 0060'],['detectorID','STRING','1/2','0018 700A'],$
    ['patientName','STRING','','0010 0010'],['patientID','STRING','','0010 0020'],['imageType','STRING','','0008 0008'],['presType','STRING','','0008 0068'],$
    ['studyDescr','STRING','','0008 1030'],['seriesName','STRING','','0008 103E'],['protocolName','STRING','','0018 1030'],['seriesNmb','LONG','','0020 0011'],['acqNmb','LONG','','0020 0012'],$
    ['imgNo','LONG','','0020 0013'],$
    ['detectorID','STRING','1','0018 700A'],$
    ['nFrames','LONG','','0028 0008'],['frameNo','LONG','',''],$
    ['wCenter','FLOAT','','0028 1050'],['wWidth','FLOAT','','0028 1051'],$
    ['zoomFactor','FLOAT','','0028 0031'],$
    ['sliceThick','FLOAT','0/3/4/5','0018 0050'], $
    ['pix', 'FLOAT','','0028 0030'],['imageSize','LONG','','0028 0011'],$
    ['FOV','FLOAT','','0018 0090'],['rekonFOV','FLOAT','0/3/4/5','0018 1100'],$
    ['zpos', 'FLOAT','0/3/4/5','0020 1041'], $
    ['reconMethod','STRING','2/3/4','0054 1103'],$
    ['kernel','STRING','0/3/4','0018 1210'],$
    ['kVp','FLOAT','0/1','0018 0060'],['mA','FLOAT','0/1','0018 8151'],['mAs','FLOAT','0/1','0018 1153'],['ExpTime','FLOAT','0/1','0018 1150'],['focalSpotSz','STRING','0/1','0018 1190'],$
    ['filterAddOn','STRING','0/1','0018 1160'],$
    ['coll','FLOAT','0','0018 9307'],['pitch','FLOAT','0','0018 9311'],$
    ['ExModType','STRING','0','0018 9323'],['ExModType','STRING','1','0018 7062'],$
    ['CTDIvol','FLOAT','0','0018 9345'],['','STRING','0/1','0018 1190'],['DAP','FLOAT','1','0018 115E'],$
    ['EI','FLOAT','1','0018 1411'],['sensitivity','FLOAT','1','0018 6000'],$
    ['sdd','FLOAT','1','0018 1110'],$
    ['collType','STRING','2/3/4','0018 1181'],$
    ['nEWindows','LONG','2/3/4','0054 0011'],['EWindowName','STRING','2/3/4',''],$
    ['radius1','FLOAT','2/3',''],['radius2','FLOAT','2/3',''],['detectorVector','STRING','2/3',''],['angle','FLOAT','2/3',''],$
    ['acqFrameDuration','DOUBLE','2/3/4','0018 1242'],['acqTerminationCond','STRING','2/3/4','0018 0017'],['radiopharmaca','STRING','2/3/4','0018 0031'],['admDose','FLOAT','2/3/4',''],['admDoseTime','STRING','2/3/4',''],$
    ['attCorrMethod','STRING','2/3/4','0054 1101'],['scaCorrMethod','STRING','2/3/4','0054 1105'],['scatterFrac','FLOAT','2/3/4','0054 1323'],$
    ['imgFreq','FLOAT','5','0018 0084'],['MRacqType','STRING','5','0018 0023'],['MRscanSeq','STRING','5','0018 0020'],['MRseqVariant','STRING','5','0018 0021'],$
    ['TR','LONG','5','0018 0080'],['TE','LONG','5','0018 0081'],['NSA','LONG','5','0018 0083'],['flipAng','LONG','5','0018 1314'],$
    ['spaceSlice','FLOAT','5','0018 0088'],['recCoilName','STRING','5','0018 1250'],['traCoilName','STRING','5','0018 1251']]

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
    ['autoContinue','Default setting to stop 0 or not 1 between automation templates','-10','','INT'],$
    ['wait','Seconds to wait for Excel/Acrobat to open before and close after reading of data from PDF','-10','','INT'],$
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
    ['offxyMTF', 'Extra offset MTF ROI dx,dy','0','MTF','INT'], $
    ['offxyMTF_X', 'Extra offset MTF ROI dx,dy','1','MTF','INT'], $
    ['offxyMTF_unit', 'Extra offset MTF pix(0) or mm(1)','0','MTF','INT'], $
    ['offxyMTF_X_unit', 'Extra offset MTF pix(0) or mm(1)','1','MTF','INT'], $
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
    ['altHomogX','Table output selection','1','HOMOG','INT'],$
    ['HomogROIszPET', 'ROI radius','4','HOMOG','FLOAT'],$
    ['HomogROIdist','ROI distance from center','0','HOMOG','FLOAT'],$
    ['HomogROIdistPET','ROI distance from center','4','HOMOG','FLOAT'],$
    ['NoiseROIsz','ROI radius','0','NOISE','FLOAT'],$
    ['NoiseXpercent','ROI (%)','1','NOISE','INTEGER'],$
    ['HUwaterROIsz','ROI radius','0','HUWATER','FLOAT'],$
    ['typeROI','ROI shape (0..2)','0','ROI','INT'],$
    ['ROIrad','ROI radius','0','ROI','FLOAT'],$
    ['ROIx','ROI size x','0','ROI','FLOAT'],$
    ['ROIy','ROI size y','0','ROI','FLOAT'],$
    ['ROIa','ROI rotation','0','ROI','FLOAT'],$
    ['offxyROI', 'Extra offset ROI dx,dy','0','ROI','INT'], $
    ['offxyROI_unit', 'Extra offset ROI pix(0) or mm(1)','0','ROI','INT'], $
    ['typeROIX','ROI shape (0..2)','1','ROI','INT'],$
    ['ROIXrad','ROI radius','1','ROI','FLOAT'],$
    ['ROIXx','ROI size x','1','ROI','FLOAT'],$
    ['ROIXy','ROI size y','1','ROI','FLOAT'],$
    ['ROIXa','ROI rotation','1','ROI','FLOAT'],$
    ['offxyROIX', 'Extra offset ROI dx,dy','1','ROI','INT'], $
    ['offxyROIX_unit', 'Extra offset ROI pix(0) or mm(1)','1','ROI','INT'], $
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
    ['unifCorrPos','Fit x,y pos when correcting','2','UNIF','INT'],$
    ['unifCorrRad','Lock fit to radius','2','UNIF','FLOAT'],$
    ['SNIcorr','Correct for point-source','2','SNI','BOOL'],$
    ['SNIcorrPos','Fit x,y pos when correcting','2','SNI','INT'],$
    ['SNIcorrRad','Lock fit to radius','2','SNI','FLOAT'],$
    ['SNI_fcd','SNI human filter c,f,d,','2','SNI','FLOAT'],$
    ['plotSNI','Plot selection (0..1)','2','SNI','INT'],$
    ['barWidths','Bar widths','2','BAR','FLOAT'],$
    ['barROIsz','ROI diameter','2','BAR','FLOAT'],$
    ['ScanSpeedAvg', 'Average over ROI width','2','SCANSPEED','INT'],$
    ['ScanSpeedHeight', 'ROI heigth','2','SCANSPEED','FLOAT'],$
    ['ScanSpeedFiltW', 'Median filter width','2','SCANSPEED','INT'],$
    ['ContrastRad1', 'ROI radius','3','CONTRAST','FLOAT'],$
    ['ContrastRad2','ROI distance from center','3','CONTRAST','FLOAT'],$
    ['CrossROIsz', 'ROI radius','4','CROSSCALIB','FLOAT'],$
    ['CrossVol', 'Volume of container','4','CROSSCALIB','FLOAT'],$
    ['SNR_MR_ROI', 'ROI size (% of phantom)','5','SNR','FLOAT'],$
    ['PUI_MR_ROI', 'ROI size (% of phantom)','5','PUI','FLOAT'],$
    ['Ghost_MR_ROI', 'ROI param (Crad, w, h, d, Copt(0/1))','5','GHOST','FLOAT'],$
    ['GD_MR_act', 'Actual phantom size (mm)','5','GEOMDIST','FLOAT'],$
    ['Slice_MR_ROI', 'ROI param (tanA, w, h, dU, dL, Copt(0/1))','5','SLICETHICK','FLOAT']]
  RETURN
end