pro editSavedDatFiles

;this code do not support change in matrix size
RESTORE, DIALOG_PICKFILE(TITLE='Open image in .dat file', /READ, FILTER='*.dat')
newMatrix=imageQCmatrix.matrix


;change newMatrix
;----------------------------------------
;Example: sum of 3 files
;adr=DIALOG_PICKFILE(TITLE='Select 3 .dat files with same matrix size', /READ, /MULTIPLE, FILTER='*.dat')
;RESTORE, adr(0)
;matrix1=imageQCmatrix.matrix
;RESTORE, adr(1)
;matrix2=imageQCmatrix.matrix
;RESTORE, adr(2)
;matrix3=imageQCmatrix.matrix
;newMatrix=matrix1+matrix2+matrix3
;----------------------------------------

imageQCmatrix.matrix=newMatrix

adr=DIALOG_PICKFILE(TITLE='Save as new .dat-file',/WRITE, FILTER='*.dat', /FIX_FILTER)
IF adr NE '' THEN BEGIN
  imageQCmatrix.filename=adr; changed when opened so that renaming/moving file is possible
  SAVE, imageQCmatrix, FILENAME=adr
ENDIF

end
