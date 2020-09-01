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


pro settings, GROUP_LEADER = mainbase, xoff, yoff, tabString;tabString = 'PARAM','QTSETUP', 'QTOUT' or 'AUTOSETUP'

  COMMON SETT, wtabSett, listSets_s, listParamAuto_s,autoNames_s, availModNmb, defModality, allTags,allDesc,$
    tblOutputSett, tblTestSett, orderTblTestSett, tblCurrMaterials, tblSelMaterials, lstCT_s, lstX_s, lstNM_s,lstMR_s, $
    lstModality_QT, QTnames, lstTempQT, lstQT, lstQTusedAuto, autoNames_qt, lstTest_qt,txtNimgTest_qt,$
    lstModality_qto, lstTemplates_qto, tblQTout, lstTest, lstAlt, lstCols, lstCalc, lstPer, txtDescr, lstQTOusedParam, setNames_qto, autoNames_qto, $
    qto_currMod, qto_currTemp, qto_currTest, qto_currOutp, qto_currSel, $
    txtAutoImpPath, lstModality_a, listTemp_a, txtBrowse_a, txtStatName_a, listSets_a, listQT_a, listElem, listSort, btnInclSub, btnOnlyLastDate,txtBrowseApp, btnMoveFiles, btnDeleteFiles,btnDeleteFilesEnd,$
    selecTemp_a,a_currMod,tempnames_a,paramSetNames,quickTempNames, tags_imgStruct, sortElem, ascElem, auto_warningBox, warningTxt1, warningTxt2,autoChanged, $
    tbl_rde, lstTemp_rdt, txtCat_rdt, txtFile_rdt, rdt_names, txtInputTest,txtFormatTest,txtOutputTest,$
    btnAdd_s,btnSave_s,btnDupliTemp_s,btnRenameTemp_s,btnUpTemp_s,btnDownTemp_s,btnDelete_s,btnSetDef,btnRefreshQT,$
    btnDuplicate_qt,btnRename_qt,btnUpTemp_qt,btnDownTemp_qt,btnDelete_qt,$
    btnDupliTemp_qto,btnRenameTemp_qto,btnDeleteTemp_qto,btnAddQTO,btnEditQTO,btnDelQTO,$
    btnAddTemp_a,btnOverWriteAuto,btnDupliTemp_a,btnRenameTemp_a,btnUpTemp_a,btnDownTemp_a,btnDelTemp_a,$
    btnAdd_rde,btnOverWrite_rde,btnDupli_rde,btnUp_rde,btnDown_rde,btnDel_rde,btnDuplicate_rdt,btnRename_rdt,btnUpTemp_rdt,btnDownTemp_rdt,btnDelete_rdt
  COMMON VARI
  COMPILE_OPT hidden
  

  IF saveOK EQ 0 THEN sv=DIALOG_MESSAGE('ImageQC is already in use. All changes to config-file (user settings) are blocked. (This block can be removed - see lower left button in Settings window if the block exist due to previous crash.)',/INFORMATION, DIALOG_PARENT=mainbase)
  autoChanged=0; =1 if any change to the automation template

  settingsbox = WIDGET_BASE(TITLE='User settings',  $
    /COLUMN, XSIZE=950, YSIZE=800, XOFFSET=xoff, YOFFSET=yoff,GROUP_LEADER=mainbase, /TLB_KILL_REQUEST_EVENTS, /MODAL)

  wtabSett=WIDGET_TAB(settingsbox, XSIZE=890, YSIZE=690, UVALUE='tabSettings')
  bParamSet=WIDGET_BASE(wtabSett, TITLE='Parameter sets', /COLUMN, UVALUE='tabParam')
  bQTsetup=WIDGET_BASE(wtabSett, TITLE='QuickTest templates', /COLUMN, UVALUE='tabQTsetup')
  bQTout=WIDGET_BASE(wtabSett, TITLE='QuickTest output templates', /COLUMN, UVALUE='tabQTout')
  bAuto=WIDGET_BASE(wtabSett, TITLE='Automation templates', /ROW, UVALUE='tabAuto')
  bRename=WIDGET_BASE(wtabSett, TITLE='RenameDICOM settings', /COLUMN, UVALUE='tabRename')
  bInfo=WIDGET_BASE(wtabSett, TITLE='Info', /COLUMN, UVALUE='tabInfo')

  RESTORE, thisPath+'data\config.dat'
  configTemp=!Null
  getParam, configTemp
  availMod=TAG_NAMES(testVisualQTNames)
  allMod=TAG_NAMES(multiOpt)
  availModNmb=!Null
  FOR m=0, N_ELEMENTS(allMod)-1 DO IF availMod.HasValue(allMod(m)) THEN availModNmb=[availModNmb, m]
  defModality=WHERE(modality EQ availModNmb); modality number in available list
  IF defModality(0) EQ -1 THEN defModality=0

  dummyImgStruct=imgStructUpdate('','')
  allTags=TAG_NAMES(dummyImgStruct)
  descImgStruct=imgStructDescTags()
  allDesc=!Null
  FOR i=0, N_TAGS(dummyImgStruct)-1 DO allDesc=[allDesc,descImgStruct.(i)]
  allTags=allTags(SORT(STRUPCASE(allDesc)))
  allDesc=allDesc(SORT(STRUPCASE(allDesc)))

  ;************ Parameter sets **************************
  ml1=WIDGET_LABEL(bParamSet, VALUE='', YSIZE=20)

  p_info0=WIDGET_LABEL(bParamSet, VALUE='The parameter set hold values and selections for output (export) and test settings.', /ALIGN_LEFT, FONT=font1)
  ml2=WIDGET_LABEL(bParamSet, VALUE='', YSIZE=10)

  bAll=WIDGET_BASE(bParamSet,/ROW)
  bLft=WIDGET_BASE(bAll, /COLUMN, XSIZE=200)
  bMlLftRgt=WIDGET_LABEL(bAll, VALUE='', XSIZE=10)
  bRgt=WIDGET_BASE(bAll, /COLUMN, XSIZE=700)

  ;list of parameter sets
  lblTopLft=WIDGET_LABEL(bLft, VALUE='Saved parameter sets ', /ALIGN_LEFT, FONT=font0)
  listSets_s=WIDGET_LIST(bLft, VALUE='', XSIZE=230, SCR_YSIZE=160, UVALUE='s_listSets', FONT=font1)
  bBtnsLft=WIDGET_BASE(bLft, /ROW)

  btnAdd_s=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\plus.bmp',/BITMAP, UVALUE='s_add', TOOLTIP='Save new parameter set with current values', SENSITIVE=saveOK)
  btnSave_s=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\save.bmp',/BITMAP, UVALUE='s_overwrite', TOOLTIP='Overwrite template with current values', SENSITIVE=saveOK); ask wether both quicktest changes (if changed) and current values?
  btnDupliTemp_s=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\copy.bmp' ,/BITMAP, TOOLTIP='Duplicate', UVALUE='s_duplicate', FONT=font1, SENSITIVE=saveOK)
  btnRenameTemp_s=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\rename.bmp' ,/BITMAP, UVALUE='s_rename', FONT=font1, SENSITIVE=saveOK)
  btnUpTemp_s=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='s_upTemp', TOOLTIP='Move template upwards in list', SENSITIVE=saveOK)
  btnDownTemp_s=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='s_downTemp', TOOLTIP='Move template downwards in list', SENSITIVE=saveOK)
  btnDelete_s=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\delete.bmp',/BITMAP, TOOLTIP='Delete selected parameter set', UVALUE='s_delete', SENSITIVE=saveOK)
  btnSetDef=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\star.bmp',/BITMAP, TOOLTIP='Set this parameter as default i.e. to be selected when opening ImageQC', UVALUE='s_setDefault', SENSITIVE=saveOK)
  ml2=WIDGET_LABEL(bLft, VALUE='', YSIZE=20)
  bBtnsRow=WIDGET_BASE(bLft, /ROW)
  bUseCurr=WIDGET_BASE(bLft, /NONEXCLUSIVE)

  ml3=WIDGET_LABEL(bLft, VALUE='', YSIZE=20)
  bBtnsLft2=WIDGET_BASE(bLft, /ROW)
  btnSetCurr=WIDGET_BUTTON(bBtnsLft2, VALUE='Use selected / Close', UVALUE='s_setCurr',FONT=font1)


  lblMlMid=WIDGET_LABEL(bLft, VALUE='', YSIZE=40)
  lblMidLft=WIDGET_LABEL(bLft, VALUE='Selected parameter set is used ', /ALIGN_LEFT, FONT=font0)
  lblMidLft=WIDGET_LABEL(bLft, VALUE='in these automation templates ', /ALIGN_LEFT, FONT=font0)
  listParamAuto_s=WIDGET_LIST(bLft, VALUE='', XSIZE=230, SCR_YSIZE=160, FONT=font1)

  ;select parameterset values and current values
  bOutputSett=WIDGET_BASE(bRgt, /ROW)
  bQTOtemp=WIDGET_BASE(bOutputSett, /COLUMN, XSIZE=180)
  lblExpTemp=WIDGET_LABEL(bQTOtemp, VALUE='QuickTest output template ', /ALIGN_LEFT, FONT=font0)
  bTempCT=WIDGET_BASE(bQTOtemp, /ROW)
  lblCT=WIDGET_LABEL(bTempCT, VALUE='CT', FONT=font1, XSIZE=60)
  lstCT_s=WIDGET_DROPLIST(bTempCT, VALUE='NA', FONT=font1, XSIZE=100)
  bTempX=WIDGET_BASE(bQTOtemp, /ROW)
  lblX=WIDGET_LABEL(bTempX, VALUE='Xray', FONT=font1, XSIZE=60)
  lstX_s=WIDGET_DROPLIST(bTempX, VALUE='NA', FONT=font1, XSIZE=100)
  bTempNM=WIDGET_BASE(bQTOtemp, /ROW)
  lblNM=WIDGET_LABEL(bTempNM, VALUE='NM planar', FONT=font1, XSIZE=60)
  lstNM_s=WIDGET_DROPLIST(bTempNM, VALUE='NA', FONT=font1, XSIZE=100)
  bTempSPECT=WIDGET_BASE(bQTOtemp, /ROW)
  lblSPECT=WIDGET_LABEL(bTempSPECT, VALUE='SPECT', FONT=font1, XSIZE=60)
  lblSPECTNA=WIDGET_LABEL(bTempSPECT, VALUE='NA', FONT=font1, XSIZE=50)
  bTempPET=WIDGET_BASE(bQTOtemp, /ROW)
  lblPET=WIDGET_LABEL(bTempPET, VALUE='PET', FONT=font1, XSIZE=60)
  lblPETNA=WIDGET_LABEL(bTempPET, VALUE='NA', FONT=font1, XSIZE=50)
  bTempMR=WIDGET_BASE(bQTOtemp, /ROW)
  lblMR=WIDGET_LABEL(bTempMR, VALUE='MR', FONT=font1, XSIZE=60)
  lstMR_s=WIDGET_DROPLIST(bTempMR, VALUE='NA', FONT=font1, XSIZE=100)

  bOutputSettTable=WIDGET_BASE(bOutputSett, /COLUMN)
  lblTopLfRgt=WIDGET_LABEL(bOutputSettTable, VALUE='Output (export) settings ', /ALIGN_LEFT, FONT=font0)
  output_rows=WHERE(configSinfo[2,*] EQ '-1', nOutpRows)
  alignm=INTARR(3,nOutpRows) & alignm[1:2,*]=1
  tblOutputSett=WIDGET_TABLE(bOutputSettTable, XSIZE=3, YSIZE=nOutpRows, COLUMN_LABELS=['Parameter','Saved value','Current value'],$
    COLUMN_WIDTHS=[250,120,120], SCR_XSIZE=500, SCR_YSIZE=120, /NO_ROW_HEADERS, FONT=font1, ALIGNMENT=alignm)
  mlOutp=WIDGET_LABEL(bOutputSett, VALUE='', XSIZE=10)

  bTestSettings=WIDGET_BASE(bRgt, /ROW)
  bTestSettings0=WIDGET_BASE(bTestSettings, XSIZE=180)
  bTestSettings1=WIDGET_BASE(bTestSettings, /COLUMN)
  lblTestsLfRgt=WIDGET_LABEL(bTestSettings1, VALUE='Test settings ', /ALIGN_LEFT, FONT=font0)
  test_rows=WHERE(LONG(configSinfo[2,*]) GE 0, nTestRows)
  alignm=INTARR(4,nTestRows) & alignm[2:3,*]=1
  tblTestSett=WIDGET_TABLE(bTestSettings1, XSIZE=4, YSIZE=nTestRows, COLUMN_LABELS=['Modality/Test','Parameter','Saved value','Current value'],$
    COLUMN_WIDTHS=[100,150,120,120], /NO_ROW_HEADERS, SCR_XSIZE=500, SCR_YSIZE=220,FONT=font1, ALIGNMENT=alignm)

  ;material
  lblMlMat=WIDGET_LABEL(bRgt, VALUE='', YSIZE=10)
  lblTestsLfRgt=WIDGET_LABEL(bRgt, VALUE='Material tables for CT number ', /ALIGN_LEFT, FONT=font0)
  bTitleMaterials=WIDGET_BASE(bRgt, /ROW)
  lblMLMat=WIDGET_LABEL(bTitleMaterials, VALUE='',XSIZE=50)
  lblSaved=WIDGET_LABEL(bTitleMaterials, VALUE='Saved material table (selected parameter set)', XSIZe=300, FONt=font1)
  lblCurr=WIDGET_LABEL(bTitleMaterials, VALUE='Current material table', FONt=font1)
  bMaterials=wIDGET_BASE(bRgt, /ROW)
  lblMLMat2=WIDGET_LABEL(bMaterials, VALUE='',XSIZE=50)
  tblSelMaterials=WIDGET_TABLE(bMaterials, XSIZE=4, YSIZE=5, COLUMN_LABELS=['Material','xpos', 'ypos','Density'],COLUMN_WIDTHS=[80,50,50,70], /NO_ROW_HEADERS, SCR_XSIZE=280, SCR_YSIZE=160,FONT=font1)
  mlTab=WIDGET_LABEL(bMaterials, VALUe='', XSIZE=17)
  tblCurrMaterials=WIDGET_TABLE(bMaterials, VALUE=currMaterials,XSIZE=4,YSIZE=nMaterials, COLUMN_LABELS=['Material','xpos', 'ypos','Density'],COLUMN_WIDTHS=[80,50,50,70], /NO_ROW_HEADERS, SCR_XSIZE=280, SCR_YSIZE=160,FONT=font1)

  s_upd, selConfig-1, 1

  ;************ QT setup *******
  ml1=WIDGET_LABEL(bQTsetup, VALUE='', YSIZE=10)

  qt_infobox0=WIDGET_BASE(bQTsetup, /COLUMN, XSIZE=850)
  qt_info00=WIDGET_LABEL(qt_infobox0, VALUE='QuickTest templates: Define for a given number of images which test(s) to perform on which image.', /ALIGN_LEFT, FONT=font1)
  qt_info01=WIDGET_LABEL(qt_infobox0, VALUE='These templates are created in the main window using MultiMark. Select images and mode and mark images. Save as template.', /ALIGN_LEFT, FONT=font1)
  ml2=WIDGET_LABEL(qt_infobox0, VALUE='', YSIZE=20)

  bRefreshQT=WIDGET_BASE(bQTsetup,/ROW)
  btnRefreshQT=WIDGET_BUTTON(bRefreshQT, VALUE='Refresh to newest version',UVALUE='qt_refresh',XSIZE=200,SENSITIVE=saveOK,FONT=font1,TOOLTIP='Since v1.7.3 can ignore images exceeding the last image marked for testing. Update old templates to the same functionality.')
  ml3=WIDGET_LABEL(bQTsetup, VALUE='', YSIZE=20)

  bAll=WIDGET_BASE(bQTsetup,/ROW)
  bLft=WIDGET_BASE(bAll, /COLUMN, XSIZE=220)
  bMlLftRgt=WIDGET_LABEL(bAll, VALUE='', XSIZE=10)
  bMid=WIDGET_BASE(bAll, /COLUMN, XSIZE=170)
  bMlMidRgt=WIDGET_LABEL(bAll, VALUE='', XSIZE=10)
  bRgt=WIDGET_BASE(bAll, /COLUMN, XSIZE=300)

  ;list of templates
  bModalityQT=WIDGET_BASE(bLft, /ROW)
  lblModalityQT=WIDGET_LABEL(bModalityQT, VALUE='Modality ', FONT=font0,/ALIGN_LEFT)
  lstModality_QT=WIDGET_DROPLIST(bModalityQT, VALUE=TAG_NAMES(testVisualQTNames), UVALUE='qt_lstModality', XSIZE=70, FONT=font1)
  WIDGET_CONTROL, lstModality_QT, SET_DROPLIST_SELECT=defModality
  mlLft0=WIDGET_LABEL(bLft, VALUE='', YSIZE=20)
  lblTopLft=WIDGET_LABEL(bLft, VALUE='QuickTest  templates ', /ALIGN_LEFT, FONT=font0)
  lstTempQT=WIDGET_LIST(bLft, VALUE='', XSIZE=230, SCR_YSIZE=200, UVALUE='qt_listTemp', FONT=font1)
  bBtnsLft=WIDGET_BASE(bLft, /ROW)
  btnDuplicate_qt=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Duplicate template', FONT=font1, UVALUE='qt_duplicate', SENSITIVE=saveOK)
  btnRename_qt=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\rename.bmp',/BITMAP, TOOLTIP='Rename template', FONT=font1, UVALUE='qt_rename', SENSITIVE=saveOK)
  btnUpTemp_qt=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='qt_upTemp', TOOLTIP='Move template upwards in list', SENSITIVE=saveOK)
  btnDownTemp_qt=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='qt_downTemp', TOOLTIP='Move template downwards in list', SENSITIVE=saveOK)
  btnDelete_qt=WIDGET_BUTTON(bBtnsLft, VALUE=thisPath+'images\delete.bmp',/BITMAP, TOOLTIP='Delete selected template', UVALUE='qt_delete', SENSITIVE=saveOK)

  mlLft0=WIDGET_LABEL(bLft, VALUE='', YSIZE=20)
  bAutoList=WIDGET_BASE(bLft, /COLUMN)
  lblAuto=WIDGET_LABEL(bAutoList, VALUE='Selected QuickTest template is used ', /ALIGN_LEFT,FONT=font0)
  lblAuto1=WIDGET_LABEL(bAutoList, VALUE='in these automation templates ', /ALIGN_LEFT,FONT=font0)
  lstQTusedAuto=WIDGET_LIsT(bAutoList, XSIZE=70, SCR_XSIZE=150, YSIZE=1, SCR_YSIZE=150, FONT=font1)

  bQTlist=WIDGET_BASE(bMid, /COLUMN)
  lblQT=WIDGET_LABEL(bQTlist, VALUE='QuickTest template ', FONT=font0)
  lstQT=WIDGET_LIST(bQTlist, XSIZE=70, SCR_XSIZE=100, YSIZE=1, SCR_YSIZE=200, FONT=font1, UVALUE='qtList');, /CONTEXT_EVENTS)

  bTestlist=WIDGET_BASE(bRgt, /COLUMN)
  lblTestlist=WIDGET_LABEL(bTestlist, VALUE='Test numbers and names', FONT=font0)
  lstTest_qt=WIDGET_LIST(bTestlist, XSIZE=170, SCR_XSIZE=170, SCR_YSIZE=150, FONT=font1, SENSITIVE=0)
  bNimgTest=WIDGET_BASE(bRgt, /ROW)
  lblNimgTest=WIDGET_LABEL(bNimgTest, VALUE='Minimum number of images expected:', FONT=font1)
  txtNimgTest_qt=WIDGET_TEXT(bNimgTest, VALUE='', XSIZE=5, FONT=font1, SENSITIVE=0)

  qt_upd, 0

  ;**************** QT output setup *******************

  qto_infobox0=WIDGET_BASE(bQTout, /COLUMN, XSIZE=850, FRAME=1)
  qto_info0=WIDGET_LABEL(qto_infobox0, VALUE='QuickTest output templates:', /ALIGN_LEFT, FONT=font0)
  qto_info1=WIDGET_LABEL(qto_infobox0, VALUE='These templates define for each test what to output to text when using QuickTest.', /ALIGN_LEFT, FONT=font1)
  qto_info2=WIDGET_LABEL(qto_infobox0, VALUE='The "alternative" column refers to the fact that some tests will have different calculations depending on the test parameters chosen.', /ALIGN_LEFT, FONT=font1)

  QTsetupBoxTop=WIDGET_BASE(bQTout, /ROW)
  ml0=WIDGET_LABEL(QTsetupBoxTop, VALUE='', xSIZE=5)

  bRgt=WIDGET_BASE(QTsetupBoxTop, /COLUMN)
  bMl=WIDGET_BASE(QTsetupBoxTop, XSIZE=30)
  bLft=WIDGET_BASE(QTsetupBoxTop, /COLUMN)

  bModality=WIDGET_BASE(bRgt, /ROW)
  lblModality=WIDGET_LABEL(bModality, VALUE='Modality ', FONT=font0,/ALIGN_LEFT)
  lstModality_qto=WIDGET_DROPLIST(bModality, VALUE=TAG_NAMES(testVisualQTNames), UVALUE='qto_lstModality', XSIZE=70, FONT=font1)

  bTemp=WIDGET_BASE(bRgt, /COLUMN, YSIZE=170)
  lblTemp=WIDGET_LABEL(bTemp, VALUE='Templates ', FONT=font0,/ALIGN_LEFT)
  lstTemplates_qto=WIDGET_LIST(bTemp, VALUE='', UVALUE='qto_lstTemp', XSIZE=100, SCR_XSIZE=130, SCR_YSIZE=150, FONT=font1)
  WIDGET_CONTROL, lstTemplates_qto, SET_LIST_SELECT=0
  bTempEdit=WIDGET_BASE(bRgt, /ROW)
  btnDupliTemp_qto=WIDGET_BUTTON(bTempEdit, VALUE=thisPath+'images\copy.bmp' ,/BITMAP, TOOLTIP='Duplicate', UVALUE='qto_duplicate', FONT=font1, SENSITIVE=saveOK)
  btnRenameTemp_qto=WIDGET_BUTTON(bTempEdit, VALUE=thisPath+'images\rename.bmp' ,/BITMAP, TOOLTIP='Rename', UVALUE='qto_rename', FONT=font1, SENSITIVE=saveOK)
  btnDeleteTemp_qto=WIDGET_BUTTON(bTempEdit, VALUE=thisPath+'images\delete.bmp' ,/BITMAP, TOOLTIP='Delete', UVALUE='qto_delete', FONT=font1, SENSITIVE=saveOK)

  ml10= WIDGET_LABEL(bLft, VALUE='', YSIZE=29)
  lbltblQTout=WIDGET_LABEL(bLft, VALUE='Selected output template ', FONT=font0, /ALIGN_LEFT)
  tblQTout=WIDGET_TABLE(bLft, XSIZE=6, YSIZE=100, COLUMN_LABELS=['Test', 'Alternative', 'Columns', 'Calculation', 'Pr img or series','Description'], COLUMN_WIDTHS=[100,100,100,70,100,190], /NO_ROW_HEADERS, SCR_XSIZE=100*6+90, SCR_YSIZE=170, /ALL_EVENTS, FONT=font1)

  bEdit=WIDGET_BASE(bLft, /ROW)
  lstTest=WIDGET_DROPLIST(bEdit, VALUE=testVisualQTNames.(defModality), XSIZE=98, FONT=font1,UVALUE='qto_lstTest')
  lstAlt=WIDGET_DROPLIST(bEdit, VALUE=TAG_NAMES(tableHeaders.(defModality).(0)), XSIZE=98, FONT=font1,UVALUE='qto_lstAlt')
  lstCols=WIDGET_LIST(bEdit, VALUE=tableHeaders.(defModality).(0).(0), /MULTIPLE, SCR_XSIZE=98, SCR_YSIZE=100, FONT=font1)
  lstCalc=WIDGET_DROPLIST(bEdit, VALUE=['=', 'Min','Max','Avg','Std','Max abs'], XSIZE=68, FONT=font1)
  lstPer=WIDGET_DROPLIST(bEdit, VALUE=['Per image', 'Per series'], XSIZE=98, FONT=font1)

  bLowRgt=WIDGET_BASE(bEdit, /COLUMN)
  txtDescr=WIDGET_TEXT(bLowRgt, VALUE='', XSIZE=100, SCR_XSIZE=190, YSIZE=1, SCR_YSIZE=20, FONT=font1, /EDITABLE)
  bEditBtns=WIDGET_BASE(bLowRgt, /ROW, YSIZE=28)
  ml2=WIDGET_LABEL(bEditBtns, VALUE='', XSIZE=80)
  btnAddQTO=WIDGET_BUTTON(bEditBtns, VALUE=thisPath+'images\plus.bmp' ,/BITMAP,TOOLTIP='Add as new output', UVALUE='qto_add', FONT=font1, SENSITIVE=saveOK)
  btnEditQTO=WIDGET_BUTTON(bEditBtns, VALUE=thisPath+'images\save.bmp', /BITMAP, TOOLTIP='Overwrite selected table row', UVALUE='qto_overwrite', FONT=font1, SENSITIVE=saveOK)
  btnDelQTO=WIDGET_BUTTON(bEditBtns, VALUE=thisPath+'images\delete.bmp' ,/BITMAP, TOOLTIP='Delete output', UVALUE='qto_deleteOutp', FONT=font1, SENSITIVE=saveOK)

  bParamList=WIDGET_BASE(bQTout, /COLUMN)
  lblParam=WIDGET_LABEL(bParamList, VALUE='Selected QuickTest output template is used ', /ALIGN_LEFT,FONT=font0)
  lblParam1=WIDGET_LABEL(bParamList, VALUE='in these parameter sets (automation templates in paranthesis) ', /ALIGN_LEFT,FONT=font0)
  lstQTOusedParam=WIDGET_LIsT(bParamList, XSIZE=300, SCR_XSIZE=300, YSIZE=1, SCR_YSIZE=200, FONT=font1)

  WIDGET_CONTROL, lstModality_qto, SET_DROPLIST_SELECT=defModality & qto_currMod=defModality & qto_updMode

  ;***************** Auto setup *********************

  ml0=WIDGET_LABEL(bAuto, VALUE='', YSIZE=20)
  autobox1=WIDGET_BASE(bAuto, /COLUMN, XSIZE=890)
  auto_infobox0=WIDGET_BASE(autobox1, /COLUMN, XSIZE=800, FRAME=1)

  info1=WIDGET_LABEL(auto_infobox0, VALUE='Automation templates can be used for automating analysis combining QuickTest template, parameter set (including QuickTest output link),', /ALIGN_LEFT, FONT=font1)
  info1=WIDGET_LABEL(auto_infobox0, VALUE='specify path for input and output and specify sorting of the input images.', /ALIGN_LEFT, FONT=font1)
  infoML=WIDGET_LABEL(autobox1, VALUE='', YSIZE=20)

  bAutoImportPath=WIDGET_BASE(autobox1,/ROW)
  lblAutoImpPath=WIDGET_LABEL(bAutoImportPath, VALUE='Default path where new files for import can be found:', FONT=font1)
  txtAutoImpPath=WIDGET_TEXT(bAutoImportPath, VALUE=configS.(1).AUTOIMPORTPATH, XSIZE=70,/EDITABLE, FONT=font1)
  btnAutoImpPath=WIDGET_BUTTON(bAutoImportPath, VALUE='Browse', UVALUE='aimp_Browse',  FONT=font1)
  btnSaveImpPath=WIDGET_BUTTON(bAutoImportPath, VALUE=thisPath+'images\save.bmp',/BITMAP, UVALUE='aimp_Save',  FONT=font1)

infoML1=WIDGET_LABEL(autobox1, VALUE='', YSIZE=20)

  bContA=WIDGET_BASE(autobox1,/ROW)
  ;templates
  bTemp=WIDGET_BASE(bContA, /COLUMN)
  bModalityA=WIDGET_BASE(bTemp, /ROW)
  lblModalityA=WIDGET_LABEL(bModalityA, VALUE='Modality ', FONT=font0,/ALIGN_LEFT)
  lstModality_a=WIDGET_DROPLIST(bModalityA, VALUE=TAG_NAMES(testVisualQTNames), UVALUE='a_lstModality', XSIZE=70, FONT=font1)
  WIDGET_CONTROL, lstModality_a, SET_DROPLIST_SELECT=defModality
  mlLft0=WIDGET_LABEL(bTemp, VALUE='', YSIZE=20)
  lblTempA=WIDGET_LABEL(bTemp, VALUE='Automation Templates:', /ALIGN_LEFT, FONT=font0)
  listTemp_a=WIDGET_LIST(bTemp, VALUE='', SCR_XSIZE=150, FONT=font1, SCR_YSIZE=150, UVALUE='a_listTemp')

  bButtEndTemp=WIDGET_BASE(bTemp, /ROW)
  btnAddTemp_a=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\plus.bmp',/BITMAP, UVALUE='a_add', TOOLTIP='Add new template', SENSITIVE=saveOK)
  btnOverWriteAuto=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\save.bmp',/BITMAP,UVALUE='a_overwrite', TOOLTIP='Overwrite selected with new values', SENSITIVE=saveOK)
  btnDupliTemp_a=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\copy.bmp' ,/BITMAP, UVALUE='a_duplicate', TOOLTIP='Duplicate', SENSITIVE=saveOK)
  btnRenameTemp_a=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\rename.bmp', /BITMAP, UVALUE='a_rename', TOOLTIP='Rename template', SENSITIVE=saveOK)
  btnUpTemp_a=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='a_upTemp', TOOLTIP='Move template upwards in list', SENSITIVE=saveOK)
  btnDownTemp_a=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='a_downTemp', TOOLTIP='Move template downwards in list', SENSITIVE=saveOK)
  btnDelTemp_a=WIDGET_BUTTON(bButtEndTemp, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='a_delTemp', TOOLTIP='Delete template', SENSITIVE=saveOK)

  mlBtnSave=WIDGET_LABEL(bTemp, VALUE='', YSIZE=20)
  btnRunAuto=WIDGET_BUTTON(bTemp, VALUE='Run selected', UVALUE='a_run', FONT=font1)

  bMlA=WIDGET_BASE(bContA, XSIZE=20)

  ;framed automation parameters
  bAutoParam=WIDGET_BASE(bContA,/COLUMN,FRAME=1, /ALIGN_LEFT)

  ;stationname
  bStatName=WIDGET_BASE(bAutoParam, /ROW)
  lblStatName=WIDGET_LABEL(bStatName, VALUE='Station name (DICOM 0008,1010):', FONT=font1)
  txtStatName_a=WIDGET_TEXT(bStatName, VALUE='', XSIZE=20,/EDITABLE, FONT=font1, UVALUE='txtStatName_a',/KBRD_FOCUS_EVENTS)
  btnStatName=WIDGET_BUTTON(bStatName, VALUE='Retrieve from DICOM file', UVALUE='a_getStatName', FONT=font1)
  lblStatName2=WIDGET_LABEL(bStatName, VALUE='(Used with the import option.)', FONT=font1)
  ml5=WIDGET_LABEL(bAutoParam, VALUE='', YSIZE=15)

  ;path
  bBrowse=WIDGET_BASE(bAutoParam, /ROW)
  lblBrowse=WIDGET_LABEL(bBrowse, VALUE='Path:', FONT=font1)
  txtBrowse_a=WIDGET_TEXT(bBrowse, VALUE='', XSIZE=70,/EDITABLE, FONT=font1, UVALUE='txtBrowse_a',/KBRD_FOCUS_EVENTS)
  btnBrowse=WIDGET_BUTTON(bBrowse, VALUE='Browse', UVALUE='a_Browse',  FONT=font1)
  lblBrowse2=WIDGET_LABEL(bAutoParam, VALUE='  Info: Where to look for the images', FONT=font1, /ALIGN_LEFT)
  ml4=WIDGET_LABEL(bAutoParam, VALUE='', YSIZE=15)

  bOpenDetails=WIDGET_BASE(bAutoParam, /Row)
  bSortBy=WIDGET_BASE(bOpenDetails, XSIZE=355,/COLUMN, FRAME=1)
  lblSortBy=WIDGET_LABEL(bSortBy, VALUE='Add elements from DICOM-header to sort the images by', FONT=font1)
  bSortByLists=WIDGET_BASE(bSortBy, /ROW)
  listElem=WIDGET_DROPLIST(bSortByLists, VALUE='', XSIZE=150, FONT=font1)
  bButtMid=WIDGET_BASE(bSortByLists, /COLUMN)
  btnAddElem=WIDGET_BUTTON(bButtMid, VALUE='>>', UVALUE='a_addElem', FONT=font1)
  listSort=WIDGET_LIST(bSortByLists, VALUE='', XSIZE=20, FONT=font1, SCR_YSIZE=100)
  bButtEndSort=WIDGET_BASE(bSortByLists, /COLUMN)
  btnDelElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='a_delElem', TOOLTIP='Delete selected element from sort list')
  btnUpElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='a_upElem', TOOLTIP='Move element upwards in sort list')
  btnDownElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='a_downElem', TOOLTIP='Move element downwards in sort list')
  btnAscElem=WIDGET_BUTTON(bButtEndSort, VALUE=thisPath+'images\sort.bmp',/BITMAP, UVALUE='a_sort_asc', TOOLTIP='Reverse sort order')

  bOpenDetailsRgt=WIDGET_BASE(bOpenDetails, /COLUMN)
  bBtnInclSub=WIDGET_BASE(bOpenDetailsRgt, /NONEXCLUSIVE)
  btnInclSub=WIDGET_BUTTON(bBtnInclSub, VALUE='Include images in subfolders', FONT=font1, UVALUE='btnInclSub')
  btnOnlyLastDate=WIDGET_BUTTON(bBtnInclSub, VALUE='Last date images only (file creation date).', FONT=font1, UVALUE='btnOnlyLastDate')
  lblOpenDetInfo=WIDGET_LABEL(bOpenDetailsRgt, VALUE='Hint: Include images in subfolders useful', FONT=font1, /ALIGN_LEFT)
  lblOpenDetInfo1=WIDGET_LABEL(bOpenDetailsRgt, VALUE='      when re-analysing the full Archive.', FONT=font1, /ALIGN_LEFT)

  ml60=WIDGET_LABEL(bAutoParam, VALUe='', YSIZE=30)
  ;parameter set
  bSets=WIDGET_BASE(bAutoParam, /ROW)
  lblSets=WIDGET_LABEL(bSets, VALUE='Use parameter set:', XSIZE=150, FONT=font1)
  listSets_a=WIDGET_DROPLIST(bSets, VALUE=paramSetNames, XSIZE=180, FONT=font1)
  WIDGET_CONTROL, listSets_a, SET_DROPLIST_SELECT=selParam

  ml6=WIDGET_LABEL(bAutoParam, VALUe='', YSIZE=15)

  bQT2=WIDGET_BASE(bAutoParam,/COLUMN)
  ;quickTemp list
  bQTlist=WIDGET_BASE(bQT2, /ROW)
  lblQT=WIDGET_LABEL(bQTlist, VALUE='Use QuickTest template:', XSIZE=150, FONT=font1)
  listQT_a=WIDGET_DROPLIST(bQTlist, VALUE='', XSIZE=180, FONT=font1)

  bQTalt=WIDGET_BASE(bAutoParam, /ROW)
  bQTalt0=WIDGET_BASE(bQTalt, XSIZE=30)
  bQTalt1=WIDGET_BASE(bQTalt, /COLUMN)

  ;append resultfile
  lblBrowseApp=WIDGET_LABEL(bQTalt1, VALUE='Append results (row, no headers) to this file:', FONT=font1, /ALIGN_LEFT)
  bBrowseApp=WIDGET_BASE(bQTalt1, /ROW)
  lblmlBrowseApp=WIDGET_LABEL(bBrowseApp, VALUE='', XSIZE=30)
  txtBrowseApp=WIDGET_TEXT(bBrowseApp, VALUE=pathApp, /EDITABLE, XSIZE=50, FONT=font1, UVALUE='txtBrowseApp',/KBRD_FOCUS_EVENTS)
  btnBrowseApp=WIDGET_BUTTON(bBrowseApp, VALUE='Browse', UVALUE='a_BrowseApp', FONT=font1)
  btnClearApp=WIDGET_BUTTON(bBrowseApp, VALUE='Clear', UVALUE='a_ClearApp', FONT=font1)
  btnShowApp=WIDGET_BUTTON(bBrowseApp, VALUE='Open', UVALUE='a_OpenApp', FONT=font1)

  bMoveFiles=WIDGET_BASE(bQTalt1, /NONEXCLUSIVE)
  btnMoveFiles=WIDGET_BUTTON(bMoveFiles, VALUE='Move images automatically to folder named "Archive" when finished calculation.', FONT=font1, UVALUE='btnMoveFiles')
  btnDeleteFiles=WIDGET_BUTTON(bMoveFiles, VALUE='Delete files not accepted (i.e. SR reports, images with no acquisition date).', FONT=font1, UVALUE='btnDeleteFiles')
  btnDeleteFilesEnd=WIDGET_BUTTON(bMoveFiles, VALUE='Send files (images) exceeding the last image marked for testing to the recycle bin.', FONT=font1, UVALUE='btnDeleteFilesEnd')

  auto_warningBox=WIDGET_BASE(autobox1, XSIZE=800, /ROW, MAP=0)
  warnimg=WIDGET_BUTTON(auto_warningBox, VALUE=thisPath+'images\warning.bmp', /BITMAP)
  auto_warningBox2=WIDGET_BASE(auto_warningBox, /COLUMN)
  warningTxt1=WIDGET_LABEL(auto_warningBox2, /ALIGN_LEFT, VALUE='', /DYNAMIC_RESIZE)
  warningTxt2=WIDGET_LABEL(auto_warningBox2, /ALIGN_LEFT, VALUE='', /DYNAMIC_RESIZE)

  selecTemp_a=0
  a_currMod=defModality
  auto_upd, selecTemp_a, 1
  
  ;************** Rename DICOM ***************'
  
  ml_Rd1=WIDGET_LABEL(bRename, VALUE='', YSIZE=20)
  rd_infobox0=WIDGET_BASE(bRename, /COLUMN, XSIZE=850, FRAME=1)
  rd_info0=WIDGET_LABEL(rd_infobox0, VALUE='Settings and template manager for RenameDICOM ', /ALIGN_LEFT, FONT=font0)
  rd_info1=WIDGET_LABEL(rd_infobox0, VALUE='RenameDICOM is a standalone application, but now also included in ImageQC for your convenience.', /ALIGN_LEFT, FONT=font1)
  rd_info2=WIDGET_LABEL(rd_infobox0, VALUE='Define dicom tags to be used for renaming and define templates on how to combine these to generate reconizable filenames.', /ALIGN_LEFT, FONT=font1)
  rd_info3=WIDGET_LABEL(rd_infobox0, VALUE='This is the template manager. Templates are built within the RenameDICOM window. Find it from the file menu.', /ALIGN_LEFT, FONT=font1)
  
  ml_Rd2=WIDGET_LABEL(bRename, VALUE='', YSIZE=20)

  bRenameMid=WIDGET_BASE(bRename, /ROW)
  mlMid1=WIDGET_LABEL(bRenameMid, VALUE='', XSIZE=10)

  bTbl_rde=WIDGET_BASE(bRenameMid, /COLUMN)
  lbl_tbl_rde=WIDGET_LABEL(bTbl_rde, VALUE='DICOM tags ', /ALIGN_LEFT, FONT=font0)
  tbl_rde=WIDGET_TABLE(bTbl_rde, XSIZE=5, YSIZE=100, SCR_XSIZE=550, SCR_YSIZE=300,COLUMN_WIDTHS=[150,60,70,110,130], /NO_ROW_HEADERS, COLUMN_LABELS=['Tag description','Tag group','Tag element','Default format code','for selected template'], UVALUE='tbl_rde',/ALL_EVENTS)
  
  bButt_tbl_rde=WIDGET_BASE(bTbl_rde, /ROW)
  btnAdd_rde=WIDGET_BUTTON(bButt_tbl_rde, VALUE=thisPath+'images\plus.bmp',/BITMAP, UVALUE='rde_add', TOOLTIP='Add new DICOM tag to list', SENSITIVE=saveOK)
  btnOverWrite_rde=WIDGET_BUTTON(bButt_tbl_rde, VALUE=thisPath+'images\edit.bmp',/BITMAP,UVALUE='rde_overwrite', TOOLTIP='Overwrite selected tag with new values.', SENSITIVE=saveOK)
  btnDupli_rde=WIDGET_BUTTON(bButt_tbl_rde, VALUE=thisPath+'images\copy.bmp' ,/BITMAP, UVALUE='rde_duplicate', TOOLTIP='Duplicate', SENSITIVE=saveOK)
  btnUp_rde=WIDGET_BUTTON(bButt_tbl_rde, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='rde_upTemp', TOOLTIP='Move upwards in list', SENSITIVE=saveOK)
  btnDown_rde=WIDGET_BUTTON(bButt_tbl_rde, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='rde_downTemp', TOOLTIP='Move downwards in list', SENSITIVE=saveOK)
  btnDel_rde=WIDGET_BUTTON(bButt_tbl_rde, VALUE=thisPath+'images\delete.bmp',/BITMAP, UVALUE='rde_delTemp', TOOLTIP='Delete element', SENSITIVE=saveOK)
  
  mlMid2=WIDGET_LABEL(bRenameMid, VALUE='', XSIZE=10)
  bFormatInfo=WIDGET_BASE(bRenameMid,/COLUMN)
  rdf_info0=WIDGET_LABEL(bFormatInfo, VALUE='Format code explained ', /ALIGN_LEFT, FONT=font0)
  
  rdf_info1=WIDGET_LABEL(bFormatInfo, VALUE='Text: a<#letters>', /ALIGN_LEFT, FONT=font1)
  rdf_info2=WIDGET_LABEL(bFormatInfo, VALUE='Integer: i<#digits>', /ALIGN_LEFT, FONT=font1)
  rdf_info3=WIDGET_LABEL(bFormatInfo, VALUE='Float: f<#digits>.<#decimals>', /ALIGN_LEFT, FONT=font1)
  rdf_info4=WIDGET_LABEL(bFormatInfo, VALUE='Specify # as 0 means all.', /ALIGN_LEFT, FONT=font1)
  rdf_info5a=WIDGET_LABEL(bFormatInfo, VALUE='For DICOM tags with \ you can extract one or more of', /ALIGN_LEFT, FONT=font1) 
  rdf_info5b=WIDGET_LABEL(bFormatInfo, VALUE='these separating format codes with \ and _ for ignore part.', /ALIGN_LEFT, FONT=font1)
  rdf_info6=WIDGET_LABEL(bFormatInfo, VALUE='', /ALIGN_LEFT, YSIZE=10)
  rdf_info6a=WIDGET_LABEL(bFormatInfo, VALUE='Examples:', /ALIGN_LEFT, FONT=font1)
  rdf_info7=WIDGET_LABEL(bFormatInfo, VALUE='testing123:  a3=tes, a0=testing123', /ALIGN_LEFT, FONT=font1)
  rdf_info8=WIDGET_LABEL(bFormatInfo, VALUE='123:           i4= 123, i04=0123, i2=xx', /ALIGN_LEFT, FONT=font1)
  rdf_info9=WIDGET_LABEL(bFormatInfo, VALUE='123.123:     f0.1=123.1, f2.1=xx', /ALIGN_LEFT, FONT=font1)
  rdf_info10=WIDGET_LABEL(bFormatInfo, VALUE='0\1\3:     _\_\i0=3, i0\_\_=0, i0\i0\i0=0_1_3', /ALIGN_LEFT, FONT=font1)
  rdf_info11=WIDGET_LABEL(bFormatInfo, VALUE='', /ALIGN_LEFT, YSIZE=10)
  rdf_info12=WIDGET_LABEL(bFormatInfo, VALUE='Note that when renaming folders special characters ', /ALIGN_LEFT, FONT=font1)
  rdf_info13=WIDGET_LABEL(bFormatInfo, VALUE='including . is converted to characters valid for folder names.', /ALIGN_LEFT, FONT=font1)
  
  ml_info=WIDGET_LABEL(bFormatInfo, VALUE='', /ALIGN_LEFT, YSIZE=10)
  bTestFormat=WIDGET_BASE(bFormatInfo, /ROW)
  lblInputTest=WIDGET_LABEL(bTestFormat, VALUE='Input string:',FONT=font1,XSIZE=70)
  txtInputTest=WIDGET_TEXT(bTestFormat, VALUE='123',FONT=font1, XSIZE=10,/EDITABLE)
  lblFormatTest=WIDGET_LABEL(bTestFormat, VALUE='   Format code:',FONT=font1)
  txtFormatTest=WIDGET_TEXT(bTestFormat, VALUE='i05',FONT=font1, XSIZE=10,/EDITABLE)
  bTestFormat2=WIDGET_BASE(bFormatInfo, /ROW)
  lblOutputTest=WIDGET_LABEL(bTestFormat2, VALUE='Output:',FONT=font1,XSIZE=70)
  txtOutputTest=WIDGET_TEXT(bTestFormat2, VALUE='00123',FONT=font1, XSIZE=15)
  btnFormatTestUpdate=WIDGET_BUTTON(bTestFormat2, VALUE='Update',FONT=font1,UVALUE='formatTestUpdate')
  
  ml_Rd3=WIDGET_LABEL(bRename, VALUE='', YSIZE=20)
  bRenameLow=WIDGET_BASE(bRename, /ROW)
  mlLow1=WIDGET_LABEL(bRenameLow, VALUE='', XSIZE=20)
  bList_rdt=WIDGET_BASE(bRenameLow, /COLUMN)
  lblTop_rdt=WIDGET_LABEL(bList_rdt, VALUE='Rename templates ', /ALIGN_LEFT, FONT=font0)
  lstTemp_rdt=WIDGET_LIST(bList_rdt, VALUE='', XSIZE=50, SCR_XSIZE=200, SCR_YSIZE=100, UVALUE='rdt_listTemp', FONT=font1)
  bBtns_rdt=WIDGET_BASE(bList_rdt, /ROW)
  btnDuplicate_rdt=WIDGET_BUTTON(bBtns_rdt, VALUE=thisPath+'images\copy.bmp',/BITMAP, TOOLTIP='Duplicate template', FONT=font1, UVALUE='rdt_duplicate', SENSITIVE=saveOK)
  btnRename_rdt=WIDGET_BUTTON(bBtns_rdt, VALUE=thisPath+'images\rename.bmp',/BITMAP, TOOLTIP='Rename template', FONT=font1, UVALUE='rdt_rename', SENSITIVE=saveOK)
  btnUpTemp_rdt=WIDGET_BUTTON(bBtns_rdt, VALUE=thisPath+'images\switch_up.bmp',/BITMAP, UVALUE='rdt_upTemp', TOOLTIP='Move template upwards in list', SENSITIVE=saveOK)
  btnDownTemp_rdt=WIDGET_BUTTON(bBtns_rdt, VALUE=thisPath+'images\switch_down.bmp',/BITMAP, UVALUE='rdt_downTemp', TOOLTIP='Move template downwards in list', SENSITIVE=saveOK)
  btnDelete_rdt=WIDGET_BUTTON(bBtns_rdt, VALUE=thisPath+'images\delete.bmp',/BITMAP, TOOLTIP='Delete selected template', UVALUE='rdt_delete', SENSITIVE=saveOK)

  bShow_rdt=WIDGET_BASE(bRenameLow,/COLUMN)
  bCat_rdt=WIDGET_BASE(bShow_rdt,/ROW)
  lblCat_rdt=WIDGET_LABEL(bCat_rdt, VALUE='Subfolder name template:', XSIZE=120)
  txtCat_rdt=WIDGET_TEXT(bCat_rdt, VALUE='', XSIZE=500, SCR_XSIZE=500)
  bFile_rdt=WIDGET_BASE(bShow_rdt,/ROW)
  lblFile_rdt=WIDGET_LABEL(bFile_rdt, VALUE='Filename template:', XSIZE=120)
  txtFile_rdt=WIDGET_TEXT(bFile_rdt, VALUE='', XSIZE=500, SCR_XSIZE=500)

  updRDT, 0
  updRDE, 0

  ;********** Info ***********
  p_1=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10)
  iinfo0=WIDGET_LABEL(bInfo, VALUE='The data\config.dat file contain the user settings described below. Consider a backup of the config file before upgrading to a newer version of ImageQC.', /ALIGN_LEFT, FONT=font1)
  p_2=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10)
  param_infobox=WIDGET_BASE(bInfo, /COLUMN, XSIZE=850, FRAME=1)
  p_info0=WIDGET_LABEL(param_infobox, VALUE='Parameter sets:', /ALIGN_LEFT, FONT=font0)
  p_info1=WIDGET_LABEL(param_infobox, VALUE='Selected values defining how to perform the different tests (f.x. ROI size and positions, choises made) can be saved within a parameter set.', /ALIGN_LEFT, FONT=font1)
  p_info2=WIDGET_LABEL(param_infobox, VALUE='In addition these parameters are saved along with the parameter set:', /ALIGN_LEFT, FONT=font1)
  p_ml3=WIDGET_LABEL(param_infobox , VALUE='', YSIZE=5)
  p_info3=WIDGET_LABEL(param_infobox, VALUE='- the general export options (include or exclude headers, transpose tables, decimal mark) - set in main window', /ALIGN_LEFT, FONT=font1)
  p_info4=WIDGET_LABEL(param_infobox, VALUE='- include or exclude filename in QuickTest export - set in main window', FONT=font1, /ALIGN_LEFT)
  p_info6=WIDGET_LABEL(param_infobox, VALUE='- QuickTest export templates to be used - set in the parameter set manager (this dialog)', FONT=font1, /ALIGN_LEFT)
  p_info5=WIDGET_LABEL(param_infobox, VALUE='- default append or not when opening new files - set in main window', FONT=font1, /ALIGN_LEFT)
  p_ml3=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10)

  qt_infobox=WIDGET_BASE(bInfo, /COLUMN, XSIZE=850, FRAME=1)
  qt_info0=WIDGET_LABEL(qt_infobox, VALUE='QuickTest templates:', /ALIGN_LEFT, FONT=font0)
  qt_info2=WIDGET_LABEL(qt_infobox, VALUE='The QuickTest template define, for a given number of images, which test(s) should be performed on which image(s).', /ALIGN_LEFT, FONT=font1)
  qt_info1=WIDGET_LABEL(qt_infobox, VALUE='Pressing QuickTest button will run all defined tests on the selected images and generate a text-output according to the QuickTest output template.', /ALIGN_LEFT, FONT=font1)
  qt_info2b=WIDGET_LABEL(qt_infobox, VALUE='QuickTest is only possible for the numbered tests (tab named with a number before the test name).', /ALIGN_LEFT, FONT=font1)
  qt_info3=WIDGET_LABEL(qt_infobox, VALUE='To create a QuickTest template: open an image set, turn on the MultiMark option,', /ALIGN_LEFT, FONT=font1)
  qt_info4=WIDGET_LABEL(qt_infobox, VALUE='   select one of the tabs with the (numbered) tests to perform, select the images to perform this test on, mark these images (right-click in list and mark).', FONT=font1, /ALIGN_LEFT)
  qt_info6=WIDGET_LABEL(qt_infobox, VALUE='   The number of the test will be the marking in the list. Repeat for all tests to perform on the image set and save.', FONT=font1, /ALIGN_LEFT)
  qt_info5=WIDGET_LABEL(qt_infobox, VALUE='The QuickTest template manager (in this dialogbox) can only be used to view, delete, reorganise or rename the templates.', FONT=font1, /ALIGN_LEFT)
  qt_ml3=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10)

  qto_infobox=WIDGET_BASE(bInfo, /COLUMN, XSIZE=850, FRAME=1)
  qto_info0=WIDGET_LABEL(qto_infobox, VALUE='QuickTest output templates:', /ALIGN_LEFT, FONT=font0)
  qto_info1=WIDGET_LABEL(qto_infobox, VALUE='These templates define for each test what to output to text when using the QuickTest.', /ALIGN_LEFT, FONT=font1)
  qto_info2=WIDGET_LABEL(qto_infobox, VALUE='The "alternative" column refers to the fact that some tests will have different calculations depending on the test parameters chosen.', /ALIGN_LEFT, FONT=font1)
  qto_ml3=WIDGET_LABEL(bInfo , VALUE='', YSIZE=10)

  auto_infobox=WIDGET_BASE(bInfo, /COLUMN, XSIZE=850, FRAME=1)
  a_info0=WIDGET_LABEL(auto_infobox, VALUE='Automation Templates:', /ALIGN_LEFT, FONT=font0)
  a_info1=WIDGET_LABEL(auto_infobox, VALUE='Automation templates can be used for automating analysis combining QuickTest template, parameter set (including QuickTest output link),', /ALIGN_LEFT, FONT=font1)
  a_info2=WIDGET_LABEL(auto_infobox, VALUE='specify path for input and output and specify sorting of the input images.', /ALIGN_LEFT, FONT=font1)
  a_info3=WIDGET_LABEL(auto_infobox, VALUE='The benefit is found when frequently analysing repeated image sets (i.e. constancy tests).', /ALIGN_LEFT, FONT=font1)
  a_ml2=WIDGET_LABEL(auto_infobox , VALUE='', YSIZE=5)
  a_info4=WIDGET_LABEL(auto_infobox, VALUE='Input/output paths ', FONT=font1, /ALIGN_LEFT)
  a_info5=WIDGET_LABEL(auto_infobox, VALUE='- specify the path where the images will be found or sent to if the Import option for automation is used based on the station name in the DICOM header.', FONT=font1, /ALIGN_LEFT)
  a_info6=WIDGET_LABEL(auto_infobox, VALUE='- specify the sorting of the incoming images based on DICOM tags to facilitate the same order of the images when analysed.' , FONT=font1, /ALIGN_LEFT)
  a_ml3=WIDGET_LABEL(auto_infobox , VALUE='', YSIZE=5)
  a_info7=WIDGET_LABEL(auto_infobox, VALUE='If a QuickTest template is selected the calculation will start automatically and generate results to clipboard.', FONT=font1, /ALIGN_LEFT)
  a_info8=WIDGET_LABEL(auto_infobox, VALUE='The images will be sorted on acquisition date first and the results will be generated per acquisition date found.', FONT=font1, /ALIGN_LEFT)
  a_info9=WIDGET_LABEL(auto_infobox, VALUE='If a result file is specified the results will be appended to this according to the output template (1 row pr date, no headers)', FONT=font1, /ALIGN_LEFT)
  a_info10=WIDGET_LABEL(auto_infobox, VALUE='else the program will pause for each date and give the opportunity to paste the results into any text-file or Excel.', FONT=font1, /ALIGN_LEFT)
  
  rename_infobox=WIDGET_BASE(bInfo, /COLUMN, XSIZE=850, FRAME=1)
  r_info0=WIDGET_LABEL(rename_infobox, VALUE='Rename Dicom Templates:', /ALIGN_LEFT, FONT=font0)
  r_info1=WIDGET_LABEL(rename_infobox, VALUE='DICOM files often have names that do not reflect the content. Rename the DICOM files based on information in the DICOM header.', /ALIGN_LEFT, FONT=font1)
  r_info2=WIDGET_LABEL(rename_infobox, VALUE='Specify DICOM tags that should be available for building reasonable names and create templates for automatic renaming (within Rename DICOM).', /ALIGN_LEFT, FONT=font1)
  ;*************

  lblMlButt=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=20)
  bBtmButt=WIDGET_BASE(settingsbox, /ROW)
  btnCleanBlock=WIDGET_BUTTON(bBtmButt, VALUE='Remove blocking', TOOLTIP='Remove timestamp that blocks ability to save to config file.', UVALUE='remBlock')
  mlBtmButt=WIDGET_LABEL(bBtmButt, VALUE='', XSIZE=350)
  btnSaveConfigBackup=WIDGET_BUTTON(bBtmButt, VALUE='Backup', TOOLTIP='Backup config file', UVALUE='backupConfig')
  btnRestoreConfig=WIDGET_BUTTON(bBtmButt, VALUE='Restore', TOOLTIP='Restore and replace config file with backup config file', UVALUE='restoreConfig')
  btnExpXMLConfig=WIDGET_BUTTON(bBtmButt, VALUE='Export to XML', TOOLTIP='Export config structures to XML file', UVALUE='expXML')
  btnInfoS=WIDGET_BUTTON(bBtmButt, VALUE=thisPath+'images\info.bmp',/BITMAP, UVALUE='s_info')
  btnCancelSett=WIDGET_BUTTON(bBtmButt, VALUE='Close', UVALUE='cancel', FONT=font1)

  tabNo=0
  CASE tabString OF
    'QTSETUP': tabNo=1
    'QTOUT': tabNo=2
    'AUTOSETUP': tabNo=3
    'RENAMEDICOM': tabNo=4
    'INFO': tabNo=5
    ELSE:
  ENDCASE
  WIDGET_CONTROL, wtabSett, SET_TAB_CURRENT=tabNo

  WIDGET_CONTROL, settingsbox, /REALIZE
  XMANAGER, 'settings', settingsbox

end

pro settings_event, event

  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval


  IF N_ELEMENTS(uval) GT 0 AND SIZE(uval, /TNAME) EQ 'STRING' THEN BEGIN
    CASE uval OF

      'cancel': BEGIN
        sv='Yes'
        IF autoChanged THEN sv=DIALOG_MESSAGE('Possible unsaved changed for the automation template not saved. Continue without saving?', /QUESTION, DIALOG_PARENT=event.top)
        IF sv EQ 'Yes' THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          IF N_ELEMENTS(quickTemp) NE 0 THEN BEGIN
            IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
              IF SIZE(quickTemp.(modality), /TNAME) EQ 'STRUCT' THEN fillQuickTempList, quickTemp.(modality) ELSE fillQuickTempList, -1
            ENDIF ELSE fillQuickTempList, -1
          ENDIF ELSE fillQuickTempList, -1
          WIDGET_CONTROL, Event.top, /DESTROY
        ENDIF
      END
      'remBlock': BEGIN
        sv=DIALOG_MESSAGE('Make sure no other users save to the same config file. Reset save block?', /QUESTION, DIALOG_PARENT=event.Top)
        IF sv EQ 'Yes' THEN BEGIN
          blockAdr=thisPath+'data\blockSaveStamp.txt'
          resBlock = FILE_TEST(blockAdr)
          IF resBlock EQ 1 THEN BEGIN
            FILE_DELETE, blockAdr, /QUIET
            resBlock=0
            saveOK=1
            OPENW, blockfile, blockAdr, /GET_LUN
            PRINTF, blockfile, 'Saving blocked by user session'
            CLOSE, blockfile & FREE_LUN, blockfile
            sv=DIALOG_MESSAGE('Blocking now removed.', /INFORMATION, DIALOG_PARENT=event.Top) ;Settings-window will closed. Reopen to refresh.', /INFORMATION, DIALOG_PARENT=event.Top)
            
            WIDGET_CONTROL, btnAdd_s, SENSITIVE=1
            WIDGET_CONTROL, btnSave_s,SENSITIVE=1
            WIDGET_CONTROL,btnDupliTemp_s,SENSITIVE=1
            WIDGET_CONTROL,btnRenameTemp_s,SENSITIVE=1
            WIDGET_CONTROL,btnUpTemp_s,SENSITIVE=1
            WIDGET_CONTROL,btnDownTemp_s,SENSITIVE=1
            WIDGET_CONTROL,btnDelete_s,SENSITIVE=1
            WIDGET_CONTROL,btnSetDef,SENSITIVE=1
            WIDGET_CONTROL,btnRefreshQT,SENSITIVE=1
            WIDGET_CONTROL,btnDuplicate_qt,SENSITIVE=1
            WIDGET_CONTROL,btnRename_qt,SENSITIVE=1
            WIDGET_CONTROL,btnUpTemp_qt,SENSITIVE=1
            WIDGET_CONTROL,btnDownTemp_qt,SENSITIVE=1
            WIDGET_CONTROL,btnDelete_qt,SENSITIVE=1
            WIDGET_CONTROL,btnDupliTemp_qto,SENSITIVE=1
            WIDGET_CONTROL,btnDeleteTemp_qto,SENSITIVE=1
            WIDGET_CONTROL,btnRenameTemp_qto,SENSITIVE=1
            WIDGET_CONTROL,btnAddQTO,SENSITIVE=1
            WIDGET_CONTROL,btnEditQTO,SENSITIVE=1
            WIDGET_CONTROL,btnDelQTO,SENSITIVE=1
            WIDGET_CONTROL,btnAddTemp_a,SENSITIVE=1
            WIDGET_CONTROL,btnOverWriteAuto,SENSITIVE=1
            WIDGET_CONTROL,btnDupliTemp_a,SENSITIVE=1
            WIDGET_CONTROL,btnRenameTemp_a,SENSITIVE=1
            WIDGET_CONTROL,btnUpTemp_a,SENSITIVE=1
            WIDGET_CONTROL,btnDownTemp_a,SENSITIVE=1
            WIDGET_CONTROL,btnDelTemp_a,SENSITIVE=1
            WIDGET_CONTROL,btnAdd_rde,SENSITIVE=1
            WIDGET_CONTROL,btnOverWrite_rde,SENSITIVE=1
            WIDGET_CONTROL,btnDupli_rde,SENSITIVE=1
            WIDGET_CONTROL,btnUp_rde,SENSITIVE=1
            WIDGET_CONTROL,btnDown_rde,SENSITIVE=1
            WIDGET_CONTROL,btnDel_rde,SENSITIVE=1
            WIDGET_CONTROL,btnDuplicate_rdt,SENSITIVE=1
            WIDGET_CONTROL,btnRename_rdt,SENSITIVE=1
            WIDGET_CONTROL,btnUpTemp_rdt,SENSITIVE=1
            WIDGET_CONTROL,btnDownTemp_rdt,SENSITIVE=1
            WIDGET_CONTROL,btnDelete_rdt,SENSITIVE=1
            ;WIDGET_CONTROL, Event.top, /DESTROY
          ENDIF
        ENDIF
      END
      's_info': WIDGET_CONTROL, wtabSett, SET_TAB_CURRENT=4
      'backupConfig':BEGIN
        adr=DIALOG_PICKFILE(TITLE='Backup config file', /WRITE, FILTER='*.dat', /FIX_FILTER, DIALOG_PARENT=event.Top, /OVERWRITE_PROMPT, DEFAULT_EXTENSION='.dat', PATH='C:\')
        IF adr(0) NE '' THEN FILE_COPY, thisPath+'data\config.dat', adr
      END

      'expXML':BEGIN

        adr=DIALOG_PICKFILE(TITLE='Select an empty folder to place the 5 xml files', /WRITE, PATH=defPath, /DIRECTORY, DIALOG_PARENT=event.Top)
        IF adr(0) NE '' THEN BEGIN
          
           RESTORE, thisPath+'data\config.dat'
           configSarr=struct2xml(configS)
           IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN  quickTempArr=struct2xml(quickTemp)
           quickToutArr=struct2xml(quickTout)
           IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN loadTempArr=struct2xml(loadTemp)
           renameTempArr=struct2xml(renameTemp)
           
           if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
           OPENW, xmlfile, adr+'\configS.xml',/GET_LUN
           PRINTF, xmlfile, STRJOIN(configSarr,newline)
           CLOSE, xmlfile & FREE_LUN, xmlfile
           IF N_ELEMENTS(quickTempArr) NE 0 THEN BEGIN
             OPENW, xmlfile, adr+'\quickTemp.xml',/GET_LUN
             PRINTF, xmlfile, STRJOIN(quickTempArr,newline)
             CLOSE, xmlfile & FREE_LUN, xmlfile
           ENDIF
           OPENW, xmlfile, adr+'\quickTout.xml',/GET_LUN
           PRINTF, xmlfile, STRJOIN(quickToutArr,newline)
           CLOSE, xmlfile & FREE_LUN, xmlfile
           IF N_ELEMENTS(loadTempArr) NE 0 THEN BEGIN
             OPENW, xmlfile, adr+'\loadTemp.xml',/GET_LUN
             PRINTF, xmlfile, STRJOIN(loadTempArr,newline)
             CLOSE, xmlfile & FREE_LUN, xmlfile
           ENDIF
           OPENW, xmlfile, adr+'\renameTemp.xml',/GET_LUN
           PRINTF, xmlfile, STRJOIN(renameTempArr,newline)
           CLOSE, xmlfile & FREE_LUN, xmlfile
     
        ENDIF
        END

      'restoreConfig':BEGIN
        IF saveOK THEN BEGIN
          adr=DIALOG_PICKFILE(TITLE='Locate config file to restore from', /READ, FILTER='*.dat', /FIX_FILTER, DIALOG_PARENT=event.Top, PATH='C:\')
          IF adr(0) NE '' THEN BEGIN
            WIDGET_CONTROL, /HOURGLASS
            configS=!Null
            config=!Null
            newConfigS=updateConfigS(adr(0))

            IF SIZE(newConfigS, /TNAME) EQ 'STRUCT' THEN BEGIN

              configS=newConfigS
              quickTemp=updateQuickT(adr(0), multiOpt)
              loadTemp=updateLoadT(adr(0),multiOpt)
              quickTout=updateQuickTout(adr(0))
              renameTemp=updateRenameTemp(adr(0))
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              ;update values on default parameterset
              selConfig=configS.(0)
              restoreTagsS=TAG_NAMES(configS)
              refreshParam, configS.(selConfig), restoreTagsS(selConfig)
              IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN fillQuickTempList, quickTemp.(modality) ELSE fillQuickTempList, -1; in refreshParam.pro
              s_upd, selConfig-1, 1
              qt_upd, 0
              WIDGET_CONTROL, lstModality_qto, SET_DROPLIST_SELECT=0 & qto_currMod=0 & qto_updMode
              auto_upd, 0, 1

              sv=DIALOG_MESSAGE('Config file restored.', /INFORMATION, DIALOG_PARENT=event.Top)

            ENDIF; not valid file

          ENDIF; no file selected
        ENDIF ELSE sv=DIALOG_MESSAGE('Save blocked by another user session.', DIALOG_PARENT=evTop)
      END
      ; ***************** Parameter sets ****************
      's_listSets': s_upd, WIDGET_INFO(listSets_s, /LIST_SELECT), 0

      's_add':BEGIN

        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Name the new parameter set:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Save as...', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.Save THEN BEGIN
          IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not save.', DIALOG_PARENT=event.top) ELSE BEGIN

            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            RESTORE, thisPath+'data\config.dat'
            tempnames_s=TAG_NAMES(configS)
            IF tempnames_s.HasValue(tempname) THEN sv=DIALOG_MESSAGE('Name already in use. Could not save.', DIALOG_PARENT=event.top) ELSE BEGIN

              saveParam, -1, tempname
              RESTORE, thisPath+'data\config.dat'

              ;saveNmb?
              saveNmb=WHERE(TAG_NAMES(configS) EQ tempname)
              WIDGET_CONTROL, lstCT_s, GET_VALUE=strCT
              WIDGET_CONTROL, lstX_s, GET_VALUE=strX
              WIDGET_CONTROL, lstNM_s, GET_VALUE=strNM
              WIDGET_CONTROL, lstMR_s, GET_VALUE=strMR
              configS.(saveNmb).qtOutTemps=[strCT(WIDGET_INFO(lstCT_s, /DROPLIST_SELECT)),strX(WIDGET_INFO(lstX_s, /DROPLIST_SELECT)),strNM(WIDGET_INFO(lstNM_s, /DROPLIST_SELECT)),'DEFAULT','DEFAULT',strMR(WIDGET_INFO(lstMR_s, /DROPLIST_SELECT))]
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              s_upd, saveNmb-1, 0
            ENDELSE
          ENDELSE
        ENDIF
      END
      's_overwrite':BEGIN
        sv=DIALOG_MESSAGE('Are you sure you want to overwrite the selected parameterset with current values?', /QUESTION, DIALOG_PARENT=event.TOP)
        IF sv EQ 'Yes' THEN BEGIN

          selSet=WIDGET_INFO(listSets_s, /LIST_SELECT)

          saveParam, selSet+1,''
          RESTORE, thisPath+'data\config.dat'
          namesCT=TAG_NAMES(quickTout.(0))
          namesX=TAG_NAMES(quickTout.(1))
          namesNM=TAG_NAMES(quickTout.(2))
          namesMR=TAG_NAMES(quickTout.(3))
          configS.(selSet+1).qtOutTemps=[namesCT(WIDGET_INFO(lstCT_s, /DROPLIST_SELECT)),namesX(WIDGET_INFO(lstX_s, /DROPLIST_SELECT)),namesNM(WIDGET_INFO(lstNM_s, /DROPLIST_SELECT)),'DEFAULT','DEFAULT',namesMR(WIDGET_INFO(lstMR_s, /DROPLIST_SELECT))]
          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'

          s_upd, selSet, 0
        ENDIF
      END
      's_duplicate': BEGIN
        ;ask for new name
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Name the duplicate:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Name the duplicate', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.Save THEN BEGIN
          IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not duplicate.', DIALOG_PARENT=event.top) ELSE BEGIN

            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            RESTORE, thisPath+'data\config.dat'
            currNames=TAG_NAMES(configS)

            IF currNames.HasValue(tempname) THEN BEGIN
              sv=DIALOG_MESSAGE('Duplication failed. A template with this name already exist.',DIALOG_PARENT=event.top)
            ENDIF ELSE BEGIN
              toDupli=WIDGET_INFO(listSets_s, /LIST_SELECT)+1
              configS=CREATE_STRUCT(configS, tempname, configS.(toDupli))
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              s_upd, N_TAGS(configS)-2, 0
            ENDELSE

          ENDELSE
        ENDIF

      END
      's_rename': BEGIN
        ;ask for new name
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Rename selected parameterset:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.Save THEN BEGIN
          IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified.', DIALOG_PARENT=event.top) ELSE BEGIN

            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            RESTORE, thisPath+'data\config.dat'

            setNames=TAG_NAMES(configS)
            IF setNames.HasValue(tempname) THEN BEGIN
              sv=DIALOG_MESSAGE('A parameterset with this name already exist. Renaming not possible.',DIALOG_PARENT=event.top)
            ENDIF ELSE BEGIN
              ;replace parameterset
              toRename=WIDGET_INFO(listSets_s, /LIST_SELECT)+1
              configS=replaceStructStruct(configS, configS.(toRename), toRename, NEW_TAG_NAME=tempname)
              setNames=TAG_NAMES(configS)
              ;if exist in loadTemp - then change also loadTemp
              IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
                IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
                  FOR i = 0, N_TAGS(loadTemp)-1 DO BEGIN
                    IF STRUPCASE(loadTemp.(i).PARAMSET) EQ STRUPCASE(setNames(toRename)) THEN BEGIN
                      loadTemp.(i).PARAMSET=tempname
                    ENDIF
                  ENDFOR
                ENDIF
              ENDIF

              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              s_upd, toRename-1, 0
            ENDELSE

          ENDELSE
        ENDIF
      END
      's_upTemp':BEGIN
        RESTORE, thisPath+'data\config.dat'
        tempnames_s=TAG_NAMES(configS)
        currSel=WIDGET_INFO(listSets_s, /LIST_SELECT)+1
        IF currSel GT 1 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(tempnames_s))
          newOrder=oldOrder
          newOrder[currSel-1:currSel]=REVERSE(oldOrder[currSel-1:currSel])
          configS=reorderStructStruct(configS, newOrder)
          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          tempnames_s=TAG_NAMES(configS)
          WIDGET_CONTROL, listSets_s, SET_VALUE=tempnames_s[1:-1], SET_LIST_SELECT=currSel-2
          s_upd, currSel-2, 0
        ENDIF
      END
      's_downTemp':BEGIN
        RESTORE, thisPath+'data\config.dat'
        tempnames_s=TAG_NAMES(configS)
        currSel=WIDGET_INFO(listSets_s, /LIST_SELECT)+1
        IF currSel LT N_ELEMENTS(tempnames_s)-1 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(tempnames_s))
          newOrder=oldOrder
          newOrder[currSel:currSel+1]=REVERSE(oldOrder[currSel:currSel+1])
          configS=reorderStructStruct(configS, newOrder)
          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          tempnames_s=TAG_NAMES(configS)
          WIDGET_CONTROL, listSets_s, SET_VALUE=tempnames_s[1:-1], SET_LIST_SELECT=currSel
          s_upd, currSel, 0
        ENDIF
      END
      's_delete':BEGIN
        RESTORE, thisPath+'data\config.dat'

        IF N_TAGS(configS) EQ 2 THEN sv=DIALOG_MESSAGE('At least one parameter set have to be kept.', DIALOG_PARENT=event.TOP) ELSE BEGIN
          selSet=WIDGET_INFO(listSets_s, /LIST_SELECT)
          proceed=1
          IF N_ELEMENTS(autoNames_s) GT 0 THEN BEGIN
            sv=DIALOG_MESSAGE('Delete this Parameter set template used in automation templates?', /QUESTION, DIALOG_PARENT=event.TOP)
            IF sv EQ 'No' THEN proceed=0
          ENDIF

          IF proceed THEN BEGIN
            configS=removeIDstructstruct(configS, selSet+1)
            IF selSet+1 LT configS.(0) THEN configS.(0)=configS.(0)-1
            IF selSet+1 EQ configS.(0) THEN configS.(0)=1; the default is selected
            currInMain=selConfig
            selConfig=configS.(0)
            setNames=TAG_NAMES(configS)
            IF selSet+1 EQ currInMain THEN refreshParam, configS.(selConfig), setNames(selConfig);the currently selected in main window is deleted, uptdat with default
            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            s_upd, selConfig-1, 0
          ENDIF
        ENDELSE
      END

      's_setDefault':BEGIN
        RESTORE, thisPath+'data\config.dat'
        configS.(0)=WIDGET_INFO(listSets_s, /LIST_SELECT)+1
        SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
        s_upd, WIDGET_INFO(listSets_s, /LIST_SELECT), 0
      END



      's_setCurr':BEGIN
        selConfig=WIDGET_INFO(listSets_s, /LIST_SELECT)+1
        RESTORE, thisPath+'data\config.dat'
        setNames=TAG_NAMES(configS)
        refreshParam, configS.(selConfig), setNames(selConfig)
        IF N_ELEMENTS(quickTemp) NE 0 THEN fillQuickTempList, quickTemp.(modality) ELSE fillQuickTempList, -1
        WIDGET_CONTROL, Event.top, /DESTROY
      END

      ;************** QT setup *********

      'qt_refresh': BEGIN
        upd_QuickTestTemplates  ; (pro in this file)
        qt_upd, WIDGET_INFO(lstTempQT, /LIST_SELECT); updating current selected test (pro in this file)
        sv=DIALOG_MESSAGE('Refersh of QuickTest templates finished', DIALOG_PARENT=event.Top)
      END
      'qt_lstModality': qt_upd, 0; update new modality (pro in this file)
      'qt_listTemp': qt_upd, WIDGET_INFO(lstTempQT, /LIST_SELECT); update test (pro in this file)
      'qt_duplicate': BEGIN
        currSel=WIDGET_INFO(lstTempQT, /LIST_SELECT)
        IF currSel(0) GE 0 THEN BEGIN
          ;ask for new name
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, , LABEL_LEFT=Name the new template:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Save',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Name the duplicate', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

          IF res.Save THEN BEGIN
            IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not duplicate.', DIALOG_PARENT=event.top) ELSE BEGIN

              tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
              RESTORE, thisPath+'data\config.dat'
              modNmb=WIDGET_INFO(lstModality_QT,/DROPLIST_SELECT)
              modSel=availModNmb(modNmb)
              currNames=TAG_NAMES(quickTemp.(modSel))

              IF currNames.HasValue(tempname) THEN BEGIN
                sv=DIALOG_MESSAGE('Duplication failed. A template with this name already exist.',DIALOG_PARENT=event.top)
              ENDIF ELSE BEGIN
                quickTempMod=CREATE_STRUCT(quickTemp.(modSel), tempname, quickTemp.(modSel).(currSel(0)))
                quickTemp=replaceStructStruct(quickTemp, quickTempMod, modSel)

                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                QTnames=TAG_NAMES(quickTemp.(modSel))
                qt_upd, N_ELEMENTS(QTnames)-1
              ENDELSE

            ENDELSE
          ENDIF
        ENDIF; any selected
      END
      'qt_rename':BEGIN
        currSel=WIDGET_INFO(lstTempQT, /LIST_SELECT)
        IF currSel(0) GE 0 THEN BEGIN
          QTnameThis=QTnames(currSel)
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, '+QTnameThis+', LABEL_LEFT=New name:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Rename',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

          IF res.Rename AND res.newname NE '' THEN BEGIN
            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            IF QTnames.HasValue(tempname) THEN BEGIN
              sv=DIALOG_MESSAGE('Template name '+tempname+' already exist. Renaming not possible.', DIALOG_PARENT=event.Top)
            ENDIF ELSE BEGIN
              RESTORE, thisPath+'data\config.dat'
              modNmb=WIDGET_INFO(lstModality_QT,/DROPLIST_SELECT)
              modSel=availModNmb(modNmb)
              ;replace quickTemp
              quickTm=replaceStructStruct(quickTemp.(modSel),quickTemp.(modSel).(currSel), currSel, NEW_TAG_NAME=tempname)
              quickTemp=replaceStructStruct(quickTemp, quickTm, modSel)

              ;if exist in loadTemp - then change also loadTemp
              IF N_ELEMENTS(autoNames_qt) GT 0 THEN BEGIN
                FOR i = 0, N_TAGS(loadTemp.(modSel))-1 DO BEGIN
                  IF STRUPCASE(loadTemp.(modSel).(i).QUICKTEMP) EQ STRUPCASE(QTnameThis) THEN BEGIN
                    loadTemp.(modSel).(i).QUICKTEMP=tempname
                  ENDIF
                ENDFOR
              ENDIF

              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              qt_upd, currSel
            ENDELSE
          ENDIF
        ENDIF; any selected
      END
      'qt_upTemp':BEGIN
        currSel=WIDGET_INFO(lstTempQT, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(QTnames))
          newOrder=oldOrder
          newOrder[currSel-1:currSel]=REVERSE(oldOrder[currSel-1:currSel])
          RESTORE, thisPath+'data\config.dat'
          modNmb=WIDGET_INFO(lstModality_QT,/DROPLIST_SELECT)
          modSel=availModNmb(modNmb)
          quickTm=reorderStructStruct(quickTemp.(modSel), newOrder)
          quickTemp=replaceStructStruct(quickTemp, quickTm, modSel)
          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          QTnames=TAG_NAMES(quickTemp.(modSel))
          WIDGET_CONTROL, lstTempQT, SET_VALUE=QTnames, SET_LIST_SELECT=currSel-1
          qt_upd, currSel-1
        ENDIF
      END
      'qt_downTemp':BEGIN
        currSel=WIDGET_INFO(lstTempQT, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(QTnames)-1 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(QTnames))
          newOrder=oldOrder
          newOrder[currSel:currSel+1]=REVERSE(oldOrder[currSel:currSel+1])
          RESTORE, thisPath+'data\config.dat'
          modNmb=WIDGET_INFO(lstModality_QT,/DROPLIST_SELECT)
          modSel=availModNmb(modNmb)
          quickTm=reorderStructStruct(quickTemp.(modSel), newOrder)
          quickTemp=replaceStructStruct(quickTemp, quickTm, modSel)
          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          QTnames=TAG_NAMES(quickTemp.(modSel))
          WIDGET_CONTROL, lstTempQT, SET_VALUE=QTnames, SET_LIST_SELECT=currSel+1
          qt_upd, currSel+1
        ENDIF
      END
      'qt_delete':BEGIN

        sel=WIDGET_INFO(lstTempQT, /LIST_SELECT)
        IF sel(0) LT -1 THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          modNmb=WIDGET_INFO(lstModality_QT,/DROPLIST_SELECT)
          modSel=availModNmb(modNmb)
          curQT=quickTemp.(modSel)
          QTnames=TAG_NAMES(curQT)

          IF N_ELEMENTS(QTnames) EQ 1 THEN BEGIN
            quickTemp=replaceStructStruct(quickTemp, -1, modSel)
          ENDIF ELSE BEGIN
            QTnameDel=QTnames(sel)

            ;in loadTemp - warning used in automation template
            proceed=1
            IF N_eLEMENTS(autoNames_qt) GT 0 THEN BEGIN
              sv=DIALOG_MESSAGE('Delete this QuickTest template used in '+STRING(N_ELEMENTS(autoNames_qt), FORMAT='(i0)')+' automation templates?', /QUESTION, DIALOG_PARENT=event.Top)
              IF sv EQ 'No' THEN proceed=0
            ENDIF

            IF proceed THEN BEGIN
              ss=INTARR(N_ELEMENTS(QTnames))+1
              ss(sel)=0
              remain=WHERE(ss EQ 1)
              updQT=!Null
              FOR ii=0, N_ELEMENTS(QTnames)-1 DO BEGIN
                IF remain.HasValue(ii) THEN BEGIN
                  updQT=CREATE_STRUCT(updQT, QTnames(ii), curQT.(ii))
                ENDIF
              ENDFOR
              quickTemp=replaceStructStruct(quickTemp, updQT, modSel)

              ;update loadTEmp
              IF N_ELEMENTS(autoNames_qt) NE 0 THEN BEGIN
                namesAll=TAG_NAMES(loadTemp)
                FOR i=0, N_ELEMENTS(autoNames_qt)-1 DO BEGIN
                  id=WHERE(namesAll EQ autoNames_qt(i))
                  IF id(0) NE -1 THEN loadTemp.(modSel).(id(0)).QUICKTEMP=''
                ENDFOR
              ENDIF
            ENDIF
            QTnames=TAG_NAMES(quickTemp.(modSel))
          ENDELSE

          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          qt_upd, 0
        ENDIF;none in this modality

      END

      ;************** QT output **************

      'qto_lstModality': BEGIN;modality changed?
        IF WIDGET_INFO(lstModality_qto, /DROPLIST_SELECT) NE qto_currMod THEN BEGIN
          qto_currMod=WIDGET_INFO(lstModality_qto, /DROPLIST_SELECT)
          qto_updMode
        ENDIF
      END
      'qto_lstTemp': BEGIN;new temp selected?
        IF WIDGET_INFO(lstTemplates_qto, /LIST_SELECT) NE qto_currTemp THEN BEGIN
          qto_currTemp=WIDGET_INFO(lstTemplates_qto, /LIST_SELECT)
          qto_updTemp
        ENDIF
      END

      'qto_duplicate': BEGIN
        ;ask for new name
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Name the new template:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Name the duplicate', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.Save THEN BEGIN
          IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not duplicate.', DIALOG_PARENT=event.top) ELSE BEGIN

            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            RESTORE, thisPath+'data\config.dat'
            currNames=TAG_NAMES(quickTout.(qto_currMod))

            IF currNames.HasValue(tempname) THEN BEGIN
              sv=DIALOG_MESSAGE('Duplication failed. A template with this name already exist.',DIALOG_PARENT=event.top)
            ENDIF ELSE BEGIN
              quickToutMod=CREATE_STRUCT(quickTout.(qto_currMod), tempname, quickTout.(qto_currMod).(WIDGET_INFO(lstTemplates_qto, /LIST_SELECT)))
              quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              WIDGET_CONTROL, lstTemplates_qto, SET_VALUE=[currnames, tempname], SET_LIST_SELECT=N_ELEMENTS(currNames), SCR_YSIZE=150;added as last, set selected
              qto_currTemp=N_ELEMENTS(currNames)
              qto_updTemp
            ENDELSE

          ENDELSE
        ENDIF

      END
      'qto_rename': BEGIN
        IF qto_currTemp NE 0 THEN BEGIN
          ;ask for new name
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, , LABEL_LEFT=Rename selected template:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Save',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

          IF res.Save THEN BEGIN
            IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified.', DIALOG_PARENT=event.top) ELSE BEGIN

              tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
              RESTORE, thisPath+'data\config.dat'
              currNames=TAG_NAMES(quickTout.(qto_currMod))

              IF currNames.HasValue(tempname) THEN BEGIN
                sv=DIALOG_MESSAGE('A template with this name already exist. Renaming not possible.',DIALOG_PARENT=event.top)
              ENDIF ELSE BEGIN
                currNames(qto_currTemp)=tempname
                FOR i=0, N_ELEMENTS(currNames)-1 DO BEGIN
                  IF i EQ 0 THEN quickToutMod=CREATE_STRUCT(currNames(i),quickTout.(qto_currMod).(i)) ELSE quickToutMod=CREATE_STRUCT(quickToutMod, currNames(i), quickTout.(qto_currMod).(i))
                ENDFOR

                quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

                ;update paramset
                IF N_ELEMENTS(setNames_qto) NE 0 THEN BEGIN
                  namesAll=TAG_NAMES(configS)
                  FOR i=0, N_ELEMENTS(setNames_qto)-1 DO BEGIN
                    id=WHERE(namesAll EQ setNames_qto(i))
                    IF id(0) NE -1 THEN configS.(id(0)).QTOUTTEMPS(qto_currMod)=tempname
                  ENDFOR
                ENDIF

                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                WIDGET_CONTROL, lstTemplates_qto, SET_VALUE=currNames, SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150
              ENDELSE

            ENDELSE
          ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be renamed', DIALOG_PARENT=event.top)
      END
      'qto_delete': BEGIN
        IF qto_currTemp NE 0 THEN BEGIN

          txt='Are you sure you want to delete the selcted template'
          IF N_ELEMENTS(setNames_qto) NE 0 THEN txt=txt + ' used in parameter sets.'
          sv=DIALOG_MESSAGE(txt, /QUESTION, DIALOG_PARENT=event.top)
          IF sv EQ 'Yes' THEN BEGIN
            RESTORE, thisPath+'data\config.dat'
            quickToutMod= removeIDstructstruct(quickTout.(qto_currMod), qto_currTemp)
            currNames=TAG_NAMES(quickToutMod)
            qto_currTemp=qto_currTemp-1

            quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

            ;update paramset
            IF N_ELEMENTS(setNames_qto) NE 0 THEN BEGIN
              namesAll=TAG_NAMES(configS)
              FOR i=0, N_ELEMENTS(setNames_qto)-1 DO BEGIN
                id=WHERE(namesAll EQ setNames_qto(i))
                IF id(0) NE -1 THEN configS.(id(0)).QTOUTTEMPS(qto_currMod)='DEFAULT'
              ENDFOR
            ENDIF
            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            WIDGET_CONTROL, lstTemplates_qto, SET_VALUE=currNames, SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150
            qto_updTemp
          ENDIF

        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be delete', DIALOG_PARENT=event.top)
      END
      'qto_lstTest':BEGIN ;new test selected - update alternatives, and columns

        WIDGET_CONTROL, lstAlt, SET_VALUE=TAG_NAMES(tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT))), SET_DROPLIST_SELECT=0
        WIDGET_CONTROL, lstCols, SET_VALUE=tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT)).(0), SET_LIST_SELECT=0
      END
      'qto_lstAlt': BEGIN;new alt selected?
        WIDGET_CONTROL, lstCols, SET_VALUE=tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT)).(WIDGET_INFO(lstAlt,/DROPLIST_SELECT)), SET_LIST_SELECT=0
      END
      'qto_add':BEGIN
        IF qto_currTemp NE 0 THEN BEGIN
          testno=WIDGET_INFO(lstTest, /DROPLIST_SELECT)
          WIDGET_CONTROL, txtDescr, GET_VALUE=testDescr
          testDescr=STRUPCASE(IDL_VALIDNAME(testDescr, /CONVERT_ALL))

          RESTORE, thisPath+'data\config.dat'
          currStruct=quickTout.(qto_currMod).(qto_currTemp).(testno)
          proceed=1
          IF SIZE(currStruct, /TNAME) EQ 'STRUCT' THEN BEGIN
            currNames=TAG_NAMES(currStruct)
            IF currNames.HasValue(testDescr) THEN BEGIN
              sv=DIALOG_MESSAGE('Test-description need to be unique. Could not add to template.', DIALOG_PARENT=event.top)
              proceed=0
            ENDIF
          ENDIF

          IF proceed THEN BEGIN

            newStruct=CREATE_STRUCT('ALT',WIDGET_INFO(lstAlt, /DROPLIST_SELECT),'COLUMNS',WIDGET_INFO(lstCols, /LIST_SELECT),'CALC',WIDGET_INFO(lstCalc, /DROPLIST_SELECT),'PER_SERIES',WIDGET_INFO(lstPer, /DROPLIST_SELECT))
            IF SIZE(currStruct, /TNAME) EQ 'STRUCT' THEN tempstructTest=CREATE_STRUCT(currStruct, testDescr, newStruct) ELSE tempstructTest=CREATE_STRUCT(testDescr, newStruct)
            quickToutTemp=replaceStructStruct(quickTout.(qto_currMod).(qto_currTemp), tempstructTest, testno)
            quickToutMod=replaceStructStruct(quickTout.(qto_currMod), quickToutTemp, qto_currTemp)
            quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            WIDGET_CONTROL, lstTemplates_qto, SET_VALUE=TAG_NAMES(quickTout.(qto_currMod)), SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150
            qto_updTemp

          ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be changed. Duplicate to make changes.', DIALOG_PARENT=event.top)
      END
      'qto_overwrite':BEGIN
        IF qto_currTemp NE 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Are you sure you want to overwrite this output element?', /Question, DIALOG_PARENT=event.top)
          IF sv EQ 'Yes' THEN BEGIN
            RESTORE, thisPath+'data\config.dat'
            WIDGET_CONTROL, txtDescr, GET_VALUE=testDescr
            testDescr=STRUPCASE(IDL_VALIDNAME(testDescr, /CONVERT_ALL))
            proceed=1
            IF SIZE(quickTout.(qto_currMod).(qto_currTemp).(qto_currTest),/TNAME) EQ 'STRUCT' THEN BEGIN
              proceed=2
              currNames=TAG_NAMES(quickTout.(qto_currMod).(qto_currTemp).(qto_currTest))
              IF currNames.HasValue(testDescr) AND testDescr NE currNames(qto_currOutp) THEN BEGIN
                sv=DIALOG_MESSAGE('Test-description need to be unique. Could not overwrite template.', DIALOG_PARENT=event.top)
                proceed=0
              ENDIF
            ENDIF

            IF proceed GE 1 THEN BEGIN

              newStruct=CREATE_STRUCT('ALT',WIDGET_INFO(lstAlt, /DROPLIST_SELECT),'COLUMNS',WIDGET_INFO(lstCols, /LIST_SELECT),'CALC',WIDGET_INFO(lstCalc, /DROPLIST_SELECT),'PER_SERIES',WIDGET_INFO(lstPer, /DROPLIST_SELECT))
              IF proceed EQ 2 THEN BEGIN
                currTest=quickTout.(qto_currMod).(qto_currTemp).(qto_currTest)
                quickToutTempTest=replaceStructStruct(currTest, newStruct, qto_currOutp, NEW_TAG_NAME=testDescr)
              ENDIF ELSE BEGIN
                quickToutTempTest=CREATE_STRUCT(testDescr, newStruct)
              ENDELSE
              quickToutTemp=replaceStructStruct(quickTout.(qto_currMod).(qto_currTemp), quickToutTempTest, qto_currTest)
              quickToutMod=replaceStructStruct(quickTout.(qto_currMod), quickToutTemp, qto_currTemp)
              quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              qto_fillTable
            ENDIF
          ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('The default template can not be changed. Duplicate to make changes.', DIALOG_PARENT=event.top)
      END
      'qto_deleteOutp':BEGIN
        IF qto_currTemp NE 0 THEN BEGIN
          sv=DIALOG_MESSAGE('Are you sure you want to delete this output element?', /Question, DIALOG_PARENT=event.top)
          IF sv EQ 'Yes' THEN BEGIN
            RESTORE, thisPath+'data\config.dat'
            IF SIZE(quickTout.(qto_currMod).(qto_currTemp).(qto_currTest), /TNAME) EQ 'STRUCT' THEN BEGIN
              nActOut=N_TAGS(quickTout.(qto_currMod).(qto_currTemp).(qto_currTest))
              IF nActOut NE 1 THEN BEGIN;keep at least one

                currTest=quickTout.(qto_currMod).(qto_currTemp).(qto_currTest)
                quickToutTempTest=removeIDstructstruct(currTest, qto_currOutp)
                quickToutTemp=replaceStructStruct(quickTout.(qto_currMod).(qto_currTemp), quickToutTempTest, qto_currTest)
                quickToutMod=replaceStructStruct(quickTout.(qto_currMod), quickToutTemp, qto_currTemp)
                quickTout=replaceStructStruct(quickTout, quickToutMod, qto_currMod)

                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                qto_updTemp

              ENDIF ELSE sv=DIALOG_MESSAGE('At least one output element for each test has to be kept.', DIALOG_PARENT=event.top)
            ENDIF ELSE sv=DIALOG_MESSAGE('At least one output element for each test has to be kept.', DIALOG_PARENT=event.top)
          ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('The default template cannot be changed. Duplicate to make changes.', DIALOG_PARENT=event.top)
      END

      ;************* Auto setup ********************
      'a_lstModality': BEGIN
        sv='Yes'
        IF autoChanged THEN sv=DIALOG_MESSAGE('Parameters for the current template have changed. Continue without saving?', /QUESTION, DIALOG_PARENT=event.top)
        IF sv EQ 'Yes' THEN auto_upd, 0,2 ELSE WIDGET_CONTROL, lstModality_a, SET_LIST_SELECT=a_currMod
      END
      'a_listTemp':BEGIN
        sv='Yes'
        IF autoChanged THEN sv=DIALOG_MESSAGE('Parameters for the current template have changed. Continue without saving?', /QUESTION, DIALOG_PARENT=event.top)
        IF sv EQ 'Yes' THEN BEGIN
          selecTemp_a=WIDGET_INFO(listTemp_a, /LIST_SELECT)
          clearParam=0
          IF selecTemp_a(0) NE -1 THEN BEGIN
            IF tempnames_a(0) NE '' THEN auto_upd, selecTemp_a(0),0 ELSE clearParam=1
          ENDIF ELSE clearParam=1
          IF clearParam THEN auto_upd, -1,0
        ENDIF ELSE WIDGET_CONTROL, listTemp_a, SET_LIST_SELECT=selecTemp_a
      END

      'a_delTemp':BEGIN
        currSel=WIDGET_INFO(listTemp_a, /LIST_SELECT)
        IF currSel GE 0 THEN BEGIN
          IF tempnames_a(0) NE '' THEN BEGIN
            sv=DIALOG_MESSAGE('Delete selected automation template?',/QUESTION, DIALOG_PARENT=event.top)
            IF sv EQ 'Yes' THEN BEGIN
              RESTORE, thisPath+'data\config.dat'
              selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
              thisMod=availModNmb(selMod)
              IF N_ELEMENTS(tempnames_a) EQ 1 THEN loadTm=-1 ELSE loadTm=removeIDstructstruct(loadTemp.(thisMod), currSel)
              loadTemp=replaceStructstruct(loadTemp, loadTm, thisMod)
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              IF N_ELEMENTS(tempnames_a) EQ 1 THEN BEGIN
                tempnames_a=''
                WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a
                auto_upd, -1,0
              ENDIF ELSE BEGIN
                tempnames_a=removeIDarr(tempnames_a,currSel)
                WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=0
                auto_upd, 0,0
              ENDELSE
            ENDIF
          ENDIF
        ENDIF
      END
      'a_upTemp':BEGIN
        currSel=WIDGET_INFO(listTemp_a, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(tempnames_a))
          newOrder=oldOrder
          newOrder[currSel-1:currSel]=REVERSE(oldOrder[currSel-1:currSel])
          RESTORE, thisPath+'data\config.dat'
          selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
          thisMod=availModNmb(selMod)
          loadTm=reorderStructStruct(loadTemp.(thisMod), newOrder)
          loadTemp=replaceStructstruct(loadTemp, loadTm, thisMod)
          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          tempnames_a=TAG_NAMES(loadTemp.(thisMod))
          WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=currSel-1
          auto_upd, currSel-1,0
        ENDIF
      END
      'a_downTemp':BEGIN
        currSel=WIDGET_INFO(listTemp_a, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(tempnames_a)-1 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(tempnames_a))
          newOrder=oldOrder
          newOrder[currSel:currSel+1]=REVERSE(oldOrder[currSel:currSel+1])
          RESTORE, thisPath+'data\config.dat'
          selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
          thisMod=availModNmb(selMod)
          loadTm=reorderStructStruct(loadTemp.(thisMod), newOrder)
          loadTemp=replaceStructstruct(loadTemp, loadTm, thisMod)
          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          tempnames_a=TAG_NAMES(loadTemp.(thisMod))
          WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=currSel+1
          auto_upd, currSel+1,0
        ENDIF
      END

      'aimp_Browse':BEGIN
        adr=dialog_pickfile(PATH=defPath, GET_PATH=defPath, /DIRECTORY, /READ, TITLE='Select folder', DIALOG_PARENT=event.top)
        IF adr(0) NE '' THEN WIDGET_CONTROL, txtAutoImpPath, SET_VALUE=adr(0)
      END

      'aimp_Save':BEGIN
        WIDGET_CONTROL, txtAutoImpPath, GET_VALUE=adr
        ;for each config update adr
        RESTORE, thisPath+'data\config.dat'
        psets=TAG_NAMES(configS)
        FOR i=1, N_ELEMENTS(psets)-1 DO configS.(i).AUTOIMPORTPATH=adr(0)
        SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
      END

      'a_Browse':BEGIN
        adr=dialog_pickfile(PATH=defPath, GET_PATH=defPath, /DIRECTORY, /READ, TITLE='Select folder', DIALOG_PARENT=event.top)
        IF adr(0) NE '' THEN BEGIN
          WIDGET_CONTROL, txtBrowse_a, SET_VALUE=adr(0)
          autoChanged=1
        ENDIF
      END

      'a_BrowseApp':BEGIN
        adr=dialog_pickfile(PATH=defPath, GET_PATH=defPath, /READ, TITLE='Select result file to append', FILTER='*.txt', /FIX_FILTER, DIALOG_PARENT=event.top)
        IF adr(0) NE '' THEN BEGIN
          WIDGET_CONTROL, txtBrowseApp, SET_VALUE=adr(0)
          autoChanged=1
        ENDIF
      END

      'a_getStatName':BEGIN
        WIDGET_CONTROL, txtBrowse_a, GET_VALUE=pathNow
        If pathNow EQ '' THEN pathNow=defPath
        adr=dialog_pickfile(PATH=pathNow, /READ, TITLE='Select DICOM image to retrieve the station name', DIALOG_PARENT=event.top)
        IF adr(0) NE '' THEN BEGIN
          IF FILE_BASENAME(adr) EQ 'DICOMDIR' THEN okDcm = 0 ELSE okDcm=QUERY_DICOM(adr)
          IF okDcm THEN BEGIN
            o=obj_new('idlffdicom')
            t=o->read(adr)

            ;check if directoryfile
            test=o->GetReference('0004'x,'1220'x)
            IF test(0) EQ -1 THEN BEGIN
              test=o->GetReference('0008'x,'1010'x);
              test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
              IF test(0) NE -1 THEN stationName=*(test_peker[0]) ELSE stationName=''
              IF stationName EQ '' THEN sv=DIALOG_MESSAGE('Station name in DICOM tag 0008,1010 not found.',DIALOG_PARENT=event.top)
              WIDGET_CONTROL, txtStatName_a, SET_VALUE=stationName
              autoChanged=1
            ENDIF ELSE sv=DIALOG_MESSAGE('Selected file is not an DICOM image file.',DIALOG_PARENT=event.top)

          ENDIF ELSE sv=DIALOG_MESSAGE('Selected file is not an DICOM image file.',DIALOG_PARENT=event.top)
        ENDIF
      END

      'a_ClearApp': BEGIN
        WIDGET_CONTROL, txtBrowseApp, SET_VALUE=''
        autoChanged=1
      END

      'a_OpenApp': BEGIN
        WIDGET_CONTROL, txtBrowseApp, GET_VALUE=file2open
        IF FILE_TEST(file2open) THEN BEGIN
          Case !version.os_family of
            'Windows': SPAWN, 'notepad.exe '+file2open, /NOSHELL, /NOWAIT
            Else: if (!version.os_name eq 'Mac OS X') then SPAWN, 'open '+file2open
          Endcase
        ENDIF ELSE sv=DIALOG_MESSAGE('File not found.',DIALOG_PARENT=event.top)
      END

      'a_add':BEGIN
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Name the new template:, WIDTH=12, TAG=newname,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Name the new template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)
        IF res.Save THEN BEGIN
          IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not create new.', DIALOG_PARENT=event.top) ELSE BEGIN
            selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
            thisMod=availModNmb(selMod)
            RESTORE, thisPath+'data\config.dat'
            newname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
              IF SIZE(loadTemp.(thisMod), /TNAME) EQ 'STRUCT' THEN alreadyNames=TAG_NAMES(loadTemp.(thisMod)) ELSE alreadyNames=''
            ENDIF ELSE alreadyNames=''
            IF alreadyNames.HasValue(newName) THEN sv=DIALOG_MESSAGE('Template name already exists. Select a new name or use Overwrite.',DIALOG_PARENT=event.top) ELSE BEGIN
              sortElem=''
              ascElem=0

              loadTempSing=CREATE_STRUCT($
                'path','',$
                'statName','',$
                'loadBy',0,$
                'includeSub',0,$
                'sortBy', '', $
                'sortAsc',0, $
                'paramSet',paramSetNames(WIDGET_INFO(listSets_a,/DROPLIST_SELECT)), $
                'quickTemp','',$
                'pathApp','',$
                'archive',0,$
                'deleteFiles',0,$
                'deleteFilesEnd',0)

              IF alreadyNames(0) EQ '' THEN BEGIN; new single
                loadTm=CREATE_STRUCT(newName, loadTempSing)
                IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN loadTemp=replaceStructStruct(loadTemp, loadTm, thisMod) ELSE BEGIN
                  loadTemp=!Null
                  modnames=TAG_NAMES(multiOpt)
                  FOR m=0, N_TAGS(multiOpt)-1 DO loadTemp=CREATE_STRUCT(loadTemp,modnames(m),-1)
                  loadTemp=replaceStructStruct(loadTemp, loadTm, thisMod)
                ENDELSE
                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                tempnames_a=newName
                WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=0
                auto_upd,0,0
              ENDIF ELSE BEGIN; add
                loadTm=loadTemp.(thisMod)
                loadTm=CREATE_STRUCT(loadTm, newName, loadTempSing)
                loadTemp=replaceStructStruct(loadTemp, loadTm, thisMod)
                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                tempnames_a=[tempnames_a,newName]
                WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=N_ELEMENTS(tempnames_a)-1
                auto_upd,N_ELEMENTS(tempnames_a)-1,0
              ENDELSE
            ENDELSE; name exist
          ENDELSE;name specified
        ENDIF;cancel specify name
      END
      'a_duplicate':BEGIN
        currSel=WIDGET_INFO(listTemp_a, /LIST_SELECT)
        selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
        thisMod=availModNmb(selMod)
        RESTORE, thisPath+'data\config.dat'
        IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
          IF SIZE(loadTemp.(thisMod), /TNAME) NE 'STRUCT' THEN currSel=-1
        ENDIF ELSE currSel=-1
        IF currSel GE 0 THEN BEGIN
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, , LABEL_LEFT=Name the duplicate:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Save',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Name the new template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)
          IF res.Save THEN BEGIN
            IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not create new.', DIALOG_PARENT=event.top) ELSE BEGIN
              WIDGET_CONTROL, /HOURGLASS
              newname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
              alreadyNames=TAG_NAMES(loadTemp.(thisMod))
              IF alreadyNames.HasValue(newName) THEN sv=DIALOG_MESSAGE('Template name already exists. Select a new name or use Overwrite.',DIALOG_PARENT=event.top) ELSE BEGIN
                loadTm=loadTemp.(thisMod)
                loadTm=CREATE_STRUCT(loadTm, newName, loadTemp.(thisMod).(currSel))
                loadTemp=replaceStructStruct(loadTemp, loadTm, thisMod)
                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                tempnames_a=[tempnames_a,newName]
                WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=N_ELEMENTS(tempnames_a)-1
                auto_upd,N_ELEMENTS(tempnames_a)-1,0
              ENDELSE; name exist
            ENDELSE;name specified
          ENDIF;cancel specify name
        ENDIF;currSel>0
      END
      'a_overwrite':BEGIN
        selecTemp_a=WIDGET_INFO(listTemp_a, /LIST_SELECT)
        IF selecTemp_a(0) NE -1 AND tempnames_a(0) NE '' THEN BEGIN
          sv=DIALOG_MESSAGE('Are you sure you want to overwrite the selected automation template?', /Question, DIALOG_PARENT=event.top)
          IF sv EQ 'Yes' THEN BEGIN
            WIDGET_CONTROL, txtBrowse_a, GET_VALUE=newPath
            IF newPath NE '' THEN BEGIN
              WIDGET_CONTROL, /HOURGLASS
              WIDGET_CONTROL, txtBrowseApp, GET_VALUE=newPathApp
              WIDGET_CONTROL, txtStatName_a, GET_VALUE=newStatName

              loadTempSing=CREATE_STRUCT($
                'path',newPath,$
                'statName', newStatName,$
                'loadBy',WIDGET_INFO(btnOnlyLastDate,/BUTTON_SET),$
                'includeSub',WIDGET_INFO(btnInclSub,/BUTTON_SET),$
                'sortBy', sortElem, $
                'sortAsc', ascElem,$
                'paramSet',paramSetNames(WIDGET_INFO(listSets_a,/DROPLIST_SELECT)), $
                'quickTemp',quicktempnames(WIDGET_INFO(listQT_a,/DROPLIST_SELECT)),$
                'pathApp',newPathApp,$
                'archive',WIDGET_INFO(btnMoveFiles,/BUTTON_SET),$
                'deleteFiles',WIDGET_INFO(btnDeleteFiles,/BUTTON_SET),$
                'deleteFilesEnd',WIDGET_INFO(btnDeleteFilesEnd,/BUTTON_SET))

              RESTORE, thisPath+'data\config.dat'
              selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
              thisMod=availModNmb(selMod)
              loadTm=loadTemp.(thisMod)
              loadTm=replaceStructStruct(loadTm, loadTempSing, selecTemp_a)
              loadTemp=replaceStructStruct(loadTemp, loadTm, thisMod)
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              auto_upd, selecTemp_a(0),0
            ENDIF ELSE  sv=DIALOG_MESSAGE('Specify path.',DIALOG_PARENT=event.top)
          ENDIF
        ENDIF ELSE sv=DIALOG_MESSAGE('No template selected to overwrite.',DIALOG_PARENT=event.top)
      END
      'a_rename':BEGIN
        selecTemp_a=WIDGET_INFO(listTemp_a, /LIST_SELECT)
        IF selecTemp_a NE -1  AND tempnames_a(0) NE '' THEN BEGIN
          ;ask for new name
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, , LABEL_LEFT=New name:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Save',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

          IF res.Save THEN BEGIN
            IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not rename.', DIALOG_PARENT=event.top) ELSE BEGIN

              newname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))

              RESTORE, thisPath+'data\config.dat'
              selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
              thisMod=availModNmb(selMod)
              alreadyNames=TAG_NAMES(loadTemp.(thisMod))

              IF alreadyNames.HasValue(newName) THEN sv=DIALOG_MESSAGE('Template name already exists. Select a new name or use Overwrite.',DIALOG_PARENT=event.top) ELSE BEGIN

                loadTm=loadTemp.(thisMod)
                loadTempSing=loadTm.(selecTemp_a)
                loadTm=replaceStructStruct(loadTm, loadTempSing, selecTemp_a, NEW_TAG_NAME=newName)
                loadTemp=replaceStructStruct(loadTemp, loadTm, thisMod)

                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                tempnames_a=TAG_NAMES(loadTemp.(thisMod))
                WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=selecTemp_a

              ENDELSE; already exist
            ENDELSE; not specified name
          ENDIF;cancel
        ENDIF;none selected

      END
      'a_run': BEGIN
        selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
        thisMod=availModNmb(selMod)
        selecTemp_a=WIDGET_INFO(listTemp_a, /LIST_SELECT)
        IF selecTemp_a NE -1  AND tempnames_a(0) NE '' THEN BEGIN
          ;check if saved
          RESTORE, thisPath+'data\config.dat'
          equal=1
          selTemp=loadTemp.(thisMod).(selecTemp_a)
          WIDGET_CONTROL, txtBrowse_a, GET_VALUE=newPath
          WIDGET_CONTROL, txtBrowseApp, GET_VALUE=newPathApp
          IF selTemp.path NE newPath THEN equal=0 ELSE BEGIN
            IF selTemp.loadBy NE WIDGET_INFO(btnOnlyLastDate,/BUTTON_SET) THEN equal=0 ELSE BEGIN
              IF selTemp.includeSub NE WIDGET_INFO(btnInclSub,/BUTTON_SET) THEN equal=0 ELSE BEGIN
                IF ~ARRAY_EQUAL(selTemp.sortBy, sortElem) THEN equal=0 ELSE BEGIN
                  IF ~ARRAY_EQUAL(selTemp.sortAsc, ascElem) THEN equal=0 ELSE BEGIN
                    IF selTemp.paramSet NE paramSetNames(WIDGET_INFO(listSets_a,/DROPLIST_SELECT)) THEN equal=0 ELSE BEGIN
                      IF selTemp.quickTemp NE quickTempNames(WIDGET_INFO(listQT_a,/DROPLIST_SELECT)) THEN equal=0 ELSE BEGIN
                        IF selTemp.pathApp NE newPathApp THEN equal=0 ELSE BEGIN
                          IF selTemp.archive NE WIDGET_INFO(btnMoveFiles,/BUTTON_SET) THEN equal=0 ELSE BEGIN
                            IF selTemp.deleteFiles NE WIDGET_INFO(btnDeleteFiles,/BUTTON_SET) THEN equal=0 ELSE BEGIN
                              IF selTemp.deleteFilesEnd NE WIDGET_INFO(btnDeleteFilesEnd,/BUTTON_SET) THEN equal=0
                            ENDELSE
                          ENDELSE
                        ENDELSE
                      ENDELSE
                    ENDELSE
                  ENDELSE
                ENDELSE
              ENDELSE
            ENDELSE
          ENDELSE
          IF equal THEN BEGIN
            WIDGET_CONTROL, Event.top, /DESTROY
            autoStopFlag=0
            thisTempName=tempnames_a(0)
            autoTempRun, selTemp, thisMod, TEMPNAME=thisTempName
          ENDIF ELSE sv=DIALOG_MESSAGE('Saved template and current values do not match. Save before running the template.',DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('No template selected.',DIALOG_PARENT=event.top)
      END
      'a_addElem':BEGIN
        newElem=tags_imgStruct(WIDGET_INFO(listElem,/DROPLIST_SELECT))
        IF sortElem(0) EQ '' THEN BEGIN
          sortElem=newElem 
          ascElem=0
        ENDIF ELSE BEGIN
          IF N_ELEMENTS(sortElem) LT 9 THEN BEGIN
            sortElem=[sortElem,newElem]
            ascElem=[ascElem,0]
          ENDIF ELSE sv=DIALOG_MESSAGE('Maximum 9 sorting levels.',DIALOG_PARENT=event.top)
        ENDELSE
        WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
        autoChanged=1
      END
      'a_delElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel GE 0 THEN BEGIN
          IF N_ELEMENTS(sortElem) GT 1 THEN BEGIN
            sortElem=removeIDarr(sortElem, currSel)
            ascElem=removeIDarr(ascElem, currSel)
            WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
          ENDIF ELSE BEGIN
            sortElem=''
            ascElem=0
            WIDGET_CONTROL, listSort, SET_VALUE=''
          ENDELSE
          autoChanged=1
        ENDIF
      END
      'a_upElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldPrev=sortElem(currSel-1)
          newList=sortElem
          newList(currSel-1)=sortElem(currSel)
          newList(currSel)=oldPrev
          sortElem=newList
          oldPrev=ascElem(currSel-1)
          newList=ascElem
          newList(currSel-1)=ascElem(currSel)
          newList(currSel)=oldPrev
          ascElem=newList
          WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
          autoChanged=1
        ENDIF
      END
      'a_downElem':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(sortElem)-1 THEN BEGIN
          oldNext=sortElem(currSel+1)
          newList=sortElem
          newList(currSel+1)=sortElem(currSel)
          newList(currSel)=oldNext
          sortElem=newList
          oldNext=ascElem(currSel+1)
          newList=ascElem
          newList(currSel+1)=ascElem(currSel)
          newList(currSel)=oldNext
          ascElem=newList
          WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
          autoChanged=1
        ENDIF
      END
      'a_sort_asc':BEGIN
        currSel=WIDGET_INFO(listSort, /LIST_SELECT)
        IF currSel(0) NE -1 THEN BEGIN
          IF N_ELEMENTS(ascElem) NE N_ELEMENTS(sortElem) THEN asc=INTARR(N_ELEMENTS(sortElem))
          FOR i=0, N_ELEMENTS(currSel)-1 DO IF ascElem(currSel(i)) EQ 0 THEN ascElem(currSel(i))=1 ELSE ascElem(currSel(i))=0
          WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem
          autoChanged=1
        ENDIF
      END
      'btnInclSub': autoChanged=1
      'btnOnlyLastDate': autoChanged=1
      'txtStatName_a': autoChanged=1
      'txtBrowse_a': autoChanged=1
      'txtBrowseApp': autoChanged=1
      'btnMoveFiles': autoChanged=1
      'btnDeleteFiles': autoChanged=1
      'btnDeleteFilesEnd': autoChanged=1
      
      ;************** Rename DICOM setup *********

      'rde_add':BEGIN
     
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, , LABEL_LEFT=Description:, WIDTH=16, TAG=newdesc,', $
          '1, BASE,, /ROW', $
          '0,  TEXT, 0000, LABEL_LEFT=Tag group/element:, WIDTH=4, TAG=group,', $
          '2,  TEXT, 0000, LABEL_LEFT= , WIDTH=4, TAG=element,', $
          '1, BASE,, /ROW', $
          '2,  TEXT, a0, LABEL_LEFT=Format code:, WIDTH=7, TAG=formatcodestr,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='New tag setup', XSIZE=300, YSIZE=150, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.Save THEN BEGIN
          desc=STRUPCASE(IDL_VALIDNAME(res.newdesc, /CONVERT_ALL))
          RESTORE, thisPath+'data\config.dat'
          currNames=TAG_NAMES(renameTemp.tags)
          
          IF currNames.HasValue(desc) THEN BEGIN
            sv=DIALOG_MESSAGE('This description ('+desc+') already exist.',/ERROR, DIALOG_PARENT=event.top)
          ENDIF ELSE BEGIN
            WIDGET_CONTROL,/HOURGLASS
            renameTempRes=addTagRenameTemp(renameTemp, desc, [res.group,res.element], res.formatcodestr) ; in a0_functionsMini.pro
         
            IF N_ELEMENTS(renameTempRes.warnings) GT 1 THEN sv=DIALOG_MESSAGE(STRJOIN(renameTempRes.warnings,newline), DIALOG_PARENT=event.top)
            renameTemp=renameTempRes.renameTemp
            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            updRDE, N_ELEMENTS(currNames)

          ENDELSE
        ENDIF
        END
      'rde_overwrite':BEGIN
        sel=WIDGET_INFO(tbl_rde,/TABLE_SELECT);left,top,right,btm
        sel=sel(1);top - first row selected
        RESTORE, thisPath+'data\config.dat'
        tagDesc=TAG_NAMES(renameTemp.tags)
        thisTagGrEl=STRING(renameTemp.tags.(sel),FORMAT='(z04)')
        thisTagFormat=STRMID(renameTemp.tagFormats.(sel),1,STRLEN(renameTemp.tagFormats.(sel))-2)
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, '+tagDesc(sel)+' , LABEL_LEFT=Description:, WIDTH=16, TAG=newdesc,', $
          '1, BASE,, /ROW', $
          '0,  TEXT, '+thisTagGrEl(0)+', LABEL_LEFT=Tag group/element:, WIDTH=4, TAG=group,', $
          '2,  TEXT, '+thisTagGrEl(1)+', LABEL_LEFT= , WIDTH=4, TAG=element,', $
          '1, BASE,, /ROW', $
          '2,  TEXT, '+thisTagFormat+', LABEL_LEFT=Format code:, WIDTH=7, TAG=formatcodestr,', $
          '1, BASE,, /COLUMN', $
          '2, BUTTON, all templates|selected template, EXCLUSIVE, LABEL_TOP=Apply new format code to..., COLUMN, SET_VALUE=1, TAG=allOrThis', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Edit tag setup', XSIZE=300, YSIZE=300, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.Save THEN BEGIN
          desc=STRUPCASE(IDL_VALIDNAME(res.newdesc, /CONVERT_ALL))

          IF tagDesc.HasValue(desc) AND desc NE tagDesc(sel) THEN BEGIN
            sv=DIALOG_MESSAGE('This description ('+desc+') already exist for another tag setup.',/ERROR, DIALOG_PARENT=event.top)
          ENDIF ELSE BEGIN
            WIDGET_CONTROL,/HOURGLASS
            renameTempRes=changeTagRenameTemp(renameTemp, tagDesc(sel), desc, [res.group, res.element], thisTagFormat, res.formatcodestr, res.allOrThis, WIDGET_INFO(lstTemp_rdt, /LIST_SELECT))
            
            IF N_ELEMENTS(renameTempRes.warnings) GT 1 THEN sv=DIALOG_MESSAGE(STRJOIN(renameTempRes.warnings,newline), DIALOG_PARENT=event.top)
            renameTemp=renameTempRes.renameTemp
            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            updRDT, WIDGET_INFO(lstTemp_rdt, /LIST_SELECT)

          ENDELSE
        ENDIF
        END
      'rde_duplicate':BEGIN
        sel=WIDGET_INFO(tbl_rde,/TABLE_SELECT);left,top,right,btm
        sel=sel(1);top - first row selected
        RESTORE, thisPath+'data\config.dat'
        tagDesc=TAG_NAMES(renameTemp.tags)
        box=[$
          '1, BASE,, /ROW', $
          '2,  TEXT, '+tagDesc(sel)+' , LABEL_LEFT=New description:, WIDTH=16, TAG=newdesc,', $
          '1, BASE,, /ROW', $
          '0, BUTTON, Save, QUIT, TAG=Save',$
          '2, BUTTON, Cancel, QUIT, TAG=Cancel']
        res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Duplicate tag setup', XSIZE=300, YSIZE=150, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

        IF res.Save THEN BEGIN
          desc=STRUPCASE(IDL_VALIDNAME(res.newdesc, /CONVERT_ALL))
          IF desc EQ tagDesc(sel) OR tagDesc.HasValue(desc) THEN BEGIN
            sv=DIALOG_MESSAGE('This new description ('+desc+') already exist. Duplication not possible.',/ERROR, DIALOG_PARENT=event.top)
          ENDIF ELSE BEGIN
            WIDGET_CONTROL,/HOURGLASS       
            f=renameTemp.tagFormats.(sel)
            renameTempRes=addTagRenameTemp(renameTemp, desc, renameTemp.tags.(sel), STRMID(f,1,STRLEN(f)-2)) ; in a0_functionsMini.pro

            IF N_ELEMENTS(renameTempRes.warnings) GT 1 THEN sv=DIALOG_MESSAGE(STRJOIN(renameTempRes.warnings,newline), DIALOG_PARENT=event.top)
            renameTemp=renameTempRes.renameTemp
            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            updRDE, N_ELEMENTS(tagDesc)
            
          ENDELSE
          ENDIF
        END
        'rde_upTemp':BEGIN
        sel=WIDGET_INFO(tbl_rde,/TABLE_SELECT);left,top,right,btm
        sel=sel(1);top - first row selected
        IF sel GT 0 THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          tagDesc=TAG_NAMES(renameTemp.tags)
          oldOrder=INDGEN(N_ELEMENTS(tagDesc))
          newOrder=oldOrder
          newOrder[sel-1:sel]=REVERSE(oldOrder[sel-1:sel])

          newtags=reorderStructStruct(renameTemp.tags, newOrder)
          renameTemp=replaceStructStruct(renameTemp,newtags,0)
          newformats=reorderStructStruct(renameTemp.tagFormats, newOrder)
          renameTemp=replaceStructStruct(renameTemp,newformats,1)

          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          updRDE, sel-1
        ENDIF
        END
      'rde_downTemp':BEGIN
        sel=WIDGET_INFO(tbl_rde,/TABLE_SELECT);left,top,right,btm
        sel=sel(1);top - first row selected
        RESTORE, thisPath+'data\config.dat'
        tagDesc=TAG_NAMES(renameTemp.tags)
        IF sel LT N_ELEMENTS(tagDesc)-1 THEN BEGIN        
          oldOrder=INDGEN(N_ELEMENTS(tagDesc))
          newOrder=oldOrder
          newOrder[sel:sel+1]=REVERSE(oldOrder[sel:sel+1])

          newtags=reorderStructStruct(renameTemp.tags, newOrder)
          renameTemp=replaceStructStruct(renameTemp,newtags,0)
          newformats=reorderStructStruct(renameTemp.tagFormats, newOrder)
          renameTemp=replaceStructStruct(renameTemp,newformats,1)

          SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
          updRDE, sel+1
        ENDIF
        END
      'rde_delTemp':BEGIN
        sel=WIDGET_INFO(tbl_rde,/TABLE_SELECT);left,top,right,btm
        sel=sel(1);top - first row selected
        RESTORE, thisPath+'data\config.dat'
        tagDesc=TAG_NAMES(renameTemp.tags)
        IF sel GT -1 THEN BEGIN
          thisDesc=tagDesc(sel)
          IF N_ELEMENTS(tagDesc) GT 1 THEN BEGIN
            ;check if in use
            nTemp=N_TAGS(renameTemp.temp)
            rentempnames=TAG_NAMES(renameTemp.temp)
            inUse=''
            FOR i=0, nTemp-1 DO BEGIN
              matchcat=WHERE(STRUPCASE(renameTemp.temp.(i).cat) EQ thisDesc)
              matchfile=WHERE(STRUPCASE(renameTemp.temp.(i).file) EQ thisDesc)
              IF matchcat(0) NE -1 OR matchfile(0) NE -1 THEN BEGIN
                inUse=[inUse,rentempnames(i)]
              ENDIF
            ENDFOR
            
            IF N_ELEMENTS(inUse) GT 1 THEN BEGIN
              sv=DIALOG_MESSAGE('Tag in use for the following templates:'+STRJOIN(inUse,newline)+newline+'Can not delete.', DIALOG_PARENT=event.top)
            ENDIF ELSE BEGIN
              newtags=removeIDStructStruct(renameTemp.tags, sel)
              newformats=removeIDStructStruct(renameTemp.tagFormats, sel)

              nTemp=N_TAGS(renameTemp.temp)
              newtemps=renameTemp.temp
              FOR t=0, nTemp-1 DO BEGIN
                newformats=removeIDStructStruct(renameTemp.temp.(t).formats, sel)
                newtemp=replaceStructStruct(renameTemp.temp.(t),newformats,2)
                newtemps=replaceStructStruct(newtemps,newtemp,t)
              ENDFOR
              ;renameTemp=replaceStructStruct(renameTemp,newtemps,2)
             renameTemp=CREATE_STRUCT('tags', newtags, 'tagformats', newformats, 'temp', newtemps)
    
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              updRDE, 0
            ENDELSE
          ENDIF ELSE sv=DIALOG_MESSAGE('Can not delete last row.', DIALOG_PARENT=event.top)
        ENDIF ELSE sv=DIALOG_MESSAGE('No row selected.', DIALOG_PARENT=event.top)
        END

      'rdt_listTemp': updRDT, WIDGET_INFO(lstTemp_rdt, /LIST_SELECT); update test (pro in this file)
      'rdt_duplicate': BEGIN
        currSel=WIDGET_INFO(lstTemp_rdt, /LIST_SELECT)
        IF currSel(0) GE 0 THEN BEGIN
          ;ask for new name
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, , LABEL_LEFT=Name the new template:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Save',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Name the duplicate', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

          IF res.Save THEN BEGIN
            IF res.newname EQ '' THEN sv=DIALOG_MESSAGE('No name specified. Could not duplicate.', DIALOG_PARENT=event.top) ELSE BEGIN

              tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
              RESTORE, thisPath+'data\config.dat'

              currNames=TAG_NAMES(renameTemp.temp)

              IF currNames.HasValue(tempname) THEN BEGIN
                sv=DIALOG_MESSAGE('Duplication failed. A template with this name already exist.',DIALOG_PARENT=event.top)
              ENDIF ELSE BEGIN
                new_temps=CREATE_STRUCT(renameTemp.temp, tempname, renameTemp.temp.(currSel(0)))
                renameTemp=replaceStructStruct(renameTemp,new_temps,2)

                SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
                rdt_names=TAG_NAMES(renameTemp.temp)
                updRDT, N_ELEMENTS(rdt_names)-1
              ENDELSE

            ENDELSE
          ENDIF
        ENDIF; any selected
      END
      'rdt_rename':BEGIN
       currSel=WIDGET_INFO(lstTemp_rdt, /LIST_SELECT)
        IF currSel(0) GE 0 THEN BEGIN
          rdt_nameThis=rdt_names(currSel)
          box=[$
            '1, BASE,, /ROW', $
            '2,  TEXT, '+rdt_nameThis+', LABEL_LEFT=New name:, WIDTH=12, TAG=newname,', $
            '1, BASE,, /ROW', $
            '0, BUTTON, Save, QUIT, TAG=Save',$
            '2, BUTTON, Cancel, QUIT, TAG=Cancel']
          res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Rename template', XSIZE=300, YSIZE=100, FOCUSNO=1, XOFFSET=xoffset+200, YOFFSET=yoffset+200)

          IF res.Save AND res.newname NE '' THEN BEGIN
            tempname=STRUPCASE(IDL_VALIDNAME(res.newname, /CONVERT_ALL))
            IF rdt_names.HasValue(tempname) THEN BEGIN
              sv=DIALOG_MESSAGE('Template name '+tempname+' already exist. Renaming not possible.', DIALOG_PARENT=event.Top)
            ENDIF ELSE BEGIN
              RESTORE, thisPath+'data\config.dat'
              ;replace temp
              new_temps=replaceStructStruct(renameTemp.temp,renameTemp.temp.(currSel), currSel, NEW_TAG_NAME=tempname)
              renameTemp=replaceStructStruct(renameTemp,new_temps,2)
              
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              rdt_names=TAG_NAMES(renameTemp.temp)
              updRDT, currSel
            ENDELSE
          ENDIF
        ENDIF; any selected
      END
      'rdt_upTemp':BEGIN
        currSel=WIDGET_INFO(lstTemp_rdt, /LIST_SELECT)
        IF currSel GT 0 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(rdt_names))
          newOrder=oldOrder
          newOrder[currSel-1:currSel]=REVERSE(oldOrder[currSel-1:currSel])
          RESTORE, thisPath+'data\config.dat'

          new_temps=reorderStructStruct(renameTemp.temp, newOrder)
              renameTemp=replaceStructStruct(renameTemp,new_temps,2)
              
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              rdt_names=TAG_NAMES(renameTemp.temp)
              updRDT, currSel-1
        ENDIF
      END
      'rdt_downTemp':BEGIN
        currSel=WIDGET_INFO(lstTemp_rdt, /LIST_SELECT)
        IF currSel LT N_ELEMENTS(rdt_names)-1 THEN BEGIN
          oldOrder=INDGEN(N_ELEMENTS(rdt_names))
          newOrder=oldOrder
          newOrder[currSel:currSel+1]=REVERSE(oldOrder[currSel:currSel+1])
          RESTORE, thisPath+'data\config.dat'
          new_temps=reorderStructStruct(renameTemp.temp, newOrder)
              renameTemp=replaceStructStruct(renameTemp,new_temps,2)
              
              SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
              rdt_names=TAG_NAMES(renameTemp.temp)
              updRDT, currSel+1
        ENDIF
      END
      'rdt_delete':BEGIN
        sel=WIDGET_INFO(lstTemp_rdt, /LIST_SELECT)
        IF sel(0) GT -1 THEN BEGIN
          RESTORE, thisPath+'data\config.dat'
          IF N_ELEMENTS(rdt_names) EQ 1 THEN BEGIN
            sv=DIALOG_MESSAGE('At least one template needed. Can not delete this template.', DIALOG_PARENT=event.Top)
          ENDIF ELSE BEGIN
            new_temps=removeIDStructStruct(renameTemp.temp, sel)
            renameTemp=replaceStructStruct(renameTemp,new_temps,2)

            SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'
            rdt_names=TAG_NAMES(renameTemp.temp)
            updRDT, 0
          ENDELSE
       ENDIF
      END
      'formatTestUpdate':BEGIN
        WIDGET_CONTROL, txtInputTest, GET_VALUE=txtInput
        WIDGET_CONTROL, txtFormatTest,GET_VALUE=txtFormat
        
        Catch, Error_status
        IF Error_status NE 0 THEN BEGIN
          outputTxt='Code not valid'
          txtFormat='a0'
          CATCH, /CANCEL
        ENDIF
        b=string(txtInput(0),FORMAT='('+txtFormat(0)+')');cause error?
        
        IF Error_status EQ 0 THEN outputTxt=b
        
        WIDGET_CONTROL, txtOutputTest, SET_VALUE=outputTxt
        END
      
      ELSE:
    ENDCASE

  ENDIF

  ;New tab selected
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TAB') THEN BEGIN
    selTab=WIDGET_INFO(event.ID, /TAB_CURRENT)
    IF autoChanged THEN sv=DIALOG_MESSAGE('Possible unsaved changes to automation template were lost.', DIALOG_PARENT=event.Top)
    CASE selTab OF
      0:s_upd, selConfig-1, 1
      1:qt_upd, 0
      2:qto_updTemp
      3:auto_upd, 0, 1
      ELSE:
    ENDCASE
  ENDIF

  ;QT output table events
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' THEN BEGIN

    sel=WIDGET_INFO(tblQTout,/TABLE_SELECT);left,top,right,btm
    sel=sel(1);top - first row selected
    IF sel NE qto_currSel THEN BEGIN
      RESTORE, thisPath+'data\config.dat'
      ;find testNmb and outputNmb
      currTemp=quickTout.(qto_currMod).(qto_currTemp)
      nTests=N_TAGS(currTemp)
      outputs=!Null
      FOR i=0, nTests-1 DO BEGIN
        IF SIZE(currTemp.(i), /TNAME) EQ 'STRUCT' THEN outputs=[outputs, INTARR(N_TAGS(currTemp.(i)))+i] ELSE outputs=[outputs, i]
      ENDFOR
      IF sel LT N_ELEMENTS(outputs) THEN BEGIN
        qto_currTest=outputs(sel)
        actOutputs=WHERE(outputs EQ qto_currTest, nActOut)
        outputs=outputs*0
        outputs(actOutputs)=INDGEN(nActOut)
        qto_currOutp=outputs(sel)
        qto_currSel=sel

        qto_UpdSelections
      ENDIF
    ENDIF

  ENDIF

  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    sv='Yes'
    IF autoChanged THEN sv=DIALOG_MESSAGE('Possible unsaved changed for the automation template not saved. Continue without saving?', /QUESTION, DIALOG_PARENT=event.top)
    IF sv EQ 'Yes' THEN BEGIN
      RESTORE, thisPath+'data\config.dat'
      IF N_ELEMENTS(quickTemp) NE 0 THEN BEGIN
        IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
          IF SIZE(quickTemp.(modality), /TNAME) EQ 'STRUCT' THEN fillQuickTempList, quickTemp.(modality) ELSE fillQuickTempList, -1
        ENDIF ELSE fillQuickTempList, -1
      ENDIF ELSE fillQuickTempList, -1
      WIDGET_CONTROL, event.top, /DESTROY
    ENDIF
  ENDIF

end

pro s_upd, listNmb, allUpd
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, /HOURGLASS

  RESTORE, thisPath+'data\config.dat'
  paramNames=TAG_NAMES(configS.(1))

  setNames=TAG_NAMES(configS)
  setNames=setNames[1:-1]
  autoNames_s=!Null
  IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
    IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
      modNames=TAG_NAMES(loadTemp)
      FOR m=0, N_TAGS(loadTemp)-1 DO BEGIN
        IF SIZE(loadTemp.(m), /TNAME) EQ 'STRUCT' THEN BEGIN
          autoAll=TAG_NAMES(loadTemp.(m))
          FOR i = 0, N_ELEMENTS(autoAll)-1 DO BEGIN
            IF STRUPCASE(loadTemp.(m).(i).PARAMSET) EQ setNames(listNmb) THEN autoNames_s=[autonames_s, modNames(m)+' / '+ autoAll(i)]
          ENDFOR
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  IF N_ELEMENTS(autoNames_s) GT 0 THEN WIDGET_CONTROL, listParamAuto_s, SET_VALUE=autoNames_s ELSE WIDGET_CONTROL, listParamAuto_s, SET_VALUE=''

  setNames(configS.(0)-1)=setNames(configS.(0)-1)+' (default)'
  WIDGET_CONTROL, listSets_s, SET_VALUE=setNAmes, SET_LIST_SELECT=listNmb

  ;outputTemps
  If allUpd THEN BEGIN
    WIDGET_CONTROL, lstCT_s, SET_VALUE=TAG_NAMES(quickTout.CT)
    WIDGET_CONTROL, lstX_s,SET_VALUE=TAG_NAMES(quickTout.Xray)
    WIDGET_CONTROL, lstNM_s, SET_VALUE=TAG_NAMES(quickTout.NM)
    WIDGET_CONTROL, lstMR_s, SET_VALUE=TAG_NAMES(quickTout.MR)
  Endif
  tempNamesSel=configS.(listNmb+1).qtOutTemps
  nmbs=INTARR(5)
  errMsg=''
  FOR i=0, N_TAGS(testVisualQTNames)-1 DO BEGIN
    tempNames=TAG_NAMES(quickTout.(i))
    nmb=WHERE(tempNames EQ tempNamesSel(i))
    IF nmb NE -1 THEN nmbs(i)=nmb ELSE errMsg= tempNamesSel(i) + ' '+newline
  ENDFOR
  WIDGET_CONTROL, lstCT_s, SET_DROPLIST_SELECT=nmbs(0)
  WIDGET_CONTROL, lstX_s, SET_DROPLIST_SELECT=nmbs(1)
  WIDGET_CONTROL, lstNM_s, SET_DROPLIST_SELECT=nmbs(2)
  WIDGET_CONTROL, lstMR_s, SET_DROPLIST_SELECT=nmbs(3)

  ;output
  output_rows=WHERE(configSinfo[2,*] EQ '-1', nOutpRows)
  IF allUpd THEN BEGIN
    getParam, configTemp
    tOutput=STRARR(3, nOutpRows)
    tOutput[0,*]=configSinfo[1,output_rows]
  ENDIF ELSE WIDGET_CONTROL, tblOutputSett, GET_VALUE=tOutput

  FOR i=0, nOutpRows-1 DO BEGIN
    tagNameThis=STRUPCASE(configSinfo[0,output_rows(i)])
    nThis=WHERE(paramNames EQ tagNameThis)
    tOutput[1,i]=STRING(configS.(listNmb+1).(nThis), FORMAT=formatCodeType(configS.(listNmb+1).(nThis), configSinfo[4,output_rows(i)]))
    IF allUpd THEN tOutput[2,i]=STRING(configTemp.(nThis), FORMAT=formatCodeType(configTemp.(nThis), configSinfo[4,output_rows(i)]))
  ENDFOR
  WIDGET_CONTROL, tblOutputSett, SET_VALUE=tOutput, BACKGROUND_COLOR=[255,255,255]
  rowNotEq=!Null
  FOR i=0, nOutpRows-1 DO IF tOutput[1,i] NE tOutput[2,i] THEN rowNotEq=[rowNotEq,i]
  IF N_ELEMENTs(rowNotEq) GT 0 THEN BEGIN
    FOR i=0, N_ELEMENTS(rowNotEq)-1 DO WIDGET_CONTROL, tblOutputSett, USE_TABLE_SELECT=[2,rowNotEq(i),2,rowNotEq(i)], BACKGROUND_COLOR=[255,100,100]
  ENDIF

  ;tests
  test_rows=WHERE(LONG(configSinfo[2,*]) GE 0, nTestRows)
  IF allUpd THEN BEGIN
    tTest=STRARR(4, nTestRows)
    modals=configSinfo[2,test_rows]
    ids=WHERE(modals EQ '0') & IF ids(0) NE -1 THEN modals(ids)='0CT '
    ids=WHERE(modals EQ '1') & IF ids(0) NE -1 THEN modals(ids)='1X  '
    ids=WHERE(modals EQ '2') & IF ids(0) NE -1 THEN modals(ids)='2NM '
    ids=WHERE(modals EQ '3') & IF ids(0) NE -1 THEN modals(ids)='3ST '
    ids=WHERE(modals EQ '4') & IF ids(0) NE -1 THEN modals(ids)='4PT '
    ids=WHERE(modals EQ '5') & IF ids(0) NE -1 THEN modals(ids)='5MR '
    ids=WHERE(modals EQ '0/1') & IF ids(0) NE -1 THEN modals(ids)='5CT/X '
    testTyp=configSinfo[3, test_rows]
    firstColText=modals+testTyp
    orderTblTestSett=BSORT(firstColText)
    tTest[0,*]=firstColText(orderTblTestSett)
    FOR i=0, nTestRows-1 DO tTest[0,i]=STRMID(tTest[0,i],1)
    paramtext=configSinfo[1,test_rows]
    tTest[1,*]=paramtext(orderTblTestSett)
  ENDIF ELSE WIDGET_CONTROL, tblTestSett, GET_VALUE=tTest

  col2=STRARR(nTestRows) & col3=tTest[3,*]
  FOR i=0, nTestRows-1 DO BEGIN
    tagNameThis=STRUPCASE(configSinfo[0,test_rows(i)])
    nThis=WHERE(paramNames EQ tagNameThis)
    col2(i)=STRING(configS.(listNmb+1).(nThis), FORMAT=formatCodeType(configS.(listNmb+1).(nThis), configSinfo[4,test_rows(i)]))
    IF allUpd THEN col3(i)=STRING(configTemp.(nThis), FORMAT=formatCodeType(configTemp.(nThis), configSinfo[4,test_rows(i)]))
  ENDFOR

  tTest[2,*]=col2(orderTblTestSett)
  IF allUpd THEN tTest[3,*]=col3(orderTblTestSett)

  WIDGET_CONTROL, tblTestSett, SET_VALUE=tTest, BACKGROUND_COLOR=[255,255,255]
  rowNotEq=!Null
  FOR i=0, nTestRows-1 DO IF tTest[2,i] NE tTest[3,i] THEN rowNotEq=[rowNotEq,i]
  IF N_ELEMENTs(rowNotEq) GT 0 THEN BEGIN
    FOR i=0, N_ELEMENTS(rowNotEq)-1 DO WIDGET_CONTROL, tblTestSett, USE_TABLE_SELECT=[3,rowNotEq(i),3,rowNotEq(i)], BACKGROUND_COLOR=[255,100,100]
  ENDIF

  ;material table
  If allUpd THEN BEGIN
    ysz=N_ELEMENTS(configTemp.LINTAB.Materials)
    fillLin=STRARR(4,ysz)
    fillLin[0,*]=TRANSPOSE(configTemp.LINTAB.Materials)
    fillLin[1,*]=STRING(TRANSPOSE(configTemp.LINTAB.posX), FORMAT='(f0.1)')
    fillLin[2,*]=STRING(TRANSPOSE(configTemp.LINTAB.posY), FORMAT='(f0.1)')
    fillLin[3,*]=STRING(TRANSPOSE(configTemp.LINTAB.RelMassD), FORMAT='(f0.3)')
    WIDGET_CONTROL, tblCurrMaterials, TABLE_YSIZE=ysz, SET_VALUE=fillLin, SET_TABLE_SELECT=[0,0,0,0]
  ENDIF

  ysz=N_ELEMENTS(configS.(listNmb+1).LINTAB.Materials)
  fillLin=STRARR(4,ysz)
  fillLin[0,*]=TRANSPOSE(configS.(listNmb+1).LINTAB.Materials)
  fillLin[1,*]=STRING(TRANSPOSE(configS.(listNmb+1).LINTAB.posX), FORMAT='(f0.1)')
  fillLin[2,*]=STRING(TRANSPOSE(configS.(listNmb+1).LINTAB.posY), FORMAT='(f0.1)')
  fillLin[3,*]=STRING(TRANSPOSE(configS.(listNmb+1).LINTAB.RelMassD), FORMAT='(f0.3)')
  WIDGET_CONTROL, tblSelMaterials, TABLE_YSIZE=ysz, SET_VALUE=fillLin, SET_TABLE_SELECT=[0,0,0,0]

end

pro qt_upd, listNmb
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, /HOURGLASS
  emptyCheck=0
  RESTORE, thisPath+'data\config.dat'
  modSel=WIDGET_INFO(lstModality_QT,/DROPLIST_SELECT)
  modNmb=availModNmb(modSel)

  WIDGET_CONTROL, lstTest_qt, SET_VALUE=testVisualQTNames.(modSel)

  IF N_ELEMENTS(quickTemp) NE 0 THEN BEGIN
    IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
      IF SIZE(quickTemp.(modNmb), /TNAME) EQ 'STRUCT' THEN BEGIN
        QTnames=TAG_NAMES(quickTemp.(modNmb))
        markArr=quickTemp.(modNmb).(listNmb)
        If markArr(0) NE -1 THEN BEGIN
          szMM=SIZE(markArr, /DIMENSIONS)
          IF N_ELEMENTS(szMM) EQ 1 THEN szMM=[szMM,1]
          strQTarr=STRARR(szMM(1))
          FOR i=0, szMM(1)-1 DO BEGIN
            FOR j=0, szMM(0)-1 DO BEGIN
              IF markArr[j,i] THEN strQTarr(i)=strQTarr(i)+STRING(j+1, FORMAT='(i0)') ELSE strQTarr(i)=strQTarr(i)+'- '
            ENDFOR
          ENDFOR
          WIDGET_CONTROL, lstQT, SET_VALUE=strQTarr
          WIDGET_CONTROL, txtNimgTest_qt, SET_VALUE=STRING(szMM(1),FORMAT='(i0)')
        ENDIF ELSE BEGIN
          WIDGET_CONTROL, lstQT, SET_VALUE=''
          WIDGET_CONTROL, txtNimgTest_qt, SET_VALUE=''
        ENDELSE


        WIDGET_CONTROL, lstTempQT, SET_VALUE=QTnames, SET_LIST_SELECT=listNmb

        autoNames_qt=!Null
        IF N_ELEMENTS(loadTemp) NE 0 THEN BEGIN
          IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
            IF SIZE(loadTemp.(modNmb), /TNAME) EQ 'STRUCT' THEN BEGIN
              autoAll=TAG_NAMES(loadTemp.(modNmb))
              FOR i = 0, N_ELEMENTS(autoAll)-1 DO BEGIN
                IF STRUPCASE(loadTemp.(modNmb).(i).QUICKTEMP) EQ QTnames(listNmb) THEN autoNames_qt=[autonames_qt, autoAll(i)]
              ENDFOR
            ENDIF
          ENDIF
        ENDIF
        IF N_ELEMENTS(autoNames_qt) GT 0 THEN WIDGET_CONTROL, lstQTusedAuto, SET_VALUe=autoNames_qt ELSE WIDGET_CONTROL, lstQTusedAuto, SET_VALUe=''
      ENDIF ELSE emptyCheck=1
    ENDIF ELSE emptyCheck=1
  ENDIF ELSE emptyCheck=1

  IF emptyCheck THEN BEGIN
    WIDGET_CONTROL, lstQT, SET_VALUE=''
    WIDGET_CONTROL, lstTempQT, SET_VALUE=''
    WIDGET_CONTROL, lstQTusedAuto, SET_VALUe=''
  ENDIF

end

pro qto_updMode
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden

  WIDGET_CONTROL, /HOURGLASS
  qto_currTemp=0;currently selected template

  RESTORE, thisPath+'data\config.dat'
  tempstruct=quickTout.(qto_currMod)
  WIDGET_CONTROL, lstTemplates_qto, YSIZE=N_TAGS(tempstruct), SET_VALUE=TAG_NAMES(tempstruct), SET_LIST_SELECT=qto_currTemp, SCR_YSIZE=150
  WIDGET_CONTROL, lstTest, SET_VALUE=testVisualQTNames.(qto_currMod), SET_DROPLIST_SELECT=0
  WIDGET_CONTROL, lstAlt, SET_VALUE=TAG_NAMES(tableHeaders.(qto_currMod).(0)), SET_DROPLIST_SELECT=0
  WIDGET_CONTROL, lstCols, SET_VALUE=tableHeaders.(qto_currMod).(0).(0), SET_LIST_SELECT=0

  IF qto_currMod EQ 1 OR qto_currMod EQ 2 THEN WIDGET_CONTROL, lstPer, SET_DROPLIST_SELECT=0, SENSITIVE=0 ELSE WIDGET_CONTROL, lstPer, SENSITIVE=1

  qto_updTemp

end

pro qto_updTemp
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden
  WIDGET_CONTROL, /HOURGLASS
  qto_currTest=0;currently selected test droplist
  qto_currOutp=0;currently selected output-number for current test
  qto_currSel=0;currently selected row in table

  setNames_qto=!Null
  RESTORE, thisPath+'data\config.dat'
  qtoNames=TAG_NAMES(quickTout.(qto_currMod))
  setAll=TAG_NAMES(configS)
  setAll=setAll[1:-1]

  FOR i = 0, N_ELEMENTS(setAll)-1 DO BEGIN
    qtouttempsThis=configS.(i+1).QTOUTTEMPS
    IF STRUPCASE(qtouttempsThis(qto_currMod)) EQ qtoNames(qto_currTemp) THEN setNames_qto=[setNames_qto, setAll(i)]
  ENDFOR
  autoNames_qto=!Null
  IF N_ELEMENTS(setNames_qto) NE 0 AND SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
    autoNames_qto=STRARR(N_ELEMENTS(setNames_qto))
    IF SIZE(loadTemp.(qto_currMod), /TNAME) EQ 'STRUCT' THEN BEGIN
      paramAutoThisMod=!Null
      loadTempNames=TAG_NAMES(loadTemp.(qto_currMod))
      FOR i=0, N_TAGS(loadTemp.(qto_currMod))-1 DO paramAutoThisMod=[paramAutoThisMod, loadTemp.(qto_currMod).(i).PARAMSET]
      FOR i = 0, N_ELEMENTS(setNames_qto)-1 DO BEGIN
        ids=WHERE(paramAutoThisMod EQ setNames_qto(i))
        IF ids(0) NE -1 THEN BEGIN
          FOR a=0, N_ELEMENTS(ids)-1 DO autoNames_qto(i)=autoNames_qto(i) + ' ' +  loadTempNames(ids(a))
        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  IF N_ELEMENTS(setNames_qto) GT 0 THEN BEGIN
    listText=setNames_qto
    IF N_ELEMENTS(autoNames_qto) GT 0 THEN listText=listText + ' ( ' + autoNames_qto + ' )'
    WIDGET_CONTROL, lstQTOusedParam, SET_VALUE=listText
  ENDIF ELSE WIDGET_CONTROL, lstQTOusedParam, SET_VALUE=''

  qto_fillTable
end

pro qto_fillTable
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden
  WIDGET_CONTROL, /HOURGLASS
  RESTORE, thisPath+'data\config.dat'
  tempstruct=quickTout.(qto_currMod).(qto_currTemp)

  WIDGET_CONTROL, tblQTout, SET_VALUE=STRARR(6,100)

  nTests=N_TAGS(tempstruct)
  testNames=TAG_NAMES(tempstruct)
  calcStrings=['=','Min','Max','Avg','Std','Max abs']
  co=0
  FOR i=0,nTests-1 DO BEGIN; for each defined test
    IF SIZE(tempstruct.(i), /TNAME) NE 'STRUCT' THEN BEGIN
      altString='1'
      colString='All'
      calcString='='
      prString='Per image'
      descr=''
      WIDGET_CONTROL, tblQTout, SET_VALUE=[testNames(i),altString,colString,calcString,prString,descr], USE_TABLE_SELECT=[0,co,5,co]
      co=co+1
    ENDIF ELSE BEGIN
      nTouts=N_TAGS(tempstruct.(i))
      namesTouts=TAG_NAMES(tempstruct.(i))
      FOR j=0, nTouts-1 DO BEGIN; for each defined output
        testName=testNames(i)
        altString=STRING(tempstruct.(i).(j).ALT+1,FORMAT='(i0)')
        colString=STRJOIN(STRING(tempstruct.(i).(j).COLUMNS,FORMAT='(i0)'),',')
        calcString=calcStrings(tempstruct.(i).(j).CALC)
        If tempstruct.(i).(j).PER_SERIES THEN prString='Per series' ELSE prString='Per image'
        descr=namesTouts(j)
        WIDGET_CONTROL, tblQTout, SET_VALUE=[testName,altString,colString,calcString,prString,descr], USE_TABLE_SELECT=[0,co,5,co]
        co=co+1
      ENDFOR
    ENDELSE
  ENDFOR

  WIDGET_CONTROL, tblQTout, SET_TABLE_SELECT=[0,qto_currSel,0,qto_currSel]
  qto_UpdSelections
end

pro qto_UpdSelections
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden
  WIDGET_CONTROL, /HOURGLASS
  WIDGET_CONTROL, tblQTout, GET_VALUE=tabRow, USE_TABLE_SELECT=[0,qto_currSel,5,qto_currSel]

  WIDGET_CONTROL, lstTest, SET_DROPLIST_SELECT=qto_currTest
  WIDGET_CONTROL, lstAlt, SET_VALUE=TAG_NAMES(tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT))), SET_DROPLIST_SELECT=LONG(tabRow(1))-1
  colStrings=tableHeaders.(qto_currMod).(WIDGET_INFO(lstTest,/DROPLIST_SELECT)).(WIDGET_INFO(lstAlt,/DROPLIST_SELECT))
  IF tabRow(2) EQ 'All' THEN cols=INDGEN(N_ELEMENTS(colStrings)) ELSE cols=LONG(STRSPLIT(tabRow(2),',',/EXTRACT))
  WIDGET_CONTROL, lstCols, SET_VALUE=colStrings, SET_LIST_SELECT=cols
  WIDGET_CONTROL, lstCalc, GET_VALUE=calStrings
  WIDGET_CONTROL, lstCalc, SET_DROPLIST_SELECT=WHERE(tabRow(3) EQ calStrings)
  IF tabRow(4) EQ 'Per image' THEN WIDGET_CONTROL, lstPer, SET_DROPLIST_SELECT=0 ELSE WIDGET_CONTROL, lstPer, SET_DROPLIST_SELECT=1
  WIDGET_CONTROL, txtDescr, SET_VALUE=tabRow(5)

end

;first = 2 if modality change and QuickTest-list should be updated
;first = -1 if regret change (when auto changed and not saved)
pro auto_upd, selT, first
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden
  WIDGET_CONTROL, /HOURGLASS
  selMod=WIDGET_INFO(lstModality_a,/DROPLIST_SELECT)
  a_currMod=selMod
  thisMod=availModNmb(selMod)

  IF first GE 1 THEN BEGIN; only first time modal opens or tab switches with possible change to config

    RESTORE, thisPath+'data\config.dat'

    IF first EQ 1 THEN BEGIN
      paramSetNames=STRUPCASE(TAG_NAMES(configS))
      paramSetNames=paramSetNames[1:-1]
      WIDGET_CONTROL, listSets_a, SET_VALUE=paramSetNames, SET_LIST_SELECT=0
      WIDGET_CONTROL, txtAutoImpPath, SET_VALUE=configS.(1).AUTOIMPORTPATH
    ENDIF

    tempnames_a=''
    IF SIZE(loadTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
      IF SIZE(loadTemp.(thisMod), /TNAME) EQ 'STRUCT' THEN tempnames_a=TAG_NAMES(loadTemp.(thisMod))
    ENDIF
    WIDGET_CONTROL, listTemp_a, SET_VALUE=tempnames_a, SET_LIST_SELECT=0

    includeArr=actualTags(allTags, imgStructInfo, thisMod)
    idsInclude=WHERE(includeArr EQ 1)
    tags_imgStruct=allTags(idsInclude)
    tags_imgStructDesc=allDesc(idsInclude)
    WIDGET_CONTROL, listElem, SET_VALUE=tags_imgStructDesc, SET_LIST_SELECT=0

    quickTempNames=''
    IF N_ELEMENTS(quickTemp) NE 0 THEN BEGIN
      IF SIZE(quickTemp, /TNAME) EQ 'STRUCT' THEN BEGIN
        IF SIZE(quickTemp.(thisMod), /TNAME) EQ 'STRUCT' THEN quickTempNames=['',STRUPCASE(TAG_NAMES(quickTemp.(thisMod)))]
      ENDIF
    ENDIF
    WIDGET_CONTROL, listQT_a, SET_VALUE=quickTempNames, SET_DROPLIST_SELECT=0

  ENDIF

  IF tempnames_a(0) EQ '' THEN selT=-1

  IF selT NE -1 THEN BEGIN; selT = -1 if no template exist
    RESTORE, thisPath+'data\config.dat'
    autoChanged=0
    WIDGET_CONTROL, txtBrowse_a, SET_VALUE=loadTemp.(thisMod).(selT).path
    WIDGET_CONTROL, txtStatName_a, SET_VALUE=loadTemp.(thisMod).(selT).statName
    WIDGET_CONTROL, txtBrowseApp, SET_VALUE=loadTemp.(thisMod).(selT).pathApp
    WIDGET_CONTROL, btnOnlyLastDate, SET_BUTTON=loadTemp.(thisMod).(selT).loadBy
    WIDGET_CONTROL, btnInclSub, SET_BUTTON=loadTemp.(thisMod).(selT).includeSub
    WIDGET_CONTROL, btnMoveFiles, SET_BUTTON=loadTemp.(thisMod).(selT).archive
    WIDGET_CONTROL, btnDeleteFiles, SET_BUTTON=loadTemp.(thisMod).(selT).deleteFiles
    WIDGET_CONTROL, btnDeleteFilesEnd, SET_BUTTON=loadTemp.(thisMod).(selT).deleteFilesEnd
    sortElem=loadTemp.(thisMod).(selT).sortBy
    ascElem=loadTemp.(thisMod).(selT).sortAsc
    IF N_ELEMENTS(sortElem) GT N_ELEMENTS(ascElem) THEN BEGIN
      ascTemp=INTARR(N_ELEMENTS(sortElem))
      ascTemp[0:N_ELEMENTS(ascElem)-1]=ascElem
      ascElem=ascTemp
    ENDIF
    IF sortElem(0) NE '' THEN WIDGET_CONTROL, listSort, SET_VALUE=ascDesc01(ascElem)+sortElem ELSE WIDGET_CONTROL, listSort, SET_VALUE=''
    ;print, 'sortElem', sortElem
    ;paramSet - exists still?
    paramSetName=STRUPCASE(loadTemp.(thisMod).(selT).paramSet)
    IF paramSetNames.HasValue(paramSetName) THEN BEGIN
      selNo=WHERE(paramSetNames EQ paramSetName)
      selParam=selNo(0)
    ENDIF ELSE BEGIN
      selParam=selConfig-1
      sv=DIALOG_MESSAGE('Automation template saved with parameter set which no longer exists ('+loadTemp.(thisMod).(selT).paramSet+')', DIALOG_PARENT=evTop)
    ENDELSE
    ;quickTemp - exists still?
    quickTempName=STRUPCASE(loadTemp.(thisMod).(selT).quickTemp)
    IF quickTempNames.HasValue(quickTempName) THEN BEGIN
      selNo=WHERE(quickTempNames EQ quickTempName)
      selQT=selNo(0)
    ENDIF ELSE BEGIN
      selQT=0
      sv=DIALOG_MESSAGE('Automation template saved with QuickTest template which no longer exists ('+loadTemp.(thisMod).(selT).quickTemp+')', DIALOG_PARENT=evTop)
    ENDELSE

    WIDGET_CONTROL, listSets_a, SET_DROPLIST_SELECT=selParam
    WIDGET_CONTROL, listQT_a, SET_DROPLIST_SELECT=selQT

    ;warning if same stationname on more than one template - run all will take first in list
    ;warning if same path in more than one template - import - all images will be put in first
    nTemps=N_TAGS(loadTemp.(thisMod))
    allStatNames=!Null
    allPathNames=!Null
    FOR a=0, nTemps-1 DO BEGIN
      IF loadTemp.(thisMod).(a).STATNAME NE '' THEN allStatNames=[allStatNames, loadTemp.(thisMod).(a).STATNAME]
      allPathNames=[allPathNames, loadTemp.(thisMod).(a).PATH]
    ENDFOR

    txt1='' & txt2=''
    IF N_ELEMENTS(allPathNames) GT 0 THEN BEGIN
      IF N_ELEMENTS(UNIQ(allPathNames)) NE nTemps THEN txt1='Found templates with the same image-path specified. This might cause unexpected behaviour.'
    ENDIF
    nStat=N_ELEMENTS(allStatNames)
    IF nStat GT 0 THEN BEGIN
      IF N_ELEMENTS(allStatNames(UNIQ(allStatNames))) NE nStat THEN txt2='Found templates with the same station-name specified. This might cause unexpected behaviour.'
    ENDIF
    IF txt1 NE '' OR txt2 NE '' THEN BEGIN
      WIDGET_CONTROL, warningTxt1, SET_VALUE=txt1
      WIDGET_CONTROL, warningTxt2, SET_VALUE=txt2
      WIDGET_CONTROL, auto_warningBox, MAP=1
    ENDIF ELSE WIDGET_CONTROL, auto_warningBox, MAP=0

  ENDIF ELSE BEGIN
    WIDGET_CONTROL, txtBrowse_a, SET_VALUE=''
    WIDGET_CONTROL, txtStatName_a, SET_VALUE=''
    WIDGET_CONTROL, btnInclSub, SET_BUTTON=0
    WIDGET_CONTROL, btnOnlyLastDate, SET_BUTTON=0
    WIDGET_CONTROL, listSort, SET_VALUE=''
    WIDGET_CONTROL, btnMoveFiles, SET_BUTTON=0
    WIDGET_CONTROL, txtBrowseApp, SET_VALUE=''
    WIDGET_CONTROL, listQT_a, SET_DROPLIST_SELECT=0
    sortElem=''
    ascElem=0
  ENDELSE

end

pro upd_QuickTestTemplates
  COMMON VARI
  COMPILE_OPT hidden
  WIDGET_CONTROL, /HOURGLASS
  RESTORE, thisPath+'data\config.dat'

  FOR m=0, N_TAGS(multiOpt)-1 DO BEGIN
    IF SIZE(quickTemp.(m),/TNAME) EQ 'STRUCT' THEN BEGIN
      idTest=WHERE(multiOpt.(m) NE 0)
      names=TAG_NAMES(quickTemp.(m))
      IF idTest(0) NE -1 THEN BEGIN
        newQuickTmod=!Null
        FOR i=0, N_TAGS(quickTemp.(m))-1 DO BEGIN
          oldArr=quickTemp.(m).(i)
          sz=SIZE(oldArr, /DIMENSIONS)

          ;remove zeros on last images
          rowSums=TOTAL(oldArr,1)
          notZero=WHERE(rowSums GT 0)
          newArr=oldArr[*,0:notZero(-1)]

          newQuickTmod=CREATE_STRUCT(newQuickTmod,names(i),newArr)
        ENDFOR
        quickTemp=replaceStructStruct(quickTemp, newQuickTmod, m)
      ENDIF
    ENDIF
  ENDFOR

  SAVE, configS, quickTemp, quickTout, loadTemp, renameTemp, FILENAME=thisPath+'data\config.dat'

end

pro updRDE, sel ;update rename dicom elements table
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden
  
  WIDGET_CONTROL, /HOURGLASS
  RESTORE, thisPath+'data\config.dat'
  
  tagList=renameTemp.tags
  formatList=renameTemp.tagFormats
  selT=WIDGET_INFO(lstTemp_rdt,/LIST_SELECT)
  
  formatThis=renameTemp.temp.(selT).formats
  nn=N_TAGS(tagList)
  
  tblContent=STRARR(5,nn)
  tblContent[0,*]=TRANSPOSE(TAG_NAMES(tagList))
  FOR i=0, nn-1 DO BEGIN
    uint_=UINT(tagList.(i))
    tblContent[1,i]=STRING(uint_(0), FORMAT='(z04)')
    tblContent[2,i]=STRING(uint_(1), FORMAT='(z04)')
    tblContent[3,i]=STRMID(formatList.(i),1,STRLEN(formatList.(i))-2)
    tblContent[4,i]=STRMID(formatThis.(i),1,STRLEN(formatThis.(i))-2)
  ENDFOR
  IF sel GT 12 THEN viewSel=sel-11 ELSE viewSel=0
  alignArr=INTARR(5,nn)
  alignArr[1:4,*]=1
  WIDGET_CONTROL, tbl_rde, TABLE_YSIZE=nn, SET_VALUE=tblContent, ALIGNMENT=alignArr, SET_TABLE_SELECT=[0,sel,0,sel], SET_TABLE_VIEW=[0,viewSel]
end

pro updRDT, sel ;update rename dicom template list and details for selected
  COMMON SETT
  COMMON VARI
  COMPILE_OPT hidden
  ;fill cat/file template based on list selection
  WIDGET_CONTROL, /HOURGLASS
  RESTORE, thisPath+'data\config.dat'
  
  rdt_names=TAG_NAMES(renameTemp.temp)
  WIDGET_CONTROL, lstTemp_rdt, SET_VALUE=rdt_names, SET_LIST_SELECT=sel
  
  ;update details for selected
  WIDGET_CONTROL, txtCat_rdt, SET_VALUE=STRUPCASE(STRJOIN(renameTemp.temp.(sel).cat,'  |  '))
  WIDGET_CONTROL, txtFile_rdt, SET_VALUE=STRUPCASE(STRJOIN(renameTemp.temp.(sel).file,'  |  '))
  
  selTag=WIDGET_INFO(tbl_rde,/TABLE_SELECT);left,top,right,btm
  selTag=selTag(1);top - first row selected
  updRDE, selTag;update file formats for this template
  
end

