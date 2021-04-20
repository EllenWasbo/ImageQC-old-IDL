;ImageQC - quality control of medical images
;Copyright (C) 2020  Ellen Wasbo, Stavanger University Hospital, Norway
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


;=findTxtHeader(tagStr,valStr,'0008,0060', defVal='')
function findTxtHeader, tags,vals,tagen,defValue, IDno=idno
  id=WHERE(tags EQ tagen, nid)
  IF N_ELEMENTS(idno) EQ 0 THEN idno=0
  IF id(0) NE -1 THEN BEGIN
    IF idno GT nid-1 THEN BEGIN
      val=defValue
    ENDIF ELSE BEGIN
      ty=SIZE(defValue,/TNAME)
      CASE ty OF
        'STRING': val=vals(id(idno))
        'FLOAT':  val=FLOAT(vals(id(idno)))
        'INT':    val=LONG(vals(id(idno)))
      ENDCASE
    ENDELSE
  ENDIF ELSE val=defValue
  return, val
end

;***********************************************
;read DICOM file header (or txt file from ImageJ)
;adr = file path

;return: imgStruct (structure)
;  .filename
;  .studydatetime
;  .acqDate
;  .imgDate
;  .institution
;  .manufacturer
;  .modality
;  .modelName
;  .stationName
;  .SWversion
;  .patientName
;  .patientID
;  .patientWeight
;  .imageType (helical or axial)
;  .presType (presentation type FOR PRESENTATION, FOR PROCESSING...)
;  .studyDescr
;  .seriesName
;  .protocolName
;  .seriesNmb
;  .seriesTime
;  .seriesUID
;  .acqNmb
;  .acqtime
;  .sliceThick
;  .pix ;floatarray x,y
;  .imageSize
;  .kVp
;  .dFOV (display FOV)
;  .rekonFOV
;  .mA
;  .mAs
;  .time (exposure time)
;  .coll [singleCollW,totalCollW]
;  .pitch
;  .ExModType (exposure modulation type)
;  .CTDIvol
;  .focalSpotSz
;  .DAP
;  .EI
;  .sensitivity
;  .sdd
;  .kernel
;  .zpos
;  .imgNo
;  .nFrames
;  .wCenter
;  .wWidth
;  .collType
;  .nEWindows
;  .EWindowName
;  .zoomFactor
;  .radius1 (gamma camera radius 1st detektor)
;  .radius2 (gamma camera radius 2nd detektor)
;  .detectorVector (gamma camera - detector pr frame)
;  .acqFrameDuration
;  .acqTerminationCond
;  .units (of pixel values ex: HU, BQML ...)
;  .radiopharmaca
;  .admDose
;  .admDoseTime
;  .reconMethod
;  .attCorrMethod
;  .scaCorrMethod
;  .scatterFrac
;   .imgFreq (MR)
;.MRacqType
;.MRscanSeq
;.MRseqVariant
;.TR
;.TE
;.NSA
;.flipAng
;.spaceSlice
;.recCoilName
;.traCoilName

;dialog_par = Dialog parent window to let messages be located on top of window calling it
;silentValue = 0 if error meassages should be displayed, 1 if silent modus

function readImgInfo, adr, dialog_par, silentValue

  imgStruct=-1

  IF STRMID(adr,2,/REVERSE_OFFSET) NE 'txt' THEN BEGIN; assume dicom or dat

    IF FILE_BASENAME(adr) EQ 'DICOMDIR' THEN okDcm = 0 ELSE okDcm=QUERY_DICOM(adr) ; IDL crash if QUERY_DICOM on DICOMDIR - unknown reason

    IF okDcm THEN BEGIN
      o=obj_new('idlffdicom')
      t=o->read(adr)

      ;check if directoryfile
      test=o->GetReference('0004'x,'1220'x)
      IF test(0) EQ -1 THEN BEGIN

        ;modality
        test=o->GetReference('0008'x,'0060'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        modality=*(test_peker[0])
        ;IF modality NE 'SR' THEN BEGIN;avoid SR files as these are not images

        ;multiframe?
        test=o->GetReference('0028'x,'0008'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN nFrames=LONG(*(test_peker[0])) ELSE nFrames=0
        IF nFrames EQ 1 THEN nFrames=0

        ;****************parameters where and when
        test=o->GetReference('0008'x,'0020'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN studyDate=*(test_peker[0]) ELSE studyDate=''
        test=o->GetReference('0008'x,'0030'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN studyTime=*(test_peker[0]) ELSE studyTime=''

        test=o->GetReference('0008'x,'0031'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN seriesTime=*(test_peker[0]) ELSE seriesTime=''

        test=o->GetReference('0020'x,'000E'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN seriesUID=*(test_peker[0]) ELSE seriesUID=''

        test=o->GetReference('0008'x,'0022'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN acqDate=*(test_peker[0]) ELSE acqDate=''

        ; acquisition time
        test=o->GetReference('0008'x,'0032'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN acqTime=*(test_peker[0]) ELSE acqTime=''

        test=o->GetReference('0008'x,'0023'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN imgDate=*(test_peker[0]) ELSE imgDate=''

        test=o->GetReference('0008'x,'0080'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN institution=*(test_peker[0]) ELSE institution=''

        test=o->GetReference('0008'x,'0070'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN manufacturer=*(test_peker[0]) ELSE manufacturer=''

        test=o->GetReference('0008'x,'1090'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN modelName=*(test_peker[0]) ELSE modelName=''

        test=o->GetReference('0008'x,'1010'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN stationName=*(test_peker[0]) ELSE stationName=''

        test=o->GetReference('0018'x,'1020'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN SWversion=*(test_peker[0]) ELSE SWversion=''

        test=o->GetReference('0018'x,'700A'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN detectorID=*(test_peker[0]) ELSE detectorID=''

        test=o->GetReference('0010'x,'0010'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        patientName=*(test_peker[0])

        test=o->GetReference('0010'x,'0020'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        patientID=*(test_peker[0])

        ;***************parameters what
        test=o->GetReference('0008'x,'0008'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN imageType=*(test_peker[0]) ELSE imageType=''

        ;presentation Type
        test=o->GetReference('0008'x,'0068'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN BEGIN
          presType=*(test_peker[0])
        ENDIF ELSE presType=''

        ;study description
        test=o->GetReference('0008'x,'1030'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN studyDescr=*(test_peker[0]) ELSE studyDescr=''

        ;series description
        test=o->GetReference('0008'x,'103E'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN seriesName=*(test_peker[0]) ELSE seriesName=''

        ; protocolName 0018 1030
        test=o->GetReference('0018'x,'1030'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN BEGIN
          protocolName=*(test_peker[0])
        ENDIF ELSE protocolName=''

        test=o->GetReference('0020'x,'0011'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        seriesNmb=LONG(*(test_peker[0]))

        test=o->GetReference('0020'x,'0012'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN acqNmb=LONG(*(test_peker[0])) ELSE acqNmb=-1

        ;***************parameters how
        test=o->GetReference('0018'x,'0050'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN sliceThick=FLOAT(*(test_peker[0])) ELSE sliceThick=-1.

        test=o->GetReference('0028'x,'0030'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN BEGIN
          pixStr=*(test_peker[0])
          pix=FLOAT(STRSPLIT(pixStr,'\',/EXTRACT))
        ENDIF ELSE pix=[-1.,-1.]
        IF pix(0) EQ -1. THEN BEGIN
          test=o->GetReference('0018'x,'1164'x)
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            pixStr=*(test_peker[0])
            pix=FLOAT(STRSPLIT(pixStr,'\',/EXTRACT))
          ENDIF
        ENDIF

        imageSize=[-1,-1]
        test=o->GetReference('0028'x,'0011'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN imageSize(0)=*(test_peker[0])
        test=o->GetReference('0028'x,'0010'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN imageSize(1)=*(test_peker[0])

        test=o->GetReference('0018'x,'0060'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN kVp=*(test_peker[0]) ELSE kVp=-1.
        IF N_ELEMENTS(kVp) GT 1 THEN kVp=FLOAT(fix(kVp, type=7))
        IF SIZE(kVp, /TNAME) NE 'FLOAT' THEN kVp=FLOAT(kVp)

        test=o->GetReference('0018'x,'0090'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN dFOV=*(test_peker[0]) ELSE dFOV=-1.
        IF N_ELEMENTS(dFOV) GT 1 THEN dFOV=FLOAT(fix(dFOV, type=7))

        test=o->GetReference('0018'x,'1100'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN rekonFOV=*(test_peker[0]) ELSE rekonFOV=-1.
        IF N_ELEMENTS(rekonFOV) GT 1 THEN rekonFOV=FLOAT(fix(rekonFOV, type=7))

        mA=-1.
        test=o->GetReference('0018'x,'8151'x); mikroA
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN BEGIN
          mA=*(test_peker[0])
          IF N_ELEMENTS(mA) GT 1 THEN mA=fix(mA,type=7)
          mA=0.001*FLOAT(mA)
        ENDIF ELSE BEGIN
          test=o->GetReference('0018'x,'1151'x); milliA
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN mA=*(test_peker[0]) ELSE BEGIN
            test=o->GetReference('0018'x,'9330'x)
            test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
            IF test(0) NE -1 THEN mA=*(test_peker[0])
          ENDELSE
        ENDELSE

        test=o->GetReference('0018'x,'1153'x); mikroAs
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN mAs=*(test_peker[0]) ELSE mAs=-1.
        IF N_ELEMENTS(mAs) GT 1 THEN mAs=float(fix(mAs,type=7))
        IF mAs LE 0 THEN BEGIN
          test=o->GetReference('0018'x,'1152'x); milliAs
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN mAs=*(test_peker[0]) ELSE mAs=-1.
          IF N_ELEMENTS(mAs) GT 1 THEN mAs=float(fix(mAs,type=7))
        ENDIF ELSE mAs=0.001*mAs

        IF modality EQ 'PT' THEN time=-1. ELSE BEGIN
          test=o->GetReference('0018'x,'1150'x)
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            time=*(test_peker[0])
          ENDIF ELSE time=-1
        ENDELSE
        IF mAs LE 0 THEN BEGIN
          IF mA GT 0 AND time GT 0 THEN mAs=FLOAT(mA)*FLOAT(time)*.001
        ENDIF

        ;IF modality EQ 'DX' OR modality EQ 'CR' THEN BEGIN
        ;  mAs=FLOAT(mA)*FLOAT(time)/1000.
        ;ENDIF

        ;AEC
        IF modality EQ 'CT' THEN BEGIN
          test=o->GetReference('0018'x,'9323'x)
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            ExModType=*(test_peker[0])
            IF N_ELEMENTS(ExModType) GT 1 THEN ExModType=fix(ExModType, type=7)
          ENDIF ELSE ExModType='-'
        ENDIF ELSE ExModType='-'
        IF modality EQ 'DX' OR modality EQ 'CR' THEN BEGIN
          test=o->GetReference('0018'x,'7060'x);MAN/AUT
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            ExModType=*(test_peker[0])
            IF N_ELEMENTS(ExModType) GT 1 THEN ExModType=fix(ExModType, type=7)
          ENDIF
          test=o->GetReference('0018'x,'7062'x);AEC descr
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            ExModType2=*(test_peker[0])
            IF N_ELEMENTS(ExModType2) GT 1 THEN ExModType2=fix(ExModType2, type=7)
            IF ExModType NE '' THEN ExModType=ExModType+' | '+ExModType2
          ENDIF
        ENDIF

        test=o->GetReference('0018'x,'9306'x)
        IF test(0) NE -1 THEN BEGIN
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          singleCollW=*(test_peker[0])
        ENDIF ELSE singleCollW=-1.
        IF N_ELEMENTS(singleCollW) EQ 8 THEN singleCollW=fix(singleCollW, 0,1, type=5); 8 byte (64 bit) double precision floating number

        test=o->GetReference('0018'x,'9307'x)
        IF test(0) NE -1 THEN BEGIN
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          totalCollW=*(test_peker[0])
        ENDIF ELSE totalCollW=-1.
        IF N_ELEMENTS(totalCollW) EQ 8 THEN totalCollW=fix(totalCollW, 0,1, type=5); 8 byte (64 bit) double precision floating number

        coll=[singleCollW,totalCollW]

        test=o->GetReference('0018'x,'9311'x)
        IF test(0) NE -1 THEN BEGIN
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          pitch=*(test_peker[0])
        ENDIF ELSE pitch=-1.
        IF N_ELEMENTS(pitch) EQ 8 THEN pitch=fix(pitch, 0,1, type=5); 8 byte (64 bit) double precision floating number

        test=o->GetReference('0018'x,'1142'x)
        IF test(0) NE -1 THEN BEGIN
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          radposString=*(test_peker[0])
          radPos1=FLOAT(STRSPLIT(radposString,'\',/EXTRACT))
          IF N_ELEMENTS(test) EQ 2 THEN BEGIN
            test_peker=o->GetValue(REFERENCE=test[1],/NO_COPY)
            radposString=*(test_peker[0])
            radPos2=FLOAT(STRSPLIT(radposString,'\',/EXTRACT))
          ENDIF ELSE radPos2=-1.
        ENDIF ELSE BEGIN
          radPos1=-1. &  radPos2=-1.
        ENDELSE
              
        test=o->GetReference('0054'x,'0020'x)
        IF test(0) NE -1 THEN BEGIN
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          detString=*(test_peker[0])
          detectorVector=STRSPLIT(detString,'\',/EXTRACT)
        ENDIF ELSE detectorVector='-'

        test=o->GetReference('0018'x,'9345'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN BEGIN
          CTDIvol=*(test_peker[0])
          IF N_ELEMENTS(CTDIvol) EQ 8 THEN BEGIN
            CTDIvol=FIX(CTDIvol, 0, 1, type=5)
            CTDIvol=CTDIvol(0)
          ENDIF
        ENDIF ELSE CTDIvol=-1.

        test=o->GetReference('0018'x,'1190'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN focalSpotSz=*(test_peker[0]) ELSE focalSpotSz='-'
        IF SIZE(focalSpotSz, /TNAME) NE 'STRING' THEN focalSpotSz=STRTRIM(STRING(focalSpotSz))

        test=o->GetReference('0018'x,'115E'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN DAP=*(test_peker[0]) ELSE BEGIN
          test=o->GetReference('0018'x,'9473'x);Acquired Image Area Dose Product
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN DAP=*(test_peker[0]) ELSE DAP=-1.
        ENDELSE
        IF N_ELEMENTS(DAP) EQ 4 THEN BEGIN
          DAP=FIX(DAP, 0, 1, type=4)
          DAP=DAP(0)
        ENDIF

        test=o->GetReference('0018'x,'1411'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN EI=*(test_peker[0]) ELSE EI=-1.
        IF N_ELEMENTS(EI) NE 1 THEN EI=fix(EI, type=7)

        test=o->GetReference('0018'x,'6000'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN sensitivity=*(test_peker[0]) ELSE sensitivity=-1.

        test=o->GetReference('0018'x,'1110'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN sdd=*(test_peker[0]) ELSE sdd=-1.

        ;filterAddOn
        test=o->GetReference('0018'x,'1160'x); NONE/MULTIPLE....
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN filterAddOn=*(test_peker[0]) ELSE filterAddOn='-'
        IF filterAddOn NE 'NONE' AND filterAddOn NE '-' THEN BEGIN
          test=o->GetReference('0018'x,'7050'x)
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            filterAddOn2=*(test_peker[0])
            IF N_ELEMENTS(filterAddOn2) GT 1 THEN filterAddOn2=fix(filterAddOn2, type=7)
            filterAddOn=filterAddOn+' | '+filterAddOn2
          ENDIF
          test=o->GetReference('0018'x,'7052'x)
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            filterAddOn2=*(test_peker[0])
            IF N_ELEMENTS(filterAddOn2) GT 1 THEN filterAddOn2=fix(filterAddOn2, type=7)
            filterAddOn=filterAddOn+' | '+filterAddOn2
          ENDIF
          test=o->GetReference('0018'x,'7054'x)
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            filterAddOn2=*(test_peker[0])
            IF N_ELEMENTS(filterAddOn2) GT 1 THEN filterAddOn2=fix(filterAddOn2, type=7)
            filterAddOn=filterAddOn+' | '+filterAddOn2
          ENDIF
        ENDIF

        ;kernel
        test=o->GetReference('0018'x,'1210'x);more than one possible...
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN kernel=*(test_peker[0]) ELSE kernel='-'
        IF STRMID(manufacturer,0,2) EQ 'GE' THEN BEGIN
          test=o->GetReference('0053'x,'1042'x);IR or AI grade
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          IF test(0) NE -1 THEN BEGIN
            IF N_ELEMENTS(*(test_peker[0])) EQ 1 THEN kernel2=', '+*(test_peker[0]) ELSE kernel2=''; bytarr 10 for older GE - something else than IR/AI info
          ENDIF ELSE kernel2=''
          kernel=kernel+kernel2
        ENDIF

        IF nFrames EQ 0 THEN BEGIN
          test=o->GetReference('0020'x,'1041'x)
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          stest=size(test_peker, /TNAME)
          IF stest EQ 'POINTER' THEN BEGIN
            zpos=FLOAT(*(test_peker[0]))
          ENDIF ELSE zpos=-999.
        ENDIF ELSE BEGIN
          test=o->GetReference('0020'x,'0032'x)
          IF N_ELEMENTS(test) EQ nFrames AND test(0) NE -1 THEN BEGIN
            zPos=FLTARR(nFrames)-999.
            FOR i=0, nFrames-1 DO BEGIN
              test_peker=o->GetValue(REFERENCE=test[i],/NO_COPY)
              imgpos=*(test_peker[0])
              t=STRSPLIT(imgpos,'\',/EXTRACT)
              IF N_ELEMENTS(t) GT 2 THEN zPos(i)=FLOAT(t(2)) ELSE zPos(i)=-999.
            ENDFOR
          ENDIF ELSE zPos=FLTARR(nFrames)-999.
        ENDELSE

        test=o->GetReference('0020'x,'0013'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN imgNo=LONG(*(test_peker[0])) ELSE imgNo=-1

        test=o->GetReference('0028'x,'1050'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN wCenter=*(test_peker[0]) ELSE wCenter=-1

        test=o->GetReference('0028'x,'1051'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN wWidth=*(test_peker[0]) ELSE wWidth=-1

        ; Nuclear Medicine
        test=o->GetReference('0018'x,'1181'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN collType=*(test_peker[0]) ELSE colltype='-'

        test=o->GetReference('0054'x,'0011'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN nEWindows=*(test_peker[0]) ELSE nEWindows=-1

        test=o->GetReference('0054'x,'1001'x)
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN units=*(test_peker[0]) ELSE units='-'

        test=o->GetReference('0054'x,'0018'x)

        IF test(0) NE -1 THEN BEGIN
          EWindowName=''
          FOR n=0, nEWindows-1 DO BEGIN
            test_peker=o->GetValue(REFERENCE=test[n],/NO_COPY)
            EWindowName=EWindowName + *(test_peker[0]) +', '
          ENDFOR
          IF STRLEN(EWindowName) GT 2 THEN EWindowName=STRMID(EWindowName, 0, STRLEN(EWindowName)-2)
        ENDIF ELSE EWindowName='-'

        test=o->GetReference('0028'x,'0031'x); 1\1
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN zoomFactor=*(test_peker[0]) ELSE zoomFactor='-'

        test=o->GetReference('0010'x,'1030'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN patientWeight=*(test_peker[0]) ELSE patientWeight='-'

        ;acqFrameDuration (msec)
        test=o->GetReference('0018'x,'1242'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN acqFrameDuration=DOUBLE(*(test_peker[0])) ELSE acqFrameDuration=-1.

        test=o->GetReference('0018'x,'0017'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN acqTerminationCond=*(test_peker[0]) ELSE acqTerminationCond='-'

        ; PET only
        test=o->GetReference('0018'x,'0031'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN radiopharmaca=*(test_peker[0]) ELSE radiopharmaca='-'

        test=o->GetReference('0018'x,'1074'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY);siemens PET, two instances and first emtpy...
        IF test(0) NE -1 THEN BEGIN
          admDose=*(test_peker[0])
          IF admDose EQ '' THEN BEGIN
            IF N_ELEMENTS(test) EQ 2 THEN BEGIN
              test_peker=o->GetValue(REFERENCE=test[1],/NO_COPY);siemens PET, two instances and first emtpy...
              admDose=*(test_peker[0])
            ENDIF ELSE admDose='-'
          ENDIF
        ENDIF ELSE admDose='-'

        test=o->GetReference('0018'x,'1072'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN admDoseTime=*(test_peker[0]) ELSE admDoseTime='-'

        test=o->GetReference('0054'x,'1103'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN reconMethod=*(test_peker[0]) ELSE reconMethod='-'

        test=o->GetReference('0054'x,'1101'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN attCorrMethod=*(test_peker[0]) ELSE attCorrMethod='-'

        test=o->GetReference('0054'x,'1105'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN scaCorrMethod=*(test_peker[0]) ELSE scaCorrMethod='-'

        test=o->GetReference('0054'x,'1323'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN scatterFrac=*(test_peker[0]) ELSE scatterFrac='-'

        test=o->GetReference('0054'x,'0090'x);
        IF nFrames EQ 0 THEN angle = -999. ELSE angle=FLTARR(nFrames)-999.
        IF test(0) NE -1 THEN BEGIN
          test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
          angleVec=*(test_peker[0])
          angles=STRSPLIT(angleVec, '\', /EXTRACT)
          FOR ff=0, N_ELEMENTS(angles)-1 DO BEGIN
            angle(ff)=FLOAT(angles(ff))
          ENDFOR
        ENDIF ELSE angle=-999.

        ;MR
        test=o->GetReference('0018'x,'0084'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN imgFreq=*(test_peker[0]) ELSE imgFreq=-1.

        test=o->GetReference('0018'x,'0023'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN MRacqType=*(test_peker[0]) ELSE MRacqType='-'

        test=o->GetReference('0018'x,'0020'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN MRscanSeq=*(test_peker[0]) ELSE MRscanSeq='-'

        test=o->GetReference('0018'x,'0021'x);
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN MRseqVariant=*(test_peker[0]) ELSE MRseqVariant='-'

        test=o->GetReference('0018'x,'0080'x);repetition time
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN TR=*(test_peker[0]) ELSE TR=-1.

        test=o->GetReference('0018'x,'0081'x);echo time
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN TE=*(test_peker[0]) ELSE TE=-1.

        test=o->GetReference('0018'x,'0083'x);number of averages
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN NSA=*(test_peker[0]) ELSE NSA=-1.

        test=o->GetReference('0018'x,'1314'x);flip angle
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN flipAng=*(test_peker[0]) ELSE flipAng=-1.

        test=o->GetReference('0018'x,'0088'x);space between slices
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN spaceSlice=*(test_peker[0]) ELSE spaceSlice=-1.
        
        test=o->GetReference('0018'x,'1250'x);receive coil name
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN recCoilName=*(test_peker[0]) ELSE recCoilName='-'

        test=o->GetReference('0018'x,'1251'x);transmit coil name
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        IF test(0) NE -1 THEN traCoilName=*(test_peker[0]) ELSE traCoilName='-'

        ;ENDIF;directoryfile

        frameNo=-1

        imgStruct=CREATE_STRUCT('filename',adr,'studydatetime', studyDate+studyTime, 'acqDate', acqDate, 'imgDate', imgDate, 'institution',institution,'manufacturer',manufacturer,'modality', modality, 'modelName',modelName,'stationName',stationName,'SWversion',SWversion,'detectorID',detectorID,$
          'patientName',patientName, 'patientID', patientID, 'patientWeight', patientWeight, 'imageType',imageType,'presType',presType,'studyDescr',studyDescr,'seriesName',seriesName, 'protocolname', protocolname,$
          'seriesNmb',seriesNmb,'seriesTime', seriesTime,'seriesUID',seriesUID,'acqNmb',acqNmb, 'acqtime',acqtime,'sliceThick',sliceThick, 'pix', pix,'imageSize',imageSize,'kVp',kVp,'FOV',dFOV,'rekonFOV',rekonFOV,'mA',mA,'mAs',mAs,'ExpTime',time,'coll',coll,'pitch',pitch,$
          'ExModType',ExModType,'CTDIvol',CTDIvol,'focalSpotSz',focalSpotSz,'DAP',DAP,'EI',EI,'sensitivity',sensitivity,'sdd',sdd,'filterAddOn',filterAddOn,'kernel',kernel,$
          'zpos', zpos, 'imgNo',imgNo,'nFrames',nFrames,'wCenter',wCenter,'wWidth',wWidth,$
          'collType',collType,'nEWindows',nEWindows,'EWindowName',EWindowName,'zoomFactor',zoomFactor,'radius1',radPos1,'radius2',radPos2,'detectorVector',detectorVector,'angle',angle,'acqFrameDuration',acqFrameDuration,'acqTerminationCond',acqTerminationCond,$
          'units',units,'radiopharmaca',radiopharmaca,'admDose',admDose,'admDoseTime',admDoseTime,'reconMethod',reconMethod,'attCorrMethod',attCorrMethod,'scaCorrMethod',scaCorrMethod, 'scatterFrac',scatterFrac,$
          'imgFreq',imgFreq,'MRacqType',MRacqType,'MRscanSeq',MRscanSeq,'MRseqVariant',MRseqVariant,'TR',TR,'TE',TE,'NSA',NSA,'flipAng',flipAng,'spaceSlice',spaceSlice,'recCoilName',recCoilName,'traCoilName',traCoilName,$
          'frameNo', frameNo)

        IF imgStruct.nFrames GT 1 THEN BEGIN; split structure into separate image-structures
          tagsToSplit= ['ZPOS','RADIUS1','RADIUS2','ANGLE','DETECTORVECTOR']
          firstStruct=structArr2elem(imgStruct,tagsToSplit, 0)
          firstStruct.frameNo=1
          imgStructMulti=CREATE_STRUCT('M0',firstStruct)
          FOR ism=1,  imgStruct.nFrames -1 DO BEGIN
            imgStructMulti=CREATE_STRUCT(imgStructMulti,'M'+STRING(ism,FORMAT='(i0)'),structArr2elem(imgStruct, tagsToSplit, ism))
            imgStructMulti.(ism).frameNo=ism+1
          ENDFOR
          imgStruct=imgStructMulti
        ENDIF

        IF OBJ_VALID(o) THEN OBJ_DESTROY, o

        stest=size(test_peker, /TNAME)
        IF stest EQ 'POINTER' THEN PTR_FREE, test_peker
        ;ENDIF ELSE imgStruct='SR'; SR report
      ENDIF ELSE imgStruct='DIR';directoryfile
    ENDIF ELSE BEGIN; okDcm=0 - dat
      CATCH, err_stat
      IF err_stat NE 0 THEN BEGIN
        CATCH, /CANCEL
        sv=DIALOG_MESSAGE('Not valid dicom or .dat/.sav file: '+adr, /INFORMATION, DIALOG_PARENT=dialog_par)
        RETURN, -1
      ENDIF
      RESTORE, adr
      IF SIZE(imageQCmatrix, /TNAME) EQ 'STRUCT' THEN BEGIN
        imgStruct=imgStructUpdate(imageQCmatrix, adr)
      ENDIF
      imageQCmatrix=!null
    ENDELSE

  ENDIF ELSE BEGIN; txt

    adrHeader=DIALOG_PICKFILE(TITLE='Locate DICOM header as .txt file for selected image as txt.', /READ, FILTER='*.txt', /FIX_FILTER, PATH=FILE_DIRNAME(adr), DIALOG_PARENT=evTop)
    IF adrHeader(0) NE '' THEN BEGIN
      OPENR, filenhet, adrHeader, /GET_LUN
      elem=''
      tagStr=!Null
      valStr=!Null
      WHILE ~ EOF(filenhet) DO BEGIN
        READF, filenhet, elem
        tagStr=[tagStr,STRMID(elem,0,9)]
        pos=STRPOS(elem,':')
        valStr=[valStr,STRMID(elem,pos+2)]
      ENDWHILE
      CLOSE, filenhet
      FREE_LUN, filenhet
      intercept=0
      slope=1.
      ori=0

      IF N_ELEMENTS(tagStr) GT 0 THEN BEGIN
        modality=findTxtHeader(tagStr,valStr,'0008,0060', '')
        nFrames=findTxtHeader(tagStr,valStr,'0028,0008', 0)
        studyDate=findTxtHeader(tagStr,valStr,'0008,0020','')
        studyTime=findTxtHeader(tagStr,valStr,'0008,0030','')
        seriesUID=findTxtHeader(tagStr,valStr,'0020,000E','')
        acqDate=findTxtHeader(tagStr,valStr,'0008,0022','')
        acqTime=findTxtHeader(tagStr,valStr,'0008,0032','')
        seriesTime=findTxtHeader(tagStr,valStr,'0008,0031','')
        imgDate=findTxtHeader(tagStr,valStr,'0008,0023','')
        institution=findTxtHeader(tagStr,valStr,'0008,0080','')
        manufacturer=findTxtHeader(tagStr,valStr,'0008,0070','')
        modelName=findTxtHeader(tagStr,valStr,'0008,1090','')
        stationName=findTxtHeader(tagStr,valStr,'0008,1010','')
        SWversion=findTxtHeader(tagStr,valStr,'0018,1020','')
        detectorID=findTxtHeader(tagStr,valStr,'0018,700A','')
        patientName=findTxtHeader(tagStr,valStr,'0010,0010','')
        patientID=findTxtHeader(tagStr,valStr,'0010,0020','')
        imageType=findTxtHeader(tagStr,valStr,'0008,0008','')
        presType=findTxtHeader(tagStr,valStr,'0008,0068','')
        studyDescr=findTxtHeader(tagStr,valStr,'0008,1030','')
        seriesName=findTxtHeader(tagStr,valStr,'0008,103E','')
        protocolName=findTxtHeader(tagStr,valStr,'0018,1030','')
        seriesNmb=findTxtHeader(tagStr,valStr,'0020,0011',-1)
        acqNmb=findTxtHeader(tagStr,valStr,'0020,0012',-1)
        sliceThick=findTxtHeader(tagStr,valStr,'0018,0050',-1.)
        pixStr=findTxtHeader(tagStr,valStr,'0028,0030','')
        IF pixStr NE '' THEN pix=FLOAT(STRSPLIT(pixStr,'\',/EXTRACT)) ELSE pix=[-1.,-1.]
        IF pix(0) EQ -1. THEN BEGIN
          pixStr=findTxtHeader(tagStr,valStr,'0018,1164','')
          IF pixStr NE '' THEN pix=FLOAT(STRSPLIT(pixStr,'\',/EXTRACT))
        ENDIF
        imageSize=[-1,-1]
        imageSize(0)=findTxtHeader(tagStr,valStr,'0028,0011',-1)
        imageSize(1)=findTxtHeader(tagStr,valStr,'0028,0010',-1)
        kVp=findTxtHeader(tagStr,valStr,'0018,0060',-1.)
        dFOV=findTxtHeader(tagStr,valStr,'0018,0090',-1.)
        rekonFOV=findTxtHeader(tagStr,valStr,'0018,1100',-1.)
        mA=findTxtHeader(tagStr,valStr,'0018,8151',-1.);mikroA
        IF mA LE 0 THEN mA=0.001*mA ELSE BEGIN
          mA=findTxtHeader(tagStr,valStr,'0018,1151',-1.);milliA
          IF mA EQ -1. THEN mA=findTxtHeader(tagStr,valStr,'0018,9330',-1.)
        ENDElse
        mAs=findTxtHeader(tagStr,valStr,'0018,1153',-1.);mikroAs
        IF mAs NE -1. THEN mAs=0.001*mAs ELSE BEGIN
          mAs=findTxtHeader(tagStr,valStr,'0018,1152',-1.);milliAs
        ENDelse
        IF modality EQ 'PT' THEN time=-1. ELSE time=findTxtHeader(tagStr,valStr,'0018,1150',-1)
        IF mAs LE 0 THEN BEGIN
          IF mA GT 0 AND time GT 0 THEN mAs=mA*time*.001
        ENDIF
        ExModType='-'
        IF modality EQ 'CT' THEN BEGIN
          ExModType=findTxtHeader(tagStr,valStr,'0018,9323','-')
          singleCollW=findTxtHeader(tagStr,valStr,'0018,9306',-1.)
          totalCollW=findTxtHeader(tagStr,valStr,'0018,9307',-1.)
          coll=[singleCollW,totalCollW]
        ENDIF ELSE coll=[-1.,-1.]
        pitch=findTxtHeader(tagStr,valStr,'0018,9311',-1.)
        IF modality EQ 'DX' OR modality EQ 'CR' THEN BEGIN
          ExModType=findTxtHeader(tagStr,valStr,'0018,7060','');MAN/AUT
          ExModType2=findTxtHeader(tagStr,valStr,'0018,7062','');AEC descr
          IF ExModType2 NE '' THEN ExModType=ExModType+' | '+ExModType2
        ENDIF
        radPosString=findTxtHeader(tagStr,valStr,'0018,1142','')
        IF radPosString NE '' THEN radPos1=FLOAT(STRSPLIT(radposString,'\',/EXTRACT)) ELSE radPos1=-1.
        IF radPos1 NE -1. THEN BEGIN
          radPosString=findTxtHeader(tagStr,valStr,'0018,1142','', IDno=1)
          radPos2=FLOAT(STRSPLIT(radposString,'\',/EXTRACT))
        ENDIF ELSE radPos2=-1.
        detNmbString=findTxtHeader(tagStr,valStr,'0054,0020','')
        IF detNmbString NE '' THEN detectorVector=STRSPLIT(detNmbStrin,'\',/EXTRACT) ELSE detectorVector='-'
        CTDIvol=findTxtHeader(tagStr,valStr,'0018,9345',-1.)
        focalSpotSz=findTxtHeader(tagStr,valStr,'0018,1190','-')
        DAP=findTxtHeader(tagStr,valStr,'0018,115E',-1.)
        IF DAP EQ -1. THEN DAP=findTxtHeader(tagStr,valStr,'0018,9473',-1.)
        EI=findTxtHeader(tagStr,valStr,'0018,1411',-1.)
        sensitivity=findTxtHeader(tagStr,valStr,'0018,6000',-1.)
        sdd=findTxtHeader(tagStr,valStr,'0018,1110',-1.)
        filterAddOn=findTxtHeader(tagStr,valStr,'0018,1160','-');NONE/MULTIPLE....
        IF filterAddOn NE 'NONE' AND filterAddOn NE '-' THEN BEGIN
          filterAddOn2=findTxtHeader(tagStr,valStr,'0018,7050','')
          IF filterAddOn2 NE '' THEN filterAddOn=filterAddOn+' | '+filterAddOn2
          filterAddOn2=findTxtHeader(tagStr,valStr,'0018,7052','')
          IF filterAddOn2 NE '' THEN filterAddOn=filterAddOn+' | '+filterAddOn2
          filterAddOn2=findTxtHeader(tagStr,valStr,'0018,7054','')
          IF filterAddOn2 NE '' THEN filterAddOn=filterAddOn+' | '+filterAddOn2
        ENDIF
        kernel=findTxtHeader(tagStr,valStr,'0018,1210','-')
        IF nFrames EQ 0 THEN zpos=findTxtHeader(tagStr,valStr,'0020,1041',-999.) ELSE zPos=FLTARR(nFrames)-999.;TODO - fix this is of value... see dicom version above
        imgNo=findTxtHeader(tagStr,valStr,'0020,0013',-1)
        wCenter=findTxtHeader(tagStr,valStr,'0028,1050',-1)
        wWidth=findTxtHeader(tagStr,valStr,'0028,1051',-1)
        colltype=findTxtHeader(tagStr,valStr,'0018,1181','-')
        nEWindows=findTxtHeader(tagStr,valStr,'0054,0011',-1)
        units=findTxtHeader(tagStr,valStr,'0054,1001','-')
        EWindowName='-';TODO - fix this is of value... see dicom version above
        zoomFactor=findTxtHeader(tagStr,valStr,'0028,0031','-')
        patientWeight=findTxtHeader(tagStr,valStr,'0010,1030','-')
        acqFrameDuration=findTxtHeader(tagStr,valStr,'0018,1242',-1.);TODO - fix this to DOUBLE value... see dicom version above
        acqTerminationCond=findTxtHeader(tagStr,valStr,'0018,0017','-')
        radiopharmaca=findTxtHeader(tagStr,valStr,'0018,0031','-')
        admDose='-';TODO - fix this is of value... see dicom version above
        admDoseTime=findTxtHeader(tagStr,valStr,'0018,1072','-')
        reconMethod=findTxtHeader(tagStr,valStr,'0054,1103','-')
        attCorrMethod=findTxtHeader(tagStr,valStr,'0054,1101','-')
        scaCorrMethod=findTxtHeader(tagStr,valStr,'0054,1105','-')
        scatterFrac=findTxtHeader(tagStr,valStr,'0054,1323','-')
        IF nFrames EQ 0 THEN angle = -999. ELSE angle=FLTARR(nFrames)-999.;TODO - fix this is of value... see dicom version above
        imgFreq=findTxtHeader(tagStr,valStr,'0018,0084',-1.)
        MRacqType=findTxtHeader(tagStr,valStr,'0018,0023','-')
        MRscanSeq=findTxtHeader(tagStr,valStr,'0018,0020','-')
        MRseqVariant=findTxtHeader(tagStr,valStr,'0018,0021','-')
        TR=findTxtHeader(tagStr,valStr,'0018,0080',-1.)
        TE=findTxtHeader(tagStr,valStr,'0018,0081',-1.)
        NSA=findTxtHeader(tagStr,valStr,'0018,0083',-1.)
        flipAng=findTxtHeader(tagStr,valStr,'0018,1314',-1.)
        spaceSlice=findTxtHeader(tagStr,valStr,'0018,0088',-1.)
        recCoilName=findTxtHeader(tagStr,valStr,'0018,1250','-')
        traCoilName=findTxtHeader(tagStr,valStr,'0018,1251','-')
        
        ;*************************
        intercept=findTxtHeader(tagStr,valStr,'0028,0052',0)
        slope=findTxtHeader(tagStr,valStr,'0028,1053',1.)
        ori=0
        strOri=STRTRIM(STRING(findTxtHeader(tagStr,valStr,'0018,1053',''),2))
        IF strOri EQ 'FFS' THEN ori=1
        ;TODO: multiframe?
        IF nFrames GT 1 THEN sv=DIALOG_MESSAGE('DICOM from text not yet implented for multiframe DICOM. Strange behaviour or crashes might occur. Contact the supplier of the software to make a wish.', DIALOG_PARENT=evTop)

      ENDIF
    ENDIF
    frameNo=-1

    IF N_ELEMENTS(patientID) EQ 0 THEN BEGIN

      imgStruct=CREATE_STRUCT('filename',adr,'studydatetime', '', 'acqDate', '', 'imgDate', '', 'institution','','manufacturer','','modality', '', 'modelName','','stationName','','SWversion','','detectorID','',$
        'patientName','', 'patientID', '', 'patientWeight', '-', 'imageType','','presType','','studyDescr','','seriesName','', 'protocolname', '',$
        'seriesNmb',-1,'seriesTime', '','seriesUID','','acqNmb',-1, 'acqtime','','sliceThick',-1., 'pix', [-1.,-1.],'imageSize',[-1,-1],'kVp',-1.,'FOV',-1.,'rekonFOV',-1.,'mA',-1.,'mAs',-1.,'ExpTime',-1.,'coll',[-1.,-1.],'pitch',-1.,$
        'ExModType','-','CTDIvol',-1.,'focalSpotSz','-','DAP',-1.,'EI',-1.,'sensitivity',-1.,'sdd',-1.,'filterAddOn','','kernel','-',$
        'zpos', -999., 'imgNo',-1,'nFrames',0,'wCenter',-1,'wWidth',-1,$
        'collType','-','nEWindows',-1,'EWindowName','-','zoomFactor','-','radius1',-1.,'radius2',-1.,'detectorVector',detectorVector,'angle',-999.,'acqFrameDuration',-1.,'acqTerminationCond','-',$
        'units','-','radiopharmaca','-','admDose','-','admDoseTime','-','reconMethod','-','attCorrMethod','-','scaCorrMethod','-', 'scatterFrac','-',$
        'imgFreq',-1.,'MRacqType','-','MRscanSeq','-','MRseqVariant','-','TR',-1.,'TE',-1.,'NSA',-1.,'flipAng',-1.,'spaceSlice',-1.,'recCoilName','-','traCoilName','-',$
        'frameNo', frameNo)

    ENDIF ELSE BEGIN

      imgStruct=CREATE_STRUCT('filename',adr,'studydatetime', studyDate+studyTime, 'acqDate', acqDate, 'imgDate', imgDate, 'institution',institution,'manufacturer',manufacturer,'modality', modality, 'modelName',modelName,'stationName',stationName,'SWversion',SWversion,'detectorID',detectorID,$
        'patientName',patientName, 'patientID', patientID, 'patientWeight', patientWeight, 'imageType',imageType,'presType',presType,'studyDescr',studyDescr,'seriesName',seriesName, 'protocolname', protocolname,$
        'seriesNmb',seriesNmb,'seriesTime', seriesTime,'seriesUID',seriesUID,'acqNmb',acqNmb, 'acqtime',acqtime,'sliceThick',sliceThick, 'pix', pix,'imageSize',imageSize,'kVp',kVp,'FOV',dFOV,'rekonFOV',rekonFOV,'mA',mA,'mAs',mAs,'ExpTime',time,'coll',coll,'pitch',pitch,$
        'ExModType',ExModType,'CTDIvol',CTDIvol,'focalSpotSz',focalSpotSz,'DAP',DAP,'EI',EI,'sensitivity',sensitivity,'sdd',sdd,'filterAddOn',filterAddOn,'kernel',kernel,$
        'zpos', zpos, 'imgNo',imgNo,'nFrames',nFrames,'wCenter',wCenter,'wWidth',wWidth,$
        'collType',collType,'nEWindows',nEWindows,'EWindowName',EWindowName,'zoomFactor',zoomFactor,'radius1',radPos1,'radius2',radPos2,'detectorVector',detectorVector,'angle',angle,'acqFrameDuration',acqFrameDuration,'acqTerminationCond',acqTerminationCond,$
        'units',units,'radiopharmaca',radiopharmaca,'admDose',admDose,'admDoseTime',admDoseTime,'reconMethod',reconMethod,'attCorrMethod',attCorrMethod,'scaCorrMethod',scaCorrMethod, 'scatterFrac',scatterFrac,$
        'imgFreq',imgFreq,'MRacqType',MRacqType,'MRscanSeq',MRscanSeq,'MRseqVariant',MRseqVariant,'TR',TR,'TE',TE,'NSA',NSA,'flipAng',flipAng,'spaceSlice',spaceSlice,'recCoilName',recCoilName,'traCoilName',traCoilName,$
        'frameNo', frameNo)

      ;********save as .dat*********
      adrDat=DIALOG_PICKFILE(PATH=FILE_DIRNAME(adr)+FILE_BASENAME(adr)+'.dat', TITLE='Save file as IDL structure (.dat-file) to continue',/WRITE, FILTER='*.dat', /FIX_FILTER, DIALOG_PARENT=evTop)
      
      IF adrDat NE '' THEN BEGIN
        fi=FILE_INFO(FILE_DIRNAME(adrDat))
        IF fi.write THEN BEGIN
          IF STRMID(adrDat,3,/REVERSE_OFFSET) NE '.dat' THEN adrDat=adrDat+'.dat'
          OPENR, filenhet, adr, /GET_LUN
          elem=''
          rows=!Null
          IF imageSize(0) NE -1 THEN BEGIN
            rows=FLTARR(imageSize(0),imageSize(1))
            FOR r=0, imageSize(1)-1 DO BEGIN
              READF, filenhet, elem
              rows[*,r]=STRSPLIT(elem,/EXTRACT)
            ENDFOR
          ENDIF ELSE BEGIN
            WHILE ~ EOF(filenhet) DO BEGIN
              READF, filenhet, elem
              rows=[[rows],[STRSPLIT(elem,/EXTRACT)]]
            ENDWHILE
          ENDELSE
          CLOSE, filenhet
          FREE_LUN, filenhet
  
          rows=REVERSE(rows,2)*slope + intercept
          IF ori EQ 1 THEN rows=REVERSE(rows)
  
          imageQCmatrix=CREATE_STRUCT(imgStruct,'matrix',rows)
          imageQCmatrix.filename=adrDat; changed when opened so that renaming/moving file is possible
          imgStruct.filename=adrDat
          IF imageQCmatrix.imageSize(0) EQ -1 THEN imageQCmatrix.imageSize=SIZE(rows,/DIMENSIONS)
          SAVE, imageQCmatrix, FILENAME=adrDat
        ENDIF ELSE sv=DIALOG_MESSAGE('Failed to save file. Missing writing permission for selected folder.',/ERROR, DIALOG_PARENT=evTop)
      ENDIF

    ENDELSE
  ENDELSE

  return, imgStruct
end

