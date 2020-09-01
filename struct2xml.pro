

function getDataStr, vals
  dataStr=!Null
  sz0=SIZE(vals,/TNAME)
  CASE sz0 OF
    'INT': fc='(i0)'
    'LONG': fc='(i0)'
    'FLOAT': fc='(f0)'
    'DOUBLE': fc='(f0)'
    'BYTE': fc='(f0)'
    ELSE: fc=''
  ENDCASE

  IF sz0 EQ 'STRUCT' THEN dataStr=['<data>','STRUCT','</data>'] ELSE BEGIN
    IF fc NE '' THEN stringArr=STRING(vals,FORMAT=fc) ELSE stringArr=vals
    dataStr=['<data>',STRJOIN(stringArr,'|'),'</data>']
  ENDELSE

  RETURN, dataStr
end

function struct2xml, s

  document=''
  IF SIZE(s, /TNAME) EQ 'STRUCT' THEN BEGIN

      document=['<?xml version="1.0"?>','<tags>']

      tagNames0=TAG_NAMES(s)
      FOR l0=0, N_TAGS(s)-1 DO BEGIN
        document=[document,'<tag0>','<name>'+tagNames0(l0)+'</name>']
        IF SIZE(s.(l0), /TNAME) EQ 'STRUCT' THEN BEGIN
          tagNames1=TAG_NAMES(s.(l0))
          FOR l1=0, N_TAGS(s.(l0))-1 DO BEGIN
            document=[document,'<tag1>','<name>'+tagNames1(l1)+'</name>']
            IF SIZE(s.(l0).(l1), /TNAME) EQ 'STRUCT' THEN BEGIN
              tagNames2=TAG_NAMES(s.(l0).(l1))
              FOR l2=0, N_TAGS(s.(l0).(l1))-1 DO BEGIN
                document=[document,'<tag2>','<name>'+tagNames2(l2)+'</name>']
                IF SIZE(s.(l0).(l1).(l2), /TNAME) EQ 'STRUCT' THEN BEGIN
                  tagNames3=TAG_NAMES(s.(l0).(l1).(l2))
                  FOR l3=0, N_TAGS(s.(l0).(l1).(l2))-1 DO BEGIN
                    document=[document,'<tag3>','<name>'+tagNames3(l3)+'</name>']
                    IF SIZE(s.(l0).(l1).(l2).(l3), /TNAME) EQ 'STRUCT' THEN BEGIN
                      tagNames4=TAG_NAMES(s.(l0).(l1).(l2).(l3))
                      FOR l4=0, N_TAGS(s.(l0).(l1).(l2).(l3))-1 DO BEGIN
                        document=[document,'<tag4>','<name>'+tagNames4(l4)+'</name>',getDataStr(s.(l0).(l1).(l2).(l3).(l4)),'</tag4>']
                      ENDFOR
                    ENDIF ELSE document=[document,getDataStr(s.(l0).(l1).(l2).(l3))]
                    document=[document,'</tag3>']
                  ENDFOR
                ENDIF ELSE document=[document,getDataStr(s.(l0).(l1).(l2))]
                document=[document,'</tag2>']
              ENDFOR
            ENDIF ELSE document=[document,getDataStr(s.(l0).(l1))]
            document=[document,'</tag1>']
          ENDFOR
        ENDIF ELSE document=[document,getDataStr(s.(l0))]
        document=[document,'</tag0>']
      ENDFOR
      document=[document,'</tags>']

  ENDIF ELSE sv=DIALOG_MESSAGE('Input variable is not a structure. No xml file generated.')
RETURN, document

end