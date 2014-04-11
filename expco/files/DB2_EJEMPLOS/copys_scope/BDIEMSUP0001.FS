REMARK* BDIEMSUP-FS.v.0001
      **********************************************************
      *MAESTRO DE INFORMACION SUPERINTENDENCIA FORMATO TIPO 8  *
      **********************************************************
       $SET IDXFORMAT"8"
           SELECT BDIEMSUP ASSIGN TO DISK TITLE-BDIEMSUP
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS BDIEMSUP-LLAVE
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"

