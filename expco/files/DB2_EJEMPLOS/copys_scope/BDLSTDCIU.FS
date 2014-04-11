      * BDLSTDCIU-FS.v.0001
REMARK******************************************************************
REMARK* DEFINICION DE ARCHIVO DE ACEPCIONES DE CIUDADES.               *
REMARK******************************************************************
           SELECT BDLSTDCIU
                  ASSIGN TO DISK TITLE-BDLSTDCIU
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDLSTDCIU
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
