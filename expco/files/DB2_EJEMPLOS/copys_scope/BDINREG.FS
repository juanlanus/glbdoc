REMARK* BDINREG-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO DE NOMBRES DE REGISTRADURIA                            *
REMARK******************************************************************
       SELECT BDINREG ASSIGN TO DISK TITLE-BDINREG
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS LLAVE-BDINREG
              LOCK MODE    IS MANUAL WITH LOCK ON RECORD.                              
