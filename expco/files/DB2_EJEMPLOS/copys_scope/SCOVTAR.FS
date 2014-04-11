REMARK* SCOVTAR.FS.v.0001
REMARK******************************************************************
REMARK* PROYECTO-VISA. 
REMARK* ARCHIVO DE TARJETAS SELECCIONADAS DE ACUERDO AL PUNTAJE DE SCORE
REMARK******************************************************************
           SELECT SCOVTAR
                  ASSIGN  TO DISK TITLE-SCOVTAR
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
