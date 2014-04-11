REMARK* DTCFAL-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE LAS ULTIMAS TRAMAS RECIBIDAS POR EL       *
REMARK* SERVIDOR ICGMDF ANTES DE BAJAR YA SEA NORMAL O ANORMALMENTE    *
REMARK******************************************************************
           SELECT DTCFAL
                  ASSIGN  TO DISK TITLE-DTCFAL
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
