REMARK* DTCCFG-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE LA CONFIGURACION PARA QUE EL PROGRAMA     *
REMARK* COBOL SE CONECTE Y EJECUTE OPERACIONES EN LA BD                *
REMARK******************************************************************
           SELECT DTCCFG
                  ASSIGN       TO DISK TITLE-DTCCFG
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
