REMARK* DTCING-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE LOS INTENTOS FALLIDOS DE INGRESO AL       *
REMARK* APLICATIVO                                                     *
REMARK******************************************************************
           SELECT DTCING
                  ASSIGN  TO DISK TITLE-DTCING
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-DTCING
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
