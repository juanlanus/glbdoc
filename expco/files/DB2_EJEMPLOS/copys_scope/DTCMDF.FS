REMARK* DTCMDF-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE EL CONSECUTIVO DE MODIFICACIONES HECHAS   *
REMARK* POR EL SUSCRIPTOR                                              *
REMARK******************************************************************
           SELECT DTCMDF
                  ASSIGN  TO DISK TITLE-DTCMDF
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-DTCMDF
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
