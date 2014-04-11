REMARK* DTCTRA-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE LOS INSTRUCCIONES SQL QUE NO SE PUDIERON  *
REMARK* APLICAR A LA BASE DE DATOS DESDE COBOL                         *
REMARK******************************************************************
           SELECT DTCTRA
                  ASSIGN  TO DISK TITLE-DTCTRA
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-DTCTRA
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
