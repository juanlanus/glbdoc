REMARK* OFICINA-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE OFICINAS Y CIUDADES ASOCIADAS A CADA      *
REMARK* CUENTA                                                         *
REMARK******************************************************************
           SELECT OFICINA
                  ASSIGN  TO DISK TITLE-OFICINA
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-OFICINA
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.

