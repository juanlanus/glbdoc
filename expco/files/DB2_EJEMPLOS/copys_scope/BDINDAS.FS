REMARK* BDINDAS-FS.v.0001
REMARK******************************************************************
REMARK* DESCRIPCION ARCHIVO DE NOMBRES DEL DAS                         *
REMARK******************************************************************
       SELECT BDINDAS ASSIGN TO DISK TITLE-BDINDAS 
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS LLAVE-BDINDAS
              LOCK MODE    IS MANUAL WITH LOCK ON RECORD.                              
