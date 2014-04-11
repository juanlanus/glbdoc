REMARK* BDINDIAN.v.0001
REMARK******************************************************************
REMARK* DESCRIPCION ARCHIVO DE NOMBRES DE DIAN                         *
REMARK******************************************************************
       SELECT BDINDIAN ASSIGN TO DISK TITLE-BDINDIAN
              ORGANIZATION    IS INDEXED
              ACCESS MODE     IS DYNAMIC
              RECORD KEY      IS LLAVE-BDINDIAN
              LOCK MODE       IS MANUAL WITH LOCK ON RECORD.                              
