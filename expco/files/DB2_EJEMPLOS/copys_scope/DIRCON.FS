REMARK*DIRCON-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO DE HUELLAS DE CONSULTA DE DIRECCIONES                  *
REMARK******************************************************************
           SELECT DIRCON
                  ASSIGN TO DISK TITLE-DIRCON         
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-DIRCON 
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.                     
