      **USUSUI-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE LAS CLAVES Y LOS SERVICIOS DE LA BASE DE  *
REMARK* DATOS DE SUI PARA REEMPLAZAR ESTA BASE DE DATOS                *
REMARK******************************************************************
           SELECT USUSUI
                  ASSIGN TO DISK "$DATABASE/USUSUI"   
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-USUSUI 
010300            LOCK MODE IS MANUAL WITH LOCK ON RECORD.

