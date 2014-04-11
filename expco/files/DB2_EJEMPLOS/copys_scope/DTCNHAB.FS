      * DTCNHAB-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO CON FECHAS NO HABILES, SE INCLUYEN SABADOS Y DOMINGOS  * 
REMARK******************************************************************
           SELECT DTCNHAB    
                  ASSIGN TO DISK  TITLE-DTCNHAB    
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-DTCNHAB    
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
