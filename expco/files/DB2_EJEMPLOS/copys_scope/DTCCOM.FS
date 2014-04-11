      * DTCCOM-FS.v.0001
      * ARCHIVO DE COMENTARIOS DEL SISTEMA DE DATACIUDADANOS            
      
           SELECT DTCCOM            
                  ASSIGN TO DISK TITLE-DTCCOM      
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-DTCCOM
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
