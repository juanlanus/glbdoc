      * DTCLOG-FS.v.0001
      * ARCHIVO DE LOG PARA SISTEMA DE MODIFICACIONES EN LINEA Y DE     
      * SISTEMA DE DATACIUDADANOS.        
      
           SELECT DTCLOG            
                  ASSIGN TO DISK TITLE-DTCLOG      
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-DTCLOG
                  ALTERNATE RECORD KEY IS LLAVE-REGISTRO-DTCLOG
                                          WITH DUPLICATES
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
