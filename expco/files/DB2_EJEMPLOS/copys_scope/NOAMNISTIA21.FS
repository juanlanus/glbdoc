      * NOAMNISTIA21-FS.V.0001
      * ARCHIVO DE IDENTIFICACIONES A LAS CUALES NO APLICA LA     
      * AMNISTIA POR QUE TODOS SUS REGISTROS SE ENCUENTRAN AL DIA
           SELECT NOAMNISTIA21
                  ASSIGN TO DISK "$DATABASE/NOAMNISTIA21"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-NOAMNISTIA21
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
