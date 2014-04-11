      * FACUCLI-FS.v.0001.
      **Maestro general de clientes de datacredito
      *----------------------------------------------------------------*
           SELECT FACUCLI
                  ASSIGN TO DISK MAESTRO-FACUCLI
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  FILE STATUS IS COD-RETORNO1
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS LLAVE-FACUCLI.
