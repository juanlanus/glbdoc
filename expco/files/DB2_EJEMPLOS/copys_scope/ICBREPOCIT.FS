      *ICBREPOCIT.FS.v.01.0001
      ******************************************************************
      * REPOSITORIO DE TRANSACCIONES CITIBANK EN FORMATO DATAHOST PARA *
      * SER CONSULTADAS POSTERIORMENTE.                                *
      ******************************************************************
       $SET IDXFORMAT"8"
      
           SELECT  REPO-CITI  ASSIGN   TO "$DATABASE/CITIBURO"
                   ORGANIZATION        IS  INDEXED
                   ACCESS  MODE        IS  DYNAMIC
                   RECORD  KEY         IS  REPCIT-LLAVE
                   LOCK  MODE          IS  MANUAL WITH LOCK ON RECORD.

       $SET IDXFORMAT"0"
