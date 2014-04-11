      *ICMCHKH.FS.v.01.0001
      ******************************************************************
      * MAESTRO QUE ALMACENA LAS CUENTAS CORRIENTES Y CUENTAS DE AHORRO*
      * REPORTADAS POR LOS DIFERENTES SUSCRIPTORES PASADAS A HISTORICO *
      ******************************************************************
       $SET IDXFORMAT"8"
           SELECT  MAESTRO-ICMCHKH  ASSIGN TO  MAES-ICMCHKH-TITLE
                   ORGANIZATION          IS  INDEXED
                   ACCESS         MODE   IS  DYNAMIC
                   RECORD         KEY    IS  LLAVE-MAES-ICMCHKH
                   LOCK           MODE   IS  MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
