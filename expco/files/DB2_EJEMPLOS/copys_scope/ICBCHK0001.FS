      *ICBCHK0001.FS.v.01.0001
      ******************************************************************
      * EST027.v.02.002                                                *
      * MAESTRO QUE ALMACENA LAS CUENTAS CORRIENTES Y CUENTAS DE AHORRO*
      * REPORTADAS POR LOS DIFERENTES SUSCRIPTORES                     *
      ******************************************************************
       $SET IDXFORMAT"8"
      
           SELECT  MAESTRO-CHECK  ASSIGN TO  MAES-CHECK-TITLE
                   ORGANIZATION          IS  INDEXED
                   ACCESS         MODE   IS  DYNAMIC
                   RECORD         KEY    IS  LLAVE-MAES-CHECK
                   LOCK           MODE   IS  MANUAL WITH LOCK ON RECORD.

       $SET IDXFORMAT"0"
