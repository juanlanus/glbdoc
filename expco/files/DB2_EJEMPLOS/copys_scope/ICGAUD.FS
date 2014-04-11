      *ICGAUD.FS.v.01.0001
      ******************************************************************
      * ICGAUD.FS : DEFINICION DE ARCHIVO DE LAS TRANSACCIONES DE AUDI-*
      *   TORIA DE LOS SCORES A LA MEDIDA, PARA CONTROL DE LA EJECUCION*
      ******************************************************************
      *
           SELECT AUDITORIA      ASSIGN  TO  ARCHIVO-AUDITORIA
                  ORGANIZATION   IS  INDEXED
                  ACCESS MODE    IS  DYNAMIC
                  RECORD KEY     IS  AUDI-LLAVE
                  LOCK  MODE     IS  MANUAL  WITH  LOCK  ON  RECORD
                  FILE STATUS    IS  W-STATUS-AUD.
      *
