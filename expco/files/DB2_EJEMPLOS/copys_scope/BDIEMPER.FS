REMARK* BDIEMPER-FS.v.0001
      *******************************************
      *  MAESTRO INICIAL DE PERSONAS SUPERINTENDENCIA
      *******************************************

           SELECT BDIEMPER ASSIGN TO DISK SI-BDIEMPER
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS BDIEMPER-LLAVE
                  LOCK MODE IS AUTOMATIC.

