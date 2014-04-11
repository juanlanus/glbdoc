REMARK* BDIEENTI-FS.v.0002
      *******************************************
      *  MAESTRO DE ENTIDADES SUPERINTENDENCIA
      *******************************************

           SELECT BDIEENTI ASSIGN TO DISK SI-BDIEENTI
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS BDIEENTI-LLAVE
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.

