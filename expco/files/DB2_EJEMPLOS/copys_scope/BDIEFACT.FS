REMARK* BDIEFACT-FS.v.0001
      ******************************************************************
      *  MAESTRO DE FECHA DE ACTUALIZACION DE ENTIDADES SUPERINTENDENCIA
      ******************************************************************

           SELECT BDIEFACT ASSIGN TO DISK SI-BDIEFACT
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS BDIEFACT-LLAVE
                  LOCK MODE IS AUTOMATIC.

