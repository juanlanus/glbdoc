REMARK* BDIEMSUP-FS.v.0001
      *******************************************
      *  MAESTRO DE INFORMACION SUPERINTENDENCIA
      *******************************************

      $set keycompress "6"
           SELECT BDIEMSUP ASSIGN TO DISK SI-BDIEMSUP
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS BDIEMSUP-LLAVE
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
      $set nokeycompress 

