REMARK* FRAPREA-FS.v.0001
REMARK* ARCHIVO QUE REGISTRA CADA UNA DE LAS PREGUNTAS REALIZADAS POR EVIDENTE
REMARK* POR CADA IDENTIFICACION DEL MES ANTERIOR 
           SELECT FRAPREA
                  ASSIGN TO DISK TITLE-FRAPREA
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-FRAPREA
      $SET KEYCOMPRESS "5"                                                     
                  ALTERNATE KEY IS
                  LLAVE-ALTERNA-FRAPREA = FORMULARIO-FRAPREA
                                           NUMPRE-FRAPREA
                                 WITH DUPLICATES
      $SET NOKEYCOMPRESS                                                       
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
