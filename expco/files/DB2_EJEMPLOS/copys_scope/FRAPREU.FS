REMARK* FRAPREU-FS.v.0001
REMARK* ARCHIVO QUE REGISTRA CADA UNA DE LAS PREGUNTAS REALIZADAS POR EVIDENTE
REMARK* POR CADA IDENTIFICACION
           SELECT FRAPREU
                  ASSIGN TO DISK TITLE-FRAPREU
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-FRAPREU
      $SET KEYCOMPRESS "5"                                                     
                  ALTERNATE KEY IS
                  LLAVE-ALTERNA-FRAPREU = FORMULARIO-FRAPREU
                                           NUMPRE-FRAPREU
                                 WITH DUPLICATES
      $SET NOKEYCOMPRESS                                                       
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
