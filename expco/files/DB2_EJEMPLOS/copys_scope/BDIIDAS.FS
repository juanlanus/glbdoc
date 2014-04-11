      * BDIIDAS.v.03.0001
      *=====================================================================
      * ARCHIVO MAESTRO DE CEDULAS DE EXTRANJERIA, PROPORCIONADAS POR EL DAS
      *=====================================================================
      $SET CONSTANT SW-STATUS-BDIIDAS"NO"
           SELECT BDIIDAS ASSIGN TO DISK TITLE-BDIIDAS
                 ORGANIZATION IS INDEXED
                 ACCESS MODE IS DYNAMIC
                 RECORD KEY IS LLAVE-BDIIDAS
      $IF SW-STATUS-BDIIDAS = "NO"
      $DISPLAY ".............COMPILANDO BDIIDAS SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDIIDAS CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDIIDAS
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
