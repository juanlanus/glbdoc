******* ICBSUS-FS.v.0003
009710$SET CONSTANT SW-STATUS-ICBSUS"NO"
009800     SELECT SUSCRIPTOR
009900            ASSIGN TO DISK SUSCRIPTOR-TITLE
010000            ORGANIZATION IS INDEXED
010100            ACCESS MODE IS DYNAMIC
010200            RECORD KEY IS LLAVE-SUSCRIP
      $IF SW-STATUS-ICBSUS = "NO"                                               
      $DISPLAY ".............COMPILANDO ICBSUS SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO ICBSUS CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-ICBSUS
      $END
010300            LOCK MODE IS MANUAL WITH LOCK ON RECORD.

