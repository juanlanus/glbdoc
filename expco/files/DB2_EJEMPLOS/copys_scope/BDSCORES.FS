      *BDSCORES.FS.v.01.0001
      *******************************************************************
      * ARCHIVO CON LOS SCORES POR IDENTIFICACION DE C/MES EN UN AÑO
      *******************************************************************
009710$SET CONSTANT SW-STATUS-BDSCORES"NO"
           SELECT BDSCORES
                  ASSIGN TO DISK TITLE-BDSCORES   
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDSCORES
      $IF SW-STATUS-BDSCORES = "NO"
      $DISPLAY ".............COMPILANDO BDSCORES SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDSCORES CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDSCORES
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
