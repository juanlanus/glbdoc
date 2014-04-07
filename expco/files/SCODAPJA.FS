      *SCODAPJA.FS.v.01.0001
      *******************************************************************
      * ARCHIVO CON LOS SCORES POR IDENTIFICACION DE PJ A              
      *******************************************************************
009710$SET CONSTANT SW-STATUS-SCODAPJA"NO"
      $SET IDXFORMAT"8"
           SELECT SCODAPJA
                  ASSIGN TO DISK TITLE-SCODAPJA   
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-SCODAPJA
      $IF SW-STATUS-SCODAPJA = "NO"
      $DISPLAY ".............COMPILANDO SCODAPJA SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO SCODAPJA CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-SCODAPJA
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
      $SET IDXFORMAT"0"
