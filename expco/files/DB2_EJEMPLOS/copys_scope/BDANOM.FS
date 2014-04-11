      * BDANOM.FS.v.01.0001
      *===============================================================
      * ESTRUCTURA DEL ARCHIVO DE NOMBRES EXTRACTADO DE LA REGIS      
      *===============================================================
      *
      *****************************************************************
      $SET CONSTANT SW-STATUS-BDANOM"NO"
           SELECT BDANOM
                  ASSIGN TO DISK TITLE-BDANOM
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDANOM  
      $IF SW-STATUS-BDANOM = "NO"
      $DISPLAY ".............COMPILANDO BDANOM   SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDANOM   CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDANOM
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
