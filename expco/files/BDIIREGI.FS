      * BDIIREGI.v.03.0001
      *===============================================================
      * ESTRUCTURA DEL ARCHIVO ENTREGADO POR LA REGISTRADURIA NACIONAL
      *===============================================================
       $SET IDXFORMAT"8"
      
      $SET CONSTANT SW-STATUS-BDIIREGI"NO"
           SELECT BDIIREGI 
                  ASSIGN TO DISK TITLE-BDIIREGI
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDIIREGI
      $IF SW-STATUS-BDIIREGI = "NO"
      $DISPLAY ".............COMPILANDO BDIIREGI SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDIIREGI CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDIIREGI
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.

      $SET IDXFORMAT"0"
