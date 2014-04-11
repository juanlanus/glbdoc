      * ICBDIM.v.01.0001.0001
      *===============================================================
      * ESTRUCTURA DEL ARCHIVO DE CARGA CONSUMIDORES Y DATACAS        
      *===============================================================
      *
      *****************************************************************
      $SET IDXFORMAT"8"
      $SET CONSTANT SW-STATUS-ICBDIM"NO"
           SELECT ICBDIM
                  ASSIGN TO DISK TITLE-ICBDIM
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICBDIM WITH DUPLICATES
      $IF SW-STATUS-ICBDIM = "NO"
      $DISPLAY ".............COMPILANDO ICBDIM   SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO ICBDIM   CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-ICBDIM
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.

      $SET IDXFORMAT"0"
