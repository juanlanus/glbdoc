REMARK*FRAINQGR.v01.001.
      *----------------------------------------------------------------*
      *ESTRUCTURA ARCHIVO DE PREGUNTAS GRUPALES EVIDENTE FRAINQGR.DAT
      *----------------------------------------------------------------*
      $SET CONSTANT SW-STATUS-FRAINQGR"NO"
           SELECT FRAINQGR ASSIGN TO DISK TITLE-FRAINQGR
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
      $IF SW-STATUS-FRAINQ = "NO"
      $DISPLAY ".............COMPILANDO FRAINQGR SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO FRAINQGR CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-FRAINQ
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
