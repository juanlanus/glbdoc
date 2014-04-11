REMARK*FRAINQ.v01.001.
      *----------------------------------------------------------------*
      *ESTRUCTURA DE ARCHIVO DE PREGUNTAS EVIDENTE FRAINQ.DAT
      *----------------------------------------------------------------*
      $SET CONSTANT SW-STATUS-FRAINQ"NO"
           SELECT FRAINQ ASSIGN TO DISK TITLE-FRAINQ
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
      $IF SW-STATUS-FRAINQ = "NO"
      $DISPLAY ".............COMPILANDO FRAINQ SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO FRAINQ CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-FRAINQ
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
