******* BDACUE.v.01.001
REMARK*****************************************************************
REMARK* ARCHIVO DE TIPOS DE CUENTA DE DATACREDITO
REMARK*****************************************************************
009710$SET CONSTANT SW-STATUS-BDACUE"NO"
           SELECT BDACUE    ASSIGN TO DISK "$DATABASE/BDACUE"
                 ORGANIZATION IS LINE SEQUENTIAL
                 ACCESS MODE IS SEQUENTIAL
      $IF SW-STATUS-BDACUE = "NO"
      $DISPLAY ".............COMPILANDO BDACUE SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDACUE CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDACUE
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
