REMARK* BDIMHIT-FS.v.0001
REMARK***************************************************************
REMARK* ARCHIVO MAESTRO HISTORICO TEMPORAL PARA MICROCREDITO
REMARK***************************************************************
      $SET CONSTANT SW-STATUS-BDIMHIT"NO"
           SELECT BDIMHIT 
                  ASSIGN TO DISK TITLE-BDIMHIT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-BDIMHIT
      $IF SW-STATUS-BDIMHIT = "NO"
      $DISPLAY ".............COMPILANDO BDIMHIT SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDIMHIT CON STATUS.........."
                  FILE STATUS IS FILE-STATUS-BDIMHIT
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
