REMARK* BDIMHIS-FS.v.0001
REMARK***************************************************************
REMARK* ARCHIVO MAESTRO HISTORICO PARA MICROCREDITO
REMARK***************************************************************
      $SET CONSTANT SW-STATUS-BDIMHIS"NO"
           SELECT BDIMHIS 
                  ASSIGN TO DISK TITLE-BDIMHIS
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-BDIMHIS
      $IF SW-STATUS-BDIMHIS = "NO"
      $DISPLAY ".............COMPILANDO BDIMHIS SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDIMHIS CON STATUS.........."
                  FILE STATUS IS FILE-STATUS-BDIMHIS
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.

