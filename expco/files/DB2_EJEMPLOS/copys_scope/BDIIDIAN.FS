      * BDIIDIAN.v.03.0001
      *==================================================================
REMARK* MAESTRO DE PERSONAS JURIDICAS NACIONALES Y EXTRANJERAS REPORTADAS    
REMARK* POR LA DIAN
      *==================================================================
      $SET CONSTANT SW-STATUS-BDIIDIAN"NO"
           SELECT BDIIDIAN ASSIGN TO DISK TITLE-BDIIDIAN
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-BDIIDIAN  
      $IF SW-STATUS-BDIIDIAN = "NO"
      $DISPLAY ".............COMPILANDO BDIIDIAN SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDIIDIAN CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDIIDIAN
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
