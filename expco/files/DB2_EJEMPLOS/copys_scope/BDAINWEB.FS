      *BDAINWEB-FS.v03.001
      ******************************************************
      *ESTRUCTURA DE INFORME DE CLAVES WEB PARA SEGUIMIENTO
      ******************************************************
      $SET CONSTANT SW-STATUS-BDAINWEB"NO"
            SELECT CLAVES-WEB-SAL ASSIGN TO DISK NOMBRE-WEB-SAL
                   ORGANIZATION IS LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
      $IF SW-STATUS-BDAINWEB = "NO"
      $DISPLAY ".............COMPILANDO BDAINWEB SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDAINWEB CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-WS
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
