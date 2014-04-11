REMARK*BDALOWEB.v03.001.
******************************************************************
REMARK*ESTRUCTURA DE ARCHIVO DE LOG DE CLAVES WEB
******************************************************************
      $SET CONSTANT SW-STATUS-BDALOWEB"NO"
                   SELECT CLAVE-WEBLOG ASSIGN TO DISK NOMBRE-RAS-LOG
                   ORGANIZATION IS LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
      $IF SW-STATUS-BDALOWEB = "NO"
      $DISPLAY ".............COMPILANDO BDALOWEB SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDALOWEB CON STATUS.........."
                   FILE STATUS IS COD-RETORNO-W
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
