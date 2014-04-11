      **BDACLWEB-FS.v03.001
      ********************************************************
      * ESTRUCTURA DE ARCHIVO DE CLAVES WEB 
      ********************************************************
      $SET CONSTANT SW-STATUS-BDACLWEB"NO"
            SELECT CLAVES-WEB ASSIGN TO DISK NOMBRE-CLAVE-WEB
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS LLAVE-CLAVE-WEB
      $IF SW-STATUS-BDACLWEB = "NO"
      $DISPLAY ".............COMPILANDO BDACLWEB SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDACLWEB CON STATUS.........."
                   FILE STATUS IS COD-RETORNO-WEB
      $END
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD.
