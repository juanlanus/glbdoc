REMARK*BDALOSFX.v03.001.
******************************************************************
REMARK*ESTRUCTURA DE ARCHIVO DE LOG DE CLAVES CREADAS EN ICBCLA
REMARK*SOLAMENTE PARA LOS CAMPOS UTILIZADOS EN LA IMPRESIÓN DEL SOBREFLEX
******************************************************************
      $SET CONSTANT SW-STATUS-BDALOSFX"NO"
            SELECT BDALOSFX ASSIGN TO DISK "$DATOS/CLAVE.LOG"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS  DYNAMIC
                  RECORD KEY IS LLAVE-BDALOSFX
      $IF SW-STATUS-BDALOSFX= "NO"
      $DISPLAY ".............COMPILANDO BDALOSFX SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDALOSFX CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDALOSFX
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
