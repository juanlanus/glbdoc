REMARK*BDALOCLA.v03.002.
******************************************************************
REMARK*ESTRUCTURA DE ARCHIVO DE LOG DE CLAVES CREADAS EN ICBCLA
*************************************************************************
      * FAC0002     * AMPLOG * CAMBIO DE NOMBRE EXTERNO A LA ESTRUCTURA 
      * F.CUADROS C.* DE LOG DEL ARCHIVO ICBCLA. AMPLIACIÓN DE LA MISMA.
      * JUL.27/2006 * 
      *******************************************************************
      $SET CONSTANT SW-STATUS-BDALOCLA"NO"
AMPLOG      SELECT CLAVE-LOG ASSIGN TO DISK "$DATOS/ICBCLA.LOG"         FAC0002 
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS  DYNAMIC
                  RECORD KEY IS LLAVE-LOG
      $IF SW-STATUS-BDALOCLA= "NO"
      $DISPLAY ".............COMPILANDO BDALOCLA SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDALOCLA CON STATUS.........."
                  FILE STATUS IS COD-RETORNO
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
