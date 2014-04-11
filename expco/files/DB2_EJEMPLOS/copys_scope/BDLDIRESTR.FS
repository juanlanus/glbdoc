      * DBLDIRESTR.v.01.0000
      *================================================================*
      * ESTRUCTURA ARCHIVO INDEXADO DESTINADO PARA EL MANEJO DE ESTRATO*
      * SOCIOECONOMICO                                                 *
      *================================================================*
       $SET IDXFORMAT"8"
       $SET CONSTANT SW-STATUS-BDLDIRESTR"NO"
           SELECT BDLDIRESTR
                  ASSIGN TO DISK "$DIRECCIONES/BDLDIRESTR"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDLDIRESTR
       $IF SW-STATUS-BDLDIRESTR = "NO"
       $DISPLAY "...........COMPILANDO BDLDIRESTR SIN STATUS..........."
       $ELSE
       $DISPLAY "...........COMPILANDO BDLDIRESTR CON STATUS..........."
                  FILE STATUS IS COD-RETORNO-BDLDIRESTR
       $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
