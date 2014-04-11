      **ICBCLA-FS.v.0006
      ***************************************************************
      * MAESTRO DE CLAVES DE ACCESO AL SISTEMA DE DATACREDITO
      * SEPTIEMBRE 19 DEL 2001 SE AGREGA LLAVE ALTERNA PARA 
      * PERMITIR EL MANEJO DE LAS CLAVES DE INTERNET
      * DICIEMBRE 2001 SE MODIFICA ESTRUCTURA DE LA LLAVE ALTERNA 
      ***************************************************************
      * FAC0006     * LLAALT * LLAVE ALTERNA NUEVA POR CODIGO DE CLAVE
      * F.CUADROS C.* PARA MANEJO SISTEMA NOVEDAT Y EN LINEA DATACREDITO
      * JUL.30/2003 *
      ***************************************************************
009710$SET CONSTANT SW-STATUS-ICBCLA"NO"
002900     SELECT CLAVE
003000            ASSIGN TO DISK CLAVE-TITLE
003100            ORGANIZATION IS INDEXED
003200            ACCESS MODE IS DYNAMIC
003300            RECORD KEY IS LLAVE-CLAVE

                  ALTERNATE KEY IS
                   LLAVE-ALTERNA-ABD09 = NUM-ID-ABD09
                   COD-CLAVE-ABD09 CLAVE-AUT-ABD09
                   WITH DUPLICATES

LLAALT            ALTERNATE KEY IS                                      FAC0006
LLAALT             LLAVE-ALTERNA2-ABD09 =                               FAC0006
LLAALT             COD-CLAVE-ABD09 CLAVE-AUT-ABD09                      FAC0006
LLAALT             WITH DUPLICATES                                      FAC0006

      $IF SW-STATUS-ICBCLA = "NO"
      $DISPLAY ".............COMPILANDO ICBCLA SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO ICBCLA CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-ICBCLA
      $END
003400            LOCK MODE IS MANUAL WITH LOCK ON RECORD.
