      *    ICBSES.v.0002
      * VARIABLES PARA EL MANEJO DE SESIONES 
      ******************************************************************
      * NDG0002    *  RECICL: TIEMPO EN DIAS TOMADOS PARA RECICLAR LAS *
      * N.DUARTE   *          SESIONES QUE POR ALGUN MOTIVO NO SE HIZO *
      * 07/02/2008 *          LA FINALIZACION PARA LIBERARLA BIEN.     *
      ******************************************************************
      *            *                                                   *
      ******************************************************************
        01 COD-RETORNO-ICBSES.
           02 S1-ICBSES        PIC X.
           02 S2-ICBSES        PIC X.
           02 BIN-ICBSES REDEFINES S2-ICBSES PIC 99 COMP-X.
        01 ICBSES-TITLE        PIC X(80) VALUE "$DATABASE/ICBSESIO".
        01 EOF-ICBSES          PIC 9(01) VALUE ZEROS.
        01 W-LIBRE-ICBSES      PIC 9(03) VALUE ZEROS.
        01 W-USO-1-ICBSES      PIC 9(03) VALUE ZEROS.
        01 W-USADA-ICBSES      PIC 9(03) VALUE ZEROS.
        01 W-MAX-ICBSES        PIC 9(03) VALUE 98.
        01 W-OTRA-ICBSES       PIC 9(03) VALUE ZEROS.
        01 W-CLAVE-ICBSES      PIC X(05) VALUE SPACE.
        01 W-FEC-AUX-ICBSES    PIC 9(08) VALUE ZEROS.
RECICL  01 W-TIEMPO-ICBSES     PIC 9(03) VALUE 1.                       NDG0002
