REMARK*LEEMAESTROS.WS.v.0003.
REMARK********************************************************************
REMARK* COPY QUE CONTIENE LAS VARIABLES NECESARIAS PARA EL MANEJO DE LA 
REMARK* REGIS (BDAMATCH.WS) Y LAS VARIABLES PARA EL MENEJO DE LA 
REMARK* AMNISTIA (AMNISTIA21.WS). 
REMARK********************************************************************
REMARK* GJC0002    * LEY001 - CAMPO PARA INICIALIZADO EN 1 PARA QUE SIEMPRE*
REMARK* G.JIMENEZ  *          ENTREGUE LA NOVEDAT QUE LEYÓ (EN 48). SI UN  *
REMARK* MAY 18/2009*                                                       *
REMARK**********************************************************************
REMARK* GJC0003    * BBVA01 - SE CREA FLAG PARA ELIMINAR O DEJAR CUENTAS DE*
REMARK* G.JIMENEZ  *          BBVA SEGUN SOLICITUD DE LA ENTIDAD EN ICGCON *
REMARK* AGO 25/2009*          NO SE DEBEN MOSTRAR Y TODO LO DEMAS (SCORES, *
REMARK*            *          PES,ETC) SI LAS DEBEN TENER EN CUENTA        *
REMARK**********************************************************************
       COPY BDAMATCH.WS.
       COPY AMNISTIA21.WS.
       COPY VALNOM.WS.
       COPY DIRECC.WS.


REMARK********************************************************************
REMARK* VARIABLES PARA EL MANEJO DIMANICO DE MATCH Y AMNISTIA PARA CADA
REMARK* PROGRAMA. POR DEFAULT SIEMPRE LO EJECUTA.
       01  HACERMATCH-LEEMSTRS        PIC 9(01)  VALUE 1.          
       01  AMNISTIA21-LEEMSTRS        PIC 9(01)  VALUE 1.          
REMARK********************************************************************
       01  SW-LECTURA-MAESTROS        PIC 9(01)  VALUE ZEROS. 
       01  TIP-IDE-ANT-LEEMSTRS       PIC 9(01)  VALUE ZEROS.
       01  NUM-IDE-ANT-LEEMSTRS       PIC 9(11)  VALUE ZEROS.
       01  REG-ICMCRE-LEEMSTRS        PIC X(220) VALUE SPACES.
       01  REG-ICMCHK-LEEMSTRS        PIC X(94)  VALUE SPACES.
       01  REG-ICBSUS-LEEMSTRS        PIC X(200) VALUE SPACES.
       01  REG-ICBDIR-LEEMSTRS        PIC X(483) VALUE SPACES.
       01  TIPO-ID-LEEMSTRS           PIC 9(01)  VALUE ZEROS.
       01  NRO-ID-LEEMSTRS            PIC 9(11)  VALUE ZEROS.
       01  SW-FCHGRACIA-LEEMSTRS      PIC 9(01)  VALUE ZEROS.
LEY001 01  SW-NOV-48-LEEMSTRS         PIC 9(01)  VALUE 1.               GJC0002
BBVA01 01  SW-ELIMINA-BBVA-LEEMSTRS   PIC 9(01)  VALUE 0. *> 0-NO 1-SI  GJC0003
