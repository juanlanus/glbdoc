      * AMNISTIA21.v.0004
      *******************************************************************
      * VARIABLES DEFINIDAS PARA APLICACION DE LEYES, AMNISTIA, PARA
      * CLEENTES SEGUN FECHA DE PROMULGACION DE LA LEY
      *******************************************************************
      * JGR  FECHA:20/08/2007 06:19:07 p.m.
      * CAV  FECHA:16/01/2008
      ******************************************************************
      * DLM0002    | HISTOR - PERMITIR EJECUCION HISTORICA DE LA AMNIS-*
      * 29-JUL-2008|    TIA. LA VARIABLE WS-FECHA-HISTORICA DESDE EL   *
      * DARIO LASSO|    PROGRAMA DEBE COLOCARSE CON LA FECHA DESEADA.  *
      ******************************************************************
      * GJC0003    | LEY002 - DEFINICIONES PARA APLICACION DE LEY DEFINI*
      * G. JIMENEZ |  TIVO. DOBLE EL TIEMPODE LA MORA                   *
      * 23-ENE-2009|                                                    *
      *******************************************************************
      * GJC0004    | CADUC1 - SE DEFINEN LAS VARIABLES NECESARIAS PARA  *
      * G. JIMENEZ |          HACER CADUCIDAD SOBRE EL VECTOR ANTES DE  *
      * 23-ENE-2009|          INICIAR APLICACION DE ARTICULO 13         *
      *******************************************************************     
       01  ABD07-AMNIST21             PIC  X(220) VALUE SPACES.
       01  MESES-FALTAN-AMNIST21      PIC  9(03)  VALUE 999.
       01  FECHA-INI-AMNIST21         PIC 9(06) VALUE ZEROS.
       01  FECHA-INI-AMNIST21-R REDEFINES FECHA-INI-AMNIST21.
           03 ANO-INI-AMNIST21        PIC 9(04).
           03 MES-INI-AMNIST21        PIC 9(02).
       01  VERSION-AMNIST21           PIC 9      VALUE ZEROS.
       01  MESES-GRACIA-AMNIST21      PIC 9(02)  VALUE 06.
       01  MESES-CANCELAC-AMNIST21    PIC 9(02)  VALUE 12.

       01  WORK-CLIENTE-AMNIST21.
           03 CTAS-MEN12-AMNIST21     PIC  9(01) VALUE ZEROS.
           03 CTAS-MAY12-AMNIST21     PIC  9(01) VALUE ZEROS.
           03 MESES-NOV-AMNIST21      PIC  9(04) VALUE ZEROS.
           03 MESES-VEC-AMNIST21      PIC  9(04) VALUE ZEROS.
           03 MESES-GRC-AMNIST21      PIC  9(04) VALUE ZEROS.
           03 MORAS-ACTUAL-AMNIST21   PIC  9(05) VALUE ZEROS.
           03 TIPO-ID-MAES-AMNIST21   PIC  9(01) VALUE ZEROS.
           03 NUM-ID-MAES-AMNIST21    PIC  9(11) VALUE ZEROS.
LEY002     03 K-AMNIST21              PIC  9(03) VALUE ZEROS.           GJC0003
LEY002     03 L-AMNIST21              PIC  9(03) VALUE ZEROS.           GJC0003
LEY002     03 M-AMNIST21              PIC  9(03) VALUE ZEROS.           GJC0003
LEY002     03 X-AMNIST21              PIC  9(03) VALUE ZEROS.           GJC0003
LEY002     03 Z-AMNIST21              PIC  9(03)  VALUE ZEROS.          GJC0003
CADUC1     03 IND-APLICO-CAD-AMNIST21 PIC  9(01) VALUE ZEROS.           GJC0004
           03 FEC-NOVEDAD-AMNIST21.
              05 ANO-NOV-AMNIST21     PIC  9(04) VALUE ZEROS.
              05 MES-NOV-AMNIST21     PIC  9(02) VALUE ZEROS.
           03 FECHA-AUX-AMNIST21      PIC  9(04).
           03 FECHA-AUX-AMNIST21-R REDEFINES FECHA-AUX-AMNIST21.
              05 MES-AUX-AMNIST21     PIC  9(02).
              05 ANO-AUX-AMNIST21     PIC  9(02).
      *---------------------------------------------------------------*
       01  LLAVE-INI-AMNIST21.
           03 TIPO-ID-AMNIST21        PIC  9.
           03 NUM-ID-AMNIST21         PIC  9(11).
           03 COD-SUS-AMNIST21        PIC  9(6).
           03 COD-CTA-AMNIST21        PIC  99.
           03 NUM-CTA-AMNIST21        PIC  X(18).
      *---------------------------------------------------------------*
       01  REG-SUSCRIPTOR-AMNISTIA21  PIC X(220).
LEY002*01  SW-CANDIDATO-AMNIST21      PIC 9(01) VALUE ZEROS.            GJC0003
      *---------------------------------------------------------------*
       01  MUMERO-ID-AMNIST21.
           03 SW-AMNIST21             PIC  9(01) VALUE ZEROS.
           03 SW-SALIR-VECT           PIC  9(01) VALUE ZEROS.
           03 TIPO-ID-INI-AMNIST21    PIC  9(01) VALUE ZEROS.
           03 NUME-ID-INI-AMNIST21    PIC  9(11) VALUE ZEROS.
      *---------------------------------------------------------------*
       01  FECHA-MILENIO-AMNIST21.
           03 FMILENIO-SIGLO-AMNIST21 PIC  9(02).
           03 FMILENIO-ANIO-AMNIST21  PIC  9(02).
           03 FMILENIO-MES-AMNIST21   PIC  9(02).
      *---------------------------------------------------------------*
      *   PRUEBAS  FECHA 200608                                       *
      *---------------------------------------------------------------*
       01  FECHA-HOY-AMNIST21         PIC X(08).
       01  FECHA-HOY-AMNIST21-R  REDEFINES FECHA-HOY-AMNIST21.
           03 FECHA-SYS-AMNIST21.
              05 ANO-HOY-AMNIST21     PIC 9(04).
              05 MES-HOY-AMNIST21     PIC 9(02).
           03 DIA-HOY-AMNIST21        PIC 9(02).
      *---------------------------------------------------------------*
       01  FECHA-GRACIA-AMNIST21.
           05 ANO-GRACIA-AMNIST21     PIC  9(04) VALUE ZEROS.
           05 MES-GRACIA-AMNIST21     PIC  9(02) VALUE ZEROS.
      *---------------------------------------------------------------
       01  VECTOR-AMNISTA21.
           03 VECTOR1-AMNIST21        PIC 9(12) VALUE ZEROS. 
           03 VECTOR2-AMNIST21        PIC 9(12) VALUE ZEROS.
           03 VECTOR3-AMNIST21        PIC 9(12) VALUE ZEROS.
           03 VECTOR4-AMNIST21        PIC 9(12) VALUE ZEROS.
      *---------------------------------------------------------------
       01  VECT-AMNIST21.
           03 VECT1-AMNIST21          PIC 9(12).
           03 VECT2-AMNIST21          PIC 9(12).
           03 VECT3-AMNIST21          PIC 9(12).
           03 VECT4-AMNIST21          PIC 9(12).
           03 HISTORIA-TOTAL-AMNIST21.
              05 VECTOR-TOT-AMNIST21  PIC X(48).
           03 FILLER REDEFINES HISTORIA-TOTAL-AMNIST21.
              05 V-VEC-AMNIST21  OCCURS 48 TIMES PIC X.
      *---------------------------------------------------------------
      *TABLA QUE IDENTIFICA EL ESTADO DEL REGISTRO POR LA NOVEDAD
      * 0-OBLIGACION CANCELADA
      * 1-OBLIGACIONES ABIERTAS
      *---------------------------------------------------------------
       01  TAB-VIGENTES-AMNIST21.
           03 TABLA-VIGENTES-AMNIST21.
              05 FILLER        PIC X(10)   VALUE  "1000000000".
              05 FILLER        PIC X(10)   VALUE  "0011111111".
              05 FILLER        PIC X(10)   VALUE  "1111111111".
              05 FILLER        PIC X(10)   VALUE  "1111111111".
              05 FILLER        PIC X(10)   VALUE  "1111101101".
           03 TABLA-VIGENTES-AMNIST21-R REDEFINES
              TABLA-VIGENTES-AMNIST21.
              05 TAB-VIG-CART-AMNIST21   OCCURS 50 TIMES  PIC 9(01).
      *---------------------------------------------------------------*
       01  TABLAS-CR-AMNIST21.
LEY002     03 TABLA-NOV-CAR-AMNIST21.                                   GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "01 01 5 AL DIA         1718192001 5".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "02 02 5 TDC NO RECLAMAD0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "08 03 5 TDC CANC.MALMAN0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "04 04 5 TDC ROBADA     0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "05 05 5 CANCEL.VOLUNTAR0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "06 06 5 CANCEL.MMX<60  0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "07 07 5 TDC EXTRAVIADA 0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "03 08 5 PAGO VOLUNT.SAT0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "03 09 5 PAGO.MMX.A.30  09EEEEEE08 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "03 10 5 PAGO.MMX.A.60  0910EEEE08 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "03 11 5 PAGO.MORA.90   091011EE08 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "03 12 5 PAGO.VOL.MOR1200910111208 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "01 13 5 MORA.30.AL.DIA 1727282913 1".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "01 14 5 MORA.60.AL.DIA 3031323314 2".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "01 15 5 MORA.90.AL.DIA 3435363715 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "01 16 5 MORA.120.AL.DIA3839404116 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "12 17 1 ESTA.MORA.30   1718192013 1".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "13 18 2 ESTA.MORA.60   2118192014 2".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "14 19 3 ESTA.MORA.90   2223192015 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "15 20 4 ESTA.MORA.120  2425262016 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "12 21 1 FU.60.Y.ESTA.302118192014 2".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "12 22 1 FU.90.Y.ESTA.302223192015 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "13 23 2 FU.90.Y.ESTA.602223192015 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "12 24 1 FU.120.ESTA.30 2425262016 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "13 25 2 FU.120.ESTA.60 2425262016 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "14 26 3 FU.120.ESTA.90 2425262016 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "13 27 2 RE.30.ESTA.60  2127282914 2".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "14 28 3 RE.30.Y.ESTA.902223282915 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "15 29 4 RE.30.ESTA.120 2425262916 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "12 30 1 RE.60.Y.ESTA.303031323314 2".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "13 31 2 RE.60.Y.ESTA.602131323314 2".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "14 32 3 RE.60.Y.ESTA.902223323315 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "15 33 4 RE.60.ESTA.120 2425263316 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "12 34 1 RE.90.Y.ESTA.303435363715 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "13 35 2 RE.90.Y.ESTA.603435363715 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "14 36 3 RE.90.Y.ESTA.902223363715 3".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "15 37 4 RE.90.ESTA.120 2425263716 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "12 38 1 RE.120.ESTA.30 3839404116 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "13 39 2 RE.120.ESTA.60 3839404116 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "14 40 3 RE.120.ESTA.90 3839404116 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "15 41 4 RE.120.ESTA.1202425264116 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "20 42   ADJ.NO.LOCALIZA0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "21 43   ADJ.CTA.COBRADO0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "22 44   FALLECIDO      0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "09 45 4 CARTERA.CASTIGA0000000000 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "10 46 5 CARTERA.RECUPER0000000000 5".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "11 47 4 DUDOSO.RECAUDO 0000000000 4".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "23 48   CONSULTA       0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "49 49 5 NO RENOVADA    0000000000 0".                GJC0003
LEY002        05  FILLER  PIC X(35) VALUE                               GJC0003
LEY002            "50                     0000000000 0".                GJC0003
           03  TABLA-NOV-CAR-AMNIST21-R  REDEFINES 
                                           TABLA-NOV-CAR-AMNIST21.
               05  TAB-NOV-CARTERA-AMNIST21   OCCURS  50  TIMES.
                   07 NOV-CART-CODINT-AMNIST21 PIC 9(02).
LEY002             07 FILLER                   PIC X.                   GJC0003
                   07 NOV-CART-COD-AMNIST21    PIC 9(02).
LEY002             07 FILLER                   PIC X.                   GJC0003
LEY002             07 NOV-CART-MORA-AMNIST21   PIC 9(01).               GJC0003
LEY002             07 FILLER                   PIC X.                   GJC0003
                   07 NOV-CART-NOM-AMNIST21    PIC X(15).
LEY002             07 NOV-CRUCES-AMNIST21   OCCURS 5 TIMES PIC X(2).    GJC0003
LEY002             07 FILLER                   PIC X.                   GJC0003
LEY002             07 NOV-CART-MORMAX-AMNIST21 PIC 9(01).               GJC0003
      *---------------------------------------------------------------*
      * DEFINICION DE VARIABLES DE AMBIENTE
      *---------------------------------------------------------------*
        01 VAR-AMBIENTE-AMNIST21          PIC X(10) VALUE "FECHA_AMNI".  
LEY002  01 VAR-AMB-HOY-AMNIST21           PIC X(10) VALUE "FECHA_HOY ". GJC0003
LEY002  01 VAR-FEC-AMB-AMNIST21.                                        GJC0003
LEY002     03 ANO-VAR-FEC-AMB-AMNIST21    PIC 9(04).                    GJC0003
LEY002     03 MES-VAR-FEC-AMB-AMNIST21    PIC 9(02).                    GJC0003
LEY002     03 DIA-VAR-FEC-AMB-AMNIST21    PIC 9(02).                    GJC0003
LEY002* 01  WS-FECHA-HISTORICA         PIC  9(08) VALUE  ZEROS.         GJC0003
LEY002* 01                       REDEFINES  WS-FECHA-HISTORICA.         GJC0003
LEY002*   03  WS-FECHA-HIST-ANNO       PIC  9(04).                      GJC0003
LEY002*   03  WS-FECHA-HIST-MES        PIC  9(02).                      GJC0003
LEY002*   03  WS-FECHA-HIST-DIA        PIC  9(02).                      GJC0003

REMARK*----------------------------------------------------------------*
REMARK*DEFINICION DE LAS VALIABLES QUE SE UTILIZAN PARA EL CALCULO DE  *
REMARK*LEY ARTICULO 13 DOBLE DEL TIEMPO DE LA MORA
REMARK*----------------------------------------------------------------*
LEY002  01 VAR-LEY-DOBLE-DE-MORA-AMNIST21.                              GJC0003
LEY002     03 SW-ART13-AMNIST21   PIC 9 VALUE 1. *> APLICAR SIEMPRE LEY GJC0003
LEY002     03 VECTORES-13-AMNIST21.                                     GJC0003
LEY002* VECTOR DE MORAS DE LA PERSONA                                   GJC0003
LEY002        05 VECTOR-MORAS-IDE-13-AMNIST21 PIC X(100) VALUE ALL "5". GJC0003
LEY002        05 VEC-MOR-IDE-13-AMNIST21 REDEFINES                      GJC0003
LEY002           VECTOR-MORAS-IDE-13-AMNIST21 PIC 9 OCCURS 100 TIMES.   GJC0003
LEY002* VECTOR DE LA CUENTA REAL                                        GJC0003
LEY002        05 VECTOR-TOT-CTA-AMNIST21.                               GJC0003
LEY002           07  VECTOR-4-CTA-AMNIST21  PIC 9(12) VALUE 0.          GJC0003
LEY002           07  VECTOR4-A-CTA-AMNIST21 REDEFINES                   GJC0003
LEY002               VECTOR-4-CTA-AMNIST21  PIC X(12).                  GJC0003
LEY002           07  VECTOR-3-CTA-AMNIST21  PIC 9(12) VALUE 0.          GJC0003
LEY002           07  VECTOR3A-CTA-AMNIST21 REDEFINES                    GJC0003
LEY002               VECTOR-3-CTA-AMNIST21  PIC X(12).                  GJC0003
LEY002           07  VECTOR-2-CTA-AMNIST21  PIC 9(12) VALUE 0.          GJC0003
LEY002           07  VECTOR2-A-CTA-AMNIST21 REDEFINES                   GJC0003
LEY002               VECTOR-2-CTA-AMNIST21  PIC X(12).                  GJC0003
LEY002           07  VECTOR-1-CTA-AMNIST21  PIC 9(12) VALUE 0.          GJC0003
LEY002           07  VECTOR1-A-CTA-AMNIST21 REDEFINES                   GJC0003
LEY002               VECTOR-1-CTA-AMNIST21  PIC X(12).                  GJC0003
LEY002           07  MOR-EST-CTA-AMNIST21   PIC 9     VALUE 0.          GJC0003
LEY002        05 VEC-TOT-CTA-AMNIST21 REDEFINES                         GJC0003
LEY002              VECTOR-TOT-CTA-AMNIST21 OCCURS 49 TIMES PIC 9.      GJC0003
LEY002* VECTOR DE LA CUENTA SOBRE EL QUE SE BUSCA SI SE PERDONA         GJC0003
LEY002       05 VECTOR-TOT-CTA2-AMNIST21.                               GJC0003
LEY002           07  VECTOR-4-CTA2-AMNIST21  PIC 9(12) VALUE 0.         GJC0003
LEY002           07  VECTOR4-A-CTA2-AMNIST21 REDEFINES                  GJC0003
LEY002               VECTOR-4-CTA2-AMNIST21  PIC X(12).                 GJC0003
LEY002           07  VECTOR-3-CTA2-AMNIST21  PIC 9(12) VALUE 0.         GJC0003
LEY002           07  VECTOR3A-CTA2-AMNIST21 REDEFINES                   GJC0003
LEY002               VECTOR-3-CTA2-AMNIST21  PIC X(12).                 GJC0003
LEY002           07  VECTOR-2-CTA2-AMNIST21  PIC 9(12) VALUE 0.         GJC0003
LEY002           07  VECTOR2-A-CTA2-AMNIST21 REDEFINES                  GJC0003
LEY002               VECTOR-2-CTA2-AMNIST21  PIC X(12).                 GJC0003
LEY002           07  VECTOR-1-CTA2-AMNIST21  PIC 9(12) VALUE 0.         GJC0003
LEY002           07  VECTOR1-A-CTA2-AMNIST21 REDEFINES                  GJC0003
LEY002               VECTOR-1-CTA2-AMNIST21  PIC X(12).                 GJC0003
LEY002           07  MOR-EST-CTA2-AMNIST21   PIC 9     VALUE 0.         GJC0003
LEY002           07  MOR-MORA-CTA2-AMNIST21  PIC 9     VALUE 1.         GJC0003
LEY002        05 VEC-TOT-CTA2-AMNIST21 REDEFINES                        GJC0003
LEY002              VECTOR-TOT-CTA2-AMNIST21 OCCURS 50 TIMES PIC 9.     GJC0003
LEY002* VECTOR DE CUENTA EN 100 POSICIONES NORMALIZADO                  GJC0003
LEY002        05 VECTOR-100-CTA-AMNIST21     PIC X(101).                GJC0003
LEY002        05 VEC-100-CTA-AMNIST21 REDEFINES                         GJC0003
LEY002           VECTOR-100-CTA-AMNIST21 pic 9 OCCURS 101 TIMES.        GJC0003
LEY002* VECTOR DE LAPSOS DE MORAS                                       GJC0003
LEY002        05 VECTOR-LAPSOS OCCURS 30 TIMES.                         GJC0003
LEY002           07 PERDON-GRUPO-AMNIST21  PIC 9(01).                   GJC0003
LEY002           07 INI-GRUPO-AMNIST21     PIC 9(02).                   GJC0003
LEY002           07 LONG-GRUPO-AMNIST21    PIC 9(02).                   GJC0003
LEY002                                                                  GJC0003
LEY002     03 VARIALES-MORAS-13-AMNIST21.                               GJC0003
LEY002        05 LONG-LAPSO-13-AMNIST21 PIC 9(02) VALUE 0.              GJC0003
LEY002        05 MAX-LONG-LAPSO-13-AMNIST21 PIC 9(02).                  GJC0003
LEY002     03  OTRAS-VAR-13-AMNIS21.                                    GJC0003
LEY002        05 CINCOS-AMNIST21        PIC X(100) VALUE ALL "5".       GJC0003
LEY002        05 UNOS-AMNIST21          PIC X(100) VALUE ALL "1".       GJC0003
LEY002        05 MAYOR-LAPSO-AMNIST21   PIC 9(02).                      GJC0003
LEY002        05 SALIR-AMNIST21         PIC 9.                          GJC0003
LEY002        05 CTA-PERDONADA-AMNIST21 PIC 9.                          GJC0003
LEY002        05 LAPSO-AMNIST21         PIC 9(02).                      GJC0003
LEY002        05 MESES-MORAS-AMNIST21   PIC 9(02).                      GJC0003
LEY002        05 ESTOY-MOROSO-AMNIST21  PIC 9.                          GJC0003
LEY002        05 MESES-AMNIST21         PIC 9(3).                       GJC0003
LEY002        05 FEC-NOV-AUX-AMNIST21 PIC 9(4).                         GJC0003
LEY002        05 FEC-NOV-AUX1-AMNIST21 PIC 9(4).                        GJC0003
LEY002        05 FEC-SIT-45Y47-AMNIST21 PIC 9(6).                       GJC0003
LEY002        05 FEC-SIT-45Y47-R-AMNIST21 REDEFINES                     GJC0003
LEY002           FEC-SIT-45Y47-AMNIST21.                                GJC0003
LEY002           07 MES-SIT-AMNIST21     PIC 9(02).                     GJC0003
LEY002           07 ANO-SIT-AMNIST21     PIC 9(04).                     GJC0003
LEY002*DEFINICION DE VARIABLES PARA HACER RECALCULO DE ESTADO Y OTROS CAGJC0003
LEY002 01 VARIABLES-RECALCULO.                                          GJC0003
LEY002    03 HISTORIA-TOTAL-AMNIST21.                                   GJC0003
LEY002       05 VECTOR-1-AMNIST21        PIC X(12).                     GJC0003
LEY002       05 VECTOR-2-AMNIST21        PIC X(12).                     GJC0003
LEY002       05 VECTOR-3-AMNIST21        PIC X(12).                     GJC0003
LEY002       05 VECTOR-4-AMNIST21        PIC X(12).                     GJC0003
LEY002       05 EST-HIST-AMNIST21        PIC X(1).                      GJC0003
LEY002    03 FILLER REDEFINES HISTORIA-TOTAL-AMNIST21.                  GJC0003
LEY002       05 V-VECTOR-AMNIST21   OCCURS 49 TIMES PIC X.              GJC0003
LEY002    03 FIN-AMNIST21                 PIC 9 VALUE 0.                GJC0003
LEY002    03 FIN-MATRIZ-AMNIST21          PIC 99 VALUE 0.               GJC0003
LEY002    03 ESTADO-AMNIST21              PIC 99.                       GJC0003
LEY002    03 ESTADO-INI-AMNIST21          PIC 99.                       GJC0003
LEY002    03 ESTADO-FIN-AMNIST21          PIC 99.                       GJC0003
LEY002    03 ESTADO-INI48-AMNIST21          PIC 99.                     GJC0003
LEY002    03 ESTADO-FIN48-AMNIST21          PIC 99.                     GJC0003
LEY002    03 FECHA-AMNIST21.                                            GJC0003
LEY002       05 ANO-T-AMNIST21            PIC 9(4).                     GJC0003
LEY002       05 FILLER REDEFINES ANO-T-AMNIST21.                        GJC0003
LEY002          07 SIGLO-AMNIST21         PIC 99.                       GJC0003
LEY002          07 ANO-AMNIST21           PIC 9(2).                     GJC0003
LEY002       05 MES-AMNIST21              PIC 9(2).                     GJC0003
LEY002    03 FECHA-1-AMNIST21.                                          GJC0003
LEY002       05 ANO-T-1-AMNIST21          PIC 9(4).                     GJC0003
LEY002       05 FILLER REDEFINES ANO-T-1-AMNIST21.                      GJC0003
LEY002          07 SIGLO-1-AMNIST21       PIC 99.                       GJC0003
LEY002          07 ANO-1-AMNIST21         PIC 9(2).                     GJC0003
LEY002       05 MES-1-AMNIST21            PIC 9(2).                     GJC0003
LEY002    03 FEC-INI-MOR-AMNIST21         PIC 9(6).                     GJC0003
LEY002    03 MES-MORA-MAX-AMNIST21        PIC 9(2).                     GJC0003
LEY002    03 MES-MORA-MAX-48-AMNIST21        PIC 9(2).                  GJC0003
LEY002    03 FEC-MM-AA-AMNIST21           PIC 9(4).                     GJC0003
LEY002    03 FEC-MM-AAAA-AMNIST21         PIC 9(6).                     GJC0003
LEY002    03 NOV-ENT-AMNIST21             PIC 9(02).                    GJC0003
LEY002    03 FEC-NOV-AMNIST21             PIC 9(04).                    GJC0003
LEY002    03 FEC-MORMAX-AMNIST21          PIC 9(06).                    GJC0003
LEY002    03 FEC-MORMAX-48-AMNIST21       PIC 9(06).                    GJC0003
LEY002    03 MORMAX-AMNIST21              PIC 9(01).                    GJC0003
LEY002    03 MORMAX48-AMNIST21            PIC 9(01).                    GJC0003
LEY002    03 NUM-MOR30-AMNIST21           PIC 9(02).                    GJC0003
LEY002    03 NUM-MOR30-48-AMNIST21        PIC 9(02).                    GJC0003
LEY002    03 NOV-SAL-AMNIST21             PIC 9(02).                    GJC0003
LEY002    03 FEC-NOV-48-AMNIST21          PIC 9(04).                    GJC0003
LEY002    03 NOV-SAL-48-AMNIST21          PIC 9(02).                    GJC0003
LEY002    03 POSICION-INI-AMNIST21        PIC 9(03).                    GJC0003


