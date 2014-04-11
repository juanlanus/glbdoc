      * BDIMIC.WS.V01.0002.
REMARK******************************************************************
REMARK*VARIABLES PARA RUTINA DE MANEJO DEL REPORTE DE MICROCREDITO   
REMARK******************************************************************
REMARK* FAC0002     * REPMI2 *SE ADICIONAN VARIABLES PARA MANEJO DEL   *
REMARK* F.CUADROS C.*         HISTORICO DE MICROCREDITO.               *
REMARK* SEP.07/2005 *                                                  *
REMARK******************************************************************
       01  CAR-ANTER               PIC 9(01).
       01  SUSANT-MICROC           PIC 9(06).
       01  FECHA-AUX-MICRO         PIC 9(06) VALUE 0.
       01  FILLER                  REDEFINES FECHA-AUX-MICRO.
           03 ANO-AUX-MICRO        PIC 9(04) .
           03 MES-AUX-MICRO        PIC 9(02) .
       01  FEC-MMAAAA              PIC 9(06).
       01  FEC-MMAAAA-R            REDEFINES FEC-MMAAAA.
           03 MES-MMAAAA           PIC 9(02).
           03 ANO-MMAAAA           PIC 9(04).
       01  IND-FAC                 PIC 9(01) VALUE 0.
       01  SUB-NUMFAC              PIC 9(05) VALUE ZEROS.
       01  SUB-VALFAC              PIC 9(10) VALUE ZEROS.
       01  VALFAC-AUX              PIC 9(10) VALUE ZEROS.
       01  COMANDO-SO              PIC X(90).
       01  POS-TEXTO               PIC 9(03).
REPMI2 01  FDA-BDIMHIS             PIC 9(01) VALUE ZEROS.               FAC0002 
       01  NOMARC-MAE1             PIC X(40).
       01  NOMARC-MAE1-IDX         PIC X(40).
       01  NOMARC-MAE2             PIC X(40).
       01  NOMARC-MAE2-IDX         PIC X(40).
       01  NOMARC-MAE3             PIC X(40).
       01  NOMARC-MAE3-IDX         PIC X(40).
       01  NOMARC-MAE4             PIC X(40).
       01  NOMARC-MAE4-IDX         PIC X(40).
       01  NOMHIS-MAE3             PIC X(40).
       01  NOMHIS-MAE3-IDX         PIC X(40).
       01  FEC-REAL-3              PIC 9(06).
       01  FEC-REAL-4              PIC 9(06).
       01  FEC-REAL-4-R            REDEFINES FEC-REAL-4.
           03 ANO-REAL-4           PIC 9(04).
           03 MES-REAL-4           PIC 9(02).
       01  COD-RET                 PIC X(02) COMP-5.
REPMI2 01  FILE-STATUS-BDIMHIS     PIC X(02).                           FAC0002 
REPMI2 01  FILE-STATUS-BDIMHIT     PIC X(02).                           FAC0002 
       01  ENCABEZA-WS             PIC X(30).
       01  IND-ARCH-MICRO          PIC X(03).
       01  RESUMEN-VALORES.
           03 MES-VALORES          OCCURS 12 TIMES.
              05 TOT-VALORES-COR   PIC 9(08).
              05 TOT-VALORES-MOR   PIC 9(08) OCCURS 6 TIMES.
              05 TOT-VALORES-VEN   PIC 9(08).
       01  RESUMEN-FACTURAS.
           03 MES-FACTURAS         OCCURS 12 TIMES.
              05 CAN-FACTURAS-COR  PIC 9(05).
              05 VAL-FACTURAS-COR  PIC 9(08).
              05 CAN-FACTURAS-VEN  PIC 9(05).
              05 VAL-FACTURAS-VEN  PIC 9(08).
              05 TAB-FACTURAS-MORA OCCURS 6 TIMES.
                 07 CAN-FACTURAS-MOR  PIC 9(05).
                 07 VAL-FACTURAS-MOR  PIC 9(08).

       01  TABLA-MESES-MICRO. 
REPMI2     03 TAB-MESES-MICRO      PIC 9(06) OCCURS  12 TIMES.          FAC0002 
       01  TABLA-CAR-MICRO.
           03 FILLER               PIC X(20) VALUE 
              "CORRIENTE           ".
           03 FILLER               PIC X(20) VALUE
              "VENCIDO 001-030 DIAS".
           03 FILLER               PIC X(20) VALUE 
              "VENCIDO 031-060 DIAS".
           03 FILLER               PIC X(20) VALUE 
              "VENCIDO 061-090 DIAS".
           03 FILLER               PIC X(20) VALUE 
              "VENCIDO 091-119 DIAS".
           03 FILLER               PIC X(20) VALUE 
              "VENCIDO 120-O-MAS   ".
           03 FILLER               PIC X(20) VALUE 
              "CARTERA CASTIGADA   ".
       01  TABLA-CAR-MICRO-R       REDEFINES TABLA-CAR-MICRO.
           03 TAB-CAR-MICRO        PIC X(20) OCCURS 7 TIMES.

       01  TABLA-DES-MICRO.
           03 FILLER               PIC X(09) VALUE
              "001-030  ".
           03 FILLER               PIC X(09) VALUE 
              "031-060  ".
           03 FILLER               PIC X(09) VALUE 
              "061-090  ".
           03 FILLER               PIC X(09) VALUE 
              "091-119  ".
           03 FILLER               PIC X(09) VALUE 
              "120-O-MAS".
           03 FILLER               PIC X(09) VALUE 
              "CASTIGO  ".
       01  TABLA-DES-MICRO-R       REDEFINES TABLA-DES-MICRO.
           03 TAB-DES-MICRO        PIC X(09) OCCURS 6 TIMES.

       01  TABLA-FECHAS-MICROC.
           03 T-FEC-MICROC         PIC 9(06) OCCURS 4 TIMES.
REMARK*****************************FORMATOS*********************
       01 FORMATO-72.
REMARK*** LINEA  MICROCREDITO
          05 F72-CAR                     PIC X(20)B.
          05 F72-SUS                     PIC X(19)B.
          05 F72-TIP                     PIC X(03)B.
          05 F72-FAC                     PIC ZZZ,ZZZB(04).
          05 F72-VAL                     PIC $$$,$$$,$$9B(01).
          05 F72-PLA                     PIC X(12).     
       01 FORMATO-73.   
REMARK*** TITULO SECCION DE MICROCREDITO
          05 FILLER                      PIC X(80) VALUE SPACES. 
          05 FILLER                      PIC X(14) VALUE ALL "=".
          05 FILLER                      PIC X(29) VALUE 
             " [ OBLIGACIONES POR PAGAR MIC".
          05 FILLER                      PIC X(15) VALUE 
             "ROCREDITO MES  ".   
          05 F73-FECHA                   PIC 9999/99B VALUE 0.             
          05 FILLER                      PIC X(02) VALUE ALL "] ".
          05 FILLER                      PIC X(12) VALUE ALL "=".
          05 FILLER                      PIC X(50) VALUE
             "EDAD CARTERA    COMPANIA-INFORMANTE    TIP.CTA  #F".
          05 FILLER                      PIC X(30) VALUE          
             "ACTURAS  VAL.FACTURAS  PLAZO  ".
          05 FILLER                      PIC X(26) VALUE ALL "=".
          05 FILLER                      PIC X(27) VALUE  
             " VALORES EN MILES DE PESOS ".
          05 FILLER                      PIC X(27) VALUE ALL "=".
          05 FILLER                      PIC X(80) VALUE SPACES. 
       01 FORMATO-74.
REMARK*** LINEA SUBTOTALES POR EDAD DE CARTERA DE MICROCREDITO
          05 FILLER                      PIC X(43) VALUE SPACES.
          05 F74-SUBFAC                  PIC Z,ZZZ,ZZZB.
          05 F74-SUBVAL                  PIC $$,$$$,$$$,$$9BBB.
          05 FILLER                      PIC X(09) VALUE SPACES.     
       01 FORMATO-75.
REMARK*** LINEAS DE SUBTOTALES POR EDAD DE CARTERA DE MICROCREDITO
          05 FILLER   PIC X(43) VALUE SPACES.
          05 FILLER   PIC X(10) VALUE "  ------- ".
          05 FILLER   PIC X(17) VALUE " -------------".          
          05 FILLER   PIC X(10) VALUE SPACES.                  
       01 FORMATO-76.   
REMARK*** TITULO RESUMEN DE VALORES DE MICROCREDITO ULTIMOS 12 MESES
          05 FILLER                      PIC X(80) VALUE SPACES. 
          05 FILLER                      PIC X(14) VALUE ALL "=".
          05 FILLER                      PIC X(52) VALUE 
             " [ RESUMEN VALORES MICROCREDITO ULTIMOS 12 MESES  ] ".
          05 FILLER                      PIC X(14) VALUE ALL "=".
          05 FILLER                      PIC X(51) VALUE
             "        CARTERA            C A R T E R A  V E N C I".
          05 FILLER                      PIC X(29) VALUE          
             "D A                   TOTAL  "  .
          05 FILLER                      PIC X(50) VALUE
             "FECHA   CORRIENTE  001-030  031-060  061-090  091-".
          05 FILLER                      PIC X(30) VALUE
             "119 120-O-MAS CASTIGO  VENCIDA".
          05 FILLER                      PIC X(80) VALUE ALL "=".
       01 FORMATO-77.
REMARK*** LINEA RESUMEN DE VALORES POR EDAD DE CARTERA DE MICROCREDITO
          05 F77-FECHA                   PIC 9999/99B.
          05 F77-TOT-COR                 PIC $$$$$,$$9B.
          05 F77-001-030                 PIC $$$$,$$9B.
          05 F77-031-060                 PIC $$$$,$$9B.
          05 F77-061-090                 PIC $$$$,$$9B.
          05 F77-091-119                 PIC $$$$,$$9B.
          05 F77-MAS-120                 PIC $$$$,$$9B.
          05 F77-CASTIGO                 PIC $$$$,$$9B.
          05 F77-TOT-VEN                 PIC $$$$,$$9.
       01 FORMATO-78.   
REMARK*** TITULO RESUMEN DE FACTURAS DE MICROCREDITO ULTIMOS 12 MESES 
          05 FILLER                      PIC X(80) VALUE SPACES.
          05 FILLER                      PIC X(14) VALUE ALL "=".
          05 FILLER                      PIC X(52) VALUE 
             " [ RESUMEN FACTURAS MICROCREDITO ULTIMOS 12 MESES ] ".
          05 FILLER                      PIC X(14) VALUE ALL "=".
          05 FILLER                      PIC X(54) VALUE
             "         CARTERA  CORRIENTE   CARTERA  VENCIDA        ".
          05 FILLER                      PIC X(26) VALUE          
             " MOROSIDAD  MAXIMA        ".
          05 FILLER                      PIC X(52) VALUE
             "FECHA    #FACTURAS     VALOR  #FACTURAS     VALOR   ".
          05 FILLER                      PIC X(28) VALUE
             "DIAS      #FACTURAS    VALOR".
          05 FILLER                      PIC X(80) VALUE ALL "=".
       01 FORMATO-79.
REMARK*** LINEA RESUMEN DE FACTURAS POR EDAD DE CARTERA DE MICROCREDITO
          05 F79-FECHA                   PIC 9999/99BB.
          05 F79-FAC-COR                 PIC ZZZZ9BBBB.
          05 F79-VAL-COR                 PIC $$$$$$,$$9BB.
          05 F79-FAC-VEN                 PIC ZZZZ9BBBB.
          05 F79-VAL-VEN                 PIC $$$$$$,$$9BBB.
          05 F79-DES-MOR                 PIC X(09)B.       
          05 F79-FAC-MOR                 PIC ZZZZ9BBB.
          05 F79-VAL-MOR                 PIC $$$$$$,$$9.
REMARK***********************SALIDA  MICROCREDITO********************
       01 SALIDA-72 .
REMARK****************INF DE  MICROCREDITO*************
          05 S72-CAR                     PIC X(20).
          05 S72-SUS                     PIC X(19).
          05 S72-TIP                     PIC X(03).
          05 S72-FAC                     PIC 9(06).
          05 S72-VAL                     PIC 9(09).
          05 S72-PLA                     PIC X(12).     
          05 FILLER                      PIC X(1834). 
