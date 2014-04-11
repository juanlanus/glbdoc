      *PE-ESPECIAL.WS.v.01.0003.
      *----------------------------------------------------------------*
      *
      *    COPY  WORKING  PROCESOS ESPECIALES VERSION NOV/2004   
      *  
      ******************************************************************
REMARK* NEP0002   * ADICVG*ADICIONA TABLA DE VIGENTES 
REMARK* NOV.26/04 *                           
REMARK* NOEL PEREZ*               
      ******************************************************************
      *    COPY AREA DE ACUMULACION DE MORAS Y EVALUACION
      *----------------------------------------------------------------*
      * JAA0003     * RQ2728
      * JALBARRACIN * SE COMPLEMENTA LA TABLA DE CUENTAS PARA QUITAR LA
      * AGO.29.2011 * LA RESTRICCION DE CUENTAS > 29 REALES DEBIDO A
      *             * QUE ESTA RESTRICCION YA NO SE CUMPLE PARA ALGUNAS
      *             * CUENTAS
      *----------------------------------------------------------------*
       01  W-VARIABLES.
           03 W-IND                       PIC 9(02) VALUE ZEROS.
           03 W-MORAS-30-ACT              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-60-ACT              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-90-ACT              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-120-ACT             PIC 9(03) VALUE ZEROS.
           03 W-MORAS-30-HIS              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-60-HIS              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-90-HIS              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-120-HIS             PIC 9(03) VALUE ZEROS.
           03 W-ACU-CREDITO-DIA           PIC 9(03) VALUE ZEROS.
ADICVG     03 W-ACU-CTACTE-ACTIVAS        PIC 9(04) VALUE ZEROS.        NEP0002 
ADICVG     03 W-ACU-CTAS-CTES             PIC 9(04) VALUE ZEROS.        NEP0002 
ADICVG     03 W-ACU-CTAS-AHOR             PIC 9(04) VALUE ZEROS.        NEP0002 
           03 FIN-CHECK                   PIC 9(01) VALUE ZEROS.
           03 SALIR-END                   PIC 9(01) VALUE ZEROS.
           03 IND-ENDEUDA                 PIC 9(03) VALUE ZEROS.
           03 IND-J                       PIC 9(02) VALUE ZEROS.
           03 SW-CODEUDOR                 PIC 9(01) VALUE ZEROS.     
           03 SW-CONSULTA                 PIC 9(01) VALUE ZEROS.
ADICVG     03 SW-END                      PIC 9(01) VALUE ZEROS.        NEP0002 
           03 EVALUA-FINAL                PIC 9(01) VALUE ZEROS.
           03 EVALUA-CREDITO              PIC 9(01) VALUE ZEROS.
           03 EVALUA-ENDEUDA              PIC 9(01) VALUE ZEROS.
           03 EVALUA-CTA-CTE              PIC 9(01) VALUE ZEROS.
           03 W-SUSCRIPTOR                PIC 9(06) VALUE ZEROS.
           03 W-SUSCRIPTOR-R REDEFINES  W-SUSCRIPTOR.
              05 W-CODIGO-SUSCRIPTORES.
                 07 W-CODIGO-TIPOCTA      PIC 9(02).
                 07 W-CODIGO-RESTCTA      PIC 9(04).
           03 TEM-NRO-CTA.
              05 TEM-NRO-CTA-C16          PIC X(16) VALUE ZEROS.
              05 TEM-NRO-CTA-C17          PIC X(01) VALUE ZEROS.
              05 TEM-NRO-CTA-C18          PIC X(01) VALUE ZEROS.
      *---------------------------------------------------------*
           03 W-TABLA-SUS-CONSU.          
              05 W-SUS-CONSU      OCCURS  50 TIMES PIC 9(06).

      *---------------------------------------------------------*        
ADICVG     03 W-TABLA-END-MAYOR.                                        NEP0002 
ADICVG        05 W-TRIMESTRE-END  OCCURS   3 TIMES.                     NEP0002 
ADICVG           07 W-FECHA-END           PIC 9(08).                    NEP0002 
ADICVG           07 W-MAYOR-END           PIC X(01).                    NEP0002 

      *----------------------------------------------------------------*
      *               AREA WORK CAUSALES DE RECHAZO  
      *----------------------------------------------------------------*
           03 W-VECTOR-COMPORTAMIENTO.
              05 V-CTA-CTE-MM             PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-CTA-EMBARGA            PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-DUD-RECAUDO            PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-CAR-CASTIGA            PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-CTA-COBRADOR           PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-120               PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-90                PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-60                PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-30                PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-TDC-CAN-MM             PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-CAR-RECUPERA           PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-HIS-120           PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-HIS-90            PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-HIS-60            PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-MORA-HIS-30            PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-ENDEUDA                PIC X(01) VALUE SPACES.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-NUM-CONSULTAS          PIC 9(03) VALUE ZEROS.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-EDAD                   PIC X(03) VALUE SPACES.
              05 FILLER                   PIC X(01) VALUE ";".
              05 V-SCORE                  PIC 9(03) VALUE ZEROS.
      *------------------------------------------------------------*
      *     AREA WORK ACUMULADORES PARA GRAFICAS
      *------------------------------------------------------------*
       01  W-ACUMULADORES-ESTA.
           03 W-APROBADOS                 PIC 9(07) VALUE ZEROS.
           03 W-ESTUDIO                   PIC 9(07) VALUE ZEROS.
           03 W-RECHAZADOS                PIC 9(07) VALUE ZEROS.
           03 W-SIN-INFORMACION           PIC 9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION POR TIPO DE IDENTIFICACION
      *------------------------------------------------------------*
           03 W-GENERO-CALIF        OCCURS  4  TIMES.
              05 W-TABLA-TIPO-ID    OCCURS  5 TIMES.
                 07 W-ACUM-TIPO-ID      PIC 9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION POR RANGO DE EDAD                
      *------------------------------------------------------------*
           03 W-RANGO-CALIF     OCCURS  4  TIMES.
              05 W-TABLA-RANGO  OCCURS  7 TIMES.
                 07 W-ACUM-RANGO-EDAD   PIC 9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION ESTADO ACTUAL                     
      *------------------------------------------------------------*
           03 W-ACUAL-CALIF     OCCURS  3  TIMES.
              05 W-TBL-ESTADO-ACT  OCCURS  10 TIMES.
                 07 W-ACUM-ESTA-ACT    PIC 9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION ESTADO HISTORICO                  
      *------------------------------------------------------------*
           03 W-HISTO-CALIF     OCCURS  3  TIMES.
              05 W-TBL-ESTADO-HIS  OCCURS  8 TIMES.
                 07 W-ACUM-ESTA-HIS    PIC 9(07) VALUE ZEROS.
      *------------------------------------------------------------*
           03 W-EDITOR-APR                PIC Z,ZZZ,ZZ9.
           03 W-EDITOR-EST                PIC Z,ZZZ,ZZ9.
           03 W-EDITOR-REC                PIC Z,ZZZ,ZZ9.
           03 W-EDITOR-SIN                PIC Z,ZZZ,ZZ9.

      *----------------------------------------------------------------*
      *     VARIABLES DEL MANEJO DE BDACUE                             *
      *----------------------------------------------------------------*
       01  VAL-VARIABLES.
           03  W-CTA-BDCUE                 PIC 9(02) VALUE ZEROS.
           03  SW-LOAD-TBL                 PIC 9(01) VALUE ZEROS.
           03  T                           PIC 9(04) VALUE ZEROS.
           03  TOPE-T                      PIC 9(04) VALUE ZEROS.
           03  TABLA-VAL-TIP-CTA OCCURS 100 TIMES.                    
               04 VEC-VALOR-BDACUE OCCURS 15 TIMES PIC 9.             

      *----------------------------------------------------------------*
      * Estas son las variables para analizar los vectores de          *
      * comportamiento de moras HISTORICAS (estado-mes-abd07) y        *
      * (vector-2-abd07)                                               *
      *----------------------------------------------------------------*
       01  WS-ESTADO-MES-1                PIC 9(12).
       01  WS-ESTADO-MES-2                PIC 9(12).
       01  WS-ESTADO-MES                  PIC 9(12).
       01  WS-ESTADO-MES-R REDEFINES WS-ESTADO-MES.
           03  WS-MES                     PIC X OCCURS 12 TIMES.
       01  ESTADO-MES-IMP.
           03  E-MES-IMP                  PIC X OCCURS 12 TIMES.

       01  ESTADO-ANUAL.
           03  CHAR-H                     PIC X OCCURS 12 TIMES.
       01  F                              PIC 9(3)   VALUE ZERO.
       01  I                              PIC 9(3)   VALUE ZERO.
       01  J                              PIC 9(3)   VALUE ZERO.
       01  R                              PIC 9(3)   VALUE ZERO.
       01  E                              PIC 9(3)   VALUE ZERO.
       01  K                              PIC 9(4)   VALUE 01.
       01  N                              PIC 9(3)   VALUE ZERO.
       01  TIV                            PIC 9(3)   VALUE ZERO.
       01  TFV                            PIC 9(3)   VALUE ZERO.
       01  F-AUX-1                        PIC 9(4)   VALUE ZERO.
       01  F-AUX                          PIC 9(4)   VALUE ZERO.
       01  F-UACT-2                       PIC 9(4)   VALUE ZERO.
       01  F-APE-7                        PIC 9(4)   VALUE ZERO.
       01  F-ING-2                        PIC 9(4)   VALUE ZERO.
       01  F-NOV-7                        PIC 9(4)   VALUE ZERO.
       01  F-VIEJ                         PIC 9(4)   VALUE ZERO.
       01  ENCONTRO                       PIC 9(1)   VALUE ZERO.
       01  RELLENO                        PIC 9(1)   VALUE ZERO.
       01  CINCO                          PIC 9(2)   VALUE 5.
       01  NUEVO                          PIC 9      VALUE 0.
       01  COD-SUSCRIP-AUX                PIC 9(6)   VALUE ZERO.
      *------------------------------------------------------------*
       01  VECTOR-S.              
           03  VECTOR-A-R1                        PIC X(6).
           03  VECTOR-A-R2                        PIC X(6).
       01  VECTOR-S-12.           
           03  VECTOR-N-R1                        PIC X(6).
           03  VECTOR-N-R2                        PIC X(6).
      *------------------------------------------------------------*
       01  SALIDA-48.
           05 S48-EST                     PIC X(15).
           05 S48-SUS                     PIC X(14).
           05 S48-FEC                     PIC 9(6).
           05 S48-FECALPHA REDEFINES S48-FEC    PIC X(6).
           05 S48-AVA                     PIC X(9).
           05 S48-VEC                     PIC X(12).
           05 S48-VEC-12                  PIC X(12).
      *------------------------------------------------------------*
       01  F-APE-07                      PIC 9999.
       01  F-APE-07-R                    REDEFINES F-APE-07.
           03  M-APE-07                  PIC 99.
           03  A-APE-07                  PIC 99.
      *------------------------------------------------------------*
       01  F-ING-02                      PIC 9999.
       01  F-ING-02-R                    REDEFINES F-ING-02.
           03  M-ING-02                  PIC 99.
           03  A-ING-02                  PIC 99.
       01  F-NOV-07                      PIC 9999.
       01  F-NOV-07-R                    REDEFINES F-NOV-07.
           03  M-NOV-07                  PIC 99.
           03  A-NOV-07                  PIC 99.
      *---------------------------------------------------------------*
       01  W-FECHA-APE.
           03  W-ANIO-APE                PIC 9(04).
           03  W-MES-APE                 PIC 9(02).
      *---------------------------------------------------------------*
       01  W-FECHA-HOY                   PIC X(08).
       01  W-FECHA-HOY-R                 REDEFINES W-FECHA-HOY.
           03  W-ANIO-HOY                PIC 9(04).
           03  W-MES-HOY                 PIC 9(02).
           03  W-DIA-HOY                 PIC 9(02).
       01  W-FECHA-MILENIO               PIC 9(08).
       01  W-FECHA-MILENIO-R             REDEFINES W-FECHA-MILENIO.
           03  W-FMILENIO-SSAA           PIC 9(04).
           03  W-FMILENIO-SSAA-R         REDEFINES W-FMILENIO-SSAA.
               05  W-FMILENIO-SIGLO      PIC 9(02).
               05  W-FMILENIO-ANIO       PIC 9(02).
           03  W-FMILENIO-MES            PIC 9(02).
           03  W-FMILENIO-DIA            PIC 9(02).
       01  CAL-NRO-MESES                 PIC S9(05).
      *---------------------------------------------------------------*
       01  F-UACT-02                     PIC 9999.
       01  F-UACT-02-R                   REDEFINES F-UACT-02.
           03  M-UACT-02                 PIC 99.
           03  A-UACT-02                 PIC 99.
       01  WS-EST-MES.
           03  WS-MES-NUEV               PIC 9.
           03  WS-MESES-VIEJ             PIC 9(12).
       01  WS-EST-MES-R REDEFINES WS-EST-MES.
           03  WS-MESES-NUEV             PIC 9(12).
           03  WS-MES-VIEJ               PIC 9.
       01  FN.
           03  FEC-NOV-S                 PIC 9(4)   VALUE ZERO.
           03  FEC-NOV-RED-S REDEFINES FEC-NOV-S
                                         PIC X(04).
       01 ANO-2000                       PIC 99  VALUE 0.    
       01  SCREEN-HEADER.
           03  FILLER                    PIC X(30) VALUE SPACES.
           03  FILLER                    PIC X(20) VALUE
               "=== DATA CREDITO ===" .
           03  FILLER                    PIC X(30) VALUE SPACES.
       01  SCREEN-TITLE REDEFINES SCREEN-HEADER PIC X(80).
ELIMIN*01  MULT-TABLA OCCURS 200 TIMES.
ELIMIN*    03 MULT-CODIGO                PIC 9(6).
ELIMIN*    03 MULT-CLAVE                 PIC X(5).
ELIMIN*    03 MULT-TERMINAL              PIC 9(4).
ELIMIN*    03 MULT-PRODUCTO              PIC 9(2).
ELIMIN*    03 MULT-MODALIDAD             PIC 9(2).
ELIMIN*    03 MULT-ESTADO                PIC 9.
ELIMIN*    03 MULT-TTY                   PIC X(8).                              
ELIMIN*    03 MULT-SESION                PIC X(4) COMP-5.                      
ELIMIN*    03 MULT-PRIV-CLAVE            PIC 9(1).
ELIMIN*    03 MULT-PRO-EXCLU             PIC 9(1).
ELIMIN*    03 MULT-NOM-SUS               PIC X(14).
ELIMIN*    03 MULT-NIT                   PIC 9(11) COMP-3.
       01  NOM-COR-AUX             PIC X(14) VALUE SPACES.
       01  FILLER REDEFINES NOM-COR-AUX.
            02 NOM-COR-AUXN        PIC 9(6).
            02 FILLER              PIC X(8).
       01  NOM-COR-AUX2            PIC X(14) VALUE SPACES.
ELIMIN*01  MENSAJE-CS.
ELIMIN*   02 HEADER.
ELIMIN*      03 HEADER-USUARIO           PIC X(10).
ELIMIN*      03 HEADER-TIPO-MENSAJE      PIC 9.
ELIMIN*      03 HEADER-LSN               PIC 9(4).
ELIMIN*      03 HEADER-LONGITUD          PIC 9(4).
ELIMIN*      03 HEADER-TERMINAL          PIC X(8).
ELIMIN*      03 HEADER-NOMB-PROG         PIC X(6).
ELIMIN*      03 HEADER-FILA              PIC 99.
ELIMIN*      03 HEADER-COLUMNA           PIC 99.
ELIMIN*      03 HEADER-CARACT-INICIO     PIC 9.
ELIMIN*      03 HEADER-CARACT-FIN        PIC 9.
ELIMIN*      03 HEADER-TIPO-TERMINAL     PIC X(6).
ELIMIN*      03 HEADER-FILLER            PIC X(5).
ELIMIN*   02 TEXTO                       PIC X(2000). 
ELIMIN*   02 TEXTO-INPUT REDEFINES TEXTO.
ELIMIN*      03 AREA-INPUT               PIC X(80).
ELIMIN*      03 FILLER                   PIC X(1920).
      *-----------------------------------------------------------*
       01  TABLA-HISTORICA-MORAS-1. 
           03 W-MORAS-ANO1             PIC  X(12) VALUE ZEROS.
           03 W-MORAS-ANO1-R REDEFINES W-MORAS-ANO1.
              05 W-MORAS-ANO-1 OCCURS 12 TIMES PIC  X(01).
           03 W-MORAS-ANO2             PIC  X(12) VALUE ZEROS.
           03 W-MORAS-ANO2-R REDEFINES W-MORAS-ANO2.
              05 W-MORAS-ANO-2 OCCURS 12 TIMES PIC  X(01).
       
      *-----------------------------------------------------------*
       01  TABLA-HISTORICA-MORAS.
           03 W-MORAS-HISTORICAS-V     PIC 9(12) VALUE ZEROS.
           03 W-MORAS-HISTORICAS-V-R REDEFINES W-MORAS-HISTORICAS-V.
              05 W-MORAS-HISTORICAS OCCURS 12 TIMES PIC 9(01).

       01  w-msup-ind-bloq-n           PIC 9(01) VALUE zeros.
       01  w-msup-ind-bloq-x redefines w-msup-ind-bloq-n
                                       PIC x(01).
      *------------------------------------------------------------*
       01  ARRASTRE            PIC 9 VALUE ZERO.
      *------------------------------------------------------------*

ADICVG 01  TABLA-VIGENTES.                                              NEP0002 
ADICVG     03  TABLA-VIGENTES-CARTERA.                                  NEP0002 
ADICVG         05 FILLER        PIC X(10)   VALUE  "1000000000".        NEP0002 
ADICVG         05 FILLER        PIC X(10)   VALUE  "0011111111".        NEP0002 
ADICVG         05 FILLER        PIC X(10)   VALUE  "1111111111".        NEP0002 
ADICVG         05 FILLER        PIC X(10)   VALUE  "1111111111".        NEP0002 
ADICVG         05 FILLER        PIC X(10)   VALUE  "1111101101".        NEP0002 
ADICVG     03  TABLA-VIGENTES-CARTERA-R REDEFINES                       NEP0002 
ADICVG         TABLA-VIGENTES-CARTERA.                                  NEP0002 
ADICVG         05 TAB-VIGENTES-CARTERA   OCCURS 50 TIMES  PIC 9(01).    NEP0002 
ADICVG*------------------------------------------------------------*    NEP0002 
ADICVG 01  TABLA-TIPO-CARTERA.                                          NEP0002 
ADICVG     03 W-TIPO-CARTERA.                                           NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0101000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0201000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0301000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0402000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0501000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0603000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0701000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0802000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 0902000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1002000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1102000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1202000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1301000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1401000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1502000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1602000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1701000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1801000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 1902000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2001000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2102000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2202000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2302000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2402000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2501000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2602000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2703000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2802000.                       NEP0002 
ADICVG        05 FILLER  PIC 9(07) VALUE 2902000.                       NEP0002 
RA2728        05 FILLER  PIC 9(07) VALUE 3002000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3102000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3202000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3302000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3402000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3502000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3602000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3702000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3802000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 3902000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4002000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4102000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4202000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4302000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4402000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4502000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4602000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4702000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4801000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 4901000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5002000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5101000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5202000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5302000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5402000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5502000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5602000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5702000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5802000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 5902000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6002000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6102000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6202000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6302000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6402000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6502000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6602000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6702000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6802000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 6902000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7002000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7102000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7202000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7302000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7402000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7502000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7602000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7702000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7802000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 7902000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8002000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8102000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8202000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8302000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8402000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8502000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8602000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8702000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8802000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 8902000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9002000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9102000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9202000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9302000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9402000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9502000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9602000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9702000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9802000.                       JAA0003 
RA2728        05 FILLER  PIC 9(07) VALUE 9902000.                       JAA0003 
ADICVG     03 W-TIPO-CARTERA-R REDEFINES W-TIPO-CARTERA.                NEP0002 
RQ2728        05 W-TIPO-CART OCCURS 99 TIMES.                           JAA0003 
ADICVG           07 W-CART-CONSEC          PIC 9(02).                   NEP0002 
ADICVG           07 W-CART-TIPO            PIC 9(02).                   NEP0002 
ADICVG           07 W-FLAG-EVALUA-SN       PIC 9(01).                   NEP0002 
ADICVG           07 W-FLAG-MESES-CONTEM    PIC 9(02).                   NEP0002 
      *------------------------------------------------------------*
