REMARK*SCOTCAR-WS.0008
REMARK**********************************************************************
REMARK*VARIABLES PARA CARACTERISTICAS
REMARK**********************************************************************
REMARK* HGO0001     * ICMVLR - AJUSTE DE TABLA DE SALARIO MINIMO A     *
REMARK* H. GARCIA   *          INDEXADA                                *
REMARK* JUNIO/2009  *                                                  *
REMARK* INTERSCHEMES*                                                  *
REMARK******************************************************************
REMARK* RMB0003     * RUNDAT - SE AJUSTA EL PROCESO PARA CONTEMPLAR    *
REMARK* R. MARTINEZ *          LA DESACTUALIZACION QUE EXISTE AL       *
REMARK* AGO/17/09   *          USAR LA RUTINA EN LINEA                 *
REMARK* DATACREDITO * SCOUHD - CARACTERISTICA ESPECIAL PARA PREDIC HD  *
REMARK*             * LEY001 - VARIABLES AUXILIARES PARA LA LEY        *
REMARK*             * MOR001 - ARREGLO POSICION 32 DEL VECTOR DE MORAS *
REMARK******************************************************************
REMARK* RMB0004     * CARA01 - SE IMPLEMENTAN LAS CARACTERISTICAS      *
REMARK* R. MARTINEZ *          00DEM026 - ESTRATO SOCIOECONOMICO       *
REMARK* SEP-30-2010 *          00DEM030 - ACTIVIDAD ECONOMICA          *
REMARK******************************************************************
REMARK* RMB0005     * SUP001 - SE ADICIONA PARAMETRO PARA LAS CARACTER *
REMARK* R. MARTINEZ *          ISTICAS DE ENDEUDAMIENTO GLOBAL         *
REMARK* OCT-30-2011 *                                                  *
REMARK******************************************************************
REMARK* RMB0006     * ACIEAA - SE IMPLEMENTAN LAS CARACTERISTICAS      *
REMARK* R. MARTINEZ *          PARA ACIERTA A .                        *
REMARK* JUN-01-2012 *          00DEM,01DEM,01END,00INQ,01INQ           *
REMARK******************************************************************
REMARK* RMB0007     * ACIEA1 - SE ACTUALIZAN  LAS CARACTERISTICAS      *
REMARK* R. MARTINEZ *          PARA ACIERTA AA.                        *
REMARK* AGO-20-2012 * DECI01 - PARAMETRO PAR VER/NOVER     FRENADAS    *
REMARK******************************************************************
REMARK* RMB0008     * PORT01 - SE IMPLEMENTAN UN NUEVA CLASIFICACION DE*
REMARK* R. MARTINEZ *          PORTAFOLIO PARA MICROCREDITO            *
REMARK* MAR-20-2013 * FALL01 - PARA LA CARACTERISTICA DEM02 SE CONTROLA*
REMARK*             *          QUE NO APLIQUE SI ES CODEUDOR           *
REMARK******************************************************************
LEY001 01 LEY-CARACT.                                                   RMB0003 
LEY001    02 AMNISTIA21-CARACT          PIC 9.                          RMB0003 
LEY001    02 AMNISTIA21-ART13-CARACT    PIC 9.                          RMB0003 
       01 VAR-CARACT.
ACIEA1    03 SUS-BLO-CARACT             PIC 9.                          RMB0007
DECI01    03 SIN-FRENADAS-CARACT        PIC 9(1) VALUE 0.               RMB0007 
ACIEAA    03 NIT-RELACION-CARACT        PIC 9(11).                      RMB0006
PORT01    03 RUTINA-PORTAFOLIO-CARACT   PIC X(03) VALUE "DEF".          RMB0008
CARA01    03 ULTEST-CARACT              PIC X(01).                      RMB0004
          03 TIPO-ID-CARACT             PIC 9(01).
          03 NRO-ID-CARACT              PIC 9(11).
          03 SW-LECTURA-CARACT          PIC 9(01).
          03 SW-LEESUSC-CARACT          PIC 9(01).
          03 SW-DET00NUM-CARACT         PIC 9(01).
          03 SW-VECMORAS-CARACT         PIC 9(01).
          03 COD-PORTAF-CARACT          PIC X(02).
          03 IND-PORTAF-CARACT          PIC 9(02).
          03 GEN-NUM-CARACT             PIC 9(01) VALUE 0.
          03 GEN-INQ-CARACT             PIC 9(01) VALUE 0.
          03 GEN-MOR-CARACT             PIC 9(01) VALUE 0.
          03 GEN-EXP-CARACT             PIC 9(01) VALUE 0.
          03 GEN-DEM-CARACT             PIC 9(01) VALUE 0.
          03 GEN-ACP-CARACT             PIC 9(01) VALUE 0.
          03 GEN-END-CARACT             PIC 9(01) VALUE 0.
SUP001    03 GEN-SUP-CARACT             PIC 9(01) VALUE 0.              RMB0005
          03 FECHA-RUN-CARACT           PIC 9(06).
          03 FILLER REDEFINES FECHA-RUN-CARACT.
             04 ANO-RUN-CARACT          PIC 9(04).
             04 MES-RUN-CARACT          PIC 9(02).
          03 CURRENT-DATE-CARACT        PIC 9(08).
          03 FILLER REDEFINES CURRENT-DATE-CARACT.
             04 ANO-CURRENT-DATE-CARACT PIC 9(04).
             04 MES-CURRENT-DATE-CARACT PIC 9(02).
             04 DIA-CURRENT-DATE-CARACT PIC 9(02).
RUNDAT    03 ACT-DATE-CARACT            PIC 9(06).                      RMB0003
RUNDAT    03 FILLER REDEFINES ACT-DATE-CARACT.                          RMB0003
RUNDAT       04 ANO-ACT-DATE-CARACT     PIC 9(04).                      RMB0003
RUNDAT       04 MES-ACT-DATE-CARACT     PIC 9(02).                      RMB0003
RUNDAT    03 RUN-DATE-AUX-CARACT        PIC 9(06).                      RMB0003
RUNDAT    03 I49                        PIC 9(02).                      RMB0003
RUNDAT    03 I13                        PIC 9(02).                      RMB0003
RUNDAT    03 I12                        PIC 9(02).                      RMB0003
RUNDAT    03 I10                        PIC 9(02).                      RMB0003
          03 RUN-DATE-CARACT            PIC 9(06).
          03 FILLER REDEFINES RUN-DATE-CARACT.
             04 ANO-RUN-DATE-CARACT     PIC 9(04).
             04 MES-RUN-DATE-CARACT     PIC 9(02).
          03 FECHA-RUN-SANATA-CARACT    PIC 9(06).
          03 FILLER REDEFINES FECHA-RUN-SANATA-CARACT.
             04 ANO-RUN-SANATA-CARACT   PIC 9(04).
             04 MES-RUN-SANATA-CARACT   PIC 9(02).
          03 FECHA-NOV24-CARACT         PIC 9(06).
          03 FILLER REDEFINES FECHA-NOV24-CARACT.
             04 ANO-NOV24-CARACT        PIC 9(04).
             04 MES-NOV24-CARACT        PIC 9(02).
          03 FECHA-NOV48-CARACT         PIC 9(06).
          03 FILLER REDEFINES FECHA-NOV48-CARACT.
             04 ANO-NOV48-CARACT        PIC 9(04).
             04 MES-NOV48-CARACT        PIC 9(02).
          03 SW-RUN-DATE-CARACT         PIC 9(01) VALUE ZERO.
          03 IND-E-CARACT               PIC 9(03) VALUE ZERO.
          03 IND-S-CARACT               PIC 9(03) VALUE ZERO.
          03 OPEN-ICMCRE-HIST-CARACT    PIC 9(01) VALUE ZERO.

       01 VAR-TMP-CARACT.
          03 REG-ABIERTO-CARACT         PIC 9(01).
          03 REG-CERRADO-CARACT         PIC 9(01).
          03 REG-POSITIVO-CARACT        PIC 9(01). 
          03 REG-NEGATIVO-CARACT        PIC 9(01). 
          03 FECHA-NOV-CARACT           PIC 9(04).
          03 FECHA-APE-CARACT           PIC 9(04).
          03 FECHA-VENC-CARACT          PIC 9(04).
          03 FECHA-SUSC-CARACT          PIC 9(04).  
          03 FECHA-CONS-CARACT          PIC 9(06).
          03 FECHA-ICMVAL-CARACT        PIC 9(06).
          03 FILLER REDEFINES FECHA-ICMVAL-CARACT.
             04 SIG-ANO-ICMVAL-CARACT   PIC 9(04).
             04 MES-ICMVAL-CARACT       PIC 9(02).
          03 FECHA-ESTADO-CARACT        PIC 9(06).
          03 FILLER REDEFINES FECHA-ESTADO-CARACT.
             04 SIG-ANO-NOV-CARACT      PIC 9(04).
             04 FILLER REDEFINES SIG-ANO-NOV-CARACT.
                05 SIG-NOV-CARACT       PIC 9(02).
                05 ANO-NOV-CARACT       PIC 9(02).
             04 MES-NOV-CARACT          PIC 9(02).
          03 FECHA-APERT-CARACT         PIC 9(06).
          03 FILLER REDEFINES FECHA-APERT-CARACT.
             04 SIG-ANO-APE-CARACT      PIC 9(04).
             04 FILLER REDEFINES SIG-ANO-APE-CARACT.
                05 SIG-APE-CARACT       PIC 9(02).
                05 ANO-APE-CARACT       PIC 9(02).
             04 MES-APE-CARACT          PIC 9(02).
          03 FECHA-VENCE-CARACT         PIC 9(06).
          03 FILLER REDEFINES FECHA-VENCE-CARACT.
             04 SIG-ANO-VENC-CARACT     PIC 9(04).
             04 FILLER REDEFINES SIG-ANO-VENC-CARACT.
                05 SIG-VENC-CARACT      PIC 9(02).
                05 ANO-VENC-CARACT      PIC 9(02).
             04 MES-VENC-CARACT         PIC 9(02).
          03 FECHA-AUX-CARACT           PIC 9(06).
          03 FILLER REDEFINES FECHA-AUX-CARACT.
             04 MES-AUX-CARACT          PIC 9(02).
             04 ANO-AUX-CARACT          PIC 9(04).
          03 FECHA-SIT-CARACT           PIC 9(06).
          03 FILLER REDEFINES FECHA-SIT-CARACT.
             04 ANO-SIT-CARACT          PIC 9(04).
             04 MES-SIT-CARACT          PIC 9(02).
          03 FECHA-VAL-CARACT           PIC 9(06).
          03 FILLER REDEFINES FECHA-VAL-CARACT.
             04 ANO-VAL-CARACT          PIC 9(04).
             04 MES-VAL-CARACT          PIC 9(02).
          03 FECHA-CONSULT-CARACT       PIC 9(06).
          03 FILLER REDEFINES FECHA-CONSULT-CARACT.
             04 SIG-ANO-CONS-CARACT     PIC 9(04).
             04 FILLER REDEFINES SIG-ANO-CONS-CARACT.
                05 SIG-CONS-CARACT      PIC 9(02).
                05 ANO-CONS-CARACT      PIC 9(02).
             04 MES-CONS-CARACT         PIC 9(02).
          03 FECHA-EXP-CARACT           PIC 9(08).
          03 FILLER REDEFINES FECHA-EXP-CARACT.
             04 ANO-EXP-CARACT          PIC 9(04).
             04 MES-EXP-CARACT          PIC 9(02).
             04 DIA-EXP-CARACT          PIC 9(02).
          03 FECHA-INGSUS-CARACT        PIC 9(06).
          03 FILLER REDEFINES FECHA-INGSUS-CARACT.
             04 SIG-ANO-INGSUS-CARACT   PIC 9(04).
             04 FILLER REDEFINES SIG-ANO-INGSUS-CARACT.
                05 SIG-INGSUS-CARACT    PIC 9(02).
                05 ANO-INGSUS-CARACT    PIC 9(02).
             04 MES-INGSUS-CARACT       PIC 9(02).
          03 FECHA-SMLMV-CARACT         PIC 9(06).
          03 FILLER REDEFINES FECHA-SMLMV-CARACT.
             04 ANO-NOV-SMLMV-CARACT    PIC 9(04).
             04 MES-NOV-SMLMV-CARACT    PIC 9(02).
          03 MESES-DESAC-CARACT         PIC S9(05).
RUNDAT    03 ACT-MESES-DESAC-CARACT     PIC S9(05).                     RMB0003
          03 MESES-MADUR-CARACT         PIC S9(05).
          03 MESES-CONST-CARACT         PIC S9(05).
          03 MESES-NOVAP-CARACT         PIC 9(05).
          03 MESES-VENCE-CARACT         PIC 9(05).
          03 MESES-VENAP-CARACT         PIC S9(05).
          03 VAL-MADUR-CARACT           PIC 9(03)V99.
          03 VAL-RATIO-CARACT           PIC 9(03)V99.
          03 EXP-CARACT                 PIC 9(04)V99999999.
          03 SUMA-CARACT                PIC 9(04)V99999999.
          03 VAL-00MOR-CARACT           PIC 999.99.
          03 VAL-01MOR-CARACT           PIC 999.99.
          03 SUMA-V-CARACT              PIC 9(10)V999.
          03 CONT-V-CARACT              PIC 9(05).
          03 TOLERANCIA-CARACT          PIC 9(03).
          03 MFLAG-CARACT               PIC 9(02).
          03 NFLAG-CARACT               PIC 9(02).
          03 NONUM-CARACT               PIC 9(02).
          03 MORA-CARACT                PIC 9(01).
          03 I-CARACT                   PIC 9(04).
          03 J-CARACT                   PIC 9(02).
          03 K-CARACT                   PIC 9(02).
          03 L-CARACT                   PIC 9(02).
          03 P-CARACT                   PIC 9(02).
          03 R-CARACT                   PIC 9(02).
          03 V-CARACT                   PIC 9(02).
          03 Z-CARACT                   PIC 9(02).
          03 J-C-CARACT                 PIC 9(04).
          03 X-C-CARACT                 PIC 9(04).
          03 V-PONDER-CARACT OCCURS 49  TIMES PIC 9.
          03 V-PON01M-CARACT OCCURS 25  TIMES PIC 9.
          03 MAX-CARACT                 PIC 9(01)V99999999999999.
          03 ADJETIVO-CARACT            PIC 9.
FALL01    03 GARANTE-CARACT             PIC 99.                         RMB0008
          03 DEM-ERR-CARACT             PIC 9.
          03 END-ERR-CARACT             PIC 9.
          03 FIN-ICSALMIN-CARACT        PIC 9.
          03 FIN-ICSANATA-CARACT        PIC 9.
          03 SW-NORMALIZ-VECTVAL        PIC 9.
          03 SW-EMPALMA-VEC-CAR         PIC 9.
          03 TMP-VAL-CARACT             PIC 9(008)V99.
          03 TMP-SUMA-EXP-CARACT        PIC 9(04).

********** VARIABLES PARA NORMALIZAR VECTOR 49 POSICIONES
        01 VARIABLES-NORMALIZA-CARACT.
          02 FECHA-NOVEDAD07-N-CARACT    PIC 9(6).
          02 FILLER REDEFINES FECHA-NOVEDAD07-N-CARACT.
             03 SIG-ANO-NOV-N-CARACT     PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-NOV-N-CARACT.
                04 SIG-NOV-N-CARACT      PIC 9(2).
                04 ANO-NOV-N-CARACT      PIC 9(2).
             03 MES-NOV-N-CARACT         PIC 9(2).
          02 FECHA-NOVEDAD07-HIS-CARACT  PIC 9(6).
          02 FILLER REDEFINES FECHA-NOVEDAD07-HIS-CARACT.
             03 SIG-ANO-NOV-HIS-CARACT   PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-NOV-HIS-CARACT.
                04 SIG-NOV-HIS-CARACT    PIC 9(2).
                04 ANO-NOV-HIS-CARACT    PIC 9(2).
             03 MES-NOV-HIS-CARACT       PIC 9(2).
          02 FECHA-INGRESO-N-CARACT      PIC 9(6).
          02 FILLER REDEFINES FECHA-INGRESO-N-CARACT.
             03 SIG-ANO-ING-N-CARACT     PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-ING-N-CARACT.
                04 SIG-ING-N-CARACT      PIC 9(2).
                04 ANO-ING-N-CARACT      PIC 9(2).
             03 MES-ING-N-CARACT         PIC 9(2).
          02 FECHA-SITMMAAAA-ORI-CARACT  PIC 9(6).
          02 FILLER REDEFINES FECHA-SITMMAAAA-ORI-CARACT.
             03 FECHA-SITMM-ORI-CARACT   PIC 9(2).
             03 FECHA-SITAAAA-ORI-CARACT PIC 9(4).
          02 FECHA-SITAAAAMM-CARACT      PIC 9(6).
          02 FILLER REDEFINES FECHA-SITAAAAMM-CARACT.
              03 FECHA-SITAAAA-CARACT    PIC 9(4).
              03 FECHA-SITMM-CARACT      PIC 9(2).
          02 FECHA-VALAAAAMM-CARACT      PIC 9(6).
          02 FILLER REDEFINES FECHA-VALAAAAMM-CARACT.
              03 FECHA-VALAAAA-CARACT    PIC 9(4).
              03 FECHA-VALMM-CARACT      PIC 9(2).
          02 FECHA-TAG-N-CARACT          PIC 9(6).
          02 FILLER REDEFINES FECHA-TAG-N-CARACT.
             03 SIG-ANO-TAG-N-CARACT     PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-TAG-N-CARACT.
                04 SIG-TAG-N-CARACT      PIC 9(2).
                04 ANO-TAG-N-CARACT      PIC 9(2).
             03 MES-TAG-N-CARACT         PIC 9(2).
          02 FECHA-APERTURA07-N-CARACT   PIC 9(6).
          02 FILLER REDEFINES FECHA-APERTURA07-N-CARACT.
             03 SIG-ANO-APE-N-CARACT     PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-APE-N-CARACT.
                04 SIG-APE-N-CARACT      PIC 9(2).
                04 ANO-APE-N-CARACT      PIC 9(2).
             03 MES-APE-N-CARACT         PIC 9(2).
          02 J-N-CARACT                  PIC 9(04).
          02 K-N-CARACT                  PIC 9(04).
          02 L-N-CARACT                  PIC 9(04).
          02 RELLENO-N-CARACT            PIC X(48).
          02 FECHA-AUX-N-CARACT          PIC 9(04).
     
        01 VEC-24-HIST-CARACT.
           02 V-NOR1-HIST-CARACT         PIC 9(12).
           02 V-NOR2-HIST-CARACT         PIC 9(12).
           

        01 VECTORES-W-CARACT.
          02 VECTOR-MORA-CARACT.
             03 FILLER PIC X(25) VALUE "5PMPPPPPPPPP5555123411212".
MOR001*      03 FILLER PIC X(25) VALUE "3234124412341234999CRD999".     RMB0003 
MOR001       03 FILLER PIC X(25) VALUE "3234123412341234999CRD999".     RMB0003 
          02 FILLER REDEFINES VECTOR-MORA-CARACT.
             03 V-MORA-CARACT OCCURS 50 TIMES PIC X.

        01 V-NOR-CARACT.
           02 V-NOR1-CARACT              PIC 9(12).
           02 V-NOR2-CARACT              PIC 9(12).
           02 V-NOR3-CARACT              PIC 9(12).
           02 V-NOR4-CARACT              PIC 9(12).
           02 V-NOV-ACT-CARACT           PIC 9(01).

       01 V-PONDERADO-CARACT.
          02 PTF-POND-CARACT OCCURS 9 TIMES.
             03 CMP-POND-CARACT OCCURS 24 TIMES.
                04 SW-CMP-CARACT        PIC 9(01).
                04 VCT-MMAX-CARACT OCCURS 25 TIMES PIC 9.

       01 V-SMLMV-CARACT.
ICMVLR*          02 VCT-SMLMV-CARACT OCCURS 100 TIMES.                  HGO0002
ICMVLR    02 VCT-SMLMV-CARACT OCCURS 100 ASCENDING KEY IS               HGO0002
ICMVLR       ANO-SMLMV-CARACT INDEXED BY J-SMLMV-CARACT.                HGO0002
             03 ANO-SMLMV-CARACT        PIC 9(04).
             03 VLR-SMLMV-CARACT        PIC 9(08).

       01 V-SANATA-CARACT.
          02 VCT-SANATA-CARACT OCCURS 500 TIMES.
             03 FCH-SANATA-CARACT       PIC 9(06).
             03 FILLER                  PIC X(01).
             03 NOM-SANATA-CARACT       PIC X(60).

       01 V-VALORES-CARACT.
          02 VEC-VALORES-CARACT OCCURS 13 TIMES. 
             03 VEC-CUOTA-CARACT        PIC 9(11).
             03 VEC-SALDO-CARACT        PIC 9(11).
             03 VEC-SALDOMOR-CARACT     PIC 9(11).
             03 VEC-INICIAL-CARACT      PIC 9(11).
             03 VEC-DISPON-CARACT       PIC 9(11).
             03 VEC-SMLMV-CARACT        PIC 9(08)V999.
             03 VEC-IND-CUO-CARACT      PIC 9.
             03 VEC-IND-SLD-CARACT      PIC 9.
             03 VEC-IND-SLDM-CARACT     PIC 9.
             03 VEC-IND-INI-CARACT      PIC 9.
             03 VEC-IND-DSP-CARACT      PIC 9.

       01 INICIAR-3-CARACT.
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".
          02 FILLER PIC X(30) VALUE " -1 -1 -1 -1 -1 -1 -1 -1 -1 -1".

       01 INICIAR-4-CARACT.
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".
          02 FILLER PIC X(32) VALUE "  -1  -1  -1  -1  -1  -1  -1  -1".

       01 INICIAR-6-CARACT.
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".
          02 FILLER PIC X(30) VALUE "    -1    -1    -1    -1    -1".

       01 INICIAR-8-CARACT.
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".
          02 FILLER PIC X(32) VALUE "      -1      -1      -1      -1".

       01 INICIAR-11-CARACT.
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".
         02 FILLER PIC X(33) VALUE "         -1         -1         -1".

       01 VAR-00NUM.
          03 00NUMLLV                   PIC X(038).
          03 00NUMPTF                   PIC X(002).
          03 00NUM001                   PIC 9(003).
          03 00NUM002                   PIC 9(003).
          03 00NUM003                   PIC 9(003).
          03 00NUM004                   PIC 9(003).
          03 00NUM005                   PIC 9(003).
          03 00NUM007                   PIC 9(003).
          03 00NUM008                   PIC 9(003).
          03 00NUM009                   PIC 9(003).
          03 00NUM010                   PIC 9(003).
          03 00NUM017                   PIC 9(003).
          03 00NUM018                   PIC 9(003).
          03 00NUM019                   PIC 9(003).
          03 00NUM022                   PIC 9(003).
          03 00NUM023                   PIC 9(003).
          03 00NUM024                   PIC 9(003).
          03 00NUM028                   PIC 9(003).
          03 00NUM029                   PIC 9(003).
          03 00NUM030                   PIC 9(003).
          03 00NUM031                   PIC 9(003).
          03 00NUM032                   PIC 9(003).
          03 00NUM033                   PIC 9(003).
          03 00NUM034                   PIC 9(003).
          03 00NUM035                   PIC 9(003).
          03 00NUM036                   PIC 9(003).
          03 00NUM037                   PIC 9(003).
          03 00NUM038                   PIC 9(003).
          03 00NUM039                   PIC 9(003).
          03 00NUM040                   PIC 9(003).
          03 00NUM041                   PIC 9(003).
          03 00NUM042                   PIC 9(003).
          03 00NUM043                   PIC 9(003).
          03 00NUM044                   PIC 9(003).
          03 00NUM045                   PIC 9(003).
          03 00NUM046                   PIC 9(003).
          03 00NUM047                   PIC 9(003).
          03 00NUM048                   PIC 9(003).
          03 00NUM049                   PIC 9(003).
          03 00NUM050                   PIC 9(003).
          03 00NUM066                   PIC 9(003).

       01 VARIABLES-00NUM.
          03 VAR-00NUMRO.
             05 00NUM001RO              PIC X(003).
             05 00NUM002RO              PIC X(003).
             05 00NUM003RO              PIC X(003).
             05 00NUM004RO              PIC X(003).
             05 00NUM005RO              PIC X(003).
             05 00NUM007RO              PIC X(003).
             05 00NUM008RO              PIC X(003).
             05 00NUM009RO              PIC X(003).
             05 00NUM010RO              PIC X(003).
             05 00NUM017RO              PIC X(003).
             05 00NUM018RO              PIC X(003).
             05 00NUM019RO              PIC X(003).
             05 00NUM022RO              PIC X(003).
             05 00NUM023RO              PIC X(003).
             05 00NUM024RO              PIC X(003).
             05 00NUM028RO              PIC X(003).
             05 00NUM029RO              PIC X(003).
             05 00NUM030RO              PIC X(003).
             05 00NUM031RO              PIC X(003).
             05 00NUM032RO              PIC X(003).
             05 00NUM033RO              PIC X(003).
             05 00NUM034RO              PIC X(003).
             05 00NUM035RO              PIC X(003).
             05 00NUM036RO              PIC X(003).
             05 00NUM037RO              PIC X(003).
             05 00NUM038RO              PIC X(003).
             05 00NUM039RO              PIC X(003).
             05 00NUM040RO              PIC X(003).
             05 00NUM041RO              PIC X(003).
             05 00NUM042RO              PIC X(003).
             05 00NUM043RO              PIC X(003).
             05 00NUM044RO              PIC X(003).
             05 00NUM045RO              PIC X(003).
             05 00NUM046RO              PIC X(003).
             05 00NUM047RO              PIC X(003).
             05 00NUM048RO              PIC X(003).
             05 00NUM049RO              PIC X(003).
             05 00NUM050RO              PIC X(003).
             05 00NUM066RO              PIC X(003).

          03 VAR-00NUMIN.
             05 00NUM001IN              PIC X(003).
             05 00NUM002IN              PIC X(003).
             05 00NUM003IN              PIC X(003).
             05 00NUM004IN              PIC X(003).
             05 00NUM005IN              PIC X(003).
             05 00NUM007IN              PIC X(003).
             05 00NUM008IN              PIC X(003).
             05 00NUM009IN              PIC X(003).
             05 00NUM010IN              PIC X(003).
             05 00NUM017IN              PIC X(003).
             05 00NUM018IN              PIC X(003).
             05 00NUM019IN              PIC X(003).
             05 00NUM022IN              PIC X(003).
             05 00NUM023IN              PIC X(003).
             05 00NUM024IN              PIC X(003).
             05 00NUM028IN              PIC X(003).
             05 00NUM029IN              PIC X(003).
             05 00NUM030IN              PIC X(003).
             05 00NUM031IN              PIC X(003).
             05 00NUM032IN              PIC X(003).
             05 00NUM033IN              PIC X(003).
             05 00NUM034IN              PIC X(003).
             05 00NUM035IN              PIC X(003).
             05 00NUM036IN              PIC X(003).
             05 00NUM037IN              PIC X(003).
             05 00NUM038IN              PIC X(003).
             05 00NUM039IN              PIC X(003).
             05 00NUM040IN              PIC X(003).
             05 00NUM041IN              PIC X(003).
             05 00NUM042IN              PIC X(003).
             05 00NUM043IN              PIC X(003).
             05 00NUM044IN              PIC X(003).
             05 00NUM045IN              PIC X(003).
             05 00NUM046IN              PIC X(003).
             05 00NUM047IN              PIC X(003).
             05 00NUM048IN              PIC X(003).
             05 00NUM049IN              PIC X(003).
             05 00NUM050IN              PIC X(003).
             05 00NUM066IN              PIC X(003).

          03 VAR-00NUMVE.
             05 00NUM001VE              PIC X(003).
             05 00NUM002VE              PIC X(003).
             05 00NUM003VE              PIC X(003).
             05 00NUM004VE              PIC X(003).
             05 00NUM005VE              PIC X(003).
             05 00NUM007VE              PIC X(003).
             05 00NUM008VE              PIC X(003).
             05 00NUM009VE              PIC X(003).
             05 00NUM010VE              PIC X(003).
             05 00NUM017VE              PIC X(003).
             05 00NUM018VE              PIC X(003).
             05 00NUM019VE              PIC X(003).
             05 00NUM022VE              PIC X(003).
             05 00NUM023VE              PIC X(003).
             05 00NUM024VE              PIC X(003).
             05 00NUM028VE              PIC X(003).
             05 00NUM029VE              PIC X(003).
             05 00NUM030VE              PIC X(003).
             05 00NUM031VE              PIC X(003).
             05 00NUM032VE              PIC X(003).
             05 00NUM033VE              PIC X(003).
             05 00NUM034VE              PIC X(003).
             05 00NUM035VE              PIC X(003).
             05 00NUM036VE              PIC X(003).
             05 00NUM037VE              PIC X(003).
             05 00NUM038VE              PIC X(003).
             05 00NUM039VE              PIC X(003).
             05 00NUM040VE              PIC X(003).
             05 00NUM041VE              PIC X(003).
             05 00NUM042VE              PIC X(003).
             05 00NUM043VE              PIC X(003).
             05 00NUM044VE              PIC X(003).
             05 00NUM045VE              PIC X(003).
             05 00NUM046VE              PIC X(003).
             05 00NUM047VE              PIC X(003).
             05 00NUM048VE              PIC X(003).
             05 00NUM049VE              PIC X(003).
             05 00NUM050VE              PIC X(003).
             05 00NUM066VE              PIC X(003).

          03 VAR-00NUMHP.
             05 00NUM001HP              PIC X(003).
             05 00NUM002HP              PIC X(003).
             05 00NUM003HP              PIC X(003).
             05 00NUM004HP              PIC X(003).
             05 00NUM005HP              PIC X(003).
             05 00NUM007HP              PIC X(003).
             05 00NUM008HP              PIC X(003).
             05 00NUM009HP              PIC X(003).
             05 00NUM010HP              PIC X(003).
             05 00NUM017HP              PIC X(003).
             05 00NUM018HP              PIC X(003).
             05 00NUM019HP              PIC X(003).
             05 00NUM022HP              PIC X(003).
             05 00NUM023HP              PIC X(003).
             05 00NUM024HP              PIC X(003).
             05 00NUM028HP              PIC X(003).
             05 00NUM029HP              PIC X(003).
             05 00NUM030HP              PIC X(003).
             05 00NUM031HP              PIC X(003).
             05 00NUM032HP              PIC X(003).
             05 00NUM033HP              PIC X(003).
             05 00NUM034HP              PIC X(003).
             05 00NUM035HP              PIC X(003).
             05 00NUM036HP              PIC X(003).
             05 00NUM037HP              PIC X(003).
             05 00NUM038HP              PIC X(003).
             05 00NUM039HP              PIC X(003).
             05 00NUM040HP              PIC X(003).
             05 00NUM041HP              PIC X(003).
             05 00NUM042HP              PIC X(003).
             05 00NUM043HP              PIC X(003).
             05 00NUM044HP              PIC X(003).
             05 00NUM045HP              PIC X(003).
             05 00NUM046HP              PIC X(003).
             05 00NUM047HP              PIC X(003).
             05 00NUM048HP              PIC X(003).
             05 00NUM049HP              PIC X(003).
             05 00NUM050HP              PIC X(003).
             05 00NUM066HP              PIC X(003).

          03 VAR-00NUMOT.
             05 00NUM001OT              PIC X(003).
             05 00NUM002OT              PIC X(003).
             05 00NUM003OT              PIC X(003).
             05 00NUM004OT              PIC X(003).
             05 00NUM005OT              PIC X(003).
             05 00NUM007OT              PIC X(003).
             05 00NUM008OT              PIC X(003).
             05 00NUM009OT              PIC X(003).
             05 00NUM010OT              PIC X(003).
             05 00NUM017OT              PIC X(003).
             05 00NUM018OT              PIC X(003).
             05 00NUM019OT              PIC X(003).
             05 00NUM022OT              PIC X(003).
             05 00NUM023OT              PIC X(003).
             05 00NUM024OT              PIC X(003).
             05 00NUM028OT              PIC X(003).
             05 00NUM029OT              PIC X(003).
             05 00NUM030OT              PIC X(003).
             05 00NUM031OT              PIC X(003).
             05 00NUM032OT              PIC X(003).
             05 00NUM033OT              PIC X(003).
             05 00NUM034OT              PIC X(003).
             05 00NUM035OT              PIC X(003).
             05 00NUM036OT              PIC X(003).
             05 00NUM037OT              PIC X(003).
             05 00NUM038OT              PIC X(003).
             05 00NUM039OT              PIC X(003).
             05 00NUM040OT              PIC X(003).
             05 00NUM041OT              PIC X(003).
             05 00NUM042OT              PIC X(003).
             05 00NUM043OT              PIC X(003).
             05 00NUM044OT              PIC X(003).
             05 00NUM045OT              PIC X(003).
             05 00NUM046OT              PIC X(003).
             05 00NUM047OT              PIC X(003).
             05 00NUM048OT              PIC X(003).
             05 00NUM049OT              PIC X(003).
             05 00NUM050OT              PIC X(003).
             05 00NUM066OT              PIC X(003).

          03 VAR-00NUMCC.
             05 00NUM001CC              PIC X(003).
             05 00NUM002CC              PIC X(003).
             05 00NUM003CC              PIC X(003).
             05 00NUM004CC              PIC X(003).
             05 00NUM005CC              PIC X(003).
             05 00NUM007CC              PIC X(003).
             05 00NUM008CC              PIC X(003).
             05 00NUM009CC              PIC X(003).
             05 00NUM010CC              PIC X(003).
             05 00NUM017CC              PIC X(003).
             05 00NUM018CC              PIC X(003).
             05 00NUM019CC              PIC X(003).
             05 00NUM022CC              PIC X(003).
             05 00NUM023CC              PIC X(003).
             05 00NUM024CC              PIC X(003).
             05 00NUM028CC              PIC X(003).
             05 00NUM029CC              PIC X(003).
             05 00NUM030CC              PIC X(003).
             05 00NUM031CC              PIC X(003).
             05 00NUM032CC              PIC X(003).
             05 00NUM033CC              PIC X(003).
             05 00NUM034CC              PIC X(003).
             05 00NUM035CC              PIC X(003).
             05 00NUM036CC              PIC X(003).
             05 00NUM037CC              PIC X(003).
             05 00NUM038CC              PIC X(003).
             05 00NUM039CC              PIC X(003).
             05 00NUM040CC              PIC X(003).
             05 00NUM041CC              PIC X(003).
             05 00NUM042CC              PIC X(003).
             05 00NUM043CC              PIC X(003).
             05 00NUM044CC              PIC X(003).
             05 00NUM045CC              PIC X(003).
             05 00NUM046CC              PIC X(003).
             05 00NUM047CC              PIC X(003).
             05 00NUM048CC              PIC X(003).
             05 00NUM049CC              PIC X(003).
             05 00NUM050CC              PIC X(003).
             05 00NUM066CC              PIC X(003).

          03 VAR-00NUMCO.
             05 00NUM001CO              PIC X(003).
             05 00NUM002CO              PIC X(003).
             05 00NUM003CO              PIC X(003).
             05 00NUM004CO              PIC X(003).
             05 00NUM005CO              PIC X(003).
             05 00NUM007CO              PIC X(003).
             05 00NUM008CO              PIC X(003).
             05 00NUM009CO              PIC X(003).
             05 00NUM010CO              PIC X(003).
             05 00NUM017CO              PIC X(003).
             05 00NUM018CO              PIC X(003).
             05 00NUM019CO              PIC X(003).
             05 00NUM022CO              PIC X(003).
             05 00NUM023CO              PIC X(003).
             05 00NUM024CO              PIC X(003).
             05 00NUM028CO              PIC X(003).
             05 00NUM029CO              PIC X(003).
             05 00NUM030CO              PIC X(003).
             05 00NUM031CO              PIC X(003).
             05 00NUM032CO              PIC X(003).
             05 00NUM033CO              PIC X(003).
             05 00NUM034CO              PIC X(003).
             05 00NUM035CO              PIC X(003).
             05 00NUM036CO              PIC X(003).
             05 00NUM037CO              PIC X(003).
             05 00NUM038CO              PIC X(003).
             05 00NUM039CO              PIC X(003).
             05 00NUM040CO              PIC X(003).
             05 00NUM041CO              PIC X(003).
             05 00NUM042CO              PIC X(003).
             05 00NUM043CO              PIC X(003).
             05 00NUM044CO              PIC X(003).
             05 00NUM045CO              PIC X(003).
             05 00NUM046CO              PIC X(003).
             05 00NUM047CO              PIC X(003).
             05 00NUM048CO              PIC X(003).
             05 00NUM049CO              PIC X(003).
             05 00NUM050CO              PIC X(003).
             05 00NUM066CO              PIC X(003).

          03 VAR-00NUMAH.
             05 00NUM001AH              PIC X(003).
             05 00NUM002AH              PIC X(003).
             05 00NUM003AH              PIC X(003).
             05 00NUM004AH              PIC X(003).
             05 00NUM005AH              PIC X(003).
             05 00NUM007AH              PIC X(003).
             05 00NUM008AH              PIC X(003).
             05 00NUM009AH              PIC X(003).
             05 00NUM010AH              PIC X(003).
             05 00NUM017AH              PIC X(003).
             05 00NUM018AH              PIC X(003).
             05 00NUM019AH              PIC X(003).
             05 00NUM022AH              PIC X(003).
             05 00NUM023AH              PIC X(003).
             05 00NUM024AH              PIC X(003).
             05 00NUM028AH              PIC X(003).
             05 00NUM029AH              PIC X(003).
             05 00NUM030AH              PIC X(003).
             05 00NUM031AH              PIC X(003).
             05 00NUM032AH              PIC X(003).
             05 00NUM033AH              PIC X(003).
             05 00NUM034AH              PIC X(003).
             05 00NUM035AH              PIC X(003).
             05 00NUM036AH              PIC X(003).
             05 00NUM037AH              PIC X(003).
             05 00NUM038AH              PIC X(003).
             05 00NUM039AH              PIC X(003).
             05 00NUM040AH              PIC X(003).
             05 00NUM041AH              PIC X(003).
             05 00NUM042AH              PIC X(003).
             05 00NUM043AH              PIC X(003).
             05 00NUM044AH              PIC X(003).
             05 00NUM045AH              PIC X(003).
             05 00NUM046AH              PIC X(003).
             05 00NUM047AH              PIC X(003).
             05 00NUM048AH              PIC X(003).
             05 00NUM049AH              PIC X(003).
             05 00NUM050AH              PIC X(003).
             05 00NUM066AH              PIC X(003).

          03 VAR-00NUMCT.
             05 00NUM001CT              PIC X(003).
             05 00NUM002CT              PIC X(003).
             05 00NUM003CT              PIC X(003).
             05 00NUM004CT              PIC X(003).
             05 00NUM005CT              PIC X(003).
             05 00NUM007CT              PIC X(003).
             05 00NUM008CT              PIC X(003).
             05 00NUM009CT              PIC X(003).
             05 00NUM010CT              PIC X(003).
             05 00NUM017CT              PIC X(003).
             05 00NUM018CT              PIC X(003).
             05 00NUM019CT              PIC X(003).
             05 00NUM022CT              PIC X(003).
             05 00NUM023CT              PIC X(003).
             05 00NUM024CT              PIC X(003).
             05 00NUM028CT              PIC X(003).
             05 00NUM029CT              PIC X(003).
             05 00NUM030CT              PIC X(003).
             05 00NUM031CT              PIC X(003).
             05 00NUM032CT              PIC X(003).
             05 00NUM033CT              PIC X(003).
             05 00NUM034CT              PIC X(003).
             05 00NUM035CT              PIC X(003).
             05 00NUM036CT              PIC X(003).
             05 00NUM037CT              PIC X(003).
             05 00NUM038CT              PIC X(003).
             05 00NUM039CT              PIC X(003).
             05 00NUM040CT              PIC X(003).
             05 00NUM041CT              PIC X(003).
             05 00NUM042CT              PIC X(003).
             05 00NUM043CT              PIC X(003).
             05 00NUM044CT              PIC X(003).
             05 00NUM045CT              PIC X(003).
             05 00NUM046CT              PIC X(003).
             05 00NUM047CT              PIC X(003).
             05 00NUM048CT              PIC X(003).
             05 00NUM049CT              PIC X(003).
             05 00NUM050CT              PIC X(003).
             05 00NUM066CT              PIC X(003).

       01 VAR-00INQ.
          03 00INQTIP                   PIC 9(001).
          03 00INQNRO                   PIC 9(011).
          03 00INQSUS                   PIC 9(006).
          03 00INQFCH                   PIC 9(006).
          03 00INQCSC                   PIC 9(002).
          03 00INQ001                   PIC 9(003).
          03 00INQ002                   PIC 9(003).
          03 00INQ005                   PIC 9(003).
          03 00INQ006                   PIC 9(003).
          03 00INQ007                   PIC 9(003).
          03 00INQ010                   PIC 9(003).
          03 00INQ011                   PIC 9(003).
          03 00INQ012                   PIC 9(003).
          03 00INQ015                   PIC 9(003).
ACIEAA    03 00INQ016                   PIC 9(003).                     RMB0006
ACIEAA    03 00INQ017                   PIC 9(003).                     RMB0006
ACIEAA    03 00INQ018                   PIC 9(003).                     RMB0006
ACIEAA    03 00INQ025                   PIC 9(003).                     RMB0006
ACIEAA    03 00INQ026                   PIC 9(003).                     RMB0006
ACIEAA    03 00INQ027                   PIC 9(003).                     RMB0006
ACIEAA    03 00INQ028                   PIC 9(003).                     RMB0006

       01 VAR-00MOR.
          03 00MORLLV                   PIC X(038).
          03 00MORPTF                   PIC X(002).
          03 00MOR001                   PIC 9(003).
          03 00MOR002                   PIC 9(003).
          03 00MOR003                   PIC 9(003).
          03 00MOR004                   PIC 9(003).
          03 00MOR005                   PIC 9(003).
          03 00MOR006                   PIC 9(003).
          03 00MOR007                   PIC 9(003).
          03 00MOR008                   PIC 9(003).
          03 00MOR009                   PIC 9(003).
          03 00MOR010                   PIC 9(003).
          03 00MOR011                   PIC 9(003).
          03 00MOR012                   PIC 9(003).
          03 00MOR013                   PIC 9(003).
          03 00MOR014                   PIC 9(003).
          03 00MOR015                   PIC 9(003).
          03 00MOR016                   PIC 9(003).
          03 00MOR017                   PIC 9(003).
          03 00MOR018                   PIC 9(003).
          03 00MOR019                   PIC 9(003).
          03 00MOR020                   PIC 9(003).
          03 00MOR021                   PIC 9(003).
          03 00MOR022                   PIC 9(003).
          03 00MOR023                   PIC 9(003).
          03 00MOR024                   PIC 9(003).
          03 00MOR025                   PIC 9(003).
          03 00MOR026                   PIC 9(003).
          03 00MOR027                   PIC 9(003).
          03 00MOR028                   PIC 9(003).
          03 00MOR029                   PIC 9(003).
          03 00MOR030                   PIC 9(003).
          03 00MOR031                   PIC 9(003).
          03 00MOR032                   PIC 9(003).
          03 00MOR033                   PIC 9(003).
          03 00MOR034                   PIC 9(003).
          03 00MOR035                   PIC 9(003).
          03 00MOR036                   PIC 9(003).
          03 00MOR037                   PIC 9(003).
          03 00MOR038                   PIC 9(003).
          03 00MOR039                   PIC 9(003).
          03 00MOR040                   PIC 9(003).
          03 00MOR041                   PIC 9(003).
          03 00MOR042                   PIC 9(003).
          03 00MOR043                   PIC 9(003).
          03 00MOR044                   PIC 9(003).
          03 00MOR045                   PIC 9(003).
          03 00MOR046                   PIC 9(003).
          03 00MOR047                   PIC 9(003).
          03 00MOR048                   PIC 9(003).
          03 00MOR049                   PIC 9(003).
          03 00MOR050                   PIC 9(003).
          03 00MOR051                   PIC 9(003).
          03 00MOR052                   PIC 9(003).
          03 00MOR053                   PIC 9(003).
          03 00MOR054                   PIC 9(003).
          03 00MOR055                   PIC 9(003).
          03 00MOR056                   PIC 9(003).
          03 00MOR057                   PIC 9(003).
          03 00MOR058                   PIC 9(003).
          03 00MOR059                   PIC 9(003).
          03 00MOR060                   PIC 9(003).
          03 00MOR061                   PIC 9(003).
          03 00MOR062                   PIC 9(003).
          03 00MOR063                   PIC 9(003).
          03 00MOR064                   PIC 9(003).
          03 00MOR065                   PIC 9(003).
          03 00MOR066                   PIC 9(003).
          03 00MOR067                   PIC 9(003).
          03 00MOR068                   PIC 9(003).
          03 00MOR069                   PIC 9(003).
          03 00MOR070                   PIC 9(003).
          03 00MOR071                   PIC 9(003).
          03 00MOR072                   PIC 9(003).
          03 00MOR073                   PIC 9(003).
          03 00MOR074                   PIC 9(003).
          03 00MOR075                   PIC 9(003).
          03 00MOR076                   PIC 9(003).
          03 00MOR077                   PIC 9(003).
          03 00MOR078                   PIC 9(003).
          03 00MOR079                   PIC 9(003).
          03 00MOR080                   PIC 999.99.
          03 00MOR081                   PIC 999.99.
          03 00MOR082                   PIC 999.99.
          03 00MOR083                   PIC 999.99.
          03 00MOR084                   PIC 999.99.
          03 00MOR085                   PIC 999.99.
          03 00MOR086                   PIC 999.99.
          03 00MOR087                   PIC 999.99.
          03 00MOR088                   PIC 999.99.
          03 00MOR089                   PIC 999.99.
          03 00MOR090                   PIC 999.99.
          03 00MOR091                   PIC 999.99.
          03 00MOR092                   PIC 999.99.
          03 00MOR093                   PIC 999.99.
          03 00MOR094                   PIC 999.99.
          03 00MOR095                   PIC 999.99.
          03 00MOR096                   PIC 999.99.
          03 00MOR097                   PIC 999.99.
          03 00MOR098                   PIC 999.99.
          03 00MOR099                   PIC 999.99.
          03 00MOR100                   PIC 999.99.
          03 00MOR101                   PIC 999.99.
          03 00MOR102                   PIC 999.99.
          03 00MOR103                   PIC 999.99.
          03 00MOR104                   PIC X(049).

       01 VARIABLES-00MOR.
          03 VAR-00MORRO.
             05 00MOR001RO              PIC X(003).
             05 00MOR002RO              PIC X(003).
             05 00MOR003RO              PIC X(003).
             05 00MOR004RO              PIC X(003).
             05 00MOR005RO              PIC X(003).
             05 00MOR006RO              PIC X(003).
             05 00MOR007RO              PIC X(003).
             05 00MOR008RO              PIC X(003).
             05 00MOR009RO              PIC X(003).
             05 00MOR010RO              PIC X(003).
             05 00MOR011RO              PIC X(003).
             05 00MOR012RO              PIC X(003).
             05 00MOR013RO              PIC X(003).
             05 00MOR014RO              PIC X(003).
             05 00MOR015RO              PIC X(003).
             05 00MOR016RO              PIC X(003).
             05 00MOR017RO              PIC X(003).
             05 00MOR018RO              PIC X(003).
             05 00MOR019RO              PIC X(003).
             05 00MOR020RO              PIC X(003).
             05 00MOR021RO              PIC X(003).
             05 00MOR022RO              PIC X(003).
             05 00MOR023RO              PIC X(003).
             05 00MOR024RO              PIC X(003).
             05 00MOR025RO              PIC X(003).
             05 00MOR026RO              PIC X(003).
             05 00MOR027RO              PIC X(003).
             05 00MOR028RO              PIC X(003).
             05 00MOR029RO              PIC X(003).
             05 00MOR030RO              PIC X(003).
             05 00MOR031RO              PIC X(003).
             05 00MOR032RO              PIC X(003).
             05 00MOR033RO              PIC X(003).
             05 00MOR034RO              PIC X(003).
             05 00MOR035RO              PIC X(003).
             05 00MOR036RO              PIC X(003).
             05 00MOR037RO              PIC X(003).
             05 00MOR038RO              PIC X(003).
             05 00MOR039RO              PIC X(003).
             05 00MOR040RO              PIC X(003).
             05 00MOR041RO              PIC X(003).
             05 00MOR042RO              PIC X(003).
             05 00MOR043RO              PIC X(003).
             05 00MOR044RO              PIC X(003).
             05 00MOR045RO              PIC X(003).
             05 00MOR046RO              PIC X(003).
             05 00MOR047RO              PIC X(003).
             05 00MOR048RO              PIC X(003).
             05 00MOR049RO              PIC X(003).
             05 00MOR050RO              PIC X(003).
             05 00MOR051RO              PIC X(003).
             05 00MOR052RO              PIC X(003).
             05 00MOR053RO              PIC X(003).
             05 00MOR054RO              PIC X(003).
             05 00MOR055RO              PIC X(003).
             05 00MOR056RO              PIC X(003).
             05 00MOR057RO              PIC X(003).
             05 00MOR058RO              PIC X(003).
             05 00MOR059RO              PIC X(003).
             05 00MOR060RO              PIC X(003).
             05 00MOR061RO              PIC X(003).
             05 00MOR062RO              PIC X(003).
             05 00MOR063RO              PIC X(003).
             05 00MOR064RO              PIC X(003).
             05 00MOR065RO              PIC X(003).
             05 00MOR066RO              PIC X(003).
             05 00MOR067RO              PIC X(003).
             05 00MOR068RO              PIC X(003).
             05 00MOR069RO              PIC X(003).
             05 00MOR070RO              PIC X(003).
             05 00MOR071RO              PIC X(003).
             05 00MOR072RO              PIC X(003).
             05 00MOR073RO              PIC X(003).
             05 00MOR074RO              PIC X(003).
             05 00MOR075RO              PIC X(003).
             05 00MOR076RO              PIC X(003).
             05 00MOR077RO              PIC X(003).
             05 00MOR078RO              PIC X(003).
             05 00MOR079RO              PIC X(003).
             05 00MOR080RO              PIC X(006).
             05 00MOR081RO              PIC X(006).
             05 00MOR082RO              PIC X(006).
             05 00MOR083RO              PIC X(006).
             05 00MOR084RO              PIC X(006).
             05 00MOR085RO              PIC X(006).
             05 00MOR086RO              PIC X(006).
             05 00MOR087RO              PIC X(006).
             05 00MOR088RO              PIC X(006).
             05 00MOR089RO              PIC X(006).
             05 00MOR090RO              PIC X(006).
             05 00MOR091RO              PIC X(006).
             05 00MOR092RO              PIC X(006).
             05 00MOR093RO              PIC X(006).
             05 00MOR094RO              PIC X(006).
             05 00MOR095RO              PIC X(006).
             05 00MOR096RO              PIC X(006).
             05 00MOR097RO              PIC X(006).
             05 00MOR098RO              PIC X(006).
             05 00MOR099RO              PIC X(006).
             05 00MOR100RO              PIC X(006).
             05 00MOR101RO              PIC X(006).
             05 00MOR102RO              PIC X(006).
             05 00MOR103RO              PIC X(006).
             05 00MOR104RO              PIC X(049).

          03 VAR-00MORIN.
             05 00MOR001IN              PIC X(003).
             05 00MOR002IN              PIC X(003).
             05 00MOR003IN              PIC X(003).
             05 00MOR004IN              PIC X(003).
             05 00MOR005IN              PIC X(003).
             05 00MOR006IN              PIC X(003).
             05 00MOR007IN              PIC X(003).
             05 00MOR008IN              PIC X(003).
             05 00MOR009IN              PIC X(003).
             05 00MOR010IN              PIC X(003).
             05 00MOR011IN              PIC X(003).
             05 00MOR012IN              PIC X(003).
             05 00MOR013IN              PIC X(003).
             05 00MOR014IN              PIC X(003).
             05 00MOR015IN              PIC X(003).
             05 00MOR016IN              PIC X(003).
             05 00MOR017IN              PIC X(003).
             05 00MOR018IN              PIC X(003).
             05 00MOR019IN              PIC X(003).
             05 00MOR020IN              PIC X(003).
             05 00MOR021IN              PIC X(003).
             05 00MOR022IN              PIC X(003).
             05 00MOR023IN              PIC X(003).
             05 00MOR024IN              PIC X(003).
             05 00MOR025IN              PIC X(003).
             05 00MOR026IN              PIC X(003).
             05 00MOR027IN              PIC X(003).
             05 00MOR028IN              PIC X(003).
             05 00MOR029IN              PIC X(003).
             05 00MOR030IN              PIC X(003).
             05 00MOR031IN              PIC X(003).
             05 00MOR032IN              PIC X(003).
             05 00MOR033IN              PIC X(003).
             05 00MOR034IN              PIC X(003).
             05 00MOR035IN              PIC X(003).
             05 00MOR036IN              PIC X(003).
             05 00MOR037IN              PIC X(003).
             05 00MOR038IN              PIC X(003).
             05 00MOR039IN              PIC X(003).
             05 00MOR040IN              PIC X(003).
             05 00MOR041IN              PIC X(003).
             05 00MOR042IN              PIC X(003).
             05 00MOR043IN              PIC X(003).
             05 00MOR044IN              PIC X(003).
             05 00MOR045IN              PIC X(003).
             05 00MOR046IN              PIC X(003).
             05 00MOR047IN              PIC X(003).
             05 00MOR048IN              PIC X(003).
             05 00MOR049IN              PIC X(003).
             05 00MOR050IN              PIC X(003).
             05 00MOR051IN              PIC X(003).
             05 00MOR052IN              PIC X(003).
             05 00MOR053IN              PIC X(003).
             05 00MOR054IN              PIC X(003).
             05 00MOR055IN              PIC X(003).
             05 00MOR056IN              PIC X(003).
             05 00MOR057IN              PIC X(003).
             05 00MOR058IN              PIC X(003).
             05 00MOR059IN              PIC X(003).
             05 00MOR060IN              PIC X(003).
             05 00MOR061IN              PIC X(003).
             05 00MOR062IN              PIC X(003).
             05 00MOR063IN              PIC X(003).
             05 00MOR064IN              PIC X(003).
             05 00MOR065IN              PIC X(003).
             05 00MOR066IN              PIC X(003).
             05 00MOR067IN              PIC X(003).
             05 00MOR068IN              PIC X(003).
             05 00MOR069IN              PIC X(003).
             05 00MOR070IN              PIC X(003).
             05 00MOR071IN              PIC X(003).
             05 00MOR072IN              PIC X(003).
             05 00MOR073IN              PIC X(003).
             05 00MOR074IN              PIC X(003).
             05 00MOR075IN              PIC X(003).
             05 00MOR076IN              PIC X(003).
             05 00MOR077IN              PIC X(003).
             05 00MOR078IN              PIC X(003).
             05 00MOR079IN              PIC X(003).
             05 00MOR080IN              PIC X(006).
             05 00MOR081IN              PIC X(006).
             05 00MOR082IN              PIC X(006).
             05 00MOR083IN              PIC X(006).
             05 00MOR084IN              PIC X(006).
             05 00MOR085IN              PIC X(006).
             05 00MOR086IN              PIC X(006).
             05 00MOR087IN              PIC X(006).
             05 00MOR088IN              PIC X(006).
             05 00MOR089IN              PIC X(006).
             05 00MOR090IN              PIC X(006).
             05 00MOR091IN              PIC X(006).
             05 00MOR092IN              PIC X(006).
             05 00MOR093IN              PIC X(006).
             05 00MOR094IN              PIC X(006).
             05 00MOR095IN              PIC X(006).
             05 00MOR096IN              PIC X(006).
             05 00MOR097IN              PIC X(006).
             05 00MOR098IN              PIC X(006).
             05 00MOR099IN              PIC X(006).
             05 00MOR100IN              PIC X(006).
             05 00MOR101IN              PIC X(006).
             05 00MOR102IN              PIC X(006).
             05 00MOR103IN              PIC X(006).
             05 00MOR104IN              PIC X(049).

          03 VAR-00MORVE.
             05 00MOR001VE              PIC X(003).
             05 00MOR002VE              PIC X(003).
             05 00MOR003VE              PIC X(003).
             05 00MOR004VE              PIC X(003).
             05 00MOR005VE              PIC X(003).
             05 00MOR006VE              PIC X(003).
             05 00MOR007VE              PIC X(003).
             05 00MOR008VE              PIC X(003).
             05 00MOR009VE              PIC X(003).
             05 00MOR010VE              PIC X(003).
             05 00MOR011VE              PIC X(003).
             05 00MOR012VE              PIC X(003).
             05 00MOR013VE              PIC X(003).
             05 00MOR014VE              PIC X(003).
             05 00MOR015VE              PIC X(003).
             05 00MOR016VE              PIC X(003).
             05 00MOR017VE              PIC X(003).
             05 00MOR018VE              PIC X(003).
             05 00MOR019VE              PIC X(003).
             05 00MOR020VE              PIC X(003).
             05 00MOR021VE              PIC X(003).
             05 00MOR022VE              PIC X(003).
             05 00MOR023VE              PIC X(003).
             05 00MOR024VE              PIC X(003).
             05 00MOR025VE              PIC X(003).
             05 00MOR026VE              PIC X(003).
             05 00MOR027VE              PIC X(003).
             05 00MOR028VE              PIC X(003).
             05 00MOR029VE              PIC X(003).
             05 00MOR030VE              PIC X(003).
             05 00MOR031VE              PIC X(003).
             05 00MOR032VE              PIC X(003).
             05 00MOR033VE              PIC X(003).
             05 00MOR034VE              PIC X(003).
             05 00MOR035VE              PIC X(003).
             05 00MOR036VE              PIC X(003).
             05 00MOR037VE              PIC X(003).
             05 00MOR038VE              PIC X(003).
             05 00MOR039VE              PIC X(003).
             05 00MOR040VE              PIC X(003).
             05 00MOR041VE              PIC X(003).
             05 00MOR042VE              PIC X(003).
             05 00MOR043VE              PIC X(003).
             05 00MOR044VE              PIC X(003).
             05 00MOR045VE              PIC X(003).
             05 00MOR046VE              PIC X(003).
             05 00MOR047VE              PIC X(003).
             05 00MOR048VE              PIC X(003).
             05 00MOR049VE              PIC X(003).
             05 00MOR050VE              PIC X(003).
             05 00MOR051VE              PIC X(003).
             05 00MOR052VE              PIC X(003).
             05 00MOR053VE              PIC X(003).
             05 00MOR054VE              PIC X(003).
             05 00MOR055VE              PIC X(003).
             05 00MOR056VE              PIC X(003).
             05 00MOR057VE              PIC X(003).
             05 00MOR058VE              PIC X(003).
             05 00MOR059VE              PIC X(003).
             05 00MOR060VE              PIC X(003).
             05 00MOR061VE              PIC X(003).
             05 00MOR062VE              PIC X(003).
             05 00MOR063VE              PIC X(003).
             05 00MOR064VE              PIC X(003).
             05 00MOR065VE              PIC X(003).
             05 00MOR066VE              PIC X(003).
             05 00MOR067VE              PIC X(003).
             05 00MOR068VE              PIC X(003).
             05 00MOR069VE              PIC X(003).
             05 00MOR070VE              PIC X(003).
             05 00MOR071VE              PIC X(003).
             05 00MOR072VE              PIC X(003).
             05 00MOR073VE              PIC X(003).
             05 00MOR074VE              PIC X(003).
             05 00MOR075VE              PIC X(003).
             05 00MOR076VE              PIC X(003).
             05 00MOR077VE              PIC X(003).
             05 00MOR078VE              PIC X(003).
             05 00MOR079VE              PIC X(003).
             05 00MOR080VE              PIC X(006).
             05 00MOR081VE              PIC X(006).
             05 00MOR082VE              PIC X(006).
             05 00MOR083VE              PIC X(006).
             05 00MOR084VE              PIC X(006).
             05 00MOR085VE              PIC X(006).
             05 00MOR086VE              PIC X(006).
             05 00MOR087VE              PIC X(006).
             05 00MOR088VE              PIC X(006).
             05 00MOR089VE              PIC X(006).
             05 00MOR090VE              PIC X(006).
             05 00MOR091VE              PIC X(006).
             05 00MOR092VE              PIC X(006).
             05 00MOR093VE              PIC X(006).
             05 00MOR094VE              PIC X(006).
             05 00MOR095VE              PIC X(006).
             05 00MOR096VE              PIC X(006).
             05 00MOR097VE              PIC X(006).
             05 00MOR098VE              PIC X(006).
             05 00MOR099VE              PIC X(006).
             05 00MOR100VE              PIC X(006).
             05 00MOR101VE              PIC X(006).
             05 00MOR102VE              PIC X(006).
             05 00MOR103VE              PIC X(006).
             05 00MOR104VE              PIC X(049).

          03 VAR-00MORHP.
             05 00MOR001HP              PIC X(003).
             05 00MOR002HP              PIC X(003).
             05 00MOR003HP              PIC X(003).
             05 00MOR004HP              PIC X(003).
             05 00MOR005HP              PIC X(003).
             05 00MOR006HP              PIC X(003).
             05 00MOR007HP              PIC X(003).
             05 00MOR008HP              PIC X(003).
             05 00MOR009HP              PIC X(003).
             05 00MOR010HP              PIC X(003).
             05 00MOR011HP              PIC X(003).
             05 00MOR012HP              PIC X(003).
             05 00MOR013HP              PIC X(003).
             05 00MOR014HP              PIC X(003).
             05 00MOR015HP              PIC X(003).
             05 00MOR016HP              PIC X(003).
             05 00MOR017HP              PIC X(003).
             05 00MOR018HP              PIC X(003).
             05 00MOR019HP              PIC X(003).
             05 00MOR020HP              PIC X(003).
             05 00MOR021HP              PIC X(003).
             05 00MOR022HP              PIC X(003).
             05 00MOR023HP              PIC X(003).
             05 00MOR024HP              PIC X(003).
             05 00MOR025HP              PIC X(003).
             05 00MOR026HP              PIC X(003).
             05 00MOR027HP              PIC X(003).
             05 00MOR028HP              PIC X(003).
             05 00MOR029HP              PIC X(003).
             05 00MOR030HP              PIC X(003).
             05 00MOR031HP              PIC X(003).
             05 00MOR032HP              PIC X(003).
             05 00MOR033HP              PIC X(003).
             05 00MOR034HP              PIC X(003).
             05 00MOR035HP              PIC X(003).
             05 00MOR036HP              PIC X(003).
             05 00MOR037HP              PIC X(003).
             05 00MOR038HP              PIC X(003).
             05 00MOR039HP              PIC X(003).
             05 00MOR040HP              PIC X(003).
             05 00MOR041HP              PIC X(003).
             05 00MOR042HP              PIC X(003).
             05 00MOR043HP              PIC X(003).
             05 00MOR044HP              PIC X(003).
             05 00MOR045HP              PIC X(003).
             05 00MOR046HP              PIC X(003).
             05 00MOR047HP              PIC X(003).
             05 00MOR048HP              PIC X(003).
             05 00MOR049HP              PIC X(003).
             05 00MOR050HP              PIC X(003).
             05 00MOR051HP              PIC X(003).
             05 00MOR052HP              PIC X(003).
             05 00MOR053HP              PIC X(003).
             05 00MOR054HP              PIC X(003).
             05 00MOR055HP              PIC X(003).
             05 00MOR056HP              PIC X(003).
             05 00MOR057HP              PIC X(003).
             05 00MOR058HP              PIC X(003).
             05 00MOR059HP              PIC X(003).
             05 00MOR060HP              PIC X(003).
             05 00MOR061HP              PIC X(003).
             05 00MOR062HP              PIC X(003).
             05 00MOR063HP              PIC X(003).
             05 00MOR064HP              PIC X(003).
             05 00MOR065HP              PIC X(003).
             05 00MOR066HP              PIC X(003).
             05 00MOR067HP              PIC X(003).
             05 00MOR068HP              PIC X(003).
             05 00MOR069HP              PIC X(003).
             05 00MOR070HP              PIC X(003).
             05 00MOR071HP              PIC X(003).
             05 00MOR072HP              PIC X(003).
             05 00MOR073HP              PIC X(003).
             05 00MOR074HP              PIC X(003).
             05 00MOR075HP              PIC X(003).
             05 00MOR076HP              PIC X(003).
             05 00MOR077HP              PIC X(003).
             05 00MOR078HP              PIC X(003).
             05 00MOR079HP              PIC X(003).
             05 00MOR080HP              PIC X(006).
             05 00MOR081HP              PIC X(006).
             05 00MOR082HP              PIC X(006).
             05 00MOR083HP              PIC X(006).
             05 00MOR084HP              PIC X(006).
             05 00MOR085HP              PIC X(006).
             05 00MOR086HP              PIC X(006).
             05 00MOR087HP              PIC X(006).
             05 00MOR088HP              PIC X(006).
             05 00MOR089HP              PIC X(006).
             05 00MOR090HP              PIC X(006).
             05 00MOR091HP              PIC X(006).
             05 00MOR092HP              PIC X(006).
             05 00MOR093HP              PIC X(006).
             05 00MOR094HP              PIC X(006).
             05 00MOR095HP              PIC X(006).
             05 00MOR096HP              PIC X(006).
             05 00MOR097HP              PIC X(006).
             05 00MOR098HP              PIC X(006).
             05 00MOR099HP              PIC X(006).
             05 00MOR100HP              PIC X(006).
             05 00MOR101HP              PIC X(006).
             05 00MOR102HP              PIC X(006).
             05 00MOR103HP              PIC X(006).
             05 00MOR104HP              PIC X(049).

          03 VAR-00MOROT.
             05 00MOR001OT              PIC X(003).
             05 00MOR002OT              PIC X(003).
             05 00MOR003OT              PIC X(003).
             05 00MOR004OT              PIC X(003).
             05 00MOR005OT              PIC X(003).
             05 00MOR006OT              PIC X(003).
             05 00MOR007OT              PIC X(003).
             05 00MOR008OT              PIC X(003).
             05 00MOR009OT              PIC X(003).
             05 00MOR010OT              PIC X(003).
             05 00MOR011OT              PIC X(003).
             05 00MOR012OT              PIC X(003).
             05 00MOR013OT              PIC X(003).
             05 00MOR014OT              PIC X(003).
             05 00MOR015OT              PIC X(003).
             05 00MOR016OT              PIC X(003).
             05 00MOR017OT              PIC X(003).
             05 00MOR018OT              PIC X(003).
             05 00MOR019OT              PIC X(003).
             05 00MOR020OT              PIC X(003).
             05 00MOR021OT              PIC X(003).
             05 00MOR022OT              PIC X(003).
             05 00MOR023OT              PIC X(003).
             05 00MOR024OT              PIC X(003).
             05 00MOR025OT              PIC X(003).
             05 00MOR026OT              PIC X(003).
             05 00MOR027OT              PIC X(003).
             05 00MOR028OT              PIC X(003).
             05 00MOR029OT              PIC X(003).
             05 00MOR030OT              PIC X(003).
             05 00MOR031OT              PIC X(003).
             05 00MOR032OT              PIC X(003).
             05 00MOR033OT              PIC X(003).
             05 00MOR034OT              PIC X(003).
             05 00MOR035OT              PIC X(003).
             05 00MOR036OT              PIC X(003).
             05 00MOR037OT              PIC X(003).
             05 00MOR038OT              PIC X(003).
             05 00MOR039OT              PIC X(003).
             05 00MOR040OT              PIC X(003).
             05 00MOR041OT              PIC X(003).
             05 00MOR042OT              PIC X(003).
             05 00MOR043OT              PIC X(003).
             05 00MOR044OT              PIC X(003).
             05 00MOR045OT              PIC X(003).
             05 00MOR046OT              PIC X(003).
             05 00MOR047OT              PIC X(003).
             05 00MOR048OT              PIC X(003).
             05 00MOR049OT              PIC X(003).
             05 00MOR050OT              PIC X(003).
             05 00MOR051OT              PIC X(003).
             05 00MOR052OT              PIC X(003).
             05 00MOR053OT              PIC X(003).
             05 00MOR054OT              PIC X(003).
             05 00MOR055OT              PIC X(003).
             05 00MOR056OT              PIC X(003).
             05 00MOR057OT              PIC X(003).
             05 00MOR058OT              PIC X(003).
             05 00MOR059OT              PIC X(003).
             05 00MOR060OT              PIC X(003).
             05 00MOR061OT              PIC X(003).
             05 00MOR062OT              PIC X(003).
             05 00MOR063OT              PIC X(003).
             05 00MOR064OT              PIC X(003).
             05 00MOR065OT              PIC X(003).
             05 00MOR066OT              PIC X(003).
             05 00MOR067OT              PIC X(003).
             05 00MOR068OT              PIC X(003).
             05 00MOR069OT              PIC X(003).
             05 00MOR070OT              PIC X(003).
             05 00MOR071OT              PIC X(003).
             05 00MOR072OT              PIC X(003).
             05 00MOR073OT              PIC X(003).
             05 00MOR074OT              PIC X(003).
             05 00MOR075OT              PIC X(003).
             05 00MOR076OT              PIC X(003).
             05 00MOR077OT              PIC X(003).
             05 00MOR078OT              PIC X(003).
             05 00MOR079OT              PIC X(003).
             05 00MOR080OT              PIC X(006).
             05 00MOR081OT              PIC X(006).
             05 00MOR082OT              PIC X(006).
             05 00MOR083OT              PIC X(006).
             05 00MOR084OT              PIC X(006).
             05 00MOR085OT              PIC X(006).
             05 00MOR086OT              PIC X(006).
             05 00MOR087OT              PIC X(006).
             05 00MOR088OT              PIC X(006).
             05 00MOR089OT              PIC X(006).
             05 00MOR090OT              PIC X(006).
             05 00MOR091OT              PIC X(006).
             05 00MOR092OT              PIC X(006).
             05 00MOR093OT              PIC X(006).
             05 00MOR094OT              PIC X(006).
             05 00MOR095OT              PIC X(006).
             05 00MOR096OT              PIC X(006).
             05 00MOR097OT              PIC X(006).
             05 00MOR098OT              PIC X(006).
             05 00MOR099OT              PIC X(006).
             05 00MOR100OT              PIC X(006).
             05 00MOR101OT              PIC X(006).
             05 00MOR102OT              PIC X(006).
             05 00MOR103OT              PIC X(006).
             05 00MOR104OT              PIC X(049).

          03 VAR-00MORCC.
             05 00MOR001CC              PIC X(003).
             05 00MOR002CC              PIC X(003).
             05 00MOR003CC              PIC X(003).
             05 00MOR004CC              PIC X(003).
             05 00MOR005CC              PIC X(003).
             05 00MOR006CC              PIC X(003).
             05 00MOR007CC              PIC X(003).
             05 00MOR008CC              PIC X(003).
             05 00MOR009CC              PIC X(003).
             05 00MOR010CC              PIC X(003).
             05 00MOR011CC              PIC X(003).
             05 00MOR012CC              PIC X(003).
             05 00MOR013CC              PIC X(003).
             05 00MOR014CC              PIC X(003).
             05 00MOR015CC              PIC X(003).
             05 00MOR016CC              PIC X(003).
             05 00MOR017CC              PIC X(003).
             05 00MOR018CC              PIC X(003).
             05 00MOR019CC              PIC X(003).
             05 00MOR020CC              PIC X(003).
             05 00MOR021CC              PIC X(003).
             05 00MOR022CC              PIC X(003).
             05 00MOR023CC              PIC X(003).
             05 00MOR024CC              PIC X(003).
             05 00MOR025CC              PIC X(003).
             05 00MOR026CC              PIC X(003).
             05 00MOR027CC              PIC X(003).
             05 00MOR028CC              PIC X(003).
             05 00MOR029CC              PIC X(003).
             05 00MOR030CC              PIC X(003).
             05 00MOR031CC              PIC X(003).
             05 00MOR032CC              PIC X(003).
             05 00MOR033CC              PIC X(003).
             05 00MOR034CC              PIC X(003).
             05 00MOR035CC              PIC X(003).
             05 00MOR036CC              PIC X(003).
             05 00MOR037CC              PIC X(003).
             05 00MOR038CC              PIC X(003).
             05 00MOR039CC              PIC X(003).
             05 00MOR040CC              PIC X(003).
             05 00MOR041CC              PIC X(003).
             05 00MOR042CC              PIC X(003).
             05 00MOR043CC              PIC X(003).
             05 00MOR044CC              PIC X(003).
             05 00MOR045CC              PIC X(003).
             05 00MOR046CC              PIC X(003).
             05 00MOR047CC              PIC X(003).
             05 00MOR048CC              PIC X(003).
             05 00MOR049CC              PIC X(003).
             05 00MOR050CC              PIC X(003).
             05 00MOR051CC              PIC X(003).
             05 00MOR052CC              PIC X(003).
             05 00MOR053CC              PIC X(003).
             05 00MOR054CC              PIC X(003).
             05 00MOR055CC              PIC X(003).
             05 00MOR056CC              PIC X(003).
             05 00MOR057CC              PIC X(003).
             05 00MOR058CC              PIC X(003).
             05 00MOR059CC              PIC X(003).
             05 00MOR060CC              PIC X(003).
             05 00MOR061CC              PIC X(003).
             05 00MOR062CC              PIC X(003).
             05 00MOR063CC              PIC X(003).
             05 00MOR064CC              PIC X(003).
             05 00MOR065CC              PIC X(003).
             05 00MOR066CC              PIC X(003).
             05 00MOR067CC              PIC X(003).
             05 00MOR068CC              PIC X(003).
             05 00MOR069CC              PIC X(003).
             05 00MOR070CC              PIC X(003).
             05 00MOR071CC              PIC X(003).
             05 00MOR072CC              PIC X(003).
             05 00MOR073CC              PIC X(003).
             05 00MOR074CC              PIC X(003).
             05 00MOR075CC              PIC X(003).
             05 00MOR076CC              PIC X(003).
             05 00MOR077CC              PIC X(003).
             05 00MOR078CC              PIC X(003).
             05 00MOR079CC              PIC X(003).
             05 00MOR080CC              PIC X(006).
             05 00MOR081CC              PIC X(006).
             05 00MOR082CC              PIC X(006).
             05 00MOR083CC              PIC X(006).
             05 00MOR084CC              PIC X(006).
             05 00MOR085CC              PIC X(006).
             05 00MOR086CC              PIC X(006).
             05 00MOR087CC              PIC X(006).
             05 00MOR088CC              PIC X(006).
             05 00MOR089CC              PIC X(006).
             05 00MOR090CC              PIC X(006).
             05 00MOR091CC              PIC X(006).
             05 00MOR092CC              PIC X(006).
             05 00MOR093CC              PIC X(006).
             05 00MOR094CC              PIC X(006).
             05 00MOR095CC              PIC X(006).
             05 00MOR096CC              PIC X(006).
             05 00MOR097CC              PIC X(006).
             05 00MOR098CC              PIC X(006).
             05 00MOR099CC              PIC X(006).
             05 00MOR100CC              PIC X(006).
             05 00MOR101CC              PIC X(006).
             05 00MOR102CC              PIC X(006).
             05 00MOR103CC              PIC X(006).
             05 00MOR104CC              PIC X(049).

          03 VAR-00MORCO.
             05 00MOR001CO              PIC X(003).
             05 00MOR002CO              PIC X(003).
             05 00MOR003CO              PIC X(003).
             05 00MOR004CO              PIC X(003).
             05 00MOR005CO              PIC X(003).
             05 00MOR006CO              PIC X(003).
             05 00MOR007CO              PIC X(003).
             05 00MOR008CO              PIC X(003).
             05 00MOR009CO              PIC X(003).
             05 00MOR010CO              PIC X(003).
             05 00MOR011CO              PIC X(003).
             05 00MOR012CO              PIC X(003).
             05 00MOR013CO              PIC X(003).
             05 00MOR014CO              PIC X(003).
             05 00MOR015CO              PIC X(003).
             05 00MOR016CO              PIC X(003).
             05 00MOR017CO              PIC X(003).
             05 00MOR018CO              PIC X(003).
             05 00MOR019CO              PIC X(003).
             05 00MOR020CO              PIC X(003).
             05 00MOR021CO              PIC X(003).
             05 00MOR022CO              PIC X(003).
             05 00MOR023CO              PIC X(003).
             05 00MOR024CO              PIC X(003).
             05 00MOR025CO              PIC X(003).
             05 00MOR026CO              PIC X(003).
             05 00MOR027CO              PIC X(003).
             05 00MOR028CO              PIC X(003).
             05 00MOR029CO              PIC X(003).
             05 00MOR030CO              PIC X(003).
             05 00MOR031CO              PIC X(003).
             05 00MOR032CO              PIC X(003).
             05 00MOR033CO              PIC X(003).
             05 00MOR034CO              PIC X(003).
             05 00MOR035CO              PIC X(003).
             05 00MOR036CO              PIC X(003).
             05 00MOR037CO              PIC X(003).
             05 00MOR038CO              PIC X(003).
             05 00MOR039CO              PIC X(003).
             05 00MOR040CO              PIC X(003).
             05 00MOR041CO              PIC X(003).
             05 00MOR042CO              PIC X(003).
             05 00MOR043CO              PIC X(003).
             05 00MOR044CO              PIC X(003).
             05 00MOR045CO              PIC X(003).
             05 00MOR046CO              PIC X(003).
             05 00MOR047CO              PIC X(003).
             05 00MOR048CO              PIC X(003).
             05 00MOR049CO              PIC X(003).
             05 00MOR050CO              PIC X(003).
             05 00MOR051CO              PIC X(003).
             05 00MOR052CO              PIC X(003).
             05 00MOR053CO              PIC X(003).
             05 00MOR054CO              PIC X(003).
             05 00MOR055CO              PIC X(003).
             05 00MOR056CO              PIC X(003).
             05 00MOR057CO              PIC X(003).
             05 00MOR058CO              PIC X(003).
             05 00MOR059CO              PIC X(003).
             05 00MOR060CO              PIC X(003).
             05 00MOR061CO              PIC X(003).
             05 00MOR062CO              PIC X(003).
             05 00MOR063CO              PIC X(003).
             05 00MOR064CO              PIC X(003).
             05 00MOR065CO              PIC X(003).
             05 00MOR066CO              PIC X(003).
             05 00MOR067CO              PIC X(003).
             05 00MOR068CO              PIC X(003).
             05 00MOR069CO              PIC X(003).
             05 00MOR070CO              PIC X(003).
             05 00MOR071CO              PIC X(003).
             05 00MOR072CO              PIC X(003).
             05 00MOR073CO              PIC X(003).
             05 00MOR074CO              PIC X(003).
             05 00MOR075CO              PIC X(003).
             05 00MOR076CO              PIC X(003).
             05 00MOR077CO              PIC X(003).
             05 00MOR078CO              PIC X(003).
             05 00MOR079CO              PIC X(003).
             05 00MOR080CO              PIC X(006).
             05 00MOR081CO              PIC X(006).
             05 00MOR082CO              PIC X(006).
             05 00MOR083CO              PIC X(006).
             05 00MOR084CO              PIC X(006).
             05 00MOR085CO              PIC X(006).
             05 00MOR086CO              PIC X(006).
             05 00MOR087CO              PIC X(006).
             05 00MOR088CO              PIC X(006).
             05 00MOR089CO              PIC X(006).
             05 00MOR090CO              PIC X(006).
             05 00MOR091CO              PIC X(006).
             05 00MOR092CO              PIC X(006).
             05 00MOR093CO              PIC X(006).
             05 00MOR094CO              PIC X(006).
             05 00MOR095CO              PIC X(006).
             05 00MOR096CO              PIC X(006).
             05 00MOR097CO              PIC X(006).
             05 00MOR098CO              PIC X(006).
             05 00MOR099CO              PIC X(006).
             05 00MOR100CO              PIC X(006).
             05 00MOR101CO              PIC X(006).
             05 00MOR102CO              PIC X(006).
             05 00MOR103CO              PIC X(006).
             05 00MOR104CO              PIC X(049).

       01 VAR-00ACP.
          03 00ACPLLV                   PIC X(038).
          03 00ACPPTF                   PIC X(002).
          03 00ACP001                   PIC 9(003).
          03 00ACP002                   PIC 9(003).
          03 00ACP003                   PIC 9(003).
          03 00ACP004                   PIC 9(003).
          03 00ACP005                   PIC 9(003).
          03 00ACP006                   PIC 9(003).
          03 00ACP007                   PIC 9(003).
          03 00ACP008                   PIC 9(003).
          03 00ACP009                   PIC 9(003).
          03 00ACP010                   PIC 9(003).
          03 00ACP011                   PIC 9(003).
          03 00ACP012                   PIC 9(003).
          03 00ACP013                   PIC 9(003).
          03 00ACP014                   PIC 9(003).
          03 00ACP015                   PIC 9(003).
          03 00ACP016                   PIC 9(003).
          03 00ACP017                   PIC 9(003).
          03 00ACP018                   PIC 9(003).
          03 00ACP019                   PIC 9(003).
          03 00ACP020                   PIC 9(003).
          03 00ACP021                   PIC 9(003).
          03 00ACP022                   PIC 9(003).
          03 00ACP023                   PIC 9(003).
          03 00ACP024                   PIC 9(003).
          03 00ACP025                   PIC 9(003).
          03 00ACP026                   PIC 9(003).
          03 00ACP027                   PIC 9(003).
          03 00ACP028                   PIC 9(003).
          03 00ACP029                   PIC 9(003).
          03 00ACP030                   PIC 9(003).
          03 00ACP031                   PIC 9(003).
          03 00ACP032                   PIC 9(003).
          03 00ACP033                   PIC 9(003).
          03 00ACP034                   PIC 9(003).
          03 00ACP035                   PIC 9(003).
          03 00ACP036                   PIC 9(003).
          03 00ACP037                   PIC 9(003).
          03 00ACP038                   PIC 9(003).
          03 00ACP039                   PIC 9(003).
          03 00ACP040                   PIC 9(003).
          03 00ACP041                   PIC 9(003).
          03 00ACP042                   PIC 9(003).

       01 VARIABLES-00ACP.
          03 VAR-00ACPRO.
             05 00ACP001RO              PIC X(003).
             05 00ACP002RO              PIC X(003).
             05 00ACP003RO              PIC X(003).
             05 00ACP004RO              PIC X(003).
             05 00ACP005RO              PIC X(003).
             05 00ACP006RO              PIC X(003).
             05 00ACP007RO              PIC X(003).
             05 00ACP008RO              PIC X(003).
             05 00ACP009RO              PIC X(003).
             05 00ACP010RO              PIC X(003).
             05 00ACP011RO              PIC X(003).
             05 00ACP012RO              PIC X(003).
             05 00ACP013RO              PIC X(003).
             05 00ACP014RO              PIC X(003).
             05 00ACP015RO              PIC X(003).
             05 00ACP016RO              PIC X(003).
             05 00ACP017RO              PIC X(003).
             05 00ACP018RO              PIC X(003).
             05 00ACP019RO              PIC X(003).
             05 00ACP020RO              PIC X(003).
             05 00ACP021RO              PIC X(003).
             05 00ACP022RO              PIC X(003).
             05 00ACP023RO              PIC X(003).
             05 00ACP024RO              PIC X(003).
             05 00ACP025RO              PIC X(003).
             05 00ACP026RO              PIC X(003).
             05 00ACP027RO              PIC X(003).
             05 00ACP028RO              PIC X(003).
             05 00ACP029RO              PIC X(003).
             05 00ACP030RO              PIC X(003).
             05 00ACP031RO              PIC X(003).
             05 00ACP032RO              PIC X(003).
             05 00ACP033RO              PIC X(003).
             05 00ACP034RO              PIC X(003).
             05 00ACP035RO              PIC X(003).
             05 00ACP036RO              PIC X(003).
             05 00ACP037RO              PIC X(003).
             05 00ACP038RO              PIC X(003).
             05 00ACP039RO              PIC X(003).
             05 00ACP040RO              PIC X(003).
             05 00ACP041RO              PIC X(003).
             05 00ACP042RO              PIC X(003).

          03 VAR-00ACPIN.
             05 00ACP001IN              PIC X(003).
             05 00ACP002IN              PIC X(003).
             05 00ACP003IN              PIC X(003).
             05 00ACP004IN              PIC X(003).
             05 00ACP005IN              PIC X(003).
             05 00ACP006IN              PIC X(003).
             05 00ACP007IN              PIC X(003).
             05 00ACP008IN              PIC X(003).
             05 00ACP009IN              PIC X(003).
             05 00ACP010IN              PIC X(003).
             05 00ACP011IN              PIC X(003).
             05 00ACP012IN              PIC X(003).
             05 00ACP013IN              PIC X(003).
             05 00ACP014IN              PIC X(003).
             05 00ACP015IN              PIC X(003).
             05 00ACP016IN              PIC X(003).
             05 00ACP017IN              PIC X(003).
             05 00ACP018IN              PIC X(003).
             05 00ACP019IN              PIC X(003).
             05 00ACP020IN              PIC X(003).
             05 00ACP021IN              PIC X(003).
             05 00ACP022IN              PIC X(003).
             05 00ACP023IN              PIC X(003).
             05 00ACP024IN              PIC X(003).
             05 00ACP025IN              PIC X(003).
             05 00ACP026IN              PIC X(003).
             05 00ACP027IN              PIC X(003).
             05 00ACP028IN              PIC X(003).
             05 00ACP029IN              PIC X(003).
             05 00ACP030IN              PIC X(003).
             05 00ACP031IN              PIC X(003).
             05 00ACP032IN              PIC X(003).
             05 00ACP033IN              PIC X(003).
             05 00ACP034IN              PIC X(003).
             05 00ACP035IN              PIC X(003).
             05 00ACP036IN              PIC X(003).
             05 00ACP037IN              PIC X(003).
             05 00ACP038IN              PIC X(003).
             05 00ACP039IN              PIC X(003).
             05 00ACP040IN              PIC X(003).
             05 00ACP041IN              PIC X(003).
             05 00ACP042IN              PIC X(003).

          03 VAR-00ACPVE.
             05 00ACP001VE              PIC X(003).
             05 00ACP002VE              PIC X(003).
             05 00ACP003VE              PIC X(003).
             05 00ACP004VE              PIC X(003).
             05 00ACP005VE              PIC X(003).
             05 00ACP006VE              PIC X(003).
             05 00ACP007VE              PIC X(003).
             05 00ACP008VE              PIC X(003).
             05 00ACP009VE              PIC X(003).
             05 00ACP010VE              PIC X(003).
             05 00ACP011VE              PIC X(003).
             05 00ACP012VE              PIC X(003).
             05 00ACP013VE              PIC X(003).
             05 00ACP014VE              PIC X(003).
             05 00ACP015VE              PIC X(003).
             05 00ACP016VE              PIC X(003).
             05 00ACP017VE              PIC X(003).
             05 00ACP018VE              PIC X(003).
             05 00ACP019VE              PIC X(003).
             05 00ACP020VE              PIC X(003).
             05 00ACP021VE              PIC X(003).
             05 00ACP022VE              PIC X(003).
             05 00ACP023VE              PIC X(003).
             05 00ACP024VE              PIC X(003).
             05 00ACP025VE              PIC X(003).
             05 00ACP026VE              PIC X(003).
             05 00ACP027VE              PIC X(003).
             05 00ACP028VE              PIC X(003).
             05 00ACP029VE              PIC X(003).
             05 00ACP030VE              PIC X(003).
             05 00ACP031VE              PIC X(003).
             05 00ACP032VE              PIC X(003).
             05 00ACP033VE              PIC X(003).
             05 00ACP034VE              PIC X(003).
             05 00ACP035VE              PIC X(003).
             05 00ACP036VE              PIC X(003).
             05 00ACP037VE              PIC X(003).
             05 00ACP038VE              PIC X(003).
             05 00ACP039VE              PIC X(003).
             05 00ACP040VE              PIC X(003).
             05 00ACP041VE              PIC X(003).
             05 00ACP042VE              PIC X(003).

          03 VAR-00ACPHP.
             05 00ACP001HP              PIC X(003).
             05 00ACP002HP              PIC X(003).
             05 00ACP003HP              PIC X(003).
             05 00ACP004HP              PIC X(003).
             05 00ACP005HP              PIC X(003).
             05 00ACP006HP              PIC X(003).
             05 00ACP007HP              PIC X(003).
             05 00ACP008HP              PIC X(003).
             05 00ACP009HP              PIC X(003).
             05 00ACP010HP              PIC X(003).
             05 00ACP011HP              PIC X(003).
             05 00ACP012HP              PIC X(003).
             05 00ACP013HP              PIC X(003).
             05 00ACP014HP              PIC X(003).
             05 00ACP015HP              PIC X(003).
             05 00ACP016HP              PIC X(003).
             05 00ACP017HP              PIC X(003).
             05 00ACP018HP              PIC X(003).
             05 00ACP019HP              PIC X(003).
             05 00ACP020HP              PIC X(003).
             05 00ACP021HP              PIC X(003).
             05 00ACP022HP              PIC X(003).
             05 00ACP023HP              PIC X(003).
             05 00ACP024HP              PIC X(003).
             05 00ACP025HP              PIC X(003).
             05 00ACP026HP              PIC X(003).
             05 00ACP027HP              PIC X(003).
             05 00ACP028HP              PIC X(003).
             05 00ACP029HP              PIC X(003).
             05 00ACP030HP              PIC X(003).
             05 00ACP031HP              PIC X(003).
             05 00ACP032HP              PIC X(003).
             05 00ACP033HP              PIC X(003).
             05 00ACP034HP              PIC X(003).
             05 00ACP035HP              PIC X(003).
             05 00ACP036HP              PIC X(003).
             05 00ACP037HP              PIC X(003).
             05 00ACP038HP              PIC X(003).
             05 00ACP039HP              PIC X(003).
             05 00ACP040HP              PIC X(003).
             05 00ACP041HP              PIC X(003).
             05 00ACP042HP              PIC X(003).

          03 VAR-00ACPOT.
             05 00ACP001OT              PIC X(003).
             05 00ACP002OT              PIC X(003).
             05 00ACP003OT              PIC X(003).
             05 00ACP004OT              PIC X(003).
             05 00ACP005OT              PIC X(003).
             05 00ACP006OT              PIC X(003).
             05 00ACP007OT              PIC X(003).
             05 00ACP008OT              PIC X(003).
             05 00ACP009OT              PIC X(003).
             05 00ACP010OT              PIC X(003).
             05 00ACP011OT              PIC X(003).
             05 00ACP012OT              PIC X(003).
             05 00ACP013OT              PIC X(003).
             05 00ACP014OT              PIC X(003).
             05 00ACP015OT              PIC X(003).
             05 00ACP016OT              PIC X(003).
             05 00ACP017OT              PIC X(003).
             05 00ACP018OT              PIC X(003).
             05 00ACP019OT              PIC X(003).
             05 00ACP020OT              PIC X(003).
             05 00ACP021OT              PIC X(003).
             05 00ACP022OT              PIC X(003).
             05 00ACP023OT              PIC X(003).
             05 00ACP024OT              PIC X(003).
             05 00ACP025OT              PIC X(003).
             05 00ACP026OT              PIC X(003).
             05 00ACP027OT              PIC X(003).
             05 00ACP028OT              PIC X(003).
             05 00ACP029OT              PIC X(003).
             05 00ACP030OT              PIC X(003).
             05 00ACP031OT              PIC X(003).
             05 00ACP032OT              PIC X(003).
             05 00ACP033OT              PIC X(003).
             05 00ACP034OT              PIC X(003).
             05 00ACP035OT              PIC X(003).
             05 00ACP036OT              PIC X(003).
             05 00ACP037OT              PIC X(003).
             05 00ACP038OT              PIC X(003).
             05 00ACP039OT              PIC X(003).
             05 00ACP040OT              PIC X(003).
             05 00ACP041OT              PIC X(003).
             05 00ACP042OT              PIC X(003).

          03 VAR-00ACPCC.
             05 00ACP001CC              PIC X(003).
             05 00ACP002CC              PIC X(003).
             05 00ACP003CC              PIC X(003).
             05 00ACP004CC              PIC X(003).
             05 00ACP005CC              PIC X(003).
             05 00ACP006CC              PIC X(003).
             05 00ACP007CC              PIC X(003).
             05 00ACP008CC              PIC X(003).
             05 00ACP009CC              PIC X(003).
             05 00ACP010CC              PIC X(003).
             05 00ACP011CC              PIC X(003).
             05 00ACP012CC              PIC X(003).
             05 00ACP013CC              PIC X(003).
             05 00ACP014CC              PIC X(003).
             05 00ACP015CC              PIC X(003).
             05 00ACP016CC              PIC X(003).
             05 00ACP017CC              PIC X(003).
             05 00ACP018CC              PIC X(003).
             05 00ACP019CC              PIC X(003).
             05 00ACP020CC              PIC X(003).
             05 00ACP021CC              PIC X(003).
             05 00ACP022CC              PIC X(003).
             05 00ACP023CC              PIC X(003).
             05 00ACP024CC              PIC X(003).
             05 00ACP025CC              PIC X(003).
             05 00ACP026CC              PIC X(003).
             05 00ACP027CC              PIC X(003).
             05 00ACP028CC              PIC X(003).
             05 00ACP029CC              PIC X(003).
             05 00ACP030CC              PIC X(003).
             05 00ACP031CC              PIC X(003).
             05 00ACP032CC              PIC X(003).
             05 00ACP033CC              PIC X(003).
             05 00ACP034CC              PIC X(003).
             05 00ACP035CC              PIC X(003).
             05 00ACP036CC              PIC X(003).
             05 00ACP037CC              PIC X(003).
             05 00ACP038CC              PIC X(003).
             05 00ACP039CC              PIC X(003).
             05 00ACP040CC              PIC X(003).
             05 00ACP041CC              PIC X(003).
             05 00ACP042CC              PIC X(003).

          03 VAR-00ACPCO.
             05 00ACP001CO              PIC X(003).
             05 00ACP002CO              PIC X(003).
             05 00ACP003CO              PIC X(003).
             05 00ACP004CO              PIC X(003).
             05 00ACP005CO              PIC X(003).
             05 00ACP006CO              PIC X(003).
             05 00ACP007CO              PIC X(003).
             05 00ACP008CO              PIC X(003).
             05 00ACP009CO              PIC X(003).
             05 00ACP010CO              PIC X(003).
             05 00ACP011CO              PIC X(003).
             05 00ACP012CO              PIC X(003).
             05 00ACP013CO              PIC X(003).
             05 00ACP014CO              PIC X(003).
             05 00ACP015CO              PIC X(003).
             05 00ACP016CO              PIC X(003).
             05 00ACP017CO              PIC X(003).
             05 00ACP018CO              PIC X(003).
             05 00ACP019CO              PIC X(003).
             05 00ACP020CO              PIC X(003).
             05 00ACP021CO              PIC X(003).
             05 00ACP022CO              PIC X(003).
             05 00ACP023CO              PIC X(003).
             05 00ACP024CO              PIC X(003).
             05 00ACP025CO              PIC X(003).
             05 00ACP026CO              PIC X(003).
             05 00ACP027CO              PIC X(003).
             05 00ACP028CO              PIC X(003).
             05 00ACP029CO              PIC X(003).
             05 00ACP030CO              PIC X(003).
             05 00ACP031CO              PIC X(003).
             05 00ACP032CO              PIC X(003).
             05 00ACP033CO              PIC X(003).
             05 00ACP034CO              PIC X(003).
             05 00ACP035CO              PIC X(003).
             05 00ACP036CO              PIC X(003).
             05 00ACP037CO              PIC X(003).
             05 00ACP038CO              PIC X(003).
             05 00ACP039CO              PIC X(003).
             05 00ACP040CO              PIC X(003).
             05 00ACP041CO              PIC X(003).
             05 00ACP042CO              PIC X(003).

          03 VAR-00ACPAH.
             05 00ACP001AH              PIC X(003).
             05 00ACP002AH              PIC X(003).
             05 00ACP003AH              PIC X(003).
             05 00ACP004AH              PIC X(003).
             05 00ACP005AH              PIC X(003).
             05 00ACP006AH              PIC X(003).
             05 00ACP007AH              PIC X(003).
             05 00ACP008AH              PIC X(003).
             05 00ACP009AH              PIC X(003).
             05 00ACP010AH              PIC X(003).
             05 00ACP011AH              PIC X(003).
             05 00ACP012AH              PIC X(003).
             05 00ACP013AH              PIC X(003).
             05 00ACP014AH              PIC X(003).
             05 00ACP015AH              PIC X(003).
             05 00ACP016AH              PIC X(003).
             05 00ACP017AH              PIC X(003).
             05 00ACP018AH              PIC X(003).
             05 00ACP019AH              PIC X(003).
             05 00ACP020AH              PIC X(003).
             05 00ACP021AH              PIC X(003).
             05 00ACP022AH              PIC X(003).
             05 00ACP023AH              PIC X(003).
             05 00ACP024AH              PIC X(003).
             05 00ACP025AH              PIC X(003).
             05 00ACP026AH              PIC X(003).
             05 00ACP027AH              PIC X(003).
             05 00ACP028AH              PIC X(003).
             05 00ACP029AH              PIC X(003).
             05 00ACP030AH              PIC X(003).
             05 00ACP031AH              PIC X(003).
             05 00ACP032AH              PIC X(003).
             05 00ACP033AH              PIC X(003).
             05 00ACP034AH              PIC X(003).
             05 00ACP035AH              PIC X(003).
             05 00ACP036AH              PIC X(003).
             05 00ACP037AH              PIC X(003).
             05 00ACP038AH              PIC X(003).
             05 00ACP039AH              PIC X(003).
             05 00ACP040AH              PIC X(003).
             05 00ACP041AH              PIC X(003).
             05 00ACP042AH              PIC X(003).

          03 VAR-00ACPCT.
             05 00ACP001CT              PIC X(003).
             05 00ACP002CT              PIC X(003).
             05 00ACP003CT              PIC X(003).
             05 00ACP004CT              PIC X(003).
             05 00ACP005CT              PIC X(003).
             05 00ACP006CT              PIC X(003).
             05 00ACP007CT              PIC X(003).
             05 00ACP008CT              PIC X(003).
             05 00ACP009CT              PIC X(003).
             05 00ACP010CT              PIC X(003).
             05 00ACP011CT              PIC X(003).
             05 00ACP012CT              PIC X(003).
             05 00ACP013CT              PIC X(003).
             05 00ACP014CT              PIC X(003).
             05 00ACP015CT              PIC X(003).
             05 00ACP016CT              PIC X(003).
             05 00ACP017CT              PIC X(003).
             05 00ACP018CT              PIC X(003).
             05 00ACP019CT              PIC X(003).
             05 00ACP020CT              PIC X(003).
             05 00ACP021CT              PIC X(003).
             05 00ACP022CT              PIC X(003).
             05 00ACP023CT              PIC X(003).
             05 00ACP024CT              PIC X(003).
             05 00ACP025CT              PIC X(003).
             05 00ACP026CT              PIC X(003).
             05 00ACP027CT              PIC X(003).
             05 00ACP028CT              PIC X(003).
             05 00ACP029CT              PIC X(003).
             05 00ACP030CT              PIC X(003).
             05 00ACP031CT              PIC X(003).
             05 00ACP032CT              PIC X(003).
             05 00ACP033CT              PIC X(003).
             05 00ACP034CT              PIC X(003).
             05 00ACP035CT              PIC X(003).
             05 00ACP036CT              PIC X(003).
             05 00ACP037CT              PIC X(003).
             05 00ACP038CT              PIC X(003).
             05 00ACP039CT              PIC X(003).
             05 00ACP040CT              PIC X(003).
             05 00ACP041CT              PIC X(003).
             05 00ACP042CT              PIC X(003).

       01 VAR-00EXP.
          03 00EXPLLV                   PIC X(038).
          03 00EXPPTF                   PIC X(002).
          03 00EXP001                   PIC 9(003).
          03 00EXP002                   PIC 9(003).
          03 00EXP003                   PIC 9(003).
          03 00EXP004                   PIC 9(003).
          03 00EXP005                   PIC 9(003).
          03 00EXP006                   PIC 9(003).
          03 00EXP007                   PIC 9(003).
          03 00EXP008                   PIC 9(003).

       01 VARIABLES-00EXP.
          03 VAR-00EXPRO.
             05 00EXP001RO              PIC X(003).
             05 00EXP002RO              PIC X(003).
             05 00EXP003RO              PIC X(003).
             05 00EXP004RO              PIC X(003).
             05 00EXP005RO              PIC X(003).
             05 00EXP006RO              PIC X(003).
             05 00EXP007RO              PIC X(003).
             05 00EXP008RO              PIC X(003).

          03 VAR-00EXPIN.
             05 00EXP001IN              PIC X(003).
             05 00EXP002IN              PIC X(003).
             05 00EXP003IN              PIC X(003).
             05 00EXP004IN              PIC X(003).
             05 00EXP005IN              PIC X(003).
             05 00EXP006IN              PIC X(003).
             05 00EXP007IN              PIC X(003).
             05 00EXP008IN              PIC X(003).

          03 VAR-00EXPVE.
             05 00EXP001VE              PIC X(003).
             05 00EXP002VE              PIC X(003).
             05 00EXP003VE              PIC X(003).
             05 00EXP004VE              PIC X(003).
             05 00EXP005VE              PIC X(003).
             05 00EXP006VE              PIC X(003).
             05 00EXP007VE              PIC X(003).
             05 00EXP008VE              PIC X(003).

          03 VAR-00EXPHP.
             05 00EXP001HP              PIC X(003).
             05 00EXP002HP              PIC X(003).
             05 00EXP003HP              PIC X(003).
             05 00EXP004HP              PIC X(003).
             05 00EXP005HP              PIC X(003).
             05 00EXP006HP              PIC X(003).
             05 00EXP007HP              PIC X(003).
             05 00EXP008HP              PIC X(003).

          03 VAR-00EXPOT.
             05 00EXP001OT              PIC X(003).
             05 00EXP002OT              PIC X(003).
             05 00EXP003OT              PIC X(003).
             05 00EXP004OT              PIC X(003).
             05 00EXP005OT              PIC X(003).
             05 00EXP006OT              PIC X(003).
             05 00EXP007OT              PIC X(003).
             05 00EXP008OT              PIC X(003).

          03 VAR-00EXPCC.
             05 00EXP001CC              PIC X(003).
             05 00EXP002CC              PIC X(003).
             05 00EXP003CC              PIC X(003).
             05 00EXP004CC              PIC X(003).
             05 00EXP005CC              PIC X(003).
             05 00EXP006CC              PIC X(003).
             05 00EXP007CC              PIC X(003).
             05 00EXP008CC              PIC X(003).

          03 VAR-00EXPCO.
             05 00EXP001CO              PIC X(003).
             05 00EXP002CO              PIC X(003).
             05 00EXP003CO              PIC X(003).
             05 00EXP004CO              PIC X(003).
             05 00EXP005CO              PIC X(003).
             05 00EXP006CO              PIC X(003).
             05 00EXP007CO              PIC X(003).
             05 00EXP008CO              PIC X(003).

          03 VAR-00EXPAH.
             05 00EXP001AH              PIC X(003).
             05 00EXP002AH              PIC X(003).
             05 00EXP003AH              PIC X(003).
             05 00EXP004AH              PIC X(003).
             05 00EXP005AH              PIC X(003).
             05 00EXP006AH              PIC X(003).
             05 00EXP007AH              PIC X(003).
             05 00EXP008AH              PIC X(003).

          03 VAR-00EXPCT.
             05 00EXP001CT              PIC X(003).
             05 00EXP002CT              PIC X(003).
             05 00EXP003CT              PIC X(003).
             05 00EXP004CT              PIC X(003).
             05 00EXP005CT              PIC X(003).
             05 00EXP006CT              PIC X(003).
             05 00EXP007CT              PIC X(003).
             05 00EXP008CT              PIC X(003).

       01 VAR-00DEM.
          03 00DEMTIP                   PIC 9(001).
          03 00DEMNRO                   PIC 9(011).
          03 00DEM001                   PIC 9(002).
          03 00DEM002                   PIC 9(002).
          03 00DEM003                   PIC 9(003).
          03 00DEM006                   PIC 9(002).
          03 00DEM007                   PIC 9(002).
          03 00DEM012                   PIC 9(003).
          03 00DEM016                   PIC 9(002).
          03 00DEM026                   PIC 9(002).
          03 00DEM027                   PIC 9(003).
CARA01    03 00DEM030                   PIC 9(004).                     RMB0004
ACIEAA    03 00DEM031                   PIC 9(011).                     RMB0006

       01 TAB-00DEM.
          02 TABLA-ENTIDADES-00DEM OCCURS 200 TIMES.
             03 TAB-00DEM027            PIC 9(11).

       01 VAR-00END.
         02 00END-SALIDA.
          03 00ENDLLV                   PIC X(038).
          03 00ENDPTF                   PIC X(002).
          03 00END001 OCCURS 13 TIMES   PIC 9(011).
          03 00END002 OCCURS 13 TIMES   PIC 9(011).
          03 00END003 OCCURS 13 TIMES   PIC 9(011).
          03 00END004 OCCURS 13 TIMES   PIC 9(011).
          03 00END005                   PIC 99999999.99.
          03 00END006                   PIC 99999999.99.
          03 00END007                   PIC 99999999.99.
          03 00END008                   PIC 99999999.99.
          03 00END009                   PIC 99999999.99.
          03 00END010                   PIC 99999999.99.
          03 00END011                   PIC 99999999.99.
          03 00END012                   PIC 99999999.99.
          03 00END013                   PIC 99999999.99.
          03 00END014                   PIC 99999999.99.
          03 00END015                   PIC 99999999.99.
          03 00END016                   PIC 99999999.99.
          03 00END017                   PIC 99999999.99.
          03 00END018                   PIC 99999999.99.
          03 00END019                   PIC 99999999.99.
          03 00END020                   PIC 99999999.99.
          03 00END021                   PIC 99999999.99.
          03 00END022                   PIC 99999999.99.
          03 00END023                   PIC 99999999.99.
          03 00END024                   PIC 99999999.99.
          03 00END025                   PIC 99999999.99.
          03 00END026                   PIC 99999999.99.
          03 00END027                   PIC 99999999.99.
          03 00END028                   PIC 99999999.99.
          03 00END029                   PIC 99999999.99.
          03 00END030                   PIC 99999999.99.
          03 00END031                   PIC 99999999.99.
          03 00END032                   PIC 99999999.99.
          03 00END033                   PIC 99999999.99.
          03 00END034                   PIC 99999999.99.
          03 00END035                   PIC 99999999.99.
          03 00END036                   PIC 99999999.99.
          03 00END037                   PIC 99999999.99.
          03 00END038                   PIC 99999999.99.
          03 00END039                   PIC 99999999.99.
          03 00END040                   PIC 99999999.99.
          03 00END041                   PIC 99999999.99.
          03 00END042                   PIC 99999999.99.
          03 00END043                   PIC 99999999.99.
          03 00END044                   PIC 99999999.99.
          03 00END045                   PIC 99999999.99.
          03 00END046                   PIC 99999999.99.
          03 00END047                   PIC 99999999.99.
          03 00END048                   PIC 99999999.99.
          03 00END049                   PIC 99999999.99.
         02 00END-VALORES.
          03 00ENDV05                   PIC 9(008)V99.
          03 00ENDV06                   PIC 9(008)V99.
          03 00ENDV07                   PIC 9(008)V99.
          03 00ENDV08                   PIC 9(008)V99.
          03 00ENDV09                   PIC 9(008)V99.
          03 00ENDV10                   PIC 9(008)V99.
          03 00ENDV11                   PIC 9(008)V99.
          03 00ENDV12                   PIC 9(008)V99.
          03 00ENDV13                   PIC 9(008)V99.
          03 00ENDV14                   PIC 9(008)V99.
          03 00ENDV15                   PIC 9(008)V99.
          03 00ENDV16                   PIC 9(008)V99.
          03 00ENDV17                   PIC 9(008)V99.
          03 00ENDV18                   PIC 9(008)V99.
          03 00ENDV19                   PIC 9(008)V99.
          03 00ENDV20                   PIC 9(008)V99.
          03 00ENDV21                   PIC 9(008)V99.
          03 00ENDV22                   PIC 9(008)V99.
          03 00ENDV23                   PIC 9(008)V99.
          03 00ENDV24                   PIC 9(008)V99.
          03 00ENDV25                   PIC 9(008)V99.
          03 00ENDV26                   PIC 9(008)V99.
          03 00ENDV27                   PIC 9(008)V99.
          03 00ENDV28                   PIC 9(008)V99.
          03 00ENDV29                   PIC 9(008)V99.
          03 00ENDV30                   PIC 9(008)V99.
          03 00ENDV31                   PIC 9(008)V99.
          03 00ENDV32                   PIC 9(008)V99.
          03 00ENDV33                   PIC 9(008)V99.
          03 00ENDV34                   PIC 9(008)V99.
          03 00ENDV35                   PIC 9(008)V99.
          03 00ENDV36                   PIC 9(008)V99.
          03 00ENDV37                   PIC 9(008)V99.
          03 00ENDV38                   PIC 9(008)V99.
          03 00ENDV39                   PIC 9(008)V99.
          03 00ENDV40                   PIC 9(008)V99.
          03 00ENDV41                   PIC 9(008)V99.
          03 00ENDV42                   PIC 9(008)V99.
          03 00ENDV43                   PIC 9(008)V99.
          03 00ENDV44                   PIC 9(008)V99.
          03 00ENDV45                   PIC 9(008)V99.
          03 00ENDV46                   PIC 9(008)V99.
          03 00ENDV47                   PIC 9(008)V99.
          03 00ENDV48                   PIC 9(008)V99.
          03 00ENDV49                   PIC 9(008)V99.

       01 VARIABLES-00END.
          03 VAR-00ENDRO.
             05 00END001RO OCCURS 13 TIMES  PIC 9(011).
             05 00END002RO OCCURS 13 TIMES  PIC 9(011).
             05 00END003RO OCCURS 13 TIMES  PIC 9(011).
             05 00END004RO OCCURS 13 TIMES  PIC 9(011).
             05 00END005RO              PIC X(011).
             05 00END006RO              PIC X(011).
             05 00END007RO              PIC X(011).
             05 00END008RO              PIC X(011).
             05 00END009RO              PIC X(011).
             05 00END010RO              PIC X(011).
             05 00END011RO              PIC X(011).
             05 00END012RO              PIC X(011).
             05 00END013RO              PIC X(011).
             05 00END014RO              PIC X(011).
             05 00END015RO              PIC X(011).
             05 00END016RO              PIC X(011).
             05 00END017RO              PIC X(011).
             05 00END018RO              PIC X(011).
             05 00END019RO              PIC X(011).
             05 00END020RO              PIC X(011).
             05 00END021RO              PIC X(011).
             05 00END022RO              PIC X(011).
             05 00END023RO              PIC X(011).
             05 00END024RO              PIC X(011).
             05 00END025RO              PIC X(011).
             05 00END026RO              PIC X(011).
             05 00END027RO              PIC X(011).
             05 00END028RO              PIC X(011).
             05 00END029RO              PIC X(011).
             05 00END030RO              PIC X(011).
             05 00END031RO              PIC X(011).
             05 00END032RO              PIC X(011).
             05 00END033RO              PIC X(011).
             05 00END034RO              PIC X(011).
             05 00END035RO              PIC X(011).
             05 00END036RO              PIC X(011).
             05 00END037RO              PIC X(011).
             05 00END038RO              PIC X(011).
             05 00END039RO              PIC X(011).
             05 00END040RO              PIC X(011).
             05 00END041RO              PIC X(011).
             05 00END042RO              PIC X(011).
             05 00END043RO              PIC X(011).
             05 00END044RO              PIC X(011).
             05 00END045RO              PIC X(011).
             05 00END046RO              PIC X(011).
             05 00END047RO              PIC X(011).
             05 00END048RO              PIC X(011).
             05 00END049RO              PIC X(011).

          03 VAR-00ENDIN.
             05 00END001IN OCCURS 13 TIMES  PIC 9(011).
             05 00END002IN OCCURS 13 TIMES  PIC 9(011).
             05 00END003IN OCCURS 13 TIMES  PIC 9(011).
             05 00END004IN OCCURS 13 TIMES  PIC 9(011).
             05 00END005IN              PIC X(011).
             05 00END006IN              PIC X(011).
             05 00END007IN              PIC X(011).
             05 00END008IN              PIC X(011).
             05 00END009IN              PIC X(011).
             05 00END010IN              PIC X(011).
             05 00END011IN              PIC X(011).
             05 00END012IN              PIC X(011).
             05 00END013IN              PIC X(011).
             05 00END014IN              PIC X(011).
             05 00END015IN              PIC X(011).
             05 00END016IN              PIC X(011).
             05 00END017IN              PIC X(011).
             05 00END018IN              PIC X(011).
             05 00END019IN              PIC X(011).
             05 00END020IN              PIC X(011).
             05 00END021IN              PIC X(011).
             05 00END022IN              PIC X(011).
             05 00END023IN              PIC X(011).
             05 00END024IN              PIC X(011).
             05 00END025IN              PIC X(011).
             05 00END026IN              PIC X(011).
             05 00END027IN              PIC X(011).
             05 00END028IN              PIC X(011).
             05 00END029IN              PIC X(011).
             05 00END030IN              PIC X(011).
             05 00END031IN              PIC X(011).
             05 00END032IN              PIC X(011).
             05 00END033IN              PIC X(011).
             05 00END034IN              PIC X(011).
             05 00END035IN              PIC X(011).
             05 00END036IN              PIC X(011).
             05 00END037IN              PIC X(011).
             05 00END038IN              PIC X(011).
             05 00END039IN              PIC X(011).
             05 00END040IN              PIC X(011).
             05 00END041IN              PIC X(011).
             05 00END042IN              PIC X(011).
             05 00END043IN              PIC X(011).
             05 00END044IN              PIC X(011).
             05 00END045IN              PIC X(011).
             05 00END046IN              PIC X(011).
             05 00END047IN              PIC X(011).
             05 00END048IN              PIC X(011).
             05 00END049IN              PIC X(011).

          03 VAR-00ENDVE.
             05 00END001VE OCCURS 13 TIMES  PIC 9(011).
             05 00END002VE OCCURS 13 TIMES  PIC 9(011).
             05 00END003VE OCCURS 13 TIMES  PIC 9(011).
             05 00END004VE OCCURS 13 TIMES  PIC 9(011).
             05 00END005VE              PIC X(011).
             05 00END006VE              PIC X(011).
             05 00END007VE              PIC X(011).
             05 00END008VE              PIC X(011).
             05 00END009VE              PIC X(011).
             05 00END010VE              PIC X(011).
             05 00END011VE              PIC X(011).
             05 00END012VE              PIC X(011).
             05 00END013VE              PIC X(011).
             05 00END014VE              PIC X(011).
             05 00END015VE              PIC X(011).
             05 00END016VE              PIC X(011).
             05 00END017VE              PIC X(011).
             05 00END018VE              PIC X(011).
             05 00END019VE              PIC X(011).
             05 00END020VE              PIC X(011).
             05 00END021VE              PIC X(011).
             05 00END022VE              PIC X(011).
             05 00END023VE              PIC X(011).
             05 00END024VE              PIC X(011).
             05 00END025VE              PIC X(011).
             05 00END026VE              PIC X(011).
             05 00END027VE              PIC X(011).
             05 00END028VE              PIC X(011).
             05 00END029VE              PIC X(011).
             05 00END030VE              PIC X(011).
             05 00END031VE              PIC X(011).
             05 00END032VE              PIC X(011).
             05 00END033VE              PIC X(011).
             05 00END034VE              PIC X(011).
             05 00END035VE              PIC X(011).
             05 00END036VE              PIC X(011).
             05 00END037VE              PIC X(011).
             05 00END038VE              PIC X(011).
             05 00END039VE              PIC X(011).
             05 00END040VE              PIC X(011).
             05 00END041VE              PIC X(011).
             05 00END042VE              PIC X(011).
             05 00END043VE              PIC X(011).
             05 00END044VE              PIC X(011).
             05 00END045VE              PIC X(011).
             05 00END046VE              PIC X(011).
             05 00END047VE              PIC X(011).
             05 00END048VE              PIC X(011).
             05 00END049VE              PIC X(011).

          03 VAR-00ENDHP.
             05 00END001HP OCCURS 13 TIMES  PIC 9(011).
             05 00END002HP OCCURS 13 TIMES  PIC 9(011).
             05 00END003HP OCCURS 13 TIMES  PIC 9(011).
             05 00END004HP OCCURS 13 TIMES  PIC 9(011).
             05 00END005HP              PIC X(011).
             05 00END006HP              PIC X(011).
             05 00END007HP              PIC X(011).
             05 00END008HP              PIC X(011).
             05 00END009HP              PIC X(011).
             05 00END010HP              PIC X(011).
             05 00END011HP              PIC X(011).
             05 00END012HP              PIC X(011).
             05 00END013HP              PIC X(011).
             05 00END014HP              PIC X(011).
             05 00END015HP              PIC X(011).
             05 00END016HP              PIC X(011).
             05 00END017HP              PIC X(011).
             05 00END018HP              PIC X(011).
             05 00END019HP              PIC X(011).
             05 00END020HP              PIC X(011).
             05 00END021HP              PIC X(011).
             05 00END022HP              PIC X(011).
             05 00END023HP              PIC X(011).
             05 00END024HP              PIC X(011).
             05 00END025HP              PIC X(011).
             05 00END026HP              PIC X(011).
             05 00END027HP              PIC X(011).
             05 00END028HP              PIC X(011).
             05 00END029HP              PIC X(011).
             05 00END030HP              PIC X(011).
             05 00END031HP              PIC X(011).
             05 00END032HP              PIC X(011).
             05 00END033HP              PIC X(011).
             05 00END034HP              PIC X(011).
             05 00END035HP              PIC X(011).
             05 00END036HP              PIC X(011).
             05 00END037HP              PIC X(011).
             05 00END038HP              PIC X(011).
             05 00END039HP              PIC X(011).
             05 00END040HP              PIC X(011).
             05 00END041HP              PIC X(011).
             05 00END042HP              PIC X(011).
             05 00END043HP              PIC X(011).
             05 00END044HP              PIC X(011).
             05 00END045HP              PIC X(011).
             05 00END046HP              PIC X(011).
             05 00END047HP              PIC X(011).
             05 00END048HP              PIC X(011).
             05 00END049HP              PIC X(011).

          03 VAR-00ENDOT.
             05 00END001OT OCCURS 13 TIMES  PIC 9(011).
             05 00END002OT OCCURS 13 TIMES  PIC 9(011).
             05 00END003OT OCCURS 13 TIMES  PIC 9(011).
             05 00END004OT OCCURS 13 TIMES  PIC 9(011).
             05 00END005OT              PIC X(011).
             05 00END006OT              PIC X(011).
             05 00END007OT              PIC X(011).
             05 00END008OT              PIC X(011).
             05 00END009OT              PIC X(011).
             05 00END010OT              PIC X(011).
             05 00END011OT              PIC X(011).
             05 00END012OT              PIC X(011).
             05 00END013OT              PIC X(011).
             05 00END014OT              PIC X(011).
             05 00END015OT              PIC X(011).
             05 00END016OT              PIC X(011).
             05 00END017OT              PIC X(011).
             05 00END018OT              PIC X(011).
             05 00END019OT              PIC X(011).
             05 00END020OT              PIC X(011).
             05 00END021OT              PIC X(011).
             05 00END022OT              PIC X(011).
             05 00END023OT              PIC X(011).
             05 00END024OT              PIC X(011).
             05 00END025OT              PIC X(011).
             05 00END026OT              PIC X(011).
             05 00END027OT              PIC X(011).
             05 00END028OT              PIC X(011).
             05 00END029OT              PIC X(011).
             05 00END030OT              PIC X(011).
             05 00END031OT              PIC X(011).
             05 00END032OT              PIC X(011).
             05 00END033OT              PIC X(011).
             05 00END034OT              PIC X(011).
             05 00END035OT              PIC X(011).
             05 00END036OT              PIC X(011).
             05 00END037OT              PIC X(011).
             05 00END038OT              PIC X(011).
             05 00END039OT              PIC X(011).
             05 00END040OT              PIC X(011).
             05 00END041OT              PIC X(011).
             05 00END042OT              PIC X(011).
             05 00END043OT              PIC X(011).
             05 00END044OT              PIC X(011).
             05 00END045OT              PIC X(011).
             05 00END046OT              PIC X(011).
             05 00END047OT              PIC X(011).
             05 00END048OT              PIC X(011).
             05 00END049OT              PIC X(011).

          03 VAR-00ENDCC.
             05 00END001CC OCCURS 13 TIMES  PIC 9(011).
             05 00END002CC OCCURS 13 TIMES  PIC 9(011).
             05 00END003CC OCCURS 13 TIMES  PIC 9(011).
             05 00END004CC OCCURS 13 TIMES  PIC 9(011).
             05 00END005CC              PIC X(011).
             05 00END006CC              PIC X(011).
             05 00END007CC              PIC X(011).
             05 00END008CC              PIC X(011).
             05 00END009CC              PIC X(011).
             05 00END010CC              PIC X(011).
             05 00END011CC              PIC X(011).
             05 00END012CC              PIC X(011).
             05 00END013CC              PIC X(011).
             05 00END014CC              PIC X(011).
             05 00END015CC              PIC X(011).
             05 00END016CC              PIC X(011).
             05 00END017CC              PIC X(011).
             05 00END018CC              PIC X(011).
             05 00END019CC              PIC X(011).
             05 00END020CC              PIC X(011).
             05 00END021CC              PIC X(011).
             05 00END022CC              PIC X(011).
             05 00END023CC              PIC X(011).
             05 00END024CC              PIC X(011).
             05 00END025CC              PIC X(011).
             05 00END026CC              PIC X(011).
             05 00END027CC              PIC X(011).
             05 00END028CC              PIC X(011).
             05 00END029CC              PIC X(011).
             05 00END030CC              PIC X(011).
             05 00END031CC              PIC X(011).
             05 00END032CC              PIC X(011).
             05 00END033CC              PIC X(011).
             05 00END034CC              PIC X(011).
             05 00END035CC              PIC X(011).
             05 00END036CC              PIC X(011).
             05 00END037CC              PIC X(011).
             05 00END038CC              PIC X(011).
             05 00END039CC              PIC X(011).
             05 00END040CC              PIC X(011).
             05 00END041CC              PIC X(011).
             05 00END042CC              PIC X(011).
             05 00END043CC              PIC X(011).
             05 00END044CC              PIC X(011).
             05 00END045CC              PIC X(011).
             05 00END046CC              PIC X(011).
             05 00END047CC              PIC X(011).
             05 00END048CC              PIC X(011).
             05 00END049CC              PIC X(011).

          03 VAR-00ENDCO.
             05 00END001CO OCCURS 13 TIMES  PIC 9(011).
             05 00END002CO OCCURS 13 TIMES  PIC 9(011).
             05 00END003CO OCCURS 13 TIMES  PIC 9(011).
             05 00END004CO OCCURS 13 TIMES  PIC 9(011).
             05 00END005CO              PIC X(011).
             05 00END006CO              PIC X(011).
             05 00END007CO              PIC X(011).
             05 00END008CO              PIC X(011).
             05 00END009CO              PIC X(011).
             05 00END010CO              PIC X(011).
             05 00END011CO              PIC X(011).
             05 00END012CO              PIC X(011).
             05 00END013CO              PIC X(011).
             05 00END014CO              PIC X(011).
             05 00END015CO              PIC X(011).
             05 00END016CO              PIC X(011).
             05 00END017CO              PIC X(011).
             05 00END018CO              PIC X(011).
             05 00END019CO              PIC X(011).
             05 00END020CO              PIC X(011).
             05 00END021CO              PIC X(011).
             05 00END022CO              PIC X(011).
             05 00END023CO              PIC X(011).
             05 00END024CO              PIC X(011).
             05 00END025CO              PIC X(011).
             05 00END026CO              PIC X(011).
             05 00END027CO              PIC X(011).
             05 00END028CO              PIC X(011).
             05 00END029CO              PIC X(011).
             05 00END030CO              PIC X(011).
             05 00END031CO              PIC X(011).
             05 00END032CO              PIC X(011).
             05 00END033CO              PIC X(011).
             05 00END034CO              PIC X(011).
             05 00END035CO              PIC X(011).
             05 00END036CO              PIC X(011).
             05 00END037CO              PIC X(011).
             05 00END038CO              PIC X(011).
             05 00END039CO              PIC X(011).
             05 00END040CO              PIC X(011).
             05 00END041CO              PIC X(011).
             05 00END042CO              PIC X(011).
             05 00END043CO              PIC X(011).
             05 00END044CO              PIC X(011).
             05 00END045CO              PIC X(011).
             05 00END046CO              PIC X(011).
             05 00END047CO              PIC X(011).
             05 00END048CO              PIC X(011).
             05 00END049CO              PIC X(011).

       01 VAR-01NUM.
          02 TABLA-PORTAFOLIOS-01NUM OCCURS 9 TIMES.
             03 01NUMPTF                PIC X(002).
             03 01NUM001                PIC 9(004).
             03 01NUM002                PIC 9(004).
             03 01NUM003                PIC 9(004).
             03 01NUM004                PIC 9(004).
             03 01NUM005                PIC 9(004).
             03 01NUM007                PIC 9(004).
             03 01NUM018                PIC 9(004).
             03 01NUM019                PIC 9(004).
             03 01NUM022                PIC 9(004).
             03 01NUM037                PIC 9(004).
             03 01NUM038                PIC 9(004).
             03 01NUM039                PIC 9(004).
             03 01NUM040                PIC 9(004).
             03 01NUM041                PIC 9(004).
             03 01NUM042                PIC 9(004).
             03 01NUM043                PIC 9(004).
             03 01NUM044                PIC 9(004).
             03 01NUM045                PIC 9(004).
             03 01NUM046                PIC 9(004).
             03 01NUM047                PIC 9(004).
             03 01NUM048                PIC 9(004).
             03 01NUM049                PIC 9(004).
             03 01NUM050                PIC 9(004).
          02 TABLA-PORTAFOLIOS-01NUM-R REDEFINES 
                                     TABLA-PORTAFOLIOS-01NUM.
             03 01NUMPTFRO              PIC X(002).
             03 01NUM001RO              PIC X(004).
             03 01NUM002RO              PIC X(004).
             03 01NUM003RO              PIC X(004).
             03 01NUM004RO              PIC X(004).
             03 01NUM005RO              PIC X(004).
             03 01NUM007RO              PIC X(004).
             03 01NUM018RO              PIC X(004).
             03 01NUM019RO              PIC X(004).
             03 01NUM022RO              PIC X(004).
             03 01NUM037RO              PIC X(004).
             03 01NUM038RO              PIC X(004).
             03 01NUM039RO              PIC X(004).
             03 01NUM040RO              PIC X(004).
             03 01NUM041RO              PIC X(004).
             03 01NUM042RO              PIC X(004).
             03 01NUM043RO              PIC X(004).
             03 01NUM044RO              PIC X(004).
             03 01NUM045RO              PIC X(004).
             03 01NUM046RO              PIC X(004).
             03 01NUM047RO              PIC X(004).
             03 01NUM048RO              PIC X(004).
             03 01NUM049RO              PIC X(004).
             03 01NUM050RO              PIC X(004).

             03 01NUMPTFIN              PIC X(002).
             03 01NUM001IN              PIC X(004).
             03 01NUM002IN              PIC X(004).
             03 01NUM003IN              PIC X(004).
             03 01NUM004IN              PIC X(004).
             03 01NUM005IN              PIC X(004).
             03 01NUM007IN              PIC X(004).
             03 01NUM018IN              PIC X(004).
             03 01NUM019IN              PIC X(004).
             03 01NUM022IN              PIC X(004).
             03 01NUM037IN              PIC X(004).
             03 01NUM038IN              PIC X(004).
             03 01NUM039IN              PIC X(004).
             03 01NUM040IN              PIC X(004).
             03 01NUM041IN              PIC X(004).
             03 01NUM042IN              PIC X(004).
             03 01NUM043IN              PIC X(004).
             03 01NUM044IN              PIC X(004).
             03 01NUM045IN              PIC X(004).
             03 01NUM046IN              PIC X(004).
             03 01NUM047IN              PIC X(004).
             03 01NUM048IN              PIC X(004).
             03 01NUM049IN              PIC X(004).
             03 01NUM050IN              PIC X(004).

             03 01NUMPTFVE              PIC X(002).
             03 01NUM001VE              PIC X(004).
             03 01NUM002VE              PIC X(004).
             03 01NUM003VE              PIC X(004).
             03 01NUM004VE              PIC X(004).
             03 01NUM005VE              PIC X(004).
             03 01NUM007VE              PIC X(004).
             03 01NUM018VE              PIC X(004).
             03 01NUM019VE              PIC X(004).
             03 01NUM022VE              PIC X(004).
             03 01NUM037VE              PIC X(004).
             03 01NUM038VE              PIC X(004).
             03 01NUM039VE              PIC X(004).
             03 01NUM040VE              PIC X(004).
             03 01NUM041VE              PIC X(004).
             03 01NUM042VE              PIC X(004).
             03 01NUM043VE              PIC X(004).
             03 01NUM044VE              PIC X(004).
             03 01NUM045VE              PIC X(004).
             03 01NUM046VE              PIC X(004).
             03 01NUM047VE              PIC X(004).
             03 01NUM048VE              PIC X(004).
             03 01NUM049VE              PIC X(004).
             03 01NUM050VE              PIC X(004).

             03 01NUMPTFHP              PIC X(002).
             03 01NUM001HP              PIC X(004).
             03 01NUM002HP              PIC X(004).
             03 01NUM003HP              PIC X(004).
             03 01NUM004HP              PIC X(004).
             03 01NUM005HP              PIC X(004).
             03 01NUM007HP              PIC X(004).
             03 01NUM018HP              PIC X(004).
             03 01NUM019HP              PIC X(004).
             03 01NUM022HP              PIC X(004).
             03 01NUM037HP              PIC X(004).
             03 01NUM038HP              PIC X(004).
             03 01NUM039HP              PIC X(004).
             03 01NUM040HP              PIC X(004).
             03 01NUM041HP              PIC X(004).
             03 01NUM042HP              PIC X(004).
             03 01NUM043HP              PIC X(004).
             03 01NUM044HP              PIC X(004).
             03 01NUM045HP              PIC X(004).
             03 01NUM046HP              PIC X(004).
             03 01NUM047HP              PIC X(004).
             03 01NUM048HP              PIC X(004).
             03 01NUM049HP              PIC X(004).
             03 01NUM050HP              PIC X(004).

             03 01NUMPTFOT              PIC X(002).
             03 01NUM001OT              PIC X(004).
             03 01NUM002OT              PIC X(004).
             03 01NUM003OT              PIC X(004).
             03 01NUM004OT              PIC X(004).
             03 01NUM005OT              PIC X(004).
             03 01NUM007OT              PIC X(004).
             03 01NUM018OT              PIC X(004).
             03 01NUM019OT              PIC X(004).
             03 01NUM022OT              PIC X(004).
             03 01NUM037OT              PIC X(004).
             03 01NUM038OT              PIC X(004).
             03 01NUM039OT              PIC X(004).
             03 01NUM040OT              PIC X(004).
             03 01NUM041OT              PIC X(004).
             03 01NUM042OT              PIC X(004).
             03 01NUM043OT              PIC X(004).
             03 01NUM044OT              PIC X(004).
             03 01NUM045OT              PIC X(004).
             03 01NUM046OT              PIC X(004).
             03 01NUM047OT              PIC X(004).
             03 01NUM048OT              PIC X(004).
             03 01NUM049OT              PIC X(004).
             03 01NUM050OT              PIC X(004).

             03 01NUMPTFCC              PIC X(002).
             03 01NUM001CC              PIC X(004).
             03 01NUM002CC              PIC X(004).
             03 01NUM003CC              PIC X(004).
             03 01NUM004CC              PIC X(004).
             03 01NUM005CC              PIC X(004).
             03 01NUM007CC              PIC X(004).
             03 01NUM018CC              PIC X(004).
             03 01NUM019CC              PIC X(004).
             03 01NUM022CC              PIC X(004).
             03 01NUM037CC              PIC X(004).
             03 01NUM038CC              PIC X(004).
             03 01NUM039CC              PIC X(004).
             03 01NUM040CC              PIC X(004).
             03 01NUM041CC              PIC X(004).
             03 01NUM042CC              PIC X(004).
             03 01NUM043CC              PIC X(004).
             03 01NUM044CC              PIC X(004).
             03 01NUM045CC              PIC X(004).
             03 01NUM046CC              PIC X(004).
             03 01NUM047CC              PIC X(004).
             03 01NUM048CC              PIC X(004).
             03 01NUM049CC              PIC X(004).
             03 01NUM050CC              PIC X(004).

             03 01NUMPTFCO              PIC X(002).
             03 01NUM001CO              PIC X(004).
             03 01NUM002CO              PIC X(004).
             03 01NUM003CO              PIC X(004).
             03 01NUM004CO              PIC X(004).
             03 01NUM005CO              PIC X(004).
             03 01NUM007CO              PIC X(004).
             03 01NUM018CO              PIC X(004).
             03 01NUM019CO              PIC X(004).
             03 01NUM022CO              PIC X(004).
             03 01NUM037CO              PIC X(004).
             03 01NUM038CO              PIC X(004).
             03 01NUM039CO              PIC X(004).
             03 01NUM040CO              PIC X(004).
             03 01NUM041CO              PIC X(004).
             03 01NUM042CO              PIC X(004).
             03 01NUM043CO              PIC X(004).
             03 01NUM044CO              PIC X(004).
             03 01NUM045CO              PIC X(004).
             03 01NUM046CO              PIC X(004).
             03 01NUM047CO              PIC X(004).
             03 01NUM048CO              PIC X(004).
             03 01NUM049CO              PIC X(004).
             03 01NUM050CO              PIC X(004).

             03 01NUMPTFAH              PIC X(002).
             03 01NUM001AH              PIC X(004).
             03 01NUM002AH              PIC X(004).
             03 01NUM003AH              PIC X(004).
             03 01NUM004AH              PIC X(004).
             03 01NUM005AH              PIC X(004).
             03 01NUM007AH              PIC X(004).
             03 01NUM018AH              PIC X(004).
             03 01NUM019AH              PIC X(004).
             03 01NUM022AH              PIC X(004).
             03 01NUM037AH              PIC X(004).
             03 01NUM038AH              PIC X(004).
             03 01NUM039AH              PIC X(004).
             03 01NUM040AH              PIC X(004).
             03 01NUM041AH              PIC X(004).
             03 01NUM042AH              PIC X(004).
             03 01NUM043AH              PIC X(004).
             03 01NUM044AH              PIC X(004).
             03 01NUM045AH              PIC X(004).
             03 01NUM046AH              PIC X(004).
             03 01NUM047AH              PIC X(004).
             03 01NUM048AH              PIC X(004).
             03 01NUM049AH              PIC X(004).
             03 01NUM050AH              PIC X(004).

             03 01NUMPTFCT              PIC X(002).
             03 01NUM001CT              PIC X(004).
             03 01NUM002CT              PIC X(004).
             03 01NUM003CT              PIC X(004).
             03 01NUM004CT              PIC X(004).
             03 01NUM005CT              PIC X(004).
             03 01NUM007CT              PIC X(004).
             03 01NUM018CT              PIC X(004).
             03 01NUM019CT              PIC X(004).
             03 01NUM022CT              PIC X(004).
             03 01NUM037CT              PIC X(004).
             03 01NUM038CT              PIC X(004).
             03 01NUM039CT              PIC X(004).
             03 01NUM040CT              PIC X(004).
             03 01NUM041CT              PIC X(004).
             03 01NUM042CT              PIC X(004).
             03 01NUM043CT              PIC X(004).
             03 01NUM044CT              PIC X(004).
             03 01NUM045CT              PIC X(004).
             03 01NUM046CT              PIC X(004).
             03 01NUM047CT              PIC X(004).
             03 01NUM048CT              PIC X(004).
             03 01NUM049CT              PIC X(004).
             03 01NUM050CT              PIC X(004).

       01 VAR-01INQ.
          03 01INQTIP                   PIC 9(001).
          03 01INQNRO                   PIC 9(011).
          03 01INQ001                   PIC 9(004).
          03 01INQ002                   PIC 9(004).
          03 01INQ005                   PIC 9(004).
          03 01INQ006                   PIC 9(004).
          03 01INQ007                   PIC 9(004).
          03 01INQ010                   PIC 9(004).
          03 01INQ011                   PIC 9(004).
          03 01INQ012                   PIC 9(004).
          03 01INQ015                   PIC 9(004).
ACIEAA    03 01INQ016                   PIC 9(004).                     RMB0006
ACIEAA    03 01INQ017                   PIC 9(004).                     RMB0006
ACIEAA    03 01INQ018                   PIC 9(004).                     RMB0006
ACIEAA    03 01INQ025                   PIC 9(004).                     RMB0006
ACIEAA    03 01INQ026                   PIC 9(004).                     RMB0006
ACIEAA    03 01INQ027                   PIC 9(004).                     RMB0006
ACIEAA    03 01INQ028                   PIC 9(004).                     RMB0006

       01 VAR-01MOR.
          03 TABLA-PORTAFOLIOS-01MOR OCCURS 9 TIMES.
           05 01MOR-EDITADOS.
             07 01MORPTF                PIC X(002).
             07 01MOR001                PIC 9(004).
             07 01MOR002                PIC 9(004).
             07 01MOR003                PIC 9(004).
             07 01MOR004                PIC 9(004).
             07 01MOR005                PIC 9(004).
             07 01MOR006                PIC 9(004).
             07 01MOR007                PIC 9(004).
             07 01MOR008                PIC 9(004).
             07 01MOR009                PIC 9(004).
             07 01MOR010                PIC 9(004).
             07 01MOR011                PIC 9(004).
             07 01MOR012                PIC 9(004).
             07 01MOR013                PIC 9(004).
             07 01MOR014                PIC 9(004).
             07 01MOR015                PIC 9(004).
             07 01MOR016                PIC 9(004).
             07 01MOR017                PIC 9(004).
             07 01MOR018                PIC 9(004).
             07 01MOR019                PIC 9(004).
             07 01MOR020                PIC 9(004).
             07 01MOR021                PIC 9(004).
             07 01MOR022                PIC 9(004).
             07 01MOR023                PIC 9(004).
             07 01MOR024                PIC 9(004).
             07 01MOR025                PIC 9(004).
             07 01MOR026                PIC 9(004).
             07 01MOR027                PIC 9(004).
             07 01MOR028                PIC 9(004).
             07 01MOR029                PIC 9(004).
             07 01MOR030                PIC 9(004).
             07 01MOR031                PIC 9(004).
             07 01MOR032                PIC 9(004).
             07 01MOR033                PIC 9(004).
             07 01MOR034                PIC 9(004).
             07 01MOR035                PIC 9(004).
             07 01MOR036                PIC 9(004).
             07 01MOR037                PIC 9(004).
             07 01MOR038                PIC 9(004).
             07 01MOR039                PIC 9(004).
             07 01MOR040                PIC 9(004).
             07 01MOR041                PIC 9(004).
             07 01MOR042                PIC 9(004).
             07 01MOR043                PIC 9(004).
             07 01MOR044                PIC 9(004).
             07 01MOR045                PIC 9(004).
             07 01MOR046                PIC 9(004).
             07 01MOR047                PIC 9(004).
             07 01MOR048                PIC 9(004).
             07 01MOR049                PIC 9(004).
             07 01MOR050                PIC 9(004).
             07 01MOR051                PIC 9(004).
             07 01MOR052                PIC 9(004).
             07 01MOR053                PIC 9(004).
             07 01MOR054                PIC 9(004).
             07 01MOR055                PIC 9(004).
             07 01MOR056                PIC 9(004).
             07 01MOR057                PIC 9(004).
             07 01MOR058                PIC 9(004).
             07 01MOR059                PIC 9(004).
             07 01MOR060                PIC 9(004).
             07 01MOR061                PIC 9(004).
             07 01MOR062                PIC 9(004).
             07 01MOR063                PIC 9(004).
             07 01MOR064                PIC 9(004).
             07 01MOR065                PIC 9(004).
             07 01MOR066                PIC 9(004).
             07 01MOR067                PIC 9(004).
             07 01MOR068                PIC 9(004).
             07 01MOR069                PIC 9(004).
             07 01MOR070                PIC 9(004).
             07 01MOR071                PIC 9(004).
             07 01MOR072                PIC 9(004).
             07 01MOR073                PIC 9(004).
             07 01MOR074                PIC 9(004).
             07 01MOR075                PIC 9(004).
             07 01MOR076                PIC 9(004).
             07 01MOR077                PIC 9(004).
             07 01MOR078                PIC 9(004).
             07 01MOR079                PIC 9(004).
             07 01MOR080                PIC 999.99.
             07 01MOR081                PIC 999.99.
             07 01MOR082                PIC 999.99.
             07 01MOR083                PIC 999.99.
             07 01MOR084                PIC 999.99.
             07 01MOR085                PIC 999.99.
             07 01MOR086                PIC 999.99.
             07 01MOR087                PIC 999.99.
             07 01MOR088                PIC 999.99.
             07 01MOR089                PIC 999.99.
             07 01MOR090                PIC 999.99.
             07 01MOR091                PIC 999.99.
             07 01MOR092                PIC 999.99.
             07 01MOR093                PIC 999.99.
             07 01MOR094                PIC 999.99.
             07 01MOR095                PIC 999.99.
             07 01MOR096                PIC 999.99.
             07 01MOR097                PIC 999.99.
             07 01MOR098                PIC 999.99.
             07 01MOR099                PIC 999.99.
             07 01MOR100                PIC 999.99.
             07 01MOR101                PIC 999.99.
             07 01MOR102                PIC 999.99.
             07 01MOR103                PIC 999.99.
           05 01MOR-VALORES.
             07 01MORF75                PIC 9(01).
             07 01MORF76                PIC 9(01).
             07 01MORF77                PIC 9(01).
             07 01MORF78                PIC 9(01).
             07 01MORF79                PIC 9(01).
          03 TABLA-PORTAFOLIOS-01MOR-R REDEFINES
                                       TABLA-PORTAFOLIOS-01MOR.
             07 01MORPTFRO              PIC X(002).
             07 01MOR001RO              PIC X(004).
             07 01MOR002RO              PIC X(004).
             07 01MOR003RO              PIC X(004).
             07 01MOR004RO              PIC X(004).
             07 01MOR005RO              PIC X(004).
             07 01MOR006RO              PIC X(004).
             07 01MOR007RO              PIC X(004).
             07 01MOR008RO              PIC X(004).
             07 01MOR009RO              PIC X(004).
             07 01MOR010RO              PIC X(004).
             07 01MOR011RO              PIC X(004).
             07 01MOR012RO              PIC X(004).
             07 01MOR013RO              PIC X(004).
             07 01MOR014RO              PIC X(004).
             07 01MOR015RO              PIC X(004).
             07 01MOR016RO              PIC X(004).
             07 01MOR017RO              PIC X(004).
             07 01MOR018RO              PIC X(004).
             07 01MOR019RO              PIC X(004).
             07 01MOR020RO              PIC X(004).
             07 01MOR021RO              PIC X(004).
             07 01MOR022RO              PIC X(004).
             07 01MOR023RO              PIC X(004).
             07 01MOR024RO              PIC X(004).
             07 01MOR025RO              PIC X(004).
             07 01MOR026RO              PIC X(004).
             07 01MOR027RO              PIC X(004).
             07 01MOR028RO              PIC X(004).
             07 01MOR029RO              PIC X(004).
             07 01MOR030RO              PIC X(004).
             07 01MOR031RO              PIC X(004).
             07 01MOR032RO              PIC X(004).
             07 01MOR033RO              PIC X(004).
             07 01MOR034RO              PIC X(004).
             07 01MOR035RO              PIC X(004).
             07 01MOR036RO              PIC X(004).
             07 01MOR037RO              PIC X(004).
             07 01MOR038RO              PIC X(004).
             07 01MOR039RO              PIC X(004).
             07 01MOR040RO              PIC X(004).
             07 01MOR041RO              PIC X(004).
             07 01MOR042RO              PIC X(004).
             07 01MOR043RO              PIC X(004).
             07 01MOR044RO              PIC X(004).
             07 01MOR045RO              PIC X(004).
             07 01MOR046RO              PIC X(004).
             07 01MOR047RO              PIC X(004).
             07 01MOR048RO              PIC X(004).
             07 01MOR049RO              PIC X(004).
             07 01MOR050RO              PIC X(004).
             07 01MOR051RO              PIC X(004).
             07 01MOR052RO              PIC X(004).
             07 01MOR053RO              PIC X(004).
             07 01MOR054RO              PIC X(004).
             07 01MOR055RO              PIC X(004).
             07 01MOR056RO              PIC X(004).
             07 01MOR057RO              PIC X(004).
             07 01MOR058RO              PIC X(004).
             07 01MOR059RO              PIC X(004).
             07 01MOR060RO              PIC X(004).
             07 01MOR061RO              PIC X(004).
             07 01MOR062RO              PIC X(004).
             07 01MOR063RO              PIC X(004).
             07 01MOR064RO              PIC X(004).
             07 01MOR065RO              PIC X(004).
             07 01MOR066RO              PIC X(004).
             07 01MOR067RO              PIC X(004).
             07 01MOR068RO              PIC X(004).
             07 01MOR069RO              PIC X(004).
             07 01MOR070RO              PIC X(004).
             07 01MOR071RO              PIC X(004).
             07 01MOR072RO              PIC X(004).
             07 01MOR073RO              PIC X(004).
             07 01MOR074RO              PIC X(004).
             07 01MOR075RO              PIC X(004).
             07 01MOR076RO              PIC X(004).
             07 01MOR077RO              PIC X(004).
             07 01MOR078RO              PIC X(004).
             07 01MOR079RO              PIC X(004).
             07 01MOR080RO              PIC X(006).
             07 01MOR081RO              PIC X(006).
             07 01MOR082RO              PIC X(006).
             07 01MOR083RO              PIC X(006).
             07 01MOR084RO              PIC X(006).
             07 01MOR085RO              PIC X(006).
             07 01MOR086RO              PIC X(006).
             07 01MOR087RO              PIC X(006).
             07 01MOR088RO              PIC X(006).
             07 01MOR089RO              PIC X(006).
             07 01MOR090RO              PIC X(006).
             07 01MOR091RO              PIC X(006).
             07 01MOR092RO              PIC X(006).
             07 01MOR093RO              PIC X(006).
             07 01MOR094RO              PIC X(006).
             07 01MOR095RO              PIC X(006).
             07 01MOR096RO              PIC X(006).
             07 01MOR097RO              PIC X(006).
             07 01MOR098RO              PIC X(006).
             07 01MOR099RO              PIC X(006).
             07 01MOR100RO              PIC X(006).
             07 01MOR101RO              PIC X(006).
             07 01MOR102RO              PIC X(006).
             07 01MOR103RO              PIC X(006).
             07 FILLER-01MORRO          PIC X(005).

             07 01MORPTFIN              PIC X(002).
             07 01MOR001IN              PIC X(004).
             07 01MOR002IN              PIC X(004).
             07 01MOR003IN              PIC X(004).
             07 01MOR004IN              PIC X(004).
             07 01MOR005IN              PIC X(004).
             07 01MOR006IN              PIC X(004).
             07 01MOR007IN              PIC X(004).
             07 01MOR008IN              PIC X(004).
             07 01MOR009IN              PIC X(004).
             07 01MOR010IN              PIC X(004).
             07 01MOR011IN              PIC X(004).
             07 01MOR012IN              PIC X(004).
             07 01MOR013IN              PIC X(004).
             07 01MOR014IN              PIC X(004).
             07 01MOR015IN              PIC X(004).
             07 01MOR016IN              PIC X(004).
             07 01MOR017IN              PIC X(004).
             07 01MOR018IN              PIC X(004).
             07 01MOR019IN              PIC X(004).
             07 01MOR020IN              PIC X(004).
             07 01MOR021IN              PIC X(004).
             07 01MOR022IN              PIC X(004).
             07 01MOR023IN              PIC X(004).
             07 01MOR024IN              PIC X(004).
             07 01MOR025IN              PIC X(004).
             07 01MOR026IN              PIC X(004).
             07 01MOR027IN              PIC X(004).
             07 01MOR028IN              PIC X(004).
             07 01MOR029IN              PIC X(004).
             07 01MOR030IN              PIC X(004).
             07 01MOR031IN              PIC X(004).
             07 01MOR032IN              PIC X(004).
             07 01MOR033IN              PIC X(004).
             07 01MOR034IN              PIC X(004).
             07 01MOR035IN              PIC X(004).
             07 01MOR036IN              PIC X(004).
             07 01MOR037IN              PIC X(004).
             07 01MOR038IN              PIC X(004).
             07 01MOR039IN              PIC X(004).
             07 01MOR040IN              PIC X(004).
             07 01MOR041IN              PIC X(004).
             07 01MOR042IN              PIC X(004).
             07 01MOR043IN              PIC X(004).
             07 01MOR044IN              PIC X(004).
             07 01MOR045IN              PIC X(004).
             07 01MOR046IN              PIC X(004).
             07 01MOR047IN              PIC X(004).
             07 01MOR048IN              PIC X(004).
             07 01MOR049IN              PIC X(004).
             07 01MOR050IN              PIC X(004).
             07 01MOR051IN              PIC X(004).
             07 01MOR052IN              PIC X(004).
             07 01MOR053IN              PIC X(004).
             07 01MOR054IN              PIC X(004).
             07 01MOR055IN              PIC X(004).
             07 01MOR056IN              PIC X(004).
             07 01MOR057IN              PIC X(004).
             07 01MOR058IN              PIC X(004).
             07 01MOR059IN              PIC X(004).
             07 01MOR060IN              PIC X(004).
             07 01MOR061IN              PIC X(004).
             07 01MOR062IN              PIC X(004).
             07 01MOR063IN              PIC X(004).
             07 01MOR064IN              PIC X(004).
             07 01MOR065IN              PIC X(004).
             07 01MOR066IN              PIC X(004).
             07 01MOR067IN              PIC X(004).
             07 01MOR068IN              PIC X(004).
             07 01MOR069IN              PIC X(004).
             07 01MOR070IN              PIC X(004).
             07 01MOR071IN              PIC X(004).
             07 01MOR072IN              PIC X(004).
             07 01MOR073IN              PIC X(004).
             07 01MOR074IN              PIC X(004).
             07 01MOR075IN              PIC X(004).
             07 01MOR076IN              PIC X(004).
             07 01MOR077IN              PIC X(004).
             07 01MOR078IN              PIC X(004).
             07 01MOR079IN              PIC X(004).
             07 01MOR080IN              PIC X(006).
             07 01MOR081IN              PIC X(006).
             07 01MOR082IN              PIC X(006).
             07 01MOR083IN              PIC X(006).
             07 01MOR084IN              PIC X(006).
             07 01MOR085IN              PIC X(006).
             07 01MOR086IN              PIC X(006).
             07 01MOR087IN              PIC X(006).
             07 01MOR088IN              PIC X(006).
             07 01MOR089IN              PIC X(006).
             07 01MOR090IN              PIC X(006).
             07 01MOR091IN              PIC X(006).
             07 01MOR092IN              PIC X(006).
             07 01MOR093IN              PIC X(006).
             07 01MOR094IN              PIC X(006).
             07 01MOR095IN              PIC X(006).
             07 01MOR096IN              PIC X(006).
             07 01MOR097IN              PIC X(006).
             07 01MOR098IN              PIC X(006).
             07 01MOR099IN              PIC X(006).
             07 01MOR100IN              PIC X(006).
             07 01MOR101IN              PIC X(006).
             07 01MOR102IN              PIC X(006).
             07 01MOR103IN              PIC X(006).
             07 FILLER-01MORIN          PIC X(005).

             07 01MORPTFVE              PIC X(002).
             07 01MOR001VE              PIC X(004).
             07 01MOR002VE              PIC X(004).
             07 01MOR003VE              PIC X(004).
             07 01MOR004VE              PIC X(004).
             07 01MOR005VE              PIC X(004).
             07 01MOR006VE              PIC X(004).
             07 01MOR007VE              PIC X(004).
             07 01MOR008VE              PIC X(004).
             07 01MOR009VE              PIC X(004).
             07 01MOR010VE              PIC X(004).
             07 01MOR011VE              PIC X(004).
             07 01MOR012VE              PIC X(004).
             07 01MOR013VE              PIC X(004).
             07 01MOR014VE              PIC X(004).
             07 01MOR015VE              PIC X(004).
             07 01MOR016VE              PIC X(004).
             07 01MOR017VE              PIC X(004).
             07 01MOR018VE              PIC X(004).
             07 01MOR019VE              PIC X(004).
             07 01MOR020VE              PIC X(004).
             07 01MOR021VE              PIC X(004).
             07 01MOR022VE              PIC X(004).
             07 01MOR023VE              PIC X(004).
             07 01MOR024VE              PIC X(004).
             07 01MOR025VE              PIC X(004).
             07 01MOR026VE              PIC X(004).
             07 01MOR027VE              PIC X(004).
             07 01MOR028VE              PIC X(004).
             07 01MOR029VE              PIC X(004).
             07 01MOR030VE              PIC X(004).
             07 01MOR031VE              PIC X(004).
             07 01MOR032VE              PIC X(004).
             07 01MOR033VE              PIC X(004).
             07 01MOR034VE              PIC X(004).
             07 01MOR035VE              PIC X(004).
             07 01MOR036VE              PIC X(004).
             07 01MOR037VE              PIC X(004).
             07 01MOR038VE              PIC X(004).
             07 01MOR039VE              PIC X(004).
             07 01MOR040VE              PIC X(004).
             07 01MOR041VE              PIC X(004).
             07 01MOR042VE              PIC X(004).
             07 01MOR043VE              PIC X(004).
             07 01MOR044VE              PIC X(004).
             07 01MOR045VE              PIC X(004).
             07 01MOR046VE              PIC X(004).
             07 01MOR047VE              PIC X(004).
             07 01MOR048VE              PIC X(004).
             07 01MOR049VE              PIC X(004).
             07 01MOR050VE              PIC X(004).
             07 01MOR051VE              PIC X(004).
             07 01MOR052VE              PIC X(004).
             07 01MOR053VE              PIC X(004).
             07 01MOR054VE              PIC X(004).
             07 01MOR055VE              PIC X(004).
             07 01MOR056VE              PIC X(004).
             07 01MOR057VE              PIC X(004).
             07 01MOR058VE              PIC X(004).
             07 01MOR059VE              PIC X(004).
             07 01MOR060VE              PIC X(004).
             07 01MOR061VE              PIC X(004).
             07 01MOR062VE              PIC X(004).
             07 01MOR063VE              PIC X(004).
             07 01MOR064VE              PIC X(004).
             07 01MOR065VE              PIC X(004).
             07 01MOR066VE              PIC X(004).
             07 01MOR067VE              PIC X(004).
             07 01MOR068VE              PIC X(004).
             07 01MOR069VE              PIC X(004).
             07 01MOR070VE              PIC X(004).
             07 01MOR071VE              PIC X(004).
             07 01MOR072VE              PIC X(004).
             07 01MOR073VE              PIC X(004).
             07 01MOR074VE              PIC X(004).
             07 01MOR075VE              PIC X(004).
             07 01MOR076VE              PIC X(004).
             07 01MOR077VE              PIC X(004).
             07 01MOR078VE              PIC X(004).
             07 01MOR079VE              PIC X(004).
             07 01MOR080VE              PIC X(006).
             07 01MOR081VE              PIC X(006).
             07 01MOR082VE              PIC X(006).
             07 01MOR083VE              PIC X(006).
             07 01MOR084VE              PIC X(006).
             07 01MOR085VE              PIC X(006).
             07 01MOR086VE              PIC X(006).
             07 01MOR087VE              PIC X(006).
             07 01MOR088VE              PIC X(006).
             07 01MOR089VE              PIC X(006).
             07 01MOR090VE              PIC X(006).
             07 01MOR091VE              PIC X(006).
             07 01MOR092VE              PIC X(006).
             07 01MOR093VE              PIC X(006).
             07 01MOR094VE              PIC X(006).
             07 01MOR095VE              PIC X(006).
             07 01MOR096VE              PIC X(006).
             07 01MOR097VE              PIC X(006).
             07 01MOR098VE              PIC X(006).
             07 01MOR099VE              PIC X(006).
             07 01MOR100VE              PIC X(006).
             07 01MOR101VE              PIC X(006).
             07 01MOR102VE              PIC X(006).
             07 01MOR103VE              PIC X(006).
             07 FILLER-01MORVE          PIC X(005).

             07 01MORPTFHP              PIC X(002).
             07 01MOR001HP              PIC X(004).
             07 01MOR002HP              PIC X(004).
             07 01MOR003HP              PIC X(004).
             07 01MOR004HP              PIC X(004).
             07 01MOR005HP              PIC X(004).
             07 01MOR006HP              PIC X(004).
             07 01MOR007HP              PIC X(004).
             07 01MOR008HP              PIC X(004).
             07 01MOR009HP              PIC X(004).
             07 01MOR010HP              PIC X(004).
             07 01MOR011HP              PIC X(004).
             07 01MOR012HP              PIC X(004).
             07 01MOR013HP              PIC X(004).
             07 01MOR014HP              PIC X(004).
             07 01MOR015HP              PIC X(004).
             07 01MOR016HP              PIC X(004).
             07 01MOR017HP              PIC X(004).
             07 01MOR018HP              PIC X(004).
             07 01MOR019HP              PIC X(004).
             07 01MOR020HP              PIC X(004).
             07 01MOR021HP              PIC X(004).
             07 01MOR022HP              PIC X(004).
             07 01MOR023HP              PIC X(004).
             07 01MOR024HP              PIC X(004).
             07 01MOR025HP              PIC X(004).
             07 01MOR026HP              PIC X(004).
             07 01MOR027HP              PIC X(004).
             07 01MOR028HP              PIC X(004).
             07 01MOR029HP              PIC X(004).
             07 01MOR030HP              PIC X(004).
             07 01MOR031HP              PIC X(004).
             07 01MOR032HP              PIC X(004).
             07 01MOR033HP              PIC X(004).
             07 01MOR034HP              PIC X(004).
             07 01MOR035HP              PIC X(004).
             07 01MOR036HP              PIC X(004).
             07 01MOR037HP              PIC X(004).
             07 01MOR038HP              PIC X(004).
             07 01MOR039HP              PIC X(004).
             07 01MOR040HP              PIC X(004).
             07 01MOR041HP              PIC X(004).
             07 01MOR042HP              PIC X(004).
             07 01MOR043HP              PIC X(004).
             07 01MOR044HP              PIC X(004).
             07 01MOR045HP              PIC X(004).
             07 01MOR046HP              PIC X(004).
             07 01MOR047HP              PIC X(004).
             07 01MOR048HP              PIC X(004).
             07 01MOR049HP              PIC X(004).
             07 01MOR050HP              PIC X(004).
             07 01MOR051HP              PIC X(004).
             07 01MOR052HP              PIC X(004).
             07 01MOR053HP              PIC X(004).
             07 01MOR054HP              PIC X(004).
             07 01MOR055HP              PIC X(004).
             07 01MOR056HP              PIC X(004).
             07 01MOR057HP              PIC X(004).
             07 01MOR058HP              PIC X(004).
             07 01MOR059HP              PIC X(004).
             07 01MOR060HP              PIC X(004).
             07 01MOR061HP              PIC X(004).
             07 01MOR062HP              PIC X(004).
             07 01MOR063HP              PIC X(004).
             07 01MOR064HP              PIC X(004).
             07 01MOR065HP              PIC X(004).
             07 01MOR066HP              PIC X(004).
             07 01MOR067HP              PIC X(004).
             07 01MOR068HP              PIC X(004).
             07 01MOR069HP              PIC X(004).
             07 01MOR070HP              PIC X(004).
             07 01MOR071HP              PIC X(004).
             07 01MOR072HP              PIC X(004).
             07 01MOR073HP              PIC X(004).
             07 01MOR074HP              PIC X(004).
             07 01MOR075HP              PIC X(004).
             07 01MOR076HP              PIC X(004).
             07 01MOR077HP              PIC X(004).
             07 01MOR078HP              PIC X(004).
             07 01MOR079HP              PIC X(004).
             07 01MOR080HP              PIC X(006).
             07 01MOR081HP              PIC X(006).
             07 01MOR082HP              PIC X(006).
             07 01MOR083HP              PIC X(006).
             07 01MOR084HP              PIC X(006).
             07 01MOR085HP              PIC X(006).
             07 01MOR086HP              PIC X(006).
             07 01MOR087HP              PIC X(006).
             07 01MOR088HP              PIC X(006).
             07 01MOR089HP              PIC X(006).
             07 01MOR090HP              PIC X(006).
             07 01MOR091HP              PIC X(006).
             07 01MOR092HP              PIC X(006).
             07 01MOR093HP              PIC X(006).
             07 01MOR094HP              PIC X(006).
             07 01MOR095HP              PIC X(006).
             07 01MOR096HP              PIC X(006).
             07 01MOR097HP              PIC X(006).
             07 01MOR098HP              PIC X(006).
             07 01MOR099HP              PIC X(006).
             07 01MOR100HP              PIC X(006).
             07 01MOR101HP              PIC X(006).
             07 01MOR102HP              PIC X(006).
             07 01MOR103HP              PIC X(006).
             07 FILLER-01MORHP          PIC X(005).

             07 01MORPTFOT              PIC X(002).
             07 01MOR001OT              PIC X(004).
             07 01MOR002OT              PIC X(004).
             07 01MOR003OT              PIC X(004).
             07 01MOR004OT              PIC X(004).
             07 01MOR005OT              PIC X(004).
             07 01MOR006OT              PIC X(004).
             07 01MOR007OT              PIC X(004).
             07 01MOR008OT              PIC X(004).
             07 01MOR009OT              PIC X(004).
             07 01MOR010OT              PIC X(004).
             07 01MOR011OT              PIC X(004).
             07 01MOR012OT              PIC X(004).
             07 01MOR013OT              PIC X(004).
             07 01MOR014OT              PIC X(004).
             07 01MOR015OT              PIC X(004).
             07 01MOR016OT              PIC X(004).
             07 01MOR017OT              PIC X(004).
             07 01MOR018OT              PIC X(004).
             07 01MOR019OT              PIC X(004).
             07 01MOR020OT              PIC X(004).
             07 01MOR021OT              PIC X(004).
             07 01MOR022OT              PIC X(004).
             07 01MOR023OT              PIC X(004).
             07 01MOR024OT              PIC X(004).
             07 01MOR025OT              PIC X(004).
             07 01MOR026OT              PIC X(004).
             07 01MOR027OT              PIC X(004).
             07 01MOR028OT              PIC X(004).
             07 01MOR029OT              PIC X(004).
             07 01MOR030OT              PIC X(004).
             07 01MOR031OT              PIC X(004).
             07 01MOR032OT              PIC X(004).
             07 01MOR033OT              PIC X(004).
             07 01MOR034OT              PIC X(004).
             07 01MOR035OT              PIC X(004).
             07 01MOR036OT              PIC X(004).
             07 01MOR037OT              PIC X(004).
             07 01MOR038OT              PIC X(004).
             07 01MOR039OT              PIC X(004).
             07 01MOR040OT              PIC X(004).
             07 01MOR041OT              PIC X(004).
             07 01MOR042OT              PIC X(004).
             07 01MOR043OT              PIC X(004).
             07 01MOR044OT              PIC X(004).
             07 01MOR045OT              PIC X(004).
             07 01MOR046OT              PIC X(004).
             07 01MOR047OT              PIC X(004).
             07 01MOR048OT              PIC X(004).
             07 01MOR049OT              PIC X(004).
             07 01MOR050OT              PIC X(004).
             07 01MOR051OT              PIC X(004).
             07 01MOR052OT              PIC X(004).
             07 01MOR053OT              PIC X(004).
             07 01MOR054OT              PIC X(004).
             07 01MOR055OT              PIC X(004).
             07 01MOR056OT              PIC X(004).
             07 01MOR057OT              PIC X(004).
             07 01MOR058OT              PIC X(004).
             07 01MOR059OT              PIC X(004).
             07 01MOR060OT              PIC X(004).
             07 01MOR061OT              PIC X(004).
             07 01MOR062OT              PIC X(004).
             07 01MOR063OT              PIC X(004).
             07 01MOR064OT              PIC X(004).
             07 01MOR065OT              PIC X(004).
             07 01MOR066OT              PIC X(004).
             07 01MOR067OT              PIC X(004).
             07 01MOR068OT              PIC X(004).
             07 01MOR069OT              PIC X(004).
             07 01MOR070OT              PIC X(004).
             07 01MOR071OT              PIC X(004).
             07 01MOR072OT              PIC X(004).
             07 01MOR073OT              PIC X(004).
             07 01MOR074OT              PIC X(004).
             07 01MOR075OT              PIC X(004).
             07 01MOR076OT              PIC X(004).
             07 01MOR077OT              PIC X(004).
             07 01MOR078OT              PIC X(004).
             07 01MOR079OT              PIC X(004).
             07 01MOR080OT              PIC X(006).
             07 01MOR081OT              PIC X(006).
             07 01MOR082OT              PIC X(006).
             07 01MOR083OT              PIC X(006).
             07 01MOR084OT              PIC X(006).
             07 01MOR085OT              PIC X(006).
             07 01MOR086OT              PIC X(006).
             07 01MOR087OT              PIC X(006).
             07 01MOR088OT              PIC X(006).
             07 01MOR089OT              PIC X(006).
             07 01MOR090OT              PIC X(006).
             07 01MOR091OT              PIC X(006).
             07 01MOR092OT              PIC X(006).
             07 01MOR093OT              PIC X(006).
             07 01MOR094OT              PIC X(006).
             07 01MOR095OT              PIC X(006).
             07 01MOR096OT              PIC X(006).
             07 01MOR097OT              PIC X(006).
             07 01MOR098OT              PIC X(006).
             07 01MOR099OT              PIC X(006).
             07 01MOR100OT              PIC X(006).
             07 01MOR101OT              PIC X(006).
             07 01MOR102OT              PIC X(006).
             07 01MOR103OT              PIC X(006).
             07 FILLER-01MOROT          PIC X(005).

             07 01MORPTFCC              PIC X(002).
             07 01MOR001CC              PIC X(004).
             07 01MOR002CC              PIC X(004).
             07 01MOR003CC              PIC X(004).
             07 01MOR004CC              PIC X(004).
             07 01MOR005CC              PIC X(004).
             07 01MOR006CC              PIC X(004).
             07 01MOR007CC              PIC X(004).
             07 01MOR008CC              PIC X(004).
             07 01MOR009CC              PIC X(004).
             07 01MOR010CC              PIC X(004).
             07 01MOR011CC              PIC X(004).
             07 01MOR012CC              PIC X(004).
             07 01MOR013CC              PIC X(004).
             07 01MOR014CC              PIC X(004).
             07 01MOR015CC              PIC X(004).
             07 01MOR016CC              PIC X(004).
             07 01MOR017CC              PIC X(004).
             07 01MOR018CC              PIC X(004).
             07 01MOR019CC              PIC X(004).
             07 01MOR020CC              PIC X(004).
             07 01MOR021CC              PIC X(004).
             07 01MOR022CC              PIC X(004).
             07 01MOR023CC              PIC X(004).
             07 01MOR024CC              PIC X(004).
             07 01MOR025CC              PIC X(004).
             07 01MOR026CC              PIC X(004).
             07 01MOR027CC              PIC X(004).
             07 01MOR028CC              PIC X(004).
             07 01MOR029CC              PIC X(004).
             07 01MOR030CC              PIC X(004).
             07 01MOR031CC              PIC X(004).
             07 01MOR032CC              PIC X(004).
             07 01MOR033CC              PIC X(004).
             07 01MOR034CC              PIC X(004).
             07 01MOR035CC              PIC X(004).
             07 01MOR036CC              PIC X(004).
             07 01MOR037CC              PIC X(004).
             07 01MOR038CC              PIC X(004).
             07 01MOR039CC              PIC X(004).
             07 01MOR040CC              PIC X(004).
             07 01MOR041CC              PIC X(004).
             07 01MOR042CC              PIC X(004).
             07 01MOR043CC              PIC X(004).
             07 01MOR044CC              PIC X(004).
             07 01MOR045CC              PIC X(004).
             07 01MOR046CC              PIC X(004).
             07 01MOR047CC              PIC X(004).
             07 01MOR048CC              PIC X(004).
             07 01MOR049CC              PIC X(004).
             07 01MOR050CC              PIC X(004).
             07 01MOR051CC              PIC X(004).
             07 01MOR052CC              PIC X(004).
             07 01MOR053CC              PIC X(004).
             07 01MOR054CC              PIC X(004).
             07 01MOR055CC              PIC X(004).
             07 01MOR056CC              PIC X(004).
             07 01MOR057CC              PIC X(004).
             07 01MOR058CC              PIC X(004).
             07 01MOR059CC              PIC X(004).
             07 01MOR060CC              PIC X(004).
             07 01MOR061CC              PIC X(004).
             07 01MOR062CC              PIC X(004).
             07 01MOR063CC              PIC X(004).
             07 01MOR064CC              PIC X(004).
             07 01MOR065CC              PIC X(004).
             07 01MOR066CC              PIC X(004).
             07 01MOR067CC              PIC X(004).
             07 01MOR068CC              PIC X(004).
             07 01MOR069CC              PIC X(004).
             07 01MOR070CC              PIC X(004).
             07 01MOR071CC              PIC X(004).
             07 01MOR072CC              PIC X(004).
             07 01MOR073CC              PIC X(004).
             07 01MOR074CC              PIC X(004).
             07 01MOR075CC              PIC X(004).
             07 01MOR076CC              PIC X(004).
             07 01MOR077CC              PIC X(004).
             07 01MOR078CC              PIC X(004).
             07 01MOR079CC              PIC X(004).
             07 01MOR080CC              PIC X(006).
             07 01MOR081CC              PIC X(006).
             07 01MOR082CC              PIC X(006).
             07 01MOR083CC              PIC X(006).
             07 01MOR084CC              PIC X(006).
             07 01MOR085CC              PIC X(006).
             07 01MOR086CC              PIC X(006).
             07 01MOR087CC              PIC X(006).
             07 01MOR088CC              PIC X(006).
             07 01MOR089CC              PIC X(006).
             07 01MOR090CC              PIC X(006).
             07 01MOR091CC              PIC X(006).
             07 01MOR092CC              PIC X(006).
             07 01MOR093CC              PIC X(006).
             07 01MOR094CC              PIC X(006).
             07 01MOR095CC              PIC X(006).
             07 01MOR096CC              PIC X(006).
             07 01MOR097CC              PIC X(006).
             07 01MOR098CC              PIC X(006).
             07 01MOR099CC              PIC X(006).
             07 01MOR100CC              PIC X(006).
             07 01MOR101CC              PIC X(006).
             07 01MOR102CC              PIC X(006).
             07 01MOR103CC              PIC X(006).
             07 FILLER-01MORCC          PIC X(005).

             07 01MORPTFCO              PIC X(002).
             07 01MOR001CO              PIC X(004).
             07 01MOR002CO              PIC X(004).
             07 01MOR003CO              PIC X(004).
             07 01MOR004CO              PIC X(004).
             07 01MOR005CO              PIC X(004).
             07 01MOR006CO              PIC X(004).
             07 01MOR007CO              PIC X(004).
             07 01MOR008CO              PIC X(004).
             07 01MOR009CO              PIC X(004).
             07 01MOR010CO              PIC X(004).
             07 01MOR011CO              PIC X(004).
             07 01MOR012CO              PIC X(004).
             07 01MOR013CO              PIC X(004).
             07 01MOR014CO              PIC X(004).
             07 01MOR015CO              PIC X(004).
             07 01MOR016CO              PIC X(004).
             07 01MOR017CO              PIC X(004).
             07 01MOR018CO              PIC X(004).
             07 01MOR019CO              PIC X(004).
             07 01MOR020CO              PIC X(004).
             07 01MOR021CO              PIC X(004).
             07 01MOR022CO              PIC X(004).
             07 01MOR023CO              PIC X(004).
             07 01MOR024CO              PIC X(004).
             07 01MOR025CO              PIC X(004).
             07 01MOR026CO              PIC X(004).
             07 01MOR027CO              PIC X(004).
             07 01MOR028CO              PIC X(004).
             07 01MOR029CO              PIC X(004).
             07 01MOR030CO              PIC X(004).
             07 01MOR031CO              PIC X(004).
             07 01MOR032CO              PIC X(004).
             07 01MOR033CO              PIC X(004).
             07 01MOR034CO              PIC X(004).
             07 01MOR035CO              PIC X(004).
             07 01MOR036CO              PIC X(004).
             07 01MOR037CO              PIC X(004).
             07 01MOR038CO              PIC X(004).
             07 01MOR039CO              PIC X(004).
             07 01MOR040CO              PIC X(004).
             07 01MOR041CO              PIC X(004).
             07 01MOR042CO              PIC X(004).
             07 01MOR043CO              PIC X(004).
             07 01MOR044CO              PIC X(004).
             07 01MOR045CO              PIC X(004).
             07 01MOR046CO              PIC X(004).
             07 01MOR047CO              PIC X(004).
             07 01MOR048CO              PIC X(004).
             07 01MOR049CO              PIC X(004).
             07 01MOR050CO              PIC X(004).
             07 01MOR051CO              PIC X(004).
             07 01MOR052CO              PIC X(004).
             07 01MOR053CO              PIC X(004).
             07 01MOR054CO              PIC X(004).
             07 01MOR055CO              PIC X(004).
             07 01MOR056CO              PIC X(004).
             07 01MOR057CO              PIC X(004).
             07 01MOR058CO              PIC X(004).
             07 01MOR059CO              PIC X(004).
             07 01MOR060CO              PIC X(004).
             07 01MOR061CO              PIC X(004).
             07 01MOR062CO              PIC X(004).
             07 01MOR063CO              PIC X(004).
             07 01MOR064CO              PIC X(004).
             07 01MOR065CO              PIC X(004).
             07 01MOR066CO              PIC X(004).
             07 01MOR067CO              PIC X(004).
             07 01MOR068CO              PIC X(004).
             07 01MOR069CO              PIC X(004).
             07 01MOR070CO              PIC X(004).
             07 01MOR071CO              PIC X(004).
             07 01MOR072CO              PIC X(004).
             07 01MOR073CO              PIC X(004).
             07 01MOR074CO              PIC X(004).
             07 01MOR075CO              PIC X(004).
             07 01MOR076CO              PIC X(004).
             07 01MOR077CO              PIC X(004).
             07 01MOR078CO              PIC X(004).
             07 01MOR079CO              PIC X(004).
             07 01MOR080CO              PIC X(006).
             07 01MOR081CO              PIC X(006).
             07 01MOR082CO              PIC X(006).
             07 01MOR083CO              PIC X(006).
             07 01MOR084CO              PIC X(006).
             07 01MOR085CO              PIC X(006).
             07 01MOR086CO              PIC X(006).
             07 01MOR087CO              PIC X(006).
             07 01MOR088CO              PIC X(006).
             07 01MOR089CO              PIC X(006).
             07 01MOR090CO              PIC X(006).
             07 01MOR091CO              PIC X(006).
             07 01MOR092CO              PIC X(006).
             07 01MOR093CO              PIC X(006).
             07 01MOR094CO              PIC X(006).
             07 01MOR095CO              PIC X(006).
             07 01MOR096CO              PIC X(006).
             07 01MOR097CO              PIC X(006).
             07 01MOR098CO              PIC X(006).
             07 01MOR099CO              PIC X(006).
             07 01MOR100CO              PIC X(006).
             07 01MOR101CO              PIC X(006).
             07 01MOR102CO              PIC X(006).
             07 01MOR103CO              PIC X(006).
             07 FILLER-01MORCO          PIC X(005).

             07 FILLER-01MORAH          PIC X(467).

             07 FILLER-01MORCT          PIC X(467).

       01 VAR-01ACP.
          02 TABLA-PORTAFOLIOS-01ACP OCCURS 9 TIMES.
             03 01ACPPTF                PIC X(002).
             03 01ACP001                PIC 9(004).
             03 01ACP002                PIC 9(004).
             03 01ACP003                PIC 9(004).
             03 01ACP004                PIC 9(004).
             03 01ACP005                PIC 9(004).
             03 01ACP006                PIC 9(004).
             03 01ACP007                PIC 9(004).
             03 01ACP008                PIC 9(004).
             03 01ACP009                PIC 9(004).
             03 01ACP010                PIC 9(004).
             03 01ACP011                PIC 9(004).
             03 01ACP012                PIC 9(004).
             03 01ACP013                PIC 9(004).
             03 01ACP014                PIC 9(004).
             03 01ACP015                PIC 9(004).
             03 01ACP016                PIC 9(004).
             03 01ACP017                PIC 9(004).
             03 01ACP018                PIC 9(004).
             03 01ACP019                PIC 9(004).
             03 01ACP020                PIC 9(004).
             03 01ACP021                PIC 9(004).
             03 01ACP022                PIC 9(004).
             03 01ACP023                PIC 9(004).
             03 01ACP024                PIC 9(004).
             03 01ACP025                PIC 9(004).
             03 01ACP026                PIC 9(004).
             03 01ACP027                PIC 9(004).
             03 01ACP028                PIC 9(004).
             03 01ACP029                PIC 9(004).
             03 01ACP030                PIC 9(004).
             03 01ACP031                PIC 9(004).
             03 01ACP032                PIC 9(004).
             03 01ACP033                PIC 9(004).
             03 01ACP034                PIC 9(004).
             03 01ACP035                PIC 9(004).
             03 01ACP036                PIC 9(004).
             03 01ACP037                PIC 9(004).
             03 01ACP038                PIC 9(004).
             03 01ACP039                PIC 9(004).
             03 01ACP040                PIC 9(004).
             03 01ACP041                PIC 9(004).
             03 01ACP042                PIC 9(004).
          02 TABLA-PORTAFOLIOS-01ACP-R REDEFINES
                                       TABLA-PORTAFOLIOS-01ACP.
             03 01ACPPTFRO              PIC X(002).
             03 01ACP001RO              PIC X(004).
             03 01ACP002RO              PIC X(004).
             03 01ACP003RO              PIC X(004).
             03 01ACP004RO              PIC X(004).
             03 01ACP005RO              PIC X(004).
             03 01ACP006RO              PIC X(004).
             03 01ACP007RO              PIC X(004).
             03 01ACP008RO              PIC X(004).
             03 01ACP009RO              PIC X(004).
             03 01ACP010RO              PIC X(004).
             03 01ACP011RO              PIC X(004).
             03 01ACP012RO              PIC X(004).
             03 01ACP013RO              PIC X(004).
             03 01ACP014RO              PIC X(004).
             03 01ACP015RO              PIC X(004).
             03 01ACP016RO              PIC X(004).
             03 01ACP017RO              PIC X(004).
             03 01ACP018RO              PIC X(004).
             03 01ACP019RO              PIC X(004).
             03 01ACP020RO              PIC X(004).
             03 01ACP021RO              PIC X(004).
             03 01ACP022RO              PIC X(004).
             03 01ACP023RO              PIC X(004).
             03 01ACP024RO              PIC X(004).
             03 01ACP025RO              PIC X(004).
             03 01ACP026RO              PIC X(004).
             03 01ACP027RO              PIC X(004).
             03 01ACP028RO              PIC X(004).
             03 01ACP029RO              PIC X(004).
             03 01ACP030RO              PIC X(004).
             03 01ACP031RO              PIC X(004).
             03 01ACP032RO              PIC X(004).
             03 01ACP033RO              PIC X(004).
             03 01ACP034RO              PIC X(004).
             03 01ACP035RO              PIC X(004).
             03 01ACP036RO              PIC X(004).
             03 01ACP037RO              PIC X(004).
             03 01ACP038RO              PIC X(004).
             03 01ACP039RO              PIC X(004).
             03 01ACP040RO              PIC X(004).
             03 01ACP041RO              PIC X(004).
             03 01ACP042RO              PIC X(004).

             03 01ACPPTFIN              PIC X(002).
             03 01ACP001IN              PIC X(004).
             03 01ACP002IN              PIC X(004).
             03 01ACP003IN              PIC X(004).
             03 01ACP004IN              PIC X(004).
             03 01ACP005IN              PIC X(004).
             03 01ACP006IN              PIC X(004).
             03 01ACP007IN              PIC X(004).
             03 01ACP008IN              PIC X(004).
             03 01ACP009IN              PIC X(004).
             03 01ACP010IN              PIC X(004).
             03 01ACP011IN              PIC X(004).
             03 01ACP012IN              PIC X(004).
             03 01ACP013IN              PIC X(004).
             03 01ACP014IN              PIC X(004).
             03 01ACP015IN              PIC X(004).
             03 01ACP016IN              PIC X(004).
             03 01ACP017IN              PIC X(004).
             03 01ACP018IN              PIC X(004).
             03 01ACP019IN              PIC X(004).
             03 01ACP020IN              PIC X(004).
             03 01ACP021IN              PIC X(004).
             03 01ACP022IN              PIC X(004).
             03 01ACP023IN              PIC X(004).
             03 01ACP024IN              PIC X(004).
             03 01ACP025IN              PIC X(004).
             03 01ACP026IN              PIC X(004).
             03 01ACP027IN              PIC X(004).
             03 01ACP028IN              PIC X(004).
             03 01ACP029IN              PIC X(004).
             03 01ACP030IN              PIC X(004).
             03 01ACP031IN              PIC X(004).
             03 01ACP032IN              PIC X(004).
             03 01ACP033IN              PIC X(004).
             03 01ACP034IN              PIC X(004).
             03 01ACP035IN              PIC X(004).
             03 01ACP036IN              PIC X(004).
             03 01ACP037IN              PIC X(004).
             03 01ACP038IN              PIC X(004).
             03 01ACP039IN              PIC X(004).
             03 01ACP040IN              PIC X(004).
             03 01ACP041IN              PIC X(004).
             03 01ACP042IN              PIC X(004).

             03 01ACPPTFVE              PIC X(002).
             03 01ACP001VE              PIC X(004).
             03 01ACP002VE              PIC X(004).
             03 01ACP003VE              PIC X(004).
             03 01ACP004VE              PIC X(004).
             03 01ACP005VE              PIC X(004).
             03 01ACP006VE              PIC X(004).
             03 01ACP007VE              PIC X(004).
             03 01ACP008VE              PIC X(004).
             03 01ACP009VE              PIC X(004).
             03 01ACP010VE              PIC X(004).
             03 01ACP011VE              PIC X(004).
             03 01ACP012VE              PIC X(004).
             03 01ACP013VE              PIC X(004).
             03 01ACP014VE              PIC X(004).
             03 01ACP015VE              PIC X(004).
             03 01ACP016VE              PIC X(004).
             03 01ACP017VE              PIC X(004).
             03 01ACP018VE              PIC X(004).
             03 01ACP019VE              PIC X(004).
             03 01ACP020VE              PIC X(004).
             03 01ACP021VE              PIC X(004).
             03 01ACP022VE              PIC X(004).
             03 01ACP023VE              PIC X(004).
             03 01ACP024VE              PIC X(004).
             03 01ACP025VE              PIC X(004).
             03 01ACP026VE              PIC X(004).
             03 01ACP027VE              PIC X(004).
             03 01ACP028VE              PIC X(004).
             03 01ACP029VE              PIC X(004).
             03 01ACP030VE              PIC X(004).
             03 01ACP031VE              PIC X(004).
             03 01ACP032VE              PIC X(004).
             03 01ACP033VE              PIC X(004).
             03 01ACP034VE              PIC X(004).
             03 01ACP035VE              PIC X(004).
             03 01ACP036VE              PIC X(004).
             03 01ACP037VE              PIC X(004).
             03 01ACP038VE              PIC X(004).
             03 01ACP039VE              PIC X(004).
             03 01ACP040VE              PIC X(004).
             03 01ACP041VE              PIC X(004).
             03 01ACP042VE              PIC X(004).

             03 01ACPPTFHP              PIC X(002).
             03 01ACP001HP              PIC X(004).
             03 01ACP002HP              PIC X(004).
             03 01ACP003HP              PIC X(004).
             03 01ACP004HP              PIC X(004).
             03 01ACP005HP              PIC X(004).
             03 01ACP006HP              PIC X(004).
             03 01ACP007HP              PIC X(004).
             03 01ACP008HP              PIC X(004).
             03 01ACP009HP              PIC X(004).
             03 01ACP010HP              PIC X(004).
             03 01ACP011HP              PIC X(004).
             03 01ACP012HP              PIC X(004).
             03 01ACP013HP              PIC X(004).
             03 01ACP014HP              PIC X(004).
             03 01ACP015HP              PIC X(004).
             03 01ACP016HP              PIC X(004).
             03 01ACP017HP              PIC X(004).
             03 01ACP018HP              PIC X(004).
             03 01ACP019HP              PIC X(004).
             03 01ACP020HP              PIC X(004).
             03 01ACP021HP              PIC X(004).
             03 01ACP022HP              PIC X(004).
             03 01ACP023HP              PIC X(004).
             03 01ACP024HP              PIC X(004).
             03 01ACP025HP              PIC X(004).
             03 01ACP026HP              PIC X(004).
             03 01ACP027HP              PIC X(004).
             03 01ACP028HP              PIC X(004).
             03 01ACP029HP              PIC X(004).
             03 01ACP030HP              PIC X(004).
             03 01ACP031HP              PIC X(004).
             03 01ACP032HP              PIC X(004).
             03 01ACP033HP              PIC X(004).
             03 01ACP034HP              PIC X(004).
             03 01ACP035HP              PIC X(004).
             03 01ACP036HP              PIC X(004).
             03 01ACP037HP              PIC X(004).
             03 01ACP038HP              PIC X(004).
             03 01ACP039HP              PIC X(004).
             03 01ACP040HP              PIC X(004).
             03 01ACP041HP              PIC X(004).
             03 01ACP042HP              PIC X(004).

             03 01ACPPTFOT              PIC X(002).
             03 01ACP001OT              PIC X(004).
             03 01ACP002OT              PIC X(004).
             03 01ACP003OT              PIC X(004).
             03 01ACP004OT              PIC X(004).
             03 01ACP005OT              PIC X(004).
             03 01ACP006OT              PIC X(004).
             03 01ACP007OT              PIC X(004).
             03 01ACP008OT              PIC X(004).
             03 01ACP009OT              PIC X(004).
             03 01ACP010OT              PIC X(004).
             03 01ACP011OT              PIC X(004).
             03 01ACP012OT              PIC X(004).
             03 01ACP013OT              PIC X(004).
             03 01ACP014OT              PIC X(004).
             03 01ACP015OT              PIC X(004).
             03 01ACP016OT              PIC X(004).
             03 01ACP017OT              PIC X(004).
             03 01ACP018OT              PIC X(004).
             03 01ACP019OT              PIC X(004).
             03 01ACP020OT              PIC X(004).
             03 01ACP021OT              PIC X(004).
             03 01ACP022OT              PIC X(004).
             03 01ACP023OT              PIC X(004).
             03 01ACP024OT              PIC X(004).
             03 01ACP025OT              PIC X(004).
             03 01ACP026OT              PIC X(004).
             03 01ACP027OT              PIC X(004).
             03 01ACP028OT              PIC X(004).
             03 01ACP029OT              PIC X(004).
             03 01ACP030OT              PIC X(004).
             03 01ACP031OT              PIC X(004).
             03 01ACP032OT              PIC X(004).
             03 01ACP033OT              PIC X(004).
             03 01ACP034OT              PIC X(004).
             03 01ACP035OT              PIC X(004).
             03 01ACP036OT              PIC X(004).
             03 01ACP037OT              PIC X(004).
             03 01ACP038OT              PIC X(004).
             03 01ACP039OT              PIC X(004).
             03 01ACP040OT              PIC X(004).
             03 01ACP041OT              PIC X(004).
             03 01ACP042OT              PIC X(004).

             03 01ACPPTFCC              PIC X(002).
             03 01ACP001CC              PIC X(004).
             03 01ACP002CC              PIC X(004).
             03 01ACP003CC              PIC X(004).
             03 01ACP004CC              PIC X(004).
             03 01ACP005CC              PIC X(004).
             03 01ACP006CC              PIC X(004).
             03 01ACP007CC              PIC X(004).
             03 01ACP008CC              PIC X(004).
             03 01ACP009CC              PIC X(004).
             03 01ACP010CC              PIC X(004).
             03 01ACP011CC              PIC X(004).
             03 01ACP012CC              PIC X(004).
             03 01ACP013CC              PIC X(004).
             03 01ACP014CC              PIC X(004).
             03 01ACP015CC              PIC X(004).
             03 01ACP016CC              PIC X(004).
             03 01ACP017CC              PIC X(004).
             03 01ACP018CC              PIC X(004).
             03 01ACP019CC              PIC X(004).
             03 01ACP020CC              PIC X(004).
             03 01ACP021CC              PIC X(004).
             03 01ACP022CC              PIC X(004).
             03 01ACP023CC              PIC X(004).
             03 01ACP024CC              PIC X(004).
             03 01ACP025CC              PIC X(004).
             03 01ACP026CC              PIC X(004).
             03 01ACP027CC              PIC X(004).
             03 01ACP028CC              PIC X(004).
             03 01ACP029CC              PIC X(004).
             03 01ACP030CC              PIC X(004).
             03 01ACP031CC              PIC X(004).
             03 01ACP032CC              PIC X(004).
             03 01ACP033CC              PIC X(004).
             03 01ACP034CC              PIC X(004).
             03 01ACP035CC              PIC X(004).
             03 01ACP036CC              PIC X(004).
             03 01ACP037CC              PIC X(004).
             03 01ACP038CC              PIC X(004).
             03 01ACP039CC              PIC X(004).
             03 01ACP040CC              PIC X(004).
             03 01ACP041CC              PIC X(004).
             03 01ACP042CC              PIC X(004).

             03 01ACPPTFCO              PIC X(002).
             03 01ACP001CO              PIC X(004).
             03 01ACP002CO              PIC X(004).
             03 01ACP003CO              PIC X(004).
             03 01ACP004CO              PIC X(004).
             03 01ACP005CO              PIC X(004).
             03 01ACP006CO              PIC X(004).
             03 01ACP007CO              PIC X(004).
             03 01ACP008CO              PIC X(004).
             03 01ACP009CO              PIC X(004).
             03 01ACP010CO              PIC X(004).
             03 01ACP011CO              PIC X(004).
             03 01ACP012CO              PIC X(004).
             03 01ACP013CO              PIC X(004).
             03 01ACP014CO              PIC X(004).
             03 01ACP015CO              PIC X(004).
             03 01ACP016CO              PIC X(004).
             03 01ACP017CO              PIC X(004).
             03 01ACP018CO              PIC X(004).
             03 01ACP019CO              PIC X(004).
             03 01ACP020CO              PIC X(004).
             03 01ACP021CO              PIC X(004).
             03 01ACP022CO              PIC X(004).
             03 01ACP023CO              PIC X(004).
             03 01ACP024CO              PIC X(004).
             03 01ACP025CO              PIC X(004).
             03 01ACP026CO              PIC X(004).
             03 01ACP027CO              PIC X(004).
             03 01ACP028CO              PIC X(004).
             03 01ACP029CO              PIC X(004).
             03 01ACP030CO              PIC X(004).
             03 01ACP031CO              PIC X(004).
             03 01ACP032CO              PIC X(004).
             03 01ACP033CO              PIC X(004).
             03 01ACP034CO              PIC X(004).
             03 01ACP035CO              PIC X(004).
             03 01ACP036CO              PIC X(004).
             03 01ACP037CO              PIC X(004).
             03 01ACP038CO              PIC X(004).
             03 01ACP039CO              PIC X(004).
             03 01ACP040CO              PIC X(004).
             03 01ACP041CO              PIC X(004).
             03 01ACP042CO              PIC X(004).

             03 01ACPPTFAH              PIC X(002).
             03 01ACP001AH              PIC X(004).
             03 01ACP002AH              PIC X(004).
             03 01ACP003AH              PIC X(004).
             03 01ACP004AH              PIC X(004).
             03 01ACP005AH              PIC X(004).
             03 01ACP006AH              PIC X(004).
             03 01ACP007AH              PIC X(004).
             03 01ACP008AH              PIC X(004).
             03 01ACP009AH              PIC X(004).
             03 01ACP010AH              PIC X(004).
             03 01ACP011AH              PIC X(004).
             03 01ACP012AH              PIC X(004).
             03 01ACP013AH              PIC X(004).
             03 01ACP014AH              PIC X(004).
             03 01ACP015AH              PIC X(004).
             03 01ACP016AH              PIC X(004).
             03 01ACP017AH              PIC X(004).
             03 01ACP018AH              PIC X(004).
             03 01ACP019AH              PIC X(004).
             03 01ACP020AH              PIC X(004).
             03 01ACP021AH              PIC X(004).
             03 01ACP022AH              PIC X(004).
             03 01ACP023AH              PIC X(004).
             03 01ACP024AH              PIC X(004).
             03 01ACP025AH              PIC X(004).
             03 01ACP026AH              PIC X(004).
             03 01ACP027AH              PIC X(004).
             03 01ACP028AH              PIC X(004).
             03 01ACP029AH              PIC X(004).
             03 01ACP030AH              PIC X(004).
             03 01ACP031AH              PIC X(004).
             03 01ACP032AH              PIC X(004).
             03 01ACP033AH              PIC X(004).
             03 01ACP034AH              PIC X(004).
             03 01ACP035AH              PIC X(004).
             03 01ACP036AH              PIC X(004).
             03 01ACP037AH              PIC X(004).
             03 01ACP038AH              PIC X(004).
             03 01ACP039AH              PIC X(004).
             03 01ACP040AH              PIC X(004).
             03 01ACP041AH              PIC X(004).
             03 01ACP042AH              PIC X(004).

             03 01ACPPTFCT              PIC X(002).
             03 01ACP001CT              PIC X(004).
             03 01ACP002CT              PIC X(004).
             03 01ACP003CT              PIC X(004).
             03 01ACP004CT              PIC X(004).
             03 01ACP005CT              PIC X(004).
             03 01ACP006CT              PIC X(004).
             03 01ACP007CT              PIC X(004).
             03 01ACP008CT              PIC X(004).
             03 01ACP009CT              PIC X(004).
             03 01ACP010CT              PIC X(004).
             03 01ACP011CT              PIC X(004).
             03 01ACP012CT              PIC X(004).
             03 01ACP013CT              PIC X(004).
             03 01ACP014CT              PIC X(004).
             03 01ACP015CT              PIC X(004).
             03 01ACP016CT              PIC X(004).
             03 01ACP017CT              PIC X(004).
             03 01ACP018CT              PIC X(004).
             03 01ACP019CT              PIC X(004).
             03 01ACP020CT              PIC X(004).
             03 01ACP021CT              PIC X(004).
             03 01ACP022CT              PIC X(004).
             03 01ACP023CT              PIC X(004).
             03 01ACP024CT              PIC X(004).
             03 01ACP025CT              PIC X(004).
             03 01ACP026CT              PIC X(004).
             03 01ACP027CT              PIC X(004).
             03 01ACP028CT              PIC X(004).
             03 01ACP029CT              PIC X(004).
             03 01ACP030CT              PIC X(004).
             03 01ACP031CT              PIC X(004).
             03 01ACP032CT              PIC X(004).
             03 01ACP033CT              PIC X(004).
             03 01ACP034CT              PIC X(004).
             03 01ACP035CT              PIC X(004).
             03 01ACP036CT              PIC X(004).
             03 01ACP037CT              PIC X(004).
             03 01ACP038CT              PIC X(004).
             03 01ACP039CT              PIC X(004).
             03 01ACP040CT              PIC X(004).
             03 01ACP041CT              PIC X(004).
             03 01ACP042CT              PIC X(004).

       01 VAR-01EXP.
          02 TABLA-PORTAFOLIOS-01EXP OCCURS 9 TIMES.
             03 01EXPPTF                PIC X(002).
             03 01EXP001                PIC 9(003).
             03 01EXP002                PIC 9(003).
             03 01EXP003                PIC 9(003).
             03 01EXP004                PIC 9(003).
             03 01EXP005                PIC 9(003).
             03 01EXP006                PIC 9(003).
             03 01EXP007                PIC 9(004).
             03 01EXP008                PIC 9(004).
             03 01EXP009                PIC 9(004).
             03 01EXP010                PIC 9(004).
             03 01EXP011                PIC 9(004).
             03 01EXP012                PIC 9(004).
             03 01EXP013                PIC 9(004).
             03 01EXPSUM                PIC 9(006).
             03 01EXPTOT                PIC 9(004).
          02 TABLA-PORTAFOLIOS-01EXP-R REDEFINES
                                       TABLA-PORTAFOLIOS-01EXP.
             03 01EXPPTFRO              PIC X(002).
             03 01EXP001RO              PIC X(003).
             03 01EXP002RO              PIC X(003).
             03 01EXP003RO              PIC X(003).
             03 01EXP004RO              PIC X(003).
             03 01EXP005RO              PIC X(003).
             03 01EXP006RO              PIC X(003).
             03 01EXP007RO              PIC X(004).
             03 01EXP008RO              PIC X(004).
             03 01EXP009RO              PIC X(004).
             03 01EXP010RO              PIC X(004).
             03 01EXP011RO              PIC X(004).
             03 01EXP012RO              PIC X(004).
             03 01EXP013RO              PIC X(004).
             03 01EXPSUMRO              PIC X(006).
             03 01EXPTOTRO              PIC X(004).

             03 01EXPPTFIN              PIC X(002).
             03 01EXP001IN              PIC X(003).
             03 01EXP002IN              PIC X(003).
             03 01EXP003IN              PIC X(003).
             03 01EXP004IN              PIC X(003).
             03 01EXP005IN              PIC X(003).
             03 01EXP006IN              PIC X(003).
             03 01EXP007IN              PIC X(004).
             03 01EXP008IN              PIC X(004).
             03 01EXP009IN              PIC X(004).
             03 01EXP010IN              PIC X(004).
             03 01EXP011IN              PIC X(004).
             03 01EXP012IN              PIC X(004).
             03 01EXP013IN              PIC X(004).
             03 01EXPSUMIN              PIC X(006).
             03 01EXPTOTIN              PIC X(004).

             03 01EXPPTFVE              PIC X(002).
             03 01EXP001VE              PIC X(003).
             03 01EXP002VE              PIC X(003).
             03 01EXP003VE              PIC X(003).
             03 01EXP004VE              PIC X(003).
             03 01EXP005VE              PIC X(003).
             03 01EXP006VE              PIC X(003).
             03 01EXP007VE              PIC X(004).
             03 01EXP008VE              PIC X(004).
             03 01EXP009VE              PIC X(004).
             03 01EXP010VE              PIC X(004).
             03 01EXP011VE              PIC X(004).
             03 01EXP012VE              PIC X(004).
             03 01EXP013VE              PIC X(004).
             03 01EXPSUMVE              PIC X(006).
             03 01EXPTOTVE              PIC X(004).

             03 01EXPPTFHP              PIC X(002).
             03 01EXP001HP              PIC X(003).
             03 01EXP002HP              PIC X(003).
             03 01EXP003HP              PIC X(003).
             03 01EXP004HP              PIC X(003).
             03 01EXP005HP              PIC X(003).
             03 01EXP006HP              PIC X(003).
             03 01EXP007HP              PIC X(004).
             03 01EXP008HP              PIC X(004).
             03 01EXP009HP              PIC X(004).
             03 01EXP010HP              PIC X(004).
             03 01EXP011HP              PIC X(004).
             03 01EXP012HP              PIC X(004).
             03 01EXP013HP              PIC X(004).
             03 01EXPSUMHP              PIC X(006).
             03 01EXPTOTHP              PIC X(004).

             03 01EXPPTFOT              PIC X(002).
             03 01EXP001OT              PIC X(003).
             03 01EXP002OT              PIC X(003).
             03 01EXP003OT              PIC X(003).
             03 01EXP004OT              PIC X(003).
             03 01EXP005OT              PIC X(003).
             03 01EXP006OT              PIC X(003).
             03 01EXP007OT              PIC X(004).
             03 01EXP008OT              PIC X(004).
             03 01EXP009OT              PIC X(004).
             03 01EXP010OT              PIC X(004).
             03 01EXP011OT              PIC X(004).
             03 01EXP012OT              PIC X(004).
             03 01EXP013OT              PIC X(004).
             03 01EXPSUMOT              PIC X(006).
             03 01EXPTOTOT              PIC X(004).

             03 01EXPPTFCC              PIC X(002).
             03 01EXP001CC              PIC X(003).
             03 01EXP002CC              PIC X(003).
             03 01EXP003CC              PIC X(003).
             03 01EXP004CC              PIC X(003).
             03 01EXP005CC              PIC X(003).
             03 01EXP006CC              PIC X(003).
             03 01EXP007CC              PIC X(004).
             03 01EXP008CC              PIC X(004).
             03 01EXP009CC              PIC X(004).
             03 01EXP010CC              PIC X(004).
             03 01EXP011CC              PIC X(004).
             03 01EXP012CC              PIC X(004).
             03 01EXP013CC              PIC X(004).
             03 01EXPSUMCC              PIC X(006).
             03 01EXPTOTCC              PIC X(004). 

             03 01EXPPTFCO              PIC X(002).
             03 01EXP001CO              PIC X(003).
             03 01EXP002CO              PIC X(003).
             03 01EXP003CO              PIC X(003).
             03 01EXP004CO              PIC X(003).
             03 01EXP005CO              PIC X(003).
             03 01EXP006CO              PIC X(003).
             03 01EXP007CO              PIC X(004).
             03 01EXP008CO              PIC X(004).
             03 01EXP009CO              PIC X(004).
             03 01EXP010CO              PIC X(004).
             03 01EXP011CO              PIC X(004).
             03 01EXP012CO              PIC X(004).
             03 01EXP013CO              PIC X(004).
             03 01EXPSUMCO              PIC X(006).
             03 01EXPTOTCO              PIC X(004).


             03 01EXPPTFAH              PIC X(002).
             03 01EXP001AH              PIC X(003).
             03 01EXP002AH              PIC X(003).
             03 01EXP003AH              PIC X(003).
             03 01EXP004AH              PIC X(003).
             03 01EXP005AH              PIC X(003).
             03 01EXP006AH              PIC X(003).
             03 01EXP007AH              PIC X(004).
             03 01EXP008AH              PIC X(004).
             03 01EXP009AH              PIC X(004).
             03 01EXP010AH              PIC X(004).
             03 01EXP011AH              PIC X(004).
             03 01EXP012AH              PIC X(004).
             03 01EXP013AH              PIC X(004).
             03 01EXPSUMAH              PIC X(006).
             03 01EXPTOTAH              PIC X(004).

             03 01EXPPTFCT              PIC X(002).
             03 01EXP001CT              PIC X(003).
             03 01EXP002CT              PIC X(003).
             03 01EXP003CT              PIC X(003).
             03 01EXP004CT              PIC X(003).
             03 01EXP005CT              PIC X(003).
             03 01EXP006CT              PIC X(003).
             03 01EXP007CT              PIC X(004).
             03 01EXP008CT              PIC X(004).
             03 01EXP009CT              PIC X(004).
             03 01EXP010CT              PIC X(004).
             03 01EXP011CT              PIC X(004).
             03 01EXP012CT              PIC X(004).
             03 01EXP013CT              PIC X(004).
             03 01EXPSUMCT              PIC X(006).
             03 01EXPTOTCT              PIC X(004).


       01 VAR-01DEM.
          03 01DEMTIP                   PIC 9(001).
          03 01DEMNRO                   PIC 9(011).
          03 01DEM001                   PIC 9(002).
ACIEAA    03 01DEM031                   PIC 9(001).                     RMB0006

       01 VAR-01END.
         03 TABLA-PORTAFOLIOS-01END OCCURS 9 TIMES.
         05 01END-EDITADOS.
          07 01ENDPTF                   PIC X(002).
          07 01END001                   PIC 99999999.99.
          07 01END002                   PIC 99999999.99.
          07 01END003                   PIC 99999999.99.
          07 01END004                   PIC 99999999.99.
          07 01END005                   PIC 99999999.99.
          07 01END006                   PIC 99999999.99.
          07 01END007                   PIC 99999999.99.
          07 01END008                   PIC 99999999.99.
          07 01END009                   PIC 99999999.99.
          07 01END010                   PIC 99999999.99.
          07 01END011                   PIC 99999999.99.
          07 01END012                   PIC 99999999.99.
          07 01END013                   PIC 99999999.99.
          07 01END014                   PIC 99999999.99.
          07 01END015                   PIC 99999999.99.
          07 01END016                   PIC 99999999.99.
          07 01END017                   PIC 99999999.99.
          07 01END018                   PIC 99999999.99.
          07 01END019                   PIC 99999999.99.
          07 01END020                   PIC 99999999.99.
          07 01END021                   PIC 99999999.99.
          07 01END022                   PIC 99999999.99.
          07 01END023                   PIC 99999999.99.
          07 01END024                   PIC 99999999.99.
          07 01END025                   PIC 99999999.99.
          07 01END026                   PIC 99999999.99.
          07 01END027                   PIC 99999999.99.
          07 01END028                   PIC 99999999.99.
          07 01END029                   PIC 99999999.99.
          07 01END030                   PIC 99999999.99.
          07 01END031                   PIC 99999999.99.
          07 01END032                   PIC 99999999.99.
          07 01END033                   PIC 99999999.99.
          07 01END034                   PIC 99999999.99.
          07 01END035                   PIC 99999999.99.
          07 01END036                   PIC 99999999.99.
          07 01END037                   PIC 99999999.99.
          07 01END038                   PIC 99999999.99.
          07 01END039                   PIC 99999999.99.
          07 01END040                   PIC 99999999.99.
          07 01END041                   PIC 99999999.99.
          07 01END042                   PIC 99999999.99.
          07 01END043                   PIC 99999999.99.
          07 01END044                   PIC 99999999.99.
          07 01END045                   PIC 99999999.99.
          07 01END046                   PIC 99999999.99.
          07 01END047                   PIC 99999999.99.
          07 01END048                   PIC 99999999.99.
          07 01END049                   PIC 99999999.99.
          07 01END050                   PIC 99999999.99.
          07 01END051                   PIC 99999999.99.
          07 01END052                   PIC 99999999.99.
          07 01END053                   PIC 99999999.99.
          07 01END054                   PIC 99999999.99.
          07 01END055                   PIC 99999999.99.
          07 01END056                   PIC 99999999.99.
          07 01END057                   PIC 99999999.99.
          07 01END058                   PIC 99999999.99.
          07 01END059                   PIC 99999999.99.
          07 01END060                   PIC 99999999.99.
          07 01END061                   PIC 99999999.99.
          07 01END062                   PIC 99999999.99.
          07 01END063                   PIC 99999999.99.
          07 01END064                   PIC 99999999.99.
          07 01END065                   PIC 99999999.99.
          07 01END066                   PIC 99999999.99.
          07 01END067                   PIC 99999999.99.
          07 01END068                   PIC 99999999.99.
          07 01END069                   PIC 99999999.99.
          07 01END070                   PIC 99999999.99.
          07 01END071                   PIC 99999999.99.
          07 01END072                   PIC 99999999.99.
          07 01END073                   PIC 99999999.99.
          07 01END074                   PIC 99999999.99.
          07 01END075                   PIC 99999999.99.
          07 01END076                   PIC 99999999.99.
          07 01END077                   PIC 99999999.99.
          07 01END078                   PIC 99999999.99.
          07 01END079                   PIC 99999999.99.
          07 01END080                   PIC 99999999.99.
          07 01END081                   PIC 99999999.99.
          07 01END082                   PIC 99999999.99.
          07 01END083                   PIC 99999999.99.
          07 01END084                   PIC 99999999.99.
          07 01END085                   PIC 99999999.99.
          07 01END086                   PIC 99999999.99.
          07 01END087                   PIC 99999999.99.
          07 01END088                   PIC 99999999.99.
          07 01END089                   PIC 99999999.99.
ACIEAA    07 01END090                   PIC 99999999.99.                RMB0006
ACIEAA    07 01END091                   PIC 99999999.99.                RMB0006
ACIEAA    07 01END092                   PIC 99999999.99.                RMB0006
ACIEAA    07 01END093                   PIC 99999999.99.                RMB0006
         05 01END-VALORES.
          07 01ENDV01                   PIC 9(008)V99.
          07 01ENDV02                   PIC 9(008)V99.
          07 01ENDV03                   PIC 9(008)V99.
          07 01ENDV04                   PIC 9(008)V99.
          07 01ENDV05                   PIC 9(008)V99.
          07 01ENDV06                   PIC 9(008)V99.
          07 01ENDV07                   PIC 9(008)V99.
          07 01ENDV08                   PIC 9(008)V99.
          07 01ENDV09                   PIC 9(008)V99.
          07 01ENDV10                   PIC 9(008)V99.
          07 01ENDV11                   PIC 9(008)V99.
          07 01ENDV12                   PIC 9(008)V99.
          07 01ENDV13                   PIC 9(008)V99.
          07 01ENDV14                   PIC 9(008)V99.
          07 01ENDV15                   PIC 9(008)V99.
          07 01ENDV16                   PIC 9(008)V99.
          07 01ENDV17                   PIC 9(008)V99.
          07 01ENDV18                   PIC 9(008)V99.
          07 01ENDV19                   PIC 9(008)V99.
          07 01ENDV20                   PIC 9(008)V99.
          07 01ENDV21                   PIC 9(008)V99.
          07 01ENDV22                   PIC 9(008)V99.
          07 01ENDV23                   PIC 9(008)V99.
          07 01ENDV24                   PIC 9(008)V99.
          07 01ENDV25                   PIC 9(008)V99.
          07 01ENDV26                   PIC 9(008)V99.
          07 01ENDV27                   PIC 9(008)V99.
          07 01ENDV28                   PIC 9(008)V99.
          07 01ENDV29                   PIC 9(008)V99.
          07 01ENDV30                   PIC 9(008)V99.
          07 01ENDV31                   PIC 9(008)V99.
          07 01ENDV32                   PIC 9(008)V99.
          07 01ENDV33                   PIC 9(008)V99.
          07 01ENDV34                   PIC 9(008)V99.
          07 01ENDV35                   PIC 9(008)V99.
          07 01ENDV36                   PIC 9(008)V99.
          07 01ENDV37                   PIC 9(008)V99.
          07 01ENDV38                   PIC 9(008)V99.
          07 01ENDV39                   PIC 9(008)V99.
          07 01ENDV40                   PIC 9(008)V99.
          07 01ENDV41                   PIC 9(008)V99.
          07 01ENDV42                   PIC 9(008)V99.
          07 01ENDV43                   PIC 9(008)V99.
          07 01ENDV44                   PIC 9(008)V99.
          07 01ENDV45                   PIC 9(008)V99.
          07 01ENDV46                   PIC 9(008)V99.
          07 01ENDV47                   PIC 9(008)V99.
          07 01ENDV48                   PIC 9(008)V99.
          07 01ENDV49                   PIC 9(008)V99.
          07 01ENDV50                   PIC 9(008)V99.
          07 01ENDV51                   PIC 9(008)V99.
          07 01ENDV52                   PIC 9(008)V99.
          07 01ENDV53                   PIC 9(008)V99.
          07 01ENDV54                   PIC 9(008)V99.
          07 01ENDV55                   PIC 9(008)V99.
          07 01ENDV56                   PIC 9(008)V99.
          07 01ENDV57                   PIC 9(008)V99.
          07 01ENDV58                   PIC 9(008)V99.
          07 01ENDV59                   PIC 9(008)V99.
          07 01ENDV60                   PIC 9(008)V99.
          07 01ENDV61                   PIC 9(008)V99.
          07 01ENDV62                   PIC 9(008)V99.
          07 01ENDV63                   PIC 9(008)V99.
          07 01ENDV64                   PIC 9(008)V99.
          07 01ENDV65                   PIC 9(008)V99.
          07 01ENDV66                   PIC 9(008)V99.
          07 01ENDV67                   PIC 9(008)V99.
          07 01ENDV68                   PIC 9(008)V99.
          07 01ENDV69                   PIC 9(008)V99.
          07 01ENDV70                   PIC 9(008)V99.
          07 01ENDV71                   PIC 9(008)V99.
          07 01ENDV72                   PIC 9(008)V99.
          07 01ENDV73                   PIC 9(008)V99.
          07 01ENDV74                   PIC 9(008)V99.
          07 01ENDV75                   PIC 9(008)V99.
          07 01ENDV76C                  PIC 9(008)V99.
          07 01ENDV76S                  PIC 9(008)V99.
          07 01ENDV77C                  PIC 9(008)V99.
          07 01ENDV77S                  PIC 9(008)V99.
          07 01ENDV78C                  PIC 9(008)V99.
          07 01ENDV78S                  PIC 9(008)V99.
          07 01ENDV79C                  PIC 9(008)V99.
          07 01ENDV79S                  PIC 9(008)V99.
          07 01ENDV80C                  PIC 9(008)V99.
          07 01ENDV80S                  PIC 9(008)V99.
          07 01ENDV81                   PIC 9(008)V99.
          07 01ENDV82                   PIC 9(008)V99.
          07 01ENDV83                   PIC 9(008)V99.
          07 01ENDV84                   PIC 9(008)V99.
          07 01ENDV85                   PIC 9(008)V99.
          07 01ENDV86C                  PIC 9(008)V99.
          07 01ENDV86S                  PIC 9(008)V99.
          07 01ENDV87C                  PIC 9(008)V99.
          07 01ENDV87S                  PIC 9(008)V99.
          07 01ENDV88C                  PIC 9(008)V99.
          07 01ENDV88S                  PIC 9(008)V99.
          07 01ENDV89C                  PIC 9(008)V99.
          07 01ENDV89S                  PIC 9(008)V99.
ACIEAA    07 01ENDV90                   PIC 9(008)V99.                  RMB0006
ACIEAA    07 01ENDV91                   PIC 9(008)V99.                  RMB0006
ACIEAA    07 01ENDV92                   PIC 9(008)V99.                  RMB0006
ACIEAA    07 01ENDV93                   PIC 9(008)V99.                  RMB0006
          07 01ENDC01                   PIC 9(005).
          07 01ENDC02                   PIC 9(005).
          07 01ENDC03                   PIC 9(005).
          07 01ENDC04                   PIC 9(005).
          07 01ENDC05                   PIC 9(005).
          07 01ENDC06                   PIC 9(005).
          07 01ENDC07                   PIC 9(005).
          07 01ENDC08                   PIC 9(005).
          07 01ENDC09                   PIC 9(005).
          07 01ENDC10                   PIC 9(005).
          07 01ENDC11                   PIC 9(005).
          07 01ENDC12                   PIC 9(005).
          07 01ENDC13                   PIC 9(005).
          07 01ENDC14                   PIC 9(005).
          07 01ENDC15                   PIC 9(005).
          07 01ENDC16                   PIC 9(005).
          07 01ENDC17                   PIC 9(005).
          07 01ENDC18                   PIC 9(005).
          07 01ENDC19                   PIC 9(005).
          07 01ENDC20                   PIC 9(005).
          07 01ENDC21                   PIC 9(005).
          07 01ENDC22                   PIC 9(005).
          07 01ENDC23                   PIC 9(005).
          07 01ENDC24                   PIC 9(005).
          07 01ENDC25                   PIC 9(005).
          07 01ENDC26                   PIC 9(005).
          07 01ENDC27                   PIC 9(005).
          07 01ENDC28                   PIC 9(005).
          07 01ENDC29                   PIC 9(005).
          07 01ENDC30                   PIC 9(005).
          07 01ENDC31                   PIC 9(005).
          07 01ENDC32                   PIC 9(005).
          07 01ENDC33                   PIC 9(005).
          07 01ENDC34                   PIC 9(005).
          07 01ENDC35                   PIC 9(005).
          07 01ENDC36                   PIC 9(005).
          07 01ENDC37                   PIC 9(005).
          07 01ENDC38                   PIC 9(005).
          07 01ENDC39                   PIC 9(005).
          07 01ENDC40                   PIC 9(005).
          07 01ENDC41                   PIC 9(005).
          07 01ENDC42                   PIC 9(005).
          07 01ENDC43                   PIC 9(005).
          07 01ENDC44                   PIC 9(005).
          07 01ENDC45                   PIC 9(005).
          07 01ENDC46                   PIC 9(005).
          07 01ENDC47                   PIC 9(005).
          07 01ENDC48                   PIC 9(005).
          07 01ENDC49                   PIC 9(005).
          07 01ENDC50                   PIC 9(005).
          07 01ENDC51                   PIC 9(005).
          07 01ENDC52                   PIC 9(005).
          07 01ENDC53                   PIC 9(005).
          07 01ENDC54                   PIC 9(005).
          07 01ENDC55                   PIC 9(005).
          07 01ENDC56                   PIC 9(005).
          07 01ENDC57                   PIC 9(005).
          07 01ENDC58                   PIC 9(005).
          07 01ENDC59                   PIC 9(005).
          07 01ENDC60                   PIC 9(005).
          07 01ENDC61                   PIC 9(005).
          07 01ENDC62                   PIC 9(005).
          07 01ENDC63                   PIC 9(005).
          07 01ENDC64                   PIC 9(005).
          07 01ENDC65                   PIC 9(005).
          07 01ENDC66                   PIC 9(005).
          07 01ENDC67                   PIC 9(005).
          07 01ENDC68                   PIC 9(005).
          07 01ENDC69                   PIC 9(005).
          07 01ENDC70                   PIC 9(005).
          07 01ENDC71                   PIC 9(005).
          07 01ENDC72                   PIC 9(005).
          07 01ENDC73                   PIC 9(005).
          07 01ENDC74                   PIC 9(005).
          07 01ENDC75                   PIC 9(005).
          07 01ENDC76                   PIC 9(005).
          07 01ENDC77                   PIC 9(005).
          07 01ENDC78                   PIC 9(005).
          07 01ENDC79                   PIC 9(005).
          07 01ENDC80                   PIC 9(005).
          07 01ENDC81                   PIC 9(005).
          07 01ENDC82                   PIC 9(005).
          07 01ENDC83                   PIC 9(005).
          07 01ENDC84                   PIC 9(005).
          07 01ENDC85                   PIC 9(005).
          07 01ENDC86                   PIC 9(005).
          07 01ENDC87                   PIC 9(005).
          07 01ENDC88                   PIC 9(005).
          07 01ENDC89                   PIC 9(005).
ACIEAA    07 01ENDC90                   PIC 9(005).                     RMB0006
ACIEAA    07 01ENDC91                   PIC 9(005).                     RMB0006
ACIEAA    07 01ENDC92                   PIC 9(005).                     RMB0006
ACIEAA    07 01ENDC93                   PIC 9(005).                     RMB0006
         03 TABLA-PORTAFOLIOS-01END-R REDEFINES
                                      TABLA-PORTAFOLIOS-01END.
         05 01ENDRO-EDITADOS.
          07 01ENDPTFRO                 PIC X(002).
          07 01END001RO                 PIC X(011).
          07 01END002RO                 PIC X(011).
          07 01END003RO                 PIC X(011).
          07 01END004RO                 PIC X(011).
          07 01END005RO                 PIC X(011).
          07 01END006RO                 PIC X(011).
          07 01END007RO                 PIC X(011).
          07 01END008RO                 PIC X(011).
          07 01END009RO                 PIC X(011).
          07 01END010RO                 PIC X(011).
          07 01END011RO                 PIC X(011).
          07 01END012RO                 PIC X(011).
          07 01END013RO                 PIC X(011).
          07 01END014RO                 PIC X(011).
          07 01END015RO                 PIC X(011).
          07 01END016RO                 PIC X(011).
          07 01END017RO                 PIC X(011).
          07 01END018RO                 PIC X(011).
          07 01END019RO                 PIC X(011).
          07 01END020RO                 PIC X(011).
          07 01END021RO                 PIC X(011).
          07 01END022RO                 PIC X(011).
          07 01END023RO                 PIC X(011).
          07 01END024RO                 PIC X(011).
          07 01END025RO                 PIC X(011).
          07 01END026RO                 PIC X(011).
          07 01END027RO                 PIC X(011).
          07 01END028RO                 PIC X(011).
          07 01END029RO                 PIC X(011).
          07 01END030RO                 PIC X(011).
          07 01END031RO                 PIC X(011).
          07 01END032RO                 PIC X(011).
          07 01END033RO                 PIC X(011).
          07 01END034RO                 PIC X(011).
          07 01END035RO                 PIC X(011).
          07 01END036RO                 PIC X(011).
          07 01END037RO                 PIC X(011).
          07 01END038RO                 PIC X(011).
          07 01END039RO                 PIC X(011).
          07 01END040RO                 PIC X(011).
          07 01END041RO                 PIC X(011).
          07 01END042RO                 PIC X(011).
          07 01END043RO                 PIC X(011).
          07 01END044RO                 PIC X(011).
          07 01END045RO                 PIC X(011).
          07 01END046RO                 PIC X(011).
          07 01END047RO                 PIC X(011).
          07 01END048RO                 PIC X(011).
          07 01END049RO                 PIC X(011).
          07 01END050RO                 PIC X(011).
          07 01END051RO                 PIC X(011).
          07 01END052RO                 PIC X(011).
          07 01END053RO                 PIC X(011).
          07 01END054RO                 PIC X(011).
          07 01END055RO                 PIC X(011).
          07 01END056RO                 PIC X(011).
          07 01END057RO                 PIC X(011).
          07 01END058RO                 PIC X(011).
          07 01END059RO                 PIC X(011).
          07 01END060RO                 PIC X(011).
          07 01END061RO                 PIC X(011).
          07 01END062RO                 PIC X(011).
          07 01END063RO                 PIC X(011).
          07 01END064RO                 PIC X(011).
          07 01END065RO                 PIC X(011).
          07 01END066RO                 PIC X(011).
          07 01END067RO                 PIC X(011).
          07 01END068RO                 PIC X(011).
          07 01END069RO                 PIC X(011).
          07 01END070RO                 PIC X(011).
          07 01END071RO                 PIC X(011).
          07 01END072RO                 PIC X(011).
          07 01END073RO                 PIC X(011).
          07 01END074RO                 PIC X(011).
          07 01END075RO                 PIC X(011).
          07 01END076RO                 PIC X(011).
          07 01END077RO                 PIC X(011).
          07 01END078RO                 PIC X(011).
          07 01END079RO                 PIC X(011).
          07 01END080RO                 PIC X(011).
          07 01END081RO                 PIC X(011).
          07 01END082RO                 PIC X(011).
          07 01END083RO                 PIC X(011).
          07 01END084RO                 PIC X(011).
          07 01END085RO                 PIC X(011).
          07 01END086RO                 PIC X(011).
          07 01END087RO                 PIC X(011).
          07 01END088RO                 PIC X(011).
          07 01END089RO                 PIC X(011).
ACIEAA    07 01END090RO                 PIC X(011).                     RMB0006
ACIEAA    07 01END091RO                 PIC X(011).                     RMB0006
ACIEAA    07 01END092RO                 PIC X(011).                     RMB0006
ACIEAA    07 01END093RO                 PIC X(011).                     RMB0006
         05 01ENDRO-VALORES.
ACIEAA*   07 FILLER-01-ENDRO            PIC X(1425).                    RMB0006
ACIEAA    07 FILLER-01-ENDRO            PIC X(1485).                    RMB0006

         05 01ENDIN-EDITADOS.
          07 01ENDPTFIN                 PIC X(002).
          07 01END001IN                 PIC X(011).
          07 01END002IN                 PIC X(011).
          07 01END003IN                 PIC X(011).
          07 01END004IN                 PIC X(011).
          07 01END005IN                 PIC X(011).
          07 01END006IN                 PIC X(011).
          07 01END007IN                 PIC X(011).
          07 01END008IN                 PIC X(011).
          07 01END009IN                 PIC X(011).
          07 01END010IN                 PIC X(011).
          07 01END011IN                 PIC X(011).
          07 01END012IN                 PIC X(011).
          07 01END013IN                 PIC X(011).
          07 01END014IN                 PIC X(011).
          07 01END015IN                 PIC X(011).
          07 01END016IN                 PIC X(011).
          07 01END017IN                 PIC X(011).
          07 01END018IN                 PIC X(011).
          07 01END019IN                 PIC X(011).
          07 01END020IN                 PIC X(011).
          07 01END021IN                 PIC X(011).
          07 01END022IN                 PIC X(011).
          07 01END023IN                 PIC X(011).
          07 01END024IN                 PIC X(011).
          07 01END025IN                 PIC X(011).
          07 01END026IN                 PIC X(011).
          07 01END027IN                 PIC X(011).
          07 01END028IN                 PIC X(011).
          07 01END029IN                 PIC X(011).
          07 01END030IN                 PIC X(011).
          07 01END031IN                 PIC X(011).
          07 01END032IN                 PIC X(011).
          07 01END033IN                 PIC X(011).
          07 01END034IN                 PIC X(011).
          07 01END035IN                 PIC X(011).
          07 01END036IN                 PIC X(011).
          07 01END037IN                 PIC X(011).
          07 01END038IN                 PIC X(011).
          07 01END039IN                 PIC X(011).
          07 01END040IN                 PIC X(011).
          07 01END041IN                 PIC X(011).
          07 01END042IN                 PIC X(011).
          07 01END043IN                 PIC X(011).
          07 01END044IN                 PIC X(011).
          07 01END045IN                 PIC X(011).
          07 01END046IN                 PIC X(011).
          07 01END047IN                 PIC X(011).
          07 01END048IN                 PIC X(011).
          07 01END049IN                 PIC X(011).
          07 01END050IN                 PIC X(011).
          07 01END051IN                 PIC X(011).
          07 01END052IN                 PIC X(011).
          07 01END053IN                 PIC X(011).
          07 01END054IN                 PIC X(011).
          07 01END055IN                 PIC X(011).
          07 01END056IN                 PIC X(011).
          07 01END057IN                 PIC X(011).
          07 01END058IN                 PIC X(011).
          07 01END059IN                 PIC X(011).
          07 01END060IN                 PIC X(011).
          07 01END061IN                 PIC X(011).
          07 01END062IN                 PIC X(011).
          07 01END063IN                 PIC X(011).
          07 01END064IN                 PIC X(011).
          07 01END065IN                 PIC X(011).
          07 01END066IN                 PIC X(011).
          07 01END067IN                 PIC X(011).
          07 01END068IN                 PIC X(011).
          07 01END069IN                 PIC X(011).
          07 01END070IN                 PIC X(011).
          07 01END071IN                 PIC X(011).
          07 01END072IN                 PIC X(011).
          07 01END073IN                 PIC X(011).
          07 01END074IN                 PIC X(011).
          07 01END075IN                 PIC X(011).
          07 01END076IN                 PIC X(011).
          07 01END077IN                 PIC X(011).
          07 01END078IN                 PIC X(011).
          07 01END079IN                 PIC X(011).
          07 01END080IN                 PIC X(011).
          07 01END081IN                 PIC X(011).
          07 01END082IN                 PIC X(011).
          07 01END083IN                 PIC X(011).
          07 01END084IN                 PIC X(011).
          07 01END085IN                 PIC X(011).
          07 01END086IN                 PIC X(011).
          07 01END087IN                 PIC X(011).
          07 01END088IN                 PIC X(011).
          07 01END089IN                 PIC X(011).
ACIEAA    07 01END090IN                 PIC X(011).                     RMB0006
ACIEAA    07 01END091IN                 PIC X(011).                     RMB0006
ACIEAA    07 01END092IN                 PIC X(011).                     RMB0006
ACIEAA    07 01END093IN                 PIC X(011).                     RMB0006
         05 01ENDIN-VALORES.
ACIEAA*   07 FILLER-01-ENDIN            PIC X(1425).                    RMB0006
ACIEAA    07 FILLER-01-ENDIN            PIC X(1485).                    RMB0006

         05 01ENDVE-EDITADOS.
          07 01ENDPTFVE                 PIC X(002).
          07 01END001VE                 PIC X(011).
          07 01END002VE                 PIC X(011).
          07 01END003VE                 PIC X(011).
          07 01END004VE                 PIC X(011).
          07 01END005VE                 PIC X(011).
          07 01END006VE                 PIC X(011).
          07 01END007VE                 PIC X(011).
          07 01END008VE                 PIC X(011).
          07 01END009VE                 PIC X(011).
          07 01END010VE                 PIC X(011).
          07 01END011VE                 PIC X(011).
          07 01END012VE                 PIC X(011).
          07 01END013VE                 PIC X(011).
          07 01END014VE                 PIC X(011).
          07 01END015VE                 PIC X(011).
          07 01END016VE                 PIC X(011).
          07 01END017VE                 PIC X(011).
          07 01END018VE                 PIC X(011).
          07 01END019VE                 PIC X(011).
          07 01END020VE                 PIC X(011).
          07 01END021VE                 PIC X(011).
          07 01END022VE                 PIC X(011).
          07 01END023VE                 PIC X(011).
          07 01END024VE                 PIC X(011).
          07 01END025VE                 PIC X(011).
          07 01END026VE                 PIC X(011).
          07 01END027VE                 PIC X(011).
          07 01END028VE                 PIC X(011).
          07 01END029VE                 PIC X(011).
          07 01END030VE                 PIC X(011).
          07 01END031VE                 PIC X(011).
          07 01END032VE                 PIC X(011).
          07 01END033VE                 PIC X(011).
          07 01END034VE                 PIC X(011).
          07 01END035VE                 PIC X(011).
          07 01END036VE                 PIC X(011).
          07 01END037VE                 PIC X(011).
          07 01END038VE                 PIC X(011).
          07 01END039VE                 PIC X(011).
          07 01END040VE                 PIC X(011).
          07 01END041VE                 PIC X(011).
          07 01END042VE                 PIC X(011).
          07 01END043VE                 PIC X(011).
          07 01END044VE                 PIC X(011).
          07 01END045VE                 PIC X(011).
          07 01END046VE                 PIC X(011).
          07 01END047VE                 PIC X(011).
          07 01END048VE                 PIC X(011).
          07 01END049VE                 PIC X(011).
          07 01END050VE                 PIC X(011).
          07 01END051VE                 PIC X(011).
          07 01END052VE                 PIC X(011).
          07 01END053VE                 PIC X(011).
          07 01END054VE                 PIC X(011).
          07 01END055VE                 PIC X(011).
          07 01END056VE                 PIC X(011).
          07 01END057VE                 PIC X(011).
          07 01END058VE                 PIC X(011).
          07 01END059VE                 PIC X(011).
          07 01END060VE                 PIC X(011).
          07 01END061VE                 PIC X(011).
          07 01END062VE                 PIC X(011).
          07 01END063VE                 PIC X(011).
          07 01END064VE                 PIC X(011).
          07 01END065VE                 PIC X(011).
          07 01END066VE                 PIC X(011).
          07 01END067VE                 PIC X(011).
          07 01END068VE                 PIC X(011).
          07 01END069VE                 PIC X(011).
          07 01END070VE                 PIC X(011).
          07 01END071VE                 PIC X(011).
          07 01END072VE                 PIC X(011).
          07 01END073VE                 PIC X(011).
          07 01END074VE                 PIC X(011).
          07 01END075VE                 PIC X(011).
          07 01END076VE                 PIC X(011).
          07 01END077VE                 PIC X(011).
          07 01END078VE                 PIC X(011).
          07 01END079VE                 PIC X(011).
          07 01END080VE                 PIC X(011).
          07 01END081VE                 PIC X(011).
          07 01END082VE                 PIC X(011).
          07 01END083VE                 PIC X(011).
          07 01END084VE                 PIC X(011).
          07 01END085VE                 PIC X(011).
          07 01END086VE                 PIC X(011).
          07 01END087VE                 PIC X(011).
          07 01END088VE                 PIC X(011).
          07 01END089VE                 PIC X(011).
ACIEAA    07 01END090VE                 PIC X(011).                     RMB0006
ACIEAA    07 01END091VE                 PIC X(011).                     RMB0006
ACIEAA    07 01END092VE                 PIC X(011).                     RMB0006
ACIEAA    07 01END093VE                 PIC X(011).                     RMB0006
         05 01ENDVE-VALORES.
ACIEAA*   07 FILLER-01-ENDVE            PIC X(1425).                    RMB0006
ACIEAA    07 FILLER-01-ENDVE            PIC X(1485).                    RMB0006

         05 01ENDHP-EDITADOS.
          07 01ENDPTFHP                 PIC X(002).
          07 01END001HP                 PIC X(011).
          07 01END002HP                 PIC X(011).
          07 01END003HP                 PIC X(011).
          07 01END004HP                 PIC X(011).
          07 01END005HP                 PIC X(011).
          07 01END006HP                 PIC X(011).
          07 01END007HP                 PIC X(011).
          07 01END008HP                 PIC X(011).
          07 01END009HP                 PIC X(011).
          07 01END010HP                 PIC X(011).
          07 01END011HP                 PIC X(011).
          07 01END012HP                 PIC X(011).
          07 01END013HP                 PIC X(011).
          07 01END014HP                 PIC X(011).
          07 01END015HP                 PIC X(011).
          07 01END016HP                 PIC X(011).
          07 01END017HP                 PIC X(011).
          07 01END018HP                 PIC X(011).
          07 01END019HP                 PIC X(011).
          07 01END020HP                 PIC X(011).
          07 01END021HP                 PIC X(011).
          07 01END022HP                 PIC X(011).
          07 01END023HP                 PIC X(011).
          07 01END024HP                 PIC X(011).
          07 01END025HP                 PIC X(011).
          07 01END026HP                 PIC X(011).
          07 01END027HP                 PIC X(011).
          07 01END028HP                 PIC X(011).
          07 01END029HP                 PIC X(011).
          07 01END030HP                 PIC X(011).
          07 01END031HP                 PIC X(011).
          07 01END032HP                 PIC X(011).
          07 01END033HP                 PIC X(011).
          07 01END034HP                 PIC X(011).
          07 01END035HP                 PIC X(011).
          07 01END036HP                 PIC X(011).
          07 01END037HP                 PIC X(011).
          07 01END038HP                 PIC X(011).
          07 01END039HP                 PIC X(011).
          07 01END040HP                 PIC X(011).
          07 01END041HP                 PIC X(011).
          07 01END042HP                 PIC X(011).
          07 01END043HP                 PIC X(011).
          07 01END044HP                 PIC X(011).
          07 01END045HP                 PIC X(011).
          07 01END046HP                 PIC X(011).
          07 01END047HP                 PIC X(011).
          07 01END048HP                 PIC X(011).
          07 01END049HP                 PIC X(011).
          07 01END050HP                 PIC X(011).
          07 01END051HP                 PIC X(011).
          07 01END052HP                 PIC X(011).
          07 01END053HP                 PIC X(011).
          07 01END054HP                 PIC X(011).
          07 01END055HP                 PIC X(011).
          07 01END056HP                 PIC X(011).
          07 01END057HP                 PIC X(011).
          07 01END058HP                 PIC X(011).
          07 01END059HP                 PIC X(011).
          07 01END060HP                 PIC X(011).
          07 01END061HP                 PIC X(011).
          07 01END062HP                 PIC X(011).
          07 01END063HP                 PIC X(011).
          07 01END064HP                 PIC X(011).
          07 01END065HP                 PIC X(011).
          07 01END066HP                 PIC X(011).
          07 01END067HP                 PIC X(011).
          07 01END068HP                 PIC X(011).
          07 01END069HP                 PIC X(011).
          07 01END070HP                 PIC X(011).
          07 01END071HP                 PIC X(011).
          07 01END072HP                 PIC X(011).
          07 01END073HP                 PIC X(011).
          07 01END074HP                 PIC X(011).
          07 01END075HP                 PIC X(011).
          07 01END076HP                 PIC X(011).
          07 01END077HP                 PIC X(011).
          07 01END078HP                 PIC X(011).
          07 01END079HP                 PIC X(011).
          07 01END080HP                 PIC X(011).
          07 01END081HP                 PIC X(011).
          07 01END082HP                 PIC X(011).
          07 01END083HP                 PIC X(011).
          07 01END084HP                 PIC X(011).
          07 01END085HP                 PIC X(011).
          07 01END086HP                 PIC X(011).
          07 01END087HP                 PIC X(011).
          07 01END088HP                 PIC X(011).
          07 01END089HP                 PIC X(011).
ACIEAA    07 01END090HP                 PIC X(011).                     RMB0006
ACIEAA    07 01END091HP                 PIC X(011).                     RMB0006
ACIEAA    07 01END092HP                 PIC X(011).                     RMB0006
ACIEAA    07 01END093HP                 PIC X(011).                     RMB0006
         05 01ENDHP-VALORES.
ACIEAA*   07 FILLER-01-ENDHP            PIC X(1425).                    RMB0006
ACIEAA    07 FILLER-01-ENDHP            PIC X(1485).                    RMB0006

         05 01ENDOT-EDITADOS.
          07 01ENDPTFOT                 PIC X(002).
          07 01END001OT                 PIC X(011).
          07 01END002OT                 PIC X(011).
          07 01END003OT                 PIC X(011).
          07 01END004OT                 PIC X(011).
          07 01END005OT                 PIC X(011).
          07 01END006OT                 PIC X(011).
          07 01END007OT                 PIC X(011).
          07 01END008OT                 PIC X(011).
          07 01END009OT                 PIC X(011).
          07 01END010OT                 PIC X(011).
          07 01END011OT                 PIC X(011).
          07 01END012OT                 PIC X(011).
          07 01END013OT                 PIC X(011).
          07 01END014OT                 PIC X(011).
          07 01END015OT                 PIC X(011).
          07 01END016OT                 PIC X(011).
          07 01END017OT                 PIC X(011).
          07 01END018OT                 PIC X(011).
          07 01END019OT                 PIC X(011).
          07 01END020OT                 PIC X(011).
          07 01END021OT                 PIC X(011).
          07 01END022OT                 PIC X(011).
          07 01END023OT                 PIC X(011).
          07 01END024OT                 PIC X(011).
          07 01END025OT                 PIC X(011).
          07 01END026OT                 PIC X(011).
          07 01END027OT                 PIC X(011).
          07 01END028OT                 PIC X(011).
          07 01END029OT                 PIC X(011).
          07 01END030OT                 PIC X(011).
          07 01END031OT                 PIC X(011).
          07 01END032OT                 PIC X(011).
          07 01END033OT                 PIC X(011).
          07 01END034OT                 PIC X(011).
          07 01END035OT                 PIC X(011).
          07 01END036OT                 PIC X(011).
          07 01END037OT                 PIC X(011).
          07 01END038OT                 PIC X(011).
          07 01END039OT                 PIC X(011).
          07 01END040OT                 PIC X(011).
          07 01END041OT                 PIC X(011).
          07 01END042OT                 PIC X(011).
          07 01END043OT                 PIC X(011).
          07 01END044OT                 PIC X(011).
          07 01END045OT                 PIC X(011).
          07 01END046OT                 PIC X(011).
          07 01END047OT                 PIC X(011).
          07 01END048OT                 PIC X(011).
          07 01END049OT                 PIC X(011).
          07 01END050OT                 PIC X(011).
          07 01END051OT                 PIC X(011).
          07 01END052OT                 PIC X(011).
          07 01END053OT                 PIC X(011).
          07 01END054OT                 PIC X(011).
          07 01END055OT                 PIC X(011).
          07 01END056OT                 PIC X(011).
          07 01END057OT                 PIC X(011).
          07 01END058OT                 PIC X(011).
          07 01END059OT                 PIC X(011).
          07 01END060OT                 PIC X(011).
          07 01END061OT                 PIC X(011).
          07 01END062OT                 PIC X(011).
          07 01END063OT                 PIC X(011).
          07 01END064OT                 PIC X(011).
          07 01END065OT                 PIC X(011).
          07 01END066OT                 PIC X(011).
          07 01END067OT                 PIC X(011).
          07 01END068OT                 PIC X(011).
          07 01END069OT                 PIC X(011).
          07 01END070OT                 PIC X(011).
          07 01END071OT                 PIC X(011).
          07 01END072OT                 PIC X(011).
          07 01END073OT                 PIC X(011).
          07 01END074OT                 PIC X(011).
          07 01END075OT                 PIC X(011).
          07 01END076OT                 PIC X(011).
          07 01END077OT                 PIC X(011).
          07 01END078OT                 PIC X(011).
          07 01END079OT                 PIC X(011).
          07 01END080OT                 PIC X(011).
          07 01END081OT                 PIC X(011).
          07 01END082OT                 PIC X(011).
          07 01END083OT                 PIC X(011).
          07 01END084OT                 PIC X(011).
          07 01END085OT                 PIC X(011).
          07 01END086OT                 PIC X(011).
          07 01END087OT                 PIC X(011).
          07 01END088OT                 PIC X(011).
          07 01END089OT                 PIC X(011).
ACIEAA    07 01END090OT                 PIC X(011).                     RMB0006
ACIEAA    07 01END091OT                 PIC X(011).                     RMB0006
ACIEAA    07 01END092OT                 PIC X(011).                     RMB0006
ACIEAA    07 01END093OT                 PIC X(011).                     RMB0006
         05 01ENDOT-VALORES.
ACIEAA*   07 FILLER-01-ENDOT            PIC X(1425).                    RMB0006
ACIEAA    07 FILLER-01-ENDOT            PIC X(1485).                    RMB0006

         05 01ENDCC-EDITADOS.
          07 01ENDPTFCC                 PIC X(002).
          07 01END001CC                 PIC X(011).
          07 01END002CC                 PIC X(011).
          07 01END003CC                 PIC X(011).
          07 01END004CC                 PIC X(011).
          07 01END005CC                 PIC X(011).
          07 01END006CC                 PIC X(011).
          07 01END007CC                 PIC X(011).
          07 01END008CC                 PIC X(011).
          07 01END009CC                 PIC X(011).
          07 01END010CC                 PIC X(011).
          07 01END011CC                 PIC X(011).
          07 01END012CC                 PIC X(011).
          07 01END013CC                 PIC X(011).
          07 01END014CC                 PIC X(011).
          07 01END015CC                 PIC X(011).
          07 01END016CC                 PIC X(011).
          07 01END017CC                 PIC X(011).
          07 01END018CC                 PIC X(011).
          07 01END019CC                 PIC X(011).
          07 01END020CC                 PIC X(011).
          07 01END021CC                 PIC X(011).
          07 01END022CC                 PIC X(011).
          07 01END023CC                 PIC X(011).
          07 01END024CC                 PIC X(011).
          07 01END025CC                 PIC X(011).
          07 01END026CC                 PIC X(011).
          07 01END027CC                 PIC X(011).
          07 01END028CC                 PIC X(011).
          07 01END029CC                 PIC X(011).
          07 01END030CC                 PIC X(011).
          07 01END031CC                 PIC X(011).
          07 01END032CC                 PIC X(011).
          07 01END033CC                 PIC X(011).
          07 01END034CC                 PIC X(011).
          07 01END035CC                 PIC X(011).
          07 01END036CC                 PIC X(011).
          07 01END037CC                 PIC X(011).
          07 01END038CC                 PIC X(011).
          07 01END039CC                 PIC X(011).
          07 01END040CC                 PIC X(011).
          07 01END041CC                 PIC X(011).
          07 01END042CC                 PIC X(011).
          07 01END043CC                 PIC X(011).
          07 01END044CC                 PIC X(011).
          07 01END045CC                 PIC X(011).
          07 01END046CC                 PIC X(011).
          07 01END047CC                 PIC X(011).
          07 01END048CC                 PIC X(011).
          07 01END049CC                 PIC X(011).
          07 01END050CC                 PIC X(011).
          07 01END051CC                 PIC X(011).
          07 01END052CC                 PIC X(011).
          07 01END053CC                 PIC X(011).
          07 01END054CC                 PIC X(011).
          07 01END055CC                 PIC X(011).
          07 01END056CC                 PIC X(011).
          07 01END057CC                 PIC X(011).
          07 01END058CC                 PIC X(011).
          07 01END059CC                 PIC X(011).
          07 01END060CC                 PIC X(011).
          07 01END061CC                 PIC X(011).
          07 01END062CC                 PIC X(011).
          07 01END063CC                 PIC X(011).
          07 01END064CC                 PIC X(011).
          07 01END065CC                 PIC X(011).
          07 01END066CC                 PIC X(011).
          07 01END067CC                 PIC X(011).
          07 01END068CC                 PIC X(011).
          07 01END069CC                 PIC X(011).
          07 01END070CC                 PIC X(011).
          07 01END071CC                 PIC X(011).
          07 01END072CC                 PIC X(011).
          07 01END073CC                 PIC X(011).
          07 01END074CC                 PIC X(011).
          07 01END075CC                 PIC X(011).
          07 01END076CC                 PIC X(011).
          07 01END077CC                 PIC X(011).
          07 01END078CC                 PIC X(011).
          07 01END079CC                 PIC X(011).
          07 01END080CC                 PIC X(011).
          07 01END081CC                 PIC X(011).
          07 01END082CC                 PIC X(011).
          07 01END083CC                 PIC X(011).
          07 01END084CC                 PIC X(011).
          07 01END085CC                 PIC X(011).
          07 01END086CC                 PIC X(011).
          07 01END087CC                 PIC X(011).
          07 01END088CC                 PIC X(011).
          07 01END089CC                 PIC X(011).
ACIEAA    07 01END090CC                 PIC X(011).                     RMB0006
ACIEAA    07 01END091CC                 PIC X(011).                     RMB0006
ACIEAA    07 01END092CC                 PIC X(011).                     RMB0006
ACIEAA    07 01END093CC                 PIC X(011).                     RMB0006
         05 01ENDCC-VALORES.
ACIEAA*   07 FILLER-01-ENDCC            PIC X(1425).                    RMB0006
ACIEAA    07 FILLER-01-ENDCC            PIC X(1485).                    RMB0006

         05 01ENDCO-EDITADOS.
          07 01ENDPTFCO                 PIC X(002).
          07 01END001CO                 PIC X(011).
          07 01END002CO                 PIC X(011).
          07 01END003CO                 PIC X(011).
          07 01END004CO                 PIC X(011).
          07 01END005CO                 PIC X(011).
          07 01END006CO                 PIC X(011).
          07 01END007CO                 PIC X(011).
          07 01END008CO                 PIC X(011).
          07 01END009CO                 PIC X(011).
          07 01END010CO                 PIC X(011).
          07 01END011CO                 PIC X(011).
          07 01END012CO                 PIC X(011).
          07 01END013CO                 PIC X(011).
          07 01END014CO                 PIC X(011).
          07 01END015CO                 PIC X(011).
          07 01END016CO                 PIC X(011).
          07 01END017CO                 PIC X(011).
          07 01END018CO                 PIC X(011).
          07 01END019CO                 PIC X(011).
          07 01END020CO                 PIC X(011).
          07 01END021CO                 PIC X(011).
          07 01END022CO                 PIC X(011).
          07 01END023CO                 PIC X(011).
          07 01END024CO                 PIC X(011).
          07 01END025CO                 PIC X(011).
          07 01END026CO                 PIC X(011).
          07 01END027CO                 PIC X(011).
          07 01END028CO                 PIC X(011).
          07 01END029CO                 PIC X(011).
          07 01END030CO                 PIC X(011).
          07 01END031CO                 PIC X(011).
          07 01END032CO                 PIC X(011).
          07 01END033CO                 PIC X(011).
          07 01END034CO                 PIC X(011).
          07 01END035CO                 PIC X(011).
          07 01END036CO                 PIC X(011).
          07 01END037CO                 PIC X(011).
          07 01END038CO                 PIC X(011).
          07 01END039CO                 PIC X(011).
          07 01END040CO                 PIC X(011).
          07 01END041CO                 PIC X(011).
          07 01END042CO                 PIC X(011).
          07 01END043CO                 PIC X(011).
          07 01END044CO                 PIC X(011).
          07 01END045CO                 PIC X(011).
          07 01END046CO                 PIC X(011).
          07 01END047CO                 PIC X(011).
          07 01END048CO                 PIC X(011).
          07 01END049CO                 PIC X(011).
          07 01END050CO                 PIC X(011).
          07 01END051CO                 PIC X(011).
          07 01END052CO                 PIC X(011).
          07 01END053CO                 PIC X(011).
          07 01END054CO                 PIC X(011).
          07 01END055CO                 PIC X(011).
          07 01END056CO                 PIC X(011).
          07 01END057CO                 PIC X(011).
          07 01END058CO                 PIC X(011).
          07 01END059CO                 PIC X(011).
          07 01END060CO                 PIC X(011).
          07 01END061CO                 PIC X(011).
          07 01END062CO                 PIC X(011).
          07 01END063CO                 PIC X(011).
          07 01END064CO                 PIC X(011).
          07 01END065CO                 PIC X(011).
          07 01END066CO                 PIC X(011).
          07 01END067CO                 PIC X(011).
          07 01END068CO                 PIC X(011).
          07 01END069CO                 PIC X(011).
          07 01END070CO                 PIC X(011).
          07 01END071CO                 PIC X(011).
          07 01END072CO                 PIC X(011).
          07 01END073CO                 PIC X(011).
          07 01END074CO                 PIC X(011).
          07 01END075CO                 PIC X(011).
          07 01END076CO                 PIC X(011).
          07 01END077CO                 PIC X(011).
          07 01END078CO                 PIC X(011).
          07 01END079CO                 PIC X(011).
          07 01END080CO                 PIC X(011).
          07 01END081CO                 PIC X(011).
          07 01END082CO                 PIC X(011).
          07 01END083CO                 PIC X(011).
          07 01END084CO                 PIC X(011).
          07 01END085CO                 PIC X(011).
          07 01END086CO                 PIC X(011).
          07 01END087CO                 PIC X(011).
          07 01END088CO                 PIC X(011).
          07 01END089CO                 PIC X(011).
ACIEAA    07 01END090CO                 PIC X(011).                     RMB0006
ACIEAA    07 01END091CO                 PIC X(011).                     RMB0006
ACIEAA    07 01END092CO                 PIC X(011).                     RMB0006
ACIEAA    07 01END093CO                 PIC X(011).                     RMB0006
         05 01ENDCO-VALORES.
ACIEAA*   SE AUMENTAN 4 CARACTERISTICAS , SE RECALCULAN LOS FILLERS     RMB0006
ACIEAA*   07 FILLER-01-ENDCO            PIC X(1425). *> = 98*10+89*5    RMB0006
ACIEAA    07 FILLER-01-ENDCO            PIC X(1485). *> = 1425+4(10+5)  RMB0006
     
ACIEAA*   07 FILLER-01-ENDAH            PIC X(2406). *> 1425+2+89*11    RMB0006
ACIEAA    07 FILLER-01-ENDAH            PIC X(2510). *> 2406+4*(11+10+5)RMB0006

ACIEAA*   07 FILLER-01-ENDCT            PIC X(2406).                    RMB0006
ACIEAA    07 FILLER-01-ENDCT            PIC X(2510).                    RMB0006


       01 VAR-02MUN.
         02 VAR-02MUN-SALIDA.
          03 VAR-02NUM-TO.
             05 02NUM001TO                 PIC 9(004).
             05 02NUM002TO                 PIC 9(004).
             05 02NUM003TO                 PIC 9(004).
             05 02NUM004TO                 PIC 9(004).
             05 02NUM005TO                 PIC 9(004).
             05 02NUM007TO                 PIC 9(004).
             05 02NUM018TO                 PIC 9(004).
             05 02NUM019TO                 PIC 9(004).
             05 02NUM022TO                 PIC 9(004).
             05 02NUM028TO                 PIC 9(004).
             05 02NUM029TO                 PIC 9(004).
             05 02NUM030TO                 PIC 9(004).
             05 02NUM031TO                 PIC 9(004).
             05 02NUM032TO                 PIC 9(004).
             05 02NUM033TO                 PIC 9(004).
             05 02NUM034TO                 PIC 9(004).
             05 02NUM035TO                 PIC 9(004).
             05 02NUM036TO                 PIC 9(004).
             05 02NUM037TO                 PIC 9(004).
             05 02NUM038TO                 PIC 9(004).
             05 02NUM039TO                 PIC 9(004).
             05 02NUM040TO                 PIC 9(004).
             05 02NUM041TO                 PIC 9(004).
          03 TABLA-02NUM OCCURS 9 TIMES.
             05 02NUM042                   PIC 999.99.
             05 02NUM043                   PIC 999.99.
             05 02NUM045                   PIC 999.99.
             05 02NUM056                   PIC 999.99.
             05 02NUM057                   PIC 999.99.
             05 02NUM060                   PIC 999.99.
             05 02NUM066                   PIC 999.99.
             05 02NUM067                   PIC 999.99.
             05 02NUM068                   PIC 999.99.
             05 02NUM069                   PIC 999.99.
             05 02NUM070                   PIC 999.99.
             05 02NUM071                   PIC 999.99.
             05 02NUM072                   PIC 999.99.
             05 02NUM073                   PIC 999.99.
             05 02NUM074                   PIC 999.99.
             05 02NUM075                   PIC 999.99.
             05 02NUM076                   PIC 999.99.
             05 02NUM077                   PIC 999.99.
          03 TABLA-02NUM-R REDEFINES TABLA-02NUM.
             05 02NUM042RO                 PIC X(006).
             05 02NUM043RO                 PIC X(006).
             05 02NUM045RO                 PIC X(006).
             05 02NUM056RO                 PIC X(006).
             05 02NUM057RO                 PIC X(006).
             05 02NUM060RO                 PIC X(006).
             05 02NUM066RO                 PIC X(006).
             05 02NUM067RO                 PIC X(006).
             05 02NUM068RO                 PIC X(006).
             05 02NUM069RO                 PIC X(006).
             05 02NUM070RO                 PIC X(006).
             05 02NUM071RO                 PIC X(006).
             05 02NUM072RO                 PIC X(006).
             05 02NUM073RO                 PIC X(006).
             05 02NUM074RO                 PIC X(006).
             05 02NUM075RO                 PIC X(006).
             05 02NUM076RO                 PIC X(006).
             05 02NUM077RO                 PIC X(006). 

             05 02NUM042IN                 PIC X(006).
             05 02NUM043IN                 PIC X(006).
             05 02NUM045IN                 PIC X(006).
             05 02NUM056IN                 PIC X(006).
             05 02NUM057IN                 PIC X(006).
             05 02NUM060IN                 PIC X(006).
             05 02NUM066IN                 PIC X(006).
             05 02NUM067IN                 PIC X(006).
             05 02NUM068IN                 PIC X(006).
             05 02NUM069IN                 PIC X(006).
             05 02NUM070IN                 PIC X(006).
             05 02NUM071IN                 PIC X(006).
             05 02NUM072IN                 PIC X(006).
             05 02NUM073IN                 PIC X(006).
             05 02NUM074IN                 PIC X(006).
             05 02NUM075IN                 PIC X(006).
             05 02NUM076IN                 PIC X(006).
             05 02NUM077IN                 PIC X(006).

             05 02NUM042VE                 PIC X(006).
             05 02NUM043VE                 PIC X(006).
             05 02NUM045VE                 PIC X(006).
             05 02NUM056VE                 PIC X(006).
             05 02NUM057VE                 PIC X(006).
             05 02NUM060VE                 PIC X(006).
             05 02NUM066VE                 PIC X(006).
             05 02NUM067VE                 PIC X(006).
             05 02NUM068VE                 PIC X(006).
             05 02NUM069VE                 PIC X(006).
             05 02NUM070VE                 PIC X(006).
             05 02NUM071VE                 PIC X(006).
             05 02NUM072VE                 PIC X(006).
             05 02NUM073VE                 PIC X(006).
             05 02NUM074VE                 PIC X(006).
             05 02NUM075VE                 PIC X(006).
             05 02NUM076VE                 PIC X(006).
             05 02NUM077VE                 PIC X(006).

             05 02NUM042HP                 PIC X(006).
             05 02NUM043HP                 PIC X(006).
             05 02NUM045HP                 PIC X(006).
             05 02NUM056HP                 PIC X(006).
             05 02NUM057HP                 PIC X(006).
             05 02NUM060HP                 PIC X(006).
             05 02NUM066HP                 PIC X(006).
             05 02NUM067HP                 PIC X(006).
             05 02NUM068HP                 PIC X(006).
             05 02NUM069HP                 PIC X(006).
             05 02NUM070HP                 PIC X(006).
             05 02NUM071HP                 PIC X(006).
             05 02NUM072HP                 PIC X(006).
             05 02NUM073HP                 PIC X(006).
             05 02NUM074HP                 PIC X(006).
             05 02NUM075HP                 PIC X(006).
             05 02NUM076HP                 PIC X(006).
             05 02NUM077HP                 PIC X(006).

             05 02NUM042OT                 PIC X(006).
             05 02NUM043OT                 PIC X(006).
             05 02NUM045OT                 PIC X(006).
             05 02NUM056OT                 PIC X(006).
             05 02NUM057OT                 PIC X(006).
             05 02NUM060OT                 PIC X(006).
             05 02NUM066OT                 PIC X(006).
             05 02NUM067OT                 PIC X(006).
             05 02NUM068OT                 PIC X(006).
             05 02NUM069OT                 PIC X(006).
             05 02NUM070OT                 PIC X(006).
             05 02NUM071OT                 PIC X(006).
             05 02NUM072OT                 PIC X(006).
             05 02NUM073OT                 PIC X(006).
             05 02NUM074OT                 PIC X(006).
             05 02NUM075OT                 PIC X(006).
             05 02NUM076OT                 PIC X(006).
             05 02NUM077OT                 PIC X(006).

             05 02NUM042CC                 PIC X(006).
             05 02NUM043CC                 PIC X(006).
             05 02NUM045CC                 PIC X(006).
             05 02NUM056CC                 PIC X(006).
             05 02NUM057CC                 PIC X(006).
             05 02NUM060CC                 PIC X(006).
             05 02NUM066CC                 PIC X(006).
             05 02NUM067CC                 PIC X(006).
             05 02NUM068CC                 PIC X(006).
             05 02NUM069CC                 PIC X(006).
             05 02NUM070CC                 PIC X(006).
             05 02NUM071CC                 PIC X(006).
             05 02NUM072CC                 PIC X(006).
             05 02NUM073CC                 PIC X(006).
             05 02NUM074CC                 PIC X(006).
             05 02NUM075CC                 PIC X(006).
             05 02NUM076CC                 PIC X(006).
             05 02NUM077CC                 PIC X(006).

             05 02NUM042CO                 PIC X(006).
             05 02NUM043CO                 PIC X(006).
             05 02NUM045CO                 PIC X(006).
             05 02NUM056CO                 PIC X(006).
             05 02NUM057CO                 PIC X(006).
             05 02NUM060CO                 PIC X(006).
             05 02NUM066CO                 PIC X(006).
             05 02NUM067CO                 PIC X(006).
             05 02NUM068CO                 PIC X(006).
             05 02NUM069CO                 PIC X(006).
             05 02NUM070CO                 PIC X(006).
             05 02NUM071CO                 PIC X(006).
             05 02NUM072CO                 PIC X(006).
             05 02NUM073CO                 PIC X(006).
             05 02NUM074CO                 PIC X(006).
             05 02NUM075CO                 PIC X(006).
             05 02NUM076CO                 PIC X(006).
             05 02NUM077CO                 PIC X(006).

             05 02NUM042AH                 PIC X(006).
             05 02NUM043AH                 PIC X(006).
             05 02NUM045AH                 PIC X(006).
             05 02NUM056AH                 PIC X(006).
             05 02NUM057AH                 PIC X(006).
             05 02NUM060AH                 PIC X(006).
             05 02NUM066AH                 PIC X(006).
             05 02NUM067AH                 PIC X(006).
             05 02NUM068AH                 PIC X(006).
             05 02NUM069AH                 PIC X(006).
             05 02NUM070AH                 PIC X(006).
             05 02NUM071AH                 PIC X(006).
             05 02NUM072AH                 PIC X(006).
             05 02NUM073AH                 PIC X(006).
             05 02NUM074AH                 PIC X(006).
             05 02NUM075AH                 PIC X(006).
             05 02NUM076AH                 PIC X(006).
             05 02NUM077AH                 PIC X(006).

             05 02NUM042CT                 PIC X(006).
             05 02NUM043CT                 PIC X(006).
             05 02NUM045CT                 PIC X(006).
             05 02NUM056CT                 PIC X(006).
             05 02NUM057CT                 PIC X(006).
             05 02NUM060CT                 PIC X(006).
             05 02NUM066CT                 PIC X(006).
             05 02NUM067CT                 PIC X(006).
             05 02NUM068CT                 PIC X(006).
             05 02NUM069CT                 PIC X(006).
             05 02NUM070CT                 PIC X(006).
             05 02NUM071CT                 PIC X(006).
             05 02NUM072CT                 PIC X(006).
             05 02NUM073CT                 PIC X(006).
             05 02NUM074CT                 PIC X(006).
             05 02NUM075CT                 PIC X(006).
             05 02NUM076CT                 PIC X(006).
             05 02NUM077CT                 PIC X(006).

          03 VAR-02NUM-CB.
             05 02NUM042CB                 PIC 999.99.
             05 02NUM043CB                 PIC 999.99.
             05 02NUM045CB                 PIC 999.99.
             05 02NUM056CB                 PIC 999.99.
             05 02NUM057CB                 PIC 999.99.
             05 02NUM060CB                 PIC 999.99.
             05 02NUM066CB                 PIC 999.99.
             05 02NUM067CB                 PIC 999.99.
             05 02NUM068CB                 PIC 999.99.
             05 02NUM069CB                 PIC 999.99.
             05 02NUM070CB                 PIC 999.99.
             05 02NUM071CB                 PIC 999.99.
             05 02NUM072CB                 PIC 999.99.
             05 02NUM073CB                 PIC 999.99.
             05 02NUM074CB                 PIC 999.99.
             05 02NUM075CB                 PIC 999.99.
             05 02NUM076CB                 PIC 999.99.
             05 02NUM077CB                 PIC 999.99.
          03 VAR-02NUM-MX.
             05 02NUM079MX                 PIC 999.99.
             05 02NUM080MX                 PIC 999.99.
             05 02NUM081MX                 PIC 999.99.
             05 02NUM082MX                 PIC 999.99.
             05 02NUM083MX                 PIC 999.99.
             05 02NUM084MX                 PIC 999.99.
             05 02NUM085MX                 PIC 999.99.
             05 02NUM086MX                 PIC 999.99.
             05 02NUM087MX                 PIC 999.99.
         02 VAR-02MUN-VALORES.
          03 02NUM042V                  PIC 9(004).
          03 02NUM042T                  PIC 9(004).
          03 02NUM043V                  PIC 9(004).
          03 02NUM043T                  PIC 9(004).
          03 02NUM045V                  PIC 9(004).
          03 02NUM045T                  PIC 9(004).
          03 02NUM056V                  PIC 9(004).
          03 02NUM056T                  PIC 9(004).
          03 02NUM057V                  PIC 9(004).
          03 02NUM057T                  PIC 9(004).
          03 02NUM060V                  PIC 9(004).
          03 02NUM060T                  PIC 9(004).
          03 02NUM066V                  PIC 9(004).
          03 02NUM066T                  PIC 9(004).
          03 02NUM067V                  PIC 9(004).
          03 02NUM067T                  PIC 9(004).
          03 02NUM068V                  PIC 9(004).
          03 02NUM068T                  PIC 9(004).
          03 02NUM069V                  PIC 9(004).
          03 02NUM069T                  PIC 9(004).
          03 02NUM070V                  PIC 9(004).
          03 02NUM070T                  PIC 9(004).
          03 02NUM071V                  PIC 9(004).
          03 02NUM071T                  PIC 9(004).
          03 02NUM072V                  PIC 9(004).
          03 02NUM072T                  PIC 9(004).
          03 02NUM073V                  PIC 9(004).
          03 02NUM073T                  PIC 9(004).
          03 02NUM074V                  PIC 9(004).
          03 02NUM074T                  PIC 9(004).
          03 02NUM075V                  PIC 9(004).
          03 02NUM075T                  PIC 9(004).
          03 02NUM076V                  PIC 9(004).
          03 02NUM076T                  PIC 9(004).
          03 02NUM077V                  PIC 9(004).
          03 02NUM077T                  PIC 9(004).
          03 02NUM079V                  PIC 9(004).
          03 02NUM7987                  PIC 9(004).
          03 02NUM080V                  PIC 9(004).
          03 02NUM081V                  PIC 9(004).
          03 02NUM082V                  PIC 9(004).
          03 02NUM083V                  PIC 9(004).
          03 02NUM084V                  PIC 9(004).
          03 02NUM085V                  PIC 9(004).
          03 02NUM086V                  PIC 9(004).
          03 02NUM087V                  PIC 9(004).
          03 02NUMF01TO                 PIC 9(001).
          03 02NUMF02TO                 PIC 9(001).
          03 02NUMF03TO                 PIC 9(001).
          03 02NUMF04TO                 PIC 9(001).
          03 02NUMF05TO                 PIC 9(001).
          03 02NUMF07TO                 PIC 9(001).
          03 02NUMF18TO                 PIC 9(001).
          03 02NUMF19TO                 PIC 9(001).
          03 02NUMF22TO                 PIC 9(001).
          03 02NUMF28TO                 PIC 9(001).
          03 02NUMF29TO                 PIC 9(001).
          03 02NUMF30TO                 PIC 9(001).
          03 02NUMF31TO                 PIC 9(001).
          03 02NUMF32TO                 PIC 9(001).
          03 02NUMF33TO                 PIC 9(001).
          03 02NUMF34TO                 PIC 9(001).
          03 02NUMF35TO                 PIC 9(001).
          03 02NUMF36TO                 PIC 9(001).
          03 02NUMF37TO                 PIC 9(001).
          03 02NUMF38TO                 PIC 9(001).
          03 02NUMF39TO                 PIC 9(001).
          03 02NUMF40TO                 PIC 9(001).
          03 02NUMF41TO                 PIC 9(001).

       01 VAR-02ACP.
          02 VAR-02ACP-SALIDA.
             03 02ACPPTF                PIC X(002).
             03 02ACP001                PIC 9(004).
             03 02ACP002                PIC 9(004).

       01 VAR-02INQ.
          02 VAR-02INQ-SALIDA.
             03 02INQPTF                PIC X(002).
             03 02INQ001                PIC 999.99.

       01 VAR-02EXP.
        02 VAR-02EXP-TOTAL.
             03 02EXPPTFTO              PIC X(002).
             03 02EXP001TO              PIC 9(003).
             03 02EXP002TO              PIC 9(003).
             03 02EXP003TO              PIC 9(003).
             03 02EXP004TO              PIC 9(003).
             03 02EXP005TO              PIC 9(003).
             03 02EXP006TO              PIC 9(003).
             03 02EXP007TO              PIC 99999.99.
             03 02EXP008TO              PIC 99999.99.
             03 02EXP009TO              PIC 99999.99.
             03 02EXP010TO              PIC 99999.99.
             03 02EXP011TO              PIC 99999.99.
             03 02EXP012TO              PIC 99999.99.
             03 02EXP013TO              PIC 99999.99.
        02 TABLA-PORTAFOLIOS-02EXP OCCURS 9 TIMES. 
          03 VAR-02EXP-SALIDA.
             05 02EXPPTF                PIC X(002).
             05 02EXP001                PIC 9(003).
             05 02EXP002                PIC 9(003).
             05 02EXP003                PIC 9(003).
             05 02EXP004                PIC 9(003).
             05 02EXP005                PIC 9(003).
             05 02EXP006                PIC 9(003).
             05 02EXP007                PIC 99999.99.
             05 02EXP008                PIC 99999.99.
             05 02EXP009                PIC 99999.99.
             05 02EXP010                PIC 99999.99.
             05 02EXP011                PIC 99999.99.
             05 02EXP012                PIC 99999.99.
             05 02EXP013                PIC 99999.99.
          03 VAR-02EXP-VALORES.
             05 02EXP007N               PIC 9(004).
             05 02EXP007D               PIC 9(004).
             05 02EXP008N               PIC 9(004).
             05 02EXP008D               PIC 9(004).
             05 02EXP009N               PIC 9(004).
             05 02EXP009D               PIC 9(004).
             05 02EXP010N               PIC 9(004).
             05 02EXP010D               PIC 9(004).
             05 02EXP011N               PIC 9(004).
             05 02EXP011D               PIC 9(004).
             05 02EXP012N               PIC 9(004).
             05 02EXP012D               PIC 9(004).
             05 02EXP013N               PIC 9(004).
             05 02EXP013D               PIC 9(004).
        02 TABLA-PORTAFOLIOS-02EXP-R REDEFINES
                                     TABLA-PORTAFOLIOS-02EXP.
             05 02EXPPTFRO              PIC X(002).
             05 02EXP001RO              PIC X(003).
             05 02EXP002RO              PIC X(003).
             05 02EXP003RO              PIC X(003).
             05 02EXP004RO              PIC X(003).
             05 02EXP005RO              PIC X(003).
             05 02EXP006RO              PIC X(003).
             05 02EXP007RO              PIC X(008).
             05 02EXP008RO              PIC X(008).
             05 02EXP009RO              PIC X(008).
             05 02EXP010RO              PIC X(008).
             05 02EXP011RO              PIC X(008).
             05 02EXP012RO              PIC X(008).
             05 02EXP013RO              PIC X(008).
             05 FILLER-02EXPRO          PIC X(056).

             05 02EXPPTFIN              PIC X(002).
             05 02EXP001IN              PIC X(003).
             05 02EXP002IN              PIC X(003).
             05 02EXP003IN              PIC X(003).
             05 02EXP004IN              PIC X(003).
             05 02EXP005IN              PIC X(003).
             05 02EXP006IN              PIC X(003).
             05 02EXP007IN              PIC X(008).
             05 02EXP008IN              PIC X(008).
             05 02EXP009IN              PIC X(008).
             05 02EXP010IN              PIC X(008).
             05 02EXP011IN              PIC X(008).
             05 02EXP012IN              PIC X(008).
             05 02EXP013IN              PIC X(008).
             05 FILLER-02EXPIN          PIC X(056).

             05 02EXPPTFVE              PIC X(002).
             05 02EXP001VE              PIC X(003).
             05 02EXP002VE              PIC X(003).
             05 02EXP003VE              PIC X(003).
             05 02EXP004VE              PIC X(003).
             05 02EXP005VE              PIC X(003).
             05 02EXP006VE              PIC X(003).
             05 02EXP007VE              PIC X(008).
             05 02EXP008VE              PIC X(008).
             05 02EXP009VE              PIC X(008).
             05 02EXP010VE              PIC X(008).
             05 02EXP011VE              PIC X(008).
             05 02EXP012VE              PIC X(008).
             05 02EXP013VE              PIC X(008).
             05 FILLER-02EXPVE          PIC X(056).

             05 02EXPPTFHP              PIC X(002).
             05 02EXP001HP              PIC X(003).
             05 02EXP002HP              PIC X(003).
             05 02EXP003HP              PIC X(003).
             05 02EXP004HP              PIC X(003).
             05 02EXP005HP              PIC X(003).
             05 02EXP006HP              PIC X(003).
             05 02EXP007HP              PIC X(008).
             05 02EXP008HP              PIC X(008).
             05 02EXP009HP              PIC X(008).
             05 02EXP010HP              PIC X(008).
             05 02EXP011HP              PIC X(008).
             05 02EXP012HP              PIC X(008).
             05 02EXP013HP              PIC X(008).
             05 FILLER-02EXPHP          PIC X(056).

             05 02EXPPTFOT              PIC X(002).
             05 02EXP001OT              PIC X(003).
             05 02EXP002OT              PIC X(003).
             05 02EXP003OT              PIC X(003).
             05 02EXP004OT              PIC X(003).
             05 02EXP005OT              PIC X(003).
             05 02EXP006OT              PIC X(003).
             05 02EXP007OT              PIC X(008).
             05 02EXP008OT              PIC X(008).
             05 02EXP009OT              PIC X(008).
             05 02EXP010OT              PIC X(008).
             05 02EXP011OT              PIC X(008).
             05 02EXP012OT              PIC X(008).
             05 02EXP013OT              PIC X(008).
             05 FILLER-02EXPOT          PIC X(056).

             05 02EXPPTFCC              PIC X(002).
             05 02EXP001CC              PIC X(003).
             05 02EXP002CC              PIC X(003).
             05 02EXP003CC              PIC X(003).
             05 02EXP004CC              PIC X(003).
             05 02EXP005CC              PIC X(003).
             05 02EXP006CC              PIC X(003).
             05 02EXP007CC              PIC X(008).
             05 02EXP008CC              PIC X(008).
             05 02EXP009CC              PIC X(008).
             05 02EXP010CC              PIC X(008).
             05 02EXP011CC              PIC X(008).
             05 02EXP012CC              PIC X(008).
             05 02EXP013CC              PIC X(008).
             05 FILLER-02EXPCC          PIC X(056).

             05 02EXPPTFCO              PIC X(002).
             05 02EXP001CO              PIC X(003).
             05 02EXP002CO              PIC X(003).
             05 02EXP003CO              PIC X(003).
             05 02EXP004CO              PIC X(003).
             05 02EXP005CO              PIC X(003).
             05 02EXP006CO              PIC X(003).
             05 02EXP007CO              PIC X(008).
             05 02EXP008CO              PIC X(008).
             05 02EXP009CO              PIC X(008).
             05 02EXP010CO              PIC X(008).
             05 02EXP011CO              PIC X(008).
             05 02EXP012CO              PIC X(008).
             05 02EXP013CO              PIC X(008).
             05 FILLER-02EXPCO          PIC X(056).

             05 02EXPPTFAH              PIC X(002).
             05 02EXP001AH              PIC X(003).
             05 02EXP002AH              PIC X(003).
             05 02EXP003AH              PIC X(003).
             05 02EXP004AH              PIC X(003).
             05 02EXP005AH              PIC X(003).
             05 02EXP006AH              PIC X(003).
             05 02EXP007AH              PIC X(008).
             05 02EXP008AH              PIC X(008).
             05 02EXP009AH              PIC X(008).
             05 02EXP010AH              PIC X(008).
             05 02EXP011AH              PIC X(008).
             05 02EXP012AH              PIC X(008).
             05 02EXP013AH              PIC X(008).
             05 FILLER-02EXPAH          PIC X(056).

             05 02EXPPTFCT              PIC X(002).
             05 02EXP001CT              PIC X(003).
             05 02EXP002CT              PIC X(003).
             05 02EXP003CT              PIC X(003).
             05 02EXP004CT              PIC X(003).
             05 02EXP005CT              PIC X(003).
             05 02EXP006CT              PIC X(003).
             05 02EXP007CT              PIC X(008).
             05 02EXP008CT              PIC X(008).
             05 02EXP009CT              PIC X(008).
             05 02EXP010CT              PIC X(008).
             05 02EXP011CT              PIC X(008).
             05 02EXP012CT              PIC X(008).
             05 02EXP013CT              PIC X(008).
             05 FILLER-02EXPCT          PIC X(056).

        02 VAR-02EXP-TMP.
             05 02EXP006V               PIC 9(004).
             05 02EXP006C               PIC 9(004).
             05 02EXP007NTO             PIC 9(004).
             05 02EXP007DTO             PIC 9(004).
             05 02EXP008NTO             PIC 9(004).
             05 02EXP008DTO             PIC 9(004).
             05 02EXP009NTO             PIC 9(004).
             05 02EXP009DTO             PIC 9(004).
             05 02EXP010NTO             PIC 9(004).
             05 02EXP010DTO             PIC 9(004).
             05 02EXP011NTO             PIC 9(004).
             05 02EXP011DTO             PIC 9(004).
             05 02EXP012NTO             PIC 9(004).
             05 02EXP012DTO             PIC 9(004).
             05 02EXP013NTO             PIC 9(004).
             05 02EXP013DTO             PIC 9(004).
             05 02EXP007FTO             PIC 9(001).
             05 02EXP008FTO             PIC 9(001).
             05 02EXP009FTO             PIC 9(001).
             05 02EXP010FTO             PIC 9(001).
             05 02EXP011FTO             PIC 9(001).
             05 02EXP012FTO             PIC 9(001).
             05 02EXP013FTO             PIC 9(001).

       01 VAR-02END.
         02 VAR-02END-SALIDA.
          03 TABLA-02END OCCURS 9 TIMES.
             05 02ENDPTF                PIC X(002).
             05 02END001                PIC -9999999.99.
             05 02END002                PIC -9999999.99.
             05 02END003                PIC -9999999.99.
             05 02END004                PIC -9999999.99.
             05 02END005                PIC -9999999.99.
             05 02END006                PIC 99999999.99.
             05 02END007                PIC 99999999.99.
             05 02END008                PIC 99999999.99.
             05 02END009                PIC 99999999.99.
             05 02END010                PIC 99999999.99.
             05 02END011                PIC -9999999.99.
             05 02END012                PIC -9999999.99.
             05 02END013                PIC -9999999.99.
             05 02END014                PIC -9999999.99.
             05 02END015                PIC 99999999.99.
             05 02END016                PIC 99999999.99.
             05 02END017                PIC 99999999.99.
             05 02END018                PIC 99999999.99.
             05 02END019                PIC 99999999.99.
             05 02END020                PIC -9999999.99.
             05 02END021                PIC -9999999.99.
             05 02END022                PIC -9999999.99.
             05 02END023                PIC -9999999.99.
             05 02END024                PIC -9999999.99.
             05 02END025                PIC -9999999.99.
             05 02END026                PIC -9999999.99.
             05 02END027                PIC -9999999.99.
             05 02END028                PIC -9999999.99.
             05 02END029                PIC -9999999.99.
             05 02END030                PIC -9999999.99.
             05 02END031                PIC -9999999.99.
             05 02END032                PIC -9999999.99.
             05 02END033                PIC -9999999.99.
             05 02END034                PIC -9999999.99.
             05 02END035                PIC -9999999.99.
             05 02END036                PIC -9999999.99.
             05 02END037                PIC -9999999.99.
             05 02END038                PIC -9999999.99.
             05 02END039                PIC -9999999.99.
             05 02END040                PIC -9999999.99.
             05 02END041                PIC -9999999.99.
             05 02END042                PIC -9999999.99.
             05 02END043                PIC -9999999.99.
             05 02END044                PIC -9999999.99.
             05 02END045                PIC -9999999.99.
             05 02END046                PIC -9999999.99.
             05 02END047                PIC -9999999.99.
          03 TABLA-02END-R REDEFINES TABLA-02END.
             05 02ENDPTFRO              PIC X(002).
             05 02END001RO              PIC X(011).
             05 02END002RO              PIC X(011).
             05 02END003RO              PIC X(011).
             05 02END004RO              PIC X(011).
             05 02END005RO              PIC X(011).
             05 02END006RO              PIC X(011).
             05 02END007RO              PIC X(011).
             05 02END008RO              PIC X(011).
             05 02END009RO              PIC X(011).
             05 02END010RO              PIC X(011).
             05 02END011RO              PIC X(011).
             05 02END012RO              PIC X(011).
             05 02END013RO              PIC X(011).
             05 02END014RO              PIC X(011).
             05 02END015RO              PIC X(011).
             05 02END016RO              PIC X(011).
             05 02END017RO              PIC X(011).
             05 02END018RO              PIC X(011).
             05 02END019RO              PIC X(011).
             05 02END020RO              PIC X(011).
             05 02END021RO              PIC X(011).
             05 02END022RO              PIC X(011).
             05 02END023RO              PIC X(011).
             05 02END024RO              PIC X(011).
             05 02END025RO              PIC X(011).
             05 02END026RO              PIC X(011).
             05 02END027RO              PIC X(011).
             05 02END028RO              PIC X(011).
             05 02END029RO              PIC X(011).
             05 02END030RO              PIC X(011).
             05 02END031RO              PIC X(011).
             05 02END032RO              PIC X(011).
             05 02END033RO              PIC X(011).
             05 02END034RO              PIC X(011).
             05 02END035RO              PIC X(011).
             05 02END036RO              PIC X(011).
             05 02END037RO              PIC X(011).
             05 02END038RO              PIC X(011).
             05 02END039RO              PIC X(011).
             05 02END040RO              PIC X(011).
             05 02END041RO              PIC X(011).
             05 02END042RO              PIC X(011).
             05 02END043RO              PIC X(011).
             05 02END044RO              PIC X(011).
             05 02END045RO              PIC X(011).
             05 02END046RO              PIC X(011).
             05 02END047RO              PIC X(011).

             05 02ENDPTFIN              PIC X(002).
             05 02END001IN              PIC X(011).
             05 02END002IN              PIC X(011).
             05 02END003IN              PIC X(011).
             05 02END004IN              PIC X(011).
             05 02END005IN              PIC X(011).
             05 02END006IN              PIC X(011).
             05 02END007IN              PIC X(011).
             05 02END008IN              PIC X(011).
             05 02END009IN              PIC X(011).
             05 02END010IN              PIC X(011).
             05 02END011IN              PIC X(011).
             05 02END012IN              PIC X(011).
             05 02END013IN              PIC X(011).
             05 02END014IN              PIC X(011).
             05 02END015IN              PIC X(011).
             05 02END016IN              PIC X(011).
             05 02END017IN              PIC X(011).
             05 02END018IN              PIC X(011).
             05 02END019IN              PIC X(011).
             05 02END020IN              PIC X(011).
             05 02END021IN              PIC X(011).
             05 02END022IN              PIC X(011).
             05 02END023IN              PIC X(011).
             05 02END024IN              PIC X(011).
             05 02END025IN              PIC X(011).
             05 02END026IN              PIC X(011).
             05 02END027IN              PIC X(011).
             05 02END028IN              PIC X(011).
             05 02END029IN              PIC X(011).
             05 02END030IN              PIC X(011).
             05 02END031IN              PIC X(011).
             05 02END032IN              PIC X(011).
             05 02END033IN              PIC X(011).
             05 02END034IN              PIC X(011).
             05 02END035IN              PIC X(011).
             05 02END036IN              PIC X(011).
             05 02END037IN              PIC X(011).
             05 02END038IN              PIC X(011).
             05 02END039IN              PIC X(011).
             05 02END040IN              PIC X(011).
             05 02END041IN              PIC X(011).
             05 02END042IN              PIC X(011).
             05 02END043IN              PIC X(011).
             05 02END044IN              PIC X(011).
             05 02END045IN              PIC X(011).
             05 02END046IN              PIC X(011).
             05 02END047IN              PIC X(011).

             05 02ENDPTFVE              PIC X(002).
             05 02END001VE              PIC X(011).
             05 02END002VE              PIC X(011).
             05 02END003VE              PIC X(011).
             05 02END004VE              PIC X(011).
             05 02END005VE              PIC X(011).
             05 02END006VE              PIC X(011).
             05 02END007VE              PIC X(011).
             05 02END008VE              PIC X(011).
             05 02END009VE              PIC X(011).
             05 02END010VE              PIC X(011).
             05 02END011VE              PIC X(011).
             05 02END012VE              PIC X(011).
             05 02END013VE              PIC X(011).
             05 02END014VE              PIC X(011).
             05 02END015VE              PIC X(011).
             05 02END016VE              PIC X(011).
             05 02END017VE              PIC X(011).
             05 02END018VE              PIC X(011).
             05 02END019VE              PIC X(011).
             05 02END020VE              PIC X(011).
             05 02END021VE              PIC X(011).
             05 02END022VE              PIC X(011).
             05 02END023VE              PIC X(011).
             05 02END024VE              PIC X(011).
             05 02END025VE              PIC X(011).
             05 02END026VE              PIC X(011).
             05 02END027VE              PIC X(011).
             05 02END028VE              PIC X(011).
             05 02END029VE              PIC X(011).
             05 02END030VE              PIC X(011).
             05 02END031VE              PIC X(011).
             05 02END032VE              PIC X(011).
             05 02END033VE              PIC X(011).
             05 02END034VE              PIC X(011).
             05 02END035VE              PIC X(011).
             05 02END036VE              PIC X(011).
             05 02END037VE              PIC X(011).
             05 02END038VE              PIC X(011).
             05 02END039VE              PIC X(011).
             05 02END040VE              PIC X(011).
             05 02END041VE              PIC X(011).
             05 02END042VE              PIC X(011).
             05 02END043VE              PIC X(011).
             05 02END044VE              PIC X(011).
             05 02END045VE              PIC X(011).
             05 02END046VE              PIC X(011).
             05 02END047VE              PIC X(011).

             05 02ENDPTFHP              PIC X(002).
             05 02END001HP              PIC X(011).
             05 02END002HP              PIC X(011).
             05 02END003HP              PIC X(011).
             05 02END004HP              PIC X(011).
             05 02END005HP              PIC X(011).
             05 02END006HP              PIC X(011).
             05 02END007HP              PIC X(011).
             05 02END008HP              PIC X(011).
             05 02END009HP              PIC X(011).
             05 02END010HP              PIC X(011).
             05 02END011HP              PIC X(011).
             05 02END012HP              PIC X(011).
             05 02END013HP              PIC X(011).
             05 02END014HP              PIC X(011).
             05 02END015HP              PIC X(011).
             05 02END016HP              PIC X(011).
             05 02END017HP              PIC X(011).
             05 02END018HP              PIC X(011).
             05 02END019HP              PIC X(011).
             05 02END020HP              PIC X(011).
             05 02END021HP              PIC X(011).
             05 02END022HP              PIC X(011).
             05 02END023HP              PIC X(011).
             05 02END024HP              PIC X(011).
             05 02END025HP              PIC X(011).
             05 02END026HP              PIC X(011).
             05 02END027HP              PIC X(011).
             05 02END028HP              PIC X(011).
             05 02END029HP              PIC X(011).
             05 02END030HP              PIC X(011).
             05 02END031HP              PIC X(011).
             05 02END032HP              PIC X(011).
             05 02END033HP              PIC X(011).
             05 02END034HP              PIC X(011).
             05 02END035HP              PIC X(011).
             05 02END036HP              PIC X(011).
             05 02END037HP              PIC X(011).
             05 02END038HP              PIC X(011).
             05 02END039HP              PIC X(011).
             05 02END040HP              PIC X(011).
             05 02END041HP              PIC X(011).
             05 02END042HP              PIC X(011).
             05 02END043HP              PIC X(011).
             05 02END044HP              PIC X(011).
             05 02END045HP              PIC X(011).
             05 02END046HP              PIC X(011).
             05 02END047HP              PIC X(011).

             05 02ENDPTFOT              PIC X(002).
             05 02END001OT              PIC X(011).
             05 02END002OT              PIC X(011).
             05 02END003OT              PIC X(011).
             05 02END004OT              PIC X(011).
             05 02END005OT              PIC X(011).
             05 02END006OT              PIC X(011).
             05 02END007OT              PIC X(011).
             05 02END008OT              PIC X(011).
             05 02END009OT              PIC X(011).
             05 02END010OT              PIC X(011).
             05 02END011OT              PIC X(011).
             05 02END012OT              PIC X(011).
             05 02END013OT              PIC X(011).
             05 02END014OT              PIC X(011).
             05 02END015OT              PIC X(011).
             05 02END016OT              PIC X(011).
             05 02END017OT              PIC X(011).
             05 02END018OT              PIC X(011).
             05 02END019OT              PIC X(011).
             05 02END020OT              PIC X(011).
             05 02END021OT              PIC X(011).
             05 02END022OT              PIC X(011).
             05 02END023OT              PIC X(011).
             05 02END024OT              PIC X(011).
             05 02END025OT              PIC X(011).
             05 02END026OT              PIC X(011).
             05 02END027OT              PIC X(011).
             05 02END028OT              PIC X(011).
             05 02END029OT              PIC X(011).
             05 02END030OT              PIC X(011).
             05 02END031OT              PIC X(011).
             05 02END032OT              PIC X(011).
             05 02END033OT              PIC X(011).
             05 02END034OT              PIC X(011).
             05 02END035OT              PIC X(011).
             05 02END036OT              PIC X(011).
             05 02END037OT              PIC X(011).
             05 02END038OT              PIC X(011).
             05 02END039OT              PIC X(011).
             05 02END040OT              PIC X(011).
             05 02END041OT              PIC X(011).
             05 02END042OT              PIC X(011).
             05 02END043OT              PIC X(011).
             05 02END044OT              PIC X(011).
             05 02END045OT              PIC X(011).
             05 02END046OT              PIC X(011).
             05 02END047OT              PIC X(011).

             05 02ENDPTFCC              PIC X(002).
             05 02END001CC              PIC X(011).
             05 02END002CC              PIC X(011).
             05 02END003CC              PIC X(011).
             05 02END004CC              PIC X(011).
             05 02END005CC              PIC X(011).
             05 02END006CC              PIC X(011).
             05 02END007CC              PIC X(011).
             05 02END008CC              PIC X(011).
             05 02END009CC              PIC X(011).
             05 02END010CC              PIC X(011).
             05 02END011CC              PIC X(011).
             05 02END012CC              PIC X(011).
             05 02END013CC              PIC X(011).
             05 02END014CC              PIC X(011).
             05 02END015CC              PIC X(011).
             05 02END016CC              PIC X(011).
             05 02END017CC              PIC X(011).
             05 02END018CC              PIC X(011).
             05 02END019CC              PIC X(011).
             05 02END020CC              PIC X(011).
             05 02END021CC              PIC X(011).
             05 02END022CC              PIC X(011).
             05 02END023CC              PIC X(011).
             05 02END024CC              PIC X(011).
             05 02END025CC              PIC X(011).
             05 02END026CC              PIC X(011).
             05 02END027CC              PIC X(011).
             05 02END028CC              PIC X(011).
             05 02END029CC              PIC X(011).
             05 02END030CC              PIC X(011).
             05 02END031CC              PIC X(011).
             05 02END032CC              PIC X(011).
             05 02END033CC              PIC X(011).
             05 02END034CC              PIC X(011).
             05 02END035CC              PIC X(011).
             05 02END036CC              PIC X(011).
             05 02END037CC              PIC X(011).
             05 02END038CC              PIC X(011).
             05 02END039CC              PIC X(011).
             05 02END040CC              PIC X(011).
             05 02END041CC              PIC X(011).
             05 02END042CC              PIC X(011).
             05 02END043CC              PIC X(011).
             05 02END044CC              PIC X(011).
             05 02END045CC              PIC X(011).
             05 02END046CC              PIC X(011).
             05 02END047CC              PIC X(011).

             05 02ENDPTFCO              PIC X(002).
             05 02END001CO              PIC X(011).
             05 02END002CO              PIC X(011).
             05 02END003CO              PIC X(011).
             05 02END004CO              PIC X(011).
             05 02END005CO              PIC X(011).
             05 02END006CO              PIC X(011).
             05 02END007CO              PIC X(011).
             05 02END008CO              PIC X(011).
             05 02END009CO              PIC X(011).
             05 02END010CO              PIC X(011).
             05 02END011CO              PIC X(011).
             05 02END012CO              PIC X(011).
             05 02END013CO              PIC X(011).
             05 02END014CO              PIC X(011).
             05 02END015CO              PIC X(011).
             05 02END016CO              PIC X(011).
             05 02END017CO              PIC X(011).
             05 02END018CO              PIC X(011).
             05 02END019CO              PIC X(011).
             05 02END020CO              PIC X(011).
             05 02END021CO              PIC X(011).
             05 02END022CO              PIC X(011).
             05 02END023CO              PIC X(011).
             05 02END024CO              PIC X(011).
             05 02END025CO              PIC X(011).
             05 02END026CO              PIC X(011).
             05 02END027CO              PIC X(011).
             05 02END028CO              PIC X(011).
             05 02END029CO              PIC X(011).
             05 02END030CO              PIC X(011).
             05 02END031CO              PIC X(011).
             05 02END032CO              PIC X(011).
             05 02END033CO              PIC X(011).
             05 02END034CO              PIC X(011).
             05 02END035CO              PIC X(011).
             05 02END036CO              PIC X(011).
             05 02END037CO              PIC X(011).
             05 02END038CO              PIC X(011).
             05 02END039CO              PIC X(011).
             05 02END040CO              PIC X(011).
             05 02END041CO              PIC X(011).
             05 02END042CO              PIC X(011).
             05 02END043CO              PIC X(011).
             05 02END044CO              PIC X(011).
             05 02END045CO              PIC X(011).
             05 02END046CO              PIC X(011).
             05 02END047CO              PIC X(011).

             05 FILLER-02ENDAH          PIC X(519).

             05 FILLER-02ENDCT          PIC X(519).

         02 VAR-02END-CB.
          03 02ENDPTFCB                 PIC X(002).
          03 02END001CB                 PIC -9999999.99.
          03 02END002CB                 PIC -9999999.99.
          03 02END003CB                 PIC -9999999.99.
          03 02END004CB                 PIC -9999999.99.
          03 02END005CB                 PIC -9999999.99.
          03 02END006CB                 PIC 99999999.99.
          03 02END007CB                 PIC 99999999.99.
          03 02END008CB                 PIC 99999999.99.
          03 02END009CB                 PIC 99999999.99.
          03 02END010CB                 PIC 99999999.99.
          03 02END011CB                 PIC -9999999.99.
          03 02END012CB                 PIC -9999999.99.
          03 02END013CB                 PIC -9999999.99.
          03 02END014CB                 PIC -9999999.99.
          03 02END015CB                 PIC 99999999.99.
          03 02END016CB                 PIC 99999999.99.
          03 02END017CB                 PIC 99999999.99.
          03 02END018CB                 PIC 99999999.99.
          03 02END019CB                 PIC 99999999.99.
          03 02END020CB                 PIC -9999999.99.
          03 02END021CB                 PIC -9999999.99.
          03 02END022CB                 PIC -9999999.99.
          03 02END023CB                 PIC -9999999.99.
          03 02END024CB                 PIC -9999999.99.
          03 02END025CB                 PIC -9999999.99.
          03 02END026CB                 PIC -9999999.99.
          03 02END027CB                 PIC -9999999.99.
          03 02END028CB                 PIC -9999999.99.
          03 02END029CB                 PIC -9999999.99.
          03 02END030CB                 PIC -9999999.99.
          03 02END031CB                 PIC -9999999.99.
          03 02END032CB                 PIC -9999999.99.
          03 02END033CB                 PIC -9999999.99.
          03 02END034CB                 PIC -9999999.99.
          03 02END035CB                 PIC -9999999.99.
         02 VAR-02END-VALORES.
          03 02END001V                  PIC S9(008)V99.
          03 02END001T                  PIC 9(008)V99.
          03 02END002V                  PIC S9(008)V99.
          03 02END002T                  PIC 9(008)V99.
          03 02END003V                  PIC S9(008)V99.
          03 02END003T                  PIC 9(008)V99.
          03 02END004V                  PIC S9(008)V99.
          03 02END004T                  PIC 9(008)V99.
          03 02END005V                  PIC S9(008)V99.
          03 02END005T                  PIC 9(008)V99.
          03 02END006V                  PIC 9(008)V99.
          03 02END006T                  PIC 9(008)V99.
          03 02END007V                  PIC 9(008)V99.
          03 02END007T                  PIC 9(008)V99.
          03 02END008V                  PIC 9(008)V99.
          03 02END008T                  PIC 9(008)V99.
          03 02END009V                  PIC 9(008)V99.
          03 02END009T                  PIC 9(008)V99.
          03 02END010V                  PIC 9(008)V99.
          03 02END010T                  PIC 9(008)V99.
          03 02END011CN                 PIC 9(008)V99.
          03 02END011SN                 PIC 9(008)V99.
          03 02END011C                  PIC 9(008)V99.
          03 02END011S                  PIC 9(008)V99.
          03 02END012CN                 PIC 9(008)V99.
          03 02END012SN                 PIC 9(008)V99.
          03 02END012C                  PIC 9(008)V99.
          03 02END012S                  PIC 9(008)V99.
          03 02END013CN                 PIC 9(008)V99.
          03 02END013SN                 PIC 9(008)V99.
          03 02END013C                  PIC 9(008)V99.
          03 02END013S                  PIC 9(008)V99.
          03 02END014CN                 PIC 9(008)V99.
          03 02END014SN                 PIC 9(008)V99.
          03 02END014C                  PIC 9(008)V99.
          03 02END014S                  PIC 9(008)V99.
          03 02END015C                  PIC 9(008)V99.
          03 02END015CV                 PIC 9(008)V99.
          03 02END016C                  PIC 9(008)V99.
          03 02END016CV                 PIC 9(008)V99.
          03 02END017C                  PIC 9(008)V99.
          03 02END017CV                 PIC 9(008)V99.
          03 02END018C                  PIC 9(008)V99.
          03 02END018CV                 PIC 9(008)V99.
          03 02END019C                  PIC 9(008)V99.
          03 02END019CV                 PIC 9(008)V99.
          03 02END020C                  PIC 9(008)V99.
          03 02END020CV                 PIC 9(008)V99.
          03 02END020V                  PIC 9(008)V99.
          03 02END020T                  PIC 9(008)V99.
          03 02END021C                  PIC 9(008)V99.
          03 02END021CV                 PIC 9(008)V99.
          03 02END021V                  PIC 9(008)V99.
          03 02END021T                  PIC 9(008)V99.
          03 02END022C                  PIC 9(008)V99.
          03 02END022CV                 PIC 9(008)V99.
          03 02END022V                  PIC 9(008)V99.
          03 02END022T                  PIC 9(008)V99.
          03 02END023C                  PIC 9(008)V99.
          03 02END023CV                 PIC 9(008)V99.
          03 02END023V                  PIC 9(008)V99.
          03 02END023T                  PIC 9(008)V99.
          03 02END024S                  PIC 9(008)V99.
          03 02END024SN                 PIC 9(008)V99.
          03 02END025S                  PIC 9(008)V99.
          03 02END025SN                 PIC 9(008)V99.
          03 02END026S                  PIC 9(008)V99.
          03 02END026SN                 PIC 9(008)V99.
          03 02END027S                  PIC 9(008)V99.
          03 02END027SN                 PIC 9(008)V99.
          03 02END028S                  PIC 9(008)V99.
          03 02END028SM                 PIC 9(008)V99.
          03 02END029S                  PIC 9(008)V99.
          03 02END029SM                 PIC 9(008)V99.
          03 02END030S                  PIC 9(008)V99.
          03 02END030SM                 PIC 9(008)V99.
          03 02END031S                  PIC 9(008)V99.
          03 02END031SM                 PIC 9(008)V99.
          03 02END032C                  PIC 9(008)V99.
          03 02END032CN                 PIC 9(008)V99.
          03 02END033C                  PIC 9(008)V99.
          03 02END033CN                 PIC 9(008)V99.
          03 02END034C                  PIC 9(008)V99.
          03 02END034CN                 PIC 9(008)V99.
          03 02END035C                  PIC 9(008)V99.
          03 02END035CN                 PIC 9(008)V99.
          03 02ENDPAGO                  PIC S9(008)V99.
          03 02ENDRATION                PIC 9(005)V99.
          03 02ENDRATIO                 PIC 9(005)V99.
SCOUHD*   ESTAS VARIABLES SE DEBEN ELIMINAR CUANDO SE IMPLEMENTEN REAL  RMB0003
SCOUHD 01 VARIABLES-EXTERNAS-USA.                                       RMB0003
SCOUHD       04 02NUM020TO          PIC 9(04) VALUE ZEROS. *> CARACNUEVARMB0003

ACIEAA 01 VAR-ENDV090-093-CARACT.                                       RMB0006
ACIEAA    03 FILLER OCCURS 9 TIMES.                                     RMB0006
ACIEAA       05 UTILAUX-CARACT                PIC 9(8)V9(2).            RMB0006
ACIEAA       05 NUMMESES090-CARACT            PIC 9(8)V9(8).            RMB0006
ACIEA1       05 SUMASALDO090-CARACT           PIC 9(11)V9(7).           RMB0007
ACIEA1       05 PROMSALDO090-CARACT           PIC 9(11)V9(7).           RMB0007
ACIEA1       05 VARIANZASALDO090-CARACT       PIC 9(13)V9(5).           RMB0007
ACIEA1       05 DESVIACIONSALDO090-CARACT     PIC 9(13)V9(5).           RMB0007
ACIEAA       05 FILLER OCCURS 13 TIMES.                                 RMB0006
ACIEA1          07 SALDOACUM090-CARACT    PIC 9(11).                    RMB0007
ACIEA1*         07 SUMACUPO090-CARACT     PIC 9(11).                    RMB0007
ACIEAA       05 V-SALDOFLAG090 .                                        RMB0006
ACIEAA          07 SALDOFLAG090 OCCURS 13 TIMES   PIC 9(1).             RMB0006 
ACIEA1*      05 V-CUPOFLAG090.                                          RMB0007
ACIEA1*         07 CUPOFLAG090  OCCURS 13 TIMES   PIC 9(1).             RMB0007
ACIEAA*                                                                 RMB0006
ACIEAA*      05 UTIL091-CARACT                PIC 9(8)V9(8).            RMB0006
ACIEAA       05 NUMMESES091-CARACT            PIC 9(8)V9(8).            RMB0006
ACIEA1       05 SUMACUOTA091-CARACT           PIC 9(11).                RMB0007
ACIEA1       05 PROMCUOTA091-CARACT           PIC 9(11)V9(7).           RMB0007
ACIEA1       05 VARIANZACUOTA091-CARACT       PIC 9(13)V9(5).           RMB0007
ACIEA1       05 DESVIACIONCUOTA091-CARACT     PIC 9(13)V9(5).           RMB0007
ACIEAA       05 FILLER OCCURS 13 TIMES.                                 RMB0006
ACIEA1          07 CUOTAACUM091-CARACT    PIC 9(11).                    RMB0007
ACIEA1*         07 SUMACUPO091-CARACT     PIC 9(11).                    RMB0007
ACIEA1       05 V-CUOTAFLAG091 .                                        RMB0007
ACIEA1          07 CUOTAFLAG091 OCCURS 13 TIMES   PIC 9(1).             RMB0007
ACIEA1*      05 V-SALDOFLAG091 .                                        RMB0007
ACIEA1*         07 SALDOFLAG091 OCCURS 13 TIMES   PIC 9(1).             RMB0007
ACIEA1*      05 V-CUPOFLAG091.                                          RMB0007
ACIEA1*         07 CUPOFLAG091  OCCURS 13 TIMES   PIC 9(1).             RMB0007
ACIEAA*                                                                 RMB0006
ACIEAA       05 UTIL092-CARACT                PIC 9(8)V9(8).            RMB0006
ACIEAA       05 NUMMESES092-CARACT            PIC 9(8)V9(8).            RMB0006
ACIEAA       05 PROMUTIL092-CARACT            PIC 9(8)V9(8).            RMB0006
ACIEAA       05 VARIANZASALCUP092-CARACT      PIC 9(8)V9(8).            RMB0006
ACIEAA       05 DESVIACIONSALCUP092-CARACT    PIC 9(8)V9(8).            RMB0006
ACIEAA       05 FILLER OCCURS 13 TIMES.                                 RMB0006
ACIEAA          07 SUMASALDO092-CARACT      PIC 9(11).                  RMB0006
ACIEAA          07 SUMACUPO092-CARACT       PIC 9(11).                  RMB0006
ACIEAA       05 V-SALDOFLAG092 .                                        RMB0006
ACIEAA          07 SALDOFLAG092 OCCURS 13 TIMES   PIC 9(1).             RMB0006 
ACIEAA       05 V-CUPOFLAG092.                                          RMB0006
ACIEAA          07 CUPOFLAG092  OCCURS 13 TIMES   PIC 9(1).             RMB0006



ACIEAA       05 UTIL093-CARACT                PIC 9(8)V9(8).            RMB0006
ACIEAA       05 NUMMESES093-CARACT            PIC 9(8)V9(8).            RMB0006
ACIEAA       05 PROMUTIL093-CARACT            PIC 9(11)V9(7).           RMB0006
ACIEAA       05 PROMCOCIENTE093-CARACT        PIC S9(11)V9(7).          RMB0006
ACIEAA       05 COCIENTETOT093-CARACT         PIC S9(11)V9(7).          RMB0006
ACIEAA       05 FILLER OCCURS 13 TIMES.                                 RMB0006
ACIEA1          07 DIFFEACUM093-CARACT    PIC S9(11).                   RMB0007
ACIEA1          07 CUPOACUM093-CARACT     PIC 9(11).                    RMB0007
ACIEA1       05 V-DIFFEFLAG093 .                                        RMB0007
ACIEA1          07 DIFFEFLAG093 OCCURS 13 TIMES   PIC 9(1).             RMB0007
ACIEA1*      05 V-CUPOFLAG093.                                          RMB0007
ACIEA1*         07 CUPOFLAG093  OCCURS 13 TIMES   PIC 9(1).             RMB0007
ACIEAA*                                                                 RMB0006
ACIEAA 01 FLAG-END090-093-CARACT.                                       RMB0006
ACIEAA   03 T-FLAG-END090-093-CARACT OCCURS 7 TIMES.                    RMB0006
ACIEAA      07 FLAG090                    PIC 9(1).                     RMB0006
ACIEAA      07 OPFLAG090                  PIC 9(1).                     RMB0006
ACIEAA*                                                                 RMB0006
ACIEAA      07 FLAG091                    PIC 9(1).                     RMB0006
ACIEAA      07 OPFLAG091                  PIC 9(1).                     RMB0006
ACIEAA*                                                                 RMB0006
ACIEAA      07 FLAG092                    PIC 9(1).                     RMB0006
ACIEAA      07 OPFLAG092                  PIC 9(1).                     RMB0006
ACIEAA*                                                                 RMB0006
ACIEAA      07 FLAG093                    PIC 9(1).                     RMB0006
ACIEAA      07 OPFLAG093                  PIC 9(1).                     RMB0006
