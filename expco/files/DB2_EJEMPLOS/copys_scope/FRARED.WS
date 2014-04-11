******* FRARED-WS.v.0003
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES
******* QUE UTILIZAN LAS RUTINAS DE BASE DE LA RUTINA DE FRARED 
***********************************************************************
REMARK* ABB0002     * REDOP3 - SE INCLUYEN VARIABLES PARA EL MANEJO    *ABB0002
REMARK* ALBEIRO     *          DE LA RED DE BANCOS                     *ABB0002
REMARK* BERNAL      *                                                  *ABB0002
REMARK* FEB.27/2009 *                                                  *ABB0002
REMARK******************************************************************ABB0002
REMARK* ABB0003     * REDOP4 - SE AUMENTA CAPACIDAD DE TABLAS PARA     *ABB0003
REMARK* ALBEIRO     *          MANEJO DE LA RED TIGO                   *ABB0003
REMARK* BERNAL      *                                                  *ABB0003
REMARK* OCT.31/2010 *                                                  *ABB0003
REMARK******************************************************************ABB0003
       01 VALORES-999-FRARED.
          02 VAL-CODPRGOK999-FRARED     PIC S9(5)V9(13).
          02 VAL-CODPRGERR999-FRARED    PIC S9(5)V9(13).
          02 VAL-PRGSUSOK999-FRARED     PIC S9(5)V9(13).
          02 VAL-PRGSUSERR999-FRARED    PIC S9(5)V9(13).
REDOP3*   VARIABLES PARA RED DE BANCOS                                  ABB0002
REDOP3    02 VAL-CODPRG9NN-FRARED       PIC S9(5)V9(13).                ABB0002
REDOP3    02 VAL-PRGSUS9NN-FRARED       PIC S9(5)V9(13).                ABB0002
REDOP3    02 VAL-VALORPREG-FRARED       PIC S9(5)V9(13).                ABB0002
       01 VALORES-FRARED.
          02 VAL-PORPRGOK-FRARED        PIC S9(5)V9(13).
          02 VAL-PORPESOK-FRARED        PIC S9(5)V9(13).
          02 VAL-RIESGOCIUD-FRARED      PIC S9(5)V9(13).
          02 VAL-REACCESO-FRARED        PIC S9(5)V9(13).
          02 V30-FRARED OCCURS 30 TIMES.
             03 VAL-CODPRG-FRARED       PIC S9(5)V9(13).
             03 VAL-PRGSUS-FRARED       PIC S9(5)V9(13).
             03 VAL-PRGBLLT-FRARED      PIC S9(5)V9(13).
             03 VAL-PRGREAL-FRARED      PIC S9(5)V9(13).
          02 FILLER REDEFINES V30-FRARED.
             03 V-PRE-FRARED OCCURS 120 TIMES
                                     PIC S9(5)V9(13).
REDOP3* VECTOR DE ENTRADA CAPA 1                                       *ABB0002
REDOP3 01 FILLER REDEFINES VALORES-FRARED.                              ABB0002 
REDOP3    02 V-CAPA1-FRARED OCCURS 124 TIMES                            ABB0002
REDOP3                                  PIC S9(5)V9(13).                ABB0002
       01 ERRORES-FRARED.
          02 ERR-PORPRGOK-FRARED        PIC 9(01) VALUE 0.
          02 ERR-PORPESOK-FRARED        PIC 9(01) VALUE 0.
          02 ERR-RIESGOCIUD-FRARED      PIC 9(01) VALUE 0.
          02 ERR-CODPRGOK-FRARED        PIC 9(01) VALUE 0.
          02 ERR-CODPRGERR-FRARED       PIC 9(01) VALUE 0.
          02 ERR-PRGSUSOK-FRARED        PIC 9(01) VALUE 0.
          02 ERR-PRGSUSERR-FRARED       PIC 9(01) VALUE 0.
          02 ERR-PRGBLLTOK-FRARED       PIC 9(01) VALUE 0.
          02 ERR-PRGBLLTERR-FRARED      PIC 9(01) VALUE 0.
          02 ERR-PRGREALOK-FRARED       PIC 9(01) VALUE 0.
          02 ERR-PRGREALERR-FRARED      PIC 9(01) VALUE 0.
REDOP3*   VARIABLES PARA RED DE BANCOS                                  ABB0002
REDOP3    02 ERR-CODPRG-FRARED          PIC 9(01) VALUE 0.              ABB0002
REDOP3    02 ERR-PRGSUS-FRARED          PIC 9(01) VALUE 0.              ABB0002
       01 VARIABLES-FRARED.                                      
          03 MAT02-FRARED OCCURS 12 TIMES
                                  PIC S9(5)V9(13).
          03 MAT03-FRARED OCCURS 16 TIMES
                                  PIC S9(5)V9(13).
          03 ERR-SCORE-FRARED     PIC 9(1).
          03 SCORE-FRARED         PIC 9(5).
          03 SCORE1-FRARED        PIC 9(5).
          03 TOTPRE-FRARED        PIC 9(3).
          03 INI2-FASE2-FRARED    PIC S9(5)V9(13).
          03 INI3-FASE2-FRARED    PIC S9(5)V9(13).
          03 INI-FASE2-FRARED     PIC S9(5)V9(13).
          03 VAL-FASE2-FRARED     PIC S9(5)V9(13).
          03 CODPRE-AUX-FRARED    PIC 9(9).
REDOP3*   ORIGEN PREGUNTA: 001=REG,004=CA/CC,005=CART,009=DIRECCIONES   ABB0002
REDOP3    03 CODPRE-AUX-FRARED-R  REDEFINES CODPRE-AUX-FRARED.          ABB0002
REDOP3       05 ORIPRE-AUX-FRARED PIC X(03).                            ABB0002
REDOP3       05 FILLER            PIC X(06).                            ABB0002
          03 STRING-INPUT-FRARED  PIC X(20).
          03 NOMTAB-INPUT-FRARED  PIC X(10).
REDOP3    03 FILLER               REDEFINES NOMTAB-INPUT-FRARED.        ABB0002
REDOP3       05 TABLA-INPUT-FRARED  PIC X(08).                          ABB0002
REDOP3       05 SEGMTO-INPUT-FRARED PIC X(01).                          ABB0002
REDOP3       05 FILLER            PIC X(01).                            ABB0002
          03 TIPBUS-INPUT-FRARED  PIC X(03).
          03 TIPDAT-INPUT-FRARED  PIC X(03).
          03 ENTTAB-INPUT-FRARED  PIC 9(18).
          03 ALFTAB-INPUT-FRARED  PIC X(18).
          03 DECTAB-INPUT-FRARED  PIC S9(5)V9(13).
          03 REACCESO-INPUT-FRARED  PIC 9(1).
          03 VALTAB-OUTPUT-FRARED PIC S9(5)V9(13).
          03 BUSOK-OUTPUT-FRARED  PIC 9(01).       
          03 NUMDEC-OUTPUT-FRARED PIC S9(5)V9(13).
          03 NUMENT-OUTPUT-FRARED PIC 9(18).        
          03 DIF1-FRARED          PIC 9(5)V9(13).   
          03 DIF2-FRARED          PIC 9(5)V9(13).   
          03 NENTERO-FRARED       PIC 9(18).
          03 ADECIMAL-FRARED      PIC X(17).
          03 NDECIMAL-FRARED REDEFINES ADECIMAL-FRARED
                                  PIC 9(17).
          03 STRING1-FRARED       PIC X(20).
          03 STRING2-FRARED       PIC X(20).
          03 FIN-FRARED           PIC 9 VALUE 0.
          03 I-FRARED             PIC 9(5) VALUE 0.
          03 J-FRARED             PIC 9(5) VALUE 0.
          03 K-FRARED             PIC 9(5) VALUE 0.
          03 P-FRARED             PIC 9(5) VALUE 0.
          03 II-FRARED            PIC 9(5) VALUE 0.
          03 JJ-FRARED            PIC 9(5) VALUE 0.
          03 LL-FRARED            PIC 9(5) VALUE 0.
          03 KK-FRARED            PIC 9(5) VALUE 0.
          03 LIMITE-FRARED        PIC 9(5) VALUE 0.
          03 AUX-FRARED           PIC 9(5) VALUE 0.
          03 INI-FRARED           PIC 9(5) VALUE 0.
          03 ALGUNA-PRGOK-FRARED  PIC 9(1) VALUE 0.
REMARK*******************************************************************
REMARK* ESTRUCTURAS DE MANEJO DE LAS TABLAS CARGADAS 
REMARK*******************************************************************
       01 MATRICES-FRARED.
REMARK*   ESTRUCTURA DE CONTROL DE LAS TABLAS CARGADAS
REDOP4    03 M-CONTROL-FRARED OCCURS 150 TIMES.                         ABB0003
             04 V-CONDES1-FRARED    PIC X(10).
             04 V-CONDES2-FRARED    PIC X(05).
             04 V-CONINI-FRARED     PIC 9(05).
             04 V-CONFIN-FRARED     PIC 9(05).
REMARK*==================================================================
REMARK*   ESTRUCTURA DONDE SE CARGAN LAS TABLAS           
          03 M-DATOS-FRARED   OCCURS 10000 TIMES.
REMARK*      PRIMER VALOR DE LA FILA 
             04 V-DATENT1-FRARED    PIC 9(18). *> VALOR ENTERO
             04 V-DATALF1-FRARED REDEFINES
                  V-DATENT1-FRARED  PIC X(18). *> VALOR ALFAFETICO
             04 V-DATDEC1-FRARED REDEFINES
                  V-DATENT1-FRARED  PIC S9(5)V9(13). *> VALOR DECIMAL
REMARK*      SEGUNDO VALOR DE LA FILA   
             04 V-DATVAL2-FRARED    PIC S9(5)V9(13).
REMARK*==================================================================
       01 CONBINACIONES-PRE-FRARED.
          02 PR01-FRARED   PIC X(10) VALUE "0000000000".
          02 PR02-FRARED   PIC X(10) VALUE "2000000000".
          02 PR03-FRARED   PIC X(10) VALUE "3000000000".
          02 PR04-FRARED   PIC X(10) VALUE "2200000000".
          02 PR05-FRARED   PIC X(10) VALUE "3200000000".
          02 PR06-FRARED   PIC X(10) VALUE "3300000000".
          02 PR07-FRARED   PIC X(10) VALUE "3220000000".
          02 PR08-FRARED   PIC X(10) VALUE "3320000000".
          02 PR09-FRARED   PIC X(10) VALUE "3330000000".
          02 PR10-FRARED   PIC X(10) VALUE "3322000000".
          02 PR11-FRARED   PIC X(10) VALUE "3332000000".
          02 PR12-FRARED   PIC X(10) VALUE "3333000000".
          02 PR13-FRARED   PIC X(10) VALUE "3332200000".
          02 PR14-FRARED   PIC X(10) VALUE "3333200000".
          02 PR15-FRARED   PIC X(10) VALUE "3333300000".
          02 PR16-FRARED   PIC X(10) VALUE "3333220000".
          02 PR17-FRARED   PIC X(10) VALUE "3333320000".
          02 PR18-FRARED   PIC X(10) VALUE "3333330000".
          02 PR19-FRARED   PIC X(10) VALUE "3333322000".
          02 PR20-FRARED   PIC X(10) VALUE "3333332000".
          02 PR21-FRARED   PIC X(10) VALUE "3333333000".
          02 PR22-FRARED   PIC X(10) VALUE "3333332200".
          02 PR23-FRARED   PIC X(10) VALUE "3333333200".
          02 PR24-FRARED   PIC X(10) VALUE "3333333300".
          02 PR25-FRARED   PIC X(10) VALUE "3333333220".
          02 PR26-FRARED   PIC X(10) VALUE "3333333320".
          02 PR27-FRARED   PIC X(10) VALUE "3333333330".
          02 PR28-FRARED   PIC X(10) VALUE "3333333322".
          02 PR29-FRARED   PIC X(10) VALUE "3333333332".
          02 PR30-FRARED   PIC X(10) VALUE "3333333333".
       01 FILLER REDEFINES CONBINACIONES-PRE-FRARED.
          03 FILLER OCCURS 30 TIMES.
             04 FILLER OCCURS 10 TIMES.
                05 COMBI-FRARED    PIC 9.
REDOP3*   NOMBRES DE LAS TABLAS DE LA RED DE BANCOS PREGUNTAS           ABB0002
REDOP3 01 TABLAS-PREGUNTAS-FRARED.                                      ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED01PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED02PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED03PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED04PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED05PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED06PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED07PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED08PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED09PRGF".                 ABB0002
REDOP3    02 FILLER        PIC X(10) VALUE "RED10PRGF".                 ABB0002
REDOP3 01 FILLER REDEFINES TABLAS-PREGUNTAS-FRARED.                     ABB0002
REDOP3    03 FILLER OCCURS 10 TIMES.                                    ABB0002
REDOP3       05 TABLA-C2-FRARED    PIC X(10).                           ABB0002
REMARK*=====================================================
