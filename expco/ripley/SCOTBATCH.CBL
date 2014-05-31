      *SCOTBATCH.v.01.0014
      *******************************************************************
      * Programa para calcular scores en batch actuales o historicos
      *
      * VERSION   21/07/2008 10:44:42 a.m.
REMARK*******************************************************************
REMARK*RMB0002    *QUAN01: ADICION DEL SCORE QUANTO -PREDICTOR DE INGRESOS
REMARK*R.MARTINEZ *PRHD01: ADICION PREDICTA HD
REMARK*09-SEP-2009*MANT01: ADICION ARCHIVOS DE MANTENIMIENTO QUANTO,HD
REMARK*******************************************************************
REMARK*LAG0002    *PAM456: HACERMATCH=SI
REMARK*G.DESARROL *
REMARK*03-DIC-2009*
REMARK*******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCOTBATCH.
       AUTHOR.     DATACREDITO.
       DATE-WRITTEN.
       DATE-COMPILED.
      $SET CONSTANT SW-STATUS-BDSCORES"SI"
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
      ******************************************************************
      * GDT0004    | CTAST8 - CAMBIO DEL ICMCHK (MAESTRO DE CUENTAS) A *
      * FEB.09-2010|    FORMATO TIPO 8.                                *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * LAG0005    | REQ601 - CUANDO NO ENCUENTRA SCORE LO CALCULA     *
      * SEP.14-2010|                                                   *
      * GER.DESARR.|                                                   *
      ******************************************************************
      ******************************************************************
      * GDT0006    | BDSCT8 - CAMBIO DEL BDSCORES (MAESTRO DE SCORES)  *
      * 07-SEP-2010|    A FORMATO TIPO 8.                              *
      * GER.DESARR.|                                                   *
      ******************************************************************
      ******************************************************************
      * GDT0007    | CARDEM - ADICIONAR LOS ARCHIVOS DE ESTRATOS Y DIAN*
      * 11-NOV-2010|    PARA EL CALCULO DE NUEVAS CARACTERISTICAS.     *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * PJV0008    | CQ3364 - COLOCAR PUNTO DE REINICIO PARA EL PROCESO*
      * 10-OCT-2011| sh-SCOREPREC CUANDO USA ESTE PROGRAMA.            *
      * PEDRO V.   |                                                   *
      ******************************************************************
      * PJV0009    | CQ4318 - POR RECOMEDACION DE RMB SE RETIRA EL OPEN*
      * 23-ENE-2012| I-O OPEN REINICIO, POR UN MANEJO ESPECIAL QUE SE  *
      * PEDRO V.   | DA A CONSULTAS ANTERIORES AL 2006.                *
      ******************************************************************
      * RMB0010    | R7129A - CAMBIAR EL REINICIO A FORMA AUTOMATICA   *
      * 07-AGO-2012| R7129B - APLIACION DE NOMBRE ARCHIVOS A 80 CARACT *
      * R MARTINEZ | R7129C - GENERAR CARACTERISTICAS                  *
      *            | R7129D - MANEJAR COMBOS DE PRODUCTOS              *
      ******************************************************************
      * STT0011    | CQ6539 - SE INCLUYE ENTRADA DESDE icestd12        *
      * 25-SEP-2012|                                                   *
      * LUIS GOMEZ |                                                   *
      ******************************************************************
      * STT0012    | ACIEAA - INCLUIR CALCULO DE ACIERTA-A PARA TELCOS *
      * 6-SEPT-2012| CQ6455                                            *
      * LUIS G.    | ALCANCE - ACIERTA A TELCOS ERA CODIGO 85 AHORA    *
      * 29-OCT-2012|           PASA A 49                               *
      * R.MARTINEZ | ACIEFI  - ACIERTA A PARA FGE,TRO,INT,HVT,COO      *
      * RMB0012    |              PRODUCTOS   047,048,095,041,045      *
      ******************************************************************
      * PJV0013    | CQ7550 - AJUSTES NUEVAS OPCIONES SCOTBARCH PARA EL*
      * 08-NOV-2012| MANEJO DE COMBOS DE LA SERIE DE SCORES (+)        *
      * PEDRO V.   |                                                   *
      ******************************************************************
      * STT0014    | R15689 - AJUSTES NUEVAS OPCIONES SCOTBATCH PARA EL*
      * 23-OCT-2013| MANEJO DE PRODUCTOS MICROCREDITO (119 Y 120)      *
      * LUIS GOMEZ |                                      CQ-15689     *
      ******************************************************************
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
CARDEM     COPY  BDLSTR.FS.                                             GDT0007
CARDEM*                                                                 GDT0007
           COPY ICBDIR.FS.
           COPY BDACSV.FS.
           COPY NOAMNISTIA21.FS.
           COPY ICBSUS.FS.
CTAST8*    COPY  ICBCHK.FS.                                             GDT0004
CTAST8     COPY  ICBCHK0001.FS.                                         GDT0004
           COPY ICBCRE.FS.
           COPY ICBCON.FS.
           COPY BDIIREGI.FS.
           COPY BDIIDIAN.FS.
           COPY BDIIDAS.FS.
           COPY BDIEMSUP0001.FS.
BDSCT8*    COPY  BDSCORES.FS.                                           GDT0006
BDSCT8     COPY  BDSCORES0001.FS.                                       GDT0006
QUAN01     COPY ICBCRE2.FS.                                             RMB0002
QUAN01     COPY ICSALMIN.FS.                                            RMB0002
QUAN01     COPY ICSANATA.FS.                                            RMB0002
QUAN01     COPY ICMVLR.FS.                                              RMB0002

           SELECT ENTRADA ASSIGN TO  DISK W-ENTRADA
                  ORGANIZATION   IS  LINE SEQUENTIAL
                  ACCESS MODE    IS  SEQUENTIAL
                  FILE STATUS    W-STATUS.

R7129D*    SELECT SALIDA ASSIGN  TO  DISK  W-SALIDA                     RMB0010
R7129D*           ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D*           ACCESS MODE    IS  SEQUENTIAL                         RMB0010
R7129D*           FILE STATUS W-STATUS-SAL.                             RMB0010
R7129D     SELECT SALIDA1 ASSIGN  TO  DISK  W-SALIDA1                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA2 ASSIGN  TO  DISK  W-SALIDA2                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA3 ASSIGN  TO  DISK  W-SALIDA3                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA4 ASSIGN  TO  DISK  W-SALIDA4                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA5 ASSIGN  TO  DISK  W-SALIDA5                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA6 ASSIGN  TO  DISK  W-SALIDA6                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA7 ASSIGN  TO  DISK  W-SALIDA7                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA8 ASSIGN  TO  DISK  W-SALIDA8                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
R7129D     SELECT SALIDA9 ASSIGN  TO  DISK  W-SALIDA9                   RMB0010
R7129D            ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129D            ACCESS MODE    IS  SEQUENTIAL.                        RMB0010
R7129D*                                                                 RMB0010
CQ3364     SELECT ULTIMO ASSIGN  TO  DISK  W-ULTIMO                     PJV0008
R7129A            ORGANIZATION   IS  RELATIVE                           RMB0010
R7129A            RELATIVE KEY IS LLAVE-ULTIMO                          RMB0010
R7129A            ACCESS MODE    IS  DYNAMIC       .                    RMB0010
R7129A*           FILE STATUS W-STATUS-ULT.                             RMB0010
R7129A*           ORGANIZATION   IS  LINE SEQUENTIAL                    RMB0010
R7129A*           ACCESS MODE    IS  SEQUENTIAL                         RMB0010
R7129A*           FILE STATUS W-STATUS-ULT.                             RMB0010
R7129C     SELECT SALIDA-00DEM ASSIGN TO DISK ARCHIVO-00DEM             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-01NUM ASSIGN TO DISK ARCHIVO-01NUM             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-01INQ ASSIGN TO DISK ARCHIVO-01INQ             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-01MOR ASSIGN TO DISK ARCHIVO-01MOR             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-01ACP ASSIGN TO DISK ARCHIVO-01ACP             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-01EXP ASSIGN TO DISK ARCHIVO-01EXP             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-01DEM ASSIGN TO DISK ARCHIVO-01DEM             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-01END ASSIGN TO DISK ARCHIVO-01END             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-02NUM ASSIGN TO DISK ARCHIVO-02NUM             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-02ACP ASSIGN TO DISK ARCHIVO-02ACP             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-02EXP ASSIGN TO DISK ARCHIVO-02EXP             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-02END ASSIGN TO DISK ARCHIVO-02END             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010
R7129C     SELECT SALIDA-02INQ ASSIGN TO DISK ARCHIVO-02INQ             RMB0010
R7129C            ORGANIZATION IS LINE SEQUENTIAL.                      RMB0010

MANT01     SELECT ARC-MANT00 ASSIGN  TO  DISK  W-MANT00                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT01 ASSIGN  TO  DISK  W-MANT01                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT02 ASSIGN  TO  DISK  W-MANT02                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT03 ASSIGN  TO  DISK  W-MANT03                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT04 ASSIGN  TO  DISK  W-MANT04                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT05 ASSIGN  TO  DISK  W-MANT05                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT06 ASSIGN  TO  DISK  W-MANT06                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT07 ASSIGN  TO  DISK  W-MANT07                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT08 ASSIGN  TO  DISK  W-MANT08                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002
MANT01     SELECT ARC-MANT09 ASSIGN  TO  DISK  W-MANT09                 RMB0002
MANT01            ORGANIZATION   IS  LINE SEQUENTIAL.                   RMB0002

       DATA DIVISION.
       FILE SECTION.
CARDEM COPY  BDLSTR.FD.                                                 GDT0007
CARDEM*                                                                 GDT0007
MANT01 FD ARC-MANT00.                                                   RMB0002
MANT01 01 REG-MANT00                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT01.                                                   RMB0002
MANT01 01 REG-MANT01                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT02.                                                   RMB0002
MANT01 01 REG-MANT02                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT03.                                                   RMB0002
MANT01 01 REG-MANT03                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT04.                                                   RMB0002
MANT01 01 REG-MANT04                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT05.                                                   RMB0002
MANT01 01 REG-MANT05                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT06.                                                   RMB0002
MANT01 01 REG-MANT06                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT07.                                                   RMB0002
MANT01 01 REG-MANT07                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT08.                                                   RMB0002
MANT01 01 REG-MANT08                      PIC X(2000).                  RMB0002
MANT01 FD ARC-MANT09.                                                   RMB0002
MANT01 01 REG-MANT09                      PIC X(2000).                  RMB0002
       COPY ICBDIR0001.FD.
       COPY BDACSV.FD.
       COPY NOAMNISTIA21.FD.
       COPY ICBSUS.FD.
       COPY ICBCHK.FD.
       COPY ICBCRE.FD.
       COPY ICBCON001.FD.
       COPY BDIIREGI.FD.
       COPY BDIIDIAN.FD.
       COPY BDIIDAS.FD.
       COPY BDIEMSUP.FD.
BDSCT8*COPY  BDSCORES.FD.                                               GDT0006
BDSCT8 COPY BDSCORES0001.FD.                                            GDT0006
QUAN01 COPY ICBCRE2.FD.                                                 RMB0002
QUAN01 COPY ICSALMIN.FD.                                                RMB0002
QUAN01 COPY ICSANATA.FD.                                                RMB0002
QUAN01 COPY ICMVLR.FD.                                                  RMB0002

       FD  ENTRADA.
       01  REG-ENTRADA                  PIC   X(200).

R7129D*FD  SALIDA.                                                      RMB0010
R7129D*01  REG-SALIDA                   PIC   X(200).                   RMB0010

R7129D FD  SALIDA1.                                                     RMB0010
R7129D 01  REG-SALIDA1                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA2.                                                     RMB0010
R7129D 01  REG-SALIDA2                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA3.                                                     RMB0010
R7129D 01  REG-SALIDA3                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA4.                                                     RMB0010
R7129D 01  REG-SALIDA4                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA5.                                                     RMB0010
R7129D 01  REG-SALIDA5                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA6.                                                     RMB0010
R7129D 01  REG-SALIDA6                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA7.                                                     RMB0010
R7129D 01  REG-SALIDA7                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA8.                                                     RMB0010
R7129D 01  REG-SALIDA8                  PIC   X(200).                   RMB0010
R7129D FD  SALIDA9.                                                     RMB0010
R7129D 01  REG-SALIDA9                  PIC   X(200).                   RMB0010
CQ3364 FD  ULTIMO.                                                      PJV0008
R7129A*01  REG-ULTIMO                   PIC   X(200).                   RMB0010
R7129A 01  REG-ULTIMO.                                                  RMB0010
R7129A     02 TIPIDE-ULTIMO             PIC 9.                          RMB0010
R7129A     02 NUMIDE-ULTIMO             PIC 9(11).                      RMB0010
R7129A     02 W-PREDICTA-ULTIMO         PIC 9(8).                       RMB0010
R7129A     02 W-ACIERTA-ULTIMO          PIC 9(8).                       RMB0010
R7129A     02 W-BASICO-ULTIMO           PIC 9(8).                       RMB0010
R7129A     02 W-ACIERTA-M-ULTIMO        PIC 9(8).                       RMB0010
R7129A     02 W-QUANTO-ULTIMO           PIC 9(8).                       RMB0010
R7129A     02 W-PREDICTA-HD-ULTIMO      PIC 9(8).                       RMB0010
R7129A     02 W-CARACTERISTICAS-ULTIMO  PIC 9(8).                       RMB0010
ACIEAA     02 W-ACIERTA-A-ULTIMO        PIC 9(8).                       STT0012
ACIEFI     02 W-ACIERTA-A-DFGE-ULTIMO   PIC 9(8).                       RMB0012
ACIEFI     02 W-ACIERTA-A-DTRO-ULTIMO   PIC 9(8).                       RMB0012
ACIEFI     02 W-ACIERTA-A-DINT-ULTIMO   PIC 9(8).                       RMB0012
ACIEFI     02 W-ACIERTA-A-DHVT-ULTIMO   PIC 9(8).                       RMB0012
ACIEFI     02 W-ACIERTA-A-DCOO-ULTIMO   PIC 9(8).                       RMB0012
R15689     02 W-MICROCREDITO-S-ULTIMO   PIC 9(8).                       STT0014
R7129C FD  SALIDA-01NUM.                                                RMB0010
R7129C 01  REG-01NUM-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-01INQ.                                                RMB0010
R7129C 01  REG-01INQ-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-01MOR.                                                RMB0010
R7129C 01  REG-01MOR-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-01ACP.                                                RMB0010
R7129C 01  REG-01ACP-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-01EXP.                                                RMB0010
R7129C 01  REG-01EXP-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-00DEM.                                                RMB0010
R7129C 01  REG-00DEM-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-01DEM.                                                RMB0010
R7129C 01  REG-01DEM-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-01END.                                                RMB0010
R7129C 01  REG-01END-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-02NUM.                                                RMB0010
R7129C 01  REG-02NUM-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-02ACP.                                                RMB0010
R7129C 01  REG-02ACP-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-02EXP.                                                RMB0010
R7129C 01  REG-02EXP-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-02END.                                                RMB0010
R7129C 01  REG-02END-SALIDA             PIC X(2000).                    RMB0010
R7129C FD  SALIDA-02INQ.                                                RMB0010
R7129C 01  REG-02INQ-SALIDA             PIC X(2000).                    RMB0010

      *------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *------------------------------------------------------------*
       COPY BDSCORES.WS.
       COPY ICFILE.WS.
       COPY BDIIVIDE.WS.
       COPY SCOT16.WS.
       COPY VALVLR.WS.
       COPY UTLBIT.WS.
       COPY LEEMAESTROS.WS.
QUAN01 COPY CARACT.WS.                                                  RMB0002
QUAN01 COPY SCOUSA.WS.                                                  RMB0002
QUAN01 COPY LEEICMVLR.WS.                                               RMB0002
      *------------------------------------------------------------*
R7129D 01  W-AREA-PRODUCTO               PIC X(200) VALUE SPACES.       RMB0010
R15689*01  W-PRODUCTO                    PIC 9(2) VALUE 0.              STT0014
R15689 01  W-PRODUCTO                    PIC 9(3) VALUE 0.              STT0014
R7129A 01  LLAVE-ULTIMO                  PIC 9(5) VALUE 1.              RMB0010
R7129A 01  SW-CAR-OPEN                   PIC 9(1) VALUE 0.              RMB0010
R7129C 01  W-CAR-NOMBRE                  PIC X(80) VALUE SPACES.        RMB0010
R7129C 01  REG1-CAR.                                                    RMB0010
R7129C     02 FIJO-CAR                   PIC X(5).                      RMB0010
R7129C     02 FECHA-CAR                  PIC 9(6).                      RMB0010
R7129C     02 BASURA-CAR                 PIC X(20).                     RMB0010
R7129C 01  REG2-CAR.                                                    RMB0010
R7129C     02 TIPIDE-CAR                 PIC 9(1).                      RMB0010
R7129C     02 NUMIDE-CAR                 PIC 9(11).                     RMB0010
R7129C     02 TEXT-CAR                   PIC X(2000).                   RMB0010
R7129C     02 FILLER REDEFINES TEXT-CAR.                                RMB0010
R7129C        03 PORTAF-CAR                 PIC X(02).                  RMB0010
R7129C        03 PTEXT-CAR                  PIC X(1998).                RMB0010
R7129C 01  REG-02END-AUX.                                               RMB0010
R7129C     02 TO-02END-AUX.                                             RMB0010
R7129C        03 FILLER OCCURS 23 TIMES PIC X(4).                       RMB0010
R7129C     02 XX-02END-AUX.                                             RMB0010
R7129C        03 FILLER OCCURS 18 TIMES PIC X(6).                       RMB0010
R7129C     02 MX-02END-AUX.                                             RMB0010
R7129C        03 FILLER OCCURS 09 TIMES PIC X(6).                       RMB0010
       01  W-AREA-TRABAJO.
           03 W-TIPO-ID-ENT              PIC 9(01) VALUE ZEROS.
           03 W-NUME-ID-ENT              PIC 9(11) VALUE ZEROS.
           03 W-SCORE                    PIC S9(05).
           03 W-RAZON1                   PIC 9(02).
           03 W-RAZON2                   PIC 9(02).
           03 W-RAZON3                   PIC 9(02).
           03 W-RAZON4                   PIC 9(02).
QUAN01     03 W-SCORE-CARD               PIC 9(02).                     RMB0002
QUAN01 01 FECHA-SISTEMA.                                                RMB0002
QUAN01    03 FECHA-SIST-YYYY                PIC 9(04).                  RMB0002
QUAN01    03 FECHA-SIST-MM                  PIC 9(02).                  RMB0002
QUAN01    03 FECHA-SIST-DD                  PIC 9(02).                  RMB0002
MANT01 01  AREA-MANTENIMIENTO            PIC X(2000).                   RMB0002
MANT01 01  SCORE-CARD-AUX                PIC 9(02).                     RMB0002
MICR01 01  HAY-COMBO-MICRO               PIC 9(01).
       01  I                             PIC 9(03).
       01  J                             PIC 9(03).
       01  K                             PIC 9(04).
       01  W-STATUS                      PIC X(02).
CQ3364 01  W-STATUS-START                PIC X(02) VALUE ZEROS.         PJV0008
CQ3364 01  W-STATUS-SAL                  PIC X(02).                     PJV0008
CQ3364 01  W-STATUS-ULT                  PIC X(02).                     PJV0008
       01  NUM-ID                        PIC 9(11).
       01  W-FIN-LECTURA                 PIC 9(01) VALUE ZEROS.
       01  W-LEIDOS                      PIC 9(07) VALUE ZEROS.
R7129C 01  W-CARACTERISTICAS             PIC 9(07) VALUE ZEROS.         RMB0010
       01  W-PREDICTA                    PIC 9(07) VALUE ZEROS.
       01  W-ACIERTA                     PIC 9(07) VALUE ZEROS.
QUAN01 01  W-ACIERTA-M                   PIC 9(07) VALUE ZEROS.         RMB0002
       01  W-PREDICTA-HD                 PIC 9(07) VALUE ZEROS.
ACIEAA 01  W-ACIERTA-A                   PIC 9(07) VALUE ZEROS.         STT0012
ACIEFI 01  W-ACIERTA-A-DFGE              PIC 9(07) VALUE ZEROS.         RMB0012
ACIEFI 01  W-ACIERTA-A-DTRO              PIC 9(07) VALUE ZEROS.         RMB0012
ACIEFI 01  W-ACIERTA-A-DINT              PIC 9(07) VALUE ZEROS.         RMB0012
ACIEFI 01  W-ACIERTA-A-DHVT              PIC 9(07) VALUE ZEROS.         RMB0012
ACIEFI 01  W-ACIERTA-A-DCOO              PIC 9(07) VALUE ZEROS.         RMB0012
R15689 01  W-MICROCREDITO-S              PIC 9(07) VALUE ZEROS.         STT0014
PAM379 01  W-QUANTO                      PIC 9(07) VALUE ZEROS.
       01  W-BASICO                      PIC 9(07) VALUE ZEROS.
       01  W-FECHA-HOY                   PIC 9(06) VALUE ZEROS.
       01  W-ANO-HOY                     PIC 9(04) VALUE ZEROS.
       01  W-FECHA-HIS.
           03 W-ANO-HIS                  PIC 9(04) VALUE ZEROS.
           03 W-MES-HIS                  PIC 9(02) VALUE ZEROS.
       01  W-FECHA-END.
           03 W-FEC-END                  PIC 9(06) VALUE ZEROS.
           03 W-DIA-END                  PIC 9(02) VALUE ZEROS.

CQ3364 01  FIN-ULTIMO                    PIC 9(01) VALUE 0.             PJV0008
MANT01 01  W-OPEN-MANT00                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT01                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT02                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT03                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT04                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT05                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT06                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT07                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT08                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-OPEN-MANT09                 PIC 9(01) VALUE 0.             RMB0002
MANT01 01  W-MANT00.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT00      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT01.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT01      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT02.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT02      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT03.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT03      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT04.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT04      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT05.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT05      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT06.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT06      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT07.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT07      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT08.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT08      PIC X(40) VALUE SPACES.              RMB0002
MANT01 01  W-MANT09.                                                    RMB0002
MANT01     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0002
MANT01     03 W-NOMBRE-MANT09      PIC X(40) VALUE SPACES.              RMB0002
      *------------------------------------------------------------*
       01  W-ENTRADA.
           03 W-ENT-DIRECT         PIC X(12) VALUE "$TEMPORALES/".
R7129B*    03 W-ENT-NOMBRE         PIC X(20) VALUE SPACES.              RMB0010
R7129B     03 W-ENT-NOMBRE         PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA1.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE1        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA2.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE2        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA3.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE3        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA4.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE4        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA5.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE5        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA6.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE6        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA7.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE7        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA8.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE8        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA9.                                                   RMB0010
R7129D     03 FILLER               PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE9        PIC X(80) VALUE SPACES.              RMB0010
R7129D 01  W-SALIDA.                                                    RMB0010
R7129D     03 W-SLD-DIRECT         PIC X(12) VALUE "$TEMPORALES/".      RMB0010
R7129D     03 W-SLD-NOMBRE         PIC X(80) VALUE SPACES.              RMB0010
CQ3364 01  W-ULTIMO.                                                    PJV0008
CQ3364     03 W-ULT-DIRECT         PIC X(12) VALUE "$TEMPORALES/".      PJV0008
R7129B*    03 W-ULT-NOMBRE         PIC X(40) VALUE "SCOREPRECS.ULTIMO". RMB0010
R7129B     03 W-ULT-NOMBRE         PIC X(80) VALUE SPACES.              RMB0010
R7129C     03  ARCHIVO-01NUM.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-01NUM  PIC X(10) VALUE "CAR-01NUM-".              RMB0010
R7129C       05  FECH-01NUM  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-01NUM        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-01INQ.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-01INQ  PIC X(10) VALUE "CAR-01INQ-".              RMB0010
R7129C       05  FECH-01INQ  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-01INQ        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-01MOR.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-01MOR  PIC X(10) VALUE "CAR-01MOR-".              RMB0010
R7129C       05  FECH-01MOR  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-01MOR        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-01ACP.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-01ACP  PIC X(10) VALUE "CAR-01ACP-".              RMB0010
R7129C       05  FECH-01ACP  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-01ACP        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-01EXP.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-01EXP  PIC X(10) VALUE "CAR-01EXP-".              RMB0010
R7129C       05  FECH-01EXP  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-01EXP        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-00DEM.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-00DEM  PIC X(10) VALUE "CAR-00DEM-".              RMB0010
R7129C       05  FECH-00DEM  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-00DEM        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-01DEM.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-01DEM  PIC X(10) VALUE "CAR-01DEM-".              RMB0010
R7129C       05  FECH-01DEM  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-01DEM        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-01END.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-01END  PIC X(10) VALUE "CAR-01END-".              RMB0010
R7129C       05  FECH-01END  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-01END        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-02NUM.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-02NUM  PIC X(10) VALUE "CAR-02NUM-".              RMB0010
R7129C       05  FECH-02NUM  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-02NUM        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-02ACP.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-02ACP  PIC X(10) VALUE "CAR-02ACP-".              RMB0010
R7129C       05  FECH-02ACP  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-02ACP        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-02EXP.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-02EXP  PIC X(10) VALUE "CAR-02EXP-".              RMB0010
R7129C       05  FECH-02EXP  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-02EXP        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-02END.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-02END  PIC X(10) VALUE "CAR-02END-".              RMB0010
R7129C       05  FECH-02END  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-02END        PIC  X(100)   VALUE SPACES.      RMB0010
R7129C     03  ARCHIVO-02INQ.                                           RMB0010
R7129C       05  FILLER      PIC X(12) VALUE "$TEMPORALES/".            RMB0010
R7129C       05  ARCH-02INQ  PIC X(10) VALUE "CAR-02INQ-".              RMB0010
R7129C       05  FECH-02INQ  PIC 9(06) VALUE ZEROS.                     RMB0010
R7129C       05                        PIC  X(01)    VALUE "-".         RMB0010
R7129C       05  NOM-ARCH-02INQ        PIC  X(100)   VALUE SPACES.      RMB0010

      *------------------------------------------------------------*
       01  ENT-BASE-IDE.
           03 ENT-TIPO-ID               PIC 9(01).
           03 ENT-NUME-ID               PIC 9(11).
R15689     03 ENT-ACTIVOS-ID            PIC 9(15).                      STT0014
      *------------------------------------------------------------*
       01  ENT-SCORE-VALID.
           03 TIP-ID-VAL                PIC 9(01).
           03 NUM-ID-VAL                PIC 9(11).
           03 NOMBRE-VAL                PIC X(45).
           03 FEC-EXP-VAL               PIC 9(08).
           03 MUN-EXP-VAL               PIC X(15).
           03 DEP-EXP-VAL               PIC X(15).
           03 VIG-VAL                   PIC X(02).
           03 GENERO-VAL                PIC X(01).
           03 EDAD-VAL                  PIC X(03).
           03 EDAD-RANGO-VAL            PIC X(05).
           03 ADICIONAL-VAL             PIC X(51).
R15689     03 ACTIVOS-VAL REDEFINES ADICIONAL-VAL    PIC 9(15).         STT0014
      *------------------------------------------------------------*
CQ6539 01  ENT-SCORE-BLQ.                                               STT0011
CQ6539     03 TIP-ID-BLQ                PIC 9(01).                      STT0011
CQ6539     03 NUM-ID-BLQ                PIC 9(11).                      STT0011
CQ6539     03 NOMBRE-BLQ                PIC X(60).                      STT0011
CQ6539     03 NOMBRE-WORK REDEFINES NOMBRE-BLQ.                         STT0011
CQ6539        05 PR-APELLIDO-WK            PIC X(15).                   STT0011
CQ6539        05 SG-APELLIDO-WK            PIC X(15).                   STT0011
CQ6539        05 PR-NOMBRE-WK              PIC X(15).                   STT0011
CQ6539        05 SG-NOMBRE-WK              PIC X(15).                   STT0011
CQ6539     03 FEC-EXP-BLQ               PIC 9(08).                      STT0011
CQ6539     03 MUN-EXP-BLQ               PIC X(15).                      STT0011
CQ6539     03 DEP-EXP-BLQ               PIC X(15).                      STT0011
CQ6539     03 VIG-BLQ                   PIC X(02).                      STT0011
CQ6539     03 GENERO-BLQ                PIC X(01).                      STT0011
CQ6539     03 EDAD-BLQ                  PIC X(03).                      STT0011
CQ6539     03 EDAD-RANGO-BLQ            PIC X(05).                      STT0011
CQ6539     03 ADICIONAL-BLQ             PIC X(51).                      STT0011
R15689     03 ACTIVOS-BLQ REDEFINES ADICIONAL-BLQ    PIC 9(15).         STT0014
      *------------------------------------------------------------*
       01  ENT-SCORE-AWK.
           03 TIP-ID-AWK                PIC 9(01).
           03 FILLER-01                 PIC X(01).
           03 NUM-ID-AWK                PIC 9(11).
           03 FILLER-02                 PIC X(01).
           03 NOMBRE-AWK                PIC X(45).
           03 FILLER-03                 PIC X(01).
           03 CALIFI-AWK                PIC X(02).
R15689     03 ACTIVOS-AWK               PIC 9(15).                      STT0014
      *------------------------------------------------------------*
       01  ENT-SCORE-NVAL.
           03 TIPO-ID-NVAL              PIC 9(01).
           03 NUME-ID-NVAL              PIC 9(11).
           03 NOMBRE-NVAL               PIC X(45).
R15689*    03 FILLER                    PIC 9(06).                      STT0014
R15689     03 ACTIVOS-NVAL              PIC 9(15).                      STT0014
R15689*    03 FILLER EDEFINES ACTIVOS-NVAL PIC 9(06).                   STT0014
      *------------------------------------------------------------*
      *    REGISTROS DE ENTRADA CALCULO DE SCORE BATCH   NUEVO
      *------------------------------------------------------------*
       01  SLD-BASE-IDE.     *> IDE
           03 SLD-TIPO-ID               PIC 9(01).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-NUME-ID               PIC 9(11).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-VAL-SCORE             PIC 9(05).
QUAN01     03 FILLER                    PIC X(01) VALUE ";".            RMB0002
QUAN01     03 SLD-SCORE-CARD            PIC 9(02).                      RMB0002

      *------------------------------------------------------------*
      *    REGISTROS DE ENTRADA CALCULO DE SCORE BATCH (CTL103-AVV)
      *------------------------------------------------------------*
       01  SLD-SCORE-VALID.  *> VAL
           03 TIP-ID-SLD                PIC 9(01).
           03 FILLER                    PIC X(01) VALUE ";".
           03 NUM-ID-SLD                PIC 9(11).
           03 FILLER                    PIC X(01) VALUE ";".
           03 NOMBRE-SLD                PIC X(45).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SCORE-SLD                 PIC S9(05).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON1-SLD                PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON2-SLD                PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON3-SLD                PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON4-SLD                PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-FEC-EXP-VAL           PIC 9(08).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-MUN-EXP-VAL           PIC X(15).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-DEP-EXP-VAL           PIC X(15).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-VIG-VAL               PIC X(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-GENERO-VAL            PIC X(01).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-EDAD-APROX            PIC 9(03).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-RANGO-EDAD            PIC X(05).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-RELLENO               PIC X(51).
CQ6539 01  SLD-SCORE-BLQ.    *> BLQ                                     STT0011
CQ6539     03 TIP-ID-SBLQ               PIC 9(01).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 NUM-ID-SBLQ               PIC 9(11).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6530     03 PR-APELLIDO-SLD           PIC X(15).
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6530     03 SG-APELLIDO-SLD           PIC X(15).
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6530     03 PR-NOMBRE-SLD             PIC X(15).
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6530     03 SG-NOMBRE-SLD             PIC X(15).
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SCORE-SBLQ                PIC S9(05).                     STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 RAZON1-SBLQ               PIC 9(02).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 RAZON2-SBLQ               PIC 9(02).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 RAZON3-SBLQ               PIC 9(02).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 RAZON4-SBLQ               PIC 9(02).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-FEC-EXP-SBLQ          PIC 9(08).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-MUN-EXP-SBLQ          PIC X(15).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-DEP-EXP-SBLQ          PIC X(15).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-VIG-SBLQ              PIC X(02).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-GENERO-SBLQ           PIC X(01).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-EDAD-APROX-B          PIC 9(03).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-RANGO-EDAD-B          PIC X(05).                      STT0011
CQ6539     03 FILLER                    PIC X(01) VALUE ";".            STT0011
CQ6539     03 SLD-RELLENO-B             PIC X(51).                      STT0011
      *------------------------------------------------------------*
      *    REGISTROS DE ENTRADA CALCULO DE SCORE BATCH (SCOTBATC)
      *------------------------------------------------------------*
       01  SLD-SCORE-AWK.  *> AWK
           03 SLD-TIP-ID-AWK            PIC 9(01).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-NUM-ID-AWK            PIC 9(11).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-NOMBRE-AWK            PIC X(45).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SLD-CALIFI-AWK            PIC X(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SCORE-AWK                 PIC S9(05).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON1-AWK                PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON2-AWK                PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON3-AWK                PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON4-AWK                PIC 9(02).
      *------------------------------------------------------------*
      *    REGISTROS DE ENTRADA CALCULO DE SCORE BATCH (CTLACIERTA)
      *------------------------------------------------------------*
       01  SLD-SCORE-NVAL.  *> PUN
           03 NVAL-TIPO-ID              PIC 9(01).
           03 FILLER                    PIC X(01) VALUE ";".
           03 NVAL-NUME-ID              PIC 9(11).
           03 FILLER                    PIC X(01) VALUE ";".
           03 NVAL-NOMBRE               PIC X(45).
           03 FILLER                    PIC X(01) VALUE ";".
           03 NVAL-PUNTAJE              PIC 9(05).

      *------------------------------------------------------------*
      *    REGISTROS DE ENTRADA CALCULO DE SCORE BATCH (CTLSCORE)
      *------------------------------------------------------------*
       01  PLA-SCORE-VALID.  *> PLA
           03 PLA-TIP-ID                PIC 9(01).
           03 PLA-NUM-ID                PIC 9(11).
           03 PLA-NOMBRE                PIC X(45).
           03 PLA-FEC-EXP-VAL           PIC 9(08).
           03 PLA-MUN-EXP-VAL           PIC X(15).
           03 PLA-DEP-EXP-VAL           PIC X(15).
           03 PLA-VIG-VAL               PIC X(02).
           03 PLA-GENERO-VAL            PIC X(01).
           03 PLA-EDAD-APROX            PIC 9(03).
           03 PLA-RANGO-EDAD            PIC X(05).
           03 PLA-FILLER-SCR            PIC X(01).
           03 PLA-SCORE                 PIC 9(05).
           03 PLA-RAZON1                PIC 9(02).
           03 PLA-RAZON2                PIC 9(02).
           03 PLA-RAZON3                PIC 9(02).
           03 PLA-RAZON4                PIC 9(02).
           03 PLA-DATOS                 PIC X(51).

CQ3364 01  ULT-SCORE-VALID.  *> ULT                                     PJV0008
CQ3364     03 ULT-TIP-ID                PIC 9(01).                      PJV0008
CQ3364     03 ULT-NUM-ID                PIC 9(11).                      PJV0008
CQ3364     03 ULT-NOMBRE                PIC X(45).                      PJV0008
CQ3364     03 ULT-FEC-EXP-VAL           PIC 9(08).                      PJV0008
CQ3364     03 ULT-MUN-EXP-VAL           PIC X(15).                      PJV0008
CQ3364     03 ULT-DEP-EXP-VAL           PIC X(15).                      PJV0008
CQ3364     03 ULT-VIG-VAL               PIC X(02).                      PJV0008
CQ3364     03 ULT-GENERO-VAL            PIC X(01).                      PJV0008
CQ3364     03 ULT-EDAD-APROX            PIC 9(03).                      PJV0008
CQ3364     03 ULT-RANGO-EDAD            PIC X(05).                      PJV0008
CQ3364     03 ULT-FILLER-SCR            PIC X(01).                      PJV0008
CQ3364     03 ULT-SCORE                 PIC 9(05).                      PJV0008
CQ3364     03 ULT-RAZON1                PIC 9(02).                      PJV0008
CQ3364     03 ULT-RAZON2                PIC 9(02).                      PJV0008
CQ3364     03 ULT-RAZON3                PIC 9(02).                      PJV0008
CQ3364     03 ULT-RAZON4                PIC 9(02).                      PJV0008
CQ3364     03 ULT-DATOS                 PIC X(51).                      PJV0008

      *------------------------------------------------------------*
      *    REGISTROS DE ENTRADA CALCULO DE SCORE BATCH (CTL103)
      *------------------------------------------------------------*
       01  EDA-SCORE-VALID.  *> EDA
           03 TIP-ID-EDAD               PIC 9(01).
           03 FILLER                    PIC X(01) VALUE ";".
           03 NUM-ID-EDAD               PIC 9(11).
           03 FILLER                    PIC X(01) VALUE ";".
           03 NOMBRE-EDAD               PIC X(45).
           03 FILLER                    PIC X(01) VALUE ";".
           03 SCORE-EDAD                PIC S9(05).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON1-EDAD               PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON2-EDAD               PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON3-EDAD               PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RAZON4-EDAD               PIC 9(02).
           03 FILLER                    PIC X(01) VALUE ";".
           03 APROXIM-EDAD              PIC 9(03).
           03 FILLER                    PIC X(01) VALUE ";".
           03 RANGO-APROX-EDAD          PIC X(05).
      *------------------------------------------------------------*
      *    REGISTROS DE SALIDA CALCULO DE SCORE QUANTO
      *------------------------------------------------------------*
QUAN01 01  SLD-SCORE-QUANTO.                                            RMB0002
QUAN01     03 SLD-TIPO-ID-QUANTO        PIC 9(01).                      RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-NRO-ID-QUANTO         PIC 9(11).                      RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-MIN-MIL-QUANTO        PIC 9(09)9.                     RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-PROM-MIL-QUANTO       PIC 9(09)9.                     RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-MAX-MIL-QUANTO        PIC 9(09)9.                     RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-MIN-SMLV-QUANTO       PIC 9(2).9(5).                  RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-PROM-SMLV-QUANTO      PIC 9(2).9(5).                  RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-MAX-SMLV-QUANTO       PIC 9(2).9(5).                  RMB0002
QUAN01     03 FILLER                    PIC X(01).                      RMB0002
QUAN01     03 SLD-RAZON-QUANTO          PIC 9(04).                      RMB0002
      *----------------------------------------------------------------*
       01  PARAMETROS-SHELL             PIC X(94) VALUE SPACES.
      *----------------------------------------------------------------*
       01  OPCIONES-SHELL.
           03 W-FECHA-BASE              PIC 9(06) VALUE ZEROS.
R7129D*    03 W-TIPO-SCORE              PIC 9(02) VALUE ZEROS.          RMB0010
R15689*    03 W-TIPO-SCORE              PIC 9(18) VALUE ZEROS.          STT0014
R15689     03 W-TIPO-SCORE              PIC X(27) VALUE ZEROS.          STT0014
R7129D     03 FILLER REDEFINES W-TIPO-SCORE.                            RMB0010
R15689*       04 W-SCORE-COMBO OCCURS 9 TIMES PIC 9(02).                STT0014
R15689        04 W-SCORE-COMBO OCCURS 9 TIMES PIC 9(03).                STT0014
R15689     03 W-CAMPO-40                PIC X(40) VALUE SPACES.
R15689     03 LONG-40                   PIC 9(02).
R15689     03 W-POS-40                  PIC 9(02).
R15689     03 W-POS-TS                  PIC 9(02).
           03 W-TIPO-ARCHIVO            PIC X(03) VALUE SPACES.
R7129B*    03 W-ARCHIVO-ENT             PIC X(20) VALUE SPACES.         RMB0010
R7129B     03 W-ARCHIVO-ENT             PIC X(80) VALUE SPACES.         RMB0010
MANT01     03 W-ARCHIVO-MANT            PIC X(20) VALUE SPACES.         RMB0002
ACIEAA     03 W-NITSUS                  PIC 9(11) VALUE ZEROS.          RMB0012
R15689     03 W-ACTIVOS                 PIC 9(15) VALUE ZEROS.          STT0014
CQ3364     03 W-REINICIO                PIC X(02) VALUE SPACES.         PJV0008
R7129A     03 W-FORZARINICIAR           PIC X(02) VALUE SPACES.         RMB0010
       01  SW-BDIIREGI                  PIC 9 VALUE 0.
QUAN01 01 CR-FILL                     PIC X VALUE x"00".                RMB0002
QUAN01 01 COMANDO-SYST.                                                 RMB0002
QUAN01    03 W-COMANDO-SYST           PIC X(100) VALUE SPACES.          RMB0002
QUAN01 01  ENV-VALUE                  PIC X(100).                       RMB0002
QUAN01 01  ENV-NAME                   PIC X(05)  VALUE "EXTFH".         RMB0002
      ******************************************************************
PAM456$SET CONSTANT HACERMATCH"SI"
QUAN01$SET CONSTANT AMNISTIA21"NO"
      ******************************************************************

      *------------------------------------------------------------*
       PROCEDURE DIVISION.
      *------------------------------------------------------------*
       0000-PROCESO-SCORE-BATCH.
            PERFORM  200710-VALIDAR-FECHA-INI-AMNI

            PERFORM  0010-ABRIR-ARCHIVOS.

            PERFORM  0100-PROCESO-PRINCIPAL.

            PERFORM  1000-CERRAR-ARCHIVOS.

            STOP RUN.

       0010-ABRIR-ARCHIVOS.
      *****************************************************************
      *0010-ABRIR-ARCHIVOS                                            *
      * Rutina que captura el nombre de archivo a ser procesado,      *
      * verifica su existencia, y de no presentarse ningun error abre *
      * el resto de archivos necesarios para realizar el proceso.     *
      * Para el archivo de salida dejara el mismo nombre del archivo  *
      * de entrada junto con la extencion ".PREDICTA."                *
      *****************************************************************
           DISPLAY "  *-----------------------------------------*"
           DISPLAY "  |     Parametros  scoring batch           |"
           DISPLAY "  *-----------------------------------------*"
           DISPLAY "                                             "
           DISPLAY "Programa: SCOTBATCH         Version:Nov/2012 "
           DISPLAY "                                             "
           ACCEPT   PARAMETROS-SHELL   FROM COMMAND-LINE
           UNSTRING PARAMETROS-SHELL DELIMITED BY SPACES INTO
                    W-FECHA-BASE,
                    W-TIPO-SCORE,
                    W-TIPO-ARCHIVO,
                    W-ARCHIVO-ENT,
                    W-ARCHIVO-MANT,
ACIEAA              W-NITSUS,                                           STT0012
CQ3364*             W-REINICIO                                          PJV0008
R7129A              W-FORZARINICIAR                                     RMB0010
           END-UNSTRING
R7129A     IF W-FORZARINICIAR = "SI"                                    RMB0010
R7129A        MOVE "NO" TO W-REINICIO    *> OBLIGA A NO TOMAR ARCHINI   RMB0010
R7129A     ELSE                                                         RMB0010
R7129A        MOVE "SI" TO W-REINICIO    *> SIEMPRE TRATA DE REINICIAR  RMB0010
R7129A     END-IF                                                       RMB0010
R15689     PERFORM 100-ANALIZAR-TIPOSCORE
R15689*    IF W-TIPO-SCORE > 99                                         STT0014
R15689     IF W-TIPO-SCORE(4:) <> ZEROS AND W-TIPO-SCORE(4:) <> SPACES  STT0014
R7129D        PERFORM 100-VALIDAR-COMBO                                 RMB0010
R7129D     END-IF.                                                      RMB0010
R15869*    IF W-ARCHIVO-MANT = "MANTENIMIENTO" AND W-TIPO-SCORE > 99    RMB0010
R15869     IF W-ARCHIVO-MANT = "MANTENIMIENTO" AND
R15869                          W-TIPO-SCORE(4:) <> ZEROS
R7129D        DISPLAY "ERROR,EN COMBO NO PUEDE HABER MANTENIMIENTO"     RMB0010
R7129D        DISPLAY "COMBO=" W-TIPO-SCORE                             RMB0010
R7129D        STOP RUN                                                  RMB0010
R7129D     END-IF                                                       RMB0010
           DISPLAY "Fecha de calculo: " W-FECHA-BASE
CQ3364     DISPLAY "    W-TIPO-SCORE: " W-TIPO-SCORE                    PJV0008
CQ3364     DISPLAY "  W-TIPO-ARCHIVO: " W-TIPO-ARCHIVO                  PJV0008
CQ3364     DISPLAY "         Archivo: " W-ARCHIVO-ENT                   PJV0008
CQ3364     DISPLAY "  W-ARCHIVO-MANT: " W-ARCHIVO-MANT                  PJV0008
CQ3364     DISPLAY "        Reinicio: " W-REINICIO                      PJV0008
           DISPLAY "                                             "
R15869*    IF W-TIPO-SCORE > 10                                         RMB0002
R15869     IF W-TIPO-SCORE(1:3) > "010"                                 RMB0002
QUAN01        PERFORM 1000-VALIDAR-FECHAS                               RMB0002
QUAN01     END-IF.                                                      RMB0002
R7129D     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9                    RMB0010
R7129D       IF W-SCORE-COMBO(I) > 0                                    RMB0010
R7129D          EVALUATE W-SCORE-COMBO(I)                               RMB0010
                    WHEN = 1
                           DISPLAY "Tipo score: PREDICTA   "
                    WHEN = 4
                           DISPLAY "Tipo score: ACIERTA    "
                    WHEN = 6
                           DISPLAY "Tipo score: BASICO     "
                    WHEN = 8
                           DISPLAY "Tipo score: ACIERTA-M  "
QUAN01              WHEN = 62                                           RMB0002
QUAN01                     DISPLAY "Tipo score: QUANTO     "            RMB0002
PRHD01              WHEN = 67                                           RMB0002
PRHD01                     DISPLAY "Tipo score: ACIERTA+   "            RMB0002
ACIEAA              WHEN = 49                                           STT0012
ACIEAA                     DISPLAY "Tipo score: ACIERTA-A  "            STT0012
ACIEFI              WHEN = 47                                           RMB0012
ACIEFI                     DISPLAY "Tipo score: ACIE-A-DFGE"            RMB0012
ACIEFI              WHEN = 48                                           RMB0012
ACIEFI                     DISPLAY "Tipo score: ACIE-A-DTRO"            RMB0012
ACIEFI              WHEN = 95                                           RMB0012
ACIEFI                     DISPLAY "Tipo score: ACIE-A-DINT"            RMB0012
ACIEFI              WHEN = 41                                           RMB0012
ACIEFI                     DISPLAY "Tipo score: ACIE-A-DHVT"            RMB0012
ACIEFI              WHEN = 45                                           RMB0012
ACIEFI                     DISPLAY "Tipo score: ACIE-A-DCOO"            RMB0012
R7129D              WHEN = 99                                           RMB0010
R7129D                     DISPLAY "Tipo score: CARACTERISTICAS"        RMB0010
R15689              WHEN = 119                                          STT0014
R15689                     DISPLAY "Tipo score: MIC-BURO"               STT0014
R15689              WHEN = 120                                          STT0014
R15689                     DISPLAY "Tipo score: MIC-EXT"                STT0014
                    WHEN ANY
                           DISPLAY "Tipo score: INCORRECTO "
                           STOP RUN
R7129D          END-EVALUATE                                            RMB0010
R7129D       END-IF                                                     RMB0010
R7129D     END-PERFORM.                                                 RMB0010
           DISPLAY "                                             "
           EVALUATE W-TIPO-ARCHIVO
                    WHEN = "AWK"
                           DISPLAY "Score formato awk        "
                    WHEN = "VAL"
                           DISPLAY "Score formato validado   "
CQ6539              WHEN = "BLQ"                                        STT0011
CQ6539                     DISPLAY "Score formato validado bloques  "   STT0011
                    WHEN = "PUN"
                           DISPLAY "Score formato puntaje    "
                    WHEN = "PLA"
                           DISPLAY "Score formato plano      "
                    WHEN = "EDA"
                           DISPLAY "Score formato solo edad  "
                    WHEN = "IDE"
                           DISPLAY "Score formato identificacion"
                    WHEN OTHER
                           DISPLAY "Formato archivo ERRADO   "
                           STOP RUN
           END-EVALUATE
           DISPLAY "                                             "
           MOVE  FUNCTION CURRENT-DATE(1:6)  TO  W-FECHA-HOY
           MOVE    W-ARCHIVO-ENT             TO  W-ENT-NOMBRE
           OPEN INPUT ENTRADA
           IF W-STATUS = ZEROS
R7129A        STRING W-ENT-NOMBRE DELIMITED BY ".",                     RMB0010
R7129A               ".ULTIMO"    DELIMITED BY SIZE                     RMB0010
R7129A                            INTO W-ULT-NOMBRE                     RMB0010
R7129A        END-STRING                                                RMB0010
R7129D        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9                 RMB0010
R7129D          IF W-SCORE-COMBO(I) > 0                                 RMB0010
R7129D             EVALUATE W-SCORE-COMBO(I)                            RMB0010
                       WHEN = 1
                          STRING W-ENT-NOMBRE DELIMITED BY ".",
                                 ".PREDICTA"  DELIMITED BY SIZE
                                                  INTO W-SLD-NOMBRE
                          END-STRING
                      WHEN = 4
                          STRING W-ENT-NOMBRE DELIMITED BY ".",
                                  ".ACIERTA"   DELIMITED BY SIZE
                                                  INTO W-SLD-NOMBRE
                          END-STRING
                      WHEN = 6
                          STRING W-ENT-NOMBRE DELIMITED BY ".",
                                 ".BASICO "   DELIMITED BY SIZE
                                                 INTO W-SLD-NOMBRE
                          END-STRING
                      WHEN = 8
                          STRING W-ENT-NOMBRE DELIMITED BY ".",
                                 ".ACI.EME"   DELIMITED BY SIZE
                                                INTO W-SLD-NOMBRE
                          END-STRING
QUAN01                WHEN = 62                                         RMB0002
QUAN01                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0002
QUAN01                           ".QUANTO"   DELIMITED BY SIZE          RMB0002
QUAN01                                         INTO W-SLD-NOMBRE        RMB0002
QUAN01                    END-STRING                                    RMB0002
PRHD01                WHEN = 67                                         RMB0002
PRHD01                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0002
PRHD01                           ".PREDHD"   DELIMITED BY SIZE          RMB0002
PRHD01                                         INTO W-SLD-NOMBRE        RMB0002
PRHD01                    END-STRING                                    RMB0002
ACIEAA                WHEN = 49                                         STT0012
ACIEAA                    STRING W-ENT-NOMBRE DELIMITED BY ".",         STT0012
ACIEAA                           ".ACIEAA"   DELIMITED BY SIZE          STT0012
ACIEAA                                         INTO W-SLD-NOMBRE        STT0012
ACIEAA                    END-STRING                                    STT0012
ACIEFI                WHEN = 47                                         RMB0012
ACIEFI                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0012
ACIEFI                           ".AADFGE"   DELIMITED BY SIZE          RMB0012
ACIEFI                                         INTO W-SLD-NOMBRE        RMB0012
ACIEFI                    END-STRING                                    RMB0012
ACIEFI                WHEN = 48                                         RMB0012
ACIEFI                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0012
ACIEFI                           ".AADTRO"   DELIMITED BY SIZE          RMB0012
ACIEFI                                         INTO W-SLD-NOMBRE        RMB0012
ACIEFI                    END-STRING                                    RMB0012
ACIEFI                WHEN = 95                                         RMB0012
ACIEFI                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0012
ACIEFI                           ".AADINT"   DELIMITED BY SIZE          RMB0012
ACIEFI                                         INTO W-SLD-NOMBRE        RMB0012
ACIEFI                    END-STRING                                    RMB0012
ACIEFI                WHEN = 41                                         RMB0012
ACIEFI                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0012
ACIEFI                           ".AADHVT"   DELIMITED BY SIZE          RMB0012
ACIEFI                                         INTO W-SLD-NOMBRE        RMB0012
ACIEFI                    END-STRING                                    RMB0012
ACIEFI                WHEN = 45                                         RMB0012
ACIEFI                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0012
ACIEFI                           ".AADCOO"   DELIMITED BY SIZE          RMB0012
ACIEFI                                         INTO W-SLD-NOMBRE        RMB0012
ACIEFI                    END-STRING                                    RMB0012
R7129D                WHEN = 99                                         RMB0010
R7129D                    STRING W-ENT-NOMBRE DELIMITED BY ".",         RMB0010
R7129D                           ".CARACT"   DELIMITED BY SIZE          RMB0010
R7129D                                         INTO W-SLD-NOMBRE        RMB0010
R7129D                    END-STRING                                    RMB0010
R15689                WHEN = 119                                        STT0014
R15689                    STRING W-ENT-NOMBRE DELIMITED BY ".",         STT0014
R15689                           ".MICR-B"   DELIMITED BY SIZE          STT0014
R15689                                         INTO W-SLD-NOMBRE        STT0014
R15689                    END-STRING                                    STT0014
R15689                WHEN = 120                                        STT0014
R15689                    STRING W-ENT-NOMBRE DELIMITED BY ".",         STT0014
R15689                           ".MICR-E"   DELIMITED BY SIZE          STT0014
R15689                                         INTO W-SLD-NOMBRE        STT0014
R15689                    END-STRING                                    STT0014
R7129D             END-EVALUATE                                         RMB0010
R7129D             PERFORM 100-ASIGNAR-NOMBRE-SALIDA                    RMB0010
R7129D          END-IF                                                  RMB0010
R7129D        END-PERFORM                                               RMB0010
R15689*       IF W-TIPO-SCORE > 10                                      STT0014
R15689        IF W-TIPO-SCORE(1:3) > "010"                              STT0014
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG0" DELIMITED BY SIZE INTO W-NOMBRE-MANT00         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG1" DELIMITED BY SIZE INTO W-NOMBRE-MANT01         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG2" DELIMITED BY SIZE INTO W-NOMBRE-MANT02         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG3" DELIMITED BY SIZE INTO W-NOMBRE-MANT03         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG4" DELIMITED BY SIZE INTO W-NOMBRE-MANT04         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG5" DELIMITED BY SIZE INTO W-NOMBRE-MANT05         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG6" DELIMITED BY SIZE INTO W-NOMBRE-MANT06         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG7" DELIMITED BY SIZE INTO W-NOMBRE-MANT07         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG8" DELIMITED BY SIZE INTO W-NOMBRE-MANT08         RMB0002
MANT01           STRING W-SLD-NOMBRE DELIMITED BY " ",                  RMB0002
MANT01           ".SEG9" DELIMITED BY SIZE INTO W-NOMBRE-MANT09         RMB0002
MANT01        END-IF                                                    RMB0002
              OPEN INPUT  BDIIREGI
PAM379        OPEN INPUT  BDIIDAS
              OPEN INPUT  MAESTRO-DIRECC
              OPEN INPUT  NOAMNISTIA21
              OPEN INPUT  MAESTRO-CHECK
CARDEM        OPEN INPUT  BDLSTR                                        GDT0007
CARDEM        OPEN INPUT  BDIIDIAN                                      GDT0007
CARDEM*                                                                 GDT0007
              OPEN INPUT  MAESTRO-CREDIT
              OPEN INPUT  SUSCRIPTOR
              OPEN INPUT  INDEXADO
              OPEN INPUT  BDIEMSUP
R7129A        IF W-REINICIO = "SI"                                      RMB0010
R7129A           OPEN I-O ULTIMO                                        RMB0010
R7129A           READ ULTIMO                                            RMB0010
R7129A              INVALID KEY                                         RMB0010
R7129A                 MOVE "NO" TO W-REINICIO                          RMB0010
R7129A                 CLOSE ULTIMO                                     RMB0010
R7129A                 DISPLAY "NO HAY DONDE REINICIAR,SE COMIENZA "    RMB0010
R7129A                 DISPLAY "DESDE EL PRINCIPIO (VACIO)         "    RMB0010
R7129A              NOT INVALID KEY                                     RMB0010
R7129A                 IF REG-ULTIMO = ZEROS                            RMB0010
R7129A                    MOVE "NO" TO W-REINICIO                       RMB0010
R7129A                    CLOSE ULTIMO                                  RMB0010
R7129A                    DISPLAY "YA HABIA TERMINADO OK EL PROGRAMA"   RMB0010
R7129A                    DISPLAY "NO HAY DONDE REINICIAR,SE COMIENZA " RMB0010
R7129A                    DISPLAY "DESDE EL PRINCIPIO (CEROS)         " RMB0010
R7129A                 ELSE                                             RMB0010
R7129A                    MOVE W-PREDICTA-ULTIMO    TO  W-PREDICTA      RMB0010
R7129A                    MOVE W-ACIERTA-ULTIMO     TO  W-ACIERTA       RMB0010
R7129A                    MOVE W-BASICO-ULTIMO      TO  W-BASICO        RMB0010
R7129A                    MOVE W-ACIERTA-M-ULTIMO   TO  W-ACIERTA-M     RMB0010
R7129A                    MOVE W-QUANTO-ULTIMO      TO  W-QUANTO        RMB0010
R7129A                    MOVE W-PREDICTA-HD-ULTIMO TO  W-PREDICTA-HD   RMB0010
ACIEAA                    MOVE W-ACIERTA-A-ULTIMO   TO  W-ACIERTA-A     STT0012
ACIEFI                    MOVE W-ACIERTA-A-DFGE-ULTIMO                  RMB0012
ACIEFI                                              TO  W-ACIERTA-A-DFGERMB0012
ACIEFI                    MOVE W-ACIERTA-A-DTRO-ULTIMO                  RMB0012
ACIEFI                                              TO  W-ACIERTA-A-DTRORMB0012
ACIEFI                    MOVE W-ACIERTA-A-DINT-ULTIMO                  RMB0012
ACIEFI                                              TO  W-ACIERTA-A-DINTRMB0012
ACIEFI                    MOVE W-ACIERTA-A-DHVT-ULTIMO                  RMB0012
ACIEFI                                              TO  W-ACIERTA-A-DHVTRMB0012
ACIEFI                    MOVE W-ACIERTA-A-DCOO-ULTIMO                  RMB0012
ACIEFI                                              TO  W-ACIERTA-A-DCOORMB0012
R15689                    MOVE W-MICROCREDITO-S-ULTIMO                  STT0014
R15689                                              TO  W-MICROCREDITO-SSTT0014
R7129A                    MOVE W-CARACTERISTICAS-ULTIMO TO              RMB0010
R7129A                         W-CARACTERISTICAS                        RMB0010
R7129A                    MOVE TIPIDE-ULTIMO TO ULT-TIP-ID              RMB0010
R7129A                    MOVE NUMIDE-ULTIMO TO ULT-NUM-ID              RMB0010
R7129A                    MOVE ZEROS TO REG-ULTIMO                      RMB0010
R7129A                 END-IF                                           RMB0010
R7129A           END-READ                                               RMB0010
R7129A        END-IF                                                    RMB0010
CQ3364        IF W-REINICIO = "SI"                                      PJV0008
R7129A           DISPLAY "PROGRAMA ARRANCA CON REINICIO AUTOMATICO"     RMB0010
R7129D*          OPEN EXTEND SALIDA                                     RMB0010
R7129D           PERFORM 100-OPEN-EXTEND-COMBO                          RMB0010
R7129A*          OPEN INPUT ULTIMO                                      RMB0010
R7129A*          READ ULTIMO NEXT INTO ULT-SCORE-VALID                  RMB0010
CQ3364           DISPLAY "REINICIO ULT-TIP-ID: " ULT-TIP-ID             PJV0008
CQ3364           DISPLAY "         ULT-NUM-ID: " ULT-NUM-ID             PJV0008
R7129A*          DISPLAY "         ULT-NOMBRE: " ULT-NOMBRE             RMB0010
R7129A*          CLOSE ULTIMO                                           RMB0010
CQ3364           MOVE 0 TO FIN-ULTIMO                                   PJV0008
CQ3364           PERFORM UNTIL FIN-ULTIMO = 1                           PJV0008
R7129A                   OR W-FIN-LECTURA = 1                           RMB0010
R7129A              PERFORM 0210-LEER-ENTRADA                           RMB0010
R7129A              IF ULT-TIP-ID = W-TIPO-ID-ENT AND                   RMB0010
R7129A                 ULT-NUM-ID = W-NUME-ID-ENT                       RMB0010
R7129A                 MOVE 1 TO FIN-ULTIMO                             RMB0010
R7129A              END-IF                                              RMB0010
CQ3364           END-PERFORM                                            PJV0008
R7129A           DISPLAY "SE LEYERON REINICIANDO: " W-LEIDOS            RMB0010
R7129A           DISPLAY "TIP-ID-ENTR: " W-TIPO-ID-ENT                  RMB0010
R7129A           DISPLAY "NUM-ID-ENTR: " W-NUME-ID-ENT                  RMB0010
R7129A           DISPLAY "===== CONTADORES AL REINICIO===="             RMB0010
R7129A           PERFORM 1000-DESPLEGAR-ESTADISTICAS                    RMB0010
R7129A           DISPLAY "================================"             RMB0010
CQ3364        ELSE                                                      PJV0008
R7129A           OPEN OUTPUT ULTIMO                                     RMB0010
R7129A           MOVE ZEROS TO REG-ULTIMO                               RMB0010
R7129A           WRITE REG-ULTIMO                                       RMB0010
R7129A           CLOSE ULTIMO                                           RMB0010
R7129A           OPEN I-O ULTIMO                                        RMB0010
R7129A           READ ULTIMO                                            RMB0010
R7129A                INVALID KEY DISPLAY "ERROR DE LOGICA EN REINICIO" RMB0010
R7129A                STOP RUN                                          RMB0010
R7129A           END-READ                                               RMB0010
R7129D*          OPEN OUTPUT SALIDA                                     RMB0010
R7129D           PERFORM 100-OPEN-OUTPUT-COMBO                          RMB0010
CQ3364        END-IF                                                    PJV0008
           ELSE
              DISPLAY "                                         "
              DISPLAY " Error.archivo no existe proceso termina "
              DISPLAY "                                         "
              STOP RUN
           END-IF.
QUAN01 1000-VALIDAR-FECHAS.                                             RMB0002
QUAN01        ACCEPT FECHA-SISTEMA FROM DATE YYYYMMDD                   RMB0002
QUAN01        DISPLAY "FECHA SISTEMA : " FECHA-SISTEMA                  RMB0002
QUAN01        MOVE W-FECHA-BASE TO FECHA-RUN-CARACT.                    RMB0002
QUAN01        DISPLAY  "FECHA RECOGIDA BD(YYYYMM) : " FECHA-RUN-CARACT  RMB0002
QUAN01        IF ANO-RUN-CARACT < 2004                                  RMB0002
QUAN01           DISPLAY "FECHA RECOGIDA BD(YYYYMM) NO EXISTE"          RMB0002
QUAN01           STOP RUN                                               RMB0002
QUAN01        ELSE                                                      RMB0002
QUAN01         IF MES-RUN-CARACT < 1 OR > 12                            RMB0002
QUAN01           DISPLAY "FECHA RECOGIDA BD(YYYYMM) ERRADA "            RMB0002
QUAN01           STOP RUN                                               RMB0002
QUAN01         ELSE                                                     RMB0002
QUAN01           IF (ANO-RUN-CARACT > FECHA-SIST-YYYY) OR               RMB0002
QUAN01              (ANO-RUN-CARACT = FECHA-SIST-YYYY AND               RMB0002
QUAN01               MES-RUN-CARACT > FECHA-SIST-MM)                    RMB0002
QUAN01              DISPLAY "FECHA RECOGIDA BD(YYYYMM) MAYOR QUE HOY"   RMB0002
QUAN01              STOP RUN                                            RMB0002
QUAN01           END-IF                                                 RMB0002
QUAN01         END-IF                                                   RMB0002
QUAN01        END-IF                                                    RMB0002
QUAN01         IF FECHA-RUN-CARACT <= 200801 AND                        RMB0002
QUAN01           FECHA-RUN-CARACT >= 200112                             RMB0002
QUAN01           DISPLAY ENV-NAME  UPON ENVIRONMENT-NAME                RMB0002
QUAN01           ACCEPT  ENV-VALUE FROM ENVIRONMENT-VALUE               RMB0002
QUAN01           STRING "rm $DATOS/extfh-tot.cfg " DELIMITED BY SIZE    RMB0002
QUAN01                  CR-FILL                    DELIMITED BY SIZE    RMB0002
QUAN01             INTO W-COMANDO-SYST                                  RMB0002
QUAN01           END-STRING                                             RMB0002
QUAN01           CALL "SYSTEM" USING COMANDO-SYST                       RMB0002
QUAN01           DISPLAY "VAR AMB : " ENV-NAME " " ENV-VALUE            RMB0002
QUAN01           INITIALIZE COMANDO-SYST                                RMB0002
QUAN01           STRING "cat "                     DELIMITED BY SIZE    RMB0002
QUAN01               ENV-VALUE                     DELIMITED BY SPACE   RMB0002
QUAN01               " $DATOS/extfh-caract.cfg > " DELIMITED BY SIZE    RMB0002
QUAN01               " $DATOS/extfh-tot.cfg "      DELIMITED BY SIZE    RMB0002
QUAN01                CR-FILL                      DELIMITED BY SIZE    RMB0002
QUAN01               INTO W-COMANDO-SYST                                RMB0002
QUAN01           END-STRING                                             RMB0002
QUAN01           display COMANDO-SYST                                   RMB0002
QUAN01           CALL "SYSTEM" USING COMANDO-SYST                       RMB0002
QUAN01           DISPLAY "EJECUTA : " COMANDO-SYST                      RMB0002
QUAN01           MOVE "$DATOS/extfh-tot.cfg" TO ENV-VALUE               RMB0002
QUAN01           DISPLAY ENV-NAME  UPON ENVIRONMENT-NAME                RMB0002
QUAN01           DISPLAY ENV-VALUE UPON ENVIRONMENT-VALUE               RMB0002
QUAN01           DISPLAY "VAR AMB1: " ENV-NAME " " ENV-VALUE            RMB0002
QUAN01         END-IF.                                                  RMB0002
       0100-PROCESO-PRINCIPAL.
      *****************************************************************
      *0100-PROCESO-PRINCIPAL.                                        *
      * Rutina que lee uno a uno los registros del archivo de entrada *
      * llama la rutina 5000-TRU-SCORING-TOTAL para calcular  el score*
      * y las razones,y finalmente llama la rutina 0010-ESCRIBE-SALIDA*
      * que escribe la informaci¢n obtenida.                          *
      *****************************************************************
           PERFORM  0200-READ-ARCHIVOS
           PERFORM  UNTIL W-FIN-LECTURA = 1
R15689*         IF  W-TIPO-SCORE < 10                                   STT0014
R15689          IF  W-TIPO-SCORE(1:3) < "010"  AND                      STT0014
R15689              W-TIPO-SCORE(1:3) > "000"                           STT0014
R15689*             MOVE W-TIPO-SCORE TO W-PRODUCTO                     STT0014
R15689              MOVE W-TIPO-SCORE(1:3) TO W-PRODUCTO                STT0014
                    MOVE  W-TIPO-ID-ENT  TO  TIP-ID-ALE
                    MOVE  W-NUME-ID-ENT  TO  NUM-ID-ALE
R15689*             MOVE  W-TIPO-SCORE   TO  TIP-SCO-GENERICO           STT0014
R15689              MOVE  W-TIPO-SCORE(2:2)   TO  TIP-SCO-GENERICO      STT0014
                    IF W-FECHA-BASE = W-FECHA-HOY
                       MOVE FUNCTION CURRENT-DATE(1:6)
                                          TO  FECHA-RUN-DATE
                       MOVE FUNCTION CURRENT-DATE(1:8)
                                          TO  ULTIMO-TRI
                       PERFORM  5000-TRU-SCORING-TOTAL
                    ELSE
                       PERFORM  0400-CALCULAR-FECHAS
                       MOVE  1  TO  USAR-HIS-INPUT-BDSCORES
                       PERFORM  5000-TRU-SCORING-BDSCORES
REQ601                 PERFORM  0500-EVALUA-SCORE                       LAG0005
REQ601                 IF W-SCORE = 0                                   LAG0005
REQ601                    PERFORM  5000-TRU-SCORING-TOTAL               LAG0005
REQ601                 END-IF                                           LAG0005
                    END-IF
                    PERFORM  0500-ESCRIBE-SALIDA
QUAN01          ELSE                                                    RMB0002
QUAN01              MOVE W-FECHA-BASE  TO FECHA-RUN-CARACT              RMB0002
QUAN01              MOVE 1 TO GEN-NUM-CARACT                            RMB0002
QUAN01                        GEN-INQ-CARACT GEN-MOR-CARACT             RMB0002
QUAN01                        GEN-EXP-CARACT GEN-DEM-CARACT             RMB0002
QUAN01                        GEN-ACP-CARACT GEN-END-CARACT             RMB0002
QUAN01              MOVE W-TIPO-ID-ENT  TO TIPO-ID-CARACT               RMB0002
QUAN01              MOVE W-NUME-ID-ENT  TO NRO-ID-CARACT                RMB0002
ACIEAA              MOVE W-NITSUS       TO  NIT-RELACION-CARACT         STT0012
R15689              IF (W-SCORE-COMBO(1) = 119 OR 120) OR               STT0014
R15689                 (W-SCORE-COMBO(2) = 119 OR 120)                  STT0014
R15689                 MOVE "MIC" TO RUTINA-PORTAFOLIO-CARACT           STT0014
R15689              END-IF                                              STT0014
R7129D              PERFORM 1000-CALCULAR-CARACTERISTICAS               RMB0010
R7129D*             SE UTILIZA J PORQUE I ES UTILIZADO DENTRO DEL       RMB0010
R7129D*             LLAMADO                                             RMB0010
R7129D              PERFORM VARYING J FROM 1 BY 1 UNTIL J > 9           RMB0010
R7129D                IF W-SCORE-COMBO(J) > 0                           RMB0010
R7129D                   MOVE W-SCORE-COMBO(J) TO W-PRODUCTO            RMB0010
R7129D                   EVALUATE W-PRODUCTO                            RMB0010
R7129D                     WHEN 62     *> QUANTO                        RMB0010
R7129D                        PERFORM 5000-CALCULAR-SCORE-USAPI         RMB0010
QUAN01                        PERFORM 0500-ESCRIBE-SALIDA-QUANTO
R7129D                     WHEN 67     *> ACIERTA +                     RMB0010
R7129D                        PERFORM 5000-CALCULAR-SCORE-USAHD         RMB0010
PRHD01                        PERFORM 0500-ESCRIBE-SALIDA
ACIEAA                     WHEN 49                                      STT0012
ACIEAA                        MOVE "DCOM"          TO PORTAFOLIO-USAAA  STT0012
ACIEAA                        PERFORM 5000-CALCULAR-SCORE-USAAA         STT0012
ACIEAA                        PERFORM 0500-ESCRIBE-SALIDA               STT0012
ACIEFI                     WHEN 47                                      RMB0012
ACIEFI                        MOVE "DFGE"          TO PORTAFOLIO-USAAA  RMB0012
ACIEFI                        PERFORM 5000-CALCULAR-SCORE-USAAA         RMB0012
ACIEFI                        PERFORM 0500-ESCRIBE-SALIDA               RMB0012
ACIEFI                     WHEN 48                                      RMB0012
ACIEFI                        MOVE "DTRO"          TO PORTAFOLIO-USAAA  RMB0012
ACIEFI                        PERFORM 5000-CALCULAR-SCORE-USAAA         RMB0012
ACIEFI                        PERFORM 0500-ESCRIBE-SALIDA               RMB0012
ACIEFI                     WHEN 95                                      RMB0012
ACIEFI                        MOVE "DINT"          TO PORTAFOLIO-USAAA  RMB0012
ACIEFI                        PERFORM 5000-CALCULAR-SCORE-USAAA         RMB0012
ACIEFI                        PERFORM 0500-ESCRIBE-SALIDA               RMB0012
ACIEFI                     WHEN 41                                      RMB0012
ACIEFI                        MOVE "DHVT"          TO PORTAFOLIO-USAAA  RMB0012
ACIEFI                        PERFORM 5000-CALCULAR-SCORE-USAAA         RMB0012
ACIEFI                        PERFORM 0500-ESCRIBE-SALIDA               RMB0012
ACIEFI                     WHEN 45                                      RMB0012
ACIEFI                        MOVE "DCOO"          TO PORTAFOLIO-USAAA  RMB0012
ACIEFI                        PERFORM 5000-CALCULAR-SCORE-USAAA         RMB0012
ACIEFI                        PERFORM 0500-ESCRIBE-SALIDA               RMB0012
R7129D                     WHEN 99     *> CARACTERISTICAS               RMB0010
R7129C                        PERFORM 9000-GENERAR-CARACTERISTICAS      RMB0010
R15689                     WHEN 119                                     STT0014
R15689                        MOVE "BURO"         TO NOMB-VALI-DAMIC    STT0014
R15689                        PERFORM 5000-CALCULAR-SCORE-DAMIC         STT0014
R15689                        PERFORM 0500-ESCRIBE-SALIDA               STT0014
R15689                     WHEN 120                                     STT0014
R15689                        MOVE "FORMULARIO"   TO NOMB-VALI-DAMIC    STT0014
R15689                        MOVE W-ACTIVOS      TO                    STT0014
R15689                                      MI-ACTIVOS-TOTALES-DAMIC    STT0014
R15689                        PERFORM 5000-CALCULAR-SCORE-DAMIC         STT0014
R15689                        PERFORM 0500-ESCRIBE-SALIDA               STT0014
R7129D                END-IF                                            RMB0010
R7129D              END-PERFORM                                         RMB0010
R7129D              IF W-ARCHIVO-MANT = "MANTENIMIENTO"                 RMB0010
R7129D                 PERFORM 0200-GENERAR-MANTENIMIENTO               RMB0010
R7129D              END-IF                                              RMB0010
R7129D*             IF  W-TIPO-SCORE = 62                               RMB0010
R7129D*                PERFORM 5000-CALCULAR-CAR-SCORE-USAPI            RMB0010
R7129D*                PERFORM 0500-ESCRIBE-SALIDA-QUANTO               RMB0010
R7129D*                IF W-ARCHIVO-MANT = "MANTENIMIENTO"              RMB0010
R7129D*                   PERFORM 0200-GENERAR-MANTENIMIENTO            RMB0010
R7129D*                END-IF                                           RMB0010
R7129D*            ELSE                                                 RMB0010
R7129D*                PERFORM 5000-CALCULAR-CAR-SCORE-USAHD            RMB0010
R7129D*                PERFORM 0500-ESCRIBE-SALIDA                      RMB0010
R7129D*                IF W-ARCHIVO-MANT = "MANTENIMIENTO"              RMB0010
R7129D*                   PERFORM 0200-GENERAR-MANTENIMIENTO            RMB0010
R7129D*                END-IF                                           RMB0010
R7129D*            END-IF                                               RMB0010
QUAN01          END-IF                                                  RMB0002
                    PERFORM  0200-READ-ARCHIVOS
           END-PERFORM.
MANT01 0200-GENERAR-MANTENIMIENTO.                                      RMB0002
REMARK*****************************************************************
REMARK*0200-GENERAR-MANTENIMIENTO.
REMARK*    SE GENERAN PARA CADA SCORE CARD LAS VARIABLES DE MANTENIMIENTO
REMARK*    QUE INCLUYEN CARACTERISTICAR Y VARIABLES DUMIES
REMARK*****************************************************************
R15689*    IF W-TIPO-SCORE = 62                                         STT0014
R15689     IF W-TIPO-SCORE = "062"                                      STT0014
MANT01        PERFORM 6000-DESPLEGAR-AUDITORIA-USAPI                    RMB0002
MANT01        MOVE AREA-VALI-USAPI  TO AREA-MANTENIMIENTO               RMB0002
MANT01        MOVE SCORE-CARD-USAPI TO SCORE-CARD-AUX                   RMB0002
MANT01     END-IF                                                       RMB0002
R15689*    IF W-TIPO-SCORE = 67                                         STT0014
R15689     IF W-TIPO-SCORE = "067"                                      STT0014
MANT01        PERFORM 6000-DESPLEGAR-AUDITORIA-USAHD                    RMB0002
MANT01        MOVE AREA-VALI-USAHD  TO AREA-MANTENIMIENTO               RMB0002
MANT01        MOVE SCORE-CARD-USAHD TO SCORE-CARD-AUX                   RMB0002
MANT01     END-IF                                                       RMB0002
R15689*    IF W-TIPO-SCORE = 49                                         STT0014
R15689     IF W-TIPO-SCORE = "049"                                      STT0014
ACIEAA        PERFORM 6000-DESPLEGAR-AUDITORIA-USAAA                    STT0012
ACIEAA        MOVE AREA-VALI-USAAA  TO AREA-MANTENIMIENTO               STT0012
ACIEAA        MOVE SCORE-CARD-USAAA TO SCORE-CARD-AUX                   STT0012
ACIEAA     END-IF                                                       STT0012
R15689*    IF W-TIPO-SCORE = 47 OR 48 OR 95 OR 41 OR 45                 STT0014
R15689     IF W-TIPO-SCORE = "047" OR "048" OR "095" OR "041" OR "045"  STT0014
ACIEFI        PERFORM 6000-DESPLEGAR-AUDITORIA-USAAA                    RMB0012
ACIEFI        MOVE AREA-VALI-USAAA  TO AREA-MANTENIMIENTO               RMB0012
ACIEFI        MOVE SCORE-CARD-USAAA TO SCORE-CARD-AUX                   RMB0012
ACIEFI     END-IF                                                       RMB0012
R15689     IF W-TIPO-SCORE = "119" OR "120"                             STT0014
R15689        PERFORM 6000-DESPLEGAR-AUDITORIA-DAMIC                    STT0014
R15689        MOVE AREA-VALI-DAMIC  TO AREA-MANTENIMIENTO               STT0014
R15689        MOVE SCORE-CARD-DAMIC TO SCORE-CARD-AUX                   STT0014
R15689     END-IF                                                       STT0014
MANT01     EVALUATE SCORE-CARD-AUX                                      RMB0002
MANT01        WHEN 0                                                    RMB0002
MANT01          IF W-OPEN-MANT00 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT00                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT00                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT00 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 1                                                    RMB0002
MANT01          IF W-OPEN-MANT01 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT01                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT01                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT01 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 2                                                    RMB0002
MANT01          IF W-OPEN-MANT02 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT02                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT02                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT02 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 3                                                    RMB0002
MANT01          IF W-OPEN-MANT03 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT03                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT03                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT03 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 4                                                    RMB0002
MANT01          IF W-OPEN-MANT04 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT04                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT04                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT04 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 5                                                    RMB0002
MANT01          IF W-OPEN-MANT05 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT05                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT05                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT05 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 6                                                    RMB0002
MANT01          IF W-OPEN-MANT06 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT06                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT06                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT06 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 7                                                    RMB0002
MANT01          IF W-OPEN-MANT07 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT07                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT07                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT07 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 8                                                    RMB0002
MANT01          IF W-OPEN-MANT08 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT08                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT08                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT08 FROM AREA-MANTENIMIENTO                RMB0002
MANT01        WHEN 9                                                    RMB0002
MANT01          IF W-OPEN-MANT09 = 0                                    RMB0002
MANT01             OPEN OUTPUT ARC-MANT09                               RMB0002
MANT01             MOVE 1 TO W-OPEN-MANT09                              RMB0002
MANT01          END-IF                                                  RMB0002
MANT01          WRITE REG-MANT09 FROM AREA-MANTENIMIENTO                RMB0002
MANT01     END-EVALUATE.                                                RMB0002

       0200-READ-ARCHIVOS.
      *****************************************************************
      *0200-READ-ARCHIVOS.                                            *
      *     Rutina de lecturas segun la opcion de archivo             *
      *****************************************************************
R7129A     IF REG-ULTIMO  <> ZEROS                                      RMB0010
R7129A        MOVE W-PREDICTA        TO W-PREDICTA-ULTIMO               RMB0010
R7129A        MOVE W-ACIERTA         TO W-ACIERTA-ULTIMO                RMB0010
R7129A        MOVE W-BASICO          TO W-BASICO-ULTIMO                 RMB0010
R7129A        MOVE W-ACIERTA-M       TO W-ACIERTA-M-ULTIMO              RMB0010
R7129A        MOVE W-QUANTO          TO W-QUANTO-ULTIMO                 RMB0010
R7129A        MOVE W-PREDICTA-HD     TO W-PREDICTA-HD-ULTIMO            RMB0010
ACIEAA        MOVE W-ACIERTA-A       TO W-ACIERTA-A-ULTIMO              STT0012
ACIEFI        MOVE W-ACIERTA-A-DFGE  TO W-ACIERTA-A-DFGE-ULTIMO         RMB0012
ACIEFI        MOVE W-ACIERTA-A-DTRO  TO W-ACIERTA-A-DTRO-ULTIMO         RMB0012
ACIEFI        MOVE W-ACIERTA-A-DINT  TO W-ACIERTA-A-DINT-ULTIMO         RMB0012
ACIEFI        MOVE W-ACIERTA-A-DHVT  TO W-ACIERTA-A-DHVT-ULTIMO         RMB0012
ACIEFI        MOVE W-ACIERTA-A-DCOO  TO W-ACIERTA-A-DCOO-ULTIMO         RMB0012
R15689        MOVE W-MICROCREDITO-S  TO W-MICROCREDITO-S-ULTIMO         STT0014
R7129A        MOVE W-CARACTERISTICAS TO W-CARACTERISTICAS-ULTIMO        RMB0010
R7129A        REWRITE REG-ULTIMO INVALID KEY                            RMB0010
R7129A          DISPLAY "ERROR DE LOGICA !!!! EN REWRITE ULTIMO"        RMB0010
R7129A        END-REWRITE                                               RMB0010
R7129A     END-IF.                                                      RMB0010
           INITIALIZE  W-AREA-TRABAJO
R7129A     PERFORM 0210-LEER-ENTRADA.                                   RMB0010
R7129A     MOVE W-TIPO-ID-ENT TO TIPIDE-ULTIMO.                         RMB0010
R7129A     MOVE W-NUME-ID-ENT TO NUMIDE-ULTIMO.                         RMB0010
R7129A 0210-LEER-ENTRADA.                                               RMB0010
R15689     MOVE 0      TO W-ACTIVOS                                     STT0014
           EVALUATE    W-TIPO-ARCHIVO
                  WHEN = "IDE"
                         INITIALIZE  ENT-BASE-IDE
                                     SLD-BASE-IDE
QUAN01                   MOVE ALL ";" TO SLD-BASE-IDE                   RMB0002
                         READ ENTRADA INTO  ENT-BASE-IDE AT END
                              MOVE  1  TO  W-FIN-LECTURA
R7129A                        SUBTRACT 1 FROM W-LEIDOS                  RMB0010
                         END-READ
                         ADD   1            TO  W-LEIDOS
                         MOVE  ENT-TIPO-ID  TO  W-TIPO-ID-ENT
                         MOVE  ENT-NUME-ID  TO  W-NUME-ID-ENT
R15689                   IF    ENT-ACTIVOS-ID  IS NUMERIC               STT0014
R15689                      MOVE  ENT-ACTIVOS-ID  TO  W-ACTIVOS         STT0014
R15869                   END-IF
                  WHEN = "VAL" OR "PLA" OR "EDA"
                         INITIALIZE  ENT-SCORE-VALID
                                     SLD-SCORE-VALID
                                     PLA-SCORE-VALID
                                     EDA-SCORE-VALID
QUAN01                   MOVE ALL ";" TO SLD-SCORE-VALID                RMB0002
QUAN01                                   EDA-SCORE-VALID                RMB0002
                         READ ENTRADA INTO  ENT-SCORE-VALID AT END
                              MOVE  1  TO  W-FIN-LECTURA
R7129A                        SUBTRACT 1 FROM W-LEIDOS                  RMB0010
                         END-READ
                         ADD   1           TO  W-LEIDOS
                         MOVE  TIP-ID-VAL  TO  W-TIPO-ID-ENT
                         MOVE  NUM-ID-VAL  TO  W-NUME-ID-ENT
R15689                   IF ACTIVOS-VAL IS NUMERIC                      STT0014
R15689                      MOVE  ACTIVOS-VAL TO  W-ACTIVOS             STT0014
R15689                   END-IF                                         STT0014
CQ6539            WHEN = "BLQ"                                          STT0011
CQ6539                   INITIALIZE  ENT-SCORE-BLQ                      STT0011
CQ6539                               SLD-SCORE-BLQ                      STT0011
CQ6539                   MOVE ALL ";" TO SLD-SCORE-BLQ                  STT0011
CQ6539                   READ ENTRADA INTO  ENT-SCORE-BLQ   AT END      STT0011
CQ6539                        MOVE  1  TO  W-FIN-LECTURA                STT0011
CQ6539                        SUBTRACT 1 FROM W-LEIDOS                  STT0011
CQ6539                   END-READ                                       STT0011
CQ6539                   ADD   1           TO  W-LEIDOS                 STT0011
CQ6539                   MOVE  TIP-ID-BLQ  TO  W-TIPO-ID-ENT            STT0011
CQ6539                   MOVE  NUM-ID-BLQ  TO  W-NUME-ID-ENT            STT0011
R15689                   IF ACTIVOS-BLQ IS NUMERIC                      STT0014
R15689                      MOVE  ACTIVOS-BLQ TO  W-ACTIVOS             STT0014
R15689                   END-IF                                         STT0014
                  WHEN = "AWK"
                         INITIALIZE  ENT-SCORE-AWK
                                     SLD-SCORE-AWK
QUAN01                   MOVE ALL ";" TO SLD-SCORE-AWK                  RMB0002
                         READ ENTRADA INTO  ENT-SCORE-AWK AT END
                              MOVE  1  TO  W-FIN-LECTURA
R7129A                        SUBTRACT 1 FROM W-LEIDOS                  RMB0010
                         END-READ
                         ADD  1  TO  W-LEIDOS
                         MOVE  TIP-ID-AWK  TO  W-TIPO-ID-ENT
                         MOVE  NUM-ID-AWK  TO  W-NUME-ID-ENT
R15689                   IF ACTIVOS-AWK IS NUMERIC                      STT0014
R15689                      MOVE  ACTIVOS-AWK TO  W-ACTIVOS             STT0014
R15689                   END-IF                                         STT0014
                  WHEN = "PUN"
                         INITIALIZE  ENT-SCORE-NVAL
                                     SLD-SCORE-NVAL
QUAN01                   MOVE ALL ";" TO SLD-SCORE-NVAL                 RMB0002
                         READ ENTRADA INTO ENT-SCORE-NVAL AT END
                              MOVE  1  TO  W-FIN-LECTURA
R7129A                        SUBTRACT 1 FROM W-LEIDOS                  RMB0010
                         END-READ
                         ADD  1  TO  W-LEIDOS
                         MOVE  TIPO-ID-NVAL  TO  W-TIPO-ID-ENT
                         MOVE  NUME-ID-NVAL  TO  W-NUME-ID-ENT
R15689                   IF ACTIVOS-NVAL  IS NUMERIC                    STT0014
R15689                      MOVE  ACTIVOS-NVAL  TO  W-ACTIVOS           STT0014
R15689                   END-IF                                         STT0014
           END-EVALUATE.

       0400-CALCULAR-FECHAS.
      *****************************************************************
      *0400-CALCULAR-FECHAS
      * Estima las fechas de calculo segun ambiente y opcion de       *
      * proceso                                                       *
      *****************************************************************
           MOVE  W-FECHA-BASE  TO  W-FECHA-HIS  W-FEC-END
           IF W-MES-HIS = 00 OR W-MES-HIS > 12
              DISPLAY "                                  "
              DISPLAY "                                  "
              DISPLAY " Fecha errada  para calculo score "
              STOP RUN
           END-IF
           MOVE FUNCTION CURRENT-DATE(1:4) TO W-ANO-HOY
           IF W-ANO-HIS < 2000 OR W-ANO-HIS > W-ANO-HOY
              DISPLAY "                                  "
              DISPLAY "                                  "
              DISPLAY " Fecha errada  para calculo score "
              STOP RUN
           END-IF
           IF W-MES-HIS = 02
              MOVE  28 TO  W-DIA-END
           ELSE
              IF W-MES-HIS = 04 OR 06 OR 09 OR 11
                 MOVE  30 TO  W-DIA-END
              ELSE
                 MOVE  31 TO  W-DIA-END
              END-IF
           END-IF
           MOVE   W-FECHA-HIS  TO  FECHA-RUN-DATE
           MOVE   W-FECHA-END  TO  ULTIMO-TRI.

REQ601 0500-EVALUA-SCORE.                                               LAG0005
REQ601     EVALUATE W-TIPO-SCORE                                        LAG0005
R15689*        WHEN = 1                                                 STT0014
R15689         WHEN = "001"                                             STT0014
REQ601              MOVE    SCORE            TO  W-SCORE                LAG0005
R15689*        WHEN = 4                                                 STT0014
R15689         WHEN = "004"                                             STT0014
REQ601              MOVE  A-SCORE            TO  W-SCORE                LAG0005
R15689*        WHEN = 6                                                 STT0014
R15689         WHEN = "006"                                             STT0014
REQ601              MOVE  M-SCORE            TO  W-SCORE                LAG0005
R15689*        WHEN = 8                                                 STT0014
R15689         WHEN = "008"                                             STT0014
REQ601              MOVE  P-SCORE            TO  W-SCORE                LAG0005
REQ601     END-EVALUATE.                                                LAG0005

       0500-ESCRIBE-SALIDA.
      *****************************************************************
      *0500-ESCRIBE-SALIDA.                                           *
      * Rutina que escribe el registro de salida con la informacion   *
      * del encabezado del registro de entrada, el score y las cuatro *
      * razones calculadas.                                           *
      *****************************************************************
R7129D*    INITIALIZE  REG-SALIDA                                       RMB0010
R7129D*    EVALUATE W-TIPO-SCORE                                        RMB0010
R7129D     EVALUATE W-PRODUCTO                                          RMB0010
               WHEN = 1
                    MOVE    SCORE            TO  W-SCORE
QUAN01              MOVE    SCORE-CARD       TO  W-SCORE-CARD           RMB0002
                    IF SCORE > ZEROS
                       ADD   1               TO  W-PREDICTA
                    END-IF
                    MOVE  ADVERSE-RAZON(1)   TO  W-RAZON1
                    MOVE  ADVERSE-RAZON(2)   TO  W-RAZON2
                    MOVE  ADVERSE-RAZON(3)   TO  W-RAZON3
                    MOVE  ADVERSE-RAZON(4)   TO  W-RAZON4
               WHEN = 4
                    MOVE  A-SCORE            TO  W-SCORE
QUAN01              MOVE  A-SCORE-CARD       TO  W-SCORE-CARD           RMB0002
                    IF A-SCORE > ZEROS
                       ADD   1               TO  W-ACIERTA
                    END-IF
                    MOVE  A-ADVERSE-RAZON(1) TO  W-RAZON1
                    MOVE  A-ADVERSE-RAZON(2) TO  W-RAZON2
                    MOVE  A-ADVERSE-RAZON(3) TO  W-RAZON3
                    MOVE  A-ADVERSE-RAZON(4) TO  W-RAZON4
               WHEN = 6
                    MOVE  M-SCORE            TO  W-SCORE
QUAN01              MOVE  M-SCORE-CARD       TO  W-SCORE-CARD           RMB0002
                    IF M-SCORE > ZEROS
                       ADD   1               TO  W-BASICO
                    END-IF
                    MOVE  M-ADVERSE-RAZON(1) TO  W-RAZON1
                    MOVE  M-ADVERSE-RAZON(2) TO  W-RAZON2
                    MOVE  M-ADVERSE-RAZON(3) TO  W-RAZON3
                    MOVE  M-ADVERSE-RAZON(4) TO  W-RAZON4
               WHEN = 8
                    MOVE  P-SCORE            TO  W-SCORE
QUAN01              MOVE  P-SCORE-CARD       TO  W-SCORE-CARD           RMB0002
                    IF P-SCORE > ZEROS
                       ADD   1               TO  W-ACIERTA-M
                    END-IF
                    MOVE  P-ADVERSE-RAZON(1) TO  W-RAZON1
                    MOVE  P-ADVERSE-RAZON(2) TO  W-RAZON2
                    MOVE  P-ADVERSE-RAZON(3) TO  W-RAZON3
                    MOVE  P-ADVERSE-RAZON(4) TO  W-RAZON4
PRHD01         WHEN = 67                                                RMB0002
PRHD01              MOVE  SCORE-USAHD        TO  W-SCORE                RMB0002
PRHD01              MOVE  SCORE-CARD-USAHD   TO  W-SCORE-CARD           RMB0002
PRHD01              IF SCORE-USAHD > ZEROS                              RMB0002
PRHD01                 ADD   1               TO  W-PREDICTA-HD          RMB0002
PRHD01              END-IF                                              RMB0002
PRHD01              MOVE  ADVERSE-RAZON-USAHD(1) TO  W-RAZON1           RMB0002
ACIEAA         WHEN = 49                                                STT0012
ACIEAA              MOVE  SCORE-USAAA        TO  W-SCORE                STT0012
ACIEAA              MOVE  SCORE-CARD-USAAA   TO  W-SCORE-CARD           STT0012
ACIEAA              IF SCORE-USAAA > ZEROS                              STT0012
ACIEAA                 ADD   1               TO  W-ACIERTA-A            STT0012
ACIEAA              END-IF                                              STT0012
ACIEAA              MOVE  ADVERSE-RAZON-USAAA(1) TO  W-RAZON1           STT0012
ACIEAA              IF PORTAFOLIO-USAAA = "DFDI"                        STT0012
ACIEAA                 MOVE  ADVERSE-RAZON-USAAA(2)    TO  W-RAZON2     STT0012
ACIEAA                 MOVE  ADVERSE-RAZON-USAAA(3)    TO  W-RAZON3     STT0012
ACIEAA                 MOVE  ADVERSE-RAZON-USAAA(4)    TO  W-RAZON4     STT0012
ACIEAA              END-IF                                              STT0012
ACIEFI         WHEN = 47                                                RMB0012
ACIEFI              MOVE  SCORE-USAAA        TO  W-SCORE                RMB0012
ACIEFI              MOVE  SCORE-CARD-USAAA   TO  W-SCORE-CARD           RMB0012
ACIEFI              IF SCORE-USAAA > ZEROS                              RMB0012
ACIEFI                 ADD   1               TO  W-ACIERTA-A-DFGE       RMB0012
ACIEFI              END-IF                                              RMB0012
ACIEFI              MOVE  ADVERSE-RAZON-USAAA(1) TO  W-RAZON1           RMB0012
ACIEFI         WHEN = 48                                                RMB0012
ACIEFI              MOVE  SCORE-USAAA        TO  W-SCORE                RMB0012
ACIEFI              MOVE  SCORE-CARD-USAAA   TO  W-SCORE-CARD           RMB0012
ACIEFI              IF SCORE-USAAA > ZEROS                              RMB0012
ACIEFI                 ADD   1               TO  W-ACIERTA-A-DTRO       RMB0012
ACIEFI              END-IF                                              RMB0012
ACIEFI              MOVE  ADVERSE-RAZON-USAAA(1) TO  W-RAZON1           RMB0012
ACIEFI         WHEN = 95                                                RMB0012
ACIEFI              MOVE  SCORE-USAAA        TO  W-SCORE                RMB0012
ACIEFI              MOVE  SCORE-CARD-USAAA   TO  W-SCORE-CARD           RMB0012
ACIEFI              IF SCORE-USAAA > ZEROS                              RMB0012
ACIEFI                 ADD   1               TO  W-ACIERTA-A-DINT       RMB0012
ACIEFI              END-IF                                              RMB0012
ACIEFI              MOVE  ADVERSE-RAZON-USAAA(1) TO  W-RAZON1           RMB0012
ACIEFI         WHEN = 41                                                RMB0012
ACIEFI              MOVE  SCORE-USAAA        TO  W-SCORE                RMB0012
ACIEFI              MOVE  SCORE-CARD-USAAA   TO  W-SCORE-CARD           RMB0012
ACIEFI              IF SCORE-USAAA > ZEROS                              RMB0012
ACIEFI                 ADD   1               TO  W-ACIERTA-A-DHVT       RMB0012
ACIEFI              END-IF                                              RMB0012
ACIEFI              MOVE  ADVERSE-RAZON-USAAA(1) TO  W-RAZON1           RMB0012
ACIEFI         WHEN = 45                                                RMB0012
ACIEFI              MOVE  SCORE-USAAA        TO  W-SCORE                RMB0012
ACIEFI              MOVE  SCORE-CARD-USAAA   TO  W-SCORE-CARD           RMB0012
ACIEFI              IF SCORE-USAAA > ZEROS                              RMB0012
ACIEFI                 ADD   1               TO  W-ACIERTA-A-DCOO       RMB0012
ACIEFI              END-IF                                              RMB0012
ACIEFI              MOVE  ADVERSE-RAZON-USAAA(1) TO  W-RAZON1           RMB0012
R15689         WHEN = 119                                               STT0014
R15689              MOVE  SCORE-DAMIC        TO  W-SCORE                STT0014
R15689              MOVE  SCORE-CARD-DAMIC   TO  W-SCORE-CARD           STT0014
R15689              IF SCORE-DAMIC > ZEROS                              STT0014
R15689                 ADD   1               TO  W-MICROCREDITO-S       STT0014
R15689              END-IF                                              STT0014
R15689              MOVE  ADVERSE-RAZON-DAMIC(1) TO  W-RAZON1           STT0014
R15689         WHEN = 120                                               STT0014
R15689              MOVE  SCORE-DAMIC        TO  W-SCORE                STT0014
R15689              MOVE  SCORE-CARD-DAMIC   TO  W-SCORE-CARD           STT0014
R15689              IF SCORE-DAMIC > ZEROS                              STT0014
R15689                 ADD   1               TO  W-MICROCREDITO-S       STT0014
R15689              END-IF                                              STT0014
R15689              MOVE  ADVERSE-RAZON-DAMIC(1) TO  W-RAZON1           STT0014
           END-EVALUATE

           EVALUATE  W-TIPO-ARCHIVO
                  WHEN = "IDE"
                         MOVE  ENT-TIPO-ID     TO  SLD-TIPO-ID
                         MOVE  ENT-NUME-ID     TO  SLD-NUME-ID
                         MOVE  W-SCORE         TO  SLD-VAL-SCORE
QUAN01                   MOVE  W-SCORE-CARD    TO  SLD-SCORE-CARD       RMB0002
R7129D*                  WRITE  REG-SALIDA  FROM   SLD-BASE-IDE         RMB0010
R7129D                   MOVE  SLD-BASE-IDE    TO  W-AREA-PRODUCTO      RMB0010
                  WHEN = "VAL"
                         MOVE  TIP-ID-VAL      TO  TIP-ID-SLD
                         MOVE  NUM-ID-VAL      TO  NUM-ID-SLD
                         MOVE  NOMBRE-VAL      TO  NOMBRE-SLD
                         MOVE  W-SCORE         TO  SCORE-SLD
                         MOVE  W-RAZON1        TO  RAZON1-SLD
                         MOVE  W-RAZON2        TO  RAZON2-SLD
                         MOVE  W-RAZON3        TO  RAZON3-SLD
                         MOVE  W-RAZON4        TO  RAZON4-SLD
                         MOVE  EDAD-VAL        TO  SLD-EDAD-APROX
                         MOVE  EDAD-RANGO-VAL  TO  SLD-RANGO-EDAD
                         MOVE  FEC-EXP-VAL     TO  SLD-FEC-EXP-VAL
                         MOVE  MUN-EXP-VAL     TO  SLD-MUN-EXP-VAL
                         MOVE  DEP-EXP-VAL     TO  SLD-DEP-EXP-VAL
                         MOVE  VIG-VAL         TO  SLD-VIG-VAL
                         MOVE  GENERO-VAL      TO  SLD-GENERO-VAL
                         MOVE  ADICIONAL-VAL   TO  SLD-RELLENO
R7129D*                  WRITE  REG-SALIDA  FROM  SLD-SCORE-VALID       RMB0010
R7129D                   MOVE  SLD-SCORE-VALID TO  W-AREA-PRODUCTO      RMB0010
CQ6539            WHEN = "BLQ"                                          STT0011
CQ6539                   MOVE  TIP-ID-BLQ      TO  TIP-ID-SBLQ          STT0011
CQ6539                   MOVE  NUM-ID-BLQ      TO  NUM-ID-SBLQ          STT0011
CQ6530                   MOVE  PR-APELLIDO-WK  TO  PR-APELLIDO-SLD
CQ6530                   MOVE  SG-APELLIDO-WK  TO  SG-APELLIDO-SLD
CQ6530                   MOVE  PR-NOMBRE-WK    TO  PR-NOMBRE-SLD
CQ6530                   MOVE  SG-NOMBRE-WK    TO  SG-NOMBRE-SLD
CQ6539                   MOVE  W-SCORE         TO  SCORE-SBLQ           STT0011
CQ6539                   MOVE  W-RAZON1        TO  RAZON1-SBLQ          STT0011
CQ6539                   MOVE  W-RAZON2        TO  RAZON2-SBLQ          STT0011
CQ6539                   MOVE  W-RAZON3        TO  RAZON3-SBLQ          STT0011
CQ6539                   MOVE  W-RAZON4        TO  RAZON4-SBLQ          STT0011
CQ6539                   MOVE  EDAD-BLQ        TO  SLD-EDAD-APROX-B     STT0011
CQ6539                   MOVE  EDAD-RANGO-VAL  TO  SLD-RANGO-EDAD-B     STT0011
CQ6539                   MOVE  FEC-EXP-BLQ     TO  SLD-FEC-EXP-SBLQ     STT0011
CQ6539                   MOVE  MUN-EXP-BLQ     TO  SLD-MUN-EXP-SBLQ     STT0011
CQ6539                   MOVE  DEP-EXP-BLQ     TO  SLD-DEP-EXP-SBLQ     STT0011
CQ6539                   MOVE  VIG-BLQ         TO  SLD-VIG-SBLQ         STT0011
CQ6539                   MOVE  GENERO-BLQ      TO  SLD-GENERO-SBLQ      STT0011
CQ6539                   MOVE  ADICIONAL-BLQ   TO  SLD-RELLENO-B        STT0011
CQ6539*                  WRITE  REG-SALIDA  FROM  SLD-SCORE-BLQ         STT0011
CQ6539                   MOVE  SLD-SCORE-BLQ   TO  W-AREA-PRODUCTO      STT0011
                  WHEN = "AWK"
                         MOVE  TIP-ID-AWK      TO  SLD-TIP-ID-AWK
                         MOVE  NUM-ID-AWK      TO  SLD-NUM-ID-AWK
                         MOVE  NOMBRE-AWK      TO  SLD-NOMBRE-AWK
                         MOVE  CALIFI-AWK      TO  SLD-CALIFI-AWK
                         MOVE  W-SCORE         TO  SCORE-AWK
                         MOVE  W-RAZON1        TO  RAZON1-AWK
                         MOVE  W-RAZON2        TO  RAZON2-AWK
                         MOVE  W-RAZON3        TO  RAZON3-AWK
                         MOVE  W-RAZON4        TO  RAZON4-AWK
R7129D*                  WRITE  REG-SALIDA  FROM  SLD-SCORE-AWK         RMB0010
R7129D                   MOVE SLD-SCORE-AWK    TO  W-AREA-PRODUCTO      RMB0010
                  WHEN = "PUN"
                         MOVE  TIPO-ID-NVAL   TO  NVAL-TIPO-ID
                         MOVE  NUME-ID-NVAL   TO  NVAL-NUME-ID
                         MOVE  NOMBRE-NVAL    TO  NVAL-NOMBRE
                         MOVE  W-SCORE        TO  NVAL-PUNTAJE
R7129D*                  WRITE  REG-SALIDA  FROM  SLD-SCORE-NVAL        RMB0010
R7129D                   MOVE SLD-SCORE-NVAL   TO  W-AREA-PRODUCTO      RMB0010
                  WHEN = "PLA"
                         MOVE  TIP-ID-VAL      TO  PLA-TIP-ID
                         MOVE  NUM-ID-VAL      TO  PLA-NUM-ID
                         MOVE  NOMBRE-VAL      TO  PLA-NOMBRE
                         MOVE  FEC-EXP-VAL     TO  PLA-FEC-EXP-VAL
                         MOVE  MUN-EXP-VAL     TO  PLA-MUN-EXP-VAL
                         MOVE  DEP-EXP-VAL     TO  PLA-DEP-EXP-VAL
                         MOVE  VIG-VAL         TO  PLA-VIG-VAL
                         MOVE  GENERO-VAL      TO  PLA-GENERO-VAL
                         MOVE  EDAD-VAL        TO  PLA-EDAD-APROX
                         MOVE  EDAD-RANGO-VAL  TO  PLA-RANGO-EDAD
                         MOVE   ";"            TO  PLA-FILLER-SCR
                         MOVE  W-SCORE         TO  PLA-SCORE
                         MOVE  W-RAZON1        TO  PLA-RAZON1
                         MOVE  W-RAZON2        TO  PLA-RAZON2
                         MOVE  W-RAZON3        TO  PLA-RAZON3
                         MOVE  W-RAZON4        TO  PLA-RAZON4
                         MOVE  ADICIONAL-VAL   TO  PLA-DATOS
R7129D*                  WRITE  REG-SALIDA  FROM  PLA-SCORE-VALID       RMB0010
R7129D                   MOVE PLA-SCORE-VALID  TO  W-AREA-PRODUCTO      RMB0010
                  WHEN = "EDA"
                         MOVE  TIP-ID-VAL      TO  TIP-ID-EDAD
                         MOVE  NUM-ID-VAL      TO  NUM-ID-EDAD
                         MOVE  NOMBRE-VAL      TO  NOMBRE-EDAD
                         MOVE  W-SCORE         TO  SCORE-EDAD
                         MOVE  W-RAZON1        TO  RAZON1-EDAD
                         MOVE  W-RAZON2        TO  RAZON2-EDAD
                         MOVE  W-RAZON3        TO  RAZON3-EDAD
                         MOVE  W-RAZON4        TO  RAZON4-EDAD
                         MOVE  EDAD-VAL        TO  APROXIM-EDAD
                         MOVE  EDAD-RANGO-VAL  TO  RANGO-APROX-EDAD
R7129D*                  WRITE  REG-SALIDA  FROM   EDA-SCORE-VALID      RMB0010
R7129D                   MOVE EDA-SCORE-VALID  TO  W-AREA-PRODUCTO      RMB0010
           END-EVALUATE.
R7129D     PERFORM 100-ESCRIBA-COMBO.                                   RMB0010

       0500-ESCRIBE-SALIDA-QUANTO.
      *****************************************************************
      *0500-ESCRIBE-SALIDA-QUANTO.                                    *
      *****************************************************************
R7129D*    INITIALIZE  REG-SALIDA                                       RMB0010
           INITIALIZE SLD-SCORE-QUANTO
           INSPECT SLD-SCORE-QUANTO REPLACING ALL SPACES BY ";"
QUAN01     MOVE TIPO-ID-CARACT TO SLD-TIPO-ID-QUANTO                    RMB0002
QUAN01     MOVE NRO-ID-CARACT  TO SLD-NRO-ID-QUANTO                     RMB0002
QUAN01     MOVE INGRESO-PESOS-INF-USAPI TO SLD-MIN-MIL-QUANTO           RMB0002
QUAN01     MOVE INGRESO-PESOS-USAPI     TO SLD-PROM-MIL-QUANTO          RMB0002
QUAN01     MOVE INGRESO-PESOS-SUP-USAPI TO SLD-MAX-MIL-QUANTO           RMB0002
QUAN01     MOVE INGRESO-INF-USAPI       TO SLD-MIN-SMLV-QUANTO          RMB0002
QUAN01     MOVE INGRESO-USAPI           TO SLD-PROM-SMLV-QUANTO         RMB0002
QUAN01     MOVE INGRESO-SUP-USAPI       TO SLD-MAX-SMLV-QUANTO          RMB0002
PAM379     INSPECT SLD-MIN-SMLV-QUANTO REPLACING ALL "." BY ","
PAM379     INSPECT SLD-PROM-SMLV-QUANTO REPLACING ALL "." BY ","
PAM379     INSPECT SLD-MAX-SMLV-QUANTO REPLACING ALL "." BY ","
QUAN01     MOVE ADVERSE-RAZON-USAPI(1)  TO SLD-RAZON-QUANTO             RMB0002
PAM379     IF INGRESO-PESOS-USAPI > ZEROS
PAM379        ADD 1 TO W-QUANTO
PAM379     END-IF
R7129D*    WRITE REG-SALIDA FROM SLD-SCORE-QUANTO.                      RMB0010
R7129D     MOVE SLD-SCORE-QUANTO TO W-AREA-PRODUCTO.                    RMB0010
R7129D     PERFORM 100-ESCRIBA-COMBO.                                   RMB0010

       1000-CERRAR-ARCHIVOS.
      *****************************************************************
      *1000-CERRAR-ARCHIVOS.                                          *
      * Rutina que  cierra los archivos utilizados en el proceso      *
      *****************************************************************
           CLOSE  BDIIREGI
           CLOSE  MAESTRO-DIRECC
           CLOSE  NOAMNISTIA21
           CLOSE  MAESTRO-CHECK
CARDEM     CLOSE  BDLSTR                                                GDT0007
CARDEM     CLOSE  BDIIDIAN                                              GDT0007
CARDEM*                                                                 GDT0007
           CLOSE  MAESTRO-CREDIT
           CLOSE  SUSCRIPTOR
           CLOSE  INDEXADO
           CLOSE  BDIEMSUP
           CLOSE  ENTRADA
R7129D*    CLOSE  SALIDA                                                RMB0010
R7129A     MOVE ZEROS TO REG-ULTIMO                                     RMB0010
R7129A     REWRITE REG-ULTIMO INVALID KEY                               RMB0010
R7129A       DISPLAY "ERROR DE LOGICA !!!! EN REWRITE ULTIMO ZEROS"     RMB0010
R7129A     END-REWRITE                                                  RMB0010
R7129A     CLOSE ULTIMO.                                                RMB0010
R7129A     DISPLAY "===== CONTADORES FINALES    ====".                  RMB0010
R7129A     PERFORM 1000-DESPLEGAR-ESTADISTICAS.                         RMB0010
R7129A     DISPLAY "================================".                  RMB0010
R7129A     DISPLAY "SE LEYERON EN TOTAL: " W-LEIDOS .                   RMB0010
R7129A 1000-DESPLEGAR-ESTADISTICAS.                                     RMB0010
R7129A***************************************************************** RMB0010
R7129A*1000-DESPLEGAR-ESTADISTICAS.                                     RMB0010
R7129A*    SE DESPLIEGAN LOS CONTADORES DEL PROCESO                     RMB0010
R7129A***************************************************************** RMB0010
R7129D     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9                    RMB0010
R7129D       IF W-SCORE-COMBO(I) <> 0                                   RMB0010
R7129D          EVALUATE W-SCORE-COMBO(I)                               RMB0010
                 WHEN = 1
                    DISPLAY "Clientes con PREDICTA mayor a ceros: "
                                                       W-PREDICTA
                 WHEN = 4
                    DISPLAY "Clientes con ACIERTA  mayor a ceros: "
                                                       W-ACIERTA
                 WHEN = 6
                    DISPLAY "Clientes con BASICO   mayor a ceros: "
                                                       W-BASICO
                 WHEN = 8
                    DISPLAY "Clientes con ACIERTA-M mayor a ceros: "
                                                       W-ACIERTA-M
PAM379           WHEN = 62
PAM379              DISPLAY "Clientes con QUANTO    mayor a ceros: "
PAM379                                                 W-QUANTO
PRHD01           WHEN = 67                                              RMB0002
PRHD01              DISPLAY "Clientes con ACIERTA+  mayor a ceros: "    RMB0002
PRHD01                                              W-PREDICTA-HD       RMB0002
ACIEAA           WHEN = 49                                              STT0012
ACIEAA              DISPLAY "Clientes con ACIERTA-A mayor a ceros: "    STT0012
ACIEAA                                              W-ACIERTA-A         STT0012
ACIEFI           WHEN = 47                                              RMB0012
ACIEFI              DISPLAY "Clientes con ACIA-DFGE mayor a ceros: "    RMB0012
ACIEFI                                              W-ACIERTA-A-DFGE    RMB0012
ACIEFI           WHEN = 48                                              RMB0012
ACIEFI              DISPLAY "Clientes con ACIA-DTRO mayor a ceros: "    RMB0012
ACIEFI                                              W-ACIERTA-A-DTRO    RMB0012
ACIEFI           WHEN = 95                                              RMB0012
ACIEFI              DISPLAY "Clientes con ACIA-DINT mayor a ceros: "    RMB0012
ACIEFI                                              W-ACIERTA-A-DINT    RMB0012
ACIEFI           WHEN = 41                                              RMB0012
ACIEFI              DISPLAY "Clientes con ACIA-DHVT mayor a ceros: "    RMB0012
ACIEFI                                              W-ACIERTA-A-DHVT    RMB0012
ACIEFI           WHEN = 45                                              RMB0012
CQ7550              DISPLAY "Clientes con ACIA-DCOO mayor a ceros: "    PJV0013
ACIEFI                                              W-ACIERTA-A-DCOO    RMB0012
R15689           WHEN = 119                                             STT0014
R15689              DISPLAY "Clientes con MICRO-B   mayor a ceros: "    STT0014
R15689                                              W-MICROCREDITO-S    STT0014
R15689           WHEN = 120                                             STT0014
R15689              DISPLAY "Clientes con MICRO-E   mayor a ceros: "    STT0014
R15689                                              W-MICROCREDITO-S    STT0014
R7129D           WHEN = 99                                              RMB0010
R7129D              DISPLAY "Clientes con CARACAT   mayor a ceros: "    RMB0010
R7129D                                              W-CARACTERISTICAS   RMB0010
R7129D          END-EVALUATE                                            RMB0010
R7129D       END-IF                                                     RMB0010
R7129D     END-PERFORM.                                                 RMB0010
R7129C 9000-GENERAR-CARACTERISTICAS.                                    RMB0010
R7129C***************************************************************** RMB0010
R7129C*9000-GENERAR-CARACTERISTICAS.                                    RMB0010
R7129C*    RUTINA QUE GENERA LAS CARACTERISTICAS USA DE NIVEL 01 Y 02   RMB0010
R7129C*    SE EXCLUYEN LAS DE ENDEUDAMIENTO GLOBAL (01SUPXXX y 02SUPXXX)RMB0010
R7129C***************************************************************** RMB0010
R7129C     ADD 1 TO W-CARACTERISTICAS.                                  RMB0010
R7129C     IF W-REINICIO <> "SI" AND SW-CAR-OPEN = 0                    RMB0010
R7129C        MOVE 1 TO SW-CAR-OPEN                                     RMB0010
R7129C        PERFORM 9000-ARMAR-NOMBRES-ARCH-CAR                       RMB0010
R7129C        PERFORM 9000-ABRIR-OUTPUT-ARCH-CAR                        RMB0010
R7129C        PERFORM 9000-WRITE-HEADER-CAR                             RMB0010
R7129C     END-IF                                                       RMB0010
R7129C     IF W-REINICIO =  "SI" AND SW-CAR-OPEN = 0                    RMB0010
R7129C        MOVE 1 TO SW-CAR-OPEN                                     RMB0010
R7129C        PERFORM 9000-ARMAR-NOMBRES-ARCH-CAR                       RMB0010
R7129C        PERFORM 9000-ABRIR-APPEND-ARCH-CAR                        RMB0010
R7129C     END-IF                                                       RMB0010
R7129C     MOVE ENT-TIPO-ID TO TIPIDE-CAR.                              RMB0010
R7129C     MOVE ENT-NUME-ID TO NUMIDE-CAR.                              RMB0010
*
R7129C     WRITE REG-00DEM-SALIDA FROM VAR-00DEM.                       RMB0010
*
R7129C     IF 01INQNRO NOT = ZEROS                                      RMB0010
R7129C        WRITE REG-01INQ-SALIDA FROM VAR-01INQ                     RMB0010
R7129C     END-IF.                                                      RMB0010
*
R7129C     WRITE REG-01DEM-SALIDA FROM VAR-01DEM.                       RMB0010
*
R7129C     MOVE 1 TO I                                                  RMB0010
R7129C     PERFORM UNTIL I > 9                                          RMB0010
R7129C       IF 01NUMPTF(I) <> ZEROS                                    RMB0010
R7129C           MOVE  TABLA-PORTAFOLIOS-01NUM(I) TO TEXT-CAR           RMB0010
R7129C           WRITE REG-01NUM-SALIDA FROM REG2-CAR                   RMB0010
R7129C       END-IF                                                     RMB0010
R7129C       IF 01MORPTF(I) <> ZEROS                                    RMB0010
R7129C          MOVE  01MOR-EDITADOS(I)          TO TEXT-CAR            RMB0010
R7129C          WRITE REG-01MOR-SALIDA FROM REG2-CAR                    RMB0010
R7129C       END-IF                                                     RMB0010
R7129C       IF 01ACPPTF(I) <> ZEROS                                    RMB0010
R7129C          MOVE  TABLA-PORTAFOLIOS-01ACP(I) TO TEXT-CAR            RMB0010
R7129C          WRITE REG-01ACP-SALIDA FROM REG2-CAR                    RMB0010
R7129C       END-IF                                                     RMB0010
R7129C       IF 01EXPPTF(I) <> ZEROS                                    RMB0010
R7129C          MOVE  TABLA-PORTAFOLIOS-01EXP(I) TO TEXT-CAR            RMB0010
R7129C          MOVE SPACES TO REG2-CAR(61:)                            RMB0010
R7129C          WRITE REG-01EXP-SALIDA FROM REG2-CAR                    RMB0010
R7129C       END-IF                                                     RMB0010
R7129C       IF 01ENDPTF(I) <> ZEROS                                    RMB0010
R7129C          MOVE  01END-EDITADOS(I)          TO TEXT-CAR            RMB0010
R7129C          WRITE REG-01END-SALIDA FROM REG2-CAR                    RMB0010
R7129C       END-IF                                                     RMB0010
R7129C       IF 02EXPPTF(I) NOT = ZEROS AND I NOT = 7                   RMB0010
R7129C          MOVE  VAR-02EXP-SALIDA(I)        TO TEXT-CAR            RMB0010
R7129C          WRITE REG-02EXP-SALIDA FROM REG2-CAR                    RMB0010
R7129C       END-IF                                                     RMB0010
R7129C       IF 02ENDPTF(I) NOT = ZEROS                                 RMB0010
R7129C          MOVE  TABLA-02END(I) TO TEXT-CAR                        RMB0010
R7129C          WRITE REG-02END-SALIDA FROM REG2-CAR                    RMB0010
R7129C       END-IF                                                     RMB0010
R7129C       ADD 1 TO I                                                 RMB0010
R7129C     END-PERFORM.                                                 RMB0010
*
R7129C     PERFORM 9000-GRABAR-02EXPTO.                                 RMB0010
*
R7129C     PERFORM 9000-GRABAR-02ENDCB.                                 RMB0010
*
R7129C     PERFORM 9000-GRABAR-02NUM.                                   RMB0010
*
R7129C     PERFORM 9000-GRABAR-02ACP.                                   RMB0010
*
R7129C     PERFORM 9000-GRABAR-02INQ.                                   RMB0010
R7129C 9000-GRABAR-02INQ.                                               RMB0010
R7129C***************************************************************** RMB0010
R7129C*9000-GRABAR-02INQ.                                               RMB0010
R7129C*    NIVEL 02 DE TIPOLOGIA INQ                                    RMB0010
R7129C*    SI HAY HUELLAS DE CONSULTA SE GENERA EL REGISTRO 02INQ       RMB0010
R7129C***************************************************************** RMB0010
R7129C     IF 01INQNRO NOT = ZEROS                                      RMB0010
R7129C        MOVE  VAR-02INQ                  TO TEXT-CAR              RMB0010
R7129C        WRITE REG-02INQ-SALIDA FROM REG2-CAR                      RMB0010
R7129C     END-IF.                                                      RMB0010
R7129C 9000-GRABAR-02EXPTO.                                             RMB0010
R7129C***************************************************************** RMB0010
R7129C*9000-GRABAR-02EXPTO.                                             RMB0010
R7129C*    NIVEL 02 TIPOLOGIA EXP PORTAFOLIO "TO"                       RMB0010
R7129C*    SE GENERA UN REGISTRO ADICIONAL A LOS PORTAFOLIOS NORMALES   RMB0010
R7129C*    PARA EL PORTAFOLIO "TO" DE TOTALES                           RMB0010
R7129C***************************************************************** RMB0010
R7129C     MOVE "TO" TO PORTAF-CAR                                      RMB0010
R7129C     MOVE VAR-02EXP-TOTAL(3:) TO PTEXT-CAR                        RMB0010
R7129C     WRITE REG-02EXP-SALIDA FROM REG2-CAR.                        RMB0010
R7129C 9000-GRABAR-02ACP.                                               RMB0010
R7129C***************************************************************** RMB0010
R7129C*9000-GRABAR-02ACP.                                               RMB0010
R7129C*    NIVEL 02 TIPOLOGIA ACP PORTAFOLIO "TO"                       RMB0010
R7129C*    SI HAY INFORMACION DE TIPOLOGIA ACP SE GENERA EL REGISTRO    RMB0010
R7129C***************************************************************** RMB0010
R7129C     IF 02ACPPTF NOT = ZEROS                                      RMB0010
R7129C         MOVE VAR-02ACP TO TEXT-CAR                               RMB0010
R7129C         WRITE REG-02ACP-SALIDA FROM REG2-CAR                     RMB0010
R7129C     END-IF.                                                      RMB0010
R7129C 9000-GRABAR-02ENDCB.                                             RMB0010
R7129C***************************************************************** RMB0010
R7129C*    NIVEL 02 TIPOLOGIA END PORTAFOLIO "CB"                       RMB0010
R7129C*    SE GENERA UN REGISTRO ADICIONAL A LOS PORTAFOLIOS NORMALES   RMB0010
R7129C*    PARA EL PORTAFOLIO "CB"                                      RMB0010
R7129C***************************************************************** RMB0010
R7129C     IF 02ENDPTFCB = "CB"                                         RMB0010
R7129C        MOVE VAR-02END-CB   TO TEXT-CAR                           RMB0010
R7129C        MOVE "        -99" TO        TEXT-CAR       (388:11)      RMB0010
R7129C             TEXT-CAR       (399:11) TEXT-CAR       (410:11)      RMB0010
R7129C             TEXT-CAR       (421:11) TEXT-CAR       (432:11)      RMB0010
R7129C             TEXT-CAR       (443:11) TEXT-CAR       (454:11)      RMB0010
R7129C             TEXT-CAR       (465:11) TEXT-CAR       (476:11)      RMB0010
R7129C             TEXT-CAR       (487:11) TEXT-CAR       (498:11)      RMB0010
R7129C             TEXT-CAR       (509:11)                              RMB0010
R7129C          WRITE REG-02END-SALIDA FROM REG2-CAR                    RMB0010
R7129C     END-IF.                                                      RMB0010
R7129C 9000-GRABAR-02NUM.                                               RMB0010
R7129C***************************************************************** RMB0010
R7129C*    NIVEL 02 TIPOLOGIA NUM                                       RMB0010
R7129C*    SE GENERA REGISTROS POR CADA PORTAFOLIO MAS LOS PORTAFOLIOS  RMB0010
R7129C*    "CB" y "MX" . DADO QUE EN LA DEFINICION CADA REGISTRO        RMB0010
R7129C*    CONTIENE LOS 11 PORTAFOLIOS, ES NECESARIO MOVER "-99" A LOS  RMB0010
R7129C*    QUE NO APLICA POR REGISTRO.                                  RMB0010
R7129C***************************************************************** RMB0010
R7129C     PERFORM 9500-INICIALIZAR-02END.                              RMB0010
R7129C     MOVE "TO" TO  PORTAF-CAR                                     RMB0010
R7129C     MOVE VAR-02NUM-TO TO TO-02END-AUX.                           RMB0010
R7129C     MOVE  REG-02END-AUX TO PTEXT-CAR.                            RMB0010
R7129C     WRITE REG-02NUM-SALIDA FROM REG2-CAR                         RMB0010
*
R7129C     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9                    RMB0010
R7129C      PERFORM 9500-INICIALIZAR-02END                              RMB0010
R7129C      IF 01NUMPTF(I) NOT = ZEROS                                  RMB0010
R7129C         EVALUATE I                                               RMB0010
R7129C           WHEN 1                                                 RMB0010
R7129C             MOVE "RO" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 2                                                 RMB0010
R7129C             MOVE "IN" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 3                                                 RMB0010
R7129C             MOVE "VE" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 4                                                 RMB0010
R7129C             MOVE "HP" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 5                                                 RMB0010
R7129C             MOVE "OT" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 6                                                 RMB0010
R7129C             MOVE "CC" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 7                                                 RMB0010
R7129C             MOVE "CO" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 8                                                 RMB0010
R7129C             MOVE "AH" TO PORTAF-CAR                              RMB0010
R7129C           WHEN 9                                                 RMB0010
R7129C             MOVE "CT" TO PORTAF-CAR                              RMB0010
R7129C         END-EVALUATE                                             RMB0010
R7129C         MOVE TABLA-02NUM(I) TO XX-02END-AUX                      RMB0010
R7129C         MOVE  REG-02END-AUX TO PTEXT-CAR                         RMB0010
R7129C         WRITE REG-02NUM-SALIDA FROM REG2-CAR                     RMB0010
R7129C      END-IF                                                      RMB0010
R7129C     END-PERFORM                                                  RMB0010
*
R7129C     PERFORM 9500-INICIALIZAR-02END                               RMB0010
R7129C     MOVE "CB" TO  PORTAF-CAR                                     RMB0010
R7129C     MOVE VAR-02NUM-CB TO XX-02END-AUX                            RMB0010
R7129C     MOVE  REG-02END-AUX TO PTEXT-CAR                             RMB0010
R7129C     WRITE REG-02NUM-SALIDA FROM REG2-CAR                         RMB0010
*
R7129C     PERFORM 9500-INICIALIZAR-02END                               RMB0010
R7129C     MOVE "MX" TO  PORTAF-CAR                                     RMB0010
R7129C     MOVE VAR-02NUM-MX TO MX-02END-AUX                            RMB0010
R7129C     MOVE  REG-02END-AUX TO PTEXT-CAR                             RMB0010
R7129C     WRITE REG-02NUM-SALIDA FROM REG2-CAR.                        RMB0010
R7129C 9500-INICIALIZAR-02END.                                          RMB0010
R7129C***************************************************************** RMB0010
R7129C*    SE INICIALIZA EL AREA TOTAL DEL REGISTRO CON VALORES -99     RMB0010
R7129C***************************************************************** RMB0010
R7129C     MOVE ALL   " -99" TO TO-02END-AUX. *> AREA PARA PORTAF "TO"  RMB0010
R7129C     MOVE ALL "   -99" TO XX-02END-AUX. *> PORT RO,VE,HP....CB    RMB0010
R7129C     MOVE ALL "   -99" TO MX-02END-AUX. *> AREA PARA PORTAF "MX"  RMB0010
R7129C 9000-ARMAR-NOMBRES-ARCH-CAR.                                     RMB0010
R7129C***************************************************************** RMB0010
R7129C*9000-ARMAR-NOMBRES-ARCH-CAR.                                     RMB0010
R7129C*    SE CONFORMAN LOS NOMBRES DE CADA ARCHIVO DE CARACTERISTICAS  RMB0010
R7129C*    CON BASE AL NIVEL,TIPOLOGIA,FECHA Y NOMBRE-ARCHIVO           RMB0010
R7129C***************************************************************** RMB0010
R7129C     MOVE SPACES TO W-CAR-NOMBRE.                                 RMB0010
R7129C     STRING W-ENT-NOMBRE DELIMITED BY "."                         RMB0010
R7129C            INTO W-CAR-NOMBRE                                     RMB0010
R7129C     END-STRING                                                   RMB0010
R7129C     MOVE W-FECHA-BASE TO FECHA-CAR.                              RMB0010
R7129C     MOVE W-CAR-NOMBRE TO                                         RMB0010
R7129C          NOM-ARCH-00DEM                                          RMB0010
R7129C          NOM-ARCH-01NUM                                          RMB0010
R7129C          NOM-ARCH-01INQ                                          RMB0010
R7129C          NOM-ARCH-01MOR                                          RMB0010
R7129C          NOM-ARCH-01ACP                                          RMB0010
R7129C          NOM-ARCH-01EXP                                          RMB0010
R7129C          NOM-ARCH-01DEM                                          RMB0010
R7129C          NOM-ARCH-01END                                          RMB0010
R7129C          NOM-ARCH-02NUM                                          RMB0010
R7129C          NOM-ARCH-02ACP                                          RMB0010
R7129C          NOM-ARCH-02EXP                                          RMB0010
R7129C          NOM-ARCH-02END                                          RMB0010
R7129C          NOM-ARCH-02INQ.                                         RMB0010
R7129C     MOVE W-FECHA-BASE TO                                         RMB0010
R7129C          FECH-00DEM                                              RMB0010
R7129C          FECH-01NUM                                              RMB0010
R7129C          FECH-01INQ                                              RMB0010
R7129C          FECH-01MOR                                              RMB0010
R7129C          FECH-01ACP                                              RMB0010
R7129C          FECH-01EXP                                              RMB0010
R7129C          FECH-01DEM                                              RMB0010
R7129C          FECH-01END                                              RMB0010
R7129C          FECH-02NUM                                              RMB0010
R7129C          FECH-02ACP                                              RMB0010
R7129C          FECH-02EXP                                              RMB0010
R7129C          FECH-02END                                              RMB0010
R7129C          FECH-02INQ.                                             RMB0010
R7129C 9000-ABRIR-OUTPUT-ARCH-CAR.                                      RMB0010
R7129C***************************************************************** RMB0010
R7129C*9000-ABRIR-OUTPUT-ARCH-CAR.                                      RMB0010
R7129C*    EN CASO DE NO REINICIO SE COMIENZAN EN VACIO LOS ARCHIVOS DE RMB0010
R7129C*    CARACTERISTICAS                                              RMB0010
R7129C***************************************************************** RMB0010
R7129C     OPEN OUTPUT                                                  RMB0010
R7129C          SALIDA-00DEM                                            RMB0010
R7129C          SALIDA-01NUM                                            RMB0010
R7129C          SALIDA-01INQ                                            RMB0010
R7129C          SALIDA-01MOR                                            RMB0010
R7129C          SALIDA-01ACP                                            RMB0010
R7129C          SALIDA-01EXP                                            RMB0010
R7129C          SALIDA-01DEM                                            RMB0010
R7129C          SALIDA-01END                                            RMB0010
R7129C          SALIDA-02NUM                                            RMB0010
R7129C          SALIDA-02ACP                                            RMB0010
R7129C          SALIDA-02EXP                                            RMB0010
R7129C          SALIDA-02END                                            RMB0010
R7129C          SALIDA-02INQ.                                           RMB0010
R7129C 9000-ABRIR-APPEND-ARCH-CAR.                                      RMB0010
R7129C******************************************************************RMB0010
R7129C*9000-ABRIR-APPEND-ARCH-CAR.                                      RMB0010
R7129C*    PARA EL CASO DE REINICIO SE GRABA LA INFORMACION A           RMB0010
R7129C*    CONTINUACION DE LA ULTIMA REGISTRADA                         RMB0010
R7129C******************************************************************RMB0010
R7129C     OPEN EXTEND                                                  RMB0010
R7129C          SALIDA-00DEM                                            RMB0010
R7129C          SALIDA-01NUM                                            RMB0010
R7129C          SALIDA-01INQ                                            RMB0010
R7129C          SALIDA-01MOR                                            RMB0010
R7129C          SALIDA-01ACP                                            RMB0010
R7129C          SALIDA-01EXP                                            RMB0010
R7129C          SALIDA-01DEM                                            RMB0010
R7129C          SALIDA-01END                                            RMB0010
R7129C          SALIDA-02NUM                                            RMB0010
R7129C          SALIDA-02ACP                                            RMB0010
R7129C          SALIDA-02EXP                                            RMB0010
R7129C          SALIDA-02END                                            RMB0010
R7129C          SALIDA-02INQ.                                           RMB0010
R7129C 9000-WRITE-HEADER-CAR.                                           RMB0010
R7129C***************************************************************   RMB0010
R7129C*9000-WRITE-HEADER-CAR.                                           RMB0010
R7129C*    PARA CADA ARCHIVO QUE CONTIENE LAS CARACTERISTICAS SE GRABA  RMB0010
R7129C*    UNO INICIAL CON EL NIVEL,TIPO DE CARACTERISTICA Y FECHA      RMB0010
R7129C*    DE LA INFORMACION QUE VA EN CADA ARCHIVO                     RMB0010
R7129C***************************************************************   RMB0010
R7129C     MOVE     "00DEM" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-00DEM-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "01NUM" TO FIJO-CAR .                               RMB0010
R7129C     MOVE "0"         TO BASURA-CAR.                              RMB0010
R7129C     WRITE REG-01NUM-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE " "         TO BASURA-CAR.                              RMB0010
R7129C     MOVE     "01INQ" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-01INQ-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "01MOR" TO FIJO-CAR .                               RMB0010
R7129C     MOVE "0"         TO BASURA-CAR.                              RMB0010
R7129C     WRITE REG-01MOR-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE " "         TO BASURA-CAR.                              RMB0010
R7129C     MOVE     "01ACP" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-01ACP-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "01EXP" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-01EXP-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "01DEM" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-01DEM-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "01END" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-01END-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "02NUM" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-02NUM-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "02ACP" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-02ACP-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "02EXP" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-02EXP-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "02END" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-02END-SALIDA FROM REG1-CAR.                        RMB0010
R7129C     MOVE     "02INQ" TO FIJO-CAR .                               RMB0010
R7129C     WRITE REG-02INQ-SALIDA FROM REG1-CAR.                        RMB0010
R7129D*************************MANEJO DE COMBOS************************ RMB0010
R7129D 100-OPEN-EXTEND-COMBO.                                           RMB0010
R7129D    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9                     RMB0010
R7129D      IF W-SCORE-COMBO(I) > 0                                     RMB0010
R7129D         EVALUATE I                                               RMB0010
R7129D           WHEN 1                                                 RMB0010
R7129D              OPEN EXTEND SALIDA1                                 RMB0010
R7129D           WHEN 2                                                 RMB0010
R7129D              OPEN EXTEND SALIDA2                                 RMB0010
R7129D           WHEN 3                                                 RMB0010
R7129D              OPEN EXTEND SALIDA3                                 RMB0010
R7129D           WHEN 4                                                 RMB0010
R7129D              OPEN EXTEND SALIDA4                                 RMB0010
R7129D           WHEN 5                                                 RMB0010
R7129D              OPEN EXTEND SALIDA5                                 RMB0010
R7129D           WHEN 6                                                 RMB0010
R7129D              OPEN EXTEND SALIDA6                                 RMB0010
R7129D           WHEN 7                                                 RMB0010
R7129D              OPEN EXTEND SALIDA7                                 RMB0010
R7129D           WHEN 8                                                 RMB0010
R7129D              OPEN EXTEND SALIDA8                                 RMB0010
R7129D           WHEN 9                                                 RMB0010
R7129D              OPEN EXTEND SALIDA9                                 RMB0010
R7129D         END-EVALUATE                                             RMB0010
R7129D      END-IF                                                      RMB0010
R7129D    END-PERFORM.                                                  RMB0010
R7129D 100-ESCRIBA-COMBO.                                               RMB0010
R7129D    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9                     RMB0010
R7129D      IF W-SCORE-COMBO(I) > 0                                     RMB0010
R7129D         IF W-SCORE-COMBO(I) = W-PRODUCTO                         RMB0010
R7129D            EVALUATE I                                            RMB0010
R7129D              WHEN 1                                              RMB0010
R7129D                WRITE REG-SALIDA1 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 2                                              RMB0010
R7129D                WRITE REG-SALIDA2 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 3                                              RMB0010
R7129D                WRITE REG-SALIDA3 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 4                                              RMB0010
R7129D                WRITE REG-SALIDA4 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 5                                              RMB0010
R7129D                WRITE REG-SALIDA5 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 6                                              RMB0010
R7129D                WRITE REG-SALIDA6 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 7                                              RMB0010
R7129D                WRITE REG-SALIDA7 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 8                                              RMB0010
R7129D                WRITE REG-SALIDA8 FROM W-AREA-PRODUCTO            RMB0010
R7129D              WHEN 9                                              RMB0010
R7129D                WRITE REG-SALIDA9 FROM W-AREA-PRODUCTO            RMB0010
R7129D            END-EVALUATE                                          RMB0010
R7129D         END-IF                                                   RMB0010
R7129D      END-IF                                                      RMB0010
R7129D    END-PERFORM.                                                  RMB0010
R7129D 100-OPEN-OUTPUT-COMBO.                                           RMB0010
R7129D    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9                     RMB0010
R7129D      IF W-SCORE-COMBO(I) > 0                                     RMB0010
R7129D         EVALUATE I                                               RMB0010
R7129D           WHEN 1                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA1                                 RMB0010
R7129D           WHEN 2                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA2                                 RMB0010
R7129D           WHEN 3                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA3                                 RMB0010
R7129D           WHEN 4                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA4                                 RMB0010
R7129D           WHEN 5                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA5                                 RMB0010
R7129D           WHEN 6                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA6                                 RMB0010
R7129D           WHEN 7                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA7                                 RMB0010
R7129D           WHEN 8                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA8                                 RMB0010
R7129D           WHEN 9                                                 RMB0010
R7129D              OPEN OUTPUT SALIDA9                                 RMB0010
R7129D         END-EVALUATE                                             RMB0010
R7129D      END-IF                                                      RMB0010
R7129D    END-PERFORM.                                                  RMB0010
R7129D 100-ASIGNAR-NOMBRE-SALIDA.                                       RMB0010
R7129D    EVALUATE I                                                    RMB0010
R7129D      WHEN 1                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE1                        RMB0010
R7129D      WHEN 2                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE2                        RMB0010
R7129D      WHEN 3                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE3                        RMB0010
R7129D      WHEN 4                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE4                        RMB0010
R7129D      WHEN 5                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE5                        RMB0010
R7129D      WHEN 6                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE6                        RMB0010
R7129D      WHEN 7                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE7                        RMB0010
R7129D      WHEN 8                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE8                        RMB0010
R7129D      WHEN 9                                                      RMB0010
R7129D        MOVE W-SLD-NOMBRE TO W-SLD-NOMBRE9                        RMB0010
R7129D    END-EVALUATE.                                                 RMB0010
R7129D 100-VALIDAR-COMBO.                                               RMB0010
R7129D     MOVE 1 TO I.                                                 RMB0010
R15689     IF HAY-COMBO-MICRO = 0                                       STT0014
R7129D     PERFORM UNTIL I > 9                                          RMB0010
ACIEAA*      IF W-SCORE-COMBO(I) = 00 OR 62 OR 67 OR 99                 STT0012
ACIEAA       IF W-SCORE-COMBO(I) = 00 OR 62 OR 67 OR 49 OR 99           STT0012
ACIEFI                       OR    47 OR 48 OR 95 OR 41 OR 45           RMB0012
ACIEFI          IF W-SCORE-COMBO(I) <> 00                               RMB0012
ACIEFI             DISPLAY "SCORE SERIE+ RECIBIDO :"  W-SCORE-COMBO(I)  RMB0012
ACIEFI             " POS: " I                                           RMB0012
ACIEFI          END-IF                                                  RMB0012
R7129D       ELSE                                                       RMB0010
R7129D          DISPLAY " EJECUCION CANCELADA POR COMBO INVALIDO!!"     RMB0010
R7129D          DISPLAY "[",W-TIPO-SCORE,"]"                            RMB0010
R7129D          STOP RUN                                                RMB0010
R7129D       END-IF                                                     RMB0010
R7129D       ADD 1 TO I                                                 RMB0010
R7129D     END-PERFORM.                                                 RMB0010
R15689 100-ANALIZAR-TIPOSCORE.                                          RMB0010
R15689     MOVE 0 TO HAY-COMBO-MICRO.                                   STT0014
R15689     IF W-TIPO-SCORE(7:) =  ZEROS                                 STT0014
R15689        IF (W-SCORE-COMBO(1) = 99 AND (W-SCORE-COMBO(2) = 119 OR  STT0014
R15689                                       W-SCORE-COMBO(2) = 120))   STT0014
R15689        OR ((W-SCORE-COMBO(1) = 119 OR 120) AND                   STT0014
R15689                                       W-SCORE-COMBO(2) = 99)     STT0014
R15689             DISPLAY "SCORE MICRO  RECIBIDO :"  W-SCORE-COMBO(1)  STT0014
R15689             " POS: " "001"                                       STT0014
R15689             DISPLAY "SCORE MICRO  RECIBIDO :"  W-SCORE-COMBO(2)  STT0014
R15689             " POS: " "002"                                       STT0014
R15689             MOVE 1 TO HAY-COMBO-MICRO                            STT0014
R15689        END-IF                                                    STT0014
R15689     END-IF.                                                      STT0014
R15689     IF HAY-COMBO-MICRO = 0
R15689        MOVE SPACES TO W-CAMPO-40
R15689        MOVE W-TIPO-SCORE TO W-CAMPO-40
R15689        INSPECT W-CAMPO-40 TALLYING  LONG-40
R15689                           FOR CHARACTERS BEFORE INITIAL "   "
R15689        IF LONG-40 = 1
R15689           MOVE "00" TO W-TIPO-SCORE(1:2)
R15689           MOVE W-CAMPO-40(1:1) TO W-TIPO-SCORE(3:1)
R15689        ELSE
R15689           IF LONG-40 = 2
R15689              MOVE "0" TO W-TIPO-SCORE(1:1)
R15689              MOVE W-CAMPO-40(1:2) TO W-TIPO-SCORE(2:2)
R15689           ELSE
R15689             IF (LONG-40 <> 3 AND LONG-40 < 27)
R15689              IF W-CAMPO-40(1:1) > "0" AND W-CAMPO-40(1:3) > "0"
R15689                 MOVE 1 TO W-POS-40
R15680                 MOVE 1 TO W-POS-TS
R15689                 PERFORM UNTIL W-CAMPO-40(W-POS-40:1) = " "
R15689                    MOVE "0" TO W-TIPO-SCORE(W-POS-TS:1)
R15680                    ADD  1 TO W-POS-TS
R15689                    MOVE W-CAMPO-40(W-POS-40:2) TO
R15689                         W-TIPO-SCORE(W-POS-TS:2)
R15689                    ADD 2 TO W-POS-40
R15689                    ADD 2 TO W-POS-TS
R15689                 END-PERFORM
R15689              END-IF
R15689             END-IF
R15689           END-IF
R15689        END-IF
R15689     END-IF.

       COPY  SCOT16.PROC.
       COPY  SCOT16.PROC1.
       COPY  BDIIVIDE.PROC.
       COPY  UTLBIT.PROC.
       COPY  VALVLR.PROC.
       COPY  LEEMAESTROS.PROC.
       COPY  BDSCORES.PROC.
QUAN01 COPY  CARACT.PROC.                                               RMB0002
QUAN01 COPY  CARACT.PROC1.                                              RMB0002
QUAN01 COPY  SCOUSA.PROC.                                               RMB0002
QUAN01 COPY  LEEICMVLR.PROC.                                            RMB0002
/d/iccol/desarrollo/fuentes>

