      * PESVNOCSV.V.01.0011
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PESVNO.
       AUTHOR. DATACREDITO.
       DATE-WRITTEN. MAYO/2001.
       DATE-COMPILED.
      ******************************************************************
      * PROGRAMA PARA VALIDACION DE CEDULAS, NITS Y CEDULAS DE         *
      * EXTRANJERIA.
      *
      * Programa: PESVNOCSV.CBL   BASE ==> $DATABASE/BDACSV
      *
      * Cliente : Datacredito
      *
      * Version : 09/06/2008 09:08:25 a.m.
      ******************************************************************
      *  Validacion especial tomando el archivo de
      *  Cuentas Sin Validar para poder reportalos en el informe final
      *
      ******************************************************************
REMARK* LAG0002      * PAM372 - SEPARADORES ";" EN INCONSISTENCIAS
REMARK* L.GOMEZ      * PARA IGUALARLO AL PESVNO
REMARK* SEPT. 30/2009*
REMARK*******************************************************************
REMARK* GJC0003      * CSVCEX - AJUSTE A LA VALIDACION PARA CUENTAS SIN*
REMARK* 2009/12/21   * VALIDAR CON CEDULA DE EXTRANJERIA.              *
REMARK* GRACIELA JIM.*                                                 *
REMARK******************************************************************
REMARK* LAG0004      * REQ550 - AJUSTE A LA VALIDACION PARA CUENTAS SIN*
REMARK* 2010/03/12   * VALIDAR INCLUYENDO VALIDACION DE RANGOS.        *
REMARK* G.DESARROLLO *                                                 *
REMARK******************************************************************
      ******************************************************************
      * GDT0005    | CTAST8 - CAMBIO DEL ICMCHK (MAESTRO DE CUENTAS) A *
      * FEB.09-2010|    FORMATO TIPO 8.                                *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * EHA0006    | MODCED - MODIFICACION PARA EVITAR ERRORES EN LA   *
      * MAY.05-2010|    ESCRITURA DE REGISTROS                         *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * LAG0007    | REQ596 - CORRECCION PARA MANEJO DE RELLENO EN     *
      * OCT.07-2010|    EN TIPOS ID DIFERENTE A 1                      *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * LAG0008    | REQ609 - CORRECCION PARA VALIDACION DE NOMBRE     *
      * OCT.14-2010|    CUANDO TRAE DE BASE ALTERNA                    *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * RMB0009    | R7094A - OPCION DE NO VALIDAR NOMBRE VALIDAR=NO   *
      * AGO-06-2012| R7094B - DESPLEGAR CONTADOR DE FALLECIDOS         *
      * GER.DESARR.| R7094C - SI BLOQUES = 1 NO SACAR SOLO EL APELLIDO *
      *            | R7094D - QUITAR DIGITO DE CHEQUEO SOLO A NITS     *
      ******************************************************************
      * PJV0010    | CQ6271 - LOG DE REGISTROS PROCESADOS.             *
      * SEP-01-2012|                                                   *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * PJV011     | CQ11407 - HACE CONTROL DE IDENTIFICACIONES        *
      * ABR-02-2013| REPETIDAS VALIDAS.                                *
      * GER.DESARR.|                                                   *
      ******************************************************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
       INPUT-OUTPUT SECTION.
           COPY BDIIDIAN.FS.
           COPY BDIIREGI.FS.
           COPY BDIIDAS.FS.
           COPY ICBTCO.FS.
           COPY ICBCLA0001.FS.
           COPY ICBCIU.FS.
CTAST8*    COPY  ICBCHK.FS.                                             GDT0005
CTAST8     COPY  ICBCHK0001.FS.                                         GDT0005
           COPY ICBCRE.FS.
           COPY ICBSUS.FS.
           COPY BDLCLI.FS.
           COPY ICBDIR.FS.
           COPY ICMUNI.FS.
           COPY BDIIALE.FS.
           COPY BDLSTDCIU.FS.
           COPY BDLNEWDIRBOG.FS.
           COPY DIREST.FS.
           COPY NOAMNISTIA21.FS.
           COPY BDACSV.FS.

           SELECT ENTRADA  ASSIGN TO DISK W-ENTRADA
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL.

           SELECT VALIDAS  ASSIGN TO DISK W-VALIDAS
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL.

PJV011     SELECT VALIDAS-REP  ASSIGN TO DISK W-VALIDAS-REP             CQ11407
PJV011            ORGANIZATION IS LINE SEQUENTIAL                       CQ11407
PJV011            ACCESS MODE  IS SEQUENTIAL.                           CQ11407

           SELECT INCONS   ASSIGN TO DISK W-INVALIDAS
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY ICBCHK.FD.
       COPY ICBSUS.FD.
       COPY BDACSV.FD.
       COPY ICBDIR0001.FD.
       COPY BDLSTDCIU.FD.
       COPY DIREST.FD.
       COPY BDLNEWDIRBOG.FD.
       COPY NOAMNISTIA21.FD.
       COPY BDIIDIAN.FD.
       COPY BDIIREGI.FD.
       COPY BDIIDAS.FD.
       COPY ICBTCO001.FD.
       COPY ICBCLA.FD.
       COPY ICBCIU.FD.
       COPY ICBCRE.FD.
       COPY BDLCLI.FD.
       COPY ICMUNI.FD.
       COPY BDIIALE.FD.

       FD  ENTRADA.
       01  REG-ENTRADA.
           03 TIPOID-E                      PIC   9(01).
           03 TIPOID-X-E REDEFINES TIPOID-E PIC   X(01).
           03 NUMID-E                       PIC   9(11).
           03 NUMID-X-E REDEFINES NUMID-E   PIC   X(11).
           03 NOMBRE-E                      PIC   X(45).
           03 FILLER-2-E                    PIC   X(60).

       FD  VALIDAS.
       01  REG-VALIDAS.
           03 TIP-VAL                   PIC 9.
           03 CED-VAL                   PIC 9(11).
           03 NOM-VAL                   PIC X(45).
           03 FEC-EXP-VAL               PIC 9(08).
           03 MUN-EXP-VAL               PIC X(15).
           03 DEP-EXP-VAL               PIC X(15).
           03 VIG-VAL                   PIC X(02).
           03 GENERO-VAL                PIC X(01).
           03 EDAD-VAL                  PIC X(03).
           03 EDAD-RANGO                PIC X(05).
PAM460*    03 ORIGEN-VAL                PIC X.
PAM460     03 FILLER-2-VAL              PIC X(51).

PJV011 FD  VALIDAS-REP.                                                 CQ11407
PJV011 01  REG-VALIDAS-REP.                                             CQ11407
PJV011     03 TIP-VALREP                PIC 9.                          CQ11407
PJV011     03 CED-VALREP                PIC 9(11).                      CQ11407
PJV011     03 NOM-VALREP                PIC X(45).                      CQ11407
PJV011     03 FEC-EXP-VALREP            PIC 9(08).                      CQ11407
PJV011     03 MUN-EXP-VALREP            PIC X(15).                      CQ11407
PJV011     03 DEP-EXP-VALREP            PIC X(15).                      CQ11407
PJV011     03 VIG-VALREP                PIC X(02).                      CQ11407
PJV011     03 GENERO-VALREP             PIC X(01).                      CQ11407
PJV011     03 EDAD-VALREP               PIC X(03).                      CQ11407
PJV011     03 EDAD-RANGO-VALREP         PIC X(05).                      CQ11407
PJV011     03 FILLER-2-VALREP           PIC X(51).                      CQ11407

       FD  INCONS.
       01  REG-INCONS.
           03 TIP-INC                   PIC 9.
PAM372     03 FILLER1                   PIC X VALUE ";".
           03 CED-INC                   PIC 9(11).
PAM372     03 FILLER2                   PIC X VALUE ";".
           03 NOM1-INC                  PIC X(45).
PAM372     03 FILLER3                   PIC X VALUE ";".
           03 NOM2-INC                  PIC X(45).
PAM372     03 FILLER4                   PIC X VALUE ";".
           03 FEC-EXP-INC               PIC 9(08).
PAM372     03 FILLER5                   PIC X VALUE ";".
           03 MUN-EXP-INC               PIC X(15).
PAM372     03 FILLER6                   PIC X VALUE ";".
           03 DEP-EXP-INC               PIC X(15).
PAM372     03 FILLER7                   PIC X VALUE ";".
           03 VIG-INC                   PIC X(02).
PAM372     03 FILLER8                   PIC X VALUE ";".
           03 CODERR-INC                PIC 9(02).
PAM372     03 FILLER9                   PIC X VALUE ";".
           03 FILLER-2-INC              PIC X(60).

      ****************************************************
       WORKING-STORAGE SECTION.
      ***************************************************
CQ6271 COPY LOGPESVNO.WS.                                               PJV0010
       01 W-COMANDO-LINEAS.
          03 FILLER                     PIC X(06) VALUE "wc -l ".
          03 ENTRADA-LINEAS.
             05 FILLER                  PIC X(12) VALUE "$TEMPORALES/".
             05 W-LINEAS-NOM-ENT        PIC X(30) VALUE SPACES.
             05 FILLER                  PIC X(30)
             VALUE " | awk '{ print ($1) }'".
          03 FILLER                     PIC X(01) VALUE LOW-VALUE.
       01 W-COMANDO-SORT.
          03 FILLER                     PIC X(13) VALUE "sort -u ".
          03 ENTRADA-SORT.
             05 FILLER                  PIC X(12) VALUE "$TEMPORALES/".
             05 W-SORT-NOM-ENT          PIC X(30) VALUE SPACES.
          03 FILLER                     PIC X(13) VALUE " -o ".
          03 SALIDA-SORT.
             05 FILLER                  PIC X(12) VALUE "$TEMPORALES/".
             05 W-SORT-NOM-SAL          PIC X(30) VALUE SPACES.
          03 FILLER                     PIC X(01) VALUE LOW-VALUE.
      *--------------------------------------------------------------*
       01  W-ENTRADA.
           03 W-ENT-DIRECTORIO      PIC X(12) VALUE "$TEMPORALES/".
           03 W-ENT-NOMBRE          PIC X(30) VALUE SPACES.
       01  W-VALIDAS.
           03 W-VAL-DIRECTORIO      PIC X(12) VALUE "$TEMPORALES/".
           03 W-VAL-NOMBRE          PIC X(30) VALUE SPACES.
       01  W-INVALIDAS.
           03 W-INVAL-DIRECTORIO    PIC X(12) VALUE "$TEMPORALES/".
           03 W-INVAL-NOMBRE        PIC X(30) VALUE SPACES.
PJV011 01  W-VALIDAS-REP.                                               CQ11407
PJV011     03 W-VAL-DIRECTORIO      PIC X(12) VALUE "$TEMPORALES/".     CQ11407
PJV011     03 W-VAL-NOMBRE-REP      PIC X(30) VALUE SPACES.             CQ11407

PJV011 01  W-KEYVAL-GRA.                                                CQ11407
PJV011     03 TIP-KEYVAL            PIC 9(01) VALUE ZEROS.              CQ11407
PJV011     03 NUM-KEYVAL            PIC 9(11) VALUE ZEROS.              CQ11407
PJV011     03 SN-VALREP             PIC X(01) VALUE SPACES.             CQ11407

       01  W-PARAMETROS             PIC X(94) VALUE SPACES.
       01  W-TARIFA                 PIC 9(04) VALUE ZEROS.
       01  W-BLOQUES                PIC 9(01) VALUE ZEROS.
       01  SW-BDIIALE               PIC 9(01) VALUE ZEROS.
       01  W-DERECHOS               PIC 9     VALUE ZEROS.
       01  OPEN-DIR                 PIC 9     VALUE ZEROS.
       01  FIN-ENTRADA              PIC 9     VALUE 0.
       01  FINS                     PIC 9     VALUE 0.
       01  LLAVE7                   PIC 9(8)  VALUE ZEROS.
       01  LLAVE7-R  REDEFINES LLAVE7.
           03 LLAVE7-1D             PIC 9.
           03 LLAVE7-FILLER         PIC 9(7).
       01  TOPE-INI-LLAVE7          PIC 9(8)  VALUE 20000000.
       01  FECHA-PROC.
           03 ANO-HOY.
              05 SIG-PROC           PIC X(2).
              05 ANO-PROC           PIC X(2).
           03 MES-PROC              PIC X(2).
           03 DIA-PROC              PIC X(2).
       01  FECHA-PROC-R  REDEFINES FECHA-PROC
                                    PIC X(8).
       01  FECHA-AUX.
           03 DIA-AUX                   PIC 9(2).
           03 MES-AUX                   PIC 9(2).
           03 ANO-AUX                   PIC 9(2).
       01  FECHA-AUX-R  REDEFINES FECHA-AUX
                                        PIC 9(6).
       01  TIEMPO-SIS.
           03 HORA-SIS                  PIC 9(02).
           03 MIN-SIS                   PIC 9(02).
           03 FILLER                    PIC 9(04).
       01  TIEMPO-SIS-R  REDEFINES TIEMPO-SIS
                                        PIC 9(08).
       01  TIEMPO-W                     PIC 9(8).
       01  W-CONTADORES.
           03  W-CONT-TIPO-5            PIC 9(06) VALUE ZEROS.
           03  W-CONT-TIPO-6            PIC 9(06) VALUE ZEROS.
           03  W-CONT-TIPO-7            PIC 9(06) VALUE ZEROS.
CQ6271     03  W-CONT-TIPO-5-PJ         PIC 9(06) VALUE ZEROS.          PJV0010
PJV011     03  W-CONT-TIPO-5-REP        PIC 9(06) VALUE ZEROS.          CQ11407
      *
       01  SUSCRIPTOR-AUX               PIC 9(06).
       01  REG-LEIDOS                   PIC 9(07) VALUE ZEROS.
R7094B 01  CONT-FALLECIDOS              PIC 9(07) VALUE ZEROS.          RMB0009
       01  REG-CORRECTOS                PIC 9(07) VALUE ZEROS.
PJV011 01  REG-CORRECTOS-R              PIC 9(07) VALUE ZEROS.          CQ11407
       01  NO-COINCIDE-DIAN             PIC 9(07) VALUE ZEROS.
       01  NO-COINCIDE-REGIS            PIC 9(07) VALUE ZEROS.
       01  NO-EXISTE-DIAN               PIC 9(07) VALUE ZEROS.
       01  NO-EXISTE-REGIS              PIC 9(07) VALUE ZEROS.
       01  SI-EXISTE-CSV                PIC 9(07) VALUE ZEROS.
       01  TIPO-ID-INVALIDO             PIC 9(07) VALUE ZEROS.
       01  NIT-BUENO                    PIC 9.
       01  NUM-ID-W                     PIC 9(11).
      *------------------------------------------------------------------*
       COPY  ICFILE.WS.
       COPY  BDIIALE.WS.
       COPY  DEPTOS.WS.
       COPY  BDIIVIDE.WS.
       COPY  BDLDIR.WS.
       COPY  LEEMAESTROS.WS.
      *------------------------------------------------------------------*
       01  CODIGO-ERROR                 PIC 9(02) VALUE ZERO.
       01  PRIMER-APELLIDO              PIC X(16) VALUE SPACES.
       01  SEGUNDO-APELLIDO             PIC X(16) VALUE SPACES.
       01  PRIMER-NOMBRE                PIC X(16) VALUE SPACES.
       01  SEGUNDO-NOMBRE               PIC X(16) VALUE SPACES.
       01  NOMBRE-DANE.
           03 PR-APELLIDO-ABD05         PIC X(15).
           03 NOMBRES-ABD05             PIC X(30).
       01  NOMBRE-DANE-AUX              PIC X(45).
       01  NOM-VAL-AUX.
           03 PR-APELLIDO-AUX           PIC X(15).
           03 SG-APELLIDO-AUX           PIC X(15).
           03 NOMBRE-AUX                PIC X(15).
       01  CONTADORES.
           03 JL                        PIC 9(02) VALUE ZERO.
           03 I                         PIC 9(02) VALUE ZERO.
           03 SW-ERROR                  PIC 9     VALUE ZERO.
           03 J                         PIC 9(02) VALUE ZERO.
           03 BLOC-IGUAL                PIC 9(02) VALUE ZERO.
           03 BLOQUES                   PIC 9(02) VALUE ZERO.
           03 ENCONTRADO                PIC 9     VALUE ZERO.
           03 SUP                       PIC 9(02) VALUE ZERO.
           03 INF                       PIC 9(02) VALUE ZERO.
           03 JJ                        PIC 9(02) VALUE ZERO.
      *------------------------------------------------------------------*
       01  W-VARIABLES.
           03 W-ANO-ACTUAL                PIC 9(04) VALUE ZEROS.
           03 W-EDAD-CEDULA               PIC 9(03) VALUE ZEROS.
           03 W-FEC-EXPE                  PIC 9(08) VALUE ZEROS.
           03 W-FEC-EXPE-R REDEFINES W-FEC-EXPE.
              05 W-ANO-EXPE               PIC 9(04).
              05 W-MES-EXPE               PIC 9(02).
              05 W-DIA-EXPE               PIC 9(02).
           03 WS-EDAD                     PIC 9(03).
      *------------------------------------------------------------------*
       01  VEC-MAESTRO.
           03 LETRA-MAESTRO             PIC X  OCCURS 65 TIMES.
       01  VEC-DANE.
           03 LETRA-DANE                PIC X  OCCURS 65 TIMES.
       01  VEC-BLOQUES.
           03 LIM-INF                   PIC 9(02)  OCCURS 44 TIMES.
           03 LIM-SUP                   PIC 9(02)  OCCURS 44 TIMES.
       01  CONTADORES-COMP-NOMBRE.
           03 K                         PIC 9(04) VALUE ZERO.
           03 NOMBRE-BIEN               PIC 9     VALUE ZERO.
       01  VEC-MAESTRO-UNO.
           03 LETRA-MAESTRO-UNO         PIC X  OCCURS 45 TIMES.
       01  VEC-DANE-UNO.
           03 LETRA-DANE-UNO            PIC X  OCCURS 45 TIMES.
       01  VEC-BLOQUES-UNO.
           03 LIM-INF-UNO               PIC 99  OCCURS 44 TIMES.
           03 LIM-SUP-UNO               PIC 99  OCCURS 44 TIMES.
       01  TITULO-1.
           03 FILLER                   PIC X(55) VALUE SPACES.
           03 FILLER                   PIC X(21) VALUE
             "D A T A C R E D I T O".
           03 FILLER                   PIC X(33) VALUE SPACES.
           03 DIA-T1                   PIC X(02).
           03 FILLER                   PIC X     VALUE "/".
           03 MES-T1                   PIC X(02).
           03 FILLER                   PIC X     VALUE "/".
           03 SIG-T1                   PIC X(02).
           03 ANO-T1                   PIC X(02).
           03 FILLER                   PIC X(13) VALUE SPACES.
       01  TITULO-2.
           03 FILLER                   PIC X(10) VALUE " PESVNOCSV".
           03 FILLER                   PIC X(41) VALUE SPACES.
           03 FILLER                   PIC X(37) VALUE
             "ESTADISTICAS DE VALIDACION DE CEDULAS".
           03 FILLER                   PIC X(24) VALUE SPACES.
           03 HORA-T2                  PIC 9(02).
           03 FILLER                   PIC X     VALUE ":".
           03 MIN-T2                   PIC 9(02).
           03 FILLER                   PIC X(06) VALUE SPACES.
           03 FILLER                   PIC X(06) VALUE "SUSC: ".
           03 SUSCRIPTOR-T2            PIC 9(06).
       01  RAYAS                       PIC X(132) VALUE ALL "-".
       01  LINEA-1.
           03 FILLER                   PIC X(45) VALUE SPACES.
           03 FILLER                   PIC X(32) VALUE
             "REGISTROS LEIDOS:               ".
           03 REG-LEIDOS-L1            PIC ZZ,ZZZ,ZZ9.
           03 FILLER                   PIC X(45) VALUE SPACES.
       01  LINEA-2.
           03 FILLER                   PIC X(45) VALUE SPACES.
           03 FILLER                   PIC X(33) VALUE
             "REGISTROS CORRECTOS:             ".
           03 REG-CORRECTOS-L2         PIC Z,ZZZ,ZZ9.
           03 FILLER                   PIC X(45) VALUE SPACES.
       01  LINEA-3.
           03 FILLER                   PIC X(45) VALUE SPACES.
           03 FILLER                   PIC X(33) VALUE
             "NO COINCIDEN CON DIAN O DAS:     ".
           03 NO-COINCIDE-DIAN-L3      PIC Z,ZZZ,ZZ9.
           03 FILLER                   PIC X(45) VALUE SPACES.
       01  LINEA-4.
           03 FILLER                   PIC X(45) VALUE SPACES.
           03 FILLER                   PIC X(33) VALUE
             "NO COINCIDEN CON REGISTARDURIA:  ".
           03 NO-COINCIDE-REGIS-L4     PIC Z,ZZZ,ZZ9.
           03 FILLER                   PIC X(45) VALUE SPACES.
       01  LINEA-5.
           03 FILLER                   PIC X(45) VALUE SPACES.
           03 FILLER                   PIC X(33) VALUE
             "NO EXISTEN EN DIAN O DAS:        ".
           03 NO-EXISTE-DIAN-L5        PIC Z,ZZZ,ZZ9.
           03 FILLER                   PIC X(45) VALUE SPACES.
       01  LINEA-6.
           03 FILLER                   PIC X(45) VALUE SPACES.
           03 FILLER                   PIC X(33) VALUE
             "NO EXISTEN EN REGISTRADURIA:     ".
           03 NO-EXISTE-REGIS-L6       PIC Z,ZZZ,ZZ9.
           03 FILLER                   PIC X(45) VALUE SPACES.
       01  LINEA-7.
           03 FILLER                   PIC X(45) VALUE SPACES.
           03 FILLER                   PIC X(33) VALUE
             "TIPO IDENTIFICACION INVALIDO:    ".
           03 TIPO-ID-INVALIDO-L7      PIC Z,ZZZ,ZZ9.
           03 FILLER                   PIC X(45) VALUE SPACES.
       01  FECHA-2000                  PIC 9(8).
       01  FECHA-2000-R  REDEFINES FECHA-2000.
           03 ANO-2000-R               PIC 9(4).
           03 MES-2000-R               PIC 9(2).
           03 DIA-2000-R               PIC 9(2).
      *------------------------------------------------------------------*
       01  HACER-MATCH                 PIC  9(01).
       01  CADENA-ENTRA                PIC  X(60).
       01  CADENA-SALE                 PIC  X(60).
       01  IND-ALFABETICO              PIC  X(01).
           88 SI-ALFABETICO           VALUE " " "A" "B" "C" "D" "E" "F"
                                            "G" "H" "I" "J" "K" "L" "M"
                                            "N" "/" "O" "P" "Q" "R" "S"
                                            "T" "U" "V" "W" "X" "Y" "Z"
                                            "Ñ".
CQ6271 LINKAGE SECTION.                                                 PJV0010
CQ6271 COPY LOGPESVNO.LKG.                                              PJV0010
      *
      *=================================================================
      $SET CONSTANT HACERMATCH"SI"
      $SET CONSTANT AMNISTIA21"SI"
      *=================================================================

       PROCEDURE DIVISION.
      ******************************************************************
      *100-PROGRAMA-PRINCIPAL.
      *RUTINA PRINCIPAL
      *VALIDA EL CODIGO DEL SUSCRIPTOR DEL ARCHIVO DE ENTRADA
      *VALIDA LA CLAVE CON QUE SE GRABARAN REGISTROS EN EL ICBTCO
      ******************************************************************
       100-PROGRAMA-PRINCIPAL.
           PERFORM 150-ABRIR-ARCHIVOS
           READ ENTRADA
                AT END
                      MOVE 1 TO FIN-ENTRADA
                NOT AT END
                      INSPECT NUMID-X-E REPLACING ALL " " BY "0"
                      INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"
           END-READ
R7094A     IF NOMBRE-E = "VALIDAR=NO" AND FIN-ENTRADA = 0               RMB0009
R7094A        MOVE 0 TO W-BLOQUES                                       RMB0009
R7094A        DISPLAY "Registro  control==> 000001 (VALIDAR=NO)"        RMB0009
R7094A        MOVE 0 TO W-BLOQUES                                       RMB0009
R7094A        READ ENTRADA                                              RMB0009
R7094A          AT END                                                  RMB0009
R7094A                MOVE 1 TO FIN-ENTRADA                             RMB0009
R7094A          NOT AT END                                              RMB0009
R7094A                INSPECT NUMID-X-E REPLACING ALL " " BY "0"        RMB0009
R7094A                INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"       RMB0009
R7094A        END-READ                                                  RMB0009
R7094A     ELSE                                                         RMB0009
R7094A        DISPLAY "Registro  control==> 000000 (VALIDAR=NO)"        RMB0009
R7094A     END-IF.                                                      RMB0009
           CLOSE CLAVE ABD14.

           MOVE FUNCTION CURRENT-DATE TO FECHA-PROC-R
           MOVE DIA-PROC TO DIA-AUX
           MOVE MES-PROC TO MES-AUX
           MOVE ANO-PROC TO ANO-AUX
           MOVE ANO-HOY  TO W-ANO-ACTUAL

           PERFORM 400-VALIDAR UNTIL FIN-ENTRADA = 1.

           IF W-CONT-TIPO-5 >  ZEROS  OR
              W-CONT-TIPO-6 >  ZEROS  OR
              W-CONT-TIPO-7 >  ZEROS
            DISPLAY "Registros Tipo 5           ==> " W-CONT-TIPO-5
            DISPLAY "Registros Tipo 6           ==> " W-CONT-TIPO-6
            DISPLAY "Registros Tipo 7           ==> " W-CONT-TIPO-7
PJV011      DISPLAY "Registros Tipo 5 repetidos ==> " W-CONT-TIPO-5-REP CQ11407
      *        DISPLAY "*-----------------------------------*"
      *        DISPLAY "Registros validados en BDACSV:" SI-EXISTE-CSV
      *        DISPLAY "*------------------------------------*"
           END-IF
           PERFORM 900-CERRAR-ARCHIVOS
CQ6271     MOVE "V"               TO TIPPRO-LOGPESVNO-WK                PJV0010
CQ6271     MOVE "PESVNO"          TO PROGRAM-LOGPESVNO-WK               PJV0010
CQ6271     PERFORM 1000-UPDATE-LOGPESVNO                                PJV0010
           STOP RUN.


      ******************************************************************
      *150-ABRIR-ARCHIVOS.
      *SE ABRE CADA UNO DE LOS ARCHIVOS QUE REQUIERE EL PROGRAMA PARA SU
      *EJECUCION: ICDIAN,CEDULAS,REG01, REG02,ICDAS,ICBTCO,ICBCLA,ICBCIU
      *ICT40D.DAT,DCVAL,DCINIC,ICMUNI,ICI40N.PRN
      *PREPARA EL LA LLAVE DEL ARCHIVO ICBTCO PARA IDENTIFICAR CON QUE
      *LLAVE MAYOR QUE 20000000 INICIA LA ESCRITURA DEL MISMO
      ******************************************************************
       150-ABRIR-ARCHIVOS.
           OPEN INPUT BDIIREGI.
      *    DISPLAY "*-------------------------------------------------*"
      *    DISPLAY "|  PROCESO VALIDACION USANDO CUENTAS SIN VALIDAR  |"
      *    DISPLAY "*-------------------------------------------------*"
           DISPLAY "!!........EL ORDEN DE LOS PARAMETROS ES ........!!"
MODBD2     DISPLAY "!!..ENTRADA CORRECTOS INCORRECTOS TARIFA BLOQUES.!!"FAC0011
           ACCEPT W-PARAMETROS   FROM COMMAND-LINE
           UNSTRING W-PARAMETROS DELIMITED BY SPACES INTO
                                                     W-ENT-NOMBRE,
                                                     W-VAL-NOMBRE,
                                                     W-INVAL-NOMBRE,
                                                     W-TARIFA,
                                                     W-BLOQUES,
                                                     W-DERECHOS
           END-UNSTRING
           IF W-TARIFA       = ZEROS  OR
              W-ENT-NOMBRE   = SPACES OR
              W-VAL-NOMBRE   = SPACES OR
              W-INVAL-NOMBRE = SPACES OR
R7094A*       (W-BLOQUES < 0 OR > 4)                                    RMB0009
R7094A         W-BLOQUES > 4                                            RMB0009
                 DISPLAY "!!!!!!    FALTA ALGUN PARAMETRO  !!!!!!! "
                 STOP RUN
           END-IF
R7094A     IF W-BLOQUES = 0                                             RMB0009
R7094A        DISPLAY "!!..EJECUCION SIN VALIDAR NOMBRES       .....!!" RMB0009
R7094A     END-IF                                                       RMB0009

PJV011     INITIALIZE W-VAL-NOMBRE-REP                                  CQ11407
PJV011     STRING W-VAL-NOMBRE DELIMITED BY "."                         CQ11407
PJV011            ".vre"       DELIMITED BY SIZE                        CQ11407
PJV011        INTO W-VAL-NOMBRE-REP                                     CQ11407
PJV011     END-STRING                                                   CQ11407

           MOVE W-ENT-NOMBRE            TO W-SORT-NOM-ENT

           MOVE W-ENT-NOMBRE            TO W-SORT-NOM-ENT
                                           W-SORT-NOM-SAL
                                           W-LINEAS-NOM-ENT
      *    DISPLAY "============================================"
           DISPLAY "REGISTROS RECIBIDOS ORIGINALMENTE:"
           CALL "SYSTEM" USING W-COMANDO-LINEAS
      *    DISPLAY " "
           DISPLAY "ARCHIVO: " W-LINEAS-NOM-ENT
      *    DISPLAY " "
           CALL "SYSTEM" USING W-COMANDO-SORT
           IF RETURN-CODE <> 0
              DISPLAY "ERROR ORDENANDO UNICOS " W-ENT-NOMBRE
              STOP RUN
           END-IF
           OPEN INPUT  ENTRADA
                       BDIIDIAN
                       BDIIDAS
                       BDIIALE
                       MUNICIPIOS
                       CLAVE
                       ABD14
XXXXXX*    OPEN I-O    SECUENCIAL
XXXXXX     OPEN INPUT  SECUENCIAL
           OPEN OUTPUT VALIDAS
                       INCONS
PJV011                 VALIDAS-REP                                      CQ11407
           MOVE 29999999 TO LLAVE-SEC
           START SECUENCIAL KEY < LLAVE-SEC INVALID KEY
                 DISPLAY " ERROR EN START DE ICBTCO  " LLAVE-SEC
                 MOVE 1 TO FINS
           END-START
           IF FINS = 1
              MOVE TOPE-INI-LLAVE7 TO LLAVE7
           ELSE
              READ SECUENCIAL PREVIOUS AT END
                   DISPLAY " ERROR LEYENDO REGISTRO PREVIO DE ICBTCO  "
                           LLAVE-SEC
                   MOVE 1 TO FINS
              END-READ
              IF FINS = 1
                 MOVE TOPE-INI-LLAVE7 TO LLAVE7
              ELSE
                 MOVE LLAVE-SEC TO LLAVE7
                 IF LLAVE7-1D NOT = 2
                    MOVE 2     TO LLAVE7-1D
                    MOVE ZEROS TO LLAVE7-FILLER
                 END-IF
              END-IF
           END-IF.

      *******************************************************************
      *400-VALIDAR
      * DEPENDIENDO DEL TIPO DE IDENTIFICACION LEIDO EN EL ARCHIVO DE EN-
      * TRADA LLAMA A LA CORRESPONDIENTE RUTINA DE VALIDACION (CEDULA,
      * NIT O DAS)
      * ACUMULA EN CONTADORES DEPENDIENDO DEL RESULTADO DE LA VALIDACION
      * LEE SECUENCIALMENTE EL ARCHIVO DE ENTRADA
      *******************************************************************
       400-VALIDAR.
           ADD  1 TO REG-LEIDOS.
           MOVE 0 TO SW-ERROR CODIGO-ERROR CODERR-INC
           INITIALIZE REG-INCONS REG-VALIDAS
           MOVE ";" TO FILLER1 FILLER2 FILLER3 FILLER4 FILLER5
                       FILLER6 FILLER7 FILLER8 FILLER9
           MOVE FILLER-2-E TO FILLER-2-VAL
           MOVE FILLER-2-E TO FILLER-2-INC
           IF TIPOID-E < 1 OR > 4
              MOVE 05 TO CODIGO-ERROR CODERR-INC
              MOVE 1  TO SW-ERROR
              PERFORM 512-CREE-REGISTRO
              WRITE REG-INCONS
              ADD  1  TO TIPO-ID-INVALIDO
           ELSE
              MOVE  TIPOID-E  TO  TIP-IDE-BDIIVIDE
              MOVE  NUMID-E   TO  NUM-IDE-BDIIVIDE
                                  NUM-ID-W
              MOVE  NOMBRE-E  TO  NOMBRE-BDIIVIDE
R7094A*       MOVE    1       TO  SW-VERIFNOM-BDIIVIDE                  RMB0009
R7094A*                           SW-DIGCHEQUEO-BDIIVIDE                RMB0009
R7094A        IF W-BLOQUES > 0                                          RMB0009
R7094A           MOVE    1       TO  SW-VERIFNOM-BDIIVIDE               RMB0009
R7094A        END-IF                                                    RMB0009
R7094D        IF TIPOID-E = 2 OR 3                                      RMB0009
R7094D           MOVE 1 TO SW-DIGCHEQUEO-BDIIVIDE                       RMB0009
R7094D        END-IF                                                    RMB0009
              MOVE  W-BLOQUES TO  NUM-BLOQUES-BDIIVIDE
              PERFORM 80100-VALIDAR-IDENTIFICACION
              MOVE  TIP-IDE-BDIIVIDE TO TIPOID-E
              MOVE  NUM-IDE-BDIIVIDE TO NUMID-E
              INITIALIZE REG-VALIDAS REG-INCONS
REQ596        MOVE FILLER-2-E TO FILLER-2-VAL                           LAG0007
REQ596        MOVE FILLER-2-E TO FILLER-2-INC                           LAG0007
              MOVE ";" TO FILLER1 FILLER2 FILLER3 FILLER4 FILLER5
                       FILLER6 FILLER7 FILLER8 FILLER9
              IF SW-ID-BDIIVIDE = 1
                 IF TIPOID-E <> 1
CSVCEX             MOVE  ZEROS         TO  HACER-MATCH                  GJC0003
MODCED             MOVE 0 TO IDE-TIPO-BDAMATCH                          EHA0006
CSVCEX             PERFORM  513-VALIDA-EN-CSV                           GJC0003
CSVCEX             IF  IDE-TIPO-BDAMATCH  =  0                          GJC0003
CSVCEX               MOVE 03           TO  CODIGO-ERROR CODERR-INC      GJC0003
CSVCEX               MOVE  1           TO SW-ERROR                      GJC0003
CSVCEX               PERFORM 512-CREE-REGISTRO                          GJC0003
CSVCEX               WRITE REG-INCONS                                   GJC0003
CSVCEX               ADD   1           TO  NO-EXISTE-DIAN               GJC0003
CSVCEX             END-IF                                               GJC0003
                 ELSE
                    MOVE  ZEROS TO  HACER-MATCH
MODCED              MOVE 0 TO IDE-TIPO-BDAMATCH                         EHA0006
                    PERFORM 513-VALIDA-EN-CSV
                    IF IDE-TIPO-BDAMATCH = 0
                       MOVE 04 TO CODIGO-ERROR CODERR-INC
                       MOVE TIPOID-E TO TIP-INC
                       ADD  1  TO NO-EXISTE-REGIS
                       MOVE 1  TO SW-ERROR
                       PERFORM 510-ESCRIBIR-INCONS
                    END-IF
                 END-IF
              ELSE
                 IF SW-NOMBRES-BDIIVIDE = 1
                    IF TIPOID-E <> 1
                       MOVE 01 TO CODIGO-ERROR CODERR-INC
                       MOVE  1 TO SW-ERROR
                       PERFORM 512-CREE-REGISTRO
                       WRITE REG-INCONS
                       ADD 1 TO NO-COINCIDE-DIAN
                       IF TIPOID-E = 4
                          MOVE NACIO-BDIIDAS TO MUN-EXP-INC
                       END-IF
                    ELSE
                       PERFORM 810-BUSCAR-MUNICIPIO
                       MOVE TIPOID-E TO TIP-VAL TIP-INC
                       MOVE NUMID-E  TO CED-VAL CED-INC
                       MOVE FECHA-EXP-BDIIREGI TO FECHA-2000
                       IF ANO-2000-R > 51
                          ADD 1900 TO ANO-2000-R
                       ELSE
                          ADD 2000 TO ANO-2000-R
                       END-IF
                       MOVE IND-SEXO-BDIIREGI TO GENERO-VAL
                       PERFORM 505-RANGO-EDADES
                       MOVE FECHA-2000 TO FEC-EXP-VAL
                                          FEC-EXP-INC
                       IF ESTADO-VIG-BDIIREGI = 12 OR = 27
                          MOVE "00"        TO VIG-VAL VIG-INC
                       ELSE
                          MOVE ESTADO-VIG-BDIIREGI TO VIG-VAL
                                                      VIG-INC
                       END-IF
                       MOVE   02 TO CODIGO-ERROR CODERR-INC
                       MOVE 1 TO SW-ERROR
                       PERFORM 510-ESCRIBIR-INCONS
                       ADD 1 TO NO-COINCIDE-REGIS
                    END-IF
                 ELSE
R7094C*             IF W-BLOQUES > 1 OR TIPOID-E <> 1                   RMB0009
R7094C*                MOVE NOM-VAL-BDIIVIDE         TO NOM-VAL         RMB0009
R7094C*             ELSE                                                RMB0009
R7094C*                MOVE PRIMER-APELLIDO-BDIIVIDE TO NOM-VAL         RMB0009
R7094C*             END-IF                                              RMB0009
R7094C              MOVE NOM-VAL-BDIIVIDE         TO NOM-VAL            RMB0009
                    INITIALIZE LLAVE-BDIIALE
                    MOVE  TIPOID-E TO TIP-IDE-BDIIALE
                    MOVE  NUMID-E  TO NUM-IDE-BDIIALE
                    MOVE  990003   TO FUENTE-BDIIALE
                    START BDIIALE KEY NOT < LLAVE-BDIIALE
                          INVALID KEY MOVE 1 TO SW-BDIIALE
                      NOT INVALID KEY MOVE 0 TO SW-BDIIALE
                          READ BDIIALE NEXT RECORD
                               AT END MOVE 1 TO SW-BDIIALE
                          END-READ
                    END-START
                    PERFORM UNTIL SW-BDIIALE = 1 OR
                            TIPOID-E <> TIP-IDE-BDIIALE OR
                            NUMID-E <> NUM-IDE-BDIIALE
                            IF FUENTE-BDIIALE = 990003 AND
                               FEC-NOV-BDIIALE <= FECHA-PROC-R AND
                               FEC-VEN-BDIIALE >= FECHA-PROC-R AND
                               BLOQUEO-BDIIALE = 0
                               MOVE 1 TO SW-BDIIALE
                            END-IF
                            IF SW-BDIIALE = 0
                               READ BDIIALE NEXT RECORD
                                    AT END MOVE 1 TO SW-BDIIALE
                               END-READ
                            END-IF
                    END-PERFORM
                    IF TIPOID-E = 1
                       IF ESTADO-VIG-BDIIREGI = 12 AND
                          W-DERECHOS = 1
                          MOVE  07   TO  CODIGO-ERROR CODERR-INC
                          MOVE   1   TO  SW-ERROR
                          MOVE "00"  TO  VIG-INC
                          PERFORM 510-ESCRIBIR-INCONS
                        ELSE
                           IF ESTADO-VIG-BDIIREGI = 21
                              MOVE  06  TO CODIGO-ERROR CODERR-INC
                              MOVE   1  TO SW-ERROR
                              MOVE "21" TO VIG-INC
                              PERFORM 510-ESCRIBIR-INCONS
                              ADD   1   TO NO-COINCIDE-REGIS
R7094B                        ADD   1   TO CONT-FALLECIDOS              RMB0009
                           ELSE
                              PERFORM 810-BUSCAR-MUNICIPIO
                              MOVE TIPOID-E TO TIP-VAL TIP-INC
                              MOVE NUMID-E  TO CED-VAL CED-INC
                              MOVE FECHA-EXP-BDIIREGI TO FECHA-2000
                              IF ANO-2000-R > 51
                                 ADD 1900 TO ANO-2000-R
                              ELSE
                                 ADD 2000 TO ANO-2000-R
                              END-IF
                              MOVE FECHA-2000 TO FEC-EXP-VAL
                                                 FEC-EXP-INC
                              IF ESTADO-VIG-BDIIREGI = 12 OR = 27
                                 MOVE "00" TO VIG-VAL VIG-INC
                              ELSE
                                 MOVE ESTADO-VIG-BDIIREGI TO VIG-VAL
                                                             VIG-INC
                              END-IF
                              MOVE IND-SEXO-BDIIREGI TO GENERO-VAL
                              PERFORM 505-RANGO-EDADES
PAM460*                       IF ORIGEN-VAL = SPACES
PAM460*                          MOVE   "O"      TO ORIGEN-VAL
PAM460*                       END-IF
PJV011                        PERFORM 301-GRABA-LOS-VALIDOS             CQ11407
                           END-IF
                        END-IF
                    ELSE
                       IF TIPOID-E = 4
                          MOVE NACIO-BDIIDAS   TO MUN-EXP-VAL
                          MOVE ZEROS           TO VIG-VAL
                       END-IF
                       MOVE NOM-VAL-BDIIVIDE TO NOM-VAL
                       MOVE TIPOID-E         TO TIP-VAL TIP-INC
                       MOVE NUMID-E          TO CED-VAL CED-INC
PAM460*                IF ORIGEN-VAL = SPACES
PAM460*                   MOVE    "O"        TO ORIGEN-VAL
PAM460*                END-IF
PJV011                 PERFORM 301-GRABA-LOS-VALIDOS                    CQ11407
                    END-IF
                 END-IF
              END-IF
           END-IF
           IF CODIGO-ERROR = 01 OR 02 OR 06 OR 07
              ADD  1 TO W-CONT-TIPO-6
           ELSE
              IF CODIGO-ERROR = 03 OR 04 OR 05
                 ADD 1  TO W-CONT-TIPO-7
              ELSE
PJV011           IF SN-VALREP = "S"                                     CQ11407
PJV011              ADD 1  TO W-CONT-TIPO-5-REP                         CQ11407
PJV011           ELSE                                                   CQ11407
                    ADD 1  TO W-CONT-TIPO-5
PJV011           END-IF                                                 CQ11407
CQ6271           IF TIPOID-E = 2 OR 3                                   PJV0010
CQ6271              ADD 1 TO W-CONT-TIPO-5-PJ                           PJV0010
CQ6271           END-IF                                                 PJV0010
              END-IF
           END-IF
           READ ENTRADA
                AT END
                      MOVE 1 TO FIN-ENTRADA
                NOT AT END
                      INSPECT  NUMID-X-E REPLACING ALL " " BY "0"
                      INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"
           END-READ.

PJV011 300-GRABA-VALREP.                                                CQ11407
PJV011     INITIALIZE REG-VALIDAS-REP                                   CQ11407
PJV011     WRITE REG-VALIDAS-REP FROM REG-VALIDAS                       CQ11407
PJV011     MOVE TIP-VAL TO TIP-KEYVAL                                   CQ11407
PJV011     MOVE CED-VAL TO NUM-KEYVAL                                   CQ11407
PJV011     ADD 1 TO REG-CORRECTOS-R  .                                  CQ11407

PJV011 301-GRABA-LOS-VALIDOS.                                           CQ11407
PJV011     MOVE "N" TO SN-VALREP                                        CQ11407
PJV011     IF TIP-KEYVAL = TIPOID-E AND                                 CQ11407
PJV011        NUM-KEYVAL = NUMID-E                                      CQ11407
PJV011        MOVE "S" TO SN-VALREP                                     CQ11407
PJV011        PERFORM 300-GRABA-VALREP                                  CQ11407
PJV011     ELSE                                                         CQ11407
PJV011        WRITE REG-VALIDAS                                         CQ11407
PJV011        MOVE TIP-VAL TO TIP-KEYVAL                                CQ11407
PJV011        MOVE CED-VAL TO NUM-KEYVAL                                CQ11407
PJV011        ADD 1 TO REG-CORRECTOS                                    CQ11407
PJV011     END-IF.                                                      CQ11407


       505-RANGO-EDADES.
      *--------------------------------------------------*
      *505-RANGO-EDADES.
      *  RANGO MUJERES: 20'000.001 A 70'000.000
      *  RANGO HOMBRES:          1 A 20'000.000
      *        MAYOR  : > 70'000.000 < 1000'000.000 (NUIPS)
      *--------------------------------------------------*
           MOVE FEC-EXP-VAL        TO  W-FEC-EXPE
           MOVE ZEROS              TO  W-EDAD-CEDULA
           IF W-ANO-EXPE >= 1975
              COMPUTE W-EDAD-CEDULA =
               ((W-ANO-ACTUAL - W-ANO-EXPE) + 18)
           ELSE
              COMPUTE W-EDAD-CEDULA =
               ((W-ANO-ACTUAL - W-ANO-EXPE) + 21)
           END-IF
           MOVE  W-EDAD-CEDULA    TO  EDAD-VAL WS-EDAD
           MOVE  FILLER-2-E TO FILLER-2-VAL FILLER-2-INC.
                 EVALUATE WS-EDAD
                    WHEN 18 THRU 21
                       MOVE "18-21" TO EDAD-RANGO
                    WHEN 22 THRU 28
                       MOVE "22-28" TO EDAD-RANGO
                    WHEN 29 THRU 35
                       MOVE "29-35" TO EDAD-RANGO
                    WHEN 36 THRU 45
                       MOVE "36-45" TO EDAD-RANGO
                    WHEN 46 THRU 55
                       MOVE "46-55" TO EDAD-RANGO
                    WHEN 56 THRU 65
                       MOVE "56-65" TO EDAD-RANGO
                    WHEN 66 THRU 999
                       MOVE "66+  " TO EDAD-RANGO
                    WHEN ANY
                       MOVE " N/A " TO EDAD-RANGO
                 END-EVALUATE.
      ****************************************************************
      *510-ESCRIBIR-INCONS.
      * BUSCA ERROR
      * CREAR REGISTRO EN EL ARCHIVO DE INCONSISTENCIAS
      ****************************************************************
       510-ESCRIBIR-INCONS.
           IF CODIGO-ERROR = 02 AND SW-ERROR = 1
              MOVE TIPOID-E TO TIP-INC.
           IF SW-ERROR = 1
               PERFORM 512-CREE-REGISTRO
               WRITE REG-INCONS.

      **************************************************************
      *512-CREE-REGISTRO.
      * ARMA EL REGISTRO DE INCONSISTENCIAS DEL ARCHIVO DCINC
      **************************************************************
       512-CREE-REGISTRO.
           MOVE TIPOID-E TO TIP-INC
           MOVE NUMID-E  TO CED-INC                                     FAC0019
           MOVE NOMBRE-E TO NOM1-INC
           IF CODIGO-ERROR = 02 OR 01 OR 06 OR 07
              MOVE NOM-VAL-BDIIVIDE TO NOM2-INC
           ELSE
              MOVE SPACES      TO NOM2-INC
                                  MUN-EXP-INC
                                  DEP-EXP-INC
                                  VIG-INC
              MOVE ZEROS       TO FEC-EXP-INC
              MOVE FILLER-2-E  TO FILLER-2-INC
           END-IF.

      **************************************************************
      *513-VALIDA-EN-CSV.
      *    VALIDA SI LA IDENTIFICACION ESTA EL LA BASE DE DATOS
      *    ALTERNA
      **************************************************************
       513-VALIDA-EN-CSV.
REQ550*    hace validacion de tipo de identificacion
REQ550     MOVE  TIPOID-E  TO  TIP-IDE-BDIIVIDE
REQ550     MOVE  NUMID-E   TO  NUM-IDE-BDIIVIDE
REQ550     PERFORM 80400-VALIDAR-TIPO-ID
REQ550     IF SW-TID-BDIIVIDE = 0
           IF HACER-MATCH = ZEROS
              IF OPEN-DIR = ZEROS
                 OPEN INPUT MAESTRO-DIRECC
                 OPEN INPUT BDLCLI
                 OPEN INPUT SUSCRIPTOR
                 OPEN INPUT MAESTRO-CREDIT
                 OPEN INPUT MAESTRO-CHECK
                 PERFORM 06000-U-CARGAR-TABLA-DIRESTAN
                 MOVE 1 TO OPEN-DIR
               END-IF
               MOVE TIPOID-E TO TIP-IDE-INPUT-BDAMATCH
               MOVE NUMID-E  TO NUM-IDE-INPUT-BDAMATCH
               PERFORM 1000-U-RECORRER-CSV-BDAMATCH
               MOVE 1 TO HACERMATCH-LEEMSTRS
REQ609         IF IDE-TIPO-BDAMATCH = 1                                 LAG0008
REQ609            IF TIPOID-E = 1 OR 4                                  LAG0008
REQ609               INSPECT NOM-OUTPUT-BDAMATCH REPLACING ALL          LAG0008
REQ609                  "," BY " ", ";" BY " "                          LAG0008
REQ609            END-IF                                                LAG0008
REQ609            MOVE NOMBRE-E            TO NOMBRE-INPUT-VALNOM       LAG0008
REQ609            MOVE NOM-OUTPUT-BDAMATCH TO NOMBOK-INPUT-VALNOM       LAG0008
REQ609            MOVE TIPOID-E            TO  TIP-ID-INPUT-VALNOM      LAG0008
REQ609            MOVE NUM-BLOQUES-BDIIVIDE TO BLOQUES-INPUT-VALNOM     LAG0008
REQ609            PERFORM 10000-MATCH-NOMBRES-VALNOM                    LAG0008
REQ609            IF MATCH-OK-OUT-VALNOM = 1                            LAG0008
REQ609               MOVE 1 TO IDE-TIPO-BDAMATCH                        LAG0008
REQ609            ELSE                                                  LAG0008
R7094A              IF W-BLOQUES > 0                                    RMB0009
R7094A                 MOVE 0 TO IDE-TIPO-BDAMATCH                      RMB0009
R7094A              END-IF                                              RMB0009
REQ609            END-IF                                                LAG0008
REQ609         END-IF                                                   LAG0008

               IF IDE-TIPO-BDAMATCH = 1
                  MOVE TIPOID-E TO TIP-VAL
                  MOVE NUMID-E  TO CED-VAL
                  MOVE NOM-OUTPUT-BDAMATCH TO CADENA-ENTRA
                  PERFORM 9800-FILTRAR-ALFABETICO
                  MOVE CADENA-SALE TO  NOM-VAL
                  MOVE ZEROS    TO FEC-EXP-VAL
                  MOVE SPACES   TO MUN-EXP-VAL
                  MOVE SPACES   TO DEP-EXP-VAL
                  MOVE SPACES   TO VIG-VAL
                  MOVE SPACES   TO GENERO-VAL
                  MOVE SPACES   TO EDAD-VAL
                  MOVE SPACES   TO EDAD-RANGO
PAM460*           MOVE "A"      TO ORIGEN-VAL
PJV011            PERFORM  301-GRABA-LOS-VALIDOS                        CQ11407
PJV011            ADD 1 TO SI-EXISTE-CSV                                CQ11407
                END-IF
             END-IF.

       9800-FILTRAR-ALFABETICO.
      ******************************************************************
      *9800-FILTRAR-ALFABETICO.                                        *
      *    SE ELIMINAN CARACTERES NO ALFABETICOS A LA CADENA DE ENTRADA*
      ******************************************************************
           MOVE FUNCTION UPPER-CASE (CADENA-ENTRA) TO CADENA-ENTRA
           INITIALIZE CADENA-SALE
           MOVE 1 TO J
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 60
              MOVE CADENA-ENTRA (I:1) TO IND-ALFABETICO
              IF SI-ALFABETICO
                 MOVE CADENA-ENTRA (I:1) TO CADENA-SALE (J:1)
                 ADD 1 TO J
              END-IF
           END-PERFORM.
      *******************************************************************

      *800-GRABAR-SECUENCIAL.
      * SE ARMA Y GRABA REGISTRO EN EL ARCHIVO ICBTCO, INDICANDO EN EL
      * NUMERO DE TERMINAL, LA TARIFA UTILIZADA Y EN EL NUMERO DE IDEN-
      * TIFICACION LA CANTIDAD DE VALIDACIONES REALIZADAS. SE GRABA UN
      * REGISTRO POR CADA TIPO DE RESULTADO DE LA VALIDACION
      *******************************************************************
       800-GRABAR-SECUENCIAL.
           INITIALIZE REG-SEC
           MOVE SUSCRIPTOR-AUX    TO COD-SUSCRIP-SEC
           MOVE FECHA-AUX-R       TO FEC-CONSUL-SEC
           MOVE ZEROS             TO TIP-ID-SEC
           MOVE W-TARIFA          TO NUM-TERM-SEC
           MOVE 2                 TO COBRA-SEC
           MOVE 1                 TO PRIV-SEC
           MOVE " "               TO SBPRIV-SEC
           MOVE 2                 TO PRODU-SEC
           MOVE COD-CLAVE-ABD09   TO COD-CLAVE-SEC
           MOVE CLAVE-AUT-ABD09   TO CLAVE-AUT-SEC
           MOVE CIUDAD-CONS-ABD09 TO CIUDAD-CONS-SEC
           MOVE REGIONAL-ABD14    TO REGIONAL-COMIS-SEC
           MOVE ZONA-FAC-ABD09    TO ZONA-FAC-SEC
           MOVE ZEROS             TO TABLA-SCOR-SEC
           ACCEPT TIEMPO-W        FROM TIME
           MOVE TIEMPO-W(1:6)     TO HORA-SEC
           IF W-CONT-TIPO-5       >  ZEROS
              ADD 1 TO LLAVE7
              MOVE LLAVE7 TO LLAVE-SEC
              MOVE W-CONT-TIPO-5  TO NUM-ID-SEC
              MOVE 5  TO TIP-CON-SEC
XXXXXX*       WRITE REG-SEC INVALID KEY
XXXXXX*             DISPLAY " ERROR ESCRIBIENDO ICBTCO:  " LLAVE-SEC
XXXXXX*       END-WRITE
           END-IF
           IF W-CONT-TIPO-6       >  ZEROS
              ADD 1 TO LLAVE7
              MOVE LLAVE7 TO LLAVE-SEC
              MOVE W-CONT-TIPO-6  TO NUM-ID-SEC
              MOVE 6  TO TIP-CON-SEC
XXXXXX*       WRITE REG-SEC INVALID KEY
XXXXXX*             DISPLAY " ERROR ESCRIBIENDO ICBTCO:  " LLAVE-SEC
XXXXXX*       END-WRITE
           END-IF
           IF W-CONT-TIPO-7       >  ZEROS
              ADD 1 TO LLAVE7
              MOVE LLAVE7 TO LLAVE-SEC
              MOVE W-CONT-TIPO-7  TO NUM-ID-SEC
              MOVE 7  TO TIP-CON-SEC
XXXXXX*       WRITE REG-SEC INVALID KEY
XXXXXX*             DISPLAY " ERROR ESCRIBIENDO ICBTCO:  " LLAVE-SEC
XXXXXX*       END-WRITE
           END-IF.
      *****************************************************************
      *810-BUSCAR-MUNICIPIO.
      * OBTIENE EL NOMBRE DEL MUNICIPIO Y EL DEPARTAMENTO AL QUE PERTE-
      * NECE CON EL FIN DE ALIMENTAR EL ARCHIVO DE VALIDAS
      *****************************************************************
       810-BUSCAR-MUNICIPIO.
           MOVE DEP-EXP-BDIIREGI TO CODIGO-DEP
           MOVE MUNIC-EXP-BDIIREGI TO CODIGO-MUNI
           READ MUNICIPIOS INVALID KEY
               MOVE SPACES TO NOMBRE-MUNI
               MOVE 71     TO CODIGO-DEP
           END-READ
           MOVE NOMBRE-MUNI           TO MUN-EXP-VAL MUN-EXP-INC
           MOVE T-NOM-DEP(CODIGO-DEP) TO DEP-EXP-VAL DEP-EXP-INC.
      *

      ******************************************************************
      *900-CERRAR-ARCHIVOS.
      * CIERRA CADA UNO DE LOS ARCHIVO UTILIZADOS EN EL PROGRMA QUE AUN
      * ESTAN ABIERTOS
      *****************************************************************
       900-CERRAR-ARCHIVOS.
           DISPLAY "REGISTROS LEIDOS                :" REG-LEIDOS
           DISPLAY "REGISTROS CORRECTOS             :" REG-CORRECTOS
PJV011     DISPLAY "REGISTROS CORRECTOS REPETIDOS   :" REG-CORRECTOS-R  CQ11407
           DISPLAY "REGISTROS NO COINCIDEN BDIIREGI :" NO-COINCIDE-REGIS
R7094B     DISPLAY "  REGISTROS DE FALLECIDOS         :" CONT-FALLECIDOSRMB0009
           DISPLAY "REGISTROS NO COINCIDEN BDIIDIAN :" NO-COINCIDE-DIAN
           DISPLAY "REGISTROS NO EXISTEN   BDIIREGI :" NO-EXISTE-REGIS
           DISPLAY "REGISTROS NO EXISTEN   BDIIDIAN :" NO-EXISTE-DIAN
           DISPLAY "TIPO IDENTIFICACION INVALIDO    :" TIPO-ID-INVALIDO
           CLOSE ENTRADA
                 BDIIDIAN
                 BDIIDAS
                 BDIIREGI
                 MUNICIPIOS
                 SECUENCIAL
                 VALIDAS
PJV011           VALIDAS-REP                                            CQ11407
                 INCONS.
      *****************************************************************
           COPY BDIIVIDE.PROC.
           COPY LEEMAESTROS.PROC.
           COPY BDLDIR.PROC.
CQ6271     COPY LOGPESVNO.PROC.                                         PJV0010
