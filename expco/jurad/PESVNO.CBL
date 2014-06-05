/d/iccol/desarrollo $cat PESVNO.CBL
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
      * LAG0002      * PAM372 - SEPARADORES ";" EN INCONSISTENCIAS
      * L.GOMEZ      * PARA IGUALARLO AL PESVNO
      * SEPT. 30/2009*
      ******************************************************************
      * GJC0003      * CSVCEX - AJUSTE A LA VALIDACION PARA CUENTAS SIN*
      * 2009/12/21   * VALIDAR CON CEDULA DE EXTRANJERIA.              *
      * GRACIELA JIM.*                                                 *
      ******************************************************************
      * LAG0004      * REQ550 - AJUSTE A LA VALIDACION PARA CUENTAS SIN*
      * 2010/03/12   * VALIDAR INCLUYENDO VALIDACION DE RANGOS.        *
      * G.DESARROLLO *                                                 *
      ******************************************************************
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
CTAST8     COPY ICBCHK0001.FS.
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

PJV011     SELECT VALIDAS-REP  ASSIGN TO DISK W-VALIDAS-REP
PJV011            ORGANIZATION IS LINE SEQUENTIAL
PJV011            ACCESS MODE  IS SEQUENTIAL.

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

PJV011 FD  VALIDAS-REP.
PJV011 01  REG-VALIDAS-REP.
PJV011     03 TIP-VALREP                PIC 9.
PJV011     03 CED-VALREP                PIC 9(11).
PJV011     03 NOM-VALREP                PIC X(45).
PJV011     03 FEC-EXP-VALREP            PIC 9(08).
PJV011     03 MUN-EXP-VALREP            PIC X(15).
PJV011     03 DEP-EXP-VALREP            PIC X(15).
PJV011     03 VIG-VALREP                PIC X(02).
PJV011     03 GENERO-VALREP             PIC X(01).
PJV011     03 EDAD-VALREP               PIC X(03).
PJV011     03 EDAD-RANGO-VALREP         PIC X(05).
PJV011     03 FILLER-2-VALREP           PIC X(51).

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
CQ6271 COPY LOGPESVNO.WS.
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
PJV011 01  W-VALIDAS-REP.
PJV011     03 W-VAL-DIRECTORIO      PIC X(12) VALUE "$TEMPORALES/".
PJV011     03 W-VAL-NOMBRE-REP      PIC X(30) VALUE SPACES.

PJV011 01  W-KEYVAL-GRA.
PJV011     03 TIP-KEYVAL            PIC 9(01) VALUE ZEROS.
PJV011     03 NUM-KEYVAL            PIC 9(11) VALUE ZEROS.
PJV011     03 SN-VALREP             PIC X(01) VALUE SPACES.

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
CQ6271     03  W-CONT-TIPO-5-PJ         PIC 9(06) VALUE ZEROS.
PJV011     03  W-CONT-TIPO-5-REP        PIC 9(06) VALUE ZEROS.
      *
       01  SUSCRIPTOR-AUX               PIC 9(06).
       01  REG-LEIDOS                   PIC 9(07) VALUE ZEROS.
R7094B 01  CONT-FALLECIDOS              PIC 9(07) VALUE ZEROS.
       01  REG-CORRECTOS                PIC 9(07) VALUE ZEROS.
PJV011 01  REG-CORRECTOS-R              PIC 9(07) VALUE ZEROS.
       01  NO-COINCIDE-DIAN             PIC 9(07) VALUE ZEROS.
       01  NO-COINCIDE-REGIS            PIC 9(07) VALUE ZEROS.
       01  NO-EXISTE-DIAN               PIC 9(07) VALUE ZEROS.
       01  NO-EXISTE-REGIS              PIC 9(07) VALUE ZEROS.
       01  SI-EXISTE-CSV                PIC 9(07) VALUE ZEROS.
       01  TIPO-ID-INVALIDO             PIC 9(07) VALUE ZEROS.
       01  NIT-BUENO                    PIC 9.
       01  NUM-ID-W                     PIC 9(11).
      *-----------------------------------------------------------------
       COPY  ICFILE.WS.
       COPY  BDIIALE.WS.
       COPY  DEPTOS.WS.
       COPY  BDIIVIDE.WS.
       COPY  BDLDIR.WS.
       COPY  LEEMAESTROS.WS.
      *-----------------------------------------------------------------
      * 05=tipo id no 1|2|3|4
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
      *-----------------------------------------------------------------
       01  W-VARIABLES.
           03 W-ANO-ACTUAL                PIC 9(04) VALUE ZEROS.
           03 W-EDAD-CEDULA               PIC 9(03) VALUE ZEROS.
           03 W-FEC-EXPE                  PIC 9(08) VALUE ZEROS.
           03 W-FEC-EXPE-R REDEFINES W-FEC-EXPE.
              05 W-ANO-EXPE               PIC 9(04).
              05 W-MES-EXPE               PIC 9(02).
              05 W-DIA-EXPE               PIC 9(02).
           03 WS-EDAD                     PIC 9(03).
      *-----------------------------------------------------------------
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
      *-----------------------------------------------------------------
       01  HACER-MATCH                 PIC  9(01).
       01  CADENA-ENTRA                PIC  X(60).
       01  CADENA-SALE                 PIC  X(60).
       01  IND-ALFABETICO              PIC  X(01).
           88 SI-ALFABETICO            VALUE " " "A" "B" "C" "D" "E" 
                                       "F" "G" "H" "I" "J" "K" "L" "M"
                                       "N" "/" "O" "P" "Q" "R" "S" "T"
                                       "U" "V" "W" "X" "Y" "Z" "".

           Este COPY no estaba: VALNOM.WS
******* VALNOM-WS.v.0002
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES QUE
******* UTILIZAN LAS RUTINAS DE NOMBRES DE LA RUTINA en VALNOM.PROC
*****************************************************************************
REMARK* GJC0002    * VALMEJ :SE CREAN ANUEVOS CAMPOS PARA MEJORAR LA VALIDACION
REMARK* G.JIMENEZ  * DE NIT. PARA MEJORA TAMBIEN SE AMPLIA TAMANO DE VARIABLES
REMARK* MARZO/2003 *
REMARK*************************************************************************
        01 VARIABLES-VALNOM.
           03 VECTOR-INPUT-VALNOM.
             04 VM-INPUT-VALNOM     OCCURS 20 TIMES .
                05 VMM-INPUT-VALNOM OCCURS 15 TIMES PIC X.
           03 VECTOR-OK-VALNOM.
             04 VM-OK-VALNOM         OCCURS 20 TIMES.
                05 VMM-OK-VALNOM    OCCURS 15 TIMES PIC X.
           03 BLOQOK-AUX-VALNOM           PIC 9(2).
           03 CADENA-VALNOM.
              04 FILLER                   PIC X(2) VALUE "A ".
              04 NOMBRE-AUX-VALNOM        PIC X(65).
              04 FILLER REDEFINES NOMBRE-AUX-VALNOM.
                 05 VNOM-AUX-VALNOM OCCURS 65 TIMES PIC X.
           03 BASURA-VALNOM               PIC X(10).
           03 BLOQUES-INPUT-VALNOM        PIC 9(2).
           03 NOMBRE-INPUT-VALNOM         PIC X(65).
           03 FILLER REDEFINES NOMBRE-INPUT-VALNOM.
              05 V-NOMBRE-VALNOM          PIC X OCCURS 65 TIMES.
           03 NOMBOK-INPUT-VALNOM         PIC X(65).
           03 FILLER REDEFINES NOMBOK-INPUT-VALNOM.
              05 V-NOMBOK-VALNOM          PIC X OCCURS 65 TIMES.
           03 TIP-ID-INPUT-VALNOM         PIC 9(02).
           03 MATCH-OK-OUT-VALNOM         PIC 9(01).
           03 A-VALNOM                    PIC 9(05).
VALMEJ     03 I-VALNOM                    PIC 9(05).                    GJC0002
VALMEJ     03 J-VALNOM                    PIC 9(05).                    GJC0002
VALMEJ     03 K-VALNOM                    PIC 9(05).                    GJC0002
VALMEJ     03 L-VALNOM                    PIC 9(05).                    GJC0002
VALMEJ     03 M-VALNOM                    PIC 9(05).                    GJC0002
           03 INI-VALNOM                  PIC 9(02).
           03 LON-VALNOM                  PIC 9(02).
           03 LETRAS-OK-VALNOM            PIC 9(02).
           03 LETRAS-OK2-VALNOM           PIC 9(02).
           03 TOTLET-OK-VALNOM            PIC 9(02).
           03 TOTLET-INP-VALNOM           PIC 9(02).
           03 PEDAZO-VALNOM               PIC X(15).
           03 POR-TOT-VALNOM              PIC 9(05).
           03 POR-INP-VALNOM              PIC 9(05).
           03 APROX-TOT-VALNOM            PIC 9(05).
           03 APROX-INP-VALNOM            PIC 9(05).
VALMEJ     03 VECTORES-VALNOM.                                          GJC0002
VALMEJ        04 FILLER                   OCCURS 256 TIMES.             GJC0002
VALMEJ           05 CONT-LET-INPUT-VALNOM PIC 9(2).                     GJC0002
VALMEJ           05 CONT-LET-OK-VALNOM    PIC 9(2).                     GJC0002
VALMEJ     03 VAL-APELLIDO-VALNOM         PIC 9.                        GJC0002



CQ6271 LINKAGE SECTION.
CQ6271 COPY LOGPESVNO.LKG.
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
R7094A     IF NOMBRE-E = "VALIDAR=NO" AND FIN-ENTRADA = 0
R7094A        MOVE 0 TO W-BLOQUES
R7094A        DISPLAY "Registro  control==> 000001 (VALIDAR=NO)"
R7094A        MOVE 0 TO W-BLOQUES
R7094A        READ ENTRADA
R7094A          AT END
R7094A                MOVE 1 TO FIN-ENTRADA
R7094A          NOT AT END
R7094A                INSPECT NUMID-X-E REPLACING ALL " " BY "0"
R7094A                INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"
R7094A        END-READ
R7094A     ELSE
R7094A        DISPLAY "Registro  control==> 000000 (VALIDAR=NO)"
R7094A     END-IF.
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
PJV011      DISPLAY "Registros Tipo 5 repetidos ==> " W-CONT-TIPO-5-REP
      *        DISPLAY "*-----------------------------------*"
      *        DISPLAY "Registros validados en BDACSV:" SI-EXISTE-CSV
      *        DISPLAY "*------------------------------------*"
           END-IF
           PERFORM 900-CERRAR-ARCHIVOS
CQ6271     MOVE "V"               TO TIPPRO-LOGPESVNO-WK
CQ6271     MOVE "PESVNO"          TO PROGRAM-LOGPESVNO-WK
CQ6271     PERFORM 1000-UPDATE-LOGPESVNO
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
MODBD2     DISPLAY "!!..ENTRADA CORRECTOS INCORRECTOS TARIFA BLOQUES.!!"
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
R7094A*       (W-BLOQUES < 0 OR > 4)
R7094A         W-BLOQUES > 4
                 DISPLAY "!!!!!!    FALTA ALGUN PARAMETRO  !!!!!!! "
                 STOP RUN
           END-IF
R7094A     IF W-BLOQUES = 0
R7094A        DISPLAY "!!..EJECUCION SIN VALIDAR NOMBRES       .....!!"
R7094A     END-IF

PJV011     INITIALIZE W-VAL-NOMBRE-REP
PJV011     STRING W-VAL-NOMBRE DELIMITED BY "."
PJV011     ".vre" DELIMITED BY SIZE INTO W-VAL-NOMBRE-REP
PJV011     END-STRING

           MOVE W-ENT-NOMBRE TO W-SORT-NOM-ENT

           MOVE W-ENT-NOMBRE TO W-SORT-NOM-ENT W-SORT-NOM-SAL
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
XXXXXX     OPEN INPUT  SECUENCIAL
           OPEN OUTPUT VALIDAS
                       INCONS
PJV011                 VALIDAS-REP
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

      ******************************************************************
      *400-VALIDAR
      * DEPENDIENDO DEL TIPO DE IDENTIFICACION LEIDO EN EL ARCHIVO DE EN
      * TRADA LLAMA A LA CORRESPONDIENTE RUTINA DE VALIDACION (CEDULA,
      * NIT O DAS)
      * ACUMULA EN CONTADORES DEPENDIENDO DEL RESULTADO DE LA VALIDACION
      * LEE SECUENCIALMENTE EL ARCHIVO DE ENTRADA
      ******************************************************************
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
R7094A*        MOVE    1       TO  SW-VERIFNOM-BDIIVIDE
R7094A*                           SW-DIGCHEQUEO-BDIIVIDE
R7094A         IF W-BLOQUES > 0
R7094A            MOVE    1       TO  SW-VERIFNOM-BDIIVIDE
R7094A         END-IF
R7094D         IF TIPOID-E = 2 OR 3
R7094D             MOVE 1 TO SW-DIGCHEQUEO-BDIIVIDE
R7094D         END-IF
               MOVE  W-BLOQUES TO  NUM-BLOQUES-BDIIVIDE
      *        Valida el numero de identificacion contra las bases de
      *        datos de validacion de informacion BDIIDIAN, BDIIDAS,
      *        BDIIREGI y verifica la coincidencia del nombre reportado
               PERFORM 80100-VALIDAR-IDENTIFICACION
               MOVE  TIP-IDE-BDIIVIDE TO TIPOID-E
               MOVE  NUM-IDE-BDIIVIDE TO NUMID-E
               INITIALIZE REG-VALIDAS REG-INCONS
REQ596         MOVE FILLER-2-E TO FILLER-2-VAL
REQ596         MOVE FILLER-2-E TO FILLER-2-INC
               MOVE ";" TO FILLER1 FILLER2 FILLER3 FILLER4 FILLER5
                        FILLER6 FILLER7 FILLER8 FILLER9
               IF SW-ID-BDIIVIDE = 1
                   IF TIPOID-E <> 1
CSVCEX                 MOVE  ZEROS TO HACER-MATCH
MODCED                 MOVE 0 TO IDE-TIPO-BDAMATCH
CSVCEX                 PERFORM  513-VALIDA-EN-CSV
CSVCEX                 IF IDE-TIPO-BDAMATCH  =  0
CSVCEX                     MOVE 03 TO CODIGO-ERROR CODERR-INC
CSVCEX                     MOVE 1 TO SW-ERROR
CSVCEX                     PERFORM 512-CREE-REGISTRO
CSVCEX                     WRITE REG-INCONS
CSVCEX                     ADD 1 TO NO-EXISTE-DIAN
CSVCEX                 END-IF
                   ELSE
                       MOVE  ZEROS TO  HACER-MATCH
MODCED                 MOVE 0 TO IDE-TIPO-BDAMATCH
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
R7094C*                IF W-BLOQUES > 1 OR TIPOID-E <> 1
R7094C*                    MOVE NOM-VAL-BDIIVIDE         TO NOM-VAL
R7094C*                ELSE
R7094C*                    MOVE PRIMER-APELLIDO-BDIIVIDE TO NOM-VAL
R7094C*                END-IF
R7094C                 MOVE NOM-VAL-BDIIVIDE         TO NOM-VAL
                       INITIALIZE LLAVE-BDIIALE
                       MOVE  TIPOID-E TO TIP-IDE-BDIIALE
                       MOVE  NUMID-E  TO NUM-IDE-BDIIALE
                       MOVE  990003   TO FUENTE-BDIIALE
                       START BDIIALE KEY NOT < LLAVE-BDIIALE
                       INVALID KEY
                           MOVE 1 TO SW-BDIIALE
                       NOT INVALID KEY
                           MOVE 0 TO SW-BDIIALE
                           READ BDIIALE NEXT RECORD
                           AT END
                               MOVE 1 TO SW-BDIIALE
                           END-READ
                       END-START
                       PERFORM UNTIL SW-BDIIALE = 1
                       or TIPOID-E <> TIP-IDE-BDIIALE
                       or NUMID-E <> NUM-IDE-BDIIALE
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
                           IF ESTADO-VIG-BDIIREGI = 12
                           AND W-DERECHOS = 1
                               MOVE 07 TO CODIGO-ERROR CODERR-INC
                               MOVE 1 TO SW-ERROR
                               MOVE "00" TO VIG-INC
                               PERFORM 510-ESCRIBIR-INCONS
                           ELSE
                               IF ESTADO-VIG-BDIIREGI = 21
                                   MOVE  06  TO CODIGO-ERROR CODERR-INC
                                   MOVE   1  TO SW-ERROR
                                   MOVE "21" TO VIG-INC
                                   PERFORM 510-ESCRIBIR-INCONS
                                   ADD   1   TO NO-COINCIDE-REGIS
R7094B                             ADD   1   TO CONT-FALLECIDOS
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
PAM460*                            IF ORIGEN-VAL = SPACES
PAM460*                                MOVE   "O"      TO ORIGEN-VAL
PAM460*                            END-IF
PJV011                             PERFORM 301-GRABA-LOS-VALIDOS
                               END-IF
                           END-IF
                       ELSE
                           IF TIPOID-E = 4
                               MOVE NACIO-BDIIDAS TO MUN-EXP-VAL
                               MOVE ZEROS TO VIG-VAL
                           END-IF
                           MOVE NOM-VAL-BDIIVIDE TO NOM-VAL
                           MOVE TIPOID-E TO TIP-VAL TIP-INC
                           MOVE NUMID-E TO CED-VAL CED-INC
PAM460*                    IF ORIGEN-VAL = SPACES
PAM460*                        MOVE "O" TO ORIGEN-VAL
PAM460*                    END-IF
PJV011                     PERFORM 301-GRABA-LOS-VALIDOS
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
PJV011             IF SN-VALREP = "S"
PJV011                 ADD 1  TO W-CONT-TIPO-5-REP
PJV011             ELSE
                       ADD 1  TO W-CONT-TIPO-5
PJV011             END-IF
CQ6271             IF TIPOID-E = 2 OR 3
CQ6271                 ADD 1 TO W-CONT-TIPO-5-PJ
CQ6271             END-IF
               END-IF
           END-IF
           READ ENTRADA
           AT END
               MOVE 1 TO FIN-ENTRADA
           NOT AT END
               INSPECT  NUMID-X-E REPLACING ALL " " BY "0"
               INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"
           END-READ.

PJV011 300-GRABA-VALREP.
PJV011     INITIALIZE REG-VALIDAS-REP
PJV011     WRITE REG-VALIDAS-REP FROM REG-VALIDAS
PJV011     MOVE TIP-VAL TO TIP-KEYVAL
PJV011     MOVE CED-VAL TO NUM-KEYVAL
PJV011     ADD 1 TO REG-CORRECTOS-R.

PJV011 301-GRABA-LOS-VALIDOS.
PJV011     MOVE "N" TO SN-VALREP
PJV011     IF TIP-KEYVAL = TIPOID-E AND
PJV011        NUM-KEYVAL = NUMID-E
PJV011        MOVE "S" TO SN-VALREP
PJV011        PERFORM 300-GRABA-VALREP
PJV011     ELSE
PJV011        WRITE REG-VALIDAS
PJV011        MOVE TIP-VAL TO TIP-KEYVAL
PJV011        MOVE CED-VAL TO NUM-KEYVAL
PJV011        ADD 1 TO REG-CORRECTOS
PJV011     END-IF.


       505-RANGO-EDADES.
      *--------------------------------------------------*
      *505-RANGO-EDADES.
      *  RANGO MUJERES: 20'000.001 A 70'000.000
      *  RANGO HOMBRES:          1 A 20'000.000
      *        MAYOR  : > 70'000.000 < 1000'000.000 (NUIPS)
      *--------------------------------------------------*
           MOVE FEC-EXP-VAL TO W-FEC-EXPE
           MOVE ZEROS       TO W-EDAD-CEDULA
           IF W-ANO-EXPE >= 1975
               COMPUTE W-EDAD-CEDULA =
               ((W-ANO-ACTUAL - W-ANO-EXPE) + 18)
           ELSE
               COMPUTE W-EDAD-CEDULA =
               ((W-ANO-ACTUAL - W-ANO-EXPE) + 21)
           END-IF
           MOVE W-EDAD-CEDULA TO EDAD-VAL WS-EDAD
           MOVE FILLER-2-E TO FILLER-2-VAL FILLER-2-INC.
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
               MOVE TIPOID-E TO TIP-INC
           end-if
           IF SW-ERROR = 1
               PERFORM 512-CREE-REGISTRO
               WRITE REG-INCONS
           end-if.

      **************************************************************
      * ARMA EL REGISTRO DE INCONSISTENCIAS DEL ARCHIVO DCINC
      **************************************************************
       512-CREE-REGISTRO.
           MOVE TIPOID-E TO TIP-INC
           MOVE NUMID-E  TO CED-INC
           MOVE NOMBRE-E TO NOM1-INC
           IF CODIGO-ERROR = 01 OR 02 OR 06 OR 07
               MOVE NOM-VAL-BDIIVIDE TO NOM2-INC
           ELSE
               MOVE SPACES TO NOM2-INC MUN-EXP-INC DEP-EXP-INC VIG-INC
               MOVE ZEROS TO FEC-EXP-INC
               MOVE FILLER-2-E TO FILLER-2-INC
           END-IF.

      **************************************************************
      * Valida si la identificacion esta en la base de datos
      * alterna
      **************************************************************
       513-VALIDA-EN-CSV.
REQ550*    hace validación del tipo de identificacion
REQ550     MOVE TIPOID-E TO TIP-IDE-BDIIVIDE
REQ550     MOVE NUMID-E TO NUM-IDE-BDIIVIDE
      *    controla que NUM-IDE-BDIIVIDE esté dentro del rango de
      *    números que corresponde al valor de TIP-IDE-BDIIVIDE
REQ550     PERFORM 80400-VALIDAR-TIPO-ID
REQ550     IF SW-TID-BDIIVIDE = 0 *> el test fué OK
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
REQ609             IF IDE-TIPO-BDAMATCH = 1
REQ609                 IF TIPOID-E = 1 OR 4
REQ609                     INSPECT NOM-OUTPUT-BDAMATCH REPLACING ALL
REQ609                     "," BY " ", ";" BY " "
REQ609                 END-IF
REQ609                 MOVE NOMBRE-E TO NOMBRE-INPUT-VALNOM
REQ609                 MOVE NOM-OUTPUT-BDAMATCH TO NOMBOK-INPUT-VALNOM
REQ609                 MOVE TIPOID-E TO TIP-ID-INPUT-VALNOM
REQ609                 MOVE NUM-BLOQUES-BDIIVIDE TO BLOQUES-INPUT-VALNOM
REQ609                 PERFORM 10000-MATCH-NOMBRES-VALNOM
REQ609                 IF MATCH-OK-OUT-VALNOM = 1
REQ609                     MOVE 1 TO IDE-TIPO-BDAMATCH
REQ609                 ELSE
R7094A                     IF W-BLOQUES > 0
R7094A                         MOVE 0 TO IDE-TIPO-BDAMATCH
R7094A                     END-IF
REQ609                 END-IF
REQ609             END-IF
     
                   IF IDE-TIPO-BDAMATCH = 1
                       MOVE TIPOID-E TO TIP-VAL
                       MOVE NUMID-E  TO CED-VAL
                       MOVE NOM-OUTPUT-BDAMATCH TO CADENA-ENTRA
                       PERFORM 9800-FILTRAR-ALFABETICO
                       MOVE CADENA-SALE TO  NOM-VAL
                       MOVE ZEROS  TO FEC-EXP-VAL
                       MOVE SPACES TO MUN-EXP-VAL
                       MOVE SPACES TO DEP-EXP-VAL
                       MOVE SPACES TO VIG-VAL
                       MOVE SPACES TO GENERO-VAL
                       MOVE SPACES TO EDAD-VAL
                       MOVE SPACES TO EDAD-RANGO
PJV011                 PERFORM 301-GRABA-LOS-VALIDOS
PJV011                 ADD 1 TO SI-EXISTE-CSV
                   END-IF
               END-IF
           end-if.

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
      ******************************************************************

      *800-GRABAR-SECUENCIAL.
      * SE ARMA Y GRABA REGISTRO EN EL ARCHIVO ICBTCO, INDICANDO EN EL
      * NUMERO DE TERMINAL, LA TARIFA UTILIZADA Y EN EL NUMERO DE IDEN-
      * TIFICACION LA CANTIDAD DE VALIDACIONES REALIZADAS. SE GRABA UN
      * REGISTRO POR CADA TIPO DE RESULTADO DE LA VALIDACION
      ******************************************************************
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
           END-IF
           IF W-CONT-TIPO-6       >  ZEROS
              ADD 1 TO LLAVE7
              MOVE LLAVE7 TO LLAVE-SEC
              MOVE W-CONT-TIPO-6  TO NUM-ID-SEC
              MOVE 6  TO TIP-CON-SEC
           END-IF
           IF W-CONT-TIPO-7       >  ZEROS
              ADD 1 TO LLAVE7
              MOVE LLAVE7 TO LLAVE-SEC
              MOVE W-CONT-TIPO-7  TO NUM-ID-SEC
              MOVE 7  TO TIP-CON-SEC
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
PJV011     DISPLAY "REGISTROS CORRECTOS REPETIDOS   :" REG-CORRECTOS-R
           DISPLAY "REGISTROS NO COINCIDEN BDIIREGI :" NO-COINCIDE-REGIS
R7094B     DISPLAY "  REGISTROS DE FALLECIDOS         :" CONT-FALLECIDOS
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
PJV011           VALIDAS-REP
                 INCONS.
      *****************************************************************
           COPY BDIIVIDE.PROC.

      *BDIIVIDE.PROC.v.03.0008
      ******************************************************************
      *RUTINA DE VALIDACION DE IDENTIFICACIONES
      ******************************************************************
      * F.CUADROS   * INCLUSION DE PARAMETRO PARA ASIGNAR EL TIPO DE
      * JUN.17/2004 * IDENTIFICACION DE ACUERDO A RANGOS PERMITIDOS
      *             * NUIP01 * RUTINA 80300-ASIGNAR-TIPO-ID
      *             * INCLUSION DE PARAMETRO PARA VALIDAR EL TIPO-ID
      * FAC0002     * NUIP01 * RUTINA 80400-VALIDAR-TIPO-ID.
      ******************************************************************
      * F.CUADROS   * RELLENAR DE CEROS EL NUMERO DE IDENTIFICAICON
      * JUL.16/2004 * CUANDO SE LE QUITA EL DIGITO DE CHEQUEO
      * FAC0003     * NUIP02 * VALIDACION DE IDENTIFICACION SI TIPO-ID 2
      ******************************************************************
      * F.CUADROS   * NUIP03: INCLUSION DE RUTINA PARA ELIMINAR EL
      * AGO.11/2004 * DIGITO DE CHEQUEO.
      * FAC0004     * CAMBIO DE TIPO ID = 2 POR 3 SI EL NIT NO
      *             * CORRESPONDE AL RANGO.
      ******************************************************************
      * F.CUADROS   * RANGO5: DEFINICION DE NUEVOS RANGOS PARA TIPO ID 3
      * OCT.22/2004 * ENTRE 700.000.001 Y 799.999.999 (OTROS DOCUMENTOS)
      * FAC0005     * DEFINICION DE NUEVOS RANGOS PARA TIPO ID 2:
      *             * ENTRE 900.000.000 Y 999.999.999 (MUISCA)
      ******************************************************************
      * F.CUADROS   * NUIP04: INCLUSION DE RANGO PARA TIPO ID. 2 EN
      * OCT.19/2005 * RUTINA VALIDAR-TIPO-ID PARA NITS QUE SON NUIPS
      * FAC006C     *
      ******************************************************************
      * C.VELASCO   * SE VUELVE A LA VERSION ANTES DE MODIFICAR LOS
      * JUL.12/2012 * RANGOS DE IDENTIFICACIONES,  LOS AJUSTES DE LA
      * CAV0008     * VERSION 07, IMPACTA APERTURAS Y PROCESOS ESPEC.
      ******************************************************************
       80100-VALIDAR-IDENTIFICACION.
      *===================================*
      * Valida el numero de identificacion contra las bases de datos
      * de validacion de informacion BDIIDIAN, BDIIDAS, BDIIREGI
      * y verifica la coincidencia del nombre reportado
      *===================================*
           INITIALIZE PARAMETROS-SALIDA-BDIIVIDE VARIABLES-VALNOM
           NOMBRES-AUX-W-BDIIVIDE
           INSPECT NUM-IDE-BDIIVIDE REPLACING ALL " " BY "0"

      *    control de validez de los parámetros de la llamada
NUIP01     IF SW-VERIFNOM-BDIIVIDE NOT NUMERIC
NUIP01        MOVE ZEROS TO SW-VERIFNOM-BDIIVIDE
NUIP01     END-IF
NUIP01     IF SW-ASGTIPID-BDIIVIDE NOT NUMERIC
NUIP01        MOVE ZEROS TO SW-ASGTIPID-BDIIVIDE
NUIP01     END-IF
NUIP01     IF SW-VALTIPID-BDIIVIDE NOT NUMERIC
NUIP01        MOVE ZEROS TO SW-VALTIPID-BDIIVIDE
NUIP01     END-IF
NUIP02     IF SW-DIGCHEQUEO-BDIIVIDE NOT NUMERIC
NUIP02        MOVE ZEROS TO SW-DIGCHEQUEO-BDIIVIDE
NUIP02     END-IF

           IF NUM-IDE-BDIIVIDE not NUMERIC or NUM-IDE-BDIIVIDE = ZEROS
               MOVE 1 TO SW-ID-BDIIVIDE
               exit paragraph
           end-if
NUIP01     IF SW-ASGTIPID-BDIIVIDE = 1
      *        calcula el tipo de ID según el número
NUIP01         PERFORM 80300-ASIGNAR-TIPO-ID
NUIP01     END-IF
NUIP01     IF SW-VALTIPID-BDIIVIDE = 1
      *        valida el tipo de ID SW-TID-BDIIVIDE = 1 si no es OK
NUIP01         PERFORM 80400-VALIDAR-TIPO-ID
NUIP01     END-IF
NUIP03     IF SW-DIGCHEQUEO-BDIIVIDE = 1
NUIP03         PERFORM 80500-ELIMINAR-DIGITO-CHEQUEO
NUIP03     END-IF
NUIP01     IF SW-VALTIPID-BDIIVIDE = 0
NUIP01     or (SW-VALTIPID-BDIIVIDE = 1 AND SW-TID-BDIIVIDE = 0)
      *        Validacion del numero de identificacion
      *        con las bases de datos de la REGISTRADURIA, DAS, DIAN
      *        dependiendo del tipo de identificación
               PERFORM 80200-VALIDAR-ID
               INITIALIZE NOM-VAL-BDIIVIDE
               MOVE NOM-LEIDO-BDIIVIDE TO NOM-VAL-BDIIVIDE
               IF SW-VERIFNOM-BDIIVIDE = 1
                   MOVE TIP-IDE-BDIIVIDE TO TIP-ID-INPUT-VALNOM
                   MOVE NOMBRE-BDIIVIDE TO NOMBRE-INPUT-VALNOM
                   MOVE NOM-VAL-BDIIVIDE TO NOMBOK-INPUT-VALNOM
                   IF SW-ID-BDIIVIDE = 0
                       IF NUM-BLOQUES-BDIIVIDE NUMERIC
                       and NUM-BLOQUES-BDIIVIDE <> ZEROS
                           MOVE NUM-BLOQUES-BDIIVIDE
                           TO BLOQUES-INPUT-VALNOM
                       ELSE 
                           IF TIP-ID-INPUT-VALNOM = 1
                               MOVE 2 TO BLOQUES-INPUT-VALNOM
                           ELSE
                               MOVE 1 TO BLOQUES-INPUT-VALNOM
                           END-IF
                       END-IF
      *                Determinar si el nombre de la persona consultada
      *                hace con el nombre enviado en la consulta
                       PERFORM 10000-MATCH-NOMBRES-VALNOM
                       IF MATCH-OK-OUT-VALNOM = 0
                           MOVE 1 TO SW-NOMBRES-BDIIVIDE
                       END-IF
                   END-IF
               END-IF
NUIP01     ELSE
NUIP01         IF SW-TID-BDIIVIDE = 1
NUIP01             MOVE 1 TO SW-ID-BDIIVIDE
NUIP01         END-IF
           END-IF.

       80200-VALIDAR-ID.
      *===================================*
      * Validacion del numero de identificacion
      * con las BASES DE DATOS DE LA REGISTRADURIA, DAS, DIAN
      * dependiendo del tipo de identificacion
      *===================================*
           EVALUATE TIP-IDE-BDIIVIDE
           WHEN 1
      *        busca ID en REGIS, arma nombre leído
               PERFORM 85010-UBICA-CEDULA
           WHEN 2
               PERFORM 85020-UBICA-NIT
           WHEN 3
      *        ubica NIT en archivo de la DIAN, BDIIDIAN
               PERFORM 85020-UBICA-NIT
           WHEN 4
               PERFORM 85030-UBICA-EXT
           WHEN OTHER
               MOVE 1 TO SW-ID-BDIIVIDE
           END-EVALUATE.

NUIP01 80300-ASIGNAR-TIPO-ID.
      *================================================================
NUIP01* ASIGNACION DEL TIPO DE IDENTIFICACION en TIP-IDE-BDIIVIDE.
NUIP01* PARAMETRO DE ENTRADA SW-ASGTIPID-BDIVIIDE = 1;
NUPI01* RETORNA TIPID-VAL-BDIIVIDE
      *================================================================
NUIP01    INITIALIZE PARAMETROS-SALIDA-BDIIVIDE TIP-IDE-BDIIVIDE
NUIP01    INSPECT NUM-IDE-BDIIVIDE REPLACING ALL " " BY "0"
NUIP01      IF (NUM-IDE-BDIIVIDE-R >= 1 AND <= 99999999)
NUIP01      OR (NUM-IDE-BDIIVIDE-R >= 1000000000 AND < 8000000000)
NUIP01         MOVE 1 TO TIP-IDE-BDIIVIDE
NUIP01      ELSE
NUIP01        IF (NUM-IDE-BDIIVIDE-R >= 800000000 AND <= 999999999)
NUIP02        OR (NUM-IDE-BDIIVIDE-R >= 8000000000 AND <= 9999999999)
NUIP01           MOVE 2 TO TIP-IDE-BDIIVIDE
NUIP01        ELSE
NUIP01          IF (NUM-IDE-BDIIVIDE-R >= 600000000 AND < 800000000)
NUIP01          OR (NUM-IDE-BDIIVIDE-R >= 6000000000 AND < 8000000000)
NUIP01             MOVE 3 TO TIP-IDE-BDIIVIDE
NUIP01          ELSE
                   MOVE 4 TO TIP-IDE-BDIIVIDE
                end-if
              end-if
NUIP01      END-IF.

NUIP01 80400-VALIDAR-TIPO-ID.
      *================================================================
NUIP01*80400-VALIDAR-TIPO-ID.
NUIP01* VALIDACION DEL TIPO DE IDENTIFICACION DE ACUERDO AL RANGO
NUIP01* CONTENIDO EN EL NUMERO DE IDENTIFICACION. SE PREGUNTA AL REVES.
NUIP01* PARAMETRO DE ENTRADA SW-VALTIPID-BDIVIIDE = 1;
NUIP01* RETORNA SW-TID-BDIIVIDE = 1 SI ESTA ERRADO.
      *================================================================
NUIP01     INITIALIZE PARAMETROS-SALIDA-BDIIVIDE
NUIP01     INSPECT NUM-IDE-BDIIVIDE REPLACING ALL " " BY "0"
NUIP01     EVALUATE TIP-IDE-BDIIVIDE
NUIP01     WHEN 1
NUIP01         IF NUM-IDE-BDIIVIDE-R > 9999999999
NUIP01             MOVE 1 TO SW-TID-BDIIVIDE
NUIP01         END-IF
NUIP01     WHEN 2
NUIP04         IF NUM-IDE-BDIIVIDE-R >  9999999999 OR
RANGO5             (NUM-IDE-BDIIVIDE-R <= 800000000 AND >= 600000000)
NUIP01             MOVE 1 TO SW-TID-BDIIVIDE
NUIP01         END-IF
NUIP01     WHEN 3
RANGO5         IF NUM-IDE-BDIIVIDE-R > 800000000 OR < 600000000
NUIP01             MOVE 1 TO SW-TID-BDIIVIDE
NUIP01         END-IF
NUIP01     END-EVALUATE.

NUIP03 80500-ELIMINAR-DIGITO-CHEQUEO.
      *================================================================
NUIP03*80500-ELIMINAR-DIGITO-CHEQUEO.
      * Elimina el digito de chequeo y cambia el tipo de identificacion
      * de acuerdo a los rangos para tipo id = 2 y 3
      *================================================================
NUIP03      IF (NUM-IDE-BDIIVIDE-R >= 600000000 AND <  800000000)
NUIP03      OR (NUM-IDE-BDIIVIDE-R >= 6000000000 AND < 8000000000)
NUIP03          MOVE 3 TO TIP-IDE-BDIIVIDE
NUIP03      END-IF
NUIP03      IF (NUM-IDE-BDIIVIDE-R >= 800000000 AND <= 999999999)
NUIP03      OR (NUM-IDE-BDIIVIDE-R >= 8000000000 AND <= 9999999999)
NUIP03          MOVE 2 TO TIP-IDE-BDIIVIDE
NUIP03      END-IF
NUIP03      IF (TIP-IDE-BDIIVIDE = 2
NUIP03      AND (NUM-IDE-BDIIVIDE-R >= 10000000000)
NUIP03      OR (NUM-IDE-BDIIVIDE-R >= 8000000000 AND <= 9999999999))
NUIP03      OR (TIP-IDE-BDIIVIDE = 3
NUIP03      AND (NUM-IDE-BDIIVIDE-R >= 6000000000 AND <  8000000000))
NUIP03          MOVE NUM-IDE-BDIIVIDE TO NUM-ID-W11-BDIIVIDE
NUIP03          MOVE NUM-ID-W10-BDIIVIDE TO NUM-IDE-BDIIVIDE-R
NUIP03      END-IF
NUIP03      INSPECT NUM-IDE-BDIIVIDE REPLACING ALL " " BY "0".

       85010-UBICA-CEDULA.
      *================================================================
      * Ubica la identificacion dada la llave en BDIIREGI
      * Arma el nombre completo en una sola variable
      *================================================================
           MOVE NUM-IDE-BDIIVIDE TO CEDULA-BDIIREGI
           MOVE TIP-IDE-BDIIVIDE TO TIPO-ID-BDIIREGI
           READ BDIIREGI INVALID KEY
               MOVE 1 TO SW-ID-BDIIVIDE
           END-READ
           IF SW-ID-BDIIVIDE = 0
              MOVE APELLIDO1-BDIIREGI TO PRIMER-APELLIDO-BDIIVIDE
              MOVE APELLIDO2-BDIIREGI TO SEGUNDO-APELLIDO-BDIIVIDE
              MOVE DES-NOMBRE2-BDIIREGI TO SEGUNDO-NOMBRE-BDIIVIDE
              MOVE DES-NOMBRE1-BDIIREGI TO PRIMER-NOMBRE-BDIIVIDE
              IF ESTADO-CIVIL-BDIIREGI = 0
                  STRING
                   SEGUNDO-APELLIDO-BDIIVIDE DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE ,
                   PRIMER-NOMBRE-BDIIVIDE    DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE ,
                   SEGUNDO-NOMBRE-BDIIVIDE   DELIMITED BY SIZE,
                   ";" DELIMITED BY SIZE ,
                   INTO NOM-LEIDO-BDIIVIDE
               END-IF
               IF ESTADO-CIVIL-BDIIREGI = 1
                   STRING
                   "DE" DELIMITED BY SIZE,
                   "*"  DELIMITED BY SIZE,
                   SEGUNDO-APELLIDO-BDIIVIDE DELIMITED BY "  ",
                   "*"  DELIMITED BY SIZE,
                   PRIMER-NOMBRE-BDIIVIDE    DELIMITED BY "  ",
                   "*"  DELIMITED BY SIZE,
                   SEGUNDO-NOMBRE-BDIIVIDE   DELIMITED BY SIZE,
                   ";"  DELIMITED BY SIZE,
                   INTO NOM-LEIDO-BDIIVIDE
               END-IF
               IF ESTADO-CIVIL-BDIIREGI = 2
                  STRING
                   "VDA DE" DELIMITED BY SIZE,
                   "*"      DELIMITED BY SIZE,
                   SEGUNDO-APELLIDO-BDIIVIDE DELIMITED BY "  ",
                   "*"      DELIMITED BY SIZE,
                   PRIMER-NOMBRE-BDIIVIDE    DELIMITED BY "  ",
                   "*"      DELIMITED BY SIZE,
                   SEGUNDO-NOMBRE-BDIIVIDE DELIMITED BY SIZE,
                   ";"      DELIMITED BY SIZE,
                   INTO NOM-LEIDO-BDIIVIDE
               END-IF
               STRING PRIMER-APELLIDO-BDIIVIDE DELIMITED BY "  ",
                  "*"             DELIMITED BY SIZE,
                  NOM-LEIDO-BDIIVIDE     DELIMITED BY SIZE, "*",
                                DELIMITED BY SIZE,
                  INTO NOM-VAL-BDIIVIDE

               INSPECT NOM-VAL-BDIIVIDE REPLACING ALL "*" BY " "
               INSPECT NOM-VAL-BDIIVIDE REPLACING ALL ";" BY " "
               INITIALIZE NOM-LEIDO-BDIIVIDE
               MOVE NOM-VAL-BDIIVIDE TO NOM-LEIDO-BDIIVIDE.

       85020-UBICA-NIT.
      *================================================================
      * Ubica NIT en archivo de la DIAN, BDIIDIAN
      *================================================================
           MOVE NUM-IDE-BDIIVIDE TO NITS-R-BDIIDIAN
           READ BDIIDIAN INVALID KEY
               MOVE 1 TO SW-ID-BDIIVIDE
           END-READ
           IF SW-ID-BDIIVIDE = 0
               MOVE DESCRIPCION-BDIIDIAN TO NOM-LEIDO-BDIIVIDE
           END-IF.

       85030-UBICA-EXT.
      *================================================================
      * Ubica cedula de extranjeria en archivo del DAS
      *================================================================
           MOVE NUM-IDE-BDIIVIDE TO NUM-ID-BDIIDAS
           MOVE TIP-IDE-BDIIVIDE TO TIPO-ID-BDIIDAS
           READ BDIIDAS INVALID KEY
               MOVE 1 TO SW-ID-BDIIVIDE
           END-READ
           IF SW-ID-BDIIVIDE = 0
               MOVE NOMBRE-BDIIDAS TO NOM-LEIDO-BDIIVIDE
           end-if.



           COPY LEEMAESTROS.PROC.
           COPY BDLDIR.PROC.
CQ6271     COPY LOGPESVNO.PROC.


REMARK*VALNOM-PROC.v.0005
REMARK******************************************************************
REMARK* RUTINA CREADA PARA HACER LA VALIDACION DE NOMBRES CONTRA LOS
REMARK* ARCHIVOS DE LA REGISTRADURIA, DIAN Y DAS, DEPENDIENDO DEL TIPO
REMARK* DE IDENTIFICACION RECIBIDA.
REMARK*    PARAMETROS
REMARK*        BLOQUES-INPUT-VALNOM - NUMERO DE BLOQUES A VALIDAR
REMARK*        NOMBRE-INPUT-VALNOM  - NOMBRE A VALIDAR
REMARK*        NOMBOK-INPUT-VALNOM  - NOMBRE CORRECTO CON EL QUE SE
REMARK*                               DEBE COMPARAR NOMBRE-INPUT-VALNOM
REMARK*        TIP-ID-INPUT-VALNOM  - TIPO DE IDENTIFICACION A VALIDAR
REMARK*    SALIDAS:
REMARK*        MATCH-OK-OUT-VALNOM  - 0-NO HUBO       1-HUBO
REMARK*******************************************************************
REMARK* RMB0002   * VAL001: SE MEJORA LA VALIDACION DE NOMBRES EN PERSONAS
REMARK* R.MARTINEZ* JURIDICAS
REMARK* DIC.2002  *
REMARK*******************************************************************
REMARK* GJC0003    * VALMEJ :SE CREAN ANUEVOS CAMPOS PARA MEJORAR LA VALIDACION
REMARK* G.JIMENEZ  * DE NIT. PARA MEJORA TAMBIEN SE AMPLIA TAMANO DE VARIABLES
REMARK* MARZO/2003 * PARA CEDULAS, EL COMODIN QUE USUALMENTE CORRESPONDE A LA N
REMARK*            * SOLO SE COMPARA CONTRA N O CONTRA BLANCOS
REMARK*************************************************************************
REMARK* GJC0004    * VALME1 :CON EL FIN DE AFINAR AUN MAS EL PROCESO DE VALIDA-
REMARK* R.MARTINEZ * CION DE NOMBRES, EN EL MATCH A PROXIMADO DE PERSONAS NATU-
REMARK* G.JIMENEZ  * RALES SE CAMBIA EL CALCULO FINAL PARA DETERMINAR SI UN BLO-
REMARK* MARZO/2003 * QUE ES O NO VALIDO. PARA QUE UN NOMBRE QUE VENGA NUMERICO
REMARK*            * NO SE CONVIERTA EN COMODIN, EN LAS SUSTITUCIONES POR *
REMARK*            * SE VALIDA QUE EL CONTENIDO NO SEA NUMERICO.
REMARK*************************************************************************
REMARK* GJC0005    * VLNM01: LOS PROGRAMAS ABORTAN CUANDO ALGUNO DE LOS
REMARK* G.JIMENEZ  *         PARAMETROS DE UN SUBSTRING QUEDA EN 0 O EN
REMARK* NOV/2005   *         UN NUMERO MENOR QUE 0. EJ: NOMBRE(I:J) DONDE I O
REMARK*            *         J ES 0.
REMARK*************************************************************************
       10000-MATCH-NOMBRES-VALNOM.
REMARK******************************************************************
REMARK*10000-MATCH-NOMBRE-VALNOM.
REMARK* SE BUSCA DETERMINAR SI EL NOMBRE DE LA PERSONA CONSULTADA HACE
REMARK*       CON EL NOMBRE ENVIADO EN LA CONSULTA.
REMARK******************************************************************
           MOVE 0 TO MATCH-OK-OUT-VALNOM.
           MOVE NOMBRE-INPUT-VALNOM TO NOMBRE-AUX-VALNOM.
           IF TIP-ID-INPUT-VALNOM  = 2
               INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VAL001           " S.A. " BY "      ",                                  RMB0002
VAL001           "." BY " ", " LTDA " BY "      ",                      RMB0002
VAL001           " LIMITADA " BY "          ",                          RMB0002
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",      RMB0002
VAL001           " INVERSIONES " BY "             ",                    RMB0002
VAL001           " EMPRESA " BY "         ",                            RMB0002
VAL001           " CORPORACION " BY "             ",                    RMB0002
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",RMB0002
VAL001           " S " BY "   ", " A " BY "   "                         RMB0002
VALMEJ           " Y " BY "   "                                         GJC0003
VAL001         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL                  RMB0002
VAL001           " S.A. " BY "      ",                                  RMB0002
VAL001           "." BY " ", " LTDA " BY "      ",                      RMB0002
VAL001           " LIMITADA " BY "          ",                          RMB0002
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",      RMB0002
VAL001           " INVERSIONES " BY "             ",                    RMB0002
VAL001           " EMPRESA " BY "         ",                            RMB0002
VAL001           " CORPORACION " BY "             ",                    RMB0002
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",RMB0002
VAL001           " S " BY "   ", " A " BY "   "                         RMB0002
VALMEJ           " Y " BY "   "                                         GJC0003
VALMEJ         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL                  GJC0003
VALMEJ                "." BY " ","-" BY " ",                            GJC0003
VALMEJ                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",      GJC0003
VALMEJ                "Y" BY "G","H" BY "G","I" BY "G"                  GJC0003
VALMEJ         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL                  GJC0003
VALMEJ                "." BY " ","-" BY " ",                            GJC0003
VALMEJ                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",      GJC0003
VALMEJ                "Y" BY "G","H" BY "G","I" BY "G"                  GJC0003
               MOVE NOMBRE-AUX-VALNOM TO NOMBRE-INPUT-VALNOM
           ELSE
              INSPECT CADENA-VALNOM    REPLACING ALL
VALME1                 "." BY " ","-" BY " ", "," BY " ",
                      "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
                      "Y" BY "G","H" BY "G","I" BY "G",
                      " DE " BY "    "," LA " BY "    ",
                      " DEL " BY "     "
VAL001        INSPECT CADENA-VALNOM    REPLACING ALL                    RMB0002
VALME1                 "." BY " ","-" BY " ", "," BY " ",
VAL001                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",      RMB0002
VAL001                "Y" BY "G","H" BY "G","I" BY "G",                 RMB0002
VAL001                " DE " BY "    "," LA " BY "    ",                RMB0002
VAL001                " DEL " BY "     "                                RMB0002
                      PERFORM VARYING I-VALNOM FROM 1 BY 1
                              UNTIL I-VALNOM >= 66
                        IF VNOM-AUX-VALNOM(I-VALNOM) NOT ALPHABETIC
VALME1                      AND VNOM-AUX-VALNOM(I-VALNOM) NOT NUMERIC
                           MOVE "*" TO VNOM-AUX-VALNOM(I-VALNOM)
                        END-IF
                      END-PERFORM
           END-IF.
           MOVE SPACES TO VECTOR-INPUT-VALNOM.
           UNSTRING CADENA-VALNOM      DELIMITED BY ALL " " INTO
                        BASURA-VALNOM
            VM-INPUT-VALNOM(1) VM-INPUT-VALNOM(2) VM-INPUT-VALNOM(3)
            VM-INPUT-VALNOM(4) VM-INPUT-VALNOM(5)
            VM-INPUT-VALNOM(6) VM-INPUT-VALNOM(7) VM-INPUT-VALNOM(8)
            VM-INPUT-VALNOM(9) VM-INPUT-VALNOM(10)
            VM-INPUT-VALNOM(11) VM-INPUT-VALNOM(12) VM-INPUT-VALNOM(13)
            VM-INPUT-VALNOM(14)
            VM-INPUT-VALNOM(15)
            VM-INPUT-VALNOM(16) VM-INPUT-VALNOM(17) VM-INPUT-VALNOM(18)
            VM-INPUT-VALNOM(19)
            VM-INPUT-VALNOM(20)
      ******************PREPARA EL NOMBRE CORRECTO DEL ARCHIVO *********
           MOVE NOMBOK-INPUT-VALNOM TO NOMBRE-AUX-VALNOM.
           IF TIP-ID-INPUT-VALNOM  NOT = 2
              INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VALME1                 "." BY " ","-" BY " ","," BY " ",
                      "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
                      "Y" BY "G","H" BY "G","I" BY "G"
                      PERFORM VARYING I-VALNOM FROM 1 BY 1
                              UNTIL I-VALNOM >= 66
                        IF VNOM-AUX-VALNOM(I-VALNOM) NOT ALPHABETIC
VALME1                      AND VNOM-AUX-VALNOM(I-VALNOM) NOT NUMERIC
                           MOVE "*" TO VNOM-AUX-VALNOM(I-VALNOM)
                        END-IF
                      END-PERFORM
           ELSE
VAL001         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL                  RMB0002
VAL001           " S.A. " BY "      ",                                  RMB0002
VAL001           "." BY " ", " LTDA " BY "      ",                      RMB0002
VAL001           " LIMITADA " BY "          ",                          RMB0002
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",      RMB0002
VAL001           " INVERSIONES " BY "             ",                    RMB0002
VAL001           " EMPRESA " BY "         ",                            RMB0002
VAL001           " CORPORACION " BY "             ",                    RMB0002
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",RMB0002
VAL001           " S " BY "   ", " A " BY "   "                         RMB0002
VALMEJ           " Y " BY "   "                                         GJC0003
VAL001         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL                  RMB0002
VAL001           " S.A. " BY "      ",                                  RMB0002
VAL001           "." BY " ", " LTDA " BY "      ",                      RMB0002
VAL001           " LIMITADA " BY "          ",                          RMB0002
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",      RMB0002
VAL001           " INVERSIONES " BY "             ",                    RMB0002
VAL001           " EMPRESA " BY "         ",                            RMB0002
VAL001           " CORPORACION " BY "             ",                    RMB0002
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",RMB0002
VAL001           " S " BY "   ", " A " BY "   "                         RMB0002
VALMEJ           " Y " BY "   "                                         GJC0003
VALMEJ        INSPECT NOMBRE-AUX-VALNOM REPLACING ALL                   GJC0003
VALMEJ                "." BY " ","-" BY " ",                            GJC0003
VALMEJ                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",      GJC0003
VALMEJ                "Y" BY "G","H" BY "G","I" BY "G"                  GJC0003
               MOVE NOMBRE-AUX-VALNOM TO NOMBOK-INPUT-VALNOM
           END-IF.
           MOVE SPACES TO VECTOR-OK-VALNOM.
           UNSTRING CADENA-VALNOM      DELIMITED BY ALL " " INTO
                        BASURA-VALNOM
            VM-OK-VALNOM(1) VM-OK-VALNOM(2) VM-OK-VALNOM(3)
            VM-OK-VALNOM(4) VM-OK-VALNOM(5)
            VM-OK-VALNOM(6) VM-OK-VALNOM(7) VM-OK-VALNOM(8)
            VM-OK-VALNOM(9) VM-OK-VALNOM(10)
            VM-OK-VALNOM(11) VM-OK-VALNOM(12) VM-OK-VALNOM(13)
            VM-OK-VALNOM(14) VM-OK-VALNOM(15)
            VM-OK-VALNOM(16) VM-OK-VALNOM(17) VM-OK-VALNOM(18)
            VM-OK-VALNOM(19) VM-OK-VALNOM(20).
REMARK***********HACE LA VALIDACION BLOQUE A BLOQUE********************
           MOVE 1 TO I-VALNOM.
           MOVE 0 TO BLOQOK-AUX-VALNOM.
           PERFORM UNTIL VM-INPUT-VALNOM(I-VALNOM) = SPACES
             MOVE 1 TO J-VALNOM
             PERFORM UNTIL J-VALNOM > 20
               IF VM-INPUT-VALNOM(I-VALNOM) = VM-OK-VALNOM(J-VALNOM)
                  ADD 1 TO BLOQOK-AUX-VALNOM
                  MOVE SPACES TO VM-OK-VALNOM(J-VALNOM)
                                 VM-INPUT-VALNOM(I-VALNOM)
                  MOVE 21 TO J-VALNOM
               ELSE
                  MOVE 0 TO M-VALNOM
                  INSPECT VM-OK-VALNOM(J-VALNOM) TALLYING M-VALNOM
                          FOR CHARACTERS BEFORE INITIAL "*"
                  IF M-VALNOM >= 15
                     MOVE 0 TO M-VALNOM
                     INSPECT VM-OK-VALNOM(J-VALNOM) TALLYING M-VALNOM
                          FOR CHARACTERS BEFORE INITIAL "N"
                  END-IF
                  IF M-VALNOM > 0 AND < 15 AND I-VALNOM < 20
VLNM01               AND (14 - M-VALNOM > 0)                            GJC0005
                     COMPUTE INI-VALNOM = M-VALNOM + 2
                     COMPUTE LON-VALNOM = 14 - M-VALNOM
                     MOVE VM-OK-VALNOM(J-VALNOM) TO PEDAZO-VALNOM
                     IF PEDAZO-VALNOM(INI-VALNOM:LON-VALNOM) =
                        VM-INPUT-VALNOM(I-VALNOM + 1 ) AND
                        PEDAZO-VALNOM(1:M-VALNOM) =
                        VM-INPUT-VALNOM(I-VALNOM)
                        ADD 1 TO BLOQOK-AUX-VALNOM
                        MOVE SPACES TO VM-OK-VALNOM(J-VALNOM)
                                 VM-INPUT-VALNOM(I-VALNOM)
                        MOVE 21 TO J-VALNOM
                        ADD 1 TO I-VALNOM
                        MOVE SPACES TO VM-INPUT-VALNOM(I-VALNOM)
                     END-IF
                  END-IF
               END-IF
               ADD 1 TO J-VALNOM
             END-PERFORM
             ADD 1 TO I-VALNOM
             IF I-VALNOM = 21
                NEXT SENTENCE
             END-IF
           END-PERFORM.
           IF BLOQOK-AUX-VALNOM >= BLOQUES-INPUT-VALNOM
              MOVE 1 TO MATCH-OK-OUT-VALNOM
           ELSE
              IF TIP-ID-INPUT-VALNOM <> 2
                 PERFORM 10100-MATCH-APROXIMADO
              ELSE
                 PERFORM 10100-MATCH-APROX-EMPRESAS
              END-IF
              IF BLOQOK-AUX-VALNOM >= BLOQUES-INPUT-VALNOM
                 MOVE 1 TO MATCH-OK-OUT-VALNOM
              END-IF
           END-IF.
REMARK**********************************************************************
REMARK*10100-MATCH-APROXIMADO.
REMARK*     EN ESTA RUTINA SE VALIDAN LOS BLOQUES QUE NO PASARON COMPLETOS
REMARK*     CON UNA APROXIMACION. ESTA RUTINA FUNCIONA CORRECTAMENTE CON
REMARK*     LOS TIPOS DE IDENTIFICACION 1, 3 Y 4.
REMARK**********************************************************************
       10100-MATCH-APROXIMADO.
           IF BLOQOK-AUX-VALNOM > 0
              MOVE 65 TO APROX-TOT-VALNOM APROX-INP-VALNOM
           ELSE
              MOVE 70 TO APROX-TOT-VALNOM APROX-INP-VALNOM
           END-IF
           MOVE 1 TO I-VALNOM.
           PERFORM UNTIL I-VALNOM > 10
             MOVE 1 TO J-VALNOM
             PERFORM UNTIL J-VALNOM > 10
               MOVE 0 TO LETRAS-OK-VALNOM LETRAS-OK2-VALNOM
               MOVE 1 TO K-VALNOM
               MOVE 1 TO L-VALNOM
               IF VM-INPUT-VALNOM(I-VALNOM) NOT = SPACES AND
                  VM-OK-VALNOM   (J-VALNOM) NOT = SPACES
                  PERFORM UNTIL K-VALNOM > 15 OR L-VALNOM > 15
                    IF VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =
                       VMM-OK-VALNOM  (J-VALNOM,K-VALNOM)
VALMEJ                 OR ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM)) = "*"    GJC0003
VALMEJ                 AND (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) = " "   GJC0003
VALMEJ                      OR = "N"))                                  GJC0003
                       IF VMM-OK-VALNOM (J-VALNOM,K-VALNOM) NOT= SPACES
                          ADD 1 TO LETRAS-OK-VALNOM
                          ADD 1 TO K-VALNOM
                          ADD 1 TO L-VALNOM
                       ELSE
                          MOVE 19 TO K-VALNOM
                       END-IF
                    ELSE
                       ADD 1   TO K-VALNOM
                       IF K-VALNOM <= 15 AND L-VALNOM < 15
                          IF VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM + 1) =
                             VMM-OK-VALNOM(J-VALNOM,K-VALNOM )
                             ADD 1 TO L-VALNOM
                          END-IF
                       END-IF
                    END-IF
                  END-PERFORM
                  MOVE 1 TO K-VALNOM
                  MOVE 1 TO L-VALNOM
                  PERFORM UNTIL K-VALNOM > 15 OR L-VALNOM > 15
                    IF VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =
                       VMM-OK-VALNOM  (J-VALNOM,K-VALNOM)
VALMEJ                 OR ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM)) = "*"    GJC0003
VALMEJ                 AND (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) = " "   GJC0003
VALMEJ                      OR = "N"))                                  GJC0003
                       IF VMM-OK-VALNOM (J-VALNOM,K-VALNOM) NOT = SPACES
                          ADD 1 TO LETRAS-OK2-VALNOM
                          ADD 1 TO K-VALNOM
                          ADD 1 TO L-VALNOM
                       ELSE
                          MOVE 19 TO K-VALNOM
                       END-IF
                    ELSE
                       ADD 1   TO L-VALNOM
                       IF L-VALNOM <= 15 AND K-VALNOM < 15
                          IF VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =
                             VMM-OK-VALNOM(J-VALNOM,K-VALNOM + 1)
                             ADD 1 TO K-VALNOM
                          END-IF
                       END-IF
                    END-IF
                  END-PERFORM
               END-IF
               IF LETRAS-OK2-VALNOM > LETRAS-OK-VALNOM
                  MOVE LETRAS-OK2-VALNOM TO LETRAS-OK-VALNOM
               END-IF
               IF LETRAS-OK-VALNOM > 0
                  MOVE 0 TO TOTLET-INP-VALNOM TOTLET-OK-VALNOM
                  INSPECT VM-INPUT-VALNOM(I-VALNOM) TALLYING
                          TOTLET-INP-VALNOM
                          FOR CHARACTERS BEFORE INITIAL " "
                  INSPECT VM-OK-VALNOM   (J-VALNOM) TALLYING
                          TOTLET-OK-VALNOM
                          FOR CHARACTERS BEFORE INITIAL " "
                  MOVE 0 TO POR-TOT-VALNOM  POR-INP-VALNOM
                  IF TOTLET-OK-VALNOM >= 3
                     COMPUTE POR-TOT-VALNOM = LETRAS-OK-VALNOM * 100 /
                                             TOTLET-OK-VALNOM
                  END-IF
                  IF TOTLET-INP-VALNOM >= 3
                     COMPUTE POR-INP-VALNOM = LETRAS-OK-VALNOM * 100 /
                                             TOTLET-INP-VALNOM
                  END-IF
                  IF TOTLET-OK-VALNOM > TOTLET-INP-VALNOM
                     COMPUTE A-VALNOM = TOTLET-INP-VALNOM * 100 /
                                       TOTLET-OK-VALNOM
                  ELSE
                     COMPUTE A-VALNOM = TOTLET-OK-VALNOM  * 100 /
                                       TOTLET-INP-VALNOM
                  END-IF
VALME1            IF ((POR-TOT-VALNOM + POR-INP-VALNOM)/2 >=
VALME1                                  APROX-TOT-VALNOM) AND
VALME1                 A-VALNOM >= 63
VALME1****   display "%TOT=",POR-TOT-VALNOM, " %INP=", POR-INP-VALNOM,
VALME1****           "A-VALNOM=", A-VALNOM
VALME1*****       IF (POR-TOT-VALNOM >= APROX-TOT-VALNOM AND
VALME1*****           POR-INP-VALNOM >= APROX-INP-VALNOM ) OR
VALME1*****          ((POR-TOT-VALNOM >= APROX-TOT-VALNOM OR
VALME1*****            POR-INP-VALNOM >= APROX-INP-VALNOM ) AND
VALME1*****            A-VALNOM >= 70)
                     ADD 1 TO BLOQOK-AUX-VALNOM
                     MOVE SPACES TO VM-OK-VALNOM(J-VALNOM)
                                 VM-INPUT-VALNOM(I-VALNOM)
                  END-IF
               END-IF
               ADD 1 TO J-VALNOM
             END-PERFORM
             ADD 1 TO I-VALNOM
           END-PERFORM.
REMARK**********************************************************************
REMARK*10100-MATCH-APROX-EMPRESAS
REMARK*     EN ESTA RUTINA SE VALIDAN LOS BLOQUES QUE NO PASARON COMPLETOS
REMARK*     CON UNA APROXIMACION. ESTA RUTINA FUNCIONA CORRECTAMENTE CON
REMARK*     LOS TIPOS DE IDENTIFICACION 2.
REMARK**********************************************************************
       10100-MATCH-APROX-EMPRESAS.
         MOVE ZEROS TO TOTLET-INP-VALNOM
         MOVE ZEROS TO LETRAS-OK2-VALNOM
VALMEJ    MOVE ZEROS TO A-VALNOM                                        GJC0003
         MOVE 1 TO K-VALNOM
         PERFORM UNTIL K-VALNOM > 65
           MOVE K-VALNOM  TO I-VALNOM
           MOVE 1       TO J-VALNOM
           MOVE ZEROS TO LETRAS-OK-VALNOM
           PERFORM UNTIL J-VALNOM > 65 OR I-VALNOM > 65
              IF V-NOMBRE-VALNOM(I-VALNOM) <> SPACES AND
                 V-NOMBRE-VALNOM(I-VALNOM) <> "."
                 IF V-NOMBRE-VALNOM(I-VALNOM) =
                    V-NOMBOK-VALNOM(J-VALNOM)
                    ADD 1 TO LETRAS-OK-VALNOM
                    ADD 1 TO I-VALNOM
                    ADD 1 TO J-VALNOM
                 ELSE
                    ADD 1 TO J-VALNOM
                 END-IF
              ELSE
                 ADD 1 TO I-VALNOM
              END-IF
           END-PERFORM
           IF LETRAS-OK-VALNOM > LETRAS-OK2-VALNOM
              MOVE LETRAS-OK-VALNOM TO LETRAS-OK2-VALNOM
           END-IF
           ADD 1 TO K-VALNOM
         END-PERFORM.
         MOVE 1 TO I-VALNOM
         PERFORM UNTIL I-VALNOM > 65
           IF V-NOMBRE-VALNOM(I-VALNOM) <> SPACES AND
              V-NOMBRE-VALNOM(I-VALNOM) <> "."
              ADD 1 TO TOTLET-INP-VALNOM
           END-IF
           ADD 1 TO I-VALNOM
         END-PERFORM.
         COMPUTE A-VALNOM = LETRAS-OK2-VALNOM * 100 /
                           TOTLET-INP-VALNOM.
         IF A-VALNOM >= 75
             ADD 1 TO BLOQOK-AUX-VALNOM
         END-IF.
VALMEJ    MOVE 0 TO K-VALNOM                                            GJC0003
VALMEJ   IF BLOQOK-AUX-VALNOM < BLOQUES-INPUT-VALNOM                    GJC0003
VALMEJ      INITIALIZE A-VALNOM VECTORES-VALNOM L-VALNOM                GJC0003
VALMEJ      PERFORM VARYING J-VALNOM FROM 1 BY 1 UNTIL J-VALNOM > 65    GJC0003
VALMEJ               IF V-NOMBRE-VALNOM(J-VALNOM) <> " "                GJC0003
VALMEJ                 MOVE FUNCTION ORD(V-NOMBRE-VALNOM(J-VALNOM))     GJC0003
VALMEJ                   TO I-VALNOM                                    GJC0003
VALMEJ                 ADD 1 TO CONT-LET-INPUT-VALNOM(I-VALNOM)         GJC0003
VALMEJ              END-IF                                              GJC0003
VALMEJ               IF V-NOMBOK-VALNOM(J-VALNOM) <> " "                GJC0003
VALMEJ                 MOVE FUNCTION ORD(V-NOMBOK-VALNOM(J-VALNOM))     GJC0003
VALMEJ                    TO I-VALNOM                                   GJC0003
VALMEJ                 ADD 1 TO CONT-LET-OK-VALNOM(I-VALNOM)            GJC0003
VALMEJ              END-IF                                              GJC0003
VALMEJ      END-PERFORM                                                 GJC0003
VALMEJ      MOVE 0 TO I-VALNOM                                          GJC0003
VALMEJ       PERFORM VARYING J-VALNOM FROM 1 BY 1 UNTIL J-VALNOM > 256  GJC0003
VALMEJ         IF CONT-LET-OK-VALNOM(J-VALNOM)  > 0                     GJC0003
VALMEJ            ADD 1 TO I-VALNOM                                     GJC0003
VALMEJ            IF CONT-LET-OK-VALNOM(J-VALNOM) <                     GJC0003
VALMEJ               CONT-LET-INPUT-VALNOM(J-VALNOM)                    GJC0003
VALMEJ               COMPUTE L-VALNOM = L-VALNOM + 100                  GJC0003
VALMEJ            ELSE                                                  GJC0003
VALMEJ               COMPUTE L-VALNOM = L-VALNOM +                      GJC0003
VALMEJ                                  (CONT-LET-INPUT-VALNOM(J-VALNOM)GJC0003
VALMEJ                           *  100 /CONT-LET-OK-VALNOM(J-VALNOM))  GJC0003
VALMEJ            END-IF                                                GJC0003
VALMEJ         END-IF                                                   GJC0003
VALMEJ      END-PERFORM                                                 GJC0003
VALMEJ      COMPUTE K-VALNOM = L-VALNOM/I-VALNOM                        GJC0003
VALMEJ       IF (A-VALNOM >= 40 AND K-VALNOM > 80) OR                   GJC0003
VALMEJ         ((A-VALNOM >= 35 AND A-VALNOM < 40) AND K-VALNOM > 85) ORGJC0003
VALMEJ         ((A-VALNOM >= 25 AND A-VALNOM < 35) AND K-VALNOM = 100)  GJC0003
VALMEJ         ADD 1 TO BLOQOK-AUX-VALNOM                               GJC0003
VALMEJ       END-IF                                                     GJC0003
VALMEJ   END-IF.                                                        GJC0003
