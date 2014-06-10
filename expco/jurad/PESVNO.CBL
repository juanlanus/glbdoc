/d/iccol/desarrollo $cat PESVNO.CBL

       cob64 lstpath"/d/iccol/desarrollo/temporales" -uv -g -P -o qqq -C sourceasm $FUENTES/PESVNO.CBL

      * PESVNOCSV.V.01.0011
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PESVNO.
       AUTHOR. DATACREDITO.
       DATE-WRITTEN. MAYO/2001.
       DATE-COMPILED.
      *****************************************************************
      * PROGRAMA PARA VALIDACION DE CEDULAS, NITS Y CEDULAS DE        *
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
      *    siempre se le asigna el mismo valos que a CODIGO-ERROR
           03 CODERR-INC                PIC 99.
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
          03 FILLER                     PIC X VALUE LOW-VALUE.
       01 W-COMANDO-SORT.
          03 FILLER                     PIC X(13) VALUE "sort -u ".
          03 ENTRADA-SORT.
             05 FILLER                  PIC X(12) VALUE "$TEMPORALES/".
             05 W-SORT-NOM-ENT          PIC X(30) VALUE SPACES.
          03 FILLER                     PIC X(13) VALUE " -o ".
          03 SALIDA-SORT.
             05 FILLER                  PIC X(12) VALUE "$TEMPORALES/".
             05 W-SORT-NOM-SAL          PIC X(30) VALUE SPACES.
          03 FILLER                     PIC X VALUE LOW-VALUE.
      *--------------------------------------------------------------*
       01  W-ENTRADA.
           03 W-ENT-DIRECTORIO          PIC X(12) VALUE "$TEMPORALES/".
           03 W-ENT-NOMBRE              PIC X(30) VALUE SPACES.
       01  W-VALIDAS.
           03 W-VAL-DIRECTORIO          PIC X(12) VALUE "$TEMPORALES/".
           03 W-VAL-NOMBRE              PIC X(30) VALUE SPACES.
       01  W-INVALIDAS.
           03 W-INVAL-DIRECTORIO        PIC X(12) VALUE "$TEMPORALES/".
           03 W-INVAL-NOMBRE            PIC X(30) VALUE SPACES.
PJV011 01  W-VALIDAS-REP.
PJV011     03 W-VAL-DIRECTORIO          PIC X(12) VALUE "$TEMPORALES/".
PJV011     03 W-VAL-NOMBRE-REP          PIC X(30) VALUE SPACES.

      * Datos para detectar IDs repetidos: se guarda el último grabado
PJV011 01  W-KEYVAL-GRA.
PJV011     03 TIP-KEYVAL                PIC 9 VALUE ZEROS.
PJV011     03 NUM-KEYVAL                PIC 9(11) VALUE ZEROS.
PJV011     03 SN-VALREP                 PIC X VALUE SPACES.

      * Parámetros de la línea de comando
       01  W-PARAMETROS                 PIC X(94) VALUE SPACES.
      *    tarifa para facturación (de la línea de comando)
       01  W-TARIFA                     PIC 9(04) VALUE ZEROS.
      *    cant de bloques del texto del nombre (de la línea de comando)
       01  W-BLOQUES                    PIC 9 VALUE ZERO.
       01  SW-BDIIALE                   PIC 9 VALUE ZERO.
       01  W-DERECHOS                   PIC 9 VALUE ZERO.
       01  OPEN-DIR                     PIC 9 VALUE ZERO.
       01  FIN-ENTRADA                  PIC 9 VALUE 0.
       01  FINS                         PIC 9 VALUE 0.
       01  LLAVE7                       PIC 9(8) VALUE ZERO.
       01  LLAVE7-R                     REDEFINES LLAVE7.
           03 LLAVE7-1D                 PIC 9.
           03 LLAVE7-FILLER             PIC 9(7).
       01  TOPE-INI-LLAVE7              PIC 9(8) VALUE 20000000.
       01  FECHA-PROC.
           03 ANO-HOY.
              05 SIG-PROC               PIC X(2).
              05 ANO-PROC               PIC X(2).
           03 MES-PROC                  PIC X(2).
           03 DIA-PROC                  PIC X(2).
       01  FECHA-PROC-R                 REDEFINES FECHA-PROC
                                        PIC X(8).
       01  FECHA-AUX.
           03 DIA-AUX                   PIC 99.
           03 MES-AUX                   PIC 99.
           03 ANO-AUX                   PIC 99.
       01  FECHA-AUX-R                  REDEFINES FECHA-AUX
                                        PIC 9(6).
       01  TIEMPO-SIS.
           03 HORA-SIS                  PIC 99.
           03 MIN-SIS                   PIC 99.
           03 FILLER                    PIC 99.
       01  TIEMPO-SIS-R  REDEFINES TIEMPO-SIS
                                        PIC 9(08).
       01  TIEMPO-W                     PIC 9(8).
       01  W-CONTADORES.
           03  W-CONT-TIPO-5            PIC 9(6) VALUE ZEROS.
           03  W-CONT-TIPO-6            PIC 9(6) VALUE ZEROS.
           03  W-CONT-TIPO-7            PIC 9(6) VALUE ZEROS.
CQ6271     03  W-CONT-TIPO-5-PJ         PIC 9(6) VALUE ZEROS.
PJV011     03  W-CONT-TIPO-5-REP        PIC 9(6) VALUE ZEROS.
      *
       01  SUSCRIPTOR-AUX               PIC 9(6).
       01  REG-LEIDOS                   PIC 9(7) VALUE ZEROS.
R7094B 01  CONT-FALLECIDOS              PIC 9(7) VALUE ZEROS.
       01  REG-CORRECTOS                PIC 9(7) VALUE ZEROS.
PJV011 01  REG-CORRECTOS-R              PIC 9(7) VALUE ZEROS.
       01  NO-COINCIDE-DIAN             PIC 9(7) VALUE ZEROS.
       01  NO-COINCIDE-REGIS            PIC 9(7) VALUE ZEROS.
      * contador de registros con error 03 -
       01  NO-EXISTE-DIAN               PIC 9(7) VALUE ZEROS.
       01  NO-EXISTE-REGIS              PIC 9(7) VALUE ZEROS.
      * (no se usa)
       01  SI-EXISTE-CSV                PIC 9(7) VALUE ZEROS.
      * contador de registros con error 05 - tipo id inválido
       01  TIPO-ID-INVALIDO             PIC 9(7) VALUE ZEROS.
       01  NIT-BUENO                    PIC 9.
       01  NUM-ID-W                     PIC 9(11).
      *-----------------------------------------------------------------
       COPY  ICFILE.WS.
       COPY  BDIIALE.WS.
       COPY  DEPTOS.WS.
      ******************************************************************
      *COPY  BDIIVIDE.WS.
      *BDIIVIDE.WS.v03.0003.
      * Variables para rutina 80100-VALIDAR-IDENTIFICACION de validación
      * de identificaciones
      *----------------------------------------------------------------*
      * F.CUADROS   * INCLUSION DE PARAMETRO PARA ASIGNAR TIPO DE
      * JUN.17/2004 * IDENTIFICACION DE ACUERDO A RANGOS PERMITIDOS.
      * FAC0002     * NUIP01 * ESTO SI NO VIENE CORRECTO EL TIPO-ID.
      *----------------------------------------------------------------*
      * F.CUADROS   * INCLUSION DE VALIDACION PARA TIPO-ID. = 2 y
      * JUL.19/2004 * QUITAR EL DIGITO DE CHEQUEO SI ES EL CASO.
      * FAC0003     *
      *----------------------------------------------------------------*
       01  NUM-ID-W-BDIIVIDE.
NUIP02     03 NUM-ID-W11-BDIIVIDE       PIC 9(11) VALUE ZEROS.
NUIP02     03 NUM-ID-W11-R-BDIIVIDE     REDEFINES NUM-ID-W11-BDIIVIDE.
NUIP02         05 NUM-ID-W10-BDIIVIDE   PIC 9(10).
NUIP02         05 RESTO-NUM-ID-BDIIVIDE PIC 9.
       01  NOMBRES-AUX-W-BDIIVIDE.
           03 PRIMER-APELLIDO-BDIIVIDE  PIC X(15) VALUE SPACES.
           03 SEGUNDO-APELLIDO-BDIIVIDE PIC X(15) VALUE SPACES.
           03 PRIMER-NOMBRE-BDIIVIDE    PIC X(15) VALUE SPACES.
           03 SEGUNDO-NOMBRE-BDIIVIDE   PIC X(15) VALUE SPACES.
           03 NOM-LEIDO-BDIIVIDE        PIC X(65) VALUE SPACES.

NUIP01 01 PARAMETROS-ENTRADA-BDIIVIDE   PIC X(100) VALUE SPACES.
       01 PARAMETROS-ENTRADA-R-BDIIVIDE REDEFINES
                                        PARAMETROS-ENTRADA-BDIIVIDE.
           03 TIP-IDE-BDIIVIDE          PIC X.
           03 NUM-IDE-BDIIVIDE          PIC X(11) JUSTIFIED RIGHT.
NUIP01     03 NUM-IDE-BDIIVIDE-R        REDEFINES NUM-IDE-BDIIVIDE
NUIP01                                  PIC 9(11).
           03 NOMBRE-BDIIVIDE           PIC X(65).
           03 SW-VERIFNOM-BDIIVIDE      PIC 9.
           03 NUM-BLOQUES-BDIIVIDE      PIC 9.
NUIP01     03 SW-VALTIPID-BDIIVIDE      PIC 9.
NUIP01     03 SW-ASGTIPID-BDIIVIDE      PIC 9.
NUIP02     03 SW-DIGCHEQUEO-BDIIVIDE    PIC 9.
NUIP02     03 FILLER-BDIIVIDE           PIC X(18).

       01 PARAMETROS-SALIDA-BDIIVIDE.
      *    nombre validado
           03 NOM-VAL-BDIIVIDE          PIC X(65) VALUE SPACES.
      *    1 = error en número de id
           03 SW-ID-BDIIVIDE            PIC 9 VALUE 0.
      *    1 = error en el nombre
           03 SW-NOMBRES-BDIIVIDE       PIC 9 VALUE 0.
      *    1 = error en tipo de id
           03 SW-TID-BDIIVIDE           PIC 9 VALUE 0.
      ******************************************************************


       COPY  BDLDIR.WS.
       COPY  LEEMAESTROS.WS.
      *-----------------------------------------------------------------
      * Los significados de los cod de error son dudosos:
      * CODIGO-ERROR: 00=OK  03=NIT no figura
      * 04=cédula no figura en base alterna
      * 01=nombre no coincide  05=tipo id no 1|2|3|4
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
           03 W-ANO-ACTUAL              PIC 9(04) VALUE ZEROS.
           03 W-EDAD-CEDULA             PIC 9(03) VALUE ZEROS.
           03 W-FEC-EXPE                PIC 9(08) VALUE ZEROS.
           03 W-FEC-EXPE-R              REDEFINES W-FEC-EXPE.
              05 W-ANO-EXPE             PIC 9(04).
              05 W-MES-EXPE             PIC 9(02).
              05 W-DIA-EXPE             PIC 9(02).
           03 WS-EDAD                   PIC 9(03).
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
           03 FILLER                    PIC X(55) VALUE SPACES.
           03 FILLER                    PIC X(21) VALUE
             "D A T A C R E D I T O".
           03 FILLER                    PIC X(33) VALUE SPACES.
           03 DIA-T1                    PIC X(02).
           03 FILLER                    PIC X     VALUE "/".
           03 MES-T1                    PIC X(02).
           03 FILLER                    PIC X     VALUE "/".
           03 SIG-T1                    PIC X(02).
           03 ANO-T1                    PIC X(02).
           03 FILLER                    PIC X(13) VALUE SPACES.
       01  TITULO-2.
           03 FILLER                    PIC X(10) VALUE " PESVNOCSV".
           03 FILLER                    PIC X(41) VALUE SPACES.
           03 FILLER                    PIC X(37) VALUE
             "ESTADISTICAS DE VALIDACION DE CEDULAS".
           03 FILLER                    PIC X(24) VALUE SPACES.
           03 HORA-T2                   PIC 9(02).
           03 FILLER                    PIC X     VALUE ":".
           03 MIN-T2                    PIC 9(02).
           03 FILLER                    PIC X(06) VALUE SPACES.
           03 FILLER                    PIC X(06) VALUE "SUSC: ".
           03 SUSCRIPTOR-T2             PIC 9(06).
       01  RAYAS                        PIC X(132) VALUE ALL "-".
       01  LINEA-1.
           03 FILLER                    PIC X(45) VALUE SPACES.
           03 FILLER                    PIC X(32) VALUE
             "REGISTROS LEIDOS:               ".
           03 REG-LEIDOS-L1             PIC ZZ,ZZZ,ZZ9.
           03 FILLER                    PIC X(45) VALUE SPACES.
       01  LINEA-2.
           03 FILLER                    PIC X(45) VALUE SPACES.
           03 FILLER                    PIC X(33) VALUE
             "REGISTROS CORRECTOS:             ".
           03 REG-CORRECTOS-L2          PIC Z,ZZZ,ZZ9.
           03 FILLER                    PIC X(45) VALUE SPACES.
       01  LINEA-3.
           03 FILLER                    PIC X(45) VALUE SPACES.
           03 FILLER                    PIC X(33) VALUE
             "NO COINCIDEN CON DIAN O DAS:     ".
           03 NO-COINCIDE-DIAN-L3       PIC Z,ZZZ,ZZ9.
           03 FILLER                    PIC X(45) VALUE SPACES.
       01  LINEA-4.
           03 FILLER                    PIC X(45) VALUE SPACES.
           03 FILLER                    PIC X(33) VALUE
             "NO COINCIDEN CON REGISTARDURIA:  ".
           03 NO-COINCIDE-REGIS-L4      PIC Z,ZZZ,ZZ9.
           03 FILLER                    PIC X(45) VALUE SPACES.
       01  LINEA-5.
           03 FILLER                    PIC X(45) VALUE SPACES.
           03 FILLER                    PIC X(33) VALUE
             "NO EXISTEN EN DIAN O DAS:        ".
           03 NO-EXISTE-DIAN-L5         PIC Z,ZZZ,ZZ9.
           03 FILLER                    PIC X(45) VALUE SPACES.
       01  LINEA-6.
           03 FILLER                    PIC X(45) VALUE SPACES.
           03 FILLER                    PIC X(33) VALUE
             "NO EXISTEN EN REGISTRADURIA:     ".
           03 NO-EXISTE-REGIS-L6        PIC Z,ZZZ,ZZ9.
           03 FILLER                    PIC X(45) VALUE SPACES.
       01  LINEA-7.
           03 FILLER                    PIC X(45) VALUE SPACES.
           03 FILLER                    PIC X(33) VALUE
             "TIPO IDENTIFICACION INVALIDO:    ".
           03 TIPO-ID-INVALIDO-L7       PIC Z,ZZZ,ZZ9.
           03 FILLER                    PIC X(45) VALUE SPACES.
       01  FECHA-2000                   PIC 9(8).
       01  FECHA-2000-R  REDEFINES FECHA-2000.
           03 ANO-2000-R                PIC 9(4).
           03 MES-2000-R                PIC 9(2).
           03 DIA-2000-R                PIC 9(2).
      *-----------------------------------------------------------------
       01  HACER-MATCH                  PIC  9.
       01  CADENA-ENTRA                 PIC  X(60).
       01  CADENA-SALE                  PIC  X(60).
       01  IND-ALFABETICO               PIC  X(01).
           88 SI-ALFABETICO             VALUE " " "A" "B" "C" "D" "E"
                                        "F" "G" "H" "I" "J" "K" "L" "M"
                                        "N" "/" "O" "P" "Q" "R" "S" "T"
                                        "U" "V" "W" "X" "Y" "Z" "".

           Este COPY no estaba: VALNOM.WS
******* VALNOM-WS.v.0002
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES Q
******* UTILIZAN LAS RUTINAS DE NOMBRES DE LA RUTINA en VALNOM.PROC
************************************************************************
REMARK* GJC0002    * VALMEJ :SE CREAN ANUEVOS CAMPOS PARA MEJORAR LA VAL
REMARK* G.JIMENEZ  * DE NIT. PARA MEJORA TAMBIEN SE AMPLIA TAMANO DE VAR
REMARK* MARZO/2003 *
REMARK******************************************************************
       01 VARIABLES-VALNOM.
           03 VECTOR-INPUT-VALNOM.
               04 VM-INPUT-VALNOM       OCCURS 20 TIMES.
                   05 VMM-INPUT-VALNOM  OCCURS 15 TIMES PIC X.
           03 VECTOR-OK-VALNOM.
               04 VM-OK-VALNOM          OCCURS 20 TIMES.
                   05 VMM-OK-VALNOM     OCCURS 15 TIMES PIC X.
           03 BLOQOK-AUX-VALNOM         PIC 9(2).
           03 CADENA-VALNOM.
               04 FILLER                PIC X(2) VALUE "A ".
               04 NOMBRE-AUX-VALNOM     PIC X(65).
               04 FILLER REDEFINES NOMBRE-AUX-VALNOM.
                   05 VNOM-AUX-VALNOM OCCURS 65 TIMES PIC X.
           03 BASURA-VALNOM             PIC X(10).
           03 BLOQUES-INPUT-VALNOM      PIC 9(2).
           03 NOMBRE-INPUT-VALNOM       PIC X(65).
           03 FILLER REDEFINES NOMBRE-INPUT-VALNOM.
               05 V-NOMBRE-VALNOM       PIC X OCCURS 65 TIMES.
           03 NOMBOK-INPUT-VALNOM       PIC X(65).
           03 FILLER REDEFINES NOMBOK-INPUT-VALNOM.
               05 V-NOMBOK-VALNOM       PIC X OCCURS 65 TIMES.
           03 TIP-ID-INPUT-VALNOM       PIC 99.
           03 MATCH-OK-OUT-VALNOM       PIC 9.
           03 A-VALNOM                  PIC 9(5).
VALMEJ     03 I-VALNOM                  PIC 9(5).
VALMEJ     03 J-VALNOM                  PIC 9(5).
VALMEJ     03 K-VALNOM                  PIC 9(5).
VALMEJ     03 L-VALNOM                  PIC 9(5).
VALMEJ     03 M-VALNOM                  PIC 9(5).
           03 INI-VALNOM                PIC 99.
           03 LON-VALNOM                PIC 99.
           03 LETRAS-OK-VALNOM          PIC 99.
           03 LETRAS-OK2-VALNOM         PIC 99.
           03 TOTLET-OK-VALNOM          PIC 99.
           03 TOTLET-INP-VALNOM         PIC 99.
           03 PEDAZO-VALNOM             PIC X(15).
           03 POR-TOT-VALNOM            PIC 9(5).
           03 POR-INP-VALNOM            PIC 9(5).
           03 APROX-TOT-VALNOM          PIC 9(5).
           03 APROX-INP-VALNOM          PIC 9(5).
VALMEJ     03 VECTORES-VALNOM.
VALMEJ         04 FILLER                OCCURS 256 TIMES.
VALMEJ             05 CONT-LET-INPUT-VALNOM PIC 9(2).
VALMEJ             05 CONT-LET-OK-VALNOM PIC 9(2).
VALMEJ     03 VAL-APELLIDO-VALNOM       PIC 9.




BDAMATCH.WS
REMARK*BDAMATCH-WS.v.0003.
REMARK******************************************************************
REMARK* COPY QUE CONTIENE LA VARIABLES NECESARIAS PARA ELECCION DE MEJOR
REMARK* PORTAFOLI Y MATCH DE NOMBRES A PARTIR DE LA INFORMACION DE LOS
REMARK* ARCHIVO BDACSV, ICBDIR ICMCRE E ICMCHK.
REMARK******************************************************************
REMARK* RMB0002 *VIGE01: SE CONTRARAN LAS CUENTAS CON ADEJETIVOS DE
REMARK*         *MUERTOS Y EL NUMERO TOTAL DE CUENTAS DEL MEJOR PORTAFOL
REMARK******************************************************************
REMARK* RMB0003     * PESO01 - OPTIMIZAR MANEJO DEL CALCULO DEL PESO
REMARK* R-MARTINEZ. *          DEL PORTAFOLIO .
REMARK* 5-SEP-2011  * FUEN01 - NCLUIR  FUENTES EXTERNAS
REMARK*             * APEL01 - INCLUIR PRIMER APELLIDO EN MATCH
REMARK*             * NOMB01 - MAQUILLAR EL MEJOR NOMBRE EN PRESENTACION
REMARK******************************************************************
NOMB01 01 VARIABLES-MAQ.
NOMB01     03 I-MAQ                    PIC 9(03)  VALUE 0.
NOMB01     03 J-MAQ                    PIC 9(03)  VALUE 0.
NOMB01     03 SW-BORRAR-MAQ            PIC 9(03)  VALUE 0.
NOMB01     03 NOMBRE-INPMAQ-ORI        PIC X(45)  VALUE SPACES.
NOMB01     03 NOMBRE-OUTMAQ-OK         PIC X(45)  VALUE SPACES.
NOMB01     03 NOMBRE-INPMAQ-X.
NOMB01        04 FILLER                PIC X(02)  VALUE "A ".
NOMB01        04 NOMBRE-INPMAQ         PIC X(45)  VALUE SPACES.
NOMB01     03 PARTICULAS-INPMAQ.
NOMB01        04 BASURA-INPMAQ            PIC X(45).
NOMB01        04 P1-INPMAQ                PIC X(45).
NOMB01        04 P2-INPMAQ                PIC X(45).
NOMB01        04 P3-INPMAQ                PIC X(45).
NOMB01        04 P4-INPMAQ                PIC X(45).
NOMB01        04 P5-INPMAQ                PIC X(45).
NOMB01        04 P6-INPMAQ                PIC X(45).
NOMB01        04 P7-INPMAQ                PIC X(45).
NOMB01        04 P8-INPMAQ                PIC X(45).
NOMB01        04 P9-INPMAQ                PIC X(45).
NOMB01        04 P10-INPMAQ               PIC X(45).
NOMB01 01  SIG-MAQ                     PIC X(1).
NOMB01     88 SIG-VOCAL-MAQ    VALUE "A" "E" "I" "O" "U".
NOMB01 01  ANT-MAQ                     PIC X(1).
NOMB01     88 ANT-VOCAL-MAQ    VALUE "A" "E" "I" "O" "U".
NOMB01 01  ACTUAL-MAQ                  PIC X(1).
NOMB01     88 TILDE-MAQ   VALUE "Á" "É" "Í" "Ó" "Ú" "á" "é" "í" "ó" "ú".
NOMB01     88 QUITAR-MAQ  VALUE  "'" "´"  "`" '"' .
NOMB01     88 BORRAR-MAQ  VALUE "-" "." "1" "2" "3" "4"
NOMB01                          "5" "6" "7" "8" "9" '0' ")" "," .
NOMB01     88 ELIMINAR-MAQ VALUE "(" .
NOMB01     88 VALIDO-MAQ   VALUE "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
NOMB01                           "K" "L" "M" "N" "O" "P" "Q" "R" "S"
NOMB01                           "T" "U" "V" "W" "X" "Y" "Z" " ".
       01 LLAVE-AUX11-BDAMATCH.
          02  TIPO-ID-AUX11-BDAMATCH    PIC 9(01).
          02  NUM-ID-AUX11-BDAMATCH     PIC 9(11).
          02  COD-SUS-AUX11-BDAMATCH    PIC 9(06).
          02  COD-CTA-AUX11-BDAMATCH    PIC 9(02).
          02  NUM-CTA-AUX11-BDAMATCH    PIC X(18).
       01 LLAVE-AUX13-BDAMATCH.
          02  TIPO-ID-AUX13-BDAMATCH    PIC 9(01).
          02  NUM-ID-AUX13-BDAMATCH     PIC 9(13).
          02  COD-SUS-AUX13-BDAMATCH    PIC 9(06).
          02  COD-CTA-AUX13-BDAMATCH    PIC 9(02).
          02  NUM-CTA-AUX13-BDAMATCH    PIC X(18).
       01 VARIABLES-BDAMATCH.
PESO01    02 I2-BDAMATCH                PIC 9(2).
PESO01    02 TIPCTA-BDAMATCH.
PESO01*******2=CK,1=CR,2=BS,3=CC,4=OT,9=INVALIDO
PESO01       03 FILLER PIC X(30) VALUE "211422244444224442444433442444".
PESO01       03 FILLER PIC X(30) VALUE "444444444444444444442999999999".
PESO01       03 FILLER PIC X(30) VALUE "999999999999999999999999999999".
PESO01       03 FILLER PIC X(10) VALUE "9999999999".
PESO01    02 FILLER REDEFINES TIPCTA-BDAMATCH.
PESO01       03 V-TIPCTA-BDAMATCH  OCCURS 100 TIMES PIC 9.
PESO01    02 APELLIDO-INPUT-BDAMATCH    PIC X(60) VALUE SPACES.
PESO01    02 ENT-FIN-PORT-BDAMATCH      PIC 9(3)  VALUE 0.
PESO01    02 ENT-UNICAS-PORT-BDAMATCH   PIC 9(3)  VALUE 0.
PESO01    02 ENT-FIN-MEJ-PORT-BDAMATCH  PIC 9(3)  VALUE 0.
VIGE01    02 FALLECIDO-BDAMATCH         PIC 9(01) VALUE 0.
          02 IDE-TIPO-BDAMATCH          PIC 9(01) VALUE 0.
          02 ABIERTO-BDAMATCH           PIC 9(01) VALUE 0.
          02 PROCESAR-BDAMATCH          PIC 9(01) VALUE 0.
          02 FIN-BDAMATCH               PIC 9(01) VALUE 0.
          02 NUM-IDE-INPUT-BDAMATCH     PIC 9(11).
          02 TIP-IDE-INPUT-BDAMATCH     PIC 9(01).
          02 I-GRU-BDAMATCH             PIC 9(5) VALUE 0.
          02 GRU-NOM-BDAMATCH           PIC 9(5) VALUE 0.
          02 MOVIDO-BDAMATCH            PIC 9(1) VALUE 0.
          02 BUSCADO-D-BDAMATCH.
             03 BUSCADO-BDAMATCH        PIC 9(3) VALUE 0.
             03 FILLER                  PIC X(1) VALUE " ".
          02 PORINSERTAR-D-BDAMATCH.
             03 PORINSERTAR-BDAMATCH    PIC 9(03) VALUE 0.
             03 FILLER                  PIC X(1) VALUE " ".
          02 EXISTE-BDAMATCH            PIC 9(05) VALUE 0.
          02 MEJOR-PORTAFOLIO-BDAMATCH  PIC 9(05) VALUE 0.
          02 GRU-ANT-BDAMATCH           PIC 9(05) VALUE 0.
          02 MAX-CUENTAS-BDAMATCH       PIC 9(05) VALUE 5.
          02 MAX-REL-BDAMATCH           PIC 9(05) VALUE 5.
          02 MAXREL-PER-BDAMATCH        PIC 9(05) VALUE 0.
          02 FIN-FUENTE-BDAMATCH        PIC 9(05) VALUE 0.
          02 FIL-BUS-BDAMATCH           PIC 9(05) VALUE 0.
          02 COL-BUS-BDAMATCH           PIC 9(05) VALUE 0.
          02 FIN-BUSCADO-BDAMATCH       PIC 9(05) VALUE 0.
          02 LIBRE-BUSCADO-BDAMATCH     PIC 9(05) VALUE 0.
          02 FIL-FUE-BDAMATCH           PIC 9(05) VALUE 0.
          02 COL-FUE-BDAMATCH           PIC 9(05) VALUE 0.
          02 I-BDAMATCH                 PIC 9(08) VALUE 0.
          02 J-BDAMATCH                 PIC 9(08) VALUE 0.
          02 I-DIR-BDAMATCH             PIC 9(05) VALUE 0.
          02 I-TEL-BDAMATCH             PIC 9(05) VALUE 0.
          02 I-NOM-BDAMATCH             PIC 9(05) VALUE 0.
          02 I-CTA-BDAMATCH             PIC 9(05) VALUE 0.
          02 NUM-DIR-ANT-BDAMATCH       PIC X(50).
          02 NUM-TEL-ANT-BDAMATCH       PIC 9(15).
          02 CTA-DIR-ANT-BDAMATCH       PIC 9(05).
          02 CTA-TEL-ANT-BDAMATCH       PIC 9(05).
          02 CTA-NOM-ANT-BDAMATCH       PIC 9(05).
          02 GRU-NOM-ANT-BDAMATCH       PIC 9(05).
          02 TABLA-DIR-BDAMATCH.
             03 T-DIR-BDAMATCH OCCURS 501 TIMES.
                04 CTA-DIR-BDAMATCH     PIC 9(03).
                04 NUM-DIR-BDAMATCH     PIC X(50).
                04 NUM-CTAS-DIR-BDAMATCH PIC 9(03).
                04 SUS-DIR-BDAMATCH     PIC 9(06).
                04 LNG-DIR-BDAMATCH     PIC 9(03).
          02 TABLA-TEL-BDAMATCH.
             03 T-TEL-BDAMATCH   OCCURS 501 TIMES.
                04 CTA-TEL-BDAMATCH     PIC 9(03).
                04 NUM-TEL-BDAMATCH     PIC 9(15).
                04 NUM-CTAS-TEL-BDAMATCH PIC 9(03).
                04 SUS-TEL-BDAMATCH     PIC 9(06).
          02 TABLA-NOM-BDAMATCH.
             03 T-NOM-BDAMATCH OCCURS 501 TIMES.
                04 CTA-NOM-BDAMATCH     PIC 9(03).
                04 NOM-NOM-BDAMATCH     PIC X(60).
                04 SUS-NOM-BDAMATCH     PIC 9(06).
                04 GRU-NOMS-BDAMATCH     PIC 9(03).
                04 LNG-NOM-BDAMATCH     PIC 9(02).
          02 TABLA-CUENTAS-BDAMATCH.
             03 T-CUENTAS-BDAMATCH OCCURS 501 TIMES.
                04 CTA-CUENTAS-BDAMATCH PIC X(38).
VIGE01          04 CTA-FALLECIDO-BDAMATCH   PIC 9(1).
FUEN01          04 FUEEXT-CUENTAS-BDAMATCH   PIC 9(1).
          02 TABLA-GRUNOM-BDAMATCH.
             03 T-GRUNOM-BDAMATCH OCCURS 501 TIMES.
                04 GRUPO-GRUNOM-BDAMATCH PIC 9(03).
                04 NOM-GRUNOM-BDAMATCH   PIC X(60).
                04 NUM-CTAS-GRUNOM-BDAMATCH PIC 9(03).
                04 LNG-GRUNOM-BDAMATCH   PIC 9(02).
APEL01          04 APE-GRUNOM-BDAMATCH   PIC 9(03).
          02  WS-PESOS-CUENTAS-BDAMATCH.
            03  ESTA-CTA-BDAMATCH        PIC  9(01)  VALUE  ZEROS.
            03  INV-BDAMATCH             PIC 9(04) VALUE ZEROS.
            03  SUSC-CTAS-BDAMATCH       PIC 9(06) VALUE ZEROS.
            03  SUSC-GRUPO-BDAMATCH.
              05  SUSC-GRUP-SUS-BDAMATCH PIC  9(06)  VALUE  ZEROS.
              05  SUSC-GRUP-GRU-BDAMATCH PIC  9(03)  VALUE  ZEROS.
            03  PESO-CTAS-TOT-BDAMATCH   PIC 9(12) VALUE ZEROS.
            03  PESOS-CTRL-BDAMATCH      PIC 9(12) VALUE ZEROS.
            03  NOM-LIMPI-BDAMATCH       PIC X(60) VALUE SPACES.
            03  CTA-A-BUSCAR-BDAMATCH    PIC 9(03) VALUE ZEROS.
            03  PESO-TEL-BDAMATCH        PIC 9(02) VALUE 1.
            03  PESO-DIR-BDAMATCH        PIC 9(02) VALUE 3.
            03  PESO-NOM-BDAMATCH        PIC 9(02) VALUE 9.
            03  POSIC-TEL-BDAMATCH       PIC 9(02) VALUE ZEROS.
            03  POSIC-DIR-BDAMATCH       PIC 9(02) VALUE ZEROS.
            03  POSIC-NOM-BDAMATCH       PIC 9(02) VALUE ZEROS.
            03  POSIC-GRU-BDAMATCH       PIC 9(02) VALUE ZEROS.
            03  NRO-VECES-1-BDAMATCH     PIC 9(03) VALUE ZEROS.
            03  NRO-VECES-2-BDAMATCH     PIC 9(03) VALUE ZEROS.
            03  NRO-VECES-3-BDAMATCH     PIC 9(03) VALUE ZEROS.
            03  POSIC-SUMA-BDAMATCH      PIC 9(03) VALUE ZEROS.
            03  POSIC-SUSC-BDAMATCH      PIC 9(03) VALUE ZEROS.
            03  POSIC-SUS-GRU-BDAMATCH   PIC 9(03) VALUE ZEROS.
       01 V-INTERRELAC-BDAMATCH.
          02 CARAC-REL-BDAMATCH  OCCURS 501 TIMES PIC X(03).
          02 FILA-REL-BDAMATCH OCCURS 501 TIMES.
             03 COLUMNA-REL-BDAMATCH OCCURS 501 TIMES.
                04 CAJA-BDAMATCH         PIC 9(03).
                04 DELIMITADOR-BDAMATCH  PIC X(01).
       01 VARIABLES-NOMBRES-BDAMATCH.
          03 LIM-BDAMATCH                PIC  9(03) VALUE 500.
          03 DIR-INPUT-BDAMATCH          PIC  X(60).
          03 DIR-OUTPUT-BDAMATCH         PIC  X(60).
          03 CTA-INPUT-BDAMATCH          PIC  X(38).
          03 TEL-INPUT-BDAMATCH          PIC  9(15).
          03 TEL-OUTPUT-BDAMATCH         PIC  9(15).
          03 NOM-INPUT-BDAMATCH          PIC  X(60).
          03 NOM-OUTPUT-BDAMATCH         PIC  X(60).
VIGE01    03 NUM-MUERTOS-OUTPUT-BDAMATCH PIC  9(03).
VIGE01    03 NUM-CTAS-OUTPUT-BDAMATCH    PIC  9(03).
          03 VALCTA-OUTPUT-BDAMATCH      PIC  9(01).
          03 CAR-ANT-BDAMATCH            PIC  X(01).
          03 CAR-ACT-BDAMATCH            PIC  9(01).
          03 T-CARDIR-BDAMATCH.
             04 V-CARDIR-BDAMATCH       OCCURS 60 TIMES PIC X(01).
          03 T-CARTOT-BDAMATCH.
             04 V-CARTOT-BDAMATCH       OCCURS 10 TIMES PIC 9(05).
       01  TABLA-SUSC-DIR-BDAMATCH.
         03  REG-SUSC-DIR-BDAMATCH   OCCURS 501 TIMES
             ASCENDING KEY IS COD-SUSC-DIR-BDAMATCH  INDEXED BY
                                                     IND-DIR-BDAMATCH.
           05  COD-SUSC-DIR-BDAMATCH     PIC 9(06).
           05  FLAG-YA-CTA-BDAMATCH      PIC 9(01).
           05  FLAG-YA-DIR-BDAMATCH      PIC 9(01).
           05  FLAG-YA-TEL-BDAMATCH      PIC 9(01).
           05  FLAG-YA-NOM-BDAMATCH      PIC 9(01).
       01  TABLA-SUSC-NOMBRE-BDAMATCH.
         03  REG-SUSC-NOM-BDAMATCH        OCCURS 501 TIMES
             ASCENDING  KEY  IS  KEY-SUSC-NOM-BDAMATCH  INDEXED
                                 BY IND-NOM-BDAMATCH.
           05  KEY-SUSC-NOM-BDAMATCH.
             07  COD-SUSC-NOM-BDAMATCH   PIC 9(06).
             07  GRUPO-SUS-NOM-BDAMATCH  PIC 9(03).




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

      *    Fecha de proceso: es la del sistema
           MOVE FUNCTION CURRENT-DATE TO FECHA-PROC-R
           MOVE DIA-PROC TO DIA-AUX
           MOVE MES-PROC TO MES-AUX
           MOVE ANO-PROC TO ANO-AUX
           MOVE ANO-HOY TO W-ANO-ACTUAL

      *    Proceso principal: control de cada uno de los regs del input
           PERFORM 400-VALIDAR UNTIL FIN-ENTRADA = 1.

      *    Totales finales
           IF W-CONT-TIPO-5 > 0 or W-CONT-TIPO-6 > 0
           or W-CONT-TIPO-7 > 0
               DISPLAY "Registros Tipo 5           ==> " W-CONT-TIPO-5
               DISPLAY "Registros Tipo 6           ==> " W-CONT-TIPO-6
               DISPLAY "Registros Tipo 7           ==> " W-CONT-TIPO-7
PJV011       DISPLAY "Registros Tipo 5 repetidos ==> " W-CONT-TIPO-5-REP
           END-IF
      *    y terminación
           PERFORM 900-CERRAR-ARCHIVOS
CQ6271     MOVE "V" TO TIPPRO-LOGPESVNO-WK
CQ6271     MOVE "PESVNO" TO PROGRAM-LOGPESVNO-WK
CQ6271     PERFORM 1000-UPDATE-LOGPESVNO
           STOP RUN.


       400-VALIDAR.
      ******************************************************************
      * Dependiendo del tipo de identificación leido en el archivo de
      * entrada llama a la correspondiente rutina de validación (CEDULA,
      * NIT O DAS)
      * Acumula en contadores dependiendo del resultado de la validación
      * Lee secuencialmente el archivo de entrada
      ******************************************************************
           ADD 1 TO REG-LEIDOS
           MOVE 0 TO SW-ERROR CODIGO-ERROR CODERR-INC
           INITIALIZE REG-INCONS REG-VALIDAS
           MOVE ";" TO FILLER1 FILLER2 FILLER3 FILLER4 FILLER5 FILLER6
           FILLER7 FILLER8 FILLER9
           MOVE FILLER-2-E TO FILLER-2-VAL FILLER-2-INC
      *    validación del tipo de id
      *    $$$$ ¿está seguro de que es numérico?
           IF TIPOID-E < 1 OR > 4
      *        cod err 05: tipo id no es 1|2|3|4
               MOVE 05 TO CODIGO-ERROR CODERR-INC
               MOVE 1 TO SW-ERROR
               PERFORM 512-CREE-REGISTRO
               WRITE REG-INCONS
               ADD 1 TO TIPO-ID-INVALIDO
           ELSE
      *        tipo id OK
               MOVE TIPOID-E TO TIP-IDE-BDIIVIDE
               perform 400-validar2
           end-if.

      *    Conteo de los registros por "tipo", dependiendo del código
      *    de error:
      *    tipo 5: sin error
      *    tipo 6: 01 02 06 07
      *    tipo 7: 03 04 05
           IF CODIGO-ERROR = 01 OR 02 OR 06 OR 07
               ADD 1 TO W-CONT-TIPO-6
           ELSE
               IF CODIGO-ERROR = 03 OR 04 OR 05
                   ADD 1 TO W-CONT-TIPO-7
               ELSE
      *            registros correctos, repetidos o no
PJV011             IF SN-VALREP = "S"
PJV011                 ADD 1 TO W-CONT-TIPO-5-REP
PJV011             ELSE
                       ADD 1 TO W-CONT-TIPO-5
PJV011             END-IF
      *            si es una empresa
CQ6271             IF TIPOID-E = 2 OR 3
CQ6271                 ADD 1 TO W-CONT-TIPO-5-PJ
CQ6271             END-IF
               END-IF
           END-IF
      *    Registro siguiente del input
           READ ENTRADA AT END
               MOVE 1 TO FIN-ENTRADA
           NOT AT END
               INSPECT NUMID-X-E REPLACING ALL " " BY "0"
               INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"
           END-READ.

       400-validar2.
      ******************************************************************
      * Más validación: el tipo de id es OK, ahora se valida el número
      * y el nombre
      ******************************************************************
           MOVE NUMID-E TO NUM-IDE-BDIIVIDE NUM-ID-W
           MOVE NOMBRE-E TO NOMBRE-BDIIVIDE
R7094A     IF W-BLOQUES > 0
R7094A         MOVE 1 TO SW-VERIFNOM-BDIIVIDE
R7094A     END-IF
R7094D     IF TIPOID-E = 2 OR 3
R7094D         MOVE 1 TO SW-DIGCHEQUEO-BDIIVIDE
R7094D     END-IF
           MOVE W-BLOQUES TO NUM-BLOQUES-BDIIVIDE

      *    Valida el numero de identificación contra las bases de
      *    datos de validación de información BDIIDIAN, BDIIDAS,
      *    BDIIREGI y verifica la coincidencia del nombre reportado
      *    Retorna SW-ID-BDIIVIDE=1 si hay error
           PERFORM 80100-VALIDAR-IDENTIFICACION
           MOVE TIP-IDE-BDIIVIDE TO TIPOID-E
           MOVE NUM-IDE-BDIIVIDE TO NUMID-E
           INITIALIZE REG-VALIDAS REG-INCONS
REQ596     MOVE FILLER-2-E TO FILLER-2-VAL FILLER-2-INC
           MOVE ";" TO FILLER1 FILLER2 FILLER3 FILLER4 FILLER5
           FILLER6 FILLER7 FILLER8 FILLER9
           IF SW-ID-BDIIVIDE = 1
      *        es un id inválido
               IF TIPOID-E <> 1
      *            no es una cédula
CSVCEX             MOVE 0 TO HACER-MATCH
MODCED             MOVE 0 TO IDE-TIPO-BDAMATCH
CSVCEX             PERFORM 513-VALIDA-EN-CSV
CSVCEX             IF IDE-TIPO-BDAMATCH  = 0
CSVCEX                 MOVE 03 TO CODIGO-ERROR CODERR-INC
CSVCEX                 MOVE 1 TO SW-ERROR
CSVCEX                 PERFORM 512-CREE-REGISTRO
      *                $$$$ no debería usar 510-ESCRIBIR-INCONS ?
CSVCEX                 WRITE REG-INCONS
CSVCEX                 ADD 1 TO NO-EXISTE-DIAN
CSVCEX             END-IF
               ELSE
      *            es una cédula
                   MOVE ZEROS TO HACER-MATCH
MODCED             MOVE 0 TO IDE-TIPO-BDAMATCH
      *            Valida si la identificación está en la base de datos
      *            alterna
                   PERFORM 513-VALIDA-EN-CSV
                   IF IDE-TIPO-BDAMATCH = 0
                       MOVE 04 TO CODIGO-ERROR CODERR-INC
                       MOVE TIPOID-E TO TIP-INC
                       ADD 1 TO NO-EXISTE-REGIS
                       MOVE 1 TO SW-ERROR
                       PERFORM 510-ESCRIBIR-INCONS
                   END-IF
               END-IF
           ELSE
>>>>>>*        el id fué encontrado, el nombre no coincide
               IF SW-NOMBRES-BDIIVIDE = 1
                   IF TIPOID-E <> 1
      *                no es una cédula
                       MOVE 01 TO CODIGO-ERROR CODERR-INC
                       MOVE 1 TO SW-ERROR
                       PERFORM 512-CREE-REGISTRO
                       WRITE REG-INCONS
                       ADD 1 TO NO-COINCIDE-DIAN
                       IF TIPOID-E = 4
                           MOVE NACIO-BDIIDAS TO MUN-EXP-INC
                       END-IF
                   ELSE
      *                es una cédula, nombre no coincide
                       PERFORM 810-BUSCAR-MUNICIPIO
                       MOVE TIPOID-E TO TIP-VAL TIP-INC
                       MOVE NUMID-E TO CED-VAL CED-INC
                       MOVE FECHA-EXP-BDIIREGI TO FECHA-2000
                       IF ANO-2000-R > 51
                           ADD 1900 TO ANO-2000-R
                       ELSE
                           ADD 2000 TO ANO-2000-R
                       END-IF
                       MOVE IND-SEXO-BDIIREGI TO GENERO-VAL
                       PERFORM 505-RANGO-EDADES
                       MOVE FECHA-2000 TO FEC-EXP-VAL FEC-EXP-INC
                       IF ESTADO-VIG-BDIIREGI = 12 OR = 27
                           MOVE "00" TO VIG-VAL VIG-INC
                       ELSE
                           MOVE ESTADO-VIG-BDIIREGI TO VIG-VAL VIG-INC
                       END-IF
                       MOVE 02 TO CODIGO-ERROR CODERR-INC
                       MOVE 1 TO SW-ERROR
                       PERFORM 510-ESCRIBIR-INCONS
                       ADD 1 TO NO-COINCIDE-REGIS
                   END-IF
>>>>>>         ELSE
R7094C             MOVE NOM-VAL-BDIIVIDE TO NOM-VAL
                   INITIALIZE LLAVE-BDIIALE
                   MOVE TIPOID-E TO TIP-IDE-BDIIALE
                   MOVE NUMID-E TO NUM-IDE-BDIIALE
                   MOVE 990003 TO FUENTE-BDIIALE
                   START BDIIALE KEY NOT < LLAVE-BDIIALE INVALID KEY
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
                           READ BDIIALE NEXT RECORD AT END
                               MOVE 1 TO SW-BDIIALE
                           END-READ
                       END-IF
                   END-PERFORM
>>>>>>             IF TIPOID-E = 1
                       IF ESTADO-VIG-BDIIREGI = 12
                       AND W-DERECHOS = 1
                           MOVE 07 TO CODIGO-ERROR CODERR-INC
                           MOVE 1 TO SW-ERROR
                           MOVE "00" TO VIG-INC
                           PERFORM 510-ESCRIBIR-INCONS
                       ELSE
                           IF ESTADO-VIG-BDIIREGI = 21
                               MOVE 06 TO CODIGO-ERROR CODERR-INC
                               MOVE 1 TO SW-ERROR
                               MOVE "21" TO VIG-INC
                               PERFORM 510-ESCRIBIR-INCONS
                               ADD 1 TO NO-COINCIDE-REGIS
R7094B                         ADD 1 TO CONT-FALLECIDOS
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
                               PERFORM 301-GRABA-LOS-VALIDOS
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
                       PERFORM 301-GRABA-LOS-VALIDOS
                   END-IF
               END-IF
           END-IF.


       301-GRABA-LOS-VALIDOS.
      ******************************************************************
      * Graba un "válido"
      ******************************************************************
           MOVE "N" TO SN-VALREP
           IF TIP-KEYVAL = TIPOID-E and NUM-KEYVAL = NUMID-E
               MOVE "S" TO SN-VALREP
               PERFORM 300-GRABA-VALREP
           ELSE
               WRITE REG-VALIDAS
               MOVE TIP-VAL TO TIP-KEYVAL
               MOVE CED-VAL TO NUM-KEYVAL
               ADD 1 TO REG-CORRECTOS
           END-IF.

       300-GRABA-VALREP.
      ******************************************************************
      * Graba un "válido - repetido"
      ******************************************************************
           INITIALIZE REG-VALIDAS-REP
           WRITE REG-VALIDAS-REP FROM REG-VALIDAS
           MOVE TIP-VAL TO TIP-KEYVAL
           MOVE CED-VAL TO NUM-KEYVAL
           ADD 1 TO REG-CORRECTOS-R.


       505-RANGO-EDADES.
      ******************************************************************
      *  RANGO MUJERES: 20'000.001 A 70'000.000
      *  RANGO HOMBRES:          1 A 20'000.000
      *        MAYOR  : > 70'000.000 < 1000'000.000 (NUIPS)
      ******************************************************************
           MOVE FEC-EXP-VAL TO W-FEC-EXPE
           MOVE ZEROS TO W-EDAD-CEDULA
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

       510-ESCRIBIR-INCONS.
      ****************************************************************
      * BUSCA ERROR
      * CREAR REGISTRO EN EL ARCHIVO DE INCONSISTENCIAS
      ****************************************************************
           IF CODIGO-ERROR = 02 AND SW-ERROR = 1
               MOVE TIPOID-E TO TIP-INC
           end-if
           IF SW-ERROR = 1
               PERFORM 512-CREE-REGISTRO
               WRITE REG-INCONS
           end-if.

       512-CREE-REGISTRO.
      **************************************************************
      * ARMA EL REGISTRO DE INCONSISTENCIAS DEL ARCHIVO DCINC
      **************************************************************
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

       513-VALIDA-EN-CSV.
      **************************************************************
      * Valida si la identificación está en la base de datos alterna
      **************************************************************
REQ550*    hace validación del tipo de identificación
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
      *                Se carga en memoria la tabla para estandarizacion
      *                de direcciones
                       PERFORM 06000-U-CARGAR-TABLA-DIRESTAN
                       MOVE 1 TO OPEN-DIR
                   END-IF
                   MOVE TIPOID-E TO TIP-IDE-INPUT-BDAMATCH
                   MOVE NUMID-E  TO NUM-IDE-INPUT-BDAMATCH
      *            Se recorre toda la estructura bdacsv para la
      *            identificación buscada.
      *            En caso de hallar registros se procede a realizar el
      *            match (BDAMATCH.PROC + .WS ~ 1000 + 200 líneas)
                   PERFORM 1000-U-RECORRER-CSV-BDAMATCH
      *            se usa en LEEMAESTROS.WS / LEEMAESTROS.PROC
                   MOVE 1 TO HACERMATCH-LEEMSTRS
      *            se usa en LEEMAESTROS.WS / LEEMAESTROS.PROC
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
      * Se eliminan caracteres no alfabéticos a la cadena de entrada
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

       800-GRABAR-SECUENCIAL.
      ******************************************************************
      * SE ARMA Y GRABA REGISTRO EN EL ARCHIVO ICBTCO, INDICANDO EN EL
      * NUMERO DE TERMINAL, LA TARIFA UTILIZADA Y EN EL NUMERO DE IDEN-
      * TIFICACION LA CANTIDAD DE VALIDACIONES REALIZADAS. SE GRABA UN
      * REGISTRO POR CADA TIPO DE RESULTADO DE LA VALIDACION
      ******************************************************************
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

       810-BUSCAR-MUNICIPIO.
      *****************************************************************
      * Obtiene el nombre del municipio y el departamento al que
      * pertenece con el fin de alimentar el archivo de válidas
      *****************************************************************
           MOVE DEP-EXP-BDIIREGI TO CODIGO-DEP
           MOVE MUNIC-EXP-BDIIREGI TO CODIGO-MUNI
           READ MUNICIPIOS INVALID KEY
               MOVE SPACES TO NOMBRE-MUNI
               MOVE 71 TO CODIGO-DEP
           END-READ
           MOVE NOMBRE-MUNI TO MUN-EXP-VAL MUN-EXP-INC
           MOVE T-NOM-DEP(CODIGO-DEP) TO DEP-EXP-VAL DEP-EXP-INC.

       900-CERRAR-ARCHIVOS.
      ******************************************************************
      * Cierra cada uno de los archivo utilizados en el progrma que aún
      * están abiertos
      *****************************************************************
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
      ******************************************************************
      * Valida el numero dentificación contra las bases de datos
      * de validación de información BDIIDIAN, BDIIDAS, BDIIREGI
      * y verifica la coincidencia del nombre reportado
      ******************************************************************
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
      *        valida si el número de id NUM-IDE-BDIIVIDE está dentro
      *        del rango que le corresponde según su tipo de ID, retorna
      *        SW-TID-BDIIVIDE = 1 si no es OK
NUIP01         PERFORM 80400-VALIDAR-TIPO-ID
NUIP01     END-IF
NUIP03     IF SW-DIGCHEQUEO-BDIIVIDE = 1
NUIP03         PERFORM 80500-ELIMINAR-DIGITO-CHEQUEO
NUIP03     END-IF
NUIP01     IF SW-VALTIPID-BDIIVIDE = 0
NUIP01     or (SW-VALTIPID-BDIIVIDE = 1 AND SW-TID-BDIIVIDE = 0)
      *        Validación del número de identificación
      *        con las bases de datos de la REGISTRADURIA, DAS, DIAN
      *        dependiendo del tipo de identificación, y obtención
      *        del nombre si el ID se encuentra, SW-ID-BDIIVIDE = 1
      *        si no se encuentra
               PERFORM 80200-VALIDAR-ID
               INITIALIZE NOM-VAL-BDIIVIDE
               MOVE NOM-LEIDO-BDIIVIDE TO NOM-VAL-BDIIVIDE
      *        Si se debe verificar el nombre
               IF SW-VERIFNOM-BDIIVIDE = 1
                   MOVE TIP-IDE-BDIIVIDE TO TIP-ID-INPUT-VALNOM
                   MOVE NOMBRE-BDIIVIDE TO NOMBRE-INPUT-VALNOM
                   MOVE NOM-VAL-BDIIVIDE TO NOMBOK-INPUT-VALNOM
                   IF SW-ID-BDIIVIDE = 0
      *                es un ID validado
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
      *                Determina si el nombre de la persona consultada
      *                hace con el nombre enviado en la consulta
                       PERFORM 10000-MATCH-NOMBRES-VALNOM
                       IF MATCH-OK-OUT-VALNOM = 0
                           MOVE 1 TO SW-NOMBRES-BDIIVIDE
                       END-IF
                   END-IF
               END-IF
NUIP01     ELSE
NUIP01         IF SW-TID-BDIIVIDE = 1
      *            el tipo de ID tiene error
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
      *        ubica NIT en archivo de la DIAN, BDIIDIAN y carga el
      *        nombre en NOM-LEIDO-BDIIVIDE
               PERFORM 85020-UBICA-NIT
           WHEN 3
      *        ubica NIT en archivo de la DIAN, BDIIDIAN y carga el
      *        nombre en NOM-LEIDO-BDIIVIDE
               PERFORM 85020-UBICA-NIT
           WHEN 4
      *        Ubica cedula de extranjeria en archivo del DAS y también
      *        obtiene el nombre
               PERFORM 85030-UBICA-EXT
           WHEN OTHER
      *        ID inválido
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
NUIP01        IF NUM-IDE-BDIIVIDE-R >= 1 AND <= 99999999
NUIP01        OR NUM-IDE-BDIIVIDE-R >= 1000000000 AND < 8000000000
NUIP01            MOVE 1 TO TIP-IDE-BDIIVIDE
NUIP01        ELSE
NUIP01            IF NUM-IDE-BDIIVIDE-R >= 800000000 AND <= 999999999
NUIP02            OR NUM-IDE-BDIIVIDE-R >= 8000000000 AND <= 9999999999
NUIP01                MOVE 2 TO TIP-IDE-BDIIVIDE
NUIP01            ELSE
NUIP01                IF NUM-IDE-BDIIVIDE-R >= 600000000 AND < 800000000
NUIP01              OR NUM-IDE-BDIIVIDE-R >= 6000000000 AND < 8000000000
NUIP01                    MOVE 3 TO TIP-IDE-BDIIVIDE
NUIP01                ELSE
                          MOVE 4 TO TIP-IDE-BDIIVIDE
                      end-if
                  end-if
NUIP01        END-IF.

NUIP01 80400-VALIDAR-TIPO-ID.
      *================================================================
NUIP01* Validacion del tipo de identificacion de acuerdo al rango
NUIP01* contenido en el numero de identificacion. Se pregunta al reves.
NUIP01* Parametro de entrada SW-VALTIPID-BDIVIIDE = 1;
NUIP01* Retorna SW-TID-BDIIVIDE = 1 si esta errado.
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
      *    Arma clave y lee REGISTRADURIA
           MOVE NUM-IDE-BDIIVIDE TO CEDULA-BDIIREGI
           MOVE TIP-IDE-BDIIVIDE TO TIPO-ID-BDIIREGI
           READ BDIIREGI INVALID KEY
               MOVE 1 TO SW-ID-BDIIVIDE
           END-READ
      *    Se encuentra en REGISTRADURIA: arma el nombre leído en
      *    NOM-LEIDO-BDIIVIDE
           IF SW-ID-BDIIVIDE = 0
               MOVE APELLIDO1-BDIIREGI TO PRIMER-APELLIDO-BDIIVIDE
               MOVE APELLIDO2-BDIIREGI TO SEGUNDO-APELLIDO-BDIIVIDE
               MOVE DES-NOMBRE2-BDIIREGI TO SEGUNDO-NOMBRE-BDIIVIDE
               MOVE DES-NOMBRE1-BDIIREGI TO PRIMER-NOMBRE-BDIIVIDE
               IF ESTADO-CIVIL-BDIIREGI = 0
                   STRING
                   SEGUNDO-APELLIDO-BDIIVIDE DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE ,
                   PRIMER-NOMBRE-BDIIVIDE DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE ,
                   SEGUNDO-NOMBRE-BDIIVIDE DELIMITED BY SIZE,
                   ";" DELIMITED BY SIZE ,
                   INTO NOM-LEIDO-BDIIVIDE
               END-IF
               IF ESTADO-CIVIL-BDIIREGI = 1
                   STRING
                   "DE" DELIMITED BY SIZE,
                   "*" DELIMITED BY SIZE,
                   SEGUNDO-APELLIDO-BDIIVIDE DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE,
                   PRIMER-NOMBRE-BDIIVIDE DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE,
                   SEGUNDO-NOMBRE-BDIIVIDE DELIMITED BY SIZE,
                   ";" DELIMITED BY SIZE,
                   INTO NOM-LEIDO-BDIIVIDE
               END-IF
               IF ESTADO-CIVIL-BDIIREGI = 2
                   STRING
                   "VDA DE" DELIMITED BY SIZE,
                   "*" DELIMITED BY SIZE,
                   SEGUNDO-APELLIDO-BDIIVIDE DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE,
                   PRIMER-NOMBRE-BDIIVIDE DELIMITED BY "  ",
                   "*" DELIMITED BY SIZE,
                   SEGUNDO-NOMBRE-BDIIVIDE DELIMITED BY SIZE,
                   ";" DELIMITED BY SIZE,
                   INTO NOM-LEIDO-BDIIVIDE
               END-IF
               STRING PRIMER-APELLIDO-BDIIVIDE DELIMITED BY "  ",
               "*" DELIMITED BY SIZE,
               NOM-LEIDO-BDIIVIDE DELIMITED BY SIZE, "*",
               DELIMITED BY SIZE,
               INTO NOM-VAL-BDIIVIDE

               INSPECT NOM-VAL-BDIIVIDE REPLACING ALL "*" BY " "
               INSPECT NOM-VAL-BDIIVIDE REPLACING ALL ";" BY " "
               INITIALIZE NOM-LEIDO-BDIIVIDE
               MOVE NOM-VAL-BDIIVIDE TO NOM-LEIDO-BDIIVIDE
           end-if.

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


      *VALNOM-PROC.v.0005
      ******************************************************************
      * RUTINA CREADA PARA HACER LA VALIDACION DE NOMBRES CONTRA LOS
      * ARCHIVOS DE LA REGISTRADURIA, DIAN Y DAS, DEPENDIENDO DEL TIPO
      * DE IDENTIFICACION RECIBIDA.
      *    PARAMETROS
      *        BLOQUES-INPUT-VALNOM - NUMERO DE BLOQUES A VALIDAR
      *        NOMBRE-INPUT-VALNOM  - NOMBRE A VALIDAR
      *        NOMBOK-INPUT-VALNOM  - NOMBRE CORRECTO CON EL QUE SE
      *                               DEBE COMPARAR NOMBRE-INPUT-VALNOM
      *        TIP-ID-INPUT-VALNOM  - TIPO DE IDENTIFICACION A VALIDAR
      *    SALIDAS:
      *        MATCH-OK-OUT-VALNOM  - 0-NO HUBO       1-HUBO
      ******************************************************************
      * RMB0002   * VAL001: SE MEJORA LA VALIDACION DE NOMBRES EN PERSON
      * R.MARTINEZ* JURIDICAS
      * DIC.2002  *
      ******************************************************************
      * GJC0003    * VALMEJ :SE CREAN ANUEVOS CAMPOS PARA MEJORAR LA VAL
      * G.JIMENEZ  * DE NIT. PARA MEJORA TAMBIEN SE AMPLIA TAMANO DE VAR
      * MARZO/2003 * PARA CEDULAS, EL COMODIN QUE USUALMENTE CORRESPONDE
      *            * SOLO SE COMPARA CONTRA N O CONTRA BLANCOS
      ******************************************************************
      * GJC0004    * VALME1 :CON EL FIN DE AFINAR AUN MAS EL PROCESO DE
      * R.MARTINEZ * CION DE NOMBRES, EN EL MATCH A PROXIMADO DE PERSONA
      * G.JIMENEZ  * RALES SE CAMBIA EL CALCULO FINAL PARA DETERMINAR SI
      * MARZO/2003 * QUE ES O NO VALIDO. PARA QUE UN NOMBRE QUE VENGA NU
      *            * NO SE CONVIERTA EN COMODIN, EN LAS SUSTITUCIONES PO
      *            * SE VALIDA QUE EL CONTENIDO NO SEA NUMERICO.
      ******************************************************************
      * GJC0005    * VLNM01: LOS PROGRAMAS ABORTAN CUANDO ALGUNO DE LOS
      * G.JIMENEZ  *         PARAMETROS DE UN SUBSTRING QUEDA EN 0 O EN
      * NOV/2005   *         UN NUMERO MENOR QUE 0. EJ: NOMBRE(I:J) DOND
      *            *         J ES 0.
      ******************************************************************

       10000-MATCH-NOMBRES-VALNOM.
      ******************************************************************
      * Se busca determinar si el nombre de la persona consultada hace
      * con el nombre enviado en la consulta.
      ******************************************************************
           MOVE 0 TO MATCH-OK-OUT-VALNOM
           MOVE NOMBRE-INPUT-VALNOM TO NOMBRE-AUX-VALNOM
      *    caso nombres de empresas
           IF TIP-ID-INPUT-VALNOM = 2
      *        normalización del nombre de empresa
               INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VAL001           " S.A. " BY "      ",
VAL001           "." BY " ", " LTDA " BY "      ",
VAL001           " LIMITADA " BY "          ",
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",
VAL001           " INVERSIONES " BY "             ",
VAL001           " EMPRESA " BY "         ",
VAL001           " CORPORACION " BY "             ",
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",
VAL001           " S " BY "   ", " A " BY "   "
VALMEJ           " Y " BY "   "
VAL001         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VAL001           " S.A. " BY "      ",
VAL001           "." BY " ", " LTDA " BY "      ",
VAL001           " LIMITADA " BY "          ",
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",
VAL001           " INVERSIONES " BY "             ",
VAL001           " EMPRESA " BY "         ",
VAL001           " CORPORACION " BY "             ",
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",
VAL001           " S " BY "   ", " A " BY "   "
VALMEJ           " Y " BY "   "
VALMEJ         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VALMEJ                "." BY " ","-" BY " ",
VALMEJ                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
VALMEJ                "Y" BY "G","H" BY "G","I" BY "G"
VALMEJ         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VALMEJ                "." BY " ","-" BY " ",
VALMEJ                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
VALMEJ                "Y" BY "G","H" BY "G","I" BY "G"
               MOVE NOMBRE-AUX-VALNOM TO NOMBRE-INPUT-VALNOM
           ELSE
      *        normalización del nombre de persona
              INSPECT CADENA-VALNOM REPLACING ALL
VALME1                 "." BY " ","-" BY " ", "," BY " ",
                      "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
                      "Y" BY "G","H" BY "G","I" BY "G",
                      " DE " BY "    "," LA " BY "    ",
                      " DEL " BY "     "
VAL001        INSPECT CADENA-VALNOM REPLACING ALL
VALME1                 "." BY " ","-" BY " ", "," BY " ",
VAL001                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
VAL001                "Y" BY "G","H" BY "G","I" BY "G",
VAL001                " DE " BY "    "," LA " BY "    ",
VAL001                " DEL " BY "     "
              PERFORM VARYING I-VALNOM FROM 1 BY 1 UNTIL I-VALNOM >= 66
                  IF VNOM-AUX-VALNOM(I-VALNOM) NOT ALPHABETIC
VALME1            AND VNOM-AUX-VALNOM(I-VALNOM) NOT NUMERIC
                      MOVE "*" TO VNOM-AUX-VALNOM(I-VALNOM)
                  END-IF
              END-PERFORM
           END-IF.
           MOVE SPACES TO VECTOR-INPUT-VALNOM.
           UNSTRING CADENA-VALNOM DELIMITED BY ALL " " INTO
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
           MOVE NOMBOK-INPUT-VALNOM TO NOMBRE-AUX-VALNOM
           IF TIP-ID-INPUT-VALNOM NOT = 2
              INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VALME1                 "." BY " ","-" BY " ","," BY " ",
                      "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
                      "Y" BY "G","H" BY "G","I" BY "G"
              PERFORM VARYING I-VALNOM FROM 1 BY 1 UNTIL I-VALNOM >= 66
                  IF VNOM-AUX-VALNOM(I-VALNOM) NOT ALPHABETIC
VALME1                AND VNOM-AUX-VALNOM(I-VALNOM) NOT NUMERIC
                      MOVE "*" TO VNOM-AUX-VALNOM(I-VALNOM)
                  END-IF
              END-PERFORM
           ELSE
VAL001         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VAL001           " S.A. " BY "      ",
VAL001           "." BY " ", " LTDA " BY "      ",
VAL001           " LIMITADA " BY "          ",
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",
VAL001           " INVERSIONES " BY "             ",
VAL001           " EMPRESA " BY "         ",
VAL001           " CORPORACION " BY "             ",
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",
VAL001           " S " BY "   ", " A " BY "   "
VALMEJ           " Y " BY "   "
VAL001         INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VAL001           " S.A. " BY "      ",
VAL001           "." BY " ", " LTDA " BY "      ",
VAL001           " LIMITADA " BY "          ",
VAL001           " CIA " BY "     ", " COMPA/IA " BY "          ",
VAL001           " INVERSIONES " BY "             ",
VAL001           " EMPRESA " BY "         ",
VAL001           " CORPORACION " BY "             ",
VAL001           " DE " BY "    ", " LA " BY "    ", " DEL " BY "     ",
VAL001           " S " BY "   ", " A " BY "   "
VALMEJ           " Y " BY "   "
VALMEJ        INSPECT NOMBRE-AUX-VALNOM REPLACING ALL
VALMEJ                "." BY " ","-" BY " ",
VALMEJ                "C" BY "S","Z" BY "S","J" BY "G","V" BY "B",
VALMEJ                "Y" BY "G","H" BY "G","I" BY "G"
               MOVE NOMBRE-AUX-VALNOM TO NOMBOK-INPUT-VALNOM
           END-IF.
           MOVE SPACES TO VECTOR-OK-VALNOM.
           UNSTRING CADENA-VALNOM DELIMITED BY ALL " " INTO
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
VLNM01               AND (14 - M-VALNOM > 0)
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

       10100-MATCH-APROXIMADO.
      ******************************************************************
      * En esta rutina se validan los bloques que no pasaron completos
      * con una aproximación. Esta rutina funciona correctamente con
      * los tipos de identificación 1, 3 y 4.
      ******************************************************************
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
                  VM-OK-VALNOM (J-VALNOM) NOT = SPACES
                  PERFORM UNTIL K-VALNOM > 15 OR L-VALNOM > 15
                    IF VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =
                       VMM-OK-VALNOM (J-VALNOM,K-VALNOM)
VALMEJ                 OR ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM)) = "*"
VALMEJ                 AND (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) = " "
VALMEJ                      OR = "N"))
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
                       VMM-OK-VALNOM (J-VALNOM,K-VALNOM)
VALMEJ                 OR ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM)) = "*"
VALMEJ                 AND (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) = " "
VALMEJ                      OR = "N"))
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
VALME1             IF ((POR-TOT-VALNOM + POR-INP-VALNOM)/2 >=
VALME1             APROX-TOT-VALNOM) AND A-VALNOM >= 63
                       ADD 1 TO BLOQOK-AUX-VALNOM
                       MOVE SPACES TO VM-OK-VALNOM(J-VALNOM)
                       VM-INPUT-VALNOM(I-VALNOM)
                   END-IF
               END-IF
               ADD 1 TO J-VALNOM
             END-PERFORM
             ADD 1 TO I-VALNOM
           END-PERFORM.
REMARK******************************************************************
REMARK*10100-MATCH-APROX-EMPRESAS
REMARK*     EN ESTA RUTINA SE VALIDAN LOS BLOQUES QUE NO PASARON COMPLET
REMARK*     CON UNA APROXIMACION. ESTA RUTINA FUNCIONA CORRECTAMENTE CON
REMARK*     LOS TIPOS DE IDENTIFICACION 2.
REMARK******************************************************************
       10100-MATCH-APROX-EMPRESAS.
         MOVE ZEROS TO TOTLET-INP-VALNOM
         MOVE ZEROS TO LETRAS-OK2-VALNOM
VALMEJ    MOVE ZEROS TO A-VALNOM
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
VALMEJ    MOVE 0 TO K-VALNOM
VALMEJ   IF BLOQOK-AUX-VALNOM < BLOQUES-INPUT-VALNOM
VALMEJ      INITIALIZE A-VALNOM VECTORES-VALNOM L-VALNOM
VALMEJ      PERFORM VARYING J-VALNOM FROM 1 BY 1 UNTIL J-VALNOM > 65
VALMEJ               IF V-NOMBRE-VALNOM(J-VALNOM) <> " "
VALMEJ                 MOVE FUNCTION ORD(V-NOMBRE-VALNOM(J-VALNOM))
VALMEJ                   TO I-VALNOM
VALMEJ                 ADD 1 TO CONT-LET-INPUT-VALNOM(I-VALNOM)
VALMEJ              END-IF
VALMEJ               IF V-NOMBOK-VALNOM(J-VALNOM) <> " "
VALMEJ                 MOVE FUNCTION ORD(V-NOMBOK-VALNOM(J-VALNOM))
VALMEJ                    TO I-VALNOM
VALMEJ                 ADD 1 TO CONT-LET-OK-VALNOM(I-VALNOM)
VALMEJ              END-IF
VALMEJ      END-PERFORM
VALMEJ      MOVE 0 TO I-VALNOM
VALMEJ       PERFORM VARYING J-VALNOM FROM 1 BY 1 UNTIL J-VALNOM > 256
VALMEJ         IF CONT-LET-OK-VALNOM(J-VALNOM)  > 0
VALMEJ            ADD 1 TO I-VALNOM
VALMEJ            IF CONT-LET-OK-VALNOM(J-VALNOM) <
VALMEJ               CONT-LET-INPUT-VALNOM(J-VALNOM)
VALMEJ               COMPUTE L-VALNOM = L-VALNOM + 100
VALMEJ            ELSE
VALMEJ               COMPUTE L-VALNOM = L-VALNOM +
VALMEJ               (CONT-LET-INPUT-VALNOM(J-VALNOM)
VALMEJ               *  100 / CONT-LET-OK-VALNOM(J-VALNOM))
VALMEJ            END-IF
VALMEJ         END-IF
VALMEJ       END-PERFORM
VALMEJ       COMPUTE K-VALNOM = L-VALNOM/I-VALNOM
VALMEJ       IF (A-VALNOM >= 40 AND K-VALNOM > 80) OR
VALMEJ         ((A-VALNOM >= 35 AND A-VALNOM < 40) AND K-VALNOM > 85) OR
VALMEJ         ((A-VALNOM >= 25 AND A-VALNOM < 35) AND K-VALNOM = 100)
VALMEJ         ADD 1 TO BLOQOK-AUX-VALNOM
VALMEJ       END-IF
VALMEJ     END-IF.


       150-ABRIR-ARCHIVOS.
      ******************************************************************
      * SE ABRE CADA UNO DE LOS ARCHIVOS QUE USA EL PROGRAMA PARA SU
      * EJECUCION:ICDIAN,CEDULAS,REG01, REG02,ICDAS,ICBTCO,ICBCLA,ICBCIU
      * ICT40D.DAT,DCVAL,DCINIC,ICMUNI,ICI40N.PRN
      * PREPARA EL LA LLAVE DEL ARCHIVO ICBTCO PARA IDENTIFICAR CON QUE
      * LLAVE MAYOR QUE 20000000 INICIA LA ESCRITURA DEL MISMO
      ******************************************************************
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
      *    controla cant de parámetros por valor, debería usar el COUNT
      *    de UNSTRING
           IF W-TARIFA = ZEROS
           or W-ENT-NOMBRE = SPACES
           or W-VAL-NOMBRE = SPACES
           or W-INVAL-NOMBRE = SPACES
R7094A     or W-BLOQUES > 4
               DISPLAY "!!!!!!    FALTA ALGUN PARAMETRO  !!!!!!! "
               STOP RUN
           END-IF
R7094A     IF W-BLOQUES = 0
R7094A         DISPLAY "!!..EJECUCION SIN VALIDAR NOMBRES       .....!!"
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

           READ ENTRADA AT END
               MOVE 1 TO FIN-ENTRADA
           NOT AT END
               INSPECT NUMID-X-E REPLACING ALL " " BY "0"
               INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"
           END-READ.
      *    Proceso de la "línea de comando" para omitir la validación
      *    de los nombres cuando el archivo es 1-11
R7094A     IF NOMBRE-E = "VALIDAR=NO" AND FIN-ENTRADA = 0
R7094A        MOVE 0 TO W-BLOQUES
R7094A        DISPLAY "Registro  control==> 000001 (VALIDAR=NO)"
R7094A        MOVE 0 TO W-BLOQUES
R7094A        READ ENTRADA AT END
R7094A            MOVE 1 TO FIN-ENTRADA
R7094A        NOT AT END
R7094A            INSPECT NUMID-X-E REPLACING ALL " " BY "0"
R7094A            INSPECT TIPOID-X-E REPLACING ALL "0" BY "1"
R7094A        END-READ
R7094A     ELSE
      *        $$$$ Ambiguo: no es el reg de control y/o no es EOF
R7094A         DISPLAY "Registro  control==> 000000 (VALIDAR=NO)"
R7094A     END-IF.
      *    $$$$ no está opened ...
           CLOSE CLAVE ABD14.






           copy BDAMATCH.PROC  *> agregado
* OJO **recordar que este copy está en LEEMAESTROS.PROC Y NO EN LOS PROG
REMARK* BDAMATCH.PROC.v.0005
REMARK******************************************************************
REMARK* ESTE COPY CONTIENE CADA UNA DE LAS RUTINA QUE ACCEDER AL MATCH
REMARK* DE NOMBRES USANDO LAS ESTRUCTURAS ESTANDARES DE DATACREDITO
REMARK* COPY QUE CONTIENE LA VARIABLES NECESARIAS PARA ELECCION DE MEJOR
REMARK* PORTAFOLIO Y MATCH DE NOMBRES A PARTIR DE LA INFORMACION DE LAS
REMARK* ESTRUCTURAS ESTANDAR DE DATACREDITO BDACSV, ICBDIR ICMCRE E ICMC
REMARK******************************************************************
REMARK* CAV0002     * CARTOT - CONTROLAR QUE EL INDICE QUE MANEJA      *
REMARK* C. VELASCO. * 2008-02  LA TABLA V-CARTOT-BDAMATCH NO EXCEDA LAS*
REMARK* R.MARTINEZ  *          10 POSICIONES DE LAS MISMA.             *
REMARK******************************************************************
REMARK* RMB0003     *VIGE01: SE CONTARAN LAS CUENTAS CON ADJETIVOS DE
REMARK* R.MARTINEZ  *        MUERTOS Y EL NUMERO TOTAL DE CUENTAS DEL
REMARK*             *        MEJOR PORTAFOLIO
REMARK* 25-MARZ-2010*        SALIDAS:  NUM-MUERTOS-OUTPUT-BDAMATCH
REMARK*             *                  NUM-CTAS-OUTPUT-BDAMATCH
REMARK*             *SINBLQ: TRABAJAR SOLO CON LOS REGISTROS DE CSV SIN
REMARK*             *        BLOQUEO (9 CONSECUTIVO 5) O CON BLOQUE
REMARK*             *        DE FRENADAS
REMARK*****************************************************************
REMARK* RMB0004     * PESO01 - OPTIMIZAR MANEJO DEL CALCULO DEL PESO  *
REMARK* R-MARTINEZ. *          DEL PORTAFOLIO .                       *
REMARK* 5-SEP-2001  * MEJO01 - DESEMPATAR PORTAFOLIOS EN CASO DE IGUL *
REMARK*             * BLOQ01 - MANEJAR BLOQUEO EN REGISTROS CSV       *
REMARK*             * FUEN01 - NCLUIR  FUENTES EXTERNAS               *
REMARK*             * APEL01 - INCLUIR PRIMER APELLIDO EN MATCH       *
REMARK*             * LIMT01 - MANEJAR LIMITES > 500 COMO TIPO 7      *
REMARK*             * NOMB01 - MAQUILLAR EL NOMBRE                    *
REMARK*             * VLNIT1 - SE ENVIA A LA RUTINA VALNOM LA VALIDA- *
REMARK*             *         CION DE LOS NITS CON 1 PARTICULA Y TIPO2*
REMARK*****************************************************************
REMARK* RMB0005     * AJUS01 - SE AJUSTA EL SOFTWARE PARA UE NO TOME  *
REMARK* R-MARTINEZ. *          EL TIPO DE CUENTA DE LAS 2 PRIMERAS PO-*
REMARK* 14-DIC 2011 *          SICIONES DEL SUSCRIPTOR SINO DEL TIPO  *
REMARK*             *          DECUENTA DE LA MISMA CUENTA            *
REMARK*****************************************************************

       1000-U-RECORRER-CSV-BDAMATCH.
REMARK******************************************************************
REMARK*1000-U-RECORRER-CSV-BDAMATCH.
REMARK* SE RECORRE TODA LA ESTRUCTURA BDACSV PARA LA IDENTIFICACION
REMARK* BUSCADA. EN CASO DE HALLAR REGISTROS SE PROCEDE A REALIZAR EL
REMARK* MATCH
REMARK******************************************************************
           MOVE 0 TO IDE-TIPO-BDAMATCH
VIGE01     MOVE 0 TO  NUM-MUERTOS-OUTPUT-BDAMATCH .
VIGE01     MOVE 0 TO  NUM-CTAS-OUTPUT-BDAMATCH.
           MOVE 0 TO MAX-CUENTAS-BDAMATCH.
           IF ABIERTO-BDAMATCH  = 0
              OPEN INPUT  BDACSV
              MOVE 1 TO ABIERTO-BDAMATCH
           END-IF.
           INITIALIZE LLAVE-BDACSV.
           MOVE TIP-IDE-INPUT-BDAMATCH  TO TIPO-ID-BDACSV .
           MOVE NUM-IDE-INPUT-BDAMATCH  TO NUM-ID-BDACSV .
           MOVE 0 TO FIN-BDAMATCH .
           MOVE 0 TO PROCESAR-BDAMATCH .
           START BDACSV  KEY > LLAVE-BDACSV
                 INVALID KEY
                    MOVE 1 TO FIN-BDAMATCH
                 NOT INVALID KEY
                    READ BDACSV IGNORE LOCK NEXT RECORD
                       AT END
                          MOVE 1 TO FIN-BDAMATCH
                    END-READ
           END-START.
           PERFORM UNTIL FIN-BDAMATCH  = 1 OR NUM-ID-BDACSV  NOT =
                                            NUM-IDE-INPUT-BDAMATCH  OR
                                            TIPO-ID-BDACSV  NOT =
                                            TIP-IDE-INPUT-BDAMATCH
              MOVE COD-SUS-BDACSV TO COD-SUSCRIP-ABD02
              READ SUSCRIPTOR IGNORE LOCK INVALID KEY
                  MOVE 0 TO PROCESAR-BDAMATCH
              NOT INVALID KEY
                  MOVE 1 TO PROCESAR-BDAMATCH
              END-READ
              IF PROCESAR-BDAMATCH = 1
                 IF ESTADO-ABD02 = 1
                    IF IND-BLOQUEO-BDACSV <> "B"
                       MOVE 1 TO PROCESAR-BDAMATCH
                       IF FUENTE-REG-BDACSV <= 01
                          PERFORM 1100-UBICAR-CHKCRE-CSV-BDAMATCH
                       ELSE
                          PERFORM 1100-LLENAR-AUX-BDAMATCH
                       END-IF
                    ELSE
                       MOVE 0 TO PROCESAR-BDAMATCH
                    END-IF
                 ELSE
                    MOVE 0 TO PROCESAR-BDAMATCH
                 END-IF
              END-IF
              IF PROCESAR-BDAMATCH = 1
                 PERFORM 1200-PROC-REG-CSV-BDAMATCH
              END-IF
              READ BDACSV IGNORE LOCK NEXT RECORD AT END
                  MOVE 1 TO FIN-BDAMATCH
              END-READ
              IF MAX-CUENTAS-BDAMATCH >= LIM-BDAMATCH *> TABLA LLENA
                 MOVE 1 TO FIN-BDAMATCH               *> SE ABLIGA A TIP
                 MOVE 0 TO MAX-CUENTAS-BDAMATCH       *> EFICIENCIA Y
                 MOVE SPACES TO NOM-OUTPUT-BDAMATCH   *> CONSISTENCIA
              END-IF
           END-PERFORM.
           IF MAX-CUENTAS-BDAMATCH > 0
APEL01        IF APELLIDO-INPUT-BDAMATCH <> SPACES AND
APEL01           MAX-CUENTAS-BDAMATCH > 1
APEL01           PERFORM 1100-LLENAR-CSV-APE-BDAMATCH
APEL01           PERFORM 1100-LLENAR-AUX-BDAMATCH
APEL01           PERFORM 1200-PROC-REG-CSV-BDAMATCH
APEL01        END-IF
              PERFORM 9000-U-REALIZAR-BDAMATCH
NOMB01        IF NOM-OUTPUT-BDAMATCH <> SPACES AND
NOMB01           (TIP-IDE-INPUT-BDAMATCH = 1 OR 4)
NOMB01           MOVE NOM-OUTPUT-BDAMATCH TO NOMBRE-INPMAQ
NOMB01           PERFORM 1100-MAQUILLAR-NOMBRE-MAQ
NOMB01           MOVE NOMBRE-OUTMAQ-OK TO NOM-OUTPUT-BDAMATCH
NOMB01        END-IF
              IF NOM-OUTPUT-BDAMATCH <> SPACES
                 MOVE 1 TO IDE-TIPO-BDAMATCH
              END-IF
           END-IF.
NOMB01 1100-MAQUILLAR-NOMBRE-MAQ.
NOMB01     MOVE FUNCTION UPPER-CASE(NOMBRE-INPMAQ) TO NOMBRE-INPMAQ
NOMB01     MOVE 0 TO I-MAQ
NOMB01     INSPECT NOMBRE-INPMAQ TALLYING I-MAQ FOR CHARACTERS BEFORE
NOMB01             INITIAL "Y/O"
NOMB01             OR      "Y\O"
NOMB01             OR      " Y/OTRO "
NOMB01             OR      " Y/OTRA "
NOMB01             OR      " Y\OTRO "
NOMB01             OR      " Y\OTRA "
NOMB01             OR      " Y OTRO "
NOMB01             OR      " Y OTRA "
NOMB01     IF I-MAQ < 45 AND I-MAQ > 2
NOMB01        ADD 1 TO I-MAQ
NOMB01        MOVE SPACES TO NOMBRE-INPMAQ(I-MAQ:)
NOMB01     ELSE
NOMB01        IF I-MAQ < 45
NOMB01           ADD 1 TO I-MAQ
NOMB01           MOVE SPACES TO NOMBRE-INPMAQ(I-MAQ:3)
NOMB01        END-IF
NOMB01     END-IF
NOMB01     MOVE 0 TO SW-BORRAR-MAQ
NOMB01     MOVE 1 TO I-MAQ.
NOMB01     PERFORM UNTIL I-MAQ > 45
NOMB01       MOVE NOMBRE-INPMAQ(I-MAQ:1) TO ACTUAL-MAQ
NOMB01       IF TILDE-MAQ
NOMB01          PERFORM 1000-CAMBIAR-TILDE-MAQ
NOMB01          MOVE ACTUAL-MAQ TO NOMBRE-INPMAQ(I-MAQ:1)
NOMB01       END-IF
NOMB01       IF FUNCTION ORD(ACTUAL-MAQ) < 31
NOMB01          MOVE "." TO ACTUAL-MAQ
NOMB01       END-IF
NOMB01       MOVE " " TO ANT-MAQ
NOMB01       IF I-MAQ > 1
NOMB01          COMPUTE J-MAQ = I-MAQ - 1
NOMB01          MOVE NOMBRE-INPMAQ(J-MAQ:1) TO ANT-MAQ
NOMB01       END-IF
NOMB01       MOVE " " TO SIG-MAQ
NOMB01       IF I-MAQ < 45
NOMB01          COMPUTE J-MAQ = I-MAQ + 1
NOMB01          MOVE NOMBRE-INPMAQ(J-MAQ:1) TO SIG-MAQ
NOMB01       END-IF
NOMB01       IF ACTUAL-MAQ = ")"
NOMB01          MOVE 0 TO SW-BORRAR-MAQ
NOMB01       END-IF
NOMB01       IF SW-BORRAR-MAQ = 0
NOMB01          IF ELIMINAR-MAQ
NOMB01             MOVE " " TO NOMBRE-INPMAQ (I-MAQ:1)
NOMB01             IF I-MAQ > 1
NOMB01                MOVE 1 TO SW-BORRAR-MAQ
NOMB01             END-IF
NOMB01          ELSE
NOMB01             IF BORRAR-MAQ
NOMB01                MOVE " " TO NOMBRE-INPMAQ (I-MAQ:1)
NOMB01             ELSE
NOMB01                IF QUITAR-MAQ
NOMB01                   COMPUTE J-MAQ = I-MAQ + 1
NOMB01                   MOVE NOMBRE-INPMAQ(J-MAQ:) TO
NOMB01                        NOMBRE-INPMAQ(I-MAQ:)
NOMB01                   COMPUTE I-MAQ = I-MAQ - 1
NOMB01                ELSE
NOMB01                   IF VALIDO-MAQ
NOMB01                      CONTINUE
NOMB01                   ELSE
NOMB01                      IF ANT-MAQ = "N" AND SIG-VOCAL-MAQ
NOMB01                         COMPUTE J-MAQ = I-MAQ + 1
NOMB01                         MOVE NOMBRE-INPMAQ(J-MAQ:) TO
NOMB01                              NOMBRE-INPMAQ(I-MAQ:)
NOMB01                         COMPUTE I-MAQ = I-MAQ - 1
NOMB01                      ELSE
NOMB01                         IF (ANT-VOCAL-MAQ AND SIG-VOCAL-MAQ) OR
NOMB01                            (ANT-VOCAL-MAQ AND SIG-MAQ = "N")
NOMB01                            IF (ANT-VOCAL-MAQ AND SIG-MAQ = "N")
NOMB01                               COMPUTE J-MAQ = I-MAQ + 1
NOMB01                               MOVE NOMBRE-INPMAQ(J-MAQ:) TO
NOMB01                                    NOMBRE-INPMAQ(I-MAQ:)
NOMB01                            ELSE
NOMB01                               MOVE "N" TO NOMBRE-INPMAQ(I-MAQ:1)
NOMB01                            END-IF
NOMB01                         ELSE
NOMB01                            IF NOT ANT-VOCAL-MAQ AND
NOMB01                                   SIG-VOCAL-MAQ
NOMB01                               MOVE "I" TO NOMBRE-INPMAQ(I-MAQ:1)
NOMB01                            ELSE
NOMB01                               MOVE " " TO NOMBRE-INPMAQ(I-MAQ:1)
NOMB01                            END-IF
NOMB01                         END-IF
NOMB01                      END-IF
NOMB01                   END-IF
NOMB01                END-IF
NOMB01             END-IF
NOMB01          END-IF
NOMB01       ELSE
NOMB01          MOVE " " TO NOMBRE-INPMAQ (I-MAQ:1)
NOMB01       END-IF
NOMB01       ADD 1 TO I-MAQ
NOMB01     END-PERFORM.
NOMB01     INITIALIZE PARTICULAS-INPMAQ
NOMB01     UNSTRING NOMBRE-INPMAQ-X DELIMITED BY ALL " "
NOMB01       INTO BASURA-INPMAQ P1-INPMAQ P2-INPMAQ P3-INPMAQ P4-INPMAQ
NOMB01            P5-INPMAQ P6-INPMAQ P7-INPMAQ P8-INPMAQ P9-INPMAQ
NOMB01            P10-INPMAQ
NOMB01     MOVE SPACES TO NOMBRE-OUTMAQ-OK
NOMB01     STRING P1-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P2-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P3-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P4-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P5-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P6-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P7-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P8-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P9-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01            P10-INPMAQ DELIMITED BY " "
NOMB01            " " DELIMITED BY SIZE
NOMB01       INTO NOMBRE-OUTMAQ-OK.
NOMB01       INSPECT NOMBRE-OUTMAQ-OK
NOMB01         REPLACING ALL  " PE A " BY " PENA ".
NOMB01  1000-CAMBIAR-TILDE-MAQ.
NOMB01       EVALUATE ACTUAL-MAQ
NOMB01          WHEN "Á"
NOMB01            MOVE "A" TO ACTUAL-MAQ
NOMB01          WHEN "É"
NOMB01            MOVE "E" TO ACTUAL-MAQ
NOMB01          WHEN "Í"
NOMB01            MOVE "I" TO ACTUAL-MAQ
NOMB01          WHEN "Ó"
NOMB01            MOVE "O" TO ACTUAL-MAQ
NOMB01          WHEN "Ú"
NOMB01            MOVE "U" TO ACTUAL-MAQ
NOMB01          WHEN "á"
NOMB01            MOVE "A" TO ACTUAL-MAQ
NOMB01          WHEN "é"
NOMB01            MOVE "E" TO ACTUAL-MAQ
NOMB01          WHEN "í"
NOMB01            MOVE "I" TO ACTUAL-MAQ
NOMB01          WHEN "ó"
NOMB01            MOVE "O" TO ACTUAL-MAQ
NOMB01          WHEN "Ú"
NOMB01            MOVE "U" TO ACTUAL-MAQ
NOMB01        END-EVALUATE.
APEL01 1100-LLENAR-CSV-APE-BDAMATCH.
APEL01     INITIALIZE REG-BDACSV
APEL01     MOVE TIP-IDE-INPUT-BDAMATCH TO  TIPO-ID-BDACSV
APEL01     MOVE NUM-IDE-INPUT-BDAMATCH TO  NUM-ID-BDACSV
APEL01     MOVE 980001         TO  COD-SUS-BDACSV COD-SUSCRIP-ABD02
APEL01     MOVE 02             TO  COD-CTA-BDACSV
APEL01     MOVE "APELLID-01"   TO  NUM-CTA-BDACSV
APEL01     MOVE APELLIDO-INPUT-BDAMATCH TO  NOMBRE-BDACSV
APEL01     MOVE ".-,"                   TO  NOMBRE-BDACSV(43:3)
APEL01     MOVE 20110801       TO  FECHA-CREA-BDACSV
APEL01     MOVE 0              TO  NUM-PORTAF-BDACSV
APEL01     MOVE 0              TO  IND-NOMBRE-BDACSV
APEL01     MOVE 0              TO  MEJOR-PORTAF-BDACSV
APEL01     MOVE 0              TO  IND-ACTUALIZ-BDACSV
APEL01     MOVE 01             TO  FUENTE-REG-BDACSV.
APEL01 1100-LLENAR-AUX-BDAMATCH.
REMARK******************************************************************
REMARK*1100-LLENAR-AUX-BDAMATCH.
REMARK* SE LLENAN LOS CAMPOS DEL CSV EN AREAS AUXILIARES DE CONVERSION D
REMARK* TAMAÑO
REMARK******************************************************************
APEL01     MOVE TIPO-ID-BDACSV    TO TIPO-ID-AUX11-BDAMATCH  .
APEL01     MOVE NUM-ID-BDACSV     TO NUM-ID-AUX11-BDAMATCH   .
APEL01     MOVE COD-SUS-BDACSV    TO COD-SUS-AUX11-BDAMATCH  .
APEL01     MOVE COD-CTA-BDACSV    TO COD-CTA-AUX11-BDAMATCH  .
APEL01     MOVE NUM-CTA-BDACSV    TO NUM-CTA-AUX11-BDAMATCH  .
APEL01     MOVE TIPO-ID-BDACSV    TO TIPO-ID-AUX13-BDAMATCH  .
APEL01     MOVE NUM-ID-BDACSV     TO NUM-ID-AUX13-BDAMATCH   .
APEL01     MOVE COD-SUS-BDACSV    TO COD-SUS-AUX13-BDAMATCH  .
APEL01     MOVE COD-CTA-BDACSV    TO COD-CTA-AUX13-BDAMATCH  .
APEL01     MOVE NUM-CTA-BDACSV    TO NUM-CTA-AUX13-BDAMATCH  .
APEL01     MOVE 0 TO FALLECIDO-BDAMATCH.
       1100-UBICAR-CHKCRE-CSV-BDAMATCH.
REMARK******************************************************************
REMARK*1100-UBICAR-CHKCRE-CSV-BDAMATCH.
REMARK* DE ACUERDO AL TIPO DE CUENTA SE PROCEDE A VALIDAR QUE ESTAS EXIS
REMARK* EN CUYO CASO SE INSERTA LA INFORMACION EN LAS ESTRUCTURAS DEL MA
REMARK******************************************************************
           MOVE TIPO-ID-BDACSV    TO TIPO-ID-AUX11-BDAMATCH  .
           MOVE NUM-ID-BDACSV     TO NUM-ID-AUX11-BDAMATCH   .
           MOVE COD-SUS-BDACSV    TO COD-SUS-AUX11-BDAMATCH  .
           MOVE COD-CTA-BDACSV    TO COD-CTA-AUX11-BDAMATCH  .
           MOVE NUM-CTA-BDACSV    TO NUM-CTA-AUX11-BDAMATCH  .
           MOVE TIPO-ID-BDACSV    TO TIPO-ID-AUX13-BDAMATCH  .
           MOVE NUM-ID-BDACSV     TO NUM-ID-AUX13-BDAMATCH   .
           MOVE COD-SUS-BDACSV    TO COD-SUS-AUX13-BDAMATCH  .
           MOVE COD-CTA-BDACSV    TO COD-CTA-AUX13-BDAMATCH  .
           MOVE NUM-CTA-BDACSV    TO NUM-CTA-AUX13-BDAMATCH  .
VIGE01     MOVE 0 TO FALLECIDO-BDAMATCH
           IF COD-CTA-BDACSV = 01 OR 51
              MOVE LLAVE-AUX11-BDAMATCH  TO LLAVE-MAES-CHECK
              READ MAESTRO-CHECK IGNORE LOCK INVALID KEY
                   MOVE 0 TO PROCESAR-BDAMATCH
VIGE01        NOT INVALID KEY
BLOQ01            IF (IND-BLOQUEO-ABD06 = 7 OR 8 OR 9)
SINBLQ                AND CONSECUTIVO-ABD06 = 1
VIGE01               IF ADJETIVO-ABD06 = 1
VIGE01                  MOVE 1 TO FALLECIDO-BDAMATCH
VIGE01               END-IF
SINBLQ            ELSE
SINBLQ               MOVE 0 TO PROCESAR-BDAMATCH
SINBLQ            END-IF
SINBLQ        END-READ
           ELSE
              MOVE LLAVE-AUX11-BDAMATCH  TO LLAVE-MAES-CREDIT
              READ MAESTRO-CREDIT IGNORE LOCK INVALID KEY
                   MOVE 0 TO PROCESAR-BDAMATCH
VIGE01        NOT INVALID KEY
BLOQ01            IF (IND-BLOQUEO-ABD07 = 7 OR 8 OR 9)
SINBLQ               AND CONSECUTIVO-ABD07 = 1
VIGE01               IF ADJETIVO1-ABD07 = 1 OR
VIGE01                  ADJETIVO2-ABD07 = 1 OR
VIGE01                  ADJETIVO3-ABD07 = 1
VIGE01                  MOVE 1 TO FALLECIDO-BDAMATCH
VIGE01               END-IF
SINBLQ            ELSE
SINBLQ               MOVE 0 TO PROCESAR-BDAMATCH
VIGE01            END-IF
              END-READ
           END-IF.

       1200-PROC-REG-CSV-BDAMATCH.
REMARK******************************************************************
REMARK*1200-PROC-REG-CSV-BDAMATCH.
REMARK* SE INSERTA LA CUENTA,EL NOMBRE Y SE UBICA EL REGISTRO DE DIRECCI
REMARK* SI LO HAY
REMARK******************************************************************
           IF MAX-CUENTAS-BDAMATCH = 0
              PERFORM 1000-U-INICIALIZAR-BDAMATCH
           END-IF.
           MOVE LLAVE-AUX11-BDAMATCH   TO CTA-INPUT-BDAMATCH.
           MOVE  COD-SUSCRIP-ABD02     TO  SUSC-CTAS-BDAMATCH.    *>DLMD
           PERFORM 2000-U-INSERTAR-CTA-BDAMATCH.
           MOVE NOMBRE-BDACSV  TO NOM-INPUT-BDAMATCH.
           PERFORM 2000-U-INSERTAR-NOM-BDAMATCH.
           MOVE LLAVE-AUX13-BDAMATCH TO LLAVE-DIRECC.
           READ MAESTRO-DIRECC IGNORE LOCK
MEJO01*         NOT INVALID KEY
MEJO01*             PERFORM 1210-DIRTEL-BDACSV-BDAMATCH
MEJO01          INVALID KEY
MEJO01              MOVE SPACES TO LLAVE-DIRECC
           END-READ.
MEJO01     IF LLAVE-DIRECC <> SPACES
MEJO01        PERFORM 1210-DIRTEL-BDACSV-BDAMATCH
MEJO01     END-IF.

       1210-DIRTEL-BDACSV-BDAMATCH.
REMARK******************************************************************
REMARK*1210-DIRTEL-BDACSV-BDAMATCH.
REMARK* SE INSERTAN LAS DIRECCIONES Y LOS TELEFONOS DEL REGISTRO ESTANDA
REMARK* DE DIRECCIONES DENTRO DE LAS ESTRUCTURAS DEL MATCH.
REMARK******************************************************************
           IF CIURES-DIRECC NOT = SPACES
              IF DIRRES-DIRECC NOT = SPACES
                 MOVE DIRRES-DIRECC TO DIR-INPUT-BDAMATCH
                 PERFORM 2000-U-INSERTAR-DIR-BDAMATCH
              END-IF
              IF TELRES-DIRECC > 0 AND
                 TELRES-DIRECC < 9999999999
                 MOVE TELRES-DIRECC TO TEL-INPUT-BDAMATCH
                 PERFORM 2000-U-INSERTAR-TEL-BDAMATCH
              END-IF
           END-IF.
           IF CIULAB-DIRECC NOT = SPACES
              IF DIRLAB-DIRECC NOT = SPACES
                 MOVE DIRLAB-DIRECC TO DIR-INPUT-BDAMATCH
                 PERFORM 2000-U-INSERTAR-DIR-BDAMATCH
              END-IF
              IF TELLAB-DIRECC > 0 AND
                 TELLAB-DIRECC < 9999999999
                 MOVE TELLAB-DIRECC TO TEL-INPUT-BDAMATCH
                 PERFORM 2000-U-INSERTAR-TEL-BDAMATCH
              END-IF
           END-IF.
           IF CIUCOR-DIRECC NOT = SPACES AND
              DIRCOR-DIRECC NOT = SPACES
              MOVE DIRCOR-DIRECC TO DIR-INPUT-BDAMATCH
              PERFORM 2000-U-INSERTAR-DIR-BDAMATCH
           END-IF.

       8000-U-VER-CTAABD07-BDAMATCH.
REMARK******************************************************************
REMARK*8000-U-VER-CTAABD07-BDAMATCH.
REMARK*  SE BUSCA EN LA TABLA DE CUENTAS DEL MEJOR PORTAFOLIO SI LA LLAV
REMARK*  LA CUENTA DEL ICMCRE ES VALIDA. EN ESTE CASO SE CAMBIA EL INDIC
REMARK*  DE BLOQUEO A 0
REMARK******************************************************************
           IF IDE-TIPO-BDAMATCH = 1
              MOVE LLAVE-MAES-CREDIT TO CTA-INPUT-BDAMATCH
              PERFORM 5000-U-VALIDAR-CUENTA-BDAMATCH
              IF VALCTA-OUTPUT-BDAMATCH = 1 AND IND-BLOQUEO-ABD07 = 9
                 MOVE 0 TO IND-BLOQUEO-ABD07
              END-IF
           END-IF.

       8000-U-VER-CTAABD06-BDAMATCH.
REMARK******************************************************************
REMARK*8000-U-VER-CTAABD06-BDAMATCH.
REMARK*  SE BUSCA EN LA TABLA DE CUENTAS DEL MEJOR PORTAFOLIO SI LA LLAV
REMARK*  LA CUENTA DEL ICMCHK ES VALIDA. EN ESTE CASO SE CAMBIA EL INDIC
REMARK*  DE BLOQUEO A 0
REMARK******************************************************************
           IF IDE-TIPO-BDAMATCH = 1
              MOVE LLAVE-MAES-CHECK  TO CTA-INPUT-BDAMATCH
              PERFORM 5000-U-VALIDAR-CUENTA-BDAMATCH
              IF VALCTA-OUTPUT-BDAMATCH = 1 AND IND-BLOQUEO-ABD06 = 9
                 MOVE 0 TO IND-BLOQUEO-ABD06
              END-IF
           END-IF.

REMARK*=================================================================
       1000-U-INICIALIZAR-BDAMATCH.
REMARK******************************************************************
REMARK*1000-U-INICIALIZAR-BDAMATCH.
REMARK* INPUT = VACIO
REMARK* OUTPUT= VACIO
REMARK* SE INICIALIZAN TODAS LAS ESTRUCTURAS UTILIZADAS EN LA REALIZACIO
REMARK* DEl MATCH.
REMARK******************************************************************
           MOVE 1 TO I-DIR-BDAMATCH I-TEL-BDAMATCH I-NOM-BDAMATCH.
           MOVE 0 TO I-CTA-BDAMATCH.
PESO01     INITIALIZE TABLA-CUENTAS-BDAMATCH.
           INITIALIZE TABLA-DIR-BDAMATCH.
           INITIALIZE TABLA-TEL-BDAMATCH.
           INITIALIZE TABLA-NOM-BDAMATCH.
           INITIALIZE TABLA-GRUNOM-BDAMATCH.
           INITIALIZE V-INTERRELAC-BDAMATCH.
           MOVE ZEROS   TO POSIC-SUSC-BDAMATCH
                            POSIC-SUS-GRU-BDAMATCH
           MOVE ALL "9" TO TABLA-SUSC-DIR-BDAMATCH
           MOVE ALL "9" TO TABLA-SUSC-NOMBRE-BDAMATCH.

       2000-U-INSERTAR-DIR-BDAMATCH.
REMARK******************************************************************
REMARK*2000-U-INSERTAR-DIR-BDAMATCH.
REMARK* INPUT = DIR-INPUT-BDAMATCH
REMARK* OUTPUT= VACIO
REMARK* SE INSERTA EN LA TABLA DE DIRECCIONES LOS NUMEROS DE LA DIRECCIO
REMARK* PASADA EN EL PARAMETRO ORDENADOS DE MAYOR A MENOR . SIEMPRE QUE
REMARK* LA DIRECCION CONTENGA ALGUN NUMERO
REMARK******************************************************************
           MOVE DIR-INPUT-BDAMATCH  TO T-CARDIR-BDAMATCH.
           MOVE 1 TO I-BDAMATCH
           MOVE 0 TO J-BDAMATCH
           MOVE " " TO CAR-ANT-BDAMATCH
           INITIALIZE T-CARTOT-BDAMATCH
CARTOT     PERFORM UNTIL I-BDAMATCH > 60 OR J-BDAMATCH >= 10
             IF V-CARDIR-BDAMATCH  (I-BDAMATCH) NUMERIC
                MOVE V-CARDIR-BDAMATCH (I-BDAMATCH) TO CAR-ACT-BDAMATCH
                IF CAR-ANT-BDAMATCH   NUMERIC
                   COMPUTE V-CARTOT-BDAMATCH (J-BDAMATCH)
                           = V-CARTOT-BDAMATCH  (J-BDAMATCH)
                           * 10 + CAR-ACT-BDAMATCH
                ELSE
                   ADD 1 TO J-BDAMATCH
                   MOVE V-CARDIR-BDAMATCH  (I-BDAMATCH)
                        TO V-CARTOT-BDAMATCH  (J-BDAMATCH)
                END-IF
             END-IF
             MOVE V-CARDIR-BDAMATCH (I-BDAMATCH) TO CAR-ANT-BDAMATCH
             ADD 1 TO I-BDAMATCH
           END-PERFORM.
           SORT V-CARTOT-BDAMATCH   DESCENDING.
           IF T-CARTOT-BDAMATCH   NOT = ZEROS AND
              I-DIR-BDAMATCH < LIM-BDAMATCH
              MOVE T-CARTOT-BDAMATCH TO
                   NUM-DIR-BDAMATCH(I-DIR-BDAMATCH)
              MOVE SUSC-CTAS-BDAMATCH TO
                   SUS-DIR-BDAMATCH(I-DIR-BDAMATCH)
              MOVE I-CTA-BDAMATCH TO CTA-DIR-BDAMATCH(I-DIR-BDAMATCH)
              PERFORM  VARYING POSIC-SUMA-BDAMATCH FROM 1 BY 1
                       UNTIL  POSIC-SUMA-BDAMATCH  >  10 OR
                       V-CARTOT-BDAMATCH(POSIC-SUMA-BDAMATCH)  =  ZERO
                 ADD   1  TO   LNG-DIR-BDAMATCH (I-DIR-BDAMATCH)
              END-PERFORM
              ADD 1 TO I-DIR-BDAMATCH
           END-IF.

       2000-U-INSERTAR-TEL-BDAMATCH.
REMARK******************************************************************
REMARK*2000-U-INSERTAR-TEL-BDAMATCH.
REMARK* INPUT = TEL-INPUT-BDAMATCH
REMARK* OUTPUT= VACIO
REMARK* SE INSERTA EN LA TABLA DE TELEFONOS EL NUMERO DEL TELEFONO
REMARK* PASADO EN EL PARAMETRO  SIEMPRE QUE SEA MAYOR A CERO
REMARK******************************************************************
           IF I-TEL-BDAMATCH < LIM-BDAMATCH AND TEL-INPUT-BDAMATCH > 0
              MOVE TEL-INPUT-BDAMATCH TO
                                      NUM-TEL-BDAMATCH(I-TEL-BDAMATCH)
              MOVE SUSC-CTAS-BDAMATCH TO
                                      SUS-TEL-BDAMATCH(I-TEL-BDAMATCH)
              MOVE I-CTA-BDAMATCH     TO
                                      CTA-TEL-BDAMATCH(I-TEL-BDAMATCH)
              ADD 1 TO I-TEL-BDAMATCH
           END-IF.

       2000-U-INSERTAR-NOM-BDAMATCH.
REMARK******************************************************************
REMARK*2000-U-INSERTAR-NOM-BDAMATCH.
REMARK* INPUT = NOM-INPUT-BDAMATCH
REMARK* OUTPUT= VACIO
REMARK* SE INSERTA EN LA TABLA DE NOMBRES   EL NOMBRE
REMARK* PASADO EN EL PARAMETRO  SIEMPRE QUE SEA DIFERENTE DE ESPACIOS
REMARK******************************************************************
           IF I-NOM-BDAMATCH < LIM-BDAMATCH AND
              NOM-INPUT-BDAMATCH <> SPACES
              MOVE NOM-INPUT-BDAMATCH  TO
                   NOM-NOM-BDAMATCH(I-NOM-BDAMATCH)
                                         NOM-LIMPI-BDAMATCH
              MOVE I-CTA-BDAMATCH TO CTA-NOM-BDAMATCH(I-NOM-BDAMATCH)
              MOVE ZEROS TO PESO-CTAS-TOT-BDAMATCH  NRO-VECES-1-BDAMATCH
                                  NRO-VECES-2-BDAMATCH
              PERFORM VARYING NRO-VECES-1-BDAMATCH FROM 1 BY 1
                UNTIL  NRO-VECES-1-BDAMATCH  >  60
                IF NRO-VECES-1-BDAMATCH  >  1 AND
                 NOM-LIMPI-BDAMATCH(NRO-VECES-1-BDAMATCH - 1:1) = " "
                   SUBTRACT  1 FROM  NRO-VECES-1-BDAMATCH
                END-IF
                IF NOM-LIMPI-BDAMATCH(NRO-VECES-1-BDAMATCH:1) = " "
                  COMPUTE  NRO-VECES-2-BDAMATCH =
                           60 - NRO-VECES-1-BDAMATCH
                  MOVE  NOM-LIMPI-BDAMATCH(NRO-VECES-1-BDAMATCH +
                        1:NRO-VECES-2-BDAMATCH)
                      TO   NOM-LIMPI-BDAMATCH(NRO-VECES-1-BDAMATCH:)
                  IF NOM-LIMPI-BDAMATCH(NRO-VECES-1-BDAMATCH +
                     1:NRO-VECES-2-BDAMATCH) = SPACES
                     MOVE  85           TO  NRO-VECES-1-BDAMATCH
                  END-IF
                END-IF
              END-PERFORM
              INSPECT NOM-LIMPI-BDAMATCH
                      TALLYING       PESO-CTAS-TOT-BDAMATCH
                      FOR CHARACTERS BEFORE INITIAL " "
              MOVE  PESO-CTAS-TOT-BDAMATCH TO
                    LNG-NOM-BDAMATCH(I-NOM-BDAMATCH)
FUEN01        IF FUENTE-REG-BDACSV >= 02
FUEN01           MOVE 99 TO LNG-NOM-BDAMATCH(I-NOM-BDAMATCH)
FUEN01        END-IF
              MOVE  SUSC-CTAS-BDAMATCH TO
                    SUS-NOM-BDAMATCH(I-NOM-BDAMATCH)
              ADD 1 TO I-NOM-BDAMATCH
           END-IF.

       2000-U-INSERTAR-CTA-BDAMATCH.
REMARK******************************************************************
REMARK*2000-U-INSERTAR-CTA-BDAMATCH.
REMARK* INPUT = CTA-INPUT-BDAMATCH
REMARK* OUTPUT= VACIO
REMARK* SE INSERTA EN LA TABLA DE CUENTAS EL VALOR DE LA CUENTA
REMARK* PASADO EN EL PARAMETRO  SIEMPRE QUE SEA DIFERENTE DE ESPACIOS
REMARK******************************************************************
           IF I-CTA-BDAMATCH < LIM-BDAMATCH AND
              CTA-INPUT-BDAMATCH <> SPACES
              ADD 1 TO I-CTA-BDAMATCH
              MOVE CTA-INPUT-BDAMATCH TO
                   CTA-CUENTAS-BDAMATCH(I-CTA-BDAMATCH)
              MOVE I-CTA-BDAMATCH     TO MAX-CUENTAS-BDAMATCH
VIGE01        MOVE FALLECIDO-BDAMATCH TO
VIGE01             CTA-FALLECIDO-BDAMATCH(I-CTA-BDAMATCH)
FUEN01        IF FUENTE-REG-BDACSV >= 2
FUEN01           MOVE 1 TO FUEEXT-CUENTAS-BDAMATCH(I-CTA-BDAMATCH)
FUEN01        END-IF
           END-IF.
       3000-U-ORDENAR-DIR-BDAMATCH.
REMARK******************************************************************
REMARK*3000-U-ORDENAR-DIR-BDAMATCH.
REMARK* INPUT = TABLA DE DIRECCIONES LLENA
REMARK* OUTPUT= TABLA DE RELACIONES LLENADA CON DIRECCIONES
REMARK* SE ORDENA LA TABLA DE DIRECCIONES EN SUBPORTAFOLIOS QUE SON
REMARK* INSERTADOS EN LA TABLA DE RELACIONAMIENTO
REMARK******************************************************************
           SORT T-DIR-BDAMATCH
                DESCENDING NUM-DIR-BDAMATCH
                ASCENDING  CTA-DIR-BDAMATCH.
           MOVE 1 TO I-CTA-BDAMATCH.
           MOVE 0 TO FIL-FUE-BDAMATCH
           MOVE 1 TO COL-FUE-BDAMATCH
           MOVE 999                TO CTA-DIR-ANT-BDAMATCH
           MOVE ALL "9"            TO NUM-DIR-ANT-BDAMATCH
           PERFORM UNTIL I-CTA-BDAMATCH > LIM-BDAMATCH OR
                               CTA-DIR-BDAMATCH(I-CTA-BDAMATCH) = 0
             IF NUM-DIR-ANT-BDAMATCH <>
                                       NUM-DIR-BDAMATCH(I-CTA-BDAMATCH)
                ADD  1 TO FIL-FUE-BDAMATCH
                MOVE 1 TO COL-FUE-BDAMATCH
                MOVE CTA-DIR-BDAMATCH(I-CTA-BDAMATCH) TO
                     CAJA-BDAMATCH(FIL-FUE-BDAMATCH,COL-FUE-BDAMATCH)
                MOVE "DIR" TO CARAC-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                ADD 1 TO NUM-CTAS-DIR-BDAMATCH(I-CTA-BDAMATCH)
                MOVE  I-CTA-BDAMATCH       TO  POSIC-SUMA-BDAMATCH
                ADD 1 TO COL-FUE-BDAMATCH
             ELSE
                IF CTA-DIR-ANT-BDAMATCH <>
                   CTA-DIR-BDAMATCH(I-CTA-BDAMATCH)
                   MOVE CTA-DIR-BDAMATCH(I-CTA-BDAMATCH) TO
                     CAJA-BDAMATCH(FIL-FUE-BDAMATCH,COL-FUE-BDAMATCH)
                   MOVE "DIR" TO CARAC-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                   ADD 1 TO COL-FUE-BDAMATCH
                END-IF
                  ADD 1 TO NUM-CTAS-DIR-BDAMATCH(POSIC-SUMA-BDAMATCH)
             END-IF
             MOVE CTA-DIR-BDAMATCH(I-CTA-BDAMATCH) TO
                                                CTA-DIR-ANT-BDAMATCH
             MOVE NUM-DIR-BDAMATCH(I-CTA-BDAMATCH) TO
                                                 NUM-DIR-ANT-BDAMATCH
             ADD 1 TO I-CTA-BDAMATCH
           END-PERFORM.
           MOVE FIL-FUE-BDAMATCH TO MAX-REL-BDAMATCH.

       3000-U-ORDENAR-TEL-BDAMATCH.
REMARK******************************************************************
REMARK*3000-U-ORDENAR-TEL-BDAMATCH.
REMARK* INPUT = TABLA DE TELEFONOS   LLENA
REMARK* OUTPUT= TABLA DE RELACIONES LLENADA CON TELEFONOS
REMARK* SE ORDENA LA TABLA DE TELEFONOS EN SUBPORTAFOLIOS QUE SON
REMARK* INSERTADOS EN LA TABLA DE RELACIONAMIENTO
REMARK******************************************************************
           SORT T-TEL-BDAMATCH
                DESCENDING NUM-TEL-BDAMATCH
                ASCENDING  CTA-TEL-BDAMATCH .
           MOVE 1 TO I-CTA-BDAMATCH.
           MOVE 1 TO COL-FUE-BDAMATCH
           MOVE 999      TO CTA-TEL-ANT-BDAMATCH
           MOVE ALL "9"  TO NUM-TEL-ANT-BDAMATCH
           PERFORM UNTIL I-CTA-BDAMATCH > LIM-BDAMATCH
                   OR  CTA-TEL-BDAMATCH(I-CTA-BDAMATCH) = 0
                   OR  FIL-FUE-BDAMATCH > LIM-BDAMATCH
                   OR  COL-FUE-BDAMATCH > LIM-BDAMATCH
             IF NUM-TEL-ANT-BDAMATCH <>
                NUM-TEL-BDAMATCH(I-CTA-BDAMATCH)
                ADD  1 TO FIL-FUE-BDAMATCH
                MOVE 1 TO COL-FUE-BDAMATCH
                MOVE CTA-TEL-BDAMATCH(I-CTA-BDAMATCH) TO
                     CAJA-BDAMATCH(FIL-FUE-BDAMATCH,COL-FUE-BDAMATCH)
                MOVE "TEL" TO CARAC-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                ADD  1 TO  NUM-CTAS-TEL-BDAMATCH(I-CTA-BDAMATCH)
                MOVE I-CTA-BDAMATCH TO POSIC-SUMA-BDAMATCH
                ADD 1 TO COL-FUE-BDAMATCH
             ELSE
                IF CTA-TEL-ANT-BDAMATCH <>
                   CTA-TEL-BDAMATCH(I-CTA-BDAMATCH)
                   MOVE CTA-TEL-BDAMATCH(I-CTA-BDAMATCH) TO
                       CAJA-BDAMATCH(FIL-FUE-BDAMATCH,COL-FUE-BDAMATCH)
                   MOVE "TEL" TO CARAC-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                   ADD 1 TO COL-FUE-BDAMATCH
                END-IF
                  ADD   1 TO NUM-CTAS-TEL-BDAMATCH(POSIC-SUMA-BDAMATCH)
             END-IF
             MOVE CTA-TEL-BDAMATCH(I-CTA-BDAMATCH) TO
                                                 CTA-TEL-ANT-BDAMATCH
             MOVE NUM-TEL-BDAMATCH(I-CTA-BDAMATCH) TO
                                                 NUM-TEL-ANT-BDAMATCH
             ADD 1 TO I-CTA-BDAMATCH
           END-PERFORM.
           MOVE FIL-FUE-BDAMATCH TO MAX-REL-BDAMATCH.

       3000-U-ORDENAR-NOM-BDAMATCH.
REMARK******************************************************************
REMARK*3000-U-ORDENAR-NOM-BDAMATCH.
REMARK* INPUT = TABLA DE NOMBRES     LLENA
REMARK* OUTPUT= TABLA DE RELACIONES LLENADA CON NOMBRES
REMARK* SE ORDENA LA TABLA DE NOMBRES   EN SUBPORTAFOLIOS QUE SON
REMARK* INSERTADOS EN LA TABLA DE RELACIONAMIENTO
REMARK******************************************************************
           SORT T-NOM-BDAMATCH
                DESCENDING NOM-NOM-BDAMATCH
                DESCENDING LNG-NOM-BDAMATCH
                ASCENDING  CTA-NOM-BDAMATCH .
           MOVE 1 TO I-NOM-BDAMATCH.
           MOVE 1 TO GRU-NOM-BDAMATCH.
           MOVE GRU-NOM-BDAMATCH TO
                GRU-NOMS-BDAMATCH(I-NOM-BDAMATCH).
           MOVE GRU-NOM-BDAMATCH TO
                GRUPO-GRUNOM-BDAMATCH(I-NOM-BDAMATCH).
           MOVE NOM-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
                NOM-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH).
           MOVE SUS-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
                SUSC-GRUP-SUS-BDAMATCH
           MOVE GRUPO-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH) TO
                SUSC-GRUP-GRU-BDAMATCH
           PERFORM  400-BUSCAR-SUSC-GRUPO-BDAMATCH
           IF  INV-BDAMATCH  =  1
             ADD   1        TO
             NUM-CTAS-GRUNOM-BDAMATCH(GRU-NOMS-BDAMATCH(I-NOM-BDAMATCH))
           END-IF
           MOVE LNG-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
                LNG-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
           ADD 1 TO I-NOM-BDAMATCH.
           PERFORM UNTIL I-NOM-BDAMATCH > LIM-BDAMATCH
                   OR CTA-NOM-BDAMATCH(I-NOM-BDAMATCH) = 0
             MOVE 1 TO I-GRU-BDAMATCH
             MOVE 0 TO MATCH-OK-OUT-VALNOM
             PERFORM UNTIL NOM-GRUNOM-BDAMATCH(I-GRU-BDAMATCH) =
                     SPACES OR I-GRU-BDAMATCH > LIM-BDAMATCH
                     OR MATCH-OK-OUT-VALNOM = 1
               MOVE NOM-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
                    NOMBRE-INPUT-VALNOM
               MOVE NOM-GRUNOM-BDAMATCH(I-GRU-BDAMATCH) TO
                    NOMBOK-INPUT-VALNOM
VLNIT1         IF  TIP-IDE-INPUT-BDAMATCH = 2 OR 3
VLNIT1            IF NUM-IDE-INPUT-BDAMATCH >= 600000000 AND
VLNIT1               NUM-IDE-INPUT-BDAMATCH <= 999999999
VLNIT1               MOVE 2 TO TIP-ID-INPUT-VALNOM
VLNIT1               MOVE 1 TO BLOQUES-INPUT-VALNOM
VLNIT1            ELSE
VLNIT1               MOVE 1 TO TIP-ID-INPUT-VALNOM
VLNIT1               MOVE 2 TO BLOQUES-INPUT-VALNOM
VLNIT1            END-IF
VLNIT1         ELSE
VLNIT1            MOVE 1 TO TIP-ID-INPUT-VALNOM
VLNIT1            MOVE 2 TO BLOQUES-INPUT-VALNOM
VLNIT1         END-IF
APEL01         IF NOMBRE-INPUT-VALNOM(43:3) = ".-," OR
APEL01            NOMBOK-INPUT-VALNOM(43:3) = ".-,"
APEL01            MOVE 1 TO BLOQUES-INPUT-VALNOM
APEL01         END-IF
               PERFORM 10000-MATCH-NOMBRES-VALNOM
               IF MATCH-OK-OUT-VALNOM = 1
                  MOVE I-GRU-BDAMATCH TO
                       GRU-NOMS-BDAMATCH(I-NOM-BDAMATCH)
                  MOVE SUS-NOM-BDAMATCH(I-NOM-BDAMATCH)
                                       TO  SUSC-GRUP-SUS-BDAMATCH
                  MOVE GRUPO-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
                                       TO  SUSC-GRUP-GRU-BDAMATCH
APEL01            IF BLOQUES-INPUT-VALNOM = 1   *> ES UN APELLIDO
APEL01               MOVE 1 TO APE-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
APEL01            END-IF
                  PERFORM  400-BUSCAR-SUSC-GRUPO-BDAMATCH
                  IF  INV-BDAMATCH  =  1
                    ADD   1  TO
             NUM-CTAS-GRUNOM-BDAMATCH(GRU-NOMS-BDAMATCH(I-NOM-BDAMATCH))
                  END-IF
                  IF  LNG-NOM-BDAMATCH(I-NOM-BDAMATCH)  >
                      LNG-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
                    MOVE LNG-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
                      LNG-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
PESO01              MOVE NOM-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
PESO01                NOM-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
                  END-IF
               END-IF
               ADD 1 TO I-GRU-BDAMATCH
             END-PERFORM
             IF MATCH-OK-OUT-VALNOM = 0  *> NUEVO GRUPO
                ADD 1 TO GRU-NOM-BDAMATCH
                MOVE NOM-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
                     NOM-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
                MOVE GRU-NOM-BDAMATCH  TO
                     GRU-NOMS-BDAMATCH(I-NOM-BDAMATCH)
                MOVE GRU-NOM-BDAMATCH  TO
                     GRUPO-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)*> DLM
                MOVE SUS-NOM-BDAMATCH(I-NOM-BDAMATCH)
                                   TO  SUSC-GRUP-SUS-BDAMATCH
                MOVE GRUPO-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
                                   TO  SUSC-GRUP-GRU-BDAMATCH
                PERFORM  400-BUSCAR-SUSC-GRUPO-BDAMATCH
                IF  INV-BDAMATCH  =  1
                  ADD   1   TO
             NUM-CTAS-GRUNOM-BDAMATCH(GRU-NOMS-BDAMATCH(I-NOM-BDAMATCH))
                END-IF
                MOVE LNG-NOM-BDAMATCH(I-NOM-BDAMATCH) TO
                     LNG-GRUNOM-BDAMATCH(GRU-NOM-BDAMATCH)
             ELSE                                                    *>
                MOVE GRU-NOM-BDAMATCH TO
                     GRU-NOMS-BDAMATCH(I-NOM-BDAMATCH)   *> DLM
             END-IF
             ADD 1 TO I-NOM-BDAMATCH
           END-PERFORM.
           SORT T-NOM-BDAMATCH
                DESCENDING NOM-NOM-BDAMATCH
                DESCENDING GRU-NOMS-BDAMATCH
                DESCENDING LNG-NOM-BDAMATCH
                ASCENDING  CTA-NOM-BDAMATCH .
           MOVE 1 TO I-CTA-BDAMATCH.
           MOVE 1 TO COL-FUE-BDAMATCH
           MOVE 999                TO CTA-NOM-ANT-BDAMATCH
           MOVE ALL "9"            TO GRU-NOM-ANT-BDAMATCH
           PERFORM UNTIL I-CTA-BDAMATCH > LIM-BDAMATCH
                   OR CTA-NOM-BDAMATCH (I-CTA-BDAMATCH) = 0
                   OR FIL-FUE-BDAMATCH > LIM-BDAMATCH
                   OR COL-FUE-BDAMATCH > LIM-BDAMATCH
             IF GRU-NOM-ANT-BDAMATCH <>
                GRU-NOMS-BDAMATCH(I-CTA-BDAMATCH)
                ADD  1 TO FIL-FUE-BDAMATCH
                MOVE 1 TO COL-FUE-BDAMATCH
                MOVE CTA-NOM-BDAMATCH(I-CTA-BDAMATCH) TO
                     CAJA-BDAMATCH(FIL-FUE-BDAMATCH,COL-FUE-BDAMATCH)
                MOVE "NOM" TO CARAC-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                ADD 1 TO COL-FUE-BDAMATCH
             ELSE
                IF CTA-NOM-ANT-BDAMATCH <>
                   CTA-NOM-BDAMATCH(I-CTA-BDAMATCH)
                   MOVE CTA-NOM-BDAMATCH(I-CTA-BDAMATCH) TO
                        CAJA-BDAMATCH(FIL-FUE-BDAMATCH,COL-FUE-BDAMATCH)
                   MOVE "NOM" TO CARAC-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                   ADD 1 TO COL-FUE-BDAMATCH
                END-IF
             END-IF
             MOVE CTA-NOM-BDAMATCH(I-CTA-BDAMATCH) TO
                  CTA-NOM-ANT-BDAMATCH
             MOVE GRU-NOMS-BDAMATCH(I-CTA-BDAMATCH) TO
                  GRU-NOM-ANT-BDAMATCH
             ADD 1 TO I-CTA-BDAMATCH
           END-PERFORM.
           MOVE FIL-FUE-BDAMATCH TO MAX-REL-BDAMATCH.

       4000-U-INTERELACIONAR-BDAMATCH.
REMARK******************************************************************
REMARK*4000-U-INTERELACIONAR-BDAMATCH.
REMARK* INPUT = TABLA DE RELACIONES LLENA CON NOMBRES,DIRECCIONES,TELEFO
REMARK* OUTPUT= TABLA DE RELACIONES CON LOS PORTAFOLIOS DETERMINADOS
REMARK* SE REALIZAN  TABLA DE NOMBRES   EN SUBPORTAFOLIOS QUE SON
REMARK* INSERTADOS EN LA TABLA DE RELACIONAMIENTO
REMARK******************************************************************
PESO01*    PERFORM 1110-MOSTRAR-MATRIZ1
           MOVE 1 TO MOVIDO-BDAMATCH    .
           PERFORM UNTIL MOVIDO-BDAMATCH = 0
              MOVE 0 TO MOVIDO-BDAMATCH
              MOVE 1 TO FIL-BUS-BDAMATCH
              PERFORM UNTIL FIL-BUS-BDAMATCH > MAX-REL-BDAMATCH
                MOVE 1 TO COL-BUS-BDAMATCH
                PERFORM UNTIL COL-BUS-BDAMATCH > MAX-CUENTAS-BDAMATCH
                  IF CAJA-BDAMATCH(FIL-BUS-BDAMATCH,COL-BUS-BDAMATCH)
                     > 0
                  MOVE CAJA-BDAMATCH(FIL-BUS-BDAMATCH,COL-BUS-BDAMATCH)
                       TO BUSCADO-BDAMATCH
                     PERFORM 4100-BUSCAR-RELACION-BDAMATCH
                  ELSE
                     MOVE MAX-CUENTAS-BDAMATCH TO COL-BUS-BDAMATCH
                  END-IF
                  ADD 1 TO COL-BUS-BDAMATCH
                END-PERFORM
                ADD 1 TO FIL-BUS-BDAMATCH
              END-PERFORM
           END-PERFORM.

       4100-BUSCAR-RELACION-BDAMATCH.
REMARK******************************************************************
REMARK*4100-BUSCAR-RELACION-BDAMATCH.
REMARK* SE BUSCA LA RELACION DE UNA CUENTA CON LAS CUENTAS DE UNA FILA
REMARK******************************************************************
           MOVE 1 TO FIL-FUE-BDAMATCH.
           PERFORM UNTIL FIL-FUE-BDAMATCH > MAX-REL-BDAMATCH
              IF FIL-FUE-BDAMATCH <> FIL-BUS-BDAMATCH
                 MOVE 0 TO EXISTE-BDAMATCH
                 INSPECT FILA-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                    TALLYING EXISTE-BDAMATCH
                    FOR CHARACTERS BEFORE INITIAL BUSCADO-D-BDAMATCH
                  ADD 1 TO EXISTE-BDAMATCH
                  IF EXISTE-BDAMATCH <= 2000
                      PERFORM 4110-MOVER-RELACIONES-BDAMATCH
                 END-IF
              END-IF
              ADD 1 TO FIL-FUE-BDAMATCH
           END-PERFORM.

       4110-MOVER-RELACIONES-BDAMATCH.
REMARK******************************************************************
REMARK*4110-MOVER-RELACIONES-BDAMATCH.
REMARK* ENCONTRADA LA RELACION SE PROCEDE A MOVER TODAS LAS RELACIONES
REMARK* QUe NO EXISTAN A LA FILA FUENTE DE LA BUSQUEDA
REMARK******************************************************************
             MOVE 0 TO FIN-FUENTE-BDAMATCH.
             INSPECT FILA-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                     TALLYING FIN-FUENTE-BDAMATCH
                     FOR CHARACTERS BEFORE INITIAL "000 "
             COMPUTE FIN-FUENTE-BDAMATCH = FIN-FUENTE-BDAMATCH / 4
             MOVE 0 TO LIBRE-BUSCADO-BDAMATCH.
             INSPECT FILA-REL-BDAMATCH(FIL-BUS-BDAMATCH)
                     TALLYING LIBRE-BUSCADO-BDAMATCH
                     FOR CHARACTERS BEFORE INITIAL "000 "
             COMPUTE LIBRE-BUSCADO-BDAMATCH = LIBRE-BUSCADO-BDAMATCH / 4
             ADD 1 TO LIBRE-BUSCADO-BDAMATCH.
             MOVE 1 TO COL-FUE-BDAMATCH.
             PERFORM UNTIL COL-FUE-BDAMATCH > FIN-FUENTE-BDAMATCH
                MOVE CAJA-BDAMATCH(FIL-FUE-BDAMATCH,COL-FUE-BDAMATCH) TO
                               PORINSERTAR-BDAMATCH
                MOVE 0 TO EXISTE-BDAMATCH
                INSPECT FILA-REL-BDAMATCH(FIL-BUS-BDAMATCH)
                     TALLYING EXISTE-BDAMATCH
                    FOR CHARACTERS BEFORE INITIAL PORINSERTAR-D-BDAMATCH
                ADD 1 TO EXISTE-BDAMATCH
                IF EXISTE-BDAMATCH > 2000
                  MOVE PORINSERTAR-BDAMATCH TO
                  CAJA-BDAMATCH(FIL-BUS-BDAMATCH,LIBRE-BUSCADO-BDAMATCH)
                  ADD 1 TO LIBRE-BUSCADO-BDAMATCH
                  MOVE 1 TO MOVIDO-BDAMATCH
                END-IF
                ADD 1 TO COL-FUE-BDAMATCH
             END-PERFORM.
             INITIALIZE
                  FILA-REL-BDAMATCH(FIL-FUE-BDAMATCH). *> SE BORRA LO TR

       5000-U-MEJOR-PORTAFOL-BDAMATCH.
REMARK******************************************************************
REMARK*5000-U-MEJOR-PORTAFOL-BDAMATCH.
REMARK* SE DETERMINA LA FILA QUE DA LAS MEJORES CARACTERISTICAS PARA EL
REMARK* PORTAFOLIO QUE TIENE MAS CUENTAS
REMARK******************************************************************
            MOVE 0 TO MEJOR-PORTAFOLIO-BDAMATCH.
            MOVE 0 TO MAXREL-PER-BDAMATCH
            MOVE 0 TO FIN-FUENTE-BDAMATCH.
            MOVE 1 TO FIL-FUE-BDAMATCH.
            MOVE 0 TO  PESOS-CTRL-BDAMATCH
            PERFORM UNTIL FIL-FUE-BDAMATCH > MAX-REL-BDAMATCH
              INSPECT FILA-REL-BDAMATCH(FIL-FUE-BDAMATCH)
                     TALLYING FIN-FUENTE-BDAMATCH
                     FOR CHARACTERS BEFORE INITIAL "000 "
              COMPUTE FIN-FUENTE-BDAMATCH = FIN-FUENTE-BDAMATCH    / 4
              MOVE 0 TO PESO-CTAS-TOT-BDAMATCH
              MOVE 1 TO POSIC-SUMA-BDAMATCH
PESO01        MOVE ALL "9" TO TABLA-SUSC-DIR-BDAMATCH
PESO01        MOVE 0 TO  POSIC-SUSC-BDAMATCH
PESO01        MOVE 0 TO ENT-FIN-PORT-BDAMATCH
PESO01        MOVE 0 TO ENT-UNICAS-PORT-BDAMATCH
              PERFORM  VARYING  NRO-VECES-1-BDAMATCH  FROM  1  BY  1
                 UNTIL  NRO-VECES-1-BDAMATCH  >  FIN-FUENTE-BDAMATCH
                 MOVE
              FILA-REL-BDAMATCH(FIL-FUE-BDAMATCH)(POSIC-SUMA-BDAMATCH:3)
                      TO  CTA-A-BUSCAR-BDAMATCH
PESO01           IF CTA-A-BUSCAR-BDAMATCH > 0
PESO01            MOVE CTA-CUENTAS-BDAMATCH(CTA-A-BUSCAR-BDAMATCH)(13:6)
PESO01                 TO SUSC-CTAS-BDAMATCH
AJUS01            MOVE CTA-CUENTAS-BDAMATCH(CTA-A-BUSCAR-BDAMATCH)(19:2)
AJUS01                 TO I2-BDAMATCH
PESO01            PERFORM 5100-CALCULAR-PESO-DIR-BDAMATCH
PESO01            PERFORM 5200-CALCULAR-PESO-TEL-BDAMATCH
PESO01            PERFORM 5300-CALCULAR-PESO-CTA-BDAMATCH
PESO01           END-IF
                 ADD 4 TO  POSIC-SUMA-BDAMATCH
              END-PERFORM
PESO01        PERFORM 5400-DEDUCIR-MEJ-PORT-BDAMATCH
              ADD 1 TO FIL-FUE-BDAMATCH
            END-PERFORM.
PESO01 5100-CALCULAR-PESO-DIR-BDAMATCH.
PESO01     PERFORM  VARYING  NRO-VECES-2-BDAMATCH  FROM  1  BY  1
PESO01        UNTIL  NRO-VECES-2-BDAMATCH  >  LIM-BDAMATCH OR
PESO01        CTA-DIR-BDAMATCH(NRO-VECES-2-BDAMATCH) =  ZEROS
PESO01        IF CTA-DIR-BDAMATCH(NRO-VECES-2-BDAMATCH)
PESO01                                     =  CTA-A-BUSCAR-BDAMATCH
PESO01          COMPUTE PESO-CTAS-TOT-BDAMATCH  =
PESO01                   PESO-CTAS-TOT-BDAMATCH +
PESO01          (NUM-CTAS-DIR-BDAMATCH(NRO-VECES-2-BDAMATCH) *
PESO01                  PESO-DIR-BDAMATCH )
PESO01        END-IF
PESO01     END-PERFORM.
PESO01 5200-CALCULAR-PESO-TEL-BDAMATCH.
PESO01     PERFORM VARYING NRO-VECES-2-BDAMATCH FROM 1 BY 1
PESO01        UNTIL NRO-VECES-2-BDAMATCH > LIM-BDAMATCH OR
PESO01        CTA-TEL-BDAMATCH(NRO-VECES-2-BDAMATCH) = ZEROS
PESO01        IF  CTA-TEL-BDAMATCH(NRO-VECES-2-BDAMATCH)
PESO01                                      =  CTA-A-BUSCAR-BDAMATCH
PESO01          COMPUTE  PESO-CTAS-TOT-BDAMATCH  =
PESO01             PESO-CTAS-TOT-BDAMATCH +
PESO01           (NUM-CTAS-TEL-BDAMATCH(NRO-VECES-2-BDAMATCH) *
PESO01             PESO-TEL-BDAMATCH )
PESO01        END-IF
PESO01     END-PERFORM.
PESO01 5300-CALCULAR-PESO-CTA-BDAMATCH.
REMARK******************************************************************
PESO01*5300-CALCULAR-PESO-CTA-BDAMATCH.
REMARK* SE BUSCA EN TABLA DE MEMORIA DE SUSCRIPTORES Y SE CUENTAN SOLO
REMARK* LAS ENTIDADES UNICAS Y LAS FINANCIERAS
REMARK******************************************************************
PESO01     SET IND-DIR-BDAMATCH TO 1.
PESO01     SEARCH ALL REG-SUSC-DIR-BDAMATCH AT END
PESO01         MOVE 1 TO  INV-BDAMATCH
PESO01       WHEN COD-SUSC-DIR-BDAMATCH(IND-DIR-BDAMATCH) =
PESO01            SUSC-CTAS-BDAMATCH
PESO01         MOVE 0 TO  INV-BDAMATCH
PESO01     END-SEARCH.
PESO01     IF INV-BDAMATCH  =  1
PESO01        AND POSIC-SUSC-BDAMATCH  <  LIM-BDAMATCH
PESO01        COMPUTE PESO-CTAS-TOT-BDAMATCH = PESO-CTAS-TOT-BDAMATCH
PESO01                                       + PESO-NOM-BDAMATCH
PESO01        ADD   1                TO  POSIC-SUSC-BDAMATCH
PESO01        MOVE SUSC-CTAS-BDAMATCH  TO
PESO01             COD-SUSC-DIR-BDAMATCH(POSIC-SUSC-BDAMATCH)
AJUS01*       MOVE  SUSC-CTAS-BDAMATCH(1:2)  TO I2-BDAMATCH
AJUS01        IF I2-BDAMATCH > 0 AND I2-BDAMATCH <= 100
AJUS01           IF V-TIPCTA-BDAMATCH(I2-BDAMATCH) = 1 OR 2
AJUS01              ADD 1 TO ENT-FIN-PORT-BDAMATCH
AJUS01           END-IF
AJUS01        END-IF
PESO01        ADD 1 TO ENT-UNICAS-PORT-BDAMATCH
PESO01     END-IF.
PESO01     IF FUEEXT-CUENTAS-BDAMATCH(CTA-A-BUSCAR-BDAMATCH) = 1
PESO01        ADD 1000 TO PESO-CTAS-TOT-BDAMATCH
PESO01     END-IF.
PESO01 5400-DEDUCIR-MEJ-PORT-BDAMATCH.
PESO01     IF FIN-FUENTE-BDAMATCH > MAXREL-PER-BDAMATCH
PESO01        MOVE  FIN-FUENTE-BDAMATCH  TO  MAXREL-PER-BDAMATCH
PESO01     END-IF
PESO01     IF PESO-CTAS-TOT-BDAMATCH  >  PESOS-CTRL-BDAMATCH
PESO01        MOVE  PESO-CTAS-TOT-BDAMATCH TO PESOS-CTRL-BDAMATCH
PESO01        MOVE  FIL-FUE-BDAMATCH TO MEJOR-PORTAFOLIO-BDAMATCH
PESO01        MOVE ENT-FIN-PORT-BDAMATCH TO ENT-FIN-MEJ-PORT-BDAMATCH
PESO01     ELSE
PESO01        IF PESO-CTAS-TOT-BDAMATCH  =  PESOS-CTRL-BDAMATCH
MEJO01           PERFORM 5410-DESEMPATAR-BDAMATCH
PESO01        END-IF
PESO01     END-IF.
MEJO01 5410-DESEMPATAR-BDAMATCH.
MEJO01      IF ENT-FIN-PORT-BDAMATCH > ENT-FIN-MEJ-PORT-BDAMATCH
MEJO01         MOVE PESO-CTAS-TOT-BDAMATCH TO PESOS-CTRL-BDAMATCH
MEJO01         MOVE FIL-FUE-BDAMATCH TO MEJOR-PORTAFOLIO-BDAMATCH
MEJO01         MOVE ENT-FIN-PORT-BDAMATCH TO ENT-FIN-MEJ-PORT-BDAMATCH
MEJO01      ELSE
MEJO01         IF ENT-FIN-PORT-BDAMATCH = ENT-FIN-MEJ-PORT-BDAMATCH
MEJO01            IF APE-GRUNOM-BDAMATCH(FIL-FUE-BDAMATCH) = 1 AND
MEJO01               NOM-GRUNOM-BDAMATCH(FIL-FUE-BDAMATCH)(43:3) <>
MEJO01                                                             ".-,"
MEJO01               MOVE  PESO-CTAS-TOT-BDAMATCH TO PESOS-CTRL-BDAMATCH
MEJO01               MOVE  FIL-FUE-BDAMATCH TO MEJOR-PORTAFOLIO-BDAMATCH
MEJO01               MOVE ENT-FIN-PORT-BDAMATCH TO
MEJO01                                         ENT-FIN-MEJ-PORT-BDAMATCH
MEJO01            ELSE
MEJO01              IF LNG-GRUNOM-BDAMATCH(FIL-FUE-BDAMATCH) >
MEJO01                 LNG-GRUNOM-BDAMATCH(MEJOR-PORTAFOLIO-BDAMATCH)
MEJO01               MOVE  PESO-CTAS-TOT-BDAMATCH TO PESOS-CTRL-BDAMATCH
MEJO01               MOVE  FIL-FUE-BDAMATCH TO MEJOR-PORTAFOLIO-BDAMATCH
MEJO01               MOVE ENT-FIN-PORT-BDAMATCH TO
MEJO01                                         ENT-FIN-MEJ-PORT-BDAMATCH
MEJO01            END-IF
MEJO01         END-IF
MEJO01      END-IF.
       5000-U-VALIDAR-CUENTA-BDAMATCH.
REMARK******************************************************************
REMARK*5000-U-VALIDAR-CUENTA-BDAMATCH.
REMARK* SE VALIDA SI LA LLAVE DE LA CUENTA "CTA-INPUT-BDAMATCH" ESTA DEN
REMARK* EL MEJOR PORTAFOLIO UBICADO. RETORNA VALCTA-OUTPUT-BDAMATCH
REMARK******************************************************************
           MOVE 0 TO VALCTA-OUTPUT-BDAMATCH
           MOVE 1 TO I-BDAMATCH.
           IF MEJOR-PORTAFOLIO-BDAMATCH > 0
              PERFORM UNTIL I-BDAMATCH > LIM-BDAMATCH OR
                VALCTA-OUTPUT-BDAMATCH = 1 OR
                CAJA-BDAMATCH(MEJOR-PORTAFOLIO-BDAMATCH,I-BDAMATCH) = 0
                MOVE CAJA-BDAMATCH(MEJOR-PORTAFOLIO-BDAMATCH,I-BDAMATCH)
                     TO J-BDAMATCH
                IF CTA-CUENTAS-BDAMATCH(J-BDAMATCH) = CTA-INPUT-BDAMATCH
                   MOVE 1 TO VALCTA-OUTPUT-BDAMATCH
                END-IF
                ADD 1 TO I-BDAMATCH
              END-PERFORM
           END-IF.
      *
       6000-U-MEJOR-DIR-BDAMATCH.
REMARK******************************************************************
REMARK*6000-U-MEJOR-DIR-BDAMATCH.
REMARK*   SALIDA DIRECCION-OUTPUT-BDAMATCH .......... MEJOR DIRECCION
REMARK******************************************************************
           SORT T-DIR-BDAMATCH
                DESCENDING  NUM-CTAS-DIR-BDAMATCH
                DESCENDING       LNG-DIR-BDAMATCH
                DESCENDING       NUM-DIR-BDAMATCH
                DESCENDING       CTA-DIR-BDAMATCH.
          MOVE  NUM-DIR-BDAMATCH(01) TO  DIR-OUTPUT-BDAMATCH.

       6000-U-MEJOR-TEL-BDAMATCH.
REMARK******************************************************************
REMARK*6000-U-MEJOR-TEL-BDAMATCH.
REMARK*   SALIDA TELEFONO-OUTPUT-BDAMATCH .......... MEJOR TELEFONO
REMARK******************************************************************
           SORT T-TEL-BDAMATCH
                DESCENDING  NUM-CTAS-TEL-BDAMATCH
                DESCENDING       NUM-TEL-BDAMATCH
                DESCENDING       CTA-TEL-BDAMATCH.
          MOVE  NUM-TEL-BDAMATCH(01) TO  TEL-OUTPUT-BDAMATCH.
      *
       6000-U-MEJOR-NOM-BDAMATCH.
REMARK******************************************************************
REMARK*6000-U-MEJOR-NOM-BDAMATCH.
REMARK*   SALIDA NOMBRE-OUTPUT-BDAMATCH .......... MEJOR NOMBRE
REMARK******************************************************************
           SORT T-GRUNOM-BDAMATCH
                DESCENDING  NUM-CTAS-GRUNOM-BDAMATCH
                DESCENDING  LNG-GRUNOM-BDAMATCH
                DESCENDING  NOM-GRUNOM-BDAMATCH.
           MOVE  ZEROS TO  ESTA-CTA-BDAMATCH.
           PERFORM VARYING NRO-VECES-3-BDAMATCH  FROM  1 BY  1
             UNTIL NRO-VECES-3-BDAMATCH  >  LIM-BDAMATCH
                OR GRUPO-GRUNOM-BDAMATCH(NRO-VECES-3-BDAMATCH)  =  ZEROS
                OR ESTA-CTA-BDAMATCH  =  1
            PERFORM VARYING NRO-VECES-2-BDAMATCH  FROM  1 BY  1
             UNTIL NRO-VECES-2-BDAMATCH  >  LIM-BDAMATCH
                OR CTA-NOM-BDAMATCH(NRO-VECES-2-BDAMATCH)  =  ZEROS
                OR ESTA-CTA-BDAMATCH  =  1
              IF  GRUPO-GRUNOM-BDAMATCH(NRO-VECES-3-BDAMATCH)  =
                  GRU-NOMS-BDAMATCH (NRO-VECES-2-BDAMATCH)
               PERFORM VARYING NRO-VECES-1-BDAMATCH  FROM  1 BY  1
                UNTIL NRO-VECES-1-BDAMATCH  >  LIM-BDAMATCH
                 OR CAJA-BDAMATCH(MEJOR-PORTAFOLIO-BDAMATCH,
                    NRO-VECES-1-BDAMATCH) = 0
                 OR ESTA-CTA-BDAMATCH  =  1
                 IF  CTA-NOM-BDAMATCH(NRO-VECES-2-BDAMATCH)  =
                     CAJA-BDAMATCH(MEJOR-PORTAFOLIO-BDAMATCH,
                                   NRO-VECES-1-BDAMATCH)
                   MOVE GRU-NOMS-BDAMATCH(NRO-VECES-2-BDAMATCH) TO
                        POSIC-SUS-GRU-BDAMATCH
                   MOVE NOM-GRUNOM-BDAMATCH(NRO-VECES-3-BDAMATCH) TO
                        NOM-OUTPUT-BDAMATCH
                   MOVE 1 TO  ESTA-CTA-BDAMATCH
                 END-IF
               END-PERFORM
              END-IF
            END-PERFORM
           END-PERFORM.
      *
       9000-U-REALIZAR-BDAMATCH.
REMARK******************************************************************
REMARK*9000-U-REALIZAR-BDAMATCH.
REMARK* HACE EL LLAMADO DE LAS RUTINAS PARA ENCONTRAR DINALMENTE EL
REMARK* MEJOR NOMBRE
REMARK******************************************************************
           MOVE  ZEROS                 TO  POSIC-SUSC-BDAMATCH
           MOVE  ALL "9"               TO  TABLA-SUSC-DIR-BDAMATCH
           PERFORM 3000-U-ORDENAR-DIR-BDAMATCH
           PERFORM 3000-U-ORDENAR-TEL-BDAMATCH
           PERFORM 3000-U-ORDENAR-NOM-BDAMATCH
           PERFORM 4000-U-INTERELACIONAR-BDAMATCH
           PERFORM 6000-U-MEJOR-DIR-BDAMATCH.
           PERFORM 6000-U-MEJOR-TEL-BDAMATCH.
           PERFORM 5000-U-MEJOR-PORTAFOL-BDAMATCH.
           PERFORM 6000-U-MEJOR-NOM-BDAMATCH.
      *
       9010-U-VER-FALLECIDO-BDAMATCH.
REMARK******************************************************************
REMARK*9010-U-VER-FALLECIDO-BDAMATCH.
REMARK* SE EXAMINAN TODAS LAS CUENTAS DEL MEJOR PORTAFOLIO, SE CUENTAN E
REMARK* NUMERO TOTAL DE CUENTAS Y DE CUENTAS CON FALLECIDO.
REMARK******************************************************************
VIGE01     MOVE 1 TO I-BDAMATCH.
VIGE01     IF MEJOR-PORTAFOLIO-BDAMATCH > 0
VIGE01        PERFORM UNTIL I-BDAMATCH > LIM-BDAMATCH OR
VIGE01          VALCTA-OUTPUT-BDAMATCH = 1 OR
VIGE01          CAJA-BDAMATCH(MEJOR-PORTAFOLIO-BDAMATCH,I-BDAMATCH) = 0
VIGE01          MOVE CAJA-BDAMATCH(MEJOR-PORTAFOLIO-BDAMATCH,I-BDAMATCH)
VIGE01               TO J-BDAMATCH
VIGE01          ADD 1 TO NUM-CTAS-OUTPUT-BDAMATCH
VIGE01          IF CTA-FALLECIDO-BDAMATCH(J-BDAMATCH) = 1
VIGE01             ADD 1 TO NUM-MUERTOS-OUTPUT-BDAMATCH
VIGE01          END-IF
VIGE01          ADD 1 TO I-BDAMATCH
VIGE01        END-PERFORM
VIGE01     END-IF.
VIGE01*
       400-BUSCAR-SUSC-GRUPO-BDAMATCH.
REMARK******************************************************************
REMARK*400-BUSCAR-SUSC-GRUPO-BDAMATCH.
REMARK* BUSCA EN TABLA DE MEMORIA DE SUSCRIPTORES Y GRUPOS.
REMARK******************************************************************
           SET IND-NOM-BDAMATCH TO 1.
           SEARCH ALL REG-SUSC-NOM-BDAMATCH AT END
               MOVE 1 TO  INV-BDAMATCH
               WHEN KEY-SUSC-NOM-BDAMATCH(IND-NOM-BDAMATCH) =
                    SUSC-GRUPO-BDAMATCH
               MOVE 0 TO  INV-BDAMATCH
           END-SEARCH.
           IF  INV-BDAMATCH = 1
           AND POSIC-SUS-GRU-BDAMATCH <  LIM-BDAMATCH
             ADD 1 TO  POSIC-SUS-GRU-BDAMATCH
             MOVE SUSC-GRUP-SUS-BDAMATCH TO
                  COD-SUSC-NOM-BDAMATCH(POSIC-SUS-GRU-BDAMATCH)
             MOVE SUSC-GRUP-GRU-BDAMATCH TO
                  GRUPO-SUS-NOM-BDAMATCH(POSIC-SUS-GRU-BDAMATCH)
           END-IF.
icjlud@codes /d/iccol/desarrollo/copys
$
