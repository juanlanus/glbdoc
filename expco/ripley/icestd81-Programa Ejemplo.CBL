      *icestd81.cbl.v.01.0002
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    icestd81-DATACREDITO.
       AUTHOR.        DATACREDITO-JAGR.
       DATE-WRITTEN.  OCT/2006.
       DATE-COMPILED.
      ******************************************************************
      * GDT0001    | LEY001 - PROCESO DE AMNISTIA SEGUN ARTICULO 0021  *
      * ENE.23/2008|    QUE INCLUYE EL VECTOR DE 48 POSICIONES.        *
      * GER.DESARR.|                                                   *
      ******************************************************************
      ******************************************************************
      * GDT0002    | CTAST8 - CAMBIO DEL ICMCHK (MAESTRO DE CUENTAS) A *
      * FEB.09-2010|    FORMATO TIPO 8.                                *
      * GER.DESARR.|                                                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
      ****************************************************************
      *
      *  GENERA STANDARD DE MAYOR INFORMACION DISPONIBLE  GENERAL
      *
      *  -SE CONTROLA FNA PARA QUE SEA CARTERA DE VIVIENDA SEGUN PLAZO
      *
      *  -SE CONTROLA QUE AVANTEL ENTRE A TELCOS  Y NO A REAL
      *
      *   sh-datainforme
      *
      *  VERSION ACTUAL  DIC/2006   07/12/2006 04:37:06 p.m.
      ****************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
LEY001     COPY ICBDIR.FS.                                              GDT0001
LEY001     COPY BDACSV.FS.                                              GDT0001
LEY001     COPY NOAMNISTIA21.FS.                                        GDT0001
LEY001*                                                                 GDT0001
           COPY ICBSUS.FS.
           COPY ICBCON.FS.
CTAST8*    COPY  ICBCHK.FS.                                             GDT0002
CTAST8     COPY  ICBCHK0001.FS.                                         GDT0002
           COPY ICBCRE.FS.
           COPY BDIEMSUP0001.FS.
           COPY BDIEENTI.FS.
           COPY BDACUE.FS.
           SELECT ENTRADA  ASSIGN TO DISK ARCHIVO-ENTRADA
                  ORGANIZATION   IS LINE SEQUENTIAL
                  ACCESS MODE    IS SEQUENTIAL
                  FILE STATUS    IS W-STATUS-ENT.

           SELECT INCONSIS ASSIGN TO DISK ARCHIVO-INCONC
                  ORGANIZATION   IS LINE SEQUENTIAL
                  ACCESS MODE    IS SEQUENTIAL
                  FILE STATUS    IS W-STATUS-INC.

           SELECT SALIDA   ASSIGN  TO DISK ARCHIVO-SALIDA
                  ORGANIZATION   IS LINE SEQUENTIAL
                  ACCESS MODE    IS SEQUENTIAL.

           SELECT ESTADIS  ASSIGN  TO DISK ARCHIVO-ESTADIS
                  ORGANIZATION   IS LINE SEQUENTIAL
                  ACCESS MODE    IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
LEY001 COPY ICBDIR0001.FD.                                              GDT0001
LEY001 COPY BDACSV.FD.                                                  GDT0001
LEY001 COPY NOAMNISTIA21.FD.                                            GDT0001
LEY001*                                                                 GDT0001
       FD  ENTRADA.
       01  REG-ENTRADA.
           03 ENT-TIPO-ID               PIC 9(01).
           03 ENT-NUME-ID               PIC 9(11).
           03 ENT-NOMBRE                PIC X(45).
           03 ENT-FEC-EXPED             PIC 9(08).
           03 ENT-CIUDAD-EXP            PIC X(15).
           03 ENT-DEP-EXP-VAL           PIC X(15).
           03 ENT-VIG-VAL               PIC X(02).
           03 ENT-GENERO-VAL            PIC X(01).
           03 ENT-EDAD                  PIC X(03).
           03 ENT-EDAD-RANGO            PIC X(05).
           03 FILLER                    PIC X(51).

       FD  INCONSIS.
       01  REG-INCONSIS.
           03 TIP-INC                   PIC 9.
           03 CED-INC                   PIC 9(11).
           03 NOM1-INC                  PIC X(45).
           03 NOM2-INC                  PIC X(45).
           03 FEC-EXP-INC               PIC 9(08).
           03 MUN-EXP-INC               PIC X(15).
           03 DEP-EXP-INC               PIC X(15).
           03 VIG-INC                   PIC X(02).
           03 CODERR-INC                PIC 9(02).
           03 FILLER-2-INC              PIC X(60).


       FD  SALIDA.
       01  REG-SALIDA.
           03 SLD-TIPO-ID                    PIC 9(01).
           03 FILLER01                       PIC X(01) VALUE ";".
           03 SLD-NUME-ID                    PIC 9(11).
           03 FILLER02                       PIC X(01) VALUE ";".
           03 SLD-NOMBRE                     PIC X(45).
           03 FILLER03                       PIC X(01) VALUE ";".
           03 SLD-RANGO-EDAD                 PIC X(05).
           03 FILLER04                       PIC X(01) VALUE ";".
           03 SLD-GENERO                     PIC X(01).
           03 FILLER05                       PIC X(01) VALUE ";".
           03 SLD-CIUDAD-EXPED               PIC X(15).
           03 FILLER06                       PIC X(01) VALUE ";".
           03 SLD-CREDITOS-ACTV              PIC 9(04).
           03 FILLER07                       PIC X(01) VALUE ";".
           03 SLD-NUM-OBLIG-CAB              PIC 9(04).
           03 FILLER08                       PIC X(01) VALUE ";".
           03 SLD-TOT-INICIAL-CAB            PIC 9(15).
           03 FILLER09                       PIC X(01) VALUE ";".
           03 SLD-TOT-SALDOS-CAB             PIC 9(15).
           03 FILLER10                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-CAB             PIC 9(11).
           03 FILLER11                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-CAB               PIC 9(11).
           03 FILLER12                       PIC X(01) VALUE ";".
           03 SLD-NUM-OBLIG-CAV              PIC 9(04).
           03 FILLER13                       PIC X(01) VALUE ";".
           03 SLD-TOT-INICIAL-CAV            PIC 9(15).
           03 FILLER14                       PIC X(01) VALUE ";".
           03 SLD-TOT-SALDOS-CAV             PIC 9(15).
           03 FILLER15                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-CAV             PIC 9(11).
           03 FILLER16                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-CAV               PIC 9(11).
           03 FILLER17                       PIC X(01) VALUE ";".
           03 SLD-NUM-OBLIG-OTR              PIC 9(04).
           03 FILLER18                       PIC X(01) VALUE ";".
           03 SLD-TOT-INICIAL-OTR            PIC 9(15).
           03 FILLER19                       PIC X(01) VALUE ";".
           03 SLD-TOT-SALDOS-OTR             PIC 9(15).
           03 FILLER20                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-OTR             PIC 9(11).
           03 FILLER21                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-OTR               PIC 9(11).
           03 FILLER22                       PIC X(01) VALUE ";".
           03 SLD-TOT-NUM-TDC                PIC 9(04).
           03 FILLER23                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUPOS-TDC              PIC 9(15).
           03 FILLER24                       PIC X(01) VALUE ";".
           03 SLD-TOT-SALDO-TDC              PIC 9(15).
           03 FILLER25                       PIC X(01) VALUE ";".
           03 SLD-POR-UTILIZ-TDC             PIC Z.ZZZZZ.
           03 FILLER26                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-TDC             PIC 9(11).
           03 FILLER27                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-TDC               PIC 9(11).
           03 FILLER28                       PIC X(01) VALUE ";".
           03 SLD-NRO-RANGO-0                PIC 9(03).
           03 FILLER29                       PIC X(01) VALUE ";".
           03 SLD-NRO-RANGO-1                PIC 9(03).
           03 FILLER30                       PIC X(01) VALUE ";".
           03 SLD-NRO-RANGO-2                PIC 9(03).
           03 FILLER31                       PIC X(01) VALUE ";".
           03 SLD-NRO-RANGO-3                PIC 9(03).
           03 FILLER32                       PIC X(01) VALUE ";".
           03 SLD-NRO-RANGO-4                PIC 9(03).
           03 FILLER33                       PIC X(01) VALUE ";".
           03 SLD-NRO-RANGO-5                PIC 9(03).
           03 FILLER34                       PIC X(01) VALUE ";".
           03 SLD-NRO-RANGO-6                PIC 9(03).
           03 FILLER35                       PIC X(01) VALUE ";".
           03 SLD-FECHA-TDC                  PIC X(06).
           03 FILLER36                       PIC X(01) VALUE ";".
           03 SLD-NUM-OBLIG-REAL             PIC 9(04).
           03 FILLER37                       PIC X(01) VALUE ";".
           03 SLD-TOT-INICIAL-REAL           PIC 9(15).
           03 FILLER38                       PIC X(01) VALUE ";".
           03 SLD-TOT-SALDOS-REAL            PIC 9(15).
           03 FILLER39                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-REAL            PIC 9(11).
           03 FILLER40                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-REAL              PIC 9(11).
           03 FILLER41                       PIC X(01) VALUE ";".
           03 SLD-NUM-OBLIG-TELE             PIC 9(04).
           03 FILLER42                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-TELE            PIC 9(11).
           03 FILLER43                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-TELE              PIC 9(11).
           03 FILLER44                       PIC X(01) VALUE ";".
           03 SLD-NUM-OBLIG-COOP             PIC 9(04).
           03 FILLER45                       PIC X(01) VALUE ";".
           03 SLD-TOT-INICIAL-COOP           PIC 9(15).
           03 FILLER46                       PIC X(01) VALUE ";".
           03 SLD-TOT-SALDOS-COOP            PIC 9(15).
           03 FILLER47                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-COOP            PIC 9(11).
           03 FILLER48                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-COOP              PIC 9(11).
           03 FILLER49                       PIC X(01) VALUE ";".
           03 SLD-NUM-OBLIG-CODEU            PIC 9(04).
           03 FILLER50                       PIC X(01) VALUE ";".
           03 SLD-TOT-SALDOS-CODEU           PIC 9(15).
           03 FILLER51                       PIC X(01) VALUE ";".
           03 SLD-TOT-CUOTAS-CODEU           PIC 9(11).
           03 FILLER52                       PIC X(01) VALUE ";".
           03 SLD-TOT-MORA-CODEU             PIC 9(11).
           03 FILLER53                       PIC X(01) VALUE ";".
           03 SLD-CREDITOS-ALDIA             PIC 9(03).
           03 FILLER54                       PIC X(01) VALUE ";".
           03 SLD-MORA-30                    PIC 9(03).
           03 FILLER55                       PIC X(01) VALUE ";".
           03 SLD-MORA-60                    PIC 9(03).
           03 FILLER56                       PIC X(01) VALUE ";".
           03 SLD-MORA-90                    PIC 9(03).
           03 FILLER57                       PIC X(01) VALUE ";".
           03 SLD-MORA-120                   PIC 9(03).
           03 FILLER58                       PIC X(01) VALUE ";".
           03 SLD-CAR-CASTIGA                PIC 9(03).
           03 FILLER59                       PIC X(01) VALUE ";".
           03 SLD-DUD-RECAUDO                PIC 9(03).
           03 FILLER60                       PIC X(01) VALUE ";".
           03 SLD-CUENTAS-COBRADOR           PIC 9(03).
           03 FILLER61                       PIC X(01) VALUE ";".
           03 SLD-MORA-30-HIS                PIC 9(03).
           03 FILLER62                       PIC X(01) VALUE ";".
           03 SLD-MORA-60-HIS                PIC 9(03).
           03 FILLER63                       PIC X(01) VALUE ";".
           03 SLD-MORA-90-HIS                PIC 9(03).
           03 FILLER64                       PIC X(01) VALUE ";".
           03 SLD-MORA-120-HIS               PIC 9(03).
           03 FILLER65                       PIC X(01) VALUE ";".
           03 SLD-CANC-MMANEJO               PIC 9(03).
           03 FILLER66                       PIC X(01) VALUE ";".
           03 SLD-CAR-RECUPERADA             PIC 9(03).
           03 FILLER67                       PIC X(01) VALUE ";".
           03 SLD-MORA-MAX-TDC               PIC 9(03).
           03 FILLER68                       PIC X(01) VALUE ";".
           03 SLD-MORA-MAX-CAB               PIC 9(03).
           03 FILLER69                       PIC X(01) VALUE ";".
           03 SLD-MORA-MAX-COOP              PIC 9(03).
           03 FILLER70                       PIC X(01) VALUE ";".
           03 SLD-MORA-MAX-CAV               PIC 9(03).
           03 FILLER71                       PIC X(01) VALUE ";".
           03 SLD-END-CALIF-1-TRIM           PIC X(01).
           03 FILLER72                       PIC X(01) VALUE ";".
           03 SLD-END-CALIF-2-TRIM           PIC X(01).
           03 FILLER73                       PIC X(01) VALUE ";".
           03 SLD-CTAS-AHORRO                PIC 9(03).
           03 FILLER74                       PIC X(01) VALUE ";".
           03 SLD-CTES-CORRIENTES            PIC 9(03).
           03 FILLER75                       PIC X(01) VALUE ";".
           03 SLD-CTES-EMBARGADAS            PIC 9(03).
           03 FILLER76                       PIC X(01) VALUE ";".
           03 SLD-CTES-MAL-MANEJO            PIC 9(03).
           03 FILLER77                       PIC X(01) VALUE ";".
           03 SLD-CTES-SALDADAS              PIC 9(03).
           03 FILLER78                       PIC X(01) VALUE ";".
           03 SLD-TOTAL-CONSULTAS            PIC 9(03).
           03 FILLER79                       PIC X(01) VALUE ";".
           03 SLD-CALIFICACION               PIC X(01).
      *-----------------------------------------------------------*
       FD  ESTADIS.
       01  REG-ESTADIS.
           03 SLD-DETALLE                    PIC X(30).
           03 FILLER                         PIC X(01) VALUE ";".
           03 SLD-ESTADIS-VALOR-1            PIC Z(11).
           03 FILLER                         PIC X(01) VALUE ";".
           03 SLD-ESTADIS-VALOR-2            PIC Z(11).
           03 FILLER                         PIC X(01) VALUE ";".
           03 SLD-ESTADIS-VALOR-3            PIC Z(11).
           03 FILLER                         PIC X(01) VALUE ";".
           03 SLD-ESTADIS-VALOR-4            PIC Z(11).
           03 FILLER                         PIC X(01) VALUE ";".
           03 SLD-ESTADIS-VALOR-5            PIC Z(07).

       COPY ICBSUS.FD.
       COPY ICBCON.001.FD.
       COPY ICBCHK.FD.
       COPY ICBCRE.FD.
       COPY BDIEMSUP.FD.
       COPY BDIEENTI.FD.
       COPY BDACUE.FD.

      *----------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------*
       01  W-REG-SALIDA.
           03 W-TIPO-ID                    PIC 9(01).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-NUME-ID                    PIC 9(11).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-NOMCLI                     PIC X(45).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-CALENDPERANT               PIC X(02).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-CALENDPERACT               PIC X(02).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-ACIERTA                    PIC 9(05).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-NUM-CTA                    PIC X(18).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-DESPRODU                   PIC X(35).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-SUSCRIPTOTOR               PIC X(40).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-FECHA-INICIAL              PIC X(06).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-FECHA-NOVEDAD              PIC X(06).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-NOVEDAD                    PIC X(10).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-VECTOR-1                   PIC X(48).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-VALOR-INI                  PIC 9(07).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-SALDO-CARTERA              PIC 9(07).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-SALDO-MORA                 PIC 9(07).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-VALOR-CUOTA                PIC 9(07).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-TIPOCLI                    PIC X(01).
           03 FILLER                       PIC X(01) VALUE ";".
           03 W-FECHA-VENCIM               PIC X(06).

       01  W-VARIABLES.
           03 W-VEC-APROBACI.
              05 W-CC-SN            PIC X VALUE SPACES.                      **> X No cumple
              05 W-DR-SN            PIC X VALUE SPACES.
              05 W-COBRA-SN         PIC X VALUE SPACES.
              05 W-CALI-SN          PIC X VALUE SPACES.
           03 FILLER  REDEFINES W-VEC-APROBACI.
              05 W-VEC-APROBACI-X   PIC X(04).
      *
           03 RELLENO                     PIC X(48).
           03 FECHA-AUX                   PIC 9(04).
           03 II                          PIC 9(04).
           03 LL                          PIC 9(04).
           03 TOPE-V                      PIC S9999     VALUE  ZEROS.
           03 W-MESES-NOV                 PIC 9(04)     VALUE  ZEROS.
           03 W-CREDITOS-ACTV             PIC 9(04)     VALUE  ZEROS.
           03 W-NUM-OBLIG-CAB             PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-INICIAL-CAB           PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-SALDOS-CAB            PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-CUOTAS-CAB            PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-CAB              PIC 9(11)     VALUE  ZEROS.
           03 W-NUM-OBLIG-CAV             PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-INICIAL-CAV           PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-SALDOS-CAV            PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-CUOTAS-CAV            PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-CAV              PIC 9(11)     VALUE  ZEROS.
           03 W-NUM-OBLIG-OTR             PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-INICIAL-OTR           PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-SALDOS-OTR            PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-CUOTAS-OTR            PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-OTR              PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-NUM-TDC               PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-CUPOS-TDC             PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-SALDO-TDC             PIC 9(15)     VALUE  ZEROS.
           03 W-SAL-CUPOS-TDC             PIC 9(11)     VALUE  ZEROS.
           03 W-UTLIZACION-TDC            PIC 9(3)V9999 VALUE  ZEROS.
           03 W-TOT-CUOTAS-TDC            PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-TDC              PIC 9(11)     VALUE  ZEROS.
           03 W-NRO-RANGO-0               PIC 9(03)     VALUE  ZEROS.
           03 W-NRO-RANGO-1               PIC 9(03)     VALUE  ZEROS.
           03 W-NRO-RANGO-2               PIC 9(03)     VALUE  ZEROS.
           03 W-NRO-RANGO-3               PIC 9(03)     VALUE  ZEROS.
           03 W-NRO-RANGO-4               PIC 9(03)     VALUE  ZEROS.
           03 W-NRO-RANGO-5               PIC 9(03)     VALUE  ZEROS.
           03 W-NRO-RANGO-6               PIC 9(03)     VALUE  ZEROS.
           03 W-FEC-MENOR-TDC.
              05 W-ANO-MEN-TDC            PIC 9(04)     VALUE  ZEROS.
              05 W-MES-MEN-TDC            PIC 9(02)     VALUE  ZEROS.
           03 W-FEC-APERTURA.
              05 W-ANO-APERTURA           PIC 9(04)     VALUE  ZEROS.
              05 W-MES-APERTURA           PIC 9(02)     VALUE  ZEROS.
           03 W-FEC-NOVEDAD.
              05 W-ANO-NOV                PIC 9(04)     VALUE  ZEROS.
              05 W-MES-NOV                PIC 9(02)     VALUE  ZEROS.
           03 W-NUM-OBLIG-REAL            PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-INICIAL-REAL          PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-SALDOS-REAL           PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-CUOTAS-REAL           PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-REAL             PIC 9(11)     VALUE  ZEROS.
           03 W-NUM-OBLIG-TELE            PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-CUOTAS-TELE           PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-TELE             PIC 9(11)     VALUE  ZEROS.
           03 W-NUM-OBLIG-COOP            PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-INICIAL-COOP          PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-SALDOS-COOP           PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-CUOTAS-COOP           PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-COOP             PIC 9(11)     VALUE  ZEROS.
           03 W-NUM-OBLIG-CODEU           PIC 9(04)     VALUE  ZEROS.
           03 W-TOT-SALDOS-CODEU          PIC 9(15)     VALUE  ZEROS.
           03 W-TOT-CUOTAS-CODEU          PIC 9(11)     VALUE  ZEROS.
           03 W-TOT-MORA-CODEU            PIC 9(11)     VALUE  ZEROS.
           03 W-CREDITOS-ALDIA            PIC 9(03)     VALUE  ZEROS.
           03 W-MORAS-30-ACT              PIC 9(03)     VALUE  ZEROS.
           03 W-MORAS-60-ACT              PIC 9(03)     VALUE  ZEROS.
           03 W-MORAS-90-ACT              PIC 9(03)     VALUE  ZEROS.
           03 W-MORAS-120-ACT             PIC 9(03) VALUE ZEROS.
           03 W-CAR-CASTIGA               PIC 9(03) VALUE ZEROS.
           03 W-DUD-RECAUDO               PIC 9(03) VALUE ZEROS.
           03 W-CUENTAS-COBRADOR          PIC 9(03) VALUE ZEROS.
           03 W-MORAS-30-HIS              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-60-HIS              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-90-HIS              PIC 9(03) VALUE ZEROS.
           03 W-MORAS-120-HIS             PIC 9(03) VALUE ZEROS.
           03 W-CANC-MMANEJO              PIC 9(03) VALUE ZEROS.
           03 W-CAR-RECUPERADA            PIC 9(03) VALUE ZEROS.
           03 W-MORA-MAX-TDC              PIC 9(03) VALUE ZEROS.
           03 W-MORA-MAX-CAB              PIC 9(03) VALUE ZEROS.
           03 W-MORA-MAX-COOP             PIC 9(03) VALUE ZEROS.
           03 W-MORA-MAX-CAV              PIC 9(03) VALUE ZEROS.
           03 W-CTAS-AHORROS              PIC 9(03) VALUE ZEROS.
           03 W-CTES-CORRIENTES           PIC 9(03) VALUE ZEROS.
           03 W-CTES-EMBARGADAS           PIC 9(03) VALUE ZEROS.
           03 W-CTES-MAL-MANEJO           PIC 9(03) VALUE ZEROS.
           03 W-CTES-SALDADAS             PIC 9(03) VALUE ZEROS.
           03 W-TOTAL-CONSULTAS           PIC 9(03) VALUE ZEROS.
           03 W-ALDIA-FINA                PIC 9(03) VALUE ZEROS.
           03 W-ALDIA-REAL                PIC 9(03) VALUE ZEROS.
           03 W-ALDIA-COOP                PIC 9(03) VALUE ZEROS.
           03 W-ALDIA-TELE                PIC 9(03) VALUE ZEROS.
           03 W-MORAS-FINA                PIC 9(03) VALUE ZEROS.
           03 W-MORAS-REAL                PIC 9(03) VALUE ZEROS.
           03 W-MORAS-COOP                PIC 9(03) VALUE ZEROS.
           03 W-MORAS-TELE                PIC 9(03) VALUE ZEROS.
           03 W-CC-DR-FINA                PIC 9(03) VALUE ZEROS.
           03 W-CC-DR-REAL                PIC 9(03) VALUE ZEROS.
           03 W-CC-DR-COOP                PIC 9(03) VALUE ZEROS.
           03 W-CC-DR-TELE                PIC 9(03) VALUE ZEROS.
           03 W-EDAD                      PIC 9(03) VALUE ZEROS.
           03 FIN-CHECK                   PIC 9(01) VALUE ZEROS.
           03 SALIR-END                   PIC 9(01) VALUE ZEROS.
           03 IND-E                       PIC 9(03) VALUE ZEROS.
           03 F                           PIC 9(01) VALUE ZEROS.
           03 J                           PIC 9(04) VALUE ZEROS.

           03 W-ENDEUDA                   PIC X(01) VALUE SPACES.
           03 IND-J                       PIC 9(02) VALUE ZEROS.
           03 SW-CODEUDOR                 PIC 9(01) VALUE ZEROS.
           03 TOPE-IND                    PIC 9(03) VALUE ZEROS.
           03 SW-FECHA-LOW                PIC 9(01) VALUE ZEROS.
           03 SW-END                      PIC 9(01) VALUE ZEROS.
           03 FEC-MENOR-TDC               PIC 9(06) VALUE ZEROS.
           03 EVALUA-FINAL                PIC 9(01) VALUE ZEROS.
           03 CONT-LECTURA                PIC 9(03) VALUE ZEROS.
           03 EVALUA-CREDITO              PIC 9(01) VALUE ZEROS.
           03 EVALUA-ENDEUDA              PIC 9(01) VALUE ZEROS.
           03 EVALUA-CTA-CTE              PIC 9(01) VALUE ZEROS.
           03 W-EDAD-CEDULA               PIC 9(03) VALUE ZEROS.
           03 W-FEC-EXPE                  PIC 9(08) VALUE ZEROS.
           03 W-FEC-EXPE-R REDEFINES W-FEC-EXPE.
              05 W-ANO-EXPE               PIC 9(04).
              05 W-MES-EXPE               PIC 9(02).
              05 W-DIA-EXPE               PIC 9(02).
           03 W-ACU-MMAX-TDC              PIC 9(03) VALUE ZEROS.
           03 W-ACU-MMAX-CAB              PIC 9(03) VALUE ZEROS.
           03 W-ACU-MMAX-COOP             PIC 9(03) VALUE ZEROS.
           03 W-ACU-MMAX-CAV              PIC 9(03) VALUE ZEROS.
           03 TEM-NRO-CTA.
              05 TEM-NRO-CTA-C16          PIC X(16) VALUE ZEROS.
              05 TEM-NRO-CTA-C17          PIC X(01) VALUE ZEROS.
              05 TEM-NRO-CTA-C18          PIC X(01) VALUE ZEROS.
           03 W-SUSCRIPTOR.
              05 W-CODIGO-TIPOCTA         PIC 9(02).
              05 W-CODIGO-RESTCTA         PIC 9(04).
           03 W-TABLA-END-MAYOR.
              05 W-TRIMESTRE-END  OCCURS   3 TIMES.
                 07 W-FECHA-END           PIC 9(08).
                 07 W-MAYOR-END           PIC X(01).
           03 W-TABLA-SUS-CONSU.
              05 W-SUS-CONSU      OCCURS  50 TIMES PIC 9(06).
           03 W-FECHA-AUX                 PIC  9(04).
           03 W-FECHA-AUX-R REDEFINES W-FECHA-AUX.
              05 W-MES-AUX                PIC  9(02).
              05 W-ANO-AUX                PIC  9(02).
           03 W-FECHA-AUX1              PIC 9(06) VALUE ZEROS.
           03 W-FECHA-AUX1-R REDEFINES W-FECHA-AUX1.
              05 W-MES-AUX1             PIC 9(02).
              05 W-ANO-AUX1             PIC 9(04).
           03 W-FECHA-OK                PIC 9(06) VALUE ZEROS.
           03 W-FECHA-OK-R REDEFINES W-FECHA-OK.
              05 W-SLGANO-OK            PIC 9(04).
              05 W-SLGANO-OK-R REDEFINES W-SLGANO-OK.
                 07 W-SGL-OK            PIC 9(02).
                 07 W-ANO-OK            PIC 9(02).
              05 W-MES-OK               PIC 9(02).
           03 W-FECHA-FINAL             PIC 9(06) VALUE ZEROS.
           03 W-FECHA-APERTURA          PIC 9(06) VALUE ZEROS.

      *-------------------------------------------------------------*
        01 FECHAS-AAAAMM.
          02 FECHA-TAG                   PIC 9(6).
          02 FILLER REDEFINES FECHA-TAG.
             03 SIG-ANO-TAG              PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-TAG.
                04 SIG-TAG               PIC 9(2).
                04 ANO-TAG               PIC 9(2).
             03 MES-TAG                  PIC 9(2).
          02 FECHA-NOVEDAD07             PIC 9(6).
          02 FILLER REDEFINES FECHA-NOVEDAD07.
             03 SIG-ANO-NOV              PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-NOV.
                04 SIG-NOV               PIC 9(2).
                04 ANO-NOV               PIC 9(2).
             03 MES-NOV                  PIC 9(2).
          02 FECHA-APERTURA07            PIC 9(6).
          02 FILLER REDEFINES FECHA-APERTURA07.
             03 SIG-ANO-APE              PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-APE.
                04 SIG-APE               PIC 9(2).
                04 ANO-APE               PIC 9(2).
             03 MES-APE                  PIC 9(2).
          02 FECHA-VENCIMIENTO07         PIC 9(6).
          02 FILLER REDEFINES FECHA-VENCIMIENTO07.
             03 SIG-ANO-VEN              PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-VEN.
                04 SIG-VEN               PIC 9(2).
                04 ANO-VEN               PIC 9(2).
             03 MES-VEN                  PIC 9(2).
         02 FECHA-INGRESO               PIC 9(6).
          02 FILLER REDEFINES FECHA-INGRESO.
             03 SIG-ANO-ING              PIC 9(4).
             03 FILLER REDEFINES SIG-ANO-ING.
                04 SIG-ING               PIC 9(2).
                04 ANO-ING               PIC 9(2).
             03 MES-ING                  PIC 9(2).
       01  FECHA-SITMMAAAA-ORI     PIC 9(6).
       01  FILLER REDEFINES FECHA-SITMMAAAA-ORI.
           03 FECHA-SITMM-ORI      PIC 9(2).
           03 FECHA-SITAAAA-ORI    PIC 9(4).
       01  FECHA-SITAAAAMM         PIC 9(6).
       01  FILLER REDEFINES FECHA-SITAAAAMM.
           03 FECHA-SITAAAA        PIC 9(4).
           03 FECHA-SITMM          PIC 9(2).
       01  FECHA-VALAAAAMM         PIC 9(6).
       01  FILLER REDEFINES FECHA-VALAAAAMM.
           03 FECHA-VALAAAA        PIC 9(4).
           03 FECHA-VALMM          PIC 9(2).
      *---------------------------------------------------------------*
       01  W-FECHA-MILENIO.
           03 W-FMILENIO-SIGLO            PIC  9(02).
           03 W-FMILENIO-ANIO             PIC  9(02).
           03 W-FMILENIO-MES              PIC  9(02).
      *----------------------------------------------------------------*
       01  W-CONSTANTES.
           03 SW-READ-INC                 PIC 9(01) VALUE ZEROS.
           03 W-IND                       PIC 9(02) VALUE ZEROS.
           03 LEIDOS                      PIC 9(08) VALUE ZEROS.
           03 GRABADOS                    PIC 9(08) VALUE ZEROS.
           03 END-RUN                     PIC X(01) VALUE " ".
           03 W-STATUS-ENT                PIC 9(02) VALUE ZEROS.
           03 W-STATUS-INC                PIC 9(02) VALUE ZEROS.
      *
           03 W-RESET-LEIDOS              PIC 9(09) VALUE 9.
           03 W-LEIDOS                    PIC 9(09) VALUE ZEROS.
           03 W-LEIDOS-RED REDEFINES W-LEIDOS.
              05 FILLER                   PIC 9(06).
              05 CEROS                    PIC 9(03).
           03 W-GRABADOS                  PIC 9(09) VALUE ZEROS.
           03 W-SOLO-CODEUDORES           PIC 9(09) VALUE ZEROS.
           03 W-SOLO-COOPERATIVAS         PIC 9(09) VALUE ZEROS.
           03 W-SOLO-CERRADOS             PIC 9(09) VALUE ZEROS.
           03 W-MORA-TOT-SEC-FINA         PIC 9(09) VALUE ZEROS.
           03 W-MORA-TOT-SEC-REAL         PIC 9(09) VALUE ZEROS.
           03 W-MORA-TOT-REAL-FINA        PIC 9(09) VALUE ZEROS.

           03 W-FECHA-BASE                PIC 9(06) VALUE ZEROS.
           03 W-OPCION                    PIC X(01) VALUE SPACES.
           03 W-VECTOR-L                  PIC 9(02) VALUE 12.
           03 END-RUN                     PIC X(01) VALUE SPACES.
           03 W-CC                        PIC 9(05) VALUE ZEROS.
           03 W-DR                        PIC 9(05) VALUE ZEROS.
           03 W-COBRA                     PIC 9(05) VALUE ZEROS.
           03 W-CALI                      PIC X(01) VALUE ZEROS.

      *------------------------------------------------------------*
      *     AREA WORK ACUMULADORES
      *------------------------------------------------------------*
       01  W-ACUMULADORES-ESTA.
           03 W-EDITOR-CON                PIC Z,ZZZ,ZZ9.
           03 W-EDITOR-SIN                PIC Z,ZZZ,ZZ9.
           03 W-EDITOR-INC                PIC Z,ZZZ,ZZ9.
           03 W-EDITOR-TOT                PIC Z,ZZZ,ZZ9.
           03 W-CON-INFORMACION           PIC 9(07) VALUE ZEROS.
           03 W-SIN-INFORMACION           PIC 9(07) VALUE ZEROS.
           03 W-INCONSISTENCIAS           PIC 9(07) VALUE ZEROS.
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
      *     VARIABLES ESTADISTICAS                                     *
      *----------------------------------------------------------------*
       01  W-ACUMULADORES-ESTA.
           03 W-ESTADO-INF          OCCURS  3 TIMES.
              05 W-TABLA-TIPO-ID    OCCURS  5 TIMES.
                 07 W-ACUM-TIPO-ID    PIC  9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION VARIABLES DEMOGRAFICAS
      *------------------------------------------------------------*
           03 W-EDAD-66           PIC  9(07) VALUE ZEROS.
           03 W-EDAD-56-65        PIC  9(07) VALUE ZEROS.
           03 W-EDAD-46-55        PIC  9(07) VALUE ZEROS.
           03 W-EDAD-36-45        PIC  9(07) VALUE ZEROS.
           03 W-EDAD-29-35        PIC  9(07) VALUE ZEROS.
           03 W-EDAD-22-28        PIC  9(07) VALUE ZEROS.
           03 W-EDAD-18-21        PIC  9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION ESTADO ACTUAL
      *------------------------------------------------------------*
           03 W-TOT-CAR-CASTIGA      PIC  9(07) VALUE ZEROS.
           03 W-TOT-DUD-RECAUDO      PIC  9(07) VALUE ZEROS.
           03 W-TOT-CTES-EMBARGADAS  PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-120-ACT    PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-90-ACT     PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-60-ACT     PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-30-ACT     PIC  9(07) VALUE ZEROS.
           03 W-TOT-W-ENDEUDA        PIC  9(07) VALUE ZEROS.
           03 W-TOT-ALDIA            PIC  9(07) VALUE ZEROS.
           03 W-TOT-SIN-CTAS         PIC  9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION ESTADO SECTOR
      *------------------------------------------------------------*
           03 W-TOT-CC-DR-FINA       PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-FINA       PIC  9(07) VALUE ZEROS.
           03 W-TOT-ALDIA-FINA       PIC  9(07) VALUE ZEROS.
           03 W-TOT-CC-DR-REAL       PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-REAL       PIC  9(07) VALUE ZEROS.
           03 W-TOT-ALDIA-REAL       PIC  9(07) VALUE ZEROS.
           03 W-TOT-CC-DR-COOP       PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-COOP       PIC  9(07) VALUE ZEROS.
           03 W-TOT-ALDIA-COOP       PIC  9(07) VALUE ZEROS.
           03 W-TOT-CC-DR-TELE       PIC  9(07) VALUE ZEROS.
           03 W-TOT-MORAS-TELE       PIC  9(07) VALUE ZEROS.
           03 W-TOT-ALDIA-TELE       PIC  9(07) VALUE ZEROS.
      *------------------------------------------------------------*
      *    ACUMULACION VALORES TOTALES
      *------------------------------------------------------------*
           03 W-SALDOS-SEC-FINA      PIC  9(15) VALUE ZEROS.
           03 W-CUOTAS-SEC-FINA      PIC  9(15) VALUE ZEROS.
           03 W-MORAS-SEC-FINA       PIC  9(15) VALUE ZEROS.
           03 W-SALDOS-SEC-REAL      PIC  9(15) VALUE ZEROS.
           03 W-CUOTAS-SEC-REAL      PIC  9(15) VALUE ZEROS.
           03 W-MORAS-SEC-REAL       PIC  9(15) VALUE ZEROS.
           03 W-SALDOS-SEC-COOP      PIC  9(15) VALUE ZEROS.
           03 W-CUOTAS-SEC-COOP      PIC  9(15) VALUE ZEROS.
           03 W-MORAS-SEC-COOP       PIC  9(15) VALUE ZEROS.

      *---------------------------------------------------------*
       01  W-PARAMETROS              PIC  X(94) VALUE SPACES.
      *---------------------------------------------------------*

      *----------------------------------------------------------------*
      * Estas son las variables para analizar los vectores de          *
      * comportamiento de moras HISTORICAS (estado-mes-abd07) y        *
      * (vector-2-abd07)                                               *
      *----------------------------------------------------------------*
       01  WS-ESTADO-MES-1                        PIC 9(12).
       01  WS-ESTADO-MES-2                        PIC 9(12).
       01  WS-ESTADO-MES                          PIC 9(12).
       01  WS-ESTADO-MES-R REDEFINES WS-ESTADO-MES.
           03  WS-MES                             PIC X OCCURS 12 TIMES.
       01  ESTADO-MES-IMP.
           03  E-MES-IMP                          PIC X OCCURS 12 TIMES.

       01  ESTADO-ANUAL.
           03  CHAR-H                             PIC X OCCURS 12 TIMES.
       01  I                                      PIC 9(3)   VALUE ZERO.

       01  E                                      PIC 9(3)   VALUE ZERO.
       01  K                                      PIC 9(4)   VALUE 01.
       01  N                                      PIC 9(3)   VALUE ZERO.
       01  TIV                                    PIC 9(3)   VALUE ZERO.
       01  TFV                                    PIC 9(3)   VALUE ZERO.
       01  F-AUX-1                                PIC 9(4)   VALUE ZERO.
       01  F-AUX                                  PIC 9(4)   VALUE ZERO.
       01  F-UACT-2                               PIC 9(4)   VALUE ZERO.
       01  F-APE-7                                PIC 9(4)   VALUE ZERO.
       01  F-ING-2                                PIC 9(4)   VALUE ZERO.
       01  F-NOV-7                                PIC 9(4)   VALUE ZERO.
       01  F-VIEJ                                 PIC 9(4)   VALUE ZERO.
       01  ENCONTRO                               PIC 9(1)   VALUE ZERO.
      *01  RELLENO                                PIC 9(1)   VALUE ZERO.
       01  CINCO                                  PIC 9(2)   VALUE 5.
       01  NUEVO                                  PIC 9      VALUE 0.
       01  COD-SUSCRIP-AUX                        PIC 9(6)   VALUE ZERO.
      *------------------------------------------------------------*
       01  VECTOR-S.
           03 VECTOR-A-R1                        PIC X(24).
           03 VECTOR-A-R2                        PIC X(24).
       01  VECTOR-S-12.
           03 VECTOR-N-R1                        PIC X(24).
           03 VECTOR-N-R2                        PIC X(24).
      *------------------------------------------------------------*
        01 V-NOR.
           02 V-NOR1                     PIC 9(12).
           02 V-NOR2                     PIC 9(12).
           02 V-NOR3                     PIC 9(12).
           02 V-NOR4                     PIC 9(12).
        01 V-NOR-AUX     PIC X(47) VALUE SPACES.

        01 VECTORES-W.
          02 VECTOR-MORA.
             03 FILLER PIC X(25) VALUE "5PMPPPPPPPPP5555123411212".
             03 FILLER PIC X(25) VALUE "3234123412341234999CRD999".
          02 FILLER REDEFINES VECTOR-MORA.
             03 V-MORA OCCURS 50 TIMES PIC X.
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
           03  W-ANO-HOY                 PIC 9(04).
           03  W-MES-HOY                 PIC 9(02).
           03  W-DIA-HOY                 PIC 9(02).
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
       01  MULT-TABLA OCCURS 200 TIMES.
           03 MULT-CODIGO                PIC 9(6).
           03 MULT-CLAVE                 PIC X(5).
           03 MULT-TERMINAL              PIC 9(4).
           03 MULT-PRODUCTO              PIC 9(2).
           03 MULT-MODALIDAD             PIC 9(2).
           03 MULT-ESTADO                PIC 9.
           03 MULT-TTY                   PIC X(8).
           03 MULT-SESION                PIC X(4) COMP-5.
           03 MULT-PRIV-CLAVE            PIC 9(1).
           03 MULT-PRO-EXCLU             PIC 9(1).
           03 MULT-NOM-SUS               PIC X(14).
           03 MULT-NIT                   PIC 9(11) COMP-3.
       01  NOM-COR-AUX             PIC X(14) VALUE SPACES.
       01  FILLER REDEFINES NOM-COR-AUX.
            02 NOM-COR-AUXN        PIC 9(6).
            02 FILLER              PIC X(8).
       01  NOM-COR-AUX2            PIC X(14) VALUE SPACES.
       01  MENSAJE-CS.
           02 HEADER.
             03 HEADER-USUARIO           PIC X(10).
             03 HEADER-TIPO-MENSAJE      PIC 9.
             03 HEADER-LSN               PIC 9(4).
             03 HEADER-LONGITUD          PIC 9(4).
             03 HEADER-TERMINAL          PIC X(8).
             03 HEADER-NOMB-PROG         PIC X(6).
             03 HEADER-FILA              PIC 99.
             03 HEADER-COLUMNA           PIC 99.
             03 HEADER-CARACT-INICIO     PIC 9.
             03 HEADER-CARACT-FIN        PIC 9.
             03 HEADER-TIPO-TERMINAL     PIC X(6).
             03 HEADER-FILLER            PIC X(5).
          02 TEXTO                       PIC X(2000).
          02 TEXTO-INPUT REDEFINES TEXTO.
             03 AREA-INPUT               PIC X(80).
             03 FILLER                   PIC X(1920).
      *-----------------------------------------------------------*
       01  TABLA-HISTORICA-MORAS.
           03 w-moras-historicas-v       pic X(48) value zeros.
           03 w-moras-historicas-v-r redefines w-moras-historicas-v.
              05 w-moras-historicas occurs 48 times pic X(01).
       01  ARRASTRE                      PIC 9 VALUE ZEROS.
      *------------------------------------------------------------*
       COPY ICFILE.WS.
       COPY UTLBIT.WS.
       COPY VALVLR.WS.
      *------------------------------------------------------------*
        COPY PE-TIME.WS.
      *------------------------------------------------------------*
       01  w-msup-ind-bloq-n           PIC 9(01) VALUE zeros.
       01  w-msup-ind-bloq-x redefines w-msup-ind-bloq-n
                                       PIC x(01).
      *------------------------------------------------------------*
       01  ARCHIVOS.
           03 ARCHIVO-ENTRADA.
              05 FILLER             PIC X(12) VALUE  "$TEMPORALES/".
              05 ARCH-ENTRADA       PIC X(30) VALUE  SPACES.
           03 ARCHIVO-INCONC.
              05 FILLER             PIC X(12) VALUE  "$TEMPORALES/".
              05 ARCH-INCONC        PIC X(30) VALUE  SPACES.
           03 ARCHIVO-SALIDA.
              05 FILLER             PIC X(12) VALUE  "$TEMPORALES/".
              05 ARCH-SALIDA        PIC X(30) VALUE  SPACES.
           03 ARCHIVO-ESTADIS.
              05 FILLER             PIC X(12) VALUE  "$TEMPORALES/".
              05 ARCH-ESTADIS       PIC X(30) VALUE  SPACES.
      *------------------------------------------------------------*
       01  TABLAS-EVALUACION.
           03  TABLA-NOVEDADES-CARTERA.
               05  FILLER  PIC X(23) VALUE  "0101NORMAL.-.AL.DIA1   ".
               05  FILLER  PIC X(23) VALUE  "0202TARJ. NO.RECLAM1   ".
               05  FILLER  PIC X(23) VALUE  "0803CANCEL.MAL.MANE1   ".
               05  FILLER  PIC X(23) VALUE  "0404TARJETA.ROBADA 1   ".
               05  FILLER  PIC X(23) VALUE  "0505CANCEL.VOLUNTAR1   ".
               05  FILLER  PIC X(23) VALUE  "0606CANCEL.MMX<60  1   ".
               05  FILLER  PIC X(23) VALUE  "0707TARJ.EXTRAV.   1   ".
               05  FILLER  PIC X(23) VALUE  "0308PAGO VOLUNT.SAT1   ".
               05  FILLER  PIC X(23) VALUE  "0309PAGO.MMX.A.30  1   ".
               05  FILLER  PIC X(23) VALUE  "0310PAGO.MMX.A.60  1   ".
               05  FILLER  PIC X(23) VALUE  "0311PAGO.MORA.90   1   ".
               05  FILLER  PIC X(23) VALUE  "0312PAGO.VOL.MOR1201   ".
               05  FILLER  PIC X(23) VALUE  "0113MORA.30.AL.DIA 1   ".
               05  FILLER  PIC X(23) VALUE  "0114MORA.60.AL.DIA 1   ".
               05  FILLER  PIC X(23) VALUE  "0115MORA.90.AL.DIA 1   ".
               05  FILLER  PIC X(23) VALUE  "0116MORA.120.AL.DIA1   ".
               05  FILLER  PIC X(23) VALUE  "1217ESTA.MORA.30   1   ".
               05  FILLER  PIC X(23) VALUE  "1318ESTA.MORA.60   1   ".
               05  FILLER  PIC X(23) VALUE  "1419ESTA.MORA.90   1   ".
               05  FILLER  PIC X(23) VALUE  "1520ESTA.MORA.120  1   ".
               05  FILLER  PIC X(23) VALUE  "1221FU.60.Y.ESTA.301   ".
               05  FILLER  PIC X(23) VALUE  "1222FU.90.Y.ESTA.301   ".
               05  FILLER  PIC X(23) VALUE  "1323FU.90.Y.ESTA.601   ".
               05  FILLER  PIC X(23) VALUE  "1224FU.120.ESTA.30 1   ".
               05  FILLER  PIC X(23) VALUE  "1325FU.120.ESTA.60 1   ".
               05  FILLER  PIC X(23) VALUE  "1426FU.120.ESTA.90 1   ".
               05  FILLER  PIC X(23) VALUE  "1327RE.30.ESTA.60  1   ".
               05  FILLER  PIC X(23) VALUE  "1428RE.30.Y.ESTA.901   ".
               05  FILLER  PIC X(23) VALUE  "1529RE.30.ESTA.120 1   ".
               05  FILLER  PIC X(23) VALUE  "1230RE.60.Y.ESTA.301   ".
               05  FILLER  PIC X(23) VALUE  "1331RE.60.Y.ESTA.601   ".
               05  FILLER  PIC X(23) VALUE  "1432RE.60.Y.ESTA.901   ".
               05  FILLER  PIC X(23) VALUE  "1533RE.60.ESTA.120 1   ".
               05  FILLER  PIC X(23) VALUE  "1234RE.90.Y.ESTA.301   ".
               05  FILLER  PIC X(23) VALUE  "1335RE.90.Y.ESTA.601   ".
               05  FILLER  PIC X(23) VALUE  "1436RE.90.Y.ESTA.901   ".
               05  FILLER  PIC X(23) VALUE  "1537RE.90.ESTA.120 1   ".
               05  FILLER  PIC X(23) VALUE  "1238RE.120.ESTA.30 1   ".
               05  FILLER  PIC X(23) VALUE  "1339RE.120.ESTA.60 1   ".
               05  FILLER  PIC X(23) VALUE  "1440RE.120.ESTA.90 1   ".
               05  FILLER  PIC X(23) VALUE  "1541RE.120.ESTA.1201   ".
               05  FILLER  PIC X(23) VALUE  "2042ADJ.NO.LOCALIZA1   ".
               05  FILLER  PIC X(23) VALUE  "2143ADJ.CTA.COBRADO1   ".
               05  FILLER  PIC X(23) VALUE  "2244FALLECIDO      0   ".
               05  FILLER  PIC X(23) VALUE  "0945CARTERA.CASTIGA1   ".
               05  FILLER  PIC X(23) VALUE  "1046CARTERA.RECUPER1   ".
               05  FILLER  PIC X(23) VALUE  "1147DUDOSO.RECAUDO 1   ".
               05  FILLER  PIC X(23) VALUE  "2348CONSULTA       0   ".
               05  FILLER  PIC X(23) VALUE  "4949NO RENOVADA    1   ".
               05  FILLER  PIC X(23) VALUE  "50                 0   ".
           03  TABLA-NOVEDADES-CARTERA-R  REDEFINES
               TABLA-NOVEDADES-CARTERA.
               05  TAB-NOVEDADES-CARTERA  OCCURS  50  TIMES.
                   07  TAB-NOV-CART-CODINT PIC 9(02).
                   07  TAB-NOV-CART-COD    PIC 9(02).
                   07  TAB-NOV-CART-NOM    PIC X(15).
                   07  TAB-NOV-CART-CAL    PIC 9(01).
                   07  TAB-NOV-CART-RAN    PIC 9(02).
                   07  TAB-NOV-CART-CAL2   PIC 9(01).

           03  TABLA-NOVEDADES-CTA-CTE.
               05 FILLER  PIC X(23) VALUE  "3001NORMAL.-.AL.DIA1   ".
               05 FILLER  PIC X(23) VALUE  "3102CANCEL.MAL.MANE1   ".
               05 FILLER  PIC X(23) VALUE  "0003               0   ".
               05 FILLER  PIC X(23) VALUE  "0004               0   ".
               05 FILLER  PIC X(23) VALUE  "3205SALDADA        1   ".
               05 FILLER  PIC X(23) VALUE  "3306EMBARGADA      1   ".
               05 FILLER  PIC X(23) VALUE  "3507EMBARGADA.NORMA1   ".
               05 FILLER  PIC X(23) VALUE  "0008               0   ".
               05 FILLER  PIC X(23) VALUE  "3409INACTIVA       1   ".
           03  TABLA-NOVEDADES-CTA-CTE-R  REDEFINES
               TABLA-NOVEDADES-CTA-CTE.
               05  TAB-NOVEDADES-CTA-CTE  OCCURS   9  TIMES.
                   07  TAB-NOV-CTAC-CODINT PIC 9(02).
                   07  TAB-NOV-CTAC-COD    PIC 9(02).
                   07  TAB-NOV-CTAC-NOM    PIC X(15).
                   07  TAB-NOV-CTAC-CAL    PIC 9(01).
                   07  TAB-NOV-CTAC-RAN    PIC 9(02).
                   07  TAB-NOV-CTAC-CAL2   PIC 9(01).

           03  TABLA-ENDEUDAMIENTO.
               05  FILLER  PIC X(23) VALUE  "24A NORMAL         1   ".
               05  FILLER  PIC X(23) VALUE  "25B SUB-NORMAL     1   ".
               05  FILLER  PIC X(23) VALUE  "26C DEFICIENTE     1   ".
               05  FILLER  PIC X(23) VALUE  "27D DIFICIL-COBRO  1   ".
               05  FILLER  PIC X(23) VALUE  "28E IRRECUPERABLE  1   ".
               05  FILLER  PIC X(23) VALUE  "29+ CONTINGENCIA   0   ".
               05  FILLER  PIC X(23) VALUE  "00T                0   ".
               05  FILLER  PIC X(23) VALUE  "00R                0   ".
           03  TABLA-ENDEUDAMIENTO-R      REDEFINES
               TABLA-ENDEUDAMIENTO.
               05  TAB-ENDEUDAMIENTO      OCCURS   8  TIMES.
                   07  TAB-END-CODINT     PIC 9(02).
                   07  TAB-END-COD        PIC X(01).
                   07  TAB-END-NOM        PIC X(16).
                   07  TAB-END-CAL        PIC 9(01).
                   07  TAB-END-RAN        PIC 9(02).
                   07  TAB-END-CAL2       PIC 9(01).
      *------------------------------------------------------------*
       01 TABLA-VIGENTES.
           03  tabla-vigentes-cartera.
               05 filler        PIC x(10)   VALUE  "1000000000".
               05 filler        PIC x(10)   VALUE  "0011111111".
               05 filler        PIC x(10)   VALUE  "1111111111".
               05 filler        PIC x(10)   VALUE  "1111111111".
               05 filler        PIC x(10)   VALUE  "1111101101".
           03  tabla-vigentes-cartera-r redefines
               tabla-vigentes-cartera.
               05 tab-vigentes-cartera   occurs 50 times  PIC 9(01).
      *------------------------------------------------------------*
       01 TABLA-TIPO-CARTERA.
           03 W-TIPO-CARTERA.
              05 FILLER  PIC 9(07) VALUE 0101000.
              05 FILLER  PIC 9(07) VALUE 0201000.
              05 FILLER  PIC 9(07) VALUE 0301000.
              05 FILLER  PIC 9(07) VALUE 0402000.
              05 FILLER  PIC 9(07) VALUE 0501000.
              05 FILLER  PIC 9(07) VALUE 0603000.
              05 FILLER  PIC 9(07) VALUE 0701000.
              05 FILLER  PIC 9(07) VALUE 0802000.
              05 FILLER  PIC 9(07) VALUE 0902000.
              05 FILLER  PIC 9(07) VALUE 1002000.
              05 FILLER  PIC 9(07) VALUE 1102000.
              05 FILLER  PIC 9(07) VALUE 1202000.
              05 FILLER  PIC 9(07) VALUE 1301000.
              05 FILLER  PIC 9(07) VALUE 1401000.
              05 FILLER  PIC 9(07) VALUE 1502000.
              05 FILLER  PIC 9(07) VALUE 1602000.
              05 FILLER  PIC 9(07) VALUE 1701000.
              05 FILLER  PIC 9(07) VALUE 1801000.
              05 FILLER  PIC 9(07) VALUE 1902000.
              05 FILLER  PIC 9(07) VALUE 2001000.
              05 FILLER  PIC 9(07) VALUE 2102000.
              05 FILLER  PIC 9(07) VALUE 2202000.
              05 FILLER  PIC 9(07) VALUE 2304000.
              05 FILLER  PIC 9(07) VALUE 2402000.
              05 FILLER  PIC 9(07) VALUE 2501000.
              05 FILLER  PIC 9(07) VALUE 2602000.
              05 FILLER  PIC 9(07) VALUE 2703000.
              05 FILLER  PIC 9(07) VALUE 2802000.
              05 FILLER  PIC 9(07) VALUE 2902000.
           03 W-TIPO-CARTERA-R REDEFINES W-TIPO-CARTERA.
              05 W-TIPO-CART OCCURS 29 TIMES.
                 07 W-CART-CONSEC          PIC 9(02).
                 07 W-CART-TIPO            PIC 9(02).
                 07 W-FLAG-EVALUA-SN       PIC 9(01).
                 07 W-FLAG-MESES-CONTEM    PIC 9(02).

      *---------------------------------------------------- *
LEY001 COPY  LEEMAESTROS.WS.                                            GDT0001
LEY001$SET CONSTANT HACERMATCH"SI"                                      GDT0001
LEY001$SET CONSTANT AMNISTIA21"SI"                                      GDT0001
       PROCEDURE DIVISION.
      *---------------------------------------------------- *
       0010-PROGRAMA-PRINCIPAL.
           DISPLAY "       "
           DISPLAY " ** Datacredito*   MAYOR INFORMACION STANDARD  **"
           DISPLAY "       "
           DISPLAY "PROGRAMA:icestd81.int           VERSION:NOV/2006 "
           DISPLAY "       "
           DISPLAY "       "
           ACCEPT   W-PARAMETROS   FROM COMMAND-LINE
           UNSTRING W-PARAMETROS  DELIMITED BY SPACES INTO
                    ARCH-ENTRADA
                    ARCH-INCONC
           END-UNSTRING
DLMDLM     EXHIBIT NAMED  W-PARAMETROS
DLMDLM     EXHIBIT NAMED  ARCH-ENTRADA
DLMDLM     EXHIBIT NAMED  ARCH-INCONC
           OPEN INPUT INCONSIS
           IF W-STATUS-INC = 35
              DISPLAY "!! Archivo inconsistencias no existe.!!"
              STOP RUN
           END-IF
           OPEN INPUT ENTRADA
           IF W-STATUS-ENT = 35
              DISPLAY "!! Archivo no existe..Proceso termina.!!"
              STOP RUN
           ELSE
              STRING  ARCH-ENTRADA DELIMITED BY "."
                      ".DATINF"    DELIMITED BY SIZE
                      INTO         ARCH-SALIDA
              END-STRING
              STRING  ARCH-ENTRADA DELIMITED BY "."
                      ".ESTADI"    DELIMITED BY SIZE
                      INTO         ARCH-ESTADIS
              END-STRING
              CLOSE   ENTRADA

              PERFORM 0015-CARGUE-TABLA-BDACUE

              PERFORM 0020-APERTURA-ARCHIVOS

              PERFORM 0030-EVALUA-CTAS

              PERFORM 0450-READ-INCONSISTENCIAS

              PERFORM 9800-GRABAR-ESTADISTICA

              PERFORM 9900-RESUMEN-PROCESO
              STOP RUN
           END-IF.

      *-----------------------------*
       COPY UTLBIT.PROC.
       COPY VALVLR.PROC.
      *-----------------------------*

      *-----------------------------------------------------------------*
      * Rutina para manejar VALORES                                     *
      *-----------------------------------------------------------------*
       0015-CARGUE-TABLA-BDACUE.
            COPY PE-VALORES.PROC.

      *-----------------------------------------------------------------*
      * Se abren los archivos de trabajo, se realiza la primera lectura *
      * del archivo de entrada para validar c¢digo y clave y se verIFica*
      * los suscriptores que esten bloqueados.                          *
      *-----------------------------------------------------------------*
       0020-APERTURA-ARCHIVOS.
           OPEN INPUT  ENTRADA
           OPEN INPUT  MAESTRO-CHECK
LEY001     OPEN  INPUT  MAESTRO-DIRECC                                  GDT0001
LEY001     OPEN  INPUT  NOAMNISTIA21                                    GDT0001
LEY001*                                                                 GDT0001
           OPEN INPUT  MAESTRO-CREDIT
           OPEN INPUT  SUSCRIPTOR
           OPEN INPUT  BDIEMSUP
           OPEN INPUT  BDIEENTI
           OPEN INPUT  INDEXADO
           OPEN OUTPUT SALIDA
           OPEN OUTPUT ESTADIS
           MOVE  FUNCTION CURRENT-DATE TO W-FECHA-HOY
           MOVE W-FECHA-HOY-R(1:6)     TO W-FECHA-BASE
           MOVE W-FECHA-BASE           TO FECHA-TAG.

      *-----------------------------------------------------------------*
      * Se verifica si el cliente posee informaci¢n crediticia y de ser *
      * as¡ se llaman las rutinas para evaluar ctasctes y cartera       *
      *-----------------------------------------------------------------*
       0030-EVALUA-CTAS.
           PERFORM  UNTIL CONT-LECTURA = 999
                    READ ENTRADA   AT  END
                             MOVE 999 TO CONT-LECTURA
                         NOT AT END
                             INITIALIZE W-VARIABLES
                             MOVE W-VECTOR-L TO LL
                             COMPUTE II = 49 - LL
      *
                             INITIALIZE REG-SALIDA
                             PERFORM 0031-FILLERS
                             MOVE  999999 TO  W-FEC-MENOR-TDC
                             ADD     1    TO  LEIDOS

                             PERFORM 0100-EVALUA-CTASCTES

                             PERFORM 0200-EVALUA-CARTERA

                             PERFORM 0300-EVALUA-ENDEUDAMIENTO

                             PERFORM 0400-EVALUA-CONSULTAS

                             PERFORM 0500-GRABAR-SALIDA

                    END-READ
           END-PERFORM.

       0031-FILLERS.
           MOVE ";" TO FILLER01 FILLER02 FILLER03 FILLER04 FILLER05
              FILLER06 FILLER07 FILLER08 FILLER09 FILLER10 FILLER11
              FILLER12 FILLER13 FILLER14 FILLER15 FILLER16 FILLER17
              FILLER18 FILLER19 FILLER20 FILLER21 FILLER22 FILLER23
              FILLER24 FILLER25 FILLER26 FILLER27 FILLER28 FILLER29
              FILLER30 FILLER31 FILLER32 FILLER33 FILLER34 FILLER35
              FILLER36 FILLER37 FILLER38 FILLER39 FILLER40 FILLER41
              FILLER42 FILLER43 FILLER44 FILLER45 FILLER46 FILLER47
              FILLER48 FILLER49 FILLER50 FILLER51 FILLER52 FILLER53
              FILLER54 FILLER55 FILLER56 FILLER57 FILLER58 FILLER59
              FILLER60 FILLER61 FILLER62 FILLER63 FILLER64 FILLER65
              FILLER66 FILLER67 FILLER68 FILLER69 FILLER70 FILLER71
              FILLER72 FILLER73 FILLER74 FILLER75 FILLER76 FILLER77
              FILLER78 FILLER79.

      *------------------------------------------------------------------*
      * Verifica las novedades de ctasctes y califica                    *
      *------------------------------------------------------------------*
       0100-EVALUA-CTASCTES.
           INITIALIZE     LLAVE-MAES-CHECK
           MOVE  ENT-TIPO-ID  TO  TIPO-ID-ABD06
           MOVE  ENT-NUME-ID  TO  NUM-ID-ABD06
LEY001     PERFORM  0600-ASIGNA-ID-ICMCHK-START                         GDT0001
           START MAESTRO-CHECK KEY NOT < LLAVE-MAES-CHECK
                 INVALID KEY
                         MOVE 9 TO TIPO-ID-ABD06
                 NOT INVALID KEY
LEY001*                   READ MAESTRO-CHECK NEXT                       GDT0001
LEY001*                        AT END                                   GDT0001
LEY001*                           MOVE 9 TO TIPO-ID-ABD06               GDT0001
LEY001*                   END-READ                                      GDT0001
LEY001     PERFORM  3000-LEE-ICMCHK-SECUENCIAL                          GDT0001
LEY001     IF  SW-LECTURA-MAESTROS  =  1                                GDT0001
LEY001                                                                  GDT0001
LEY001                                                                  GDT0001
LEY001                             MOVE 9 TO TIPO-ID-ABD06              GDT0001
LEY001                                                                  GDT0001
LEY001     END-IF                                                       GDT0001
           END-START
           PERFORM UNTIL  TIPO-ID-ABD06 NOT = ENT-TIPO-ID  OR
                          NUM-ID-ABD06  NOT = ENT-NUME-ID
                 MOVE COD-SUS-ABD06  TO  COD-SUSCRIP-ABD02
                 MOVE COD-SUS-ABD06  TO  W-SUSCRIPTOR
                 READ SUSCRIPTOR  IGNORE LOCK
                      INVALID KEY
                              MOVE 0 TO ESTADO-ABD02
                 END-READ
                 IF nov-report-abd06 > 0 AND nov-report-abd06  < 10
                    IF ESTADO-ABD02 = 1  AND IND-BLOQUEO-ABD06 =  0
                       move tab-nov-ctac-cal (nov-report-abd06)
                                          to  evalua-cta-cte

                       PERFORM 0120-EVALUA-CUENTAS-BANCARIAS

                       IF EVALUA-FINAL < evalua-cta-cte
                          move evalua-cta-cte to EVALUA-FINAL
                       END-IF
                    END-IF
                 END-IF
LEY001*          READ MAESTRO-CHECK NEXT                                GDT0001
LEY001*               AT END                                            GDT0001
LEY001*                  MOVE 9 TO TIPO-ID-ABD06                        GDT0001
LEY001*          END-READ                                               GDT0001
LEY001     PERFORM  3000-LEE-ICMCHK-SECUENCIAL                          GDT0001
LEY001     IF  SW-LECTURA-MAESTROS  =  1                                GDT0001
LEY001                                                                  GDT0001
LEY001                                                                  GDT0001
LEY001                    MOVE 9 TO TIPO-ID-ABD06                       GDT0001
LEY001                                                                  GDT0001
LEY001     END-IF                                                       GDT0001
           END-PERFORM.

      *-----------------------------------------------*
       0120-EVALUA-CUENTAS-BANCARIAS.
      *-----------------------------------------------*
           IF W-CODIGO-TIPOCTA = 01
              EVALUATE TAB-NOV-CTAC-CODINT(NOV-REPORT-ABD06)
                    WHEN 30
                         ADD  1  TO  W-CTES-CORRIENTES
                    WHEN 31
                         ADD  1  TO  W-CTES-MAL-MANEJO
                    WHEN 32
                         ADD  1  TO  W-CTES-SALDADAS
                    WHEN 33
                         ADD  1  TO  W-CTES-EMBARGADAS
                    WHEN 35
                         ADD  1  TO  W-CTES-CORRIENTES
              END-EVALUATE
           ELSE
              IF TAB-NOV-CTAC-CODINT(NOV-REPORT-ABD06) = 30 OR 35
                 ADD   1   TO   W-CTAS-AHORROS
              END-IF
           END-IF.

      *-------------------------------------------------------*
      * Verifica las novedades de cartera y califica          *
      *-------------------------------------------------------*
       0200-EVALUA-CARTERA.
           INITIALIZE        LLAVE-MAES-CREDIT
           MOVE   ZEROS      TO  W-SUSCRIPTOR
           MOVE  ENT-TIPO-ID TO  TIPO-ID-ABD07
           MOVE  ENT-NUME-ID TO  NUM-ID-ABD07
LEY001     PERFORM  0500-ASIGNA-ID-ICMCRE-START                         GDT0001
           START MAESTRO-CREDIT  KEY NOT < LLAVE-MAES-CREDIT
                 INVALID KEY
                         MOVE 9 TO TIPO-ID-ABD07
                 NOT INVALID KEY
LEY001     PERFORM  1000-LEE-ICMCRE-SECUENCIAL                          GDT0001
LEY001     IF  SW-LECTURA-MAESTROS  =  0                                GDT0001
LEY001                                                                  GDT0001
LEY001                              MOVE COD-CTA-ABD07 TO W-CTA-BDCUE   GDT0001
LEY001                              IF COD-CTA-ABD07 > 29               GDT0001
LEY001                                 MOVE 11 TO COD-CTA-ABD07         GDT0001
LEY001                              END-IF                              GDT0001
LEY001                                                                  GDT0001
LEY001     END-IF                                                       GDT0001
LEY001     IF  SW-LECTURA-MAESTROS  =  1                                GDT0001
LEY001                                                                  GDT0001
LEY001                                                                  GDT0001
LEY001                                 MOVE 9 TO TIPO-ID-ABD07          GDT0001
LEY001     END-IF                                                       GDT0001
           END-START
           PERFORM UNTIL TIPO-ID-ABD07 NOT = ENT-TIPO-ID  OR
                         NUM-ID-ABD07  NOT = ENT-NUME-ID
                 MOVE COD-SUS-ABD07 TO COD-SUSCRIP-ABD02
                 READ SUSCRIPTOR  IGNORE LOCK
                      INVALID KEY
                              MOVE 0 TO ESTADO-ABD02
                 END-READ
                 IF NOV-REPORT-ABD07 > 0 AND NOV-REPORT-ABD07 < 51
                    IF ESTADO-ABD02  = 1 AND IND-BLOQUEO-ABD07 = 0
                       move tab-nov-cart-cal (nov-report-abd07)
                                         to   evalua-credito
                       IF COD-CTA-ABD07 = 02 OR
                          COD-SUS-ABD07 = 240033 OR 080001
                          PERFORM 5000-EVALUAR-PRODUCTO
                       END-IF
                       IF COD-SUS-ABD07 = 020151 OR 020167 OR 020370
                          MOVE   11  TO  COD-CTA-ABD07
                       END-IF
                       IF COD-SUS-ABD07 = 020055 OR 020007
                          MOVE   03  TO  COD-CTA-ABD07
                       END-IF

                       INITIALIZE  W-REG-SALIDA
                       PERFORM  5900-EVALUA-FECHAS
                       PERFORM  1900-EVALUAR-CODEUDOR
                       PERFORM  2000-EVALUAR-CLASE-CARTERA
                       PERFORM  6000-CUENTA-MORAS-ACTUAL-ESTADO
      *                PERFORM  7500-ANALIZA-VECTOR-HISTORICO

                       MOVE V-NOR(II:LL) TO  W-VECTOR-1
                       MOVE W-VECTOR-1   TO  VECTOR-S
                       MOVE VECTOR-S     TO  W-MORAS-HISTORICAS-V
                       DISPLAY "VECTOR ; " ENT-TIPO-ID ";" ENT-NUME-ID
                       ";" ENT-NOMBRE ";" NUM-CTA-ABD07 ";" W-VECTOR-1
                       PERFORM  5900-CUENTA-MORAS-HISTORICAS
      *                 PERFORM  5800-CUENTA-MORAS-HISTORICAS
                       PERFORM  6200-MORA-MAXIMA-CUENTAS
                       IF EVALUA-CREDITO >  EVALUA-FINAL
                          MOVE EVALUA-CREDITO TO EVALUA-FINAL
                       END-IF
                    END-IF
                 END-IF
LEY001     PERFORM  1000-LEE-ICMCRE-SECUENCIAL                          GDT0001
LEY001     IF  SW-LECTURA-MAESTROS  =  0                                GDT0001
LEY001                                                                  GDT0001
LEY001                     MOVE COD-CTA-ABD07 TO W-CTA-BDCUE            GDT0001
LEY001                     IF COD-CTA-ABD07 > 29                        GDT0001
LEY001                        MOVE 11         TO COD-CTA-ABD07          GDT0001
LEY001                     END-IF                                       GDT0001
LEY001                                                                  GDT0001
LEY001     END-IF                                                       GDT0001
LEY001     IF  SW-LECTURA-MAESTROS  =  1                                GDT0001
LEY001                                                                  GDT0001
LEY001                                                                  GDT0001
LEY001                     MOVE 9 TO TIPO-ID-ABD07                      GDT0001
LEY001     END-IF                                                       GDT0001
           END-PERFORM.

      *------------------------------------------------------------------*
      * VerIFica el endeudamiento y calIFica                             *
      *------------------------------------------------------------------*
       0300-EVALUA-ENDEUDAMIENTO.
           move zeros       to  salir-end
           MOVE    1        TO  E
           move ent-tipo-id to BDIEMSUP-tip-id
           move ent-nume-id to BDIEMSUP-num-id
           start BDIEMSUP key >= BDIEMSUP-llave
                  invalid key
                          move 1 to salir-end
           end-start
           IF salir-end  =  0
              PERFORM  0330-READ-NEXT-ENDEUDA
           END-IF.

      *----------------------------------------------------------------*
      * READ INDEXD ENDEUDAMIENTO                                      *
      *----------------------------------------------------------------*
       0330-READ-NEXT-ENDEUDA.
           READ BDIEMSUP next record
                at end
                move 9 to BDIEMSUP-tip-id
           END-READ
           PERFORM UNTIL BDIEMSUP-tip-id not = ent-tipo-id or
                         BDIEMSUP-num-id not = ent-nume-id
                IF BDIEMSUP-ind-bloq is not numeric
                   move 0 to BDIEMSUP-ind-bloq
                END-IF
                IF BDIEMSUP-ind-bloq not = 1
                   IF BDIEMSUP-fecha > zeros
                      PERFORM varying IND-E from 1 by 1
                                            until IND-E > 8
                           IF BDIEMSUP-CALIFICA = tab-end-cod(IND-E)
                              MOVE TAB-END-CAL(IND-E)
                                            TO evalua-endeuda
                              MOVE TAB-END-COD(IND-E)  TO W-ENDEUDA

                              PERFORM 0360-TRIMESTRE-ENDEUDA

                              IF EVALUA-FINAL < evalua-endeuda
                                 MOVE evalua-endeuda to EVALUA-FINAL
                                 MOVE TAB-END-COD(IND-E) TO W-ENDEUDA
                              END-IF
                              ADD 7 TO IND-E
                           END-IF
                      END-PERFORM
                   END-IF
                END-IF
                read BDIEMSUP next record
                     at end
                        move 9 to BDIEMSUP-tip-id
                end-read
           END-PERFORM.

      *----------------------------------------------------------------*
      * VALORES ENDEUDAMIENTO                                          *
      *----------------------------------------------------------------*
       0360-TRIMESTRE-ENDEUDA.
            IF BDIEMSUP-CALIFICA NOT = "T"
               IF SW-END = ZEROS
                  MOVE     1               TO  SW-END
                  MOVE  BDIEMSUP-CALIFICA  TO  W-MAYOR-END (E)
                  MOVE  BDIEMSUP-FECHA     TO  W-FECHA-END (E)
               END-IF
               IF BDIEMSUP-FECHA = W-FECHA-END (E)
                  IF BDIEMSUP-CALIFICA > W-MAYOR-END (E)
                     MOVE  BDIEMSUP-CALIFICA  TO  W-MAYOR-END (E)
                  END-IF
               ELSE
                  ADD   1   TO   E
                  IF E > 3
                     MOVE  3  TO   E
                     MOVE W-MAYOR-END (E)  TO  W-MAYOR-END (2)
                     MOVE W-FECHA-END (E)  TO  W-FECHA-END (2)
                  END-IF
                  MOVE BDIEMSUP-FECHA      TO  W-FECHA-END (E)
                  MOVE BDIEMSUP-CALIFICA   TO  W-MAYOR-END (E)
               END-IF
            END-IF.
      *----------------------------------------------------------------*
      * Rutina que establece el numero de consultas que se hicieron por*
      * parte de entidades financieras solamente                       *
      *----------------------------------------------------------------*
       0400-EVALUA-CONSULTAS.
      *----------------------*
            INITIALIZE           LLAVE-INDEX
            MOVE ENT-TIPO-ID TO  TIP-ID-IND
            MOVE ENT-NUME-ID TO  NUM-ID-IND
            START INDEXADO KEY NOT < LLAVE-INDEX
                  INVALID KEY
                    MOVE 9 TO TIP-ID-IND
                  NOT INVALID KEY
                      READ INDEXADO NEXT
                      AT END
                         MOVE 9 TO TIP-ID-IND
                      END-READ
            END-START
            PERFORM UNTIL  TIP-ID-IND NOT = ENT-TIPO-ID  OR
                           NUM-ID-IND NOT = ENT-NUME-ID
                  IF COD-SUSCRIP-IND < 800000
                     ADD  1  TO  W-TOTAL-CONSULTAS
                  END-IF
                  READ INDEXADO NEXT
                       AT END
                       MOVE 9 TO TIP-ID-IND
                  END-READ
            END-PERFORM.
|
      *----------------------------------------------------------------*
      * Lee el total de inconsistencias para estadisticas              *
      *----------------------------------------------------------------*
       0450-READ-INCONSISTENCIAS.
           PERFORM  UNTIL SW-READ-INC = 9
                    READ INCONSIS  AT  END
                         MOVE  9  TO SW-READ-INC
                    NOT AT END
                        ADD   1   TO  W-INCONSISTENCIAS
                        ADD   1   TO  W-ACUM-TIPO-ID (3, 1)
                    END-READ
           END-PERFORM.

      *-----------------------------------------------------------------*
      * Evalua el estado final de la variable de calIFicaci¢n y arma el *
      * registro de salida.                                             *
      *-----------------------------------------------------------------*
       0500-GRABAR-SALIDA.
            IF EVALUA-FINAL = 1
               MOVE "1" TO SLD-CALIFICACION
               MOVE  1  TO F
               ADD   1  TO W-CON-INFORMACION
            ELSE
               IF EVALUA-FINAL = 0
                  MOVE "5" TO SLD-CALIFICACION
                  MOVE  2  TO F
                  ADD   1  TO W-SIN-INFORMACION
               END-IF
            END-IF
            MOVE    ENT-TIPO-ID       TO  SLD-TIPO-ID
            MOVE    ENT-NUME-ID       TO  SLD-NUME-ID
            MOVE    ENT-NOMBRE        TO  SLD-NOMBRE
            MOVE    ENT-CIUDAD-EXP    TO  SLD-CIUDAD-EXPED
            IF ENT-TIPO-ID = 1
               MOVE  ENT-GENERO-VAL   TO  SLD-GENERO
               MOVE  ENT-EDAD-RANGO   TO  SLD-RANGO-EDAD
            END-IF
            MOVE  W-CREDITOS-ACTV     TO  SLD-CREDITOS-ACTV
      *------------------------------------------------------*
      *     INFORMACION SECTOR FINANCIERO
      *------------------------------------------------------*
            MOVE  W-NUM-OBLIG-CAB  TO  SLD-NUM-OBLIG-CAB
            COMPUTE SLD-TOT-INICIAL-CAB = W-TOT-INICIAL-CAB * 1000
            COMPUTE SLD-TOT-SALDOS-CAB  = W-TOT-SALDOS-CAB  * 1000
            COMPUTE SLD-TOT-CUOTAS-CAB  = W-TOT-CUOTAS-CAB  * 1000
            COMPUTE SLD-TOT-MORA-CAB    = W-TOT-MORA-CAB    * 1000

            MOVE  W-NUM-OBLIG-CAV  TO  SLD-NUM-OBLIG-CAV
            COMPUTE SLD-TOT-INICIAL-CAV = W-TOT-INICIAL-CAV * 1000
            COMPUTE SLD-TOT-SALDOS-CAV  = W-TOT-SALDOS-CAV  * 1000
            COMPUTE SLD-TOT-CUOTAS-CAV  = W-TOT-CUOTAS-CAV  * 1000
            COMPUTE SLD-TOT-MORA-CAV    = W-TOT-MORA-CAV    * 1000

            MOVE  W-NUM-OBLIG-OTR    TO  SLD-NUM-OBLIG-OTR
            COMPUTE SLD-TOT-INICIAL-OTR = W-TOT-INICIAL-OTR * 1000
            COMPUTE SLD-TOT-SALDOS-OTR  = W-TOT-SALDOS-OTR  * 1000
            COMPUTE SLD-TOT-CUOTAS-OTR  = W-TOT-CUOTAS-OTR  * 1000
            COMPUTE SLD-TOT-MORA-OTR    = W-TOT-MORA-OTR    * 1000
      *------------------------------------------------------*
      *     RANGOS DE TARJETAS ACTIVAS
      *------------------------------------------------------*
            MOVE  W-TOT-NUM-TDC   TO  SLD-TOT-NUM-TDC
            COMPUTE SLD-TOT-CUPOS-TDC = W-TOT-CUPOS-TDC * 1000
            COMPUTE SLD-TOT-SALDO-TDC = W-TOT-SALDO-TDC * 1000
            IF W-TOT-CUPOS-TDC > 0
               COMPUTE W-UTLIZACION-TDC =
               (W-SAL-CUPOS-TDC * 1000) / (W-TOT-CUPOS-TDC * 1000)
               IF W-UTLIZACION-TDC > ZEROS
                  MOVE W-UTLIZACION-TDC  TO SLD-POR-UTILIZ-TDC
               END-IF
            END-IF
            COMPUTE SLD-TOT-CUOTAS-TDC = W-TOT-CUOTAS-TDC * 1000
            COMPUTE SLD-TOT-MORA-TDC   = W-TOT-MORA-TDC   * 1000
            MOVE  W-NRO-RANGO-0     TO   SLD-NRO-RANGO-0
            MOVE  W-NRO-RANGO-1     TO   SLD-NRO-RANGO-1
            MOVE  W-NRO-RANGO-2     TO   SLD-NRO-RANGO-2
            MOVE  W-NRO-RANGO-3     TO   SLD-NRO-RANGO-3
            MOVE  W-NRO-RANGO-4     TO   SLD-NRO-RANGO-4
            MOVE  W-NRO-RANGO-5     TO   SLD-NRO-RANGO-5
            MOVE  W-NRO-RANGO-6     TO   SLD-NRO-RANGO-6
            IF W-FEC-MENOR-TDC < 999999
               MOVE W-FEC-MENOR-TDC TO   SLD-FECHA-TDC
            END-IF
      *------------------------------------------------------*
      *     INFORMACION SECTOR REAL
      *------------------------------------------------------*
            MOVE  W-NUM-OBLIG-REAL TO  SLD-NUM-OBLIG-REAL
            COMPUTE SLD-TOT-INICIAL-REAL = W-TOT-INICIAL-REAL * 1000
            COMPUTE SLD-TOT-SALDOS-REAL = W-TOT-SALDOS-REAL * 1000
            COMPUTE SLD-TOT-CUOTAS-REAL = W-TOT-CUOTAS-REAL * 1000
            COMPUTE SLD-TOT-MORA-REAL   = W-TOT-MORA-REAL   * 1000
      *------------------------------------------------------*
      *     INFORMACION SECTOR TELCOS
      *------------------------------------------------------*
           MOVE W-NUM-OBLIG-TELE   TO   SLD-NUM-OBLIG-TELE
           COMPUTE  SLD-TOT-CUOTAS-TELE = W-TOT-CUOTAS-TELE * 1000
           COMPUTE  SLD-TOT-MORA-TELE   = W-TOT-MORA-TELE * 1000
      *------------------------------------------------------*
      *     INFORMACION SECTOR COOPERATIVO
      *------------------------------------------------------*
            MOVE  W-NUM-OBLIG-COOP TO  SLD-NUM-OBLIG-COOP
            COMPUTE SLD-TOT-INICIAL-COOP = W-TOT-INICIAL-COOP * 1000
            COMPUTE SLD-TOT-SALDOS-COOP = W-TOT-SALDOS-COOP * 1000
            COMPUTE SLD-TOT-CUOTAS-COOP = W-TOT-CUOTAS-COOP * 1000
            COMPUTE SLD-TOT-MORA-COOP   = W-TOT-MORA-COOP   * 1000
      *------------------------------------------------------*
      *     INFORMACION CODEUDORES
      *------------------------------------------------------*
            MOVE W-NUM-OBLIG-CODEU  TO  SLD-NUM-OBLIG-CODEU
            COMPUTE SLD-TOT-SALDOS-CODEU = W-TOT-SALDOS-CODEU * 1000
            COMPUTE SLD-TOT-CUOTAS-CODEU = W-TOT-CUOTAS-CODEU * 1000
            COMPUTE SLD-TOT-MORA-CODEU   = W-TOT-MORA-CODEU   * 1000
      *------------------------------------------------------*
      *     MORAS  ACTUALES
      *------------------------------------------------------*
            MOVE   W-CREDITOS-ALDIA    TO   SLD-CREDITOS-ALDIA
            MOVE   W-MORAS-30-ACT      TO   SLD-MORA-30
            MOVE   W-MORAS-60-ACT      TO   SLD-MORA-60
            MOVE   W-MORAS-90-ACT      TO   SLD-MORA-90
            MOVE   W-MORAS-120-ACT     TO   SLD-MORA-120
            MOVE   W-CAR-CASTIGA       TO   SLD-CAR-CASTIGA
            MOVE   W-DUD-RECAUDO       TO   SLD-DUD-RECAUDO
            MOVE   W-CUENTAS-COBRADOR  TO   SLD-CUENTAS-COBRADOR
      *------------------------------------------------------*
      *     MORAS  HISTORICAS
      *------------------------------------------------------*
            MOVE  W-MORAS-30-HIS       TO   SLD-MORA-30-HIS
            MOVE  W-MORAS-60-HIS       TO   SLD-MORA-60-HIS
            MOVE  W-MORAS-90-HIS       TO   SLD-MORA-90-HIS
            MOVE  W-MORAS-120-HIS      TO   SLD-MORA-120-HIS
            MOVE  W-CANC-MMANEJO       TO   SLD-CANC-MMANEJO
            MOVE  W-CAR-RECUPERADA     TO   SLD-CAR-RECUPERADA
      *------------------------------------------------------*
      *     MORAS  MAXIMAS POR SECTOR
      *------------------------------------------------------*
            MOVE  W-MORA-MAX-TDC      TO   SLD-MORA-MAX-TDC
            MOVE  W-MORA-MAX-CAB      TO   SLD-MORA-MAX-CAB
            MOVE  W-MORA-MAX-COOP     TO   SLD-MORA-MAX-COOP
            MOVE  W-MORA-MAX-CAV      TO   SLD-MORA-MAX-CAV
      *------------------------------------------------------*
      *     ENDEUDAMIENTO
      *------------------------------------------------------*
            IF E = 3
               MOVE  W-MAYOR-END (3) TO SLD-END-CALIF-1-TRIM
               MOVE  W-MAYOR-END (2) TO SLD-END-CALIF-2-TRIM
            ELSE
               IF E = 2
                  MOVE W-MAYOR-END (2) TO SLD-END-CALIF-1-TRIM
                  MOVE W-MAYOR-END (1) TO SLD-END-CALIF-2-TRIM
               ELSE
                  MOVE W-MAYOR-END (1) TO SLD-END-CALIF-1-TRIM
                  MOVE SPACES          TO SLD-END-CALIF-2-TRIM
               END-IF
            END-IF
      *------------------------------------------------------*
      *     CUENTA CORRIENTE
      *------------------------------------------------------*
            MOVE  W-CTAS-AHORROS        TO  SLD-CTAS-AHORRO
            MOVE  W-CTES-CORRIENTES     TO  SLD-CTES-CORRIENTES
            MOVE  W-CTES-EMBARGADAS     TO  SLD-CTES-EMBARGADAS
            MOVE  W-CTES-MAL-MANEJO     TO  SLD-CTES-MAL-MANEJO
            MOVE  W-CTES-SALDADAS       TO  SLD-CTES-SALDADAS
            MOVE  W-TOTAL-CONSULTAS     TO  SLD-TOTAL-CONSULTAS
            PERFORM 1000-ACUMULAR-ESTADISTICO
            WRITE REG-SALIDA
            ADD    1    TO  GRABADOS.

      *----------------------------------------*
      * Rutina manejo valores estadisticos     *
      *----------------------------------------*
       1000-ACUMULAR-ESTADISTICO.
           IF ENT-TIPO-ID = 1
              EVALUATE ENT-EDAD-RANGO
                    WHEN "66+  "
                         ADD  1  TO  W-EDAD-66
                    WHEN "56-65"
                         ADD  1  TO  W-EDAD-56-65
                    WHEN "46-55"
                         ADD  1  TO  W-EDAD-46-55
                    WHEN "36-45"
                         ADD  1  TO  W-EDAD-36-45
                    WHEN "29-35"
                         ADD  1  TO  W-EDAD-29-35
                    WHEN "22-28"
                         ADD  1  TO  W-EDAD-22-28
                    WHEN "18-21"
                         ADD  1  TO  W-EDAD-18-21
              END-EVALUATE
           END-IF
           PERFORM  1100-ACUMULAR-ESTADO.

      *------------------------------------------------------------*
      *    ACUMULACION PARA CLIENTES POR ESTADO
      *------------------------------------------------------------*
       1100-ACUMULAR-ESTADO.
           ADD   1   TO  W-ACUM-TIPO-ID (F, 1)
           IF ENT-TIPO-ID = 1
              IF ENT-GENERO-VAL = "M"
                 ADD  1  TO  W-ACUM-TIPO-ID (F, 2)
              ELSE
                 IF ENT-GENERO-VAL = "F"
                    ADD  1  TO  W-ACUM-TIPO-ID (F, 3)
                 ELSE
                    ADD  1  TO  W-ACUM-TIPO-ID (F, 2)
                 END-IF
              END-IF
           ELSE
              IF ENT-TIPO-ID = 4
                 ADD  1  TO  W-ACUM-TIPO-ID (F, 4)
              ELSE
                 ADD  1  TO  W-ACUM-TIPO-ID (F, 5)
              END-IF
           END-IF
           IF F = 1
              PERFORM  1200-SELECT-PEOR-ESTADO
              PERFORM  1400-SELECT-SECTOR
           END-IF.

      *------------------------------------------------------------*
      *    PEOR ESTADO ACTUAL INFORME GENERAL
      *------------------------------------------------------------*
       1200-SELECT-PEOR-ESTADO.
           IF W-CAR-CASTIGA > ZEROS
              ADD 1 TO W-TOT-CAR-CASTIGA
           ELSE
              IF W-DUD-RECAUDO > ZEROS
                 ADD 1 TO W-TOT-DUD-RECAUDO
              ELSE
                 IF W-CTES-EMBARGADAS > ZEROS
                    ADD 1 TO W-TOT-CTES-EMBARGADAS
                 ELSE
                    IF W-MORAS-120-ACT  > ZEROS
                       ADD 1 TO W-TOT-MORAS-120-ACT
                    ELSE
                       IF W-MORAS-90-ACT  > ZEROS
                          ADD 1 TO W-TOT-MORAS-90-ACT
                       ELSE
                          IF W-MORAS-60-ACT > ZEROS
                             ADD 1 TO W-TOT-MORAS-60-ACT
                          ELSE
                             IF W-MORAS-30-ACT  > ZEROS
                                ADD 1 TO W-TOT-MORAS-30-ACT
                             ELSE
                                IF W-ENDEUDA = "B" OR "C" OR
                                               "D" OR "E"
                                   ADD 1 TO W-TOT-W-ENDEUDA
                                ELSE
                                   IF W-CREDITOS-ALDIA > ZEROS OR
                                      W-ENDEUDA = "A"
                                      ADD 1 TO W-TOT-ALDIA
                                   ELSE
                                      ADD 1 TO W-TOT-SIN-CTAS
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

      *------------------------------------------------------------*
      *    PEOR ESTADO ACTUAL SECTORES
      *------------------------------------------------------------*
       1400-SELECT-SECTOR.
            IF W-CC-DR-FINA > ZEROS
               ADD 1 TO W-TOT-CC-DR-FINA
            ELSE
               IF W-MORAS-FINA > ZEROS
                  ADD 1 TO W-TOT-MORAS-FINA
               ELSE
                  IF W-ALDIA-FINA > ZEROS
                     ADD 1 TO W-TOT-ALDIA-FINA
                  END-IF
               END-IF
            END-IF
            IF W-CC-DR-REAL > ZEROS
               ADD 1 TO W-TOT-CC-DR-REAL
            ELSE
               IF W-MORAS-REAL > ZEROS
                  ADD 1 TO W-TOT-MORAS-REAL
               ELSE
                  IF W-ALDIA-REAL > ZEROS
                     ADD 1 TO W-TOT-ALDIA-REAL
                  END-IF
               END-IF
            END-IF
            IF W-CC-DR-COOP > ZEROS
               ADD 1 TO W-TOT-CC-DR-COOP
            ELSE
               IF W-MORAS-COOP > ZEROS
                  ADD 1 TO W-TOT-MORAS-COOP
               ELSE
                  IF W-ALDIA-COOP > ZEROS
                     ADD 1 TO W-TOT-ALDIA-COOP
                  END-IF
               END-IF
            END-IF
            IF W-CC-DR-TELE > ZEROS
               ADD 1 TO W-TOT-CC-DR-TELE
            ELSE
               IF W-MORAS-TELE > ZEROS
                  ADD 1 TO W-TOT-MORAS-TELE
               ELSE
                  IF W-ALDIA-TELE > ZEROS
                     ADD 1 TO W-TOT-ALDIA-TELE
                  END-IF
               END-IF
            END-IF.
      *-------------------------------------*
      * REVISA CODEUDOR                     *
      *-------------------------------------*
       1900-EVALUAR-CODEUDOR.
            MOVE  ZEROS        TO SW-CODEUDOR
            MOVE NUM-CTA-ABD07 TO TEM-NRO-CTA
            IF TEM-NRO-CTA-C17 = "C" OR "c"
               MOVE  1 TO  SW-CODEUDOR
            ELSE
               IF GARANTE-ABD07 > ZEROS AND GARANTE-ABD07 < 8 AND
                 (IND-AMPLIACION-ABD02 = 5 OR 2)
                  MOVE  1  TO  SW-CODEUDOR
               END-IF
            END-IF.

      *-------------------------------------------------*
      * EVALUA TODA CLASE DE CARTERA PARA INFORMES
      *-------------------------------------------------*
       2000-EVALUAR-CLASE-CARTERA.
           IF TAB-VIGENTES-CARTERA (NOV-REPORT-ABD07) = 1
              PERFORM 32000-PROCESAR-VALORES
              PERFORM 33000-PROCESAR-BDACUE
              ADD   1   TO   W-CREDITOS-ACTV
              IF SW-CODEUDOR = ZEROS
                 IF W-CART-TIPO(COD-CTA-ABD07) = 01
                    PERFORM 2200-SECTOR-FINANCIERO
                 ELSE
                    IF W-CART-TIPO(COD-CTA-ABD07) = 02
                       PERFORM 4200-SECTOR-REAL
                    ELSE
                       IF W-CART-TIPO(COD-CTA-ABD07) = 03
                          PERFORM 4400-SECTOR-COOPERATIVO
                       ELSE
                          IF W-CART-TIPO(COD-CTA-ABD07) = 04
                             PERFORM 4000-SECTOR-TELCOS
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 PERFORM  4600-VALORES-CODEUDORES
              END-IF
           END-IF.

      *----------------------------------------------------------------*
      * EVALUA VALORES SECTOR FINANCIERO
      *----------------------------------------------------------------*
       2200-SECTOR-FINANCIERO.
            IF COD-CTA-ABD07 = 02
               PERFORM 3000-EVALUA-TDC-ACTIVA
            ELSE
               IF COD-CTA-ABD07 = 13
                  PERFORM 2300-ACUM-VLRES-CAV
               ELSE
                  IF COD-CTA-ABD07 = 05 OR 03
                     IF TIPO-OBLIGACION-ABD07 = 3
                        PERFORM 2300-ACUM-VLRES-CAV
                     ELSE
                        PERFORM 2400-ACUM-VLRES-CAB
                     END-IF
                  ELSE
                     PERFORM 2500-ACUM-VLRES-OTROS
                  END-IF
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * VALORES CARTERA  CAV
      *----------------------------------------------------------------*
       2300-ACUM-VLRES-CAV.
            ADD  1  TO  W-NUM-OBLIG-CAV
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (2) = 1
                  ADD VALOR-INICIAL-ABD07      TO W-TOT-INICIAL-CAV
               END-IF
               IF VEC-VALOR-VALVLR (3) = 1
                  ADD VALOR-SALDO-DEUDAM-ABD07 TO W-TOT-SALDOS-CAV
                                                  W-SALDOS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-CAV
                                                  W-CUOTAS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-CAV
                                                  W-MORAS-SEC-FINA
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * VALORES CARTERA  CAB
      *----------------------------------------------------------------*
       2400-ACUM-VLRES-CAB.
            ADD  1  TO  W-NUM-OBLIG-CAB
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (2) = 1
                  ADD VALOR-INICIAL-ABD07      TO W-TOT-INICIAL-CAB
               END-IF
               IF VEC-VALOR-VALVLR (3) = 1
                  ADD VALOR-SALDO-DEUDAM-ABD07 TO W-TOT-SALDOS-CAB
                                                  W-SALDOS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-CAB
                                                  W-CUOTAS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-CAB
                                                  W-MORAS-SEC-FINA
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * VALORES CARTERA  OTRAS CUENTAS SECTOR FINANCIERO
      *----------------------------------------------------------------*
       2500-ACUM-VLRES-OTROS.
            ADD  1  TO  W-NUM-OBLIG-OTR
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (2) = 1
                  ADD VALOR-INICIAL-ABD07      TO W-TOT-INICIAL-OTR
               END-IF
               IF VEC-VALOR-VALVLR (3) = 1
                  ADD VALOR-SALDO-DEUDAM-ABD07 TO W-TOT-SALDOS-OTR
                                                  W-SALDOS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-OTR
                                                  W-CUOTAS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-OTR
                                                  W-MORAS-SEC-FINA
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * EVALUAR VALORES DE TARJETA DE CREDITO                          *
      *----------------------------------------------------------------*
       3000-EVALUA-TDC-ACTIVA.
            ADD   1    TO    W-TOT-NUM-TDC
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (2) = 1
                  IF VALOR-INICIAL-ABD07 > ZEROS
                     ADD VALOR-INICIAL-ABD07  TO W-TOT-CUPOS-TDC
                     IF VEC-VALOR-VALVLR (3) = 1
                        ADD VALOR-SALDO-DEUDAM-ABD07
                                              TO W-SAL-CUPOS-TDC
                     END-IF
                  END-IF
               END-IF
               IF VEC-VALOR-VALVLR (3) = 1
                  ADD VALOR-SALDO-DEUDAM-ABD07 TO W-TOT-SALDO-TDC
                                                  W-SALDOS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-TDC
                                                  W-CUOTAS-SEC-FINA
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-TDC
                                                  W-MORAS-SEC-FINA
               END-IF
            END-IF
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (2) = 1
                  IF VALOR-INICIAL-ABD07 = ZEROS
                     ADD   1   TO   W-NRO-RANGO-0
                  ELSE
                     IF VALOR-INICIAL-ABD07 <= 1000
                        ADD   1   TO   W-NRO-RANGO-1
                     ELSE
                        IF VALOR-INICIAL-ABD07 <= 2000
                           ADD   1   TO   W-NRO-RANGO-2
                        ELSE
                           IF VALOR-INICIAL-ABD07 <= 4000
                              ADD   1   TO   W-NRO-RANGO-3
                           ELSE
                              IF VALOR-INICIAL-ABD07 <= 8000
                                 ADD   1   TO   W-NRO-RANGO-4
                              ELSE
                                 IF VALOR-INICIAL-ABD07 <= 12000
                                    ADD   1   TO   W-NRO-RANGO-5
                                 ELSE
                                    ADD   1   TO   W-NRO-RANGO-6
                                 END-IF
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                  END-IF
               ELSE
                  ADD   1   TO   W-NRO-RANGO-0
               END-IF
            ELSE
               ADD   1   TO   W-NRO-RANGO-0
            END-IF
            MOVE  ZEROS             TO  W-FECHA-AUX
            MOVE FECHA-APERT-ABD07  TO  W-FECHA-AUX
            MOVE  W-MES-AUX         TO  W-FMILENIO-MES
            MOVE  W-ANO-AUX         TO  W-FMILENIO-ANIO
            IF W-ANO-AUX   > 60
               MOVE 19              TO  W-FMILENIO-SIGLO
            ELSE
               MOVE 20              TO  W-FMILENIO-SIGLO
            END-IF
            MOVE  W-FECHA-MILENIO   TO  W-FEC-APERTURA
            IF W-FEC-APERTURA < W-FEC-MENOR-TDC
               MOVE W-FEC-APERTURA  TO  W-FEC-MENOR-TDC
            END-IF.

      *----------------------------------------------------------------*
      * VALORES SECTOR TELCOS
      *----------------------------------------------------------------*
       4000-SECTOR-TELCOS.
            ADD  1  TO  W-NUM-OBLIG-TELE
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-TELE
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-TELE
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * VALORES CARTERA  SECTOR REAL
      *----------------------------------------------------------------*
       4200-SECTOR-REAL.
            ADD  1  TO  W-NUM-OBLIG-REAL
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (2) = 1
                  ADD VALOR-INICIAL-ABD07      TO W-TOT-INICIAL-REAL
               END-IF
               IF VEC-VALOR-VALVLR (3) = 1
                  ADD VALOR-SALDO-DEUDAM-ABD07 TO W-TOT-SALDOS-REAL
                                                  W-SALDOS-SEC-REAL
               END-IF
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-REAL
                                                  W-CUOTAS-SEC-REAL
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-REAL
                                                  W-MORAS-SEC-REAL
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * VALORES CARTERA  SECTOR COOPERATIVO
      *----------------------------------------------------------------*
       4400-SECTOR-COOPERATIVO.
            ADD  1  TO  W-NUM-OBLIG-COOP
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (2) = 1
                  ADD VALOR-INICIAL-ABD07      TO W-TOT-INICIAL-COOP
               END-IF
               IF VEC-VALOR-VALVLR (3) = 1
                  ADD VALOR-SALDO-DEUDAM-ABD07 TO W-TOT-SALDOS-COOP
                                                  W-SALDOS-SEC-COOP
               END-IF
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-COOP
                                                  W-CUOTAS-SEC-COOP
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-COOP
                                                  W-MORAS-SEC-COOP
               END-IF
            END-IF.

      *--------------------------------------------------*
      * REVISA VALORES DE CODEUDORES
      *--------------------------------------------------*
       4600-VALORES-CODEUDORES.
            ADD  1  TO  W-NUM-OBLIG-CODEU
            IF IND-VALORES-VALVLR = 1
               IF VEC-VALOR-VALVLR (3) = 1
                  ADD VALOR-SALDO-DEUDAM-ABD07 TO W-TOT-SALDOS-CODEU
               END-IF
               IF VEC-VALOR-VALVLR (1) = 1
                  ADD CUANTIA-ABD07            TO W-TOT-CUOTAS-CODEU
               END-IF
               IF VEC-VALOR-VALVLR (5) = 1
                  IF NOV-REPORT-ABD07  = 01 OR 13 OR 14 OR 15 OR 16
                     MOVE ZEROS TO VALOR-SALDO-MORAM-ABD07
                  END-IF
                  ADD VALOR-SALDO-MORAM-ABD07  TO W-TOT-MORA-CODEU
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * RECATEGORIZA EL PRODUCTO DEPENDIENDO DEL SUSCRIPTOR            *
      *----------------------------------------------------------------*
       5000-EVALUAR-PRODUCTO.
      *---------------------*
           IF COD-SUS-ABD07 = 080001
              IF TAB-VIGENTES-CARTERA (NOV-REPORT-ABD07) = 1
                 PERFORM 32000-PROCESAR-VALORES
                 PERFORM 33000-PROCESAR-BDACUE
                 IF IND-VALORES-VALVLR    = 1 AND
                    VEC-VALOR-VALVLR (11) = 1
                    IF TOTAL-CUOTAS-ABD07 > 60
                       MOVE  13  TO  COD-CTA-ABD07
                    END-IF
                 END-IF
              END-IF
           ELSE
              IF COD-SUS-ABD07 = 020151 OR 020167 OR 020370
                 MOVE   11  TO  COD-CTA-ABD07
              ELSE
                 IF COD-SUS-ABD07 = 020055 OR 020007
                    MOVE   03  TO  COD-CTA-ABD07
                 ELSE
                    IF COD-SUS-ABD07 = 240033
                       MOVE   23  TO  COD-CTA-ABD07
                    END-IF
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
      * CUENTA MORAS ACTUALES Y ESTADOS ESPECIALES                     *
      *----------------------------------------------------------------*
       6000-CUENTA-MORAS-ACTUAL-ESTADO.
            EVALUATE TAB-NOV-CART-CODINT(NOV-REPORT-ABD07)
                 WHEN 01
                      ADD  1  TO  W-CREDITOS-ALDIA
                 WHEN 12
                      ADD  1  TO  W-MORAS-30-ACT
                 WHEN 13
                      ADD  1  TO  W-MORAS-60-ACT
                 WHEN 14
                      ADD  1  TO  W-MORAS-90-ACT
                 WHEN 15
                      ADD  1  TO  W-MORAS-120-ACT
                 WHEN 09
                      ADD  1  TO  W-CAR-CASTIGA
                 WHEN 11
                      ADD  1  TO  W-DUD-RECAUDO
                 WHEN 08
                      ADD  1  TO  W-CANC-MMANEJO
                 WHEN 10
                      ADD  1  TO  W-CAR-RECUPERADA
            END-EVALUATE
            IF TAB-NOV-CART-CODINT(NOV-REPORT-ABD07) = 01
               IF W-CART-TIPO(COD-CTA-ABD07) = 01
                  ADD  1  TO  W-ALDIA-FINA
               ELSE
                  IF W-CART-TIPO(COD-CTA-ABD07) = 02
                     ADD  1  TO  W-ALDIA-REAL
                  ELSE
                     IF W-CART-TIPO(COD-CTA-ABD07) = 03
                        ADD  1  TO  W-ALDIA-COOP
                     ELSE
                        IF W-CART-TIPO(COD-CTA-ABD07) = 04
                           ADD  1  TO  W-ALDIA-TELE
                        END-IF
                     END-IF
                  END-IF
               END-IF
            ELSE
               IF TAB-NOV-CART-CODINT(NOV-REPORT-ABD07) = 12 OR 13
                                                      OR  14 OR 15
                  IF W-CART-TIPO(COD-CTA-ABD07) = 01
                     ADD  1  TO  W-MORAS-FINA
                  ELSE
                     IF W-CART-TIPO(COD-CTA-ABD07) = 02
                        ADD  1  TO  W-MORAS-REAL
                     ELSE
                        IF W-CART-TIPO(COD-CTA-ABD07) = 03
                           ADD  1  TO  W-MORAS-COOP
                        ELSE
                           IF W-CART-TIPO(COD-CTA-ABD07) = 04
                              ADD  1  TO  W-MORAS-TELE
                           END-IF
                        END-IF
                     END-IF
                  END-IF
               ELSE
                  IF TAB-NOV-CART-CODINT(NOV-REPORT-ABD07) = 09 OR 11
                     IF W-CART-TIPO(COD-CTA-ABD07) = 01
                        ADD  1  TO  W-CC-DR-FINA
                     ELSE
                        IF W-CART-TIPO(COD-CTA-ABD07) = 02
                           ADD  1  TO  W-CC-DR-REAL
                        ELSE
                           IF W-CART-TIPO(COD-CTA-ABD07) = 03
                              ADD  1  TO  W-CC-DR-COOP
                           ELSE
                              IF W-CART-TIPO(COD-CTA-ABD07) = 04
                                 ADD  1  TO  W-CC-DR-TELE
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                  END-IF
               END-IF
            END-IF.

      *-------------------------------------------*
      *    DETERMINACION DE LA MORA MAXIMA
      *-------------------------------------------*
       6200-MORA-MAXIMA-CUENTAS.
           IF COD-CTA-ABD07 = 03 OR 05
              IF TIPO-OBLIGACION-ABD07 = 3
                 IF MORA-MAXIMA-ABD07 > W-MORA-MAX-CAV
                    MOVE  MORA-MAXIMA-ABD07 TO W-MORA-MAX-CAV
                 END-IF
              ELSE
                 IF MORA-MAXIMA-ABD07 > W-MORA-MAX-CAB
                    MOVE  MORA-MAXIMA-ABD07  TO  W-MORA-MAX-CAB
                 END-IF
              END-IF
           ELSE
              IF COD-CTA-ABD07 = 02
                 IF MORA-MAXIMA-ABD07 > W-MORA-MAX-TDC
                    MOVE  MORA-MAXIMA-ABD07  TO  W-MORA-MAX-TDC
                 END-IF
              ELSE
                 IF COD-CTA-ABD07 = 13
                    IF MORA-MAXIMA-ABD07 > W-MORA-MAX-CAV
                       MOVE  MORA-MAXIMA-ABD07 TO W-MORA-MAX-CAV
                    END-IF
                 ELSE
                    IF COD-CTA-ABD07 = 06 OR 27
                       IF MORA-MAXIMA-ABD07 > W-MORA-MAX-COOP
                          MOVE  MORA-MAXIMA-ABD07 TO  W-MORA-MAX-COOP
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
           IF ADJETIVO1-ABD07 = 2  OR
              ADJETIVO2-ABD07 = 2  OR
              ADJETIVO3-ABD07 = 2
              ADD  1 TO W-CUENTAS-COBRADOR
           END-IF.

      *----------------------------------------------------------------*
      * EVALUA  SI EN EL ULTIMO AÑO TUVO ALGUN TIPO DE MORA            *
      *----------------------------------------------------------------*
       5800-CUENTA-MORAS-HISTORICAS.
            MOVE    ZEROS     TO     TOPE-V
            IF TAB-VIGENTES-CARTERA (NOV-REPORT-ABD07) = 1
               MOVE     12       TO  TOPE-V
               PERFORM  5900-CUENTA-MORAS-HIST-12MESES
            ELSE
               MOVE  ZEROS              TO  W-FECHA-AUX
               MOVE  FEC-NOVEDAD-ABD07  TO  W-FECHA-AUX
               MOVE  W-MES-AUX          TO  W-FMILENIO-MES
               MOVE  W-ANO-AUX          TO  W-FMILENIO-ANIO
               IF W-ANO-AUX   > 60
                  MOVE 19               TO  W-FMILENIO-SIGLO
               ELSE
                  MOVE 20               TO  W-FMILENIO-SIGLO
               END-IF
               MOVE  W-FECHA-MILENIO    TO  W-FEC-NOVEDAD
               MOVE    ZEROS            TO  W-MESES-NOV
               COMPUTE W-MESES-NOV =
                      (W-ANO-HOY * 12 + W-MES-HOY) -
                      (W-ANO-NOV * 12 + W-MES-NOV)
               IF W-MESES-NOV < 13
                  COMPUTE  TOPE-V = 12 - W-MESES-NOV
                  PERFORM  5900-CUENTA-MORAS-HIST-12MESES
               END-IF
            END-IF.

      *----------------------------------------------------------------*
      * EVALUA  SI EN LOS ULTIMOS 12 meses TUVO ALGUN TIPO DE MORA     *
      *----------------------------------------------------------------*
       5900-CUENTA-MORAS-HIST-12MESES.
            INSPECT W-MORAS-HISTORICAS-V REPLACING  ALL "5" BY "N"
            INSPECT W-MORAS-HISTORICAS-V REPLACING  ALL "9" BY "-"
            INSPECT W-MORAS-HISTORICAS-V REPLACING  ALL "6" BY "-"
            INSPECT W-MORAS-HISTORICAS-V REPLACING  ALL "7" BY "-"
            INSPECT W-MORAS-HISTORICAS-V REPLACING  ALL "8" BY "N"
            INSPECT W-MORAS-HISTORICAS-V REPLACING  ALL "0" BY " "
            PERFORM VARYING W-IND FROM 1 BY 1 UNTIL W-IND > TOPE-V
                  IF W-MORAS-HISTORICAS (W-IND) = "1"
                     ADD 1 TO W-MORAS-30-HIS
                  ELSE
                     IF W-MORAS-HISTORICAS (W-IND) = "2"
                        ADD 1 TO W-MORAS-60-HIS
                     ELSE
                        IF W-MORAS-HISTORICAS (W-IND) = "3"
                           ADD 1 TO W-MORAS-90-HIS
                        ELSE
                           IF W-MORAS-HISTORICAS (W-IND) = "4"
                              ADD 1 TO W-MORAS-120-HIS
                           END-IF
                        END-IF
                     END-IF
                  END-IF
            END-PERFORM.

      *----------------------------------------------------------------*
      * EVALUA SI EN LOS ULTIMOS meses TUVO ALGUN TIPO DE MORA         *
      *----------------------------------------------------------------*
       5900-CUENTA-MORAS-HISTORICAS.
            PERFORM VARYING W-IND FROM 1 BY 1 UNTIL W-IND > W-VECTOR-L
                  IF W-MORAS-HISTORICAS (W-IND) = "1"
                     ADD 1 TO W-MORAS-30-HIS
                  ELSE
                     IF W-MORAS-HISTORICAS (W-IND) = "2"
                        ADD 1 TO W-MORAS-60-HIS
                     ELSE
                        IF W-MORAS-HISTORICAS (W-IND) = "3"
                           ADD 1 TO W-MORAS-90-HIS
                        ELSE
                           IF W-MORAS-HISTORICAS (W-IND) = "4"
                              ADD 1 TO W-MORAS-120-HIS
                           END-IF
                        END-IF
                     END-IF
                  END-IF
      *
            END-PERFORM.


      *------------------------------------------------------------*
      *    ESTADISTICO  POR TIPO DE IDENTIFICACION
      *------------------------------------------------------------*
       9800-GRABAR-ESTADISTICA.
           PERFORM VARYING  F FROM 1 BY 1 UNTIL F > 3
                 INITIALIZE  REG-ESTADIS
                 IF F = 1
                    MOVE "Clientes con informacion"  TO SLD-DETALLE
                 ELSE
                    IF F = 2
                       MOVE "Clientes sin informacion" TO SLD-DETALLE
                    ELSE
                       MOVE "Inconsistencias         " TO SLD-DETALLE
                    END-IF
                 END-IF
                 MOVE  W-ACUM-TIPO-ID (F, 1) TO SLD-ESTADIS-VALOR-1
                 MOVE  W-ACUM-TIPO-ID (F, 2) TO SLD-ESTADIS-VALOR-2
                 MOVE  W-ACUM-TIPO-ID (F, 3) TO SLD-ESTADIS-VALOR-3
                 MOVE  W-ACUM-TIPO-ID (F, 4) TO SLD-ESTADIS-VALOR-4
                 MOVE  W-ACUM-TIPO-ID (F, 5) TO SLD-ESTADIS-VALOR-5
                 WRITE  REG-ESTADIS
           END-PERFORM
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL ";" BY " "
           WRITE  REG-ESTADIS
           WRITE  REG-ESTADIS
      *------------------------------------------------------------*
      *    ESTADISTICO POR RANGO DE EDAD
      *------------------------------------------------------------*
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL " " BY ";"
           INITIALIZE  REG-ESTADIS
           MOVE "18 a 21 años"    TO   SLD-DETALLE
           MOVE W-EDAD-18-21      TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "22 a 28 años"    TO   SLD-DETALLE
           MOVE W-EDAD-22-28      TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "29 a 35 años"    TO   SLD-DETALLE
           MOVE W-EDAD-29-35      TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "36 a 45 años"    TO   SLD-DETALLE
           MOVE W-EDAD-36-45      TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "46 a 55 años"    TO   SLD-DETALLE
           MOVE W-EDAD-46-55      TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "56 a 65 años"    TO   SLD-DETALLE
           MOVE W-EDAD-56-65      TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "más de 66 años"  TO   SLD-DETALLE
           MOVE W-EDAD-66         TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL ";" BY " "
           WRITE  REG-ESTADIS
           WRITE  REG-ESTADIS
      *------------------------------------------------------------*
      *    ESTADISTICO ESTADO ACTUAL
      *------------------------------------------------------------*
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL " " BY ";"
           INITIALIZE  REG-ESTADIS
           MOVE "Al día        "  TO   SLD-DETALLE
           MOVE W-TOT-ALDIA       TO   SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Mora 30       "    TO  SLD-DETALLE
           MOVE W-TOT-MORAS-30-ACT  TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Mora 60       "    TO  SLD-DETALLE
           MOVE W-TOT-MORAS-60-ACT  TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Mora 90       "    TO  SLD-DETALLE
           MOVE W-TOT-MORAS-90-ACT  TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Mora 120      "    TO  SLD-DETALLE
           MOVE W-TOT-MORAS-120-ACT TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Cartera castigada" TO  SLD-DETALLE
           MOVE W-TOT-CAR-CASTIGA   TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Dudoso recaudo   " TO  SLD-DETALLE
           MOVE W-TOT-DUD-RECAUDO   TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Endeudamiento    " TO  SLD-DETALLE
           MOVE W-TOT-W-ENDEUDA     TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Cuenta corriente Embargada" TO  SLD-DETALLE
           MOVE W-TOT-CTES-EMBARGADAS TO SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Sin cuentas activas" TO  SLD-DETALLE
           MOVE W-TOT-SIN-CTAS        TO  SLD-ESTADIS-VALOR-1
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL ";" BY " "
           WRITE  REG-ESTADIS
           WRITE  REG-ESTADIS
      *------------------------------------------------------------*
      *    ESTADISTICO ESTADO ACTUAL POR SECTOR
      *------------------------------------------------------------*
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL " " BY ";"
           INITIALIZE  REG-ESTADIS
           MOVE "Al día        "  TO  SLD-DETALLE
           MOVE W-TOT-ALDIA-FINA  TO  SLD-ESTADIS-VALOR-1
           MOVE W-TOT-ALDIA-REAL  TO  SLD-ESTADIS-VALOR-2
           MOVE W-TOT-ALDIA-COOP  TO  SLD-ESTADIS-VALOR-3
           MOVE W-TOT-ALDIA-TELE  TO  SLD-ESTADIS-VALOR-4
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Mora 30 - 120 "  TO  SLD-DETALLE
           MOVE W-TOT-MORAS-FINA  TO  SLD-ESTADIS-VALOR-1
           MOVE W-TOT-MORAS-REAL  TO  SLD-ESTADIS-VALOR-2
           MOVE W-TOT-MORAS-COOP  TO  SLD-ESTADIS-VALOR-3
           MOVE W-TOT-MORAS-TELE  TO  SLD-ESTADIS-VALOR-4
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Cart.Castig -  Dud.Recaud"  TO  SLD-DETALLE
           MOVE W-TOT-CC-DR-FINA  TO  SLD-ESTADIS-VALOR-1
           MOVE W-TOT-CC-DR-REAL  TO  SLD-ESTADIS-VALOR-2
           MOVE W-TOT-CC-DR-COOP  TO  SLD-ESTADIS-VALOR-3
           MOVE W-TOT-CC-DR-TELE  TO  SLD-ESTADIS-VALOR-4
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL ";" BY " "
           WRITE  REG-ESTADIS
           WRITE  REG-ESTADIS
      *------------------------------------------------------------*
      *    ESTADISTICO VALORES POR SECTOR
      *------------------------------------------------------------*
           INITIALIZE  REG-ESTADIS
           INSPECT REG-ESTADIS REPLACING  ALL " " BY ";"
           INITIALIZE  REG-ESTADIS
           MOVE "Sector Financiero  " TO  SLD-DETALLE
           MOVE W-SALDOS-SEC-FINA     TO  SLD-ESTADIS-VALOR-1
           MOVE W-CUOTAS-SEC-FINA     TO  SLD-ESTADIS-VALOR-2
           MOVE W-MORAS-SEC-FINA      TO  SLD-ESTADIS-VALOR-3
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Sector Real        " TO  SLD-DETALLE
           MOVE W-SALDOS-SEC-REAL     TO  SLD-ESTADIS-VALOR-1
           MOVE W-CUOTAS-SEC-REAL     TO  SLD-ESTADIS-VALOR-2
           MOVE W-MORAS-SEC-REAL      TO  SLD-ESTADIS-VALOR-3
           WRITE  REG-ESTADIS
           INITIALIZE  REG-ESTADIS
           MOVE "Sector Cooperativo " TO  SLD-DETALLE
           MOVE W-SALDOS-SEC-COOP     TO  SLD-ESTADIS-VALOR-1
           MOVE W-CUOTAS-SEC-COOP     TO  SLD-ESTADIS-VALOR-2
           MOVE W-MORAS-SEC-COOP      TO  SLD-ESTADIS-VALOR-3
           WRITE  REG-ESTADIS.


      *------------------------------*
       9900-RESUMEN-PROCESO.
      *------------------------------*
           DISPLAY "       "
           DISPLAY "REG. LEIDOS   => " LEIDOS
           DISPLAY "REG. GRABADOS => " GRABADOS
           DISPLAY "       "
           DISPLAY "                "
           MOVE   W-CON-INFORMACION   TO  W-EDITOR-CON
           MOVE   W-SIN-INFORMACION   TO  W-EDITOR-SIN
           MOVE   W-INCONSISTENCIAS   TO  W-EDITOR-INC
           MOVE   LEIDOS              TO  W-EDITOR-TOT
           DISPLAY "+------------------------------------------"
                   "---------+"
           DISPLAY "|      *** RESULTADOS DEL INFORME       ***"
                   "         |"
           DISPLAY "+----------------------------------+-------"
                   "---------+"
           DISPLAY "|     DETALLE CALIFICACION         | CALF |"
                   "  # REG. |"
           DISPLAY "+----------------------------------+------+"
                   "---------+"
           DISPLAY "| - Registros con informacion      | O.K  |"
                                                  W-EDITOR-CON
           DISPLAY "| - Registros sin informacion      | SIN  |"
                                                  W-EDITOR-SIN
           DISPLAY "| - Registros inconsistencias      | INC  |"
                                                  W-EDITOR-INC
           DISPLAY "+----------------------------------+------+"
                   "---------+"
           DISPLAY "| - Total Registro procesados             |"
                                                  W-EDITOR-TOT
           DISPLAY "+----------------------------------+------+"
                   "---------+"
           CLOSE ENTRADA
           CLOSE MAESTRO-CHECK
LEY001     CLOSE  MAESTRO-DIRECC                                        GDT0001
LEY001     CLOSE  NOAMNISTIA21                                          GDT0001
LEY001*                                                                 GDT0001
           CLOSE MAESTRO-CREDIT
           CLOSE SUSCRIPTOR
           CLOSE BDIEMSUP
           CLOSE BDIEENTI
           CLOSE INDEXADO
           CLOSE SALIDA.


REMARK******************************************************************
REMARK*  INTERCAMBIA EL VECTOR DEL ESTADO-MES Y COMPLETA EL VECTOR SI  *
REMARK*  LA FECHA DE ACTUALIZACION ES MAYOR QUE LA FECHA DE LA ULTIMA  *
REMARK*  NOVEDAD DE LA CUENTA                                          *
REMARK******************************************************************
       7500-ANALIZA-VECTOR-HISTORICO.
173600     MOVE ZEROS            TO WS-ESTADO-MES.
173700     MOVE ESTADO-MES-ABD07 TO WS-ESTADO-MES.
173800     MOVE ZEROS            TO ESTADO-MES-IMP.
173900     PERFORM 7600-MESES VARYING I FROM 1 BY 1 UNTIL I = 13.
NEWCTA     MOVE ESTADO-MES-IMP   TO WS-ESTADO-MES-1.
NEWCTA     MOVE ZEROS            TO WS-ESTADO-MES.
NEWCTA     MOVE VECTOR-2-ABD07   TO WS-ESTADO-MES.
NEWCTA     MOVE ZEROS  TO ESTADO-MES-IMP.
NEWCTA     PERFORM 7600-MESES VARYING I FROM 1 BY 1 UNTIL I = 13.
NEWCTA     MOVE ESTADO-MES-IMP   TO WS-ESTADO-MES-2.
174100     MOVE COD-SUS-ABD07    TO COD-SUSCRIP-AUX.
174200     PERFORM 7700-BUS-SUSCR.
NEWCTA     MOVE WS-ESTADO-MES-1 TO ESTADO-MES-IMP.
NORMAL     MOVE ESTADO-MES-IMP  TO VECTOR-S
           INSPECT ESTADO-MES-IMP REPLACING ALL "0" BY SPACES.
           INSPECT ESTADO-MES-IMP REPLACING ALL "5" BY "N".
           INSPECT ESTADO-MES-IMP REPLACING ALL "9" BY "-".
           IF NUEVO = 1
              INSPECT ESTADO-MES-IMP REPLACING ALL "6" BY "-"
              INSPECT ESTADO-MES-IMP REPLACING ALL "7" BY "-"
              INSPECT ESTADO-MES-IMP REPLACING ALL "8" BY "n"
           ELSE
              INSPECT ESTADO-MES-IMP REPLACING ALL "6" BY "-"
              INSPECT ESTADO-MES-IMP REPLACING ALL "7" BY "-"
              INSPECT ESTADO-MES-IMP REPLACING ALL "8" BY "n"
           END-IF.
ALFABE     MOVE ESTADO-MES-IMP  TO VECTOR-S-12
           MOVE WS-ESTADO-MES-2 TO ESTADO-MES-IMP.
NORMAL     MOVE ESTADO-MES-IMP  TO S48-VEC
           INSPECT ESTADO-MES-IMP REPLACING ALL "0" BY SPACES.
           INSPECT ESTADO-MES-IMP REPLACING ALL "5" BY "N".
           INSPECT ESTADO-MES-IMP REPLACING ALL "9" BY "-".
           INSPECT ESTADO-MES-IMP REPLACING ALL "6" BY "-".
           INSPECT ESTADO-MES-IMP REPLACING ALL "7" BY "-".
           INSPECT ESTADO-MES-IMP REPLACING ALL "8" BY "n".
ALFABE     MOVE ESTADO-MES-IMP  TO S48-VEC-12
           MOVE VECTOR-S        TO W-MORAS-HISTORICAS-V.

174800 7600-MESES.
REMARK******************************************************************
REMARK*7600-MESES.
REMARK* SE RECORRE EL VECTOR DE COMPORTAMIENTO MOVIENDO LOS CAMPOS DE  *
REMARK* ACUERDO AL REPORTE DERECHA A IZQUIERDA.                        *
REMARK******************************************************************
174900     SUBTRACT I FROM 13 GIVING J.
175000     MOVE WS-MES ( I ) TO E-MES-IMP ( J ).
175200 7700-BUS-SUSCR.
REMARK******************************************************************
REMARK*7700-BUS-SUSCR.
REMARK* SE BUSCA LA FECHA DE ULTIMA ACTUALIZACION DEL SUSCRIPTOR Y SE  *
REMARK* PROCEDE A REALIZAR CORRIMIENTOS EN CASO DE ESTAR AL DIA        *
REMARK******************************************************************
175300     PERFORM 6600-LECT-SUSCR.
175400     IF ENCONTRO = 0
000000        MOVE 1 TO IND-BLOQUEO-ABD07
175700     ELSE
175800         PERFORM 7800-COMP-VEC.
175900 7800-COMP-VEC.
REMARK*****************************************************************
REMARK*7800-COMP-VEC.
REMARK*  SE COMPARAN LAS FECHAS DE ULTIMA NOVEDAD E INGRESO DEL SUSCR.*
REMARK*  DANDO ASI EL NUMERO DE POSICIONES DEL VECTOR. EN CASO DE QUE *
REMARK*  LA ULTIMA NOVEDAD SEA 01 O 13 O 14 O 15 O 16 SE LE AUMENTAN
REMARK*  LAS POSICIONES DE NORMALIDAD QUE FALTAN HASTA LA FECHA DE LA *
REMARK*  ULTIMA ACTUALIZACION DEL SUSCRIPTOR.                         *
REMARK*****************************************************************
vmlg       MOVE 1 TO FIN-CHECK
176700     IF F-UACT-2 = 0
176800        MOVE F-ING-2 TO F-UACT-2.
176900     IF FIN-CHECK = 1
177000        MOVE FECHA-APERT-ABD07 TO F-APE-07
177100     ELSE
177200        MOVE FECHA-APERT-ABD06 TO F-APE-07.
177300     IF FIN-CHECK = 1
177400        MOVE FEC-NOVEDAD-ABD07 TO F-NOV-07
177500     ELSE
177600        MOVE FEC-NOVEDAD-ABD06 TO F-NOV-07.
           MOVE A-APE-07             TO ANO-2000                        ANO2000
           PERFORM 7800-CONVERSION-INPUT-2000                           ANO2000
           MOVE ANO-2000             TO A-APE-07                        ANO2000
           MOVE A-NOV-07             TO ANO-2000                        ANO2000
           PERFORM 7800-CONVERSION-INPUT-2000                           ANO2000
           MOVE ANO-2000             TO A-NOV-07                        ANO2000
           MOVE A-ING-02             TO ANO-2000                        ANO2000
           PERFORM 7800-CONVERSION-INPUT-2000                           ANO2000
           MOVE ANO-2000             TO A-ING-02                        ANO2000
           MOVE A-UACT-02            TO ANO-2000                        ANO2000
           PERFORM 7800-CONVERSION-INPUT-2000                           ANO2000
           MOVE ANO-2000             TO A-UACT-02                       ANO2000
177700     COMPUTE  F-APE-7 = A-APE-07 * 12 + M-APE-07.
177800     COMPUTE  F-NOV-7 = A-NOV-07 * 12 + M-NOV-07.
177900     COMPUTE  F-ING-2 = A-ING-02 * 12 + M-ING-02.
178000     COMPUTE  F-UACT-2 = A-UACT-02 * 12 + M-UACT-02.
178100     IF F-APE-7 < F-ING-2
178200        MOVE  F-ING-2 TO F-VIEJ
178300     ELSE
178400        MOVE F-APE-7 TO F-VIEJ.
178500     IF (F-NOV-7 > F-VIEJ)
178600     OR (F-NOV-7 = F-VIEJ)
178700        COMPUTE TIV = F-NOV-7 - F-VIEJ
178800     ELSE
178900        MOVE 0 TO TIV.
      * OJO LO SIGUIENTE ARREGLA CUANDO DA NEGATIVA LA RESTA !!!!!!
      * ES UNA INCONSISTENCIA EN LA BD POR FEC-NOVEDAD < FEC-INGRESO
           ADD 1 to tiv giving n
NEWCTA     perform varying i from n by 1 until i > 24
NEWCTA       IF i < 13
NEWCTA         MOVE "0" TO WS-ESTADO-MES-1(I:1)
NEWCTA       ELSE
NEWCTA         COMPUTE J = I - 12
NEWCTA         MOVE "0" TO WS-ESTADO-MES-2(J:1)
NEWCTA       END-IF
           end-perform
179000     IF (F-UACT-2 > F-VIEJ)
179100     OR (F-UACT-2 = F-VIEJ)
179200        COMPUTE TFV = F-UACT-2 - F-VIEJ + 1
179300     ELSE
179500        MOVE 0 TO TFV.
179600     IF TFV > TIV
179700        COMPUTE N = TFV - TIV - 1
179800     ELSE
179900        MOVE 0 TO N.
           MOVE A-APE-07             TO ANO-2000                        ANO2000
           PERFORM 7900-CONVERSION-OUTPUT-2000                          ANO2000
           MOVE ANO-2000             TO A-APE-07                        ANO2000
           MOVE A-NOV-07             TO ANO-2000                        ANO2000
           PERFORM 7900-CONVERSION-OUTPUT-2000                          ANO2000
           MOVE ANO-2000             TO A-NOV-07                        ANO2000
           MOVE A-ING-02             TO ANO-2000                        ANO2000
           PERFORM 7900-CONVERSION-OUTPUT-2000                          ANO2000
           MOVE ANO-2000             TO A-ING-02                        ANO2000
           MOVE A-UACT-02            TO ANO-2000                        ANO2000
           PERFORM 7900-CONVERSION-OUTPUT-2000                          ANO2000
           MOVE ANO-2000             TO A-UACT-02                       ANO2000
ARRAVE     IF (NOV-REPORT-ABD07 = 01 OR 13 OR 14 OR 16 OR 15) AND
ARRAVE        (ARRASTRE = 1)
181400        MOVE CINCO TO RELLENO
181500        MOVE F-UACT-02 TO F-AUX
181600        MOVE F-AUX TO FEC-NOV-S
NEWCTA        IF N > 24
NEWCTA           MOVE 25 TO N
NEWCTA        END-IF
181700        PERFORM  7900-ADICION N TIMES
           END-IF.
182600*
182700 7900-ADICION.
REMARK*****************************************************************
REMARK*7900-ADICION.
REMARK*  SE ADICIONA UNA N (NORMAL) AL VECTOR POR CADA VEZ QUE SE     *
REMARK*  LLAMA ESTA RUTINA.                                           *
REMARK*                                                               *
REMARK*****************************************************************
182800     MOVE ZEROS           TO WS-EST-MES.
NEWCTA     MOVE WS-ESTADO-MES-1 TO WS-MESES-VIEJ.
183000     MOVE RELLENO         TO WS-MES-NUEV.
NEWCTA     MOVE WS-MESES-NUEV   TO WS-ESTADO-MES-1.
NEWCTA     MOVE WS-MES-VIEJ     TO WS-MES-NUEV.
NEWCTA     MOVE WS-ESTADO-MES-2 TO WS-MESES-VIEJ.
NEWCTA     MOVE WS-MESES-NUEV   TO WS-ESTADO-MES-2.
157000 6600-LECT-SUSCR.
REMARK*****************************************************************
REMARK*6600-LECT-SUSCR.
REMARK*  SE LEE LA TABLA DE SUSCRIPTORES Y SE ASIGNAN LAS FECHAS DE   *
REMARK*  INGRESO Y ULTIMA ACTUALIZACION DEL SUSCRIPTOR.               *
REMARK*                                                               *
REMARK*****************************************************************
157100     MOVE COD-SUSCRIP-AUX TO COD-SUSCRIP-ABD02.
157200     MOVE 1 TO ENCONTRO.
ARRAVE     READ SUSCRIPTOR IGNORE LOCK INVALID KEY
157400          MOVE 0 TO ENCONTRO
                MOVE 0 TO IND-COR-ABD02.
           IF ENCONTRO NOT = 0
              IF IND-COR-ABD02 = 1 OR 2
                 MOVE NOM-COR1-ABD02 TO NOM-COR-AUX
                 MOVE NOM-COR2-ABD02 TO NOM-COR-AUX2
              ELSE
                 MOVE SPACES TO NOM-COR-AUX
                 MOVE COD-SUSCRIP-ABD02 TO NOM-COR-AUXN
              END-IF
157600        MOVE FECHA-ING-ABD02 TO F-ING-02
157700        MOVE F-ULT-ACT-ABD02 TO F-UACT-02.

       7900-CONVERSION-OUTPUT-2000.                                     ANO2000
REMARK******************************************************************
REMARK*7900-CONVERSION-OUTPUT-2000.                                     ANO2000
REMARK* RUTINA ESTANDAR PARA Y2K, METODO WINDOW ANO 61. CONVERSION DESPUES
REMARK* DE LA OPERACION ENTRE FECHAS.                                  *
REMARK******************************************************************
           IF ANO-2000                  <  39                           ANO2000
              ADD 61                    TO   ANO-2000                   ANO2000
           ELSE
              SUBTRACT 39               FROM ANO-2000.                  ANO2000

       7800-CONVERSION-INPUT-2000.                                      ANO2000
REMARK******************************************************************
REMARK*7800-CONVERSION-INPUT-2000.                                      ANO2000
REMARK* RUTINA ESTANDAR PARA Y2K, METODO WINDOW ANO 61. CONVERSION ANTES
REMARK* DE LA OPERACION ENTRE FECHAS.                                  *
REMARK******************************************************************
           IF ANO-2000                  >  61                           ANO2000
              SUBTRACT 61               FROM ANO-2000                   ANO2000
           ELSE                                                         ANO2000
              ADD 39                    TO   ANO-2000.                  ANO2000

LEY001  COPY  LEEMAESTROS.PROC.                                         GDT0001

      *------------------------------------------------------*
      * EVALUAR FECHAS APERTURA Y NOVEDAD                    *
      *------------------------------------------------------*
       5900-EVALUA-FECHAS.
           MOVE FECHA-APERT-ABD07  TO  W-FECHA-AUX
           IF W-ANO-AUX > 50
              MOVE 19 TO W-SGL-OK    SIG-APE
           ELSE
              MOVE 20 TO W-SGL-OK    SIG-APE
           END-IF
           MOVE W-ANO-AUX   TO  W-ANO-OK     ANO-APE
           MOVE W-MES-AUX   TO  W-MES-OK     MES-APE
           MOVE W-FECHA-OK  TO  W-FECHA-INICIAL
           MOVE FEC-NOVEDAD-ABD07  TO  W-FECHA-AUX
           IF W-ANO-AUX > 50
              MOVE 19 TO W-SGL-OK
           ELSE
              MOVE 20 TO W-SGL-OK
           END-IF
           MOVE W-ANO-AUX   TO  W-ANO-OK
           MOVE W-MES-AUX   TO  W-MES-OK
           MOVE W-FECHA-OK  TO  W-FECHA-NOVEDAD
***********
           MOVE 0  TO J
           MOVE FECHA-VENC-ABD07 TO FECHA-AUX
           MOVE FECHA-AUX(1:2) TO MES-VEN
           MOVE FECHA-AUX(3:2) TO ANO-VEN
           IF ANO-VEN < 50
                  MOVE 20 TO SIG-VEN
           ELSE
                  MOVE 19 TO SIG-VEN
           END-IF
           MOVE W-ANO-AUX   TO  W-ANO-OK
           MOVE W-MES-AUX   TO  W-MES-OK

           MOVE FECHA-VENCIMIENTO07  TO  W-FECHA-VENCIM
           IF FECHA-VENCIMIENTO07 > FECHA-APERTURA07
                  COMPUTE J = ( SIG-ANO-VEN * 12 + MES-VEN ) -
                              ( SIG-ANO-APE * 12 + MES-APE )
           END-IF
           PERFORM 030-NORMALIZAR-VECTOR
***********
           PERFORM 32000-PROCESAR-VALORES
           PERFORM 8000-EXAMINA-VALORES.

       8000-EXAMINA-VALORES.
      *---------------------------------------------------*
           IF COD-SUS-ABD07 = 020008 OR 029908
              MOVE 1 TO IND-VALORES-VALVLR
              MOVE 1 TO VEC-VALOR-VALVLR (2)
              MOVE 1 TO VEC-VALOR-VALVLR (3)
           END-IF
           IF IND-VALORES-VALVLR = 1
              IF VEC-VALOR-VALVLR (2) = 1
                 MOVE VALOR-INICIAL-ABD07    TO W-VALOR-INI
              END-IF
              IF VEC-VALOR-VALVLR (1) = 1
                 MOVE CUANTIA-ABD07          TO W-VALOR-CUOTA
              END-IF
              IF VEC-VALOR-VALVLR (3) = 1
                 MOVE VALOR-SALDO-DEUDAM-ABD07 TO W-SALDO-CARTERA
              END-IF
              IF VEC-VALOR-VALVLR (5) = 1
                 MOVE VALOR-SALDO-MORAM-ABD07 TO W-SALDO-MORA
                 IF TAB-NOV-CART-CODINT(NOV-REPORT-ABD07) = 01
                    MOVE ZERO                 TO W-SALDO-MORA
                 END-IF
              END-IF
           END-IF.


       030-NORMALIZAR-VECTOR.
      *===============================================================
REMARK*030-NORMALIZAR-VECTOR.
REMARK* Se dejan todas las fechas en formato ANO(4).MES(2)          *
      *===============================================================
           MOVE FEC-NOVEDAD-ABD07 TO FECHA-AUX.
           MOVE FECHA-AUX(1:2) TO MES-NOV.
           MOVE FECHA-AUX(3:2) TO ANO-NOV.
           IF ANO-NOV < 50
              MOVE 20          TO SIG-NOV
           ELSE
              MOVE 19          TO SIG-NOV
           END-IF.
           MOVE FECHA-ING-ABD02      TO FECHA-AUX.
           MOVE FECHA-AUX(1:2) TO MES-ING.
           MOVE FECHA-AUX(3:2) TO ANO-ING.
           IF ANO-ING < 50
              MOVE 20          TO SIG-ING
           ELSE
              MOVE 19          TO SIG-ING
           END-IF.
REMARK***************************************************************
REMARK* Se calcula si es necesario borrar información inválida      *
REMARK***************************************************************
           MOVE ESTADO-MES-ABD07 TO V-NOR4.
           MOVE VECTOR-2-ABD07   TO V-NOR3.
           MOVE ESTADO-MES-R-ABD07 TO V-NOR2.
           MOVE VECTOR-2-R-ABD07   TO V-NOR1.
           IF FECHA-APERTURA07 > FECHA-INGRESO
              MOVE FECHA-APERTURA07 TO FECHA-INGRESO
           END-IF
           IF FECHA-NOVEDAD07 > FECHA-INGRESO
              COMPUTE J = ( SIG-ANO-NOV * 12 + MES-NOV ) -
                          ( SIG-ANO-ING * 12 + MES-ING )
           ELSE
              MOVE 0 TO J
           END-IF
           IF J < 48
              COMPUTE J = 48 - J
           ELSE
              MOVE 0 TO J
           END-IF
           PERFORM UNTIL J = 0
              MOVE "0" TO V-NOR(J:1)
              SUBTRACT 1 FROM J
           END-PERFORM
REMARK*    CASTIGADAS Y DUDOSO RECAUDO
           IF NOV-REPORT-ABD07 = 45 OR 47
              IF NOV-REPORT-ABD07 = 47
                 MOVE ALL "D" TO RELLENO
              ELSE
                 MOVE ALL "C" TO RELLENO
              END-IF
              MOVE FECHA-SITUACION-ABD07 TO FECHA-SITMMAAAA-ORI
              MOVE FECHA-SITAAAA-ORI     TO FECHA-SITAAAA
              MOVE FECHA-SITMM-ORI       TO FECHA-SITMM
              MOVE FECHA-ACTUALIZ-VALOR-ABD07 TO FECHA-SITMMAAAA-ORI
              MOVE FECHA-SITAAAA-ORI     TO FECHA-VALAAAA
              MOVE FECHA-SITMM-ORI       TO FECHA-VALMM
              IF FECHA-VALAAAAMM > FECHA-SITAAAAMM
                 MOVE FECHA-VALAAAAMM TO FECHA-SITAAAAMM
              END-IF
              IF FECHA-SITAAAAMM  > FECHA-NOVEDAD07
                 COMPUTE J = ( FECHA-SITAAAA * 12 + FECHA-SITMM ) -
                             ( SIG-ANO-NOV   * 12 + MES-NOV     )
                 IF J < 48
                    PERFORM UNTIL J = 0
                       MOVE V-NOR(2:47) TO V-NOR(1:47)
                       MOVE RELLENO(1:1) TO V-NOR(48:1)
                       SUBTRACT 1 FROM J
                    END-PERFORM
                 ELSE
                    MOVE RELLENO TO V-NOR(1:48)
                 END-IF
                 MOVE  FECHA-SITAAAAMM  TO  FECHA-NOVEDAD07
              END-IF
           END-IF.
REMARK*    ADICIONAR NOVEDAD
           MOVE V-NOR(2:47) TO V-NOR(1:47)
           MOVE V-MORA(NOV-REPORT-ABD07) TO V-NOR(48:1)
REMARK*    NORMALIZAR
           IF FECHA-TAG >= FECHA-NOVEDAD07
              COMPUTE J = ( SIG-ANO-TAG * 12 + MES-TAG ) -
                          ( SIG-ANO-NOV * 12 + MES-NOV )
              IF J < 48
                 PERFORM UNTIL J = 0
                   MOVE V-NOR(2:47) TO V-NOR(1:47)
                   MOVE "0"         TO V-NOR(48:1)
                   SUBTRACT 1 FROM J
                 END-PERFORM
              ELSE
                 MOVE ALL "0"        TO V-NOR
              END-IF
           ELSE                           *> NO SE CONSIDERA
                   INITIALIZE V-NOR-AUX
                   MOVE V-NOR       TO V-NOR-AUX
                   MOVE V-NOR-AUX   TO V-NOR(2:47)
                   MOVE "0"         TO V-NOR(1:1)
           END-IF.
      *    REEMPLAZOS
           INSPECT V-NOR REPLACING ALL "0" BY "."
           INSPECT V-NOR REPLACING ALL "5" BY "0"   *> AL DIA
           INSPECT V-NOR REPLACING ALL "P" BY "0"   *> PAGO TOTAL
           INSPECT V-NOR REPLACING ALL "7" BY "."
           INSPECT V-NOR REPLACING ALL "8" BY "."
           INSPECT V-NOR REPLACING ALL "9" BY "."
           INSPECT V-NOR REPLACING ALL "M" BY "5"   *> CANCELADA MM
           INSPECT V-NOR REPLACING ALL "D" BY "6"   *> DUDOSO RECAUDO
           INSPECT V-NOR REPLACING ALL "C" BY "7"   *> CASTIGADA
           INSPECT V-NOR REPLACING ALL "R" BY "8".  *> RECUPERADA
      *===============================================================
