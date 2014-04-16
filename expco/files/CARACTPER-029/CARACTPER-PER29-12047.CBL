      * CARACTPER-PER29.V01.001
       IDENTIFICATION DIVISION.
      * ******************************************************** *
      * GRUPO:   COMPUTEC-DATACREDITO.                           *
      * FECHA ULTIMA MODIFICACION: 2012/05/08                    *
      * ESTE PROGRAMA EJECUTA LA RUTINA DE CARACTERISTICAS PARA  *	
      * LAS IDENTIFICACIONES DEL ARCHIVO DE ENTRADA              *
      * EL PROGRAMA DEBE CAPTURAR LOS SIGUIENTES DATOS:          *
      * FECHA RECODIGA BASE DE DATOS YYYYMM (HISTORICO San_ata)  *
      * INDICADOR PARA GENERAR CARACTERISTICAS NUM 1-SI 0-NO     *
      * INDICADOR PARA GENERAR CARACTERISTICAS INQ 1-SI 0-NO     *
      * INDICADOR PARA GENERAR CARACTERISTICAS MOR 1-SI 0-NO     *
      * INDICADOR PARA GENERAR CARACTERISTICAS EXP 1-SI 0-NO     *
      * INDICADOR PARA GENERAR CARACTERISTICAS DEM 1-SI 0-NO     *
      * INDICADOR PARA GENERAR CARACTERISTICAS ACP 1-SI 0-NO     *
      * INDICADOR PARA GENERAR CARACTERISTICAS END 1-SI 0-NO     *
      * ******************************************************** *
       PROGRAM-ID. CARACTPER-PER29.
       AUTHOR. IVAN MAURICIO SALAMANCA GIL.
       DATE-WRITTEN.  2012/07/12.
       DATE-COMPILED. HOY.
      ******************************************************************
      * IMS0001    | RIPLAY. PERSONALIZACION 29.                       *
      * 24-MAY-2013|    REQ. CQ. 12047.                                *
      * M.SALAMANCA|                                                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY  ICBCRE.FS.
           COPY  BDIEMSUP0001.FS.
           COPY  ICBCRE2.FS.
           COPY  ICBSUS.FS.
           COPY  ICBCHK0001.FS.
           COPY  ICTCAD.FS.
           COPY  ICBDIR.FS.
           COPY  BDACSV.FS.
           COPY  NOAMNISTIA21.FS.
           COPY  ICBCON.FS.
           COPY  BDIIREGI.FS.
           COPY  BDIIDAS.FS.
           COPY  BDIIDIAN.FS.
           COPY  BDLSTR.FS.
           COPY  ICMVLR.FS.
           COPY  ICSALMIN.FS.
           COPY  ICSANATA.FS.
           COPY  DCSRARCH.FS.
           COPY  BDACUE.FS.

           SELECT  ENTRADA            ASSIGN TO ARCH-ENT-NAME
                   ORGANIZATION   IS  LINE  SEQUENTIAL.

           SELECT  SALIDA-PER29-RPL ASSIGN TO SALIDA-PER29
                   ORGANIZATION   IS  LINE  SEQUENTIAL.


       DATA DIVISION.
      *
       FILE SECTION.
       COPY ICBSUS.FD.
       COPY ICBCRE.FD.
       COPY ICBCRE2.FD.
       COPY BDIEMSUP.FD.
       COPY ICBCHK.FD.
       COPY ICTCAD.FD.
       COPY ICBDIR0001.FD.
       COPY BDACSV.FD.
       COPY NOAMNISTIA21.FD.
       COPY ICBCON001.FD.
       COPY BDIIREGI.FD.
       COPY BDIIDAS.FD.
       COPY BDIIDIAN.FD.
       COPY BDLSTR.FD.
       COPY ICMVLR.FD.
       COPY ICSALMIN.FD.
       COPY ICSANATA.FD.
       COPY DCSRARCH.FD.
       COPY BDACUE.FD.

      * ******************************************************* *
      *               DESCRIPCION ARCHIVO DE SALIDA             *
      * ******************************************************* *
       FD  ENTRADA.
       01  REG-ENTRADA.
         03  ENT-TIPO-ID               PIC  9(01).
         03  ENT-NRO-ID                PIC  9(11).
      *
       FD  SALIDA-PER29-RPL.
       01  REG-SLD-PER29-RPL.
         03  TIP-ID-PER29-RPL          PIC  9(01).
         03  NUM-ID-PER29-RPL          PIC  9(11).
         03  SLD-01END999RO029         PIC  X(11).
         03  SLD-01END998RO029         PIC  X(11).
         03  SLD-01END997RO029         PIC  X(11).
         03  SLD-01END996RO029         PIC  X(11).
         03  SLD-01END995RO029         PIC  X(11).
         03  SLD-01END994RO029         PIC  X(11).
         03  SLD-01END993RO029         PIC  X(11).
         03  SLD-01END992RO029         PIC  X(11).
         03  SLD-01END991RO029         PIC  X(11).
         03  SLD-01END990RO029         PIC  X(11).
         03  SLD-01END989RO029         PIC  X(04).
         03  SLD-01NUM998RO029         PIC  X(04).
         03  SLD-01NUM997RO029         PIC  X(04).
         03  SLD-01NUM996RO029         PIC  X(04).
         03  SLD-02NUM995TO029         PIC  X(04).
         03  SLD-01EXP999RO029         PIC  X(04).
         03  SLD-01EXP998RO029         PIC  X(04).
         03  SLD-01EXP997RO029         PIC  X(04).
         03  SLD-01EXP996RO029         PIC  X(04).
         03  SLD-02EXP995TO029         PIC  X(04).
         03  SLD-02EXP994TO029         PIC  X(04).
           
      *
      * ******************************************************* *
       WORKING-STORAGE SECTION.
      * ******************************************************* *
       01  IND-ARCHIVOS.
         03  IND-FIN-ARCHIVO           PIC  9  VALUE  0.

       01  CONTADORES.
         03  CON-NO-ENC                PIC  9(08)  VALUE 0.
         03  CON-LEIDOS                PIC  9(08)  VALUE 0.
         03  CON-GRABADOS              PIC  9(08)  VALUE 0.
         03  MUESTRA                   PIC  9(08)  VALUE 0.

       01 SWITCHES.
         03  SW-ERROR                  PIC  9(01)  VALUE 0.
         03  SW-IND-FRENADOS           PIC  9(01)  VALUE 0.
         03  W-ITERA                   PIC  9(01)  VALUE 0.

       01 INDICES.
         03  I                         PIC  9(2).

       01 FECHA-SISTEMA.
         03  FECHA-SIST-YYYY           PIC  9(04).
         03  FECHA-SIST-MM             PIC  9(02).
         03  FECHA-SIST-DD             PIC  9(02).
       01 TEMPORALES.
          03  W-FIN-ENTRADA            PIC  9(01)  VALUE ZEROS.

       01  ARCHIVOS.
         03 ARCH-ENT-NAME              PIC  X(100) VALUE SPACES.
         03 SALIDA-PER29               PIC  X(100) VALUE SPACES.

       01  LINEA-COMANDO               PIC  X(100) VALUE SPACES.
       01  COMANDO                     PIC  X(100) VALUE SPACES.

       01  CR-FILL                     PIC  X VALUE x"00".
       01  COMANDO-SYST.
         03  W-COMANDO-SYST            PIC  X(100) VALUE SPACES.

       01  ENV-VALUE                   PIC  X(100).
       01  ENV-NAME                    PIC  X(05)  VALUE "EXTFH".
       01  PROGRAMA-EMAIL              PIC  X(10)  VALUE  SPACES.

      * ******************************************************** *
       COPY  ICFILE.WS.
       COPY  LEEMAESTROS.WS.
       COPY  LEEICMVLR.WS.
       COPY  CARACTTEM.WS.
       COPY  CARACT.WS.
       COPY  CARACTPER.WS.
       COPY  CARACTCAL.WS.
       COPY  DCSRARCH.WS.
       COPY  VALVLR.WS.
       COPY  PESPECIAL.WS.
       COPY  UTLBIT.WS.
      *
      $SET CONSTANT HACERMATCH"NO"
      $SET CONSTANT AMNISTIA21"SI"
      * ******************************************************** *
       PROCEDURE DIVISION.
      * ******************************************************** *
       0010-PROGRAMA-PRINCIPAL.
            MOVE "CARACTPER29-RPL"     TO  PROGRAMA-EMAIL
            ACCEPT   LINEA-COMANDO   FROM  COMMAND-LINE
            UNSTRING LINEA-COMANDO DELIMITED BY ALL " "
                                     INTO  FECHA-RUN-CARACT 
                                           ARCH-ENT-NAME
            STRING  ARCH-ENT-NAME                    DELIMITED BY SPACES
                   "-PER29-12047"                    DELIMITED BY SIZE
                                     INTO  SALIDA-PER29
            END-STRING
            ACCEPT  FECHA-SISTEMA    FROM DATE YYYYMMDD
            DISPLAY "FECHA SISTEMA : " FECHA-SISTEMA
            IF  FECHA-RUN-CARACT  =  ZEROS
              MOVE  FECHA-SISTEMA      TO  FECHA-RUN-CARACT
            END-IF
            MOVE  1                    TO  GEN-MOR-CARACT
                                           GEN-NUM-CARACT
                                           GEN-INQ-CARACT
                                           GEN-EXP-CARACT
                                           GEN-DEM-CARACT
                                           GEN-ACP-CARACT
                                           GEN-END-CARACT
            MOVE  29                   TO  NUM-PERSO-CARACT
            PERFORM 1000-CARACTERISTICAS
            STOP RUN.

REMARK******************************************************************
       1000-CARACTERISTICAS.
              MOVE  0                  TO  W-FIN-ENTRADA.
              PERFORM  0020-APERTURA-ARCHIVOS
              PERFORM  1000-PROCESAR
              PERFORM  0060-CERRAR-ARCHIVOS
              PERFORM  0100-DISPLAYS-PROGRAMA
              STOP RUN.

REMARK******************************************************************
       0020-APERTURA-ARCHIVOS.
            OPEN INPUT  SUSCRIPTOR INDEXADO ENTRADA
            OPEN INPUT  MAESTRO-CREDIT MAESTRO-CHECK
            OPEN INPUT  BDIEMSUP
            OPEN INPUT  MAESTRO-DIRECC NOAMNISTIA21
            OPEN INPUT  BDIIREGI BDIIDAS .
            OPEN INPUT  BDIIDIAN.
            OPEN INPUT  BDLSTR.
            IF NUM-PERSO-CARACT = 29
               OPEN OUTPUT SALIDA-PER29-RPL
            END-IF
            PERFORM  1000-ABRIR-DCSRARCH.

       1000-PROCESAR.
      * ******************************************************** *
      *1000-PROCESAR.
      * ******************************************************** *
           PERFORM UNTIL W-FIN-ENTRADA = 1
            READ ENTRADA NEXT RECORD AT END MOVE 1 TO W-FIN-ENTRADA
            END-READ
            IF  W-FIN-ENTRADA = 0
              ADD   1                  TO  CON-LEIDOS
              MOVE  ENT-TIPO-ID        TO  TIPO-ID-CARACT
              MOVE  ENT-NRO-ID         TO  NRO-ID-CARACT
              PERFORM  4299-INICIALIZE-CARACTBC
              PERFORM  1000-CALCULAR-CARACTERISTICAS
              IF NUM-PERSO-CARACT = 29
      *
                 INITIALIZE  REG-SLD-PER29-RPL
                 MOVE  ENT-TIPO-ID       TO  TIP-ID-PER29-RPL
                 MOVE  ENT-NRO-ID        TO  NUM-ID-PER29-RPL
                 MOVE  01END999RO029     TO  SLD-01END999RO029
                 MOVE  01END998RO029     TO  SLD-01END998RO029
                 MOVE  01END997RO029     TO  SLD-01END997RO029
                 MOVE  01END996RO029     TO  SLD-01END996RO029
                 MOVE  01END995RO029     TO  SLD-01END995RO029
                 MOVE  01END994RO029     TO  SLD-01END994RO029
                 MOVE  01END993RO029     TO  SLD-01END993RO029
                 MOVE  01END992RO029     TO  SLD-01END992RO029
                 MOVE  01END991RO029     TO  SLD-01END991RO029
                 MOVE  01END990RO029     TO  SLD-01END990RO029
                 MOVE  01END989RO029     TO  SLD-01END989RO029
                 MOVE  01NUM998RO029     TO  SLD-01NUM998RO029
                 MOVE  01NUM997RO029     TO  SLD-01NUM997RO029
                 MOVE  01NUM996RO029     TO  SLD-01NUM996RO029
                 MOVE  02NUM995TO029     TO  SLD-02NUM995TO029
                 MOVE  01EXP999RO029     TO  SLD-01EXP999RO029
                 MOVE  01EXP998RO029     TO  SLD-01EXP998RO029
                 MOVE  01EXP997RO029     TO  SLD-01EXP997RO029
                 MOVE  01EXP996RO029     TO  SLD-01EXP996RO029
                 MOVE  02EXP995TO029     TO  SLD-02EXP995TO029
                 MOVE  02EXP994TO029     TO  SLD-02EXP994TO029
                 WRITE REG-SLD-PER29-RPL
      *
                 ADD   1                   TO  CON-GRABADOS
              END-IF
            END-IF
            IF  CON-LEIDOS(3:6) = ZEROS
              DISPLAY "LEIDOS: "    CON-LEIDOS
              DISPLAY "GRABADOS: "  CON-GRABADOS
            END-IF
           END-PERFORM.

      * *********************************************************** *
       0060-CERRAR-ARCHIVOS.
            PERFORM  1000-CERRAR-DCSRARCH.
            CLOSE ENTRADA SUSCRIPTOR
            CLOSE MAESTRO-DIRECC NOAMNISTIA21 INDEXADO
            CLOSE MAESTRO-CREDIT MAESTRO-CHECK
            CLOSE BDIIREGI BDIIDAS .
            IF NUM-PERSO-CARACT = 29
               CLOSE SALIDA-PER29-RPL
            END-IF.

REMARK******************************************************************
       2000-CEDULAS.
           OPEN INPUT MAESTRO-CREDIT.
           OPEN OUTPUT ENTRADA.
           READ MAESTRO-CREDIT NEXT AT END MOVE 1 TO W-FIN-ENTRADA.
           PERFORM UNTIL MUESTRA > 10000  OR W-FIN-ENTRADA = 1
             IF ENT-TIPO-ID <> TIPO-ID-ABD07 OR
                ENT-NRO-ID  <> NUM-ID-ABD07
                MOVE TIPO-ID-ABD07 TO ENT-TIPO-ID
                MOVE NUM-ID-ABD07  TO ENT-NRO-ID
                ADD 1 TO MUESTRA
                WRITE REG-ENTRADA
              END-IF
              READ MAESTRO-CREDIT NEXT AT END
                   MOVE 1 TO W-FIN-ENTRADA
              END-READ
           END-PERFORM.
           CLOSE MAESTRO-CREDIT.
           CLOSE ENTRADA.

       COPY  CARACT.PROC.
       COPY  CARACTTEM.PROC.
       COPY  CARACTPER.PROC.
       COPY  CARACTCAL.PROC.
       COPY  LEEMAESTROS.PROC.
       COPY  LEEICMVLR.PROC.
       COPY  DCSRARCH.PROC.
       COPY  PE-VALORES.PROC.
       COPY  VALVLR.PROC.
       COPY  UTLBIT.PROC.


      * *********************************************************** *
       0100-DISPLAYS-PROGRAMA.
              DISPLAY " "
              DISPLAY " "

              DISPLAY "REG. LEIDOS      => "
              DISPLAY CON-LEIDOS

              DISPLAY "REG. GRABADOS      => "
              DISPLAY CON-GRABADOS

              DISPLAY " "
              DISPLAY " ".
      * ************************************************************ *
