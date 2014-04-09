      $set mf
REMARK*TESTPGM.01.0001
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTPGM.
REMARK******************************************************************
REMARK* Programa de prueba: lee un archivo con IDs (tipo y numero), y
REMARK* con cada ID lee el registro relacionado del archivo SCODAPJA
REMARK******************************************************************
REMARK******************************************************************
       AUTHOR. GLOBANT.
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *******************************************************************
      * ARCHIVO CON UNA IDENTIFICACION DE PJ A POR REGISTRO
      *******************************************************************
           SELECT IDSPJA
                  ASSIGN TO DISK TITLE-IDSPJA
      *           ASSIGN TO DISK "Z:\DATACREDITO\DB2\idspja.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS COD-RETORNO-IDSPJA.

      *******************************************************************
      *    COPY SCODAPJA.FS.
                  SELECT SCODAPJA
                         ASSIGN TO DISK TITLE-SCODAPJA
      *                  ASSIGN TO DISK "Z:\DATACREDITO\DB2\SCODAPJA"
                         ORGANIZATION IS INDEXED
                         ACCESS MODE IS DYNAMIC
                         RECORD KEY IS LLAVE-SCODAPJA
                         FILE STATUS IS COD-RETORNO-SCODAPJA.
                  SELECT SECUENCIAL-SALIDA
                         ASSIGN TO DISK  SEC-SAL-TITLE
      *                  ASSIGN TO DISK "Z:\DATACREDITO\DB2\salscore.txt"
                         ORGANIZATION IS LINE SEQUENTIAL
                         ACCESS MODE IS SEQUENTIAL
                         FILE STATUS IS COD-RETORNO-SALIDA
                         LOCK MODE IS MANUAL WITH LOCK ON RECORD.

       DATA DIVISION.
       FILE SECTION.

      **************************************************************
      * Lista de las identificaciones de los registros de SCODAPJA
      * que debe leer este programa
      **************************************************************
       FD IDSPJA.
       01 REG-IDSPJA.
          02 TIP-ID-IDSPJA              PIC 9(01).
          02 NUM-ID-IDSPJA              PIC 9(11).

           COPY SCODAPJA.FD.

           FD SECUENCIAL-SALIDA.
           01 REG-SEC-SAL.
              02 SAL-TIP-ID         PIC X(01).
              02 SAL-NUM-ID         PIC X(11).
              02 SAL-FECHA          PIC X(08).
              02 SAL-SCORE          PIC X(03).
              02 SAL-EXCLUSION      PIC X(02).
              02 SAL-SEGMENTO       PIC X(01).

       WORKING-STORAGE SECTION.

REMARK*** PARAMETROS
REMARK***************
REMARK*** DE ENTRADA

REMARK*** VARIABLES GENERALES
REMARK***********************
       01 COD-RETORNO-IDSPJA           PIC XX VALUE SPACE.
       01 COD-RETORNO-SCODAPJA         PIC XX VALUE SPACE.
       01 COD-RETORNO-SALIDA           PIC XX VALUE SPACE.
       01 IND-IDSPJA                   PIC 9(01) VALUE ZEROS.
       01 IND-SCODAPJA                 PIC 9(01) VALUE ZEROS.
       01 TITLE-IDSPJA                 PIC X(80)
                                       VALUE "$TEMPORALES/SCOREENT".
       01 SEC-SAL-TITLE                PIC X(80)
                                       VALUE "$TEMPORALES/SCORESAL".

REMARK*** VARIABLES TIPO REGISTRO
REMARK****************************

REMARK*** FECHAS
REMARK***********

REMARK*** INDICADORAS
REMARK****************

REMARK*** CONSTANTES
REMARK***************

REMARK*** CONTADORES
REMARK***************
       01  W-LEIDOS-IDSPJA             PIC 9(12) VALUE ZERO.
       01  W-LEIDOS-SCODAPJA           PIC 9(12) VALUE ZERO.
       01  W-LEIDOS-SCODAPJA-IK        PIC 9(12) VALUE ZERO.
       01  W-GRABADOS                  PIC 9(12) VALUE ZERO.
       01  W-EDIT-CONTADOR             PIC ZZZ,ZZZ,ZZZ,ZZZ.

REMARK*** ARREGLOS
REMARK*************

REMARK*** COPYS
REMARK**********
        COPY ICFILE.WS.
      ******************************************************************
      *             COPY AREA DE LOG PARA CONTROL DE TIEMPOS           *
      ******************************************************************
        COPY PE-TIME.WS.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       0000-PROGRAMA-PRINCIPAL.
REMARK******************************************************************
REMARK*0000-PROGRAMA-PRINCIPAL.                                        *
REMARK******************************************************************
      *
           DISPLAY "                                                "
           DISPLAY "************************************************"
           DISPLAY "* PROGRAMA PRUEBA DE TIEMPO DE LECTURA DE      *"
           DISPLAY "* REGISTROS DEL ARCHIVO SCODAPJA               *"
           DISPLAY "************************************************"
           DISPLAY "*                                              *"
           DISPLAY "*           PROGRAMA : TESTPGM.CBL             *"
           DISPLAY "*                                              *"
           DISPLAY "************************************************"
           DISPLAY "                                                "
           PERFORM 10-CONTROL-TIEMPO
           PERFORM 1000-ABRIR-ARCHIVOS
           MOVE ZEROS             TO IND-IDSPJA
           PERFORM 1001-PROCESO-REGISTRO-IDSPJA
           UNTIL IND-IDSPJA = 1
           PERFORM 1003-MOSTRAR-CONTADORES
           PERFORM 1004-CERRAR-ARCHIVOS
           STOP RUN.

     ****************************************************************
      *10-CONTROL-TIEMPO
      *RUTINA PARA TOMAR LA FECHA DEL SISTEMA
      ****************************************************************
       10-CONTROL-TIEMPO.
      *
           ACCEPT FECHA-S                   FROM DATE
           ACCEPT HORA-SIS                  FROM TIME
           MOVE HORA-S                      TO HORA-P
           MOVE MINU-S                      TO MINU-P
           MOVE SEGU-S                      TO SEGU-P
           MOVE 20                          TO ANIO-SIG1
           MOVE ANIO-S                      TO ANIO-P
           MOVE MESE-S                      TO MESE-P
           MOVE DIAS-S                      TO DIAS-P.
      *
           DISPLAY " Fecha Inicio Proceso : " FECHA-PG
           DISPLAY " Hora  Inicio Proceso : " HORA-PG.
      *

      ******************************************************************
      *1000-ABRIR-ARCHIVOS.
      * APERTURA DE LOS ARCHIVOS DEL PROGRAMA
      ******************************************************************
       1000-ABRIR-ARCHIVOS.
           OPEN INPUT IDSPJA
           IF COD-RETORNO-IDSPJA <> "00"
               DISPLAY "Error Abriendo el Archivo IDSPJA "
               COD-RETORNO-IDSPJA
               STOP RUN
           END-IF.

           OPEN INPUT SCODAPJA
           IF COD-RETORNO-SCODAPJA <> "00"
               DISPLAY "Error Abriendo el Archivo SCODAPJA "
               COD-RETORNO-SCODAPJA
               STOP RUN
           END-IF.

           OPEN OUTPUT SECUENCIAL-SALIDA
           IF COD-RETORNO-SALIDA NOT = "00"
              DISPLAY "Error Abriendo el Archivo SECUENCIAL SALIDA"
              COD-RETORNO-SALIDA
              STOP RUN
           END-IF.


      ******************************************************************
      *1001-PROCESO-REGISTRO-IDSPJA.
      * Lee un registro de IDSPJA con un ID, usa ese ID para leer un
      * registro de SCODAPJA y mostrarlo
      ******************************************************************
       1001-PROCESO-REGISTRO-IDSPJA.
           READ IDSPJA NEXT RECORD
               AT END MOVE 1              TO IND-IDSPJA
           END-READ.
           IF IND-IDSPJA NOT = 1
              ADD 1 TO W-LEIDOS-IDSPJA
              PERFORM 1002-PROCESO-SCODAPJA
           END-IF.

      ******************************************************************
      *1002-PROCESO-SCODAPJA
      * Hay un ID leido en el archivo IDSPJA, hay que leer el registro
      * que le corresponde del archivo SCOSAPJA y mostrarlo
      ******************************************************************
       1002-PROCESO-SCODAPJA.

           MOVE ZEROS      TO IND-SCODAPJA
           MOVE TIP-ID-IDSPJA    TO TIPIDE-SCODAPJA
           MOVE NUM-ID-IDSPJA    TO NUMIDE-SCODAPJA
           READ SCODAPJA INVALID KEY
                ADD 1 TO    W-LEIDOS-SCODAPJA-IK
                MOVE 1 TO   IND-SCODAPJA
           END-READ

           IF IND-SCODAPJA NOT = 1
              ADD 1 TO W-LEIDOS-SCODAPJA
              PERFORM 5000-GRABAR-SALIDA
           END-IF.


      ******************************************************************
      *5000-GRABAR-SALIDA.
      *GRABAR ARCHIVO SECUENCIAL SALIDA CON REGISTROS ENCONTRADOS EN BD
      ******************************************************************
       5000-GRABAR-SALIDA.
           INITIALIZE REG-SEC-SAL
           MOVE REG-SCODAPJA              TO REG-SEC-SAL
           ADD 1                          TO W-GRABADOS
           WRITE REG-SEC-SAL
           END-WRITE.

      ****************************************************************
      *1003-MOSTRAR-CONTADORES.
      *MOSTRAR CONTADORES DEL PROCESO
      ****************************************************************
       1003-MOSTRAR-CONTADORES.
           MOVE W-LEIDOS-IDSPJA      TO W-EDIT-CONTADOR
           DISPLAY "IDs leidos .............. "   W-EDIT-CONTADOR
           MOVE W-LEIDOS-SCODAPJA    TO W-EDIT-CONTADOR
           DISPLAY "SCODAPJA leídos ......... "  W-EDIT-CONTADOR
           MOVE W-LEIDOS-SCODAPJA-IK TO W-EDIT-CONTADOR
           DISPLAY "SCODAPJA claves inválidas "  W-EDIT-CONTADOR.
           MOVE W-GRABADOS           TO W-EDIT-CONTADOR
           DISPLAY "SALIDA grabados "   W-EDIT-CONTADOR.


      ******************************************************************
      *1004-CERRAR-ARCHIVOS.
      *SE CIERRAN LOS ARCHIVOS QUE SE UTILIZAN EN EL PROGRAMA
      ******************************************************************
       1004-CERRAR-ARCHIVOS.
           PERFORM 10-CONTROL-TIEMPO
           DISPLAY " Fecha Fin    Proceso : " FECHA-PG
           DISPLAY " Hora  Fin    Proceso : " HORA-PG.
           CLOSE IDSPJA SCODAPJA SECUENCIAL-SALIDA.
