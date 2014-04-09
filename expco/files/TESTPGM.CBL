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
????              ASSIGN TO DISK TITLE-IDSPJA
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS COD-RETORNO-IDSPJA.

      *******************************************************************
           COPY SCODAPJA.FS.
delete*******************************************************************
delete* ARCHIVO CON LOS SCORES POR IDENTIFICACION DE PJ A              
delete*******************************************************************
delete*    SELECT SCODAPJA
delete*           ASSIGN TO DISK TITLE-SCODAPJA   
delete*           ORGANIZATION IS INDEXED
delete*           ACCESS MODE IS DYNAMIC
delete*           RECORD KEY IS LLAVE-SCODAPJA
delete*           FILE STATUS IS COD-RETORNO-SCODAPJA
delete*           LOCK MODE IS MANUAL WITH LOCK ON RECORD.
 
 
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
delete**************************************************************
delete* ARCHIVO QUE MANTIENE LOS SCORES POR IDENTIFICACION POR MES
delete* DE UNA AÑO COMPLETO.
delete**************************************************************
delete*FD SCODAPJA.
delete*01 REG-SCODAPJA.
delete*   02 LLAVE-SCODAPJA.
delete*      03 TIPIDE-SCODAPJA        PIC 9.
delete*      03 NUMIDE-SCODAPJA        PIC 9(11).
delete*   02 INORMACION-SCODAPJA.
delete*      03 FECHA-SCODAPJA         PIC X(08).
delete*      03 SCORE-SCODAPJA         PIC 9(03).
delete*      03 EXCLUSION-SCODAPJA     PIC 9(02).
delete*      03 SEGMENTO-SCODAPJA      PIC 9(01).
delete*      03 FILLER-SCODAPJA        PIC 9(12).


       WORKING-STORAGE SECTION. 

REMARK*** PARAMETROS
REMARK***************
REMARK*** DE ENTRADA

REMARK*** VARIABLES GENERALES
REMARK***********************
       01 COD-RETORNO-IDSPJA           PIC XX VALUE SPACE.
       01 COD-RETORNO-SCODAPJA         PIC XX VALUE SPACE.


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
       01  W-EDIT-CONTADOR             PIC ZZZ,ZZZ,ZZZ,ZZZ.
             
REMARK*** ARREGLOS   
REMARK*************

REMARK*** COPYS
REMARK**********
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
           PERFORM 1001-PROCESO-REGISTRO-IDSPJA
           UNTIL COD-RETORNO-IDSPJA NOT EQUAL "00"
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

      ******************************************************************        
      *1001-PROCESO-REGISTRO-IDSPJA.                 
      * Lee un registro de IDSPJA con un ID, usa ese ID para leer un
      * registro de SCODAPJA y mostrarlo
      ****************************************************************** 
       1001-PROCESO-REGISTRO-IDSPJA.                 
           READ IDSPJA
           EVALUATE COD-RETORNO-IDSPJA
           WHEN "00"
               ADD 1 TO W-LEIDOS-IDSPJA
               PERFORM 1002-PROCESO-SCODAPJA
           WHEN "10"
               NEXT SENTENCE
           WHEN OTHER
               DISPLAY "Error leyendo el archivo IDSPJA "
               COD-RETORNO-IDSPJA
               PERFORM 1003-MOSTRAR-CONTADORES          
               STOP RUN 
           END-IF.


      ******************************************************************
      *1002-PROCESO-SCODAPJA
      * Hay un ID leido en el archivo IDSPJA, hay que leer el registro
      * que le corresponde del archivo SCOSAPJA y mostrarlo
      ******************************************************************
       1002-PROCESO-SCODAPJA.

????       MOVE REG-IDSPJA TO LLAVE-SCODAPJA
           READ SCODAPJA
           EVALUATE COD-RETORNO-SCODAPJA
           WHEN "00"
               ADD 1 TO W-LEIDOS-SCODAPJA
               PERFORM 1002-PROCESO-SCODAPJA
           WHEN "23"
               ADD 1 TO W-LEIDOS-SCODAPJA-IK
               NEXT SENTENCE
           WHEN OTHER
               DISPLAY "Error leyendo el archivo SCODAPJA "
               COD-RETORNO-SCODAPJA
               PERFORM 1003-MOSTRAR-CONTADORES          
               STOP RUN 
           END-IF.

      ****************************************************************
      *1003-MOSTRAR-CONTADORES.
      *MOSTRAR CONTADORES DEL PROCESO 
      ****************************************************************
       1003-MOSTRAR-CONTADORES.
           MOVE W-LEIDOS-IDSPJA TO W-EDIT-CONTADOR
           DISPLAY "IDs leidos .............. "  W-EDIT-CONTADOR
           MOVE W-LEIDOS-SCODAPJA TO W-EDIT-CONTADOR
           DISPLAY "SCODAPJA leídos ......... "  W-EDIT-CONTADOR                      
           MOVE W-LEIDOS-SCODAPJA-IK TO W-EDIT-CONTADOR
           DISPLAY "SCODAPJA claves inválidas "  W-EDIT-CONTADOR.
           
      ******************************************************************
      *1004-CERRAR-ARCHIVOS.                           
      *SE CIERRAN LOS ARCHIVOS QUE SE UTILIZAN EN EL PROGRAMA        
      ******************************************************************
       1004-CERRAR-ARCHIVOS.  
           PERFORM 10-CONTROL-TIEMPO
           DISPLAY " Fecha Fin    Proceso : " FECHA-PG
           DISPLAY " Hora  Fin    Proceso : " HORA-PG.
           CLOSE IDSPJA SCODAPJA.
