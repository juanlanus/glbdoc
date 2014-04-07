      $set mf
REMARK*OPABDIIRE.01.0001
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    OPABDIIRE.
REMARK******************************************************************
REMARK* ESTE  PROGRAMA   TIENE COMO FUNCION ACTUALIZAR UNA CANTIDAD DE *
REMARK* REGISTROS EN EL ARCHIVO BDIIREGI.                              *
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
 
      * BDIIREGI
      *===============================================================
      * ESTRUCTURA DEL ARCHIVO ENTREGADO POR LA REGISTRADURIA NACIONAL
      *===============================================================
             SELECT BDIIREGI 
      *         ASSIGN TO DISK "/despeciales2/temporales/BDIIREGI/"
      *         ASSIGN TO DISK "Z:\OPTIMIZACION\BDIIRE\BDIIRE"
                ASSIGN TO EXTERNAL BDIIRE
                ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC
                RECORD KEY IS LLAVE-BDIIREGI
                FILE STATUS W-STATUS-BDIIREGI.
 
       DATA DIVISION.
       FILE SECTION.
       COPY BDIIREGI.FD.
       WORKING-STORAGE SECTION.


REMARK*** PARAMETROS
REMARK***************
REMARK*** DE ENTRADA

REMARK*** VARIABLES GENERALES
REMARK***********************
       01 W-STATUS-BDIIREGI            PIC XX VALUE SPACE.
       01 W-EDITCON                    PIC ZZZ,ZZZ,ZZZ,ZZZ.
       01 W-CEDULA-BDIIREGI            PIC 9(11).


REMARK*** VARIABLES TIPO REGISTRO
REMARK****************************

REMARK*** FECHAS
REMARK***********

REMARK*** INDICADORAS
REMARK****************
       01 SW-ACTUAL-REGIS                  PIC 9(01) VALUE ZEROS.

REMARK*** CONSTANTES
REMARK***************

REMARK*** CONTADORES
REMARK***************
       01  W-ACTUALIZADOS                  PIC  9(12) VALUE ZEROS.
       01  W-NO-ACTUALIZADOS               PIC  9(12) VALUE ZEROS.      
             
REMARK*** ARREGLOS   
REMARK*************

REMARK*** COPYS
REMARK**********
      ******************************************************************
      *             COPY AREA DE LOG PARA CONTROL DE TIEMPOS           *
      ******************************************************************
      *
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
           DISPLAY "*PROGRAMA PARA REALIZAR ACTUALIZACION DE       *"
           DISPLAY "*REGISTROS EN EL ARCHIVO BDIIREGI              *"
           DISPLAY "************************************************"
           DISPLAY "*                                              *"
           DISPLAY "*           PROGRAMA : OPABDIIRE.CBL           *"
           DISPLAY "*                                              *"
           DISPLAY "************************************************"
           DISPLAY "                                                "
           PERFORM 10-CONTROL-TIEMPO
      *
           DISPLAY " Fecha Inicio Proceso : " FECHA-PG
           DISPLAY " Hora  Inicio Proceso : " HORA-PG.
      *           
           PERFORM 1000-ABRIR-ARCHIVOS
           PERFORM 1001-ACTUALIZAR-BDIIREGI
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
          
      ******************************************************************        
      *1000-ABRIR-ARCHIVOS.                           
      *SE ABREN LOS ARCHIVOS QUE SE UTILIZAN EN EL PROGRAMA        
      ****************************************************************** 
       1000-ABRIR-ARCHIVOS.                 
           OPEN I-O  BDIIREGI
           IF W-STATUS-BDIIREGI <> "00"
              DISPLAY "Error Abriendo el Archivo BDIIRE  "
              W-STATUS-BDIIREGI
              STOP RUN
           END-IF.

      ******************************************************************        
      *1001-ACTUALIZAR-BDIIREGI.                           
      *ACTUALIZACION REGISTROS ARCHIVO BDIIREGI
      ****************************************************************** 
       1001-ACTUALIZAR-BDIIREGI.                 
           MOVE ZEROS                     TO W-ACTUALIZADOS 
                                             W-NO-ACTUALIZADOS
                                             W-CEDULA-BDIIREGI
           PERFORM 1000000 TIMES 
               PERFORM 1002-MOVER-A-REGIS
               REWRITE REG-BDIIREGI INVALID KEY        
                 MOVE 1 TO SW-ACTUAL-REGIS        
               END-REWRITE                                           
               IF SW-ACTUAL-REGIS > ZEROS                           
                  DISPLAY "!ERROR: EN ACTUALIZACION DE REGIS !"                
                       TIPO-ID-BDIIREGI " " CEDULA-BDIIREGI
                  ADD 1                   TO W-NO-ACTUALIZADOS
               ELSE        
                  ADD 1                   TO W-ACTUALIZADOS        
               END-IF        
            END-PERFORM.  

      ******************************************************************
      *1002-MOVER-A-REGIS                                              *
      *  MOVER VARIABLES DE TRABAJO A REG MAESTRO DE REGISTRADURIA     *
      ******************************************************************
       1002-MOVER-A-REGIS.       
           INITIALIZE REG-BDIIREGI
           MOVE ZEROS                  TO SW-ACTUAL-REGIS
           MOVE 9                      TO TIPO-ID-BDIIREGI                                       
           IF W-CEDULA-BDIIREGI = ZEROS
               MOVE 999999             TO W-CEDULA-BDIIREGI
           ELSE
               ADD 100000              TO W-CEDULA-BDIIREGI
           END-IF
           MOVE W-CEDULA-BDIIREGI      TO CEDULA-BDIIREGI
           MOVE ALL 'B'                TO IND-SEXO-BDIIREGI
                                          BLOQUEO-TUTELA-BDIIREGI
                                          IND-CREACION-BDIIREGI
                                          IND-CAMBIO-BDIIREGI.
                                          
      ****************************************************************
      *1003-MOSTRAR-CONTADORES.
      *MOSTRAR CONTADORES DEL PROCESO 
      ****************************************************************
       1003-MOSTRAR-CONTADORES.
           MOVE W-ACTUALIZADOS                TO W-EDITCON 
           DISPLAY "Reg. ACTUALIZADOS........ "  W-EDITCON
           MOVE W-NO-ACTUALIZADOS             TO W-EDITCON 
           DISPLAY "Reg. No ACTUALIZADOS..... "  W-EDITCON.                      
           
      ******************************************************************
      *1004-CERRAR-ARCHIVOS.                           
      *SE CIERRAN LOS ARCHIVOS QUE SE UTILIZAN EN EL PROGRAMA        
      ******************************************************************
       1004-CERRAR-ARCHIVOS. 

           PERFORM 10-CONTROL-TIEMPO
      *
           DISPLAY " Fecha Fin    Proceso : " FECHA-PG
           DISPLAY " Hora  Fin    Proceso : " HORA-PG.
      *                            
 
           CLOSE BDIIREGI.
