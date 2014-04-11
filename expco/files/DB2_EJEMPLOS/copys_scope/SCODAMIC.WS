REMARK*  SCODAMIC-WS.V.0001
REMARK*****************************************************************
REMARK* ESTE COPY CONTIENE LAS DEFINICIONES DE LAS VARIABLES PROPIAS DEL
REMARK* SCORE DE MICROCREDITO
REMARK******************************************************************
       01 VARIABLES-WORK-DAMIC.       
          03 SCORE-DAMIC                        PIC 9(03).            
          03 ADVERSE-RAZON-DAMIC OCCURS 4 TIMES PIC 9(02).      
          03 SCORE-CARD-DAMIC                   PIC 9(01).
          03 CON-EXCLUSION-DAMIC                PIC 9(01).
          03 AREA-VALI-DAMIC                    PIC X(2000).
          03 NOMB-VALI-DAMIC                    PIC X(20).
          03 DUMMIES-DESPLEGAR-DAMIC            PIC X(501).
       01 VARIABLES-DAMIC.       
          02 MI-SCOREN                 PIC S9(09)V9(05).
          02 I-DAMIC                   PIC 9(5).            
          02 J-DAMIC                   PIC 9(5).            
       01 PARAMETROS-EXTERNOS-DAMIC.       
          02 MI-ACTIVOS-TOTALES-DAMIC  PIC 9(15) VALUE 999999999999999.
          02 MI-PERSONAS-A-CARGO-DAMIC PIC 9(05) VALUE 99999.
       01 VARIABLES-CARD01-DAMIC.       
          02 ACOTADAS-S1-DAMIC.
             04 MI-S1-QALDIA            PIC 9(05).
          02 DUMMYS-S1-DAMIC.
             04 MI-S1-CO00DEM016D01     PIC 9(1).             
