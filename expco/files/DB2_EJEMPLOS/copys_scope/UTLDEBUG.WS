REMARK* UTLDEBUG-WS.0001
REAMRK*******************************************************************
REMARK* VARIABLES PARA USO DE RUTINAS DE MANEJO DE DEBUGGER
REMARK*
REMARK*******************************************************************
       01 OPERACION-TRANSAC           PIC X(01).
       01 ACTIVAR-DEBUG               PIC X(01)  VALUE "G".
       01 IND-DEBUG-UTLDEBUG          PIC 9(01)  VALUE 0.
       01 LINEA-AUX-UTLDEBUG.
          03 FILLER                   PIC X(02)  VALUE "I ".
          03 LIN-AUX-UTLDEBUG         PIC X(50).
       01 LIN-DEBUG-UTLDEBUG          PIC 9(05).
       01 CON-DEBUG-UTLDEBUG          PIC 9(05)  VALUE 0.
       01 AUXILIAR-UTLDEBUG           PIC X(01).
       01 HORA-AUX-UTLDEBUG.       
          03 HOR-AUX-UTLDEBUG         PIC 9(02).
          03 MIN-AUX-UTLDEBUG         PIC 9(02).
          03 SEG-AUX-UTLDEBUG         PIC 9(02).
       01 HORA-SEG-ENTRADA-UTLDEBUG   PIC 9(05) VALUE 0.    
       01 IND-ERR-UTLDEBUG            PIC 9(01).
