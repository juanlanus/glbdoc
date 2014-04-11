REMARK* UTLSEG.WS.v.0001  
REMARK* ESTE COPY CONTIENE CADA UNA DE LAS VARIABLES QUE SE DEBEN MANEJAR
REMARK* CUANDO CUALQUIER PROGRAMA REQUIERE QUE SE CALCULE O VERIFIQUE
REMARK* EL CODIGO DE SEGURIDAD QUE SE PRESENTA EN LA CONSULTA
REMARK*******************************************************************
       01 HORA-AUX-SEC-UTLSEG           PIC 9(8)  VALUE ZEROS.  
       01 FRAU-VARIABLES-UTLSEG.                                                
          02 OUTPUT-VERIFICACION-UTLSEG PIC 9 VALUE 0.
          02 FRAU-LETRA-UTLSEG          PIC X VALUE SPACES. 
          02 FRAU-ENE-UTLSEG            PIC X VALUE X'D1'. 
          02 FRAU01-UTLSEG              PIC 9 VALUE 0.    
          02 I-UTLSEG                   PIC 9(7) VALUE 0.
          02 J-UTLSEG                   PIC 9(7) VALUE 0. 
          02 K-UTLSEG                   PIC 9(7) VALUE 0.
          02 FRAU-RES-UTLSEG            PIC 9(03).      
          02 FRAU-DIV-UTLSEG            PIC 9(08).     
          02 FRAU-DIGITO-UTLSEG         PIC 9(01).    
          02 INPUT-NUMIDE-UTLSEG        PIC 9(11).   
          02 INPUT-NUMSEC-UTLSEG        PIC 9(08).  
          02 FILLER REDEFINES INPUT-NUMSEC-UTLSEG.            
             03 FILLER                  PIC 9(5). 
             03 FRAU-ICBTCO-3DIG-UTLSEG PIC 9(3).
          02 FRAU-CODIGO-PURO-UTLSEG.                                           
             03 FRAU-XXX-UTLSEG         PIC 9(03).
             03 FRAU-COD-PURO-UTLSEG.                                           
                05 FRAU-Z-UTLSEG        PIC 9(01). 
                05 FRAU-YY-UTLSEG       PIC 9(02).
                05 FRAU-W-UTLSEG        PIC 9(01).
          02 OUTPUT-CODSEG-UTLSEG.                                              
             03 FRAU-SEM-ENCR-UTLSEG    PIC X(03).
             03 FRAU-COD-ENCR-UTLSEG    PIC X(04).
       01 TAB1-FRAUDE-UTLSEG.                                                   
          02   FILLER                   PIC X(4) VALUE "1WSC". 
          02   FILLER                   PIC X(4) VALUE "2EDV".
          02   FILLER                   PIC X(4) VALUE "3RFB". 
          02   FILLER                   PIC X(4) VALUE "4TGN".
          02   FILLER                   PIC X(4) VALUE "5YHM".
          02   FILLER                   PIC X(4) VALUE "6UJ6".
          02   FILLER                   PIC X(4) VALUE "7IK7".
          02   FILLER                   PIC X(4) VALUE "8OL8".
          02   FILLER                   PIC X(4) VALUE "9PZ9".
          02   FILLER                   PIC X(4) VALUE "0QAX".
       01 TAB2-FRAUDE-UTLSEG REDEFINES TAB1-FRAUDE-UTLSEG.                      
          02   TAB2-NUMERO-UTLSEG OCCURS 10 TIMES.                              
               03 TAB2-MASCARA-UTLSEG OCCURS 4 TIMES PIC X.                     
