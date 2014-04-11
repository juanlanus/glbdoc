REMARK*BDASEPNOM-WS.v.0001.
REMARK********************************************************************
REMARK* COPY QUE CONTIENE LA VARIABLES NECESARIAS PARA SEPARACION DE UN        
REMARK* NOMBRE COMPLETO EN CADA UNO DE LOS 2 POSIBLE NOMBRES Y 2 POSIBLES   
REMARK* APELLIDOS                                
REMARK********************************************************************
       01  OPEN-BDANOM-BDASEPNOM            PIC 9 VALUE 0.                      
       01  POR-EXI-BDASEPNOM                PIC 9V9(10).                 
       01  TOT-EXI-BDASEPNOM                PIC 9999.                    
       01  NODO-BDASEPNOM                   PIC 9999.                    
       01  BASURA-BDASEPNOM                 PIC X(80).                   
       01  I-BDASEPNOM                      PIC 9999.                    
       01  J-BDASEPNOM                      PIC 9999.                    
       01  K-BDASEPNOM                      PIC 9999.                    
       01  L-BDASEPNOM                      PIC 9999.                    
       01  K1-BDASEPNOM                     PIC 9999.                    
       01  K2-BDASEPNOM                     PIC 9999.                    
       01  NUM-PARTES-BDASEPNOM             PIC 9999.                    
       01  TOTAL-PARTICULAS-BDASEPNOM       PIC 9(8).                    
       01  NOMBRE-AUX-BDASEPNOM             PIC X(45).                   
       01  NOMBRE-BDASEPNOM.                                                    
           02 FILLER                        PIC X(02) VALUE "A ".               
           02  NOMBRE-INPUT-BDASEPNOM       PIC X(45).                   
       01  NOMBRE1-OUTPUT-BDASEPNOM         PIC X(20).                   
       01  NOMBRE2-OUTPUT-BDASEPNOM         PIC X(20).                   
       01  NOMBRES-OUTPUT-BDASEPNOM         PIC X(30).                   
       01  PRIAPEL-OUTPUT-BDASEPNOM         PIC X(30).                   
       01  SEGAPEL-OUTPUT-BDASEPNOM         PIC X(30).                   
       01  ADJETIVO-AUX-BDASEPNOM           PIC X(15).                   
       01  EXC-OUTPUT-BDASEPNOM             PIC 9(01).                   
       01  FLAGS1-BDASEPNOM.
           02 V-FLAG1-BDASEPNOM             OCCURS 4 TIMES PIC 9.   
       01  FLAGS2-BDASEPNOM.                                                    
           02 V-FLAG2-BDASEPNOM             OCCURS 4 TIMES PIC 9.  
       01  V-A-BDASEPNOM.                                                       
           02 A-BDASEPNOM                   OCCURS 20 TIMES PIC X(45).  
           02 LONG-BDASEPNOM                OCCURS 20 TIMES PIC 9(02).  
       01  V-FINAL-BDASEPNOM.                                                   
           02 ADJETIVO-BDASEPNOM            OCCURS 20 TIMES PIC X(15). 
           02 PARTICULA-BDASEPNOM           OCCURS 20 TIMES PIC X(45).  
           02 POR-NOM-BDASEPNOM             OCCURS 20 TIMES PIC 9V9(8).  
           02 EXISTE-EN-TAB-BDASEPNOM       OCCURS 20 TIMES PIC 9.
              
