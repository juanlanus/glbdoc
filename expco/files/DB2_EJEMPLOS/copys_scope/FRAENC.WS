******* FRAENC-WS.v.0001
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES
******* QUE UTILIZAN LAS RUTINAS DE BASE DE LA RUTINA DE BITS      .
***********************************************************************
       01 VARIABLES-FRAENC.                                      
          03 RES-FRAENC           PIC 9(2) VALUE 0.    
          03 FIN-FRAENC           PIC 9(1) VALUE 0.
          03 I-FRAENC             PIC 9(5) VALUE 0.
          03 NO-LOOP-FRAENC       PIC 9(5) VALUE 0.
          03 NUMPRE-AUX-FRAENC    PIC 9(2) VALUE 0.
          03 HAY-01-FRAENC        PIC 9(5) VALUE 0.
          03 HAY-02-FRAENC        PIC 9(5) VALUE 0.
          03 CODPRE-FRAENC-ACTUAL PIC X(9) VALUE SPACES.
          03 CODPRE-FRAENC-AUX.
             04 NUMEST-FRAENC-AUX PIC 9(3) VALUE 999.
             04 NUMENC-FRAENC-AUX PIC 9(3) VALUE 0.
             04 NUMPRE-FRAENC-AUX PIC 9(3) VALUE 0.
       01 TABLA-FRAENC.                 
          03 INF-FRALOG-FRAENC.
             04 FILLER OCCURS 170 TIMES.
                05 FILLER                 PIC X(1).
                05 CODPRE-FRALOG-FRAENC   PIC X(9).
                05 RESPUS-FRALOG-FRAENC   PIC 9(02).
          03 V-FRAENC OCCURS 1001 TIMES. 
             04 NIT-FRAENC          PIC 9(11). 
             04 TIPREG-FRAENC       PIC 9(2). *>99=CODPRE,01=ENU,02=OPC
             04 TEXTO-FRAENC        PIC X(200).
             04 REDEFINES TEXTO-FRAENC.
                05 NUMENC-FRAENC    PIC 9(3). *> NUMERO DE ENCUESTA
                05 NUMPRE-FRAENC    PIC 9(3). *> NUMERO DE ENCUESTA
                05 FILLER           PIC X(194). 
