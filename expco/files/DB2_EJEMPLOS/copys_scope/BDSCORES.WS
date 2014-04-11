******* BDSCORES-WS.v.0001
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES
******* QUE UTILIZAN LAS RUTINAS DE BASE DE LA RUTINA DE BDSCORES  .
***********************************************************************
       01 VARIABLES-BDSCORES.                                    
          03 USAHIS-OUTPUT-BDSCORES       PIC 9(01) VALUE 0.
          03 HAYHIS-OUTPUT-BDSCORES       PIC 9(01) VALUE 0.
          03 USAR-HIS-INPUT-BDSCORES      PIC 9(01) VALUE 0.
          03 ERR-BDSCORES                 PIC 9(01) VALUE 0.
          03 OPEN-BDSCORES                PIC 9(01) VALUE 0.
          03 COD-RETORNO-BDSCORES         PIC X(02) VALUE SPACES.
          03 M-EXISTE-BDSCORES            PIC 9(10) VALUE 0.
          03 FILLER REDEFINES M-EXISTE-BDSCORES.
             04 V-EXISTE-BDSCORES OCCURS 10 TIMES PIC 9(01).
