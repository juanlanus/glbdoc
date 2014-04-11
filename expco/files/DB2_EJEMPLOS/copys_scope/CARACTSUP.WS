REMARK*CARACTSUP-WS.0001
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS ENDEUDAMIENTO GLOBAL.             
REMARK**********************************************************************
        01 TIPDEUDA-CARACT    PIC X(02).
           88 TIPDEUDA-VALIDA-CARACT   VALUE "S" "M" "H" "C" .
        01 CALDEUDA-CARACT    PIC X(02).
           88 CALDEUDA-VALIDA-CARACT   VALUE "A" "B" "C" "D" "E".
        01 VARIABLES-SUSP-CARACT.
           02 FINSUP-CARACT    PIC 9(1) VALUE 0.
           02 D-CARACT         PIC 9(1) VALUE 0.
           02 C-CARACT         PIC 9(1) VALUE 0.
           02 T-CARACT         PIC 9(1) VALUE 0.
           02 FECHA-SUP-CARACT.
              03 ANO-SUP-CARACT     PIC 9(4).
              03 MES-SUP-CARACT     PIC 9(2).
              03 DIA-SUP-CARACT     PIC 9(2).
           02 MESES-SUP-CARACT      PIC S9(6).  
        01 PORTAFOLIO-SUP00-CARACT    PIC X(02).
        01 PORTAFOLIOS-SUP01-CARACT.
           02 V-PORTAFOLIO-SUP01-CARACT OCCURS 4 TIMES PIC X(02).
        01 ESTRUCTURA-SUP00-CARACT.   
           02 TIPCAR-SUP00-CARACT OCCURS 4 TIMES.
              03 CALIFI-SUP00-CARACT OCCURS 5 TIMES.
                 04 V-CAR-SUP00-CARACT  PIC 9(04).               
        01 SUP00-CARACT REDEFINES ESTRUCTURA-SUP00-CARACT. 
           02 00SUP001SS000             PIC 9(04).
           02 00SUP002SS000             PIC 9(04).
           02 00SUP003SS000             PIC 9(04).
           02 00SUP004SS000             PIC 9(04).
           02 00SUP005SS000             PIC 9(04).
      *
           02 00SUP001SC000             PIC 9(04).
           02 00SUP002SC000             PIC 9(04).
           02 00SUP003SC000             PIC 9(04).
           02 00SUP004SC000             PIC 9(04).
           02 00SUP005SC000             PIC 9(04).
      *
           02 00SUP001SH000             PIC 9(04).
           02 00SUP002SH000             PIC 9(04).
           02 00SUP003SH000             PIC 9(04).
           02 00SUP004SH000             PIC 9(04).
           02 00SUP005SH000             PIC 9(04).
      *
           02 00SUP001SM000             PIC 9(04).
           02 00SUP002SM000             PIC 9(04).
           02 00SUP003SM000             PIC 9(04).
           02 00SUP004SM000             PIC 9(04).
           02 00SUP005SM000             PIC 9(04).
        01 ESTRUCTURA-SUP01-CARACT.
           02 TIPCAR-SUP01-CARACT OCCURS 4 TIMES.
              03 CALIFI-SUP01-CARACT OCCURS 5 TIMES.
                 04 TRIMES-SUP01-CARACT OCCURS 3 TIMES.
                    05 V-CAR-SUP01-CARACT  PIC 9(04).
        01 SUP01-CARACT REDEFINES ESTRUCTURA-SUP01-CARACT.
           02 01SUP001SS000             PIC 9(04).
           02 01SUP002SS000             PIC 9(04).
           02 01SUP003SS000             PIC 9(04).
           02 01SUP004SS000             PIC 9(04).
           02 01SUP005SS000             PIC 9(04).
           02 01SUP006SS000             PIC 9(04).
           02 01SUP007SS000             PIC 9(04).
           02 01SUP008SS000             PIC 9(04).
           02 01SUP009SS000             PIC 9(04).
           02 01SUP010SS000             PIC 9(04).
           02 01SUP011SS000             PIC 9(04).
           02 01SUP012SS000             PIC 9(04).
           02 01SUP013SS000             PIC 9(04).
           02 01SUP014SS000             PIC 9(04).
           02 01SUP015SS000             PIC 9(04).
      *
           02 01SUP001SC000             PIC 9(04).
           02 01SUP002SC000             PIC 9(04).
           02 01SUP003SC000             PIC 9(04).
           02 01SUP004SC000             PIC 9(04).
           02 01SUP005SC000             PIC 9(04).
           02 01SUP006SC000             PIC 9(04).
           02 01SUP007SC000             PIC 9(04).
           02 01SUP008SC000             PIC 9(04).
           02 01SUP009SC000             PIC 9(04).
           02 01SUP010SC000             PIC 9(04).
           02 01SUP011SC000             PIC 9(04).
           02 01SUP012SC000             PIC 9(04).
           02 01SUP013SC000             PIC 9(04).
           02 01SUP014SC000             PIC 9(04).
           02 01SUP015SC000             PIC 9(04).
      *
           02 01SUP001SH000             PIC 9(04).
           02 01SUP002SH000             PIC 9(04).
           02 01SUP003SH000             PIC 9(04).
           02 01SUP004SH000             PIC 9(04).
           02 01SUP005SH000             PIC 9(04).
           02 01SUP006SH000             PIC 9(04).
           02 01SUP007SH000             PIC 9(04).
           02 01SUP008SH000             PIC 9(04).
           02 01SUP009SH000             PIC 9(04).
           02 01SUP010SH000             PIC 9(04).
           02 01SUP011SH000             PIC 9(04).
           02 01SUP012SH000             PIC 9(04).
           02 01SUP013SH000             PIC 9(04).
           02 01SUP014SH000             PIC 9(04).
           02 01SUP015SH000             PIC 9(04).
      *
           02 01SUP001SM000             PIC 9(04).
           02 01SUP002SM000             PIC 9(04).
           02 01SUP003SM000             PIC 9(04).
           02 01SUP004SM000             PIC 9(04).
           02 01SUP005SM000             PIC 9(04).
           02 01SUP006SM000             PIC 9(04).
           02 01SUP007SM000             PIC 9(04).
           02 01SUP008SM000             PIC 9(04).
           02 01SUP009SM000             PIC 9(04).
           02 01SUP010SM000             PIC 9(04).
           02 01SUP011SM000             PIC 9(04).
           02 01SUP012SM000             PIC 9(04).
           02 01SUP013SM000             PIC 9(04).
           02 01SUP014SM000             PIC 9(04).
           02 01SUP015SM000             PIC 9(04).
        01 ESTRUCTURA-SUP02-CARACT.
            03 CALIFI-SUP02-CARACT OCCURS 5 TIMES.
               04 TRIMES-SUP02-CARACT OCCURS 3 TIMES.
                    05 V-CAR-SUP02-CARACT  PIC 9(04).
        01 SUP02-CARACT REDEFINES ESTRUCTURA-SUP02-CARACT.
           02 02SUP001TO000             PIC 9(04).
           02 02SUP002TO000             PIC 9(04).
           02 02SUP003TO000             PIC 9(04).
           02 02SUP004TO000             PIC 9(04).
           02 02SUP005TO000             PIC 9(04).
           02 02SUP006TO000             PIC 9(04).
           02 02SUP007TO000             PIC 9(04).
           02 02SUP008TO000             PIC 9(04).
           02 02SUP009TO000             PIC 9(04).
           02 02SUP010TO000             PIC 9(04).
           02 02SUP011TO000             PIC 9(04).
           02 02SUP012TO000             PIC 9(04).
           02 02SUP013TO000             PIC 9(04).
           02 02SUP014TO000             PIC 9(04).
           02 02SUP015TO000             PIC 9(04).
