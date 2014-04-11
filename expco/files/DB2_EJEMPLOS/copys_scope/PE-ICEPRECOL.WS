      *PE-ICEPRECOL.WS.v.01.0001.
      *----------------------------------------------------------------*
      *
      *    COPY  WORKING  PROCESO ESPECIAL COLPATRIA
      *
      ******************************************************************
       01 W-VARIABLES-ALGORITMOS.
          03 V24                        PIC 9(05) VALUE ZERO.
          03 V34-D                      PIC 9(07) VALUE ZERO.
          03 V77                        PIC 9(04) VALUE ZERO.
          03 V82                        PIC 9(04) VALUE ZERO.
          03 V28                        PIC 9(04) VALUE ZERO.
          03 V22                        PIC 9(09) VALUE ZERO.
          03 V43                        PIC 9(03) VALUE ZERO.
          03 V2                         PIC 9(03) VALUE ZERO.
          03 V52                        PIC 9(03) VALUE ZERO.
          03 V19                        PIC 9(09) VALUE ZERO.
          03 V25                        PIC 9(09) VALUE ZERO.
          03 V21                        PIC 9(03) VALUE ZERO.
          03 V5                         PIC 9(03) VALUE ZERO.
          03 V73                        PIC 9(03) VALUE ZERO.
          03 V6                         PIC 9(03) VALUE ZERO.
          03 V3                         PIC X(01) VALUE " ".
          03 V72                        PIC 9(03) VALUE ZERO.
          03 V9                         PIC 9(09) VALUE ZERO.
          03 V11                        PIC 9(03) VALUE ZERO.
          03 V38                        PIC 9(09) VALUE ZERO.
          03 V76                        PIC 9(03) VALUE ZERO.
          03 V7                         PIC 9(09) VALUE ZERO.
          03 V14                        PIC 9(09) VALUE ZERO.
          03 V50                        PIC 9(09) VALUE ZERO.
          03 V36                        PIC 9(09) VALUE ZERO.
          03 V101-M                     PIC 9(07) VALUE ZERO.
          03 V102-M                     PIC 9(07) VALUE ZERO.
          03 V103-M                     PIC 9(07) VALUE ZERO.
          03 V104                       PIC 9(03) VALUE ZERO.
          03 V105                       PIC 9(03) VALUE ZERO.
          03 V106                       PIC 9(03) VALUE ZERO.
          03 V107                       PIC 9(03) VALUE ZERO.
          03 V108                       PIC 9(03) VALUE ZERO.
          03 V109                       PIC 9(03) VALUE ZERO.
          03 V104-D                     PIC 9(03) VALUE ZERO.
          03 W-FUNCIONES.
             05 ALG1-FUNCION1           PIC S9(2)V9(16) VALUE 0.04934.
             05 ALG1-FUNCION2           PIC S9(2)V9(16) 
                                              VALUE -12.1159899237565.
             05 ALG1-FUNCION3           PIC S9(2)V9(16) 
                                              VALUE -4.85064607152125.
             05 ALG2-FUNCION1           PIC S9(2)V9(16) VALUE ZERO.
             05 ALG3-FUNCION1           PIC S9(2)V9(16) VALUE 1.524.
          03 W-VAR-SALIDA.
             05 W-RESPUESTA-S           PIC 9(03) VALUE ZERO.
             05 W-CLIENTE-S             PIC 9(03) VALUE ZERO.
             05 W-P-S                   PIC 9(03) VALUE ZERO.
             05 W-PUNTAJE-S             PIC 9(05) VALUE ZERO.
             05 W-RANGO-S               PIC 9(03) VALUE ZERO.
          03 W-TOT-CUPOS-TDC            PIC 9(09) VALUE ZERO.
          03 W-FEC-MENOR-TDC            PIC 9(06).
          03 W-RED-MENOR-TDC REDEFINES W-FEC-MENOR-TDC.
             05 AAAA-TDC                PIC 9(04).
             05 MM-TDC                  PIC 9(02).
          03 W-OTRA-FECHA-TDC           PIC 9(08).
          03 W-OTRA-FECHA-TDC-RED REDEFINES W-OTRA-FECHA-TDC.
             05 AAAA-TDC-R              PIC 9(04).
             05 MM-TDC-R                PIC 9(02).
             05 DD-TDC-R                PIC 9(02).
          03 W-CUOTAS-OCF               PIC 9(09) VALUE ZERO.
          03 W-CUOTAS-CAB               PIC 9(09) VALUE ZERO.
          03 W-CUOTAS-CAV               PIC 9(09) VALUE ZERO.
          03 W-MONTOS-CAB               PIC 9(09) VALUE ZERO.
          03 W-MONTOS-REAL              PIC 9(09) VALUE ZERO.
          03 W-NUM-CAB-ACTIVAS          PIC 9(03) VALUE ZERO.
          03 W-NUM-CAV-ACTIVAS          PIC 9(03) VALUE ZERO.
          03 W-NUM-AHO-ACTIVAS          PIC 9(03) VALUE ZERO.
          03 W-FECHA-ANT-REAL           PIC 9(06) VALUE 999999.
          03 W-FECHA-ANT-REAL-R REDEFINES W-FECHA-ANT-REAL.
             05 AAAA-REAL               PIC 9(04).
             05 MM-REAL                 PIC 9(02).
          03 W-NUM-REAL-INACTIVAS       PIC 9(03) VALUE ZERO.
          03 W-NUM-CAB-INACTIVAS        PIC 9(03) VALUE ZERO.
          03 W-FECHA-ANT-CAB            PIC 9(06) VALUE 999999.
          03 W-FECHA-ANT-CAB-R REDEFINES W-FECHA-ANT-CAB.
             05 AAAA-CAB                PIC 9(04).
             05 MM-CAB                  PIC 9(02).
          03 W-MORAS-30-ACT-TDC         PIC 9(03) VALUE ZERO.
          03 W-MORAS-30-ACT-CAB         PIC 9(03) VALUE ZERO.
          03 W-MORAS-30-ACT-CAV         PIC 9(03) VALUE ZERO.
          03 W-MORAS-30-ACT-REAL        PIC 9(03) VALUE ZERO.
          03 PUNTAJE                    PIC 9(07) VALUE ZERO.
          03 PUNTAJEP                   PIC 9(07) VALUE ZERO.
          03 PUNTAJER                   PIC 9(07) VALUE ZERO.
          03 WP1                        PIC S9(2)V9(16) VALUE ZERO.
          03 WP2                        PIC S9(2)V9(16) VALUE ZERO.
          03 WP3                        PIC S9(2)V9(16) VALUE ZERO.
          03 WPMAX                      PIC S9(2)V9(16) VALUE ZERO.
          03 F1                         PIC S9(2)V9(16) VALUE ZERO.
          03 F2                         PIC S9(2)V9(16) VALUE ZERO.
          03 F3                         PIC S9(2)V9(16) VALUE ZERO.
          03 F4                         PIC S9(2)V9(16) VALUE ZERO.
          03 F5                         PIC S9(2)V9(16) VALUE ZERO.
          03 FMAX                       PIC S9(2)V9(16) VALUE ZERO.
          03 PROB-A3                    PIC 9(03) VALUE ZERO.

