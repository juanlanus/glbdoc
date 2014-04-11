REMARK*  SCOUSAPI-WS.V.0002
REMARK******************************************************************
REMARK* DEFINICION DE LAS VARIABLES PROPIAS DEL SCORE PREDITOR DE INGRESOS
REMARK* AHORA LLAMADO QUANTO
REMARK******************************************************************
      * RMB0002    | QUANNW * IMPLEMENTACION DE LA NUEVA VERSION DE QUANTO *
      * R.MARTINEZ |          SE REEMPLAZAN LOS SCORE CARDS POR FILOSOFIA  *
      * 20-NOV-2009|          DE REDES NEURONALES                          *
      **********************************************************************
       01 VARIABLES-WORK-USAPI.       
QUANNW    03 CUPOTOTAL-USAPI                    PIC S9(08)V9(8).        RMB0002
QUANNW    03 CUPOTOTALHP-USAPI                  PIC S9(08)V9(8).        RMB0002
QUANNW    03 CUOTATOTAL-USAPI                   PIC S9(08)V9(8).        RMB0002
QUANNW    03 SALDOTOTALHP-USAPI                 PIC S9(08)V9(8).        RMB0002
QUANNW    03 CUOTATOTALHP-3-USAPI               PIC S9(08)V9(8).        RMB0002
QUANNW    03 FINANCIERO-C                       PIC 9.                  RMB0002
QUANNW    03 REAL-C                             PIC 9.                  RMB0002
QUANNW    03 TELCOS-C                           PIC 9.                  RMB0002
QUANNW    03 TIPOREL-C                          PIC X(10).              RMB0002
QUANNW    03 SEGMENTACION-USAPI.                                        RMB0002
QUANNW       04 CUPOTOTALHP-AC                     PIC S9(08)V9(8).     RMB0002
QUANNW       04 CO02NUM003TO-AC                    PIC 9(4).            RMB0002
QUANNW       04 CO01END026RO-AC                    PIC S9(08)V9(8).     RMB0002
QUANNW       04 CO01EXP001AH-AC                    PIC 9(03).           RMB0002
QUANNW       04 CO02NUM043RO-AC                    PIC S9(08)V9(8).     RMB0002
QUANNW       04 CO02EXP009TO-AC                    PIC S9(08)V9(8).     RMB0002
QUANNW       04 CO00DEM003-AC                      PIC 9(03).           RMB0002
QUANNW       04 CO00DEM027-AC                      PIC 9(03).           RMB0002
QUANNW       04 CO02NUM042CB-AC                    PIC S9(08)V9(8).     RMB0002
QUANNW       04 CO01END007RO-AC                    PIC S9(08)V9(8).     RMB0002
QUANNW       04 TIPORELD01                         PIC 9(1).            RMB0002
QUANNW       04 TIPORELD02                         PIC 9(1).            RMB0002
QUANNW       04 TIPORELD03                         PIC 9(1).            RMB0002
QUANNW       04 TIPORELD04                         PIC 9(1).            RMB0002
QUANNW       04 CO01END007RO-AC-D01                PIC 9(1).            RMB0002
QUANNW       04 CO01END007RO-AC-D02                PIC 9(1).            RMB0002
QUANNW       04 CO01END007RO-AC-D03                PIC 9(1).            RMB0002
QUANNW       04 CO01END007RO-AC-D04                PIC 9(1).            RMB0002
QUANNW       04 CO01NUM005CT-AC-D01                PIC 9(1).            RMB0002
QUANNW       04 CO01NUM005CT-AC-D02                PIC 9(1).            RMB0002
QUANNW       04 CO00DEM006-AC-D01                  PIC 9(1).            RMB0002
QUANNW       04 CO00DEM026-AC-D01                  PIC 9(1).            RMB0002
QUANNW       04 CO00DEM026-AC-D02                  PIC 9(1).            RMB0002
QUANNW       04 CO00DEM026-AC-D03                  PIC 9(1).            RMB0002
QUANNW       04 CO00DEM030-AC-D01                  PIC 9(1).            RMB0002
QUANNW       04 EXPONENTE-AC                       PIC S9(5)V9(13).     RMB0002
QUANNW       04 SEGMENTO-AC                        PIC S9(5).           RMB0002
QUANNW    03 SUBSEGMENTACION-USAPI.                                     RMB0002
QUANNW       04 CUPOTOTAL-ACS1                     PIC S9(08)V9(8).     RMB0002
QUANNW       04 CO01END007RO-ACS1                  PIC 9(8)V99.         RMB0002
QUANNW       04 CO01NUM002CT-ACS1                  PIC 9(4).            RMB0002
QUANNW       04 CO01EXP001CC-ACS1                  PIC 9(3).            RMB0002
QUANNW       04 CO01ACP008CC-ACS1                  PIC 9(4).            RMB0002
QUANNW       04 CO01EXP001CT-ACS1                  PIC 9(3).            RMB0002
QUANNW       04 CO00DEM006-ACS1-D01                PIC 9(1).            RMB0002
QUANNW       04 CO00DEM026-ACS1-D00                PIC 9(1).            RMB0002
QUANNW       04 CO00DEM026-ACS1-D01                PIC 9(1).            RMB0002
QUANNW       04 CO00DEM026-ACS1-D02                PIC 9(1).            RMB0002
QUANNW       04 CO00DEM026-ACS1-D03                PIC 9(1).            RMB0002
QUANNW       04 CO00DEM030-ACS1-D01                PIC 9(1).            RMB0002
QUANNW       04 EXPONENTE-ACS1                     PIC S9(5)V9(13).     RMB0002
QUANNW       04 SEGMENTO-ACS1                      PIC S9(5).           RMB0002
          03 VLR-SMLMV-USAPI                    PIC 9(8).         
          03 INGRESO-PESOS-USAPI                PIC 9(09)9.
          03 INGRESO-PESOS-N-USAPI              PIC 9(10).      
          03 INGRESO-PESOS-INF-USAPI            PIC 9(09)9.    
          03 INGRESO-PESOS-N-INF-USAPI          PIC 9(10).      
          03 INGRESO-PESOS-SUP-USAPI            PIC 9(09)9.    
          03 INGRESO-PESOS-N-SUP-USAPI          PIC 9(10).      
          03 INGRESO-USAPI                      PIC 9(2).9(5).
          03 INGRESO-N-USAPI                    PIC 9(4)V9(6).
          03 INGRESO-INF-USAPI                  PIC 9(2).9(5).
          03 INGRESO-INF-N-USAPI                PIC 9(2)V9(5).    
          03 INGRESO-SUP-USAPI                  PIC 9(2).9(5).
          03 INGRESO-SUP-N-USAPI                PIC 9(2)V9(5).    
          03 INGRESO-USAPI1                     PIC Z(1).9(04).
          03 INGRESO-USAPI2                     PIC Z(2).9(03).
          03 ADVERSE-RAZON-USAPI OCCURS 4 TIMES PIC 9(02).      
          03 SCORE-CARD-USAPI                   PIC 9(01).
          03 SIN-EXCLUSION-USAPI                PIC 9(01).
          03 I-USAPI                            PIC 9(06).
          03 J-USAPI                            PIC 9(06).
          03 L-USAPI                            PIC 9(06).
          03 T-USAPI                            PIC 9(06).
          03 AREA-VALI-USAPI                    PIC X(2000).
          03 NOMB-VALI-USAPI                    PIC X(20).
          03 DUMMIES-DESPLEGAR-USAPI            PIC X(101).
       01 VARIABLES-CONSTANTES-USAPI.       
          03 K-INTERVALO-USAPI          PIC 9V9(3)  VALUE 0.45.   
       01 VARIABLES-CARD01-USAPI.       
          02 ACOTADAS-S1-USAPI.
QUANNW       04 CUPOTOTAL-SEG1       PIC S9(08)V9(8).                   RMB0002
QUANNW       04 CO01END030RO-SEG1    PIC 9(8)V99.                       RMB0002
QUANNW       04 CO01NUM050CT-SEG1    PIC 9(4).                          RMB0002
QUANNW       04 CO01END006RO-SEG1    PIC 9(8)V99.                       RMB0002
          02 DUMMYS-S1-USAPI.
QUANNW       04 CO01END010RO-SEG1-D03   PIC 9(1).                       RMB0002
QUANNW       04 CO01END010RO-SEG1-D04   PIC 9(1).                       RMB0002
QUANNW       04 CO01END005IN-SEG1-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END005IN-SEG1-D02   PIC 9(1).                       RMB0002
QUANNW       04 CO01END035IN-SEG1-D04   PIC 9(1).                       RMB0002
QUANNW       04 CO01END021IN-SEG1-D04   PIC 9(1).                       RMB0002
QUANNW       04 CO02EXP012RO-SEG1-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO02EXP012RO-SEG1-D02   PIC 9(1).                       RMB0002
QUANNW       04 CO01END077RO-SEG1-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END028HP-SEG1-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM006-SEG1         PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM030-SEG1-D02     PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM026-SEG1-D05     PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM026-SEG1-D06     PIC 9(1).                       RMB0002
             04 S1-INGRESO-C         PIC S9(3)V9(15).
             04 S1-INGRESO-INF-C     PIC S9(3)V9(15).
             04 S1-INGRESO-SUP-C     PIC S9(3)V9(15).
       01 VARIABLES-CARD02-USAPI.       
          02 ACOTADAS-S2-USAPI.
QUANNW       04 CO01END030RO-SEG2    PIC 9(8)V99.                       RMB0002
QUANNW       04 CUOTATOTAL-SEG2      PIC 9(8)V99.                       RMB0002
QUANNW       04 CO01END028RO-SEG2    PIC 9(8)V99.                       RMB0002
QUANNW       04 CUPOTOTAL-SEG2       PIC 9(8)V99.                       RMB0002
QUANNW       04 CO01END089RO-SEG2    PIC 9(8)V99.                       RMB0002
QUANNW       04 CO00DEM003-SEG2      PIC 9(8)V99.                       RMB0002
QUANNW    02 ESTANDARIZADAS-S2-USAPI.                                   RMB0002
QUANNW       04 S-CO01END023IN-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END007IN-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END005IN-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END038RO-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END055IN-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END052IN-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END068RO-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END018RO-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO02END010RO-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END073RO-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO02END010CB-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END071RO-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO02END007RO-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO02END009CB-SEG2-D01   PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO00DEM006-SEG2         PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO00DEM030-SEG2-D02     PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO00DEM026-SEG2-D02     PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO00DEM026-SEG2-D04     PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO00DEM026-SEG2-D05     PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END030RO-SEG2       PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CUOTATOTAL-SEG2         PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END028RO-SEG2       PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CUPOTOTAL-SEG2          PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO01END089RO-SEG2       PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-CO00DEM003-SEG2         PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-TIPORELD03              PIC S9(3)V9(15).              RMB0002
QUANNW       04 S-TIPORELD04              PIC S9(3)V9(15).              RMB0002
QUANNW       04 H11-SEG2                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H12-SEG2                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H13-SEG2                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H11A-SEG2                 PIC S9(4)V9(14).              RMB0002
QUANNW       04 H12A-SEG2                 PIC S9(4)V9(14).              RMB0002
QUANNW       04 H13A-SEG2                 PIC S9(4)V9(14).              RMB0002
          02 DUMMYS-S2-USAPI.
QUANNW       04 CO01END023IN-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END007IN-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END005IN-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END038RO-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END055IN-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END052IN-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END068RO-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END018RO-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO02END010RO-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END073RO-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO02END010CB-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO01END071RO-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO02END007RO-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO02END009CB-SEG2-D01   PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM006-SEG2         PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM030-SEG2-D02     PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM026-SEG2-D02     PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM026-SEG2-D04     PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM026-SEG2-D05     PIC 9(1).                       RMB0002
       01 VARIABLES-CARD03-USAPI.       
          02 ACOTADAS-S3-USAPI.
QUANNW       04 CUPOTOTAL-SEG3       PIC 9(8)V99.                       RMB0002
QUANNW       04 CO02NUM003TO-SEG3    PIC 9(4).                          RMB0002
QUANNW       04 CO01EXP001RO-SEG3    PIC 9(3).                          RMB0002
QUANNW       04 SALDOTOTALHP-SEG3    PIC 9(8)V99.                       RMB0002
QUANNW       04 CO02EXP012TO-SEG3    PIC 9(8)V99.                       RMB0002
QUANNW       04 CO00DEM003-SEG3      PIC 9(3).                          RMB0002
QUANNW       04 CO02EXP001TO-SEG3    PIC 9(3).                          RMB0002
QUANNW       04 CO00DEM006-SEG3         PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM030-SEG3-D01     PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM026-SEG3-D01     PIC 9(1).                       RMB0002
QUANNW       04 CO00DEM026-SEG3-D02     PIC 9(1).                       RMB0002
QUANNW    02 ESTANDARIZADAS-S3-USAPI.                                   RMB0002
QUANNW       04 S-CO00DEM030-SEG3-D01         PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO00DEM003-SEG3             PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01ACP007CT-SEG3-D00       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END007IN-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END007RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END007RO-SEG3-D02       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END010RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END010RO-SEG3-D04       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END023IN-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END023IN-SEG3-D02       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END023IN-SEG3-D04       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END026IN-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END030RO-SEG3-D04       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END037RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END040RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END050RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END060RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END070RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END071RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END072RO-SEG3-D01       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01END075RO-SEG3-D04       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01EXP001CC-SEG3-D00       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01EXP001CC-SEG3-D03       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01EXP001CC-SEG3-D04       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01EXP001RO-SEG3           PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01NUM001CC-SEG3-D03       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO01NUM001CC-SEG3-D04       PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO02EXP001TO-SEG3           PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO02EXP012TO-SEG3           PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO02NUM003TO-SEG3           PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CUOTATOTALHP-3-SEG3-D03     PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CUOTATOTALHP-3-SEG3-D04     PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CUPOTOTAL-SEG3              PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO00DEM026-SEG3-D01         PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-CO00DEM026-SEG3-D02         PIC S9(3)V9(15).          RMB0002
QUANNW       04 S-SALDOTOTALHP-SEG3           PIC S9(3)V9(15).          RMB0002
QUANNW       04 H11-SEG3                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H12-SEG3                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H13-SEG3                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H11A-SEG3                 PIC S9(4)V9(14).              RMB0002
QUANNW       04 H12A-SEG3                 PIC S9(4)V9(14).              RMB0002
QUANNW       04 H13A-SEG3                 PIC S9(4)V9(14).              RMB0002
          02 DUMMYS-S3-USAPI.
QUANNW       04 CO01END030RO-SEG3-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END023IN-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END023IN-SEG3-D02  PIC 9(1).                        RMB0002
QUANNW       04 CO01END023IN-SEG3-D04  PIC 9(1).                        RMB0002
QUANNW       04 CUOTATOTALHP-3-SEG3-D03  PIC 9(1).                      RMB0002
QUANNW       04 CUOTATOTALHP-3-SEG3-D04  PIC 9(1).                      RMB0002
QUANNW       04 CO01END010RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END010RO-SEG3-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01EXP001CC-SEG3-D00  PIC 9(1).                        RMB0002
QUANNW       04 CO01EXP001CC-SEG3-D03  PIC 9(1).                        RMB0002
QUANNW       04 CO01EXP001CC-SEG3-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END040RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END007IN-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01NUM001CC-SEG3-D03  PIC 9(1).                        RMB0002
QUANNW       04 CO01NUM001CC-SEG3-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END050RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END026IN-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END060RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END070RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END007RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END007RO-SEG3-D02  PIC 9(1).                        RMB0002
QUANNW       04 CO01END037RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01ACP007CT-SEG3-D00  PIC 9(1).                        RMB0002
QUANNW       04 CO01END071RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END072RO-SEG3-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END075RO-SEG3-D04  PIC 9(1).                        RMB0002
       01 VARIABLES-CARD04-USAPI.       
          02 ACOTADAS-S4-USAPI.
QUANNW       04 CUPOTOTAL-SEG4       PIC 9(8)V99.                       RMB0002
QUANNW       04 CO02NUM003TO-SEG4    PIC 9(4).                          RMB0002
QUANNW       04 CO02EXP001TO-SEG4    PIC 9(3).                          RMB0002
QUANNW       04 CO00DEM003-SEG4      PIC 9(3).                          RMB0002
QUANNW    02 ESTANDARIZADAS-S4-USAPI.                                   RMB0002
QUANNW       04 S-CUOTATOTAL-SEG4-D01  PIC S9(3)V9(15).                 RMB0002
QUANNW       04 S-CUOTATOTAL-SEG4-D02  PIC S9(3)V9(15).                 RMB0002
QUANNW       04 S-CUOTATOTAL-SEG4-D03  PIC S9(3)V9(15).                 RMB0002
QUANNW       04 S-CUOTATOTAL-SEG4-D04  PIC S9(3)V9(15).                 RMB0002
QUANNW       04 S-CO01END008RO-SEG4-D01  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END008RO-SEG4-D04  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END037RO-SEG4-D01  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01NUM001CC-SEG4-D00  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01NUM001CC-SEG4-D03  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01ACP008CC-SEG4-D02  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END007IN-SEG4-D01  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END007IN-SEG4-D04  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01NUM002CC-SEG4-D03  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END028RO-SEG4-D04  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END013RO-SEG4-D04  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END030RO-SEG4-D04  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END010RO-SEG4-D04  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END003CC-SEG4-D01  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01NUM003AH-SEG4-D02  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END026IN-SEG4-D01  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END026IN-SEG4-D04  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO01END033CC-SEG4-D00  PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO00DEM030-SEG4-D01    PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO00DEM026-SEG4-D01    PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CUPOTOTAL-SEG4         PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO02NUM003TO-SEG4      PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO02EXP001TO-SEG4      PIC S9(3)V9(15).               RMB0002
QUANNW       04 S-CO00DEM003-SEG4        PIC S9(3)V9(15).               RMB0002
QUANNW       04 H11-SEG4                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H12-SEG4                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H13-SEG4                  PIC S9(4)V9(14).              RMB0002
QUANNW       04 H11A-SEG4                 PIC S9(4)V9(14).              RMB0002
QUANNW       04 H12A-SEG4                 PIC S9(4)V9(14).              RMB0002
QUANNW       04 H13A-SEG4                 PIC S9(4)V9(14).              RMB0002
          02 DUMMYS-S4-USAPI.
QUANNW       04 CUOTATOTAL-SEG4-D01  PIC 9(1).                          RMB0002
QUANNW       04 CUOTATOTAL-SEG4-D02  PIC 9(1).                          RMB0002
QUANNW       04 CUOTATOTAL-SEG4-D03  PIC 9(1).                          RMB0002
QUANNW       04 CUOTATOTAL-SEG4-D04  PIC 9(1).                          RMB0002
QUANNW       04 CO01END008RO-SEG4-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END008RO-SEG4-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END037RO-SEG4-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01NUM001CC-SEG4-D00  PIC 9(1).                        RMB0002
QUANNW       04 CO01NUM001CC-SEG4-D03  PIC 9(1).                        RMB0002
QUANNW       04 CO01ACP008CC-SEG4-D02  PIC 9(1).                        RMB0002
QUANNW       04 CO01END007IN-SEG4-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END007IN-SEG4-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01NUM002CC-SEG4-D03  PIC 9(1).                        RMB0002
QUANNW       04 CO01END028RO-SEG4-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END013RO-SEG4-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END030RO-SEG4-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END010RO-SEG4-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END003CC-SEG4-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01NUM003AH-SEG4-D02  PIC 9(1).                        RMB0002
QUANNW       04 CO01END026IN-SEG4-D01  PIC 9(1).                        RMB0002
QUANNW       04 CO01END026IN-SEG4-D04  PIC 9(1).                        RMB0002
QUANNW       04 CO01END033CC-SEG4-D00  PIC 9(1).                        RMB0002
QUANNW       04 CO00DEM030-SEG4-D01    PIC 9(1).                        RMB0002
QUANNW       04 CO00DEM026-SEG4-D01    PIC 9(1).                        RMB0002
