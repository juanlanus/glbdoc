REMARK*  SCOUSAHD-WS.V.0005
REMARK*****************************************************************
REMARK* ESTE COPY CONTIENE LAS DEFINICIONES DE LAS VARIABLES PROPIAS DEL
REMARK* SCORE PREDICTA HD
REMARK******************************************************************
REMARK* RMB0002    * IGUA01 : SCORE CARD 1 DEFINICIONES SIN CAMBIO
REMARK* 29-OCT-2009* NUEV01 : SCORE CARD 1 DEFINICIONES NUEVAS
REMARK*            * MODI01 : SCORE CARD 1 DEFINICIONES MODIFICADAS
REMARK**********************************************************************
REMARK* RMB0003    * EXCL01 : SE ADICIONA PARA EXCLUSION PARA CUANDO ES
REMARK* 22-DIC-2009*          SOLO REAL Y EXPERIENCIA MENOR A 7 MESES.
REMARK*            *          GUARDAR EL SCORE REAL CALCULADO 
REMARK**********************************************************************
REMARK* RMB0004    * OPTI01 : SE OPTIMIZA EL MODELO CON LA INCLUSIN DE
REMARK* 12-MAR-2010*          MORAS DE 120 EN LAS VARIABLES
REMARK**********************************************************************
REMARK* GJC0005    * EXCL02 : SE CREA VARIABLE PARA PODER CAMBIAR LA  
REMARK* 05-DIC-2011*          EXCLUSION 76                                 
REMARK* G.JIMENEZ  *
REMARK**********************************************************************

       01 VARIABLES-WORK-USAHD.       
          03 VLR-SMLMV-USAHD                    PIC 9(8).         
          03 SCORE-USAHD                        PIC 9(03).            
EXCL01    03 SCORE-SINEXC-USAHD                 PIC 9(03).              RMB0003
          03 ADVERSE-RAZON-USAHD OCCURS 4 TIMES PIC 9(02).      
          03 SCORE-CARD-USAHD                   PIC 9(01).
          03 SIN-EXCLUSION-USAHD                PIC 9(01).
          03 I-USAHD                            PIC 9(06).
          03 J-USAHD                            PIC 9(06).
          03 L-USAHD                            PIC 9(06).
          03 T-USAHD                            PIC 9(06).
          03 AREA-VALI-USAHD                    PIC X(2000).
          03 NOMB-VALI-USAHD                    PIC X(20).
          03 DUMMIES-DESPLEGAR-USAHD            PIC X(501).
       01 VARIABLES-USAHD.       
          02 HD-SCOREN                 PIC S9(09)V9(05).
          02 HD-QTOT                   PIC 9(04).            
          02 HD-QTOTAB                 PIC 9(04).            
OPTI01    02 HD-EVER12006              PIC 9(04).                       RMB0004
OPTI01    02 HD-EVER12012              PIC 9(04).                       RMB0004
OPTI01    02 HD-EVER12018              PIC 9(04).                       RMB0004
OPTI01    02 HD-EVER12024              PIC 9(04).                       RMB0004
OPTI01    02 HD-EVER12036              PIC 9(04).                       RMB0004
          02 HD-EVER12048              PIC 9(04).            
OPTI01    02 HD-NEW-EVER12048          PIC 9(04).                       RMB0004
OPTI01    02 HD-01MOR05TO              PIC 9(04).                       RMB0004
          02 HD-QREAL                  PIC 9(04).            
EXCL02    02 HD-MOB-SOLOREAL           PIC 9(04).                       GJC0005
          02 HD-QCTASDEFAULT           PIC 9(04).            
          02 HD-RECMORA                PIC 9(04).           
NUEV01    02 HD-RECMORA60              PIC 9(04).                       RMB0002
          02 HD-RECMORACC              PIC 9(04).           
          02 HD-A                      PIC 9(2)V9(16) 
                                       VALUE 0.969940959113301.
          02 HD-B                      PIC 9(2)V9(16) 
                                       VALUE 0.0483065327920809.
          02 HD-C                      PIC 9(2)V9(16) 
                                       VALUE 8.31776616671934.   
          02 HD-D                      PIC 9(2)V9(16) 
                                       VALUE 0.0138629436111989.
       01 VARIABLES-CARD01-USAHD.       
          02 ACOTADAS-S1-USAHD.
             04 HD-S1-QALDIA            PIC 9(05).
             04 HD-S1-CO00DEM003-C      PIC 9(03).
NUEV01       04 HD-S1-CO02EXP003TO-C    PIC 9(03).                      RMB0002
MODI01       04 HD-S1-TO01END004        PIC 9(07)V9(11).                RMB0002 
             04 HD-S1-EXPONENTE-C       PIC S9(3)V9(15).     
NUEV01       04 HD-S1-RECMORA           PIC 9(04).                      RMB0002
NUEV01       04 HD-S1-RECMORA60         PIC 9(04).                      RMB0002
          02 DUMMYS-S1-USAHD.
IGUA01       04 HD-S1-CO00DEM016D01     PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO00DEM016D02     PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO00DEM016D03     PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO02END001CBD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO02END001CBD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO02END001CBD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO02END001CBD04   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END017CCD01   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END017CCD02   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END017CCD03   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END017CCD04   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END018CCD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END018CCD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END018CCD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END027CCD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END027CCD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END027CCD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END027CCD04   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01END027CCD05   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END027OTD01   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END027OTD02   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END027OTD03   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01END027OTD04   PIC 9(1).                       RMB0002
MODI01       04 HD-S1-TO01END004D01     PIC 9(1).                       RMB0002
MODI01       04 HD-S1-TO01END004D02     PIC 9(1).                       RMB0002
MODI01       04 HD-S1-TO01END004D03     PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR090CCD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR090CCD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR090CCD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR022OTD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR022OTD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR022OTD03   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01MOR101OTD01   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01MOR101OTD02   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01MOR101OTD03   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01MOR101OTD04   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-RECMORAD01        PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-RECMORAD02        PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-RECMORAD03        PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-RECMORAD04        PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-RECMORAD05        PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-RECMORA60D01      PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-RECMORA60D02      PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR005CCD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR005CCD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR005CCD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR017CCD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR017CCD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR017CCD03   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01MOR033CCD01   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01MOR033CCD02   PIC 9(1).                       RMB0002
IGUA01       04 HD-S1-CO01MOR033CCD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR034CCD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR034CCD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR034CCD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR034CCD04   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR035OTD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR035OTD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR035OTD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR043OTD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR043OTD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01MOR043OTD03   PIC 9(1).                       RMB0002
MODI01       04 HD-S1-CO02NUM028TOD01   PIC 9(1).                       RMB0002
MODI01       04 HD-S1-CO02NUM028TOD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01NUM050AHD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01NUM050AHD02   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO01NUM050AHD03   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO02NUM018TOD01   PIC 9(1).                       RMB0002
NUEV01       04 HD-S1-CO02NUM018TOD02   PIC 9(1).                       RMB0002
       01 VARIABLES-CARD02-USAHD.       
          02 ACOTADAS-S2-USAHD.
             04 HD-S2-EXPONENTE-C       PIC S9(3)V9(15).     
             04 HD-S2-RECMORA           PIC 9(04).           
             04 HD-S2-QTOT90            PIC 9(05).
             04 HD-S2-EVER9048          PIC 9(05).
          02 DUMMYS-S2-USAHD.
             04 HD-S2-CO01NUM050AHD01   PIC 9(1).    
             04 HD-S2-CO01NUM050AHD02   PIC 9(1).    
             04 HD-S2-CO01NUM050AHD03   PIC 9(1).    
             04 HD-S2-CO01NUM050AHD04   PIC 9(1).    
             04 HD-S2-CO01END004IND01   PIC 9(1).    
             04 HD-S2-CO01END004IND02   PIC 9(1).    
             04 HD-S2-CO01END004IND03   PIC 9(1).    
             04 HD-S2-CO01END004IND04   PIC 9(1).    
             04 HD-S2-CO01END012OTD01   PIC 9(1).    
             04 HD-S2-CO01END012OTD02   PIC 9(1).    
             04 HD-S2-CO01END012OTD03   PIC 9(1).    
             04 HD-S2-CO01NUM049CCD01   PIC 9(1).    
             04 HD-S2-CO01NUM049CCD02   PIC 9(1).    
             04 HD-S2-CO01NUM049CCD03   PIC 9(1).    
             04 HD-S2-CO01END089ROD01   PIC 9(1).    
             04 HD-S2-CO01END089ROD02   PIC 9(1).    
             04 HD-S2-CO01END089ROD03   PIC 9(1).    
             04 HD-S2-CO01END089ROD04   PIC 9(1).    
             04 HD-S2-CO01END089ROD05   PIC 9(1).    
             04 HD-S2-CO02END017CBD01   PIC 9(1).    
             04 HD-S2-CO02END017CBD02   PIC 9(1).    
             04 HD-S2-CO02END017CBD03   PIC 9(1).    
             04 HD-S2-CO02END017CBD04   PIC 9(1).    
             04 HD-S2-CO01ACP009OTD01   PIC 9(1).    
             04 HD-S2-CO01ACP009OTD02   PIC 9(1).    
             04 HD-S2-CO01ACP009OTD03   PIC 9(1).    
             04 HD-S2-RECMORAD01        PIC 9(1).    
             04 HD-S2-RECMORAD02        PIC 9(1).    
             04 HD-S2-RECMORAD03        PIC 9(1).    
             04 HD-S2-RECMORAD04        PIC 9(1).    
             04 HD-S2-RECMORAD05        PIC 9(1).    
             04 HD-S2-CO01MOR080CCD01   PIC 9(1).    
             04 HD-S2-CO01MOR080CCD02   PIC 9(1).    
             04 HD-S2-CO01MOR080CCD03   PIC 9(1).    
             04 HD-S2-CO01MOR080CCD04   PIC 9(1).    
             04 HD-S2-EVER9048D01       PIC 9(1).    
             04 HD-S2-EVER9048D02       PIC 9(1).    
       01 VARIABLES-CARD03-USAHD.       
          02 ACOTADAS-S3-USAHD.
             04 HD-S3-EXPONENTE-C       PIC S9(3)V9(15).     
             04 HD-S3-CO00DEM003-C      PIC 9(03).
             04 HD-S3-MMAXHIST          PIC 9(03).
             04 HD-S3-OPEN6             PIC 9(03).
             04 HD-S3-SALDOMO           PIC 9(11)V9(5).
          02 DUMMYS-S3-USAHD.
             04 HD-S3-CO02EXP008CCD01   PIC 9(1).    
             04 HD-S3-CO02EXP008CCD02   PIC 9(1).    
             04 HD-S3-CO02EXP008CCD03   PIC 9(1).    
             04 HD-S3-CO01EXP008COD01   PIC 9(1).    
             04 HD-S3-CO01EXP008COD02   PIC 9(1).    
             04 HD-S3-CO01MOR008IND01   PIC 9(1).    
             04 HD-S3-CO01MOR008IND02   PIC 9(1).    
             04 HD-S3-CO01MOR008IND03   PIC 9(1).    
             04 HD-S3-CO01MOR009IND01   PIC 9(1).    
             04 HD-S3-CO01MOR009IND02   PIC 9(1).    
             04 HD-S3-CO01MOR009IND03   PIC 9(1).    
             04 HD-S3-CO01NUM042IND01   PIC 9(1).    
             04 HD-S3-CO01NUM042IND02   PIC 9(1).    
             04 HD-S3-CO01NUM042IND03   PIC 9(1).    
             04 HD-S3-MMAXHISTD01       PIC 9(1).
             04 HD-S3-MMAXHISTD02       PIC 9(1).
             04 HD-S3-MMAXHISTD03       PIC 9(1).
             04 HD-S3-MMAXHISTD04       PIC 9(1).
             04 HD-S3-OPEN6D01          PIC 9(1).
             04 HD-S3-OPEN6D02          PIC 9(1).
             04 HD-S3-OPEN6D03          PIC 9(1).
             04 HD-S3-OPEN6D04          PIC 9(1).
             04 HD-S3-CO01NUM001OTD01   PIC 9(1).    
             04 HD-S3-CO01NUM001OTD02   PIC 9(1).    
             04 HD-S3-CO01END020ROD01   PIC 9(1).    
             04 HD-S3-CO01END020ROD02   PIC 9(1).    
             04 HD-S3-CO01END020ROD03   PIC 9(1).    
             04 HD-S3-CO01END020ROD04   PIC 9(1).    
             04 HD-S3-CO01END020ROD05   PIC 9(1).    
             04 HD-S3-CO01END080ROD01   PIC 9(1).    
             04 HD-S3-CO01END080ROD02   PIC 9(1).    
             04 HD-S3-CO01END080ROD03   PIC 9(1).    
             04 HD-S3-CO01END085ROD01   PIC 9(1).    
             04 HD-S3-CO01END085ROD02   PIC 9(1).    
             04 HD-S3-CO01END085ROD03   PIC 9(1).    
             04 HD-S3-SALDOMOD01        PIC 9(1).    
             04 HD-S3-SALDOMOD02        PIC 9(1).    
             04 HD-S3-SALDOMOD03        PIC 9(1).    
OPTI01       04 HD-S3-QTOTABD01         PIC 9(1).                       RMB0004
OPTI01       04 HD-S3-QTOTABD02         PIC 9(1).                       RMB0004
OPTI01       04 HD-S3-QTOTABD03         PIC 9(1).                       RMB0004
OPTI01       04 HD-S3-QTOTABD04         PIC 9(1).                       RMB0004
OPTI01       04 HD-S3-CO01END006ROD01   PIC 9(1).                       RMB0004
OPTI01       04 HD-S3-CO01END006ROD02   PIC 9(1).                       RMB0004
OPTI01       04 HD-S3-CO01END006ROD03   PIC 9(1).                       RMB0004
OPTI01       04 HD-S3-CO01END006ROD04   PIC 9(1).                       RMB0004
       01 VARIABLES-CARD04-USAHD.       
          02 ACOTADAS-S4-USAHD.
             04 HD-S4-EXPONENTE-C       PIC S9(3)V9(15).     
             04 HD-S4-EVER6024          PIC 9(05).
             04 HD-S4-QALDIA            PIC 9(05).
             04 HD-S4-RECMORACC         PIC 9(04).           
OPTI01       04 HD-S4-RECMORACCUSA      PIC 9(04).                      RMB0004
             04 HD-S4-MORMAX            PIC 9(04).           
             04 HD-S4-SALDOMO           PIC 9(11)V9(5).
          02 DUMMYS-S4-USAHD.
             04 HD-S4-CO01MOR083CCD01   PIC 9(1).    
             04 HD-S4-CO01MOR083CCD02   PIC 9(1).    
             04 HD-S4-CO01MOR083CCD03   PIC 9(1).    
             04 HD-S4-EVER6024D01       PIC 9(1).
             04 HD-S4-EVER6024D02       PIC 9(1).
             04 HD-S4-EVER6024D03       PIC 9(1).
             04 HD-S4-CO01MOR098IND01   PIC 9(1).    
             04 HD-S4-CO01MOR098IND02   PIC 9(1).    
             04 HD-S4-CO01MOR098IND03   PIC 9(1).    
             04 HD-S4-CO01MOR098IND04   PIC 9(1).    
             04 HD-S4-CO01MOR098IND05   PIC 9(1).    
             04 HD-S4-QALDIAD01         PIC 9(1).
             04 HD-S4-QALDIAD02         PIC 9(1).
             04 HD-S4-QALDIAD03         PIC 9(1).
             04 HD-S4-QALDIAD04         PIC 9(1).
             04 HD-S4-RECMORACCD01      PIC 9(1).           
             04 HD-S4-RECMORACCD02      PIC 9(1).           
             04 HD-S4-RECMORACCD03      PIC 9(1).           
             04 HD-S4-RECMORACCD04      PIC 9(1).           
             04 HD-S4-MORMAXD01         PIC 9(1).           
             04 HD-S4-MORMAXD02         PIC 9(1).           
             04 HD-S4-MORMAXD03         PIC 9(1).           
             04 HD-S4-MORMAXD04         PIC 9(1).           
             04 HD-S4-CO01MOR008IND01   PIC 9(1).    
             04 HD-S4-CO01MOR008IND02   PIC 9(1).    
             04 HD-S4-CO01MOR008IND03   PIC 9(1).    
             04 HD-S4-CO01MOR009VED01   PIC 9(1).    
             04 HD-S4-CO01MOR009VED02   PIC 9(1).    
             04 HD-S4-CO01MOR009VED03   PIC 9(1).    
             04 HD-S4-CO01MOR009COD01   PIC 9(1).    
             04 HD-S4-CO01MOR009COD02   PIC 9(1).    
             04 HD-S4-CO01MOR009COD03   PIC 9(1).    
             04 HD-S4-CO01MOR009COD04   PIC 9(1).    
             04 HD-S4-CO01MOR009CCD01   PIC 9(1).    
             04 HD-S4-CO01MOR009CCD02   PIC 9(1).    
             04 HD-S4-CO01MOR009CCD03   PIC 9(1).    
             04 HD-S4-CO01MOR009CCD04   PIC 9(1).    
             04 HD-S4-CO01END017CCD01   PIC 9(1).    
             04 HD-S4-CO01END017CCD02   PIC 9(1).    
             04 HD-S4-CO01END017CCD03   PIC 9(1).    
             04 HD-S4-CO01END022COD01   PIC 9(1).    
             04 HD-S4-CO01END022COD02   PIC 9(1).    
             04 HD-S4-CO01END022COD03   PIC 9(1).    
             04 HD-S4-CO01END022COD04   PIC 9(1).    
             04 HD-S4-CO01END080ROD01   PIC 9(1).    
             04 HD-S4-CO01END080ROD02   PIC 9(1).    
             04 HD-S4-CO01END080ROD03   PIC 9(1).    
             04 HD-S4-CO01END080ROD04   PIC 9(1).    
             04 HD-S4-CO01END080ROD05   PIC 9(1).    
             04 HD-S4-CO01END087ROD01   PIC 9(1).    
             04 HD-S4-CO01END087ROD02   PIC 9(1).    
             04 HD-S4-CO01END087ROD03   PIC 9(1).    
             04 HD-S4-CO01END087ROD04   PIC 9(1).    
             04 HD-S4-CO01END087ROD05   PIC 9(1).    
             04 HD-S4-CO02END018CBD01   PIC 9(1).    
             04 HD-S4-CO02END018CBD02   PIC 9(1).    
             04 HD-S4-CO02END018CBD03   PIC 9(1).    
             04 HD-S4-SALDOMOD01        PIC 9(1).
             04 HD-S4-SALDOMOD02        PIC 9(1).
             04 HD-S4-SALDOMOD03        PIC 9(1).
             04 HD-S4-SALDOMOD04        PIC 9(1).
             04 HD-S4-SALDOMOD05        PIC 9(1).
             04 HD-S4-CO01EXP006CCD01   PIC 9(1).    
             04 HD-S4-CO01EXP006CCD02   PIC 9(1).    
             04 HD-S4-CO01EXP006CCD03   PIC 9(1).    
             04 HD-S4-CO01EXP006CCD04   PIC 9(1).    
             04 HD-S4-CO01EXP006CCD05   PIC 9(1).    
             04 HD-S4-CO02EXP008OTD01   PIC 9(1).    
             04 HD-S4-CO02EXP008OTD02   PIC 9(1).    
             04 HD-S4-CO02EXP008OTD03   PIC 9(1).    
             04 HD-S4-CO01EXP006ROD01   PIC 9(1).    
             04 HD-S4-CO01EXP006ROD02   PIC 9(1).    
             04 HD-S4-CO01EXP006ROD03   PIC 9(1).    
             04 HD-S4-CO01EXP006ROD04   PIC 9(1).    
             04 HD-S4-CO01EXP001IND01   PIC 9(1).    
             04 HD-S4-CO01EXP001IND02   PIC 9(1).    
             04 HD-S4-CO01EXP001IND03   PIC 9(1).    
             04 HD-S4-CO01EXP001IND04   PIC 9(1).    
             04 HD-S4-CO01EXP001IND05   PIC 9(1).    
             04 HD-S4-CO02ACP002D01     PIC 9(1).    
             04 HD-S4-CO02ACP002D02     PIC 9(1).    
             04 HD-S4-CO02ACP002D03     PIC 9(1).    
             04 HD-S4-CO02ACP002D04     PIC 9(1).    
             04 HD-S4-CO02ACP002D05     PIC 9(1).    
             04 HD-S4-CO00DEM016D01     PIC 9(1).    
             04 HD-S4-CO00DEM016D02     PIC 9(1).    
             04 HD-S4-CO00DEM016D03     PIC 9(1).    
       01 VARIABLES-CARD05-USAHD.       
          02 ACOTADAS-S5-USAHD.
             04 HD-S5-EXPONENTE-C       PIC S9(3)V9(15).     
             04 HD-S5-CO00DEM003-C      PIC 9(03).
             04 HD-S5-EVER3024          PIC 9(05).
             04 HD-S5-MOB6              PIC 9(05).
             04 HD-S5-OPEN12            PIC 9(05).
             04 HD-S5-TIENEHP           PIC 9(05).
             04 HD-S5-TO01END004        PIC 9(09)V9(3).
          02 DUMMYS-S5-USAHD.
             04 HD-S5-CO00DEM016D01     PIC 9(1).    
             04 HD-S5-CO00DEM016D02     PIC 9(1).    
             04 HD-S5-CO02ACP002D01     PIC 9(1).    
             04 HD-S5-CO02ACP002D02     PIC 9(1).    
             04 HD-S5-CO02ACP002D03     PIC 9(1).    
             04 HD-S5-CO02ACP002D04     PIC 9(1).    
             04 HD-S5-CO02END019CBD01   PIC 9(1).    
             04 HD-S5-CO02END019CBD02   PIC 9(1).    
             04 HD-S5-CO02END019CBD03   PIC 9(1).    
             04 HD-S5-CO02NUM043CBD01   PIC 9(1).    
             04 HD-S5-CO02NUM043CBD02   PIC 9(1).    
             04 HD-S5-CO02NUM043CBD03   PIC 9(1).    
             04 HD-S5-CO01END017CCD01   PIC 9(1).    
             04 HD-S5-CO01END017CCD02   PIC 9(1).    
             04 HD-S5-CO01END017CCD03   PIC 9(1).    
             04 HD-S5-CO01EXP001CCD01   PIC 9(1).    
             04 HD-S5-CO01EXP001CCD02   PIC 9(1).    
             04 HD-S5-CO01EXP001CCD03   PIC 9(1).    
             04 HD-S5-CO01EXP001CCD04   PIC 9(1).    
             04 HD-S5-CO01EXP001CCD05   PIC 9(1).    
             04 HD-S5-CO01END007COD01   PIC 9(1).    
             04 HD-S5-CO01END007COD02   PIC 9(1).    
             04 HD-S5-CO01END007COD03   PIC 9(1).    
             04 HD-S5-CO01END007COD04   PIC 9(1).    
             04 HD-S5-CO01END007COD05   PIC 9(1).    
             04 HD-S5-CO01END021COD01   PIC 9(1).    
             04 HD-S5-CO01END021COD02   PIC 9(1).    
             04 HD-S5-CO01END021COD03   PIC 9(1).    
             04 HD-S5-CO01END021COD04   PIC 9(1).    
             04 HD-S5-CO01END021COD05   PIC 9(1).    
             04 HD-S5-CO01EXP006CTD01   PIC 9(1).    
             04 HD-S5-CO01EXP006CTD02   PIC 9(1).    
             04 HD-S5-CO01EXP006CTD03   PIC 9(1).    
             04 HD-S5-CO01EXP006CTD04   PIC 9(1).    
             04 HD-S5-EVER3024D01       PIC 9(1).
             04 HD-S5-EVER3024D02       PIC 9(1).
             04 HD-S5-EVER3024D03       PIC 9(1).
             04 HD-S5-CO01END018IND01   PIC 9(1).
             04 HD-S5-CO01END018IND02   PIC 9(1).
             04 HD-S5-CO01END018IND03   PIC 9(1).
             04 HD-S5-CO01MOR068IND01   PIC 9(1).
             04 HD-S5-CO01MOR068IND02   PIC 9(1).
             04 HD-S5-CO01MOR068IND03   PIC 9(1).
             04 HD-S5-CO01NUM002IND01   PIC 9(1).
             04 HD-S5-CO01NUM002IND02   PIC 9(1).
             04 HD-S5-CO01NUM002IND03   PIC 9(1).
             04 HD-S5-MOB6D01           PIC 9(1).
             04 HD-S5-MOB6D02           PIC 9(1).
             04 HD-S5-MOB6D03           PIC 9(1).
             04 HD-S5-MOB6D04           PIC 9(1).
             04 HD-S5-OPEN12D01         PIC 9(1).
             04 HD-S5-OPEN12D02         PIC 9(1).
             04 HD-S5-OPEN12D03         PIC 9(1).
             04 HD-S5-CO01EXP003OTD01   PIC 9(1).
             04 HD-S5-CO01EXP003OTD02   PIC 9(1).
             04 HD-S5-CO01EXP003OTD03   PIC 9(1).
             04 HD-S5-CO01ACP001ROD01   PIC 9(1).
             04 HD-S5-CO01ACP001ROD02   PIC 9(1).
             04 HD-S5-CO01ACP001ROD03   PIC 9(1).
             04 HD-S5-CO01END030ROD01   PIC 9(1).
             04 HD-S5-CO01END030ROD02   PIC 9(1).
             04 HD-S5-CO01END030ROD03   PIC 9(1).
             04 HD-S5-CO01END030ROD04   PIC 9(1).
             04 HD-S5-CO01END030ROD05   PIC 9(1).
             04 HD-S5-CO01END071ROD01   PIC 9(1).
             04 HD-S5-CO01END071ROD02   PIC 9(1).
             04 HD-S5-CO01END071ROD03   PIC 9(1).
             04 HD-S5-CO01END071ROD04   PIC 9(1).
             04 HD-S5-CO01END071ROD05   PIC 9(1).
             04 HD-S5-CO01END080ROD01   PIC 9(1).
             04 HD-S5-CO01END080ROD02   PIC 9(1).
             04 HD-S5-CO01END080ROD03   PIC 9(1).
             04 HD-S5-CO01END080ROD04   PIC 9(1).
             04 HD-S5-CO01END080ROD05   PIC 9(1).
             04 HD-S5-CO02END045ROD01   PIC 9(1).
             04 HD-S5-CO02END045ROD02   PIC 9(1).
             04 HD-S5-CO02END045ROD03   PIC 9(1).
             04 HD-S5-CO02END045ROD04   PIC 9(1).
             04 HD-S5-CO02END045ROD05   PIC 9(1).
             04 HD-S5-TIENEHPD01        PIC 9(1).
             04 HD-S5-TIENEHPD02        PIC 9(1).
             04 HD-S5-TO01END004D01     PIC 9(1).
             04 HD-S5-TO01END004D02     PIC 9(1).
             04 HD-S5-CO01END005VED01   PIC 9(1).
             04 HD-S5-CO01END005VED02   PIC 9(1).
             04 HD-S5-CO01END005VED03   PIC 9(1).
             04 HD-S5-CO01EXP001AHD01   PIC 9(1).
             04 HD-S5-CO01EXP001AHD02   PIC 9(1).
             04 HD-S5-CO01EXP001AHD03   PIC 9(1).
             04 HD-S5-CO01EXP001AHD04   PIC 9(1).
             04 HD-S5-CO02EXP013TOD01   PIC 9(1).
             04 HD-S5-CO02EXP013TOD02   PIC 9(1).
             04 HD-S5-CO02EXP013TOD03   PIC 9(1).
       01 VARIABLES-CARD06-USAHD.       
          02 ACOTADAS-S6-USAHD.
             04 HD-S6-EXPONENTE-C       PIC S9(3)V9(15).     
             04 HD-S6-TIENEHP           PIC 9(05).
             04 HD-S6-CO00DEM003-C      PIC 9(03).
             04 HD-S6-RECMORA           PIC 9(05).
             04 HD-S6-CO02ACP002-C      PIC 9(04).
          02 DUMMYS-S6-USAHD.
             04 HD-S6-CO01MOR061IND01   PIC 9(1).    
             04 HD-S6-CO01MOR061IND02   PIC 9(1).    
             04 HD-S6-CO01MOR061IND03   PIC 9(1).    
             04 HD-S6-TIENEHPD01        PIC 9(1).
             04 HD-S6-TIENEHPD02        PIC 9(1).
             04 HD-S6-CO01NUM050AHD01   PIC 9(1).    
             04 HD-S6-CO01NUM050AHD02   PIC 9(1).    
             04 HD-S6-CO01NUM050AHD03   PIC 9(1).    
             04 HD-S6-CO01NUM050AHD04   PIC 9(1).    
             04 HD-S6-CO01NUM001OTD01   PIC 9(1).    
             04 HD-S6-CO01NUM001OTD02   PIC 9(1).    
             04 HD-S6-CO01NUM001OTD03   PIC 9(1).    
             04 HD-S6-CO02NUM035TOD01   PIC 9(1).    
             04 HD-S6-CO02NUM035TOD02   PIC 9(1).    
             04 HD-S6-CO02NUM035TOD03   PIC 9(1).    
             04 HD-S6-CO01END089ROD01   PIC 9(1).    
             04 HD-S6-CO01END089ROD02   PIC 9(1).    
             04 HD-S6-CO01END089ROD03   PIC 9(1).    
             04 HD-S6-CO01END089ROD04   PIC 9(1).    
             04 HD-S6-CO01END089ROD05   PIC 9(1).    
             04 HD-S6-CO01EXP004AHD01   PIC 9(1).    
             04 HD-S6-CO01EXP004AHD02   PIC 9(1).    
             04 HD-S6-CO01EXP004AHD03   PIC 9(1).    
             04 HD-S6-CO01EXP008CCD01   PIC 9(1).    
             04 HD-S6-CO01EXP008CCD02   PIC 9(1).    
             04 HD-S6-CO01EXP008CCD03   PIC 9(1).    
             04 HD-S6-CO02EXP001TOD01   PIC 9(1).    
             04 HD-S6-CO02EXP001TOD02   PIC 9(1).    
             04 HD-S6-CO02EXP001TOD03   PIC 9(1).    
             04 HD-S6-CO02EXP001TOD04   PIC 9(1).    
             04 HD-S6-CO02EXP002TOD01   PIC 9(1).    
             04 HD-S6-CO02EXP002TOD02   PIC 9(1).    
             04 HD-S6-CO02EXP002TOD03   PIC 9(1).    
             04 HD-S6-CO02EXP002TOD04   PIC 9(1).    
             04 HD-S6-CO01ACP005ROD01   PIC 9(1).    
             04 HD-S6-CO01ACP005ROD02   PIC 9(1).    
             04 HD-S6-CO01ACP005ROD03   PIC 9(1).    
             04 HD-S6-CO01ACP005ROD04   PIC 9(1).    
             04 HD-S6-CO00DEM016D01     PIC 9(1).    
             04 HD-S6-CO00DEM016D02     PIC 9(1).    
