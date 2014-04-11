REMARK*  SCOUSAAA-WS.V.0004
REMARK*****************************************************************
REMARK* ESTE COPY CONTIENE LAS DEFINICIONES DE LAS VARIABLES PROPIAS DEL
REMARK* SCORE ACIERTA A    
REMARK******************************************************************
      * RMB0002     * DTRN01 : SCORCARD 3 : ROTATIVOS Y TC NUEVOS      *
      * R.MARTINEZ  * DTRE01 : SCORCARD 4 : ROTATIVOS Y TC EXISTENTES  *
      * 2012-OCT-10 * DINT01 : SCORCARD 5 : INSTALAMENTOS NUEVOS Y EXIS*
      *             * DHVT01 : SCORCARD 6 : HIPO Y AUTOMO NUEVOS Y EXIS*
      *             * DFUT01 : SCORCARD 9 : FINANCIERO UN NUEVOS Y EXIS*
      ******************************************************************
      * RMB0003     * DCPN01 : SCORCARD 7 : COOPERATIVO    NUEVOS      *
      * R.MARTINEZ  * DCPE01 : SCORCARD 8 : COOPERATIVO    EXISTENTES  *
      * 2012-OCT-05 * COMB01 : FILOSOFIA DE COMBOS                     *
      ******************************************************************
REMARK* RMB0004     * EXCL01 : SE CORRIGE LA EXCLUSION 075 PARA QUE    *
REMARK* R.MARTINEZ  *        : MANEJE BIEN LAS CARTERAS CASTIGAS.      *
REMARK* 2013-ENE-25 *        : COPIA LA ESTRATEGIA HECHA EN  ACIERTA + *
REMARK******************************************************************
       01 PORTAFOLIO-USAAA                      PIC X(04) VALUE SPACES.
COMB01 01 PORTAFOLIO-COMBO-USAAA                PIC X(04) VALUE SPACES. RMB0003
COMB01 01 INF-SCORE-COMBO-USAAA.                                        RMB0003
COMB01    03 SCORE-CARD-COMBO-USAAA             PIC 9(01).              RMB0003
COMB01    03 ADVERSE-RAZON-COMBO-USAAA OCCURS 4 TIMES PIC 9(03).        RMB0003
COMB01    03 SCORE-COMBO-USAAA                  PIC 9(03).              RMB0003
COMB01    03 PROBABILIDAD-COMBO-USAAA           PIC 9(03).              RMB0003
       01 VARIABLES-WORK-USAAA.       
          03 AA-SCOREN                          PIC S999999V9(12).
          03 VLR-SMLMV-USAAA                    PIC 9(8).
          03 SCORE-USAAA                        PIC 9(03).            
          03 SCORE-DTRO                         PIC 9(03).            
          03 SCORE-DINS                         PIC 9(03).            
          03 SCORE-DHVE                         PIC 9(03).            
          03 PROBABILIDAD-USAAA                 PIC 9(03).            
          03 ADVERSE-RAZON-USAAA OCCURS 4 TIMES PIC 9(03).      
          03 SCORE-CARD-USAAA                   PIC 9(01).
          03 SIN-EXCLUSION-USAAA                PIC 9(01).
          03 I-USAAA                            PIC 9(06).
          03 J-USAAA                            PIC 9(06).
          03 L-USAAA                            PIC 9(06).
          03 T-USAAA                            PIC 9(06).
          03 AREA-VALI-USAAA                    PIC X(2000).
          03 NOMB-VALI-USAAA                    PIC X(20).
          03 DUMMIES-DESPLEGAR-USAAA            PIC X(501).
       01 VARIABLES-USAAA.       
          02 AA-EXPOSICION             PIC S9(04)V9(04).
          02 AA-PREEXPOSICIONRO        PIC S9(04)V9(04).
          02 AA-PREEXPOSICIONIN        PIC S9(04)V9(04).
          02 AA-PREEXPOSICIONVE        PIC S9(04)V9(04).
          02 AA-PREEXPOSICIONHP        PIC S9(04)V9(04).
          02 AA-PREEXPOSICIONOT        PIC S9(04)V9(04).
          02 AA-EVER03006P             PIC S9(04).
          02 AA-RECMORA001             PIC S9(04).
          02 AA-RECMORA001VSA          PIC S9(04).
          02 AA-RECMORA030             PIC S9(04).
          02 AA-RECMORA060             PIC S9(04).
          02 AA-RECMORA090             PIC S9(04).
          02 AA-RECMORA120             PIC S9(04).
          02 AA-RECMORACC              PIC S9(04).
          02 AA-MMAXHIS                PIC S9(04).
          02 AA-MMAX24                 PIC S9(04).
DCPE01    02 AA-MMAX48                 PIC S9(04).                      RMB0003
          02 AA-FINANCIERO             PIC S9(01).
          02 AA-REAL                   PIC S9(01).
          02 AA-TELCOS                 PIC S9(01).
          02 AA-TIPOREL                PIC X(06).
          02 AA-TIPORELD01             PIC S9(01).
          02 AA-TIPORELD02             PIC S9(01).
          02 AA-TIPORELD03             PIC S9(01).
          02 AA-TIPORELD04             PIC S9(01).
          02 AA-M30                    PIC S9(04).
          02 AA-M60                    PIC S9(04).
          02 AA-M90                    PIC S9(04).
          02 AA-M120                   PIC S9(04).
          02 AA-DMOR                   PIC S9(04).
EXCL01    02 AA-EMOR                   PIC S9(04).                      RMB0004
EXCL01    02 AA-QCTASDEFAULT           PIC S9(04).                      RMB0004
EXCL01    02 AA-QTOTABHD               PIC S9(04).                      RMB0004
          02 AA-QALDIA                 PIC S9(04).
          02 AA-PRQALDIA               PIC S9(04)V9(04).
          02 AA-QTOT030                PIC S9(04).
          02 AA-QTOT060                PIC S9(04).
          02 AA-QTOT090                PIC S9(04).
          02 AA-QTOT120                PIC S9(04).
          02 AA-QTOTCC                 PIC S9(04).
          02 AA-QTOTCR                 PIC S9(04).
          02 AA-QTOTMR                 PIC S9(04)V9(04).
          02 AA-QTOTAB                 PIC S9(04).            
DTRN01    02 AA-QTOT                   PIC S9(04).                      RMB0002
DTRN01    02 AA-PROQTOTAB              PIC S9(04)V9(04).                RMB0002
DFUT01    02 AA-MOB12                  PIC S9(04).                      RMB0002
DFUT01    02 AA-MOB60M                 PIC S9(04).                      RMB0002
       01 VARIABLES-CARD01-USAAA.       
          02 ACOTADAS-S1-USAAA.
             04 AA-S1-EXPONENTE            PIC S9(3)V9(15).     
             04 AA-S1-TIPORELD01-DCCN      PIC S9(8)V9(4).
             04 AA-S1-TIPORELD04-DCCN      PIC S9(8)V9(4).
             04 AA-S1-CO02EXP008TO-DCCN    PIC S9(8)V9(4).
             04 AA-S1-CO02EXP001TO-DCCN    PIC S9(8)V9(4).
             04 AA-S1-PRQALDIA-DCCN        PIC S9(8)V9(4).
             04 AA-S1-CO02NUM029TO-DCCN    PIC S9(8)V9(4).
             04 AA-S1-CO02EXP006TO-DCCN    PIC S9(8)V9(4).
          02 DUMMYS-S1-USAAA.
             04 AA-S1-MMAXHIS-DCCND01   PIC 9(1).                       
             04 AA-S1-MMAXHIS-DCCND02   PIC 9(1).                       
             04 AA-S1-CO02END015CB-DCCND01  PIC 9(1).
             04 AA-S1-CO02END015CB-DCCND02  PIC 9(1).
             04 AA-S1-RECMORA001VSA-DCCND01 PIC 9(1).
             04 AA-S1-RECMORA001VSA-DCCND02 PIC 9(1).
             04 AA-S1-RECMORA001VSA-DCCND03 PIC 9(1).
             04 AA-S1-CO01END089RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01END089RO-DCCND02  PIC 9(1).
             04 AA-S1-CO01END089RO-DCCND03  PIC 9(1).
             04 AA-S1-CO01END089RO-DCCND04  PIC 9(1).
             04 AA-S1-CO01END088RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01END088RO-DCCND02  PIC 9(1).
             04 AA-S1-CO01END088RO-DCCND03  PIC 9(1).
             04 AA-S1-CO01END088RO-DCCND04  PIC 9(1).
             04 AA-S1-CO01EXP001RO-DCCND00  PIC 9(1).
             04 AA-S1-CO01EXP001RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01EXP001RO-DCCND02  PIC 9(1).
             04 AA-S1-CO01EXP001RO-DCCND03  PIC 9(1).
             04 AA-S1-CO01EXP001RO-DCCND04  PIC 9(1).
             04 AA-S1-CO01EXP001AH-DCCND00  PIC 9(1).
             04 AA-S1-CO01EXP001AH-DCCND01  PIC 9(1).
             04 AA-S1-CO01EXP001AH-DCCND02  PIC 9(1).
             04 AA-S1-CO01EXP001AH-DCCND03  PIC 9(1).
             04 AA-S1-CO01EXP001AH-DCCND04  PIC 9(1).
             04 AA-S1-CO01MOR040RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01MOR040RO-DCCND02  PIC 9(1).
             04 AA-S1-CO01MOR040RO-DCCND03  PIC 9(1).
             04 AA-S1-EVER03006P-DCCND01    PIC 9(1).
             04 AA-S1-EVER03006P-DCCND02    PIC 9(1).
             04 AA-S1-CO01END030RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01END030RO-DCCND02  PIC 9(1).
             04 AA-S1-CO01END030RO-DCCND03  PIC 9(1).
             04 AA-S1-CO01END030RO-DCCND04  PIC 9(1).
             04 AA-S1-CO01MOR062RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01MOR062RO-DCCND02  PIC 9(1).
             04 AA-S1-EXPOSICION-DCCND01    PIC 9(1).
             04 AA-S1-EXPOSICION-DCCND02    PIC 9(1).
             04 AA-S1-EXPOSICION-DCCND03    PIC 9(1).
             04 AA-S1-EXPOSICION-DCCND04    PIC 9(1).
             04 AA-S1-CO01END022RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01END022RO-DCCND02  PIC 9(1).
             04 AA-S1-CO01MOR055CC-DCCND01  PIC 9(1).
             04 AA-S1-CO01MOR055CC-DCCND02  PIC 9(1).
             04 AA-S1-CO01NUM050RO-DCCND01  PIC 9(1).
             04 AA-S1-CO01NUM050RO-DCCND02  PIC 9(1).
             04 AA-S1-CO01NUM050RO-DCCND03  PIC 9(1).
       01 VARIABLES-CARD02-USAAA.       
          02 ACOTADAS-S2-USAAA.
             04 AA-S2-EXPONENTE         PIC S9(3)V9(15).     
             04 AA-S2-PRQALDIA-DCCE        PIC S9(8)V9(4).
             04 AA-S2-CO02EXP001TO-DCCE    PIC S9(8)V9(4).
             04 AA-S2-CO01EXP001CC-DCCE    PIC S9(8)V9(4).
             04 AA-S2-CO02NUM042CB-DCCE    PIC S9(8)V9(4).
             04 AA-S2-CO02EXP006TO-DCCE    PIC S9(8)V9(4).
             04 AA-S2-CO01NUM050CC-DCCE    PIC S9(8)V9(4).
             04 AA-S2-TIPORELD04-DCCE      PIC S9(8)V9(4).
          02 DUMMYS-S2-USAAA.
             04 AA-S2-RECMORA001VSA-DCCED01 PIC 9(1).    
             04 AA-S2-RECMORA001VSA-DCCED02 PIC 9(1).    
             04 AA-S2-RECMORA001VSA-DCCED03 PIC 9(1).    
             04 AA-S2-MMAXHIS-DCCED01       PIC 9(1).    
             04 AA-S2-MMAXHIS-DCCED02       PIC 9(1).    
             04 AA-S2-MMAXHIS-DCCED03       PIC 9(1).    
             04 AA-S2-CO01END027CC-DCCED01  PIC 9(1).    
             04 AA-S2-CO01END027CC-DCCED02  PIC 9(1).    
             04 AA-S2-CO01END027RO-DCCED01  PIC 9(1).    
             04 AA-S2-CO01END027RO-DCCED02  PIC 9(1).    
             04 AA-S2-CO01MOR033RO-DCCED00  PIC 9(1).    
             04 AA-S2-CO01MOR033RO-DCCED01  PIC 9(1).    
             04 AA-S2-CO01MOR033RO-DCCED02  PIC 9(1).    
             04 AA-S2-CO01MOR033RO-DCCED03  PIC 9(1).    
             04 AA-S2-CO01END030RO-DCCED01  PIC 9(1).    
             04 AA-S2-CO01END030RO-DCCED02  PIC 9(1).    
             04 AA-S2-CO01END030RO-DCCED03  PIC 9(1).    
             04 AA-S2-CO01END030RO-DCCED04  PIC 9(1).    
             04 AA-S2-CO02END015CB-DCCED01  PIC 9(1).    
             04 AA-S2-CO02END015CB-DCCED02  PIC 9(1).    
             04 AA-S2-CO01END092RO-DCCED01  PIC 9(1).    
             04 AA-S2-CO01END092RO-DCCED02  PIC 9(1).    
             04 AA-S2-CO01END092RO-DCCED03  PIC 9(1).    
             04 AA-S2-CO01END092RO-DCCED04  PIC 9(1).    
             04 AA-S2-CO01EXP001AH-DCCED00  PIC 9(1).    
             04 AA-S2-CO01EXP001AH-DCCED01  PIC 9(1).    
             04 AA-S2-CO01EXP001AH-DCCED02  PIC 9(1).    
             04 AA-S2-CO01EXP001AH-DCCED03  PIC 9(1).    
             04 AA-S2-CO01EXP001AH-DCCED04  PIC 9(1).    
             04 AA-S2-CO01MOR009CC-DCCED01  PIC 9(1).    
             04 AA-S2-CO01MOR009CC-DCCED02  PIC 9(1).    
             04 AA-S2-CO01NUM050AH-DCCED00  PIC 9(1).    
             04 AA-S2-CO01NUM050AH-DCCED01  PIC 9(1).    
             04 AA-S2-CO01NUM050AH-DCCED02  PIC 9(1).    
             04 AA-S2-CO01NUM050AH-DCCED03  PIC 9(1).    
             04 AA-S2-MMAX24-DCCED01        PIC 9(1).    
             04 AA-S2-MMAX24-DCCED02        PIC 9(1).    
             04 AA-S2-RECMORA120-DCCED01    PIC 9(1).    
             04 AA-S2-RECMORA120-DCCED02    PIC 9(1).    
             04 AA-S2-CO01END071RO-DCCED01  PIC 9(1).    
             04 AA-S2-CO01END071RO-DCCED02  PIC 9(1).    
             04 AA-S2-CO01END071RO-DCCED03  PIC 9(1).    
             04 AA-S2-CO01END071RO-DCCED04  PIC 9(1).    
       01 VARIABLES-CARD03-USAAA.       
          02 ACOTADAS-S3-USAAA.
             04 AA-S3-EXPONENTE         PIC S9(3)V9(15).     
DTRN01       04 AA-S3-CO02EXP001TO-DTRN     PIC S9(8)V9(4).             RMB0002
DTRN01       04 AA-S3-CO02EXP006TO-DTRN     PIC S9(8)V9(4).             RMB0002
DTRN01       04 AA-S3-CO02EXP008TO-DTRN     PIC S9(8)V9(4).             RMB0002
DTRN01       04 AA-S3-PROQTOTAB-DTRN        PIC S9(8)V9(4).             RMB0002
DTRN01       04 AA-S3-PRQALDIA-DTRN         PIC S9(8)V9(4).             RMB0002
          02 DUMMYS-S3-USAAA.
DTRN01*      04 AA-S3-CO02EXP008CCD01   PIC 9(1).                       RMB0002
DTRN01       04 AA-S3-CO01END030RO-DTRND02  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01END030RO-DTRND03  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01END030RO-DTRND04  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01END089RO-DTRND01  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01END089RO-DTRND04  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01EXP001RO-DTRND01  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01EXP001RO-DTRND04  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01MOR068RO-DTRND02  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01END092RO-DTRND01  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01END092RO-DTRND04  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01MOR074CC-DTRND01  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01MOR047RO-DTRND03  PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-RECMORA001VSA-DTRND01 PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-RECMORA001VSA-DTRND03 PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-MMAXHIS-DTRND02       PIC 9(1).                   RMB0002
DTRN01       04 AA-S3-CO01INQ016-DTRND02    PIC 9(1).                   RMB0002
       01 VARIABLES-CARD04-USAAA.       
          02 ACOTADAS-S4-USAAA.
             04 AA-S4-EXPONENTE         PIC S9(3)V9(15).     
DTRE01       04 AA-S4-CO02EXP006TO-DTRE     PIC S9(8)V9(4).             RMB0002
DTRE01       04 AA-S4-CO01NUM050RO-DTRE     PIC S9(8)V9(4).             RMB0002
DTRE01       04 AA-S4-PRQALDIA-DTRE         PIC S9(8)V9(4).             RMB0002
DTRE01       04 AA-S4-CO02NUM042CB-DTRE     PIC S9(8)V9(4).             RMB0002
DTRE01       04 AA-S4-CO01ACP004RO-DTRE     PIC S9(8)V9(4).             RMB0002
DTRE01       04 AA-S4-PREEXPOSICIONIN-DTRE  PIC S9(8)V9(4).             RMB0002
DTRE01       04 AA-S4-CO02EXP008TO-DTRE     PIC S9(8)V9(4).             RMB0002
          02 DUMMYS-S4-USAAA.
DTRE01*      04 AA-S4-EVER6024D03       PIC 9(1).                       RMB0002
DTRE01       04 AA-S4-CO01END092RO-DTRED03  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END092RO-DTRED04  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END071RO-DTRED01  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END071RO-DTRED02  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END071RO-DTRED04  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END089RO-DTRED01  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END089RO-DTRED02  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END089RO-DTRED04  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END010RO-DTRED03  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01END010RO-DTRED04  PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01INQ017-DTRED00    PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01INQ017-DTRED02    PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-CO01INQ017-DTRED03    PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-RECMORA001VSA-DTRED01 PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-RECMORA001VSA-DTRED02 PIC 9(1).                   RMB0002
DTRE01       04 AA-S4-MMAXHIS-DTRED02       PIC 9(1).                   RMB0002
       01 VARIABLES-CARD05-USAAA.       
          02 ACOTADAS-S5-USAAA.
             04 AA-S5-EXPONENTE         PIC S9(3)V9(15).     
DINT01       04 AA-S5-PRQALDIA-DINT         PIC S9(8)V9(4).             RMB0002
DINT01       04 AA-S5-CO02NUM042CB-DINT     PIC S9(8)V9(4).             RMB0002
DINT01       04 AA-S5-CO02EXP001TO-DINT     PIC S9(8)V9(4).             RMB0002
DINT01       04 AA-S5-CO02NUM005TO-DINT     PIC S9(8)V9(4).             RMB0002
          02 DUMMYS-S5-USAAA.
DINT01*      04 AA-S5-CO00DEM016D01     PIC 9(1).                       RMB0002
DINT01       04 AA-S5-RECMORA001-DINTD01    PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO02END015CB-DINTD02  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-RECMORA030-DINTD01    PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END089RO-DINTD01  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END089RO-DINTD02  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END089RO-DINTD04  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01MOR046RO-DINTD01  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-MMAXHIS-DINTD03       PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END017RO-DINTD02  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END071RO-DINTD01  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END071RO-DINTD02  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END071RO-DINTD04  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01EXP001RO-DINTD01  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END070RO-DINTD01  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END070RO-DINTD03  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01END070RO-DINTD04  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01EXP001AH-DINTD04  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01ACP007CT-DINTD02  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01MOR009CC-DINTD01  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01MOR054IN-DINTD01  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01MOR054IN-DINTD02  PIC 9(1).                   RMB0002
DINT01       04 AA-S5-CO01INQ025-DINTD03    PIC 9(1).                   RMB0002
       01 VARIABLES-CARD06-USAAA.       
          02 ACOTADAS-S6-USAAA.
             04 AA-S6-EXPONENTE         PIC S9(3)V9(15).     
DHVT01       04 AA-S6-CO02NUM042CB-DHVT     PIC S9(8)V9(4).             RMB0002
DHVT01       04 AA-S6-PRQALDIA-DHVT         PIC S9(8)V9(4).             RMB0002
DHVT01       04 AA-S6-CO02EXP009TO-DHVT     PIC S9(8)V9(4).             RMB0002
          02 DUMMYS-S6-USAAA.
DHVT01*      04 AA-S6-CO01MOR061IND01   PIC 9(1).                       RMB0002
DHVT01       04 AA-S6-RECMORA001VSA-DHVTD01 PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01END071RO-DHVTD01  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01END071RO-DHVTD02  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01END071RO-DHVTD04  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01END089RO-DHVTD01  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01END089RO-DHVTD02  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-RECMORA030-DHVTD01    PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-RECMORA030-DHVTD02    PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-MMAXHIS-DHVTD01       PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01END015RO-DHVTD03  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01END015RO-DHVTD04  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01INQ017-DHVTD02    PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01MOR009CC-DHVTD01  PIC 9(1).                   RMB0002
DHVT01       04 AA-S6-CO01MOR067RO-DHVTD01  PIC 9(1).                   RMB0002
       01 VARIABLES-CARD07-USAAA.       
          02 ACOTADAS-S7-USAAA.
             04 AA-S7-EXPONENTE         PIC S9(3)V9(15).     
DCPN01       04 AA-S7-MOB12-DCPN            PIC S9(8)V9(4).             RMB0003
DCPN01       04 AA-S7-CO02EXP006TO-DCPN     PIC S9(8)V9(4).             RMB0003
DCPN01       04 AA-S7-PRQALDIA-DCPN         PIC S9(8)V9(4).             RMB0003
          02 DUMMYS-S7-USAAA.
DCPN01*      04 AA-S7-CO01MOR061IND01   PIC 9(1).                       RMB0003
DCPN01       04 AA-S7-CO00DEM003-DCPND01    PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO00DEM003-DCPND02    PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-RECMORA001VSA-DCPND01 PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-RECMORA001VSA-DCPND02 PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-MMAXHIS-DCPND01       PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO01INQ017-DCPND02    PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO01INQ017-DCPND03    PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO01MOR055CC-DCPND01  PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO01END089RO-DCPND01  PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO01END089RO-DCPND02  PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO01END027CC-DCPND01  PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO02END015CB-DCPND00  PIC 9(1).                   RMB0003
DCPN01       04 AA-S7-CO02END015CB-DCPND02  PIC 9(1).                   RMB0003
       01 VARIABLES-CARD08-USAAA.       
          02 ACOTADAS-S8-USAAA.
             04 AA-S8-EXPONENTE         PIC S9(3)V9(15).     
DCPE01       04 AA-S8-PRQALDIA-DCPE         PIC S9(8)V9(4).             RMB0003
DCPE01       04 AA-S8-CO01MOR040IN-DCPE     PIC S9(8)V9(4).             RMB0003
DCPE01       04 AA-S8-CO02NUM042CB-DCPE     PIC S9(8)V9(4).             RMB0003
DCPE01       04 AA-S8-CO02EXP006TO-DCPE     PIC S9(8)V9(4).             RMB0003
          02 DUMMYS-S8-USAAA.
DCPE01*      04 AA-S8-CO01MOR061IND01   PIC 9(1).                       RMB0003
DCPE01       04 AA-S8-RECMORA001-DCPED01    PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01END017IN-DCPED02  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-MMAX48-DCPED01        PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01END089RO-DCPED01  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01END089RO-DCPED04  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01MOR056RO-DCPED02  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01INQ017-DCPED02    PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01END027CC-DCPED01  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO02NUM030TO-DCPED02  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-EVER03006P-DCPED01    PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01ACP015IN-DCPED03  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01ACP015IN-DCPED04  PIC 9(1).                   RMB0003
DCPE01       04 AA-S8-CO01MOR054IN-DCPED02  PIC 9(1).                   RMB0003
       01 VARIABLES-CARD09-USAAA.       
          02 ACOTADAS-S9-USAAA.
             04 AA-S9-EXPONENTE         PIC S9(3)V9(15).     
DFUT01       04 AA-S9-CO02EXP006TO-DPU      PIC S9(8)V9(4).             RMB0002
DFUT01       04 AA-S9-PRQALDIA-DPU          PIC S9(8)V9(4).             RMB0002
DFUT01       04 AA-S9-MOB60M-DPU            PIC S9(8)V9(4).             RMB0002
DFUT01       04 AA-S9-CO02NUM042CB-DPU      PIC S9(8)V9(4).             RMB0002
DFUT01       04 AA-S9-CO02EXP007TO-DPU      PIC S9(8)V9(4).             RMB0002
DFUT01       04 AA-S9-MOB12-DPU             PIC S9(8)V9(4).             RMB0002
          02 DUMMYS-S9-USAAA.
DFUT01*      04 AA-S9-CO01MOR061IND01       PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO00DEM003-DPUD04     PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01END089RO-DPUD03   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01END089RO-DPUD04   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-RECMORA001VSA-DPUD01  PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01MOR060RO-DPUD01   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01MOR060RO-DPUD02   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-MMAXHIS-DPUD02        PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01NUM050RO-DPUD02   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01NUM050RO-DPUD03   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01INQ017-DPUD01     PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01INQ017-DPUD03     PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01END020RO-DPUD01   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01END020RO-DPUD02   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01END020RO-DPUD04   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01MOR060CC-DPUD01   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01MOR060CC-DPUD02   PIC 9(1).                   RMB0002
DFUT01       04 AA-S9-CO01END017IN-DPUD02   PIC 9(1).                   RMB0002
