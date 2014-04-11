REMARK*CARACTTEM-WS.0007
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PROPIAS PARA BANCO DE CREDITO
REMARK**********************************************************************
REMARK* CAV0002     | RQ5539 -SE INCLUYE CARACTERISTICAS NUM020XX      *
REMARK* 14-ABR-2011 |         PRODUCTOS EN DUDOSO RECAUDO PARA CADA    *
REMARK* C.VELASCO   |         PORTAFOLIO, PARA LOS NIVELES 00, 01 y 02 *
REMARK******************************************************************
REMARK* CAV0003     | RQ7293 -SE INCLUYE CARACTERISTICAS ESTANDAR      *
REMARK* 23-AGO-2012 |         MOR117 A MOR120 PARA PORTAFOLIOS         *
REMARK* C.VELASCO   |         RO,IN,VE,HP,CC,CO,OT NIVELES 00, 01 y 02 *
REMARK******************************************************************
REMARK* IMS0004     | CQ9773 -SE INCLUYE LAS CARACTERISTICAS AÑO, MES, *
REMARK* 11-ENE-2013 |         DIA Y SALARIO MINIMO COMO CARACTERISTICAS*
REMARK* M.SALAMANCA |         ESTANDAR. Y TIPO ID.                     *
REMARK* CAV0004     | CARMOR  SE INCLUYEN LAS CARACTERISTICAS 
REMARK* C.VELASCO   |         01MOR105 A 01MOR108
REMARK******************************************************************
REMARK* IMS0005     | R10865 -SE INCLUYE LA CARACTERISTICA ESTANDARD   *
REMARK* 08-ABR-2013 |         CO01DEM32_000 TIPO DE CONSULTA.          *
REMARK* M.SALAMANCA |                                          CQ10865 *
REMARK******************************************************************
REMARK* IMS0006     | R11684 -SE INCLUYEN 2 CARACTERISTICAS ESTANDAR   *
REMARK* 16-ABR-2013 |         PARA ESTRATEGIA DE BANCOLOMBIA.          *
REMARK* M.SALAMANCA |                                          CQ11684 *
REMARK******************************************************************
REMARK* STT0007     | R11682 -SE INCLUYE LA CARACTERISTICA ESTANDARD   *
REMARK* 11-MAY-2013 |         CO01DEM033    EDAD CIUDADANO.            *
REMARK* L.GOMEZ     |                                          CQ11682 *
REMARK******************************************************************
       01 VARIABLES-CARACTBC.
          02 02MOR001BC           PIC 9(3). *> MORAS  30 ULTIMOS 12 MESE
          02 02MOR002BC           PIC 9(3). *> MORAS  60 ULTIMOS 12 MESE
          02 02MOR003BC           PIC 9(3). *> MORAS  90 ULTIMOS 12 MESE
          02 02MOR004BC           PIC 9(3). *> MORAS 120 ULTIMOS 12 MESE
          02 02MOR005BC           PIC 9(3). *> MORAS  30 FIN ULT 12 MESE
          02 02MOR006BC           PIC 9(3). *> MORAS  30 REAL ULT 12 MESE
          02 02MOR007BC           PIC 9(3). *> MESE MORA 60 MAS RECIENTE 
      ******************************************************************
      *DEFINICION DE CARACTERISTICAS PROPIAS PARA BANCO POPULAR         
      ******************************************************************
       01 NOM-POR-CARACT PIC X(18) VALUE "ROINVEHPOTCCCOAHCT".          
       01 FILLER REDEFINES NOM-POR-CARACT.                              
          02 V-POR-CARACT   OCCURS 9 TIMES PIC X(02).                   
       01 VARIABLES-AUXCARACTBP.                                        
          02 SW-02MOR            PIC 9(1).                              
          02 DEBUGC              PIC 9(1) .                             
R10865    02 CARACT-NO-REGI      PIC 9(1).                              IMS0005
R10865    02 CARACT-NO-DIAN      PIC 9(1).                              IMS0005
R10865    02 CARACT-NO-DAS       PIC 9(1).                              IMS0005
R11682    02 FECHA-EXP-CARACTT          PIC 9(08).                      STT0007
R11682    02 FILLER REDEFINES FECHA-EXP-CARACTT.                        STT0007
R11682       04 ANO-EXP-CARACTT         PIC 9(04).                      STT0007
R11682       04 MES-EXP-CARACTT         PIC 9(02).                      STT0007
R11682       04 DIA-EXP-CARACTT         PIC 9(02).                      STT0007

       01 VARIABLES-CARACTBP.                                           
          02 VAR-CARACTBP.                                              
             03 TABLA-PORTA-0001MOR OCCURS 9 TIMES.                     
               05 01MOR109       PIC 9(4). *> MORAS  30 ULTIMOS 12 MESE 
               05 01MOR110       PIC 9(4). *> MORAS  60 ULTIMOS 12 MESE 
               05 01MOR111       PIC 9(4). *> MORAS  90 ULTIMOS 12 MESE 
               05 01MOR112       PIC 9(4). *> MORAS 120 ULTIMOS 12 MESE 
          02 VAR-CARACTBPX REDEFINES VAR-CARACTBP.                      
      *                                                                 
             05 01MOR109RO       PIC 9(4).                              
             05 01MOR110RO       PIC 9(4).                              
             05 01MOR111RO       PIC 9(4).                              
             05 01MOR112RO       PIC 9(4).                              
      *                                                                 
             05 01MOR109IN       PIC 9(4).                              
             05 01MOR110IN       PIC 9(4).                              
             05 01MOR111IN       PIC 9(4).                              
             05 01MOR112IN       PIC 9(4).                              
      *                                                                 
             05 01MOR109VE       PIC 9(4).                              
             05 01MOR110VE       PIC 9(4).                              
             05 01MOR111VE       PIC 9(4).                              
             05 01MOR112VE       PIC 9(4).                              
      *                                                                 
             05 01MOR109HP       PIC 9(4).                              
             05 01MOR110HP       PIC 9(4).                              
             05 01MOR111HP       PIC 9(4).                              
             05 01MOR112HP       PIC 9(4).                              
      *                                                                 
             05 01MOR109OT       PIC 9(4).                              
             05 01MOR110OT       PIC 9(4).                              
             05 01MOR111OT       PIC 9(4).                              
             05 01MOR112OT       PIC 9(4).                              
      *                                                                 
             05 01MOR109CC       PIC 9(4).                              
             05 01MOR110CC       PIC 9(4).                              
             05 01MOR111CC       PIC 9(4).                              
             05 01MOR112CC       PIC 9(4).                              
      *                                                                 
             05 01MOR109CO       PIC 9(4).                              
             05 01MOR110CO       PIC 9(4).                              
             05 01MOR111CO       PIC 9(4).                              
             05 01MOR112CO       PIC 9(4).                              
      *                                                                 
             05 01MOR109AH       PIC 9(4).                              
             05 01MOR110AH       PIC 9(4).                              
             05 01MOR111AH       PIC 9(4).                              
             05 01MOR112AH       PIC 9(4).                              
      *                                                                 
             05 01MOR109CT       PIC 9(4).                              
             05 01MOR110CT       PIC 9(4).                              
             05 01MOR111CT       PIC 9(4).                              
             05 01MOR112CT       PIC 9(4).                              
          02 VAR-00-02MORXXX.                                           
             05 00MOR109         PIC 9(3). *> MORAS  30 ULTIMOS 12 MESE 
             05 00MOR110         PIC 9(3). *> MORAS  60 ULTIMOS 12 MESE 
             05 00MOR111         PIC 9(3). *> MORAS  90 ULTIMOS 12 MESE 
             05 00MOR112         PIC 9(3). *> MORAS 120 ULTIMOS 12 MESE 
             05 02MOR002TO       PIC 9(3). *> PRODUCTOS EN MORA  30     
             05 02MOR003TO       PIC 9(3). *> PRODUCTSO EN MORA  60     
             05 02MOR004TO       PIC 9(3). *> PRODUCTSO EN MORA  90     
             05 02MOR005TO       PIC 9(3). *> PRODUCTSO EN MORA  120    
             05 02MOR006TO       PIC 9(3). *> PRODUCTOS CASITIGADOS     
             05 02MOR007TO       PIC 9(3). *> PRODUCTOS RECUPERADOS     
             05 02MOR109TO       PIC 9(3). *> MORAS  30 DIAS ULT 12 MESE
             05 02MOR110TO       PIC 9(3). *> MORAS  60 DIAS ULT 12 MESE
             05 02MOR111TO       PIC 9(3). *> MORAS  90 DIAS ULT 12 MESE
             05 02MOR112TO       PIC 9(3). *> MORAS 120 DIAS ULT 12 MESE
RQ7293       05 00MOR117         PIC 9(4). *> MORAS  30 ULTIMOS 24 MESE CAV0003
RQ7293       05 00MOR118         PIC 9(4). *> MORAS  60 ULTIMOS 24 MESE CAV0003
RQ7293       05 00MOR119         PIC 9(4). *> MORAS  90 ULTIMOS 24 MESE CAV0003
RQ7293       05 00MOR120         PIC 9(4). *> MORAS 120 ULTIMOS 24 MESE CAV0003
RQ7293       05 02MOR117TO       PIC 9(4). *> MORAS  30 ULTIMOS 24 MESE CAV0003
RQ7293       05 02MOR118TO       PIC 9(4). *> MORAS  60 ULTIMOS 24 MESE CAV0003
RQ7293       05 02MOR119TO       PIC 9(4). *> MORAS  90 ULTIMOS 24 MESE CAV0003
RQ7293       05 02MOR120TO       PIC 9(4). *> MORAS 120 ULTIMOS 24 MESE CAV0003
CARMOR       03 00MOR105         PIC 9(4). *> MORAS  30 ULTIMOS 6 MESE  CAV0004
CARMOR       03 00MOR106         PIC 9(4). *> MORAS  60 ULTIMOS 6 MESE  CAV0004
CARMOR       03 00MOR107         PIC 9(4). *> MORAS  90 ULTIMOS 6 MESE  CAV0004
CARMOR       03 00MOR108         PIC 9(4). *> MORAS 120 ULTIMOS 6 MESE  CAV0004
CARMOR       03 02MOR105TO       PIC 9(4). *> MORAS  30 ULTIMOS 6 MESE  CAV0004
CARMOR       03 02MOR106TO       PIC 9(4). *> MORAS  60 ULTIMOS 6 MESE  CAV0004
CARMOR       03 02MOR107TO       PIC 9(4). *> MORAS  90 ULTIMOS 6 MESE  CAV0004
CARMOR       03 02MOR108TO       PIC 9(4). *> MORAS 120 ULTIMOS 6 MESE  CAV0004
R11684       03 00MOR140TO       PIC 9(4).                              IMS0006
R11684       03 02MOR140TO       PIC 9(4).                              IMS0006

CQ9773     02 EST-09-NIVEL-00-DEM.                                      IMS0004
CQ9773        03 00DEM03100             PIC 9999.                       IMS0004
CQ9773        03 00DEM03200             PIC 99.                         IMS0004
CQ9773        03 00DEM03300             PIC 99.                         IMS0004
CQ9773        03 00DEM03400             PIC 99999.999.                  IMS0004
CQ9773        03 00DEM99900             PIC 9.                          IMS0004
R10865        03 01DEM032TO             PIC 99.                         IMS0005
R11682        03 01DEM033               PIC 999.                        STT0007


          02 IND-CARACTBP.                                              
             05 02MORI002TO             PIC 9(1).                       
             05 02MORI003TO             PIC 9(1).                       
             05 02MORI004TO             PIC 9(1).                       
             05 02MORI005TO             PIC 9(1).                       
             05 02MORI006TO             PIC 9(1).                       
             05 02MORI007TO             PIC 9(1).                       
             05 02MORI109TO             PIC 9(1).                       
             05 02MORI110TO             PIC 9(1).                       
             05 02MORI111TO             PIC 9(1).                       
             05 02MORI112TO             PIC 9(1).                       
RQ5539    02 VAR-CARACTTEM-00NUM020.                                    CAV0002
RQ5539       05 00NUM020RO              PIC 9(04).                      CAV0002
RQ5539       05 00NUM020IN              PIC 9(04).                      CAV0002
RQ5539       05 00NUM020VE              PIC 9(04).                      CAV0002
RQ5539       05 00NUM020HP              PIC 9(04).                      CAV0002
RQ5539       05 00NUM020CC              PIC 9(04).                      CAV0002
RQ5539       05 00NUM020OT              PIC 9(04).                      CAV0002
RQ5539       05 00NUM020CO              PIC 9(04).                      CAV0002
RQ5539    02 VAR-CARACTTEM-01NUM020.                                    CAV0002
RQ5539       05 01NUM020RO              PIC 9(04).                      CAV0002
RQ5539       05 01NUM020IN              PIC 9(04).                      CAV0002
RQ5539       05 01NUM020VE              PIC 9(04).                      CAV0002
RQ5539       05 01NUM020HP              PIC 9(04).                      CAV0002
RQ5539       05 01NUM020CC              PIC 9(04).                      CAV0002
RQ5539       05 01NUM020OT              PIC 9(04).                      CAV0002
RQ5539       05 01NUM020CO              PIC 9(04).                      CAV0002
RQ5539****** la caracteristica 02NUM020TO esta definida en CARACT.WS    CAV0002
RQ5539****** 05 02NUM020TO              PIC 9(04).                      CAV0002

R11684    02 VAR-CARACTTEM-00EXP020.                                    IMS0006
R11684       05 00EXP014TO              PIC 9(04).                      IMS0006
R11684    02 VAR-CARACTTEM-01EXP020.                                    IMS0006
R11684       05 02EXP014TO              PIC 9(04).                      IMS0006

RQ7293 01 VARIABLES-CARACTTEM-117.                                      CAV0003
RQ7293    02 VAR-CARACTTEM-117.                                         CAV0003
RQ7293       03 TABLA-PORTA-01MOR-117 OCCURS 9 TIMES.                   CAV0003
RQ7293         05 01MOR117       PIC 9(4). *> MORAS  30 ULTIMOS 24 MESE CAV0003
RQ7293         05 01MOR118       PIC 9(4). *> MORAS  60 ULTIMOS 24 MESE CAV0003
RQ7293         05 01MOR119       PIC 9(4). *> MORAS  90 ULTIMOS 24 MESE CAV0003
RQ7293         05 01MOR120       PIC 9(4). *> MORAS 120 ULTIMOS 24 MESE CAV0003
RQ7293    02 VAR-CARACTTEM-117R REDEFINES VAR-CARACTTEM-117.            CAV0003
      *
RQ7293       05 01MOR117RO       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118RO       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119RO       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120RO       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117IN       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118IN       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119IN       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120IN       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117VE       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118VE       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119VE       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120VE       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117HP       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118HP       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119HP       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120HP       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117OT       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118OT       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119OT       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120OT       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117CC       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118CC       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119CC       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120CC       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117CO       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118CO       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119CO       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120CO       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117AH       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118AH       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119AH       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120AH       PIC 9(4).                              CAV0003
      *
RQ7293       05 01MOR117CT       PIC 9(4).                              CAV0003
RQ7293       05 01MOR118CT       PIC 9(4).                              CAV0003
RQ7293       05 01MOR119CT       PIC 9(4).                              CAV0003
RQ7293       05 01MOR120CT       PIC 9(4).                              CAV0003

CARMOR 01 VARIABLES-CARACTTEM-105.                                      CAV0004
CARMOR    02 VAR-CARACTTEM-105.                                         CAV0004
CARMOR       03 TABLA-PORTA-01MOR-105 OCCURS 9 TIMES.                   CAV0004
CARMOR         05 01MOR105       PIC 9(4). *> MORAS  30 ULTIMOS 6 MESE  CAV0004
CARMOR         05 01MOR106       PIC 9(4). *> MORAS  60 ULTIMOS 6 MESE  CAV0004
CARMOR         05 01MOR107       PIC 9(4). *> MORAS  90 ULTIMOS 6 MESE  CAV0004
CARMOR         05 01MOR108       PIC 9(4). *> MORAS 120 ULTIMOS 6 MESE  CAV0004
CARMOR    02 VAR-CARACTTEM-105R REDEFINES VAR-CARACTTEM-105.            CAV0004
      *
CARMOR       05 01MOR105RO       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106RO       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107RO       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108RO       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105IN       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106IN       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107IN       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108IN       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105VE       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106VE       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107VE       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108VE       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105HP       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106HP       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107HP       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108HP       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105OT       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106OT       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107OT       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108OT       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105CC       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106CC       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107CC       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108CC       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105CO       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106CO       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107CO       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108CO       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105AH       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106AH       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107AH       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108AH       PIC 9(4).                              CAV0004
      *
CARMOR       05 01MOR105CT       PIC 9(4).                              CAV0004
CARMOR       05 01MOR106CT       PIC 9(4).                              CAV0004
CARMOR       05 01MOR107CT       PIC 9(4).                              CAV0004
CARMOR       05 01MOR108CT       PIC 9(4).                              CAV0004


R11684     02 TIP-CTA-SEC              PIC 99 VALUE ZEROS.              IMS0006
R11684        88 SEC-FINC            VALUE 02 03 05 07 13 14 17 18 20   IMS0006
R11684                                     48 49.                       IMS0006
R11684        88 SEC-REAL            VALUE 04 08 09 10 11 12 15 16 19   IMS0006
R11684                                     21 22 28 30 31 32 33 34 35   IMS0006
R11684                                     36 37 38 39 40 41 42 43 44   IMS0006
R11684                                     45 46 47.                    IMS0006
R11684        88 SEC-SERV            VALUE 23 24 29.                    IMS0006
R11684        88 SEC-COOP            VALUE 06 27.                       IMS0006
R11684     02 P-VECNOR                 PIC 99 VALUE ZEROS.              IMS0006
