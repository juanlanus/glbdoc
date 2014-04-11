REMARK*  CARDEC-WS.V.0016
REMARK******************************************************************
REMARK* RUTINA QUE CONTINENE LAS VARIABLES UTILIZADAS EN CARDEC.PROC.  *
REMARK* CORRESPONDEN A LAS CARACTERISTICAS DE LOS PARAMETRIZADORES DC  *
REMARK******************************************************************
REMARK*                     LOG DE MODIFICACIONES                      *
REMARK*----------------------------------------------------------------*
REMARK*  ANALISTA                        DESCRIPCION                   *
REMARK* ----------- * ------------------------------------------------ *
REMARK* EHA0002     | ADDCAR - SE ADICIONAN NUEVAS CARACTERISTICAS.    *
REMARK* E. ALVAREZ  |                                                  *
REMARK* 22-OCT-2010 |                                                  *
REMARK* ----------- * ------------------------------------------------ *
REMARK* EHA0003     | MD1033 - SE ADICIONAN NUEVAS CARACTERISTICAS     *
REMARK* E. ALVAREZ  |          PARA SCORE DE CAJASAN                   *
REMARK* 29-DIC-2010 |                                                  *
REMARK******************************************************************
REMARK* CIGD004     | CQ1224 - SE ADICIONAN NUEVAS CARACTERISTICAS     *
REMARK* C.GAONA     |          BANCOLOMBIA (CONSERVA CLIENTES)         *
REMARK* 08-FEB-2010 |                                                  *
REMARK******************************************************************
REMARK* EHA0005     | MOD734 - SE ADICIONAN NUEVAS CARACTERISTICAS     *
REMARK* E.ALVAREZ   |          PARA ESTRAVAL (ICGD54D)                 *
REMARK* 21-FEB-2010 |                                                  *
REMARK******************************************************************
      * EHA0006     | CQ1343 - SCORE COLPATRIA VEHICULOS E HIPOTECARIO.
      * E.ALVAREZ.  |   RQ. CQ. 1343.                                  *
      * 25-FEB-2010 |                                                  *
      ******************************************************************
      * DLM0007     | CQ1342 - SCORE COLPATRIA HIPOTECARIO.            *
      * DARIO LASSO.|   RQ. CQ. 1342.                                  *
      * 07-MAR-2011 |                                                  *
      ******************************************************************
      * EHA0008     | CQ1445 - MODIFICACIONES PARA SCORE TUYA ICGD64D  *
      * E. ALVAREZ  |   RQ. CQ. 1445.                                  *
      * 10-MAR-2011 |                                                  *
      ******************************************************************
      * EHA0009     | AL1342 - MODIFICACIONES SCORES COLPATRIA         *
      * E. ALVAREZ  |          VEHICULOS - HIPOTECARIO RQ. CQ. 1342.   *
      * 18-MAR-2011 |                                                  *
      ******************************************************************
      * EHA0010     | AL1224 - SE ADICIONAN NUEVAS CARACTERISTICAS     *
      * E. ALVAREZ  |           BANCOLOMBIA ICGD59.                    *
      * 22-MAR-2011 |                                                  *
      ******************************************************************
      * CIG0011     | RQ1648 - SCORE COLPATRIA CONSUMO                 *
      * C. GAONA    |   RQ. CQ 1648                                    *
      * 02-ABR-2011 |                                                  *
      ******************************************************************    
      * DLM0012     | CQ1812 - SCORE TELECOM.                          *
      * DARIO LASSO |   RQ. CQ 1812                                    *
      * 20-MAY-2011 |                                                  *
      ******************************************************************
      * CIG0013     | RQ1914 - IMPLEMENTACION DEL SCORE PARA BANCOLOM- *
      * C. GAONA    |       BIA ICGS57D.                               *
      * 07-JUN-2011 |                                                  *
      ******************************************************************
      * EHA0014     | RQ2171 - SE ADICIONA NUEVA CARACTERISTICA PARA   *
      * E. ALVAREZ  |       ESTRAVAL ICGD54D.                          *
      * 16-JUN-2011 |                                                  *
      ******************************************************************
      * CIG0015     | RQ2212 - SE ADICIONAN NUEVAS CARACTERISTICAS PARA*
      * C. GAONA    |          BANCO POPULAR.                          *
      * 24-JUN-2011 | RQ1947 - SE ADICIONAN NUEVAS CARACTERISTICAS PARA*
      *             |          FUNDACION MULDIAL DE LA MUJER           *
      ******************************************************************
      * EHA0016     | CQ2393 - SE ADICIONA NUEVA VARIABLE PARA EL SCORE*
      * E. ALVAREZ  |       ICGD61D.                                   *
      * 13-JUL-2011 |                                                  *
      ******************************************************************
      
       01 VARIABLES-CARDEC.  
CQ1812   03  WOR-VAR-CARDEC            PIC  9(010)  VALUE  ZEROS.       DLM0012
CQ1812   03  LNG-VAR-CARDEC            PIC  9(003)  VALUE  ZEROS.       DLM0012
CQ1812   03  TOT-VAR-CARACTERIST       PIC  9(003)  VALUE  ZEROS.       DLM0012
CQ1812   03  V-EST-CARACT-CARDEC-X     PIC  X(10000)  VALUE  SPACES.    DLM0012
CQ1812   03                      REDEFINES  V-EST-CARACT-CARDEC-X.      DLM0012
CQ1812     05  V-EST-CARACT-CARDEC  OCCURS  500  TIMES  PIC  X(20).     DLM0012
CQ1812   03  SW-UNA-VEZ-MAESTRO        PIC  9(001)  VALUE  ZEROS.       DLM0012
          03 TIPIDE-CARDEC              PIC 9(001) VALUE 0.        
          03 NUMIDE-CARDEC              PIC 9(011) VALUE 0.        
          03 FECHA-RUN-TIME-CARDEC      PIC 9(008) VALUE 0.        
          03 FINCRE-CARDEC              PIC 9(001) VALUE 0.        
          03 FINCHK-CARDEC              PIC 9(001) VALUE 0.        
          03 FINEND-CARDEC              PIC 9(001) VALUE 0.        
          03 FINCON-CARDEC              PIC 9(001) VALUE 0.        
          03 VALIDA-CARDEC              PIC 9(001) VALUE 0.        
          03 AMNISTIA21-LEEMSTRS-CARDEC PIC 9(001) VALUE 0.        
          03 SW-ART13-AMNIST21-CARDEC   PIC 9(001) VALUE 0.        
          03 SW-NOV-48-CARDEC           PIC 9(001) VALUE 0.        
          03 I-CARDEC                   PIC 9(005) VALUE 0.        
          03 NOM-VAR-CARDEC             PIC X(025) VALUE SPACES.        
          03 VAL-VAR-CARDEC             PIC X(25).        
          03 FILLER REDEFINES VAL-VAR-CARDEC.        
             04 VAL3-VAR-CARDEC        PIC 9(003).          
             04 FILLER                  PIC X(22).        
          03 FILLER REDEFINES VAL-VAR-CARDEC.        
             04 VAL4-VAR-CARDEC        PIC B9(004).          
             04 FILLER                  PIC X(20).        
          03 FILLER REDEFINES VAL-VAR-CARDEC.        
             04 VAL5-VAR-CARDEC         PIC 9(005).          
             04 FILLER                  PIC X(20).        
          03 FILLER REDEFINES VAL-VAR-CARDEC.        
             04 VAL10-VAR-CARDEC        PIC 9(010).          
             04 FILLER                  PIC X(15).  
          03 FILLER REDEFINES VAL-VAR-CARDEC.        
             04 VAL13-VAR-CARDEC        PIC 9(013).          
             04 FILLER                  PIC X(12). 
AL1342    03 FILLER REDEFINES VAL-VAR-CARDEC.                           EHA0009
AL1342       04 VAL13DEC-VAR-CARDEC     PIC 9(009).999.                 EHA0009
AL1342       04 FILLER                  PIC X(12).                      EHA0009
          03 FILLER REDEFINES VAL-VAR-CARDEC.        
             04 STR01-VAR-CARDEC        PIC X(001).          
             04 FILLER                  PIC X(24).        
          03 FILLER REDEFINES VAL-VAR-CARDEC.        
             04 STR06-VAR-CARDEC        PIC X(006).          
             04 FILLER                  PIC X(19).
          03 TOPE-CONS-CARDEC           PIC  9(03) VALUE ZEROS.        
          03 TOPE-V-CARDEC              PIC  S9999 VALUE ZEROS.        
          03 X-CARDEC                   PIC  9(02)  VALUE  0.        
          03 L-CARDEC                   PIC  9(02)  VALUE  0.        
          03 TOPE-NIT-CARDEC            PIC  9(03) VALUE ZEROS.        
          03 IND-NO-BBVA-CARDEC         PIC  9(01) VALUE ZEROS.        
      *        
       01  TABLA-CONSULTAS-CARDEC.        
           03 W-NIT-SUS-CONS-CARDEC   OCCURS  50  TIMES  PIC 9(11).        
       01  TABLA-NIT-FINANCIERO-CARDEC.        
           03 W-NIT-SEC-FINA-CARDEC   OCCURS  50  TIMES  PIC 9(11).        
       01  TABLA-HISTORY-MORAS-CARDEC.        
           03 W-MORAS-HISTORY-V-CARDEC  PIC X(12) VALUE ZEROS.        
           03 W-MORAS-HISTORY-V-R-CARDEC  REDEFINES        
                                         W-MORAS-HISTORY-V-CARDEC.        
              05 W-MORAS-HISTORY-CARDEC  OCCURS 12 TIMES PIC X(01).        
      *
       01 CARACTERISTICAS-CARDEC.        
      *-------------------------- ESPECIALES --------------------------*        
      *    03 99ESP001EX36               PIC 9(03).        
      *-------------------------- CONSULTAS ---------------------------*        
          03 99INQ001EX36               PIC 9(03).
ADDCAR    03 99INQ002TO00               PIC 9(03).                      EHA0002
MOD734    03 99INQ002TO01               PIC 9(03).                      EHA0005
MOD734    03 99INQ002TO02               PIC 9(03).                      EHA0005
MOD734    03 99INQ002TO03               PIC 9(03).                      EHA0005
MOD734    03 99INQ002TO04               PIC 9(03).                      EHA0005
MD1033    03 99INQ003EX58               PIC 9(03).                      EHA0003
MD1033    03 99INQ004EX58               PIC 9(03).                      EHA0003
RQ1947    03 99INQ005TO00               PIC 9(05).                      CIG0015

      *-------------------------- DEMOGRAFICAS ------------------------*        
          03 99DEM001TO00               PIC 9(02).        
          03 99DEM002TO00               PIC 9(03).        
CQ1343    03 99DEM003TO00               PIC 9(03).                      EHA0006
RQ1947    03 99DEM003TO02               PIC 9(05).                      CIG0015
RQ1947    03 99DEM003TO05               PIC 9(05).                      CIG0015
CQ1343    03 99DEM003TO12               PIC 9(03).                      EHA0006
          03 99DEM004TO00               PIC 9(03).
MD1033    03 99DEM005TO00               PIC 9(03).                      EHA0003
          03 99DEM006TO00               PIC 9(08).
CQ1343    03 99DEM007TO12               PIC 9(06).                      EHA0006
CQ1343    03 99DEM008TO00               PIC 9(01).                      EHA0006
CQ1342    03 99DEM009TO00               PIC 9(10).                      DLM0007
      *-------------------------- ENDEUDAMIENTO -----------------------*        
          03 99END001TO00               PIC 9(07).        
          03 99END002TO00               PIC X(01).                  
          03 99END003TO00               PIC 9(10).        
          03 99END004TO00               PIC 9(10).
CQ1224    03 99END004HP00               PIC 9(13).                      CIGD004
CQ1224    03 99END004CO00               PIC 9(13).                      CIGD004
CQ1224    03 99END004TO09               PIC 9(13).                      CIGD004
CQ1224    03 99END004TO10               PIC 9(13).                      CIGD004
CQ1224    03 99END004TO11               PIC 9(13).                      CIGD004
CQ1343    03 99END004TO13               PIC 9(13).                      EHA0006
CQ1224    03 99END004CO07               PIC 9(13).                      CIGD004
          03 99END005TO00               PIC 9(10).
CQ1224    03 99END005TC00               PIC 9(13).                      CIGD004
MD1033    03 99END006EX00               PIC 9(01).                      EHA0003
MD1033    03 99END007TC00               PIC 9(03).                      EHA0003
CQ1224    03 99END010EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END011EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END012EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END013EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END014EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END015EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END016EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END017EX59               PIC 9(13).                      CIGD004
CQ1224    03 99END018EX59               PIC 9(13).                      CIGD004
AL1224    03 99END019TO01               PIC 9(13).                      EHA0010
CQ1343    03 99END019TO13               PIC 9(13).                      EHA0006
MD1033    03 99END019TO04               PIC 9(13).                      EHA0003
CQ1343    03 99END020EX61               PIC 9(04)V999.                  EHA0006
      *-------------------------- HIBRIDAS ----------------------------*        
          03 99HIB001EX00               PIC 9(03).        
          03 99HIB002EX00               PIC 9(03).        
          03 99HIB003EX00               PIC 9(03).          
CQ1224    03 99HIB004EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB005EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB006EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB007EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB008EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB009EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB010EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB011EX59               PIC 9(13).                      CIGD004
CQ1224    03 99HIB012EX59               PIC 9(13).                      CIGD004
      *-------------------------- MOROSIDAD ---------------------------*        
CQ1343    03 99MOR001CT00               PIC 9(06).                      EHA0006
CQ1343    03 99MOR001TO00               PIC 9(06).                      EHA0006
CQ1343    03 99MOR002TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR002CT00               PIC 9(05).                      CIG0015
RQ2212    03 99MOR002AH00               PIC 9(05).                      CIG0015
CQ1343    03 99MOR003TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR003TO01               PIC 9(05).                      CIG0015
CQ1343    03 99MOR003TO02               PIC 9(06).                      EHA0006
RQ2212    03 99MOR003TO03               PIC 9(05).                      CIG0015
CQ1343    03 99MOR003TO05               PIC 9(06).                      EHA0006
RQ1947    03 99MOR003CO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR004TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR004TO01               PIC 9(05).                      CIG0015
CQ1343    03 99MOR004TO02               PIC 9(06).                      EHA0006
RQ2212    03 99MOR004TO03               PIC 9(05).                      CIG0015
CQ1343    03 99MOR004TO05               PIC 9(06).                      EHA0006
RQ1947    03 99MOR004CO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR005TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR005TO01               PIC 9(05).                      CIG0015
CQ1343    03 99MOR005TO02               PIC 9(06).                      EHA0006
RQ2212    03 99MOR005TO03               PIC 9(05).                      CIG0015
CQ1343    03 99MOR005TO05               PIC 9(06).                      EHA0006
RQ1947    03 99MOR005CO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR006TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR006TO01               PIC 9(05).                      CIG0015
CQ1343    03 99MOR006TO02               PIC 9(06).                      EHA0006
RQ2212    03 99MOR006TO03               PIC 9(05).                      CIG0015
CQ1343    03 99MOR006TO05               PIC 9(06).                      EHA0006
RQ1947    03 99MOR006CO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR007TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR007TO01               PIC 9(05).                      CIG0015
CQ1343    03 99MOR007TO02               PIC 9(06).                      EHA0006
RQ2212    03 99MOR007TO03               PIC 9(05).                      CIG0015
CQ1343    03 99MOR007TO05               PIC 9(06).                      EHA0006
CQ1343    03 99MOR007TO12               PIC 9(06).                      EHA0006
CQ1343    03 99MOR008TO12               PIC 9(06).                      EHA0006
CQ1343    03 99MOR008TO00               PIC 9(06).                      EHA0006
CQ1343    03 99MOR008TO02               PIC 9(06).                      EHA0006
CQ1343    03 99MOR008TO05               PIC 9(06).                      EHA0006
CQ1343    03 99MOR009TO00               PIC 9(06).                      EHA0006
RQ1947    03 99MOR009TO02               PIC 9(05).                      CIG0015
RQ1947    03 99MOR009TO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR009TO12               PIC 9(06).                      EHA0006
CQ1343    03 99MOR010TC00               PIC 9(06).                      EHA0006
RQ1947    03 99MOR010TC05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR011TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR011TO01               PIC 9(05).                      CIG0015
RQ2212    03 99MOR011TC00               PIC 9(05).                      CIG0015
CQ1343    03 99MOR011TO12               PIC 9(06).                      EHA0006
CQ1343    03 99MOR012TO00               PIC 9(06).                      EHA0006
CQ1343    03 99MOR013TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR013TO01               PIC 9(05).                      CIG0015
RQ1947    03 99MOR013TO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR013TO12               PIC 9(06).                      EHA0006
RQ2212    03 99MOR013TC00               PIC 9(05).                      CIG0015
CQ1343    03 99MOR014TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR014TO01               PIC 9(05).                      CIG0015
RQ1947    03 99MOR014TO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR014TO12               PIC 9(06).                      EHA0006
RQ2212    03 99MOR014TC00               PIC 9(05).                      CIG0015
CQ1343    03 99MOR015TO00               PIC 9(06).                      EHA0006
RQ2212    03 99MOR015TO01               PIC 9(05).                      CIG0015
RQ1947    03 99MOR015TO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR015TO12               PIC 9(06).                      EHA0006
RQ2212    03 99MOR015TC00               PIC 9(05).                      CIG0015
CQ1343    03 99MOR016TO00               PIC 9(06).                      EHA0006
CQ1343    03 99MOR016TO12               PIC 9(06).                      EHA0006
CQ1343    03 99MOR017TO00               PIC 9(06).                      EHA0006
CQ1343    03 99MOR017TO02               PIC 9(06).                      EHA0006
CQ1343    03 99MOR017TO05               PIC 9(06).                      EHA0006
MD1033    03 99MOR018EX58               PIC 9(01).                      EHA0003
MD1033    03 99MOR019EX58               PIC 9(01).                      EHA0003
MD1033    03 99MOR020EX58               PIC 9(01).                      EHA0003
MD1033    03 99MOR020TO04               PIC 9(01).                      EHA0003
CQ1343    03 99MOR021TO00               PIC 9(06).                      EHA0006
CQ1343    03 99MOR022TO00               PIC 9(06).                      EHA0006
RQ1947    03 99MOR022TO05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR023TC00               PIC 9(06).                      EHA0006
RQ1947    03 99MOR023TC05               PIC 9(05).                      CIG0015
CQ1343    03 99MOR024EX58               PIC 9(01).                      EHA0006
CQ1224    03 99MOR025EX59               PIC 9(01).                      CIGD004
CQ1343    03 99MOR026TO00               PIC 9(06).                      EHA0006
RQ2171    03 99MOR026TO05               PIC 9(06).                      EHA0014
CQ1343    03 99MOR027EX54               PIC 9(06).                      EHA0006
CQ1343    03 99MOR028EX61               PIC 9(06).                      EHA0006
RQ1914    03 99MOR029TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR029CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR030TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR030CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR031TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR031CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR032TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR032CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR033TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR033CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR034TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR034CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR035TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR035CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR037TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR037CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR038TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR038CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR039TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR039CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR040TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR040CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR041TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR041CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR042TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR042CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR043TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR043CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR044TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR044CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR045TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR045CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR046TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR046CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR047TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR047CO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR048TO00               PIC 9(05).                      CIG0013
RQ1914    03 99MOR048CO00               PIC 9(05).                      CIG0013
RQ1947    03 99MOR049TO02               PIC 9(05).                      CIG0015
RQ1947    03 99MOR049TO05               PIC 9(05).                      CIG0015
RQ1947    03 99MOR050TO02               PIC 9(05).                      CIG0015
RQ1947    03 99MOR050TO05               PIC 9(05).                      CIG0015
RQ1947    03 99MOR051TO02               PIC 9(05).                      CIG0015
RQ1947    03 99MOR052TO02               PIC 9(05).                      CIG0015
RQ1947    03 99MOR053TO02               PIC 9(05).                      CIG0015
RQ1947    03 99MOR054TO02               PIC 9(05).                      CIG0015

      *-------------------------- NUM. PRODUCTOS ----------------------*        
CQ1343    03 99NUM001EX36               PIC 9(06).                      EHA0006
CQ1343    03 99NUM002TO00               PIC 9(06).                      EHA0006
CQ1343    03 99NUM002TO02               PIC 9(06).                      EHA0006
CQ1343    03 99NUM002TO05               PIC 9(06).                      EHA0006
RQ1947    03 99NUM003TO02               PIC 9(05).                      CIG0015
CQ1343    03 99NUM003EX58               PIC 9(06).                      EHA0006
CQ1343    03 99NUM004EX58               PIC 9(06).                      EHA0006
RQ1648    03 99NUM005CT00               PIC 9(06).                      CIG0011
RQ1648    03 99NUM006HP00               PIC 9(06).                      CIG0011
RQ1648    03 99NUM007EX65               PIC 9(06).                      CIG0011
MD1033*-------------------------- EXPERIENCIA--------------------------*EHA0003
MD1033    03 99EXP001EX58               PIC 9(05).                      EHA0003
CQ1343    03 99EXP002EX61               PIC 9(05).                      EHA0006
RQ1648    03 99NUM002TO14               PIC 9(05).                      CIG0011
CQ1343    03 99EXP003TC00               PIC 9(05).                      EHA0006
CQ1343    03 99EXP003TO07               PIC 9(05).                      EHA0006
CQ1342    03 99EXP003TO14               PIC 9(05).                      DLM0007
RQ1648    03 99EXP001CT00               PIC 9(05).                      CIG0011
RQ1648    03 99EXP003TO13               PIC 9(05).                      CIG0011
RQ1648    03 99EXP004HP00               PIC 9(05).                      CIG0011
      *-------------------------- SCORES ------------------------------*
AL1224 01 W-CARACTERISTICAS-SCORES.
          03 CSCO0004                   PIC 9(005).        
          03 CSCO0008                   PIC 9(005).        
          03 CSCOE002                   PIC X(001).        
          03 CSCOC002                   PIC S9(005).        
          03 CSCO0S62                   PIC 9(005).        
          03 CSCO0M62                   PIC 9(005).        
AL1224    03 CSCOPI62                   PIC 9(013).
AL1224    03 CSCOPS62                   PIC 9(013).        
AL1224    03 CSCOPM62                   PIC 9(013).        
          03 CSCO0I62                   PIC 9(005).        
          03 CSCO0067                   PIC 9(005).        
CQ1342 01 W-CONSTANTES-TRABAJO-CARDEC.                                  DLM0007
CQ2393    03  W-INGREAL99END020EX61       PIC  9(13)  VALUE  ZEROS.     EHA0016
AL1224    03  KTE-FORMULARIO-CARDEC       PIC X(04) VALUE  SPACES.
CQ1342    03  INGRESO99END020EX61         PIC  9(13)  VALUE  ZEROS.     DLM0007
CQ1342    03  TASA-99END020EX61           PIC  9(03)V9999.              DLM0007
CQ1342    03  ACTIVID99END020EX61         PIC  9(02)  VALUE  ZEROS.     DLM0007
RQ1648    03  INGRESO99END020EX65         PIC  9(13)  VALUE  ZEROS.     CIG0011
RQ1648    03  ACTIVID99END020EX65         PIC  9(02)  VALUE  ZEROS.     CIG0011
RQ1648    03  ACTIIND99END020EX65         PIC  9(02)  VALUE  ZEROS.     CIG0011
CQ1342    03  W-SALARIO-99DEM006TO00      PIC  9(07)  VALUE  ZEROS.     DLM0007
CQ1445    03  W-INGRESOS-SF-99HIB011EX59  PIC  9(13)  VALUE ZEROS.      EHA0008
CQ1445    03  W-INGRESOS-SV-99HIB011EX59  PIC  9(13)  VALUE ZEROS.      EHA0008
CQ1445    03  W-OTROS-ING-99HIB011EX59    PIC  9(13)  VALUE ZEROS.      EHA0008
AL1224    03  W-INGR-SIS-99HIB011EX59     PIC  9(13)  VALUE ZEROS.      EHA0010
CQ1445    03  W-TIP-PROD-99HIB012EX59     PIC  9(1)   VALUE ZEROS.      EHA0008
CQ1342*                                                                 DLM0007
MD1033 01 VARIABLES-TRABAJO-CARDEC.                                     EHA0003
          03  FACTURA-ACIERTAMAS-CARDEC   PIC  9(01)  VALUE 0.
          03  FACTURA-QUANTO-CARDEC       PIC  9(01)  VALUE 0.
AL1224    03  W-INGR-FIN-99HIB011EX59     PIC  9(13)  VALUE  ZEROS.     EHA0010
AL1224    03  DIF-SIS-TOT-99HIB011EX59    PIC  9(04)  VALUE  ZEROS.     EHA0010
CQ1343    03  CUOTAS-99END020EX61         PIC  9(13)  VALUE  ZEROS.     EHA0006
CQ1343    03  INGREAL99END020EX61         PIC  9(13)  VALUE  ZEROS.     EHA0006
RQ1648    03  99END020EX61-INTVALOR       PIC  9(07).                   CIG0011
MD1033    03  T-SLD-MORA-99END06EX00      PIC  9(15)  VALUE  ZEROS.     EHA0003
MD1033    03  T-SLD-DEUDA-99END06EX00     PIC  9(15)  VALUE  ZEROS.     EHA0003
MD1033    03  PORC-40-99END006EX00        PIC  9(11)  VALUE  ZEROS.     EHA0003
MD1033    03  PORC-75-99END007TC00        PIC  9(11)  VALUE  ZEROS.     EHA0003
MD1033    03  T-SLD-MORA-29-99END006EX00  PIC  9(15)  VALUE  ZEROS.     EHA0003
MD1033    03  T-SLD-DEUDA-29-99END006EX00 PIC  9(15)  VALUE  ZEROS.     EHA0003
CQ1343    03  T-VAL-INICIAL-99END007TC00  PIC  9(15)  VALUE  ZEROS.     EHA0006
CQ1343    03  T-SLD-DEUDA-99END007TC00    PIC  9(15)  VALUE  ZEROS.     EHA0006
CQ1343    03  W-OBL-DIA-99MOR024EX58      PIC  9(06)  VALUE  ZEROS.     EHA0006
CQ1343    03  W-NUM-MORAS-99MOR024EX58    PIC  9(06)  VALUE  ZEROS.     EHA0006
CQ1342    03  MORAS30-99EXP002EX61        PIC  9(06)  VALUE  ZEROS.     DLM0007
CQ1342    03  MORAS60-99EXP002EX61        PIC  9(06)  VALUE  ZEROS.     DLM0007
CQ1342    03  MORAS90-99EXP002EX61        PIC  9(06)  VALUE  ZEROS.     DLM0007
CQ1342    03  MORAS120-99EXP002EX61       PIC  9(06)  VALUE  ZEROS.     DLM0007
CQ1343    03  ALTURA-MORA-99MOR024EX58    PIC  9(06)  VALUE  ZEROS.     EHA0006
MD1033    03  W-MESES-REPO-99MOR024EX58   PIC  9(04)  VALUE ZEROS.      EHA0003
CQ1224    03  W-INGR-TOT-99HIB011EX59     PIC  9(13)  VALUE ZEROS.      CIGD004
CQ1224    03  W-INGR-NET-99HIB011EX59     PIC  S9(13)  VALUE ZEROS.     CIGD004
CQ1224    03  W-GASTOS-FAM-99HIB011EX59   PIC  9(13)  VALUE ZEROS.      CIGD004
CQ1224    03  W-LIMS-INGR-99HIB012EX59    PIC  9(13)  VALUE ZEROS.      CIGD004
CQ1224    03  W-CUPO-ESTIM-99HIB012EX59   PIC  9(13)  VALUE ZEROS.      CIGD004
CQ1224    03  W-CESTIM-99HIB012EX59                                     CIGD004
CQ1224                            REDEFINES W-CUPO-ESTIM-99HIB012EX59.  CIGD004
CQ1224      05 W-RESTO-FILLER             PIC  9(8).                    CIGD004
CQ1224      05 W-DECENAS-MILLON           PIC  9(1).                    CIGD004
CQ1224      05 W-UNIDADES-MILLON          PIC  9(1).                    CIGD004
CQ1224      05 W-CENTENAS-MIL             PIC  9(1).                    CIGD004
CQ1224      05 W-DECENAS-MIL              PIC  9(1).                    CIGD004
CQ1224      05 W-UNIDADES-MIL             PIC  9(1).                    CIGD004
CQ1224    03  W-NUM-CHPNOREP-99END013EX59 PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CHPNOREP-99END014EX59 PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CHPNOREP-99END015EX59 PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CHPNOREP-99END016EX59 PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CHPNOREP-99END018EX59 PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-OBLIG-HIP             PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CRE-CON-PR            PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CRE-CONSU-PR          PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CRE-MICRO-PR          PIC  9(04)   VALUE ZEROS.     CIGD004
CQ1224    03  W-NUM-CRE-CONSU-COD         PIC  9(04)   VALUE ZEROS.     CIGD004
MD1033    03 VECTOR-W-99EXP001EX58.                                     EHA0003
MD1033       05 VECTOR-W4-99EXP001EX58    PIC 9(12).                    EHA0003
MD1033       05 VECTOR-W4-R-99EXP001EX58  REDEFINES                     EHA0003
MD1033          VECTOR-W4-99EXP001EX58    OCCURS 12 TIMES PIC 9.        EHA0003
MD1033       05 VECTOR-W3-99EXP001EX58    PIC 9(12).                    EHA0003
MD1033       05 VECTOR-W3-R-99EXP001EX58  REDEFINES                     EHA0003
MD1033          VECTOR-W3-99EXP001EX58    OCCURS 12 TIMES PIC 9.        EHA0003
MD1033       05 VECTOR-W2-99EXP001EX58    PIC 9(12).                    EHA0003
MD1033       05 VECTOR-W2-R-99EXP001EX58  REDEFINES                     EHA0003
MD1033          VECTOR-W2-99EXP001EX58    OCCURS 12 TIMES PIC 9.        EHA0003
MD1033       05 VECTOR-W1-99EXP001EX58    PIC 9(12).                    EHA0003
MD1033       05 VECTOR-W1-R-99EXP001EX58  REDEFINES                     EHA0003
MD1033          VECTOR-W1-99EXP001EX58    OCCURS 12 TIMES PIC 9.        EHA0003
CQ1343    03 REPORTES-EXP-99EXP001EX58    PIC 9(06) VALUE ZEROS.        EHA0006
MD1033*                                                                 EHA0003
       01 VARIABLES-INI-CARDEC.
          03  W-DIF-MESES-CARDEC          PIC  9(08)  VALUE  ZEROS.
          03  WS-NRO-DIAS-CARDEC          PIC  9(08)  VALUE  ZEROS.
          03  W-F-MMAA-CARDEC             PIC  9(04).        
          03  W-FECHA-AUX-CARDEC          PIC  9(06)  VALUE  ZEROS.        
          03                      REDEFINES  W-FECHA-AUX-CARDEC.        
            05  W-MES-AUX-CARDEC          PIC  9(02).        
            05  W-ANO-AUX-CARDEC          PIC  9(04).        
          03  W-MESES-TMP-CARDEC          PIC S9(08).        
          03  W-FECHA-APERTURA-CARDEC     PIC  9(06) VALUE ZEROS.        
          03                      REDEFINES  W-FECHA-APERTURA-CARDEC.        
            05  W-FECHA-APER-AA-CARDEC    PIC  9(04).        
            05  W-FECHA-APER-MM-CARDEC    PIC  9(02).        
          03  W-FECHA-CIERRE-CARDEC       PIC  9(06).        
          03                      REDEFINES  W-FECHA-CIERRE-CARDEC.        
            05  W-FECHA-CIER-AA-CARDEC    PIC  9(04).        
            05  W-FECHA-CIER-MM-CARDEC    PIC  9(02).        
          03  W-FECHA-NOVEDAD-CARDEC      PIC  9(06).        
          03                      REDEFINES  W-FECHA-NOVEDAD-CARDEC.        
            05  W-FECHA-NOVE-AA-CARDEC    PIC  9(04).        
            05  W-FECHA-NOVE-MM-CARDEC    PIC  9(02).        
          03  W-MESES-OBLIGACION-CARDEC   PIC S9(08)  VALUE  ZEROS.        
          03  W-MESES-EXPERIENCIA-CARDEC  PIC S9(08).        
          03  W-MESES-NOVEDAD-CARDEC      PIC S9(08)  VALUE  ZEROS.        
          03  W-MESES-CIERRE-CARDEC       PIC S9(08).        
          03  W-MESES-APERTURA-CARDEC     PIC S9(08)  VALUE  ZEROS.        
          03  W-FECHA-HOY-CARDEC          PIC X(08).        
          03  W-FECHA-HOY-R-CARDEC        REDEFINES W-FECHA-HOY-CARDEC.        
            05  W-ANIO-HOY-CARDEC           PIC 9(04).        
            05  W-MES-HOY-CARDEC            PIC 9(02).        
            05  W-DIA-HOY-CARDEC            PIC 9(02).        
      *        
          03 BOM-CAR-CARDEC             PIC X(5000).        
          03 ERR-EST-CREDIT-CARDEC      PIC 9(001) VALUE 0.        
          03 ERR-EST-CTACOR-CARDEC      PIC 9(001) VALUE 0.        
          03 ERR-EST-ENDEUD-CARDEC      PIC 9(001) VALUE 0.        
          03 ERR-EST-CONSUL-CARDEC      PIC 9(001) VALUE 0.        
          03 ERR-EST-IDENTI-CARDEC      PIC 9(001) VALUE 0.        
          03 ERR-EST-HIBRID-CARDEC      PIC 9(001) VALUE 0.        
          03 IND-ERROR-ID-CARDEC        PIC 9(001) VALUE 0.        
          03 ERR-EST-SCORES-CARDEC      PIC 9(001) VALUE 0.        
          03 EST-CREDIT-CARDEC          PIC 9(001) VALUE 0.        
          03 EST-CTACOR-CARDEC          PIC 9(001) VALUE 0.        
          03 EST-ENDEUD-CARDEC          PIC 9(001) VALUE 0.        
          03 EST-CONSUL-CARDEC          PIC 9(001) VALUE 0.        
          03 EST-IDENTI-CARDEC          PIC 9(001) VALUE 0.        
          03 EST-HIBRID-CARDEC          PIC 9(001) VALUE 0.        
          03 C-EST-CREDIT-CARDEC OCCURS 500 TIMES PIC X(14).        
          03 C-EST-CTACOR-CARDEC OCCURS 500 TIMES PIC X(14).        
          03 C-EST-ENDEUD-CARDEC OCCURS 500 TIMES PIC X(14).        
          03 C-EST-CONSUL-CARDEC OCCURS 500 TIMES PIC X(14).        
          03 C-EST-IDENTI-CARDEC OCCURS 500 TIMES PIC X(14).        
          03 C-EST-SCORES-CARDEC OCCURS 500 TIMES PIC X(08).        
          03 C-EST-HIBRID-CARDEC OCCURS 500 TIMES PIC X(14).        
      * SE INCLUYE VECTOR PARA CAMBIOS EN TIPOS DE CUENTA        
       01 WS-VCTR-MODTIPCTA-CARDEC         OCCURS 10 TIMES.        
          03 VCTR-MODTIPCTA-CARDEC         PIC 9(01)  VALUE 0.        
      *        
       01 WS-VCTR-TIPIDVAL-CARDEC OCCURS 4 TIMES.        
          03 VCTR-TIPIDVAL-CARDEC          PIC 9(01)  VALUE 0.        
