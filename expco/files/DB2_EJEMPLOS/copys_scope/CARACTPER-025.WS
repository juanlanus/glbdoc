REMARK*CARACTPER-025.WS.v.0006.
      ******************************************************************
      * IMS0001    | CQ7064 - DAVIVIENDA.                              *
      * 13-AGO-2012|    REQ. CQ. 7064.                                 *
      * M.SALAMANCA|                                                   *
      ******************************************************************
      * IMS0002    | CQ9050 - DAVIVIENDA.                              *
      * 17-DIC-2012|    REQ. CQ. 9050.                                 *
      * M.SALAMANCA|                                                   *
      ******************************************************************
      * CAV0003    | CQ9566 - DAVIVIENDA.                              *
      * 19-ENE-2013|          INCLUSION NUEVSA CARACTERISTICAS         *
      * C.VELASCO  |                                                   *
      ******************************************************************
      * IMS0004    | CQ9986 - SE ADICIONA CARACTERISTICA 01END999VE025 *
      * 13-FEB-2013|                                                   *
      * M.SALAMANCA|                                   REQ. CQ. 9986.  *
      ******************************************************************
      * IMS0005    | R11031 - SE ADICIONAN CARACTERISTICAS             *
      * 20-MAR-2013|          01END991RO025 Y 01END990RO025            *
      * M.SALAMANCA|                                      REQ. CQ11031 *
      ******************************************************************
      * IMS0006    | R10959 - SE ADICIONAN CARACTERISTICAS             *
      * 27-MAR-2013|          PERSONALIZADAS.                          *
      * M.SALAMANCA|                                      REQ. CQ10959 *
      ******************************************************************
        01 PERSONALIZACION-25.
           
           02 PER-25-NIVEL-00-END.
CQ9986*       03 00END999IN025               PIC 9(11).                 IMS0004
CQ9986        03 00END999IN025               PIC 99999999.99.           IMS0004
CQ9986        03 00END999VE025               PIC 99999999.99.           IMS0004
              03 00END998RO025               PIC 9(11).
              03 00END996HP025               PIC 9(11).
              03 00END995HP025               PIC 9(11).
              03 00END994TO025               PIC 9(11).
              03 00END997RO025               PIC 9(09)V9(02).
CQ9050        03 00END994RO025               PIC 9(06)V9(05).           IMS0002
CQ9050        03 00END993RO025               PIC 9(06)V9(05).           IMS0002
CQ9566        03 00END992MX025               PIC 99999999.99.           CAV0003
R11031        03 00END991RO025               PIC 99999999.99.           IMS0005
R11031        03 00END990RO025               PIC 99999999.99.           IMS0005
                        
           02 PER-25-NIVEL-01-END.
              03 01END999IN025               PIC 99999999.99.
CQ9986        03 01END999VE025               PIC 99999999.99.           IMS0004
              03 01END998RO025               PIC 99999999.99.
              03 01END997RO025               PIC 99999999.99. 
              03 01END996HP025               PIC 99999999.99.
              03 01END995HP025               PIC 99999999.99.
              03 01END994TO025               PIC 99999999.99.
CQ9050        03 01END994RO025               PIC 99999999.99.           IMS0002
CQ9050        03 01END993RO025               PIC 99999999.99.           IMS0002
CQ9566        03 01END992MX025               PIC 99999999.99.           CAV0003
R11031        03 01END991RO025               PIC 99999999.99.           IMS0005
R11031        03 01END990RO025               PIC 99999999.99.           IMS0005

CQ9566     02 PER-25-NIVEL-00-ENDV-TMP.                                 CAV0003
CQ9566        03 00ENDV992MX025              PIC 9(08)V99.              CAV0003
CQ9986        03 00ENDV999IN025              PIC 9(08)V99.              IMS0004
CQ9986        03 00ENDV999VE025              PIC 9(08)V99.              IMS0004
R11031        03 00ENDV991RO025              PIC 9(08)V99.              IMS0005
R11031        03 00ENDV990RO025              PIC 9(08)V99.              IMS0005

CQ9566     02 PER-25-NIVEL-01-ENDV-TMP.                                 CAV0003
CQ9566        03 01ENDV992MX025              PIC 9(08)V99.              CAV0003
CQ9986        03 01ENDV999IN025              PIC 9(08)V99.              IMS0004
CQ9986        03 01ENDV999VE025              PIC 9(08)V99.              IMS0004
R11031        03 01ENDV991RO025              PIC 9(08)V99.              IMS0005
R11031        03 01ENDV990RO025              PIC 9(08)V99.              IMS0005

R10959     02 PER-25-NIVEL-00-NUM.                                      IMS0006
R10959        03 00NUM999RO025               PIC 9(4).                  IMS0006

R10959     02 PER-25-NIVEL-01-NUM.                                      IMS0006
R10959        03 01NUM999RO025               PIC 9(4).                  IMS0006

R10959     02 PER-25-NIVEL-00-EXP.                                      IMS0006
R10959        03 00EXP999RO025               PIC 9(4).                  IMS0006
R10959        03 00EXP998RO025               PIC 9(4).                  IMS0006
R10959        03 00EXP997RO025               PIC 9(4).                  IMS0006
R10959        03 00EXP996RO025               PIC 9(4).                  IMS0006

R10959     02 PER-25-NIVEL-01-EXP.                                      IMS0006
R10959        03 01EXP999RO025               PIC 9(4).                  IMS0006
R10959        03 01EXP998RO025               PIC 9(4).                  IMS0006
R10959        03 01EXP997RO025               PIC 9(4).                  IMS0006
R10959        03 01EXP996RO025               PIC 9(4).                  IMS0006

R10959     02 PER-25-NIVEL-01-INQ.                                      IMS0006
R10959        03 01INQ999_025                PIC 9(04).                 IMS0006
R10959        03 01INQ998_025                PIC 9(04).                 IMS0006

           02 PER-25-NIVEL-01-DEM.
              03 01DEM999_025                PIC 9.
              03 01DEM998_025                PIC 9.
CQ9566        03 01DEM997_025                PIC 9.                     CAV0003
           
           02 PER-25-VAR-TMP.
              03 END-CUOT25                  PIC 9(09)V99.
              03 REL-025                     PIC 9(09)V99999999.
              03 E-UTL                       PIC 9(09)V99999999.
              03 UTL-K                       PIC S9(09)V99999999.
              03 END-P                       PIC 9(09)V99999999.
              03 00END997RO025-AUX           PIC 99999999.99.
CQ9050        03 00END994RO025-AUX           PIC 99999999.99.           IMS0002
CQ9050        03 00END993RO025-AUX           PIC 99999999.99.           IMS0002
CQ9050        03 E-UTL-X                     PIC 9(09)V99999.           IMS0002
CQ9050        03 UTL-K-X                     PIC S9(09)V99999.          IMS0002
CQ9050        03 END-P-X                     PIC 9(09)V99999.           IMS0002
CQ9050        03 SAL-RO-X                    PIC 9(09)V9(05).           IMS0002
R10959        03 VEC-NUM-CTA                 PIC 9(18).                 IMS0006
              03           REDEFINES VEC-NUM-CTA.
                04 VEC-NUM-CTA-R    OCCURS   18 TIMES.
                  05 VEC-NUM-CTA-POS         PIC 9.                 
R10959        03 I-BIN                       PIC 9(02).                 IMS0006
R10959        03 F-BIN                       PIC 9(02).                 IMS0006
R10959        03 MESES-APE-RNDT              PIC 9(04).                 IMS0006
              
           02 PER-25-VAR-ACU.
              03 COUNT-HP-25                 PIC 9(03).
              03 MAYOR-HP-25                 PIC 9(09)V99.
              03 MEDIO-HP-25                 PIC 9(09)V99.
              03 SWTCH-HP-25                 PIC 9.
       
           02 TABLA-CUO-X-MILLON-25.
              03      PIC X(23) VALUE "01TDC         000668000". 
              03      PIC X(23) VALUE "02Rotativo    000668000".
              03      PIC X(23) VALUE "03Plazo Fijo  000411000".
              03      PIC X(23) VALUE "04Hipotecario 000127000".
              03      PIC X(23) VALUE "05Vehiculo    000288000".
              03      PIC X(23) VALUE "06Normalizados000262000".
              03      PIC X(23) VALUE "07Portafolio  000668000".
              03      PIC X(23) VALUE "08Sector Real 000411000".
           02 TAB-CUO-X-MILLON-25 REDEFINES TABLA-CUO-X-MILLON-25.
              03 TAB-CUO-X-MILLN-25    OCCURS   8 TIMES.
                 04 TAB-TIP-CRED-25          PIC 99.                    
                 04 TAB-DES-CRED-25          PIC X(12).        
                 04 TAB-CUO-MILL-25          PIC 9(04)V9(05).

CQ9566     02 TABLA-CUO-X-MILLON-25R.                                   CAV0003
CQ9566        03 TAB-CUO-X-MILLN-25R   OCCURS   8 TIMES.                CAV0003
CQ9566           04 TAB-TIP-CRED-25R         PIC 99.                    CAV0003
CQ9566           04 TAB-DES-CRED-25R         PIC X(12).                 CAV0003
CQ9566           04 TAB-CUO-MILL-25R         PIC 9(04)V9(05).           CAV0003
                   
           02 TABLA-FACTOR-25.
CQ9550        03      PIC X(19) VALUE "01Factor A000.27667".            IMS0002
R11031        03      PIC X(19) VALUE "02Factor B000.42860".            IMS0005
R11031*CQ9550 03      PIC X(19) VALUE "02Factor B000.04286".            IMS0005
CQ9550     02 TABLA-FACTOR-25-R  REDEFINES  TABLA-FACTOR-25.            IMS0002
CQ9550        03 TABLA-FACTOR-25-R     OCCURS   2 TIMES.                IMS0002
CQ9550           04 TAB-FAC-POS-25           PIC 9(02).                 IMS0002
CQ9550           04 TAB-FAC-DES-25           PIC X(08).                 IMS0002
CQ9550           04 TAB-FAC-VAL-25           PIC 999.99999.             IMS0002
CQ9050           04           REDEFINES  TAB-FAC-VAL-25.                IMS0002
CQ9050               05 TAB-FIL-FAC-25       PIC 99.                    IMS0002
CQ9050               05 TAB-VAL-FAC-25       PIC 9.99999.               IMS0002
CQ9050*                                                                 IMS0002
CQ9550*       03  FAC-A-25    PIC 9.9999 VALUE 0.2736.                  IMS0002
CQ9550*       03  FAC-B-25    PIC 9.9999 VALUE 0.4268.                  IMS0002

R10959     02 PER-11-COD-BIN-DAV       PIC 999999 VALUE ZEROS.          IMS0006
R10959        88 COD-BIN-DAV         VALUE 320000 320511 320523 320586  IMS0006
R10959                                     320609 320650 360324 439116  IMS0006
R10959                                     455983 455986 491646 520024  IMS0006
R10959                                     531378 540692 547130.        IMS0006

R10959     02 PER-11-COD-SUS-DAV       PIC 999999 VALUE ZEROS.          IMS0006
R10959        88 COD-SUS-DAV         VALUE 004321 010001 010042 010047  IMS0006
R10959         020002 020042 020047 020055 020068 020142 020242 020247  IMS0006
R10959         020342 020347 020421 020442 020521 020542 020642 020647  IMS0006
R10959         020742 020747 020842 020942 020947 021142 021242 021342  IMS0006
R10959         021347 021421 021442 021447 021542 021742 021747 021842  IMS0006
R10959         021921 021942 021947 022042 022047 022142 022347 022742  IMS0006
R10959         022942 023042 023047 023242 023342 023442 023842 023942  IMS0006
R10959         024242 024442 024842 025342 025542 025842 026342 027042  IMS0006
R10959         027242 028142 028242 028842 029147 029821 029942 029968  IMS0006
R10959         030001 050001 050002 050004 050006 050018 050019 050022  IMS0006
R10959         050042 050047 050101 050147 050201 050247 050301 050304  IMS0006
R10959         050404 050601 050701 050901 051101 051147 051201 051547  IMS0006
R10959         052101 052501 059347 059447 059547 059647 059701 059747  IMS0006
R10959         059801 059842 059847 059901 059947 120047 130002 130102  IMS0006
R10959         130202 130402 130502 130602 130802 130902 131202 132102  IMS0006
R10959         132302 132502 132602 133202 133902 134402 140047 140058  IMS0006
R10959         180015 180055 180155 180255 180355 180455 180615 180755  IMS0006
R10959         180955 200047 250001 259447 259942 510001.               IMS0006
              
           02 CONSTANTES-DAV-25.
              03 NUM-CUO-ROT                 PIC 9(02) VALUE 24. 
                                                    
           02 WS-SWITCHES-DAV.
              03 SW-NODESEADOS-DAV           PIC 9(01).
              03 SW-VIP-DAV                  PIC 9(01).
CQ9566        03 SW-EMPL-DAV                 PIC 9(01).                 CAV0003

CQ9050     02 SW-SWITCH-PARAM-DAV.                                      IMS0018
CQ9050        03 W-FIN-PARAM-DAV             PIC 9(01) VALUE ZERO.      IMS0018
CQ9566        03 W-FIN-CUOXM-DAV             PIC 9(01) VALUE ZERO.      CAV0003
CQ9566        03 W-IND-CUOXM-DAV             PIC 9(01) VALUE ZERO.      CAV0003
CQ9566*                                                                 CAV0003
CQ9566     02 VAR-00MOR-PER25.                                          CAV0003
CQ9566         05 00MOR999-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR998-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR997-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR996-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR995-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR994-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR993-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR992-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR991-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR990-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR989-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR988-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR987-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR986-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR985-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR984-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR983-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR982-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR981-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR980-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR979-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR978-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR977-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR976-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR975-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR974-PER25             PIC 9(4).                  CAV0003
CQ9566         05 00MOR973-PER25             PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566 01 VARIABLES-01MOR-PER25.                                        CAV0003
CQ9566    02 VAR-01MOR-PER25.                                           CAV0003
CQ9566       03 TAB-PORT-01MOR-PER25 OCCURS 9 TIMES.                    CAV0003
CQ9566         05 01MOR999-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR998-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR997-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR996-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR995-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR994-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR993-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR992-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR991-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR990-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR989-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR988-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR987-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR986-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR985-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR984-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR983-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR982-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR981-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR980-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR979-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR978-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR977-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR976-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR975-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR974-PER25             PIC 9(4).                  CAV0003
CQ9566         05 01MOR973-PER25             PIC 9(4).                  CAV0003
CQ9566    02 VAR-CARACTBPX-PER25 REDEFINES VAR-01MOR-PER25.             CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974RO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973RO025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974IN025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973IN025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974VE025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973VE025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974HP025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973HP025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974OT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973OT025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974CC025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973CC025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974CO025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973CO025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974AH025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973AH025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566         05 01MOR999CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR998CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR997CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR996CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR995CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR994CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR993CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR992CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR991CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR990CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR989CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR988CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR987CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR986CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR985CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR984CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR983CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR982CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR981CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR980CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR979CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR978CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR977CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR976CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR975CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR974CT025              PIC 9(4).                  CAV0003
CQ9566         05 01MOR973CT025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003
CQ9566  01 VARIABLES-02MOR-PER25.                                       CAV0003
CQ9566     02 VAR-02MOR-PER25.                                          CAV0003
CQ9566         05 02MOR999TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR998TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR997TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR996TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR995TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR994TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR993TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR992TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR991TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR990TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR989TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR988TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR987TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR986TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR985TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR984TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR983TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR982TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR981TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR980TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR979TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR978TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR977TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR976TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR975TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR974TO025              PIC 9(4).                  CAV0003
CQ9566         05 02MOR973TO025              PIC 9(4).                  CAV0003
CQ9566*                                                                 CAV0003

