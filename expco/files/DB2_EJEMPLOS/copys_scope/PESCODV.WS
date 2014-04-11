REMARK*PESCODV.WS.V.01.0001
REMARK**********************************************************************
REMARK*** DEFINICION DE VARIABLES PARA UTILIZAR EL SCORING DE DAVIVIENDA ***
REMARK**********************************************************************
REMARK**** VARIABLES GENERALES
REMARK**** ===================
       01  W-VARIABLES-PESCODV.
         03  DAVISC-TRABAJO            PIC  9(08)   VALUE  ZEROS.
         03  DAVISC-CONTADOR           PIC  9(08)   VALUE  ZEROS.
         03  DV-TASA-ACIERTA           PIC  9(03)   VALUE  ZEROS.
         03  DV-X1                     PIC  9(03)   VALUE  ZEROS.
         03  DV-X2                     PIC  9(03)   VALUE  ZEROS.
         03  DV-X3                     PIC  9(03)   VALUE  ZEROS.
         03  DV-X                      PIC V9(02)   VALUE  ZEROS.
         03  DV-Y                      PIC V9(02)   VALUE  ZEROS.
         03  DV-W                      PIC V9(02)   VALUE  ZEROS.
         03  DV-ACUM-PUNT              PIC  9(03)   VALUE  ZEROS.
         03  DV-ACUM-ACIER             PIC  9(03)   VALUE  ZEROS.
         03  DV-ACUM-CLIE              PIC  9(03)   VALUE  ZEROS.
         03  DV-CONT-CLIE              PIC  9(03)   VALUE  ZEROS.
         03  DV-CONT-ANTI              PIC  9(03)   VALUE  ZEROS.
         03  DV-CONT-ACIE              PIC  9(03)   VALUE  ZEROS.
         03  DV-MES-ANTIGUE            PIC  9(08)   VALUE  ZEROS.
         03  DV-ANTIG-PUNT             PIC  9(03)   VALUE  ZEROS.
         03  DV-SC                     PIC  9(02)   VALUE  ZEROS.
         03  DV-SEGMENTO               PIC  9(01)   VALUE  ZEROS.
         03  DV-PRODUCTO               PIC  X(15)   VALUE  SPACES.
         03  DV-SCORE                  PIC S9(04)   VALUE  ZEROS.
         03  DV-SCORE-FINAL            PIC S9(04)V99  VALUE  ZEROS.         
         03  DV-SCORE-SLD              PIC  9(04)   VALUE  ZEROS.
         03  WK-DV-ERROR               PIC  X(10)   VALUE  SPACES.
         03  SW-CLTE-DAVIENDA          PIC  X(01)   VALUE  SPACES.
         03  DV-TASA-INI1              PIC  9(04)   VALUE  ZEROS.
         03  DV-TASA-FIN1              PIC  9(04)   VALUE  ZEROS.
         03  DV-PUNT-AC1               PIC  9(04)   VALUE  ZEROS.
         03  DV-TASA-INI2              PIC  9(04)   VALUE  ZEROS.
         03  DV-TASA-FIN2              PIC  9(04)   VALUE  ZEROS.
         03  DV-PUNT-AC2               PIC  9(04)   VALUE  ZEROS.
         03  DV-TASA-INI3              PIC  9(04)   VALUE  ZEROS.
         03  DV-TASA-FIN3              PIC  9(04)   VALUE  ZEROS.
         03  DV-PUNT-AC3               PIC  9(04)   VALUE  ZEROS.
         03  DV-TASA-INI4              PIC  9(04)   VALUE  ZEROS.
         03  DV-TASA-FIN4              PIC  9(04)   VALUE  ZEROS.
         03  DV-PUNT-AC4               PIC  9(04)   VALUE  ZEROS.         
         03  DV-ANTI-INI1              PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-INI2              PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-INI3              PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-INI4              PIC  9(04)   VALUE  ZEROS.         
         03  DV-ANTI-FIN1              PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-FIN2              PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-FIN3              PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-FIN4              PIC  9(04)   VALUE  ZEROS.         
         03  DV-ANTI-AC1               PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-AC2               PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-AC3               PIC  9(04)   VALUE  ZEROS.
         03  DV-ANTI-AC4               PIC  9(04)   VALUE  ZEROS.         
      **** ========
      **** SWITCHES
      **** ========
         03  SW-SI-GAR-DAVI            PIC  9(01)   VALUE  ZEROS.
         03  SW-CARGUE-DAVI            PIC  9(01)   VALUE  ZEROS.
         03  SW-NO-GAR-DAVI            PIC  9(01)   VALUE  ZEROS.
         03  SW-NO-PROD-DAVI           PIC  9(01)   VALUE  ZEROS.
         03  SW-CARGUES-DAVI.
           05  SW-PAR-DAV-55           PIC  9(01)   VALUE  ZEROS.
           05  SW-PAR-DAV-56           PIC  9(01)   VALUE  ZEROS.
           05  SW-PAR-DAV-57           PIC  9(01)   VALUE  ZEROS.
           05  SW-PAR-DAV-58           PIC  9(01)   VALUE  ZEROS.
           05  SW-PAR-DAV-59           PIC  9(01)   VALUE  ZEROS.
         03 TABLA-RAN-DAV-ACI.
            05  PIC X(09) VALUE "650950090".
            05  PIC X(09) VALUE "630649079".
            05  PIC X(09) VALUE "610629062".
            05  PIC X(09) VALUE "000609000".            
         03                  REDEFINES  TABLA-RAN-DAV-ACI.
            05   TABLA-RANGO        OCCURS 04 TIMES.
              07  TAB-RANGO-INI  PIC  9(03).
              07  TAB-RANGO-FIN  PIC  9(03).
              07  TAB-RANGO-PUNT PIC  9(03).
         03  TABLA-RAN-ANT.
            05  PIC X(09) VALUE "101999097".
            05  PIC X(09) VALUE "006100097".            
            05  PIC X(09) VALUE "002005084".
            05  PIC X(09) VALUE "000001070".
         03                  REDEFINES  TABLA-RAN-ANT.
            05   TABLA-RANGOS       OCCURS 04 TIMES.
              07  TAB-ANTIG-INI  PIC  9(03).
              07  TAB-ANTIG-FIN  PIC  9(03).
              07  TAB-ANTIG-PUNT PIC  9(03).
         03  TABLA-CLI-PRO.
            05 PIC X(36) VALUE "01 CLIENTE BANCO-GARANTIA        100".
            05 PIC X(36) VALUE "02 CLIENTE BANCO-NO GARANTIA     100".
            05 PIC X(36) VALUE "03 NO CLIENTE BANCO-GARANTIA     085".
            05 PIC X(36) VALUE "04 NO CLIENTE BANCO-NO GARANTIA  075".
            05 PIC X(36) VALUE "05 NO CLIENTE BANCO-NO PRODUCTOS 000".
         03                  REDEFINES  TABLA-CLI-PRO.
            05   TABLA-CLIENT       OCCURS 05 TIMES.
              07  TAB-CLIEN-COD-PRODU  PIC  9(02).
              07  TAB-CLIEN-DESC-PRODU PIC  X(31).
              07  TAB-CLIEN-TASA-PRODU PIC  9(03).
         03  TABLA-PES-VAR.
            05  PIC X(35) VALUE "01 ACIERTA             X         43".
            05  PIC X(35) VALUE "02 ANTIGUEDAD          Y         14".
            05  PIC X(35) VALUE "03 CLIENTE - PRODUCTO  W         43".
         03                  REDEFINES  TABLA-PES-VAR.
            05   TABLA-PESOVAR      OCCURS 03 TIMES.
              07  TAB-PESO-COD-VARIA  PIC  9(02).
              07  TAB-PESO-DESC-VARIA PIC  X(31).
              07  TAB-PESO-TASA-VARIA PIC V9(02).
         03  TABLA-SCORE.
            05  PIC X(36) VALUE "01 CORTE SCORE                   085".
         03                  REDEFINES  TABLA-SCORE.
            05   TABLA-PESOVAR      OCCURS 01 TIMES.
              07  TAB-PESO-COD-SCORE  PIC  9(02).
              07  TAB-PESO-DES-SCORE  PIC  X(31).
              07  TAB-PESO-VAL-SCORE  PIC  9(03).
         03  TABLA-PORCENTAJES-LTV.
           05  TABLA-PORC-LTV      OCCURS  500  TIMES
               ASCENDING           KEY  IS  TAB-LTV-KEY
               INDEXED  BY  IND-T.
             07  TAB-LTV-KEY.
               09  TAB-LTV-KEY-TIPVEH PIC  9(02).
               09  TAB-LTV-KEY-INDUSO PIC  9(02).
               09  TAB-LTV-KEY-MODELO PIC  9(04).
             07  TAB-LTV-KEY-PORCENT  PIC  9(03)V99.
         03  W-STATUS-DAV1             PIC  9(02)    VALUE  ZEROS.
         03                      REDEFINES  W-STATUS-DAV1.
           05  W-STATUS-DAV-2          PIC  9(01).
           05  W-STATUS-DAV-3          PIC  9(02)    COMP.
       01  ARCH-PARAM-DAVIENDA.
         03  ARCHIVO-PARAMDAVI.
           05                          PIC  X(07) VALUE "$DATOS/".
           05                          PIC  X(30)
                                          VALUE "PARAM-PESCODV.DAT".
