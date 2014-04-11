REMARK*BDAGCLA.WS.V03.0004.
REMARK*VARIABLES PARA GENERACION DE CLAVES DESDE PROGRAMA ICGMDF
      *----------------------------------------------------------------*
REMARK*FAC0003      *GENSE2*VARIABLE PARA CONTROLAR GENERACION DE CLAVES
REMARK*F. CUADROS C.*
REMARK*MAR.19/2003  *
      *----------------------------------------------------------------*

      * GJC0004       * CLVRND - SE CAMBA EL COPY DE GENERACION ALEATORIA
      * G.JIMENEZ     *          DE CLAVES PORQUE AL LLEGAR A UN LIMITE
      * NOVIEMBRE/2011*          LA RUTINA QUEDA EN LOOP. CON EL CAMBIO
      *               *          SE PUEDEN GENERAR HASTA APROX 1.300.000
      *               *          CLAVES
      ******************************************************************

       01  VARIABLES-BDAGCLA.
           03  ANO-VENCE-BDAGCLA            PIC 9(4) VALUE 0.
           03  SW-LECTURA-BDAGCLA           PIC 9    VALUE ZERO.
           03  SW-INVALIDO-BDAGCLA          PIC 9    VALUE ZERO.
           03  SW-ERROR-BDAGCLA             PIC 9    VALUE ZERO.
           03  SW-GRABAR-BDAGCLA            PIC 9    VALUE ZERO.
           03  COD-RETORNOT                 PIC XX   VALUE "00".
           03  COD-RETORNO-WEB              PIC XX   VALUE "00".
           03  COD-RETORNO-WS               PIC XX   VALUE "00".
           03  COD-RETORNO-ICBSUS           PIC XX   VALUE "00".
           03  COD-RETORNO-ICBTER           PIC XX   VALUE "00".
           03  I1-BDAGCLA                   PIC 9(2) VALUE ZEROS.
           03  K-BDAGCLA                    PIC 9(4)  VALUE ZEROS.
           03  N-BDAGCLA                    PIC 9(4)  VALUE ZEROS.
           03  J-BDAGCLA                    PIC 9(4)  VALUE ZEROS.
           03  L-BDAGCLA                    PIC 9(2)  VALUE ZEROS.
           03  NUM-1-BDAGCLA                PIC 9(3)  VALUE ZEROS.
           03  NUM-2-BDAGCLA                PIC 9(2)V9(8)  VALUE ZEROS.
           03  LET-N-BDAGCLA                PIC 9(3)  VALUE ZEROS.
           03  LET-I-BDAGCLA                PIC 9(3)  VALUE ZEROS.
           03  NUM-I-BDAGCLA                PIC 9(4)  VALUE ZEROS.
           03  NUM-J-BDAGCLA                PIC 9(4)  VALUE ZEROS.
           03  NOM-VAL-W-BDAGCLA            PIC X(45) VALUE SPACES.
           03  SW-NOMBRES-BDAGCLA           PIC 9     VALUE 0.
           03  NUM-ID                       PIC 9(10) VALUE ZEROS.
           03  FECHA-SIST-BDAGCLA           PIC X(08) VALUE ZEROS.
           03  FECHA-HORA-BDAGCLA           PIC X(16) VALUE ZEROS.
GENSE2     03  TOT-GEN-BDAGCLA              PIC 9(4)  VALUE ZEROS.      FAC0003
      *
CLVRND*----------------------------------------------------------------*GJC0004
CLVRND* VARIABLES PARA GENERACION ALEATORIA DE CLAVES                   GJC0004
CLVRND 01  VARIABLES-CLAVE-BDAGCLA.                                     GJC0004
CLVRND     02 RESIDUO-RANDOM-BDAGCLA PIC 99.                            GJC0004
CLVRND     02 SEMILLA-CLV-BDAGCLA PIC 9(5).                             GJC0004
CLVRND     02 NUM-RANDOM-BDAGCLA PIC 9(9).                              GJC0004
CLVRND     02 FILLER REDEFINES NUM-RANDOM-BDAGCLA.                      GJC0004
CLVRND        03 NUM1-RANDOM-BDAGCLA PIC 9(01).                         GJC0004
CLVRND        03 NUM2-RANDOM-BDAGCLA PIC 9(02).                         GJC0004
CLVRND        03 FILLER OCCURS 3 TIMES.                                 GJC0004
CLVRND           04 NUM3-RANDOM-BDAGCLA PIC 9(02).                      GJC0004
CLVRND*----------------------------------------------------------------*GJC0004

       01  TABLA-CIUDADES-BDAGCLA.
           03 TAB-CIU-W-BDAGCLA OCCURS 999 ASCENDING KEY IS
                  SEC-CIUDAD-W-BDAGCLA INDEXED BY I-BDAGCLA.
              05  SEC-CIUDAD-W-BDAGCLA      PIC 9(3).
              05  SEC-CIUDAD-W-R-BDAGCLA REDEFINES SEC-CIUDAD-W-BDAGCLA
                                      PIC X(3).
              05  DES-CORTA-W-BDAGCLA PIC X(3).
              05  NOMBRE-W-BDAGCLA    PIC X(30).
              05  REGIONAL-W-BDAGCLA  PIC 9(3).

       01  TAB-LETRAS-BDAGCLA.
           03 FILLER PIC X(26) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01  TAB-LETRAS-R-BDAGCLA REDEFINES TAB-LETRAS-BDAGCLA.
           03 TAB-LET-BDAGCLA        OCCURS 26    PIC X(1).

       01 TAB-EQUIV-BDAGCLA.
           03 FILLER PIC X(52) VALUE
              "6566676869707172737475767778798081828384858687888990".
       01  TAB-EQUIV-R-BDAGCLA REDEFINES TAB-EQUIV-BDAGCLA.
           03 TAB-EQUI-BDAGCLA       OCCURS 26 PIC 9(2).

       01  TAB-LETRAS-2-BDAGCLA.
           03 FILLER PIC X(26) VALUE
              "abcdefghijklmnopqrstuvwxyz".
       01  TAB-LETRAS-2-R-BDAGCLA  REDEFINES TAB-LETRAS-2-BDAGCLA.
           03 TAB-LET-2-BDAGCLA      OCCURS 26 PIC X(1).

       01 TAB-EQUIV-2-BDAGCLA.
           03 FILLER PIC X(55) VALUE
              "0970980991001011021031041051061071081091101111121131141".
           03 FILLER PIC X(23) VALUE
              "15116117118119120121122".
       01  TAB-EQUIV-2-R-BDAGCLA  REDEFINES TAB-EQUIV-2-BDAGCLA.
           03 TAB-EQUI-2-BDAGCLA     OCCURS 26 PIC 9(3).

       01  VECTOR-LETRAS-BDAGCLA.
           03 LET-BDAGCLA OCCURS 3 TIMES PIC 999V9(8).

       01  NUM-ID-W-BDAGCLA.
           03  NUM-ID-W10-BDAGCLA    PIC 9(10) VALUE ZEROS.
           03  NUM-ID-W10-R-BDAGCLA REDEFINES NUM-ID-W10-BDAGCLA.
               05 NUM-ID-W9-BDAGCLA      PIC 9(09).
               05  RESTO-NUM-ID-BDAGCLA  PIC 9.
       01  NOMBRES-AUX-W-BDAGCLA.
           03 PRIMER-APELLIDO-BDAGCLA    PIC X(15).
           03 SEGUNDO-APELLIDO-BDAGCLA   PIC X(15).
           03 PRIMER-NOMBRE-BDAGCLA      PIC X(15).
           03 SEGUNDO-NOMBRE-BDAGCLA     PIC X(15).
       01  VECTOR-ERRORES-BDAGCLA        PIC X(102).
       01  VECTOR-ERROR-W-BDAGCLA REDEFINES VECTOR-ERRORES-BDAGCLA.
           03 ERROR-W-BDAGCLA OCCURS 102 PIC 9(1).

       01 PARAMETROS-ENTRADA-BDAGCLA     PIC X(80).
       01 PARAMETROS-ENTRADA-R-BDAGCLA REDEFINES
                                       PARAMETROS-ENTRADA-BDAGCLA.
           03 TIP-IDE-BDAGCLA            PIC X.
           03 NUM-IDE-BDAGCLA            PIC X(11).
           03 COD-SUS-BDAGCLA            PIC X(6).
           03 NOM-DUE-BDAGCLA            PIC X(24).
           03 NOM-OFI-BDAGCLA            PIC X(15).
           03 COD-CIU-BDAGCLA            PIC X(3).
           03 FLA-COK-BDAGCLA            PIC X(1).
           03 IND-HOR-BDAGCLA            PIC X(1).
           03 FILLER-BDAGCLA             PIC X(18).

       01 PARAMETROS-SALIDA-BDAGCLA.
          03 COD-RET-BDAGCLA             PIC 9(2).
          03 VECTOR-ERROR-BDAGCLA        PIC X(102).
          03 TOT-CLA-BDAGCLA             PIC X(5).
          03 CLAVE-WEB-BDAGCLA           PIC X(10).
          03 IND-WEB-BDAGCLA             PIC X.
