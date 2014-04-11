REMARK*BDIIVIDE.WS.v03.0003.
REMARK*VARIABLES PARA RUTINA DE VALIDACION DE IDENTIFICACIONES
      *----------------------------------------------------------------*
      * F.CUADROS   * INCLUSION DE PARAMETRO PARA ASIGNAR TIPO DE
      * JUN.17/2004 * IDENTIFICACION DE ACUERDO A RANGOS PERMITIDOS.
      * FAC0002     * NUIP01 * ESTO SI NO VIENE CORRECTO EL TIPO-ID.
      *----------------------------------------------------------------*
      * F.CUADROS   * INCLUSION DE VALIDACION PARA TIPO-ID. = 2 y
      * JUL.19/2004 * QUITAR EL DIGITO DE CHEQUEO SI ES EL CASO.    
      * FAC0003     *
      *----------------------------------------------------------------*
       01  NUM-ID-W-BDIIVIDE.
NUIP02     03  NUM-ID-W11-BDIIVIDE           PIC 9(11) VALUE ZEROS.     FAC0003
NUIP02     03  NUM-ID-W11-R-BDIIVIDE REDEFINES NUM-ID-W11-BDIIVIDE.     FAC0003
NUIP02         05 NUM-ID-W10-BDIIVIDE        PIC 9(10).                 FAC0003
NUIP02         05 RESTO-NUM-ID-BDIIVIDE      PIC 9.                     FAC0003
       01  NOMBRES-AUX-W-BDIIVIDE.
           03 PRIMER-APELLIDO-BDIIVIDE       PIC X(15) VALUE SPACES.
           03 SEGUNDO-APELLIDO-BDIIVIDE      PIC X(15) VALUE SPACES.
           03 PRIMER-NOMBRE-BDIIVIDE         PIC X(15) VALUE SPACES.
           03 SEGUNDO-NOMBRE-BDIIVIDE        PIC X(15) VALUE SPACES.
           03 NOM-LEIDO-BDIIVIDE             PIC X(65) VALUE SPACES.

NUIP01 01 PARAMETROS-ENTRADA-BDIIVIDE        PIC X(100) VALUE SPACES.   FAC0002 
       01 PARAMETROS-ENTRADA-R-BDIIVIDE REDEFINES
                                            PARAMETROS-ENTRADA-BDIIVIDE.
           03 TIP-IDE-BDIIVIDE               PIC X.
           03 NUM-IDE-BDIIVIDE               PIC X(11) JUSTIFIED RIGHT.
NUIP01     03 NUM-IDE-BDIIVIDE-R REDEFINES   NUM-IDE-BDIIVIDE           FAC0002 
NUIP01                                       PIC 9(11).                 FAC0002 
           03 NOMBRE-BDIIVIDE                PIC X(65).
           03 SW-VERIFNOM-BDIIVIDE           PIC 9.
           03 NUM-BLOQUES-BDIIVIDE           PIC 9.
NUIP01     03 SW-VALTIPID-BDIIVIDE           PIC 9.                     FAC0002 
NUIP01     03 SW-ASGTIPID-BDIIVIDE           PIC 9.                     FAC0002 
NUIP02     03 SW-DIGCHEQUEO-BDIIVIDE         PIC 9.                     FAC0003 
NUIP02     03 FILLER-BDIIVIDE                PIC X(18).                 FAC0003 

       01 PARAMETROS-SALIDA-BDIIVIDE.
          03 NOM-VAL-BDIIVIDE                PIC X(65) VALUE SPACES.
          03 SW-ID-BDIIVIDE                  PIC 9 VALUE 0.
          03 SW-NOMBRES-BDIIVIDE             PIC 9 VALUE 0.
NUIP01    03 SW-TID-BDIIVIDE                 PIC 9 VALUE 0.             FAC0002 
