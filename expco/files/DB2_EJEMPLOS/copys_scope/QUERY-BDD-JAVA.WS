REMARK*QUERY-BDD-JAVA.WS.v.01.0002.
JAVA02******************************************************************ABB0001
      *DLM0002 * CQ3744 : AMPLIACION FILER                             *
JAVA02******************************************************************ABB0001
JAVA02 01 I-JAVA02                         PIC 9(0006) VALUE ZEROS.     ABB0001
JAVA02 01 J-JAVA02                         PIC 9(0006) VALUE ZEROS.     ABB0001
JAVA02 01 K-JAVA02                         PIC 9(0006) VALUE ZEROS.     ABB0001
JAVA02 01 L-JAVA02                         PIC 9(0006) VALUE ZEROS.     ABB0001
JAVA02 01 M-JAVA02                         PIC 9(0006) VALUE ZEROS.     ABB0001
JAVA02 01 BDD-RETORNO-CONSULTA             PIC X(130020) VALUE SPACES.  ABB0001
CQ3744*JAVA02 01 BDD-CLASE-CONSULTA        PIC X(80)  VALUE SPACES.     DLM0002
CQ3744 01 BDD-CLASE-CONSULTA               PIC X(200) VALUE SPACES.     DLM0002
JAVA02 01 WSS-CLASE-CONSULTA-BDD.                                       ABB0001
JAVA02    03 BDD-NROIDEUSU                 PIC 9(11) VALUE ZEROS.       ABB0001
JAVA02    03 BDD-FILLER1                   PIC X(01) VALUE ",".         ABB0001
JAVA02    03 BDD-IDECLIE                   PIC 9(12) VALUE ZEROS.       ABB0001
JAVA02    03 BDD-FILLER2                   PIC X(01) VALUE ",".         ABB0001
JAVA02    03 BDD-ESTRATEGIA                PIC X(50) VALUE SPACES.      ABB0001
CQ3744    03                               PIC X(125) VALUE SPACES.     DLM0002
JAVA02 01 BDD-CARCTRSTCAS-GRP.                                          ABB0001
JAVA02    02 BDD-CARCTRSTCAS-OCC OCCURS 10000.                          ABB0001
JAVA02       03 BDD-CAR-NOM                PIC X(50).                   ABB0001
JAVA02       03 BDD-CAR-VAL                PIC X(50).                   ABB0001
JAVA02 01 WSS-CONSULTA-BDD-ULTIMO.                                      ABB0001
JAVA02    03 BDD-ESTRATEGIA-ULT            PIC X(50) VALUE SPACES.      ABB0001
JAVA02    03 BDD-FILLER1                   PIC X(01) VALUE ",".         ABB0001
JAVA02    03 BDD-IDECLIE-ULT               PIC 9(12) VALUE ZEROS.       ABB0001
JAVA02    03 BDD-FILLER2                   PIC X(01) VALUE ",".         ABB0001
JAVA02    03 BDD-NROIDEUSU-ULT             PIC 9(11) VALUE ZEROS.       ABB0001
CQ3744    03                               PIC X(125) VALUE SPACES.     DLM0002
JAVA02******************************************************************ABB0004
