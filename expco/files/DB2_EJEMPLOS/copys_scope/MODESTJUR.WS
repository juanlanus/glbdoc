      * MODESTJUR.PROC.v.01.0003
      ******************************************************************
      * VARIABLES HIBRIDAS PARA CALCULAR MODELO DE PERSONAS JURIDICAS. *
      * - 2009-06-01.                                                  *
      ******************************************************************
      * DLM0002    | MOESJU * IMPLEMENTACION DEL MODELO PARA PERSONAS  *
      * DARIO LASSO|   JURIDICAS. COMENZO CON EL 215 Y SER REVERSO.    *
      * 05-JUN-2009|   REQ. 239.                                       *
      ******************************************************************
      * DLM0003    | INICIA * INICIALIZACION DE VARIABLES PARA CALIFI- *
      * DARIO LASSO|   CACIONES.                                       *
      * 08-JUL-2009|   REQ. 80526.                                     *
      ******************************************************************
       01  WS-VARIABLES-MODESTJUR.
         03  A-01ACP003CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP003CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP003HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP003IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP003OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP003RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP003VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP007CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP007CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP007HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP007IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP007OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP007RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01ACP007VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01EXP011CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01EXP011CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01EXP011HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01EXP011IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01EXP011OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01EXP011RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01EXP011VE              PIC S9(12)     VALUE  ZEROS.
MOESJU   03  A-01EXP012CC              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP012CO              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP012HP              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP012IN              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP012OT              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP012RO              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP012VE              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP013CC              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP013CO              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP013HP              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP013IN              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP013OT              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP013RO              PIC S9(12)     VALUE  ZEROS.     DLM0002
MOESJU   03  A-01EXP013VE              PIC S9(12)     VALUE  ZEROS.     DLM0002
         03  A-01MOR001CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR001CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR001HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR001IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR001OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR001RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR001VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR055CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR055CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR055HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR055IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR055OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR055RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR055VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR056CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR056CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR056HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR056IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR056OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR056RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR056VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR057CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR057CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR057HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR057IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR057OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR057RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR057VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR058CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR058CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR058HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR058IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR058OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR058RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR058VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR059CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR059CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR059HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR059IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR059OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR059RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR059VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR069CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR069CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR069HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR069IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR069OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR069RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR069VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR072CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR072CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR072HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR072IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR072OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR072RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR072VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR075CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR075CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR075HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR075IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR075OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR075RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR075VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR076CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR076CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR076HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR076IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR076OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR076RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR076VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR077CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR077CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR077HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR077IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR077OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR077RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR077VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR078CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR078CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR078HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR078IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR078OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR078RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR078VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR079CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR079CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR079HP              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR079IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR079OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR079RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR079VE              PIC S9(12)     VALUE  ZEROS.

         03  A-01MOR073RO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR073IN              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR073OT              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR073CO              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR073VE              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR073CC              PIC S9(12)     VALUE  ZEROS.
         03  A-01MOR073HP              PIC S9(12)     VALUE  ZEROS.

         03  EVER12024P                PIC S9(12)     VALUE  ZEROS.
         03  EVER12048P                PIC S9(12)     VALUE  ZEROS.
         03  EVER24P60                 PIC S9(12)     VALUE  ZEROS.
         03  EVER24P                   PIC S9(12)     VALUE  ZEROS.
         03  EVER3048P                 PIC S9(12)     VALUE  ZEROS.
         03  EVER48P120                PIC S9(12)     VALUE  ZEROS.
         03  EVER6024P                 PIC S9(12)     VALUE  ZEROS.
         03  EVER9024P                 PIC S9(12)     VALUE  ZEROS.
         03  EVERCC48P                 PIC S9(12)     VALUE  ZEROS.
         03  MOB48                     PIC S9(12)     VALUE  ZEROS.
         03  OPEN12                    PIC S9(12)     VALUE  ZEROS.
         03  OPEN48                    PIC S9(12)     VALUE  ZEROS.
         03  QALDIA                    PIC S9(12)     VALUE  ZEROS.
         03  RECMORA120                PIC S9(12)     VALUE  ZEROS.
         03  RECMORA30                 PIC S9(12)     VALUE  ZEROS.
         03  RECMORA60                 PIC S9(12)     VALUE  ZEROS.
         03  RECMORA90                 PIC S9(12)     VALUE  ZEROS.
         03  RECMORACC                 PIC S9(12)     VALUE  ZEROS.
         03  RECMORCC                  PIC S9(12)     VALUE  ZEROS.

         03  WOR-VALOR-PTO-COMA        PIC S9(12)V99  VALUE  ZEROS.
         03  WOR-VALOR-PTO-COMA2       PIC S9(12)V99  VALUE  ZEROS.
         03  WS-ARB-SCORE              PIC  9(04)     VALUE  ZEROS.

         03  W-VECTOR-MODEST           PIC  9(01)     VALUE  ZEROS.
         03  W-CONT-REPOR-MODEST       PIC  9(04)     VALUE  ZEROS.
INICIA   03  W-IND-MODEST              PIC S9(10)     VALUE  ZEROS.     DLM0003
INICIA   03  W-WORK-MODEST             PIC S9(10)     VALUE  ZEROS.     DLM0003
