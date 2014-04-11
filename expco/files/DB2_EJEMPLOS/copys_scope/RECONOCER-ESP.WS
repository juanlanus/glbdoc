      *----------------------------------------------------------------*
      *  VARIABALES PARA SCORE RECONOCER
      *----------------------------------------------------------------*
       01 VARIABLES-RECONOCER.
          03 FIN-BUSCAR                  PIC 99  VALUE 0.
          03 NADA                        PIC 9   VALUE 0.

       01 VARIABLES-SCO.
          03 PESO-DIR                    PIC 999.
          03 PESO-TEL                    PIC 999.
          03 PESO-CIU                    PIC 999.
          03 PESO-TOTAL-SCORE            PIC 999.
          03 TELEFONO-AUX                PIC X(15) JUSTIFIED RIGHT.
          03 AMP-F                       PIC X(10).
          03 PRO-F                       PIC X(10).
          03 INTERNET                    PIC 9(03).
          03 CIUDAD-F                    PIC X(15).
          03 DIRECCION-F                 PIC X(45).
          03 TELEFONO-F                  PIC X(15) .
          03 CALIFIC-AUX                 PIC 99 VALUE 0.
          03 ENTERO-AUX                  PIC 9(4) VALUE 0.
          03 DECIMAL-AUX                 PIC 99 VALUE 0.
          03 CALIFIC-DIR                 PIC 99 VALUE 0.
          03 HITS                        PIC 99 VALUE 0.
          03 DIG-DIR                     PIC 99 VALUE 0.
          03 DIG-DIR-F                   PIC 99 VALUE 0.
          03 POR-DIR                     PIC 999V99 VALUE 0.
       01 SALIDA-SCORE.
          03 FILLER                      PIC X    VALUE SPACES.
          03 FILLER                      PIC X(6) VALUE " DIR: ".
          03 DIR-SCORE                   PIC X(7) VALUE SPACES.
          03 FILLER                      PIC X(6) VALUE " TEL: ".
          03 TEL-SCORE                   PIC X(7) VALUE SPACES.
          03 FILLER                      PIC X(6) VALUE " CIU: ".
          03 CIU-SCORE                   PIC X(7) VALUE SPACES.
       01 DIR-SCORE-X.
          03 FILLER                      PIC X VALUE SPACES.
          03 DIR-SCORE-POR               PIC 999 VALUE 0.
          03 FILLER                      PIC X(3) VALUE "%  ".

