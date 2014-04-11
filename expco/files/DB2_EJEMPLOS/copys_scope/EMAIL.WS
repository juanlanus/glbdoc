       01  COMANDO-EMAIL.
           03 FILLER               PIC X(05) VALUE "echo ".
           03 MENSAJE-EMAIL        PIC X(70).
           03 FILLER               PIC X(12) VALUE " | mail -s ".
           03 PROGRAMA-EMAIL       PIC X(09).
           03 REFERENCIA-EMAIL     PIC X(24) VALUE
              " ERROR EN EJECUCION !!!'".
           03 FILLER               PIC X(05) VALUE " root".
           03 FILLER               PIC X     VALUE x"00".
