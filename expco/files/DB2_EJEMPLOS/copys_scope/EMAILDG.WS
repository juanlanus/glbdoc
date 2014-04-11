      *
      * Comando mail para DG/UX.  No soporta "subject" de SCO
      * 
       01  COMANDO-EMAIL.
           03 FILLER               PIC X(05) VALUE "echo ".
           03 FILLER               PIC X(6)  VALUE "'     ".
           03 FILLER               PIC X(10)  VALUE "Programa: ".
           03 PROGRAMA-EMAIL       PIC X(09).
           03 FILLER               PIC X(1)  VALUE x"0A".
           03 REFERENCIA-EMAIL     PIC X(40) VALUE
              "     !!! ERROR EN EJECUCION !!! ".
           03 FILLER               PIC X(1)  VALUE x"0A".
           03 FILLER               PIC X(1)  VALUE x"0A".
           03 FILLER               PIC X(5)  VALUE "     ".
           03 MENSAJE-EMAIL        PIC X(70).
           03 FILLER               PIC X(1)  VALUE "'".
           03 FILLER               PIC X(12) VALUE " | mail ".
           03 USUARIO-EMAIL        PIC X(09) VALUE " sysadm".
           03 FILLER               PIC X     VALUE x"00".
