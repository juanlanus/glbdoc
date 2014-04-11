      *GNRFUN-WS.v.0001
      *DEFINICION PARA EL MANEJO DE FUNCIONES F1...F10
      *ESCAPE, ENTER, BARRA SPACIADORA.
      *DE DEBE DEFINIR : SPECIAL-NAMES.
      *                   CRT STATUS IS KEY-STATUS.
      *SE DEBE PREGUNTAR POR EL VALOR DE KEY-CODE-1
      *EN EL PROGRAMA COBOL.
       01 KEY-STATUS.
         03 KEY-TYPE           PIC X.
           78 NO-KEY-RETURNED                  VALUE "0".
           78 USER-KEY-RETURNED                VALUE "1".
           78 FUNCTION-KEY-RETURNED            VALUE "2".
           78 DATA-KEY-RETURNED                VALUE "3".
           78 UNKNOWN-KEY-RETURNED             VALUE "4".
         03 KEY-CODE-1         PIC 9(2) COMP-X.
           78 ESCAPE-KEY                       VALUE 0.
           78 F1                               VALUE 1.
           78 F2                               VALUE 2.
           78 F3                               VALUE 3.
           78 F4                               VALUE 4.
           78 F5                               VALUE 5.
           78 F6                               VALUE 6.
           78 F7                               VALUE 7.
           78 F8                               VALUE 8.
           78 F9                               VALUE 9.
           78 F10                              VALUE 10.
           78 SPACE-BAR                        VALUE 32.
           78 CR-KEY                           VALUE 41.
           78 ENTER-KEY                        VALUE 48.
           78 78-ESCAPE                        VALUE 65.
         03 KEY-CODE-2         PIC 9(2) COMP-X.
