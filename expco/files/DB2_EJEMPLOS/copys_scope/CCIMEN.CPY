
       01 MENSAJE-CS.
          02 HEADER.
             03 HEADER-USUARIO            PIC X(10).
             03 HEADER-TIPO-MENSAJE       PIC 9(1).
             03 HEADER-LSN                PIC 9(4).
             03 HEADER-LONGITUD           PIC 9(4).
             03 HEADER-TERMINAL           PIC X(8).
             03 HEADER-NOMB-PROG          PIC X(6).
             03 HEADER-FILA               PIC 9(2).
             03 HEADER-COLUMNA            PIC 9(2).
             03 HEADER-CARACT-INICIO      PIC 9(1).
             03 HEADER-CARACT-FIN         PIC 9(1).
             03 HEADER-TIPO-TERMINAL      PIC X(6).
             03 HEADER-FILLER             PIC X(5).
          02 TEXTO                        PIC X(2000).

