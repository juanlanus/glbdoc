
       01 MENSAJE-CS-DIAN.
          02 HEADER-DIAN.
             03 HEADER-USUARIO-DIAN       PIC X(10).
             03 HEADER-TIPO-MENSAJE-DIAN  PIC 9(1).
             03 HEADER-LSN-DIAN           PIC 9(4).
             03 HEADER-LONGITUD-DIAN      PIC 9(4).
             03 HEADER-TERMINAL-DIAN      PIC X(8).
             03 HEADER-NOMB-PROG-DIAN     PIC X(6).
             03 HEADER-FILA-DIAN          PIC 9(2).
             03 HEADER-COLUMNA-DIAN       PIC 9(2).
             03 HEADER-CARACT-INICIO-DIAN PIC 9(1).
             03 HEADER-CARACT-FIN-DIAN    PIC 9(1).
             03 HEADER-TIPO-TERMINAL-DIAN PIC X(6).
             03 HEADER-FILLER-DIAN        PIC X(5).
          02 TEXTO-DIAN                   PIC X(2000).

