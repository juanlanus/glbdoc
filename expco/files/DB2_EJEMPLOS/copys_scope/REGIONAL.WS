      *REGIONAL-WS.v.0002
      *********************************************************************
      * TABLA DE REGIONALES. SE DEBE TENER EN CUENTA QUE EL INDICE EN COBOL
      * NO PUEDE SER CERO, POR LO TANTO DEBE HACERSE UN MANEJO "ESPECIAL"
      * DE INDICES, DE TAL FORMA QUE EL SUBINDICE 1 SENALE A BOGOTA LA CUAL
      * TIENE CODIGO 0. LO MISMO APLICA PARA TODO EL ARREGLO.
      *********************************************************************
       01  TABLA-REGIONALES.
           03 FILLER                   PIC X(12) VALUE "BOGOTA      ".
           03 FILLER                   PIC X(12) VALUE "CALI        ".
           03 FILLER                   PIC X(12) VALUE "MEDELLIN    ".
           03 FILLER                   PIC X(12) VALUE "BARRANQUILLA".
           03 FILLER                   PIC X(12) VALUE "            ".
           03 FILLER                   PIC X(12) VALUE "            ".
           03 FILLER                   PIC X(12) VALUE "BUCARAMANGA ".
           03 FILLER                   PIC X(12) VALUE "            ".
           03 FILLER                   PIC X(12) VALUE "IBAGUE      ".
           03 FILLER                   PIC X(12) VALUE "PEREIRA     ".
           03 FILLER                   PIC X(12) VALUE "            ".
           03 FILLER                   PIC X(12) VALUE "            ".
           03 FILLER                   PIC X(12) VALUE "            ".
           03 FILLER                   PIC X(12) VALUE "            ".
           03 FILLER                   PIC X(12) VALUE "            ".
       01  TABLA-REGINALES-R           REDEFINES TABLA-REGIONALES.
           03 T-NOM-REG                PIC X(12) OCCURS 15 TIMES.
