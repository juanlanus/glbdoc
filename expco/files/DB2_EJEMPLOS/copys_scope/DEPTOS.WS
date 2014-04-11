      *DEPTOS-WS.v.0003
      ******************************************************************
      * TABLA DE DEPARTAMENTOS                                         *
      ******************************************************************
REMARK* GJC0003  * CORDEP - SE COORIGE EL NOMBRE DE CASANARE Y LA      *  
REMARK* ENERO/2007* GUAJIRA                                            *
REMARK******************************************************************
       01  TABLA-DEPTOS.
           03 FILLER                   PIC X(10) VALUE "ANTIOQUIA ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "ATLANTICO ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "BOLIVAR   ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "BOYACA    ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "CALDAS    ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "CAUCA     ".
           03 FILLER                   PIC X(10) VALUE "CESAR     ".
           03 FILLER                   PIC X(10) VALUE "CORDOBA   ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "CUNDINAMAR".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "CHOCO     ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "HUILA     ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "MAGDALENA ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "NARI/O    ".
           03 FILLER                   PIC X(10) VALUE "RISARALDA ".
           03 FILLER                   PIC X(10) VALUE "N.DE S/DER".
           03 FILLER                   PIC X(10) VALUE "QUINDIO   ".
           03 FILLER                   PIC X(10) VALUE "SANTANDER ".
           03 FILLER                   PIC X(10) VALUE "SUCRE     ".
           03 FILLER                   PIC X(10) VALUE "TOLIMA    ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "VALLE     ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "ARAUCA    ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "CAQUETA   ".
           03 FILLER                   PIC X(10) VALUE "          ".
CORDEP     03 FILLER                   PIC X(10) VALUE "CASANARE  ".    GJC0003 
           03 FILLER                   PIC X(10) VALUE "          ".
CORDEP     03 FILLER                   PIC X(10) VALUE "LA GUAJIRA".    GJC0003 
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "GUAINIA   ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "META      ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "GUAVIARE  ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "SAN ANDRES".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "AMAZONAS  ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "PUTUMAYO  ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "VAUPES    ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "          ".
           03 FILLER                   PIC X(10) VALUE "EXTERIOR  ".
           03 FILLER                   PIC X(10) VALUE "VICHADA   ".
       01  TABLA-DEPTOS-R  REDEFINES TABLA-DEPTOS.
           03 T-NOM-DEP                PIC X(10) OCCURS 72 TIMES.
