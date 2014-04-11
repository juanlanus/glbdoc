******* VALNOM-WS.v.0002
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES QUE
******* UTILIZAN LAS RUTINAS DEL       DE NOMBRES DE LA RUTINA VALNOM.PROC
*****************************************************************************
REMARK* GJC0002    * VALMEJ :SE CREAN ANUEVOS CAMPOS PARA MEJORAR LA VALIDACION
REMARK* G.JIMENEZ  * DE NIT. PARA MEJORA TAMBIEN SE AMPLIA TAMANO DE VARIABLES
REMARK* MARZO/2003 *
REMARK*************************************************************************
        01 VARIABLES-VALNOM.                                   
           03 VECTOR-INPUT-VALNOM.
             04 VM-INPUT-VALNOM     OCCURS 20 TIMES .
                05 VMM-INPUT-VALNOM OCCURS 15 TIMES PIC X.
           03 VECTOR-OK-VALNOM.
             04 VM-OK-VALNOM         OCCURS 20 TIMES.
                05 VMM-OK-VALNOM    OCCURS 15 TIMES PIC X.
           03 BLOQOK-AUX-VALNOM           PIC 9(2).
           03 CADENA-VALNOM.
              04 FILLER                   PIC X(2) VALUE "A ".
              04 NOMBRE-AUX-VALNOM        PIC X(65).          
              04 FILLER REDEFINES NOMBRE-AUX-VALNOM.
                 05 VNOM-AUX-VALNOM OCCURS 65 TIMES PIC X.
           03 BASURA-VALNOM               PIC X(10).
           03 BLOQUES-INPUT-VALNOM        PIC 9(2).
           03 NOMBRE-INPUT-VALNOM         PIC X(65).
           03 FILLER REDEFINES NOMBRE-INPUT-VALNOM.
              05 V-NOMBRE-VALNOM          PIC X OCCURS 65 TIMES.
           03 NOMBOK-INPUT-VALNOM         PIC X(65).
           03 FILLER REDEFINES NOMBOK-INPUT-VALNOM.
              05 V-NOMBOK-VALNOM          PIC X OCCURS 65 TIMES.
           03 TIP-ID-INPUT-VALNOM         PIC 9(02).
           03 MATCH-OK-OUT-VALNOM         PIC 9(01).
           03 A-VALNOM                    PIC 9(05).
VALMEJ     03 I-VALNOM                    PIC 9(05).                    GJC0002 
VALMEJ     03 J-VALNOM                    PIC 9(05).                    GJC0002 
VALMEJ     03 K-VALNOM                    PIC 9(05).                    GJC0002 
VALMEJ     03 L-VALNOM                    PIC 9(05).                    GJC0002 
VALMEJ     03 M-VALNOM                    PIC 9(05).                    GJC0002 
           03 INI-VALNOM                  PIC 9(02).
           03 LON-VALNOM                  PIC 9(02).
           03 LETRAS-OK-VALNOM            PIC 9(02).          
           03 LETRAS-OK2-VALNOM           PIC 9(02).          
           03 TOTLET-OK-VALNOM            PIC 9(02).          
           03 TOTLET-INP-VALNOM           PIC 9(02).          
           03 PEDAZO-VALNOM               PIC X(15).
           03 POR-TOT-VALNOM              PIC 9(05).          
           03 POR-INP-VALNOM              PIC 9(05).          
           03 APROX-TOT-VALNOM            PIC 9(05).          
           03 APROX-INP-VALNOM            PIC 9(05).          
VALMEJ     03 VECTORES-VALNOM.                                          GJC0002 
VALMEJ        04 FILLER                   OCCURS 256 TIMES.             GJC0002 
VALMEJ           05 CONT-LET-INPUT-VALNOM PIC 9(2).                     GJC0002 
VALMEJ           05 CONT-LET-OK-VALNOM    PIC 9(2).                     GJC0002 
VALMEJ     03 VAL-APELLIDO-VALNOM         PIC 9.                        GJC0002 
