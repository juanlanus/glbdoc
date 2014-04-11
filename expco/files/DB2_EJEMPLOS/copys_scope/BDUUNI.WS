******* BDUUNI-WS.v.0006
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES QUE
******* UTILIZAN LAS RUTINAS DE BASE DE DATOS UNIVERSAL
      ******************************************************************
      * RMB0002     * BDUTIT - SE ADAPTA PARA ACEPTAR DINAMICAMENTE LA *
      * R MARTINEZ  *          PUBLICIDAD                              *
      * SEP.20/2002 *                                                  *
      ******************************************************************       
      * RMB0003     * BOL001 - AJUSTES PARA CAPA DE PRESENTACION A     *
      * R MARTINEZ  *          TRAVES DE WEB                           *
      * ABRIL/2004  *                                                  *
      ******************************************************************       
      * RMB0004     * HIPE01 - VARIABLE DE ALMACENAMIENTO DEL CONTROL  *
      * R MARTINEZ  *          DEL HIPERVINCULO PARA BDUS              *
      * MAY-10-2005 *                                                  *
      ******************************************************************       
      * RMB0005     * EMP001 - CREACION DE LA MECÁNICA PARA EL MANEJO  * 
      * R.MARTINEZ  *          DEL SCORE EMPIRICO                      *
      * MARZO/2006  *                                                  *
      ******************************************************************    
      * GJC0006    * PROEX1: MANEJO DE PRODUCTOS EXTERNOS AL ICGCON      *
      * G.JIMENEZ C*                                                     *
      * DIC./2007  *                                                     *
     *********************************************************************
BOL001 01 TIT-BDUS.                                                     RMB0003 
BOL001       04 FILLER            PIC X(24) VALUE ALL "=" .             RMB0003 
BOL001       04 FILLER            PIC X(3) VALUE " [ ".                 RMB0003 
BOL001       04 FILLE    PIC X(27) VALUE "OTRA INFORMACION DISPONIBLE". RMB0003 
BOL001       04 FILLER            PIC X(3) VALUE " ] ".                 RMB0003 
BOL001       04 FILLER            PIC X(23) VALUE ALL "=".              RMB0003 
NEWUNI 01 AREA-PUBLICIDAD.
NEWUNI    03 PUB-LIN                     PIC X(80) VALUE ALL "@@".
NEWUNI    03 PUB-COM.
NEWUNI       04 FILLER                   PIC XX VALUE "@<".
NEWUNI       04 PUB-COM-NOM              PIC X(3) VALUE SPACES.
EMP001       04 FILLER                   PIC X(2) VALUE "> ".           RMB0005 
EMP001       04 PUB-TIP-FOR              PIC X    VALUE SPACES. *>E=EJE RMB0005 
EMP001       04 FILLER                   PIC X    VALUE SPACES. *>F=FOR RMB0005 
EMP001*      04 FILLER                   PIC X(4) VALUE ">   ".         RMB0005 
BDU001       04 PUB-COM-EXP              PIC X(20) VALUE SPACES.
BDUTIT       04 PUB-MSG                  PIC X(49) .                    RMB0002
NEWUNI       04 FILLER                   PIC X(2) VALUE " @".
NEWUNI    03 PUB-VECTOR .
NEWUNI       04 PUB-VEC-COM     PIC X(03) OCCURS 50 TIMES. 
NEWUNI       04 PUB-VEC-EXP     PIC X(56) OCCURS 50 TIMES.
NEWUNI       04 PUB-VEC-HUE     PIC 9     OCCURS 50 TIMES.
HIPE01       04 PUB-VEC-HIP     PIC 9     OCCURS 50 TIMES.              RMB0004
PROEX1       04 PUB-VEC-EXT     PIC X     OCCURS 50 TIMES.              GJC0006
NEWUNI    03 PUB-MEMORIA .
NEWUNI        04 PUB-VEC-MEMORIA OCCURS 50 TIMES PIC X(80).
NEWUNI    03 PUB-FILLER           PIC X(100).
          03 PUB-COMANDO4         PIC X(04).                                   
NEWUNI    03 PUB-COMANDO          PIC X(03).
NEWUNI    03 PUB-PRODUCTO         PIC X(05) VALUE "<   >".
NEWUNI    03 FIN-BDUUNI           PIC 9 VALUE 0.
NEWUNI    03 OK-BDUUNI            PIC 9 VALUE 0.
BDU001    03 TIP-CON-BDUUNI-AUX   PIC 9 VALUE 0.
BDUTIT    03 MSG-DEFAULT-BDU          PIC X(49) 
BDUTIT       VALUE "    HAY MAS INFORMACION DE SU CLIENTE,CONSULTENOS".
NEWUNI    03 LINEA1-UNI.
NEWUNI       04 FILLER            PIC X(14) VALUE "! FUENTE    : ".
NEWUNI       04 SUS-UNI           PIC X(15).
NEWUNI       04 FILLER            PIC X(22) VALUE SPACES.
NEWUNI       04 FILLER            PIC X(16) VALUE " ACTUALIZADO A: ".
NEWUNI       04 FEC-UNI           PIC 9999/99/99.    
NEWUNI       04 FILLER            PIC X(03) VALUE "  !".
NEWUNI    03 TITULO-UNI.
NEWUNI       04 FILLER            PIC X(01) VALUE "!".
NEWUNI       04 FILLER            PIC X(26) VALUE ALL "=" .
BOL001       04 FILLER            PIC X(3) VALUE " [ ".                 RMB0003 
NEWUNI       04 LEYENDA-UNI       PIC X(20) VALUE SPACES.
BOL001       04 FILLER            PIC X(3) VALUE " ] ".                 RMB0003 
NEWUNI       04 FILLER            PIC X(26) VALUE ALL "=".     
NEWUNI       04 FILLER            PIC X(01) VALUE "!".
NEWUNI    03 LINEA-UNIVERSAL.
NEWUNI       04 FILLER            PIC X VALUE "!".
NEWUNI       04 INF-LINEA-UNI     PIC X(78).  
NEWUNI       04 FILLER            PIC X VALUE "!".
NEWUNI    03 IGUALES              PIC X(78) VALUE ALL "=".
MATCH      03 NOMBRE-COMPLETO.
MATCH        04  PR-APELLIDO               PIC X(15).
MATCH        04  FILLER                    PIC X(30).
MATCH      03 VECTOR-INPUT.
MATCH        04 V-INPUT OCCURS 10 TIMES    PIC X(15).
MATCH      03 VECTOR-ARCHI.
MATCH        04 V-ARCHI OCCURS 10 TIMES    PIC X(15).
MATCH      03 BLOQUES-OK                 PIC 9(2).
MATCH      03 MIN-BLOQUES                PIC 9(2).
MATCH      03 TOT-BLOQUES                PIC 9(2).
MATCH      03 CALIFICACION-NOM           PIC 9(5).
MATCH      03 LON-MEJOR-NOMBRE           PIC 9(5).
MATCH      03 MEJOR-NOMBRE               PIC X(45).
MATCH      03 NOMBRE-AUX                 PIC X(45).          
MATCH     02 V-SECUENCIAS.
MATCH        03 V-SEC OCCURS 100 TIMES      PIC 9.
MATCH     02 PAR-MATCH    PIC 99.
MATCH     02 SEC-AUX      PIC 99.                                              
       01 FORMATO-MENU-PRODUCTOS-BDU.
          02 TITULO-MENU-PRO-BDU.
             05 FILLER   PIC X(16) VALUE ALL "@".
             05 FILLER   PIC X(24) VALUE " PRODUCTOS  DISPONIBLES ".
             05 FILLER   PIC X(24) VALUE "BASE DE DATOS UNIVERSAL ".
             05 FILLER   PIC X(16) VALUE ALL "@".
          02 LINEA-MENU-PRO-BDU.
             05 FILLER           PIC X     VALUE "@".
             05 FILLER           PIC X(21) VALUE SPACES.                        
             05 FILLER           PIC X(02) VALUE " <".                          
             05 INI-MENU-BDU     PIC X(3).
             05 FILLER           PIC X(3)  VALUE ">. ".
             05 MENSAJE-MENU-BDU PIC X(20).
             05 FILLER           PIC X(29).
             05 FILLER           PIC X     VALUE "@".
         02 RAYAS-BDU.
          05 FILLER      PIC X(78) VALUE ALL "@".
          05 FILLER      PIC X(02) VALUE ALL "!*".   
