REMARK* VALVLR-WS.0003
REMARK*************************************************************************
REMARK* CONTIENE LAS VARIABLES NECESARIAS PARA EL COPY VALCRE.PROC
REMARK*************************************************************************
REMARK* FAC0002     * SE ADICIONA VARIABLE FLAG-VALVLR PARA MANEJO DE MAPA    *
REMARK* FLOR ANGELA * DE BITS                                                 *
REMARK* CUADROS     *                                                         *
REMARK* MAY.28/2002 *                                                         *
REMARK*************************************************************************
REMARK* RMB0003     * COLP02- SE ACTIVA LA OCURRENCIA 16 DEL MAPA DE BITS PARA*
REMARK* R.MARTINEZ  *         MANEJO DEL CUPO INICIAL COMO USO EXCLUSIVO DEL  *
REMARK* JUN.15/2006 *         CLIENTE                                         *
REMARK*************************************************************************
VALVAL 01  FECHA-SITUACION-AUX-VALVLR     PIC 9(06).                           
VALVAL 01  FECHA-SITUACION-AUX-R-VALVLR   REDEFINES
VALVAL                                    FECHA-SITUACION-AUX-VALVLR.          
VALVAL     03 ANO-SITUACION-AUX-VALVLR    PIC 9(04).                           
VALVAL     03 MES-SITUACION-AUX-VALVLR    PIC 9(02).                           
VALVAL 01  F-ULT-ACT-AUX-VALVLR           PIC 9(06).                           
VALVAL 01  F-ULT-ACT-AUX-R-VALVLR         REDEFINES
VALVAL                                    F-ULT-ACT-AUX-VALVLR.       
VALVAL     03 ANO-ULT-ACT-AUX-VALVLR      PIC 9(04).                           
VALVAL     03 MES-ULT-ACT-AUX-VALVLR      PIC 9(02).                           
VALVAL 01  FEC-NOVEDAD-AUX-VALVLR         PIC 9(06).                           
VALVAL 01  FEC-NOVEDAD-AUX-R-VALVLR REDEFINES FEC-NOVEDAD-AUX-VALVLR.          
VALVAL     03 ANO-NOVEDAD-AUX-VALVLR      PIC 9(04).                           
VALVAL     03 MES-NOVEDAD-AUX-VALVLR      PIC 9(02).                           
VALVAL 01  FECHA-ACT-VALOR-AUX-VALVLR     PIC 9(06).                       
VALVAL 01  FECHA-ACT-VALOR-AUX-R-VALVLR                                   
VALVAL     REDEFINES               FECHA-ACT-VALOR-AUX-VALVLR.            
VALVAL     03 ANO-ACT-VALOR-AUX-VALVLR    PIC 9(04).                       
VALVAL     03 MES-ACT-VALOR-AUX-VALVLR    PIC 9(02).                       
VALVAL 01  FEC-4-MA-VALVLR                PIC 9(04).                           
VALVAL 01  FEC-4-MA-R-VALVLR       REDEFINES FEC-4-MA-VALVLR.                  
VALVAL     03 MES-4-MA-VALVLR             PIC 9(02).                           
VALVAL     03 ANO-4-MA-VALVLR             PIC 9(02).                           
VALVAL 01  FEC-6-MA-VALVLR                PIC 9(06).                           
VALVAL 01  FEC-6-MA-R-VALVLR            REDEFINES FEC-6-MA-VALVLR.             
VALVAL     03 MES-6-MA-VALVLR             PIC 9(02).                           
VALVAL     03 ANO-6-MA-VALVLR .
VALVAL        04 SIG-6-MA-VALVLR          PIC 9(02).                           
VALVAL        04 ANO2-6-MA-VALVLR         PIC 9(02).                           
VALVAL 01  FEC-6-AM-VALVLR                PIC 9(06).                           
VALVAL 01  FEC-6-AM-R-VALVLR            REDEFINES FEC-6-AM-VALVLR.             
VALVAL     03 SIG-6-AM-VALVLR             PIC 9(02).                           
VALVAL     03 ANO-6-AM-VALVLR             PIC 9(02).                           
VALVAL     03 MES-6-AM-VALVLR             PIC 9(02).                           
VALVAL 01  IND-VALORES-VALVLR             PIC 9(01) VALUE 0.
       01  FLAG-VALVLR                    PIC 9(02) VALUE 0.            FAC0002
VALVAL 01  VECTOR-VALORES-VALVLR.
COLP02     03 VEC-VALOR-VALVLR            PIC 9(01) OCCURS 16 TIMES.    RMB0003 
COLP02 01  VECTOR-ACTIVADO-VALVLR         PIC X(16) VALUE               RMB0003 
COLP02                                             "1111111111111110".  RMB0003 
