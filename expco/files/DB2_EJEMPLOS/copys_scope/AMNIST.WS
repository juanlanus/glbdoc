******* AMNIST-WS.v.0007  
*************************************************************************
******* VARIABLES DEFINIDAS PARA APLICACION DE LEYES, AMNISTIA, REESTRUC- 
******* TURACION Y DACION DE PAGO.
*************************************************************************
      * GJC0003 * INCLUSION DE VARIABLES PARA APLICACION DE DACION Y REES
      * 22-05-00* TRUCTURACION.
      * GRACIELA*                                                      
      *******************************************************************
      * GJC0004 * PARA LAS CUENTAS AMNISTIADAS  SE PERDONA ADJETIVO SI ES
      * 11-10-00* DIFERENTE DE FALLECIDO Y LA FECHA ES MENOR A LA MAXIMA
      * GRACIELA* DE AMNISTIA                                          
      *******************************************************************
      * GJC0005 * PARA AMNISTIA DE ADJETIVOS, SE REQUIERE CONOCER LA FECHA
      * 21-11-00* DEL PRIMER PERDON (MAS RECIENTE) Y ULTIMO PERDON (MAS 
      * GRACIELA* VIEJO) PARA ESTO SE CREAN LAS DOS VARIABLES          
      *******************************************************************
      * RMB0006 * SE DEFINEN VARIABLES PARA UTILIZAR EN EL PROCEDIMIENTO   
      * ALIVIO:   DE ALIVIO DE LA LEY 716                                 
      * 5-FEB-2002DE ALIVIO DE LA LEY 716                                 
      *******************************************************************
REMARK* VHF0007     * LEY002 - SE IMPLEMENTAN CAMBIOS PARA LEY, EN     *
REMARK* VICTOR HUGO *          PARTICULAR LOS CAMBIOS DE LO RELACIONADO*
REMARK* FORERO      *          CON 48 MESES DE HISTORIA                *
REMARK* MAY.09/2008 *                                                  *
REMARK******************************************************************
       01 MATRICES-AMNISTIA.
          03 FILLER OCCURS 60 TIMES.
             05 MATRIZ-AMNI  OCCURS 5 TIMES PIC X(2).
             05 V-NOVEDAD-AMNI        PIC 9.     
             05 V-MORMAX-AMNI         PIC 9.     
             05 V-NOMBRE-AMNI         PIC X(14).
             05 V-MORA-AMNI           PIC 9.   
          03 HISTORIA-TOTAL-AMNI.               
             05 EST-HIST-AMNI         PIC X(1).
             05 VECTOR-AMNI           PIC X(24).
          03 FILLER REDEFINES HISTORIA-TOTAL-AMNI.
             05 V-VECTOR-AMNI   OCCURS 25 TIMES PIC X.
LEY002    03 HISTORIA-TOTAL-AMNI-48.                                    VHF0007 
LEY002       05 EST-HIST-AMNI-48      PIC X(1).                         VHF0007 
LEY002       05 VECTOR-AMNI-48        PIC X(48).                        VHF0007 
LEY002    03 FILLER REDEFINES HISTORIA-TOTAL-AMNI-48.                   VHF0007 
LEY002       05 V-VECTOR-AMNI-48      OCCURS 49 TIMES PIC X.            VHF0007 
       01 OTROS-AMNISTIA.                       
          03 FIN-AMNI                 PIC 9 VALUE 0.
          03 FIN-MATRIZ-AMNI          PIC 99 VALUE 0.    
          03 ANM                      PIC 99.    
          03 ESTADO-AMNI              PIC 99.    
          03 ESTADO-INI-AMNI          PIC 99.    
          03 ESTADO-FIN-AMNI          PIC 99.   
          03 FECHA-AMNI.                       
             05 ANO-T-AMNI            PIC 9(4).
             05 FILLER REDEFINES ANO-T-AMNI.    
                07 SIGLO-AMNI         PIC 99.
                07 ANO-AMNI           PIC 9(2).
             05 MES-AMNI              PIC 9(2).
          03 FECHA-1-AMNI.                                              GJC0003 
             05 ANO-T-1-AMNI          PIC 9(4).                         GJC0003 
             05 FILLER REDEFINES ANO-T-1-AMNI.                          GJC0003 
                07 SIGLO-1-AMNI       PIC 99.                           GJC0003 
                07 ANO-1-AMNI         PIC 9(2).                         GJC0003 
             05 MES-1-AMNI            PIC 9(2).                         GJC0003 
          03 FECHA-INI-AMNI           PIC 9(6).
          03 MES-MORA-MAX-AMNI        PIC 9(2).
          03 FEC-MM-AA-AMNI           PIC 9(4).
          03 FEC-MM-AAAA-AMNI         PIC 9(6).
          03 VECTOR1-AMNI             PIC 9(12).
          03 VECTOR2-AMNI             PIC 9(12).
LEY002    03 VECTOR3-AMNI             PIC 9(12).                        VHF0007 
LEY002    03 VECTOR4-AMNI             PIC 9(12).                        VHF0007 
          03 FEC-LIM-SUP-AMNI         PIC 9(6).                         GJC0003 
          03 FEC-LIM-SUP-AMNI-R REDEFINES FEC-LIM-SUP-AMNI.             GJC0003 
             05 SIGLO-LIM-SUP-AMNI    PIC 9(2).                         GJC0003 
             05 ANO-LIM-SUP-AMNI      PIC 9(2).                         GJC0003 
             05 MES-LIM-SUP-AMNI      PIC 9(2).                         GJC0003 
          03 FEC-LIM-SUP-1-AMNI       PIC 9(6).                         GJC0003 
          03 FEC-LIM-INF-AMNI         PIC 9(6).                         GJC0003 
          03 FEC-LIM-INF-1-AMNI       PIC 9(6).                         GJC0003 
          03 TEMP-AMNI                PIC 9(2).                         GJC0003 
          03 FECHA-SITUACION-AMNI     PIC 9(6).                         GJC0003 
          03 F-MA-AUX-AMNI            PIC 9(4).                         GJC0004 
          03 F-MA-AUX-AMNI-R REDEFINES F-MA-AUX-AMNI.                   GJC0004 
             05 F-MA-MES-AUX-AMNI     PIC 9(2).                         GJC0004 
             05 F-MA-ANO-AUX-AMNI     PIC 9(2).                         GJC0004 
          03 F-AM-AUX-AMNI.                                             GJC0004 
             05 F-AM-SIG-AUX-AMNI     PIC 9(2).                         GJC0004 
             05 F-AM-ANO-AUX-AMNI     PIC 9(2).                         GJC0004 
             05 F-AM-MES-AUX-AMNI     PIC 9(2).                         GJC0004 
          03 FEC-PRIM-AMNI            PIC 9(6).                         GJC0005 
          03 FEC-ULT-AMNI             PIC 9(6).                         GJC0005 
ALIVIO 01 VARIABLES-ALV.                                                RMB0006
ALIVIO    03 VAUX-ALV.                                                  RMB0006
ALIVIO       04 VECTOR1-AUX-ALV       PIC X(12).                        RMB0006
ALIVIO       04 VECTOR2-AUX-ALV       PIC X(12).                        RMB0006
ALIVIO    03 VACT-ALV.                                                  RMB0006
ALIVIO       04 VECTOR1-ACT-ALV       PIC X(12).                        RMB0006
ALIVIO       04 VECTOR2-ACT-ALV       PIC X(12).                        RMB0006
ALIVIO    03 CTAS-POR-ALIVIAR-ALV     PIC 9(5).                         RMB0006
ALIVIO    03 EN-HABITO-PAGO-ALV       PIC 9.                            RMB0006
ALIVIO    03 APLICA-LEY716-ALV        PIC 9.                            RMB0006
ALIVIO    03 CUENTA-ALIVIADA-ALV      PIC 9.                            RMB0006
ALIVIO    03 PAGO-EN-2002-ALV         PIC 9.                            RMB0006
ALIVIO    03 CTAS-TOT-ALV             PIC 9(5).                         RMB0006
ALIVIO    03 ALIVIO-NEGADO-ALV        PIC 9.                            RMB0006
ALIVIO    03 POS-PARTIR-ALV           PIC 99.                           RMB0006
ALIVIO    03 FEC-PAGO-ALV             PIC 9(6).                         RMB0006
ALIVIO    03 NOV-REAL-ALV             PIC 99.                           RMB0006
ALIVIO    03 MORA-MAXIMA-ALV          PIC 9.                            RMB0006
ALIVIO    03 NUM-MORAS30-ALV          PIC 99.                           RMB0006
ALIVIO    03 FECHA-MORA-MAXIMA-ALV    PIC 9(6).                         RMB0006
ALIVIO    03 M-PARTIR-ALV.                                              RMB0006
ALIVIO       04 FILLER OCCURS 200 TIMES.                                RMB0006
ALIVIO          05 V-NUMCTA-ALV       PIC 9(4).                         RMB0006
ALIVIO          05 V-PARTIR-ALV       PIC 9(2).                         RMB0006
ALIVIO          05 V-TIPCTA-ALV        PIC X(03).                       RMB0006
ALIVIO          05 V-ENTINF-ALV        PIC X(14).                       RMB0006
ALIVIO          05 V-FECNOV-ALV        PIC X(6).                        RMB0006
ALIVIO          05 V-NROCTA-ALV        PIC X(09).                       RMB0006
ALIVIO          05 V-FECAPE-ALV        PIC X(6).                        RMB0006
ALIVIO          05 V-FECVEN-ALV        PIC X(6).                        RMB0006
ALIVIO          05 V-ENTIN2-ALV        PIC X(14).                       RMB0006
ALIVIO          05 V-NROCT2-ALV        PIC X(9).                        RMB0006
ALIVIO          05 V-VECTOR-ALV.                                        RMB0006
ALIVIO             06 VECMOR-ALV OCCURS 24 TIMES PIC X.                 RMB0006
ALIVIO    03 M-MORA-AVL.                                                RMB0006
ALIVIO       04 FILLER PIC X(25) VALUE "5999999999995555123411212".     RMB0006
ALIVIO       04 FILLER PIC X(25) VALUE "3234123412341234999494999".     RMB0006
ALIVIO    03 FILLER REDEFINES M-MORA-AVL.                               RMB0006
ALIVIO       04 V-MORA-AVL OCCURS 50 TIMES PIC 9.                       RMB0006
