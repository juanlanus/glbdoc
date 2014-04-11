      * RECHIST.WS.v.0002   
      *******************************************************************
REMARK* CAV0002    * NOVD48 - MANEJO DE NOVEDAD, MORA MAXIMA           *
REMARK* FEB.10/2009*          CON RESPECTO AL VECTOR DE 48 POSICIONES. *
REMARK* CAV0002    * NOVE48 - MANEJO DE MORAS 30,FECHA MORA MAX  DE 48 *
REMARK******************************************************************
REMARK* VARIABLES DEFINIDAS PARA RECONSTRUCCIÓN HISTÓRICA DE NOVEDADES Y
REMARK* CÁLCULO DE MORA MÁXIMA.
      *******************************************************************
REMARK**PARÁMETROS DE ENTRADA 
          03 NOV-ENT-RECHIST             PIC 9(02).
          03 VECTOR1-RECHIST             PIC 9(12).
          03 VECTOR2-RECHIST             PIC 9(12).
NOVD48    03 VECTOR3-RECHIST             PIC 9(12).                     CAV0002
NOVD48    03 VECTOR4-RECHIST             PIC 9(12).                     CAV0002
          03 FEC-NOV-RECHIST             PIC 9(04).
REMARK**PARÁMETROS DE SALIDA 
          03 MORMAX-RECHIST              PIC 9(01).
          03 NUM-MOR30-RECHIST           PIC 9(02).
          03 FEC-MORMAX-RECHIST          PIC 9(06).
          03 NOV-SAL-RECHIST             PIC 9(02).
NOVD48    03 FEC-NOV48-RECHIST           PIC 9(04).                     CAV0002
NOVD48    03 MORMAX48-RECHIST            PIC 9(01).                     CAV0002
NOVD48    03 NOV48-SAL-RECHIST           PIC 9(02).                     CAV0002
NOVE48    03 NUM-MOR30-48-RECHIST        PIC 9(02).                     CAV0002
NOVE48    03 FEC-MORMAX48-RECHIST        PIC 9(06).                     CAV0002
*************************************************************************
       01 MATRIZ-CADUCIDADES.
          03 FILLER OCCURS 60 TIMES.
             05 MATRIZ-RECHIST  OCCURS 5 TIMES PIC X(2).
             05 V-NOVEDAD-RECHIST        PIC 9.     
             05 V-MORMAX-RECHIST         PIC 9.     
             05 V-NOMBRE-RECHIST         PIC X(14).
             05 V-MORA-RECHIST           PIC 9.   
          03 HISTORIA-TOTAL-RECHIST.               
NOVD48       05 VECTOR-H4-RECHIST        PIC X(12).                     CAV0002
NOVD48       05 VECTOR-H3-RECHIST        PIC X(12).                     CAV0002
NOVD48       05 VECTOR-H2-RECHIST        PIC X(12).                     CAV0002
NOVD48       05 VECTOR-H1-RECHIST        PIC X(12).                     CAV0002
             05 EST-HIST-RECHIST         PIC X(1).
NOVD48*      05 VECTOR-RECHIST           PIC X(24).                     CAV0002
          03 FILLER REDEFINES HISTORIA-TOTAL-RECHIST.
NOVD48       05 V-VECTOR-RECHIST   OCCURS 49 TIMES PIC X.               CAV0002
       01 OTROS-CADUCIDADES.                       
          03 FIN-RECHIST                 PIC 9 VALUE 0.
          03 FIN-MATRIZ-RECHIST          PIC 99 VALUE 0.    
          03 ESTADO-RECHIST              PIC 99.    
          03 ESTADO-INI-RECHIST          PIC 99.    
          03 ESTADO-FIN-RECHIST          PIC 99.   
NOVD48    03 ESTADO-INI48-RECHIST        PIC 99.                        CAV0002
NOVD48    03 ESTADO-FIN48-RECHIST        PIC 99.                        CAV0002
          03 FECHA-RECHIST.                       
             05 ANO-T-RECHIST            PIC 9(4).
             05 FILLER REDEFINES ANO-T-RECHIST.    
                07 SIGLO-RECHIST         PIC 99.
                07 ANO-RECHIST           PIC 9(2).
             05 MES-RECHIST              PIC 9(2).
          03 FECHA-1-RECHIST.    
             05 ANO-T-1-RECHIST          PIC 9(4).     
             05 FILLER REDEFINES ANO-T-1-RECHIST.     
                07 SIGLO-1-RECHIST       PIC 99.     
                07 ANO-1-RECHIST         PIC 9(2).  
             05 MES-1-RECHIST            PIC 9(2). 
          03 FECHA-INI-RECHIST           PIC 9(6).
          03 MES-MORA-MAX-RECHIST        PIC 9(2).
          03 FEC-MM-AA-RECHIST           PIC 9(4).
          03 FEC-MM-AAAA-RECHIST         PIC 9(6).
NOVE48    03 MES-MORA-MAX48-RECHIST      PIC 9(2).                      CAV0002
NOVE48    03 I-RECHIST                   PIC 9(3) VALUE ZEROS.          CAV0002
NOVE48    03 J-RECHIST                   PIC 9(3) VALUE ZEROS.          CAV0002
NOVE48    03 K-RECHIST                   PIC 9(3) VALUE ZEROS.          CAV0002
