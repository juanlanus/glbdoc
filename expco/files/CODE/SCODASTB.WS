REMARK*  SCODASTB-WS.V.0001
REMARK*****************************************************************
REMARK* ESTE COPY CONTIENE LAS DEFINICIONES DE LAS VARIABLES PROPIAS DEL
REMARK* SCORE STABILITY  (DASTB)                
REMARK******************************************************************

       01 VARIABLES-WORK-DASTB.       
          03 CLASN-OUTPUT-DASTB        PIC 9(03).            
          03 CLASA-OUTPUT-DASTB        PIC X(02).            
          03 CLASDA-OUTPUT-DASTB       PIC X(02).            
          03 ADVERSE-RAZON-DASTB OCCURS 4 TIMES PIC 9(02).      
          03 SCORE-CARD-DASTB          PIC 9(02).
          03 CON-EXCLUSION-DASTB       PIC 9(01).

       01 CONSTANTES-DASTB.       
          03 DIM-VEC-DASTB             PIC 9(12) VALUE 13.
          03 K-EXC-DASTB               PIC 9(02) VALUE 99.

       01 VARIABLES-DASTB.       
          02 OPEN-BDSCOREHIS-DASTB     PIC 9(1).
          02 FECHA-DASTB.
             03 ANO-DASTB              PIC 9(4).
             03 MES-DASTB              PIC 9(2).
          02 SUMA-DASTB                PIC S9(8)V9(5) VALUE 0.
          02 SUMACUAD-DASTB            PIC S9(8)V9(5) VALUE 0.
          02 NUM-U-DASTB               PIC S9(8)V9(5) VALUE 0.
          02 NUM-D-DASTB               PIC S9(8)V9(5) VALUE 0.
          02 PROMEDIO-DASTB            PIC S9(8)V9(5) VALUE 0.
          02 DIF-ABS-DASTB             PIC 9(8)V9(5) VALUE 0.
          02 ULTIMO-DASTB              PIC S9(8)V9(5) VALUE 0.
          02 VARIANZA-DASTB            PIC S9(8)V9(5) VALUE 0.
          02 LI-DASTB                  PIC S9(8)V9(5) VALUE 0.
          02 LS-DASTB                  PIC S9(8)V9(5) VALUE 0.
          02 U-DASTB                   PIC S9(8)V9(5) VALUE 0.
          02 D-DASTB                   PIC S9(8)V9(5) VALUE 0.
          02 N-DASTB                   PIC S9(8)V9(5) VALUE 0.
          02 I-DASTB                   PIC S9(8)V9(5) VALUE 0.            
          02 J-DASTB                   PIC S9(8)V9(5) VALUE 0.            
          02 V-DIF-SCO-DASTB OCCURS 50 TIMES PIC S9(8)V9(5).

       01 PARAMETROS-EXTERNOS-DASTB.       
          02 TIP-IDE-INPUT-DASTB       PIC 9(01) VALUE 0.
          02 NUM-IDE-INPUT-DASTB       PIC 9(11) VALUE 0.
          02 PUN-SCO-INPUT-DASTB       PIC 9(03) VALUE 0.
          02 EXC-SCO-INPUT-DASTB       PIC 9(02) VALUE 0.
          02 TIP-SCO-INPUT-DASTB       PIC X(02) VALUE SPACES.
          02 REL-SCO-INPUT-DASTB       PIC 9(02) VALUE 0.
          02 IND-TEC-INPUT-DASTB       PIC 9(02) VALUE 0.
          02 V-PUN-SCO-INPUT-DASTB OCCURS 50 TIMES PIC 9(3).
          02 V-EXC-SCO-INPUT-DASTB OCCURS 50 TIMES PIC 9(3).
