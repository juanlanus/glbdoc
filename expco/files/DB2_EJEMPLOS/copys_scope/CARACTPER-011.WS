REMARK*CARACTPER-011.WS.v.0005.
      * IMS0002    | CQ7267 - SE AGREGAN LAS CARACTERISTICAS           *
      * 07-SEP-2012|          01END990END011 Y 01END989END011          *
      * M.SALAMANCA|                                                   *
      ******************************************************************
      * CAV0003    | CQ8961 - SE AGREGAN LAS CARACTERISTICAS           *
      * 01-DIC-2012|          02END999TO011 A 02END994TO011 Y          *
      * C.VELASCO. |          01END998RO011 A 01END985RO011            *
      ******************************************************************
      * IMS0004    | CQ9218 - SE AGREGAN LAS CARACTERISTICAS           *
      * 20-DIC-2012|          01NUM999RO011 Y 01NUM998RO011.           *
      * M.SALAMANCA|          REQ. 9218.                               *
      ******************************************************************
      * STT0005    | CQ10684 - SE AGREGAN LAS CARACTERISTICAS          *
      * 11-MAR-2013|          01END984RO011, 01NUM997MX011,            *
      * L.GOMEZ    |          01INQ999-011                             *
      ******************************************************************
        01 PERSONALIZACION-11.
           02  FECHA-AUX-11.
             05 MIL-AUX-11                PIC 9(02).
             05 ANO-AUX-11                PIC 9(02).
             05 MES-AUX-11                PIC 9(02).
             05 DIA-AUX-11                PIC 9(02).
           02  FECHA-AUX-R-11   REDEFINES FECHA-AUX-11   
                                          PIC 9(08).  
           02 VAR-AMBIENTE-CONT-11      PIC X(10) VALUE "FECHA_PER".
           02 FECHA-CONT-11             PIC X(08) VALUE SPACES.      
           02 SW-FECHA-VAR-AMBIENTE-11  PIC 9(01) VALUE ZEROS.
           02 SW-FECHA-RUN-INQ-11       PIC 9(08) VALUE ZEROS.
           02 SW-FECHA-CONS-INQ-11      PIC 9(08) VALUE ZEROS. 
           02 SW-DIAS-CONS-CARACT-30    PIC 9(10) VALUE ZEROS.
           02 PER-11-NIVEL-00-END-TMP.
              03 00END999VALUEHP0411    PIC 99999999999.
              03 00END998VALUEMX0311    PIC 99999999999.
              03 00END997VALUEMX0311    PIC 99999999999.
              03 00END996VALUEMX0311    PIC 99999999999.
              03 00END995VALUERO0111    PIC 99999999999.
              03 00END994VALUEHP0411    PIC 99999999999.
              03 00END993VALUEMX0311    PIC 99999999999.
              03 00END992VALUEMX0311    PIC 99999999999.
              03 00END991VALUEMX0311    PIC 99999999999.
CQ7267        03 00END990RO011          PIC 99999999999.                IMS0002
CQ7267        03 00END989RO011          PIC 99999999999.                IMS0002
CQ8961        03 00END999TO011          PIC 9(11).                      CAV0003
CQ8961        03 00END998TO011          PIC 9(11).                      CAV0003
CQ8961        03 00END997TO011          PIC 9(11).                      CAV0003
CQ8961        03 00END996TO011          PIC 9(11).                      CAV0003
CQ8961        03 00END995TO011          PIC 9(11).                      CAV0003
CQ8961        03 00END988RO011          PIC 9(11).                      CAV0003
CQ8961        03 00END987RO011          PIC 9(11).                      CAV0003
CQ8961        03 00END986RO011          PIC 9(11).                      CAV0003
CQ8961        03 00END985RO011          PIC 9(11).                      CAV0003
CQ8961        03 00END984RO04011        PIC 9(11).                      CAV0003
CQ8961     02 PER-11-NIVEL-00-END-TMP1.                                 CAV0003
CQ8961        03 00END994TO011          PIC 9(04).                      CAV0003
CQ8961        03 00ENDV999TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 00ENDV998TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 00ENDV997TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 00ENDV996TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 00ENDV995TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 02ENDV999TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 02ENDV998TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 02ENDV997TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 02ENDV996TO011         PIC 9(08)V99.                   CAV0003
CQ8961        03 02ENDV995TO011         PIC 9(08)V99.                   CAV0003
CQ9218     02 PER-11-NIVEL-00-NUM.                                      IMS0004
CQ9218        03 00NUM999RO011          PIC 9999.                       IMS0004
CQ9218        03 00NUM998RO011          PIC 9999.                       IMS0004
           02 PER-11-NIVEL-00-END-CONT.
              03 00END995RO01011-CONT   PIC 99999999999.
              03 00END995RO01011-SUM    PIC 99999999999.
           02 PER-11-NIVEL-01-END.
              03 99END999HP04011        PIC 99999999999.
              03 99END998MX03011        PIC 99999999999.
              03 99END997MX03011        PIC 99999999999.
              03 99END996MX03011        PIC 99999999999.
              03 99END995RO01011        PIC 99999999999.
              03 99END994HP04011        PIC 99999999999.
              03 99END993MX03011        PIC 99999999999.
              03 99END992MX03011        PIC 99999999999.
              03 99END991MX03011        PIC 99999999999.
CQ7267        03 01END990RO011          PIC 99999999999.                IMS0002
CQ7267        03 01END989RO011          PIC 99999999999.                IMS0002
CQ8961        03 02END999TO011          PIC 99999999.99.                CAV0003
CQ8961        03 02END998TO011          PIC 99999999.99.                CAV0003
CQ8961        03 02END997TO011          PIC 99999999.99.                CAV0003
CQ8961        03 02END996TO011          PIC 99999999.99.                CAV0003
CQ8961        03 02END995TO011          PIC 99999999.99.                CAV0003
CQ8961        03 01END988RO011          PIC 99999999999.                CAV0003
CQ8961        03 01END987RO011          PIC 99999999999.                CAV0003
CQ8961        03 01END986RO011          PIC 99999999999.                CAV0003
CQ8961        03 01END985RO011          PIC 99999999999.                CAV0003
C10684        03 01END984RO011          PIC 999999.9999.                STT0005
CQ8961     02 PER-11-NIVEL-02-END.                                      CAV0003
CQ8961        03 02END994TO011          PIC 9(04).                      CAV0003
           02 PER-11-NIVEL-00-MOR-TMP.
              03 01MORRO011             PIC 99999999999.
              03 01MORVE011             PIC 99999999999.
              03 01MORHP011             PIC 99999999999.
              03 01MORIN011             PIC 99999999999.
              03 01MORCO011             PIC 99999999999.
              03 01MORCC011             PIC 99999999999.
              03 01MOROT011             PIC 99999999999.
           02 PER-11-NIVEL-00-MOR.
              03 00MOR994RO011          PIC 99999999999.
              03 00MOR994VE011          PIC 99999999999.
              03 00MOR994HP011          PIC 99999999999.
              03 00MOR994IN011          PIC 99999999999.
              03 00MOR994CO011          PIC 99999999999.
              03 00MOR994CC011          PIC 99999999999.
              03 00MOR994OT011          PIC 99999999999.
              03 00MOR995RO011          PIC 99999999999.
              03 00MOR995VE011          PIC 99999999999.
              03 00MOR995HP011          PIC 99999999999.
              03 00MOR995IN011          PIC 99999999999.
              03 00MOR995CO011          PIC 99999999999.
              03 00MOR995CC011          PIC 99999999999.
              03 00MOR995OT011          PIC 99999999999.
              03 00MOR996RO011          PIC 99999999999.
              03 00MOR996VE011          PIC 99999999999.
              03 00MOR996HP011          PIC 99999999999.
              03 00MOR996IN011          PIC 99999999999.
              03 00MOR996CO011          PIC 99999999999.
              03 00MOR996CC011          PIC 99999999999.
              03 00MOR996OT011          PIC 99999999999.
              03 00MOR997RO011          PIC 99999999999.
              03 00MOR997VE011          PIC 99999999999.
              03 00MOR997HP011          PIC 99999999999.
              03 00MOR997IN011          PIC 99999999999.
              03 00MOR997CO011          PIC 99999999999.
              03 00MOR997CC011          PIC 99999999999.
              03 00MOR997OT011          PIC 99999999999.
              03 00MOR998RO011          PIC 99999999999.
              03 00MOR998VE011          PIC 99999999999.
              03 00MOR998HP011          PIC 99999999999.
              03 00MOR998IN011          PIC 99999999999.
              03 00MOR998CO011          PIC 99999999999.
              03 00MOR998CC011          PIC 99999999999.
              03 00MOR998OT011          PIC 99999999999.
              03 00MOR999RO011          PIC 99999999999.
              03 00MOR999VE011          PIC 99999999999.
              03 00MOR999HP011          PIC 99999999999.
              03 00MOR999IN011          PIC 99999999999.
              03 00MOR999CO011          PIC 99999999999.
              03 00MOR999CC011          PIC 99999999999.
              03 00MOR999OT011          PIC 99999999999.
           02 PER-11-NIVEL-01-MOR.
              03 01MOR994RO011          PIC 999.
              03 01MOR994VE011          PIC 999.
              03 01MOR994HP011          PIC 999.
              03 01MOR994IN011          PIC 999.
              03 01MOR994CO011          PIC 999.
              03 01MOR994CC011          PIC 999.
              03 01MOR994OT011          PIC 999.
              03 01MOR995RO011          PIC 999.
              03 01MOR995VE011          PIC 999.
              03 01MOR995HP011          PIC 999.
              03 01MOR995IN011          PIC 999.
              03 01MOR995CO011          PIC 999.
              03 01MOR995CC011          PIC 999.
              03 01MOR995OT011          PIC 999.
              03 01MOR996RO011          PIC 999.
              03 01MOR996VE011          PIC 999.
              03 01MOR996HP011          PIC 999.
              03 01MOR996IN011          PIC 999.
              03 01MOR996CO011          PIC 999.
              03 01MOR996CC011          PIC 999.
              03 01MOR996OT011          PIC 999.
              03 01MOR997RO011          PIC 999.
              03 01MOR997VE011          PIC 999.
              03 01MOR997HP011          PIC 999.
              03 01MOR997IN011          PIC 999.
              03 01MOR997CO011          PIC 999.
              03 01MOR997CC011          PIC 999.
              03 01MOR997OT011          PIC 999.
              03 01MOR998RO011          PIC 999.
              03 01MOR998VE011          PIC 999.
              03 01MOR998HP011          PIC 999.
              03 01MOR998IN011          PIC 999.
              03 01MOR998CO011          PIC 999.
              03 01MOR998CC011          PIC 999.
              03 01MOR998OT011          PIC 999.
              03 01MOR999RO011          PIC 999.
              03 01MOR999VE011          PIC 999.
              03 01MOR999HP011          PIC 999.
              03 01MOR999IN011          PIC 999.
              03 01MOR999CO011          PIC 999.
              03 01MOR999CC011          PIC 999.
              03 01MOR999OT011          PIC 999.
CQ9218     02 PER-11-NIVEL-01-NUM.                                      IMS0004
CQ9218        03 01NUM999RO011          PIC 9999.                       IMS0004
CQ9218        03 01NUM998RO011          PIC 9999.                       IMS0004
C10684     02 PER-11-NIVEL-00-NUM-2.                                    STT0005
C10684        03 00NUM997MX011          PIC 999.                        STT0005
C10684     02 PER-11-NIVEL-01-NUM-2.                                    STT0005
C10684        03 01NUM997MX011          PIC 999.                        STT0005
           02 PER-11-NIVEL-01-INQ.
              03 01INQ999011           PIC 9999.
C10684        03 01INQ999-011          PIC 999.                         STT0005
           02 PER-11-SUSCRIP-BANCOL     PIC 999999 VALUE ZEROS.
              88 PER-11-BANCOL          VALUE 010232 010253 020003 
                                       020053 020153 020203 020232 
                                       020233 020234 020253 020353 
                                       020453 020553 020953 050003 
                                       050032 050053 050103 050203 
                                       050232 050253 050303 050353 
                                       050403 050553 050603 050803 
                                       050903 050953 051103 051203 
                                       051403 051703 059703 059803 
                                       059903 059953 080253 130003 
                                       130053 130103 130203 130253 
                                       130303 130503 130603 130703 
                                       130753 130803 130953 131153 
                                       131203 131303 131403 131903 
                                       133403 139503 139603 139703 
                                       139803 139903 180280 250003
CQ9218                                 030013 050075 140011 180033      IMS0004
CQ9218                                 700203 700211 709903             IMS0004
                                       250203 500103 510232 600232.
C10684     02 PER-11-NIVEL-01-END-T.                                    STT0005
C10684        03 I-011                 PIC 999.                         STT0005
C10684        03 I-011-1               PIC 999.                         STT0005
C10684        03 CUPO-984-011  OCCURS 6        PIC 9(11).               STT0005
C10684        03 SALDO-984-011  OCCURS 6       PIC 9(11).               STT0005
C10684        03 UTMES-R4-011   OCCURS 6       PIC 9(06)V9(04).         STT0005
C10684        03 SUMUT-R4-011           PIC 9(06)V9(04).                STT0005
C10684        03 TOTUT-R4-011           PIC 9(06).                      STT0005
C10684        03 ROFFLAG-011            PIC 9(02).                      STT0005
