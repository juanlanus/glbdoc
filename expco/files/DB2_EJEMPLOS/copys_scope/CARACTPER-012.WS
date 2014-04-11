REMARK*CARACTPER-012.WS.v.0004.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA LA POLAR
REMARK**********************************************************************
      * IMS0002    | CQ6119 - SE AGREGAN 4 NUEVAS CARACTERISTICAS      *
      * 29-MAY-2012|    REQ. CQ. 6119.                                 *
      * M.SALAMANCA|                                                   *
      ******************************************************************
      * IMS0003    | CQ6440 - SE AGREGAN 2 NUEVAS CARACTERISTICAS      *
      * 25-JUN-2012|    REQ. CQ. 6440.                                 *
      * M.SALAMANCA|                                                   *
      ******************************************************************
      * IMS0004    | CQ6629 - SE AGREGA 1 NUEVA CARACTERISTICA         *
      * 09-JUL-2012|    REQ. CQ. 6629.                                 *
      * M.SALAMANCA|                                                   *
      ******************************************************************
        01 PERSONALIZACION-12.
           02 PER-12-NIVEL-00-NUM.
              03 00NUM999TO012          PIC 9(4).
              03 00NUM998TO012          PIC 9(4).
              03 00NUM997RO012          PIC 9(4).
              03 00NUM996HP012          PIC 9(4).
CQ6119        03 00NUM995RO012          PIC 9(4).                       IMS0002
CQ6119        03 00NUM994MX012          PIC 9(4).                       IMS0002
              
           02 PER-12-NIVEL-00-END.
              03 00END999RO012          PIC 9(11).
              03 00END998TO012          PIC 9(11).
CQ6119        03 00END997RO012          PIC 9(11).                      IMS0002
CQ6440        03 00END995RO012          PIC 9(11).                      IMS0003
CQ6440        03 00END996RO012          PIC 9(11).                      IMS0003

CQ6629     02  PER-12-NIVEL-00-END-CNT.                                 IMS0004
CQ6629        03 00END994RO012          PIC 9(04).                      IMS0004

           02 PER-12-NIVEL-00-END-TMP.
              03 00ENDMAXCUP012         PIC 9(11).
              03 00ENDSUMCUP012         PIC 9(11).
              03 00ENDNUMCUP012         PIC 9(11).
CQ6119        03 00ENDSUMCUP997RO012    PIC 9(11).                      IMS0002
CQ6119        03 00ENDSUMSLD997RO012    PIC 9(11).                      IMS0002
CQ6629        03 00ENDTOTUTL994RO012    PIC 9(6)V9(4).                  IMS0004
CQ6629        03 01ENDSUMCUP994RO012    PIC 9(11).                      IMS0004
CQ6629        03 01ENDSUMSLD994RO012    PIC 9(11).                      IMS0004
           02 PER-12-NIVEL-00-EXP.
              03 00EXP999RO012          PIC 9(4).
              03 00EXP998IN012          PIC 9(4).
           02 PER-12-NIVEL-00-MOR.
              03 00MOR999MX012          PIC 9(4).
              03 00MOR998MX012          PIC 9(4).
              03 00MOR997MX012          PIC 9(4).
              03 00MOR996TO012          PIC 9(4).
CQ6119        03 00MOR995TO012          PIC 9(4).                       IMS0002

           02 PER-12-NIVEL-01-END-TMP.
              03 01ENDMAXCUP012         PIC 9(11).
              03 01ENDSUMCUP012         PIC 9(11).
              03 01ENDNUMCUP012         PIC 9(11).
              03 01ENDPRMCUP012         PIC 9(11).
CQ6119        03 01ENDSUMCUP997RO012    PIC 9(11).                      IMS0002
CQ6119        03 01ENDSUMSLD997RO012    PIC 9(11).                      IMS0002
CQ6119        03 00END997CUP12          PIC 9(11).                      IMS0002
CQ6119        03 00END997SAL12          PIC 9(11).                      IMS0002
CQ6440        03 00END995SAL12          PIC 9(11).                      IMS0003
CQ6440        03 00END996CUP12          PIC 9(11).                      IMS0003

           02 PER-12-NIVEL-02-NUM.
              03 02NUM999TO012          PIC 9(4).
              03 02NUM998TO012          PIC 9(4).
              03 01NUM997RO012          PIC 9(4).
              03 01NUM996HP012          PIC 9(4).
CQ6119        03 01NUM995RO012          PIC 9(4).                       IMS0002
CQ6119        03 01NUM994MX012          PIC 9(4).                       IMS0002
           02 PER-12-NIVEL-02-END.
              03 01END999RO012          PIC 9(11).
              03 02END998TO012          PIC 9(11).
              
CQ6119        03 01END997RO012          PIC 999999.9999.                IMS0002
CQ6440        03 01END995RO012          PIC 99999999.99.                IMS0003
CQ6440        03 01END996RO012          PIC 99999999.99.                IMS0003

CQ6629     02 PER-12-NIVEL-01-END-CNT.                                  IMS0004
CQ6629        03 01END994RO012          PIC 9(4).                       IMS0004

           02 PER-12-NIVEL-02-EXP.
              03 01EXP999RO012          PIC 9(4).
              03 01EXP998IN012          PIC 9(4).
           02 PER-12-NIVEL-02-MOR.
              03 01MOR999MX012          PIC 9(4).
              03 01MOR998MX012          PIC 9(4).
              03 01MOR997MX012          PIC 9(4).
              03 02MOR996TO012          PIC 9(4).
CQ6119        03 02MOR995TO012          PIC 9(4).                       IMS0002
           02 PER-12-NIVEL-02-INQ.   
              03 01INQ999IQ012          PIC 9(4).              
