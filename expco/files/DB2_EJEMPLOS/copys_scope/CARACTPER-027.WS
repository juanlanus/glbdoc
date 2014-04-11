REMARK*CARACTPER-027.WS.v.0002.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA GMAC     
REMARK**********************************************************************
      * CAV0002    | R10122 - GMAC  CQ-10122                           *
      * 07-FEB-2013|          INCLUSION NUEVSA CARACTERISTICAS         *
      * C.VELASCO  |          01NUM998 y 02MOR992                      *
      ******************************************************************
        01 PERSONALIZACION-27.
           02 PER-27-NIVEL-00-NUM.
              03 00NUM999TO027          PIC 9(9).

R10122     02 PER-27-NIVEL-00NUM4.                                      CAV0002
R10122        03 00NUM998RO027          PIC 9(4).                       CAV0002
R10122        03 00NUM998IN027          PIC 9(4).                       CAV0002
R10122        03 00NUM998VE027          PIC 9(4).                       CAV0002
R10122        03 00NUM998HP027          PIC 9(4).                       CAV0002
R10122        03 00NUM998CC027          PIC 9(4).                       CAV0002
R10122        03 00NUM998OT027          PIC 9(4).                       CAV0002
              
           02 PER-27-NIVEL-00-END.
              03 00END999RO027          PIC 9(9).
              03 00END999TO027          PIC 9(9).
              03 00END998TO027          PIC 9(9).
           02 PER-27-NIVEL-00-END1.
              03 00END997TO027          PIC 9(4).

           02 PER-27-NIVEL-00-EXP.
              03 00EXP999TO027          PIC 9(4).
              03 00EXP998TO027          PIC 9(4).

           02 PER-27-NIVEL-00-MOR.
              03 00MOR998TO027          PIC 9(4).
              03 00MOR997TO027          PIC 9(4).
              03 00MOR996TO027          PIC 9(4).
              03 00MOR995TO027          PIC 9(4).
              03 00MOR994TO027          PIC 9(4).
              03 00MOR993TO027          PIC 9(4).
R10122        03 00MOR992TO027          PIC 9(4).                       CAV0002
           02 PER-27-NIVEL-00-MOR1.
              03 00MOR999TO027          PIC 9(9).

           02 PER-27-NIVEL-00-INQ.   
              03 00INQ999TO027          PIC 9(4).              
              03 00INQ998TO027          PIC 9(4).              

           02 PER-27-NIVEL-02-NUM.
              03 02NUM999TO027          PIC 99.999999.

           02 PER-27-NUM-TMP.
              03 00NUMTOTCTA027         PIC 9(9).
              03 00NUM24MCTA027         PIC 9(9).

R10122     02 PER-27-NIVEL-01NUM4.                                      CAV0002
R10122        03 01NUM998RO027          PIC 9(4).                       CAV0002
R10122        03 01NUM998IN027          PIC 9(4).                       CAV0002
R10122        03 01NUM998VE027          PIC 9(4).                       CAV0002
R10122        03 01NUM998HP027          PIC 9(4).                       CAV0002
R10122        03 01NUM998CC027          PIC 9(4).                       CAV0002
R10122        03 01NUM998OT027          PIC 9(4).                       CAV0002

           02 PER-27-NIVEL-02-END.
              03 01END999RO027          PIC 99.999999.
              03 02END999TO027          PIC 99.999999.
              03 02END998TO027          PIC 99.999999.
              03 02END997TO027          PIC 9(04).

           02 PER-27-NIVEL-02-EXP.
              03 02EXP999TO027          PIC 9(4).
              03 02EXP998TO027          PIC 9(4).

           02 PER-27-NIVEL-02-MOR.
              03 02MOR998TO027          PIC 9(4).
              03 02MOR997TO027          PIC 9(4).
              03 02MOR996TO027          PIC 9(4).
              03 02MOR995TO027          PIC 9(4).
              03 02MOR994TO027          PIC 9(4).
              03 02MOR993TO027          PIC 9(4).
R10122        03 02MOR992TO027          PIC 9(4).                       CAV0002
           02 PER-27-NIVEL-02-MOR1.
              03 02MOR999TO027          PIC 99.999999.

           02 PER-27-NIVEL-01-INQ.
              03 01INQ999TO027          PIC 9(4).
              03 01INQ998TO027          PIC 9(4).

           02 PER-27-END-TMP.
              03 TOT-END-UTI-027        PIC 9(03)V999999.
              03 TOT-END-CTN-027        PIC 9(11).
              03 TOT-END-SLD-027        PIC 9(11).
              03 TOT-END-CUP-027        PIC 9(11).
              03 SLD-END-ALT-027        PIC 9(11).
              03 CUP-END-ALT-027        PIC 9(11).

           02 PER-27-MOR-TMP.
              03 TOT-MOR-CTA-027        PIC 9(11).
              03 NUM-MOR-CTA-027        PIC 9(11).
              03 DIA-MOR-CTA-027        PIC 9(11).

R10122     02 PER-27-00MOR-TMP.                                         CAV0002
R10122        03 00MOR992TMP027         PIC 9(01).                      CAV0002

           02 PER-27-INQ-TMP.
              03 TABS-NIT-INQ-027.
                 05 TABLS-NIT-INQ-027 OCCURS 50 TIMES PIC 9(11).
              03 TAB-NIT-INQ-027.
                 05 TABL-NIT-INQ-027  OCCURS 50 TIMES PIC 9(11).
