REMARK*CARACTPER-015.WS.v.0001.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA LAGOBO
REMARK**********************************************************************
        01 PERSONALIZACION-15.
           02 PER-15-NIVEL-02-NUM.
              03 02NUM999TO015          PIC 9(3).
R12426        03 02NUM998TO015          PIC 9(3).
R12426        03 02NUM997TO015          PIC 9(3).
           02 PER-15-NIVEL-02-END.
              03 02END999TO015          PIC 9(11).
              03 02END998TO015          PIC 9(11).
           02 PER-15-NIVEL-02-ENDV.
              03 02ENDV999TO015         PIC 9(11).
              03 02ENDV998TO015         PIC 9(11).
           02 PER-15-NIVEL-01-MOR.
              03 01MOR999HP015          PIC 9(3).
              03 01MOR999IN015          PIC 9(3).
              03 01MOR999OT015          PIC 9(3).
              03 01MOR999RO015          PIC 9(3).
              03 01MOR999VE015          PIC 9(3).
           02 PER-15-NIVEL-02-MOR.
              03 02MOR998TO015          PIC 9(3).
              03 02MOR997TO015          PIC 9(3).
              03 02MOR996TO015          PIC 9(3).
              03 02MOR995TO015          PIC 9(3).
           02 PER-15-NIVEL-02-MORAUX.
              03 02MOR998TOAUX          PIC 9(3).
              03 02MOR997TOAUX          PIC 9(3).
              03 02MOR996TOAUX          PIC 9(3).
              03 02MOR995TOAUX          PIC 9(3).
           02 PER-15-NIVEL-00-END-TMP.
              03 00ENDCUOTA015          PIC 9(11).
              03 00ENDCUPOS015          PIC 9(11).
              03 00ENDSALDO015          PIC 9(11).
           02 PER-15-NIVEL-00-ENDV-TMP.
              03 00ENDVCUOTA015         PIC 9(11).
              03 00ENDVCUPOS015         PIC 9(11).
              03 00ENDVSALDO015         PIC 9(11).
           02 PER-15-VARIABLES-ADD.
              03 W-F-MMAA-PER15         PIC 9(04).
              03 W-FECHA-AUX-PER15      PIC  9(06)  VALUE  ZEROS.
              03                      REDEFINES  W-FECHA-AUX-PER15.
                05  W-MES-AUX-PER15     PIC  9(02).
                05  W-ANO-AUX-PER15     PIC  9(04).
              03 W-MESES-NOV-PER15      PIC  S9(08).
              
              
