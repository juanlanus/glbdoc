REMARK*CARACTPER-006.WS-V005.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA BCSC
REMARK**********************************************************************
REMARK* EHA0002     * RQ4022 - SE INCLUYEN VARIBLES POR DEMANDA        *
REMARK* DIC/2011    *          REQ 4022.                               *
REMARK******************************************************************
REMARK* CAV0003     * RQ5522 - SE INCLUYEN CARACTERISTICAS TIPOLOGIA   *
REMARK* ABR/2012    *          EXP NIVEL 02.                           *
REMARK******************************************************************
REMARK* CAV0004     * RQ6505 - SE INCLUYEN LA CARACTERISTICA TIPOLOGIA *
REMARK* JUN/2012    *          END 02END989FI006.                      *
REMARK******************************************************************
REMARK* IMS0005     * CQ6726 - SE INCLUYEN LA CARACTERISTICA TIPOLOGIA *
REMARK* AGO/2012    *          END 02END988HP006.                      *
REMARK******************************************************************
        01 PERSONALIZACION-06.
           02 PER-06-NIVEL-00-MOR.
              03 00MOR999FI006          PIC 9(4).
              03 00MOR998FI006          PIC 9(4).
              03 00MOR997FI006          PIC 9(4).
              03 00MOR996FI006          PIC 9(4).
              03 00MOR995CC006          PIC 9(4).
              03 00MOR994CC006          PIC 9(4).
              03 00MOR993CC006          PIC 9(4).
              03 00MOR992CC006          PIC 9(4).
              03 00MOR991OT006          PIC 9(4).
              03 00MOR990OT006          PIC 9(4).
              03 00MOR989OT006          PIC 9(4).
              03 00MOR988OT006          PIC 9(4).
              03 00MOR987CO006          PIC 9(4).
              03 00MOR986CO006          PIC 9(4).
              03 00MOR985CO006          PIC 9(4).
              03 00MOR984CO006          PIC 9(4).
           02 PER-06-NIVEL-00-MOR-TMP.
              03 00MOR109006            PIC 9(4).
              03 00MOR110006            PIC 9(4).
              03 00MOR111006            PIC 9(4).
              03 00MOR112006            PIC 9(4).
           02 PER-06-NIVEL-00-END.
              03 00END999FI006          PIC 99999999.99.
              03 00END998TC006          PIC 99999999.99.
              03 00END997RO006          PIC 99999999.99.
              03 00END996HP006          PIC 99999999.99.
              03 00END995IN006          PIC 99999999.99.
              03 00END994TC006          PIC 99999999.99.
              03 00END993TC006          PIC 99999999.99.
              03 00END992TC006          PIC 99999999.99.
              03 00END991TC006          PIC 99999999.99.
              03 00END990TC006          PIC 99999999.99.
RQ6505        03 00END989FI006          PIC 99999999.99.                CAV0004
           02 PER-06-NIVEL-00-END-TMP.
              03 00ENDCUOTA006          PIC 99999999.99.
              03 00ENDCUPOS006          PIC 99999999.99.
              03 00ENDSALDO006          PIC 99999999.99.
           02 PER-06-NIVEL-00-ENDV-TMP.
              03 00ENDVCUOTA006         PIC 9(08)V99.
              03 00ENDVCUPOS006         PIC 9(08)V99.
              03 00ENDVSALDO006         PIC 9(08)V99.
RQ5522     02 PER-06-NIVEL-00-EXP.                                      CAV0003
RQ5522        03 00EXP999TO006          PIC 9(4).                       CAV0003
RQ5522        03 00EXP998TO006          PIC 9(4).                       CAV0003
RQ5522        03 00EXP997TO006          PIC 9(4).                       CAV0003
RQ5522        03 00EXP996TO006          PIC 9(4).                       CAV0003
           02 PER-06-NIVEL-02-MOR.
              03 02MOR999FI006          PIC 9(4).
              03 02MOR998FI006          PIC 9(4).
              03 02MOR997FI006          PIC 9(4).
              03 02MOR996FI006          PIC 9(4).
              03 02MOR995CC006          PIC 9(4).
              03 02MOR994CC006          PIC 9(4).
              03 02MOR993CC006          PIC 9(4).
              03 02MOR992CC006          PIC 9(4).
              03 02MOR991OT006          PIC 9(4).
              03 02MOR990OT006          PIC 9(4).
              03 02MOR989OT006          PIC 9(4).
              03 02MOR988OT006          PIC 9(4).
              03 02MOR987CO006          PIC 9(4).
              03 02MOR986CO006          PIC 9(4).
              03 02MOR985CO006          PIC 9(4).
              03 02MOR984CO006          PIC 9(4).
           02 PER-06-NIVEL-02-END.
              03 02END999FI006          PIC 99999999.99.
              03 02END998TC006          PIC 99999999.99.
              03 02END997RO006          PIC 99999999.99.
              03 02END996HP006          PIC 99999999.99.
              03 02END995IN006          PIC 99999999.99.
              03 02END994TC006          PIC 99999999.99.
              03 02END993TC006          PIC 99999999.99.
              03 02END992TC006          PIC 99999999.99.
              03 02END991TC006          PIC 99999999.99.
              03 02END990TC006          PIC 99999999.99.
RQ6505        03 02END989FI006          PIC 99999999.99.                CAV0004
CQ6726        03 02END988HP006          PIC 99999999.99.                IMS0005
           02 PER-06-NIVEL-02-ENDV.
              03 02ENDV999FI006         PIC 9(08)V99.
              03 02ENDV998TC006         PIC 9(08)V99.
              03 02ENDV997RO006         PIC 9(08)V99.
              03 02ENDV996HP006         PIC 9(08)V99.
              03 02ENDV995IN006         PIC 9(08)V99.
              03 02ENDV994TC006         PIC 9(08)V99.
              03 02ENDV993TC006         PIC 9(08)V99.
              03 02ENDV992TC006         PIC 9(08)V99.
              03 02ENDV991TC006         PIC 9(08)V99.
              03 02ENDV990TC006         PIC 9(08)V99.
RQ6505        03 02ENDV989FI006         PIC 9(08)V99.                   CAV0004
RQ5522     02 PER-06-NIVEL-02-EXP.                                      CAV0003
RQ5522        03 02EXP999TO006          PIC 9(4).                       CAV0003
RQ5522        03 02EXP998TO006          PIC 9(4).                       CAV0003
RQ5522        03 02EXP997TO006          PIC 9(4).                       CAV0003
RQ5522        03 02EXP996TO006          PIC 9(4).                       CAV0003
           02 PER-06-NODESEADOS-BCSC.
              03 NODESEADOSBCSC         PIC 9(01).
RQ4022     02 PER-06-DEMANDA.                                           EHA0002
RQ4022        03 MONTO-OTORGAR          PIC 9(11).                      EHA0002
RQ4022        03 PLAZO                  PIC 9(03).                      EHA0002
RQ4022        03 TASA-VIVIENDA          PIC 9(11).                      EHA0002
RQ4022        03 NROCONSDECISOR         PIC 9(11).                      EHA0002
RQ4022        03 RES-ANTERIOR           PIC 9(01).                      EHA0002
RQ4022        03 FEC-RESPUESTA          PIC 9(08).                      EHA0002
RQ4022        03 SIS-AMORT              PIC X(11).                      EHA0002
RQ4022        03 PERFIL-OTORGAMIENTO    PIC X(10).                      EHA0002
RQ4022        03 DECISION-ANTERIOR      PIC X(10).                      EHA0002
RQ4022     02 V-COMP-DEMANDA-PER06 OCCURS 10000.                        EHA0002
RQ4022        03 VAR-COMP-DEMANDA-PER06 PIC X(50).                      EHA0002
RQ4022     02 PER-06-OTROS.                                             EHA0002
RQ4022        03 FEC-RESPUESTA-AUX      PIC 9(08).                      EHA0002
RQ4022        03 CONSULT-ID-AUX         PIC 9(08).                      EHA0002
RQ4022        03 CONSULT-ID-ACT         PIC 9(08).                      EHA0002
RQ6260        03 WS-MAX-ID-PER          PIC 9(08).                      IMS0004
