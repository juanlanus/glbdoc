REMARK*CARACTPER-018.WS-V002.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA FINTRAVALORES
REMARK**********************************************************************
      * IMS0002    | CQ6439 - SE AGREGAN 5 NUEVAS CARACTERISTICAS      *
      * 25-JUN-2012|    REQ. CQ. 6439.                                 *
      * M.SALAMANCA|                                                   *
      ******************************************************************
        01 PERSONALIZACION-18.
           02 PER-18-NIVEL-00-NUM.
              03 00NUM999TO018          PIC 9(4).
              03 00NUM998TO018          PIC 9(4).
CQ6439        03 00NUM997CO018          PIC 9(4).                       IMS0002
           02 PER-18-NIVEL-00-MOR.
              03 00MOR999TO018          PIC 9(4).
              03 00MOR998TO018          PIC 9(4).
              03 00MOR997TO018          PIC 9(4).
              03 00MOR996TO018          PIC 9(4).
CQ6439        03 00MOR995CO018          PIC 9(4).                       IMS0002
CQ6439        03 00MOR994CO018          PIC 9(4).                       IMS0002
CQ6439        03 00MOR993CO018          PIC 9(4).                       IMS0002
CQ6439        03 00MOR992CO018          PIC 9(4).                       IMS0002
           02 PER-18-NIVEL-02-NUM.
              03 02NUM999TO018          PIC 9(4).
              03 02NUM998TO018          PIC 9(4).
CQ6439        03 01NUM997CO018          PIC 9(4).                       IMS0002
           02 PER-18-NIVEL-02-MOR.
              03 02MOR999TO018          PIC 9(4).
              03 02MOR998TO018          PIC 9(4).
              03 02MOR997TO018          PIC 9(4).
              03 02MOR996TO018          PIC 9(4).
CQ6439        03 01MOR995CO018          PIC 9(4).                       IMS0002
CQ6439        03 01MOR994CO018          PIC 9(4).                       IMS0002
CQ6439        03 01MOR993CO018          PIC 9(4).                       IMS0002
CQ6439        03 01MOR992CO018          PIC 9(4).                       IMS0002
