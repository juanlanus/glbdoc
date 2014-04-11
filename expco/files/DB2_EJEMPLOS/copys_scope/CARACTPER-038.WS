REMARK*CARACTPER-038.WS.v.0001.
REMARK******************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA 038            *
REMARK******************************************************************
        01 PERSONALIZACION-38. 
           02  WS-NIT-SUSCRIPTOR                  PIC 9(11).
               88  COPROCENVA    VALUE                891900492.
           02  WS-INICIO-38                       PIC 9(02).
           02  WS-FIN-38                          PIC 9(02).
           02  WS-NN-38                           PIC 9(02).
        01 PER-38-NIVEL-02-NUM.
           02  02NUM999TO038                      PIC 9(03).
        01 PER-38-NIVEL-02-END.
           02  02END999TO038                      PIC 9(11).
           02  02END998TO038                      PIC 9(11).
           02  02END997TO038                      PIC 9(11).
        01 PER-38-NIVEL-02-MOR.
           02  02MOR999TO038                      PIC 9(03).
           02  02MOR998TO038                      PIC 9(03).
           02  02MOR997TO038                      PIC 9(03).
           02  02MOR996TO038                      PIC 9(03).
           02  02MOR995TO038                      PIC 9(03).
           02  02MOR994TO038                      PIC 9(03).
           02  02MOR993TO038                      PIC 9(03).
           02  02MOR992TO038                      PIC 9(03).
           02  02MOR991TO038                      PIC 9(03).
           02  02MOR990TO038                      PIC 9(03).
           02  02MOR989TO038                      PIC 9(03).
        01 PER-38-NIVEL-00-NUM.
           02  00NUM999TO038                      PIC 9(03).
        01 PER-38-NIVEL-00-END.
           02  00END999TO038                      PIC 9(11).
           02  00END998TO038                      PIC 9(11).
           02  00END997TO038                      PIC 9(11).
        01 PER-38-NIVEL-00-MOR.
           02 PER-38-NIVEL-00-MOR-1.
              03  00MOR999TO038                   PIC 9(03).
              03  00MOR998TO038                   PIC 9(03).
              03  00MOR997TO038                   PIC 9(03).
              03  00MOR996TO038                   PIC 9(03).
           02 PER-38-NIVEL-00-MOR-2.
              03  00MOR995TO038                   PIC 9(03).
              03  00MOR994TO038                   PIC 9(03).
              03  00MOR993TO038                   PIC 9(03).
              03  00MOR992TO038                   PIC 9(03).
           02 PER-38-NIVEL-00-MOR-3.
              03  00MOR991TO038                   PIC 9(03).
              03  00MOR990TO038                   PIC 9(03).
              03  00MOR989TO038                   PIC 9(03).
