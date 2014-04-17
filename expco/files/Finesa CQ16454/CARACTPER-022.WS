REMARK*CARACTPER-022.WS.v.0005.
REMARK******************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA FINESA         *
REMARK******************************************************************
REMARK* IMS0001     | CQ6485 - SE CREA LA PERSONALIZACION 22           *
REMARK* JUL.12.2012 |          PARA FINESA                             *
REMARK* M.SALAMANCA |                                                  *
REMARK******************************************************************
REMARK* IMS0002     | CQ9579 - SE REALIZA AJUSTE A LAS CARACTERISTICAS *
REMARK* 12/01/2013  |          END998 Y END999.                        *
REMARK* M.SALAMANCA |                                                  *
REMARK******************************************************************
REMARK* STT0003     | R12048 - SE REALIZA AJUSTE A LAS CARACTERISTICAS *
REMARK* 14/05/2013  |          END MOR Y INQ  CQ12048                  *
REMARK* L.GOMEZ     |                                                  *
REMARK******************************************************************
REMARK* IMS0004     | R16454 - SE AGREGAN CARACTERISTICAS MOR998,      *
REMARK* 21/11/2013  |          MOR997 Y MOR996.                        *
REMARK* M.SALAMANCA |                                                  *
REMARK******************************************************************
REMARK* STT0005    | R17723 - CARACTERISTICAS COMPRA CARTERA BANBOGOTA *
REMARK* 24/02/2014 |          CQ-17723                                 *
REMARK* LUIS GOMEZ |                                                   *
REMARK******************************************************************
        01 PERSONALIZACION-22.
              
           02 PER-22-NIVEL-00-END.
              03 00END999TO022                 PIC 99999999.99.
              03 00END998RO022                 PIC 99999999.99.
R12048        03 00END997TO022                 PIC 99999999.99.         STT0003
R17723        03 00END996TO022                 PIC 99999999.99.         STT0005
R17723        03 00END995TO022                 PIC 99999999.99.         STT0005
R17723        03 00END994TO022                 PIC 99999999.99.         STT0005
R17723        03 00END993TO022                 PIC 99999999.99.         STT0005
R17723        03 00END992TO022                 PIC 99999999.99.         STT0005
R17723        03 00END991TO022                 PIC 99999999.99.         STT0005
              
           02 PER-22-NIVEL-00-INQ.
              03 00INQ999_022                  PIC 9(04).
              03 00INQ998_022                  PIC 9(06).
              03 COD-SUSCRIP-TMP               PIC 9(02).
              
           02 PER-22-NIVEL-00-MOR.
              03 00MOR999RO022                 PIC 9(04).
              03 00MOR998TO022                 PIC 9(04).
              03 00MOR997TO022                 PIC 9(04).
              03 00MOR996TO022                 PIC 9(04).
              03 00MOR995TO022                 PIC 9(04).
R16454        03 00MOR998MX022                 PIC 9(04).               IMS0004
R16454        03 00MOR997MX022                 PIC 9(04).               IMS0004
R16454        03 00MOR996MX022                 PIC 9(04).               IMS0004
              
           02 PER-22-NIVEL-00-END-TMP.
              03 ENDPLAZO022                   PIC 9(04).
CQ9579        03 00END999CUO22                 PIC 99999999.99.         IMS0002
CQ9579        03 00END999REL22                 PIC 99999999.99.         IMS0002
CQ9579        03 00END998CUO22                 PIC 99999999.99.         IMS0002
CQ9579        03 00END998REL22                 PIC 99999999.99.         IMS0002
R12048        03 00END997CUO22                 PIC 99999999.99.         STT0003
R12048        03 00END997REL22                 PIC 99999999.99.         STT0003
              
           02 PER-22-NIVEL-01-INQ-TMP.
              03 INQ-I                         PIC 9.
              03 INQ998HUELLA                  PIC 9(04).
              
           02 PER-22-01INQ998-TAB  OCCURS  7  TIMES.
              03 INQ-HUELLA                    PIC 9(04).
              03 INQ-FECHA                     PIC 9(06).
              
           02 PER-22-NIVEL-01-DEM.
              03 01DEM999_022                  PIC 9(04).
              03 01DEM998_022                  PIC 9(02).
              
           02 PER-22-NIVEL-01-END.
              03 02END999TO022                 PIC 99999999.99.
              03 01END998RO022                 PIC 99999999.99.
R12048        03 02END997TO022                 PIC 99999999.99.
R17723        03 02END996TO022                 PIC 99999999.99.         STT0005
R17723        03 02END995TO022                 PIC 99999999.99.         STT0005
R17723        03 02END994TO022                 PIC 99999999.99.         STT0005
R17723        03 02END993TO022                 PIC 99999999.99.         STT0005
R17723        03 02END992TO022                 PIC 99999999.99.         STT0005
R17723        03 02END991TO022                 PIC 99999999.99.         STT0005
              
           02 PER-22-NIVEL-01-INQ.
              03 PER-22-NIVEL-01-INQ-04.
                 05 01INQ999_022               PIC 9(04).
              03 PER-22-NIVEL-01-INQ-06.
                 05 01INQ998_022               PIC 9(06).

           02 PER-22-NIVEL-01-MOR.
              03 01MOR999RO022                 PIC 9(04).
              03 02MOR998TO022                 PIC 9(04).
              03 02MOR997TO022                 PIC 9(04).
              03 02MOR996TO022                 PIC 9(04).
              03 02MOR995TO022                 PIC 9(04).
R16454        03 01MOR998MX022                 PIC 9(04).               IMS0004
R16454        03 01MOR997MX022                 PIC 9(04).               IMS0004
R16454        03 01MOR996MX022                 PIC 9(04).               IMS0004

R12048   02 PER-22-VARIABLES.                                           STT0003
R12048        03  WS-CODSUS-22                 PIC 9(06).               STT0003
R12048        88  CLIENTE-FINESA VALUE 020026,029742,050007,050013,     STT0003
R12048                                 050051,050113,050213,050313,     STT0003
R12048                                 050613,059942,110003,110031,     STT0003
R12048                                 110051,110083,110092,110126,     STT0003
R12048                                 110142,110187,110192,110292,     STT0003
R12048                                 110803,110826,111303,111703,     STT0003
R12048                                 112003,115503,116503,117503,     STT0003
R12048                                 119703,280009,280018,280022,     STT0003
R12048                                 280046,280048,280051,280052,     STT0003
R12048                                 280055,280089,280097,280098,     STT0003
R12048                                 280146,280188,280189,280197,     STT0003
R12048                                 280198,280222,280246,280297,     STT0003
R12048                                 280298,280346,280397,280398,     STT0003
R12048                                 280622,280646,280649,280697,     STT0003
R12048                                 280698,280997,281197,281397,     STT0003
R12048                                 281398,281726,283222,285597,     STT0003
R12048                                 289948,310010,310011,310014,     STT0003
R12048                                 310016,310019,310020,310024,     STT0003
R12048                                 310026,310047,310061,380646,     STT0003
R12048                                 380846,380946,440027,440127,     STT0003
R12048                                 440243,500083,500162.            STT0003

R16454        03  WS-NITSUS-22                 PIC 9(11).               IMS0004
R16454        88  WS-NIT-SUS-22  VALUE 805025964, 860010522, 900301949, IMS0004
R16454                      860000261, 890300466, 891300382, 892002290, IMS0004
R16454                      860042945, 890303215, 800170523, 860004828, IMS0004
R16454                      891100445, 891100190, 890301463, 890700172, IMS0004
R16454                      860525838, 890300466, 900462511, 900239396, IMS0004
R16454                      890900291.                                  IMS0004
              
CQ9579   02 PER-22-CONSTANTES.                                          IMS0002
CQ9579        03  WS-0P85-22                   PIC 9.99 VALUE 0.85.     IMS0002
CQ9579        03  WS-1P15-22                   PIC 9.99 VALUE 1.15.     IMS0002
