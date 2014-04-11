REMARK*CARACTPER-001.WS.v.0001.
        01 PERSONALIZACION-01.
           02 PER-01-NIVEL-00.
              03 00NUM049CT001          PIC 9(3).
              03 00ACP999XX001          PIC 9(3).
              03 00MORXXXOT001.
                 04 00MOR999OT001          PIC 9(3).
                 04 00MOR998OT001          PIC 9(3).
                 04 00MOR997OT001          PIC 9(3).
                 04 00MOR996OT001          PIC 9(3).
           02 PER-01-NIVEL-01.
              03 01NUM049CT001          PIC 9(4).
              03 01ACP999XX001-M.
                 04 01ACP999RO001       PIC 9(4).
                 04 01ACP999IN001       PIC 9(4).
                 04 01ACP999VE001       PIC 9(4).
                 04 01ACP999HP001       PIC 9(4).
                 04 01ACP999OT001       PIC 9(4).
                 04 01ACP999CC001       PIC 9(4).
                 04 01ACP999CO001       PIC 9(4).
                 04 01ACP999AH001       PIC 9(4).
                 04 01ACP999CT001       PIC 9(4).
              03 FILLER REDEFINES 01ACP999XX001-M.
                 04 01ACP999XX001 OCCURS 9 TIMES PIC 9(04).
              03 01MORXXXOT001.
                 04 01MOR999OT001          PIC 9(4).
                 04 01MOR998OT001          PIC 9(4).
                 04 01MOR997OT001          PIC 9(4).
                 04 01MOR996OT001          PIC 9(4).
           02 PER-01-NIVEL-02.
              03 02ACP999TO001          PIC 9(4).
              03 02MORXXXTO001.
                 04 02MOR999TO001          PIC 9(4).
                 04 02MOR998TO001          PIC 9(4).
                 04 02MOR997TO001          PIC 9(4).
                 04 02MOR996TO001          PIC 9(4).
