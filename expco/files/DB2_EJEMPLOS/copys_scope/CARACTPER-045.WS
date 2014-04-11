      *CARACTPER-045.WS-V001.
      ******************************************************************
      *DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA NATURA         *
      ******************************************************************
      * IMS0001    | R12212 - NATURA - VERSION INICIAL.                *
      * 06-JUN-2013|                                                   *
      * M.SALAMANCA|                                       REQ.CQ12212 *
      ******************************************************************
        01 PERSONALIZACION-45.

           02 PER-45-NIVEL-00-MOR-TAB   OCCURS   7 TIMES.
              03 00MOR999XX045          PIC 9(04).

           02 PER-45-NIVEL-00-END-TAB   OCCURS   7 TIMES.
              03 00END999XX045          PIC 9(11).
              03 00END998XX045          PIC 9(11).
              03 00END997XX045          PIC 9(11).
              03 00END996XX045          PIC 9(11).

           02 PER-45-NIVEL-01-MOR-TAB   OCCURS   7 TIMES.
              03 01MOR999XX045          PIC 9(04).

           02 PER-45-NIVEL-01-MOR  REDEFINES  PER-45-NIVEL-01-MOR-TAB.
              03 01MOR999RO045          PIC 9(04).
              03 01MOR999IN045          PIC 9(04).
              03 01MOR999VE045          PIC 9(04).
              03 01MOR999HP045          PIC 9(04).
              03 01MOR999CC045          PIC 9(04).
              03 01MOR999OT045          PIC 9(04).
              03 01MOR999CO045          PIC 9(04).

           02 PER-45-NIVEL-01-END-TAB   OCCURS   7 TIMES.
              03 01END999XX045          PIC 9(11).
              03 01END998XX045          PIC 9(11).
              03 01END997XX045          PIC 9(11).
              03 01END996XX045          PIC 9(11).

           02 PER-45-NIVEL-01-END  REDEFINES  PER-45-NIVEL-01-END-TAB.
              03 01END999RO045          PIC 9(11).
              03 01END998RO045          PIC 9(11).
              03 01END997RO045          PIC 9(11).
              03 01END996RO045          PIC 9(11).

              03 01END999IN045          PIC 9(11).
              03 01END998IN045          PIC 9(11).
              03 01END997IN045          PIC 9(11).
              03 01END996IN045          PIC 9(11).

              03 01END999VE045          PIC 9(11).
              03 01END998VE045          PIC 9(11).
              03 01END997VE045          PIC 9(11).
              03 01END996VE045          PIC 9(11).

              03 01END999HP045          PIC 9(11).
              03 01END998HP045          PIC 9(11).
              03 01END997HP045          PIC 9(11).
              03 01END996HP045          PIC 9(11).

              03 01END999CC045          PIC 9(11).
              03 01END998CC045          PIC 9(11).
              03 01END997CC045          PIC 9(11).
              03 01END996CC045          PIC 9(11).

              03 01END999OT045          PIC 9(11).
              03 01END998OT045          PIC 9(11).
              03 01END997OT045          PIC 9(11).
              03 01END996OT045          PIC 9(11).

              03 01END999CO045          PIC 9(11).
              03 01END998CO045          PIC 9(11).
              03 01END997CO045          PIC 9(11).
              03 01END996CO045          PIC 9(11).

          02 VAR-AUX-PER-45.
              03 PRT-45                 PIC 9(01) VALUE ZERO.
              03 POS-PER45              PIC 9(02) VALUE ZEROS.
              03 PORT-PER45             PIC X(02) VALUE SPACES.
