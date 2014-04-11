REMARK*CARACTPER-023.WS.v.0004.
REMARK******************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS 023 SUFINANCIAMIENTO
REMARK******************************************************************
      * IMS0002    | CQ7951 - SE INCLUYE CARACTERISTICA EXP998RO023    *
      * 28-SEP-2012|                                                   *
      ******************************************************************
      * IMS0003    | R10809 - SE AGREGAN LAS CARACTERISTICAS           *
      * 14-MAR-2013|       02EXP998TO023, 02EXP997TO023, 02EXP996TO023 *
      * M.SALAMANCA|       Y 02EXP995TO023                REQ. CQ10809 *
      ******************************************************************
      * IMS0004    | R10994 - SE AGREGAN 20 CARACTERISTICAS PARA       *
      * 19-MAR-2013|          TIPOLOGIA NUM.                           *
      * M.SALAMANCA|                                      REQ. CQ10994 *
      ******************************************************************
        01 PERSONALIZACION-23.
           02 PER-23-NIVEL-00-MOR.
              03 00MOR999IN023          PIC 9(4).
              03 00MOR998IN023          PIC 9(4).
              03 00MOR997IN023          PIC 9(4).
              03 00MOR996MX023          PIC 9(4).
              03 00MOR995MX023          PIC 9(4).
              03 00MOR994IN023          PIC 9(4).
              03 00MOR993IN023          PIC 9(4).
              03 00MOR992IN023          PIC 9(4).
              03 00MOR991MX023          PIC 9(4).
              03 00MOR990MX023          PIC 9(4).
              03 00MOR989MX023          PIC 9(4).
              03 00MOR988MX023          PIC 9(4).
              03 00MOR987MX023          PIC 9(4).
              03 00MOR986MX023          PIC 9(4).
              03 00MOR985TO023          PIC 9(4).
              03 00MOR997MX023          PIC 9(4).
              03 00MOR984MX023          PIC 9(4).
              03 00MOR983MX023          PIC 9(4).
              03 00MOR982MX023          PIC 9(4).
              03 00MOR981MX023          PIC 9(4).
           02 PER-23-NIVEL-00-NUM.
              03 00NUM999HP023          PIC 9(4).
R10994        03 00NUM998HP023          PIC 9(4).                       IMS0004
R10994        03 00NUM998VE023          PIC 9(4).                       IMS0004
R10994        03 00NUM998RO023          PIC 9(4).                       IMS0004
R10994        03 00NUM998IN023          PIC 9(4).                       IMS0004
R10994        03 00NUM997HP023          PIC 9(4).                       IMS0004
R10994        03 00NUM997VE023          PIC 9(4).                       IMS0004
R10994        03 00NUM997RO023          PIC 9(4).                       IMS0004
R10994        03 00NUM997IN023          PIC 9(4).                       IMS0004
R10994        03 00NUM996HP023          PIC 9(4).                       IMS0004
R10994        03 00NUM996VE023          PIC 9(4).                       IMS0004
R10994        03 00NUM996RO023          PIC 9(4).                       IMS0004
R10994        03 00NUM996IN023          PIC 9(4).                       IMS0004
R10994        03 00NUM995HP023          PIC 9(4).                       IMS0004
R10994        03 00NUM995VE023          PIC 9(4).                       IMS0004
R10994        03 00NUM995RO023          PIC 9(4).                       IMS0004
R10994        03 00NUM995IN023          PIC 9(4).                       IMS0004
R10994        03 00NUM994HP023          PIC 9(4).                       IMS0004
R10994        03 00NUM994VE023          PIC 9(4).                       IMS0004
R10994        03 00NUM994RO023          PIC 9(4).                       IMS0004
R10994        03 00NUM994IN023          PIC 9(4).                       IMS0004
           02 PER-23-NIVEL-00-END.
              03 00END999R2023          PIC 9(11).
              03 00END999RO023          PIC 9(11).
              03 00END998R2023          PIC 9(11).
              03 00END998RO023          PIC 9(11).
              03 00END997R2023          PIC 9(11).
              03 00END997RO023          PIC 9(11).
              03 00END996R2023          PIC 9(11).
              03 00END996RO023          PIC 9(11).
              03 00END995R2023          PIC 9(11).
              03 00END995RO023          PIC 9(11).
              03 00END994RO023          PIC 9(11).
              03 00END993RO023          PIC 9(11).
              03 00END992RO023          PIC 9(11).
              03 00END991RO023          PIC 9(11).
              03 00END990RO023          PIC 9(11).
              03 00END989RO023          PIC 9(11).
              03 00END988TO023          PIC 9(11).
              03 00END987R2023          PIC 9(11).
              03 00END986R2023          PIC 9(11).
              03 00END985R2023          PIC 9(11).
              03 00END984R2023          PIC 9(11).
              03 00END983R2023          PIC 9(11).
              03 00END982RO023          PIC 9(11).
              03 00END981RO023          PIC 9(11).
              03 00END980RO023          PIC 9(11).
              03 00END979RO023          PIC 9(11).
              03 00END978RO023          PIC 9(11).
              03 00END977RO023          PIC 9(11).
              03 00END976RO023          PIC 9(11).
              03 00END975RO023          PIC 9(11).
              03 00END974RO023          PIC 9(11).
              03 00END973RO023          PIC 9(11).
           02 PER-23-NIVEL-00-EXP.
              03 00EXP999IN023          PIC 9(4).
CQ7951        03 00EXP998RO023          PIC 9(4).                       IMS0002
R10809        03 00EXP998TO023          PIC 9(4).                       IMS0003
R10809        03 00EXP997TO023          PIC 9(4).                       IMS0003
R10809        03 00EXP996TO023          PIC 9(4).                       IMS0003
R10809        03 00EXP995TO023          PIC 9(4).                       IMS0003
           02 PER-23-NIVEL-01-MOR.
              03 01MOR999IN023          PIC 9(4).
              03 01MOR998IN023          PIC 9(4).
              03 01MOR997IN023          PIC 9(4).
              03 01MOR996MX023          PIC 9(4).
              03 01MOR995MX023          PIC 9(4).
              03 01MOR994IN023          PIC 9(4).
              03 01MOR993IN023          PIC 9(4).
              03 01MOR992IN023          PIC 9(4).
              03 01MOR991MX023          PIC 9(4).
              03 01MOR990MX023          PIC 9(4).
              03 01MOR989MX023          PIC 9(4).
              03 01MOR988MX023          PIC 9(4).
              03 01MOR987MX023          PIC 9(4).
              03 01MOR986MX023          PIC 9(4).
              03 02MOR985TO023          PIC 9(4).
              03 01MOR997MX023          PIC 9(4).
              03 01MOR984MX023          PIC 9(4).
              03 01MOR983MX023          PIC 9(4).
              03 01MOR982MX023          PIC 9(4).
              03 01MOR981MX023          PIC 9(4).
           02 PER-23-NIVEL-01-NUM.
              03 01NUM999HP023          PIC 9(4).
R10994        03 01NUM998HP023          PIC 9(4).                       IMS0004
R10994        03 01NUM998VE023          PIC 9(4).                       IMS0004
R10994        03 01NUM998RO023          PIC 9(4).                       IMS0004
R10994        03 01NUM998IN023          PIC 9(4).                       IMS0004
R10994        03 01NUM997HP023          PIC 9(4).                       IMS0004
R10994        03 01NUM997VE023          PIC 9(4).                       IMS0004
R10994        03 01NUM997RO023          PIC 9(4).                       IMS0004
R10994        03 01NUM997IN023          PIC 9(4).                       IMS0004
R10994        03 01NUM996HP023          PIC 9(4).                       IMS0004
R10994        03 01NUM996VE023          PIC 9(4).                       IMS0004
R10994        03 01NUM996RO023          PIC 9(4).                       IMS0004
R10994        03 01NUM996IN023          PIC 9(4).                       IMS0004
R10994        03 01NUM995HP023          PIC 9(4).                       IMS0004
R10994        03 01NUM995VE023          PIC 9(4).                       IMS0004
R10994        03 01NUM995RO023          PIC 9(4).                       IMS0004
R10994        03 01NUM995IN023          PIC 9(4).                       IMS0004
R10994        03 01NUM994HP023          PIC 9(4).                       IMS0004
R10994        03 01NUM994VE023          PIC 9(4).                       IMS0004
R10994        03 01NUM994RO023          PIC 9(4).                       IMS0004
R10994        03 01NUM994IN023          PIC 9(4).                       IMS0004
           02 PER-23-NIVEL-01-END.
              03 01END999R2023          PIC 9(11).
              03 01END999RO023          PIC 9(11).
              03 01END998R2023          PIC 9(11).
              03 01END998RO023          PIC 9(11).
              03 01END997R2023          PIC 9(11).
              03 01END997RO023          PIC 9(11).
              03 01END996R2023          PIC 9(11).
              03 01END996RO023          PIC 9(11).
              03 01END995R2023          PIC 9(11).
              03 01END995RO023          PIC 9(11).
              03 01END994RO023          PIC 9(11).
              03 01END993RO023          PIC 9(11).
              03 01END992RO023          PIC 9(11).
              03 01END991RO023          PIC 9(11).
              03 01END990RO023          PIC 9(11).
              03 01END989RO023          PIC 9(11).
              03 02END988TO023          PIC 9(08).99.
              03 01END987R2023          PIC 9(11).
              03 01END986R2023          PIC 9(11).
              03 01END985R2023          PIC 9(11).
              03 01END984R2023          PIC 9(11).
              03 01END983R2023          PIC 9(11).
              03 01END982RO023          PIC 9(11).
              03 01END981RO023          PIC 9(11).
              03 01END980RO023          PIC 9(11).
              03 01END979RO023          PIC 9(11).
              03 01END978RO023          PIC 9(11).
              03 01END977RO023          PIC 9(11).
              03 01END976RO023          PIC 9(11).
              03 01END975RO023          PIC 9(11).
              03 01END974RO023          PIC 9(11).
              03 01END973RO023          PIC 9(11).
              03 02END988TO023V         PIC 9(11).
           02 PER-23-NIVEL-01-INQ.
              03 01INQ999TO023          PIC 9(4).
           02 PER-23-NIVEL-01-EXP.
              03 01EXP999IN023          PIC 9(4).
CQ7951        03 01EXP998RO023          PIC 9(4).                       IMS0002
R10809        03 02EXP998TO023          PIC 9(4).                       IMS0003
R10809        03 02EXP997TO023          PIC 9(4).                       IMS0003
R10809        03 02EXP996TO023          PIC 9(4).                       IMS0003
R10809        03 02EXP995TO023          PIC 9(4).                       IMS0003
           02 PER-23-NIVEL-01-DEM.
              03 01DEM999TO023          PIC 99999.9.
R10809     02 PER-23-VAR-AUX.                                           IMS0003
R10809        03 MESES-APE-HOY          PIC 9(4).                       IMS0003
R10809        03 PV-23                  PIC 9(2).                       IMS0003
