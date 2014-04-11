REMARK*CARACTPER-009.WS.v.0003.
      ******************************************************************
      * CAV0002     | CQ8177 - SE INCLUYE CARACTERISTICAS DE LA        *
      * 26-OCT-2012 |          01NUM999IN009  A LA 01NUM990IN009.      *
      ******************************************************************
      * IMS0003    | R11706 - SE AGREGAN CARACTERISTICAS PARA PORTAF.  *
      * 16-ABR-2012|          HP,VE,RO,IN DE CALIFICACIONES A,B,C,D,E. *
      * M.SALAMANCA|                                      REQ. CQ11706 *
      ******************************************************************
        01 PERSONALIZACION-09.
           02 PER-09-NIVEL-XX-OTR.
              03 PFLAG-009              PIC 999.
           02 PER-09-NIVEL-00-MOR.
              03 00MOR105CC000          PIC 999.
              03 00MOR105CO000          PIC 999.
              03 00MOR105HP000          PIC 999.
              03 00MOR105IN000          PIC 999.
              03 00MOR105OT000          PIC 999.
              03 00MOR105RO000          PIC 999.
              03 00MOR105VE000          PIC 999.
              03 00MOR129CC000          PIC 9999.
              03 00MOR129CO000          PIC 9999.
              03 00MOR129HP000          PIC 9999.
              03 00MOR129IN000          PIC 9999.
              03 00MOR129OT000          PIC 9999.
              03 00MOR129RO000          PIC 9999.
              03 00MOR129VE000          PIC 9999.
           02 PER-09-NIVEL-01-MOR.
              03 01MOR105CC000          PIC 9999.
              03 01MOR105CO000          PIC 9999.
              03 01MOR105HP000          PIC 9999.
              03 01MOR105IN000          PIC 9999.
              03 01MOR105OT000          PIC 9999.
              03 01MOR105RO000          PIC 9999.
              03 01MOR105VE000          PIC 9999.
              03 01MOR129CC000          PIC 9999.
              03 01MOR129CO000          PIC 9999.
              03 01MOR129HP000          PIC 9999.
              03 01MOR129IN000          PIC 9999.
              03 01MOR129OT000          PIC 9999.
              03 01MOR129RO000          PIC 9999.
              03 01MOR129VE000          PIC 9999.
           02 PER-09-NIVEL-02-MOR.
              03 02MOR105TO000          PIC 9999.
              03 02MOR129TO000          PIC 9999.
      *
           02 PER-09-NIVEL-00-EXP.
              03 00EXP999IN009          PIC 9999.
              03 00EXP999VE009          PIC 9999.
              03 00EXP999HP009          PIC 9999.
              03 00EXP999RO009          PIC 9999.
           02 PER-09-NIVEL-01-EXP.
              03 01EXP999IN009          PIC 9999.
              03 01EXP999VE009          PIC 9999.
              03 01EXP999HP009          PIC 9999.
              03 01EXP999RO009          PIC 9999.
      *
           02 PER-09-NIVEL-00-DEM.
              03 00DEM031000            PIC 9999.
              03 00DEM032000            PIC 99.
              03 00DEM033000            PIC 99.
              03 00DEM034000            PIC 99999.999.
           02 PER-09-NIVEL-00-DEMV.
              03 00DEMV034000           PIC 99999V999.
           02 PER-09-NIVEL-01-DEM.
              03 01DEM999009            PIC 9.
      *
CQ8177     02 PER-09-NIVEL-00-NUM.                                      CAV0002
CQ8177        03 00NUM999IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM998IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM997IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM996IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM995IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM994IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM993IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM992IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM991IN009          PIC 9999.                       CAV0002
CQ8177        03 00NUM990IN009          PIC 9999.                       CAV0002
R11706        03 00NUM998HP009          PIC 9(4).                       IMS0003
R11706        03 00NUM998VE009          PIC 9(4).                       IMS0003
R11706        03 00NUM998RO009          PIC 9(4).                       IMS0003
R11706        03 00NUM998IX009          PIC 9(4).                       IMS0003
R11706        03 00NUM997HP009          PIC 9(4).                       IMS0003
R11706        03 00NUM997VE009          PIC 9(4).                       IMS0003
R11706        03 00NUM997RO009          PIC 9(4).                       IMS0003
R11706        03 00NUM997IX009          PIC 9(4).                       IMS0003
R11706        03 00NUM996HP009          PIC 9(4).                       IMS0003
R11706        03 00NUM996VE009          PIC 9(4).                       IMS0003
R11706        03 00NUM996RO009          PIC 9(4).                       IMS0003
R11706        03 00NUM996IX009          PIC 9(4).                       IMS0003
R11706        03 00NUM995HP009          PIC 9(4).                       IMS0003
R11706        03 00NUM995VE009          PIC 9(4).                       IMS0003
R11706        03 00NUM995RO009          PIC 9(4).                       IMS0003
R11706        03 00NUM995IX009          PIC 9(4).                       IMS0003
R11706        03 00NUM994HP009          PIC 9(4).                       IMS0003
R11706        03 00NUM994VE009          PIC 9(4).                       IMS0003
R11706        03 00NUM994RO009          PIC 9(4).                       IMS0003
R11706        03 00NUM994IX009          PIC 9(4).                       IMS0003

CQ8177     02 PER-09-NIVEL-01-NUM.                                      CAV0002
CQ8177        03 01NUM999IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM998IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM997IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM996IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM995IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM994IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM993IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM992IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM991IN009          PIC 9999.                       CAV0002
CQ8177        03 01NUM990IN009          PIC 9999.                       CAV0002
R11706        03 01NUM998HP009          PIC 9(4).                       IMS0003
R11706        03 01NUM998VE009          PIC 9(4).                       IMS0003
R11706        03 01NUM998RO009          PIC 9(4).                       IMS0003
R11706        03 01NUM998IX009          PIC 9(4).                       IMS0003
R11706        03 01NUM997HP009          PIC 9(4).                       IMS0003
R11706        03 01NUM997VE009          PIC 9(4).                       IMS0003
R11706        03 01NUM997RO009          PIC 9(4).                       IMS0003
R11706        03 01NUM997IX009          PIC 9(4).                       IMS0003
R11706        03 01NUM996HP009          PIC 9(4).                       IMS0003
R11706        03 01NUM996VE009          PIC 9(4).                       IMS0003
R11706        03 01NUM996RO009          PIC 9(4).                       IMS0003
R11706        03 01NUM996IX009          PIC 9(4).                       IMS0003
R11706        03 01NUM995HP009          PIC 9(4).                       IMS0003
R11706        03 01NUM995VE009          PIC 9(4).                       IMS0003
R11706        03 01NUM995RO009          PIC 9(4).                       IMS0003
R11706        03 01NUM995IX009          PIC 9(4).                       IMS0003
R11706        03 01NUM994HP009          PIC 9(4).                       IMS0003
R11706        03 01NUM994VE009          PIC 9(4).                       IMS0003
R11706        03 01NUM994RO009          PIC 9(4).                       IMS0003
R11706        03 01NUM994IX009          PIC 9(4).                       IMS0003
