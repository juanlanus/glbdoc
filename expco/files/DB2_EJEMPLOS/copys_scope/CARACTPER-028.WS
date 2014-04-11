REMARK*CARACTPER-028.WS.v.0002.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA FNA 
REMARK**********************************************************************
      * IMS0002    | CQ8984 - SE AJUSTA PARA QUE CUANDO NO ENCUENTRE   *
      * 20-NOV-2012|          LA TASA CORRESPONDIENTE AL DIA, TRAIGA   *
      * M.SALAMANCA|          LA DE LA ANTERIOR FECHA ENCONTRADA.      *
      ******************************************************************
        01 PERSONALIZACION-28.
           02 PER-28-NIVEL-01-DEM.
              03 01DEM999TO028          PIC 9(05).9999.
              03 01DEM998TO028          PIC 9(05).9999.
              03 01DEM997TO028          PIC 9(05).9999.
CQ8984     02 PER-28-VAR-AUX.                                           IMS0002
CQ8984        03 SW-ERROR-FNA           PIC 9(01).                      IMS0002
