      *BDSCORES0001.FS.v.01.0002
      *******************************************************************
      * ARCHIVO CON LOS SCORES POR IDENTIFICACION DE C/MES EN UN AÑO
      * CONVERTIDO A FORMATO INDEXADO TIPO 8
      *******************************************************************
REMARK* GJC0002     * T8AMPL SE CONVIERTA A FORMATO INDEXADO TIPO 8
REMARK*             *        Y SE AMPLIA EL TAMAÑO DE LOS CAMPOS
REMARK*             *        DE LOS SCORES
REMARK**************************************************************
T8AMPL $SET IDXFORMAT"8"                                                GJC0002
009710$SET CONSTANT SW-STATUS-BDSCORES"NO"
           SELECT BDSCORES
                  ASSIGN TO DISK TITLE-BDSCORES   
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDSCORES
      $IF SW-STATUS-BDSCORES = "NO"
      $DISPLAY ".............COMPILANDO BDSCORES SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDSCORES CON STATUS.........."
                  FILE STATUS IS COD-RETORNO-BDSCORES
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
T8AMPL $SET IDXFORMAT"0"                                                GJC0002
