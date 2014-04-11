      * BDLSTR-fs.v01.001
      * MAESTRO DE ESTRATOS HISTORICOS   
       $SET IDXFORMAT"8"
      $SET CONSTANT SW-STATUS-BDLSTR"NO"
           SELECT BDLSTR              
                  ASSIGN TO DISK "$DIRECCIONES/BDLSTR"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDLSTR        
      $IF SW-STATUS-BDLSTR = "NO"
      $DISPLAY ".............COMPILANDO BDLSTR SIN STATUS.........."
      $ELSE
      $DISPLAY ".............COMPILANDO BDLSTR CON STATUS.........."
                  FILE STATUS IS EST-BDLSTR
      $END
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
