*******ICMVAL-FS.v.01.0001
      * ARCHIVO HISTORICO DE VALORES. MANEJA 49 MESES HISTORICOS EL ACTUAL
      * TAMBIEN ESTA EN EL ICMCRE Y 48 MAS QUE CORRESPONDEN A 4 AÑOA ATRAS
       $SET IDXFORMAT"8"        
              
           SELECT ICMVAL                  
                  ASSIGN  TO DISK  "$DATABASE/ICMVAL"                   
                  ORGANIZATION IS  INDEXED        
                  ACCESS  MODE IS  DYNAMIC        
                  RECORD  KEY  IS  LLAVE-ICMVAL              
                  LOCK  MODE   IS  MANUAL WITH LOCK ON RECORD.        
       $SET IDXFORMAT"0"        
