REMARK* BDIIALE-FS.v01.0001        
REMARK* MAESTRO DE ALERTAS REPORTADAS POR SUSCRIPTOTRES                     
              
      $SET CONSTANT SW-STATUS-BDIIALE"NO"        
           SELECT BDIIALE                   
                  ASSIGN TO DISK MAES-BDIIALE-TITLE        
                  ORGANIZATION IS INDEXED        
                  ACCESS MODE IS DYNAMIC        
                  RECORD KEY IS LLAVE-BDIIALE                
      $IF SW-STATUS-BDIIALE = "NO"        
      $DISPLAY ".............COMPILANDO BDIIALE SIN STATUS.........."        
      $ELSE        
      $DISPLAY ".............COMPILANDO BDIIALE CON STATUS.........."        
                  FILE STATUS IS EST-BDIIALE           
      $END        
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.        
