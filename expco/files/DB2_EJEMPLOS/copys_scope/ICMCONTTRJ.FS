      * ICMCONTTRJ.v.01.001
      * MAESTRO QUE ALMACENA INFORMACION DE CONTRATO Y TARJETA PARA       
      * LAS ENTIDADES QUE REPORTAN TIPO DE CUENTA 02
REMARK******************************************************************
       $SET IDXFORMAT"8"
      
           SELECT ICMCONTTRJ    
                  ASSIGN TO DISK MAES-ICMCONTTRJ-TITLE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MAES-ICMCONTTRJ
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
