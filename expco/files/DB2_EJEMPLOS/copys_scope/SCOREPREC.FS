      *SCOREPREC.FS.v.01.0001
      ******************************************************************
      * INDEXADO QUE ALMACENA SCORES PARA CLIENTES TDC BANCOLOMBIA     *
      ******************************************************************
       $SET IDXFORMAT"8"
      
           SELECT  SCOREPREC  ASSIGN TO  SCOREPREC-TITLE
                   ORGANIZATION          IS  INDEXED
                   ACCESS         MODE   IS  DYNAMIC
                   RECORD         KEY    IS  SCOREPREC-LLAVE
                   LOCK           MODE   IS  MANUAL WITH LOCK ON RECORD.

       $SET IDXFORMAT"0"
