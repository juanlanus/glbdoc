REMARK* BDIERECL-FS.v.0002
      *******************************************
      *  ARCHIVO DE RECLAMOS
      ***************************************************************
      * FLT0001     * INDEXA * SE INDEXA EL ARCHIVO DE RECLAMOS CON   
      * F.LOZANO T. * LOCK MANUAL PARA PODER ACCESARSE SIMULTANEAMENTE  
      * OCT/2007    * SIN ERROR.
      ***************************************************************


           SELECT BDIERECL ASSIGN TO DISK SI-BDIERECL
INDEXA            ORGANIZATION IS INDEXED
INDEXA            RECORD KEY   IS BDIERECL-LLAVE
INDEXA            ACCESS MODE  IS DYNAMIC
INDEXA            FILE STATUS  IS COD-RETORNO-BDIRECL
INDEXA            LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
