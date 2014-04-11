      *LOGPESVNO.FS.v.00001
      ************************************************************
      *P.VARGAS   *CQ6271  LOG TRANSACCIONES PESVNO              *
      *SEP/2012   *                                              *
      ************************************************************
           SELECT LOGPESVNO  ASSIGN TO DISK W-ARCHIVO-LOGPESVNO
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC 
                  RECORD KEY   IS LLAVE1-LOGPESVNO
                  ALTERNATE RECORD KEY IS LLAVE2-LOGPESVNO
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE3-LOGPESVNO
                  WITH DUPLICATES
                  FILE STATUS IS W-STATUS-LOGPESVNO.

