      *LOGPESVNO.WS.v.00001
      ************************************************************
      *P.VARGAS   *CQ6271  LOG TRANSACCIONES PESVNO              *
      *SEP/2012   *                                              *
      ************************************************************
       01  W-ARCHIVO-LOGPESVNO.
           03 W-LOGPESVNO-DIRECTORIO      PIC X(07) VALUE "$DATOS/".
           03 W-LOGPESVNO-NOMBRE          PIC X(30) 
              VALUE "LOGPESVNO.DAT".

       01  LOGPESVNO-WK.
           05 LLAVE1-LOGPESVNO-WK.
              07 ARCHI-LOGPESVNO-WK       PIC   X(40).
              07 TIPPRO-LOGPESVNO-WK      PIC   X(01).
           05 LLAVE2-LOGPESVNO-WK.        
              07 NUMOP-LOGPESVNO-WK       PIC   9(12).
           05 LLAVE3-LOGPESVNO-WK.        
              07 FECPRO-LOGPESVNO-WK      PIC   9(08).
              07 HORPRO-LOGPESVNO-WK      PIC   9(04).
           05 DATA-LOGPESVNO-WK.          
              07 LEIDOS-LOGPESVNO-WK      PIC   9(12).
              07 TIPO5-LOGPESVNO-WK       PIC   9(12).
              07 TIPO6-LOGPESVNO-WK       PIC   9(12).
              07 TIPO7-LOGPESVNO-WK       PIC   9(12).
              07 TIPO5PJ-LOGPESVNO-WK     PIC   9(12).
              07 CONPRO-LOGPESVNO-WK      PIC   9(05).
              07 ESTADO-LOGPESVNO-WK      PIC   X(01).
              07 TARIFA-LOGPESVNO-WK      PIC   9(12).
              07 VALORPJ-LOGPESVNO-WK     PIC   9(12).
              07 PROGRAM-LOGPESVNO-WK     PIC   X(30).
              07 FILLER                   PIC   X(100).
              
       01  TRANSAC-STATUS-WK              PIC   X(02).
       01  W-STATUS-LOGPESVNO             PIC   9(02).
           88 W-STATUS-LOGPESVNO-OK    VALUE 00 05.

