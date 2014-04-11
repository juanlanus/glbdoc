      *PE-TIME.WS.v.01.0001.
      *----------------------------------------------------------------*
      *
      *    COPY  WORKING  PROCESOS ESPECIALES VERSION OCT/2004   
      *  
      *----------------------------------------------------------------*
      *    COPY AREA DE LOG PARA CONTROL DE TIEMPOS           
      *----------------------------------------------------------------*

      *-----------------------------------------------------------*
      *      VARIABLES DE   LOG  DE EJECUCION
      *-----------------------------------------------------------*
       01  W-RETORNO-LOG.
           03 W-STATUS-LOG            PIC  9(02) VALUE ZEROS.
           03 W-ARCHIVO-LOG.
              05 W-ENTIDAD-LOG        PIC  X(03) VALUE SPACES.
              05 W-FILLER-LOG         PIC  X(27) VALUE SPACES.
           03 W-PROGRAMA              PIC  X(20) VALUE SPACES.
           03 W-FECHA-HORA-INI.
              05 W-FECHA-INI          PIC  X(10) VALUE SPACES.
              05 W-HORA-INI           PIC  X(08) VALUE SPACES.
           03 W-FECHA-HORA-FIN.
              05 W-FECHA-FIN          PIC  X(10) VALUE SPACES.
              05 W-HORA-FIN           PIC  X(08) VALUE SPACES.
           03 W-USER-UNIX             PIC  9(06) VALUE ZEROS.
      *-----------------------------------------------------------*
      *      VARIABLES DE TIEMPO DE EJECUCION
      *-----------------------------------------------------------*
       01  FECHAS-LOG.
           03  HORA-SIS               PIC  9(08).
           03  HORA-SIS-R  REDEFINES  HORA-SIS.
               05  HORA-S             PIC  9(02).
               05  MINU-S             PIC  9(02).
               05  SEGU-S             PIC  9(02).
               05  DESE-S             PIC  9(02).
           03  FECHA-S                PIC  9(08).
           03  FECHA-CURRENT-DATE  REDEFINES  FECHA-S.
               05  FECHA-SISTEMA      PIC  X(8).
           03  FECHA-S-R           REDEFINES  FECHA-S.
               05 ANIO-S             PIC  9(04).
               05 ANIO-S-R         REDEFINES  ANIO-S.
                  07  ANIO-SIG1      PIC  9(02).
                  07  ANIO-SIG2      PIC  9(02).
               05 MESE-S             PIC  9(02).
               05 DIAS-S             PIC  9(02).
           03 FECHA-PG.
              05 ANIO-P              PIC  9(04)     VALUE ZEROS.
              05 SLASH-1             PIC  X(01)     VALUE "/".
              05 MESE-P              PIC  9(02)     VALUE ZEROS.
              05 SLASH-2             PIC  X(01)     VALUE "/".
              05 DIAS-P              PIC  9(02)     VALUE ZEROS.
           03 HORA-PG.
              05 HORA-P              PIC  9(02)     VALUE ZERO.
              05 DOSPUNTO-1          PIC  X(01)     VALUE ":".
              05 MINU-P              PIC  9(02)     VALUE ZERO.
              05 DOSPUNTO-2          PIC  X(01)     VALUE ":".
              05 SEGU-P              PIC  9(02)     VALUE ZEROS.
