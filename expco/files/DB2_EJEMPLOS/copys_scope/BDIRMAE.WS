REMARK* BDIRMAE-WS.v.0001
REAMRK* REGISTRO GENERAL DEL MAESTRO DE SECTOR REAL

       01  REG-BDIRMAE.
           03  LLAVE-BDIRMAE.
               05  IDEN-BDIRMAE.
                   07  TIPIDE-BDIRMAE  PIC 9(01).
                   07  NUMIDE-BDIRMAE  PIC 9(11).
               05  FECHA-BDIRMAE       PIC 9(06).
               05  MORA-BDIRMAE        PIC 9(01).
               05  SUSCRIPTOR-BDIRMAE  PIC 9(06).
               05  PLAZO-BDIRMAE       PIC 9(01).
           03  SECACT-BDIRMAE          PIC 9(01).
           03  TIPCTA-BDIRMAE          PIC 9(02).
           03  FEC-VINCULADO-BDIRMAE   PIC 9(08)  COMP-3.
           03  FEC-DESVIN-BDIRMAE      PIC 9(08)  COMP-3.
           03  CANTIDAD-BDIRMAE        PIC 9(05)  COMP-3.
           03  VALOR-BDIRMAE           PIC 9(12)  COMP-3.
           03  BLOQUEO-BDIRMAE         PIC 9(01).
           03  CODRECL-BDIRMAE         PIC 9(02).
           03  NUMRECL-BDIRMAE         PIC 9(14)  COMP-3.
           03  FECRECL-BDIRMAE         PIC 9(08)  COMP-3.
           03  IND-MODIF-BDIRMAE       PIC 9(01).
           03  FILLER                  PIC X(30).
