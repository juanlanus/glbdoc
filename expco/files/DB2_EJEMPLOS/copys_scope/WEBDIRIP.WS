REMARK*** WEBDIRIP-WS.v.0001
REMARK******************************************************************
REMARK* DEFINICION DE VARIABLES PARA UTILIZAR EN RUTINA DE VALIDACION  *
REMARK* DE DIRECCIONES IP                                              *
REMARK******************************************************************
REMARK*** ARREGLOS
REMARK*** ========
       01 DIRECCIONES-WEBDIRIP. 
          03 DIR-WEBDIRIP                OCCURS 2000 TIMES
             ASCENDING KEY IS LLAVE-DIR-WEBDIRIP
             INDEXED   BY  I-WEBDIRIP.
             05 LLAVE-DIR-WEBDIRIP.
                07 IP-DIR-WEBDIRIP       PIC X(20).
                07 NIT-DIR-WEBDIRIP      PIC 9(11).
             05 IND-DIR-WEBDIRIP         PIC X(01).

REMARK*** INDICADORAS
REMARK*** ===========
       01 FDA-WEBDIRIP                   PIC 9(01).
       01 ENC-WEBDIRIP                   PIC 9(04).

REMARK*** CONTADORES 
REMARK*** ==========
       01 IDX-WEBDIRIP                   PIC 9(04).
       01 CON-WEBDIRIP                   PIC 9(02).

REMARK*** GENERALES 
REMARK*** =========
       01 REG-AUX-WEBDIRIP.
          03 FIL-AUX-WEBDIRIP            PIC X(04).
          03 IND-AUX-WEBDIRIP            PIC X(01).
          03 DES-AUX-WEBDIRIP            PIC X(50).
          03 IP-AUX-WEBDIRIP             PIC X(20).
          03 NITSDIR-WEBDIRIP.
             05 NIT-WEBDIRIP             PIC 9(11)  OCCURS 20 TIMES.
       01 LLAVE-TMP-WEBDIRIP.
          03 IP-TMP-WEBDIRIP             PIC X(20).
          03 NIT-TMP-WEBDIRIP            PIC 9(11).
