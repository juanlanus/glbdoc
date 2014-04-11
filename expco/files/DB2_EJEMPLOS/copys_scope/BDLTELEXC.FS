REMARK*BDLTELEXC-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO DE EXCLUSIONES DE TELEFONOS                            *
REMARK******************************************************************
REMARK* RGH0001       * MOD001 - ESTANDARIZACION DE MANEJO DE ARCHIVOS *
REMARK* R. GALVIS H.  *          BDLTELEXC FILE STATUS                 *
REMARK* ENE. 01/2010  *                                                *
REMARK* INTERSCHEMES. *                                                *
REMARK******************************************************************
MOD001$SET CONSTANT SW-STATUS-BDLTELEXC"NO"                             RGH0001
       SELECT BDLTELEXC
              ASSIGN TO DISK TITLE-BDLTELEXC
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS LLAVE-BDLTELEXC
MOD001*       FILE STATUS IS COD-RETORNO-BDLTELEXC                      RGH0001
MOD001$IF SW-STATUS-BDLTELEXC = "NO"                                    RGH0001
MOD001$DISPLAY ".............COMPILANDO BDLTELEXC SIN STATUS.........." RGH0001
MOD001$ELSE                                                             RGH0001
MOD001$DISPLAY ".............COMPILANDO BDLTELEXC CON STATUS.........." RGH0001
MOD001            FILE STATUS IS COD-RETORNO-BDLTELEXC                  RGH0001
MOD001$END                                                              RGH0001
              LOCK MODE IS MANUAL WITH LOCK ON RECORD.
