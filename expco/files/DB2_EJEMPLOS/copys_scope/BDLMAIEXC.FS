REMARK*BDLMAIEXC-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO DE EXCLUSION DE MAILS                                  *
REMARK******************************************************************
REMARK* RGH0001       * MOD001 - ESTANDARIZACION DE MANEJO DE ARCHIVOS *
REMARK* R. GALVIS H.  *          BDLMAIEXC FILE STATUS                 *
REMARK* ENE. 01/2010  *                                                *
REMARK* INTERSCHEMES. *                                                *
REMARK******************************************************************
MOD001$SET CONSTANT SW-STATUS-BDLMAIEXC"NO"                             RGH0001
       SELECT BDLMAIEXC
              ASSIGN TO DISK TITLE-BDLMAIEXC
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS LLAVE-BDLMAIEXC
MOD001*       FILE STATUS IS COD-RETORNO-BDLMAIEXC                      RGH0001
MOD001$IF SW-STATUS-BDLMAIEXC = "NO"                                    RGH0001
MOD001$DISPLAY ".............COMPILANDO BDLMAIEXC SIN STATUS.........." RGH0001
MOD001$ELSE                                                             RGH0001
MOD001$DISPLAY ".............COMPILANDO BDLMAIEXC CON STATUS.........." RGH0001
MOD001            FILE STATUS IS COD-RETORNO-BDLMAIEXC                  RGH0001
MOD001$END                                                              RGH0001
              LOCK MODE IS MANUAL WITH LOCK ON RECORD.
