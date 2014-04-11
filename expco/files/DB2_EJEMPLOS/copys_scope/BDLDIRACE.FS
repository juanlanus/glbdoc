REMARK*BDLDIRACE-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO DE ACEPCIONES DE DIRECCIONES                      *
REMARK******************************************************************
REMARK* RGH0001       * MOD001 - ESTANDARIZACION DE MANEJO DE ARCHIVOS *
REMARK* R. GALVIS H.  *          BDLDIRACE FILE STATUS                 *
REMARK* ENE. 01/2010  *                                                *
REMARK* INTERSCHEMES. *                                                *
REMARK*               *                                                *
REMARK******************************************************************
MOD001$SET CONSTANT SW-STATUS-BDLDIRACE"NO"                             RGH0001
       SELECT BDLDIRACE
              ASSIGN TO DISK TITLE-BDLDIRACE
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS LLAVE-BDLDIRACE
MOD001*       FILE STATUS IS COD-RETORNO-BDLDIRACE                      RGH0001
MOD001$IF SW-STATUS-BDLDIRACE = "NO"                                    RGH0001
MOD001$DISPLAY ".............COMPILANDO BDLDIRACE SIN STATUS.........." RGH0001
MOD001$ELSE                                                             RGH0001
MOD001$DISPLAY ".............COMPILANDO BDLDIRACE CON STATUS.........." RGH0001
MOD001            FILE STATUS IS COD-RETORNO-BDLDIRACE                  RGH0001
MOD001$END                                                              RGH0001
              LOCK MODE IS MANUAL WITH LOCK ON RECORD.
