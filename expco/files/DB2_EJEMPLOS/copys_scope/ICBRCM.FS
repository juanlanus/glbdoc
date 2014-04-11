REMARK***  ICBRCM-FS.v.0002
REMARK***  DEFINICION ARCHIVO DE COMENTARIOS DE RECLAMOS
REMARK******************************************************************
REMARK* VHF0002     * SE CATALOGA LA ESTRUCTURA PARA COLOCAR LA VERSION*
REMARK* VICTOR HUGO *                                                  *
REMARK* FORERO      *                                                  *
REMARK* MAR.22/2002 *                                                  *
REMARK******************************************************************
           SELECT RECLACOM
                  ASSIGN TO DISK RECLACOM-TITLE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-RECLACOM
                  ALTERNATE KEY IS NUM-REC-COM
                            WITH DUPLICATES
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.

