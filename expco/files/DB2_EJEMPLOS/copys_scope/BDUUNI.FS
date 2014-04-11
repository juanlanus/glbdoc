******* BDUUNI-FS.v.0003
******* ARCHIVO DE BASE DE DATOS UNIVERSAL                                 
****************************************************************************
REMARK*RMB0001    * SE INCLUYE NOMBRE DEL ARCHIVO EN ICFILE.WS
REMARK*R. MARTINEZ*
REMARK*RMB0002    * SE ADICIONA MANEJO DEL ARCHIVO COMO INDEXADO TIPO 8
REMARK*NOV.23/2000* PREVIENDO EL TAMANO QUE TENDRA EN EL FUTURA INMEDIATO
REMARK**********************************************************************
BDUTP8 $SET IDXFORMAT"8"                                                RMB0003 
BDU002 SELECT BDUUNI ASSIGN TO DISK BDUUNI-TITLE                        RMB0002
         ORGANIZATION IS INDEXED
         ACCESS MODE IS DYNAMIC
         RECORD KEY IS LLAVE-BDUUNI
         LOCK MODE IS MANUAL WITH LOCK ON RECORD.
DBUTP8 $SET IDXFORMAT"0"                                                RMB0003
