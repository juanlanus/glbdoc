      **ICMVLR-FS.v.0001
REMARK*************************************************************************
REMARK* MAESTRO DE VALORES EXISTE UN MAESTRO POR CADA A�O HISTORICO
REMARK* SE DEFINE HASTA MANEJAR 6 ARCHIVOS SIMULTANEAMENTE PARA PODER
REAMRK* HACER EL MANEJO DE HASRA 48 MESES DE VALORES
REMARK*************************************************************************
*******A�O 1
       $SET IDXFORMAT"8"
           SELECT ICMVLR
                  ASSIGN TO DISK MAES-ICMVLR-TITLE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******A�O 2
       $SET IDXFORMAT"8"
           SELECT ICMVLR2
                  ASSIGN TO DISK MAES-ICMVLR-TITLE2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR2
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******A�O 3
       $SET IDXFORMAT"8"
           SELECT ICMVLR3
                  ASSIGN TO DISK MAES-ICMVLR-TITLE3
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR3
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******A�O 4
       $SET IDXFORMAT"8"
           SELECT ICMVLR4
                  ASSIGN TO DISK MAES-ICMVLR-TITLE4
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR4
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******A�O 5
       $SET IDXFORMAT"8"
           SELECT ICMVLR5
                  ASSIGN TO DISK MAES-ICMVLR-TITLE5
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR5
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"

*******A�O 6
       $SET IDXFORMAT"8"
           SELECT ICMVLR6
                  ASSIGN TO DISK MAES-ICMVLR-TITLE6
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR6
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
