      **ICMVLR-FS.v.0001
REMARK*************************************************************************
REMARK* MAESTRO DE VALORES EXISTE UN MAESTRO POR CADA AÑO HISTORICO
REMARK* SE DEFINE HASTA MANEJAR 6 ARCHIVOS SIMULTANEAMENTE PARA PODER
REAMRK* HACER EL MANEJO DE HASRA 48 MESES DE VALORES
REMARK*************************************************************************
*******AÑO 1
       $SET IDXFORMAT"8"
           SELECT ICMVLR
                  ASSIGN TO DISK MAES-ICMVLR-TITLE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******AÑO 2
       $SET IDXFORMAT"8"
           SELECT ICMVLR2
                  ASSIGN TO DISK MAES-ICMVLR-TITLE2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR2
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******AÑO 3
       $SET IDXFORMAT"8"
           SELECT ICMVLR3
                  ASSIGN TO DISK MAES-ICMVLR-TITLE3
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR3
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******AÑO 4
       $SET IDXFORMAT"8"
           SELECT ICMVLR4
                  ASSIGN TO DISK MAES-ICMVLR-TITLE4
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR4
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
*******AÑO 5
       $SET IDXFORMAT"8"
           SELECT ICMVLR5
                  ASSIGN TO DISK MAES-ICMVLR-TITLE5
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR5
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"

*******AÑO 6
       $SET IDXFORMAT"8"
           SELECT ICMVLR6
                  ASSIGN TO DISK MAES-ICMVLR-TITLE6
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ICMVLR6
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
       $SET IDXFORMAT"0"
