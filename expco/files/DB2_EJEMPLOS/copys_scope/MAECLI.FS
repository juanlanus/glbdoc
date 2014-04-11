REMARK* MAECLI-FD.v.0001
REMARK******************************************************************
REMARK* MAESTRO DE REPORTE DE CLIENTES                                 *
REMARK******************************************************************
           SELECT MAECLI
                  ASSIGN           TO  DISK MOV-CLIENTES
                  ORGANIZATION     IS  LINE SEQUENTIAL
                  ACCESS MODE      IS  SEQUENTIAL
                  FILE STATUS      IS  FILSTAT.