REMARK* MOVCLI-FD.v.0001
REMARK******************************************************************
REMARK* ARCHIVO DE MOVIMIENTOS DE CLIENTES                             *
REMARK******************************************************************
           SELECT MOVCLI
                  ASSIGN           TO  DISK MOV-CLIENTES
                  ORGANIZATION     IS  LINE SEQUENTIAL
                  ACCESS MODE      IS  SEQUENTIAL
                  FILE STATUS      IS  FILSTAT.