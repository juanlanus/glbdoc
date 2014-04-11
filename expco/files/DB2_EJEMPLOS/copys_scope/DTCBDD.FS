REMARK* DTCBDD-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO PARA INDICAR SI EL CLIENTE INTERBASE PARA NOVEDAT ESTA *
REMARK* ACTIVO O ESTA FUERA DE LINEA                                   *
REMARK******************************************************************
           SELECT DTCBDD
                  ASSIGN  TO DISK "$DATABASE/DTCBDD"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
