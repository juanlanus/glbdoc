      * FRABDV.FS.v.01.001
BDU001*----------------------------------------------------------------*ABB0001
BDU001* BASE DE DATOS UNIVERSAL DE PREGUNTAS                           *ABB0001
BDU001* REPORTADAS POR LOS DIFERENTES SUSCRIPTORES                     *ABB0001
BDU001* DEFINICION PARA CREAR COPIA FRABDU.NEW                         *ABB0001
BDU001*----------------------------------------------------------------*ABB0001
BDU001 $SET IDXFORMAT"8"                                                ABB0001
BDU001     SELECT FRABDV                                                ABB0001
BDU001            ASSIGN TO DISK FRABDV-TITLE                           ABB0001
BDU001            ORGANIZATION IS INDEXED                               ABB0001
BDU001            ACCESS MODE IS DYNAMIC                                ABB0001
BDU001            RECORD KEY IS LLAVE-FRABDV                            ABB0001
BDU001         ALTERNATE RECORD KEY IS LLAVE-FRABDV-ALT WITH DUPLICATES ABB0001
BDU001            LOCK MODE IS MANUAL WITH LOCK ON RECORD.              ABB0001
BDU001 $SET IDXFORMAT"0"                                                ABB0001
