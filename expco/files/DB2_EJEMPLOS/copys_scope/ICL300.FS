      * EST026.v.02.002
      *   DEFINICION DE ARCHIVO DE LOG PARA SISTEMA DE MANTENIMIENTO
      *   DE DOCUMENTOS DE IDENTIDAD.
      *
           SELECT MANREG                               
                  ASSIGN TO DISK "$LISTADOS/ICL300"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MANREG
                  ALTERNATE RECORD KEY IS LLAVE-MANREG-1
                            WITH DUPLICATES 
                  ALTERNATE RECORD KEY IS MRG-PAQUETE
                            WITH DUPLICATES
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
