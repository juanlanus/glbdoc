REMARK* BDLNEWDIRBOG-FS.0002
REMARK* ARCHIVO DE NUEVAS DIRECCIONES DE BOGOTA                         
REMARK************************************************************************
REMARK* RMB0002    * DIRB02  SE CAMBIA AL DIRECTORIO $DIRECCIONES EL ARCHIVO
REMARK* R.MARTINEZ *         DE NUEVA NOMENCLATURA                           
REMARK* 26/FEB/2007* 
REMARK************************************************************************
           SELECT BDLNEWDIRBOG
DIRB02            ASSIGN TO DISK "$DIRECCIONES/BDLNEWDIRBOG"            RMB0002 
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDLNEWDIRBOG
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
