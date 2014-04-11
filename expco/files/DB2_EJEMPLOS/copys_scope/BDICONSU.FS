REMARK* BDICONSU-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO QUE ALMACENA  LA INFORMACION DE DE LOS CONSULADOS PARA
REMARK* VALIDAR EN EL PROCESO DE ACTUALIZACION DE LA REGISTRADURIA        
REMARK******************************************************************
           SELECT BDICONSU ASSIGN TO DISK "$DATABASE/BDICONSU.DAT"        
                           ORGANIZATION IS LINE SEQUENTIAL        
                           ACCESS MODE IS SEQUENTIAL.        
