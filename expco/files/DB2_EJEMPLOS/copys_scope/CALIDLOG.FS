******* ARCHIVO LOG PROGRAMAS UNIX DE CALIDAD EJECUTADOS
******* CALIDLOG-FS.v.0002
REMARK*********************************************************************
REMARK* GJC0002  * SE INCLUE MANEJO DE LOCK AL REGISTRO AUTOMÁTICO PARA    
REMARK* G.JIMENEZ* QUE PUEDA HABER MÁS DE UN PROGRAMA ACTUALIZANDO
REMARK* JUN/2009 *
REMARK********************************************************************
           SELECT CALIDLOG ASSIGN TO DISK NOMBRE-CALIDLOG
                           ORGANIZATION IS LINE SEQUENTIAL
                           ACCESS MODE IS SEQUENTIAL
                           LOCK MODE IS AUTOMATIC.
