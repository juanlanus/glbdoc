      * DTCABD-FS.v.0003
      * ARCHIVO QUE ALMACENA POR FECHA LAS NOVEDADES QUE SE HAN REALIZADO
      * POR ALGUN PROGRAMA UNIX A UNA CUENTA CON RECLAMO VIGENTE
REMARK******************************************************************
REMARK* VHF0002     * SE COLOCA FILE-STATUS PARA HACER UN MEJOR MANEJO *
REMARK* V.H.FORERO  * EN EL PROGRAMA QUE LO RECORRE                    *
REMARK* F.CUADROS   *                                                  *
REMARK* FEB.15/2003 *                                                  *
REMARK******************************************************************
REMARK* FAC0003     * INCLUSION DE CLASULA LOCK MODE MANUAL
REMARK* F.CUADROS C.* MOD003 *
REMARK* ENE.11/2004 *
REMARK******************************************************************
      $SET CONSTANT SW-STATUS-DTCABD"NO"                                FAC0002
           SELECT DTCABD            
                  ASSIGN TO DISK TITLE-DTCABD      
                  ORGANIZATION IS LINE SEQUENTIAL
      $IF SW-STATUS-DTCABD = "NO"                                       FAC0002
      $DISPLAY ".............COMPILANDO DTCABD SIN STATUS.........."    FAC0002
      $ELSE                                                             FAC0002
      $DISPLAY ".............COMPILANDO DTCABD CON STATUS.........."    FAC0002
                  FILE STATUS IS FILE-STATUS-DTCABD                     VHF0002
      $END                                                              FAC0002
MOD003            LOCK MODE IS MANUAL WITH LOCK ON RECORD               FAC0003
                  ACCESS MODE IS SEQUENTIAL.
