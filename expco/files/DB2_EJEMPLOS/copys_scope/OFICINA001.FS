REMARK* OFICINA-FS.v.0002
REMARK******************************************************************
REMARK* ARCHIVO QUE CONTIENE OFICINAS Y CIUDADES ASOCIADAS A CADA      *
REMARK* CUENTA                                                         *
REMARK******************************************************************
REMARK* JULIO/15/2020* OFIC02 - SE CONVIERTE EL ARCHIVO A TIPO 8
REAMRK* GRACIELA J   *
REMARK******************************************************************
OFIC02 $SET IDXFORMAT"8"                                                GJC0002
           SELECT OFICINA
                  ASSIGN  TO DISK TITLE-OFICINA
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-OFICINA
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
OFIC02 $SET IDXFORMAT"0"                                                GJC0002
