      *LIBSQL-FS.v.0003.
REMARK******************************************************************
REMARK* VHF0002     * PRUAUT - SE INCLUYE MANEJO DEL NOMBRE DE ARCHIVO *
REMARK* VICTOR HUGO *          POR VARIABLE PARA PERMITIR PRUEBAS      *
REMARK* FORERO      *          AUTOMATICAS                             *
REMARK* JUN.06/2008 *                                                  *
REMARK******************************************************************
REMARK* NDG0003     * CONSQL - SE INCLUYE LOGICA PARA UTILIZAR DE FOR- *
REMARK* N.DUARTE    *          MA INDEPENDIENTE EL ARCHIVO LIBSQL-CF   *
REMARK* 11/02/2010  *          PARA LEER LOS PARAMETROS DE CONEXION A  *
REMARK*             *          LA BASE DE DATOS DB2.                   *
REMARK*             *                                                  *
REMARK******************************************************************

REMARK******************************************************************
REMARK*ARCHIVO PARA GUARDAR EL RESULTADO DE CONSULTAS A BD RELACIONES  *
REMARK******************************************************************
           SELECT LIBSQL ASSIGN TO DISK TITLE-LIBSQL
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  FILE STATUS  IS FILE-STATUS-LIBSQL.

REMARK******************************************************************
REMARK*ARCHIVO PARA GUARDAR EL LOG DE MODO DEBUG                       *
REMARK******************************************************************
PRUAUT     SELECT DEBUG-ARCH-LIBSQL ASSIGN TO DISK TITLE-SQLLOG-LIBSQL  VHF0002
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
PRUAUT            FILE STATUS  IS FILSTA-SQLLOG-LIBSQL.                 VHF0002

REMARK******************************************************************
REMARK*ARCHIVO DE CONFIGURACION CONEXION DB2                           *
REMARK******************************************************************
CONSQL     SELECT LIBSQL-CF ASSIGN TO DISK TITLE-LIBSQL-CF              NDG0003
CONSQL            ORGANIZATION IS LINE SEQUENTIAL                       NDG0003
CONSQL            ACCESS MODE  IS SEQUENTIAL                            NDG0003
CONSQL            LOCK MODE IS MANUAL WITH LOCK ON RECORD               NDG0003
CONSQL            FILE STATUS  IS FILE-STATUS-LIBSQL-CF.                NDG0003
