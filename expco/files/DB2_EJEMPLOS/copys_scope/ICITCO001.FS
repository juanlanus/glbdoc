      * ICITCONEW-FS.V.01.002
      * ESTE ARCHIVO CONTIENE LOS INDICES DEL ARCHIVO ICBTCO PARA GENERACION
      * DE ESTADISTICAS EN LINEA. lO GENERA EL PROGRAMA ICGCON Y LO LEE EL
      * PROGRAMA ICGTRA
      **************************************************************************
      * NDG0002    * MULSRV : SE CAMBIA LA ESTRUCTURA DE RELATIVO A INDEXADO   *
      * N.DUARTE   *          POR IMPLEMENTACION VARIAS INSTANCIAS DEL ICGCON. *
      * 01/06/2011 *                                                           *
      **************************************************************************
           SELECT ICITCO
                  ASSIGN TO DISK   TITLE-ICITCO         
MULSRV            ORGANIZATION IS INDEXED                               NDG0002
                  ACCESS MODE IS DYNAMIC
MULSRV            RECORD KEY IS LLAVE-ICITCO                            NDG0002
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
