REMARK*BDLDIR-WS.v.0015
REMARK*******************************************************************
REMARK* RMB0002      * ALG001 - SE AUMENTAN LAS VARIABLES NECESARIAS POR
REMARK*              * ALG001 - EL CAMBIO EN EL ORDEN DE LA DIRECCION MAS
REMARK* FEB-15-2003  *          PROBABLE AUMENTANDO UN PESO CON BASE A LA
REMARK* R.MARTINEZ   *          CONFORMACION DE LA DIRECCION O DEL TELEFONO
REMARK*******************************************************************
REMARK* RMB0003      * DIRACC - SE DEJA COMO RUTINA LA VALIDACION DE LOS
REMARK* MAR-15-2003  *          PERMISOS DE ACCESO A LA BASE DE DATOS DE
REMARK* R.MARTINEZ   *          DIRECCIONES
REMARK*******************************************************************
REMARK* VHF0004      * ESTAN1 - SE AUMENTAN LOS TAMANOS DE VARIABLES 
REMARK* MAY-27-2003  *          PARA PERMITIR MAXIMO HASTA 30 PARTICULAS
REMARK* V.FORERO     *          PARA UNA DIRECCION
REMARK*******************************************************************
REMARK* FAC0005      * AMPDIR - AMPLIACION DE LA ESTRUCTURA DE ARCHIVO
REMARK* F.CUADROS C. *          ICBDIR 
REMARK* FEB.15/2006  * DOMICI - SEÑAL PARA MATCH DE DOMICILIACION
REMARK*******************************************************************
REMARK* RMB0006      * RCNCER - OPTIMIZACION PARA PRODUCTO RECONOCER.  *
REMARK* R.MARTINEZ   *          SE CAMBIAN VARIABLES QUE UTILIZA LA    *
REMARK* JUN.15/2006  *          RUTINA PARA ESTANDARIZACION ADEMAS DE  *
REMARK*              *          LA DEFINICION DE NUEVAS VARIABLES PARA *
REMARK*              *          OPTIMIZACION (VER DETALLES EN PATCH DE *
REMARK*              *          BDLDIR.PROC).
REMARK******************************************************************
REMARK* GJC0007      * NWDIRB - VARIABLES PARA IDENTIFICAR CUANDO UNA  *
REMARK* G.JIMENEZ    *          DIRECCION DE BOGOTA TIENE NUEVA NOMEN- *
REMARK* FEBRERO/2007 *          CLATURA                                *
REMARK******************************************************************
REMARK* HGO0008      * UNIDIR - UNIFICAR DIRECCIONES QUE TENGAN        *
REMARK* H.GARCIA     *          COINCIDENCIAS CON OTRAS DIRECCIONES CON*
REMARK* MARZO/2009   *          MAYOR CANTIDAD DE INFORMACION - INCLUIR*
REMARK* INTERSCHEMES *          EN LA SALIDA EL CAMPO FECHA ULTIMO REP *
REMARK*              * TELEXC - SE UNIFICAN TELEFONOS QUE COINCIDAN CON*
REMARK*              *          LOS ULTIMOS 7 NUMEROS DEL TELEFONO. SE *
REMARK*              *          CREA TABLA PARA LA EXCLUSION DE TELEFO-*
REMARK*              *          NOS. SE AGREGA EN EL REPORTE FECHA     *
REMARK*              *          ULTIMO REPORTE.                        *
REMARK******************************************************************
REMARK* RGH0009      * TELEX9 - UTILIZAR REDEFINICION PARA VARIABLE    *
REMARK* R.GALVIS     *          V-TELFIN-BDLDIR                        *
REMARK* MARZO/2009   *                                                 *
REMARK* INTERSCHEMES *                                                 *
REMARK******************************************************************
REMARK* HGO0010       * TEXTEL - Se agregan nuevas exclusiones en la   *
REMARK* H. GARCIA O.  *          tabla de telefonos y se amplia el     *
REMARK* SEP.01/2009   *          numero de ocurrencias                 *
REMARK* INTERSCHEMES  *                                                *
REMARK******************************************************************
REMARK* HGO0011       * PYSPAR - AJUSTE QUE BUSCAR GENERAR CALIFICACION*
REMARK* H. GARCIA O.  *          PARA LA PRIMERA Y SEGUNDA PARTICULA Y *
REMARK* AGO. 25/2009  *          LUEGO UNIFICAR TENIENDO EN CUENTA ESTE*
REMARK* INTERSCHEMES. *          CRITERIO                              *
REMARK******************************************************************
REMARK* PJV0012      * NN0012 - AJUSTE, PARA EVITAR QUE SE REPITAN     *
REMARK* P.VARGAS     *          DIRECCIONES EN EL MISMO REPORTE CUNADO *
REMARK* JUL 01/2010  *          SE TRATAN DE NUEVAS NOMENCLATURAS EN   *
REMARK* DATACREDITO  *          BOGOTA.                                *
REMARK******************************************************************
REMARK* PJV0013      * AMP013 -SE AMPLIAN LOS LIMITES DE LAS TABLAS DE *
REMARK* P.VARGAS     *         ACEPCIONES QUE SE CARGAN EN MEMORIA.    *
REMARK* SEP 09/2010  *         TAMBIEN SE AJUSTA PARA EVITAR LA        *
REMARK* DATACREDITO  *         REPETICION DE DIRECCIONES PARA UNA MISMA*
REMARK*              *         IDENTIFICACION.                         *
REMARK******************************************************************
REMARK* CAV0014      * CALDYT - SE ADICIONA LAS VARIABLES PARA VALIDAR *
REMARK* C.VELASCO.   *          LOS CAMPOS EMAIL Y TELEFONO CELULAR.   *
REMARK* SEP 18/2012  *                                                 *
REMARK******************************************************************
REMARK* RMB0015      * FCRGCR- SE INTEGRA EL AJUSTE DE FECHAS DE       *
REMARK* R.MARTINEZ   *         DIRECCIONES DE CUENTAS CERRADAS         *
REMARK* OCT 16/2012  *         PARA IGUALAR CON LA RUTINA BDLDI2       *
REMARK******************************************************************
      *
FCRGCR 01  FECHAS-BDLDI2.                                               RMB0015
FCRGCR     02 FEC-AAAAMM-BDLDI2              PIC 9(06).                 RMB0015
FCRGCR     02 FEC-MMAA-BDLDI2                PIC 9(04).                 RMB0015
FCRGCR     02 CTA-ACTIVA                     PIC 9(01).                 RMB0015
RCNCER 01 PARAMETROS-BDLDIR.                                            RMB0006 
RCNCER    02 POLITICA-EXC-CAL-DIR-BDLDIR    PIC 9(02) VALUE 0.          RMB0006 
RCNCER    02 POLITICA-EXC-MAL-DIR-BDLDIR    PIC 9(02) VALUE 1.          RMB0006 
RCNCER*** PARAMETROS DE INDICADOR DE DIRECCION CERTIFICADA              RMB0006 
RCNCER    02 DIRCER-BDLDIR                  PIC 9(01).                  RMB0006 
RCNCER*** PARAMETROS DE PUNTOS ADICIONALES                              RMB0006 
RCNCER    02 PTSADI-BDLDIR                  PIC 9(02).                  RMB0006 
RCNCER*** PARANETRO DE CIUDAD ORIGINAL                                  RMB0006 
RCNCER    02 CIUDAD-BDLDIR                  PIC X(20).                  RMB0006 
RCNCER 01 VARIABLES-CIU.                                                RMB0006 
RCNCER    02 I-CIU-BDLDIR                   PIC 9(04).                  RMB0006 
RCNCER    02 J-CIU-BDLDIR                   PIC 9(04).                  RMB0006 
RCNCER    02 CIUDAD-ESTANDAR-CIU-BDLDIR     PIC X(30).                  RMB0006 
RCNCER    02 PATRON-CIU-BDLDIR              PIC 9(03) VALUE 0.          RMB0006 
RCNCER    02 CAMPO-CIU-BDLDIR               PIC X(30) VALUE SPACES.     RMB0006 
RCNCER    02 CODIGO-DANE-CIU-BDLDIR         PIC X(08) VALUE SPACES.     RMB0006 
RCNCER    02 CODIGO-DANE-N-CIU-BDLDIR       REDEFINES                   RMB0006 
RCNCER       CODIGO-DANE-CIU-BDLDIR         PIC 9(08).                  RMB0006 
RCNCER    02 LLAVE-AUX21-CIU-BDLDIR.                                    RMB0006 
RCNCER       03 FILLER                      PIC X(01) VALUE SPACES.     RMB0006 
RCNCER       03 LLAVE-AUX-CIU-BDLDIR        PIC X(20) VALUE SPACES.     RMB0006 
RCNCER    02 OPEN-BDLSTDCIU-BDLDIR          PIC 9(01) VALUE 0.          RMB0006 
RCNCER    02 BASURA-CIU-BDLDIR              PIC X(01) VALUE SPACES.     RMB0006 
TELEXC 01 VARIABLES-TELEXC.                                             HGO0008
TELEXC     03  W-OCURR                      PIC 99999.                  HGO0008
TELEXC     03  W-I                          PIC 99999.                  HGO0008
TELEXC     03  W-POSIUNI                    PIC 99999.                  HGO0008
TELEXC     03  W-TELANT                     PIC X(12).                  HGO0008
TELEXC     03  W-TELDES                     PIC X(12).                  HGO0008
TELEXC     03  W-POSANT                     PIC 99999.                  HGO0008
TELEXC     03  W-POSDES                     PIC 99999.                  HGO0008
TELEXC     03  W-POSRC                      PIC 99999.                  HGO0008
TELEXC     03  SW-1                         PIC 9.                      HGO0008
TELEXC     03  SW-2                         PIC 9.                      HGO0008
TELEXC     03  SW-ENCUENTRA-PI              PIC 9.                      HGO0008
TELEXC     03  W-POSI                       PIC 99999.                  HGO0008
TELEXC     03  LONGCOMP-EXCLU               PIC 99999.                  HGO0008
TELEXC     03  POSI-EXCLU                   PIC 99999.                  HGO0008
TELEXC     03  IQ                           PIC 99999.                  HGO0008
TELEXC 01  TELS-EXCLU.                                                  HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     0000000".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1111111".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1234546".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1475632".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1477500".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1756874".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1975090".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1978030".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1985110".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1986090".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1992070".HGO0008
TELEXC     03 FILLER                     PIC X(12) VALUE "     1993020".HGO0008
TEXTEL     03 FILLER                     PIC X(12) VALUE "     1995091".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TEXTEL     03 FILLER                     PIC X(12) VALUE "            ".HGO0010
TELEXC 01  FILLER                        REDEFINES TELS-EXCLU.          HGO0008
TEXTEL*TELEXC     02 T-TELS-EXCLU               PIC X(12) OCCURS 12 TIMEHGO0010
TEXTEL     02 T-TELS-EXCLU               PIC X(12) OCCURS 24 TIMES.     HGO0010
RCNCER 01 VARIABLES-BDLDIR.                                             RMB0006 
RCNCER    03 TEL-OK-BDLDIR                  PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 TABLA-TELFINALES-BDLDIR.                                   RMB0006 
RCNCER       04 T-TELFINALES-BDLDIR         OCCURS 100 TIMES.           RMB0006 
TELEXC*         05 V-TELFIN-BDLDIR          PIC X(12).                  HGO0008
TELEX9*         05 V-TELFIN-BDLDIR.                                     RGH0009
TELEX9          05 V-TELFIN-BDLDIR          PIC X(12).                  RGH0009
TELEX9          05 V-TELFIN-BDLDIR-R        REDEFINES                   RGH0009
TELEX9             V-TELFIN-BDLDIR.                                     RGH0009
TELEXC             10 V-TELFIN-BDLDIR-1     PIC X(05).                  HGO0008
TELEXC             10 V-TELFIN-BDLDIR-2     PIC X(07).                  HGO0008
RCNCER          05 V-CIUTELFIN-BDLDIR       PIC X(20).                  RMB0006 
RCNCER          05 V-FECTELFIN-BDLDIR       PIC 9(06).                  RMB0006 
RCNCER          05 V-TIPTELFIN-BDLDIR       PIC X(01).                  RMB0006 
RCNCER          05 V-NUMTELFIN-BDLDIR       PIC 9(03).                  RMB0006 
RCNCER          05 V-SUSTELFIN-BDLDIR       PIC 9(06).                  RMB0006 
RCNCER          05 V-HASTELFIN-BDLDIR       PIC X(50).                  RMB0006 
RCNCER          05 V-VECTELFIN-BDLDIR       PIC X(50).                  RMB0006 
RCNCER          05 V-CALTELFIN-BDLDIR       PIC 9(03).                  RMB0006 
TELEXC          05 V-FECULTTELFIN-BDLDIR    PIC 9(06).                  HGO0008
RCNCER    03 TABLA-TEL-BDLDIR.                                          RMB0006 
RCNCER       04 T-TEL-BDLDIR                OCCURS 100 TIMES.           RMB0006 
RCNCER          05 V-TELEFONO-BDLDIR        PIC X(12).                  RMB0006 
RCNCER          05 V-CIUDADTEL-BDLDIR       PIC X(20).                  RMB0006 
RCNCER          05 V-TIPOTEL-BDLDIR         PIC X(15).                  RMB0006 
RCNCER          05 V-FECHATEL-BDLDIR        PIC 9(06).                  RMB0006 
RCNCER          05 V-SUSTEL-BDLDIR          PIC 9(06).                  RMB0006 
RCNCER          05 V-HASTEL-BDLDIR          PIC X(50).                  RMB0006 
TELEXC          05 V-FECULTTEL-BDLDIR       PIC 9(06).                  HGO0008
RCNCER    03 NUMTEL-BDLDIR                  PIC X(12).                  RMB0006 
RCNCER    03 DIR-OK-BDLDIR                  PIC 9(05).                  RMB0006 
RCNCER    03 TABLA-DIRFINALES-BDLDIR.                                   RMB0006 
RCNCER       04 T-DIRFINALES-BDLDIR         OCCURS 100 TIMES.           RMB0006 
RCNCER          05 V-DIRFIN-BDLDIR          PIC X(60).                  RMB0006 
PYSPAR          05 V-PRIPARTFIN-BDLDIR      PIC X(60).                  HGO0011
PYSPAR          05 V-SEGPARTFIN-BDLDIR      PIC X(60).                  HGO0011
RCNCER          05 V-CIUFIN-BDLDIR          PIC X(20).                  RMB0006 
RCNCER          05 V-TIPFIN-BDLDIR          PIC X(01).                  RMB0006 
RCNCER          05 V-FECFIN-BDLDIR          PIC 9(06).                  RMB0006 
RCNCER          05 V-NUMFIN-BDLDIR          PIC 9(03).                  RMB0006 
RCNCER          05 V-SUSFIN-BDLDIR          PIC 9(06).                  RMB0006 
RCNCER          05 V-HASDIRFIN-BDLDIR       PIC X(100).                 RMB0006 
RCNCER          05 V-PRIDIGFIN-BDLDIR       PIC 9(10).                  RMB0006 
RCNCER          05 V-VECFIN-BDLDIR          PIC X(12).                  RMB0006 
RCNCER          05 V-STAFIN-BDLDIR          PIC X(60).                  RMB0006 
RCNCER          05 V-CALFIN-BDLDIR          PIC 9(03).                  RMB0006 
PYSPAR          05 V-CALFIN-PART1-BDLDIR    PIC 9(03).                  HGO0011
PYSPAR          05 V-CALFIN-PART2-BDLDIR    PIC 9(03).                  HGO0011
UNIDIR          05 V-FECULTFIN-BDLDIR       PIC 9(06).                  HGO0008
UNIDIR          05 V-NUENOMFIN-BDLDIR       PIC 9(01).                  HGO0008
UNIDIR          05 V-NUMDIRFIN-BDLDIR       PIC X(60).                  HGO0008
UNIDIR    03 TABLA-DIRFINALESFIN-BDLDIR.                                HGO0008
UNIDIR       04 T-DIRFINALESFIN-BDLDIR      OCCURS 100 TIMES.           HGO0008
UNIDIR          05 V-DIRFINFIN-BDLDIR       PIC X(60).                  HGO0008
PYSPAR          05 V-PRIPARTFINFIN-BDLDIR   PIC X(60).                  HGO0011
PYSPAR          05 V-SEGPARTFINFIN-BDLDIR   PIC X(60).                  HGO0011
UNIDIR          05 V-CIUFINFIN-BDLDIR       PIC X(20).                  HGO0008
UNIDIR          05 V-TIPFINFIN-BDLDIR       PIC X(01).                  HGO0008
UNIDIR          05 V-FECFINFIN-BDLDIR       PIC 9(06).                  HGO0008
UNIDIR          05 V-NUMFINFIN-BDLDIR       PIC 9(03).                  HGO0008
UNIDIR          05 V-SUSFINFIN-BDLDIR       PIC 9(06).                  HGO0008
UNIDIR          05 V-HASDIRFINFIN-BDLDIR    PIC X(100).                 HGO0008
UNIDIR          05 V-PRIDIGFINFIN-BDLDIR    PIC 9(10).                  HGO0008
UNIDIR          05 V-VECFINFIN-BDLDIR       PIC X(12).                  HGO0008
UNIDIR          05 V-STAFINFIN-BDLDIR       PIC X(60).                  HGO0008
UNIDIR          05 V-CALFINFIN-BDLDIR       PIC 9(03).                  HGO0008
PYSPAR          05 V-CALFINFIN-PART1-BDLDIR PIC 9(03).                  HGO0011
PYSPAR          05 V-CALFINFIN-PART2-BDLDIR PIC 9(03).                  HGO0011
UNIDIR          05 V-FECULTFINFIN-BDLDIR    PIC 9(06).                  HGO0008
UNIDIR          05 V-NUENOMFINFIN-BDLDIR    PIC 9(01).                  HGO0008
UNIDIR          05 V-NUMDIRFINFIN-BDLDIR    PIC X(60).                  HGO0008
UNIDIR    03 T-DIRFINALESAUX-BDLDIR.                                    HGO0008
UNIDIR       04 V-DIRFINAUX-BDLDIR          PIC X(60).                  HGO0008
PYSPAR       04 V-PRIPARTFINAUX-BDLDIR      PIC X(60).                  HGO0011
PYSPAR       04 V-SEGPARTFINAUX-BDLDIR      PIC X(60).                  HGO0011
UNIDIR       04 V-CIUFINAUX-BDLDIR          PIC X(20).                  HGO0008
UNIDIR       04 V-TIPFINAUX-BDLDIR          PIC X(01).                  HGO0008
UNIDIR       04 V-FECFINAUX-BDLDIR          PIC 9(06).                  HGO0008
UNIDIR       04 V-NUMFINAUX-BDLDIR          PIC 9(03).                  HGO0008
UNIDIR       04 V-SUSFINAUX-BDLDIR          PIC 9(06).                  HGO0008
UNIDIR       04 V-HASDIRFINAUX-BDLDIR       PIC X(100).                 HGO0008
UNIDIR       04 V-PRIDIGFINAUX-BDLDIR       PIC 9(10).                  HGO0008
UNIDIR       04 V-VECFINAUX-BDLDIR          PIC X(12).                  HGO0008
UNIDIR       04 V-STAFINAUX-BDLDIR          PIC X(60).                  HGO0008
UNIDIR       04 V-CALFINAUX-BDLDIR          PIC 9(03).                  HGO0008
PYSPAR       04 V-CALFINAUX-PART1-BDLDIR    PIC 9(03).                  HGO0011
PYSPAR       04 V-CALFINAUX-PART2-BDLDIR    PIC 9(03).                  HGO0011
UNIDIR       04 V-FECULTFINAUX-BDLDIR       PIC 9(06).                  HGO0008
UNIDIR       04 V-NUENOMFINAUX-BDLDIR       PIC 9(01).                  HGO0008
UNIDIR       04 V-NUMDIRFINAUX-BDLDIR       PIC X(60).                  HGO0008
RCNCER    03 TABLA-DIR-BDLDIR.                                          RMB0006 
RCNCER       04 T-DIR-BDLDIR                OCCURS 100 TIMES.           RMB0006 
RCNCER          05 T-NUMDIR-BDLDIR .                                    RMB0006 
RCNCER             06 V-NUMDIR-BDLDIR       OCCURS 10 TIMES PIC 9(5).   RMB0006 
RCNCER          05 V-CIUDADDIR-BDLDIR       PIC X(20).                  RMB0006 
RCNCER          05 V-DIRECCION-BDLDIR       PIC X(60).                  RMB0006 
RCNCER          05 V-TIPODIR-BDLDIR         PIC X(01).                  RMB0006 
RCNCER          05 V-FECHADIR-BDLDIR        PIC 9(06).                  RMB0006 
RCNCER          05 V-SUSDIR-BDLDIR          PIC 9(06).                  RMB0006 
RCNCER          05 V-HASDIR-BDLDIR          PIC X(100).                 RMB0006 
RCNCER          05 V-PRIDIG-BDLDIR          PIC 9(10).                  RMB0006 
UNIDIR          05 V-FECULT-BDLDIR          PIC 9(06).                  HGO0008
UNIDIR          05 V-NUENOM-BDLDIR          PIC 9(01) VALUES 0.         HGO0008
RCNCER    03   TABLA-FECHADIR-AUX-BDLDIR.                               RMB0006 
RCNCER          04 O-FECHADIR-AUX-BDLDIR    OCCURS 100 TIMES.           RMB0006 
RCNCER             05 V-FECHADIR-AUX-BDLDIR PIC 9(06).                  RMB0006 
RCNCER    03 FECHA-RUTINA-BDLDIR.                                       RMB0006 
RCNCER       05 ANO-RUTINA-BDLDIR           PIC 9(04).                  RMB0006 
RCNCER       05 MES-RUTINA-BDLDIR           PIC 9(02).                  RMB0006 
RCNCER    03 VEC-HIS-BDLDIR                 PIC X(12).                  RMB0006 
RCNCER    03 NUMDIR-BDLDIR                  PIC X(50).                  RMB0006 
PYSPAR    03 DIRECCION-BDLDIR               PIC X(60).                  HGO0011
RCNCER    03 T-CARDIR-BDLDIR.                                           RMB0006 
RCNCER       04 V-CARDIR-BDLDIR             OCCURS 60 TIMES PIC X(01).  RMB0006 
RCNCER    03 T-CARTOT-BDLDIR.                                           RMB0006 
RCNCER       04 V-CARTOT-BDLDIR             OCCURS 10 TIMES PIC 9(05).  RMB0006 
RCNCER    03 CAR-ANT-BDLDIR                 PIC X(01).                  RMB0006 
RCNCER    03 CAR-ACT-BDLDIR                 PIC 9(01).                  RMB0006 
RCNCER    03 FECHA-MEJOR-BDLDIR             PIC 9(06) VALUE 0.          RMB0006 
RCNCER    03 IND-DIRCER-BDLDIR              PIC 9(01) VALUE 0.          RMB0006 
UNIDIR    03 COMPO                          PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 RMAYOR                         PIC 9(01) VALUE 0.          HGO0008
UNIDIR    03 CAMBIO                         PIC 9(01) VALUE 0.          HGO0008
UNIDIR    03 NN                             PIC 9(01) VALUE 0.          HGO0008
UNIDIR    03 CONTA                          PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 CONTA1                         PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 DIRECCION                      PIC X(60) VALUE SPACES.     HGO0008
UNIDIR    03 DIRECCION1                     PIC X(60) VALUE SPACES.     HGO0008
UNIDIR    03 DIRECCION2                     PIC X(60) VALUE SPACES.     HGO0008
UNIDIR    03 E-BDLDIR                       PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 F-BDLDIR                       PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 G-BDLDIR                       PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 H-BDLDIR                       PIC 9(05) VALUE 0.          HGO0008
RCNCER    03 I-BDLDIR                       PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 J-BDLDIR                       PIC 9(05) VALUE 0.          RMB0006 
PYSPAR    03 J1-BDLDIR                      PIC 9(05) VALUE 0.          HGO0011
PYSPAR    03 J2-BDLDIR                      PIC 9(05) VALUE 0.          HGO0011
PYSPAR    03 CONTADOR-TEMPORAL-BDLDIR       PIC 9(03).                  HGO0011
RCNCER    03 M-BDLDIR                       PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 N-BDLDIR                       PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 K-BDLDIR                       PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 II-BDLDIR                      PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 JJ-BDLDIR                      PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 NUM-OK-SEND-BDLDIR             PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 I-STA-BDLDIR                   PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 J-STA-BDLDIR                   PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 K-STA-BDLDIR                   PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 II-STA-BDLDIR                  PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 JJ-STA-BDLDIR                  PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 KK-STA-BDLDIR                  PIC 9(05) VALUE 0.          RMB0006 
RCNCER    03 CALIFICA-DIR-BDLDIR            PIC 9(03) VALUE 0.          RMB0006 
PYSPAR    03 CON-PART-ESTANDARIZADA-BDLDIR  PIC 9(03) VALUE 0.          HGO0011
PYSPAR    03 CALIFICA-DIRPART1-BDLDIR       PIC 9(03) VALUE 0.          HGO0011
PYSPAR    03 CALIFICA-DIRPART2-BDLDIR       PIC 9(03) VALUE 0.          HGO0011
RCNCER    03 CALIFICA-REAL-BDLDIR           PIC 9(03) VALUE 0.          RMB0006 
RCNCER    03 CAMPOS-N-DIR-BDLDIR            PIC 9(03) VALUE 0.          RMB0006 
RCNCER    03 COSI-DIR-BDLDIR                PIC 9(09) VALUE 0.          RMB0006 
RCNCER    03 RESI-DIR-BDLDIR                PIC 9(09) VALUE 0.          RMB0006 
RCNCER    03 TELE-DIR-BDLDIR                PIC 9(12) VALUE 0.          RMB0006 
RCNCER*** NOMBRE DEL ARCHIVO DE DIRECCIONES CERTIFICADAS                RMB0006 
RCNCER    03 TITLE-BDLSTDCER                PIC X(80) VALUE             RMB0006 
RCNCER       "$DATABASE/BDLSTDCER".                                     RMB0006 
RCNCER*** LA SIGUIENTE VARIBALE INDICA EL TIPO DE DATO EN PROCESO ASI:  RMB0006 
RCNCER*** R - RESIDENCIA                                                RMB0006 
RCNCER*** L - LABORAL                                                   RMB0006 
RCNCER*** C - CORRESPONDENCIA                                           RMB0006 
RCNCER    03 IND-DATO-BDLDIR                PIC X(01).                  RMB0006 
RCNCER*** VARIABLE QUE CONTIENE LA SUMATORIA DE LOS DIGITOS DE LAS      RMB0006 
RCNCER*** PARTICULAS NUMERICAS DE UNA DIRECCION                         RMB0006 
RCNCER    03 SUM-DIGITOS-BDLDIR             PIC 9(10).                  RMB0006 
RCNCER    03 CAL-TEL-BDLDIR                 PIC 9(03) VALUE 0.          RMB0006 
RCNCER    02 TELEFONO-BDLDIR                PIC 9(12) VALUE 0.          RMB0006 
RCNCER***************************VARIABLES DE STANDARIZAR************** RMB0006 
RCNCER 01 DIRACC-BDLDIR-BDLDIR.                                         RMB0006 
RCNCER    03 VALIDACION-DIR-OUT-BDLDIR      PIC 9(01).                  RMB0006 
RCNCER    03 PRIVILEGIO-INPUT-BDLDIR        PIC 9(01).                  RMB0006 
RCNCER    03 NIT-CONSULTANTE-INPUT-BDLDIR   PIC 9(11).                  RMB0006 
RCNCER 01  FDA-DIREST-BDLDIR                PIC 9(01).                  RMB0006 
RCNCER 01  IND-TIPCAR-BDLDIR                PIC X(01).                  RMB0006 
RCNCER 01  TIPCAR-WS-BDLDIR                 PIC X(01).                  RMB0006 
RCNCER 01  ENC-DIREST-BDLDIR                PIC 9(01).                  RMB0006 
RCNCER*01 TABLA-DIR-ESTANDAR-BDLDIR         OCCURS 500 TIMES            RMB0006
AMP013 01 TABLA-DIR-ESTANDAR-BDLDIR         OCCURS 2000 TIMES           PJV0013
RCNCER    ASCENDING KEY IS DIR-ORIGINAL-BDLDIR                          RMB0006 
RCNCER    INDEXED   BY  I-DIR-ESTANDAR-BDLDIR.                          RMB0006 
RCNCER    03 DIR-ORIGINAL-BDLDIR            PIC X(20).                  RMB0006 
RCNCER    03 DIR-ESTANDAR-BDLDIR            PIC X(10).                  RMB0006 
RCNCER* 01 TABLA-DIR-COMPUESTAS-BDLDIR.                                 RMB0006 
RCNCER*    03 DIR-COMPUESTAS-AST-BDLDIR      PIC X(20) OCCURS 90 TIMES. RMB0006 
RCNCER*    03 DIR-COMPUESTAS-BLN-BDLDIR      PIC X(20) OCCURS 90 TIMES. RMB0006 
RCNCER*    03 DIR-COMPUESTAS-LON-BDLDIR      PIC 9(02) OCCURS 90 TIMES. RMB0006 
RCNCER*    03 DIR-COMPUESTAS-TIP-BDLDIR      PIC 9(01) OCCURS 90 TIMES. RMB0006
AMP013 01 TABLA-DIR-COMPUESTAS-BDLDIR.                                  PJV0013
AMP013    03 DIR-COMPUESTAS-AST-BDLDIR      PIC X(20) OCCURS 500 TIMES. PJV0013
AMP013    03 DIR-COMPUESTAS-BLN-BDLDIR      PIC X(20) OCCURS 500 TIMES. PJV0013
AMP013    03 DIR-COMPUESTAS-LON-BDLDIR      PIC 9(02) OCCURS 500 TIMES. PJV0013
AMP013    03 DIR-COMPUESTAS-TIP-BDLDIR      PIC 9(01) OCCURS 500 TIMES. PJV0013
RCNCER 01 TIP-NUEVA-DIRECCION-BDLDIR.                                   RMB0006 
RCNCER    03 TIP-NVA-DIR-BDLDIR             PIC X(01) OCCURS 60 TIMES.  RMB0006 
RCNCER 01 NUEVA-DIRECCION-BDLDIR.                                       RMB0006 
RCNCER    03 NVA-DIR-BDLDIR                 PIC X(60) OCCURS 60 TIMES.  RMB0006 
RCNCER 01 DIRECCION-A-ESTANDARIZAR-BDLDIR.                              RMB0006 
RCNCER    03 DIR-A-EST-BDLDIR               PIC X(01) OCCURS 60 TIMES.  RMB0006 
CALDYT 01 DIR-A-STD-BDLDIR REDEFINES DIRECCION-A-ESTANDARIZAR-BDLDIR.   CAV0014
CALDYT    02 FILLER PIC X(60).                                          CAV0014
RCNCER*** VARIABLES QUE CONTIENE LA CIUDAD ORIGINAL                     RMB0006 
RCNCER 01 CIUDAD-RES-A-ESTANDAR-BDLDIR      PIC X(20).                  RMB0006 
RCNCER 01 CIUDAD-LAB-A-ESTANDAR-BDLDIR      PIC X(20).                  RMB0006 
RCNCER 01 CIUDAD-COR-A-ESTANDAR-BDLDIR      PIC X(20).                  RMB0006 
RCNCER 01 PART-A-ESTANDARIZAR-BDLDIR.                                   RMB0006 
RCNCER    03 PART-A-ESTAN-BDLDIR            PIC X(01) OCCURS 60 TIMES.  RMB0006 
RCNCER 01 PART-ESTANDARIZADA-BDLDIR         PIC X(60).                  RMB0006 
PYSPAR 01 PRIPART-ESTANDARIZADA-BDLDIR      PIC X(60).                  HGO0011
PYSPAR 01 SEGPART-ESTANDARIZADA-BDLDIR      PIC X(60).                  HGO0011
RCNCER 01 DIRECCION-ESTANDARIZADA-BDLDIR    PIC X(60).                  RMB0006 
RCNCER 01 DIRECCION-AUX-BDLDIR              PIC X(60).                  RMB0006 
RCNCER 01 DIRECCION-62-BDLDIR.                                          RMB0006 
RCNCER    03 FILLER                         PIC X(01) VALUE SPACES.     RMB0006 
RCNCER    03 DIRECCION-60-BDLDIR            PIC X(60).                  RMB0006 
RCNCER    03 FILLER                         PIC X(01) VALUE SPACES.     RMB0006 
RCNCER 01 DIR-COMPUESTAS-BLN-AUX-BDLDIR     PIC X(20) VALUE SPACES.     RMB0006 
RCNCER 01 DIR-COMPUESTAS-AST-AUX-BDLDIR     PIC X(20) VALUE SPACES.     RMB0006 
RCNCER 01 CASO-NUMEROLETRA-BDLDIR           PIC 9(01) VALUE 0.          RMB0006 
NWDIRB 01 VAR-RUTINA-NEWDIRBOG-BDLDIR.                                  GJC0007 
NWDIRB    02 LETRA-BDLDIR             PIC X.                            GJC0007 
NWDIRB    02 ORDEN-ASCII-BDLDIR       PIC 9(03) VALUE 0.                GJC0007 
NWDIRB    02 TOTAL-ASCII-BDLDIR       PIC 9(10).                        GJC0007 
NWDIRB    02 NN8-BDLDIR               PIC 9(05).                        GJC0007 
NWDIRB    02 NN9-BDLDIR               PIC 9(05).                        GJC0007 
NWDIRB    02 NN10-BDLDIR              PIC 9(05).                        GJC0007 
NWDIRB    02 I-MEJ-BDLDIR          PIC 9(3) VALUE 0.                    GJC0007 
NWDIRB    02 J-MEJ-BDLDIR          PIC 9(3) VALUE 0.                    GJC0007 
NWDIRB    02 K-MEJ-BDLDIR          PIC 9(3) VALUE 0.                    GJC0007 
NWDIRB*   02 CAR-ANT-BDLDIR        PIC X(1) VALUE SPACES.               GJC0007 
NWDIRB*   02 CAR-ACT-BDLDIR        PIC 9(1) VALUE 0.                    GJC0007 
NWDIRB    02 T-L-BDLDIR.                                                GJC0007 
NWDIRB       03 L-BDLDIR OCCURS 30 TIMES PIC X(20).                     GJC0007 
NWDIRB    02 T-NUMMEJ-BDLDIR.                                           GJC0007 
NWDIRB       04 V-NUMMEJ-BDLDIR    OCCURS 10 TIMES PIC 9(5).            GJC0007 
NWDIRB    02 T-CARMEJ-BDLDIR.                                           GJC0007 
NWDIRB       04 V-CARMEJ-BDLDIR OCCURS 60 TIMES PIC X.                  GJC0007 
NWDIRB    02 T-LETRAS-BDLDIR.                                           GJC0007 
NWDIRB       04 V-LETRAS-BDLDIR OCCURS 200 TIMES PIC X.                 GJC0007 
NWDIRB 01 VAR-NEWDIR.                                                   GJC0007 
NWDIRB    02 OPEN-BDLNEWDIRBOG-BDLDIR     PIC 9 VALUE 0.                GJC0007 
NWDIRB    02 HAY-BDLNEWDIRBOG-BDLDIR      PIC 9 VALUE 0.                GJC0007 
NWDIRB    02 CIUDAD-BTA-BDLDIR            PIC X(15).                    GJC0007 
NWDIRB    02 DIRECCION-BTA-BDLDIR         PIC X(60).                    GJC0007
NN0012    02 SW-NN                        PIC 9 VALUE 0.                PJV0012
CALDYT 01 VAR-EMAIL-BDLDIR.                                             CAV0014
CALDYT    02 EMAIL-BDLDIR                 PIC X(60) VALUE SPACES.       CAV0014
CALDYT    02 TMP-EMAIL-BDLDIR             PIC X(60) VALUE SPACES.       CAV0014
CALDYT    02 ERR-EMAIL-BDLDIR             PIC 9(01) VALUE 0.            CAV0014
CALDYT    02 CONT-ESP-INIC-BDLDIR         PIC 9(4) VALUE ZEROS.         CAV0014
CALDYT    02 CONT-ARO-BDLDIR              PIC 9(4) VALUE ZEROS.         CAV0014
CALDYT    02 CONT-PUN-BDLDIR              PIC 9(4) VALUE ZEROS.         CAV0014
CALDYT    02 CONT-ERR-BDLDIR              PIC 9(4) VALUE ZEROS.         CAV0014
CALDYT    02 LONG-EMIAL-BDLDIR            PIC 9(4) VALUE ZEROS.         CAV0014
CALDYT 01 VAR-CELULAR-BDLDIR.                                           CAV0014
CALDYT    02 TEL-CELULAR-BDLDIR           PIC 9(12).                    CAV0014
CALDYT    02 ERR-CELULAR-BDLDIR           PIC 9(01) VALUE 0.            CAV0014
CALDYT    02 TEL-AUX-CELULAR-BDLDIR       PIC 9(12).                    CAV0014
CALDYT    02 FILLER REDEFINES TEL-AUX-CELULAR-BDLDIR.                   CAV0014
CALDYT       04 P1-TEL-AUX-CEL-BDLDIR     PIC 9(2).                     CAV0014
CALDYT       04 P2-TEL-AUX-CEL-BDLDIR     PIC 9(3).                     CAV0014
CALDYT          88 ES-CELULAR-CEL-BDLDIR VALUES 300,301,   *>TIGO       CAV0014
CALDYT                                     310,311,312,313,314,320,321, CAV0014
CALDYT                                     315,316,317,  *>MOVISTAR     CAV0014
CALDYT                                     350,          *>AVANTEL      CAV0014
CALDYT                                     302,303,304,305,306,307,308, CAV0014
CALDYT                                     309,318,319,322,323,324,325, CAV0014
CALDYT                                     351,352,353,354,355. *>ASIGN CAV0014
CALDYT     04 P3-TEL-AUX-CEL-BDLDIR        PIC 9(7).                    CAV0014
