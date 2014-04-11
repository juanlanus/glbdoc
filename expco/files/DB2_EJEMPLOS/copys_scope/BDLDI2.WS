REMARK*BDLDI2-WS.v.0022
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
REMARK* SEP. 27/2009  *          LUEGO UNIFICAR TENIENDO EN CUENTA ESTE*
REMARK* INTERSCHEMES. *          CRITERIO                              *
REMARK******************************************************************
REMARK* HGO0012       * RECPF2 - AJUSTE VALIDACION TABLAS DE ACEPCIONES*
REMARK* H. GARCIA O.  *          Y EXCLUSIONES                         *
REMARK* SEP. 17/2009  *                                                *
REMARK* INTERSCHEMES. *                                                *
REMARK******************************************************************
REMARK******************************************************************
REMARK* RGH0013       * MOD013 - ESTANDARIZACION DE MANEJO DE ARCHIVOS *
REMARK* R. GALVIS H.  *          BDLSTDCIU                             *
REMARK* ENE. 01/2010  *          BDLDIRACE                             *
REMARK* INTERSCHEMES. *          BDLMAIEXC                             *
REMARK*               *          BDLTELEXC                             *
REMARK******************************************************************
REMARK* RMB0014       * CELU01 - INCLUIR LOS CELULARES DEL ARCHIVO     *
REMARK* R. MARTINEZ.  *          DE DIRECCIONES EN LA TABLA DE CELULAR *
REMARK* MAR. 25/2010  *                                                *
REMARK******************************************************************
REMARK* PJV0015       * CELU02 - INCLUIR INDICATIVO DE CELULARES QUE   *
REMARK* P. VARGAS  .  *          INICIEN POR 314.                      *
REMARK* MAY. 31/2010  *                                                *
REMARK******************************************************************
REMARK* PJV0016      * NN0016 - AJUSTE, PARA EVITAR QUE SE REPITAN     *
REMARK* P.VARGAS     *          DIRECCIONES EN EL MISMO REPORTE CUNADO *
REMARK* JUL 01/2010  *          SE TRATAN DE NUEVAS NOMENCLATURAS EN   *
REMARK* DATACREDITO  *          BOGOTA.                                *
REMARK******************************************************************
REMARK* PJV0017      * AMP017 -SE AMPLIAN LOS LIMITES DE LAS TABLAS DE *
REMARK* P.VARGAS     *         ACEPCIONES QUE SE CARGAN EN MEMORIA.    *
REMARK* SEP 09/2010  *         TAMBIEN SE AJUSTA PARA EVITAR LA        *
REMARK* DATACREDITO  *         REPETICION DE DIRECCIONES PARA UNA MISMA*
REMARK*              *         IDENTIFICACION.                         *
REMARK******************************************************************
      * FARF018      * PLNCHQ - SE AINTEGRA LAS REGLAS DESARROLLADAS   *  
      * Jul.17-2012  *  DESARROLLADAS POR RMB PARA EL PLAN DE CHOQUE.  *
      * FRANKLIN.R.  *                                                 *
      ******************************************************************
      * FARF019       * AJCIUU - SE AINTEGRA EL AJUSTE DESARROLLADO POR 
      * Sep.11-2012   * RMB QUE CAMBIA LA BUSQUEDA DEL CIU PARA QUE NO 
      * FRANKLIN.R.   * FALLE CUANDO SE UTILIZA FUERA DE LA RUTINA 
      *               * PRINCIPAL.
      ******************************************************************
REMARK* ABB0020      * NCAM18 -SE CREAN NUEVOS CAMPOS EN TRAMAS:       *
REMARK* ALBEIRO      *         ZONA                ZONADIR             *
REMARK* BERNAL       *         FECHA DE CREACION   FECCRE              *
REMARK* Oct 04/2012  *         NUMERO DE ENTIDADES NUMENT              *
REMARK*              *                                                 *
REMARK*              * FCRGCR- SE INTEGRA EL AJUSTE DE FECHAS DE       *
      *              *  DIRECCIONES DE CUENTAS CERRADAS DESARROLLADO   *
      *              *  POR RMB.                                       *
      *
REMARK*              * NCAM19 -SE CREA NUEVOS CAMPOS:                  *
REMARK*              *      TIPODI = 1=Res,2=Lab,4=Cor,3=Res+Lab       *
REMARK*              *               5=Res+Cor, 6=Lab+Cor,7=Res+Lab+Cor*
REMARK*              *      TIPOTE = 1=Res,2=Lab,3=Res+Lab             *
REMARK******************************************************************
      * FARF021       * AJCONT - SE AINTEGRA EL AJUSTE DESARROLLADO POR 
      * Nov.13-2012   * RMB QUE PERMITE HACER UNA CORRELACION ENTRE LAS  
      * FRANKLIN.R.   * DIRECCIONES FINALES Y LAS DIRECCIONES INCICIALES 
      *               * DE DONDE SALIERON.
      *               * SE AJUSTA EL TAMAÑO DEL CORREO DE 36 A 40.
      ******************************************************************
REMARK* ABB0022      * NCAM22 -NUEVOS CAMPOS DESCRIPCION               *
REMARK* ALBEIRO      *                                                 *
REMARK* BERNAL       *                                                 *
REMARK*Nov 15/2012   * *                                               *
      ******************************************************************
      *
AJCONT 01  M-CONTEOS-TEX.                                               FARF021
AJCONT     02 V-CONTEOS-TEX OCCURS 100 TIMES.                           FARF021
AJCONT        03 V-TEX-CONTEO1                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO2                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO3                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO4                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO5                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO6                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO7                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO8                  PIC X(30).              FARF021
AJCONT        03 V-TEX-CONTEO9                  PIC X(30).              FARF021
      *
AJCONT 01  M-CONTEOS-NUM.                                               FARF021
AJCONT     02 V-CONTEOS-NUM OCCURS 100 TIMES.                           FARF021
AJCONT        03 V-NUM-CONTEO1                  PIC 9(18).              FARF021
AJCONT        03 V-NUM-CONTEO3                  PIC 9(18) .             FARF021
AJCONT        03 V-NUM-CONTEO4                  PIC 9(18) .             FARF021
AJCONT        03 V-NUM-CONTEO5                  PIC 9(18) .             FARF021
AJCONT        03 V-NUM-CONTEO6                  PIC 9(18) .             FARF021
AJCONT        03 V-NUM-CONTEO7                  PIC 9(18) .             FARF021
AJCONT        03 V-NUM-CONTEO8                  PIC 9(18) .             FARF021
AJCONT        03 V-NUM-CONTEO9                  PIC 9(18) .             FARF021
      
FCRGCR 01  FECHAS-BDLDI2.                                               ABB0020
FCRGCR     02 FEC-AAAAMM-BDLDI2              PIC 9(06).                 ABB0020
FCRGCR     02 FEC-MMAA-BDLDI2                PIC 9(04).                 ABB0020
FCRGCR     02 CTA-ACTIVA                     PIC 9(01).                 ABB0020
      *
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

NCAM18    02 CODANE-BDLDI2                  PIC 9(08).                  ABB0020
REPLUS    02 CODIGO-RES                     PIC 9(08).
REPLUS    02 CODIGO-LAB                     PIC 9(08).
REPLUS    02 CODIGO-COR                     PIC 9(08).
REPLUS    02 COD-DPTO                       PIC 9(02) VALUE 0.
REPLUS    02 FIN-DUPCIUDAD                  PIC 9(01) VALUE 0.
REPLUS    02 CON-DUPCIUDAD                  PIC 9(03) VALUE 1.
REPLUS    02 CON1-DUPCIUDAD                 PIC 9(03) VALUE 0.
REPLUS    02 DUPCIUDAD-BDLDIR               PIC X(20).
REPLUS    02 SW-DUPCIUDAD                   PIC 9(01) VALUE 0.

RCNCER    02 LLAVE-AUX21-CIU-BDLDIR.                                    RMB0006 
RCNCER       03 FILLER                      PIC X(01) VALUE SPACES.     RMB0006 
RCNCER       03 LLAVE-AUX-CIU-BDLDIR        PIC X(20) VALUE SPACES.     RMB0006 
RCNCER    02 OPEN-BDLSTDCIU-BDLDIR          PIC 9(01) VALUE 0.          RMB0006
MOD013    02 OPEN-BDLMAIEXC-BDLDIR          PIC 9(01) VALUE 0.          RGH0013
MOD013    02 OPEN-BDLTELEXC-BDLDIR          PIC 9(01) VALUE 0.          RGH0013
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
MOD013     03  FIN-BDLTELEXC-BDLDIR         PIC 9(01) VALUE ZEROS.      RGH0013
RECPF2*TELEXC 01  TELS-EXCLU.                                           HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     00HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "     10HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     11HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     12HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     14HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     14HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     17HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     19HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     19HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     19HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     19HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     19HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     19HGO0012
RECPF2*TEXTEL     03 FILLER                     PIC X(12) VALUE "     19HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "       HGO0012
RECPF2*TELEXC     03 FILLER                     PIC X(12) VALUE "       HGO0012
RECPF2*TELEXC 01  FILLER                        REDEFINES TELS-EXCLU.   HGO0012
RECPF2*TEXTEL*TELEXC     02 T-TELS-EXCLU               PIC X(12) OCCURS HGO0012
RECPF2*TEXTEL     02 T-TELS-EXCLU               PIC X(12) OCCURS 24 TIMEHGO0012
CELU01 01  FEC-AUX-BDLDIR                 PIC 9(06).                    RMB0014 
CELU01 01  TEL-AUX-BDLDIR                 PIC 9(12).                    RMB0014 
CELU01 01  FILLER REDEFINES TEL-AUX-BDLDIR.                             RMB0014 
CELU01     04 P1-TEL-AUX-BDLDIR           PIC 9(2).                     RMB0014 
CELU01     04 P2-TEL-AUX-BDLDIR           PIC 9(3).                     RMB0014
PLNCHQ        88 ES-CELULAR-BDLDIR 
PLNCHQ           VALUES 300,301,304,303,304,305,306,307,308,309, *>TIGO FARF032
PLNCHQ                  310,311,312,313,314,320,321,            *>CLARO FARF032
PLNCHQ                  315,316,317,318,                     *>MOVISTAR FARF032
PLNCHQ                  350.                                  *>AVANTEL FARF032
CELU01     04 P3-TEL-AUX-BDLDIIR          PIC 9(7).                     RMB0014 
      *
PLNCHQ*    
PLNCHQ*CELU01 88 ES-CELULAR-BDLDIR VALUES 300,301,   *>TIGO           RMB0014 
PLNCHQ*CELU02*                              310,311,312,313,313,320,321,PJV0015
PLNCHQ*CELU02                               310,311,312,313,314,320,321,PJV0015
PLNCHQ*CELU01                               315,316,317,  *>MOVISTAR    RMB0014 
PLNCHQ*CELU01                               350,          *>AVANTEL     RMB0014 
PLNCHQ*CELU01                               302,303,304,305,306,307,308,RMB0014 
PLNCHQ*CELU01                               309,318,319,322,323,324,325,RMB0014 
PLNCHQ*CELU01                               351,352,353,354,355. *>ASIGNRMB0014 
PLNCHQ*CELU01 4 P3-TEL-AUX-BDLDIIR          PIC 9(7).                   RMB0014
      *                                                                 FARF032 
PLNCHQ 01  VIAS-PRINCIPALES-BDLDIR.                                     FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "CL   ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "KR   ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "AV KR".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "TV   ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "AC   ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "AK   ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "AUTOP".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "AV   ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "CIR  ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "DG   ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ     02 FILLER       PIC X(5) VALUE "     ".                      FARF032 
PLNCHQ 01  FILLER REDEFINES VIAS-PRINCIPALES-BDLDIR.                    FARF032 
PLNCHQ     02 V-VIAS-PRIN-BDLDIR OCCURS 20 TIMES PIC X(5).              FARF032 
      *      
RCNCER 01  VARIABLES-BDLDIR.                                            RMB0006 
RCNCER     03 TEL-OK-BDLDIR                  PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 TABLA-TELFINALES-BDLDIR.                                  RMB0006 
RCNCER        04 T-TELFINALES-BDLDIR         OCCURS 100 TIMES.          RMB0006 
TELEXC*          05 V-TELFIN-BDLDIR          PIC X(12).                 HGO0008
TELEX9*          05 V-TELFIN-BDLDIR.                                    RGH0009
TELEX9           05 V-TELFIN-BDLDIR          PIC X(12).                 RGH0009
TELEX9           05 V-TELFIN-BDLDIR-R        REDEFINES                  RGH0009
TELEX9              V-TELFIN-BDLDIR.                                    RGH0009
TELEXC              10 V-TELFIN-BDLDIR-1     PIC X(05).                 HGO0008
TELEXC              10 V-TELFIN-BDLDIR-2     PIC X(07).                 HGO0008
RCNCER           05 V-CIUTELFIN-BDLDIR       PIC X(20).                 RMB0006 
RCNCER           05 V-FECTELFIN-BDLDIR       PIC 9(06).                 RMB0006 
RCNCER           05 V-TIPTELFIN-BDLDIR       PIC X(01).                 RMB0006 
RCNCER           05 V-NUMTELFIN-BDLDIR       PIC 9(03).                 RMB0006 
RCNCER           05 V-SUSTELFIN-BDLDIR       PIC 9(06).                 RMB0006 
RCNCER           05 V-HASTELFIN-BDLDIR       PIC X(50).                 RMB0006 
RCNCER           05 V-VECTELFIN-BDLDIR       PIC X(50).                 RMB0006 
RCNCER           05 V-CALTELFIN-BDLDIR       PIC 9(03).                 RMB0006 
TELEXC           05 V-FECULTTELFIN-BDLDIR    PIC 9(06).                 HGO0008
REPLUS           05 V-DEPTELFIN-BDLDIR       PIC X(20).                
NCAM18           05 V-FECCRETELFIN-BDLDIR    PIC 9(06).                 ABB0020
NCAM18           05 V-NUMENTTELFIN-BDLDIR    PIC 9(06).                 ABB0020
NCAM19           05 V-TIPOTEFIN-BDLDIR       PIC 9(01).                 ABB0020
AJCONT           05 V-SEC01TELFIN-BDLDIR     PIC 9(03).                 FARF021
AJCONT           05 V-SECUNI01TELFIN-BDLDIR  PIC 9(03).                 FARF021
      *
RCNCER     03 TABLA-TEL-BDLDIR.                                         RMB0006 
RCNCER        04 T-TEL-BDLDIR                OCCURS 100 TIMES.          RMB0006 
RCNCER           05 V-TELEFONO-BDLDIR        PIC X(12).                 RMB0006 
RCNCER           05 V-CIUDADTEL-BDLDIR       PIC X(20).                 RMB0006 
RCNCER           05 V-TIPOTEL-BDLDIR         PIC X(15).                 RMB0006 
RCNCER           05 V-FECHATEL-BDLDIR        PIC 9(06).                 RMB0006 
RCNCER           05 V-SUSTEL-BDLDIR          PIC 9(06).                 RMB0006 
RCNCER           05 V-HASTEL-BDLDIR          PIC X(50).                 RMB0006 
TELEXC           05 V-FECULTTEL-BDLDIR       PIC 9(06).                 HGO0008
                                                                        
REPLUS           05 V-DEPTEL-BDLDIR          PIC X(20).                 HGO0008
NCAM18           05 V-FECCRETEL-BDLDIR       PIC 9(06).                 ABB0020
NCAM18           05 V-NUMENTTEL-BDLDIR       PIC 9(06).                 ABB0020
NCAM19           05 V-TIPOTE-BDLDIR          PIC 9(01).                 ABB0020
AJCONT           05 V-SEC00TEL-BDLDIR        PIC 9(03).                 FARF021
AJCONT           05 V-SECUNI00TEL-BDLDIR     PIC 9(03).                 FARF021
                                                                        
RCNCER     03 NUMTEL-BDLDIR                  PIC X(12).                 RMB0006 
RCNCER     03 DIR-OK-BDLDIR                  PIC 9(05).                 RMB0006
      *                                                                 
RCNCER     03 TABLA-DIRFINALES-BDLDIR.                                  RMB0006 
AJCONT*       TABLA 01DIRXXX                                            FARF021
RCNCER        04 T-DIRFINALES-BDLDIR         OCCURS 100 TIMES.          RMB0006 
RCNCER           05 V-DIRFIN-BDLDIR          PIC X(60).                 RMB0006 
PYSPAR           05 V-PRIPARTFIN-BDLDIR      PIC X(60).                 HGO0011
PYSPAR           05 V-SEGPARTFIN-BDLDIR      PIC X(60).                 HGO0011
RCNCER           05 V-CIUFIN-BDLDIR          PIC X(20).                 RMB0006 
RCNCER           05 V-TIPFIN-BDLDIR          PIC X(01).                 RMB0006 
RCNCER           05 V-FECFIN-BDLDIR          PIC 9(06).                 RMB0006 
RCNCER           05 V-NUMFIN-BDLDIR          PIC 9(03).                 RMB0006 
RCNCER           05 V-SUSFIN-BDLDIR          PIC 9(06).                 RMB0006 
RCNCER           05 V-HASDIRFIN-BDLDIR       PIC X(100).                RMB0006 
RCNCER           05 V-PRIDIGFIN-BDLDIR       PIC 9(10).                 RMB0006 
RCNCER           05 V-VECFIN-BDLDIR          PIC X(12).                 RMB0006 
RCNCER           05 V-STAFIN-BDLDIR          PIC X(60).                 RMB0006 
RCNCER           05 V-CALFIN-BDLDIR          PIC 9(03).                 RMB0006 
PYSPAR           05 V-CALFIN-PART1-BDLDIR    PIC 9(03).                 HGO0011
PYSPAR           05 V-CALFIN-PART2-BDLDIR    PIC 9(03).                 HGO0011
UNIDIR           05 V-FECULTFIN-BDLDIR       PIC 9(06).                 HGO0008
UNIDIR           05 V-NUENOMFIN-BDLDIR       PIC 9(01).                 HGO0008
UNIDIR           05 V-NUMDIRFIN-BDLDIR       PIC X(60).                 HGO0008
         
REPLUS           05 V-DANEFIN-BDLDIR         PIC 9(08).
REPLUS           05 V-DEPFIN-BDLDIR          PIC X(20).
REPLUS           05 V-ESTFIN-BDLDIR          PIC X(02).
RECPF2           05 V-PRBFIN-BDLDIR          PIC X(03).                 HGO0012
NCAM18           05 V-ZONAFIN-BDLDIR         PIC X(01).                 ABB0020
NCAM18           05 V-FECCREFIN-BDLDIR       PIC 9(06).                 ABB0020
NCAM18           05 V-NUMENTFIN-BDLDIR       PIC 9(06).                 ABB0020
NCAM19           05 V-TIPODIFIN-BDLDIR       PIC 9(01).                 ABB0020
AJCONT           05 V-NN01DIRFIN-BDLDIR      PIC 9(01).                 FARF021
AJCONT           05 V-SEC01DIRFIN-BDLDIR     PIC 9(03).                 FARF021
AJCONT           05 V-SECUNI01DIRFIN-BDLDIR  PIC 9(03).                 FARF021
      *
AJCIUU     03 V-DANEFIN-BDLDIR-AUX           PIC 9(08).                 FARF019
      *
UNIDIR    03 TABLA-DIRFINALESFIN-BDLDIR.                                HGO0008
AJCONT*      TABLA 02DIRXXX                                             FARF021
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

REPLUS          05 V-DANEFINFIN-BDLDIR      PIC 9(08).
REPLUS          05 V-DEPFINFIN-BDLDIR       PIC X(20).
REPLUS          05 V-ESTFINFIN-BDLDIR       PIC X(02).
RECPF2          05 V-PRBFINFIN-BDLDIR       PIC X(03).                  HGO0012
NCAM18          05 V-ZONAFINFIN-BDLDIR      PIC X(01).                  ABB0020
NCAM18          05 V-FECCREFINFIN-BDLDIR    PIC 9(06).                  ABB0020
NCAM18          05 V-NUMENTFINFIN-BDLDIR    PIC 9(06).                  ABB0020
NCAM19          05 V-TIPODIFINFIN-BDLDIR    PIC 9(01).                  ABB0020
AJCONT          05 V-NN02DIRFINFIN-BDLDIR   PIC 9(01).                  FARF021
AJCONT          05 V-SEC02DIRFINFIN-BDLDIR  PIC 9(03).                  FARF021
AJCONT          05 V-SECUNI02DIRFINFIN-BDLDIR PIC 9(03).                FARF021

UNIDIR     03 T-DIRFINALESAUX-BDLDIR.                                   HGO0008
UNIDIR        04 V-DIRFINAUX-BDLDIR          PIC X(60).                 HGO0008
PYSPAR        04 V-PRIPARTFINAUX-BDLDIR      PIC X(60).                 HGO0011
PYSPAR        04 V-SEGPARTFINAUX-BDLDIR      PIC X(60).                 HGO0011
UNIDIR        04 V-CIUFINAUX-BDLDIR          PIC X(20).                 HGO0008
UNIDIR        04 V-TIPFINAUX-BDLDIR          PIC X(01).                 HGO0008
UNIDIR        04 V-FECFINAUX-BDLDIR          PIC 9(06).                 HGO0008
UNIDIR        04 V-NUMFINAUX-BDLDIR          PIC 9(03).                 HGO0008
UNIDIR        04 V-SUSFINAUX-BDLDIR          PIC 9(06).                 HGO0008
UNIDIR        04 V-HASDIRFINAUX-BDLDIR       PIC X(100).                HGO0008
UNIDIR        04 V-PRIDIGFINAUX-BDLDIR       PIC 9(10).                 HGO0008
UNIDIR        04 V-VECFINAUX-BDLDIR          PIC X(12).                 HGO0008
UNIDIR        04 V-STAFINAUX-BDLDIR          PIC X(60).                 HGO0008
UNIDIR        04 V-CALFINAUX-BDLDIR          PIC 9(03).                 HGO0008
PYSPAR        04 V-CALFINAUX-PART1-BDLDIR    PIC 9(03).                 HGO0011
PYSPAR        04 V-CALFINAUX-PART2-BDLDIR    PIC 9(03).                 HGO0011
UNIDIR        04 V-FECULTFINAUX-BDLDIR       PIC 9(06).                 HGO0008
UNIDIR        04 V-NUENOMFINAUX-BDLDIR       PIC 9(01).                 HGO0008
UNIDIR        04 V-NUMDIRFINAUX-BDLDIR       PIC X(60).                 HGO0008
                                                                       
REPLUS        04 V-DANEFINAUX-BDLDIR         PIC 9(08).                
REPLUS        04 V-DEPFINAUX-BDLDIR          PIC X(20).                
REPLUS        04 V-ESTFINAUX-BDLDIR          PIC X(02).                
RECPF2        04 V-PRBFINAUX-BDLDIR          PIC X(03).                 HGO0012
NCAM18        04 V-ZONAFINAUX-BDLDIR         PIC X(01).                 ABB0020
NCAM18        04 V-FECCREFINAUX-BDLDIR       PIC 9(06).                 ABB0020
NCAM18        04 V-NUMENTFINAUX-BDLDIR       PIC 9(06).                 ABB0020
NCAM19        04 V-TIPODIFINAUX-BDLDIR       PIC 9(01).                 ABB0020
AJCONT        04 V-NN02DIRFINAUX-BDLDIR      PIC 9(01).                 FARF021
AJCONT        04 V-SEC02DIRFINAUX-BDLDIR     PIC 9(03).                 FARF021
AJCONT        04 V-SECUNI02DIRINAUX-BDLDIR   PIC 9(03).                 FARF021
                                                                       
RCNCER     03 TABLA-DIR-BDLDIR.                                         RMB0006 
RCNCER        04 T-DIR-BDLDIR                OCCURS 100 TIMES.          RMB0006 
RCNCER           05 T-NUMDIR-BDLDIR .                                   RMB0006 
RCNCER              06 V-NUMDIR-BDLDIR       OCCURS 10 TIMES PIC 9(5).  RMB0006 
RCNCER           05 V-CIUDADDIR-BDLDIR       PIC X(20).                 RMB0006 
RCNCER           05 V-DIRECCION-BDLDIR       PIC X(60).                 RMB0006 
RCNCER           05 V-TIPODIR-BDLDIR         PIC X(01).                 RMB0006 
RCNCER           05 V-FECHADIR-BDLDIR        PIC 9(06).                 RMB0006 
RCNCER           05 V-SUSDIR-BDLDIR          PIC 9(06).                 RMB0006 
RCNCER           05 V-HASDIR-BDLDIR          PIC X(100).                RMB0006 
RCNCER           05 V-PRIDIG-BDLDIR          PIC 9(10).                 RMB0006 
UNIDIR           05 V-FECULT-BDLDIR          PIC 9(06).                 HGO0008
UNIDIR           05 V-NUENOM-BDLDIR          PIC 9(01) VALUES 0.        HGO0008
                                                                       
REPLUS           05 V-DANEDIR-BDLDIR         PIC 9(08).                
REPLUS           05 V-DEPTODIR-BDLDIR        PIC X(20).                
REPLUS           05 V-ESTRATODIR-BDLDIR      PIC X(02).                
RECPF2           05 V-PRBENTDIR-BDLDIR       PIC X(03).                 HGO0012
NCAM18           05 V-ZONADIR-BDLDIR         PIC X(01).                 ABB0020
NCAM18           05 V-FECCRE-BDLDIR          PIC 9(06).                 ABB0020
NCAM18           05 V-NUMENT-BDLDIR          PIC 9(06).                 ABB0020
NCAM19           05 V-TIPODI-BDLDIR          PIC 9(01).                 ABB0020
AJCONT           05 V-DIRNN00DIR-BDLDIR      PIC X(30).                 FARF021
AJCONT           05 V-SEC00DIR-BDLDIR        PIC 9(03).                 FARF021
AJCONT           05 V-SECUNI00DIR-BDLDIR     PIC 9(03).                 FARF021
                                                                       
RCNCER     03   TABLA-FECHADIR-AUX-BDLDIR.                              RMB0006 
RCNCER           04 O-FECHADIR-AUX-BDLDIR    OCCURS 100 TIMES.          RMB0006 
RCNCER              05 V-FECHADIR-AUX-BDLDIR PIC 9(06).                 RMB0006
                                                                       
REPLUS    03 CEL-OK-BDLDIR                  PIC 9(05) VALUE 0.          
REPLUS    03 TABLA-CELFINALES-BDLDIR.                                   
REPLUS       04 T-CELFINALES-BDLDIR         OCCURS 100 TIMES.           
REPLUS          05 V-CELFIN-BDLDIR          PIC X(12).                  
REPLUS          05 V-CELFIN-BDLDIR-R        REDEFINES                   
REPLUS             V-CELFIN-BDLDIR.                                     
REPLUS             10 V-CELFIN-BDLDIR-1     PIC X(05).                  
REPLUS             10 V-CELFIN-BDLDIR-2     PIC X(07).                  
REPLUS          05 V-FECCELFIN-BDLDIR       PIC 9(06). 
REPLUS          05 V-NUMCELFIN-BDLDIR       PIC 9(03).
REPLUS          05 V-SUSCELFIN-BDLDIR       PIC 9(06).                  
REPLUS          05 V-HASCELFIN-BDLDIR       PIC X(50).                  
REPLUS          05 V-VECCELFIN-BDLDIR       PIC X(50).                  
REPLUS          05 V-CALCELFIN-BDLDIR       PIC 9(03).                  
REPLUS          05 V-FECULTCELFIN-BDLDIR    PIC 9(06).
REPLUS          05 V-ORIGENCELFIN-BDLDIR    PIC 9(01).
REPLUS          05 V-INDICACELFIN-BDLDIR    PIC 9(01).
NCAM18          05 V-FECCRECELFIN-BDLDIR    PIC 9(06).                  ABB0020
NCAM18          05 V-NUMENTCELFIN-BDLDIR    PIC 9(06).                  ABB0020
AJCONT          05 V-SEC01CELFIN-BDLDIR     PIC 9(03).                  FARF021 
AJCONT          05 V-SECUNI01CELFIN-BDLDIR  PIC 9(03).                  FARF021 
      *
REPLUS    03 TABLA-CEL-BDLDIR.                                          
REPLUS       04 T-CEL-BDLDIR                OCCURS 100 TIMES.           
REPLUS          05 V-CELULAR-BDLDIR         PIC X(12).                  
REPLUS          05 V-FECHACEL-BDLDIR        PIC 9(06).                  
REPLUS          05 V-SUSCEL-BDLDIR          PIC 9(06).                  
REPLUS          05 V-HASCEL-BDLDIR          PIC X(50).                  
REPLUS          05 V-FECULTCEL-BDLDIR       PIC 9(06).
REPLUS          05 V-ORIGENCEL-BDLDIR       PIC 9(01).
NCAM18          05 V-FECCRECEL-BDLDIR       PIC 9(06).                  ABB0020
NCAM18          05 V-NUMENTCEL-BDLDIR       PIC 9(06).                  ABB0020
AJCONT          05 V-SEC00CEL-BDLDIR        PIC 9(03).                  FARF021 
AJCONT          05 V-SECUNI00CEL-BDLDIR     PIC 9(03).                  FARF021 
      *
REPLUS    03 NUMCEL-BDLDIR                  PIC X(12).
REPLUS    03 MAIL-OK-BDLDIR                 PIC 9(05) VALUE 0.          
REPLUS    03 TABLA-MAILFINALES-BDLDIR.                                  
REPLUS       04 T-MAILFINALES-BDLDIR        OCCURS 100 TIMES.           
AJCONT          05 V-MAILFIN-BDLDIR         PIC X(40).                  FARF021
REPLUS          05 V-FECMAILFIN-BDLDIR      PIC 9(06).                  
REPLUS          05 V-NUMMAILFIN-BDLDIR      PIC 9(03).                  
REPLUS          05 V-SUSMAILFIN-BDLDIR      PIC 9(06).                  
REPLUS          05 V-HASMAILFIN-BDLDIR      PIC X(50).                  
REPLUS          05 V-VECMAILFIN-BDLDIR      PIC X(50).                  
REPLUS          05 V-CALMAILFIN-BDLDIR      PIC 9(03).                  
REPLUS          05 V-FECULTMAILFIN-BDLDIR   PIC 9(06).
REPLUS          05 V-ORIGENMAILFIN-BDLDIR   PIC 9(01).
REPLUS          05 V-INDICAMAILFIN-BDLDIR   PIC 9(01).
NCAM18          05 V-FECCREMAILFIN-BDLDIR   PIC 9(06).                  ABB0020
NCAM18          05 V-NUMENTMAILFIN-BDLDIR   PIC 9(06).                  ABB0020
AJCONT          05 V-SEC01MAILFIN-BDLDIR    PIC 9(03).                  FARF021 
AJCONT          05 V-SECUNI01MAILFIN-BDLDIR PIC 9(03).                  FARF021 
      *
REPLUS    03 TABLA-MAIL-BDLDIR.                                         
REPLUS       04 T-MAIL-BDLDIR               OCCURS 100 TIMES.           
AJCONT          05 V-MAIL-BDLDIR            PIC X(40).                  FARF021
REPLUS          05 V-FECHAMAIL-BDLDIR       PIC 9(06).                  
REPLUS          05 V-SUSMAIL-BDLDIR         PIC 9(06).                  
REPLUS          05 V-HASMAIL-BDLDIR         PIC X(50).                  
REPLUS          05 V-FECULTMAIL-BDLDIR      PIC 9(06).
REPLUS          05 V-ORIGENMAIL-BDLDIR      PIC 9(01).
NCAM18          05 V-FECCREMAIL-BDLDIR      PIC 9(06).                  ABB0020
NCAM18          05 V-NUMENTMAIL-BDLDIR      PIC 9(06).                  ABB0020
AJCONT          05 V-SEC00MAIL-BDLDIR       PIC 9(03).                  FARF021 
AJCONT          05 V-SECUNI00MAIL-BDLDIR    PIC 9(03).                  FARF021 
      *
AJCONT    03 NUMMAIL-BDLDIR                 PIC X(40).                  FARF021
REPLUS    03 TABLA-DUPCIUDAD.
REPLUS       04 T-DUPCIUDAD                 OCCURS 100 TIMES.
REPLUS          05 CIUDAD-DUPCIUDAD         PIC X(20).
NCAM22    03 TABLA-TRA-BDLDIR.                                          ABB0022
NCAM22******************************************************************ABB0022
NCAM22*   VECTOR DE 100 POSICIONES QUE GUARDA LAS DESCRIPCIONES DE LOS *ABB0022
NCAM22*   CAMPOS NUEVOS ZONA, TIPO DE DIRECCION Y TIPO DE TELEFONO     *ABB0022
NCAM22*        V-ZON-BDLDIR  ZONA                                      *ABB0022
NCAM22*        V-TDI-BDLDIR  TIPO DE DIRECCION                         *ABB0022
NCAM22*        V-TTE-BDLDIR  TIPO DE TELEFONO                          *ABB0022
NCAM22*                                                                *ABB0022
NCAM22******************************************************************ABB0022
NCAM22       04 T-TRA-BDLDIR                OCCURS 100 TIMES.           ABB0022
NCAM22          05 V-ZON-BDLDIR             PIC X(03).                  ABB0022
NCAM22          05 V-TDI-BDLDIR             PIC X(15).                  ABB0022
NCAM22          05 V-TTE-BDLDIR             PIC X(15).                  ABB0022
NCAM22******************************************************************ABB0022
NCAM22*   TABLA QUE CONTIENE EL VECTOR DE COMPORTAMIENTO DE DIRECCIONES*ABB0022
NCAM22*   EN LOS ULTIMOS 24 MESES ANTERIORES AL MES ACTUAL             *ABB0022
NCAM22*        T-MES-BDLDIR AAAAMM ANNO MES                            *ABB0022
NCAM22*        T-VRE-BDLDIR VECTOR DIRECCION RESIDENCIA(I=IGUAL,N=NUEVA*ABB0022
NCAM22*        T-VLA-BDLDIR VECTOR DIRECCION LABORAL                   *ABB0022
NCAM22*        T-VCR-BDLDIR VECTOR DIRECCION CORRESPONDENCIA           *ABB0022
NCAM22*                                                                *ABB0022
NCAM22******************************************************************ABB0022
NCAM22    03 TABLA-VEC-BDLDIR.                                          ABB0022
NCAM22       04 T-MES-BDLDIR                PIC 9(06) OCCURS 24 TIMES.  ABB0022
NCAM22       04 T-VRE-BDLDIR                PIC X(24).                  ABB0022
NCAM22       04 T-VLA-BDLDIR                PIC X(24).                  ABB0022
NCAM22       04 T-VCR-BDLDIR                PIC X(24).                  ABB0022
NCAM22    03 FECHA-VEC-BDLDIR               PIC 9(08).                  ABB0022
NCAM22    03 FECHA-VEC-BDLDIR-R REDEFINES FECHA-VEC-BDLDIR.             ABB0022
NCAM22       04 ANOMES-VEC-BDLDIR           PIC 9(06).                  ABB0022
NCAM22       04 ANOMES-VEC-BDLDIR-R REDEFINES ANOMES-VEC-BDLDIR.        ABB0022
NCAM22          05 ANO-VEC-BDLDIR           PIC 9(04).                  ABB0022
NCAM22          05 MES-VEC-BDLDIR           PIC 9(02).                  ABB0022
NCAM22       04 DIA-VEC-BDLDIR              PIC 9(02).                  ABB0022
NCAM22    03 I-TRA-BDLDIR                   PIC 9(04).                  ABB0022
NCAM22    03 J-TRA-BDLDIR                   PIC 9(04).                  ABB0022
NCAM22    03 K-TRA-BDLDIR                   PIC 9(04).                  ABB0022
NCAM22    03 W-RES-BDLDIR                   PIC 9(01).                  ABB0022
NCAM22    03 W-LAB-BDLDIR                   PIC 9(01).                  ABB0022
NCAM22    03 W-CRR-BDLDIR                   PIC 9(01).                  ABB0022
NCAM18    03 TABLA-SUS-BDLDIR.                                          ABB0020
NCAM18       04 T-SUS-BDLDIR                OCCURS 100 TIMES.           ABB0020
NCAM18          05 V-SUS-BDLDIR             PIC 9(11).                  ABB0020
NCAM18    03 N-SUS-BDLDIR                   PIC 9(11).                  ABB0020
NCAM18    03 I-SUS-BDLDIR                   PIC 9(04).                  ABB0020
NCAM18    03 J-SUS-BDLDIR                   PIC 9(04).                  ABB0020
NCAM18    03 K-SUS-BDLDIR                   PIC 9(04).                  ABB0020
NCAM19    03 I-RES-BDLDIR                   PIC 9(01).                  ABB0020
NCAM19    03 I-LAB-BDLDIR                   PIC 9(01).                  ABB0020
NCAM19    03 I-COR-BDLDIR                   PIC 9(01).                  ABB0020
NCAM19    03 I-SDT-BDLDIR                   PIC 9(01).                  ABB0020

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
AJCONT    03 SEC-BDLDIR                     PIC 9(03).                  FARF021 
AJCONT    03 UNI-BDLDIR                     PIC 9(03).                  FARF021 
UNIDIR    03 COMPO                          PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 RMAYOR                         PIC 9(01) VALUE 0.          HGO0008
UNIDIR    03 CAMBIO                         PIC 9(01) VALUE 0.          HGO0008
UNIDIR    03 NN                             PIC 9(01) VALUE 0.          HGO0008
UNIDIR    03 CONTA                          PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 CONTA1                         PIC 9(05) VALUE 0.          HGO0008
UNIDIR    03 DIRECCION                      PIC X(60) VALUE SPACES.     HGO0008
UNIDIR    03 DIRECCION1                     PIC X(60) VALUE SPACES.     HGO0008
UNIDIR    03 DIRECCION2                     PIC X(60) VALUE SPACES.     HGO0008
      *                                                                 
UNIDIR     03 E-BDLDIR                       PIC 9(05) VALUE 0.         HGO0008
UNIDIR     03 F-BDLDIR                       PIC 9(05) VALUE 0.         HGO0008
UNIDIR     03 G-BDLDIR                       PIC 9(05) VALUE 0.         HGO0008
UNIDIR     03 H-BDLDIR                       PIC 9(05) VALUE 0.         HGO0008
PLNCHQ     03 HAY-NUMEROS4-VIAS-BDLDIR       PIC 9(05) VALUE 0.         FARF032 
PLNCHQ     03 HAY-NUMEROS-BDLDIR             PIC 9(05) VALUE 0.         FARF032 
PLNCHQ     03 HAY-LETRAS-BDLDIR              PIC 9(05) VALUE 0.         FARF032 
PLNCHQ     03 HAY-MASDE3-REPETIDAS-BDLDIR    PIC 9(05) VALUE 0.         FARF032 
PLNCHQ     03 CONT-REPETIDAS-BDLDIR          PIC 9(05) VALUE 0.         FARF032 
PLNCHQ     03 HAY-PRIMERA-MALA-BDLDIR        PIC 9(05) VALUE 0.         FARF032 
RCNCER     03 I-BDLDIR                       PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 J-BDLDIR                       PIC 9(05) VALUE 0.         RMB0006 
PYSPAR     03 J1-BDLDIR                      PIC 9(05) VALUE 0.         HGO0011
PYSPAR     03 J2-BDLDIR                      PIC 9(05) VALUE 0.         HGO0011
PYSPAR     03 CONTADOR-TEMPORAL-BDLDIR       PIC 9(03).                 HGO0011
RCNCER     03 M-BDLDIR                       PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 N-BDLDIR                       PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 K-BDLDIR                       PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 II-BDLDIR                      PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 JJ-BDLDIR                      PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 NUM-OK-SEND-BDLDIR             PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 I-STA-BDLDIR                   PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 J-STA-BDLDIR                   PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 K-STA-BDLDIR                   PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 II-STA-BDLDIR                  PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 JJ-STA-BDLDIR                  PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 KK-STA-BDLDIR                  PIC 9(05) VALUE 0.         RMB0006 
RCNCER     03 CALIFICA-DIR-BDLDIR            PIC 9(03) VALUE 0.         RMB0006 
RECPF2     03 SW-PART-VAL-BDLDIR             PIC 9(01) VALUE 0.         HGO0012
PYSPAR     03 CON-PART-ESTANDARIZADA-BDLDIR  PIC 9(03) VALUE 0.         HGO0011
PYSPAR     03 CALIFICA-DIRPART1-BDLDIR       PIC 9(03) VALUE 0.         HGO0011
PYSPAR     03 CALIFICA-DIRPART2-BDLDIR       PIC 9(03) VALUE 0.         HGO0011
RCNCER     03 CALIFICA-REAL-BDLDIR           PIC 9(03) VALUE 0.         RMB0006 
RCNCER     03 CAMPOS-N-DIR-BDLDIR            PIC 9(03) VALUE 0.         RMB0006 
RCNCER     03 COSI-DIR-BDLDIR                PIC 9(09) VALUE 0.         RMB0006 
RCNCER     03 RESI-DIR-BDLDIR                PIC 9(09) VALUE 0.         RMB0006 
RCNCER     03 TELE-DIR-BDLDIR                PIC 9(12) VALUE 0.         RMB0006 
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
AJCONT    02 MAIL-BDLDIR                    PIC X(40).                  FARF021 
REPLUS    02 CAL-MAIL-BDLDIR                PIC 9(03) VALUE 0.
RCNCER***************************VARIABLES DE STANDARIZAR************** RMB0006 
RCNCER 01 DIRACC-BDLDIR-BDLDIR.                                         RMB0006 
RCNCER    03 VALIDACION-DIR-OUT-BDLDIR      PIC 9(01).                  RMB0006 
RCNCER    03 PRIVILEGIO-INPUT-BDLDIR        PIC 9(01).                  RMB0006 
RCNCER    03 NIT-CONSULTANTE-INPUT-BDLDIR   PIC 9(11).                  RMB0006 
RCNCER 01  FDA-DIREST-BDLDIR                PIC 9(01).                  RMB0006 
RCNCER 01  IND-TIPCAR-BDLDIR                PIC X(01).                  RMB0006 
RCNCER 01  TIPCAR-WS-BDLDIR                 PIC X(01).                  RMB0006 
RCNCER 01  ENC-DIREST-BDLDIR                PIC 9(01).                  RMB0006 
RCNCER* 01 TABLA-DIR-ESTANDAR-BDLDIR         OCCURS 500 TIMES           RMB0006 
AMP017 01 TABLA-DIR-ESTANDAR-BDLDIR         OCCURS 2000 TIMES           PJV0017
RCNCER    ASCENDING KEY IS DIR-ORIGINAL-BDLDIR                          RMB0006 
RCNCER    INDEXED   BY  I-DIR-ESTANDAR-BDLDIR.                          RMB0006 
RCNCER    03 DIR-ORIGINAL-BDLDIR            PIC X(20).                  RMB0006 
RCNCER    03 DIR-ESTANDAR-BDLDIR            PIC X(10).                  RMB0006 
RCNCER* 01 TABLA-DIR-COMPUESTAS-BDLDIR.                                 RMB0006 
RCNCER*    03 DIR-COMPUESTAS-AST-BDLDIR      PIC X(20) OCCURS 90 TIMES. RMB0006 
RCNCER*    03 DIR-COMPUESTAS-BLN-BDLDIR      PIC X(20) OCCURS 90 TIMES. RMB0006 
RCNCER*    03 DIR-COMPUESTAS-LON-BDLDIR      PIC 9(02) OCCURS 90 TIMES. RMB0006 
RCNCER*    03 DIR-COMPUESTAS-TIP-BDLDIR      PIC 9(01) OCCURS 90 TIMES. RMB0006
AMP017 01 TABLA-DIR-COMPUESTAS-BDLDIR.                                  PJV0017
AMP017    03 DIR-COMPUESTAS-AST-BDLDIR      PIC X(20) OCCURS 500 TIMES. PJV0017
AMP017    03 DIR-COMPUESTAS-BLN-BDLDIR      PIC X(20) OCCURS 500 TIMES. PJV0017
AMP017    03 DIR-COMPUESTAS-LON-BDLDIR      PIC 9(02) OCCURS 500 TIMES. PJV0017
AMP017    03 DIR-COMPUESTAS-TIP-BDLDIR      PIC 9(01) OCCURS 500 TIMES. PJV0017
RCNCER 01 TIP-NUEVA-DIRECCION-BDLDIR.                                   RMB0006 
RCNCER    03 TIP-NVA-DIR-BDLDIR             PIC X(01) OCCURS 60 TIMES.  RMB0006 
RCNCER 01 NUEVA-DIRECCION-BDLDIR.                                       RMB0006 
RCNCER    03 NVA-DIR-BDLDIR                 PIC X(60) OCCURS 60 TIMES.  RMB0006 
RCNCER 01 DIRECCION-A-ESTANDARIZAR-BDLDIR.                              RMB0006 
RCNCER    03 DIR-A-EST-BDLDIR               PIC X(01) OCCURS 60 TIMES.  RMB0006 
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
NN0016    02 SW-NN                        PIC 9 VALUE 0.                PJV0016

