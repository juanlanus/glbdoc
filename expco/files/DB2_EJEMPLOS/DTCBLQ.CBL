      *DTCBLQ.CBL.v.01.0024
BDDDB2$set db2(VALIDATE=BIND INIT=PROT BIND COLLECTION=COBOLAPP)        VHF0017
BDDDB2$set db2(ACTION=REPLACE DEGREE=ANY EXPLAIN=NO EXPLSNAP=NO)        VHF0017
BDDDB2$set db2(FORMAT=LOC VERSION=V8)                                   VHF0017
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DTCBLQ.
BDDDB2*@DB2CFG_BD=DB2_NOVEDAT                                           VHF0017
REMARK******************************************************************
REMARK* ESTE PROGRAMA PROCESA LAS CUENTAS QUE TIENEN RECLAMO Y QUE NO  *
REMARK* HA SIDO PROCESADO EN EL PLAZO MAXIMO PERMITIDO BLOQUEANDOLAS O *
REMARK* RATIFICANDOLAS SEGUN INDICADOR EN LA TABLA MODIFCACIONES       *
REMARK******************************************************************
REMARK* VHF0002     * DIAAUT - SE MODIFICA EL PROGRAMA PARA QUE NO     *
REMARK* VICTOR HUGO *          EXIGA QUE EL DIA A PARTIR DEL CUAL SE   *
REMARK* FORERO      *          APLICA LA AUTOREGULACION SEA HABIL      *
REMARK* SEP.22/2003 *                                                  *
REMARK******************************************************************
REMARK* VHF0003     * INDBLQ - SE MODIFICA EL PROGRAMA PARA UTILIZAR EL*
REMARK* VICTOR HUGO *          NUEVO CAMPO BLO_AUTORREGULA CUANDO SE   *
REMARK* FORERO      *          APLICA LA AUTOREGULACION POR BLOQUEO    *
REMARK* OCT.15/2003 *          TEMPORAL                                *
REMARK*             * DTRLLA - SE ADICIONA CONTROL PARA EVITAR LOOP POR*
REMARK*             *          INCONSISTENCIAS EN DTRLLA               *
REMARK*             * ERRSQL - SE MODIFICA EL MANEJO DE ERRORES PARA   *
REMARK*             *          LAS FUNCIONES C, INCLUYENDO LOS SELECTS,*
REMARK*             *          Y SE GRABAN EN DTCTRA CON DESCRIPCION DE*
REMARK*             *          ERROR                                   *
REMARK******************************************************************
REMARK* VHF0004     * ERRSQ1 - SE MUESTRA EN EL MENSAJE DE ERROR DE    *
REMARK* VICTOR HUGO *          TRANSACCIONES SQL EL CODIGO SQL ASOCIADO*
REMARK* FORERO      * ERRSQ2 - SE GRABA EN EL ARCHIVO DTCTRA FECHA Y   *
REMARK* DIC.02/2003 *          HORA DE ERROR SQL                       *
REMARK******************************************************************
REMARK* VHF0005     * INDBDD - SE INCLUYE LOGICA AL PROGRAMA PARA QUE  *
REMARK* VICTOR HUGO *          VERIFIQUE EN EL ARCHIVO DTCBDD SI LA BD *
REMARK* FORERO      *          BDD INTERBASE NOVEDAT ESTA DISPONIBLE O *
REMARK* ENE.20/2004 *          NO PARA EL PROCESO                      *
REMARK*             * DIANHB - SE CAMBIA LA FORMA DE DETERMINAR SI LA  *
REMARK*             *          TABLA DE DIAS HABILES NO ESTA           *
REMARK*             *          ACTUALIZADA Y SI ES ASI SE INTENTA      *
REMARK*             *          ACTUALIZAR CON LA DE NOVEDAT            *
REMARK******************************************************************
REMARK* VHF0006     * ARCTMP - SE CORRIGE NOMBRE DE ARCHIVO TEMPORAL   *
REMARK* VICTOR HUGO *          DE REGISTROS A AUTREGULAR PARA LA       *
REMARK* FORERO      *          GENERACION EN C                         *
REMARK* ENE.22/2004 *                                                  *
REMARK******************************************************************
REMARK* VHF0007     * DIANH2 - SE CORRIGE CARGE DE TABLA DE DIAS NO    *
REMARK* VICTOR HUGO *          HABILES YA QUE EL NOMBRE DE ARCHIVO     *
REMARK* FORERO      *          QUEDA INCOMPLETO                        *
REMARK* ABR.07/2004 * COMENT - SE CORRIGE PARA QUE AL BUSCAR COMENTARIO*
REMARK*             *          ASOCIADO A RECLAMO RATIFICADO NO SE     *
REMARK*             *          UTILICE LA LLAVE DE LA CONSULTA SINO LA *
REMARK*             *          LLAVE DEL MAESTRO, YA QUE PARA MIGRACION*
REMARK*             *          SE GENERA ERROR                         *
REMARK*             * DTRLL2 - CUANDO SE UTILICE ESTE ARCHIVO A PARTIR *
REMARK*             *          DE MAYO 1 DE 2004 SE ENVIA UN MAIL AL   *
REMARK*             *          ADMINISTRADOR DE UNIX PARA INFORMARLO   *
REMARK*             * DIANH3 - PARA FECHAS EN LAS QUE NO HAY REGISTROS *
REMARK*             *          PARA PROCESAR SE VERIFICA QUE EL DIA DE *
REMARK*             *          PROCESO NO SEA HABIL CASO EN EL CUAL ES *
REMARK*             *          VALIDO, EN OTRO CASO SE GENERA UNA      *
REMARK*             *          ADVERTENCIA                             *
REMARK******************************************************************
REMARK* VHF0008     * CIUREC - SE ADICIONA CODIGO DE INGRESO           *
REMARK* VICTOR HUGO *          CORRESPONDIENTE A RECLAMOS DE CIUDADANOS*
REMARK* FORERO      *          POR WEB                                 *
REMARK* JUL.14/2004 *                                                  *
REMARK******************************************************************
REMARK* VHF0009     * DTRLL3 - CUANDO SE UTILICE ESTE ARCHIVO A PARTIR *
REMARK* VICTOR HUGO *          DE SEPT 1 DE 2004 SE ENVIA UN MAIL AL   *
REMARK* FORERO      *          ADMINISTRADOR DE UNIX PARA INFORMARLO   *
REMARK* AGO.05/2004 *                                                  *
REMARK******************************************************************
REMARK* VHF0010     * TIPING - SE MODIFICA EL MANEJO DE CODIGO DE      *
REMARK* VICTOR HUGO *          INGRESO PARA MANEJO POR GRUPOS SEGUN EL *
REMARK* FORERO      *          TIPO (EJ 02, 04 Y 05 DE RECLAMOS)       *
REMARK* SEP.07/2004 *                                                  *
REMARK******************************************************************
REMARK* VHF0011     * LIBSQL - SE INCLUYE LA LOGICA AL PROGRAMA PARA   *
REMARK* VICTOR HUGO *          EL MANEJO DE LA LIBRERIA LIBSQL.e DE    *
REMARK* FORERO      *          FUNCIONES C-SQL QUE REEMPLAZA A DTCSQL.e*
REMARK* DIC.14/2004 * DTRLL4 - CUANDO SE UTILICE EL DTRLLA A PARTIR DE *
REMARK*             *          AGOSTO 31 DE 2006 SE ENVIA UN MAIL AL   *
REMARK*             *          ADMINISTRADOR DEL SISTEMA               *
REMARK*             * LOGCTE - SE CORRIGE PROGRAMA PARA QUE SE GRABE   *
REMARK*             *          LOG PARA CUENTAS CORRIENTES             *
REMARK*             * REGRAT - SE GUARDA EL REGISTRO DESPUES PARA      *
REMARK*             *          RATIFICACION AUTOMATICA                 *
REMARK******************************************************************
      * FAC012C     * SUPT08* CAMBIO DE ESTRUCTURA BDIEMSUP A FORMATO
      * F.CUADROS   * TIPO 8
      * NOV/11/2005 *
REMARK******************************************************************
REMARK* VHF0013     * REGRA2 - SE GUARDA REGISTRO ANTERIOR PARA        *
REMARK* VICTOR HUGO *          RATIFICACION AUTOMATICA                 *
REMARK* FORERO      *                                                  *
REMARK* NOV.29/2005 *                                                  *
REMARK******************************************************************
REMARK* VHF0014     * DIANH4 - SE CAMBIA LA FORMA DE MANEJO DE DIAS NO *
REMARK* VICTOR HUGO *          HABILES. SE ACTUALIZARAN DESDE UNA AÑO  *
REMARK* FORERO      *          ATRAS HASTA LA MAYOR FECHA DE LA TABLA  *
REMARK* ENE.12/2006 *          INTERBASE.                              *
REMARK******************************************************************
REMARK* VHF0015     * DIANH5 - SE ADICIONA ORDER BY FECHA AL SELECT DE *
REMARK* VICTOR HUGO *          LA TABLA NO_HABILES PARA ACTUALIZACION  *
REMARK* FORERO      *                                                  *
REMARK* ENE.13/2006 *                                                  *
REMARK******************************************************************
      * FAC0016     * REGLOC - SE INCLUYE CAMPO REG-LOCK-ABD06 PARA    *
      * F.CUADROS   *          CONTROLAR REGISTROS CERRADOS POR NOVEDAT*
      * FEB.06/2006 *          SE ELIMINA CONTROL DEL CAMPO            *
      *             *          TIP-CALIFIC-ABD06                       *
REMARK******************************************************************
REMARK* VHF0017     * BDDDB2 - SE MODIFICA PROGRAMA PARA MANEJO DE     *
REMARK* VICTOR HUGO *          INSTRUCCIONES SQL DE DB2                *
REMARK* FORERO      *                                                  *
REMARK* SEP.01/2005 *                                                  *
REMARK******************************************************************
      * GDT0018    | LEY001 - PROCESO DE AMNISTIA SEGUN ARTICULO 0021  *
      * ENE.23/2008|    QUE INCLUYE EL VECTOR DE 48 POSICIONES.        *
      * GER.DESARR.|                                                   *
      ******************************************************************
REMARK* VHF0019    * LEY002 - SE IMPLEMENTAN CAMBIOS PARA LEY, EN      *
REMARK* VICTOR HUGO *          PARTICULAR LOS CAMBIOS DE LO RELACIONADO*
REMARK* FORERO      *          CON 48 MESES DE HISTORIA                *
REMARK* JUN.16/2008 *                                                  *
REMARK*             *                                                  *
REMARK******************************************************************
REMARK* VHF0020     * MATC02 - SE MODIFICA PROGRAMA PARA QUE NO SE DANE*
REMARK* VICTOR HUGO *          EL INDICADOR DE BLOQUEO DE CUENTAS SIN  *
REMARK* ABR.14/2009 *          VALIDAR                                 *
REMARK******************************************************************
REMARK* VHF0021     * LEY003 - SE INHIBE LO REFERENTE A NOAMNISTIA YA  *
REMARK* VICTOR HUGO *          QUE SE ELIMINO EL CONCEPTO DE MANEJAR   *
REMARK* FORERO      *          ARCHIVO DE CUENTAS NO AMNISTIADAS       *
REMARK* MAY.04/2009 *                                                  *
REMARK******************************************************************
REMARK* ABB0022     * NUEPRO - SE MODIFICA PROGRAMA PARA QUE MANEJE    *
REMARK* ALBEIRO B.  *          NUEVA PRORROGA                          *
REMARK* MAY.27/2009 *                                                  *
REMARK******************************************************************
      * GDT0001    | CTAST8 - CAMBIO DEL ICMCHK (MAESTRO DE CUENTAS) A *
      * FEB.09-2010|    FORMATO TIPO 8.                                *
      * GER.DESARR.|                                                   *
      ******************************************************************
      * GJC0023    * MODCPY - SE EL CAMBIO DEL COPYS DTCLOG.FD POR     *
      * MAYO/2012  *    DTCLOG001.FD, SE ADICIONAN LOS COPYS REQUERIDOS*    
      * G.JIMENEZ  *    PARA NUEVO MAESPLAN Y SE ADICIONA NULO EN 2    *
      *            *    SENTENCIAS SQL
      ******************************************************************
REMARK* VHF0024     * MIGNIT - MIGRACION DE NIT DE COMPUTEC A          *
REMARK* VICTOR HUGO *          EXPERIAN COMPUTEC S.A.S                 *
REMARK* FORERO      *                                                  *
REMARK* OCT.25/2012 *                                                  *
REMARK******************************************************************
LEY003 AUTHOR. DATACREDITO.                                             VHF0021
LEY003 DATE-WRITTEN.                                                    VHF0021
LEY003 DATE-COMPILED.                                                   VHF0021
LEY003 CONFIGURATION SECTION.                                           VHF0021
LEY003 SOURCE-COMPUTER. MICROFOCUS.                                     VHF0021
LEY003 OBJECT-COMPUTER. MICROFOCUS.                                     VHF0021
LEY003 INPUT-OUTPUT SECTION.                                            VHF0021
LEY003 FILE-CONTROL.                                                    VHF0021
LEY001     COPY ICBDIR.FS.                                              GDT0018 
LEY001     COPY BDACSV.FS.                                              GDT0018 
LEY003*    COPY NOAMNISTIA21.FS.                                        VHF0021
LEY001*                                                                 GDT0018 
       COPY ICBCRE.FS.
CTAST8*COPY  ICBCHK.FS.                                                 GDT0001
CTAST8     COPY  ICBCHK0001.FS.                                         GDT0001
       COPY ICBSUS.FS.
SUPT08 COPY BDIEMSUP0001.FS.                                            FAC012C 
       COPY DTRLLA.FS.
       COPY DTCNHAB.FS.
       COPY DTCLOG.FS.
       COPY DTCTRA.FS.
       COPY DTCCOM.FS.
INDBDD COPY DTCBDD.FS.                                                  VHF0005 
BDDDB2 COPY LIBSQL.FS.                                                  VHF0017 
MATC02 COPY BDIIDIAN.FS.                                                VHF0020
MATC02 COPY BDIIDAS.FS.                                                 VHF0020
MATC02 COPY BDIIREGI.FS.                                                VHF0020
MODCPY COPY ICMCLN.FS.                                                  GJC0023
MODCPY COPY ICMCHQ.FS.                                                  GJC0023
MODCPY COPY ICMVLR.FS.                                                  GJC0023
MODCPY COPY BDLSTDCIU.FS.                                               GJC0023

           SELECT SALIDA ASSIGN TO DISK TITLE-SALIDA
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
LEY001 COPY ICBDIR0001.FD.                                              GDT0018 
LEY001 COPY BDACSV.FD.                                                  GDT0018 
LEY003*COPY NOAMNISTIA21.FD.                                            VHF0021
LEY001*                                                                 GDT0018 
       COPY ICBCRE.FD.
       COPY ICBCHK.FD.
       COPY ICBSUS.FD.
       COPY BDIEMSUP.FD.
       COPY DTRLLA.FD.
       COPY DTCNHAB.FD.
MODCPY*COPY DTCLOG.FD.                                                  GJC0023
MODCPY COPY DTCLOG0001.FD.                                              GJC0023
BDDDB2 COPY DTCTRA0001.FD.                                              VHF0017
       COPY DTCCOM.FD.
INDBDD COPY DTCBDD.FD.                                                  VHF0005 
BDDDB2 COPY LIBSQL.FD.                                                  VHF0017 
MATC02 COPY BDIIDIAN.FD.                                                VHF0020
MATC02 COPY BDIIDAS.FD.                                                 VHF0020
MATC02 COPY BDIIREGI.FD.                                                VHF0020
MODCPY COPY ICMCLN.FD.                                                  GJC0023
MODCPY COPY ICMCHQ.FD.                                                  GJC0023
MODCPY COPY ICMVLR.FD.                                                  GJC0023
MODCPY COPY BDLSTDCIU.FD.                                               GJC0023
       FD  SALIDA.
       01  REG-SALIDA                 PIC X(200).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

REMARK*** PARAMETROS
REMARK***************
REMARK*** DE ENTRADA
      
REMARK*** VARIABLES GENERALES
REMARK************************
       01 DIAS-PLAZO                  PIC 9(10).
NUEPRO 01 DIAS-PLAZO-W                PIC 9(10).                        ABB0022
       01 DIAS-VENCE                  PIC 9(10)  VALUE 0.
       01 NUMIDE-AUX                  PIC 9(11).
       01 NUMCON-AUX                  PIC 9(10).
CIUREC 01 CODING-AUX                  PIC 9(02).                        VHF0008 
       01 COMPLETO-ABD06-AUX          PIC X(200).
       01 COMPLETO-ABD07-AUX          PIC X(400).
       01 REG-PLANO-INI               PIC X(400).
       01 REG-PLANO-FIN               PIC X(400).
       01 LLAVE-ANT-AUX               PIC X(38).
       01 IND-BLOQUEO-AUX             PIC 9(01).
       01 IND-RECLAMO-AUX             PIC 9(01).
       01 REG-DTCCOM-AUX              PIC X(500).
       01 COD-ESTADO-AUX              PIC X(01).
       01 COD-EVENTO-AUX              PIC X(03).
       01 COD-SOLUCION-AUX            PIC X(02).
       01 COD-JUS-AUX                 PIC X(03).
REMARK*** VARIABLE UTILIZADA PARA PROCESAR LAS VARIABLES DE AMBIENTE    VHF0019
LEY002 01 VAR-AMBIENTE                PIC X(80).                        VHF0019

REMARK*** VARIABLES TIPO REGISTRO
REMARK****************************
REGRA2 01 REG-CTACTE-A.                                                 VHF0013 
REGRA2    03 TIPO-ID-CTACTE-A         PIC 9(01).                        VHF0013 
REGRA2    03 NUM-ID-CTACTE-A          PIC 9(11).                        VHF0013 
REGRA2    03 COD-SUS-CTACTE-A         PIC 9(06).                        VHF0013 
REGRA2    03 COD-CTA-CTACTE-A         PIC 9(02).                        VHF0013 
REGRA2    03 NUM-CTA-CTACTE-A         PIC X(18).                        VHF0013 
REGRA2    03 IND-BLOQUEO-CTACTE-A     PIC 9(01).                        VHF0013 
REGRA2    03 FECHA-APERT-CTACTE-A     PIC 9(04).                        VHF0013 
REGRA2    03 CALIFIC-CTACTE-A         PIC 9(01).                        VHF0013 
REGRA2    03 TIP-CALIFIC-CTACTE-A     PIC 9(01).                        VHF0013 
REGRA2    03 NOV-REPORT-CTACTE-A      PIC 9(02).                        VHF0013 
REGRA2    03 FEC-NOVEDAD-CTACTE-A     PIC 9(04).                        VHF0013 
REGRA2    03 NUM-EMBARGO-CTACTE-A     PIC 9(02).                        VHF0013 
REGRA2    03 FEC-ULT-EMB-CTACTE-A     PIC 9(04).                        VHF0013 
REGRA2    03 ADJETIVO-CTACTE-A        PIC 9(01).                        VHF0013 
REGRA2    03 FEC-ADJETIVO-CTACTE-A    PIC 9(04).                        VHF0013 
REGRA2    03 ART15-CTACTE-A           PIC 9(01).                        VHF0013 
REGRA2    03 IND-MODIFICACION-CTACTE-A    PIC 9(01)                     VHF0013 
REGRA2       REDEFINES                ART15-CTACTE-A.                   VHF0013 
REGRA2    03 RECLAMO-CTACTE-A         PIC 9(03).                        VHF0013 
REGRA2    03 RECLAMO-CTACTE-A-R       REDEFINES RECLAMO-CTACTE-A.       VHF0013 
REGRA2       05 IND-RECLAMO-CTACTE-A  PIC 9(01).                        VHF0013 
REGLOC       05 REG-LOCK-CTACTE-A     PIC 9(01).                        FAC0016 
REGLOC       05 FILLER-RECLAMO-CTACTE-A   PIC 9(01).                    FAC0016 
REGRA2    03 CONSECUTIVO-CTACTE-A     PIC 9(02).                        VHF0013 
REGRA2    03 OFICINA-CTACTE-A         PIC X(15).                        VHF0013 
REGRA2    03 CIUDAD-CTACTE-A          PIC X(06).                        VHF0013 
REGRA2    03 FEC-NOVANT-CTACTE-A      PIC 9(04).                        VHF0013 
REGRA2    03 NOVANT-CTACTE-A          PIC 9(02).                        VHF0013 
REGRA2    03 FILLER-CTACTE-A          PIC X(04).                        VHF0013 
REGRA2 01 REG-CARTERA-A.                                                VHF0013 
REGRA2    03 TIPO-ID-CARTERA-A        PIC 9(01).                        VHF0013 
REGRA2    03 NUM-ID-CARTERA-A         PIC 9(11).                        VHF0013 
REGRA2    03 COD-SUS-CARTERA-A        PIC 9(06).                        VHF0013 
REGRA2    03 COD-CTA-CARTERA-A        PIC 9(02).                        VHF0013 
REGRA2    03 NUM-CTA-CARTERA-A        PIC X(18).                        VHF0013 
REGRA2    03 IND-BLOQUEO-CARTERA-A    PIC 9(01).                        VHF0013 
REGRA2    03 FECHA-APERT-CARTERA-A    PIC 9(04).                        VHF0013 
REGRA2    03 FECHA-VENC-CARTERA-A     PIC 9(04).                        VHF0013 
REGRA2    03 CUANTIA-CARTERA-A        PIC 9(05).                        VHF0013 
REGRA2    03 CALIFIC-CARTERA-A        PIC 9(01).                        VHF0013 
REGRA2    03 TIP-CALIFIC-CARTERA-A    PIC 9(01).                        VHF0013 
REGRA2    03 NOV-REPORT-CARTERA-A     PIC 9(02).                        VHF0013 
REGRA2    03 FEC-NOVEDAD-CARTERA-A    PIC 9(04).                        VHF0013 
REGRA2    03 NUM-MORAS30-CARTERA-A    PIC 9(02).                        VHF0013 
REGRA2    03 ESTADO-MES-CARTERA-A     PIC 9(12).                        VHF0013 
REGRA2    03 MORA-MAXIMA-CARTERA-A    PIC 9(01).                        VHF0013 
REGRA2    03 ADJETIVO1-CARTERA-A      PIC 9(01).                        VHF0013 
REGRA2    03 FEC-MEN-ADJ-CARTERA-A    PIC 9(04).                        VHF0013 
REGRA2    03 ADJETIVO2-CARTERA-A      PIC 9(01).                        VHF0013 
REGRA2    03 FEC-MAY-ADJ-CARTERA-A    PIC 9(04).                        VHF0013 
REGRA2    03 ADJETIVO3-CARTERA-A      PIC 9(01).                        VHF0013 
REGRA2    03 ART15-CARTERA-A          PIC 9(01).                        VHF0013 
REGRA2    03 IND-MODIFICACION-CARTERA-A   PIC 9(01)                     VHF0013 
REGRA2       REDEFINES                ART15-CARTERA-A.                  VHF0013 
REGRA2    03 RECLAMO-CARTERA-A        PIC 9(03).                        VHF0013 
REGRA2    03 RECLAMO-CARTERA-A-R      REDEFINES RECLAMO-CARTERA-A.      VHF0013 
REGLOC       05 IND-RECLAMO-CARTERA-A PIC 9(01).                        FAC0016 
REGLOC       05 REG-LOCK-CARTERA-A    PIC 9(01).                        FAC0016 
REGRA2       05 FILLER-RECLAMO-CARTERA-A  PIC 9(01).                    VHF0013 
REGRA2    03 IND-BLOQUEO-R-CARTERA-A  PIC 9(01).                        VHF0013 
REGRA2    03 CALIFIC-R-CARTERA-A      PIC 9(01).                        VHF0013 
REGRA2    03 IND-MAPA-BITS-VLR-CARTERA-A                                VHF0013 
REGRA2                                PIC 9(01).                        VHF0013 
REGRA2    03 NOV-REPORT-R-CARTERA-A   PIC 9(02).                        VHF0013 
REGRA2    03 FEC-NOVEDAD-R-CARTERA-A  PIC 9(04).                        VHF0013 
REGRA2    03 MAPA-BITS-VLR-CARTERA-A  PIC 9(15).                        VHF0013 
REGRA2    03 ESTADO-MES-R-CARTERA-A   PIC 9(12).                        VHF0013 
REGRA2    03 MORA-MAXIMA-R-CARTERA-A  PIC 9(01).                        VHF0013 
REGRA2    03 VALOR-INICIAL-CARTERA-A  PIC 9(07).                        VHF0013 
REGRA2    03 VALOR-SALDO-DEUDA-CARTERA-A                                VHF0013 
REGRA2                                PIC 9(07)  OCCURS 4 TIMES.        VHF0013 
REGRA2    03 VALOR-DISPONIBLE-CARTERA-A                                 VHF0013 
REGRA2                                PIC 9(07)  OCCURS 4 TIMES.        VHF0013 
REGRA2    03 VALOR-SALDO-MORA-CARTERA-A                                 VHF0013 
REGRA2                                PIC 9(07)  OCCURS 4 TIMES.        VHF0013 
REGRA2    03 CALIFICACION-CARTERA-A   PIC 9(01)  OCCURS 4 TIMES.        VHF0013 
REGRA2    03 VALOR-SALDO-DEUDAM-CARTERA-A                               VHF0013 
REGRA2                                PIC 9(07).                        VHF0013 
REGRA2    03 VALOR-DISPONIBLEM-CARTERA-A                                VHF0013 
REGRA2                                PIC 9(07).                        VHF0013 
REGRA2    03 VALOR-SALDO-MORAM-CARTERA-A                                VHF0013 
REGRA2                                PIC 9(07).                        VHF0013 
REGRA2    03 CALIFICACIONM-CARTERA-A  PIC 9(01).                        VHF0013 
REGRA2    03 TIPO-MONEDA-CARTERA-A    PIC 9(01).                        VHF0013 
REGRA2    03 TIPO-OBLIGACION-CARTERA-A                                  VHF0013 
REGRA2                                PIC 9(01).                        VHF0013 
REGRA2    03 TIPO-GARANTIA-CARTERA-A  PIC 9(01).                        VHF0013 
REGRA2    03 CUOTAS-CANCELADAS-CARTERA-A                                VHF0013 
REGRA2                                PIC 9(03).                        VHF0013 
REGRA2    03 FECHA-DE-PAGO-CARTERA-A  PIC 9(08).                        VHF0013 
REGRA2    03 TOTAL-CUOTAS-CARTERA-A   PIC 9(03).                        VHF0013 
REGRA2    03 CUOTAS-EN-MORA-CARTERA-A PIC 9(03).                        VHF0013 
REGRA2    03 SITUACION-CARTERA-A      PIC X(01).                        VHF0013 
REGRA2    03 FECHA-ACT-END-CARTERA-A  PIC 9(06).                        VHF0013 
REGRA2    03 CONSECUTIVO-CARTERA-A    PIC 9(02).                        VHF0013 
REGRA2    03 FECHA-MORA-MAXIMA-CARTERA-A                                VHF0013 
REGRA2                                PIC 9(06).                        VHF0013 
REGRA2    03 FECHA-ACTUALIZ-VALOR-CARTERA-A                             VHF0013 
REGRA2                                PIC 9(06).                        VHF0013 
REGRA2    03 FECHA-SITUACION-CARTERA-A                                  VHF0013 
REGRA2                                PIC 9(06).                        VHF0013 
REGRA2    03 VECTOR-2-CARTERA-A       PIC 9(12).                        VHF0013 
REGRA2    03 PERIODICIDAD-PAGO-CARTERA-A                                VHF0013 
REGRA2                                PIC 9(01).                        VHF0013 
REGRA2    03 GARANTE-CARTERA-A        PIC 9(02).                        VHF0013 
REGRA2    03 FORMA-PAGO-TOTAL-CARTERA-A                                 VHF0013 
REGRA2                                PIC 9(01).                        VHF0013 
REGRA2    03 MOROSIDAD-CARTERA-A      PIC 9(03).                        VHF0013 
REGRA2    03 NOV-REPORT2-CARTERA-A    PIC 9(02).                        VHF0013 
REGRA2    03 VECTOR-2-R-CARTERA-A     PIC 9(12).                        VHF0013 
REGRAT 01 REG-CTACTE-D.                                                 VHF0011 
REGRAT    03 TIPO-ID-CTACTE-D         PIC 9(01).                        VHF0011 
REGRAT    03 NUM-ID-CTACTE-D          PIC 9(11).                        VHF0011 
REGRAT    03 COD-SUS-CTACTE-D         PIC 9(06).                        VHF0011 
REGRAT    03 COD-CTA-CTACTE-D         PIC 9(02).                        VHF0011 
REGRAT    03 NUM-CTA-CTACTE-D         PIC X(18).                        VHF0011 
REGRAT    03 IND-BLOQUEO-CTACTE-D     PIC 9(01).                        VHF0011 
REGRAT    03 FECHA-APERT-CTACTE-D     PIC 9(04).                        VHF0011 
REGRAT    03 CALIFIC-CTACTE-D         PIC 9(01).                        VHF0011 
REGRAT    03 TIP-CALIFIC-CTACTE-D     PIC 9(01).                        VHF0011 
REGRAT    03 NOV-REPORT-CTACTE-D      PIC 9(02).                        VHF0011 
REGRAT    03 FEC-NOVEDAD-CTACTE-D     PIC 9(04).                        VHF0011 
REGRAT    03 NUM-EMBARGO-CTACTE-D     PIC 9(02).                        VHF0011 
REGRAT    03 FEC-ULT-EMB-CTACTE-D     PIC 9(04).                        VHF0011 
REGRAT    03 ADJETIVO-CTACTE-D        PIC 9(01).                        VHF0011 
REGRAT    03 FEC-ADJETIVO-CTACTE-D    PIC 9(04).                        VHF0011 
REGRAT    03 ART15-CTACTE-D           PIC 9(01).                        VHF0011 
REGRAT    03 IND-MODIFICACION-CTACTE-D    PIC 9(01)                     VHF0011 
REGRAT       REDEFINES                ART15-CTACTE-D.                   VHF0011 
REGRAT    03 RECLAMO-CTACTE-D         PIC 9(03).                        VHF0011 
REGRAT    03 RECLAMO-CTACTE-D-R       REDEFINES RECLAMO-CTACTE-D.       VHF0011 
REGRAT       05 IND-RECLAMO-CTACTE-D  PIC 9(01).                        VHF0011 
REGLOC       05 REG-LOCK-CTACTE-D     PIC 9(01).                        FAC0016 
REGLOC       05 FILLER-RECLAMO-CTACTE-D   PIC 9(01).                    FAC0016 
REGRAT    03 CONSECUTIVO-CTACTE-D     PIC 9(02).                        VHF0011 
REGRAT    03 OFICINA-CTACTE-D         PIC X(15).                        VHF0011 
REGRAT    03 CIUDAD-CTACTE-D          PIC X(06).                        VHF0011 
REGRAT    03 FEC-NOVANT-CTACTE-D      PIC 9(04).                        VHF0011 
REGRAT    03 NOVANT-CTACTE-D          PIC 9(02).                        VHF0011 
REGRAT    03 FILLER-CTACTE-D          PIC X(04).                        VHF0011 
REGRAT 01 REG-CARTERA-D.                                                VHF0011 
REGRAT    03 TIPO-ID-CARTERA-D        PIC 9(01).                        VHF0011 
REGRAT    03 NUM-ID-CARTERA-D         PIC 9(11).                        VHF0011 
REGRAT    03 COD-SUS-CARTERA-D        PIC 9(06).                        VHF0011 
REGRAT    03 COD-CTA-CARTERA-D        PIC 9(02).                        VHF0011 
REGRAT    03 NUM-CTA-CARTERA-D        PIC X(18).                        VHF0011 
REGRAT    03 IND-BLOQUEO-CARTERA-D    PIC 9(01).                        VHF0011 
REGRAT    03 FECHA-APERT-CARTERA-D    PIC 9(04).                        VHF0011 
REGRAT    03 FECHA-VENC-CARTERA-D     PIC 9(04).                        VHF0011 
REGRAT    03 CUANTIA-CARTERA-D        PIC 9(05).                        VHF0011 
REGRAT    03 CALIFIC-CARTERA-D        PIC 9(01).                        VHF0011 
REGRAT    03 TIP-CALIFIC-CARTERA-D    PIC 9(01).                        VHF0011 
REGRAT    03 NOV-REPORT-CARTERA-D     PIC 9(02).                        VHF0011 
REGRAT    03 FEC-NOVEDAD-CARTERA-D    PIC 9(04).                        VHF0011 
REGRAT    03 NUM-MORAS30-CARTERA-D    PIC 9(02).                        VHF0011 
REGRAT    03 ESTADO-MES-CARTERA-D     PIC 9(12).                        VHF0011 
REGRAT    03 MORA-MAXIMA-CARTERA-D    PIC 9(01).                        VHF0011 
REGRAT    03 ADJETIVO1-CARTERA-D      PIC 9(01).                        VHF0011 
REGRAT    03 FEC-MEN-ADJ-CARTERA-D    PIC 9(04).                        VHF0011 
REGRAT    03 ADJETIVO2-CARTERA-D      PIC 9(01).                        VHF0011 
REGRAT    03 FEC-MAY-ADJ-CARTERA-D    PIC 9(04).                        VHF0011 
REGRAT    03 ADJETIVO3-CARTERA-D      PIC 9(01).                        VHF0011 
REGRAT    03 ART15-CARTERA-D          PIC 9(01).                        VHF0011 
REGRAT    03 IND-MODIFICACION-CARTERA-D   PIC 9(01)                     VHF0011 
REGRAT       REDEFINES                ART15-CARTERA-D.                  VHF0011 
REGRAT    03 RECLAMO-CARTERA-D        PIC 9(03).                        VHF0011 
REGRAT    03 RECLAMO-CARTERA-D-R      REDEFINES RECLAMO-CARTERA-D.      VHF0011 
REGRAT       05 IND-RECLAMO-CARTERA-D PIC 9(01).                        VHF0011 
REGLOC       05 REG-LOCK-CARTERA-D    PIC 9(01).                        FAC0016 
REGLOC       05 FILLER-RECLAMO-CARTERA-D  PIC 9(01).                    FAC0016 
REGRAT    03 IND-BLOQUEO-R-CARTERA-D  PIC 9(01).                        VHF0011 
REGRAT    03 CALIFIC-R-CARTERA-D      PIC 9(01).                        VHF0011 
REGRAT    03 IND-MAPA-BITS-VLR-CARTERA-D                                VHF0011 
REGRAT                                PIC 9(01).                        VHF0011 
REGRAT    03 NOV-REPORT-R-CARTERA-D   PIC 9(02).                        VHF0011 
REGRAT    03 FEC-NOVEDAD-R-CARTERA-D  PIC 9(04).                        VHF0011 
REGRAT    03 MAPA-BITS-VLR-CARTERA-D  PIC 9(15).                        VHF0011 
REGRAT    03 ESTADO-MES-R-CARTERA-D   PIC 9(12).                        VHF0011 
REGRAT    03 MORA-MAXIMA-R-CARTERA-D  PIC 9(01).                        VHF0011 
REGRAT    03 VALOR-INICIAL-CARTERA-D  PIC 9(07).                        VHF0011 
REGRAT    03 VALOR-SALDO-DEUDA-CARTERA-D                                VHF0011 
REGRAT                                PIC 9(07)  OCCURS 4 TIMES.        VHF0011 
REGRAT    03 VALOR-DISPONIBLE-CARTERA-D                                 VHF0011 
REGRAT                                PIC 9(07)  OCCURS 4 TIMES.        VHF0011 
REGRAT    03 VALOR-SALDO-MORA-CARTERA-D                                 VHF0011 
REGRAT                                PIC 9(07)  OCCURS 4 TIMES.        VHF0011 
REGRAT    03 CALIFICACION-CARTERA-D   PIC 9(01)  OCCURS 4 TIMES.        VHF0011 
REGRAT    03 VALOR-SALDO-DEUDAM-CARTERA-D                               VHF0011 
REGRAT                                PIC 9(07).                        VHF0011 
REGRAT    03 VALOR-DISPONIBLEM-CARTERA-D                                VHF0011 
REGRAT                                PIC 9(07).                        VHF0011 
REGRAT    03 VALOR-SALDO-MORAM-CARTERA-D                                VHF0011 
REGRAT                                PIC 9(07).                        VHF0011 
REGRAT    03 CALIFICACIONM-CARTERA-D  PIC 9(01).                        VHF0011 
REGRAT    03 TIPO-MONEDA-CARTERA-D    PIC 9(01).                        VHF0011 
REGRAT    03 TIPO-OBLIGACION-CARTERA-D                                  VHF0011 
REGRAT                                PIC 9(01).                        VHF0011 
REGRAT    03 TIPO-GARANTIA-CARTERA-D  PIC 9(01).                        VHF0011 
REGRAT    03 CUOTAS-CANCELADAS-CARTERA-D                                VHF0011 
REGRAT                                PIC 9(03).                        VHF0011 
REGRAT    03 FECHA-DE-PAGO-CARTERA-D  PIC 9(08).                        VHF0011 
REGRAT    03 TOTAL-CUOTAS-CARTERA-D   PIC 9(03).                        VHF0011 
REGRAT    03 CUOTAS-EN-MORA-CARTERA-D PIC 9(03).                        VHF0011 
REGRAT    03 SITUACION-CARTERA-D      PIC X(01).                        VHF0011 
REGRAT    03 FECHA-ACT-END-CARTERA-D  PIC 9(06).                        VHF0011 
REGRAT    03 CONSECUTIVO-CARTERA-D    PIC 9(02).                        VHF0011 
REGRAT    03 FECHA-MORA-MAXIMA-CARTERA-D                                VHF0011 
REGRAT                                PIC 9(06).                        VHF0011 
REGRAT    03 FECHA-ACTUALIZ-VALOR-CARTERA-D                             VHF0011 
REGRAT                                PIC 9(06).                        VHF0011 
REGRAT    03 FECHA-SITUACION-CARTERA-D                                  VHF0011 
REGRAT                                PIC 9(06).                        VHF0011 
REGRAT    03 VECTOR-2-CARTERA-D       PIC 9(12).                        VHF0011 
REGRAT    03 PERIODICIDAD-PAGO-CARTERA-D                                VHF0011 
REGRAT                                PIC 9(01).                        VHF0011 
REGRAT    03 GARANTE-CARTERA-D        PIC 9(02).                        VHF0011 
REGRAT    03 FORMA-PAGO-TOTAL-CARTERA-D                                 VHF0011 
REGRAT                                PIC 9(01).                        VHF0011 
REGRAT    03 MOROSIDAD-CARTERA-D      PIC 9(03).                        VHF0011 
REGRAT    03 NOV-REPORT2-CARTERA-D    PIC 9(02).                        VHF0011 
REGRAT    03 VECTOR-2-R-CARTERA-D     PIC 9(12).                        VHF0011 
BDDDB2 01 REG-TEMPORAL.                                                 VHF0017 
BDDDB2    03 LLAVE-TEMPORAL.                                            VHF0017 
BDDDB2       05 TIPIDE-TEMPORAL       PIC 9(01).                        VHF0017 
BDDDB2       05 NUMIDE-TEMPORAL       PIC X(11).                        VHF0017 
BDDDB2       05 CODSUS-TEMPORAL       PIC 9(06).                        VHF0017 
BDDDB2       05 CODCTA-TEMPORAL       PIC 9(02).                        VHF0017 
BDDDB2       05 NUMCTA-TEMPORAL       PIC X(18).                        VHF0017 
BDDDB2    03 NUMCON-TEMPORAL          PIC X(10).                        VHF0017 
BDDDB2    03 CODING-TEMPORAL          PIC X(02).                        VHF0017 
BDDDB2    03 CODEST-TEMPORAL          PIC X(01).                        VHF0017 
BDDDB2    03 INDCAD-TEMPORAL          PIC X(01).                        VHF0017 
BDDDB2 01 REG-TEMPORAL02.                                               VHF0017 
BDDDB2    03 FECHA-TEMPORAL02         PIC X(10).                        VHF0017 

REMARK*** FECHAS
REMARK***********
       01 FECHA-INI                   PIC X(14).
       01 FECHA-FIN                   PIC X(14).
DIANH4 01 FEC-MAX-AUX                 PIC 9(08).                        VHF0014 
       01 FECHA-AUX                   PIC 9(08).
       01 FECHA-AUX-R                 REDEFINES  FECHA-AUX.
          03 ANO-AUX                  PIC 9(04).
          03 MES-AUX                  PIC 9(02).
          03 DIA-AUX                  PIC 9(02).
       01 FEC-8-AUX                   PIC 9(08).
       01 FEC-8-AUX-R                 REDEFINES  FEC-8-AUX.
          03 ANO-8-AUX                PIC 9(04).
          03 MES-8-AUX                PIC 9(02).
          03 DIA-8-AUX                PIC 9(02).
DIAAUT 01 HORA-AUX                    PIC 9(06).
DIAAUT 01 HORA-AUX-R                  REDEFINES  HORA-AUX.
DIAAUT    03 HOR-AUX                  PIC 9(02).
DIAAUT    03 MIN-AUX                  PIC 9(02).
DIAAUT    03 SEG-AUX                  PIC 9(02).
ERRSQ2 01 FECHA-ERROR                 PIC 9(08).                        VHF0004 
ERRSQ2 01 HORA-ERROR                  PIC 9(06).                        VHF0004 
DTRLL2 01 AAAAMMDD-REN-AUX            PIC 9(08).                        VHF0007 

REMARK*** INDICADORAS
REMARK****************
       01 IND-BISIESTO                PIC 9(01).
       01 IND-HABIL                   PIC 9(01).
       01 FDA-TEMPORAL                PIC 9(01).
       01 ENC-MAESTRO                 PIC 9(01).
       01 IND-DTRLLA                  PIC 9(01).
       01 ACT-MAESTRO                 PIC 9(01).
       01 ENC-SUSCRIPTOR              PIC 9(01).
       01 IND-ERR-BD                  PIC 9(01).
       01 IND-DTCCOM                  PIC 9(01).
       01 IND-RECLAMO                 PIC 9(01).
       01 IND-RATIFICACION            PIC 9(01).
DTRLLA 01 IND-PROANT-DTRLLA           PIC 9(03).                        VHF0003 
DIANHB 01 IND-TABLA-NOHAB             PIC 9(01).                        VHF0005 
DTRLL2 01 IND-USO-DTRLLA              PIC 9(01).                        VHF0007 
DIANH3 01 IND-DIAACT-HABIL            PIC 9(01).                        VHF0007 
REMARK*** VARIABLE QUE INDICA SI SE DEBE APLICAR LA APLICACION DE LEY   VHF0019
LEY002 01 SW-LEY                      PIC X(02).                        VHF0019
REMARK*** INDICA SI UNA IDENTIFICACIÓN ES DE CSV (1)  O   NO (0)        VHF0020
MATC02 01 IND-CSV                     PIC 9(01).                        VHF0020

REMARK*** CONSTANTES
REMARK***************
       01 NULO                        PIC X(01)  VALUE X'00'.
       01 SI                          PIC 9(01)  VALUE 1.
       01 NO-ES                       PIC 9(01)  VALUE 0.
       01 TITLE-SALIDA.
          03 FILLER                   PIC X(21)  VALUE
          "$LISTADOS/DTCBLQ.PRN.".
          03 FEC-TIT-SAL              PIC X(08).
          03 FILLER                   PIC X(01)  VALUE ".".
          03 HOR-TIT-SAL              PIC X(06).
       01 TITLE-TEMPORAL              PIC X(80)  VALUE
          "$TEMPORALES/DTCBLQ.TMP".
DIANHB 01 TITLE-TEMPORAL02            PIC X(80)  VALUE                  VHF0005
DIANH3    "$TEMPORALES/DTCBL2.TMP".                                     VHF0007 
NUEPRO 01 TITLE-TEMPORAL03            PIC X(80)  VALUE                  ABB0022
NUEPRO    "$TEMPORALES/DTCBL3.TMP".                                     ABB0022
NUEPRO 01 TITULO-SALIDA               PIC X(99)  VALUE SPACES.          ABB0022
NUEPRO 01 SW-PRORROGA                 PIC 9(01)  VALUE ZEROS.           ABB0022
DTRLLA 01 PENDIENTE                   PIC 9(01)  VALUE 9.               VHF0003 
DTRLLA 01 ERROR-DTRLLA                PIC 9(01)  VALUE 8.               VHF0003 
DTRLL4 01 FEC-USO-DTRLLA              PIC 9(08)  VALUE 20060831.        VHF0011 
BDDDB2 01 VAR-AMBIENTE-BD             PIC X(11)  VALUE "DB2_NOVEDAT".   VHF0017 
BDDDB2 01 PROGRAMA-BD                 PIC X(20)  VALUE "DTCBLQ".        VHF0017 

REMARK*** CONTADORES
REMARK***************
       01 CON-LEIDOS                  PIC 9(09)  VALUE 0.
       01 CON-LEIDOS-R                REDEFINES  CON-LEIDOS.
          03 FILLER                   PIC 9(06).
          03 IND-DESPLIEGUE           PIC 9(03).
       01 CON-LEI-CHECK               PIC 9(09)  VALUE 0.
       01 CON-BLQ-CHECK               PIC 9(09)  VALUE 0.
       01 CON-RAT-CHECK               PIC 9(09)  VALUE 0.
       01 CON-AUT-CHECK               PIC 9(09)  VALUE 0.
       01 CON-ERR-CHECK               PIC 9(09)  VALUE 0.
       01 CON-LEI-CREDIT              PIC 9(09)  VALUE 0.
       01 CON-BLQ-CREDIT              PIC 9(09)  VALUE 0.
       01 CON-RAT-CREDIT              PIC 9(09)  VALUE 0.
       01 CON-AUT-CREDIT              PIC 9(09)  VALUE 0.
       01 CON-ERR-CREDIT              PIC 9(09)  VALUE 0.

REMARK*** ARREGLOS
REMARK*************
       01 DIAS-MESES.
          03 FILLER                   PIC X(24)  VALUE
             "312831303130313130313031".
       01 DIAS-MESES-R                REDEFINES  DIAS-MESES.
          03 DIAS-MES                 PIC 9(02)  OCCURS 12 TIMES.
DTRLLA 01 VEC-PROANT-DTRLLA.                                            VHF0003 
DTRLLA    03 PROANT-DTRLLA            OCCURS 500 TIMES                  VHF0003 
DTRLLA       DESCENDING KEY IS LLAVE-PROANT-DTRLLA                      VHF0003 
DTRLLA       INDEXED   BY  I-PROANT-DTRLLA.                             VHF0003 
DTRLLA       05 LLAVE-PROANT-DTRLLA   PIC X(38).                        VHF0003 

REMARK*** COPYS
REMARK**********
       COPY ICFILE.WS.
       COPY VALVLR.WS.  
MODCPY COPY MAESPLANEXT.WS.                                             GJC0023
       COPY UTLBIT.WS.
DTRLL2 COPY EMAILDG.WS.                                                 VHF0007 
BDDDB2 COPY LIBSQL.WS.                                                  VHF0017 
MATC02 COPY BDIIVIDE.WS.                                                VHF0020
MATC02 COPY LEEMAESTROS.WS.                                             VHF0020

      ******************************************************************
MATC02$SET CONSTANT HACERMATCH"SI"                                      VHF0020
LEY001$SET CONSTANT AMNISTIA21"NO"                                      GDT0018 
       PROCEDURE DIVISION.
      ******************************************************************
       0000-PROGRAMA-PRINCIPAL.
REMARK******************************************************************
REMARK*0000-PROGRAMA-PRINCIPAL.                                        *
REMARK******************************************************************
DTRLL2     MOVE "DTCBLQ:  " TO PROGRAMA-EMAIL                           VHF0007 
           MOVE FUNCTION CURRENT-DATE (1:14) TO FECHA-INI
BDDDB2     MOVE VAR-AMBIENTE-BD TO VAR-AMB-BD-LIBSQL                    VHF0017 
BDDDB2     MOVE PROGRAMA-BD     TO PROGRAMA-LIBSQL                      VHF0017 
           PERFORM 0100-ABRIR-ARCHIVOS
INDBDD     PERFORM 0200-VERIFICAR-ACCESO-BD                             VHF0005 
DIANH4     PERFORM 5400-ACTUALIZAR-NOHABIL                              VHF0014 
DIANHB     PERFORM 5300-VERIFICAR-TABLA-NOHABIL                         VHF0005 
DIANHB     IF IND-TABLA-NOHAB = NO-ES                                   VHF0005 
DIANH4        MOVE " ERROR: TABLA BDD RELACIONAL DE DIAS NO HABILES DESAVHF0014 
DIANH4-            "CTUALIZADA. EL PROGRAMA TERMINA   A N O R M A L M E VHF0014 
DIANH4-            "N T E"                                              VHF0014 
DIANH4             TO REG-SALIDA                                        VHF0014 
DIANH4        DISPLAY REG-SALIDA                                        VHF0014 
DIANH4        WRITE REG-SALIDA                                          VHF0014 
DIANH4        STOP RUN                                                  VHF0014 
DIANHB     END-IF                                                       VHF0005 
           PERFORM 1000-PROCESAR-CADUCIDADES
DIANH3     IF CON-LEIDOS > 0                                            VHF0007 
NUEPRO        MOVE "               RESUMEN DEL PROCESO DTCBLQ         " ABB0022
NUEPRO        TO   TITULO-SALIDA                                        ABB0022
DIANH3        PERFORM 8000-PRESENTAR-RESUMEN                            VHF0007 
DIANH3     END-IF                                                       VHF0007 
NUEPRO     PERFORM 1050-CADUCIDADES-PRORROGA                            ABB0022
NUEPRO     IF CON-LEIDOS > 0                                            ABB0022
NUEPRO        MOVE "     RESUMEN DEL PROCESO DTCBLQ - PRORROGA"         ABB0022
NUEPRO        TO   TITULO-SALIDA                                        ABB0022
NUEPRO        PERFORM 8000-PRESENTAR-RESUMEN                            ABB0022
NUEPRO     END-IF                                                       ABB0022
           PERFORM 9000-CERRAR-ARCHIVOS
           STOP RUN.

       0100-ABRIR-ARCHIVOS.
REMARK******************************************************************
REMARK*0100-ABRIR-ARCHIVOS.                                            *
REMARK*    SE ABREN DTCNHAB (ARCHIVO DE DIAS NO HABILES)               *
REMARK*             SALIDA  (ARCHIVO DE RESULTADO DE PROCESO)          *
REMARK******************************************************************
           MOVE FECHA-INI (1:8) TO FEC-TIT-SAL
           MOVE FECHA-INI (9:6) TO HOR-TIT-SAL
           OPEN INPUT  DTCNHAB
                       SUSCRIPTOR
INDBDD                 DTCBDD                                           VHF0005 
MATC02                 BDIIREGI                                         VHF0020
MATC02                 BDIIDIAN                                         VHF0020
MATC02                 BDIIDAS                                          VHF0020
MATC02                 MAESTRO-DIRECC                                   VHF0020
                I-O    DTRLLA
                       DTCLOG
                       DTCCOM
                       MAESTRO-CHECK
                       MAESTRO-CREDIT
INDBDD          OUTPUT SALIDA.                                          VHF0005
BDDDB2     PERFORM 80100-CONNECT-BD-LIBSQL                              VHF0017 
BDDDB2     IF CODRET-BD-LIBSQL <> 0                                     VHF0017 
BDDDB2        DISPLAY "ERROR FATAL: NO SE PUDO ESTABLECER CONEXION "    VHF0017 
BDDDB2                "CON LA BDD - "                                   VHF0017 
BDDDB2                CODRET-ED-LIBSQL                                  VHF0017 
BDDDB2                " - "                                             VHF0017 
BDDDB2                DESERR-BD-LIBSQL                                  VHF0017 
BDDDB2        PERFORM 9000-CERRAR-ARCHIVOS                              VHF0017 
BDDDB2        STOP RUN                                                  VHF0017 
BDDDB2     END-IF.                                                      VHF0017 
REMARK***  SE LEE EL CONTENIDO DE LA VARIABLE FECHA_AMNI PARA DETERMINARVHF0019
REMARK***  SI SE DEBEN UTILIZAR 48 MESES O NO                           VHF0019
LEY002     MOVE "FECHA_AMNI" TO VAR-AMBIENTE                            VHF0019
LEY002     DISPLAY VAR-AMBIENTE UPON ENVIRONMENT-NAME                   VHF0019
LEY002     ACCEPT VAR-AMBIENTE FROM ENVIRONMENT-VALUE                   VHF0019
LEY002     IF VAR-AMBIENTE (8:1) = "1"                                  VHF0019
LEY002        MOVE "SI" TO SW-LEY                                       VHF0019
LEY002     ELSE                                                         VHF0019
LEY002        MOVE "NO" TO SW-LEY                                       VHF0019
LEY002     END-IF.                                                      VHF0019

INDBDD 0200-VERIFICAR-ACCESO-BD.                                        VHF0005 
REMARK******************************************************************
REMARK*0200-VERIFICAR-ACCESO-BD.                                       *
REMARK*    SE VERIFICA QUE EL ACCESO A LA BD NOVEDAT ESTE DISPONIBLE   *
REMARK******************************************************************
INDBDD     READ DTCBDD                                                  VHF0005 
INDBDD        AT END                                                    VHF0005 
BDDDB2           MOVE " ERROR: NO SE PUDO DETERMINAR SI DB2 ESTA DISPONIVHF0017 
BDDDB2-               "BLE PARA EL PROCESO. PROGRAMA CANCELADO"         VHF0017 
INDBDD                 TO REG-SALIDA                                    VHF0005 
INDBDD           DISPLAY REG-SALIDA                                     VHF0005 
INDBDD           WRITE REG-SALIDA                                       VHF0005 
BDDDB2           PERFORM 9000-CERRAR-ARCHIVOS                           VHF0017 
INDBDD           STOP RUN                                               VHF0005 
INDBDD     END-READ                                                     VHF0005 
INDBDD     IF BD-ACTIVA-DTCBDD = "NO"                                   VHF0005 
BDDDB2        MOVE " ERROR: DB2 NO ESTA DISPONIBLE PARA EL PROCESO. PROGVHF0017 
BDDDB2-            "RAMA CANCELADO"                                     VHF0017 
INDBDD             TO REG-SALIDA                                        VHF0005 
INDBDD        DISPLAY REG-SALIDA                                        VHF0005 
INDBDD        WRITE REG-SALIDA                                          VHF0005 
BDDDB2        PERFORM 9000-CERRAR-ARCHIVOS                              VHF0017 
INDBDD        STOP RUN                                                  VHF0005 
INDBDD     END-IF.                                                      VHF0005 

       1000-PROCESAR-CADUCIDADES.
REMARK******************************************************************
REMARK*1000-PROCESAR-CADUCIDADES.                                      *
REMARK*    SE CONSULTA EL NUMERO DE DIAS DE PLAZO, SE CALCULA LA FECHA *
REMARK*    PARA FILTRAR LOS RECLAMOS, SE EFECTUA LA CONSULTA DE LOS    *
REMARK*    RECLAMOS PENDIENTES Y SE PROCESAN LOS REGISTROS DETECTADOS  *
REMARK******************************************************************
           PERFORM 1100-CONSULTAR-DIAS-PLAZO
           PERFORM 1200-CALCULAR-FECHA-FILTRO
           PERFORM 1300-EJECUTAR-CONSUL-RECLAMOS 
           PERFORM 1400-PROCESAR-CUENTAS.

NUEPRO 1050-CADUCIDADES-PRORROGA.                                       ABB0022
NUEPRO******************************************************************ABB0022
NUEPRO*1050-CADUCIDADES-PRORROGA.                                      *ABB0022
NUEPRO*    SE CONSULTA EL NUMERO DE DIAS DE PLAZO, SE CALCULA LA FECHA *ABB0022
NUEPRO*    PARA FILTRAR LOS RECLAMOS, SE EFECTUA LA CONSULTA DE LOS    *ABB0022
NUEPRO*    RECLAMOS PENDIENTES Y SE PROCESAN LOS REGISTROS DETECTADOS  *ABB0022
NUEPRO******************************************************************ABB0022
NUEPRO     MOVE ZEROS TO CON-LEIDOS     CON-LEI-CHECK  CON-BLQ-CHECK    ABB0022
NUEPRO                   CON-RAT-CHECK  CON-AUT-CHECK  CON-ERR-CHECK    ABB0022
NUEPRO                   CON-LEI-CREDIT CON-BLQ-CREDIT CON-RAT-CREDIT   ABB0022
NUEPRO                   CON-AUT-CREDIT CON-ERR-CREDIT                  ABB0022
NUEPRO     MOVE 1 TO SW-PRORROGA                                        ABB0022
NUEPRO     PERFORM 1100-CONSULTAR-DIAS-PLAZO                            ABB0022
NUEPRO     MOVE DIAS-PLAZO TO DIAS-PLAZO-W                              ABB0022
NUEPRO     MOVE 0 TO SW-PRORROGA                                        ABB0022
NUEPRO     PERFORM 1100-CONSULTAR-DIAS-PLAZO                            ABB0022
NUEPRO     ADD  DIAS-PLAZO TO DIAS-PLAZO-W                              ABB0022
NUEPRO     MOVE DIAS-PLAZO-W TO DIAS-PLAZO                              ABB0022
NUEPRO     PERFORM 1200-CALCULAR-FECHA-FILTRO                           ABB0022
NUEPRO     PERFORM 1350-CONSUL-RECLAMOS-PRORROGA                        ABB0022
NUEPRO     PERFORM 1400-PROCESAR-CUENTAS.                               ABB0022
NUEPRO******************************************************************ABB0022
       1100-CONSULTAR-DIAS-PLAZO.
REMARK******************************************************************
REMARK*1100-CONSULTAR-DIAS-PLAZO.                                      *
REMARK*    SE CONSULTA DE LA TABLA PARAMETROS DE LA BD ADMINISTRATIVA  *
REMARK*    LOS DIAS DE PLAZO (CODIGO DE PARAMETRO 006)                 *
REMARK******************************************************************
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL                                  VHF0017 
NUEPRO     IF SW-PRORROGA = 1                                           ABB0022
NUEPRO        STRING                                                    ABB0022
NUEPRO        "SELECT VALOR FROM "        DELIMITED BY SIZE,            ABB0022
NUEPRO        "PARAM.PARAMETROS WHERE "   DELIMITED BY SIZE,            ABB0022
NUEPRO        "COD_PARAMETRO = '024'"     DELIMITED BY SIZE,            ABB0022
NUEPRO        INTO INSSQL-BD-LIBSQL                                     ABB0022
MODCPY        DISPLAY "BUSCO PARAMETRO TIPO 024"                        GJC0023
MODCPY        MOVE x'00' TO INSSQL-BD-LIBSQL(64:1)                      GJC0023
NUEPRO     ELSE                                                         ABB0022
NUEPRO        STRING                                                    ABB0022
NUEPRO        "SELECT VALOR FROM "        DELIMITED BY SIZE,            ABB0022
NUEPRO        "PARAM.PARAMETROS WHERE "   DELIMITED BY SIZE,            ABB0022
NUEPRO        "COD_PARAMETRO = '006'"     DELIMITED BY SIZE,            ABB0022
NUEPRO        INTO INSSQL-BD-LIBSQL                                     ABB0022
MODCPY        MOVE x'00' TO INSSQL-BD-LIBSQL(64:1)                      GJC0023
MODCPY        DISPLAY "BUSCO PARAMETRO TIPO 006"                        GJC0023
NUEPRO     END-IF                                                       ABB0022
BDDDB2     PERFORM 70100-CONSULTA-SQL-LIBSQL                            VHF0017 
BDDDB2     IF CODRET-BD-LIBSQL <> 0                                     VHF0017 
ERRSQ2        MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-ERROR           VHF0004 
ERRSQ2        MOVE FUNCTION CURRENT-DATE (9:6) TO HORA-ERROR            VHF0004 
ERRSQ1        INITIALIZE REG-SALIDA                                     VHF0004
ERRSQ1        STRING                                                    VHF0004
ERRSQ1           " ERROR EN SELECT DE PARAMETRO"   DELIMITED BY SIZE    VHF0004 
ERRSQ1           " DIAS DE PLAZO"                  DELIMITED BY SIZE    VHF0004 
ERRSQ2           " FECHA: "                        DELIMITED BY SIZE    VHF0004 
ERRSQ2           FECHA-ERROR                       DELIMITED BY SIZE    VHF0004 
ERRSQ2           HORA-ERROR                        DELIMITED BY SIZE    VHF0004 
ERRSQ1           " SQLCOD("                        DELIMITED BY SIZE    VHF0004 
BDDDB2           CODRET-ED-LIBSQL                  DELIMITED BY " "     VHF0017 
ERRSQ1           ")"                               DELIMITED BY SIZE    VHF0004 
ERRSQ1           INTO REG-SALIDA                                        VHF0004
ERRSQ1        END-STRING                                                VHF0004
ERRSQL        DISPLAY REG-SALIDA                                        VHF0003 
ERRSQL        WRITE REG-SALIDA                                          VHF0003 
BDDDB2        PERFORM 9000-CERRAR-ARCHIVOS                              VHF0017 
ERRSQL        STOP RUN                                                  VHF0003 
ERRSQL     ELSE                                                         VHF0003 
REMARK*       DADO QUE ES VARCHAR SE UTILIZA LA LONGITUD DE LA          VHF0017
REMARK*       VARIABLE PARA DETERMINAR SI SE OBTUVO EL DATO             VHF0017
BDDDB2        IF RESSEL-02-L-LIBSQL = 0                                 VHF0017 
ERRSQL           MOVE " ERROR: NO SE ENCONTRO PARAMETRO DIAS DE PLAZO"  VHF0003 
ERRSQL                TO REG-SALIDA                                     VHF0003 
ERRSQL           DISPLAY REG-SALIDA                                     VHF0003 
ERRSQL           WRITE REG-SALIDA                                       VHF0003 
BDDDB2           PERFORM 9000-CERRAR-ARCHIVOS                           VHF0017 
ERRSQL           STOP RUN                                               VHF0003 
ERRSQL        ELSE                                                      VHF0003 
BDDDB2           UNSTRING RESSEL-02-V-LIBSQL DELIMITED BY " "           VHF0017 
BDDDB2                    INTO DIAS-PLAZO                               VHF0017 
ERRSQL        END-IF                                                    VHF0003 
MODCPY     DISPLAY "DIAS-PLAZO=",DIAS-PLAZO                             GJC0023
           END-IF.

       1200-CALCULAR-FECHA-FILTRO.
REMARK******************************************************************
REMARK*1200-CALCULAR-FECHA-FILTRO.                                     *
REMARK*    CON BASE EN LOS DIAS DE PLAZO Y LA TABLA DE DIAS NO HABILES *
REMARK*    SE CALCULA LA FECHA DESDE LA CUAL SE HARA LA CONSULTA DE    *
REMARK*    RECLAMOS PENDIENTES PARA RATIFICAR O BLOQUEAR POR           *
REMARK*    VENCIMIENTO DE PLAZO                                        *
REMARK******************************************************************
           MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-AUX-R
DIAAUT     MOVE FUNCTION CURRENT-DATE (9:6) TO HORA-AUX-R
DIAAUT     IF HOR-AUX < 20
REMARK***     SE CONSIDERA QUE ANTES DE LAS 8:00 PM SE DEBE EJECUTAR EL VHF0002
REMARK***     PROCESO CON FECHA DEL DIA ANTERIOR                        VHF0002
DIAAUT        PERFORM 5100-CALCULAR-NUEVA-FECHA
DIAAUT     END-IF
DIANH3     PERFORM 5000-DETERMINAR-DIA-HABIL                            VHF0007 
DIANH3     MOVE IND-HABIL TO IND-DIAACT-HABIL                           VHF0007 
           PERFORM UNTIL DIAS-VENCE >= DIAS-PLAZO
              PERFORM 5000-DETERMINAR-DIA-HABIL
              IF IND-HABIL = SI
                 ADD 1 TO DIAS-VENCE
              END-IF
              PERFORM 5100-CALCULAR-NUEVA-FECHA
DIAAUT     END-PERFORM.

       1300-EJECUTAR-CONSUL-RECLAMOS.
REMARK******************************************************************
REMARK*1300-EJECUTAR-CONSUL-RECLAMOS.                                  *
REMARK*    SE CONSULTA DE LA TABLA MODIFICACION LOS RECLAMOS QUE ESTEN *
REMARK*    SIN FINALIZAR, CUYA FECHA DE COLOCACION SEA SUPERIOR A LA DE*
REMARK*    FILTRO CALCULADA ANTERIORMENTE Y QUE NO HAYAN SIDO          *
REMARK*    BLOQUEADOS O RATIFICADOS EN PROCESOS PREVIOS                *
REMARK******************************************************************
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL.
           STRING
              "SELECT TIP_ID || "                     DELIMITED BY SIZE,
              "CAST(CAST(NRO_ID AS INT) AS CHAR(11)) || "
                                                      DELIMITED BY SIZE,
              "COD_ENTIDAD || "                       DELIMITED BY SIZE,
              "COD_CTA || "                           DELIMITED BY SIZE,
              "NRO_CTA || "                           DELIMITED BY SIZE,
              "CAST(NRO_CONSECUTIVO AS CHAR(10)) || " DELIMITED BY SIZE,
BDDDB2        "REGISTRO.MODIFICACION.COD_INGRESO ||"  DELIMITED BY SIZE,VHF0017 
              "COD_ESTADO ||"                         DELIMITED BY SIZE,
              "COD_CAD_RECLAMO "                      DELIMITED BY SIZE,
BDDDB2        "FROM REGISTRO.MODIFICACION, APOYO.TIP_INGRESO "          VHF0017
BDDDB2                                                DELIMITED BY SIZE,VHF0017 
BDDDB2        "WHERE REGISTRO.MODIFICACION.COD_INGRESO = "              VHF0017
BDDDB2                                                DELIMITED BY SIZE,VHF0017 
BDDDB2        "APOYO.TIP_INGRESO.COD_INGRESO AND "    DELIMITED BY SIZE,VHF0017 
TIPING        "IND = 'R' AND COD_ESTADO <> '3' AND "  DELIMITED BY SIZE,VHF0010 
INDBLQ        "BLO_AUTORREGULA = '0' AND "            DELIMITED BY SIZE,VHF0003 
NUEPRO        "COD_TIP_PRORROGA <> '1' AND "          DELIMITED BY SIZE,ABB0022
NUEPRO        "COD_LEY_RECLAMO  <> '2' AND "          DELIMITED BY SIZE,ABB0022
              "FEC_COLOCA <= '"                       DELIMITED BY SIZE,
              ANO-AUX                                 DELIMITED BY SIZE,
BDDDB2        "-"                                     DELIMITED BY SIZE,VHF0017 
              MES-AUX                                 DELIMITED BY SIZE,
BDDDB2        "-"                                     DELIMITED BY SIZE,VHF0017 
              DIA-AUX                                 DELIMITED BY SIZE,
              " "                                     DELIMITED BY SIZE,
              "23:59:59'"                             DELIMITED BY SIZE,
BDDDB2        INTO INSSQL-BD-LIBSQL                                     VHF0017 
BDDDB2     MOVE TITLE-TEMPORAL TO TITLE-LIBSQL                          VHF0017 
BDDDB2     PERFORM 70200-CONSUL01-SQL-LIBSQL                            VHF0017 
BDDDB2     IF CODRET-BD-LIBSQL <> 0 AND CODRET-BD-LIBSQL <> 100         VHF0017 
ERRSQ2        MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-ERROR           VHF0004 
ERRSQ2        MOVE FUNCTION CURRENT-DATE (9:6) TO HORA-ERROR            VHF0004 
ERRSQ1        INITIALIZE REG-SALIDA                                     VHF0004
ERRSQ1        STRING                                                    VHF0004
ERRSQ1           " ERROR EN SELECT DE CUENTAS"     DELIMITED BY SIZE    VHF0004 
ERRSQ1           " PARA AUTOREGULAR"               DELIMITED BY SIZE    VHF0004 
ERRSQ2           " FECHA: "                        DELIMITED BY SIZE    VHF0004 
ERRSQ2           FECHA-ERROR                       DELIMITED BY SIZE    VHF0004 
ERRSQ2           HORA-ERROR                        DELIMITED BY SIZE    VHF0004 
ERRSQ1           " SQLCOD("                        DELIMITED BY SIZE    VHF0004 
BDDDB2           CODRET-ED-LIBSQL                  DELIMITED BY " "     VHF0017 
ERRSQ1           ")"                               DELIMITED BY SIZE    VHF0004 
ERRSQ1           INTO REG-SALIDA                                        VHF0004
ERRSQ1        END-STRING                                                VHF0004
ERRSQL        DISPLAY REG-SALIDA                                        VHF0003 
ERRSQL        WRITE REG-SALIDA                                          VHF0003 
BDDDB2        PERFORM 9000-CERRAR-ARCHIVOS                              VHF0017 
ERRSQL        STOP RUN                                                  VHF0003 
           END-IF.
NUEPRO 1350-CONSUL-RECLAMOS-PRORROGA.                                   ABB0022
NUEPRO******************************************************************ABB0022
NUEPRO*1300-EJECUTAR-CONSUL-RECLAMOS.                                  *ABB0022
NUEPRO*    SE CONSULTA DE LA TABLA MODIFICACION LOS RECLAMOS QUE ESTEN *ABB0022
NUEPRO*    SIN FINALIZAR, CUYA FECHA DE PRORROGA   SEA SUPERIOR A LA DE*ABB0022
NUEPRO*    FILTRO CALCULADA ANTERIORMENTE Y QUE NO HAYAN SIDO          *ABB0022
NUEPRO*    BLOQUEADOS O RATIFICADOS EN PROCESOS PREVIOS                *ABB0022
NUEPRO******************************************************************ABB0022
NUEPRO     INITIALIZE INSSQL-BD-LIBSQL                                  ABB0022
NUEPRO     STRING                                                       ABB0022
NUEPRO        "SELECT TIP_ID || "                     DELIMITED BY SIZE,ABB0022
NUEPRO        "CAST(CAST(NRO_ID AS INT) AS CHAR(11)) || "               ABB0022
NUEPRO                                                DELIMITED BY SIZE,ABB0022
NUEPRO        "COD_ENTIDAD || "                       DELIMITED BY SIZE,ABB0022
NUEPRO        "COD_CTA || "                           DELIMITED BY SIZE,ABB0022
NUEPRO        "NRO_CTA || "                           DELIMITED BY SIZE,ABB0022
NUEPRO        "CAST(NRO_CONSECUTIVO AS CHAR(10)) || " DELIMITED BY SIZE,ABB0022
NUEPRO        "REGISTRO.MODIFICACION.COD_INGRESO ||"  DELIMITED BY SIZE,ABB0022
NUEPRO        "COD_ESTADO ||"                         DELIMITED BY SIZE,ABB0022
NUEPRO        "COD_CAD_RECLAMO "                      DELIMITED BY SIZE,ABB0022
NUEPRO        "FROM REGISTRO.MODIFICACION, APOYO.TIP_INGRESO "          ABB0022
NUEPRO                                                DELIMITED BY SIZE,ABB0022
NUEPRO        "WHERE REGISTRO.MODIFICACION.COD_INGRESO = "              ABB0022
NUEPRO                                                DELIMITED BY SIZE,ABB0022
NUEPRO        "APOYO.TIP_INGRESO.COD_INGRESO AND "    DELIMITED BY SIZE,ABB0022
NUEPRO        "IND = 'R' AND COD_ESTADO <> '3' AND "  DELIMITED BY SIZE,ABB0022
NUEPRO        "COD_TIP_PRORROGA = '1' AND "           DELIMITED BY SIZE,ABB0022
NUEPRO        "COD_LEY_RECLAMO  <> '2' AND "          DELIMITED BY SIZE,ABB0022
NUEPRO        "FEC_COLOCA <= '"                       DELIMITED BY SIZE,ABB0022
NUEPRO        ANO-AUX                                 DELIMITED BY SIZE,ABB0022
NUEPRO        "-"                                     DELIMITED BY SIZE,ABB0022
NUEPRO        MES-AUX                                 DELIMITED BY SIZE,ABB0022
NUEPRO        "-"                                     DELIMITED BY SIZE,ABB0022
NUEPRO        DIA-AUX                                 DELIMITED BY SIZE,ABB0022
NUEPRO        " "                                     DELIMITED BY SIZE,ABB0022
NUEPRO        "23:59:59'"                             DELIMITED BY SIZE,ABB0022
NUEPRO        INTO INSSQL-BD-LIBSQL                                     ABB0022
NUEPRO     MOVE TITLE-TEMPORAL03 TO TITLE-LIBSQL                        ABB0022
NUEPRO     PERFORM 70200-CONSUL01-SQL-LIBSQL                            ABB0022
NUEPRO     IF CODRET-BD-LIBSQL <> 0 AND CODRET-BD-LIBSQL <> 100         ABB0022
NUEPRO        MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-ERROR           ABB0022
NUEPRO        MOVE FUNCTION CURRENT-DATE (9:6) TO HORA-ERROR            ABB0022
NUEPRO        INITIALIZE REG-SALIDA                                     ABB0022
NUEPRO        STRING                                                    ABB0022
NUEPRO           " ERROR EN SELECT DE CUENTAS"     DELIMITED BY SIZE    ABB0022
NUEPRO           " PARA AUTOREGULAR PRORROGA"      DELIMITED BY SIZE    ABB0022
NUEPRO           " FECHA: "                        DELIMITED BY SIZE    ABB0022
NUEPRO           FECHA-ERROR                       DELIMITED BY SIZE    ABB0022
NUEPRO           HORA-ERROR                        DELIMITED BY SIZE    ABB0022
NUEPRO           " SQLCOD("                        DELIMITED BY SIZE    ABB0022
NUEPRO           CODRET-ED-LIBSQL                  DELIMITED BY " "     ABB0022
NUEPRO           ")"                               DELIMITED BY SIZE    ABB0022
NUEPRO           INTO REG-SALIDA                                        ABB0022
NUEPRO        END-STRING                                                ABB0022
NUEPRO        DISPLAY REG-SALIDA                                        ABB0022
NUEPRO        WRITE REG-SALIDA                                          ABB0022
NUEPRO        PERFORM 9000-CERRAR-ARCHIVOS                              ABB0022
NUEPRO        STOP RUN                                                  ABB0022
NUEPRO     END-IF.                                                      ABB0022

       1400-PROCESAR-CUENTAS.
REMARK******************************************************************
REMARK*1400-PROCESAR-CUENTAS.                                          *
REMARK*    SE RECORRE EL ARCHIVO TEMPORAL GENERADO POR LA CONSULTA Y SE*
REMARK*    VERIFICAN LAS CUENTAS INEXISTENTES, LAS BLOQUEADAS, LOS     *
REMARK*    INDICADORES DE RECLAMO Y MODIFICACION Y SI EXISTE EL        *
REMARK*    SUSCRIPTOR.                                                 *
REMARK******************************************************************
BDDDB2     OPEN INPUT LIBSQL                                            VHF0017 
           MOVE NO-ES TO FDA-TEMPORAL
BDDDB2     READ LIBSQL                                                  VHF0017 
              AT END 
DIANH3           MOVE SI TO FDA-TEMPORAL                                VHF0007 
DIANH3           INITIALIZE REG-SALIDA                                  VHF0007 
DIANH3           IF IND-DIAACT-HABIL = SI                               VHF0007 
DIANH3              STRING                                              VHF0007 
DIANH3                 "ADVERTENCIA: NO HAY "         DELIMITED BY SIZE,VHF0007 
DIANH3                 "REGISTROS PARA AUTOREGULAR"   DELIMITED BY SIZE,VHF0007 
DIANH3                 ". FAVOR VERIFICAR CON EL "    DELIMITED BY SIZE,VHF0007 
DIANH3                 "ADMINISTRADOR DE NOVEDAT"     DELIMITED BY SIZE VHF0007 
DIANH3                 INTO REG-SALIDA                                  VHF0007 
DIANH3              END-STRING                                          VHF0007 
DIANH3           ELSE                                                   VHF0007 
DIANH3              INITIALIZE REG-SALIDA                               VHF0007 
DIANH3              STRING                                              VHF0007 
DIANH3                 "NO HAY REGISTROS PENDIENTES"  DELIMITED BY SIZE,VHF0007 
DIANH3                 " PARA AUTOREGULAR"            DELIMITED BY SIZE,VHF0007 
DIANH3                 ". LA FECHA CORRESPONDE A "    DELIMITED BY SIZE,VHF0007 
DIANH3                 " UN DIA NO HABIL"             DELIMITED BY SIZE VHF0007 
DIANH3                 INTO REG-SALIDA                                  VHF0007 
DIANH3              END-STRING                                          VHF0007 
DIANH3           END-IF                                                 VHF0007 
DIANH3           WRITE REG-SALIDA                                       VHF0007 
DIANH3           DISPLAY REG-SALIDA                                     VHF0007 
           END-READ
           PERFORM UNTIL FDA-TEMPORAL = SI
REMARK**      SE CONVIERTEN A NUMERICOS EL NUMERO DE IDENTIFICACION Y EL
REMARK**      CONSECUTIVO DE LA TRANSACCION
BDDDB2        MOVE REG-LIBSQL TO RESSEL-02-LIBSQL                       VHF0017 
BDDDB2        MOVE RESSEL-02-V-LIBSQL TO REG-TEMPORAL                   VHF0017 
              UNSTRING NUMIDE-TEMPORAL DELIMITED BY " " INTO NUMIDE-AUX
              UNSTRING NUMCON-TEMPORAL DELIMITED BY " " INTO NUMCON-AUX
CIUREC        UNSTRING CODING-TEMPORAL DELIMITED BY " " INTO CODING-AUX VHF0008 
              MOVE NUMIDE-AUX TO NUMIDE-TEMPORAL
              MOVE NUMCON-AUX TO NUMCON-TEMPORAL
CIUREC        MOVE CODING-AUX TO CODING-TEMPORAL                        VHF0008 
              IF IND-DESPLIEGUE = 0
                 DISPLAY " VAN  ....... " CON-LEIDOS
              END-IF
              ADD 1 TO CON-LEIDOS
REMARK***     SE DETERMINA SI ES UNA CUENTA SIN VALIDAR                 VHF0020
MATC02        MOVE TIPIDE-TEMPORAL TO TIP-IDE-BDIIVIDE                  VHF0020
MATC02        MOVE NUMIDE-TEMPORAL TO NUM-IDE-BDIIVIDE                  VHF0020
MATC02        MOVE 0 TO SW-VERIFNOM-BDIIVIDE                            VHF0020
MATC02        PERFORM 80100-VALIDAR-IDENTIFICACION                      VHF0020
MATC02        IF SW-ID-BDIIVIDE = 1                                     VHF0020
REMARK***        IDENTIFICACION NO EXISTE EN BDI DATACREDITO SE         VHF0020
REMARK***        INTENTA BUSCAR EN ARCHIVO DE CUENTAS DE MATCH          VHF0020
MATC02           MOVE TIPIDE-TEMPORAL TO TIP-IDE-INPUT-BDAMATCH         VHF0020
MATC02           MOVE NUMIDE-TEMPORAL TO NUM-IDE-INPUT-BDAMATCH         VHF0020
MATC02           PERFORM 1000-U-RECORRER-CSV-BDAMATCH                   VHF0020
MATC02           MOVE IDE-TIPO-BDAMATCH TO IND-CSV                      VHF0020
MATC02        END-IF                                                    VHF0020
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-LEI-CHECK 
DTRLL2              MOVE NO-ES TO IND-USO-DTRLLA                        VHF0007 
                    MOVE LLAVE-TEMPORAL TO LLAVE-MAES-CHECK
                    PERFORM 9200-LEER-MAES-CHECK
                    IF ENC-MAESTRO = SI
DTRLL2                 IF LLAVE-MAES-CHECK <> LLAVE-TEMPORAL            VHF0007 
DTRLL2                    IF AAAAMMDD-REN-AUX >= FEC-USO-DTRLLA         VHF0007 
DTRLL2                       MOVE "  !!! ADVERTENCIA !!!"               VHF0007 
DTRLL2                            TO REFERENCIA-EMAIL                   VHF0007 
DTRLL2                       INITIALIZE MENSAJE-EMAIL                   VHF0007 
DTRLL2                       STRING "SE UTILIZO ARCHIVO DTRLLA PARA "   VHF0007 
DTRLL2                                             DELIMITED BY SIZE,   VHF0007 
DTRLL2                              LLAVE-TEMPORAL DELIMITED BY SIZE    VHF0007 
DTRLL2                              INTO MENSAJE-EMAIL                  VHF0007 
DTRLL2                       CALL "SYSTEM" USING COMANDO-EMAIL          VHF0007 
DTRLL2                    END-IF                                        VHF0007 
DTRLL2                 END-IF                                           VHF0007 
                       MOVE IND-BLOQUEO-ABD06 TO IND-BLOQUEO-AUX
                       IF IND-RECLAMO-ABD06 NUMERIC
                          MOVE IND-RECLAMO-ABD06 TO IND-RECLAMO-AUX
                       ELSE
                          MOVE 0 TO IND-RECLAMO-AUX
                       END-IF
                    END-IF
                 WHEN OTHER
                    ADD 1 TO CON-LEI-CREDIT
DTRLL2              MOVE NO-ES TO IND-USO-DTRLLA                        VHF0007 
                    MOVE LLAVE-TEMPORAL TO LLAVE-MAES-CREDIT
                    PERFORM 9210-LEER-MAES-CREDIT
                    IF ENC-MAESTRO = SI
DTRLL2                 IF LLAVE-MAES-CREDIT <> LLAVE-TEMPORAL           VHF0007 
DTRLL2                    IF AAAAMMDD-REN-AUX >= FEC-USO-DTRLLA         VHF0007 
DTRLL2                       MOVE "  !!! ADVERTENCIA !!!"               VHF0007 
DTRLL2                            TO REFERENCIA-EMAIL                   VHF0007 
DTRLL2                       INITIALIZE MENSAJE-EMAIL                   VHF0007 
DTRLL2                       STRING "SE UTILIZO ARCHIVO DTRLLA PARA "   VHF0007 
DTRLL2                                             DELIMITED BY SIZE,   VHF0007 
DTRLL2                              LLAVE-TEMPORAL DELIMITED BY SIZE    VHF0007 
DTRLL2                              INTO MENSAJE-EMAIL                  VHF0007 
DTRLL2                       CALL "SYSTEM" USING COMANDO-EMAIL          VHF0007 
DTRLL2                    END-IF                                        VHF0007 
DTRLL2                 END-IF                                           VHF0007 
                       MOVE IND-BLOQUEO-ABD07 TO IND-BLOQUEO-AUX
                       IF IND-RECLAMO-ABD07 NUMERIC
                          MOVE IND-RECLAMO-ABD07 TO IND-RECLAMO-AUX
                       ELSE
                          MOVE 0 TO IND-RECLAMO-AUX
                       END-IF
                    END-IF
              END-EVALUATE
REMARK**      SE DETERMINA SI EXISTE LA CUENTA EN EL MAESTRO
              IF ENC-MAESTRO = NO-ES
                 PERFORM 4200-FINALIZAR-RECLAMO-AUT
              ELSE
REMARK**         SE VALIDA SI HAY RECLAMO VIGENTE
                 IF IND-RECLAMO-AUX = 1 OR 
                    IND-RECLAMO-AUX = 2 OR 
                    IND-RECLAMO-AUX = 3
REMARK**            SE VERIFICA LA EXISTENCIA DEL SUSCRIPTOR
                    MOVE SI TO ENC-SUSCRIPTOR
                    MOVE CODSUS-TEMPORAL TO COD-SUSCRIP-ABD02
                    READ SUSCRIPTOR
                       INVALID KEY
                          MOVE NO-ES TO ENC-SUSCRIPTOR
                    END-READ
                    IF ENC-SUSCRIPTOR = SI
REMARK**               SE VERIFICA QUE LA CUENTA NO ESTE BLOQUEADA
                       IF IND-BLOQUEO-AUX = 0 OR  
                          IND-BLOQUEO-AUX = 7 OR 
                          IND-BLOQUEO-AUX = 8
                          EVALUATE INDCAD-TEMPORAL
                             WHEN "1"
                                PERFORM 2000-BLOQUEAR-CUENTAS
                             WHEN "2"
                                PERFORM 3000-RATIFICAR-CUENTAS
                             WHEN OTHER
                                INITIALIZE REG-SALIDA
                                STRING 
                                   " ERROR: EN TIPO DE CADUCIDAD" 
                                                      DELIMITED BY SIZE,
                                   " - "              DELIMITED BY SIZE,
                                   LLAVE-TEMPORAL     DELIMITED BY SIZE
                                   INTO REG-SALIDA
                                WRITE REG-SALIDA
                                EVALUATE CODCTA-TEMPORAL
                                   WHEN 01
                                      ADD 1 TO CON-ERR-CHECK 
                                   WHEN OTHER
                                      ADD 1 TO CON-ERR-CREDIT
                                END-EVALUATE
                          END-EVALUATE
                       ELSE
                          EVALUATE CODCTA-TEMPORAL
                             WHEN 01
                                PERFORM 3500-FINALIZAR-BLQ-CTACTE
                             WHEN OTHER
                                PERFORM 3510-FINALIZAR-BLQ-CREDIT 
                          END-EVALUATE
                       END-IF
                    ELSE
                       INITIALIZE REG-SALIDA
                       STRING 
                          " ERROR: SUSCRIPTOR NO ENCONTRADO" 
                                                   DELIMITED BY SIZE,
                          " - "                    DELIMITED BY SIZE,
                          LLAVE-TEMPORAL           DELIMITED BY SIZE
                          INTO REG-SALIDA
                       WRITE REG-SALIDA
                       EVALUATE CODCTA-TEMPORAL
                          WHEN 01
                             ADD 1 TO CON-ERR-CHECK
                          WHEN OTHER
                             ADD 1 TO CON-ERR-CREDIT
                       END-EVALUATE
                    END-IF
                 ELSE
                    INITIALIZE REG-SALIDA
                    STRING
                       " ERROR: CUENTA SIN RECLAMO"
                                                   DELIMITED BY SIZE,
                       " ABIERTO EN UNIX - "       DELIMITED BY SIZE,
                       LLAVE-TEMPORAL              DELIMITED BY SIZE
                       INTO REG-SALIDA
                    WRITE REG-SALIDA
                    EVALUATE CODCTA-TEMPORAL
                       WHEN 01
                          ADD 1 TO CON-ERR-CHECK 
                       WHEN OTHER
                          ADD 1 TO CON-ERR-CREDIT 
                    END-EVALUATE
                 END-IF
              END-IF
BDDDB2        READ LIBSQL                                               VHF0017 
                 AT END
                    MOVE SI TO FDA-TEMPORAL
              END-READ
           END-PERFORM
BDDDB2     CLOSE LIBSQL.                                                VHF0017 

       2000-BLOQUEAR-CUENTAS.
REMARK******************************************************************
REMARK*2000-BLOQUEAR-CUENTAS.                                          *
REMARK*    DE ACUERDO AL TIPO DE CUENTA SE EJECUTA LA RUTINA DE BLOQUEO*
REMARK******************************************************************
           IF IND-BLOQUEO-AUX <> 8
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    PERFORM 2100-BLOQUEAR-MAES-CHECK
                 WHEN OTHER
                    PERFORM 2200-BLOQUEAR-MAES-CREDIT
              END-EVALUATE
           ELSE
              INITIALIZE REG-SALIDA
              STRING
                 " ERROR: CUENTA YA FUE BLOQUEADA TEMPORALMENTE - "
                                                     DELIMITED BY SIZE,
                 LLAVE-TEMPORAL                      DELIMITED BY SIZE
                 INTO REG-SALIDA
              WRITE REG-SALIDA
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-ERR-CHECK 
                 WHEN OTHER
                    ADD 1 TO CON-ERR-CREDIT
              END-EVALUATE
           END-IF.
   
       2100-BLOQUEAR-MAES-CHECK.
REMARK******************************************************************
REMARK*2100-BLOQUEAR-MAES-CHECK.                                       *
REMARK*    SE TOMA CADA LLAVE DE CUENTA, SE BLOQUEA EN EL UNIX Y SE    *
REMARK*    REGISTRA EN EL LOG (DTCLOG), SE ADICIONA EL EVENTO A LA     *
REMARK*    TABLA EVENTO-ESTADO Y SE CAMBIA EL CODIGO DE SOLUCION EN LA *
REMARK*    TABLA MODIFICACION DE LA BD ADMINISTRATIVA                  *
REMARK******************************************************************
MATC02     IF SW-ID-BDIIVIDE = 1 AND IND-CSV = 1 AND                    VHF0020
MATC02        IND-BLOQUEO-ABD06 = 0                                     VHF0020
REMARK**      CORRESPONDE A UNA CUENTA SIN VALIDAR                      VHF0020
MATC02        MOVE 9 TO IND-BLOQUEO-ABD06                               VHF0020
MATC02     END-IF                                                       VHF0020
           MOVE COMPLETO-ABD06 TO COMPLETO-ABD06-AUX
           MOVE 02 TO TIP-CTA-RECLAMO-WS
           PERFORM 3300-GENERAR-R-MAESTRO-PLANO
           MOVE REG-MAESTR-PLANO TO REG-PLANO-INI
           MOVE 8 TO IND-BLOQUEO-ABD06
           MOVE 2 TO TIP-CALIFIC-ABD06
           PERFORM 9300-ACTUALIZAR-MAES-CHECK
           IF ACT-MAESTRO = SI
              PERFORM 3300-GENERAR-R-MAESTRO-PLANO
              MOVE REG-MAESTR-PLANO TO REG-PLANO-FIN
              PERFORM 4000-ACTUALIZAR-BD-ADMIN
LOGCTE        IF IND-ERR-BD = NO-ES                                     VHF0011 
                 MOVE CODEST-TEMPORAL TO COD-ESTADO-AUX
                 PERFORM 4970-GRABAR-LOGS
              END-IF
           ELSE
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO MAESTRO CHECK - "
                                              DELIMITED BY SIZE,
                     LLAVE-TEMPORAL           DELIMITED BY SIZE
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              ADD 1 TO CON-ERR-CHECK 
           END-IF.

       2200-BLOQUEAR-MAES-CREDIT.
REMARK******************************************************************
REMARK*2200-BLOQUEAR-MAES-CREDIT.                                      *
REMARK*    SE TOMA CADA LLAVE DE CUENTA, SE BLOQUEA EN EL UNIX Y SE    *
REMARK*    REGISTRA EN EL LOG (DTCLOG), SE ADICIONA EL EVENTO A LA     *
REMARK*    TABLA EVENTO-ESTADO Y SE CAMBIA EL CODIGO DE SOLUCION EN LA *
REMARK*    TABLA MODIFICACION DE LA BD ADMINISTRATIVA                  *
REMARK******************************************************************
MATC02     IF SW-ID-BDIIVIDE = 1 AND IND-CSV = 1 AND                    VHF0020
MATC02        IND-BLOQUEO-ABD07 = 0                                     VHF0020
REMARK**      CORRESPONDE A UNA CUENTA SIN VALIDAR                      VHF0020
MATC02        MOVE 9 TO IND-BLOQUEO-ABD07                               VHF0020
MATC02     END-IF                                                       VHF0020
           MOVE COMPLETO-ABD07 TO COMPLETO-ABD07-AUX
           MOVE 03 TO TIP-CTA-RECLAMO-WS
           PERFORM 3300-GENERAR-R-MAESTRO-PLANO
           MOVE REG-MAESTR-PLANO TO REG-PLANO-INI
           MOVE 8 TO IND-BLOQUEO-ABD07
           MOVE 2 TO TIP-CALIFIC-ABD07
           PERFORM 9310-ACTUALIZAR-MAES-CREDIT
           IF ACT-MAESTRO = SI
              PERFORM 3300-GENERAR-R-MAESTRO-PLANO
              MOVE REG-MAESTR-PLANO TO REG-PLANO-FIN
              PERFORM 4000-ACTUALIZAR-BD-ADMIN
              IF IND-ERR-BD = NO-ES 
                 MOVE CODEST-TEMPORAL TO COD-ESTADO-AUX
                 PERFORM 4970-GRABAR-LOGS
              END-IF
           ELSE
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO MAESTRO CREDIT - "
                                              DELIMITED BY SIZE,
                     LLAVE-TEMPORAL           DELIMITED BY SIZE
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              ADD 1 TO CON-ERR-CREDIT
           END-IF.

       3000-RATIFICAR-CUENTAS.
REMARK******************************************************************
REMARK*3000-RATIFICAR-CUENTAS.                                         *
REMARK*    DE ACUERDO AL TIPO DE CUENTA SE EJECUTA LA RUTINA DE        *
REMARK*    RATIFICACION                                                *
REMARK******************************************************************
           IF IND-BLOQUEO-AUX <> 8
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    PERFORM 3100-RATIFICAR-MAES-CHECK
                 WHEN OTHER
                    PERFORM 3200-RATIFICAR-MAES-CREDIT
              END-EVALUATE
           ELSE
              INITIALIZE REG-SALIDA
              STRING
                 " ERROR: CUENTA FUE BLOQUEADA TEMPORALMENTE - "
                                                     DELIMITED BY SIZE,
                 LLAVE-TEMPORAL                      DELIMITED BY SIZE
                 INTO REG-SALIDA
              WRITE REG-SALIDA
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-ERR-CHECK 
                 WHEN OTHER
                    ADD 1 TO CON-ERR-CREDIT
              END-EVALUATE
           END-IF.
   
       3100-RATIFICAR-MAES-CHECK.
REMARK******************************************************************
REMARK*3100-RATIFICAR-MAES-CHECK.                                      *
REMARK*    SE TOMA CADA LLAVE DE CUENTA, SE CIERRA EN EL UNIX Y SE     *
REMARK*    REGISTRA EN EL LOG (DTCLOG), SE ADICIONAN LOS EVENTOS A LA  *
REMARK*    TABLA EVENTO-ESTADO Y SE CAMBIA EL CODIGO DE SOLUCION Y     *
REMARK*    ESTADO EN TABLA MODIFICACION DE LA BD ADMINISTRATIVA PARA   *
REMARK*    DAR POR FINALIZADO EL RECLAMO POR RATIFICACION              *
REMARK******************************************************************
MATC02     IF SW-ID-BDIIVIDE = 1 AND IND-CSV = 1 AND                    VHF0020
MATC02        IND-BLOQUEO-ABD06 = 0                                     VHF0020
REMARK**      CORRESPONDE A UNA CUENTA SIN VALIDAR                      VHF0020
MATC02        MOVE 9 TO IND-BLOQUEO-ABD06                               VHF0020
MATC02     END-IF                                                       VHF0020
           MOVE COMPLETO-ABD06 TO COMPLETO-ABD06-AUX
           MOVE 02 TO TIP-CTA-RECLAMO-WS
           PERFORM 3300-GENERAR-R-MAESTRO-PLANO
           MOVE REG-MAESTR-PLANO TO REG-PLANO-INI
REGRA2                                 REG-CTACTE-A                     VHF0013 
           MOVE 2 TO TIP-CALIFIC-ABD06
           MOVE 4 TO IND-RECLAMO-ABD06
           PERFORM 9300-ACTUALIZAR-MAES-CHECK
           IF ACT-MAESTRO = SI
              PERFORM 3300-GENERAR-R-MAESTRO-PLANO
              MOVE REG-MAESTR-PLANO TO REG-PLANO-FIN
REGRAT                                 REG-CTACTE-D                     VHF0011 
              MOVE SI TO IND-RATIFICACION
              PERFORM 4500-CERRAR-COMENTARIO
              PERFORM 4100-ACTUALIZAR-BD-ADMIN2
LOGCTE        IF IND-ERR-BD = NO-ES                                     VHF0011 
                 MOVE "3" TO COD-ESTADO-AUX
                 PERFORM 4970-GRABAR-LOGS
              END-IF
           ELSE
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO MAESTRO CHECK - "
                                              DELIMITED BY SIZE,
                     LLAVE-TEMPORAL           DELIMITED BY SIZE
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              ADD 1 TO CON-ERR-CHECK 
           END-IF.

       3200-RATIFICAR-MAES-CREDIT.
REMARK******************************************************************
REMARK*3200-RATIFICAR-MAES-CREDIT.                                     *
REMARK*    SE TOMA CADA LLAVE DE CUENTA, SE CIERRA EN EL UNIX Y SE     *
REMARK*    REGISTRA EN EL LOG (DTCLOG), SE ADICIONAN LOS EVENTOS A LA  *
REMARK*    TABLA EVENTO-ESTADO Y SE CAMBIA EL CODIGO DE SOLUCION Y     *
REMARK*    ESTADO EN TABLA MODIFICACION DE LA BD ADMINISTRATIVA PARA   *
REMARK*    DAR POR FINALIZADO EL RECLAMO POR RATIFICACION              *
REMARK******************************************************************
MATC02     IF SW-ID-BDIIVIDE = 1 AND IND-CSV = 1 AND                    VHF0020
MATC02        IND-BLOQUEO-ABD07 = 0                                     VHF0020
REMARK**      CORRESPONDE A UNA CUENTA SIN VALIDAR                      VHF0020
MATC02        MOVE 9 TO IND-BLOQUEO-ABD07                               VHF0020
MATC02     END-IF                                                       VHF0020
           MOVE COMPLETO-ABD07 TO COMPLETO-ABD07-AUX
           MOVE 03 TO TIP-CTA-RECLAMO-WS
           PERFORM 3300-GENERAR-R-MAESTRO-PLANO
           MOVE REG-MAESTR-PLANO TO REG-PLANO-INI
REGRA2                                 REG-CARTERA-A                    VHF0013 
           MOVE 2 TO TIP-CALIFIC-ABD07
           MOVE 4 TO IND-RECLAMO-ABD07
           PERFORM 9310-ACTUALIZAR-MAES-CREDIT
           IF ACT-MAESTRO = SI
              PERFORM 3300-GENERAR-R-MAESTRO-PLANO
              MOVE REG-MAESTR-PLANO TO REG-PLANO-FIN
REGRAT                                 REG-CARTERA-D                    VHF0011 
              MOVE SI TO IND-RATIFICACION
              PERFORM 4500-CERRAR-COMENTARIO
              PERFORM 4100-ACTUALIZAR-BD-ADMIN2
              IF IND-ERR-BD = NO-ES 
                 MOVE "3" TO COD-ESTADO-AUX
                 PERFORM 4970-GRABAR-LOGS
              END-IF
           ELSE
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO MAESTRO CREDIT - "
                                              DELIMITED BY SIZE,
                     LLAVE-TEMPORAL           DELIMITED BY SIZE
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              ADD 1 TO CON-ERR-CREDIT
           END-IF.

       3500-FINALIZAR-BLQ-CTACTE.
REMARK******************************************************************
REMARK*3500-FINALIZAR-BLQ-CTACTE.                                      *
REMARK*    SE TOMA CADA LLAVE DE CUENTA, SE CIERRA RECLAMO EN UNIX Y SE*
REMARK*    REGISTRA EN EL LOG (DTCLOG), SE ADICIONAN LOS EVENTOS EN LA *
REMARK*    TABLA EVENTO-ESTADO Y SE CIERRA EL RECLAMO EN LA BD         *
REMARK*    ADMINISTRATIVA                                              *
REMARK******************************************************************
MATC02     IF SW-ID-BDIIVIDE = 1 AND IND-CSV = 1 AND                    VHF0020
MATC02        IND-BLOQUEO-ABD06 = 0                                     VHF0020
REMARK**      CORRESPONDE A UNA CUENTA SIN VALIDAR                      VHF0020
MATC02        MOVE 9 TO IND-BLOQUEO-ABD06                               VHF0020
MATC02     END-IF                                                       VHF0020
           MOVE COMPLETO-ABD06 TO COMPLETO-ABD06-AUX
           MOVE 02 TO TIP-CTA-RECLAMO-WS
           PERFORM 3300-GENERAR-R-MAESTRO-PLANO
           MOVE REG-MAESTR-PLANO TO REG-PLANO-INI
           MOVE 4 TO IND-RECLAMO-ABD06
           MOVE 2 TO TIP-CALIFIC-ABD06
           PERFORM 9300-ACTUALIZAR-MAES-CHECK
           IF ACT-MAESTRO = SI
              PERFORM 3300-GENERAR-R-MAESTRO-PLANO
              MOVE REG-MAESTR-PLANO TO REG-PLANO-FIN
              MOVE NO-ES TO IND-RATIFICACION
              PERFORM 4500-CERRAR-COMENTARIO
              PERFORM 4200-FINALIZAR-RECLAMO-AUT
LOGCTE        IF IND-ERR-BD = NO-ES                                     VHF0011 
                 MOVE "3" TO COD-ESTADO-AUX
                 PERFORM 4970-GRABAR-LOGS
              END-IF
           ELSE
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO MAESTRO CHECK - "
                                              DELIMITED BY SIZE,
                     LLAVE-TEMPORAL           DELIMITED BY SIZE
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              ADD 1 TO CON-ERR-CHECK 
           END-IF.

       3510-FINALIZAR-BLQ-CREDIT.
REMARK******************************************************************
REMARK*3510-FINALIZAR-BLQ-CREDIT.                                      *
REMARK*    SE TOMA CADA LLAVE DE CUENTA, SE CIERRA RECLAMO EN UNIX Y SE*
REMARK*    REGISTRA EN EL LOG (DTCLOG), SE ADICIONAN LOS EVENTOS EN LA *
REMARK*    TABLA EVENTO-ESTADO Y SE CIERRA EL RECLAMO EN LA BD         *
REMARK*    ADMINISTRATIVA                                              *
REMARK******************************************************************
MATC02     IF SW-ID-BDIIVIDE = 1 AND IND-CSV = 1 AND                    VHF0020
MATC02        IND-BLOQUEO-ABD07 = 0                                     VHF0020
REMARK**      CORRESPONDE A UNA CUENTA SIN VALIDAR                      VHF0020
MATC02        MOVE 9 TO IND-BLOQUEO-ABD07                               VHF0020
MATC02     END-IF                                                       VHF0020
           MOVE COMPLETO-ABD07 TO COMPLETO-ABD07-AUX
           MOVE 03 TO TIP-CTA-RECLAMO-WS
           PERFORM 3300-GENERAR-R-MAESTRO-PLANO
           MOVE REG-MAESTR-PLANO TO REG-PLANO-INI
           MOVE 4 TO IND-RECLAMO-ABD07
           MOVE 2 TO TIP-CALIFIC-ABD07
           PERFORM 9310-ACTUALIZAR-MAES-CREDIT
           IF ACT-MAESTRO = SI
              PERFORM 3300-GENERAR-R-MAESTRO-PLANO
              MOVE REG-MAESTR-PLANO TO REG-PLANO-FIN
              MOVE NO-ES TO IND-RATIFICACION
              PERFORM 4500-CERRAR-COMENTARIO
              PERFORM 4200-FINALIZAR-RECLAMO-AUT
LOGCTE        IF IND-ERR-BD = NO-ES                                     VHF0011 
                 MOVE "3" TO COD-ESTADO-AUX
                 PERFORM 4970-GRABAR-LOGS
              END-IF
           ELSE
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO MAESTRO CREDIT - "
                                              DELIMITED BY SIZE,
                     LLAVE-TEMPORAL           DELIMITED BY SIZE
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              ADD 1 TO CON-ERR-CREDIT
           END-IF.

       4000-ACTUALIZAR-BD-ADMIN.
REMARK******************************************************************
REMARK*4000-ACTUALIZAR-BD-ADMIN.                                       *
REMARK*    SE ACTUALIZA LA BD ADMINISTRATIVA, SE CREA UN EVENTO EN LA  *
REMARK*    LA TABLA EVENTO-ESTADO Y SE ACTUALIZA LA TABLA MODIFICACION *
REMARK*    CON EL CODIGO SOLUCION BLOQUEO TEMPORAL                     *
REMARK******************************************************************
REMARK**   SE INICIALIZAN VARIABLES Y ESTRUCTURAS PARA MANEJO DE SQLS
BDDDB2     PERFORM 90200-INICIALIZAR-INS-LIBSQL                         VHF0017 
           MOVE CODEST-TEMPORAL  TO COD-ESTADO-AUX
           MOVE "026"  TO COD-EVENTO-AUX
           PERFORM 7000-INSERTAR-EVENTO
           PERFORM 7010-ACTUALIZAR-MODIFICACION
           PERFORM 8300-ACTUALIZAR-BD-RELACIONAL
           IF IND-ERR-BD = SI
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO BD ADMINISTRATIVA - "
                                                  DELIMITED BY SIZE,
                     LLAVE-TEMPORAL               DELIMITED BY SIZE
ERRSQ2               " FECHA: "                   DELIMITED BY SIZE     VHF0004 
ERRSQ2               FECHA-ERROR                  DELIMITED BY SIZE     VHF0004 
ERRSQ2               HORA-ERROR                   DELIMITED BY SIZE     VHF0004 
ERRSQ1               " SQLCOD("                   DELIMITED BY SIZE     VHF0004 
BDDDB2               CODRET-ED-LIBSQL             DELIMITED BY " "      VHF0017 
ERRSQ1               ")"                          DELIMITED BY SIZE     VHF0004 
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-ERR-CHECK 
                    PERFORM 8600-REVERSAR-CTACTE
                 WHEN OTHER
                    ADD 1 TO CON-ERR-CREDIT
                    PERFORM 8610-REVERSAR-CARTERA
              END-EVALUATE
           ELSE
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-BLQ-CHECK 
                 WHEN OTHER
                    ADD 1 TO CON-BLQ-CREDIT
              END-EVALUATE
           END-IF.

       4100-ACTUALIZAR-BD-ADMIN2.
REMARK******************************************************************
REMARK*4100-ACTUALIZAR-BD-ADMIN2.                                      *
REMARK*    SE ACTUALIZA LA BD ADMINISTRATIVA, SE CREAN LOS EVENTOS EN  *
REMARK*    LA TABLA EVENTO-ESTADO Y SE ACTUALIZA LA TABLA MODIFICACION *
REMARK*    CON EL CODIGO SOLUCION RATIFICADO A FAVOR DEL SUSCRIPTOR Y  *
REMARK*    SE CIERRA EL RECLAMO                                        *
REMARK******************************************************************
REMARK**   SE INICIALIZAN VARIABLES Y ESTRUCTURAS PARA MANEJO DE SQLS
BDDDB2     PERFORM 90200-INICIALIZAR-INS-LIBSQL                         VHF0017 
           IF CODEST-TEMPORAL = 1 OR CODEST-TEMPORAL = 4
              MOVE "2" TO COD-ESTADO-AUX
              MOVE "025"  TO COD-EVENTO-AUX
              PERFORM 7000-INSERTAR-EVENTO
              MOVE "3" TO COD-ESTADO-AUX
              MOVE "056"  TO COD-EVENTO-AUX
              PERFORM 7000-INSERTAR-EVENTO
           END-IF
           IF CODEST-TEMPORAL = 2
              MOVE "3" TO COD-ESTADO-AUX
              MOVE "056"  TO COD-EVENTO-AUX
              PERFORM 7000-INSERTAR-EVENTO
           END-IF
           MOVE "01" TO COD-SOLUCION-AUX
           MOVE "035"   TO COD-JUS-AUX
           PERFORM 7020-ACTUALIZAR-MODIFICACION2
REGRAT     EVALUATE CODCTA-TEMPORAL                                     VHF0011 
REGRAT        WHEN 01                                                   VHF0011 
REGRAT           PERFORM 7100-ACTUALIZAR-MOD-CTACTE                     VHF0011 
REGRAT        WHEN OTHER                                                VHF0011 
REGRAT           PERFORM 7200-ACTUALIZAR-MOD-CARTERA                    VHF0011 
REGRAT     END-EVALUATE                                                 VHF0011 
           PERFORM 8300-ACTUALIZAR-BD-RELACIONAL
           IF IND-ERR-BD = SI
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO BD ADMINISTRATIVA - "
                                                  DELIMITED BY SIZE,
                     LLAVE-TEMPORAL               DELIMITED BY SIZE
ERRSQ2               " FECHA: "                   DELIMITED BY SIZE     VHF0004 
ERRSQ2               FECHA-ERROR                  DELIMITED BY SIZE     VHF0004 
ERRSQ2               HORA-ERROR                   DELIMITED BY SIZE     VHF0004 
ERRSQ1               " SQLCOD("                   DELIMITED BY SIZE     VHF0004 
BDDDB2               CODRET-ED-LIBSQL             DELIMITED BY " "      VHF0017 
ERRSQ1               ")"                          DELIMITED BY SIZE     VHF0004 
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-ERR-CHECK 
                    PERFORM 8600-REVERSAR-CTACTE
                 WHEN OTHER
                    ADD 1 TO CON-ERR-CREDIT
                    PERFORM 8610-REVERSAR-CARTERA
              END-EVALUATE
              IF IND-DTCCOM = SI
COMENT           EVALUATE CODCTA-TEMPORAL                               VHF0007 
COMENT              WHEN 01                                             VHF0007 
COMENT                 MOVE LLAVE-MAES-CHECK  TO LLAVE-MAESTRO-DTCCOM   VHF0007 
COMENT              WHEN OTHER                                          VHF0007 
COMENT                 MOVE LLAVE-MAES-CREDIT TO LLAVE-MAESTRO-DTCCOM   VHF0007 
COMENT           END-EVALUATE                                           VHF0007 
                 MOVE 3  TO TIPO-COMENTARIO-DTCCOM
                 READ DTCCOM
                    INVALID KEY
                       MOVE NO-ES TO IND-DTCCOM
                 END-READ
                 IF IND-DTCCOM = SI
                    MOVE REG-DTCCOM-AUX TO REG-DTCCOM
                    REWRITE REG-DTCCOM
                       INVALID KEY
                          MOVE NO-ES TO IND-DTCCOM 
                    END-REWRITE
                 END-IF
              END-IF 
           ELSE
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-RAT-CHECK 
                 WHEN OTHER
                    ADD 1 TO CON-RAT-CREDIT
              END-EVALUATE
           END-IF.

       4200-FINALIZAR-RECLAMO-AUT.
REMARK******************************************************************
REMARK*4200-FINALIZAR-RECLAMO-AUT.                                     *
REMARK*    SE ACTUALIZA LA BD ADMINISTRATIVA, SE CREAN LOS EVENTOS EN  *
REMARK*    LA TABLA EVENTO-ESTADO Y SE ACTUALIZA LA TABLA MODIFICACION *
REMARK*    PARA CIERRE DEL RECLAMO                                     *
REMARK******************************************************************
REMARK**   SE INICIALIZAN VARIABLES Y ESTRUCTURAS PARA MANEJO DE SQLS
BDDDB2     PERFORM 90200-INICIALIZAR-INS-LIBSQL                         VHF0017 
           IF CODEST-TEMPORAL = 1 OR CODEST-TEMPORAL = 4
              MOVE "2" TO COD-ESTADO-AUX
              MOVE "025"  TO COD-EVENTO-AUX
              PERFORM 7000-INSERTAR-EVENTO
              MOVE "3" TO COD-ESTADO-AUX
              MOVE "056"  TO COD-EVENTO-AUX
              PERFORM 7000-INSERTAR-EVENTO
           END-IF
           IF CODEST-TEMPORAL = 2
              MOVE "3" TO COD-ESTADO-AUX
              MOVE "056"  TO COD-EVENTO-AUX
              PERFORM 7000-INSERTAR-EVENTO
           END-IF
           MOVE "06" TO COD-SOLUCION-AUX
           MOVE "029"   TO COD-JUS-AUX
           PERFORM 7020-ACTUALIZAR-MODIFICACION2
           PERFORM 8300-ACTUALIZAR-BD-RELACIONAL
           IF IND-ERR-BD = SI
              INITIALIZE REG-SALIDA
              STRING " ERROR ACTUALIZANDO BD ADMINISTRATIVA - "
                                                  DELIMITED BY SIZE,
                     LLAVE-TEMPORAL               DELIMITED BY SIZE
ERRSQ2               " FECHA: "                   DELIMITED BY SIZE     VHF0004 
ERRSQ2               FECHA-ERROR                  DELIMITED BY SIZE     VHF0004 
ERRSQ2               HORA-ERROR                   DELIMITED BY SIZE     VHF0004 
ERRSQ1               " SQLCOD("                   DELIMITED BY SIZE     VHF0004 
BDDDB2               CODRET-ED-LIBSQL             DELIMITED BY " "      VHF0017 
ERRSQ1               ")"                          DELIMITED BY SIZE     VHF0004 
                     INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-ERR-CHECK 
                    PERFORM 8600-REVERSAR-CTACTE
                 WHEN OTHER
                    ADD 1 TO CON-ERR-CREDIT
                    PERFORM 8610-REVERSAR-CARTERA
              END-EVALUATE
              IF IND-DTCCOM = SI
COMENT           EVALUATE CODCTA-TEMPORAL                               VHF0007 
COMENT              WHEN 01                                             VHF0007 
COMENT                 MOVE LLAVE-MAES-CHECK  TO LLAVE-MAESTRO-DTCCOM   VHF0007 
COMENT              WHEN OTHER                                          VHF0007 
COMENT                 MOVE LLAVE-MAES-CREDIT TO LLAVE-MAESTRO-DTCCOM   VHF0007 
COMENT           END-EVALUATE                                           VHF0007 
                 MOVE 3  TO TIPO-COMENTARIO-DTCCOM
                 READ DTCCOM
                    INVALID KEY
                       MOVE NO-ES TO IND-DTCCOM
                 END-READ
                 IF IND-DTCCOM = SI
                    MOVE REG-DTCCOM-AUX TO REG-DTCCOM
                    REWRITE REG-DTCCOM
                       INVALID KEY
                          MOVE NO-ES TO IND-DTCCOM 
                    END-REWRITE
                 END-IF
              END-IF 
           ELSE
              EVALUATE CODCTA-TEMPORAL
                 WHEN 01
                    ADD 1 TO CON-AUT-CHECK 
                 WHEN OTHER
                    ADD 1 TO CON-AUT-CREDIT
              END-EVALUATE
           END-IF.

       4500-CERRAR-COMENTARIO.
REMARK*****************************************************************
REMARK*4500-CERRAR-COMENTARIO.                                        *
REMARK*    SE CIERRA COMENTARIO ASOCIADO A RECLAMO DE CUENTA SI SE    *
REMARK*    PROCESA UNA CUENTA CON RECLAMO EN TRAMITE                  *
REMARK*****************************************************************
           MOVE SI TO IND-DTCCOM
COMENT     EVALUATE CODCTA-TEMPORAL                                     VHF0007 
COMENT        WHEN 01                                                   VHF0007 
COMENT           MOVE LLAVE-MAES-CHECK  TO LLAVE-MAESTRO-DTCCOM         VHF0007 
COMENT        WHEN OTHER                                                VHF0007 
COMENT           MOVE LLAVE-MAES-CREDIT TO LLAVE-MAESTRO-DTCCOM         VHF0007 
COMENT     END-EVALUATE                                                 VHF0007 
           MOVE 3  TO TIPO-COMENTARIO-DTCCOM
           READ DTCCOM
              INVALID KEY
                 MOVE NO-ES TO IND-DTCCOM
                 INITIALIZE REG-SALIDA
                 STRING " ADVERTENCIA RECLAMO SIN COMENTARIO - "
                                                     DELIMITED BY SIZE,
                        LLAVE-TEMPORAL               DELIMITED BY SIZE
                        INTO REG-SALIDA
                 END-STRING
                 WRITE REG-SALIDA
           END-READ
           IF IND-DTCCOM = SI
              MOVE REG-DTCCOM TO REG-DTCCOM-AUX
              MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-CIERRE-DTCCOM
              IF IND-RATIFICACION = SI
                 MOVE FECHA-CIERRE-DTCCOM TO FEC-8-AUX
                 ADD  1 TO MES-8-AUX
                 IF MES-8-AUX > 12
                    ADD  1 TO ANO-8-AUX
                    MOVE 1 TO MES-8-AUX
                 END-IF
                 IF DIA-8-AUX > DIAS-MES (MES-8-AUX)
                    COMPUTE DIA-8-AUX = DIA-8-AUX - DIAS-MES (MES-8-AUX)
                    ADD  1 TO MES-8-AUX
                    IF MES-8-AUX > 12
                       ADD  1 TO ANO-8-AUX
                       MOVE 1 TO MES-8-AUX
                    END-IF
                 END-IF
                 MOVE FEC-8-AUX TO FECHA-CADUCIDAD-DTCCOM
                 MOVE 9         TO TIPO-RECLAMO-DTCCOM
              END-IF
              REWRITE REG-DTCCOM
                 INVALID KEY
                    MOVE NO-ES TO IND-DTCCOM 
                    INITIALIZE REG-SALIDA
                    STRING " ADVERTENCIA NO SE PUDO ACTUALIZAR "
                                                     DELIMITED BY SIZE,
                           " COMENTARIO DE RATIFICACION DE RECLAMO - "
                                                     DELIMITED BY SIZE,
                           LLAVE-TEMPORAL            DELIMITED BY SIZE
                           INTO REG-SALIDA
                    END-STRING
                    WRITE REG-SALIDA
              END-REWRITE
           END-IF.

       4970-GRABAR-LOGS.
REMARK******************************************************************
REMARK*4970-GRABAR-LOGS.                                               *
REMARK*    SE GRABA EL REGISTRO DE LOG DE LA MODIFICACION EFECTUADA    *
REMARK******************************************************************
           MOVE 99999999 TO LLAVE-DTCLOG
           START DTCLOG KEY <= LLAVE-DTCLOG
              INVALID KEY
                 MOVE 0 TO LLAVE-DTCLOG
              NOT INVALID KEY
                 READ DTCLOG PREVIOUS RECORD
                    AT END
                       MOVE 0 TO LLAVE-DTCLOG
                    NOT AT END
                       ADD  1 TO LLAVE-DTCLOG
                 END-READ
           END-START
           MOVE "DTCBLQ" TO PROGRAMA-DTCLOG
           MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-DTCLOG
           MOVE FUNCTION CURRENT-DATE (9:6) TO HORA-DTCLOG
           MOVE "71"                        TO IND-ORIGEN-DTCLOG
           MOVE COD-ESTADO-AUX              TO ESTADO-TRANS-DTCLOG
MIGNIT*    MOVE 860024151                   TO NIT-ENTIDAD-DTCLOG       VHF0024
MIGNIT     MOVE 900422614                   TO NIT-ENTIDAD-DTCLOG       VHF0024
           MOVE 99999999999                 TO USUARIO-CED-DTCLOG
           MOVE NUMCON-TEMPORAL             TO CON-MODIFICACION-DTCLOG
           MOVE TIP-CTA-RECLAMO-WS          TO TIPO-ARCHIVO-DTCLOG
           MOVE REG-PLANO-INI (1:38)        TO LLAVE-REGISTRO-DTCLOG
           MOVE REG-PLANO-INI               TO REG-ANTERIOR-DTCLOG
           MOVE REG-PLANO-FIN               TO REG-NUEVO-DTCLOG
           WRITE REG-DTCLOG.

       5000-DETERMINAR-DIA-HABIL.
REMARK******************************************************************
REMARK*5000-DETERMINAR-DIA-HABIL.                                      *
REMARK*    RUTINA PARA VERIFICAR SI LA FECHA ES HABIL                  *
REMARK******************************************************************
           MOVE NO-ES TO IND-HABIL
           MOVE FECHA-AUX TO FECHA-DTCNHAB
           READ DTCNHAB
              INVALID KEY
                 MOVE SI  TO IND-HABIL
           END-READ.

       5100-CALCULAR-NUEVA-FECHA.
REMARK******************************************************************
REMARK*5100-CALCULAR-NUEVA-FECHA.                                      *
REMARK*    SE CALCULA DIA ANTERIOR VALIDO                              *
REMARK******************************************************************
           SUBTRACT 1 FROM DIA-AUX
           IF DIA-AUX = 0
              SUBTRACT 1 FROM MES-AUX
              IF MES-AUX = 0
                 SUBTRACT 1 FROM ANO-AUX
                 MOVE 12 TO MES-AUX
              END-IF
              MOVE DIAS-MES (MES-AUX) TO DIA-AUX
              IF MES-AUX = 2
                 PERFORM 5200-DETERMINAR-BISIESTO
                 IF IND-BISIESTO = SI
                    MOVE 29 TO DIA-AUX
                 END-IF
              END-IF
           END-IF.

       5200-DETERMINAR-BISIESTO.
REMARK******************************************************************
REMARK*5200-DETERMINAR-BISIESTO                                        *
REMARK*    RUTINA PARA VERIFICAR ANO BISIESTO                          *
REMARK******************************************************************
           MOVE NO-ES TO IND-BISIESTO
           IF (FUNCTION REM (ANO-AUX 4)     = 0   AND
               (FUNCTION REM (ANO-AUX 100) <> 0   OR
                FUNCTION REM (ANO-AUX 400)  = 0)) AND
              (ANO-AUX <> 3600)
              MOVE SI TO IND-BISIESTO
           END-IF.

DIANHB 5300-VERIFICAR-TABLA-NOHABIL.                                    VHF0005 
REMARK******************************************************************
REMARK*5300-VERIFICAR-TABLA-NOHABIL.                                   *
REMARK*    SE VERIFICA QUE EL ARCHIVO DE DIAS NO HABILES ESTE          *
REMARK*    ACTUALIZADO PARA ELLO SE HACE START AL ARCHIVO DTCNHAB PARA *
REMARK*    EL ANO ACTUAL SI NO ESTA ACTUALIZADA SE ACTUALIZA DE NOVEDAT*
REMARK******************************************************************
DIANHB     MOVE SI TO IND-TABLA-NOHAB                                   VHF0005 
DIANH4     MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-AUX-R              VHF0014 
DIANH4     IF FECHA-AUX > FEC-MAX-AUX                                   VHF0014 
DIANH4        MOVE NO-ES TO IND-TABLA-NOHAB                             VHF0014 
DIANH4     END-IF.                                                      VHF0014 

DIANHB 5400-ACTUALIZAR-NOHABIL.                                         VHF0005 
REMARK******************************************************************
REMARK*5400-ACTUALIZAR-NOHABIL.                                        *
REMARK*    SE HACE LA CONSULTA A LA TABLA NO_HABILES DE NOVEDAT DE UN  *VHF0014
REMARK*    ANO ATRAS HASTA LA MAYOR FECHA DE LA TABLA Y SE ACTUALIZA   *VHF0014
REMARK*    CON EL RESULTADO EL ARCHIVO DTCNHAB                         *VHF0014
REMARK******************************************************************
DIANH4     MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-AUX-R              VHF0014 
DIANH4     SUBTRACT 1 FROM ANO-AUX                                      VHF0014 
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL                                  VHF0017 
DIANHB     STRING                                                       VHF0005 
DIANHB        "SELECT CAST(FECHA AS CHAR(10)) "       DELIMITED BY SIZE,VHF0005 
BDDDB2        "FROM APOYO.NO_HABILES "                DELIMITED BY SIZE,VHF0017 
DIANHB        "WHERE FECHA >= '"                      DELIMITED BY SIZE,VHF0005 
DIANHB        ANO-AUX                                 DELIMITED BY SIZE,VHF0005 
BDDDB2        "-"                                     DELIMITED BY SIZE,VHF0017 
DIANHB        MES-AUX                                 DELIMITED BY SIZE,VHF0005 
BDDDB2        "-"                                     DELIMITED BY SIZE,VHF0017 
DIANHB        DIA-AUX                                 DELIMITED BY SIZE,VHF0005 
DIANH5        "' ORDER BY FECHA"                      DELIMITED BY SIZE,VHF0015 
BDDDB2        INTO INSSQL-BD-LIBSQL                                     VHF0017 
BDDDB2     MOVE TITLE-TEMPORAL02 TO TITLE-LIBSQL                        VHF0017 
BDDDB2     PERFORM 70200-CONSUL01-SQL-LIBSQL                            VHF0017 
BDDDB2     IF CODRET-BD-LIBSQL <> 0 AND CODRET-BD-LIBSQL <> 100         VHF0017 
DIANHB        MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-ERROR           VHF0005 
DIANHB        MOVE FUNCTION CURRENT-DATE (9:6) TO HORA-ERROR            VHF0005 
DIANHB        INITIALIZE REG-SALIDA                                     VHF0005 
DIANHB        STRING                                                    VHF0005 
DIANHB           " ERROR: EN ACTUALIZACION DE "    DELIMITED BY SIZE    VHF0005 
DIANHB           " DIAS NO HABILES "               DELIMITED BY SIZE    VHF0005 
DIANHB           ANO-AUX                           DELIMITED BY SIZE    VHF0005 
DIANHB           ". PROGRAMA TERMINA  "            DELIMITED BY SIZE    VHF0005 
DIANHB           "A N O R M A L M E N T E "        DELIMITED BY SIZE    VHF0005 
DIANHB           " SQLCOD("                        DELIMITED BY SIZE    VHF0005 
BDDDB2           CODRET-ED-LIBSQL                  DELIMITED BY " "     VHF0017 
DIANHB           ")"                               DELIMITED BY SIZE    VHF0005 
DIANHB           INTO REG-SALIDA                                        VHF0005 
DIANHB        END-STRING                                                VHF0005 
DIANHB        DISPLAY REG-SALIDA                                        VHF0005 
DIANHB        WRITE REG-SALIDA                                          VHF0005 
BDDDB2        PERFORM 9000-CERRAR-ARCHIVOS                              VHF0017 
DIANHB        STOP RUN                                                  VHF0005 
DIANHB     END-IF                                                       VHF0005 
BDDDB2     OPEN INPUT LIBSQL                                            VHF0017 
DIANHB     MOVE NO-ES TO FDA-TEMPORAL                                   VHF0005 
BDDDB2     READ LIBSQL                                                  VHF0017 
DIANHB        AT END                                                    VHF0005 
DIANHB           MOVE " ERROR: TABLA DE DIAS NO HABILES DESACTUALIZADA. VHF0005 
DIANHB-               "EL PROGRAMA TERMINA   A N O R M A L M E N T E"   VHF0005 
DIANHB                TO REG-SALIDA                                     VHF0005 
DIANHB           DISPLAY REG-SALIDA                                     VHF0005 
DIANHB           WRITE REG-SALIDA                                       VHF0005 
BDDDB2           PERFORM 9000-CERRAR-ARCHIVOS                           VHF0017 
DIANHB           STOP RUN                                               VHF0005 
DIANHB     END-READ                                                     VHF0005 
DIANHB     CLOSE DTCNHAB                                                VHF0005 
DIANH4     INITIALIZE FEC-MAX-AUX                                       VHF0014 
DIANHB     OPEN  I-O DTCNHAB                                            VHF0005 
DIANHB     PERFORM UNTIL FDA-TEMPORAL = SI                              VHF0005 
BDDDB2        MOVE REG-LIBSQL TO RESSEL-01-LIBSQL                       VHF0017 
BDDDB2        MOVE RESSEL-01-LIBSQL TO REG-TEMPORAL02                   VHF0017 
DIANHB        UNSTRING FECHA-TEMPORAL02 DELIMITED BY ALL "-"            VHF0005 
BDDDB2                 INTO MES-AUX DIA-AUX ANO-AUX                     VHF0017 
DIANHB        MOVE FECHA-AUX TO FECHA-DTCNHAB                           VHF0005 
DIANH4                          FEC-MAX-AUX                             VHF0014 
DIANHB        WRITE REG-DTCNHAB                                         VHF0005 
DIANHB           INVALID KEY                                            VHF0005 
DIANHB              REWRITE REG-DTCNHAB                                 VHF0005 
DIANHB        END-WRITE                                                 VHF0005 
BDDDB2        READ LIBSQL                                               VHF0017 
DIANHB           AT END                                                 VHF0005 
DIANHB              MOVE SI TO FDA-TEMPORAL                             VHF0005 
DIANHB        END-READ                                                  VHF0005 
DIANHB     END-PERFORM                                                  VHF0005 
DIANHB     CLOSE DTCNHAB                                                VHF0005 
DIANHB     OPEN  INPUT DTCNHAB                                          VHF0005 
BDDDB2     CLOSE LIBSQL.                                                VHF0017 

       7000-INSERTAR-EVENTO.
REMARK******************************************************************
REMARK*7000-INSERTAR-EVENTO.                                           *
REMARK*    SE INSERTA EN LA TABLA EVENTOS DE LA BD RELACIONAL          *
REMARK******************************************************************
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL                                  VHF0017 
           STRING
BDDDB2        "INSERT INTO REGISTRO.EVENTO_ESTADO "                     VHF0017
BDDDB2                                        DELIMITED BY SIZE,        VHF0017
              "(NRO_CONSECUTIVO, COD_INGRESO, FEC_EVENTO_ESTADO, "
                                              DELIMITED BY SIZE,
              "COD_USUARIO, COD_ESTADO, COD_EVENTO, COD_JUSTIFICA) "
                                              DELIMITED BY SIZE,
              "VALUES ("                      DELIMITED BY SIZE,
              NUMCON-TEMPORAL                 DELIMITED BY SIZE,
              ",'"                            DELIMITED BY SIZE,
CIUREC        CODING-TEMPORAL                 DELIMITED BY SIZE,        VHF0008 
              "','"                           DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (1:4)     DELIMITED BY SIZE,
BDDDB2        "-"                             DELIMITED BY SIZE,        VHF0017 
              FUNCTION CURRENT-DATE (5:2)     DELIMITED BY SIZE,
BDDDB2        "-"                             DELIMITED BY SIZE,        VHF0017 
              FUNCTION CURRENT-DATE (7:2)     DELIMITED BY SIZE,
              " "                             DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (9:2)     DELIMITED BY SIZE,
              ":"                             DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (11:2)    DELIMITED BY SIZE,
              ":"                             DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (13:2)    DELIMITED BY SIZE,
              "',"                            DELIMITED BY SIZE,
              "99999999999"                   DELIMITED BY SIZE,
              ",'"                            DELIMITED BY SIZE,
              COD-ESTADO-AUX                  DELIMITED BY SIZE,
              "','"                           DELIMITED BY SIZE,
              COD-EVENTO-AUX                  DELIMITED BY SIZE,
              "',"                            DELIMITED BY SIZE,
              "NULL"                          DELIMITED BY SIZE,
              ")"                             DELIMITED BY SIZE,
              "||"                            DELIMITED BY SIZE
BDDDB2        INTO INSSQL-BD-LIBSQL                                     VHF0017 
           END-STRING
BDDDB2     PERFORM 90400-CONCATENAR-INSTR-LIBSQL.                       VHF0017 

       7010-ACTUALIZAR-MODIFICACION.
REMARK******************************************************************
REMARK*7010-ACTUALIZAR-MODIFICACION.                                   *
REMARK*    SE ACTUALIZA LA TABLA MAESTRO DE MODIFICACION POR BLOQUEO   *
REMARK*    TEMPORAL                                                    *
REMARK******************************************************************
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL                                  VHF0017 
           STRING
BDDDB2        "UPDATE REGISTRO.MODIFICACION SET BLO_AUTORREGULA = '1'"  VHF0017  
                                                    DELIMITED BY SIZE,
              " WHERE NRO_CONSECUTIVO = "           DELIMITED BY SIZE,
              NUMCON-TEMPORAL                       DELIMITED BY SIZE,
              " AND COD_INGRESO = '"                DELIMITED BY SIZE,
CIUREC        CODING-TEMPORAL                       DELIMITED BY SIZE,  VHF0008 
              "'"                                   DELIMITED BY SIZE,
              "||"                                  DELIMITED BY SIZE
BDDDB2        INTO INSSQL-BD-LIBSQL                                     VHF0017 
           END-STRING
BDDDB2     PERFORM 90400-CONCATENAR-INSTR-LIBSQL.                       VHF0017 

       7020-ACTUALIZAR-MODIFICACION2.
REMARK******************************************************************
REMARK*7020-ACTUALIZAR-MODIFICACION2.                                  *
REMARK*    SE ACTUALIZA LA TABLA MAESTRO DE MODIFICACION POR           *
REMARK*    RATIFICACION A FAVOR DEL SUSCRIPTOR                         *
REMARK******************************************************************
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL                                  VHF0017 
           STRING
BDDDB2        "UPDATE REGISTRO.MODIFICACION SET COD_ESTADO = '"         VHF0017
              "3"                                   DELIMITED BY SIZE,
              "', COD_SOLUCION = '"                 DELIMITED BY SIZE,
              COD-SOLUCION-AUX                      DELIMITED BY SIZE,
              "', COD_USU_MODIFICA = "              DELIMITED BY SIZE,
              "99999999999"                         DELIMITED BY SIZE,
              ", FEC_MODIFICA = '"                  DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (1:4)           DELIMITED BY SIZE,
BDDDB2        "-"                                   DELIMITED BY SIZE,  VHF0017 
              FUNCTION CURRENT-DATE (5:2)           DELIMITED BY SIZE,
BDDDB2        "-"                                   DELIMITED BY SIZE,  VHF0017 
              FUNCTION CURRENT-DATE (7:2)           DELIMITED BY SIZE,
              " "                                   DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (9:2)           DELIMITED BY SIZE,
              ":"                                   DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (11:2)          DELIMITED BY SIZE,
              ":"                                   DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (13:2)          DELIMITED BY SIZE,
              "', OBS_MODIFICA = "                  DELIMITED BY SIZE,
              "NULL"                                DELIMITED BY SIZE,
              ", COD_JUS_MODIFICA = '"              DELIMITED BY SIZE,
              COD-JUS-AUX                           DELIMITED BY SIZE,
              "', NRO_APRO_MODIFICA = "             DELIMITED BY SIZE,
              "NULL"                                DELIMITED BY SIZE,
              ", COD_USU_REVISA = "                 DELIMITED BY SIZE,
              "99999999999"                         DELIMITED BY SIZE,
              ", FEC_REVISA = '"                    DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (1:4)           DELIMITED BY SIZE,
BDDDB2        "-"                                   DELIMITED BY SIZE,  VHF0017 
              FUNCTION CURRENT-DATE (5:2)           DELIMITED BY SIZE,
BDDDB2        "-"                                   DELIMITED BY SIZE,  VHF0017 
              FUNCTION CURRENT-DATE (7:2)           DELIMITED BY SIZE,
              " "                                   DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (9:2)           DELIMITED BY SIZE,
              ":"                                   DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (11:2)          DELIMITED BY SIZE,
              ":"                                   DELIMITED BY SIZE,
              FUNCTION CURRENT-DATE (13:2)          DELIMITED BY SIZE,
              "', OBS_REVISA = "                    DELIMITED BY SIZE,
              "NULL"                                DELIMITED BY SIZE,
              ", COD_JUS_REVISA = "                 DELIMITED BY SIZE,
              "NULL"                                DELIMITED BY SIZE,
              ", NRO_APRO_REVISA = "                DELIMITED BY SIZE,
              "NULL"                                DELIMITED BY SIZE,
              " WHERE NRO_CONSECUTIVO = "           DELIMITED BY SIZE,
              NUMCON-TEMPORAL                       DELIMITED BY SIZE,
CIUREC        " AND COD_INGRESO = '"                DELIMITED BY SIZE,  VHF0008 
CIUREC        CODING-TEMPORAL                       DELIMITED BY SIZE,  VHF0008 
CIUREC        "'"                                   DELIMITED BY SIZE,  VHF0008 
              "||"                                  DELIMITED BY SIZE
BDDDB2        INTO INSSQL-BD-LIBSQL                                     VHF0017 
           END-STRING
BDDDB2     PERFORM 90400-CONCATENAR-INSTR-LIBSQL.                       VHF0017 

REGRAT 7100-ACTUALIZAR-MOD-CTACTE.                                      VHF0011 
REMARK******************************************************************
REMARK*7100-ACTUALIZAR-MOD-CTACTE.                                     *
REMARK*    SE ACTUALIZA LA TABLA DETALLE DE MODIFICACION DE CUENTAS    *
REMARK*    CORRIENTES                                                  *
REMARK******************************************************************
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL                                  VHF0017 
REGRAT     STRING                                                       VHF0011 
BDDDB2        "UPDATE REGISTRO.MODIFICA_CTACTE SET "                    VHF0017
BDDDB2                                        DELIMITED BY SIZE,        VHF0017 
REGRA2        "IND_BLO_A = '"                 DELIMITED BY SIZE,        VHF0013 
REGRA2        IND-BLOQUEO-CTACTE-A            DELIMITED BY SIZE,        VHF0013 
REGRA2        "', FEC_APE_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        FECHA-APERT-CTACTE-A            DELIMITED BY SIZE,        VHF0013 
REGRA2        "', NOVEDAD_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        NOV-REPORT-CTACTE-A             DELIMITED BY SIZE,        VHF0013 
REGRA2        "', FEC_NOV_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        FEC-NOVEDAD-CTACTE-A            DELIMITED BY SIZE,        VHF0013 
REGRA2        "', EMBARGO_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        NUM-EMBARGO-CTACTE-A            DELIMITED BY SIZE,        VHF0013 
REGRA2        "', ADJETIVO_A = '"             DELIMITED BY SIZE,        VHF0013 
REGRA2        ADJETIVO-CTACTE-A               DELIMITED BY SIZE,        VHF0013 
REGRA2        "', REGISTRO_A = '"             DELIMITED BY SIZE,        VHF0013 
REGRA2        REG-CTACTE-A                    DELIMITED BY SIZE,        VHF0013 
REGRA2        "', IND_BLO_D = '"              DELIMITED BY SIZE,        VHF0013 
REGRAT        IND-BLOQUEO-CTACTE-D            DELIMITED BY SIZE,        VHF0011 
REGRAT        "', FEC_APE_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        FECHA-APERT-CTACTE-D            DELIMITED BY SIZE,        VHF0011 
REGRAT        "', NOVEDAD_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        NOV-REPORT-CTACTE-D             DELIMITED BY SIZE,        VHF0011 
REGRAT        "', FEC_NOV_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        FEC-NOVEDAD-CTACTE-D            DELIMITED BY SIZE,        VHF0011 
REGRAT        "', EMBARGO_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        NUM-EMBARGO-CTACTE-D            DELIMITED BY SIZE,        VHF0011 
REGRAT        "', ADJETIVO_D = '"             DELIMITED BY SIZE,        VHF0011 
REGRAT        ADJETIVO-CTACTE-D               DELIMITED BY SIZE,        VHF0011 
REGRAT        "', REGISTRO_D = '"             DELIMITED BY SIZE,        VHF0011 
REGRAT        REG-CTACTE-D                    DELIMITED BY SIZE,        VHF0011 
REGRAT        "' WHERE NRO_CONSECUTIVO = "    DELIMITED BY SIZE,        VHF0011 
REGRAT        NUMCON-TEMPORAL                 DELIMITED BY SIZE,        VHF0011 
REGRAT        " AND COD_INGRESO = '"          DELIMITED BY SIZE,        VHF0011 
REGRAT        CODING-TEMPORAL                 DELIMITED BY SIZE,        VHF0011 
REGRAT        "'"                             DELIMITED BY SIZE,        VHF0011 
REGRAT        "||"                            DELIMITED BY SIZE         VHF0011 
BDDDB2        INTO INSSQL-BD-LIBSQL                                     VHF0017 
REGRAT     END-STRING                                                   VHF0011 
BDDDB2     PERFORM 90400-CONCATENAR-INSTR-LIBSQL.                       VHF0017 

REGRAT 7200-ACTUALIZAR-MOD-CARTERA.                                     VHF0011 
REMARK******************************************************************
REMARK*7200-ACTUALIZAR-MOD-CARTERA.                                    *
REMARK*    SE ACTUALIZA LA TABLA DETALLE DE MODIFICACION DE CARTERA    *
REMARK******************************************************************
BDDDB2     INITIALIZE INSSQL-BD-LIBSQL                                  VHF0017 
REGRAT     STRING                                                       VHF0011 
BDDDB2        "UPDATE REGISTRO.MODIFICA_CARTERA SET "                   VHF0017
BDDDB2                                        DELIMITED BY SIZE,        VHF0017 
REGRA2        "IND_BLO_A = '"                 DELIMITED BY SIZE,        VHF0013 
REGRA2        IND-BLOQUEO-CARTERA-A           DELIMITED BY SIZE,        VHF0013 
REGRA2        "', FEC_APE_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        FECHA-APERT-CARTERA-A           DELIMITED BY SIZE,        VHF0013 
REGRA2        "', FEC_VEN_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        FECHA-VENC-CARTERA-A            DELIMITED BY SIZE,        VHF0013 
REGRA2        "', VAL_CUO_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        CUANTIA-CARTERA-A               DELIMITED BY SIZE,        VHF0013 
REGRA2        "', NOVEDAD_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        NOV-REPORT-CARTERA-A            DELIMITED BY SIZE,        VHF0013 
REGRA2        "', FEC_NOV_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        FEC-NOVEDAD-CARTERA-A           DELIMITED BY SIZE,        VHF0013 
REGRA2        "', VEC_1_A = '"                DELIMITED BY SIZE,        VHF0013 
REGRA2        ESTADO-MES-CARTERA-A            DELIMITED BY SIZE,        VHF0013 
REGRA2        "', VEC_2_A = '"                DELIMITED BY SIZE,        VHF0013 
REGRA2        VECTOR-2-CARTERA-A              DELIMITED BY SIZE,        VHF0013 
REGRA2        "', MOR_MAX_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        MORA-MAXIMA-CARTERA-A           DELIMITED BY SIZE,        VHF0013 
REGRA2        "', ADJETIVO1_A = '"            DELIMITED BY SIZE,        VHF0013 
REGRA2        ADJETIVO1-CARTERA-A             DELIMITED BY SIZE,        VHF0013 
REGRA2        "', ADJETIVO2_A = '"            DELIMITED BY SIZE,        VHF0013 
REGRA2        ADJETIVO2-CARTERA-A             DELIMITED BY SIZE,        VHF0013 
REGRA2        "', ADJETIVO3_A = '"            DELIMITED BY SIZE,        VHF0013 
REGRA2        ADJETIVO3-CARTERA-A             DELIMITED BY SIZE,        VHF0013 
REGRA2        "', VAL_INI_A = '"              DELIMITED BY SIZE,        VHF0013 
REGRA2        VALOR-INICIAL-CARTERA-A         DELIMITED BY SIZE,        VHF0013 
REGRA2        "', VAL_SAL_DEUDA_A = '"        DELIMITED BY SIZE,        VHF0013 
REGRA2        VALOR-SALDO-DEUDAM-CARTERA-A    DELIMITED BY SIZE,        VHF0013 
REGRA2        "', VAL_SAL_MORA_A = '"         DELIMITED BY SIZE,        VHF0013 
REGRA2        VALOR-SALDO-MORAM-CARTERA-A     DELIMITED BY SIZE,        VHF0013 
REGRA2        "', FEC_SITUACION_A = '"        DELIMITED BY SIZE,        VHF0013 
REGRA2        FECHA-SITUACION-CARTERA-A       DELIMITED BY SIZE,        VHF0013 
REGRA2        "', FOR_PAGO_A = '"             DELIMITED BY SIZE,        VHF0013 
REGRA2        FORMA-PAGO-TOTAL-CARTERA-A      DELIMITED BY SIZE,        VHF0013 
REGRA2        "', REGISTRO_A = '"             DELIMITED BY SIZE,        VHF0013 
REGRA2        REG-CARTERA-A                   DELIMITED BY SIZE,        VHF0013 
REGRA2        "', IND_BLO_D = '"              DELIMITED BY SIZE,        VHF0013 
REGRAT        IND-BLOQUEO-CARTERA-D           DELIMITED BY SIZE,        VHF0011 
REGRAT        "', FEC_APE_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        FECHA-APERT-CARTERA-D           DELIMITED BY SIZE,        VHF0011 
REGRAT        "', FEC_VEN_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        FECHA-VENC-CARTERA-D            DELIMITED BY SIZE,        VHF0011 
REGRAT        "', VAL_CUO_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        CUANTIA-CARTERA-D               DELIMITED BY SIZE,        VHF0011 
REGRAT        "', NOVEDAD_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        NOV-REPORT-CARTERA-D            DELIMITED BY SIZE,        VHF0011 
REGRAT        "', FEC_NOV_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        FEC-NOVEDAD-CARTERA-D           DELIMITED BY SIZE,        VHF0011 
REGRAT        "', VEC_1_D = '"                DELIMITED BY SIZE,        VHF0011 
REGRAT        ESTADO-MES-CARTERA-D            DELIMITED BY SIZE,        VHF0011 
REGRAT        "', VEC_2_D = '"                DELIMITED BY SIZE,        VHF0011 
REGRAT        VECTOR-2-CARTERA-D              DELIMITED BY SIZE,        VHF0011 
REGRAT        "', MOR_MAX_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        MORA-MAXIMA-CARTERA-D           DELIMITED BY SIZE,        VHF0011 
REGRAT        "', ADJETIVO1_D = '"            DELIMITED BY SIZE,        VHF0011 
REGRAT        ADJETIVO1-CARTERA-D             DELIMITED BY SIZE,        VHF0011 
REGRAT        "', ADJETIVO2_D = '"            DELIMITED BY SIZE,        VHF0011 
REGRAT        ADJETIVO2-CARTERA-D             DELIMITED BY SIZE,        VHF0011 
REGRAT        "', ADJETIVO3_D = '"            DELIMITED BY SIZE,        VHF0011 
REGRAT        ADJETIVO3-CARTERA-D             DELIMITED BY SIZE,        VHF0011 
REGRAT        "', VAL_INI_D = '"              DELIMITED BY SIZE,        VHF0011 
REGRAT        VALOR-INICIAL-CARTERA-D         DELIMITED BY SIZE,        VHF0011 
REGRAT        "', VAL_SAL_DEUDA_D = '"        DELIMITED BY SIZE,        VHF0011 
REGRAT        VALOR-SALDO-DEUDAM-CARTERA-D    DELIMITED BY SIZE,        VHF0011 
REGRAT        "', VAL_SAL_MORA_D = '"         DELIMITED BY SIZE,        VHF0011 
REGRAT        VALOR-SALDO-MORAM-CARTERA-D     DELIMITED BY SIZE,        VHF0011 
REGRAT        "', FEC_SITUACION_D = '"        DELIMITED BY SIZE,        VHF0011 
REGRAT        FECHA-SITUACION-CARTERA-D       DELIMITED BY SIZE,        VHF0011 
REGRAT        "', FOR_PAGO_D = '"             DELIMITED BY SIZE,        VHF0011 
REGRAT        FORMA-PAGO-TOTAL-CARTERA-D      DELIMITED BY SIZE,        VHF0011 
REGRAT        "', REGISTRO_D = '"             DELIMITED BY SIZE,        VHF0011 
REGRAT        REG-CARTERA-D                   DELIMITED BY SIZE,        VHF0011 
LEY002        "'||"                           DELIMITED BY SIZE         VHF0019
LEY002        INTO INSSQL-BD-LIBSQL                                     VHF0019
LEY002     END-STRING                                                   VHF0019
LEY002     IF SW-LEY = "SI"                                             VHF0019
LEY002        STRING                                                    VHF0019
LEY002           INSSQL-BD-LIBSQL             DELIMITED BY "||"         VHF0019
LEY002           ", NOVEDAD_R_A = '"          DELIMITED BY SIZE         VHF0019
LEY002           NOV-REPORT-R-CARTERA-A       DELIMITED BY SIZE         VHF0019
LEY002           "', NOVEDAD_R_D = '"         DELIMITED BY SIZE         VHF0019
LEY002           NOV-REPORT-R-CARTERA-D       DELIMITED BY SIZE         VHF0019
LEY002           "', FEC_NOV_R_A = '"         DELIMITED BY SIZE         VHF0019
LEY002           FEC-NOVEDAD-R-CARTERA-A      DELIMITED BY SIZE         VHF0019
LEY002           "', FEC_NOV_R_D = '"         DELIMITED BY SIZE         VHF0019
LEY002           FEC-NOVEDAD-R-CARTERA-D      DELIMITED BY SIZE         VHF0019
LEY002           "', VEC_3_A = '"             DELIMITED BY SIZE         VHF0019
LEY002           ESTADO-MES-R-CARTERA-A       DELIMITED BY SIZE         VHF0019
LEY002           "', VEC_3_D = '"             DELIMITED BY SIZE         VHF0019
LEY002           ESTADO-MES-R-CARTERA-D       DELIMITED BY SIZE         VHF0019
LEY002           "', VEC_4_A = '"             DELIMITED BY SIZE         VHF0019
LEY002           VECTOR-2-R-CARTERA-A         DELIMITED BY SIZE         VHF0019
LEY002           "', VEC_4_D = '"             DELIMITED BY SIZE         VHF0019
LEY002           VECTOR-2-R-CARTERA-D         DELIMITED BY SIZE         VHF0019
LEY002           "'||"                        DELIMITED BY SIZE         VHF0019
LEY002           INTO INSSQL-BD-LIBSQL                                  VHF0019
LEY002        END-STRING                                                VHF0019
LEY002     END-IF                                                       VHF0019
LEY002     STRING                                                       VHF0019
LEY002        INSSQL-BD-LIBSQL                DELIMITED BY "||"         VHF0019
LEY002        " WHERE NRO_CONSECUTIVO = "     DELIMITED BY SIZE,        VHF0019
REGRAT        NUMCON-TEMPORAL                 DELIMITED BY SIZE,        VHF0011 
REGRAT        " AND COD_INGRESO = '"          DELIMITED BY SIZE,        VHF0011 
REGRAT        CODING-TEMPORAL                 DELIMITED BY SIZE,        VHF0011 
REGRAT        "'"                             DELIMITED BY SIZE,        VHF0011 
REGRAT        "||"                            DELIMITED BY SIZE         VHF0011 
BDDDB2        INTO INSSQL-BD-LIBSQL                                     VHF0017 
REGRAT     END-STRING                                                   VHF0011 
BDDDB2     PERFORM 90400-CONCATENAR-INSTR-LIBSQL.                       VHF0017 

       8000-PRESENTAR-RESUMEN.
REMARK******************************************************************
REMARK*8000-PRESENTAR-RESUMEN.                                         *
REMARK*    SE DESPLIEGA EL RESUMEN DEL PROCESO                         *
REMARK******************************************************************
           MOVE FUNCTION CURRENT-DATE (1:14) TO FECHA-FIN
           INITIALIZE REG-SALIDA
           WRITE REG-SALIDA
           WRITE REG-SALIDA
NUEPRO     MOVE TITULO-SALIDA TO REG-SALIDA                             ABB0022
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           WRITE REG-SALIDA
           STRING
              "INICIO : "                            DELIMITED BY SIZE,
              FECHA-INI                              DELIMITED BY SIZE,
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "FIN    : "                            DELIMITED BY SIZE,
              FECHA-FIN                              DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "DIAS PLAZO MAXIMO                 : " DELIMITED BY SIZE,
              DIAS-PLAZO                             DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "FECHA FILTRO                      : " DELIMITED BY SIZE,
              FECHA-AUX                              DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           WRITE REG-SALIDA
           STRING
              "REGISTROS PROCESADOS              : " DELIMITED BY SIZE,
              CON-LEIDOS                             DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "   CUENTAS CORRIENTES             : " DELIMITED BY SIZE,
              CON-LEI-CHECK                          DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      BLOQUEADOS                  : " DELIMITED BY SIZE,
              CON-BLQ-CHECK                          DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      RATIFICADOS                 : " DELIMITED BY SIZE,
              CON-RAT-CHECK                          DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      CERRADOS AUTOMATICAMENTE    : " DELIMITED BY SIZE,
              CON-AUT-CHECK                          DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      ERRORES                     : " DELIMITED BY SIZE,
              CON-ERR-CHECK                          DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           WRITE REG-SALIDA
           STRING
              "   CARTERA                        : " DELIMITED BY SIZE,
              CON-LEI-CREDIT                         DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      BLOQUEADOS                  : " DELIMITED BY SIZE,
              CON-BLQ-CREDIT                         DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      RATIFICADOS                 : " DELIMITED BY SIZE,
              CON-RAT-CREDIT                         DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      CERRADOS AUTOMATICAMENTE    : " DELIMITED BY SIZE,
              CON-AUT-CREDIT                         DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA
           STRING
              "      ERRORES                     : " DELIMITED BY SIZE,
              CON-ERR-CREDIT                         DELIMITED BY SIZE
              INTO REG-SALIDA
           END-STRING
           WRITE REG-SALIDA.

       8300-ACTUALIZAR-BD-RELACIONAL.
REMARK******************************************************************
REMARK*8300-ACTUALIZAR-BD-RELACIONAL.                                  *
REMARK*    SE EJECUTAN LOS SQLS SOLICITADOS A LA BASE DE DATOS, SI     *
REMARK*    ALGUNO FALLA, SE DEVUELVE ERROR Y SE GRABA EN UN ARCHIVO.   *
REMARK*    ESTA RUTINA ESTA HECHA PARA MARCACION DE RECLAMOS           *
REMARK******************************************************************
BDDDB2     PERFORM 70000-EJECUTAR-SQL-LIBSQL                            VHF0017 
BDDDB2     IF CODRET-BD-LIBSQL <> 0                                     VHF0017 
BDDDB2        MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-ERROR           VHF0017 
BDDDB2        MOVE FUNCTION CURRENT-DATE (9:6) TO HORA-ERROR            VHF0017 
BDDDB2        MOVE SI TO IND-ERR-BD                                     VHF0017 
BDDDB2     END-IF.                                                      VHF0017 

       8600-REVERSAR-CTACTE.
REMARK******************************************************************
REMARK*8600-REVERSAR-CTACTE.                                           *
REMARK*    SE REESTABLECE EL REGISTRO ORIGINAL DE CUENTAS CORRIENTES   *
REMARK******************************************************************
           MOVE LLAVE-TEMPORAL TO LLAVE-MAES-CHECK
           PERFORM 9200-LEER-MAES-CHECK
           IF ENC-MAESTRO = NO-ES
              INITIALIZE REG-SALIDA
              STRING
                 " ERROR NO SE PUDO LEER PARA REVERSAR CTACTE - "
                                                   DELIMITED BY SIZE,
                 LLAVE-TEMPORAL                    DELIMITED BY SIZE
                 INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
           ELSE
              MOVE COMPLETO-ABD06-AUX  TO COMPLETO-ABD06
              PERFORM 9300-ACTUALIZAR-MAES-CHECK
              IF ACT-MAESTRO = NO-ES
                 INITIALIZE REG-SALIDA
                 STRING
                    " ERROR NO SE PUDO REESCRIBIR" DELIMITED BY SIZE,
                    " PARA REVERSAR CTACTE - "     DELIMITED BY SIZE,
                    LLAVE-TEMPORAL                 DELIMITED BY SIZE
                    INTO REG-SALIDA
                 END-STRING
                 WRITE REG-SALIDA
              END-IF
           END-IF.

       8610-REVERSAR-CARTERA.
REMARK******************************************************************
REMARK*8610-REVERSAR-CARTERA.                                          *
REMARK*    SE REESTABLECE EL REGISTRO ORIGINAL DE CARTERA              *
REMARK******************************************************************
           MOVE LLAVE-TEMPORAL TO LLAVE-MAES-CREDIT
           PERFORM 9210-LEER-MAES-CREDIT
           IF ENC-MAESTRO = NO-ES
              INITIALIZE REG-SALIDA
              STRING
                 " ERROR NO SE PUDO LEER PARA REVERSAR CARTERA - "
                                               DELIMITED BY SIZE,
                 LLAVE-TEMPORAL                DELIMITED BY SIZE
                 INTO REG-SALIDA
              END-STRING
              WRITE REG-SALIDA
           ELSE
              MOVE COMPLETO-ABD07-AUX  TO COMPLETO-ABD07
              PERFORM 9310-ACTUALIZAR-MAES-CREDIT
              IF ACT-MAESTRO = NO-ES
                 INITIALIZE REG-SALIDA
                 STRING
                    " ERROR NO PUDO REESCRIBIR"   DELIMITED BY SIZE,
                    " PARA REVERSAR CARTERA "     DELIMITED BY SIZE,
                    LLAVE-TEMPORAL                DELIMITED BY SIZE
                    INTO REG-SALIDA
                 END-STRING
                 WRITE REG-SALIDA
              END-IF
           END-IF.

       9000-CERRAR-ARCHIVOS.
REMARK******************************************************************
REMARK*9000-CERRAR-ARCHIVOS.                                           *
REMARK******************************************************************
           CLOSE MAESTRO-CHECK
                 MAESTRO-CREDIT
                 SUSCRIPTOR
                 DTCNHAB
                 DTRLLA
                 DTCLOG
INDBDD           DTCBDD                                                 VHF0005 
MATC02           BDIIREGI                                               VHF0020
MATC02           BDIIDIAN                                               VHF0020
MATC02           BDIIDAS                                                VHF0020
MATC02           MAESTRO-DIRECC                                         VHF0020
                 SALIDA.
BDDDB2     PERFORM 80000-DISCONNECT-BD-LIBSQL.                          VHF0017 

       9200-LEER-MAES-CHECK.
REMARK******************************************************************
REMARK*9200-LEER-MAES-CHECK.                                           *
REMARK*    SE LEE EL ARCHIVO MAESTRO DE CUENTAS CORRIENTES, SI NO SE   *
REMARK*    ENCUENTRA EL REGISTRO SE BUSCA EN EL DTRLLA PARA VER SI FUE *
REMARK*    ELIMINADO O MIGRADO A UNA NUEVA CUENTA                      *
REMARK******************************************************************
DTRLLA     INITIALIZE VEC-PROANT-DTRLLA                                 VHF0003 
DTRLLA     MOVE 1  TO IND-DTRLLA                                        VHF0003 
DTRLLA     MOVE PENDIENTE TO ENC-MAESTRO                                VHF0003 
DTRLLA     PERFORM UNTIL ENC-MAESTRO = SI OR ENC-MAESTRO = NO-ES OR     VHF0003 
DTRLLA                   ENC-MAESTRO = ERROR-DTRLLA                     VHF0003 
LEY001     PERFORM  4000-LEE-ICMCHK-DINAMICO                            GDT0018 
LEY001     IF  SW-LECTURA-MAESTROS  =  0                                GDT0018 
LEY001                                                                  GDT0018 
LEY001               MOVE SI TO ENC-MAESTRO                             GDT0018 
LEY001                                                                  GDT0018 
LEY001     END-IF                                                       GDT0018 
LEY001     IF  SW-LECTURA-MAESTROS  =  1                                GDT0018 
LEY001                                                                  GDT0018 
LEY001                                                                  GDT0018 
LEY001               MOVE LLAVE-MAES-CHECK TO LLAVE-ANT-DTRLLA          GDT0018 
LEY001               PERFORM 9290-BUSCAR-DTRLLA                         GDT0018 
LEY001               IF ENC-MAESTRO = PENDIENTE                         GDT0018 
LEY001                  MOVE LLAVE-ACT-DTRLLA TO LLAVE-MAES-CHECK       GDT0018 
LEY001               END-IF                                             GDT0018 
LEY001     END-IF                                                       GDT0018 
           END-PERFORM.

       9210-LEER-MAES-CREDIT.
REMARK******************************************************************
REMARK*9210-LEER-MAES-CREDIT.                                          *
REMARK*    SE LEE EL ARCHIVO MAESTRO DE CARTERA, SI NO SE ENCUENTRA EL *
REMARK*    REGISTRO SE BUSCA EN EL DTRLLA PARA VER SI FUE ELIMINADO O  *
REMARK*    MIGRADO A UNA NUEVA CUENTA                                  *
REMARK******************************************************************
DTRLLA     INITIALIZE VEC-PROANT-DTRLLA                                 VHF0003 
DTRLLA     MOVE 1  TO IND-DTRLLA                                        VHF0003 
DTRLLA     MOVE PENDIENTE TO ENC-MAESTRO                                VHF0003 
DTRLLA     PERFORM UNTIL ENC-MAESTRO = SI OR ENC-MAESTRO = NO-ES OR     VHF0003 
DTRLLA                   ENC-MAESTRO = ERROR-DTRLLA                     VHF0003 
LEY001     PERFORM  2000-LEE-ICMCRE-DINAMICO                            GDT0018 
LEY001     IF  SW-LECTURA-MAESTROS  =  0                                GDT0018 
LEY001                                                                  GDT0018 
LEY001               MOVE SI TO ENC-MAESTRO                             GDT0018 
LEY001                                                                  GDT0018 
LEY001     END-IF                                                       GDT0018 
LEY001     IF  SW-LECTURA-MAESTROS  =  1                                GDT0018 
LEY001                                                                  GDT0018 
LEY001                                                                  GDT0018 
LEY001               MOVE LLAVE-MAES-CREDIT TO LLAVE-ANT-DTRLLA         GDT0018 
LEY001               PERFORM 9290-BUSCAR-DTRLLA                         GDT0018 
LEY001               IF ENC-MAESTRO = PENDIENTE                         GDT0018 
LEY001                  MOVE LLAVE-ACT-DTRLLA TO LLAVE-MAES-CREDIT      GDT0018 
LEY001               END-IF                                             GDT0018 
LEY001     END-IF                                                       GDT0018 
           END-PERFORM.

       9290-BUSCAR-DTRLLA.
REMARK******************************************************************
REMARK*9290-BUSCAR-DTRLLA.                                             *
REMARK*    SE LEE EN EL ARCHIVO PUENTE DTRLLA EL REGISTRO PARA         *
REMARK*    DETERMINAR SI FUE ELIMINADO O MIGRADO                       *
REMARK******************************************************************
DTRLLA     SEARCH ALL PROANT-DTRLLA                                     VHF0003 
DTRLLA        AT END                                                    VHF0003 
DTRLLA           MOVE 0 TO IND-PROANT-DTRLLA                            VHF0003 
DTRLLA        WHEN LLAVE-PROANT-DTRLLA (I-PROANT-DTRLLA) =              VHF0003 
DTRLLA             LLAVE-ANT-DTRLLA                                     VHF0003 
DTRLLA           SET IND-PROANT-DTRLLA TO I-PROANT-DTRLLA               VHF0003 
DTRLLA     END-SEARCH                                                   VHF0003 
DTRLLA     IF IND-PROANT-DTRLLA > 0                                     VHF0003 
DTRLLA        MOVE NO-ES TO ENC-MAESTRO                                 VHF0003 
DTRLLA     ELSE                                                         VHF0003 
DTRLLA        IF IND-DTRLLA <= 500                                      VHF0003 
DTRLLA           MOVE LLAVE-ANT-DTRLLA                                  VHF0003 
DTRLLA                TO LLAVE-PROANT-DTRLLA (IND-DTRLLA)               VHF0003 
DTRLLA           ADD 1 TO IND-DTRLLA                                    VHF0003 
DTRLLA           SORT PROANT-DTRLLA DESCENDING LLAVE-PROANT-DTRLLA      VHF0003 
DTRLLA           READ DTRLLA                                            VHF0003 
DTRLLA              INVALID KEY                                         VHF0003 
DTRLLA                 MOVE NO-ES TO ENC-MAESTRO                        VHF0003 
DTRLLA              NOT INVALID KEY                                     VHF0003 
DTRLLA                 IF NUM-CTA-ACT-DTRLLA = SPACES                   VHF0003 
DTRLLA                    MOVE NO-ES  TO ENC-MAESTRO                    VHF0003 
DTRLLA                 END-IF                                           VHF0003 
DTRLL2                 MOVE AAAAMMDD-REN-DTRLLA TO AAAAMMDD-REN-AUX     VHF0007 
DTRLLA           END-READ                                               VHF0003 
DTRLLA        ELSE                                                      VHF0003 
DTRLLA           MOVE ERROR-DTRLLA TO ENC-MAESTRO                       VHF0003 
DTRLLA        END-IF                                                    VHF0003 
DTRLLA     END-IF.                                                      VHF0003 

       9300-ACTUALIZAR-MAES-CHECK.
REMARK******************************************************************
REMARK*9300-ACTUALIZAR-MAES-CHECK.                                     *
REMARK*    SE REECRIBE ARCHIVO MAESTRO DE CUENTAS CORRIENTES           *
REMARK******************************************************************
           MOVE SI TO ACT-MAESTRO.
           REWRITE COMPLETO-ABD06
              INVALID KEY
                 MOVE NO-ES TO ACT-MAESTRO
           END-REWRITE
           IF IND-DTRLLA = 3  OR IND-DTRLLA = 3
              MOVE LLAVE-MAES-CHECK TO LLAVE-ANT-AUX
              PERFORM 9390-ACTUALIZAR-DTRLLA
           END-IF.

       9310-ACTUALIZAR-MAES-CREDIT.
REMARK******************************************************************
REMARK*9310-ACTUALIZAR-MAES-CREDIT.                                    *
REMARK*    SE REECRIBE ARCHIVO MAESTRO DE CARTERA                      *
REMARK******************************************************************
           MOVE SI TO ACT-MAESTRO.
           REWRITE COMPLETO-ABD07
              INVALID KEY
                 MOVE NO-ES TO ACT-MAESTRO
           END-REWRITE
           IF IND-DTRLLA = 3  OR IND-DTRLLA = 4
              MOVE LLAVE-MAES-CREDIT TO LLAVE-ANT-AUX
              PERFORM 9390-ACTUALIZAR-DTRLLA
           END-IF.

       9390-ACTUALIZAR-DTRLLA.
REMARK******************************************************************
REMARK*9390-ACTUALIZAR-DTRLLA.                                         *
REMARK*    SE REECRIBE ARCHIVO PUENTE DTRLLA                           *
REMARK******************************************************************
           MOVE LLAVE-ANT-AUX TO LLAVE-ANT-DTRLLA
           READ DTRLLA
              NOT INVALID KEY
                 MOVE FUNCTION CURRENT-DATE (1:8)
                      TO AAAAMMDD-CIEREC-DTRLLA
                 REWRITE REG-DTRLLA
           END-READ.

REMARK******************************************************************
REMARK*    RUTINAS EXTERNAS                                            *
REMARK******************************************************************
           COPY VALVLR.PROC.
MODCPY     COPY MAESPLANEXT.PROC.                                       GJC0023
           COPY UTLBIT.PROC.
BDDDB2     COPY LIBSQL.PROC.                                            VHF0017 
MATC02     COPY LEEMAESTROS.PROC.                                       VHF0020
MATC02     COPY BDIIVIDE.PROC.                                          VHF0020

