       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ACTALEJUR.
      *******************************************************************
      *   PROGRAMA QUE ACTUALIZA EL ARCHIVO DE ALERTAS CON LA INFORMACION
      *   ENTREGADA POR JURISCOOP 
      *******************************************************************
       author.        DATACREDITO.
       date-written.
       date-compiled.
       configuration section.
       source-computer. microfocus.
       object-computer. microfocus.
       input-output section.
       file-control.
      ************************************************************************
      ************************************************************************
           copy BDIIREGI.FS.
      * BDIIREGI.v.03.0001
      *===============================================================
      * ESTRUCTURA del ARCHIVO ENTREGADO POR LA REGISTRADURIA NACIONAL
      * Equivale al Registro Civil de Argentina, contiene los ids de
      * gente
      *===============================================================
      $SET IDXFORMAT"8"
      $SET CONSTANT SW-STATUS-BDIIREGI"NO"
           select BDIIREGI
               assign to disk TITLE-BDIIREGI
               organization is indexed
               access mode is dynamic
               record key is LLAVE-BDIIREGI
      $if SW-STATUS-BDIIREGI = "NO"
      $display ".............COMPILANDO BDIIREGI SIN status.........."
      $else
      $display ".............COMPILANDO BDIIREGI CON status.........."
               file status is COD-RETORNO-BDIIREGI
      $end
               lock mode is manual with lock on record.
      $SET IDXFORMAT"0"

      ************************************************************************
      ************************************************************************
       copy BDIIDIAN.FS.
      * BDIIDIAN.v.03.0001
      * MAESTRO DE PERSONAS JURIDICAS NACIONALES Y EXTRANJERAS REPORTADAS
      * POR LA DIAN
      * DIAN equivale a la AFIP de Argentina, contiene ids de empresas
      *==================================================================
      $SET CONSTANT SW-STATUS-BDIIDIAN"NO"
           select BDIIDIAN assign to disk TITLE-BDIIDIAN
               organization is indexed
               access mode  is dynamic
               record key   is LLAVE-BDIIDIAN
      $if SW-STATUS-BDIIDIAN = "NO"
      $display ".............COMPILANDO BDIIDIAN SIN status.........."
      $else
      $display ".............COMPILANDO BDIIDIAN CON status.........."
               file status is COD-RETORNO-BDIIDIAN
      $end
               lock mode is manual with lock on record.
      *==================================================================

      ************************************************************************
      ************************************************************************
       copy BDIIDAS.FS.
      * BDIIDAS.v.03.0001
      *=====================================================================
      * ARCHIVO MAESTRO DE CEDULAS DE EXTRANJERIA, PROPORCIONADAS POR EL DAS
      * Equivale a la SIDE de Argentina, provee ids de extranjeros
      *=====================================================================
      $SET CONSTANT SW-STATUS-BDIIDAS"NO"
           select BDIIDAS assign to disk TITLE-BDIIDAS
               organization is indexed
               access mode is dynamic
               record key is LLAVE-BDIIDAS
      $if SW-STATUS-BDIIDAS = "NO"
      $display ".............COMPILANDO BDIIDAS SIN status.........."
      $else
      $display ".............COMPILANDO BDIIDAS CON status.........."
               file status is COD-RETORNO-BDIIDAS
      $end
               lock mode is manual with lock on record.


      ************************************************************************
      ************************************************************************
           copy BDIIALE.FS.
      * BDIIALE-FS.v01.0001
      * Maestro de alertas reportadas por suscriptotres
      * Suscriptores son bancos, financieras y otros clientes de
      * Datacredito

      $SET CONSTANT SW-STATUS-BDIIALE"NO"
           select BDIIALE
               assign to disk MAES-BDIIALE-TITLE
               organization is indexed
               access mode is dynamic
               record key is LLAVE-BDIIALE
      $if SW-STATUS-BDIIALE = "NO"
      $display ".............COMPILANDO BDIIALE SIN status.........."
      $else
      $display ".............COMPILANDO BDIIALE CON status.........."
               file status is EST-BDIIALE
      $end
               lock mode is manual with lock on record.

      ************************************************************************
      ************************************************************************
       select ENTRADA assign to "$TEMPORALES/ACTALEJUR.ENT"
           organization is line sequential.
      ************************************************************************
      ************************************************************************
       select SALIDA assign to "$TEMPORALES/ACTALEJUR.SAL"
           organization is line sequential.
      *----------------------------------------------------------------*
       DATA DIVISION.
       file section.

      ************************************************************************
      ************************************************************************
      * Contiene los ids de gente (personas) y otros datos personales
       copy BDIIREGI.FD. 
      * BDIIREGI.v.03.0006
      *=================================================================
      * ESTRUCTURA del ARCHIVO ENTREGADO POR LA REGISTRADURIA NACIONAL
      *=================================================================
      * GJC0002   * BLQTUT * CAMPO PARA MARCAR LAS IDENDIFICACIONES
      * G.JIMENEZ * QUE HAN SIDO BLOQUEADAS POR TUTELAS PERDIDAS
      * SEPT.18/03*
      ******************************************************************
      * FAC0003   * FECMOD * FECHA DE CREACION Y DE MODIFICACION DE LAS
      * F.CUADROS * IDENTIFICACIONES. ADEMAS INCLUSION DE INDICADOR CON
      * SEPT.16/05* LA FUENTE: A-ARCHIVO REGIS, C-COMUNICACION ESCRITA
      *           * REGISTRADURIA, S-SOLICITUD INTERNA CAS.
      ******************************************************************
      * GJC0004   * INDMOD * SE CREA CAMPO PARA DEJAR INDICADOR DE MOFI-
      * F.CUADROS * FICACIONES, QUEDA IGUAL QUE EN EL OTRO INDICADOR
      * ENERO/2007* LA FUENTE: A-ARCHIVO REGIS, C-COMUNICACION ESCRITA
      *           * REGISTRADURIA, S-SOLICITUD INTERNA CAS T-TERCERO.
      ******************************************************************
      * CAV0005   *CONSUL * SE ADICIONAN LOS CAMPOS PARA IDENTIFICAR
      * C.VELASCO *QUE LA CEDULA CORRESPONDE A UN CONSULADO Y EL CODIGO
      * ABRIL/2007*del CONSULADO ,ARCHIVO CONSULADOS.DAT, PAIS Y CIUDAD
      ******************************************************************
      * CAV0006   *INDE01 * SE ADICIONAN LOS CAMPOS PARA IDENTIFICAR
      * C.VELASCO *         SI EL REGISTRO CORRESPONDE A INDEPENDIENTE
      * SEPTI/2013*         DE CAMARA C O BURO. (INDEPTES CON TIPO ID 9)
      *                     SE MARCA CAMPO IND-CREACION-BDIIREGI = "I"
      ******************************************************************
       FD  BDIIREGI.
       01  REG-BDIIREGI.
           03  LLAVE-BDIIREGI          pic 9(12).
           03  LLAVE-BDIIREGI-R        redefines LLAVE-BDIIREGI.
               05  TIPO-ID-BDIIREGI    pic 9.
               05  CEDULA-BDIIREGI     pic 9(11).
           03  DES-NOMBRE1-BDIIREGI    pic X(12).
           03  DES-NOMBRE2-BDIIREGI    pic X(12).
           03  APELLIDO1-BDIIREGI      pic X(16).
           03  APELLIDO2-BDIIREGI      pic X(16).
           03  ESTADO-CIVIL-BDIIREGI   pic 9.
           03  FECHA-EXPEDI-BDIIREGI.
               05  SIGLO-EXP-BDIIREGI  pic 9(2).
               05  FECHA-EXP-BDIIREGI  pic 9(6).
           03  INF-NUMERICA-BDIIREGI   pic 9(7).
           03  INF-NUMERICA-BDIIREGI-R redefines INF-NUMERICA-BDIIREGI.
               05 DEP-EXP-BDIIREGI     pic 99.
               05 MUNIC-EXP-BDIIREGI   pic 999.
               05 ESTADO-VIG-BDIIREGI  pic 99.
           03  FECHA-NACIMNTO-BDIIREGI.
               05  SIGLO-NAC-BDIIREGI  pic 9(2).
               05  FECHA-NAC-BDIIREGI  pic 9(6).
           03  FECHA-EDICTO-BDIIREGI.
               05  SIGLO-EDI-BDIIREGI  pic 9(2).
               05  FECHA-EDI-BDIIREGI  pic 9(6).
           03  IND-SEXO-BDIIREGI       pic X.
           03  IND-ESTADO-BDIIREGI     pic 9.
           03  NUM-RESOLUC-BDIIREGI    pic 9(4).
           03  ANO-RESOLUC-BDIIREGI    pic 9(2).
           03  BLOQUEO-TUTELA-BDIIREGI pic X(1).                        GJC0002
           03  FECHA-CREAC-BDIIREGI    pic 9(8).                        FAC0003
           03  FECHA-MODIF-BDIIREGI    pic 9(8).                        FAC0003
           03  IND-MODIF-BDIIREGI      pic X(1).                        FAC0003
           03  IND-CREACION-BDIIREGI redefines                          GJC0004
                    IND-MODIF-BDIIREGI pic X(1).                        GJC0004
           03  IND-CAMBIO-BDIIREGI     pic X(1).                        GJC0004
           03  IND-CONSULADO-BDIIREGI  pic 9(1).                        CAV0005
           03  COD-CONSULADO-BDIIREGI  pic 9(3).                        CAV0005
           03  ES-IND-CAMARA-BDIIREGI  pic 9(1).                        CAV0006
           03  ES-IND-BURO-BDIIREGI    pic 9(1).                        CAV0006
           03  FILLER-BDIIREGI         pic X(27).                       CAV0006
      *----------------------------------------------------------------*

      ************************************************************************
      ************************************************************************
       copy BDIIDIAN.FD. 
      * BDIIDIAN.v.03.0004
      *==================================================================
      * Maestro de personas juridicas nacionales y extranjeras reportadas
      * por la DIAN
      *==================================================================
      * FAC0002 * INCIIU* INCLUSION DE CODIGO DE ACTIVIDAD ECONOMICA- CIIU
      * JUL.17/2003 * Y FECHA DE INSCRIPCION
      *==================================================================
      * CAV0003 * INDMOD* INCLUSION DE FECHA-CREACION,FECHA-MODIFCACION E
      * C. VELASCO  * INDICADOR MODIFICACION: A-ARCHIVO DIAN,
      * OCT/25/2007 * C-COMUNICACION DIAN, S-SOLICTUD CAS.
      *==================================================================
      * RMB0004     * INDE01* INCLUSION DE MARCAS PARA :
      * R.MARTINEZ  *       A)  ES CLIENTE DE EXPERIAN-DATACREDITO
      * SEP/20/2013 *       B)  CLIENTE QUE VERA A LOS INDEPENDIENTES
      * C.VELASCO.  *       SE CREA CAMPO FECHA-MODIF-INDE-BDIIDIAN
      *==================================================================
       FD  BDIIDIAN.
       01  REG-BDIIDIAN.
           03 LLAVE-BDIIDIAN.
              05 NITS-BDIIDIAN            pic X(11).
              05 NITS-R-BDIIDIAN redefines NITS-BDIIDIAN
                                          pic 9(11).
           03 DIGITO-CONTROL-BDIIDIAN     pic X(01).
           03 DESCRIPCION1-BDIIDIAN       pic X(80).
           03 DESCRIPCION1-BDIIDIAN-R redefines DESCRIPCION1-BDIIDIAN.
              05 DESCRIPCION-BDIIDIAN     pic X(79).
              05 ESTADO-BDIIDIAN          pic 9.
           03 COD-CIIU-BDIIDIAN           pic 9(4).                     FAC0002
           03 FEC-INSCRIP-BDIIDIAN        pic 9(8).                     FAC0002
           03 FECHA-CREAC-BDIIDIAN        pic 9(8).                     CAV0003
           03 FECHA-MODIF-BDIIDIAN        pic 9(8).                     CAV0003
           03 IND-MODIF-BDIIDIAN          pic X(1).                     CAV0003
           03 ES-CLIENTE-DATACR-BDIIDIAN  pic X(1). *> S,N              RMB0004
           03 VE-INDEPENDIENTES-BDIIDIAN  pic X(1). *> S,N              RMB0004
           03 FECHA-MODIF-INDE-BDIIDIAN   pic 9(04). *> AAMM            CAV0004
      *    03 FILLER-BDIIDIAN             pic X(19).                    RMB0004
           03 FILLER-BDIIDIAN             pic X(13).                    CAV0004

      ************************************************************************
      ************************************************************************
       copy BDIIDAS.FD. 
      * BDIIDAS.v.03.0002
      *====================================================================
      * Archivo maestro de cedulas de extranjeria suministradas por el DAS
      *====================================================================
      * CAV0002 * INDMOD* INCLUSION DE FECHA-CREACION FECHA MODIFCACION E
      * C. VELASCO  * INDICADOR MODIFICACION, A-ARCHIVO DAS,
      * OCT/25/2007 * C-COMUNICACION DAS, S-SOLICTUD CAS.
      *==================================================================
       FD  BDIIDAS.
       01  REG-BDIIDAS.
           03 LLAVE-BDIIDAS.
              05 TIPO-ID-BDIIDAS           pic 9.
              05 NUM-ID-BDIIDAS            pic 9(11).
           03 NACIO-BDIIDAS                pic X(15).
           03 NOMBRE-BDIIDAS               pic X(45).
           03 FECHA-CREAC-BDIIDAS          pic 9(8).                    CAV0002
           03 FECHA-MODIF-BDIIDAS          pic 9(8).                    CAV0002
           03 IND-MODIF-BDIIDAS            pic X(1).                    CAV0002
           03 FILLER-BDIIDAS               pic X(31).                   CAV0002

      ************************************************************************
      ************************************************************************
       copy BDIIALE.FD.
      * BDIIALE-FD.v01.0003.
      * Maestro de alertas reportadas por suscriptores
      *****************************************************************
      * RMB0003   * CIU001: SE INCLUYE EL CODIGO DE LA ALERTA DENTRO DE
      * R.MARTINEZ* LA LLAVE, SE CAMBIA LA LONGITUD DE LLAVE-SECUNDARIA
      * OCTUB/2006* Y SE MANEJAN ALERTAS DE TEXTO FIJO
      ******************************************************************
       FD BDIIALE.
       01 REG-BDIIALE.
          02 LLAVE-BDIIALE.
             03 TIP-IDE-BDIIALE        pic 9(01).
             03 NUM-IDE-BDIIALE        pic 9(11).
             03 FUENTE-BDIIALE         pic 9(06).
             03 COD-ALERTA-BDIIALE     pic 9(03).                       RMB0003
             03 LLASEC-BDIIALE         pic X(08).                       RMB0003
          02 ENCABEZADO-BDIIALE.
             03 FEC-NOV-BDIIALE        pic 9(08).                       RMB0003
             03 FEC-VEN-BDIIALE        pic 9(08).                       RMB0003
             03 FEC-REP-BDIIALE        pic 9(08).                       RMB0003
             03 BLOQUEO-BDIIALE        pic 9(01).
             03 FILLER-BDIIALE         pic X(10).                       RMB0003
             03 INF-DATAHOST-BDIIALE   pic X(18).                       RMB0003
             03 LINEAS-BDIIALE.                                         RMB0003
                04 LINEA1-BDIIALE      pic X(50).                       RMB0003
                04 LINEA2-BDIIALE      pic X(50).                       RMB0003
             03 filler redefines LINEAS-BDIIALE.                        RMB0003
                04 DATOS-BDIIALE       pic X(100).                      RMB0003

      ************************************************************************
      ************************************************************************
       FD ENTRADA.
       01 ENTRADA-reg                   pic X(200).

      ************************************************************************
      ************************************************************************
       FD SALIDA.
       01 SALIDA-reg.                    
          02 SALIDA-errores              pic X(50).
          02 SALIDA-filler               pic X(1).
          02 SALIDA-registro             pic X(200).

      *-----------------------------------------------------------------
       working-storage section.

      ************************************************************************
      ************************************************************************
       copy BDIIVIDE.WS.     
      *BDIIVIDE.WS.v03.0003.
      * Variables para rutina de validacion de identificaciones
      *----------------------------------------------------------------*
      * F.CUADROS   * INCLUSION DE PARAMETRO PARA ASIGNAR TIPO DE
      * JUN.17/2004 * IDENTIFICACION DE ACUERDO A RANGOS PERMITIDOS.
      * FAC0002     * NUIP01 * ESTO SI NO VIENE CORRECTO EL TIPO-ID.
      *----------------------------------------------------------------*
      * F.CUADROS   * INCLUSION DE VALIDACION PARA TIPO-ID. = 2 y
      * JUL.19/2004 * QUITAR EL DIGITO DE CHEQUEO SI ES EL CASO.
      * FAC0003     *
      *----------------------------------------------------------------*
       01  NUM-ID-W-BDIIVIDE.
           03  NUM-ID-W11-BDIIVIDE           pic 9(11) value zeros.     FAC0003
           03  NUM-ID-W11-R-BDIIVIDE redefines NUM-ID-W11-BDIIVIDE.     FAC0003
               05 NUM-ID-W10-BDIIVIDE        pic 9(10).                 FAC0003
               05 RESTO-NUM-ID-BDIIVIDE      pic 9.                     FAC0003
       01  NOMBRES-AUX-W-BDIIVIDE.
           03 PRIMER-APELLIDO-BDIIVIDE       pic X(15) value spaces.
           03 SEGUNDO-APELLIDO-BDIIVIDE      pic X(15) value spaces.
           03 PRIMER-NOMBRE-BDIIVIDE         pic X(15) value spaces.
           03 SEGUNDO-NOMBRE-BDIIVIDE        pic X(15) value spaces.
           03 NOM-LEIDO-BDIIVIDE             pic X(65) value spaces.

       01 PARAMETROS-ENTRADA-BDIIVIDE        pic X(100) value spaces.   FAC0002
       01 PARAMETROS-ENTRADA-R-BDIIVIDE redefines
                                            PARAMETROS-ENTRADA-BDIIVIDE.
           03 TIP-IDE-BDIIVIDE               pic X.
           03 NUM-IDE-BDIIVIDE               pic X(11) JUSTIFIED right.
           03 NUM-IDE-BDIIVIDE-R redefines   NUM-IDE-BDIIVIDE           FAC0002
                                             pic 9(11).                 FAC0002
           03 NOMBRE-BDIIVIDE                pic X(65).
           03 SW-VERIFNOM-BDIIVIDE           pic 9.
           03 NUM-BLOQUES-BDIIVIDE           pic 9.
           03 SW-VALTIPID-BDIIVIDE           pic 9.                     FAC0002
           03 SW-ASGTIPID-BDIIVIDE           pic 9.                     FAC0002
           03 SW-DIGCHEQUEO-BDIIVIDE         pic 9.                     FAC0003
           03 FILLER-BDIIVIDE                pic X(18).                 FAC0003

       01 PARAMETROS-SALIDA-BDIIVIDE.
          03 NOM-VAL-BDIIVIDE                pic X(65) value spaces.
          03 SW-ID-BDIIVIDE                  pic 9 value 0.
          03 SW-NOMBRES-BDIIVIDE             pic 9 value 0.
          03 SW-TID-BDIIVIDE                 pic 9 value 0.             FAC0002

      ************************************************************************
      ************************************************************************
       copy BDIIALE.WS.      
      * BDIIALE-WS.v01.0007
      * Variables relacionadas con las alertas del sistema
      **************************************************************************
      * RMB0002   * TIPALE: CREACION DE DIFERENTES TIPOS DE ALERTA, QUE SE
      * R.MARTINEZ* PUEDEN GENERAR SOBRE DIFERENTES TIPOS DE INFORMACION
      * ABRIL/2006* (ARCHIVOS)
      *************************************************************************
      * RMB0003   * CIU001: AJUSTES PARA MANEJO DE ALERTAS DE CIUDADANOS DE
      * R.MARTINEZ* COLOMBIA.
      * NOV./2006 *
      *************************************************************************
      * VHF0004     * REFCOM - SE AJUSTA EL TAMA▒O DE LAS LINEAS DE    *
      * VICTOR HUGO *          DETALLE DE ALERTA YA QUE SU TAMA▒O      *
      * FORERO      *          ESTABA DE 81 Y no DE 80 QUE ES ESTANDAR *
      * OCT.29/2008 *                                                  *
      ******************************************************************
      * GJC0005     * LEY001 - SE CAMBIA EL GUIN INICIAL DE LA PRESENTACION
      * G.JIMENEZ   *          DE LA ALEERTA POR UN ESPACIO EN BLANCO. ESTO
      * MARZO 2/2009*          PARA EVITAR QUE SE INTERPRETE COMO ALGO
      *             *          NEGATIVO
      ******************************************************************
      * RMB0006     * INDE01 - SE AGREGA VARIABLE QUE INDICA QUE YA SE SABE
      * R.MARTINEZ  *          QUE LA OTRA IDENTIFICACION TIENE CUENTAS
      * JUNIO 4/2013*          PORQUE SE DETERMINO ANTES QUE ERA INDEPENDIENTE
      ******************************************************************
       01 VARIABLES-BDIIALE.
          02 ES-INDEPENDIENTE-BDIIALE           pic 9(01) value 0.      RMB0006
          02 MIRO-SIES-INDEPENDIENTE-BDIIALE    pic 9(01) value 0.      RMB0006
          02 MAES-BDIIALE-TITLE    pic X(20)                            RMB0003
                                   value "$DATABASE/BDIIALE".           RMB0003
          02 EST-BDIIALE           pic X(02) value "00".
          02 FIN-BDIIALE           pic 9(01) value 0.
          02 PROCESAR-BDIIALE      pic 9(01) value 0.                   RMB0002
          02 CONT-LINEAS-BDIIALE   pic 9(03) value 0.
          02 ABIERTO-BDIIALE       pic 9(01) value 0.
          02 TIP-IDE-AUX-BDIIALE   pic 9(01) value 0.
          02 NUM-IDE-AUX-BDIIALE   pic 9(11) value 0.
          02 MARCAR-ICBTCO-BDIIALE pic 9(01) value 0.                   VHF0004
          02 FECHA-AUX-BDIIALE     pic 9(08) value 0.                   RMB0003
          02 FECHA-NOV-AUX-BDIIALE pic 9(08) value 0.                   RMB0003
          02 FECHA-VEN-AUX-BDIIALE pic 9(08) value 0.                   RMB0003
          02 FECHA-REP-AUX-BDIIALE pic 9(08) value 0.                   RMB0003
          02 FUENTE-AUX-BDIIALE    pic 9(06) value 0.                   RMB0003
          02 NOMFUE1-AUX-BDIIALE   pic X(14) value spaces.              RMB0003
          02 NOMFUE2-AUX-BDIIALE   pic X(14) value spaces.              RMB0003
          02 CODALE-AUX-BDIIALE    pic 9(03) value 0.                   RMB0003
          02 TIPALE-AUX-BDIIALE    pic X(01) value spaces.              RMB0003
          02 TEXTO1-AUX-BDIIALE    pic X(50) value spaces.              RMB0003
          02 INF-DATAHOST-AUX-BDIIALE      pic X(18) value spaces.      RMB0003
          02 PAR-CODBLOALER        pic 9(1)  value 1.                   RMB0002
          02 PAR-MSGBLOALER        pic X(60) value                      RMB0002
             " ALERTA DADA POR LA ENTIDAD".                             RMB0002
       01 M-ALERTA.                                                     RMB0002
          03 V-ALERTA   occurs 40 TIMES pic 9(02).                      RMB0002
       01 TITULOS-BDIIALE.
          02 filler                pic X(30) value all "=".
          02 filler    pic X(20) value "==== [ ALERTAS ] ===".
          02 filler                pic X(30) value all "=".
          02 filler    pic X(20) value "FUENTE           FEC".
          02 filler    pic X(20) value "HA           DESCRIP".
          02 filler    pic X(20) value "CION                ".
          02 filler    pic X(20) value "                    ".
          02 filler                pic X(80) value all "=".
       01 LINEA1-BDIIALE-TIT.                                           RMB0003
          02 RAYA-BDIIALE          pic X(01) value " ".                 GJC0005
          02 NOMFUE-LIN1-BDIIALE   pic X(14) value spaces.              RMB0003
          02 filler                pic X(01) value spaces.              RMB0003
          02 FEC-NOV-LIN1-BDIIALE  pic X(11) value spaces.              RMB0003
          02 filler                pic X(01) value spaces.              RMB0003
          02 TEXTO-LIN1-BDIIALE    pic X(50) value spaces.              RMB0003
          02 filler                pic X(02) value spaces.              VHF0004
       01 LINEA2-BDIIALE-TIT.                                           RMB0003
          02 filler                pic X(01) value spaces.              RMB0003
          02 NOMFUE-LIN2-BDIIALE   pic X(14) value spaces.              RMB0003
          02 filler                pic X(01) value spaces.              RMB0003
          02 filler                pic X(11) value spaces.              RMB0003
          02 filler                pic X(01) value spaces.              RMB0003
          02 TEXTO-LIN2-BDIIALE    pic X(50) value spaces.              RMB0003
          02 filler                pic X(02) value spaces.              VHF0004
          02 TEXTO2-AUX-BDIIALE    pic X(50) value spaces.              RMB0003

       copy VALNOM.WS.       
      * VALNOM-WS.v.0002
      * DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES QUE
      * UTILIZAN LAS RUTINAS del       DE NOMBRES DE LA RUTINA VALNOM.PROC
      ***********************************************************************
      * GJC0002    * VALMEJ :SE CREAN ANUEVOS CAMPOS PARA MEJORAR LA VALIDACION
      * G.JIMENEZ  * DE NIT. PARA MEJORA TAMBIEN SE AMPLIA TAMANO DE VARIABLES
      * MARZO/2003 *
      *************************************************************************
        01 VARIABLES-VALNOM.
           03 VECTOR-INPUT-VALNOM.
             04 VM-INPUT-VALNOM     occurs 20 TIMES .
                05 VMM-INPUT-VALNOM occurs 15 TIMES pic X.
           03 VECTOR-OK-VALNOM.
             04 VM-OK-VALNOM         occurs 20 TIMES.
                05 VMM-OK-VALNOM    occurs 15 TIMES pic X.
           03 BLOQOK-AUX-VALNOM           pic 9(2).
           03 CADENA-VALNOM.
              04 filler                   pic X(2) value "A ".
              04 NOMBRE-AUX-VALNOM        pic X(65).
              04 filler redefines NOMBRE-AUX-VALNOM.
                 05 VNOM-AUX-VALNOM occurs 65 TIMES pic X.
           03 BASURA-VALNOM               pic X(10).
           03 BLOQUES-INPUT-VALNOM        pic 9(2).
           03 NOMBRE-INPUT-VALNOM         pic X(65).
           03 filler redefines NOMBRE-INPUT-VALNOM.
              05 V-NOMBRE-VALNOM          pic X occurs 65 TIMES.
           03 NOMBOK-INPUT-VALNOM         pic X(65).
           03 filler redefines NOMBOK-INPUT-VALNOM.
              05 V-NOMBOK-VALNOM          pic X occurs 65 TIMES.
           03 TIP-ID-INPUT-VALNOM         pic 9(02).
           03 MATCH-OK-OUT-VALNOM         pic 9(01).
           03 A-VALNOM                    pic 9(05).
           03 I-VALNOM                    pic 9(05).                    GJC0002
           03 J-VALNOM                    pic 9(05).                    GJC0002
           03 K-VALNOM                    pic 9(05).                    GJC0002
           03 L-VALNOM                    pic 9(05).                    GJC0002
           03 M-VALNOM                    pic 9(05).                    GJC0002
           03 INI-VALNOM                  pic 9(02).
           03 LON-VALNOM                  pic 9(02).
           03 LETRAS-OK-VALNOM            pic 9(02).
           03 LETRAS-OK2-VALNOM           pic 9(02).
           03 TOTLET-OK-VALNOM            pic 9(02).
           03 TOTLET-INP-VALNOM           pic 9(02).
           03 PEDAZO-VALNOM               pic X(15).
           03 POR-TOT-VALNOM              pic 9(05).
           03 POR-INP-VALNOM              pic 9(05).
           03 APROX-TOT-VALNOM            pic 9(05).
           03 APROX-INP-VALNOM            pic 9(05).
           03 VECTORES-VALNOM.                                          GJC0002
              04 filler                   occurs 256 TIMES.             GJC0002
                 05 CONT-LET-INPUT-VALNOM pic 9(2).                     GJC0002
                 05 CONT-LET-OK-VALNOM    pic 9(2).                     GJC0002
           03 VAL-APELLIDO-VALNOM         pic 9.                        GJC0002

       copy ICFILE.WS.
      * ICFILE-WS.v.0040.
      * COPY PARA DIRECCIONAR LOS ARCHIVOS.
      *****************************************************************
      * RMB0008     * SE ADICIONA NOMBRE DE ARCHIVO DE BASE DE DATOS
      *R.MARTINEZ   * UNIVERSAL.
      ******************************************************************
      * VHF0009     * SE ADICIONA TABLA DE CIIUS                       *
      * VICTOR HUGO *                                                  *
      * FORERO      *                                                  *
      * DIC.01/2000 *                                                  *
      ******************************************************************
      * VHF0010     * SE ADICIONAN COPYS PARA MANEJOS DE NOMBRES DE :  *
      * VICTOR HUGO * NOMCCB - CAMARA DE COMERCIO DE BOGOTA            *
      * FORERO      * NOMREG - REGISTRADURIA                           *
      * SEP.10/2001 * NOMDAS - DAS                                     *
      *             * NMDIAN - DIAN                                    *
      ******************************************************************
      * VHF0011     * SE ADICIONAN NOMBRES DE ARCHIVO DE NOMBRES DE    *
      * VICTOR HUGO * BYINGTON                                         *
      * FORERO      *                                                  *
      * FEB.18/2002 *                                                  *
      ******************************************************************
      * FAC0012     * BDIEND - SE ADICIONAN RUTAS DE ARCHIVOS PARA     *   
      * F.CUADROS   * MANEJO DE ENDEUDAMIENTO GLOBAL                   *
      * SEP.04/2002 *                                                  *
      * GJC0012     * DTCLOG - SE ADICIONA NOMBRE del ARCHIVO DE LOG DE*
      * G.JIMENEZ   * MODIFIFICACIONES EN linea Y DATACIUDADANOS       *
      * OCT.29/2002 *                                                  *
      ******************************************************************
      * VHF0013     * ARCHIVOS ASOCIADOS CON SISTEMA DATACIUDADANOS    *
      * VICTOR HUGO * DTCING - ARCHIVO DE INGRESOS FALLIDOS            *
      * FORERO      * DTCFAL - MUESTRA LA ULTIMA TRANSACCION ANTES DE  *
      * ENE.16/2003 *          LAS BAJADAS del SISTEMA NORMAL O ANORMAL*
      *             * DTCMDF - CONTIENE CONSECUTIVOS DE RADICACION Y   *
      *             *          ENTRADA-ENTIDAD                         *
      *             * DTCTRA - LOG DE INSTRUCCIONES SQL FALLIDAS       *
      * GJC0013     * BDL001 - NOMBRE DE ARCHIVO DE CLIENTES DE DIREC- *
      *             *          CIONES Y TELEFONOS                      *
      *             * DTCABD - NOMBRE DE ARCHIVO DE MANEJO DE CAMBIOS  *
      *             *          EN REGISTROS DE CARTERA Y CUENTAS CO-   *   
      *             *          RRIENTES CON RECLAMOS VIGENTES          *
      ******************************************************************
      * GJC0014     * DTCAB1 - NOMBRE DE ARCHIVO DE MANEJO DE CAMBIOS  *
      * G.JIMENEZ   *          EN REGISTROS DE CARTERA Y CUENTAS CO-   *
      * ENE.16/2003 *          RRIENTES CON RECLAMOS VIGENTES AJUSTE EN*
      *             *          NOMBRE                                  *
      ******************************************************************
      * FAC0015     * WEB001 - INCLUSION DE NOMBRES DE ARCHIVOS PARA   *
      * F.CUADROS C.*          MANEJO DE CLAVES WEB                    *
      * FEB.14/2003 *                                                  *
      ******************************************************************
      * FAC0016     * MODBDI - INCLUSION DE NOMBRES DE ESTRUCTURAS DE  *
      * F.CUADROS C.*          BDII: BASES DE DATOS DE IDENTIFICACION, *
      * MAR.03/2003 *          CON NUEVOS DISENOS.                     *
      ******************************************************************
      * GJC0017     * DTCCOM - CREACION del ARCHIVO DE COMENTARIOS del *
      * G.JIMENEZ   *          SISTEMA DE DATACIUDADANOS.              *
      * JUN.05/2003 *                                                  *
      * VHF0017     * DTCNHB - DIRECCIONA ARCHIVO DE DIAS NO HABILES   *
      * VICTOR HUGO * ICDACT - SE ADICIONA NOMBRE DE ARCHIVO DE        *
      * FORERO      *          ESTADISTICAS DE SUSCRIPTORES ICDACT     *
      ******************************************************************
      * FAC0018     * MODDIR-  INCLUSION DE NOMBRES DE ESTRUCTURAS DE  *
      * F.CUADROS C.*          ACTALOG Y VALVEST EN NUEVA RUTA         *
      * JUL.09/2003 *          en $TRABAJO/logs                        *
      ******************************************************************
      * GJC0019     * CONRAZ-  INCLUSION DE NOMBRE DE ESTRUCTURA CONRAZ*
      * G.JIMENEZ  .*                                                  *
      * SEP.11/2003 *                                                  *
      ******************************************************************
      * GJC0020     * FRAUDE - INCLUSION DE MANEJO DE ARCHIVO FRALOG   *
      * G.JIMENEZ  .*                                                  *
      * OCT.14/2004 *                                                  *
      ******************************************************************
      * GJC0021     * PRODUC - SE INCLUYE MANEJO DE ARCHIVO DE         *
      * G.JIMENEZ   *          PRODUCTOS                               *
      * ABR/2005    *                                                  *
      ******************************************************************
      * FAC0022     * FRAUD2 - SE INCLUYE MANEJO DE ARCHIVO DE         *
      * F.CUADROS   *          FRAINQ.DAT                              *
      * JUN.23/2005 *                                                  *
      ******************************************************************
      * FAC0023     * REPMIC - SE INCLUYE TITULO DE ARCHIVO            *
      * F.CUADROS   *          HISTORICO PARA MICROCREDITO             *
      * SEP.02/2005 *                                                  *
      ******************************************************************
      * FAC0023     * SUPT08 - INCLUSION DE TITULO DE MAESTRO DE       *
      * F.CUADROS   *          LA SUPERBANCARIA POR CAMBIO A FORMATO   *
      * OCT.28/2005 *          TIPO 8                                  *
      *             *                                                  *
      ******************************************************************
      * FAC0024     * LOGCAL - INCLUSION DE RUTA DE ARCHIVO DE LOGS    *
      * F.CUADROS   *          PARA PROCESOS del AREA DE CALIDAD.      *
      * OCT.17/2006 *                                                  *
      ******************************************************************
      * CAV0025     * NVAPRE - INCLUSION DE RUTA DE ARCHIVO DE         *
      * F.CUADROS   *          PREGUNTAS GRUPALES PARA FRAINQGR        *
      * SEP.11/2007 *                                                  *
      ******************************************************************
      * FLT0026     * BDSCOR - INCLUSION DE RUTA DE ARCHIVO DE         *
      * F.LOZANO T. *          SCORES MENSUALES.                       *
      * OCT.31/2007 *                                                  *
      ******************************************************************
      * CAV0027     * LOGCAL - INCLUSION DE RUTA DE ARCHIVO DE LOG     *
      * C.VELASCO   *          DE LOS PROCESOS DE CALIDAD EJECUTADOS   *
      * MAR.05/2009 * LOGDRP - RUTA ARCHIVO PARA ACTUALIZAR DRP        *
      ******************************************************************
      * eal0028     * ICMVLR - INCLUSION DE RUTA DE ARCHIVOS ICMVLR    *
      * E.ALVAREZ   *          PARA PROCESAR 48 MESES del ARCHIVO      *
      * JUN.19/2009 *                                                  *
      ******************************************************************
      * RGH0029     * MOD029 - MODIFICAR LONGUITUD DE VARIABLES        *
      * R.GALVIS    *          MAES-DIRECTORY ARCHIVOS ICMVLR          *
      * JUL.09/2009 *          DE 17 A 16 POSICIONES                   *
      *             * CHEQ01 - SE INCLUYE DEFINICION DE MAESTRO DE     *
      *             *          CHEQUES                                 *
      ******************************************************************
      * VHF0030     * ICMCLN - INCLUSION DE RUTA DE NUEVO ARCHIVO DE   *
      * V.FORERO    *          CLIENTES                                *
      * SEP.28/2009 *                                                  *
      ******************************************************************
      * CAV0031     * NHC002 - INCLUSION DE RUTA DE NUEVO ARCHIVO DE   *
      * C.VELASCO   *          MARCAS PRIVADAS PARA TARJETAS           *
      * OCT.09/2009 *                                                  *
      ******************************************************************
      * RGH0032       * MOD032 - INCLUIR ARCHIVOS DE RECONOCER +       *
      * R. GALVIS H.  *          BDLDIRACE                             *
      * ENE. 01/2010  *          BDLMAIEXC                             *
      * INTERSCHEMES. *          BDLTELEXC                             *
      ******************************************************************
      * LAG0033       * SCOPRE - INCLUIR ARCHIVO SCOREPREC             *
      * AGO. 02/2010  *          BDLMAIEXC                             *
      * G.DESARROLLO  *          BDLTELEXC                             *
      * GJC0033       * FRZMYR - SE INCLUYE MANEJO DE ARCHIVO DE DES-  *
      * G.JIMENEZ     *          PLAZADOS Y SECUESTRADOS - FUERZA MA-  *
      * OCT./2010     *          YOR                                   *
      ******************************************************************
      * GJC0034       * BDANOM - SE INCLUYE MANEJO DE ARCHIVO DE POR-  *
      * G.JIMENEZ     *          CENTAJE DE NOMBRES Y APELLIDOS BDANOM *
      * FEB./2011     *                                                *
      ******************************************************************
      * CAV0035       * AJDANE - SE INCLUYE MANEJO DE ARCHIVO DANE DE  *
      * C.VELASCO     *          CIUDADES,DEPARTEAMENTOS Y CABECERAS O *
      * ABR./2011     *          CENTROS POBLADOS (DVPDANE)            *
      ******************************************************************
      * CAV0036       * CARIND - SE INCLUYE EL NOMBRE del ARCHIVO  DE  *
      * C.VELASCO     *          CARACTERISTICAS USA PRECALCULADAS     *
      * MAY./2011     *          TITLE-CARACT-IND                      *
      ******************************************************************
      * FARF037       * FRABDU - SE INCLUYE MANEJO DE ARCHIVO DE BASE  *
      * F.RODRIGUEZ   *          DE DATOS UNIVERSAL DE PREGUNTAS       *
      * JUN.22/2011   *                                                *
      ******************************************************************
      * CAV0038       * AJCIIU - SE INCLUYE MANEJO DE ARCHIVO DE BASE  *
      * C.VELASCO.    *          HISTORICA DE CODIGOS CIIU Y ACTIVIDAD *
      * SEP.21/2011   *          ECONOMICA                             *
      ******************************************************************
      * CAV0039       * MCREHI - SE INCLUYE EL NOMBRE del ARCHIVO  DE  *
      * C.VELASCO     *          ICMCREH QUE CONTIENE EL HISTORICO     *
      * JUN./2011     *          DE REGISTRO CERRADOS > 5 AÑOS         *
      ******************************************************************
      * CAV0040       * NUECMP - SE INCLUYE EL NOMBRE del ARCHIVO  DE  *
      * C.VELASCO     *          ICMCONTTRJ  CONTIENE LA RELACION DE   *
      * 09/ABR./2013  *          CONTRATO-TARJETA PARA TIPO CTA = 02.  *
      ******************************************************************
      * GJC0041       * VALOR1 - REQUERIMIENTO 15452 CREACION PRODUCTO *
      * G.JIMENEZ     *          VALORA. DEFINICION DE ARCHIVO QUE MA- *
      * 20/SEP./2013  *          NEJA LOS COBROS EN 2 MOMENTOS, UN     *
      *               *          QUE COBRA Y LOS OTROS NO
      ******************************************************************
      * GLB0042       * EVID03 - MEJORAS SISTEMA EVIDENTE              *
      * GLOBANT       *          PROD00006585                          *
      * NOV-2013      * SPRINT3                                        *
      ******************************************************************
      * JJM0043       * SCORPJ - SE INCLUYE UN NUEVO TITLE PARA EL     *
      * J. MELGAREJO  *          SCORE PJ.                             *
      * MAR-2014      * SCOSTB - SE INCLUYE UN NUEVO TITLE PARA EL     *
      *               *          SCORE STABILITY  (BDSCOREHIS)
      ******************************************************************
      *
       01  FILE-TITLES.
           05  BDUUNI-TITLE        pic X(80) value "$BDU/BDUUNI".       RMB0008
           05  NOMB-APEL-TITLE     pic X(80) value "$DATABASE/ICBNAP".
           05  CLAVE-TITLE         pic X(80) value "$DATABASE/ICBCLA".
           05  TERM-TITLE          pic X(80) value "$DATABASE/ICBTER".
           05  MAES-PERSON-TITLE   pic X(80) value "$DATABASE/ICMPER".
           05  MAES-CHECK-TITLE    pic X(80) value "$DATABASE/ICMCHK".
           05  MAES-CREDIT-TITLE   pic X(80) value "$DATABASE/ICMCRE".
           05  MAES-MICROC-TITLE   pic X(80) value "$DATABASE/ICMMCR".  DLM0001
           05  MAES-ICMCHQ-TITLE   pic X(80) value "$DATABASE/ICMCHQ".  GJC0029
           05  INDEXADO-TITLE      pic X(80) value "$DATABASE/ICBCON".
           05  SECUENCIAL-TITLE    pic X(80) value "$DATABASE/ICBTCO".
           05  SUSCRIPTOR-TITLE    pic X(80) value "$DATABASE/ICBSUS".
           05  ARCHNITS-TITLE      pic X(80) value "ICDIAN".
           05  ARCHIVO-CLIENTE     pic X(80) value "$DATABASE/ICMCLI".
           05  MAES-ICMCLN-TITLE   pic X(80) value "$DATABASE/ICMCLN".  VHF0030
           05  ARCHIVO-PREPAGOS    pic X(80) value "$DATABASE/ICMPRE".
           05  ARCHIVO-USUARIOS    pic X(80) value "$DATABASE/ICMUSU".
           05  TITLE-SCODTB        pic X(80) value "$DATABASE/SCODTB".
           05  TITLE-BDIRMAE1      pic X(80) value "$DATABASE/BDIRMAE1".
           05  TITLE-BDIRMAE2      pic X(80) value "$DATABASE/BDIRMAE2".
           05  TITLE-BDIRMAE3      pic X(80) value "$DATABASE/BDIRMAE3".
           05  TITLE-BDIRMAE4      pic X(80) value "$DATABASE/BDIRMAE4".
           05  TITLE-BDIRCTR       pic X(80) value "$DATABASE/BDIRCTR".
           05  TITLE-BDIMHIS       pic X(80) value "$DATABASE/BDIMHIS". FAC0023 
           05  BDAPRVTR-TITLE      pic X(80) value "$DATABASE/BDAPRVTR".CAV0031
           05  TITLE-DTCABD. 
               07 filler           pic X(13) value "$DATOS/DTCABD".     GJC0014
               07 ANOMESDIA-DTCABD pic 9(08) value zeros.               GJC0014
           05  BDLCLI-TITLE        pic X(80) value "$DATABASE/BDLCLI".  GJC0013
           05  SCOREPREC-TITLE     pic X(80) value "$DATOS/SCOREPREC".  LAG0033
           05  ARCHIVO-LOGS        pic X(80) value
               "/ic/datacred/desarrollo/logs/ICL325".
           05  ARCHIVO-TEMPORAL    pic X(80) value 
                                       "$TEMPORALES/TEMPORAL".
           05  ESTAD-CADUC         pic X(80) value  
                                       "$TEMPORALES/ESTCAD.DAT".
           05  MATRIZ-CADUC        pic X(80) value  
                                       "$DATABASE/ICTCAD".
           05  ETQ-CIN             pic X(80) value
                                       "$TEMPORALES/ICINCO.DAT".
           05  RECLACOM-TITLE      pic X(80) value "$DATABASE/ICMRCM".
           05  RECLAMOS-TITLE      pic X(80) value "$DATABASE/ICMRCL".
           05 MAESTRO-FACUCLI      pic X(80) value "$DATABASE/FACUCLI".
           05 TITLE-FACUCLP        pic X(80) value "$DATABASE/FACUCLP".
           05 TITLE-FACUAUT        pic X(80) value "$DATABASE/FACUAUT".
           05 TITLE-FACULOG        pic X(80) value "$DATABASE/FACULOG".
           05 TITLE-FACUPAR        pic X(80) value 
                                       "$DATABASE/FACUPAR.DAT".
           05 MAES-ICMCREH-TITLE   pic X(80) value "$DATABASE/ICMCREH". CAV0039
           05 MAES-ICMCHKH-TITLE   pic X(80) value "$DATABASE/ICMCHKH". CAV0039
           05 MAES-ICMCHQH-TITLE   pic X(80) value "$DATABASE/ICMCHQH". CAV0039

           05 TITLE-CIIU          pic X(80) value "$DATABASE/ICTCIIU".  VHF0009
           05 TITLE-BDINCCB       pic X(80) value "$ICREGIS/BDINCCB".   VHF0010
           05 TITLE-BDINREG       pic X(80) value "$ICREGIS/BDINREG".   VHF0010
           05 TITLE-BDINDAS       pic X(80) value "$ICREGIS/BDINDAS".   VHF0010
           05 TITLE-BDINDIAN      pic X(80) value "$ICREGIS/BDINDIAN".  VHF0010
           05 TITLE-CONRAZ        pic X(80)                             GJC0019 
                                  value "$DATABASE/CONRAZ.DAT".         GJC0019 
           05 TITLE-FRALOG        pic x(80) value "$DATABASE/FRALOG".   GJC0020
           05 TITLE-FRAINQ        pic X(80) value                       FAC0022
                                  "$DATABASE/FRAINQ.DAT".               FAC0022
           05 TITLE-FRAINQGR      pic X(80) value                       CAV0025 
                                  "$DATABASE/FRAINQGR.DAT".             CAV0025 
           05 TITLE-BDSCORES.                                           FLT0026 
              07 filler           pic X(15) value "$SCORES/BDSCORE".    FLT0026
              07 T-FECHA-PROC     pic 9(04).                            FLT0026
           05 TITLE-ICBPRO        pic x(80) value "$DATABASE/ICBPRO".   GJC0021
           05 TITLE-DTRLLA        pic x(80) value "$DATABASE/DTRLLA".
           05 TITLE-DTCLOG        pic X(80) value "$DATABASE/DTCLOG".   GJC0012
           05 TITLE-DTCCOM        pic X(80) value "$DATABASE/DTCCOM".   GJC0017
           05 TITLE-DTCING        pic X(80) value "$DATABASE/DTCING".   VHF0013
           05 TITLE-DTCFAL        pic X(80) value "$DATABASE/DTCFAL".   VHF0013
           05 TITLE-DTCMDF        pic X(80) value "$DATABASE/DTCMDF".   VHF0013
           05 TITLE-DTCTRA        pic X(80) value "$DATABASE/DTCTRA".   VHF0013
           05 TITLE-BDINBYN       pic X(80) value "$ICREGIS/BDINBYN".   VHF0011
           05 SI-BDIEMSUP         pic X(80) value "$DBSUPER/BDIEMSUP".  FAC0012 
           05 TITLE-BDIEMSUP      pic X(80) value "$DBSUPER/BDIESU".    FAC0023 
           05 SI-BDIEENTI         pic X(80) value "$DBSUPER/BDIEENTI".  FAC0012 
           05 SI-BDIEMPER         pic X(80) value "$DBSUPER/BDIEMPER".  FAC0012 
           05 SI-BDIETEMA         pic X(80) value "$DBSUPER/BDIETEMA".  FAC0012 
           05 SI-BDIEAPER         pic X(80) value "$DBSUPER/BDIEAPER".  FAC0012 
           05 SI-BDIEMENT         pic X(80) value "$DBSUPER/BDIEMENT".  FAC0012 
           05 SI-BDIEMOVI         pic X(80) value "$DBSUPER/BDIEMOVI".  FAC0012 
           05 SI-BDIEHSUP         pic X(80) value "$DBSUPER/BDIEHSUP".  FAC0012 
           05 SI-BDIERECL         pic X(80) value "$DBSUPER/BDIERECL".  FAC0012 
           05 SI-BDIEMTOT         pic X(80) value "$DBSUPER/BDIEMTOT".  FAC0012 
           05 NOMBRE-WEB-SAL.                                           FAC0015
              07 NOM-DUE-WEB-SAL pic X(15) value "$DATOS/WEB-SAL.".     FAC0015
              07 FECHA-WEB-SAL   pic 9(8).                              FAC0015
           05 NOMBRE-RAS-SAL.                                           FAC0015
              07 NOM-DUE-WEB-SAL pic X(15) value "$DATOS/RAS-SAL.".     FAC0015
              07 FECHA-RAS-SAL   pic 9(8).                              FAC0015
           05 NOMBRE-CLAVE-WEB.                                         FAC0015
              07 NOMBRE-WEB-W    pic X(80) value "$DATABASE/ARCH-WEB".  FAC0015
           05 NOMBRE-RAS-LOG.                                           FAC0015
              07 NOM-RAS-LOG     pic X(19) value                        FAC0015
                                 "$DATOS/WEB-RAS.LOG.".                 FAC0015
              07 FECHA-RAS-LOG   pic 9(8).                              FAC0015
           05 TITLE-BDIIDIAN     pic X(80)  value "$DATABASE/BDIIDIAN". FAC0016
           05 TITLE-BDIIREGI     pic X(80)  value "$ICREGIS/BDIIRE".    FAC0016
           05 TITLE-BDIIDAS      pic X(80)  value "$DATABASE/BDIIDAS".  FAC0016
           05 TITLE-DTCNHAB      pic X(80)  value "$DATABASE/DTCNHAB".  VHF0017
           05 TITLE-ICDACT       pic X(80)  value                       VHF0017
                                 "$TEMPORALES/ICDACT.DAT".              VHF0017
           05 NOMBRE-ACTALOG     pic X(80)  value                       FAC0018
                                 "$TRABAJO/logs/ACTALOG".               FAC0018
           05 NOMBRE-VALVEST     pic X(80)  value                       FAC0018
                                 "$TRABAJO/logs/VALVEST.DAT".           FAC0018
           05 NOMBRE-CALIDLOG.                                          CAV0027 
              07 NOM-CALIDLOG    pic X(23)  value                       CAV0027
                                 "$TRABAJO/logs/CALIDLOG-".             CAV0027 
              07 FCH-CALIDLOG    pic 9(8).                              CAV0027
           05 NOMBRE-CALIDDRP.                                          CAV0027
              07 NOM-CALIDDRP    pic X(24) value                        CAV0027
                                 "$TEMPORALES/CALIDAD-DRP.".            CAV0027
              07 FCH-CALIDDRP    pic 9(8).                              CAV0027
           05  MAES-ICMVLR-TITLE.                                       eal0028
               07 MAES-DIRECTORY1          pic X(16) value              RGH0029
                                                    "$VALORES/ICMVLR-". eal0028
               07 MAES-FILE1               pic X(63).                   eal0028
                                                                        eal0028
           05  MAES-ICMVLR-TITLE2.                                      eal0028
               07 MAES-DIRECTORY2          pic X(16) value              RGH0029
                                                    "$VALORES/ICMVLR-". eal0028
               07 MAES-FILE2               pic X(63).                   eal0028
                                                                        eal0028
           05  MAES-ICMVLR-TITLE3.                                      eal0028
               07 MAES-DIRECTORY3          pic X(16) value              RGH0029
                                                    "$VALORES/ICMVLR-". eal0028
               07 MAES-FILE3               pic X(63).                   eal0028
                                                                        eal0028
           05  MAES-ICMVLR-TITLE4.                                      eal0028
               07 MAES-DIRECTORY4          pic X(16) value              RGH0029
                                                    "$VALORES/ICMVLR-". eal0028
               07 MAES-FILE4               pic X(63).                   eal0028
                                                                        eal0028
           05  MAES-ICMVLR-TITLE5.                                      eal0028
               07 MAES-DIRECTORY5          pic X(16) value              RGH0029
                                                    "$VALORES/ICMVLR-". eal0028
               07 MAES-FILE5               pic X(63).                   eal0028
                                                                        eal0028
           05  MAES-ICMVLR-TITLE6.                                      eal0028
               07 MAES-DIRECTORY6          pic X(16) value              RGH0029
                                                    "$VALORES/ICMVLR-". eal0028
               07 MAES-FILE6               pic X(63).                   eal0028
           05 TITLE-BDLDIRACE  pic X(80) value "$DATABASE/BDLDIRACE".   RGH0032
           05 TITLE-BDLMAIEXC  pic X(80) value "$DATABASE/BDLMAIEXC".   RGH0032
           05 TITLE-BDLTELEXC  pic X(80) value "$DATABASE/BDLTELEXC".   RGH0032
           05 TITLE-BDANOM     pic X(80) value "$DATABASE/BDANOM".      GJC0034
           05 TITLE-FRZMYR     pic X(80) value "$DATABASE/FRZMYR".      GJC0033
           05 TITLE-DVPDANE    pic X(80) value "$DATABASE/DVPDANE.DAT". CAV0035
           05 TITLE-CARACT-IND pic X(80) value                          CAV0036
                                         "$DATABASE/CARACTUSA-INDCL".   CAV0036
           05 FRABDU-TITLE     pic X(25) value                          FARF037
                                       "$VALORES/frabduact/FRABDU".     FARF037
           05 FRABDV-TITLE     pic X(29) value                          FARF037  
                                       "$VALORES/frabduact/FRABDU.NEW". FARF037
           05 MAES-ICMHCIIU-TITLE pic X(80) value "$DATABASE/ICMHCIIU". CAV0038
           05 MAES-ICMCONTTRJ-TITLE pic X(80) value                     CAV0040
                                          "$DATABASE/ICMCONTTRJ".       CAV0040
           05 TITLE-ICBCOB     pic X(16) value "$DATABASE/ICBCOB".      GJC0041
           05 TITLE-SCODAPJA   pic X(37) value "$DATABASE/SCODAPJA".    JJM0043
       01 TITLE-BDSCOREHIS.                                             JJM0043
          02 filler          pic X(21) value "$DATABASE/BDSCOREHIS.".   JJM0043
          02 ANO-ANT-TITLE-BDSCOREHIS  pic 9(04) value 2013.            JJM0043
          02 filler                    pic X(01) value "-".             JJM0043
          02 ANO-ACT-TITLE-BDSCOREHIS  pic 9(04) value 2014.            JJM0043


       01 PARAMETROS.
          03 COMANDO                     pic X(80)  value spaces.
          03 linea                       pic X(80)  value spaces.
       01 INDICES.
          03 I                           pic 9(5) value 0.
          03 J                           pic 9(5) value 0.
       01 CONTADORES.
          03 CONT-LEIDOS                 pic 9(10)  value 0.
          03 CONT-WRITE-OK               pic 9(10)  value 0.
          03 CONT-WRITE-ERR              pic 9(10)  value 0.
          03 CONT-REWRITE-OK             pic 9(10)  value 0.
          03 CONT-REWRITE-ERR            pic 9(10)  value 0.
          03 CONT-REGISTROS-OK           pic 9(10)  value 0.
          03 CONT-REGISTROS-ERR          pic 9(10)  value 0.
       01 LINEA1-AUX.
          02 filler    pic X(29) 
                       value "DEMANDAS JUDICIALES VIGENTES:".
          02 NUM-VIG-1 pic 9(05) value 0.  
          02 filler    pic X(08) value " ULTIMA:".                 
          02 FEC-VIG-1 pic 9(08) value 0.   
       01 LINEA2-AUX.
          02 filler    pic X(31) 
                       value "DEMANDAS JUDICIALES TERMINADAS:".
          02 NUM-TER-2 pic 9(05) value 0.  
          02 filler    pic X(14) value spaces.
       01 VARIABLES-GENERALES.
          03 real                        pic 9      value 0.
          03 FIN-ENTRADA                 pic 9      value 0.
          03 HAY-BDIIALE                 pic 9      value 0.
          03 FECHA-PROCESO               pic 9(8)   value 0.
          03 FECHA-VENCIMIENTO           pic 9(8)   value 0.
          03 DIAS-DESDE-1600             pic 9(8)   value 0.
       01 VALIDACIONES-FECHAS.
          02 ANO-VAL-X                   pic 9(04).
             88 ANO-VAL  value 1900 thru 2050.
          02 MES-VAL-X                   pic 9(02).
             88 MES-VAL  value 01   thru 12.
          02 DIA-VAL-X                   pic 9(02).
             88 DIA-VAL  value 01   thru 31.     
       01 INF-DATAHOST-AUX.
          02 NUM-DEMA-VIG-AUX            pic 9(5).
          02 NUM-DEMA-TER-AUX            pic 9(5).
          02 FEC-DEMA-VIG-AUX            pic 9(8).
       01 VARIABLES-INPUT.
          03 TIPO-IDE-INP                pic 9.
          03 NUM-IDE-INP                 pic 9(11).
          03 NOMBRES-INP                 pic X(30).
          03 APELLIDO-PAT-INP            pic X(30).
          03 APELLIDO-MAT-INP            pic X(30).
          03 NUM-DEMA-VIG-INP            pic 9(05).
          03 NUM-DEMA-TER-INP            pic 9(05).
          03 FEC-DEMA-VIG-INP            pic 9(08).
          03 TIPO-IDE-INP-X              pic X.
          03 NUM-IDE-INP-X               pic X(11).
          03 NUM-DEMA-VIG-INP-X          pic X(05).
          03 NUM-DEMA-TER-INP-X          pic X(05).
          03 FEC-DEMA-VIG-INP-X          pic X(08).
       01 M-ERRORES.
          03 V-ERR occurs 50 TIMES   pic 9(1).


      ************************************************************************
      ************************************************************************
       PROCEDURE DIVISION.

      * 0000-PROGRAMA-PRINCIPAL.
      * 1000-CREARDEF.
      * 1000-PROCESAR-DEMANDAS.   
      * 2000-ABRIR-ARCHIVOS.
      * 4000-PROCESAR-ENTRADA.
      * 4100-VALIDACIONES-BASICAS.
      * 4200-VALIDACION-OFICIALES.
      * 4300-MODIFICAR-BDIIALE.
      * 4310-CALCULAR-FECHAS.
      * 4320-ARMAR-TEXTO-ALERTA.
      * 4330-CREAR-BDIIALE.
      * 4340-ACTUALIZAR-BDIIALE.
      * 4400-GRABAR-REGISTROS-ERR.
      * 6000-SACAR-RESUMEN.
      * 80100-VALIDAR-IDENTIFICACION.
      * 80200-VALIDAR-ID.
      * 80300-ASIGNAR-TIPO-ID.
      * 80400-VALIDAR-TIPO-ID.
      * 80500-ELIMINAR-DIGITO-CHEQUEO.
      * 85010-UBICA-CEDULA.
      * 85020-UBICA-NIT.                                                 
      * 85030-UBICA-EXT.                                                 
      * 10000-MATCH-NOMBRES-VALNOM.                 
      * 10100-MATCH-APROXIMADO.
      * 10100-MATCH-APROX-EMPRESAS.


       0000-PROGRAMA-PRINCIPAL.
      ******************************************************************
      * PROGRAMA QUE EXTRAE INFORMACION del ARCHIVO DE LA REGIS
      * EJECUCION : X RMBREGIS 10 1                                  
      * MAXIMO HUECO DE 10,MINIMO DE UNO (DEFAULT SI NO SE DA NADA)
      ******************************************************************
           accept linea from COMMAND-LINE.
           unstring linea   delimited by all " " into
           COMANDO 
           evaluate COMANDO 
               when "CREARDEF"
                   perform 1000-CREARDEF                        
               when "real"            
                   move 1 to real
                   open i-o BDIIALE
                   perform 1000-PROCESAR-DEMANDAS
               when "FALSO"                 
                   move 0 to real
                   open input BDIIALE
                   perform 1000-PROCESAR-DEMANDAS
               when any 
                   display "COMANDO INVALIDO:" COMANDO(1:10)
                   display "A) real          "
                   display "B) FALSO         "
                   end-evaluate.
           stop run.


       1000-CREARDEF.
           move function CURRENT-DATE (1:8) to FECHA-PROCESO.
           open i-o BDIIALE.
           move 0 to TIP-IDE-BDIIALE.   
           move 0 to NUM-IDE-BDIIALE.   
           move 000001 to FUENTE-BDIIALE
           move 001 to COD-ALERTA-BDIIALE
           move "a" to LLASEC-BDIIALE.
           move 1 to HAY-BDIIALE.
           read BDIIALE invalid key 
               move 0 to HAY-BDIIALE
               end-read.
           if HAY-BDIIALE = 0
               move 
               "LA INDENTIFICACION NO TIENE NI HA TENIDO PROCESOS "
               to LINEA1-BDIIALE
               move 
               "JUDICIALES REGISTRADOS                            "
               to LINEA2-BDIIALE
               move FECHA-PROCESO to FEC-NOV-BDIIALE
               FEC-REP-BDIIALE
               move 20501231 to FEC-VEN-BDIIALE
               move 0 to BLOQUEO-BDIIALE
               move zeros to INF-DATAHOST-AUX
               move INF-DATAHOST-AUX to INF-DATAHOST-BDIIALE
               move spaces to FILLER-BDIIALE
               write REG-BDIIALE invalid key
                   display "ERROR EN rewrite"    
               not invalid key
                   display "ALERTA CREADA OK"    
                   end-write
           else
               display "YA EXISTE LA ALERTA"
           end-if.


       1000-PROCESAR-DEMANDAS.   
      ****************************************************************
      ****************************************************************
           perform 2000-ABRIR-ARCHIVOS.
           read ENTRADA at end
               move 1 to FIN-ENTRADA.
           perform until FIN-ENTRADA = 1
               add 1 to CONT-LEIDOS
               perform 4000-PROCESAR-ENTRADA
               read ENTRADA at end
                   move 1 to FIN-ENTRADA
               end-read
           end-perform.
           perform 6000-SACAR-RESUMEN.


       2000-ABRIR-ARCHIVOS.
      ****************************************************************
      ****************************************************************
           open input BDIIREGI.
           open input BDIIDIAN.
           open input BDIIDAS.
           open input ENTRADA.
           open output SALIDA.


       4000-PROCESAR-ENTRADA.
      ****************************************************************
      ****************************************************************
           string ENTRADA-reg delimited by "                  "
           ";"
           into ENTRADA-reg.
           INITIALIZE VARIABLES-INPUT.
           unstring ENTRADA-reg delimited by all ";" into
           TIPO-IDE-INP-X
           NUM-IDE-INP-X     
           NOMBRES-INP       
           APELLIDO-PAT-INP  
           APELLIDO-MAT-INP  
           NUM-DEMA-VIG-INP-X
           NUM-DEMA-TER-INP-X
           FEC-DEMA-VIG-INP-X .
           unstring ENTRADA-reg delimited by all ";" into
           TIPO-IDE-INP       
           NUM-IDE-INP       
           NOMBRES-INP       
           APELLIDO-PAT-INP  
           APELLIDO-MAT-INP  
           NUM-DEMA-VIG-INP  
           NUM-DEMA-TER-INP  
           FEC-DEMA-VIG-INP .
           perform 4100-VALIDACIONES-BASICAS.
           if M-ERRORES = zeros 
               perform 4200-VALIDACION-OFICIALES
           end-if
           if M-ERRORES = zeros 
               perform 4300-MODIFICAR-BDIIALE
               add 1 to CONT-REGISTROS-OK
           else
               perform 4400-GRABAR-REGISTROS-ERR
               add 1 to CONT-REGISTROS-ERR
           end-if.


       4100-VALIDACIONES-BASICAS.
      ****************************************************************
      ****************************************************************
           inspect TIPO-IDE-INP-X     replacing all " " by "0"
           inspect NUM-IDE-INP-X      replacing all " " by "0"
           inspect NUM-DEMA-VIG-INP-X replacing all " " by "0"
           inspect NUM-DEMA-TER-INP-X replacing all " " by "0"
           inspect FEC-DEMA-VIG-INP-X replacing all " " by "0"
           move zeros to M-ERRORES.
           if (TIPO-IDE-INP <> 1 and TIPO-IDE-INP <> 4) or
               TIPO-IDE-INP-X not numeric
               move 1 to V-ERR(1)
           end-if.
           if NUM-IDE-INP-X not numeric
               move 1 to V-ERR(2)
           end-if.
           if NOMBRES-INP = spaces or NOMBRES-INP numeric
               move 1 to V-ERR(3)
           end-if.
           if APELLIDO-PAT-INP  numeric
               move 1 to V-ERR(4)
           end-if.
           if APELLIDO-MAT-INP  numeric
               move 1 to V-ERR(5)
           end-if.
           if APELLIDO-PAT-INP  numeric
               move 1 to V-ERR(5)
           end-if.
           if NUM-DEMA-VIG-INP-X not numeric
               move 1 to V-ERR(6)
           end-if.
           if NUM-DEMA-TER-INP-X not numeric
               move 1 to V-ERR(7)
           end-if.
           if FEC-DEMA-VIG-INP-X not numeric
               move 1 to V-ERR(8)
           else
               move FEC-DEMA-VIG-INP(1:4) to ANO-VAL-X(1:4)
               move FEC-DEMA-VIG-INP(5:2) to MES-VAL-X(1:2)
               move FEC-DEMA-VIG-INP(5:2) to DIA-VAL-X(1:2)
               if not ANO-VAL or not MES-VAL or not DIA-VAL
                   move 1 to V-ERR(8)
               end-if
           end-if.
           TRIANGULACIONES
           if APELLIDO-PAT-INP = spaces and APELLIDO-MAT-INP = spaces
               move 1 to V-ERR(21)
           end-if.
           if NUM-DEMA-TER-INP = zeros and NUM-DEMA-VIG-INP = zeros
               move 1 to V-ERR(22)
           end-if.


       4200-VALIDACION-OFICIALES.
      ****************************************************************
      ****************************************************************
           move TIPO-IDE-INP to TIP-IDE-BDIIVIDE.
           move NUM-IDE-INP to NUM-IDE-BDIIVIDE.
           move spaces to NOMBRE-BDIIVIDE.
           string NOMBRES-INP      delimited by "   "
           " "
           APELLIDO-PAT-INP delimited by "   "
           " "
           APELLIDO-MAT-INP delimited by "   "
           into NOMBRE-BDIIVIDE.
           move 1 to SW-VERIFNOM-BDIIVIDE          
           move 0 to SW-DIGCHEQUEO-BDIIVIDE SW-ASGTIPID-BDIIVIDE  
           perform 80100-VALIDAR-IDENTIFICACION                  
           if SW-ID-BDIIVIDE = 0    *> EXISTE LA CEDULA
               if SW-NOMBRES-BDIIVIDE = 1    *> NOMBRE NO COINCIDE
                   move 1 to V-ERR(31)
               end-if
           end-if.


       4300-MODIFICAR-BDIIALE.
      ****************************************************************
      ****************************************************************
           move TIPO-IDE-INP to TIP-IDE-BDIIALE.   
           move NUM-IDE-INP to NUM-IDE-BDIIALE.   
           move 000001 to FUENTE-BDIIALE
           move 001 to COD-ALERTA-BDIIALE
           move "a" to LLASEC-BDIIALE.
           move 1 to HAY-BDIIALE.
           read BDIIALE invalid key 
               move 0 to HAY-BDIIALE
           end-read.
           perform 4310-CALCULAR-FECHAS.
           perform 4320-ARMAR-TEXTO-ALERTA.
           if HAY-BDIIALE = 0
               perform 4330-CREAR-BDIIALE
           else
               perform 4340-ACTUALIZAR-BDIIALE
           end-if.


       4310-CALCULAR-FECHAS.
      ****************************************************************
      ****************************************************************
           move function CURRENT-DATE (1:8) to FECHA-PROCESO 
           move function integer-of-date (FECHA-PROCESO)     
           to DIAS-DESDE-1600                        
           add  360 to DIAS-DESDE-1600
           move function date-of-integer (DIAS-DESDE-1600)      
           to FECHA-VENCIMIENTO.


       4320-ARMAR-TEXTO-ALERTA.
      ****************************************************************
      ****************************************************************
           move NUM-DEMA-VIG-INP to NUM-DEMA-VIG-AUX
           NUM-VIG-1
           move NUM-DEMA-TER-INP to NUM-DEMA-TER-AUX
           NUM-TER-2
           move FEC-DEMA-VIG-INP to FEC-DEMA-VIG-AUX
           FEC-VIG-1.


       4330-CREAR-BDIIALE.
      ****************************************************************
      ****************************************************************
           move FECHA-PROCESO to FEC-NOV-BDIIALE
           FEC-REP-BDIIALE.
           move FECHA-VENCIMIENTO to FEC-VEN-BDIIALE.
           move 0 to BLOQUEO-BDIIALE.
           move INF-DATAHOST-AUX to INF-DATAHOST-BDIIALE.
           move spaces to FILLER-BDIIALE.
           move LINEA1-AUX to LINEA1-BDIIALE.
           move LINEA2-AUX to LINEA2-BDIIALE.
           if real = 1
               write REG-BDIIALE invalid key
                   add 1 to CONT-WRITE-ERR
               not invalid key
                   add 1 to CONT-WRITE-OK   
               end-write
           else
               add 1 to CONT-WRITE-OK   
           end-if.


       4340-ACTUALIZAR-BDIIALE.
      ****************************************************************
      ****************************************************************
           move FECHA-PROCESO to FEC-NOV-BDIIALE.
           move FECHA-VENCIMIENTO to FEC-VEN-BDIIALE.
           move INF-DATAHOST-AUX to INF-DATAHOST-BDIIALE.
           move LINEA1-AUX to LINEA1-BDIIALE
           move LINEA2-AUX to LINEA2-BDIIALE.
           if real = 1
               rewrite REG-BDIIALE invalid key
                   add 1 to CONT-REWRITE-ERR
               not invalid key
                   add 1 to CONT-REWRITE-OK   
               end-rewrite
           else
               add 1 to CONT-REWRITE-OK   
           end-if.


       4400-GRABAR-REGISTROS-ERR.
      ****************************************************************
      ****************************************************************
           move M-ERRORES to SALIDA-errores.   
           move ";" to SALIDA-filler. 
           move ENTRADA-reg to SALIDA-registro.
           write SALIDA-reg.


       6000-SACAR-RESUMEN.
      ****************************************************************
      ****************************************************************
           display "==============[ RESUMEN del PROCESO ]============"
           display "TIPO DE PROCESO             = "   COMANDO(1:10)
           display "DEMANDADOS LEIDOS           = " CONT-LEIDOS  
           display "  DEMANDADOS VALIDOS        = " CONT-REGISTROS-OK 
           display "    DEMANDADOS CREADOS      = " CONT-WRITE-OK   
           display "    DEMANDADOS ACTUALIZADOS = " CONT-REWRITE-OK  
           display "    DEMANDADOS ERR CREAR    = " CONT-WRITE-ERR 
           display "    DEMANDADOS ERR ACTUALIZ = " CONT-REWRITE-ERR 
           display "  DEMANDADOS NO VALIDOS     = " CONT-REGISTROS-ERR 
           display "=================================================".

           copy BDIIVIDE.PROC.
      *BDIIVIDE.PROC.v.03.0006
      ******************************************************************
      * Rutina de validacion de identificaciones
      ******************************************************************
      * F.CUADROS   * INCLUSION DE PARAMETRO PARA ASIGNAR EL TIPO DE
      * JUN.17/2004 * IDENTIFICACION DE ACUERDO A RANGOS PERMITIDOS
      *             * NUIP01 * RUTINA 80300-ASIGNAR-TIPO-ID
      *             * INCLUSION DE PARAMETRO PARA VALIDAR EL TIPO-ID
      * FAC0002     * NUIP01 * RUTINA 80400-VALIDAR-TIPO-ID.
      ******************************************************************
      * F.CUADROS   * RELLENAR DE CEROS EL NUMERO DE IDENTIFICAICON 
      * JUL.16/2004 * CUANDO SE LE QUITA EL DIGITO DE CHEQUEO       
      * FAC0003     * NUIP02 * VALIDACION DE IDENTIFICACION SI TIPO-ID 2
      ******************************************************************
      * F.CUADROS   * NUIP03: INCLUSION DE RUTINA PARA ELIMINAR EL  
      * AGO.11/2004 * DIGITO DE CHEQUEO. 
      * FAC0004     * CAMBIO DE TIPO ID = 2 POR 3 SI EL NIT NO 
      *             * CORRESPONDE AL RANGO.
      ******************************************************************
      * F.CUADROS   * RANGO5: DEFINICION DE NUEVOS RANGOS PARA TIPO ID 3:
      * OCT.22/2004 * ENTRE 700.000.001 Y 799.999.999 (OTROS DOCUMENTOS)
      * FAC0005     * DEFINICION DE NUEVOS RANGOS PARA TIPO ID 2:
      *             * ENTRE 900.000.000 Y 999.999.999 (MUISCA)
      ******************************************************************
      * F.CUADROS   * NUIP04: INCLUSION DE RANGO PARA TIPO ID. 2 EN     
      * OCT.19/2005 * RUTINA VALIDAR-TIPO-ID PARA NITS QUE SON NUIPS    
      * FAC006C     * 
      ******************************************************************
      * C.VELASCO   * SE VUELVE A LA VERSION ANTES DE MODIFICAR LOS
      * JUL.12/2012 * RANGOS DE IDENTIFICACIONES,  LOS AJUSTES DE LA
      * CAV0008     * VERSION 07, IMPACTA APERTURAS Y PROCESOS ESPEC.
      ******************************************************************


       80100-VALIDAR-IDENTIFICACION.
      *===================================*
      *80100-VALIDAR-IDENTIFICACION.
      * Valida el numero de identificacion contra las bases de datos
      * de validacion de informacion BDIIDIAN, BDIIDAS, BDIIREGI
      * y verifica la coincidencia del nombre reportado
      *===================================*
           INITIALIZE PARAMETROS-SALIDA-BDIIVIDE VARIABLES-VALNOM 
           NOMBRES-AUX-W-BDIIVIDE
           move zeros to NUM-IDE-BDIIVIDE
           if SW-VERIFNOM-BDIIVIDE not numeric                          FAC0002 
               move zeros to SW-VERIFNOM-BDIIVIDE                       FAC0002 
           end-if                                                       FAC0002 
           if SW-ASGTIPID-BDIIVIDE not numeric                          FAC0002 
               move zeros to SW-ASGTIPID-BDIIVIDE                       FAC0002 
           end-if                                                       FAC0002 
           if SW-VALTIPID-BDIIVIDE not numeric                          FAC0002 
               move zeros to SW-VALTIPID-BDIIVIDE                       FAC0002 
           end-if                                                       FAC0002 
           if SW-DIGCHEQUEO-BDIIVIDE not numeric                        FAC0003 
               move zeros to SW-DIGCHEQUEO-BDIIVIDE                     FAC0003 
           end-if                                                       FAC0003 
           if NUM-IDE-BDIIVIDE is numeric and NUM-IDE-BDIIVIDE <> zeros
               if SW-ASGTIPID-BDIIVIDE = 1                              FAC0002 
                   perform 80300-ASIGNAR-TIPO-ID                        FAC0002 
               end-if                                                   FAC0002 
               if SW-VALTIPID-BDIIVIDE = 1                              FAC0002 
                   perform 80400-VALIDAR-TIPO-ID                        FAC0002 
               end-if                                                   FAC0002 
               if SW-DIGCHEQUEO-BDIIVIDE = 1                            FAC0004 
                   perform 80500-ELIMINAR-DIGITO-CHEQUEO                FAC0004 
               end-if                                                   FAC0004 
               if SW-VALTIPID-BDIIVIDE = 0 or                           FAC0004 
                   (SW-VALTIPID-BDIIVIDE = 1 and SW-TID-BDIIVIDE = 0)   FAC0002 
                   perform 80200-VALIDAR-ID
                   INITIALIZE NOM-VAL-BDIIVIDE
                   move NOM-LEIDO-BDIIVIDE to NOM-VAL-BDIIVIDE
                   if SW-VERIFNOM-BDIIVIDE = 1 
                       move TIP-IDE-BDIIVIDE to TIP-ID-INPUT-VALNOM
                       move NOMBRE-BDIIVIDE to NOMBRE-INPUT-VALNOM
                       move NOM-VAL-BDIIVIDE to NOMBOK-INPUT-VALNOM
                       if SW-ID-BDIIVIDE = 0
                           if NUM-BLOQUES-BDIIVIDE numeric and 
                               NUM-BLOQUES-BDIIVIDE <> zeros
                               move NUM-BLOQUES-BDIIVIDE 
                               to BLOQUES-INPUT-VALNOM
                           else
                               if TIP-ID-INPUT-VALNOM = 1
                                   move 2 to BLOQUES-INPUT-VALNOM
                               else
                                   move 1 to BLOQUES-INPUT-VALNOM
                               end-if
                           end-if
                           perform 10000-MATCH-NOMBRES-VALNOM
                           if MATCH-OK-OUT-VALNOM = 0
                               move 1 to SW-NOMBRES-BDIIVIDE
                           end-if
                       end-if
                   end-if
               else if SW-TID-BDIIVIDE = 1                              FAC0002 
                   move 1 to SW-ID-BDIIVIDE                             FAC0002 
               end-if                                                   FAC0002 
               end-if                                                   FAC0002 
           else
               move 1 to SW-ID-BDIIVIDE
           end-if.



       80200-VALIDAR-ID.
      *===================================*
      *80200-VALIDAR-ID.
      *Validacion del numero de identificacion
      *con las BASES DE DATOS DE LA REGISTRADURIA, DAS, DIAN:
      *BDIAREGI, BDIADAS, BDIADIAN respectivamente
      *Dependiendo del tipo de identificacion
      *===================================*
           evaluate TIP-IDE-BDIIVIDE        
           when 1                                                   
               perform 85010-UBICA-CEDULA
           when 2                                                   
               perform 85020-UBICA-NIT                             
           when 3                                                 
               perform 85020-UBICA-NIT                           
           when 4                                               
               perform 85030-UBICA-EXT                         
           when other                                         
               move 1 to SW-ID-BDIIVIDE
           end-evaluate.          



       80300-ASIGNAR-TIPO-ID.                                           FAC0002 
      *================================================================
      *80300-ASIGNAR-TIPO-ID.                                           FAC0002 
      *ASIGNACION del TIPO DE IDENTIFICACION. PARAMETRO DE ENTRADA      FAC0002 
      *SW-ASGTIPID-BDIVIIDE = 1; RETORNA TIPID-VAL-BDIIVIDE
      *================================================================
           INITIALIZE PARAMETROS-SALIDA-BDIIVIDE TIP-IDE-BDIIVIDE       FAC0002 
           inspect NUM-IDE-BDIIVIDE replacing all " " by "0"            FAC0002 
           if (NUM-IDE-BDIIVIDE-R >= 1 and                              FAC0002 
               NUM-IDE-BDIIVIDE-R  <= 99999999) or                      FAC0002 
               (NUM-IDE-BDIIVIDE-R >= 1000000000 and                    FAC0002 
               NUM-IDE-BDIIVIDE-R  <  8000000000)                       FAC0003 
               move 1 to TIP-IDE-BDIIVIDE                               FAC0002 
           else                                                         FAC0002 
               if (NUM-IDE-BDIIVIDE-R >= 800000000 and                  FAC0002 
                   NUM-IDE-BDIIVIDE-R  <= 999999999) or                 FAC0005 
                   (NUM-IDE-BDIIVIDE-R >= 8000000000 and                FAC0003 
                   NUM-IDE-BDIIVIDE-R  <= 9999999999)                   FAC0005 
                   move 2 to TIP-IDE-BDIIVIDE                           FAC0002 
      *          if NUM-IDE-BDIIVIDE-R >= 8000000000                    FAC0004 
      *             move NUM-IDE-BDIIVIDE-R to NUM-ID-W11-BDIIVIDE      FAC0004 
      *             move NUM-ID-W10-BDIIVIDE to NUM-IDE-BDIIVIDE-R      FAC0004 
      *          end-if                                                 FAC0004 
               else                                                     FAC0002 
                   if (NUM-IDE-BDIIVIDE-R >= 600000000 and              FAC0002 
                       NUM-IDE-BDIIVIDE-R  <  800000000) or             FAC0005 
                       (NUM-IDE-BDIIVIDE-R >= 6000000000 and            FAC0002 
                       NUM-IDE-BDIIVIDE-R  <  8000000000)               FAC0005 
                       move 3 to TIP-IDE-BDIIVIDE                       FAC0002 
                   else move 4 to TIP-IDE-BDIIVIDE                      FAC0002 
                   end-if.                                              FAC0002 



       80400-VALIDAR-TIPO-ID.                                           FAC0002 
      *================================================================
      *80400-VALIDAR-TIPO-ID.                                           FAC0002 
      *VALIDACION del TIPO DE IDENTIFICACION DE ACUERDO AL RANGO        FAC0002 
      *CONTENIDO EN EL NUMERO DE IDENTIFICACION. SE PREGUNTA AL REVES.  FAC0002 
      *PARAMETRO DE ENTRADA SW-VALTIPID-BDIVIIDE = 1;                   FAC0002 
      *RETORNA SW-TID-BDIIVIDE = 1 SI ESTA ERRADO.                      FAC0002 
      *================================================================
           INITIALIZE PARAMETROS-SALIDA-BDIIVIDE                        FAC0002 
           inspect NUM-IDE-BDIIVIDE replacing all " " by "0"            FAC0002 
           evaluate TIP-IDE-BDIIVIDE                                    FAC0002 
               when 1                                                   FAC0002 
                   if NUM-IDE-BDIIVIDE-R > 9999999999                   FAC0002 
                       move 1 to SW-TID-BDIIVIDE                        FAC0002 
                   end-if                                               FAC0002 
               when 2                                                   FAC0002 
                   if NUM-IDE-BDIIVIDE-R >  9999999999 or               FAC006C 
                   (NUM-IDE-BDIIVIDE-R <= 800000000 and >= 600000000)   FAC0005 
                       move 1 to SW-TID-BDIIVIDE                        FAC0002 
                   end-if                                               FAC0002 
               when 3                                                   FAC0002 
                   if NUM-IDE-BDIIVIDE-R > 800000000 or < 600000000     FAC0005 
                       move 1 to SW-TID-BDIIVIDE                        FAC0002 
                   end-if                                               FAC0002 
                   end-evaluate.                                        FAC0002 



       80500-ELIMINAR-DIGITO-CHEQUEO.                                   FAC0004 
      *================================================================
      *80500-ELIMINAR-DIGITO-CHEQUEO.                                   FAC0004 
      *Elimina el digito de chequeo y cambia el tipo de identificacion
      *de acuerdo a los rangos para tipo id = 2 y 3
      *================================================================
           if (NUM-IDE-BDIIVIDE-R >= 600000000 and                      FAC0004 
               NUM-IDE-BDIIVIDE-R  <  800000000) or                     FAC0005 
               (NUM-IDE-BDIIVIDE-R >= 6000000000 and                    FAC0004 
               NUM-IDE-BDIIVIDE-R  < 8000000000)                        FAC0005 
               move 3 to TIP-IDE-BDIIVIDE                               FAC0004 
           end-if                                                       FAC0004
           if (NUM-IDE-BDIIVIDE-R >= 800000000 and                      FAC0004 
               NUM-IDE-BDIIVIDE-R  <= 999999999) or                     FAC0005 
               (NUM-IDE-BDIIVIDE-R >= 8000000000 and                    FAC0004 
               NUM-IDE-BDIIVIDE-R  <= 9999999999)                       FAC0005 
               move 2 to TIP-IDE-BDIIVIDE                               FAC0004 
           end-if                                                       FAC0004
           if (TIP-IDE-BDIIVIDE = 2 and                                 FAC0004
               (NUM-IDE-BDIIVIDE-R >= 10000000000) or                   FAC0004
               (NUM-IDE-BDIIVIDE-R >= 8000000000 and                    FAC0004 
               NUM-IDE-BDIIVIDE-R <= 9999999999)) or                    FAC0005 
               (TIP-IDE-BDIIVIDE = 3 and                                FAC0004
               (NUM-IDE-BDIIVIDE-R >= 6000000000 and                    FAC0004 
               NUM-IDE-BDIIVIDE-R <  8000000000))                       FAC0005 
               move NUM-IDE-BDIIVIDE to NUM-ID-W11-BDIIVIDE             FAC0004
               move NUM-ID-W10-BDIIVIDE to NUM-IDE-BDIIVIDE-R           FAC0004 
           end-if                                                       FAC0004
           inspect NUM-IDE-BDIIVIDE replacing all " " by "0".           FAC0004


       85010-UBICA-CEDULA.
      *================================================================
      *85010-UBICA-CEDULA.
      *Ubica la identificacion dada la llave en BDIIREGI
      *Arma el nombre completo en una sola variable
      *================================================================
           move NUM-IDE-BDIIVIDE to CEDULA-BDIIREGI
           move TIP-IDE-BDIIVIDE to TIPO-ID-BDIIREGI
           read BDIIREGI invalid key
               move 1 to SW-ID-BDIIVIDE
           end-read
               if SW-ID-BDIIVIDE = 0
                   move APELLIDO1-BDIIREGI to PRIMER-APELLIDO-BDIIVIDE
                   move APELLIDO2-BDIIREGI to SEGUNDO-APELLIDO-BDIIVIDE
                   move DES-NOMBRE2-BDIIREGI to SEGUNDO-NOMBRE-BDIIVIDE
                   move DES-NOMBRE1-BDIIREGI to PRIMER-NOMBRE-BDIIVIDE
                   if ESTADO-CIVIL-BDIIREGI = 0                         
                       string  
                       SEGUNDO-APELLIDO-BDIIVIDE delimited by "  ",   
                       "*" delimited by size ,                          
                       PRIMER-NOMBRE-BDIIVIDE    delimited by "  ",  
                       "*" delimited by size ,                         
                       SEGUNDO-NOMBRE-BDIIVIDE   delimited by size, 
                       ";" delimited by size ,                    
                       into NOM-LEIDO-BDIIVIDE        
                   end-if
                   if ESTADO-CIVIL-BDIIREGI = 1                        
                       string 
                       "DE" delimited by size,                        
                       "*"  delimited by size,                         
                       SEGUNDO-APELLIDO-BDIIVIDE delimited by "  ", 
                       "*"  delimited by size,
                       PRIMER-NOMBRE-BDIIVIDE    delimited by "  ",  
                       "*"  delimited by size,                        
                       SEGUNDO-NOMBRE-BDIIVIDE   delimited by size,   
                       ";"  delimited by size,                    
                       into NOM-LEIDO-BDIIVIDE                       
                   end-if
                   if ESTADO-CIVIL-BDIIREGI = 2 
                       string
                       "VDA DE" delimited by size,                          
                       "*"      delimited by size,                          
                       SEGUNDO-APELLIDO-BDIIVIDE delimited by "  ", 
                       "*"      delimited by size,                          
                       PRIMER-NOMBRE-BDIIVIDE    delimited by "  ",  
                       "*"      delimited by size,                          
                       SEGUNDO-NOMBRE-BDIIVIDE delimited by size,
                       ";"      delimited by size,             
                       into NOM-LEIDO-BDIIVIDE         
                   end-if
                   string PRIMER-APELLIDO-BDIIVIDE delimited by "  ",
                   "*"             delimited by size,                    
                   NOM-LEIDO-BDIIVIDE     delimited by size, "*",
                   delimited by size,
                   into NOM-VAL-BDIIVIDE

                   inspect NOM-VAL-BDIIVIDE replacing all "*" by " "
                   inspect NOM-VAL-BDIIVIDE replacing all ";" by " "
                   initialize NOM-LEIDO-BDIIVIDE
                   move NOM-VAL-BDIIVIDE to NOM-LEIDO-BDIIVIDE
               end-if.  *> was missing

       85020-UBICA-NIT.                                                 
      *================================================================
      *85020-UBICA-NIT.
      *Ubica NIT en archivo de la DIAN, BDIIDIAN
      *================================================================
           move NUM-IDE-BDIIVIDE to NITS-R-BDIIDIAN
           read BDIIDIAN invalid key
               move 1 to SW-ID-BDIIVIDE
           end-read                                                 
           if SW-ID-BDIIVIDE = 0
               move DESCRIPCION-BDIIDIAN to NOM-LEIDO-BDIIVIDE  
           end-if.                                                  


       85030-UBICA-EXT.                                                 
      *================================================================
      *85030-UBICA-EXT.
      *Ubica cedula de extranjeria en archivo del DAS
      *================================================================
           move NUM-IDE-BDIIVIDE to NUM-ID-BDIIDAS
           move TIP-IDE-BDIIVIDE to TIPO-ID-BDIIDAS
           read BDIIDAS invalid key
               move 1 to SW-ID-BDIIVIDE
           end-read                                                     
           if SW-ID-BDIIVIDE = 0 
               move NOMBRE-BDIIDAS to NOM-LEIDO-BDIIVIDE
           end-if.



                   copy VALNOM.PROC. 
      *VALNOM-PROC.v.0005
      ******************************************************************
      * RUTINA CREADA PARA HACER LA VALIDACION DE NOMBRES CONTRA LOS 
      * ARCHIVOS DE LA REGISTRADURIA, DIAN Y DAS, DEPENDIENDO del TIPO
      * DE IDENTIFICACION RECIBIDA.    
      *    PARAMETROS                      
      *        BLOQUES-INPUT-VALNOM - NUMERO DE BLOQUES A VALIDAR
      *        NOMBRE-INPUT-VALNOM  - NOMBRE A VALIDAR
      *        NOMBOK-INPUT-VALNOM  - NOMBRE CORRECTO CON EL QUE SE 
      *                               DEBE COMPARAR NOMBRE-INPUT-VALNOM
      *        TIP-ID-INPUT-VALNOM  - TIPO DE IDENTIFICACION A VALIDAR
      *    SALIDAS:
      *        MATCH-OK-OUT-VALNOM  - 0-NO HUBO       1-HUBO      
      *******************************************************************
      * RMB0002   * VAL001: SE MEJORA LA VALIDACION DE NOMBRES EN PERSONAS
      * R.MARTINEZ* JURIDICAS                                             
      * DIC.2002  * 
      *******************************************************************
      * GJC0003    * VALMEJ :SE CREAN ANUEVOS CAMPOS PARA MEJORAR LA VALIDACION
      * G.JIMENEZ  * DE NIT. PARA MEJORA TAMBIEN SE AMPLIA TAMANO DE VARIABLES
      * MARZO/2003 * PARA CEDULAS, EL COMODIN QUE USUALMENTE CORRESPONDE A LA N
      *            * SOLO SE COMPARA CONTRA N O CONTRA BLANCOS
      *************************************************************************
      * GJC0004    * VALME1 :CON EL FIN DE AFINAR AUN MAS EL PROCESO DE VALIDA- 
      * R.MARTINEZ * CION DE NOMBRES, EN EL MATCH A PROXIMADO DE PERSONAS NATU- 
      * G.JIMENEZ  * RALES SE CAMBIA EL CALCULO FINAL PARA DETERMINAR SI UN BLO-
      * MARZO/2003 * QUE ES O NO VALIDO. PARA QUE UN NOMBRE QUE VENGA NUMERICO 
      *            * NO SE CONVIERTA EN COMODIN, EN LAS SUSTITUCIONES POR *
      *            * SE VALIDA QUE EL CONTENIDO NO SEA NUMERICO.
      *************************************************************************
      * GJC0005    * VLNM01: LOS PROGRAMAS ABORTAN CUANDO ALGUNO DE LOS       
      * G.JIMENEZ  *         PARAMETROS DE UN SUBSTRING QUEDA EN 0 O EN         
      * NOV/2005   *         UN NUMERO MENOR QUE 0. EJ: NOMBRE(I:J) DONDE I O  
      *            *         J ES 0.                                        
      *************************************************************************


       10000-MATCH-NOMBRES-VALNOM.                 
      ******************************************************************
      *10000-MATCH-NOMBRE-VALNOM.                   
      * SE BUSCA DETERMINAR SI EL NOMBRE DE LA PERSONA CONSULTADA HACE 
      * CON EL NOMBRE ENVIADO EN LA CONSULTA. 
      ******************************************************************
           move 0 to MATCH-OK-OUT-VALNOM.
           move NOMBRE-INPUT-VALNOM to NOMBRE-AUX-VALNOM.
           if TIP-ID-INPUT-VALNOM  = 2 
               inspect NOMBRE-AUX-VALNOM replacing all
               " S.A. " by "      ",                                    RMB0002 
               "." by " ", " LTDA " by "      ",                        RMB0002 
               " LIMITADA " by "          ",                            RMB0002 
               " CIA " by "     ", " COMPA/IA " by "          ",        RMB0002 
               " INVERSIONES " by "             ",                      RMB0002 
               " EMPRESA " by "         ",                              RMB0002 
               " CORPORACION " by "             ",                      RMB0002 
               " DE " by "    ", " LA " by "    ", " del " by "     ",  RMB0002 
               " S " by "   ", " A " by "   "                           RMB0002 
               " Y " by "   "                                           GJC0003 
               inspect NOMBRE-AUX-VALNOM replacing all                  RMB0002 
               " S.A. " by "      ",                                    RMB0002 
               "." by " ", " LTDA " by "      ",                        RMB0002 
               " LIMITADA " by "          ",                            RMB0002 
               " CIA " by "     ", " COMPA/IA " by "          ",        RMB0002 
               " INVERSIONES " by "             ",                      RMB0002 
               " EMPRESA " by "         ",                              RMB0002 
               " CORPORACION " by "             ",                      RMB0002 
               " DE " by "    ", " LA " by "    ", " del " by "     ",  RMB0002 
               " S " by "   ", " A " by "   "                           RMB0002 
               " Y " by "   "                                           GJC0003 
               inspect NOMBRE-AUX-VALNOM replacing all                  GJC0003 
               "." by " ","-" by " ",                                   GJC0003 
               "C" by "S","Z" by "S","J" by "G","V" by "B",             GJC0003 
               "Y" by "G","H" by "G","I" by "G"                         GJC0003 
               inspect NOMBRE-AUX-VALNOM replacing all                  GJC0003 
               "." by " ","-" by " ",                                   GJC0003 
               "C" by "S","Z" by "S","J" by "G","V" by "B",             GJC0003 
               "Y" by "G","H" by "G","I" by "G"                         GJC0003 
               move NOMBRE-AUX-VALNOM to NOMBRE-INPUT-VALNOM
           else   
               inspect CADENA-VALNOM    replacing all 
               "." by " ","-" by " ", "," by " ",
               "C" by "S","Z" by "S","J" by "G","V" by "B",
               "Y" by "G","H" by "G","I" by "G",
               " DE " by "    "," LA " by "    ",
               " del " by "     " 
               inspect CADENA-VALNOM    replacing all                   RMB0002 
               "." by " ","-" by " ", "," by " ", 
               "C" by "S","Z" by "S","J" by "G","V" by "B",             RMB0002 
               "Y" by "G","H" by "G","I" by "G",                        RMB0002 
               " DE " by "    "," LA " by "    ",                       RMB0002 
               " del " by "     "                                       RMB0002 
               perform varying I-VALNOM from 1 by 1 
                   until I-VALNOM >= 66
                   if VNOM-AUX-VALNOM(I-VALNOM) not alphabetic 
                   and VNOM-AUX-VALNOM(I-VALNOM) not numeric
                       move "*" to VNOM-AUX-VALNOM(I-VALNOM)
                   end-if
                   end-perform
           end-if.
           move spaces to VECTOR-INPUT-VALNOM.
           unstring CADENA-VALNOM delimited by all " " into 
           BASURA-VALNOM
           VM-INPUT-VALNOM(1) VM-INPUT-VALNOM(2) VM-INPUT-VALNOM(3)
           VM-INPUT-VALNOM(4) VM-INPUT-VALNOM(5) VM-INPUT-VALNOM(6)
           VM-INPUT-VALNOM(7) VM-INPUT-VALNOM(8) VM-INPUT-VALNOM(9)
           VM-INPUT-VALNOM(10) VM-INPUT-VALNOM(11) VM-INPUT-VALNOM(12)
           VM-INPUT-VALNOM(13) VM-INPUT-VALNOM(14) VM-INPUT-VALNOM(15) 
           VM-INPUT-VALNOM(16) VM-INPUT-VALNOM(17) VM-INPUT-VALNOM(18)
           VM-INPUT-VALNOM(19) VM-INPUT-VALNOM(20) 
      ******************PREPARA EL NOMBRE CORRECTO del ARCHIVO *********
           move NOMBOK-INPUT-VALNOM to NOMBRE-AUX-VALNOM.
           if TIP-ID-INPUT-VALNOM  not = 2  
               inspect NOMBRE-AUX-VALNOM replacing all 
               "." by " ","-" by " ","," by " ",
               "C" by "S","Z" by "S","J" by "G","V" by "B",
               "Y" by "G","H" by "G","I" by "G"
               perform varying I-VALNOM from 1 by 1 
                   until I-VALNOM >= 66
                   if VNOM-AUX-VALNOM(I-VALNOM) not alphabetic 
                       and VNOM-AUX-VALNOM(I-VALNOM) not numeric
                       move "*" to VNOM-AUX-VALNOM(I-VALNOM)
                   end-if
                   end-perform
           else                            
               inspect NOMBRE-AUX-VALNOM replacing all                  RMB0002 
               " S.A. " by "      ",                                    RMB0002 
               "." by " ", " LTDA " by "      ",                        RMB0002 
               " LIMITADA " by "          ",                            RMB0002 
               " CIA " by "     ", " COMPA/IA " by "          ",        RMB0002 
               " INVERSIONES " by "             ",                      RMB0002 
               " EMPRESA " by "         ",                              RMB0002 
               " CORPORACION " by "             ",                      RMB0002 
               " DE " by "    ", " LA " by "    ", " del " by "     ",  RMB0002 
               " S " by "   ", " A " by "   "                           RMB0002 
               " Y " by "   "                                           GJC0003 
               inspect NOMBRE-AUX-VALNOM replacing all                  RMB0002 
               " S.A. " by "      ",                                    RMB0002 
               "." by " ", " LTDA " by "      ",                        RMB0002 
               " LIMITADA " by "          ",                            RMB0002 
               " CIA " by "     ", " COMPA/IA " by "          ",        RMB0002 
               " INVERSIONES " by "             ",                      RMB0002 
               " EMPRESA " by "         ",                              RMB0002 
               " CORPORACION " by "             ",                      RMB0002 
               " DE " by "    ", " LA " by "    ", " del " by "     ",  RMB0002 
               " S " by "   ", " A " by "   "                           RMB0002 
               " Y " by "   "                                           GJC0003 
               inspect NOMBRE-AUX-VALNOM replacing all                  GJC0003 
               "." by " ","-" by " ",                                   GJC0003 
               "C" by "S","Z" by "S","J" by "G","V" by "B",             GJC0003 
               "Y" by "G","H" by "G","I" by "G"                         GJC0003 
               move NOMBRE-AUX-VALNOM to NOMBOK-INPUT-VALNOM
           end-if.
           move spaces to VECTOR-OK-VALNOM.      
           unstring CADENA-VALNOM      delimited by all " " into 
           BASURA-VALNOM
           VM-OK-VALNOM(1) VM-OK-VALNOM(2) VM-OK-VALNOM(3)
           VM-OK-VALNOM(4) VM-OK-VALNOM(5) VM-OK-VALNOM(6)
           VM-OK-VALNOM(7) VM-OK-VALNOM(8) VM-OK-VALNOM(9)
           VM-OK-VALNOM(10) VM-OK-VALNOM(11) VM-OK-VALNOM(12)
           VM-OK-VALNOM(13) VM-OK-VALNOM(14) VM-OK-VALNOM(15) 
           VM-OK-VALNOM(16) VM-OK-VALNOM(17) VM-OK-VALNOM(18)
           VM-OK-VALNOM(19) VM-OK-VALNOM(20).
      ***********HACE LA VALIDACION BLOQUE A BLOQUE********************
           move 1 to I-VALNOM.
           move 0 to BLOQOK-AUX-VALNOM.
           perform until VM-INPUT-VALNOM(I-VALNOM) = spaces
               move 1 to J-VALNOM 
               perform until J-VALNOM > 20 
                   if VM-INPUT-VALNOM(I-VALNOM) = VM-OK-VALNOM(J-VALNOM)
                       add 1 to BLOQOK-AUX-VALNOM
                       move spaces to VM-OK-VALNOM(J-VALNOM)
                       VM-INPUT-VALNOM(I-VALNOM)
                       move 21 to J-VALNOM
                   else
                       move 0 to M-VALNOM
                       inspect VM-OK-VALNOM(J-VALNOM) tallying M-VALNOM
                       for CHARACTERS BEFORE initial "*"
                       if M-VALNOM >= 15
                           move 0 to M-VALNOM
                           inspect VM-OK-VALNOM(J-VALNOM) tallying
                           M-VALNOM for CHARACTERS BEFORE initial "N"
                       end-if
                       if M-VALNOM > 0 and < 15 and I-VALNOM < 20
                           and (14 - M-VALNOM > 0)                      GJC0005 
                           compute INI-VALNOM = M-VALNOM + 2  
                           compute LON-VALNOM = 14 - M-VALNOM 
                           move VM-OK-VALNOM(J-VALNOM) to PEDAZO-VALNOM
                           if PEDAZO-VALNOM(INI-VALNOM:LON-VALNOM) =
                               VM-INPUT-VALNOM(I-VALNOM + 1 ) and
                               PEDAZO-VALNOM(1:M-VALNOM) = 
                               VM-INPUT-VALNOM(I-VALNOM)
                               add 1 to BLOQOK-AUX-VALNOM
                               move spaces to VM-OK-VALNOM(J-VALNOM)
                               VM-INPUT-VALNOM(I-VALNOM)
                               move 21 to J-VALNOM
                               add 1 to I-VALNOM 
                               move spaces to VM-INPUT-VALNOM(I-VALNOM)
                           end-if
                       end-if 
                   end-if
                   add 1 to J-VALNOM
                   end-perform
                   add 1 to I-VALNOM
                   if I-VALNOM = 21 
                       next sentence
                   end-if
                   end-perform.
           if BLOQOK-AUX-VALNOM >= BLOQUES-INPUT-VALNOM
               move 1 to MATCH-OK-OUT-VALNOM
           else
               if TIP-ID-INPUT-VALNOM <> 2
                   perform 10100-MATCH-APROXIMADO  
               else
                   perform 10100-MATCH-APROX-EMPRESAS  
               end-if
               if BLOQOK-AUX-VALNOM >= BLOQUES-INPUT-VALNOM
                   move 1 to MATCH-OK-OUT-VALNOM
               end-if
           end-if.
      *
      *
      **********************************************************************
      *10100-MATCH-APROXIMADO.
      *     EN ESTA RUTINA SE VALIDAN LOS BLOQUES QUE NO PASARON COMPLETOS
      *     CON UNA APROXIMACION. ESTA RUTINA FUNCIONA CORRECTAMENTE CON     
      *     LOS TIPOS DE IDENTIFICACION 1, 3 Y 4.
      **********************************************************************
       10100-MATCH-APROXIMADO.
           if BLOQOK-AUX-VALNOM > 0
               move 65 to APROX-TOT-VALNOM APROX-INP-VALNOM
           else
               move 70 to APROX-TOT-VALNOM APROX-INP-VALNOM
           end-if
           move 1 to I-VALNOM.
           perform until I-VALNOM > 10                         
               move 1 to J-VALNOM 
               perform until J-VALNOM > 10 
                   move 0 to LETRAS-OK-VALNOM LETRAS-OK2-VALNOM
                   move 1 to K-VALNOM
                   move 1 to L-VALNOM
                   if VM-INPUT-VALNOM(I-VALNOM) not = spaces and
                       VM-OK-VALNOM   (J-VALNOM) not = spaces
                       perform until K-VALNOM > 15 or L-VALNOM > 15
                           if VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) = 
                           VMM-OK-VALNOM  (J-VALNOM,K-VALNOM)
                           or ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM)) = "*"GJC0003 
                           and (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =   GJC0003 
                           " " or = "N"))                               GJC0003 
                               if VMM-OK-VALNOM (J-VALNOM,K-VALNOM)
                               not = spaces
                                   add 1 to LETRAS-OK-VALNOM    
                                   add 1 to K-VALNOM
                                   add 1 to L-VALNOM
                               else
                                   move 19 to K-VALNOM
                               end-if
                           else
                               add 1 to K-VALNOM
                               if K-VALNOM <= 15 and L-VALNOM < 15
                                   if VMM-INPUT-VALNOM(I-VALNOM, 
                                   L-VALNOM + 1) =
                                       VMM-OK-VALNOM(J-VALNOM,K-VALNOM )
                                       add 1 to L-VALNOM
                                   end-if
                               end-if
                           end-if
                           end-perform
                           move 1 to K-VALNOM
                           move 1 to L-VALNOM
                           perform until K-VALNOM > 15 or L-VALNOM > 15
                               if VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) = 
                               VMM-OK-VALNOM  (J-VALNOM,K-VALNOM)
                               or ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM))
                               = "*"                                    GJC0003 
                               and (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM)
                               = " "   GJC0003 or = "N"))               GJC0003 
                                   if VMM-OK-VALNOM (J-VALNOM,K-VALNOM)
                                   not = spaces
                                       add 1 to LETRAS-OK2-VALNOM   
                                       add 1 to K-VALNOM
                                       add 1 to L-VALNOM
                                   else
                                       move 19 to K-VALNOM
                                   end-if
                               else
                                   add 1 to L-VALNOM
                                   if L-VALNOM <= 15 and K-VALNOM < 15
                                       if VMM-INPUT-VALNOM(I-VALNOM, 
                                       L-VALNOM) = VMM-OK-VALNOM
                                       (J-VALNOM,K-VALNOM + 1)
                                           add 1 to K-VALNOM
                                       end-if
                                   end-if
                               end-if
                               end-perform
                   end-if
                   if LETRAS-OK2-VALNOM > LETRAS-OK-VALNOM
                       move LETRAS-OK2-VALNOM to LETRAS-OK-VALNOM
                   end-if
                   if LETRAS-OK-VALNOM > 0 
                       move 0 to TOTLET-INP-VALNOM TOTLET-OK-VALNOM
                       inspect VM-INPUT-VALNOM(I-VALNOM) tallying
                       TOTLET-INP-VALNOM
                       for CHARACTERS BEFORE initial " "
                       inspect VM-OK-VALNOM   (J-VALNOM) tallying 
                       TOTLET-OK-VALNOM 
                       for CHARACTERS BEFORE initial " "
                       move 0 to POR-TOT-VALNOM  POR-INP-VALNOM
                       if TOTLET-OK-VALNOM >= 3               
                           compute POR-TOT-VALNOM = LETRAS-OK-VALNOM *
                           100 / TOTLET-OK-VALNOM 
                       end-if
                       if TOTLET-INP-VALNOM >= 3
                           compute POR-INP-VALNOM = LETRAS-OK-VALNOM *
                           100 / TOTLET-INP-VALNOM
                       end-if
                       if TOTLET-OK-VALNOM > TOTLET-INP-VALNOM
                           compute A-VALNOM = TOTLET-INP-VALNOM * 100 /
                           TOTLET-OK-VALNOM
                       else
                           compute A-VALNOM = TOTLET-OK-VALNOM  * 100 /
                           TOTLET-INP-VALNOM
                       end-if
                       if ((POR-TOT-VALNOM + POR-INP-VALNOM)/2 >=
                           APROX-TOT-VALNOM) and
                           A-VALNOM >= 63
      ****   display "%TOT=",POR-TOT-VALNOM, " %INP=", POR-INP-VALNOM,
      ****           "A-VALNOM=", A-VALNOM
      *****       if (POR-TOT-VALNOM >= APROX-TOT-VALNOM and
      *****           POR-INP-VALNOM >= APROX-INP-VALNOM ) or
      *****          ((POR-TOT-VALNOM >= APROX-TOT-VALNOM or 
      *****            POR-INP-VALNOM >= APROX-INP-VALNOM ) and
      *****            A-VALNOM >= 70)
                           add 1 to BLOQOK-AUX-VALNOM
                           move spaces to VM-OK-VALNOM(J-VALNOM)
                           VM-INPUT-VALNOM(I-VALNOM)
                       end-if       
                   end-if
                   add 1 to J-VALNOM
                   end-perform
                   add 1 to I-VALNOM
                   end-perform.
      *
      *
      **********************************************************************
      *10100-MATCH-APROX-EMPRESAS
      *     EN ESTA RUTINA SE VALIDAN LOS BLOQUES QUE NO PASARON COMPLETOS
      *     CON UNA APROXIMACION. ESTA RUTINA FUNCIONA CORRECTAMENTE CON     
      *     LOS TIPOS DE IDENTIFICACION 2.        
      **********************************************************************
       10100-MATCH-APROX-EMPRESAS.
           move zeros to TOTLET-INP-VALNOM
           move zeros to LETRAS-OK2-VALNOM
           move zeros to A-VALNOM                                       GJC0003 
           move 1 to K-VALNOM
           perform until K-VALNOM > 65
               move K-VALNOM to I-VALNOM
               move 1 to J-VALNOM
               move zeros to LETRAS-OK-VALNOM
               perform until J-VALNOM > 65 or I-VALNOM > 65
                   if V-NOMBRE-VALNOM(I-VALNOM) <> spaces and
                       V-NOMBRE-VALNOM(I-VALNOM) <> "."
                       if V-NOMBRE-VALNOM(I-VALNOM) =
                           V-NOMBOK-VALNOM(J-VALNOM)
                           add 1 to LETRAS-OK-VALNOM
                           add 1 to I-VALNOM
                           add 1 to J-VALNOM
                       else
                           add 1 to J-VALNOM
                       end-if
                   else
                       add 1 to I-VALNOM
                   end-if
                   end-perform
                   if LETRAS-OK-VALNOM > LETRAS-OK2-VALNOM
                       move LETRAS-OK-VALNOM to LETRAS-OK2-VALNOM
                   end-if
                   add 1 to K-VALNOM
                   end-perform.
           move 1 to I-VALNOM
           perform until I-VALNOM > 65
               if V-NOMBRE-VALNOM(I-VALNOM) <> spaces and
                   V-NOMBRE-VALNOM(I-VALNOM) <> "."
                   add 1 to TOTLET-INP-VALNOM
               end-if
               add 1 to I-VALNOM
               end-perform.
           compute A-VALNOM = LETRAS-OK2-VALNOM * 100 /
           TOTLET-INP-VALNOM.
           if A-VALNOM >= 75
               add 1 to BLOQOK-AUX-VALNOM
           end-if.
           move 0 to K-VALNOM                                           GJC0003 
           if BLOQOK-AUX-VALNOM < BLOQUES-INPUT-VALNOM                  GJC0003 
               INITIALIZE A-VALNOM VECTORES-VALNOM L-VALNOM             GJC0003 
               perform varying J-VALNOM from 1 by 1 until J-VALNOM > 65 GJC0003 
                   if V-NOMBRE-VALNOM(J-VALNOM) <> " "                  GJC0003 
                       move function ORD(V-NOMBRE-VALNOM(J-VALNOM))     GJC0003 
                       to I-VALNOM                                      GJC0003 
                       add 1 to CONT-LET-INPUT-VALNOM(I-VALNOM)         GJC0003 
           end-if                                                       GJC0003 
           if V-NOMBOK-VALNOM(J-VALNOM) <> " "                          GJC0003 
               move function ORD(V-NOMBOK-VALNOM(J-VALNOM))             GJC0003 
               to I-VALNOM                                              GJC0003 
               add 1 to CONT-LET-OK-VALNOM(I-VALNOM)                    GJC0003 
               end-if                                                   GJC0003 
               end-perform                                              GJC0003 
               move 0 to I-VALNOM                                       GJC0003 
               perform varying J-VALNOM from 1 by 1 until J-VALNOM > 256GJC0003 
                   if CONT-LET-OK-VALNOM(J-VALNOM)  > 0                 GJC0003 
                       add 1 to I-VALNOM                                GJC0003 
                       if CONT-LET-OK-VALNOM(J-VALNOM) <                GJC0003 
                           CONT-LET-INPUT-VALNOM(J-VALNOM)              GJC0003 
                           compute L-VALNOM = L-VALNOM + 100            GJC0003 
                       else                                             GJC0003 
                           compute L-VALNOM = L-VALNOM +                GJC0003 
                           (CONT-LET-INPUT-VALNOM(J-VALNOM)             GJC0003 
      *  100 /CONT-LET-OK-VALNOM(J-VALNOM))  GJC0003 
                       end-if                                           GJC0003 
                   end-if                                               GJC0003 
                   end-perform                                          GJC0003 
                   compute K-VALNOM = L-VALNOM/I-VALNOM                 GJC0003 
                   if (A-VALNOM >= 40 and K-VALNOM > 80) or             GJC0003 
                   ((A-VALNOM >= 35 and A-VALNOM < 40)
                   and K-VALNOM > 85) ORGJC0003 
                   ((A-VALNOM >= 25 and A-VALNOM < 35)
                   and K-VALNOM = 100)  GJC0003 
                       add 1 to BLOQOK-AUX-VALNOM                       GJC0003 
                   end-if                                               GJC0003 
               end-if.                                                  GJC0003 
