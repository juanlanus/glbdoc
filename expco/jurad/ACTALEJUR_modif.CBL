       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ACTALEJUR.
      ******************************************************************
      * Actualización del archivo de alertas con los datos provistos
      * por Jurad
      * Procesa el archivo JURAD. Controla cada registro: si es OK usa
      * los datos para actualizar BDIIALE, si tiene defecto lo graba en
      * el archivo ERRS.
      ******************************************************************
       AUTHOR.        DATACREDITO.
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION section.
       SOURCE-COMPUTER. microfocus.
       OBJECT-COMPUTER. microfocus.
       INPUT-OUTPUT section.
       FILE-CONTROL.

      ******************************************************************
      ******************************************************************
       select JURAD assign to P-file-name
           organization is line sequential
           file status is JURAD-fs.

      ******************************************************************
      ******************************************************************
       select ERRS assign to ERRS-file-name
           organization is line sequential
           file status is ERRS-fs.

      ******************************************************************
      ******************************************************************
           copy BDIIREGI.FS.
      * TODO: debe compilar con file status
c     $SET CONSTANT SW-STATUS-BDIIREGI"SI"
c     * BDIIREGI.v.03.0001
c     ******************************************************************
c     * ESTRUCTURA del ARCHIVO ENTREGADO POR LA REGISTRADURIA NACIONAL
c     * Equivale al Registro Civil de Argentina, contiene los ids de
c     * gente
c     ******************************************************************
c     $SET IDXFORMAT"8"
c     $SET CONSTANT SW-STATUS-BDIIREGI"NO"
c     *    select BDIIREGI
c     *        assign to disk TITLE-BDIIREGI
c     *        organization is indexed
c     *        access mode is dynamic
c     *        record key is LLAVE-BDIIREGI
c     $if SW-STATUS-BDIIREGI = "NO"
c     $display ".............COMPILANDO BDIIREGI SIN status.........."
c     $else
c     $display ".............COMPILANDO BDIIREGI CON status.........."
c     *        file status is COD-RETORNO-BDIIREGI
c     $end
c     *        lock mode is manual with lock on record.
c     $SET IDXFORMAT"0"

      ******************************************************************
      ******************************************************************
       copy BDIIDIAN.FS.
c     * BDIIDIAN.v.03.0001
c     * MAESTRO DE PERSONAS JURIDICAS NACIONALES Y EXTRANJERAS REPORTADA
c     * POR LA DIAN
c     * DIAN equivale a la AFIP de Argentina, contiene ids de empresas
c     ******************************************************************
c     $SET CONSTANT SW-STATUS-BDIIDIAN"NO"
c     *    select BDIIDIAN assign to disk TITLE-BDIIDIAN
c     *        organization is indexed
c     *        access mode  is dynamic
c     *        record key   is LLAVE-BDIIDIAN
c     $if SW-STATUS-BDIIDIAN = "NO"
c     $display ".............COMPILANDO BDIIDIAN SIN status.........."
c     $else
c     $display ".............COMPILANDO BDIIDIAN CON status.........."
c     *        file status is COD-RETORNO-BDIIDIAN
c     $end
c     *        lock mode is manual with lock on record.
 
      ******************************************************************
      ******************************************************************
       copy BDIIDAS.FS.
c     * BDIIDAS.v.03.0001
c     *=================================================================
c     * MAESTRO DE CEDULAS DE EXTRANJERIA, PROPORCIONADAS POR EL DAS
c     * Equivale a la SIDE de Argentina, provee ids de extranjeros
c     *=================================================================
c     $SET CONSTANT SW-STATUS-BDIIDAS"NO"
c     *    select BDIIDAS assign to disk TITLE-BDIIDAS
c     *        organization is indexed
c     *        access mode is dynamic
c     *        record key is LLAVE-BDIIDAS
c     $if SW-STATUS-BDIIDAS = "NO"
c     $display ".............COMPILANDO BDIIDAS SIN status.........."
c     $else
c     $display ".............COMPILANDO BDIIDAS CON status.........."
c     *        file status is COD-RETORNO-BDIIDAS
c     $end
c     *        lock mode is manual with lock on record.


      ******************************************************************
      ******************************************************************
           copy BDIIALE.FS.
c     * BDIIALE-FS.v01.0001
c     * Maestro de alertas reportadas por suscriptotres
c     * Suscriptores son bancos, financieras y otros clientes de
c     * Datacredito
c
c     $SET CONSTANT SW-STATUS-BDIIALE"NO"
c     *    select BDIIALE
c     *        assign to disk MAES-BDIIALE-TITLE
c     *        organization is indexed
c     *        access mode is dynamic
c     *        record key is LLAVE-BDIIALE
c     $if SW-STATUS-BDIIALE = "NO"
c     $display ".............COMPILANDO BDIIALE SIN status.........."
c     $else
c     $display ".............COMPILANDO BDIIALE CON status.........."
c     *        file status is EST-BDIIALE
c     $end
c     *        lock mode is manual with lock on record.

      ******************************************************************
      ******************************************************************
       DATA DIVISION.
       FILE section.

      ******************************************************************
      ******************************************************************
       FD JURAD.
       01  JURAD-reg.
      * Registros de movimientos I, M, E
      *    Tipo de Novedad {I, M, E} Inser/Modif/Elim
           03 JURAD-tipo-novedad        pic x.
      *    Tipo de identificacion Numero {1, 2, 3, 4}
           03 JURAD-id-tipo             pic x.
      *    Numero de identificacion
           03 JURAD-id-numeroX          pic x(11).
           03 JURAD-id-numero           redefines 03 JURAD-id-numeroX
                                        pic 9(11).
      *    Nombres y Apellidos
           03 JURAD-nombre              pic x(45).
      *    Numero de demandas vigentes
           03 JURAD-demandas-vigentes   pic 999.
      *    Numero de demandas terminadas
           03 JURAD-demandas-terminadas pic 999.
      *    Fecha ultima demanda vigente YYMMDD
           03 JURAD-demandas-fecha-ultima pic x(6).
      *    Numero de embargos vigentes
           03 JURAD-embargos-vigentes   pic 999.
      *    Numero de embargos terminados
           03 JURAD-embargos-terminados pic 999.
      *    Fecha ultimo embargo vigente YYMMDD
           03 JURAD-embargos-fecha-ultimo pic x(6).

       01  JURADT-reg.
      * Registro de totales de control T
      *    Tipo de Novedad = "T"
           03 JURADT-tipo-novedad       pic x.
           03                           pic x.
      *    Cantidad de registros
           03 JURADT-registros          pic 9(11).
           03                           pic x.
      *    Total numero de demandas vigentes
           03 JURADT-demandas-vigentes  pic 9(7).
           03                           pic x.
      *    Total numero de demandas terminadas
           03 JURADT-demandas-terminadas pic 9(7).
           03                           pic x.
      *    Total numero de embargos vigentes
           03 JURADT-embargos-vigentes  pic 9(7).
           03                           pic x.
      *    Total numero de embargos terminados
           03 JURADT-embargos-terminados pic 9(7).
           03                           pic x(41).

      ******************************************************************
      ******************************************************************
       FD ERRS.
       01  ERRS-reg.
      *    Tipo de Novedad {I, M, E, T} Inser/Modif/Elim/Totales
           03 ERRS-tipo-novedad         pic x.
      *    Tipo de identificacion Numero {1, 2, 3, 4}
           03 ERRS-id-tipo              pic x.
      *    Numero de identificacion
           03 ERRS-id-numero            pic x(11).
      *    Nombres y Apellidos
           03 ERRS-nombre               pic x(45).
      *    Numero de demandas vigentes
           03 ERRS-demandas-vigentes    pic 999.
      *    Numero de demandas terminadas
           03 ERRS-demandas-terminadas  pic 999.
      *    Fecha ultima demanda vigente YYMMDD
           03 ERRS-demandas-fecha-ultima pic x(6).
      *    Numero de embargos vigentes
           03 ERRS-embargos-vigentes    pic 999.
      *    Numero de embargos terminados
           03 ERRS-embargos-terminados  pic 999.
      *    Fecha ultimo embargo vigente YYMMDD
           03 ERRS-embargos-fecha-ultimo pic x(6).
      *    Causa del rechazo
           03 ERRS-causa-rechazo        pic x(100).

      ******************************************************************
      ******************************************************************
      * Contiene los ids de gente (personas) y otros datos personales
       copy BDIIREGI.FD.
c     * BDIIREGI.v.03.0006
c     ******************************************************************
c     * ESTRUCTURA del ARCHIVO ENTREGADO POR LA REGISTRADURIA NACIONAL
c     ******************************************************************
c     *FD  BDIIREGI.
c     *01  REG-BDIIREGI.
c     *    03  LLAVE-BDIIREGI          pic 9(12).
c     *    03  LLAVE-BDIIREGI-R        redefines LLAVE-BDIIREGI.
c     *        05  TIPO-ID-BDIIREGI    pic 9.
c     *        05  CEDULA-BDIIREGI     pic 9(11).
c     *    03  DES-NOMBRE1-BDIIREGI    pic X(12).
c     *    03  DES-NOMBRE2-BDIIREGI    pic X(12).
c     *    03  APELLIDO1-BDIIREGI      pic X(16).
c     *    03  APELLIDO2-BDIIREGI      pic X(16).
c     *    03  ESTADO-CIVIL-BDIIREGI   pic 9.
c     *    03  FECHA-EXPEDI-BDIIREGI.
c     *        05  SIGLO-EXP-BDIIREGI  pic 9(2).
c     *        05  FECHA-EXP-BDIIREGI  pic 9(6).
c     *    03  INF-NUMERICA-BDIIREGI   pic 9(7).
c     *    03  INF-NUMERICA-BDIIREGI-R redefines INF-NUMERICA-BDIIREGI.
c     *        05 DEP-EXP-BDIIREGI     pic 99.
c     *        05 MUNIC-EXP-BDIIREGI   pic 999.
c     *        05 ESTADO-VIG-BDIIREGI  pic 99.
c     *    03  FECHA-NACIMNTO-BDIIREGI.
c     *        05  SIGLO-NAC-BDIIREGI  pic 9(2).
c     *        05  FECHA-NAC-BDIIREGI  pic 9(6).
c     *    03  FECHA-EDICTO-BDIIREGI.
c     *        05  SIGLO-EDI-BDIIREGI  pic 9(2).
c     *        05  FECHA-EDI-BDIIREGI  pic 9(6).
c     *    03  IND-SEXO-BDIIREGI       pic X.
c     *    03  IND-ESTADO-BDIIREGI     pic 9.
c     *    03  NUM-RESOLUC-BDIIREGI    pic 9(4).
c     *    03  ANO-RESOLUC-BDIIREGI    pic 9(2).
c     *    03  BLOQUEO-TUTELA-BDIIREGI pic X(1).
c     *    03  FECHA-CREAC-BDIIREGI    pic 9(8).
c     *    03  FECHA-MODIF-BDIIREGI    pic 9(8).
c     *    03  IND-MODIF-BDIIREGI      pic X(1).
c     *    03  IND-CREACION-BDIIREGI redefines
c     *             IND-MODIF-BDIIREGI pic X(1).
c     *    03  IND-CAMBIO-BDIIREGI     pic X(1).
c     *    03  IND-CONSULADO-BDIIREGI  pic 9(1).
c     *    03  COD-CONSULADO-BDIIREGI  pic 9(3).
c     *    03  ES-IND-CAMARA-BDIIREGI  pic 9(1).
c     *    03  ES-IND-BURO-BDIIREGI    pic 9(1).
c     *    03  FILLER-BDIIREGI         pic X(27).

      ******************************************************************
      ******************************************************************
       copy BDIIDIAN.FD.
c     * BDIIDIAN.v.03.0004
c     ******************************************************************
c     * Maestro de personas juridicas nacionales y extranjeras reportada
c     * por la DIAN
c     ******************************************************************
c     *FD  BDIIDIAN.
c     *01  REG-BDIIDIAN.
c     *    03 LLAVE-BDIIDIAN.
c     *       05 NITS-BDIIDIAN            pic X(11).
c     *       05 NITS-R-BDIIDIAN redefines NITS-BDIIDIAN
c     *                                   pic 9(11).
c     *    03 DIGITO-CONTROL-BDIIDIAN     pic X(01).
c     *    03 DESCRIPCION1-BDIIDIAN       pic X(80).
c     *    03 DESCRIPCION1-BDIIDIAN-R redefines DESCRIPCION1-BDIIDIAN.
c     *       05 DESCRIPCION-BDIIDIAN     pic X(79).
c     *       05 ESTADO-BDIIDIAN          pic 9.
c     *    03 COD-CIIU-BDIIDIAN           pic 9(4).
c     *    03 FEC-INSCRIP-BDIIDIAN        pic 9(8).
c     *    03 FECHA-CREAC-BDIIDIAN        pic 9(8).
c     *    03 FECHA-MODIF-BDIIDIAN        pic 9(8).
c     *    03 IND-MODIF-BDIIDIAN          pic X(1).
c     *    03 ES-CLIENTE-DATACR-BDIIDIAN  pic X(1). *> S,N
c     *    03 VE-INDEPENDIENTES-BDIIDIAN  pic X(1). *> S,N
c     *    03 FECHA-MODIF-INDE-BDIIDIAN   pic 9(04). *> AAMM
c     *    03 FILLER-BDIIDIAN             pic X(19).
c     *    03 FILLER-BDIIDIAN             pic X(13).

      ******************************************************************
      ******************************************************************
       copy BDIIDAS.FD.
c     * BDIIDAS.v.03.0002
c     ******************************************************************
c     * Archivo maestro de cedulas de extranjeria suministrado por DAS
c     ******************************************************************
c     *FD  BDIIDAS.
c     *01  REG-BDIIDAS.
c     *    03 LLAVE-BDIIDAS.
c     *       05 TIPO-ID-BDIIDAS           pic 9.
c     *       05 NUM-ID-BDIIDAS            pic 9(11).
c     *    03 NACIO-BDIIDAS                pic X(15).
c     *    03 NOMBRE-BDIIDAS               pic X(45).
c     *    03 FECHA-CREAC-BDIIDAS          pic 9(8).
c     *    03 FECHA-MODIF-BDIIDAS          pic 9(8).
c     *    03 IND-MODIF-BDIIDAS            pic X(1).
c     *    03 FILLER-BDIIDAS               pic X(31).

      ******************************************************************
      ******************************************************************
       copy BDIIALE.FD.
c     * BDIIALE-FD.v01.0003.
c     * Maestro de alertas reportadas por suscriptores
c     *****************************************************************
c     *FD BDIIALE.
c     *01 REG-BDIIALE.
c     *   02 LLAVE-BDIIALE.
c     *      03 TIP-IDE-BDIIALE        pic 9(01).
c     *      03 NUM-IDE-BDIIALE        pic 9(11).
c     *      03 FUENTE-BDIIALE         pic 9(06).
c     *      03 COD-ALERTA-BDIIALE     pic 9(03).
c     *      03 LLASEC-BDIIALE         pic X(08).
c     *   02 ENCABEZADO-BDIIALE.
c     *      03 FEC-NOV-BDIIALE        pic 9(08).
c     *      03 FEC-VEN-BDIIALE        pic 9(08).
c     *      03 FEC-REP-BDIIALE        pic 9(08).
c     *      03 BLOQUEO-BDIIALE        pic 9(01).
c     *      03 FILLER-BDIIALE         pic X(10).
c     *      03 INF-DATAHOST-BDIIALE   pic X(18).
c     *      03 LINEAS-BDIIALE.
c     *         04 LINEA1-BDIIALE      pic X(50).
c     *         04 LINEA2-BDIIALE      pic X(50).
c     *      03 filler redefines LINEAS-BDIIALE.
c     *         04 DATOS-BDIIALE       pic X(100).

      ******************************************************************
      ******************************************************************
       FD SALIDA.  reemplazado por ERRS, hay referencias (pocas)
       01 SALIDA-reg.
          02 SALIDA-errores              pic X(50).
          02 SALIDA-filler               pic X(1).
          02 SALIDA-registro             pic X(200).

      ******************************************************************
      ******************************************************************
       WORKING-STORAGE section.

      ******************************************************************
      ******************************************************************
       copy BDIIVIDE.WS.
c     *BDIIVIDE.WS.v03.0003.
c     * Variables para rutina de validacion de identificaciones
c     ******************************************************************
c     *01  NUM-ID-W-BDIIVIDE.
c     *    03  NUM-ID-W11-BDIIVIDE           pic 9(11) value zeros.
c     *    03  NUM-ID-W11-R-BDIIVIDE redefines NUM-ID-W11-BDIIVIDE.
c     *        05 NUM-ID-W10-BDIIVIDE        pic 9(10).
c     *        05 RESTO-NUM-ID-BDIIVIDE      pic 9.
c     *01  NOMBRES-AUX-W-BDIIVIDE.
c     *    03 PRIMER-APELLIDO-BDIIVIDE       pic X(15) value spaces.
c     *    03 SEGUNDO-APELLIDO-BDIIVIDE      pic X(15) value spaces.
c     *    03 PRIMER-NOMBRE-BDIIVIDE         pic X(15) value spaces.
c     *    03 SEGUNDO-NOMBRE-BDIIVIDE        pic X(15) value spaces.
c     *    03 NOM-LEIDO-BDIIVIDE             pic X(65) value spaces.
c
c     *01 PARAMETROS-ENTRADA-BDIIVIDE        pic X(100) value spaces.
c     *01 PARAMETROS-ENTRADA-R-BDIIVIDE redefines
c     *                                     PARAMETROS-ENTRADA-BDIIVIDE.
c     *    03 TIP-IDE-BDIIVIDE               pic X.
c     *    03 NUM-IDE-BDIIVIDE               pic X(11) JUSTIFIED right.
c     *    03 NUM-IDE-BDIIVIDE-R redefines   NUM-IDE-BDIIVIDE
c     *                                      pic 9(11).
c     *    03 NOMBRE-BDIIVIDE                pic X(65).
c     *    03 SW-VERIFNOM-BDIIVIDE           pic 9.
c     *    03 NUM-BLOQUES-BDIIVIDE           pic 9.
c     *    03 SW-VALTIPID-BDIIVIDE           pic 9.
c     *    03 SW-ASGTIPID-BDIIVIDE           pic 9.
c     *    03 SW-DIGCHEQUEO-BDIIVIDE         pic 9.
c     *    03 FILLER-BDIIVIDE                pic X(18).
c
c     *01 PARAMETROS-SALIDA-BDIIVIDE.
c     *   03 NOM-VAL-BDIIVIDE                pic X(65) value spaces.
c     *   03 SW-ID-BDIIVIDE                  pic 9 value 0.
c     *   03 SW-NOMBRES-BDIIVIDE             pic 9 value 0.
c     *   03 SW-TID-BDIIVIDE                 pic 9 value 0.

      ******************************************************************
      ******************************************************************
       copy BDIIALE.WS.
c     * BDIIALE-WS.v01.0007
c     * Variables relacionadas con las alertas del sistema
c     ******************************************************************
c
c     *01 VARIABLES-BDIIALE.
c     *   02 ES-INDEPENDIENTE-BDIIALE           pic 9(01) value 0.
c     *   02 MIRO-SIES-INDEPENDIENTE-BDIIALE    pic 9(01) value 0.
c     *   02 MAES-BDIIALE-TITLE    pic X(20)
c     *                            value "$DATABASE/BDIIALE".
c     *   02 EST-BDIIALE           pic X(02) value "00".
c     *   02 FIN-BDIIALE           pic 9(01) value 0.
c     *   02 PROCESAR-BDIIALE      pic 9(01) value 0.
c     *   02 CONT-LINEAS-BDIIALE   pic 9(03) value 0.
c     *   02 ABIERTO-BDIIALE       pic 9(01) value 0.
c     *   02 TIP-IDE-AUX-BDIIALE   pic 9(01) value 0.
c     *   02 NUM-IDE-AUX-BDIIALE   pic 9(11) value 0.
c     *   02 MARCAR-ICBTCO-BDIIALE pic 9(01) value 0.
c     *   02 FECHA-AUX-BDIIALE     pic 9(08) value 0.
c     *   02 FECHA-NOV-AUX-BDIIALE pic 9(08) value 0.
c     *   02 FECHA-VEN-AUX-BDIIALE pic 9(08) value 0.
c     *   02 FECHA-REP-AUX-BDIIALE pic 9(08) value 0.
c     *   02 FUENTE-AUX-BDIIALE    pic 9(06) value 0.
c     *   02 NOMFUE1-AUX-BDIIALE   pic X(14) value spaces.
c     *   02 NOMFUE2-AUX-BDIIALE   pic X(14) value spaces.
c     *   02 CODALE-AUX-BDIIALE    pic 9(03) value 0.
c     *   02 TIPALE-AUX-BDIIALE    pic X(01) value spaces.
c     *   02 TEXTO1-AUX-BDIIALE    pic X(50) value spaces.
c     *   02 INF-DATAHOST-AUX-BDIIALE      pic X(18) value spaces.
c     *   02 PAR-CODBLOALER        pic 9(1)  value 1.
c     *   02 PAR-MSGBLOALER        pic X(60) value
c     *      " ALERTA DADA POR LA ENTIDAD".
c     *01 M-ALERTA.
c     *   03 V-ALERTA   occurs 40 TIMES pic 9(02).
c     *01 TITULOS-BDIIALE.
c     *   02 filler                pic X(30) value all "=".
c     *   02 filler    pic X(20) value "==== [ ALERTAS ] ===".
c     *   02 filler                pic X(30) value all "=".
c     *   02 filler    pic X(20) value "FUENTE           FEC".
c     *   02 filler    pic X(20) value "HA           DESCRIP".
c     *   02 filler    pic X(20) value "CION                ".
c     *   02 filler    pic X(20) value "                    ".
c     *   02 filler                pic X(80) value all "=".
c     *01 LINEA1-BDIIALE-TIT.
c     *   02 RAYA-BDIIALE          pic X(01) value " ".
c     *   02 NOMFUE-LIN1-BDIIALE   pic X(14) value spaces.
c     *   02 filler                pic X(01) value spaces.
c     *   02 FEC-NOV-LIN1-BDIIALE  pic X(11) value spaces.
c     *   02 filler                pic X(01) value spaces.
c     *   02 TEXTO-LIN1-BDIIALE    pic X(50) value spaces.
c     *   02 filler                pic X(02) value spaces.
c     *01 LINEA2-BDIIALE-TIT.
c     *   02 filler                pic X(01) value spaces.
c     *   02 NOMFUE-LIN2-BDIIALE   pic X(14) value spaces.
c     *   02 filler                pic X(01) value spaces.
c     *   02 filler                pic X(11) value spaces.
c     *   02 filler                pic X(01) value spaces.
c     *   02 TEXTO-LIN2-BDIIALE    pic X(50) value spaces.
c     *   02 filler                pic X(02) value spaces.
c     *   02 TEXTO2-AUX-BDIIALE    pic X(50) value spaces.

       copy VALNOM.WS.
c     * VALNOM-WS.v.0002
c     * Datos para las rutinas de control de nombres de VALNOM.PROC
c     ******************************************************************
c     * 01 VARIABLES-VALNOM.
c     *    03 VECTOR-INPUT-VALNOM.
c     *      04 VM-INPUT-VALNOM     occurs 20 TIMES .
c     *         05 VMM-INPUT-VALNOM occurs 15 TIMES pic X.
c     *    03 VECTOR-OK-VALNOM.
c     *      04 VM-OK-VALNOM         occurs 20 TIMES.
c     *         05 VMM-OK-VALNOM    occurs 15 TIMES pic X.
c     *    03 BLOQOK-AUX-VALNOM           pic 9(2).
c     *    03 CADENA-VALNOM.
c     *       04 filler                   pic X(2) value "A ".
c     *       04 NOMBRE-AUX-VALNOM        pic X(65).
c     *       04 filler redefines NOMBRE-AUX-VALNOM.
c     *          05 VNOM-AUX-VALNOM occurs 65 TIMES pic X.
c     *    03 BASURA-VALNOM               pic X(10).
c     *    03 BLOQUES-INPUT-VALNOM        pic 9(2).
c     *    03 NOMBRE-INPUT-VALNOM         pic X(65).
c     *    03 filler redefines NOMBRE-INPUT-VALNOM.
c     *       05 V-NOMBRE-VALNOM          pic X occurs 65 TIMES.
c     *    03 NOMBOK-INPUT-VALNOM         pic X(65).
c     *    03 filler redefines NOMBOK-INPUT-VALNOM.
c     *       05 V-NOMBOK-VALNOM          pic X occurs 65 TIMES.
c     *    03 TIP-ID-INPUT-VALNOM         pic 9(02).
c     *    03 MATCH-OK-OUT-VALNOM         pic 9(01).
c     *    03 A-VALNOM                    pic 9(05).
c     *    03 I-VALNOM                    pic 9(05).
c     *    03 J-VALNOM                    pic 9(05).
c     *    03 K-VALNOM                    pic 9(05).
c     *    03 L-VALNOM                    pic 9(05).
c     *    03 M-VALNOM                    pic 9(05).
c     *    03 INI-VALNOM                  pic 9(02).
c     *    03 LON-VALNOM                  pic 9(02).
c     *    03 LETRAS-OK-VALNOM            pic 9(02).
c     *    03 LETRAS-OK2-VALNOM           pic 9(02).
c     *    03 TOTLET-OK-VALNOM            pic 9(02).
c     *    03 TOTLET-INP-VALNOM           pic 9(02).
c     *    03 PEDAZO-VALNOM               pic X(15).
c     *    03 POR-TOT-VALNOM              pic 9(05).
c     *    03 POR-INP-VALNOM              pic 9(05).
c     *    03 APROX-TOT-VALNOM            pic 9(05).
c     *    03 APROX-INP-VALNOM            pic 9(05).
c     *    03 VECTORES-VALNOM.
c     *       04 filler                   occurs 256 TIMES.
c     *          05 CONT-LET-INPUT-VALNOM pic 9(2).
c     *          05 CONT-LET-OK-VALNOM    pic 9(2).
c     *    03 VAL-APELLIDO-VALNOM         pic 9.

       copy ICFILE.WS.
c     * ICFILE-WS.v.0040.
c     * COPY PARA DIRECCIONAR LOS ARCHIVOS.
c     *****************************************************************
c
c     *01  FILE-TITLES.
c     *    05  BDUUNI-TITLE        pic X(80) value "$BDU/BDUUNI".
c     *    05  NOMB-APEL-TITLE     pic X(80) value "$DATABASE/ICBNAP".
c     *    05  CLAVE-TITLE         pic X(80) value "$DATABASE/ICBCLA".
c     *    05  TERM-TITLE          pic X(80) value "$DATABASE/ICBTER".
c     *    05  MAES-PERSON-TITLE   pic X(80) value "$DATABASE/ICMPER".
c     *    05  MAES-CHECK-TITLE    pic X(80) value "$DATABASE/ICMCHK".
c     *    05  MAES-CREDIT-TITLE   pic X(80) value "$DATABASE/ICMCRE".
c     *    05  MAES-MICROC-TITLE   pic X(80) value "$DATABASE/ICMMCR".
c     *    05  MAES-ICMCHQ-TITLE   pic X(80) value "$DATABASE/ICMCHQ".
c     *    05  INDEXADO-TITLE      pic X(80) value "$DATABASE/ICBCON".
c     *    05  SECUENCIAL-TITLE    pic X(80) value "$DATABASE/ICBTCO".
c     *    05  SUSCRIPTOR-TITLE    pic X(80) value "$DATABASE/ICBSUS".
c     *    05  ARCHNITS-TITLE      pic X(80) value "ICDIAN".
c     *    05  ARCHIVO-CLIENTE     pic X(80) value "$DATABASE/ICMCLI".
c     *    05  MAES-ICMCLN-TITLE   pic X(80) value "$DATABASE/ICMCLN".
c     *    05  ARCHIVO-PREPAGOS    pic X(80) value "$DATABASE/ICMPRE".
c     *    05  ARCHIVO-USUARIOS    pic X(80) value "$DATABASE/ICMUSU".
c     *    05  TITLE-SCODTB        pic X(80) value "$DATABASE/SCODTB".
c     *    05  TITLE-BDIRMAE1      pic X(80) value "$DATABASE/BDIRMAE1".
c     *    05  TITLE-BDIRMAE2      pic X(80) value "$DATABASE/BDIRMAE2".
c     *    05  TITLE-BDIRMAE3      pic X(80) value "$DATABASE/BDIRMAE3".
c     *    05  TITLE-BDIRMAE4      pic X(80) value "$DATABASE/BDIRMAE4".
c     *    05  TITLE-BDIRCTR       pic X(80) value "$DATABASE/BDIRCTR".
c     *    05  TITLE-BDIMHIS       pic X(80) value "$DATABASE/BDIMHIS".
c     *    05  BDAPRVTR-TITLE      pic X(80) value "$DATABASE/BDAPRVTR".
c     *    05  TITLE-DTCABD.
c     *        07 filler           pic X(13) value "$DATOS/DTCABD".
c     *        07 ANOMESDIA-DTCABD pic 9(08) value zeros.
c     *    05  BDLCLI-TITLE        pic X(80) value "$DATABASE/BDLCLI".
c     *    05  SCOREPREC-TITLE     pic X(80) value "$DATOS/SCOREPREC".
c     *    05  ARCHIVO-LOGS        pic X(80) value
c     *        "/ic/datacred/desarrollo/logs/ICL325".
c     *    05  ARCHIVO-TEMPORAL    pic X(80) value
c     *                                "$TEMPORALES/TEMPORAL".
c     *    05  ESTAD-CADUC         pic X(80) value
c     *                                "$TEMPORALES/ESTCAD.DAT".
c     *    05  MATRIZ-CADUC        pic X(80) value
c     *                                "$DATABASE/ICTCAD".
c     *    05  ETQ-CIN             pic X(80) value
c     *                                "$TEMPORALES/ICINCO.DAT".
c     *    05  RECLACOM-TITLE      pic X(80) value "$DATABASE/ICMRCM".
c     *    05  RECLAMOS-TITLE      pic X(80) value "$DATABASE/ICMRCL".
c     *    05 MAESTRO-FACUCLI      pic X(80) value "$DATABASE/FACUCLI".
c     *    05 TITLE-FACUCLP        pic X(80) value "$DATABASE/FACUCLP".
c     *    05 TITLE-FACUAUT        pic X(80) value "$DATABASE/FACUAUT".
c     *    05 TITLE-FACULOG        pic X(80) value "$DATABASE/FACULOG".
c     *    05 TITLE-FACUPAR        pic X(80) value
c     *                                "$DATABASE/FACUPAR.DAT".
c     *    05 MAES-ICMCREH-TITLE   pic X(80) value "$DATABASE/ICMCREH".
c     *    05 MAES-ICMCHKH-TITLE   pic X(80) value "$DATABASE/ICMCHKH".
c     *    05 MAES-ICMCHQH-TITLE   pic X(80) value "$DATABASE/ICMCHQH".

c     *    05 TITLE-CIIU          pic X(80) value "$DATABASE/ICTCIIU".
c     *    05 TITLE-BDINCCB       pic X(80) value "$ICREGIS/BDINCCB".
c     *    05 TITLE-BDINREG       pic X(80) value "$ICREGIS/BDINREG".
c     *    05 TITLE-BDINDAS       pic X(80) value "$ICREGIS/BDINDAS".
c     *    05 TITLE-BDINDIAN      pic X(80) value "$ICREGIS/BDINDIAN".
c     *    05 TITLE-CONRAZ        pic X(80)
c     *                           value "$DATABASE/CONRAZ.DAT".
c     *    05 TITLE-FRALOG        pic x(80) value "$DATABASE/FRALOG".
c     *    05 TITLE-FRAINQ        pic X(80) value
c     *                           "$DATABASE/FRAINQ.DAT".
c     *    05 TITLE-FRAINQGR      pic X(80) value
c     *                           "$DATABASE/FRAINQGR.DAT".
c     *    05 TITLE-BDSCORES.
c     *       07 filler           pic X(15) value "$SCORES/BDSCORE".
c     *       07 T-FECHA-PROC     pic 9(04).
c     *    05 TITLE-ICBPRO        pic x(80) value "$DATABASE/ICBPRO".
c     *    05 TITLE-DTRLLA        pic x(80) value "$DATABASE/DTRLLA".
c     *    05 TITLE-DTCLOG        pic X(80) value "$DATABASE/DTCLOG".
c     *    05 TITLE-DTCCOM        pic X(80) value "$DATABASE/DTCCOM".
c     *    05 TITLE-DTCING        pic X(80) value "$DATABASE/DTCING".
c     *    05 TITLE-DTCFAL        pic X(80) value "$DATABASE/DTCFAL".
c     *    05 TITLE-DTCMDF        pic X(80) value "$DATABASE/DTCMDF".
c     *    05 TITLE-DTCTRA        pic X(80) value "$DATABASE/DTCTRA".
c     *    05 TITLE-BDINBYN       pic X(80) value "$ICREGIS/BDINBYN".
c     *    05 SI-BDIEMSUP         pic X(80) value "$DBSUPER/BDIEMSUP".
c     *    05 TITLE-BDIEMSUP      pic X(80) value "$DBSUPER/BDIESU".
c     *    05 SI-BDIEENTI         pic X(80) value "$DBSUPER/BDIEENTI".
c     *    05 SI-BDIEMPER         pic X(80) value "$DBSUPER/BDIEMPER".
c     *    05 SI-BDIETEMA         pic X(80) value "$DBSUPER/BDIETEMA".
c     *    05 SI-BDIEAPER         pic X(80) value "$DBSUPER/BDIEAPER".
c     *    05 SI-BDIEMENT         pic X(80) value "$DBSUPER/BDIEMENT".
c     *    05 SI-BDIEMOVI         pic X(80) value "$DBSUPER/BDIEMOVI".
c     *    05 SI-BDIEHSUP         pic X(80) value "$DBSUPER/BDIEHSUP".
c     *    05 SI-BDIERECL         pic X(80) value "$DBSUPER/BDIERECL".
c     *    05 SI-BDIEMTOT         pic X(80) value "$DBSUPER/BDIEMTOT".
c     *    05 NOMBRE-WEB-SAL.
c     *       07 NOM-DUE-WEB-SAL pic X(15) value "$DATOS/WEB-SAL.".
c     *       07 FECHA-WEB-SAL   pic 9(8).
c     *    05 NOMBRE-RAS-SAL.
c     *       07 NOM-DUE-WEB-SAL pic X(15) value "$DATOS/RAS-SAL.".
c     *       07 FECHA-RAS-SAL   pic 9(8).
c     *    05 NOMBRE-CLAVE-WEB.
c     *       07 NOMBRE-WEB-W    pic X(80) value "$DATABASE/ARCH-WEB".
c     *    05 NOMBRE-RAS-LOG.
c     *       07 NOM-RAS-LOG     pic X(19) value
c     *                          "$DATOS/WEB-RAS.LOG.".
c     *       07 FECHA-RAS-LOG   pic 9(8).
c     *    05 TITLE-BDIIDIAN     pic X(80)  value "$DATABASE/BDIIDIAN".
c     *    05 TITLE-BDIIREGI     pic X(80)  value "$ICREGIS/BDIIRE".
c     *    05 TITLE-BDIIDAS      pic X(80)  value "$DATABASE/BDIIDAS".
c     *    05 TITLE-DTCNHAB      pic X(80)  value "$DATABASE/DTCNHAB".
c     *    05 TITLE-ICDACT       pic X(80)  value
c     *                          "$TEMPORALES/ICDACT.DAT".
c     *    05 NOMBRE-ACTALOG     pic X(80)  value
c     *                          "$TRABAJO/logs/ACTALOG".
c     *    05 NOMBRE-VALVEST     pic X(80)  value
c     *                          "$TRABAJO/logs/VALVEST.DAT".
c     *    05 NOMBRE-CALIDLOG.
c     *       07 NOM-CALIDLOG    pic X(23)  value
c     *                          "$TRABAJO/logs/CALIDLOG-".
c     *       07 FCH-CALIDLOG    pic 9(8).
c     *    05 NOMBRE-CALIDDRP.
c     *       07 NOM-CALIDDRP    pic X(24) value
c     *                          "$TEMPORALES/CALIDAD-DRP.".
c     *       07 FCH-CALIDDRP    pic 9(8).
c     *    05  MAES-ICMVLR-TITLE.
c     *        07 MAES-DIRECTORY1       pic X(16) value
c     *                                 "$VALORES/ICMVLR-".
c     *        07 MAES-FILE1            pic X(63).
c
c     *    05  MAES-ICMVLR-TITLE2.
c     *        07 MAES-DIRECTORY2       pic X(16) value
c     *                                 "$VALORES/ICMVLR-".
c     *        07 MAES-FILE2            pic X(63).
c
c     *    05  MAES-ICMVLR-TITLE3.
c     *        07 MAES-DIRECTORY3       pic X(16) value
c     *                                 "$VALORES/ICMVLR-".
c     *        07 MAES-FILE3            pic X(63).
c
c     *    05  MAES-ICMVLR-TITLE4.
c     *        07 MAES-DIRECTORY4       pic X(16) value
c     *                                 "$VALORES/ICMVLR-".
c     *        07 MAES-FILE4            pic X(63).
c
c     *    05  MAES-ICMVLR-TITLE5.
c     *        07 MAES-DIRECTORY5       pic X(16) value
c     *                                 "$VALORES/ICMVLR-".
c     *        07 MAES-FILE5            pic X(63).
c
c     *    05  MAES-ICMVLR-TITLE6.
c     *        07 MAES-DIRECTORY6       pic X(16) value
c     *                                 "$VALORES/ICMVLR-".
c     *        07 MAES-FILE6            pic X(63).
c     *    05 TITLE-BDLDIRACE  pic X(80) value "$DATABASE/BDLDIRACE".
c     *    05 TITLE-BDLMAIEXC  pic X(80) value "$DATABASE/BDLMAIEXC".
c     *    05 TITLE-BDLTELEXC  pic X(80) value "$DATABASE/BDLTELEXC".
c     *    05 TITLE-BDANOM     pic X(80) value "$DATABASE/BDANOM".
c     *    05 TITLE-FRZMYR     pic X(80) value "$DATABASE/FRZMYR".
c     *    05 TITLE-DVPDANE    pic X(80) value "$DATABASE/DVPDANE.DAT".
c     *    05 TITLE-CARACT-IND pic X(80) value
c     *                                  "$DATABASE/CARACTUSA-INDCL".
c     *    05 FRABDU-TITLE     pic X(25) value
c     *                                "$VALORES/frabduact/FRABDU".
c     *    05 FRABDV-TITLE     pic X(29) value
c     *                                "$VALORES/frabduact/FRABDU.NEW".
c     *    05 MAES-ICMHCIIU-TITLE pic X(80) value "$DATABASE/ICMHCIIU".
c     *    05 MAES-ICMCONTTRJ-TITLE pic X(80) value
c     *                                   "$DATABASE/ICMCONTTRJ".
c     *    05 TITLE-ICBCOB     pic X(16) value "$DATABASE/ICBCOB".
c     *    05 TITLE-SCODAPJA   pic X(37) value "$DATABASE/SCODAPJA".
c     *01 TITLE-BDSCOREHIS.
c     *   02 filler          pic X(21) value "$DATABASE/BDSCOREHIS.".
c     *   02 ANO-ANT-TITLE-BDSCOREHIS  pic 9(04) value 2013.
c     *   02 filler                    pic X(01) value "-".
c     *   02 ANO-ACT-TITLE-BDSCOREHIS  pic 9(04) value 2014.


       01 INDICES.
          03 I                          pic 9(5) value 0.
          03 J                          pic 9(5) value 0.
       01 CONTADORES.
          03 CONT-LEIDOS                pic 9(10)  value 0.
          03 CONT-WRITE-OK              pic 9(10)  value 0.
          03 CONT-WRITE-ERR             pic 9(10)  value 0.
          03 CONT-REWRITE-OK            pic 9(10)  value 0.
          03 CONT-REWRITE-ERR           pic 9(10)  value 0.
          03 CONT-REGISTROS-OK          pic 9(10)  value 0.
          03 CONT-REGISTROS-ERR         pic 9(10)  value 0.
       01 LINEA1-AUX.
      * TIENE [&&&&&] DEMANDAS CERRADAS Y [&&&&&] ABIERTAS
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
          03 real                       pic 9      value 0.
          03 FIN-ENTRADA                pic 9      value 0.
          03 HAY-BDIIALE                pic 9      value 0.
          03 FECHA-PROCESO              pic 9(8)   value 0.
          03 FECHA-VENCIMIENTO          pic 9(8)   value 0.
          03 DIAS-DESDE-1600            pic 9(8)   value 0.
       01 VALIDACIONES-FECHAS.
          02 ANO-VAL-X                  pic 9(04).
             88 ANO-VAL  value 1900 thru 2050.
          02 MES-VAL-X                  pic 9(02).
             88 MES-VAL  value 01   thru 12.
          02 DIA-VAL-X                  pic 9(02).
             88 DIA-VAL  value 01   thru 31.
       01 INF-DATAHOST-AUX.
          02 NUM-DEMA-VIG-AUX           pic 9(5).
          02 NUM-DEMA-TER-AUX           pic 9(5).
          02 FEC-DEMA-VIG-AUX           pic 9(8).
       01 VARIABLES-INPUT.
          03 TIPO-IDE-INP               pic 9.
          03 NUM-IDE-INP                pic 9(11).
          03 NOMBRES-INP                pic X(30).
          03 APELLIDO-PAT-INP           pic X(30).
          03 APELLIDO-MAT-INP           pic X(30).
          03 NUM-DEMA-VIG-INP           pic 9(05).
          03 NUM-DEMA-TER-INP           pic 9(05).
          03 FEC-DEMA-VIG-INP           pic 9(08).
          03 TIPO-IDE-INP-X             pic X.
          03 NUM-IDE-INP-X              pic X(11).
          03 NUM-DEMA-VIG-INP-X         pic X(05).
          03 NUM-DEMA-TER-INP-X         pic X(05).
          03 FEC-DEMA-VIG-INP-X         pic X(08).
       01 M-ERRORES.
          03 V-ERR occurs 50 TIMES      pic 9(1).

      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      * Indica que se termino el proceso, por agotamiento del archivo
      * de entrada (normal) o alguna otra condicion (error)
       01  IND-fin-proceso              pic xx value "NO".

      * Copia del ultimo reg de input, para mensajes (Ult reg: ...)
       01  JURAD-reg-anterior           pic x(100) value "(vacio)".
      ******************************************************************
      * Parametros iniciales, de la linea de comando
      * La linea de comando que contiene los parametros
       01 P-command-line                pic x(80) value spaces.
       01 P-parametros.
      *    Proceso real: contiene "SI" para activar 
      * TODO: fijo en "NO" para pruebas
       01  P-proceso-real               pic xx value "NO".
           88 PROCESO-REAL              value "SI".

      *    Nombre del archivo de entrada JJJJJJaaaDDMMAAbbXX.txt
       01 P-file-name                   pic x(80) value spaces.
       01 P-file-name-R                 redefines P-file-name.
      *        JJJJJJ = Código de suscriptor JURAD en Experian
               05 P-cod-suscriptor      pic 9(6).
      *        aaa = Codificacion del producto ("100")
               05 P-producto            pic xxx.
      *        DDMMAA = Fecha de proceso
               05 P-fecha-proceso       pic x(6).
      *        bb =  01 a CI
               05 P-ci-bi               pic xx.
      *        XX = Tipo de proceso
      *        01 Envio Original - Carga TOTAL - primera vez
      *        02 Archivo Semanal - Carga PARCIAL
      *        03 Archivo Mensual - Carga PARCIAL
      *        04 Refresque Total - Carga TOTAL
               05 P-tipo-proceso        pic xx.
      *        extension del archivo ".txt"
               05 P-extension           pic x(4).
               05                       pic x(57).
      * Nombre del archivo de errores JJJJJJaaaDDMMAAbbXX_ERRORES.txt
          03 ERRS-file-name             pic x(80) value spaces.

      * Fecha de proceso editada YYYYMMDD
       01 P-fecha-proceso-YYYYMMDD      pic x(8).
      * Fecha de proceso + 50 años
       01 P-fecha-proceso-50.
           03  P-fecha-proceso-50YYYY   pic 9999.
           03                           pic x(4).
      ******************************************************************
      * Contadores
       01 N-contadores.
      *    Registros el el archivo de entrada
           03 N-reg-leidos              pic 9(8) value zero.
      *    Registros procesados sin error
           03 N-reg-OK                  pic 9(8) value zero.
      *    Registros con error
           03 N-reg-con-error           pic 9(8) value zero.
      *    Registros borrados previo al refresque total
           03 N-reg-borrados            pic 9(8) value zero.

      ******************************************************************
      * Armado de las lineas de alerta
       01 LA-lineas-alerta.
      *    Los modelos a continuacion deberan obtenerse de un 
      *    archivo, como BDIIALE-ALERTAS
           03 LA-modelos.
      *         12345678901234567890123456789012345678901234567890
               05 pic x(50) value
               "TIENE &&& DEMANDAS CERRADAS Y &&& ABIERTAS        ".
               05 pic x(50) value
               "LA ABIERTA MAS RECIENTE SE INSTAURO EL [&&&&&&&&]".
               05 pic x(50) value
               "TIENE &&& EMBARGOS CANCELADOS Y &&& VIGENTES      ".
               05 pic x(50) value
               "EL EMBARGO MAS RECIENTE SE INSTAURO EL [&&&&&&&&] ".
               05 pic x(50) value high-values. <* end marker
           03 LAend                     pic 999 comp value 0.
           03 LA-modelos2               redefines LA-modelos.
               05 LA-modelo             pic x(50) occurs 5 
                                        indexed LAm.
      *    Funcionamiento de los templates.
      *    Hay dos templates por de alerta, uno por linea.
      *    Cada template consiste en hasta 10 frases fijas con
      *    ampersands (&) donde se van a reemplazar los valores.
      *    Por ejemplo: 
      *    TIENE &&& DEMANDAS CERRADAS Y &&& ABIERTAS
      *    tiene 3 frases fijas y 2 reemplazos. 
      *    La cantidad de ampersands es irrelevante, pero parece
      *    conveniente que informe sobre el tamaño del contenido.
           03 LA-templates.
               05 LA-template           occurs 4 indexed LAx.
                   07 LA-parte          pic x(50)
                                        occurs 10 indexed LAy.
      *    Las dos lineas armadas
           03 LA-lineas.
               05 LA-linea1             pic x(50).
               05 LA-linea2             pic x(50).
      *    Los valores a insertar en una linea
           03 LA-valores.
               LA-valor                 pic x(50) 
                                        occurs 10 indexed LAv.
      *    Datos para editar valores a insertar
           03 LA-num                    pic -(11)0.
           03 LA-descarte               pic x(50).


      ******************************************************************
      * Errores de consistencia del input
      * Cada error tiene un código de 2 digitos, un espacio, contador
      * de ocurrencias de 7 digitos, un espacio, y descripcion de 80
       01 E-errores.
      *    indica si el registro actual tiene error
           03 E-hay-error               pic xx value "NO".
      *    contador de registros con error
           03 E-cont-regs-con-error     pic 9(8) value zero.
      *    tabla de errores
           03 pic x(11) value "01 0000000 ".
           03 pic x(59) value
           "Tipo de movimiento invalido, debe ser I M E T".
           03 pic x(11) value "02 0000000 ".
           03 pic x(59) value
           "Tipo de identificación invalido, debe ser 1 2 3 4".
           03 pic x(11) value "03 0000000 ".
           03 pic x(59) value
           "Numero de identificación invalido, debe ser numerico".
           03 pic x(11) value "04 0000000 ".
           03 pic x(59) value
           "Tipo de identificación debe ser mayor que cero".
           03 pic x(11) value "05 0000000 ".
           03 pic x(59) value
           "Falta el nombre o denominacion".
           03 pic x(11) value "06 0000000 ".
           03 pic x(59) value
           "Numero de demandas vigentes: debe ser numerico".
           03 pic x(11) value "07 0000000 ".
           03 pic x(59) value
           "Numero de demandas terminadas: debe ser numerico".
           03 pic x(11) value "08 0000000 ".
           03 pic x(59) value
           "Fecha ultima demanda vigente YYMMDD: debe ser numerica".
           03 pic x(11) value "09 0000000 ".
           03 pic x(59) value
           "Fecha ultima demanda vigente YYMMDD: fuera de rango".
           03 pic x(11) value "10 0000000 ".
           03 pic x(59) value
           "Numero de embargos vigentes: debe ser numerico".
           03 pic x(11) value "11 0000000 ".
           03 pic x(59) value
           "Numero de embargos terminados: debe ser numerico".
           03 pic x(11) value "12 0000000 ".
           03 pic x(59) value
           "Fecha ultimo embargo vigente YYMMDD: debe ser numerica".
           03 pic x(11) value "13 0000000 ".
           03 pic x(59) value
           "Fecha ultimo embargo vigente YYMMDD: fuera de rango".
           03 pic x(11) value "14 0000000 ".
           03 pic x(59) value
           "El numero de id no se encuentra en el archivo maestro".
           03 pic x(11) value "15 0000000 ".
           03 pic x(59) value
           "El nombre en el input no coincide con el registrado".
           03 pic x(11) value "16 0000000 ".
           03 pic x(59) value
           "Numero de id fuera del rango del tipo de id informado".
      *----------------- insertar nuevos items aqui -------------------
           03 pic x(11) value "00 0000000 ".
           03 pic x(59) value "--- fin de la lista (cod 00) ---".
           03                           pic x(3500). *> los 50 items
       01 E-errores-2                   redefines E-errores.
           03                           pic xx.
           03                           pic 9(8).
           03 E-error                   occurs 50 indexed Ex.
               05 E-codigo              pic xx.
               05                       pic x.
               05 E-contador            pic 9(7).
               05                       pic x.
               05 E-descripcion         pic x(59).

      ******************************************************************
      * Control de rangos de valores de las fechas del input
       01 VALIDACIONES-FECHAS.
          02 ANO-VAL-X                   pic 9(04).
             88 ANO-VAL  value 1900 thru 2050.
          02 MES-VAL-X                   pic 9(02).
             88 MES-VAL  value 01   thru 12.
          02 DIA-VAL-X                   pic 9(02).
             88 DIA-VAL  value 01   thru 31.
      ******************************************************************
      ******************************************************************
       PROCEDURE DIVISION.

      * 0000-PROGRAMA-PRINCIPAL
      * 1000-CREARDEF
      * 1000-PROCESAR-DEMANDAS
      * 2000-ABRIR-ARCHIVOS
      * 4000-PROCESAR-ENTRADA
      * 4100-VALIDACIONES-BASICAS
      * 4200-VALIDACION-OFICIALES
      * 4300-MODIFICAR-BDIIALE
      * 4310-CALCULAR-FECHAS
      * 4320-ARMAR-TEXTO-ALERTA
      * 4330-CREAR-BDIIALE
      * 4340-ACTUALIZAR-BDIIALE
      * 4400-GRABAR-REGISTROS-ERR
      * 6000-SACAR-RESUMEN
      * 80100-VALIDAR-IDENTIFICACION
      * 80200-VALIDAR-ID
      * 80300-ASIGNAR-TIPO-ID
      * 80400-VALIDAR-TIPO-ID
      * 80500-ELIMINAR-DIGITO-CHEQUEO
      * 85010-UBICA-CEDULA
      * 85020-UBICA-NIT
      * 85030-UBICA-EXT
      * 10000-MATCH-NOMBRES-VALNOM
      * 10100-MATCH-APROXIMADO
      * 10100-MATCH-APROX-EMPRESAS


       0000-PROGRAMA-PRINCIPAL.
      ******************************************************************
           perform 1000-RUTINA-INICIAL
           perform 4000-PROCESAR-REGISTRO until IND-fin-proceso = "SI"
           perform 7000-RUTINA-FINAL
           goback.

       4000-PROCESAR-REGISTRO.
      ****************************************************************
      * Proceso de un registro del archivo de entrada JURAD
      * Lectura y control del file status del archivo
      ****************************************************************
      *    lectura del proximo registro
           read JURAD
           evaluate JURAD-fs
           when "00"
      *        controla el reg y lo aplica si es OK
               move JURAD-reg to JURAD-reg-anterior
               perform 4000-PROCESAR-REGISTRO-2
           when "10"
      *        fin del archivo de input:
      *        esto es un error, no se encontro el registro "T" antes
      *        de que se terminara el archivo
               display "Falta el reg de totales del archivo JURAD "
               display "Nombre del archivo: " P-file-name
               display "File status: " JURAD-fs
               display "Ultimo registro leido: "
               display JURAD-reg-anterior
               perform 9000-CANCELAR-PROGRAMA *> no retorna
           when other
      *        error de lectura: cancelacion del proceso
               display "Error en lectura del archivo JURAD "
               display "Nombre del archivo: " P-file-name
               display "File status: " JURAD-fs
               display "Ultimo registro leido: "
               display JURAD-reg-anterior
               perform 9000-CANCELAR-PROGRAMA *> no retorna
           end-evaluate.


       4000-PROCESAR-REGISTRO-2.
      ****************************************************************
      * Controla los datos de un registro de JURAD y si es OK lo usa
      * para actualizar el archivo de alertas en linea
      * La información está leida en JURAD-reg
      ****************************************************************
           move "NO" to E-hay-error
           add 1 to N-reg-leidos

      *    validacion del tipo de movimiento
           if JURAD-tipo-novedad not = "I" and "M" and "E" and "T" 
               call "PONERR" using "01" JURAD-reg E-errores
           end-if

      *    si es el registro "T" hay que señalar el fin de proceso
           if JURAD-tipo-novedad = "T" 
               move "SI" to IND-fin-proceso
               exit paragraph
           end-if

      *    validacion formal de los datos
           perform 4100-VALIDACION-FORMAL
           if E-hay-error = "SI"
               exit paragraph
           end-if

      *    si la validación formal dio OK se hace validacion semantica
           perform 4200-VALIDACION-SEMANTICA
           if E-hay-error = "SI"
               exit paragraph
           end-if

      *    actualizacion de BDIIALE o grabacion en arch ERRS de defectos
           if E-hay-error = "SI"
               perform 4900-GRABAR-ERROR
               add 1 to N-reg-con-error
           else
               perform 4300-MODIFICAR-BDIIALE
               add 1 to N-reg-OK
           end-if.



       4100-VALIDACION-FORMAL.
      ******************************************************************
      * Validacion formal de los datos del input
      ******************************************************************
      *    Tipo de identificacion Numero {1, 2, 3, 4}
           if JURAD-id-tipo not = "1" and "2" and "3" and "4"
               call "PONERR" using "02" JURAD-reg E-errores
           end-if
      *    Numero de identificacion
           if JURAD-id-numeroX is not numeric
               call "PONERR" using "03" JURAD-reg E-errores
           end-if
           if JURAD-id-numero not > 0
               call "PONERR" using "04" JURAD-reg E-errores
           end-if
      *    si el movimiento es una eliminacion los datos subsiguientes
      *    no se controlan
           if JURAD-tipo-novedad = "E"
               exit paragraph
           end-if
      *    Nombres y Apellidos
           if JURAD-nombre = spaces
               call "PONERR" using "05" JURAD-reg E-errores
           end-if
      *    Numero de demandas vigentes
           if JURAD-demandas-vigentes is not numeric
               call "PONERR" using "06" JURAD-reg E-errores
           end-if
      *    Numero de demandas terminadas
           if JURAD-demandas-terminadas is not numeric
               call "PONERR" using "07" JURAD-reg E-errores
           end-if
      *    Fecha ultima demanda vigente YYMMDD
           if JURAD-demandas-fecha-ultima is not numeric
               call "PONERR" using "08" JURAD-reg E-errores
           end-if
           if JURAD-demandas-fecha-ultima(1:2) > "50"
               string "19" JURAD-demandas-fecha-ultima into 
               VALIDACIONES-FECHAS
           else
               string "20" JURAD-demandas-fecha-ultima into 
               VALIDACIONES-FECHAS
           end-if
           if not ANO-VAL or not MES-VAL or not DIA-VAL
               call "PONERR" using "09" JURAD-reg E-errores
           end-if
      *    Numero de embargos vigentes
           if JURAD-embargos-vigentes not-numeric
               call "PONERR" using "10" JURAD-reg E-errores
           end-if
      *    Numero de embargos terminados
           if JURAD-embargos-terminados not-numeric
               call "PONERR" using "11" JURAD-reg E-errores
           end-if
      *    Fecha ultimo embargo vigente YYMMDD
           if JURAD-embargos-fecha-ultimo is not numeric
               call "PONERR" using "12" JURAD-reg E-errores
           end-if
           if JURAD-embargos-fecha-ultimo(1:2) > "50"
               string "19" JURAD-embargos-fecha-ultimo into 
               VALIDACIONES-FECHAS
           else
               string "20" JURAD-embargos-fecha-ultimo into 
               VALIDACIONES-FECHAS
           end-if
           if not ANO-VAL or not MES-VAL or not DIA-VAL
               call "PONERR" using "13" JURAD-reg E-errores
           end-if.


       4200-VALIDACION-SEMANTICA.
      ****************************************************************
      * Validacion del numero de identificacion y de su relacion con
      * el nombre
      ****************************************************************
           move JURAD-id-tipo to TIP-IDE-BDIIVIDE
           move JURAD-id-numero to NUM-IDE-BDIIVIDE-R
           move JURAD-nombre to NOMBRE-BDIIVIDE
           move 1 to SW-VERIFNOM-BDIIVIDE
           move 0 to SW-DIGCHEQUEO-BDIIVIDE SW-ASGTIPID-BDIIVIDE
           perform 80100-VALIDAR-IDENTIFICACION
      *    a ver si la validacion cambio el tipo de ide
           if JURAD-id-tipo not = TIP-IDE-BDIIVIDE
               call "PONERR" using "16" JURAD-reg E-errores
               display "Tipo ide cambiado de " JURAD-id-tipo " a " 
               TIP-IDE-BDIIVIDE " num ide: " JURAD-id-numero
           enf-if
      *    SW-ID-BDIIVIDE: 0 si existe el id, 1 si no existe
           if SW-ID-BDIIVIDE = 1
               call "PONERR" using "14" JURAD-reg E-errores
           else
      *        existe id, a ver si el nombre coincide
               if SW-NOMBRES-BDIIVIDE = 1    *> nombre no coincide
                   call "PONERR" using "15" JURAD-reg E-errores
                   display "novedad:  " JURAD-nombre
                   display "registro: " NOM-LEIDO-BDIIVIDE
               end-if
           end-if.


       4300-MODIFICAR-BDIIALE.
      ******************************************************************
      * Se impacta el archivo BDIIALE con una insercion, modificacion o 
      * eliminacion segun el tipo de movimiento que se trate
      ******************************************************************
c     *   02 LLAVE-BDIIALE.
c     *      03 TIP-IDE-BDIIALE        pic 9(01).
c     *      03 NUM-IDE-BDIIALE        pic 9(11).
c     *      03 FUENTE-BDIIALE         pic 9(06).
c     *      03 COD-ALERTA-BDIIALE     pic 9(03).
c     *      03 LLASEC-BDIIALE         pic X(08).

      *    si hay demandas
           if JURAD-demandas-vigentes > zero
           or JURAD-demandas-terminadas > zero 
               move "J001" to LLASEC-BDIIALE
      >>>> ESTO es igual para J001 y J002 salvo INF-DATAHOST-BDIIALE
               perform 4300-LEER-BDIIALE-RANDOM
      *        procesa los tipos de movimiento I M E
               evaluate JURAD-tipo-novedad
               when "I"
      *            carga todos los datos del registro
                   initialize ENCABEZADO-BDIIALE
                   move P-fecha-proceso-YYYYMMDD to FEC-NOV-BDIIALE
                   FEC-REP-BDIIALE 
                   move P-fecha-proceso-50YYYY to FEC-VEN-BDIIALE
                   move 0 to BLOQUEO-BDIIALE
      *            TODO: controlar que haya fecha, o siempre hay?
                   string "00" JURAD-demandas-vigentes
                   "00" JURAD-demandas-terminadas
                   "20" JURAD-demandas-fecha-ultima
                   delimited size into INF-DATAHOST-BDIIALE
                   perform (insertar BDIIALE)
               when "M"
      *            reemplaza la fecha de reporte y los valores
                   move P-fecha-proceso-YYYYMMDD to FEC-REP-BDIIALE 
      *            TODO: controlar que haya fecha, o siempre hay?
                   string "00" JURAD-demandas-vigentes
                   "00" JURAD-demandas-terminadas
                   "20" JURAD-demandas-fecha-ultima
                   delimited size into INF-DATAHOST-BDIIALE
                   perform (regrabar BDIIALE)
               when "E"
      *            elimina el registro
                   perform (eliminar BDIIALE)
               when other
                   display "error de logica: tipo-novedad="
                   JURAD-tipo-novedad " reg n°: " N-reg-leidos
                   display "Registro de JURAD: " display JURAD-reg
                   perform 9000-CANCELAR-PROGRAMA
               end-evaluate

c     *   02 ENCABEZADO-BDIIALE.
c     *      03 FEC-NOV-BDIIALE         pic 9(8).
c     *      03 FEC-VEN-BDIIALE         pic 9(8).
c     *      03 FEC-REP-BDIIALE         pic 9(8).
c     *      03 BLOQUEO-BDIIALE         pic 9.
c     *      03 FILLER-BDIIALE          pic X(10).
c     *      03 INF-DATAHOST-BDIIALE    pic X(18).
c     *      03 LINEAS-BDIIALE.
c     *         04 LINEA1-BDIIALE       pic X(50).
c     *         04 LINEA2-BDIIALE       pic X(50).
c     *      03 filler redefines        LINEAS-BDIIALE.
c     *         04 DATOS-BDIIALE        pic X(100).


       4300-LEER-BDIIALE-RANDOM.
      ******************************************************************
      * Arma clave exacta y lee un registro de BDIIALE
      * Usa los datos del reg input, excepto LLASEC que esta precargado
      * Cuando pudo leer OK el file status EST-BDIIALE queda con "00"
      ******************************************************************
           move JURAD-id-tipo to TIP-IDE-BDIIALE
           move JURAD-id-numero to NUM-IDE-BDIIALE
           move P-cod-suscriptor to FUENTE-BDIIALE
           move 000 to COD-ALERTA-BDIIALE

           read BDIIALE
           evaluate EST-BDIIALE
           when "00"
               move 1 to HAY-BDIIALE
           when "23"
               move 0 to HAY-BDIIALE
           when other
               display "Error al leer BDIIALE fs: " EST-BDIIALE
               " key: " LLAVE-BDIIALE
               display "Ultimo registro procesado: "
               display JURAD-reg-anterior
               perform 9000-CANCELAR-PROGRAMA
           end-read.




      *    TODO: copiado del programa ejemplo, sin analizar
           perform 4310-CALCULAR-FECHAS
           perform 4320-ARMAR-TEXTO-ALERTA.

      *    JURAD-tipo-novedad




           if HAY-BDIIALE = 0
               perform 4330-CREAR-BDIIALE
           else
               perform 4340-ACTUALIZAR-BDIIALE
           end-if.

       4900-GRABAR-ERROR.
      ******************************************************************
      * El registro de JURAD tiene error: se graba en el archivo ERRS
      * con indicacion del error detectado
      ******************************************************************
      * TODO: implementar la grabación del archivo ERRS
           display "NO GRABA ERRS".
 

       1000-CREARDEF.
      ******************************************************************
           move function CURRENT-DATE(1:8) to FECHA-PROCESO
           move 0 to TIP-IDE-BDIIALE
           move 0 to NUM-IDE-BDIIALE
           move 000001 to FUENTE-BDIIALE
           move 001 to COD-ALERTA-BDIIALE
           move "a" to LLASEC-BDIIALE
           move 1 to HAY-BDIIALE
           read BDIIALE invalid key
               move 0 to HAY-BDIIALE
           end-read
           if HAY-BDIIALE = 0
               move "LA INDENTIFICACION NO TIENE NI HA TENIDO PROCESOS"
               to LINEA1-BDIIALE
               move "JUDICIALES REGISTRADOS" to LINEA2-BDIIALE
               move FECHA-PROCESO to FEC-NOV-BDIIALE FEC-REP-BDIIALE
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


       4310-CALCULAR-FECHAS.
      ****************************************************************
      ****************************************************************
           move function CURRENT-DATE (1:8) to FECHA-PROCESO
           move function integer-of-date (FECHA-PROCESO)
           to DIAS-DESDE-1600
           add 360 to DIAS-DESDE-1600
           move function date-of-integer (DIAS-DESDE-1600)
           to FECHA-VENCIMIENTO.


       4320-ARMAR-TEXTO-ALERTA.
      ****************************************************************
      * Hace el reemplazo de los valores adecuados en los templates
      * de las linea de alerta que corresponde
      ****************************************************************
      *    según se trate de demandas o embargos
           if JURAD-demandas-vigentes not = 0 
           or JURAD-demandas-terminadas not = 0
               set LAx to 1
               move high-values to LA-valores
               move JURAD-demandas-vigentes to LA-num
               unstring LA-num into LA-descarte LA-valor(1)
               delimited by all spaces
               move JURAD-demandas-terminadas to LA-num
               unstring LA-num into LA-descarte LA-valor(2)
               delimited by all spaces
               string LA-parte(LAx, 1) LA-valor(1)
               string LA-parte(LAx, 2) LA-valor(2)
               string LA-parte(LAx, 3) LA-valor(3)
               string LA-parte(LAx, 4) LA-valor(4)
               string LA-parte(LAx, 5) LA-valor(5)
               string LA-parte(LAx, 6) LA-valor(6)
               string LA-parte(LAx, 7) LA-valor(7)
               string LA-parte(LAx, 8) LA-valor(8)
               string LA-parte(LAx, 9) LA-valor(9)
               string LA-parte(LAx, 10) LA-valor(10)
               delimited by high-value
           end-if


           move NUM-DEMA-VIG-INP to NUM-DEMA-VIG-AUX NUM-VIG-1
           move NUM-DEMA-TER-INP to NUM-DEMA-TER-AUX NUM-TER-2
           move FEC-DEMA-VIG-INP to FEC-DEMA-VIG-AUX FEC-VIG-1.


       4330-CREAR-BDIIALE.
      ****************************************************************
      ****************************************************************
           move FECHA-PROCESO to FEC-NOV-BDIIALE
           FEC-REP-BDIIALE
           move FECHA-VENCIMIENTO to FEC-VEN-BDIIALE
           move 0 to BLOQUEO-BDIIALE
           move INF-DATAHOST-AUX to INF-DATAHOST-BDIIALE
           move spaces to FILLER-BDIIALE
           move LINEA1-AUX to LINEA1-BDIIALE
           move LINEA2-AUX to LINEA2-BDIIALE
           if PROCESO-REAL
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
           move FECHA-PROCESO to FEC-NOV-BDIIALE
           move FECHA-VENCIMIENTO to FEC-VEN-BDIIALE
           move INF-DATAHOST-AUX to INF-DATAHOST-BDIIALE
           move LINEA1-AUX to LINEA1-BDIIALE
           move LINEA2-AUX to LINEA2-BDIIALE
           if real = 1
               rewrite REG-BDIIALE invalid key
                   add 1 to CONT-REWRITE-ERR
               not invalid key
                   add 1 to CONT-REWRITE-OK
               end-rewrite
           else
               add 1 to CONT-REWRITE-OK
           end-if.



       1000-RUTINA-INICIAL.
      ******************************************************************
      * Todas las operaciones que se realizan una sola vez, al comienzo
      * del programa:
      *   - obtencion de los parametros
      *   - apertura del archivo de entrada
      *   - apertura de todos los demas archivos
      *   - eliminacion de datos existentes si es refresque completo
      ******************************************************************
      * Registra hora de inicio
           accept W-hora-inicio from time

      * El programa se anuncia en la consola
      * TODO: se usa un anuncio mas vistoso
           display "Carga de archivo JURAD"

      * Obtiene informacion sobre la corrida de la linea de comando: el
      * nombre del archivo de entrada (que contiene el tipo de proceso)
           accept P-command-line from COMMAND-LINE
           display "command line: " P-command-line
           unstring P-command-line delimited by all " " into
           P-file-name P-proceso-real
      * TODO: controlar los componentes del filename
           evaluate P-tipo-proceso
           when "01" *> 01 Envio Original - Carga TOTAL 1a vez
               perform 1000-CARGA-TOTAL
           when "02" *> 02 Archivo Semanal - Carga PARCIAL
               perform 1000-ACTUALIZACION
           when "03" *> 03 Archivo Mensual - Carga PARCIAL
               perform 1000-ACTUALIZACION
           when "04" *> 04 Refresque Total - Carga TOTAL
               perform 1000-CARGA-TOTAL
      *    valor invalido
           when other
      * TODO: hay un standard para comunicar una cancelacion de proceso?
               display "El tipo de proceso " P-tipo-proceso
               " es invalido (debe ser 01 02 03 o 04)"
               perform 9000-CANCELAR-PROGRAMA *> no retorna
           end-evaluate.

      * Edita la fecha de proceso DDMMAA del parametro en formato
      * YYYYMMDD 
           string "20" P-fecha-proceso(5:6) P-fecha-proceso(3:4)
           P-fecha-proceso(1:2) delimited size 
           into P-fecha-proceso-YYYYMMDD

      * Calcula una decha 50 años posterior a la de proceso
           move P-fecha-proceso-YYYYMMDD to P-fecha-proceso-50YYYY
           add 50 to P-fecha-proceso-50YYYY

      * Apertura del archivo de entrada
           open input JURAD
           evaluate JURAD-fs
               when "00"
                    continue
               when other
                   display "Error en open del archivo jurad "
                   display "Nombre del archivo: " P-file-name
                   display "File status: " JURAD-fs
                   perform 9000-CANCELAR-PROGRAMA *> no retorna
                   goback
           end-evaluate.

      * Apertura del archivo de errores
           string P-file-name(1:19) "_ERRORES.txt" into
           ERRS-file-name
           open output ERRS
           evaluate ERRS-fs
               when "00"
                    continue
               when other
                   display "Error en open del archivo de errores "
                   display "Nombre del archivo: " ERRS-file-name
                   display "File status: " ERRS-fs
                   perform 9000-CANCELAR-PROGRAMA *> no retorna
                   goback
           end-evaluate.

      * Apertura de los archivos maestros
      * TODO: falta capturar el status de estas operaciones?
           open input BDIIREGI
           open input BDIIDIAN
           open input BDIIDAS.

      * Apertura del archivo de alertas en linea
           if PROCESO-REAL
               open i-o BDIIALE
           else
               open input BDIIALE
           end-if.

      * Si es un refresque total (codigo 04) se eliminan los registros
      * de Jurad existentes
           if P-tipo-proceso = "04" and PROCESO-REAL
               perform 1001-ELIMINAR-TODO
           end-if.

      * Carga de los templates para armar el texto de alertas
      * TODO: por ahora los templates son literales
           set LAx to 0
           move high-values to LA-templates
           perform varying LAm frm 1 by 1 
           until LA-modelo(LAm) = high-values
               set LAx up by 1
      *        separa las partes de la linea
               unstring LA-modelo(LAm) into LA-parte(LAx, 1)
               LA-parte(LAx, 2) LA-parte(LAx, 3) LA-parte(LAx, 4)
               LA-parte(LAx, n) LA-parte(LAx, 6) LA-parte(LAx, 7)
               LA-parte(LAx, 8) LA-parte(LAx, 9) LA-parte(LAx, 10)
               delimited by all "&"
      *        prepara para omitir los espacios al final de c/parte
               perform varying LAy from 1 by 1 until LAy > 10
                   inspect PA-parte(LAx, LAy) replacing trailing
                   spaces by high-values
                   inspect PA-parte(LAx, LAy) replacing first
                   high-value by space
               end-perform
           end-perform.



       1001-ELIMINAR-TODO.
      ******************************************************************
      * Elimina los registros de alertas existentes en el archivo de
      * alertas en linea, previo a un refresque total
      ******************************************************************
           display "Eliminando toda la informacion con fuente JURAD"
      *    posiciona en el primer reg de JURAD
           move zero to TIP-IDE-BDIIALE
           move zero to NUM-IDE-BDIIALE
           move P-cod-suscriptor to FUENTE-BDIIALE
           move low-values to COD-ALERTA-BDIIALE LLASEC-BDIIALE
           start BDIIALE key > LLAVE-SEQ invalid key
      *        no hay registros
               exit paragraph

      *    lee secuencialmente y elimina los registros de JURAD
      *    hasta el fin del archivo
           perform until EST-BDIIALE not = "00"
               read BDIIALE next record
               evaluate EST-BDIIALE
               when "00"
                   if FUENTE-BDIIALE = P-cod-suscriptor
      *                es un alerta de JURAD
                       delete BDIIALE
                       if EST-BDIIALE = "00"
                           add 1 to N-reg-borrados
                       else
      *                    error en la eliminacion
                           display "Error al eliminar un registro de "
                           "BDIIALE vaciando los datos"
                           display "Clave: " LLAVE-BDIIALE
                           display "Reg borrados: " N-reg-borrados
                           perform 9000-CANCELAR-PROGRAMA
                       end-if
                   end-if
               when "10"
      *            fin de archivo: sale normalmente
                   continue
               when other
      *            lectura anormal: cancelacion del programa
                   display "Error en lectura del archivo de alertas "
                   "online"
                   display "File status: " EST-BDIIALE
                   perform 9000-CANCELAR-PROGRAMA *> no retorna
               end-evaluate
           end-perform.
           display "Termino la eliminacion total de JURAD"
           display "Reg borrados: " N-reg-borrados. 


       7000-RUTINA-FINAL.
      ******************************************************************
      * Registra hora de finalizacion
           accept W-hora-fin from time.
      *    leer JURAD a ver si hay registros despues del "T"


       9000-CANCELAR-PROGRAMA.
      ******************************************************************
      * Procedimiento standard de cancelacion de programa batch
      ******************************************************************
      * TODO: hay un standard para comunicar una cancelacion de proceso?
           display "Proceso cancelado"
           goback.




       IDENTIFICATION DIVISION.
       PROGRAM-ID. PONERR.
      ******************************************************************
      * Rutina de conveniencia para procesar un error detectado en un
      * registro de input
      * Contabiliza el error en la tabla de errores, activa el indicador
      * de registro con error, muestra el mensaje en la pantalla, cuenta
      * el registro erroneo
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Registro de input (primeros 80 caracteres)
       01  reg-input-anterior           pic x(80) value low-values.
      ******************************************************************

       LINKAGE SECTION.
      ******************************************************************
      * Codigo del error detectado
       01  cod-error                    pic xx.
      ******************************************************************
      * Registro de input (primeros 80 caracteres)
       01  reg-input                    pic x(80).
      ******************************************************************
      * Tabla de errores de consistencia del input
       01 E-errores.
      *    indica si el registro actual tiene error
           03 E-hay-error               pic xx.
      *    contador de registros con error
           03 E-cont-regs-con-error     pic 9(8).
      *    tabla de errores
           03 E-error                   occurs 50 indexed Ex.
               05 E-codigo              pic xx.
               05                       pic x.
               05 E-contador            pic 9(7).
               05                       pic x.
               05 E-descripcion         pic x(59).

      ******************************************************************

       PROCEDURE DIVISION using cod-error reg-input E-errores
       00.
      *    marca al reg actual como erroneo
           move "SI" to E-hay-error

      *    cuenta y muestra el registro de input
           if reg-input(1:80) not = reg-input-anterior
               add 1 to E-cont-regs-con-error
               display reg-input(1:80)
               move reg-input(1:80) to reg-input-anterior
           end-if

      *    contabiliza el error en la tabla de errores
           set Ex to 1
           search E-error varying Ex
           at end
               display "--- falla en la tabla de errores ---" cod-error
           when E-codigo(Ex) = "00"
               add 1 to E-contador(Ex)
               display "--- error tipo exotico: " cod-error
           when E-codigo(Ex) = cod-error
               add 1 to E-contador(Ex)
               display "Error " cod-error " " E-descripcion(Ex)
           end-search

           exit program.

       end program PONERR.

       end program ACTALEJUR.

      ******************************************************************
      * De aquí para abajo se va a eliminar, son recortes del programa
      * ejemplo (rescatar los COPYs)
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

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
c     *BDIIVIDE.PROC.v.03.0006
c     ******************************************************************
c     * Rutina de validacion de identificaciones
c     ******************************************************************
c
c     *80100-VALIDAR-IDENTIFICACION.
c     *===================================*
c     * Valida el numero de identificacion contra las bases de datos
c     * de validacion de informacion BDIIDIAN, BDIIDAS, BDIIREGI
c     * y verifica la coincidencia del nombre reportado
c     *===================================*
c     *    INITIALIZE PARAMETROS-SALIDA-BDIIVIDE VARIABLES-VALNOM
c     *    NOMBRES-AUX-W-BDIIVIDE
c     *    move zeros to NUM-IDE-BDIIVIDE
c     *    if SW-VERIFNOM-BDIIVIDE not numeric
c     *        move zeros to SW-VERIFNOM-BDIIVIDE
c     *    end-if
c     *    if SW-ASGTIPID-BDIIVIDE not numeric
c     *        move zeros to SW-ASGTIPID-BDIIVIDE
c     *    end-if
c     *    if SW-VALTIPID-BDIIVIDE not numeric
c     *        move zeros to SW-VALTIPID-BDIIVIDE
c     *    end-if
c     *    if SW-DIGCHEQUEO-BDIIVIDE not numeric
c     *        move zeros to SW-DIGCHEQUEO-BDIIVIDE
c     *    end-if
c     *    if NUM-IDE-BDIIVIDE is numeric and NUM-IDE-BDIIVIDE <> zeros
c     *        if SW-ASGTIPID-BDIIVIDE = 1
c     *            perform 80300-ASIGNAR-TIPO-ID
c     *        end-if
c     *        if SW-VALTIPID-BDIIVIDE = 1
c     *            perform 80400-VALIDAR-TIPO-ID
c     *        end-if
c     *        if SW-DIGCHEQUEO-BDIIVIDE = 1
c     *            perform 80500-ELIMINAR-DIGITO-CHEQUEO
c     *        end-if
c     *        if SW-VALTIPID-BDIIVIDE = 0 or
c     *            (SW-VALTIPID-BDIIVIDE = 1 and SW-TID-BDIIVIDE = 0)
c     *            perform 80200-VALIDAR-ID
c     *            INITIALIZE NOM-VAL-BDIIVIDE
c     *            move NOM-LEIDO-BDIIVIDE to NOM-VAL-BDIIVIDE
c     *            if SW-VERIFNOM-BDIIVIDE = 1
c     *                move TIP-IDE-BDIIVIDE to TIP-ID-INPUT-VALNOM
c     *                move NOMBRE-BDIIVIDE to NOMBRE-INPUT-VALNOM
c     *                move NOM-VAL-BDIIVIDE to NOMBOK-INPUT-VALNOM
c     *                if SW-ID-BDIIVIDE = 0
c     *                    if NUM-BLOQUES-BDIIVIDE numeric and
c     *                        NUM-BLOQUES-BDIIVIDE <> zeros
c     *                        move NUM-BLOQUES-BDIIVIDE
c     *                        to BLOQUES-INPUT-VALNOM
c     *                    else
c     *                        if TIP-ID-INPUT-VALNOM = 1
c     *                            move 2 to BLOQUES-INPUT-VALNOM
c     *                        else
c     *                            move 1 to BLOQUES-INPUT-VALNOM
c     *                        end-if
c     *                    end-if
c     *                    perform 10000-MATCH-NOMBRES-VALNOM
c     *                    if MATCH-OK-OUT-VALNOM = 0
c     *                        move 1 to SW-NOMBRES-BDIIVIDE
c     *                    end-if
c     *                end-if
c     *            end-if
c     *        else if SW-TID-BDIIVIDE = 1
c     *            move 1 to SW-ID-BDIIVIDE
c     *        end-if
c     *        end-if
c     *    else
c     *        move 1 to SW-ID-BDIIVIDE
c     *    end-if.
c
c
c
c     *80200-VALIDAR-ID.
c     *===================================*
c     * Validacion del numero de identificacion
c     * con las BASES DE DATOS DE LA REGISTRADURIA, DAS, DIAN:
c     * BDIAREGI, BDIADAS, BDIADIAN respectivamente
c     * dependiendo del tipo de identificacion
c     *===================================*
c     *    evaluate TIP-IDE-BDIIVIDE
c     *    when 1
c     *        perform 85010-UBICA-CEDULA
c     *    when 2
c     *        perform 85020-UBICA-NIT
c     *    when 3
c     *        perform 85020-UBICA-NIT
c     *    when 4
c     *        perform 85030-UBICA-EXT
c     *    when other
c     *        move 1 to SW-ID-BDIIVIDE
c     *    end-evaluate.
c
c
c
c     *80300-ASIGNAR-TIPO-ID.
c     *================================================================
c     *ASIGNACION del TIPO DE IDENTIFICACION. PARAMETRO DE ENTRADA
c     *SW-ASGTIPID-BDIVIIDE = 1; RETORNA TIPID-VAL-BDIIVIDE
c     *================================================================
c     *    INITIALIZE PARAMETROS-SALIDA-BDIIVIDE TIP-IDE-BDIIVIDE
c     *    inspect NUM-IDE-BDIIVIDE replacing all " " by "0"
c     *    if NUM-IDE-BDIIVIDE-R >= 1 and <= 99 999999
c     *    or NUM-IDE-BDIIVIDE-R >= 1000 000000 and < 8000 000000
c     *        move 1 to TIP-IDE-BDIIVIDE
c     *    else
c     *        if NUM-IDE-BDIIVIDE-R >= 800 000000 and <= 999 999999
c     *        or NUM-IDE-BDIIVIDE-R >= 8000 000000 and <= 9999 999999
c     *            move 2 to TIP-IDE-BDIIVIDE
c     *            if NUM-IDE-BDIIVIDE-R >= 8000 000000
c     *                move NUM-IDE-BDIIVIDE-R to NUM-ID-W11-BDIIVIDE
c     *                move NUM-ID-W10-BDIIVIDE to NUM-IDE-BDIIVIDE-R
c     *            end-if
c     *        else
c     *            if NUM-IDE-BDIIVIDE-R >= 600 000000 and < 800 000000
c     *            or NUM-IDE-BDIIVIDE-R >= 6000 000000 and < 8000 000000
c     *                move 3 to TIP-IDE-BDIIVIDE
c     *            else
c     *                move 4 to TIP-IDE-BDIIVIDE
c     *            end-if.
c     *********end-if.
c     *****end-if.

c
c
c     *80400-VALIDAR-TIPO-ID.
c     *================================================================
c     *80400-VALIDAR-TIPO-ID.
c     *VALIDACION del TIPO DE IDENTIFICACION DE ACUERDO AL RANGO
c     *CONTENIDO EN EL NUMERO DE IDENTIFICACION. SE PREGUNTA AL REVES.
c     *PARAMETRO DE ENTRADA SW-VALTIPID-BDIVIIDE = 1;
c     *RETORNA SW-TID-BDIIVIDE = 1 SI ESTA ERRADO.
c     *================================================================
c     *    INITIALIZE PARAMETROS-SALIDA-BDIIVIDE
c     *    inspect NUM-IDE-BDIIVIDE replacing all " " by "0"
c     *    evaluate TIP-IDE-BDIIVIDE
c     *        when 1
c     *            if NUM-IDE-BDIIVIDE-R > 9999999999
c     *                move 1 to SW-TID-BDIIVIDE
c     *            end-if
c     *        when 2
c     *            if NUM-IDE-BDIIVIDE-R >  9999999999 or
c     *            (NUM-IDE-BDIIVIDE-R <= 800000000 and >= 600000000)
c     *                move 1 to SW-TID-BDIIVIDE
c     *            end-if
c     *        when 3
c     *            if NUM-IDE-BDIIVIDE-R > 800000000 or < 600000000
c     *                move 1 to SW-TID-BDIIVIDE
c     *            end-if
c     *            end-evaluate.
c
c
c
c     *80500-ELIMINAR-DIGITO-CHEQUEO.
c     *================================================================
c     *80500-ELIMINAR-DIGITO-CHEQUEO.
c     *Elimina el digito de chequeo y cambia el tipo de identificacion
c     *de acuerdo a los rangos para tipo id = 2 y 3
c     *================================================================
c     *    if (NUM-IDE-BDIIVIDE-R >= 600000000 and
c     *        NUM-IDE-BDIIVIDE-R  <  800000000) or
c     *        (NUM-IDE-BDIIVIDE-R >= 6000000000 and
c     *        NUM-IDE-BDIIVIDE-R  < 8000000000)
c     *        move 3 to TIP-IDE-BDIIVIDE
c     *    end-if
c     *    if (NUM-IDE-BDIIVIDE-R >= 800000000 and
c     *        NUM-IDE-BDIIVIDE-R  <= 999999999) or
c     *        (NUM-IDE-BDIIVIDE-R >= 8000000000 and
c     *        NUM-IDE-BDIIVIDE-R  <= 9999999999)
c     *        move 2 to TIP-IDE-BDIIVIDE
c     *    end-if
c     *    if (TIP-IDE-BDIIVIDE = 2 and
c     *        (NUM-IDE-BDIIVIDE-R >= 10000000000) or
c     *        (NUM-IDE-BDIIVIDE-R >= 8000000000 and
c     *        NUM-IDE-BDIIVIDE-R <= 9999999999)) or
c     *        (TIP-IDE-BDIIVIDE = 3 and
c     *        (NUM-IDE-BDIIVIDE-R >= 6000000000 and
c     *        NUM-IDE-BDIIVIDE-R <  8000000000))
c     *        move NUM-IDE-BDIIVIDE to NUM-ID-W11-BDIIVIDE
c     *        move NUM-ID-W10-BDIIVIDE to NUM-IDE-BDIIVIDE-R
c     *    end-if
c     *    inspect NUM-IDE-BDIIVIDE replacing all " " by "0".
c
c
c     *85010-UBICA-CEDULA.
c     *================================================================
c     * Ubica la identificacion dada la llave en BDIIREGI
c     * Arma el nombre completo en una sola variable
c     *================================================================
c     *    move NUM-IDE-BDIIVIDE to CEDULA-BDIIREGI
c     *    move TIP-IDE-BDIIVIDE to TIPO-ID-BDIIREGI
c     *    read BDIIREGI invalid key
c     *        move 1 to SW-ID-BDIIVIDE
c     *    end-read
c     *    if SW-ID-BDIIVIDE = 0 *> la cedula existe
c     *        move APELLIDO1-BDIIREGI to PRIMER-APELLIDO-BDIIVIDE
c     *        move APELLIDO2-BDIIREGI to SEGUNDO-APELLIDO-BDIIVIDE
c     *        move DES-NOMBRE2-BDIIREGI to SEGUNDO-NOMBRE-BDIIVIDE
c     *        move DES-NOMBRE1-BDIIREGI to PRIMER-NOMBRE-BDIIVIDE
c     *        if ESTADO-CIVIL-BDIIREGI = 0
c     *            string
c     *            SEGUNDO-APELLIDO-BDIIVIDE delimited by "  ",
c     *            "*" delimited by size ,
c     *            PRIMER-NOMBRE-BDIIVIDE delimited by "  ",
c     *            "*" delimited by size ,
c     *            SEGUNDO-NOMBRE-BDIIVIDE delimited by size,
c     *            ";" delimited by size ,
c     *            into NOM-LEIDO-BDIIVIDE
c     *        end-if
c     *        if ESTADO-CIVIL-BDIIREGI = 1
c     *            string
c     *            "DE" delimited by size,
c     *            "*" delimited by size,
c     *            SEGUNDO-APELLIDO-BDIIVIDE delimited by "  ",
c     *            "*" delimited by size,
c     *            PRIMER-NOMBRE-BDIIVIDE delimited by "  ",
c     *            "*" delimited by size,
c     *            SEGUNDO-NOMBRE-BDIIVIDE delimited by size,
c     *            ";" delimited by size,
c     *            into NOM-LEIDO-BDIIVIDE
c     *        end-if
c     *        if ESTADO-CIVIL-BDIIREGI = 2
c     *            string
c     *            "VDA DE" delimited by size,
c     *            "*" delimited by size,
c     *            SEGUNDO-APELLIDO-BDIIVIDE delimited by "  ",
c     *            "*" delimited by size,
c     *            PRIMER-NOMBRE-BDIIVIDE delimited by "  ",
c     *            "*" delimited by size,
c     *            SEGUNDO-NOMBRE-BDIIVIDE delimited by size,
c     *            ";" delimited by size,
c     *            into NOM-LEIDO-BDIIVIDE
c     *        end-if
c     *        string PRIMER-APELLIDO-BDIIVIDE delimited by "  ",
c     *        "*" delimited by size,
c     *        NOM-LEIDO-BDIIVIDE delimited size, "*" delimited size,
c     *        into NOM-VAL-BDIIVIDE
c     *        inspect NOM-VAL-BDIIVIDE replacing all "*" by " "
c     *        inspect NOM-VAL-BDIIVIDE replacing all ";" by " "
c     *        initialize NOM-LEIDO-BDIIVIDE
c     *        move NOM-VAL-BDIIVIDE to NOM-LEIDO-BDIIVIDE
c     *    end-if.  *> was missing
c
c     *85020-UBICA-NIT.
c     *================================================================
c     *85020-UBICA-NIT.
c     *Ubica NIT en archivo de la DIAN, BDIIDIAN
c     *================================================================
c     *    move NUM-IDE-BDIIVIDE to NITS-R-BDIIDIAN
c     *    read BDIIDIAN invalid key
c     *        move 1 to SW-ID-BDIIVIDE
c     *    end-read
c     *    if SW-ID-BDIIVIDE = 0
c     *        move DESCRIPCION-BDIIDIAN to NOM-LEIDO-BDIIVIDE
c     *    end-if.
c
c
c     *85030-UBICA-EXT.
c     *================================================================
c     *85030-UBICA-EXT.
c     *Ubica cedula de extranjeria en archivo del DAS
c     *================================================================
c     *    move NUM-IDE-BDIIVIDE to NUM-ID-BDIIDAS
c     *    move TIP-IDE-BDIIVIDE to TIPO-ID-BDIIDAS
c     *    read BDIIDAS invalid key
c     *        move 1 to SW-ID-BDIIVIDE
c     *    end-read
c     *    if SW-ID-BDIIVIDE = 0
c     *        move NOMBRE-BDIIDAS to NOM-LEIDO-BDIIVIDE
c     *    end-if.



                   copy VALNOM.PROC.
c     *VALNOM-PROC.v.0005
c     ******************************************************************
c     * RUTINA CREADA PARA HACER LA VALIDACION DE NOMBRES CONTRA LOS
c     * ARCHIVOS DE LA REGISTRADURIA, DIAN Y DAS, DEPENDIENDO del TIPO
c     * DE IDENTIFICACION RECIBIDA.
c     *    PARAMETROS
c     *        BLOQUES-INPUT-VALNOM - NUMERO DE BLOQUES A VALIDAR
c     *        NOMBRE-INPUT-VALNOM  - NOMBRE A VALIDAR
c     *        NOMBOK-INPUT-VALNOM  - NOMBRE CORRECTO CON EL QUE SE
c     *                               DEBE COMPARAR NOMBRE-INPUT-VALNOM
c     *        TIP-ID-INPUT-VALNOM  - TIPO DE IDENTIFICACION A VALIDAR
c     *    SALIDAS:
c     *        MATCH-OK-OUT-VALNOM  - 0-NO HUBO       1-HUBO
c     ******************************************************************
c
c
c     *10000-MATCH-NOMBRES-VALNOM.
c     ******************************************************************
c     *10000-MATCH-NOMBRE-VALNOM.
c     * SE BUSCA DETERMINAR SI EL NOMBRE DE LA PERSONA CONSULTADA HACE
c     * CON EL NOMBRE ENVIADO EN LA CONSULTA.
c     ******************************************************************
c     *    move 0 to MATCH-OK-OUT-VALNOM.
c     *    move NOMBRE-INPUT-VALNOM to NOMBRE-AUX-VALNOM.
c     *    if TIP-ID-INPUT-VALNOM  = 2
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        " S.A. " by "      ",
c     *        "." by " ", " LTDA " by "      ",
c     *        " LIMITADA " by "          ",
c     *        " CIA " by "     ", " COMPA/IA " by "          ",
c     *        " INVERSIONES " by "             ",
c     *        " EMPRESA " by "         ",
c     *        " CORPORACION " by "             ",
c     *        " DE " by "    ", " LA " by "    ", " DEL " by "     ",
c     *        " S " by "   ", " A " by "   "
c     *        " Y " by "   "
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        " S.A. " by "      ",
c     *        "." by " ", " LTDA " by "      ",
c     *        " LIMITADA " by "          ",
c     *        " CIA " by "     ", " COMPA/IA " by "          ",
c     *        " INVERSIONES " by "             ",
c     *        " EMPRESA " by "         ",
c     *        " CORPORACION " by "             ",
c     *        " DE " by "    ", " LA " by "    ", " DEL " by "     ",
c     *        " S " by "   ", " A " by "   "
c     *        " Y " by "   "
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        "." by " ","-" by " ",
c     *        "C" by "S","Z" by "S","J" by "G","V" by "B",
c     *        "Y" by "G","H" by "G","I" by "G"
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        "." by " ","-" by " ",
c     *        "C" by "S","Z" by "S","J" by "G","V" by "B",
c     *        "Y" by "G","H" by "G","I" by "G"
c     *        move NOMBRE-AUX-VALNOM to NOMBRE-INPUT-VALNOM
c     *    else
c     *        inspect CADENA-VALNOM    replacing all
c     *        "." by " ","-" by " ", "," by " ",
c     *        "C" by "S","Z" by "S","J" by "G","V" by "B",
c     *        "Y" by "G","H" by "G","I" by "G",
c     *        " DE " by "    "," LA " by "    ",
c     *        " del " by "     "
c     *        inspect CADENA-VALNOM    replacing all
c     *        "." by " ","-" by " ", "," by " ",
c     *        "C" by "S","Z" by "S","J" by "G","V" by "B",
c     *        "Y" by "G","H" by "G","I" by "G",
c     *        " DE " by "    "," LA " by "    ",
c     *        " del " by "     "
c     *        perform varying I-VALNOM from 1 by 1
c     *            until I-VALNOM >= 66
c     *            if VNOM-AUX-VALNOM(I-VALNOM) not alphabetic
c     *            and VNOM-AUX-VALNOM(I-VALNOM) not numeric
c     *                move "*" to VNOM-AUX-VALNOM(I-VALNOM)
c     *            end-if
c     *            end-perform
c     *    end-if.
c     *    move spaces to VECTOR-INPUT-VALNOM.
c     *    unstring CADENA-VALNOM delimited by all " " into
c     *    BASURA-VALNOM
c     *    VM-INPUT-VALNOM(1) VM-INPUT-VALNOM(2) VM-INPUT-VALNOM(3)
c     *    VM-INPUT-VALNOM(4) VM-INPUT-VALNOM(5) VM-INPUT-VALNOM(6)
c     *    VM-INPUT-VALNOM(7) VM-INPUT-VALNOM(8) VM-INPUT-VALNOM(9)
c     *    VM-INPUT-VALNOM(10) VM-INPUT-VALNOM(11) VM-INPUT-VALNOM(12)
c     *    VM-INPUT-VALNOM(13) VM-INPUT-VALNOM(14) VM-INPUT-VALNOM(15)
c     *    VM-INPUT-VALNOM(16) VM-INPUT-VALNOM(17) VM-INPUT-VALNOM(18)
c     *    VM-INPUT-VALNOM(19) VM-INPUT-VALNOM(20)
c     ******************PREPARA EL NOMBRE CORRECTO del ARCHIVO *********
c     *    move NOMBOK-INPUT-VALNOM to NOMBRE-AUX-VALNOM.
c     *    if TIP-ID-INPUT-VALNOM  not = 2
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        "." by " ","-" by " ","," by " ",
c     *        "C" by "S","Z" by "S","J" by "G","V" by "B",
c     *        "Y" by "G","H" by "G","I" by "G"
c     *        perform varying I-VALNOM from 1 by 1
c     *            until I-VALNOM >= 66
c     *            if VNOM-AUX-VALNOM(I-VALNOM) not alphabetic
c     *                and VNOM-AUX-VALNOM(I-VALNOM) not numeric
c     *                move "*" to VNOM-AUX-VALNOM(I-VALNOM)
c     *            end-if
c     *            end-perform
c     *    else
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        " S.A. " by "      ",
c     *        "." by " ", " LTDA " by "      ",
c     *        " LIMITADA " by "          ",
c     *        " CIA " by "     ", " COMPA/IA " by "          ",
c     *        " INVERSIONES " by "             ",
c     *        " EMPRESA " by "         ",
c     *        " CORPORACION " by "             ",
c     *        " DE " by "    ", " LA " by "    ", " del " by "     ",
c     *        " S " by "   ", " A " by "   "
c     *        " Y " by "   "
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        " S.A. " by "      ",
c     *        "." by " ", " LTDA " by "      ",
c     *        " LIMITADA " by "          ",
c     *        " CIA " by "     ", " COMPA/IA " by "          ",
c     *        " INVERSIONES " by "             ",
c     *        " EMPRESA " by "         ",
c     *        " CORPORACION " by "             ",
c     *        " DE " by "    ", " LA " by "    ", " del " by "     ",
c     *        " S " by "   ", " A " by "   "
c     *        " Y " by "   "
c     *        inspect NOMBRE-AUX-VALNOM replacing all
c     *        "." by " ","-" by " ",
c     *        "C" by "S","Z" by "S","J" by "G","V" by "B",
c     *        "Y" by "G","H" by "G","I" by "G"
c     *        move NOMBRE-AUX-VALNOM to NOMBOK-INPUT-VALNOM
c     *    end-if.
c     *    move spaces to VECTOR-OK-VALNOM.
c     *    unstring CADENA-VALNOM delimited by all " " into
c     *    BASURA-VALNOM
c     *    VM-OK-VALNOM(1) VM-OK-VALNOM(2) VM-OK-VALNOM(3)
c     *    VM-OK-VALNOM(4) VM-OK-VALNOM(5) VM-OK-VALNOM(6)
c     *    VM-OK-VALNOM(7) VM-OK-VALNOM(8) VM-OK-VALNOM(9)
c     *    VM-OK-VALNOM(10) VM-OK-VALNOM(11) VM-OK-VALNOM(12)
c     *    VM-OK-VALNOM(13) VM-OK-VALNOM(14) VM-OK-VALNOM(15)
c     *    VM-OK-VALNOM(16) VM-OK-VALNOM(17) VM-OK-VALNOM(18)
c     *    VM-OK-VALNOM(19) VM-OK-VALNOM(20).
c     ***********HACE LA VALIDACION BLOQUE A BLOQUE********************
c     *    move 1 to I-VALNOM.
c     *    move 0 to BLOQOK-AUX-VALNOM.
c     *    perform until VM-INPUT-VALNOM(I-VALNOM) = spaces
c     *        move 1 to J-VALNOM
c     *        perform until J-VALNOM > 20
c     *            if VM-INPUT-VALNOM(I-VALNOM) = VM-OK-VALNOM(J-VALNOM)
c     *                add 1 to BLOQOK-AUX-VALNOM
c     *                move spaces to VM-OK-VALNOM(J-VALNOM)
c     *                VM-INPUT-VALNOM(I-VALNOM)
c     *                move 21 to J-VALNOM
c     *            else
c     *                move 0 to M-VALNOM
c     *                inspect VM-OK-VALNOM(J-VALNOM) tallying M-VALNOM
c     *                for CHARACTERS BEFORE initial "*"
c     *                if M-VALNOM >= 15
c     *                    move 0 to M-VALNOM
c     *                    inspect VM-OK-VALNOM(J-VALNOM) tallying
c     *                    M-VALNOM for CHARACTERS BEFORE initial "N"
c     *                end-if
c     *                if M-VALNOM > 0 and < 15 and I-VALNOM < 20
c     *                    and (14 - M-VALNOM > 0)
c     *                    compute INI-VALNOM = M-VALNOM + 2
c     *                    compute LON-VALNOM = 14 - M-VALNOM
c     *                    move VM-OK-VALNOM(J-VALNOM) to PEDAZO-VALNOM
c     *                    if PEDAZO-VALNOM(INI-VALNOM:LON-VALNOM) =
c     *                        VM-INPUT-VALNOM(I-VALNOM + 1 ) and
c     *                        PEDAZO-VALNOM(1:M-VALNOM) =
c     *                        VM-INPUT-VALNOM(I-VALNOM)
c     *                        add 1 to BLOQOK-AUX-VALNOM
c     *                        move spaces to VM-OK-VALNOM(J-VALNOM)
c     *                        VM-INPUT-VALNOM(I-VALNOM)
c     *                        move 21 to J-VALNOM
c     *                        add 1 to I-VALNOM
c     *                        move spaces to VM-INPUT-VALNOM(I-VALNOM)
c     *                    end-if
c     *                end-if
c     *            end-if
c     *            add 1 to J-VALNOM
c     *            end-perform
c     *            add 1 to I-VALNOM
c     *            if I-VALNOM = 21
c     *                next sentence
c     *            end-if
c     *            end-perform.
c     *    if BLOQOK-AUX-VALNOM >= BLOQUES-INPUT-VALNOM
c     *        move 1 to MATCH-OK-OUT-VALNOM
c     *    else
c     *        if TIP-ID-INPUT-VALNOM <> 2
c     *            perform 10100-MATCH-APROXIMADO
c     *        else
c     *            perform 10100-MATCH-APROX-EMPRESAS
c     *        end-if
c     *        if BLOQOK-AUX-VALNOM >= BLOQUES-INPUT-VALNOM
c     *            move 1 to MATCH-OK-OUT-VALNOM
c     *        end-if
c     *    end-if.
c     *
c     *
c     ******************************************************************
c     *10100-MATCH-APROXIMADO.
c     * EN ESTA RUTINA SE VALIDAN LOS BLOQUES QUE NO PASARON COMPLETOS
c     * CON UNA APROXIMACION. ESTA RUTINA FUNCIONA CORRECTAMENTE CON
c     * LOS TIPOS DE IDENTIFICACION 1, 3 Y 4.
c     ******************************************************************
c     *10100-MATCH-APROXIMADO.
c     *    if BLOQOK-AUX-VALNOM > 0
c     *        move 65 to APROX-TOT-VALNOM APROX-INP-VALNOM
c     *    else
c     *        move 70 to APROX-TOT-VALNOM APROX-INP-VALNOM
c     *    end-if
c     *    move 1 to I-VALNOM.
c     *    perform until I-VALNOM > 10
c     *        move 1 to J-VALNOM
c     *        perform until J-VALNOM > 10
c     *            move 0 to LETRAS-OK-VALNOM LETRAS-OK2-VALNOM
c     *            move 1 to K-VALNOM
c     *            move 1 to L-VALNOM
c     *            if VM-INPUT-VALNOM(I-VALNOM) not = spaces and
c     *                VM-OK-VALNOM   (J-VALNOM) not = spaces
c     *                perform until K-VALNOM > 15 or L-VALNOM > 15
c     *                    if VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =
c     *                    VMM-OK-VALNOM  (J-VALNOM,K-VALNOM)
c     *                    or ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM)) = "*"
c     *                    and (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =
c     *                    " " or = "N"))
c     *                        if VMM-OK-VALNOM (J-VALNOM,K-VALNOM)
c     *                        not = spaces
c     *                            add 1 to LETRAS-OK-VALNOM
c     *                            add 1 to K-VALNOM
c     *                            add 1 to L-VALNOM
c     *                        else
c     *                            move 19 to K-VALNOM
c     *                        end-if
c     *                    else
c     *                        add 1 to K-VALNOM
c     *                        if K-VALNOM <= 15 and L-VALNOM < 15
c     *                            if VMM-INPUT-VALNOM(I-VALNOM,
c     *                            L-VALNOM + 1) =
c     *                                VMM-OK-VALNOM(J-VALNOM,K-VALNOM )
c     *                                add 1 to L-VALNOM
c     *                            end-if
c     *                        end-if
c     *                    end-if
c     *                    end-perform
c     *                    move 1 to K-VALNOM
c     *                    move 1 to L-VALNOM
c     *                    perform until K-VALNOM > 15 or L-VALNOM > 15
c     *                        if VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM) =
c     *                        VMM-OK-VALNOM  (J-VALNOM,K-VALNOM)
c     *                        or ((VMM-OK-VALNOM(J-VALNOM, K-VALNOM))
c     *                        = "*"
c     *                        and (VMM-INPUT-VALNOM(I-VALNOM,L-VALNOM)
c     *                        = " "   GJC0003 or = "N"))
c     *                            if VMM-OK-VALNOM (J-VALNOM,K-VALNOM)
c     *                            not = spaces
c     *                                add 1 to LETRAS-OK2-VALNOM
c     *                                add 1 to K-VALNOM
c     *                                add 1 to L-VALNOM
c     *                            else
c     *                                move 19 to K-VALNOM
c     *                            end-if
c     *                        else
c     *                            add 1 to L-VALNOM
c     *                            if L-VALNOM <= 15 and K-VALNOM < 15
c     *                                if VMM-INPUT-VALNOM(I-VALNOM,
c     *                                L-VALNOM) = VMM-OK-VALNOM
c     *                                (J-VALNOM,K-VALNOM + 1)
c     *                                    add 1 to K-VALNOM
c     *                                end-if
c     *                            end-if
c     *                        end-if
c     *                        end-perform
c     *            end-if
c     *            if LETRAS-OK2-VALNOM > LETRAS-OK-VALNOM
c     *                move LETRAS-OK2-VALNOM to LETRAS-OK-VALNOM
c     *            end-if
c     *            if LETRAS-OK-VALNOM > 0
c     *                move 0 to TOTLET-INP-VALNOM TOTLET-OK-VALNOM
c     *                inspect VM-INPUT-VALNOM(I-VALNOM) tallying
c     *                TOTLET-INP-VALNOM
c     *                for CHARACTERS BEFORE initial " "
c     *                inspect VM-OK-VALNOM   (J-VALNOM) tallying
c     *                TOTLET-OK-VALNOM
c     *                for CHARACTERS BEFORE initial " "
c     *                move 0 to POR-TOT-VALNOM  POR-INP-VALNOM
c     *                if TOTLET-OK-VALNOM >= 3
c     *                    compute POR-TOT-VALNOM = LETRAS-OK-VALNOM *
c     *                    100 / TOTLET-OK-VALNOM
c     *                end-if
c     *                if TOTLET-INP-VALNOM >= 3
c     *                    compute POR-INP-VALNOM = LETRAS-OK-VALNOM *
c     *                    100 / TOTLET-INP-VALNOM
c     *                end-if
c     *                if TOTLET-OK-VALNOM > TOTLET-INP-VALNOM
c     *                    compute A-VALNOM = TOTLET-INP-VALNOM * 100 /
c     *                    TOTLET-OK-VALNOM
c     *                else
c     *                    compute A-VALNOM = TOTLET-OK-VALNOM  * 100 /
c     *                    TOTLET-INP-VALNOM
c     *                end-if
c     *                if ((POR-TOT-VALNOM + POR-INP-VALNOM)/2 >=
c     *                    APROX-TOT-VALNOM) and
c     *                    A-VALNOM >= 63
c     ****   display "%TOT=",POR-TOT-VALNOM, " %INP=", POR-INP-VALNOM,
c     ****           "A-VALNOM=", A-VALNOM
c     *****       if (POR-TOT-VALNOM >= APROX-TOT-VALNOM and
c     *****           POR-INP-VALNOM >= APROX-INP-VALNOM ) or
c     *****          ((POR-TOT-VALNOM >= APROX-TOT-VALNOM or
c     *****            POR-INP-VALNOM >= APROX-INP-VALNOM ) and
c     *****            A-VALNOM >= 70)
c     *                    add 1 to BLOQOK-AUX-VALNOM
c     *                    move spaces to VM-OK-VALNOM(J-VALNOM)
c     *                    VM-INPUT-VALNOM(I-VALNOM)
c     *                end-if
c     *            end-if
c     *            add 1 to J-VALNOM
c     *            end-perform
c     *            add 1 to I-VALNOM
c     *            end-perform.
c     *
c     *
c     ******************************************************************
c     *10100-MATCH-APROX-EMPRESAS
c     * EN ESTA RUTINA SE VALIDAN LOS BLOQUES QUE NO PASARON COMPLETOS
c     * CON UNA APROXIMACION. ESTA RUTINA FUNCIONA CORRECTAMENTE CON
c     * LOS TIPOS DE IDENTIFICACION 2.
c     ******************************************************************
c     *10100-MATCH-APROX-EMPRESAS.
c     *    move zeros to TOTLET-INP-VALNOM
c     *    move zeros to LETRAS-OK2-VALNOM
c     *    move zeros to A-VALNOM
c     *    move 1 to K-VALNOM
c     *    perform until K-VALNOM > 65
c     *        move K-VALNOM to I-VALNOM
c     *        move 1 to J-VALNOM
c     *        move zeros to LETRAS-OK-VALNOM
c     *        perform until J-VALNOM > 65 or I-VALNOM > 65
c     *            if V-NOMBRE-VALNOM(I-VALNOM) <> spaces and
c     *                V-NOMBRE-VALNOM(I-VALNOM) <> "."
c     *                if V-NOMBRE-VALNOM(I-VALNOM) =
c     *                    V-NOMBOK-VALNOM(J-VALNOM)
c     *                    add 1 to LETRAS-OK-VALNOM
c     *                    add 1 to I-VALNOM
c     *                    add 1 to J-VALNOM
c     *                else
c     *                    add 1 to J-VALNOM
c     *                end-if
c     *            else
c     *                add 1 to I-VALNOM
c     *            end-if
c     *            end-perform
c     *            if LETRAS-OK-VALNOM > LETRAS-OK2-VALNOM
c     *                move LETRAS-OK-VALNOM to LETRAS-OK2-VALNOM
c     *            end-if
c     *            add 1 to K-VALNOM
c     *            end-perform.
c     *    move 1 to I-VALNOM
c     *    perform until I-VALNOM > 65
c     *        if V-NOMBRE-VALNOM(I-VALNOM) <> spaces and
c     *            V-NOMBRE-VALNOM(I-VALNOM) <> "."
c     *            add 1 to TOTLET-INP-VALNOM
c     *        end-if
c     *        add 1 to I-VALNOM
c     *        end-perform.
c     *    compute A-VALNOM = LETRAS-OK2-VALNOM * 100 /
c     *    TOTLET-INP-VALNOM.
c     *    if A-VALNOM >= 75
c     *        add 1 to BLOQOK-AUX-VALNOM
c     *    end-if.
c     *    move 0 to K-VALNOM
c     *    if BLOQOK-AUX-VALNOM < BLOQUES-INPUT-VALNOM
c     *        INITIALIZE A-VALNOM VECTORES-VALNOM L-VALNOM
c     *        perform varying J-VALNOM from 1 by 1 until J-VALNOM > 65
c     *            if V-NOMBRE-VALNOM(J-VALNOM) <> " "
c     *                move function ORD(V-NOMBRE-VALNOM(J-VALNOM))
c     *                to I-VALNOM
c     *                add 1 to CONT-LET-INPUT-VALNOM(I-VALNOM)
c     *    end-if
c     *    if V-NOMBOK-VALNOM(J-VALNOM) <> " "
c     *        move function ORD(V-NOMBOK-VALNOM(J-VALNOM))
c     *        to I-VALNOM
c     *        add 1 to CONT-LET-OK-VALNOM(I-VALNOM)
c     *        end-if
c     *        end-perform
c     *        move 0 to I-VALNOM
c     *        perform varying J-VALNOM from 1 by 1 until J-VALNOM > 256
c     *            if CONT-LET-OK-VALNOM(J-VALNOM)  > 0
c     *                add 1 to I-VALNOM
c     *                if CONT-LET-OK-VALNOM(J-VALNOM) <
c     *                    CONT-LET-INPUT-VALNOM(J-VALNOM)
c     *                    compute L-VALNOM = L-VALNOM + 100
c     *                else
c     *                    compute L-VALNOM = L-VALNOM +
c     *                    (CONT-LET-INPUT-VALNOM(J-VALNOM)
c     *  100 /CONT-LET-OK-VALNOM(J-VALNOM))  GJC0003
c     *                end-if
c     *            end-if
c     *            end-perform
c     *            compute K-VALNOM = L-VALNOM/I-VALNOM
c     *            if (A-VALNOM >= 40 and K-VALNOM > 80) or
c     *            ((A-VALNOM >= 35 and A-VALNOM < 40)
c     *            and K-VALNOM > 85) ORGJC0003
c     *            ((A-VALNOM >= 25 and A-VALNOM < 35)
c     *            and K-VALNOM = 100)  GJC0003
c     *                add 1 to BLOQOK-AUX-VALNOM
c     *            end-if
c     *        end-if.

