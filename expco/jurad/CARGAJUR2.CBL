      $SET LIST SETTING LISTWIDTH"120"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CARGAJUR.
      ******************************************************************
      * ACTUALIZACION DEL ARCHIVO DE ALERTAS CON LOS DATOS PROVISTOS
      * POR JURAD
      * PROCESA EL ARCHIVO JURAD. CONTROLA CADA REGISTRO: SI ES OK USA
      * LOS DATOS PARA ACTUALIZAR BDIIALE, SI TIENE DEFECTO LO GRABA EN
      * EL ARCHIVO ERRS.
      * Prueba: 0000011000905140101.txt
      ******************************************************************
       AUTHOR.        GLOBANT JL.
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS with debugging mode.
       OBJECT-COMPUTER. MICROFOCUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ******************************************************************
      ******************************************************************
       SELECT JURAD ASSIGN TO P-FILE-PATH
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS JURAD-FS.

      ******************************************************************
      ******************************************************************
       SELECT ERRS ASSIGN TO ERRS-FILE-NAME
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS ERRS-FS.

      ******************************************************************
      ******************************************************************
           COPY BDIIREGI.FS.

      ******************************************************************
      ******************************************************************
           COPY BDIIDIAN.FS.

      ******************************************************************
      ******************************************************************
           COPY BDIIDAS.FS.

      ******************************************************************
      ******************************************************************
           COPY BDIIALE.FS REPLACING == ON RECORD.== BY == ON RECORD
              FILE STATUS IS EST-BDIIALE.==.

      ******************************************************************
      * Archivos de base alterna
      ******************************************************************
           copy BDACSV.FS.
           copy ICBDIR.FS.
           copy BDLCLI.FS.
           COPY ICBSUS.FS.
           COPY ICBCHK.FS.
           COPY ICBCRE.FS.
           COPY BDLSTDCIU.FS.
           COPY DIREST.FS.
           COPY BDLNEWDIRBOG.FS.

      ******************************************************************
      ******************************************************************
       DATA DIVISION.
       FILE section.

      ******************************************************************
      * Archivo de entrada con los datos de demandas y embargos de JURAD
      ******************************************************************
       FD JURAD.
       01  JURAD-reg.
      * Registros de movimientos I, M, E
      *    Tipo de Novedad {I, M, E} Inser/Modif/Elim
           03 JURAD-TIPO-NOVEDAD          PIC X.
      *    Tipo de identificacion Numero {1, 2, 3, 4}
           03 JURAD-ID-TIPO               PIC X.
      *    Numero de identificacion
           03 JURAD-ID-NUMEROX            PIC X(11).
           03 JURAD-ID-NUMERO             REDEFINES JURAD-ID-NUMEROX
                                          PIC 9(11).
      *    Nombres y Apellidos
           03 JURAD-NOMBRE                PIC X(45).
      *    Numero de demandas vigentes
           03 JURAD-DEMANDAS-VIGENTES     PIC 9(07).
      *    Numero de demandas terminadas
           03 JURAD-DEMANDAS-TERMINADAS   PIC 9(07).
      *    Fecha ultima demanda vigente YYYYMMDD
           03 JURAD-DEMANDAS-FECHA-ULTIMA PIC X(8).
      *    Numero de embargos vigentes
           03 JURAD-EMBARGOS-VIGENTES     PIC 9(07).
      *    Numero de embargos terminados
           03 JURAD-EMBARGOS-TERMINADOS   PIC 9(07).
      *    Fecha ultimo embargo vigente YYYYMMDD
           03 JURAD-EMBARGOS-FECHA-ULTIMO PIC X(08).

       01  JURADT-REG.
      * Registro de totales de control T
      *    Tipo de Novedad = "T"
           03 JURADT-TIPO-NOVEDAD         PIC X.
           03                             PIC X.
      *    Cantidad de registros
           03 JURADT-REGISTROS            PIC 9(11).
           03                             PIC X(45).
      *    Total numero de demandas vigentes
           03 JURADT-DEMANDAS-VIGENTES    PIC 9(7).
      *    Total numero de demandas terminadas
           03 JURADT-DEMANDAS-TERMINADAS  PIC 9(7).
           03                             PIC X(08).
      *    Total numero de embargos vigentes
           03 JURADT-EMBARGOS-VIGENTES    PIC 9(7).
      *    Total numero de embargos terminados
           03 JURADT-EMBARGOS-TERMINADOS  PIC 9(7).
           03                             PIC X(08).

      ******************************************************************
      * Archivo de salida con los registros defectuosos del arch JURAD
      ******************************************************************
       FD ERRS.
       01  ERRS-REG.
      *    Tipo de Novedad {I, M, E, T} Inser/Modif/Elim/Totales
           03 ERRS-TIPO-NOVEDAD          PIC X.
      *    Tipo de identificacion Numero {1, 2, 3, 4}
           03 ERRS-ID-TIPO               PIC X.
      *    Numero de identificacion
           03 ERRS-ID-NUMERO             PIC X(11).
      *    Nombres y Apellidos
           03 ERRS-NOMBRE                PIC X(45).
      *    Numero de demandas vigentes
      *    03 ERRS-DEMANDAS-VIGENTES     PIC 999.
           03 ERRS-DEMANDAS-VIGENTES     PIC 9(07).
      *    Numero de demandas terminadas
      *    03 ERRS-DEMANDAS-TERMINADAS   PIC 999.
           03 ERRS-DEMANDAS-TERMINADAS   PIC 9(07).
      *    Fecha ultima demanda vigente YYMMDD
           03 ERRS-DEMANDAS-FECHA-ULTIMA PIC X(8).
      *    Numero de embargos vigentes
      *    03 ERRS-EMBARGOS-VIGENTES     PIC 999.
           03 ERRS-EMBARGOS-VIGENTES     PIC 9(07).
      *    Numero de embargos terminados
      *    03 ERRS-EMBARGOS-TERMINADOS   PIC 999.
           03 ERRS-EMBARGOS-TERMINADOS   PIC 9(07).
      *    Fecha ultimo embargo vigente YYMMDD
           03 ERRS-EMBARGOS-FECHA-ULTIMO PIC X(8).
      *    Causa del rechazo
           03 ERRS-CAUSA-RECHAZO         PIC X(300).

      ******************************************************************
      * Contiene los ids y nombres de personas
      ******************************************************************
       COPY BDIIREGI.FD.

      ******************************************************************
      * Maestro de personas juridicas nacionales y extranjeras reportada
      * por la DIAN
      ******************************************************************
       COPY BDIIDIAN.FD.

      ******************************************************************
      * Archivo maestro de cedulas de extranjeria suministrado por DAS
      ******************************************************************
       COPY BDIIDAS.FD.

      ******************************************************************
      * Archivos de base alterna
      ******************************************************************
           COPY BDACSV.FD.
           COPY ICBDIR0001.FD.
           COPY BDLCLI.FD.
           COPY ICBSUS.FD.
           COPY ICBCHK.FD.
           COPY ICBCRE.FD.
           COPY BDLSTDCIU.FD.
           COPY DIREST.FD.
           COPY BDLNEWDIRBOG.FD.

      ******************************************************************
      * Maestro de alertas reportadas por suscriptores
      ******************************************************************
       COPY BDIIALE.FD.

      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Nombre del programa
       01  PGM                          PIC X(10) VALUE "CARGAJUR".


      ******************************************************************
      * Datos para la búsqueda en base alterna
       01  W-DERECHOS                   PIC 9 VALUE 0.
       01  CODIGO-ERROR                 PIC 99 VALUE 00.

       01  SW-BDIIALE                   PIC 9 VALUE 0.
       01  HACER-MATCH                  PIC 9 VALUE 0.
       01  CADENA-ENTRA                 PIC X(60).
       01  CADENA-SALE                  PIC X(60).
       01  IND-ALFABETICO              PIC  X(01).
           88 SI-ALFABETICO           VALUE " " "A" "B" "C" "D" "E" "F"
                                       "G" "H" "I" "J" "K" "L" "M" "N"
                                       "/" "O" "P" "Q" "R" "S" "T" "U"
                                       "V" "W" "X" "Y" "Z" "Ñ".

       01  DATOS-E.
           03 TIPOID-E                  PIC 9.
           03 NUMID-E                   PIC 9(11).
           03 NOMBRE-E                  PIC x(45).

       01  W-CONTADORES.
           03  W-CONT-TIPO-5            PIC 9(6) VALUE ZERO.
           03  W-CONT-TIPO-6            PIC 9(6) VALUE ZERO.
           03  W-CONT-TIPO-7            PIC 9(6) VALUE ZERO.
           03  W-CONT-TIPO-5-PJ         PIC 9(6) VALUE ZERO.
           03  W-CONT-TIPO-5-REP        PIC 9(6) VALUE ZERO.

       01  SUSCRIPTOR-AUX               PIC 9(6).
       01  REG-MAL-FORMADOS             PIC 9(7) VALUE ZERO.
       01  REG-LEIDOS                   PIC 9(7) VALUE ZERO.
       01  CONT-FALLECIDOS              PIC 9(7) VALUE ZERO.
       01  REG-CORRECTOS                PIC 9(7) VALUE ZERO.
       01  NO-COINCIDE-DIAN             PIC 9(7) VALUE ZERO.
       01  NO-COINCIDE-REGIS            PIC 9(7) VALUE ZERO.
       01  NO-EXISTE-DIAN               PIC 9(7) VALUE ZERO.
       01  NO-EXISTE-REGIS              PIC 9(7) VALUE ZERO.
       01  SI-EXISTE-CSV                PIC 9(7) VALUE ZERO.
       01  TIPO-ID-INVALIDO             PIC 9(7) VALUE ZERO.
       01  NUM-ID-W                     PIC 9(11) VALUE ZERO.
       01  FECHA-PROC.
           03 ANO-HOY.
              05 SIG-PROC               PIC X(2).
              05 ANO-PROC               PIC X(2).
           03 MES-PROC                  PIC X(2).
           03 DIA-PROC                  PIC X(2).
       01  FECHA-PROC-R                 REDEFINES FECHA-PROC
                                        PIC X(8).

       01 MAS-DATOS.
           03 I                         PIC 9(02) VALUE ZERO.
           03 J                         PIC 9(02) VALUE ZERO.

           COPY  BDLDIR.WS.

      ******************************************************************
      * Variables para rutina de validacion de identificaciones
      ******************************************************************
           COPY BDIIVIDE.WS.

      ******************************************************************
      * Variables relacionadas con la la base BDIIALE de alertas online
      ******************************************************************
           COPY BDIIALE.WS.

      ******************************************************************
      * Datos para las rutinas de validación de nombres
      ******************************************************************
           COPY VALNOM.WS.

      *****************************************************************
      * Direcciones de los archivos en el file system
      *****************************************************************
       COPY ICFILE.WS.

      *****************************************************************
      * Variables validacion cuentas si validar
      *****************************************************************
           COPY BDAMATCH.WS.

      ******************************************************************
      * Indica que se termino el proceso, por agotamiento del archivo
      * de entrada (normal) o alguna otra condicion (error)
       01  IND-FIN-PROCESO              PIC XX VALUE "NO".

      * Copia del ultimo reg de input, para mensajes (Ult reg: ...)
       01  JURAD-REG-ANTERIOR           PIC X(100) VALUE "(VACIO)".
       01  JURAD-FS                     PIC XX VALUE SPACES.
       01  ERRS-FS                      PIC XX VALUE SPACES.

      * Datos de demandas o embargos que van en el registro BDIIALE
       01 D-DATAHOST.
           05 D-VIGENTES                PIC 9(5).
           05 D-TERMINADOS              PIC 9(5).
           05 D-FECHA-ULTIMA            PIC X(8).
      ******************************************************************
      * Parametros iniciales, de la linea de comando
      * La linea de comando que contiene los parametros
       01 P-COMMAND-LINE                PIC X(80) VALUE SPACES.
       01 P-PARAMETROS.
      *    Proceso real: contiene "1" para activar, "2" por defecto
           03 P-PROCESO-REAL            PIC X VALUE "2".
           88 PROCESO-REAL              VALUE "2".

      *    Nombre del archivo de entrada JJJJJJaaaDDMMAAbbXX.txt
           03 P-FILE-NAME               PIC X(80) VALUE SPACES.
           03 P-FILE-NAME-R             REDEFINES P-FILE-NAME.
      *        JJJJJJ = Codigo de suscriptor JURAD en Experian
               05 P-COD-SUSCRIPTOR      PIC 9(6).
      *        aaa = Codificacion del producto ("100")
               05 P-PRODUCTO            PIC XXX.
      *        DDMMAA = Fecha de proceso
               05 P-FECHA-PROCESO       PIC X(6).
      *        bb =  01 a CI
               05 P-CI-BI               PIC XX.
      *        XX = Tipo de proceso
      *        01 Envio Original - Carga TOTAL - primera vez
      *        02 Archivo Semanal - Carga PARCIAL
      *        03 Archivo Mensual - Carga PARCIAL
      *        04 Refresque Total - Carga TOTAL
               05 P-TIPO-PROCESO        PIC XX.
      *        extension del archivo ".txt"
               05 P-EXTENSION           PIC X(4).
               05                       PIC X(57).
           03 P-FILE-PATH               PIC X(80) VALUE SPACES.
      * Nombre del archivo de errores JJJJJJaaaDDMMAAbbXX.errs
          03 ERRS-FILE-NAME             PIC X(80) VALUE SPACES.

      * Fecha de proceso editada YYYYMMDD
       01 P-FECHA-PROCESO-YYYYMMDD      PIC X(8).
      * Fecha de proceso + 50 años
       01 P-FECHA-PROCESO-50.
           03  P-FECHA-PROCESO-50YYYY   PIC 9999.
           03                           PIC X(4).
      ******************************************************************
      * Datos para el reporte final
       01 W-TIEMPO.
           03 W-HORA-INICIO             PIC 9(8).
           03 W-HORA-FIN                PIC 9(8).
       01 N-EDIT                        PIC Z(7)9B.
       01 N-EDITP100                    REDEFINES N-EDIT
                                        PIC Z(5)9.9B.
       01 N-CONTADORES.
      *    Registros en el archivo de entrada
           03 N-REG-LEIDOS              PIC 9(8) VALUE ZERO.
      *    Registros procesados sin error
           03 N-REG-PROCESADOS          PIC 9(8) VALUE ZERO.
      *    Registros con error
           03 N-REG-CON-ERROR           PIC 9(8) VALUE ZERO.

      *    Cantidades de registros por tipo de novedad
           03 N-REG-TIPO-I-OK           PIC 9(8) VALUE ZERO.
           03 N-REG-TIPO-M-OK           PIC 9(8) VALUE ZERO.
           03 N-REG-TIPO-E-OK           PIC 9(8) VALUE ZERO.
           03 N-REG-TIPO-I-ERR          PIC 9(8) VALUE ZERO.
           03 N-REG-TIPO-M-ERR          PIC 9(8) VALUE ZERO.
           03 N-REG-TIPO-E-ERR          PIC 9(8) VALUE ZERO.

      *    Alertas borradas previo al refresque total
           03 N-ALE-BORRADAS            PIC 9(8) VALUE ZERO.
      *    Alertas insertadas
           03 N-ALE-INSERTADAS          PIC 9(8) VALUE ZERO.
      *    Alertas modificadas
           03 N-ALE-MODIFICADAS         PIC 9(8) VALUE ZERO.
      *    Alertas eliminadas
           03 N-ALE-ELIMINADAS          PIC 9(8) VALUE ZERO.
      *    Insercion de alerta existente
           03 N-ALE-YA-EXISTE           PIC 9(8) VALUE ZERO.
      *    Modificacion o eliminacion de alerta inexistente
           03 N-ALE-NO-EXISTE           PIC 9(8) VALUE ZERO.

      *    Sumas de cantidades de demandas y embargos
           03 N-DEMANDAS-VIGENTES       PIC 9(7) VALUE ZERO.
           03 N-DEMANDAS-TERMINADAS     PIC 9(7) VALUE ZERO.
           03 N-EMBARGOS-VIGENTES       PIC 9(7) VALUE ZERO.
           03 N-EMBARGOS-TERMINADOS     PIC 9(7) VALUE ZERO.

      ******************************************************************
      * Errores de consistencia del input
      * Cada error tiene 70 caracteres:
      *     codigo de 2 digitos,
      *     indicador de impactar BDIIALE aunque haya este error "+"
      *     un espacio en blanco
      *     contador de ocurrencias de 7 digitos
      *     un espacio en blanco, y 
      *     descripcion de 59 caractares
       01 E-ERRORES.
      *    indica si el registro actual tiene error
           03 E-HAY-ERROR               PIC XX VALUE "NO".
      *    indica si el registro actual impacta la base de datos
           03 E-IMPACTAR-BDIIALE        PIC XX VALUE "SI".
      *    errores del registro actual
           03 E-ERRORES-REG             PIC X(300) VALUE SPACES.
      *    contador de registros con error
           03 E-CONT-REGS-CON-ERROR     PIC 9(8) VALUE ZERO.
      *    tabla de errores
           03 PIC X(11) VALUE "01 0000000 ".
           03 PIC X(59) VALUE
           "Tipo de movimiento invalido, debe ser I M E T".

           03 PIC X(11) VALUE "02 0000000 ".
           03 PIC X(59) VALUE
           "Tipo de identificacion invalido, debe ser 1 2 3 4".

           03 PIC X(11) VALUE "03 0000000 ".
           03 PIC X(59) VALUE
           "Numero de identificacion invalido, debe ser numerico".

           03 PIC X(11) VALUE "04 0000000 ".
           03 PIC X(59) VALUE
           "Tipo de identificacion debe ser mayor que cero".

           03 PIC X(11) VALUE "05 0000000 ".
           03 PIC X(59) VALUE
           "Falta el nombre o denominacion".

           03 PIC X(11) VALUE "06 0000000 ".
           03 PIC X(59) VALUE
           "Numero de demandas vigentes: debe ser numerico".

           03 PIC X(11) VALUE "07 0000000 ".
           03 PIC X(59) VALUE
           "Numero de demandas terminadas: debe ser numerico".

           03 PIC X(11) VALUE "08 0000000 ".
           03 PIC X(59) VALUE
           "Fecha ultima demanda vigente YYYYMMDD: debe ser numerica".

           03 PIC X(11) VALUE "09 0000000 ".
           03 PIC X(59) VALUE
           "Fecha ultima demanda vigente YYYYMMDD: fuera de rango".

           03 PIC X(11) VALUE "10 0000000 ".
           03 PIC X(59) VALUE
           "Numero de embargos vigentes: debe ser numerico".

           03 PIC X(11) VALUE "11 0000000 ".
           03 PIC X(59) VALUE
           "Numero de embargos terminados: debe ser numerico".

           03 PIC X(11) VALUE "12 0000000 ".
           03 PIC X(59) VALUE
           "Fecha ultimo embargo vigente YYYYMMDD: debe ser numerica".

           03 PIC X(11) VALUE "13 0000000 ".
           03 PIC X(59) VALUE
           "Fecha ultimo embargo vigente YYYYMMDD: fuera de rango".

           03 PIC X(11) VALUE "14+0000000 ".
           03 PIC X(59) VALUE
           "El id no se encuentra en el archivo maestro (tipo 7)".

           03 PIC X(11) VALUE "15 0000000 ".
           03 PIC X(59) VALUE
           "El nombre en el input no coincide con el registrado".

           03 PIC X(11) VALUE "16 0000000 ".
           03 PIC X(59) VALUE
           "Numero de id fuera del rango del tipo de id informado".

           03 PIC X(11) VALUE "17 0000000 ".
           03 PIC X(59) VALUE
           "Modificación de alerta que no existe".

           03 PIC X(11) VALUE "18 0000000 ".
           03 PIC X(59) VALUE
           "Eliminación de alerta que no existe".

           03 PIC X(11) VALUE "19 0000000 ".
           03 PIC X(59) VALUE
           "Inserción de alerta que ya existe".
      *----------------- insertar nuevos items aqui -------------------
           03 PIC X(11) VALUE "00 0000000 ".
           03 PIC X(59) VALUE "--- FIN DE LA LISTA (COD 00) ---".
           03                           PIC X(3500). *> LOS 50 ITEMS
       01 E-ERRORES-2                   REDEFINES E-ERRORES.
           03                           PIC XX.
           03                           PIC XX.
           03                           PIC X(300).
           03                           PIC 9(8).
           03 E-ERROR                   OCCURS 50 INDEXED EX.
               05 E-CODIGO              PIC XX.
               05 E-IMPACTAR            PIC X.
               05 E-CONTADOR            PIC 9(7).
               05                       PIC X.
               05 E-DESCRIPCION         PIC X(59).
       01 E-EDIT                        PIC Z(8)B.

      ******************************************************************
      * Control de rangos de valores de las fechas del input
       01 VALIDACIONES-FECHAS.
           02 ANO-VAL-X                 pic 9(4).
              88 ANO-VAL                value 1900 thru 2050.
           02 MES-VAL-X                 pic 9(2).
              88 MES-VAL                value 01 thru 12.
           02 DIA-VAL-X                 pic 9(2).
              88 DIA-VAL                value 01 thru 31.


      /*****************************************************************
      ******************************************************************
       PROCEDURE DIVISION.


       0000-PROGRAMA-PRINCIPAL.
      ******************************************************************
      *    inicializa el ambiente y abre los archivos
           PERFORM 1000-RUTINA-INICIAL
      *    procesa cada uno de los registros del archivo JURAD hasta
      *    encontrar el registro "T" de totales
           PERFORM 4000-PROCESAR-REGISTRO UNTIL IND-FIN-PROCESO = "SI"
      *    cierra archivos y produce el reporte final
           PERFORM 7000-RUTINA-FINAL
           GOBACK.

       4000-PROCESAR-REGISTRO.
      ****************************************************************
      * Proceso de un registro del archivo de entrada JURAD
      * Lectura y control del file status del archivo
      ****************************************************************
      *    lectura del proximo registro
           READ JURAD
           EVALUATE JURAD-FS
           WHEN "00"
      *        CONTROLA EL REG Y LO APLICA SI ES OK
      D        display JURAD-REG
               PERFORM 4000-PROCESAR-REGISTRO-2
               MOVE JURAD-REG TO JURAD-REG-ANTERIOR
               IF JURAD-TIPO-NOVEDAD <> "T"
                  ADD 1 TO REG-LEIDOS
               END-IF
      D        PERFORM DISPLAY-CONTADORES
           WHEN "10"
      *        Fin del archivo de input:
      *        Esto es un error, no se encontro el registro "T" antes
      *        de que se terminara el archivo
               DISPLAY "FALTA EL REG DE TOTALES DEL ARCHIVO JURAD "
               DISPLAY "NOMBRE DEL ARCHIVO: " P-FILE-NAME
               DISPLAY "FILE STATUS: " JURAD-FS
               DISPLAY "ULTIMO REGISTRO LEIDO: " N-REG-LEIDOS
               DISPLAY JURAD-REG-ANTERIOR
               PERFORM 9000-CANCELAR-PROGRAMA
           WHEN OTHER
      *        Error de lectura: cancelacion del proceso
               DISPLAY "ERROR EN LECTURA DEL ARCHIVO JURAD "
               DISPLAY "NOMBRE DEL ARCHIVO: " P-FILE-NAME
               DISPLAY "FILE STATUS: " JURAD-FS
               DISPLAY "ULTIMO REGISTRO LEIDO: " N-REG-LEIDOS
               DISPLAY JURAD-REG-ANTERIOR
               PERFORM 9000-CANCELAR-PROGRAMA
           END-EVALUATE.


       4000-PROCESAR-REGISTRO-2.
      ****************************************************************
      * Controla los datos de un registro de JURAD y si es OK lo usa
      * para actualizar el archivo de alertas en linea
      * La informacion esta leida en JURAD-reg
      * Hay dos indicadores: E-HAY-ERROR es si se encontró algún error
      * en el registro, mientras que E-IMPACTAR-BDIIALE significa si
      * los alertas se graban, independientemente de los errores 
      * detectados (algunos errores no impiden la grabación)
      ****************************************************************
           MOVE "NO" TO E-HAY-ERROR
           MOVE "SI" TO E-IMPACTAR-BDIIALE
           MOVE 00 TO CODIGO-ERROR

      *    Validacion del tipo de movimiento
           IF JURAD-TIPO-NOVEDAD NOT = "I" AND "M" AND "E" AND "T"
               CALL "PONERR" USING "01" JURAD-REG E-ERRORES
           END-IF

      *    Si es el registro "T" hay que señalar el fin de proceso
           IF JURAD-TIPO-NOVEDAD = "T"
               MOVE "SI" TO IND-FIN-PROCESO
               PERFORM 4800-PROCESO-REG-T
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO N-REG-LEIDOS

      *    Validación formal: que los datos sean numéricos, etc ...
           PERFORM 4100-VALIDACION-FORMAL
           IF E-HAY-ERROR = "SI"
               ADD 1 TO REG-MAL-FORMADOS
               PERFORM 4900-GRABAR-ERROR
               EXIT PARAGRAPH
           END-IF

      *    Validación del contenido
           PERFORM 4200-VALIDACION-SEMANTICA
           PERFORM 4240-CONTAR-TIPO-CUENTA

           IF E-IMPACTAR-BDIIALE = "SI"
               PERFORM 4300-MODIFICAR-BDIIALE
               ADD 1 TO N-REG-PROCESADOS
           END-IF

           IF E-HAY-ERROR = "SI"
               PERFORM 4900-GRABAR-ERROR
           END-IF.


       4100-VALIDACION-FORMAL.
      ******************************************************************
      * VALIDACION FORMAL DE LOS DATOS DEL INPUT
      ******************************************************************
      *    TIPO DE IDENTIFICACION NUMERICA
           IF JURAD-ID-TIPO NOT NUMERIC
               CALL "PONERR" USING "02" JURAD-REG E-ERRORES
            END-IF
      *    NUMERO DE IDENTIFICACION
           IF JURAD-ID-NUMEROX IS NOT NUMERIC
               CALL "PONERR" USING "03" JURAD-REG E-ERRORES
           END-IF
           IF JURAD-ID-NUMERO NOT > 0
               CALL "PONERR" USING "04" JURAD-REG E-ERRORES
           END-IF
      *    SI EL MOVIMIENTO ES UNA ELIMINACION LOS DATOS SUBSIGUIENTES
      *    NO SE CONTROLAN
           IF JURAD-TIPO-NOVEDAD = "E"
               EXIT PARAGRAPH
           END-IF
      *    NOMBRES Y APELLIDOS
           IF JURAD-NOMBRE = SPACES
               CALL "PONERR" USING "05" JURAD-REG E-ERRORES
           END-IF
      *    NUMERO DE DEMANDAS VIGENTES
           IF JURAD-DEMANDAS-VIGENTES IS NOT NUMERIC
               CALL "PONERR" USING "06" JURAD-REG E-ERRORES
           END-IF
      *    NUMERO DE DEMANDAS TERMINADAS
           IF JURAD-DEMANDAS-TERMINADAS IS NOT NUMERIC
               CALL "PONERR" USING "07" JURAD-REG E-ERRORES
           END-IF
      *    FECHA ULTIMA DEMANDA VIGENTE YYMMDD
           IF JURAD-DEMANDAS-FECHA-ULTIMA IS NOT NUMERIC
               CALL "PONERR" USING "08" JURAD-REG E-ERRORES
           END-IF
           MOVE JURAD-DEMANDAS-FECHA-ULTIMA TO VALIDACIONES-FECHAS
           IF NOT ANO-VAL OR NOT MES-VAL OR NOT DIA-VAL
               CALL "PONERR" USING "09" JURAD-REG E-ERRORES
           END-IF
      *    NUMERO DE EMBARGOS VIGENTES
           IF JURAD-EMBARGOS-VIGENTES NOT NUMERIC
               CALL "PONERR" USING "10" JURAD-REG E-ERRORES
           END-IF
      *    NUMERO DE EMBARGOS TERMINADOS
           IF JURAD-EMBARGOS-TERMINADOS NOT NUMERIC
               CALL "PONERR" USING "11" JURAD-REG E-ERRORES
           END-IF
      *    FECHA ULTIMO EMBARGO VIGENTE YYMMDD
           IF JURAD-EMBARGOS-FECHA-ULTIMO IS NOT NUMERIC
               CALL "PONERR" USING "12" JURAD-REG E-ERRORES
           END-IF
           MOVE JURAD-EMBARGOS-FECHA-ULTIMO TO VALIDACIONES-FECHAS
           IF NOT ANO-VAL OR NOT MES-VAL OR NOT DIA-VAL
               CALL "PONERR" USING "13" JURAD-REG E-ERRORES
           END-IF.


       4200-VALIDACION-SEMANTICA.
      ****************************************************************
      * Validacion del tipo y numero de identificacion y de su relacion
      * con el nombre
      ****************************************************************
         IF JURAD-ID-TIPO NOT = "1" AND "2" AND "3" AND "4"
           MOVE 05 TO CODIGO-ERROR
           ADD 1 TO TIPO-ID-INVALIDO
           CALL "PONERR" USING "02" JURAD-REG E-ERRORES
         ELSE
      *    Parámetros para las rutinas de validación en BDIIVIDE
           MOVE JURAD-ID-TIPO TO TIP-IDE-BDIIVIDE
           MOVE JURAD-ID-NUMERO TO NUM-IDE-BDIIVIDE NUM-ID-W
           MOVE JURAD-NOMBRE TO NOMBRE-BDIIVIDE
      *    Switches de opciones de validación
           MOVE 1 TO SW-VERIFNOM-BDIIVIDE SW-DIGCHEQUEO-BDIIVIDE
           MOVE 2 TO NUM-BLOQUES-BDIIVIDE
      *    Valida el número de identificación contra las bases de
      *    datos BDIIDIAN, BDIIDAS, BDIIREGI y verifica la coincidencia
      *    del nombre reportado
      *    retorna SW-ID-BDIIVIDE=1 si hay error
           PERFORM 80100-VALIDAR-IDENTIFICACION
           MOVE  TIP-IDE-BDIIVIDE TO TIPOID-E
           MOVE  NUM-IDE-BDIIVIDE TO NUMID-E
           IF SW-ID-BDIIVIDE = 1
      *      es un id que no existe en BDIIREGI, BDIIDAS o BDIIDIAN
             CALL "PONERR" USING "14" JURAD-REG E-ERRORES
      *      valida contra la base alterna
             PERFORM 4210-VALIDACION-ID-NO-FIGURA
           ELSE
             IF SW-ID-BDIIVIDE = 1
                 IF TIPOID-E <> 1
                   MOVE ZEROS TO HACER-MATCH
                   MOVE 0 TO IDE-TIPO-BDAMATCH
                   PERFORM 513-VALIDA-EN-CSV
                   IF  IDE-TIPO-BDAMATCH  =  0
                     MOVE 03 TO CODIGO-ERROR
                     ADD 1 TO NO-EXISTE-DIAN
                   END-IF
                 ELSE
                    MOVE ZEROS TO HACER-MATCH
                    MOVE 0 TO IDE-TIPO-BDAMATCH
                    PERFORM 513-VALIDA-EN-CSV
                    IF IDE-TIPO-BDAMATCH = 0
                       MOVE 04 TO CODIGO-ERROR
                       ADD 1 TO NO-EXISTE-REGIS
                    END-IF
                 END-IF
              ELSE
                 IF SW-NOMBRES-BDIIVIDE = 1
                    CALL "PONERR" USING "15" JURAD-REG E-ERRORES
                    IF TIPOID-E <> 1
                       MOVE 01 TO CODIGO-ERROR
                       ADD 1 TO NO-COINCIDE-DIAN
                    ELSE
                       MOVE 02 TO CODIGO-ERROR
                       ADD 1 TO NO-COINCIDE-REGIS
                    END-IF
                 ELSE
                    INITIALIZE LLAVE-BDIIALE
                    MOVE TIPOID-E TO TIP-IDE-BDIIALE
                    MOVE NUMID-E TO NUM-IDE-BDIIALE
                    MOVE 990003 TO FUENTE-BDIIALE
                    START BDIIALE KEY NOT < LLAVE-BDIIALE INVALID KEY
                        MOVE 1 TO SW-BDIIALE
                    NOT INVALID KEY
                        MOVE 0 TO SW-BDIIALE
                        READ BDIIALE NEXT RECORD AT END
                            MOVE 1 TO SW-BDIIALE
                        END-READ
                    END-START
                    MOVE FUNCTION CURRENT-DATE TO FECHA-PROC-R
                    PERFORM UNTIL SW-BDIIALE = 1 OR
                    TIPOID-E <> TIP-IDE-BDIIALE OR
                    NUMID-E <> NUM-IDE-BDIIALE
                        IF FUENTE-BDIIALE = 990003 AND
                           FEC-NOV-BDIIALE <= FECHA-PROC-R AND
                           FEC-VEN-BDIIALE >= FECHA-PROC-R AND
                           BLOQUEO-BDIIALE = 0
                           MOVE 1 TO SW-BDIIALE
                        END-IF
                        IF SW-BDIIALE = 0
                            READ BDIIALE NEXT RECORD AT END
                                MOVE 1 TO SW-BDIIALE
                            END-READ
                        END-IF
                    END-PERFORM
                    IF TIPOID-E = 1
                        IF ESTADO-VIG-BDIIREGI = 12
                            MOVE 07 TO CODIGO-ERROR
                        ELSE
                            IF ESTADO-VIG-BDIIREGI = 21
                                MOVE 06 TO CODIGO-ERROR
                                CALL "PONERR" USING "15" JURAD-REG
                                E-ERRORES
                                ADD 1 TO NO-COINCIDE-REGIS
                                ADD 1 TO CONT-FALLECIDOS
                            END-IF
                        END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.


       4240-CONTAR-TIPO-CUENTA.
      ******************************************************************
      *    Conteo de los registros por "tipo", dependiendo del código
      *    de error:
      *    00=OK
      *    01=NIT OK, nombre no coincide
      *    02=cédula OK, nombre no coincide
      *    03=NIT no figura
      *    04=cédula no figura pero está en base alterna
      *    05=tipo id no es 1|2|3|4
      *    cuenta tipo 5: sin error
      *    cuenta tipo 6: 01 02 06 07
      *    cuenta tipo 7: 03 04 05
      ******************************************************************
           IF CODIGO-ERROR = 01 OR 02 OR 06 OR 07
               ADD 1 TO W-CONT-TIPO-6
           ELSE
               IF CODIGO-ERROR = 03 OR 04 OR 05
                   ADD 1 TO W-CONT-TIPO-7
               ELSE
      *            es 00: registros correctos
                   ADD 1 TO W-CONT-TIPO-5
                   ADD 1 TO REG-CORRECTOS
      *            si es un NIT
                   IF TIPOID-E = 2 OR 3
                       ADD 1 TO W-CONT-TIPO-5-PJ
                   END-IF
               END-IF
           END-IF.


       4210-VALIDACION-ID-NO-FIGURA.
      ******************************************************************
      * Validación de IDs que no figuran en las bases normales
      ******************************************************************
           MOVE 0 TO HACER-MATCH
           MOVE 0 TO IDE-TIPO-BDAMATCH
      *    Valida si la identificación está en la base de datos alterna
           PERFORM 513-VALIDA-EN-CSV
           IF IDE-TIPO-BDAMATCH = 0
               IF TIP-IDE-BDIIVIDE = 1
      *            Es una cédula
                   MOVE 04 TO CODIGO-ERROR
                   ADD 1 TO NO-EXISTE-REGIS
               ELSE
      *            No es una cédula
                   MOVE 03 TO CODIGO-ERROR
                   ADD 1 TO NO-EXISTE-DIAN
               END-IF
           END-IF.


      DDISPLAY-CONTADORES.
      ******************************************************************
      * Código para controlar la evolución de los contadores
      ******************************************************************
      D    DISPLAY "REG-MAL-FORMADOS :" REG-MAL-FORMADOS
      D    DISPLAY "REG-LEIDOS       :" REG-LEIDOS
      D    DISPLAY "REG-CORRECTOS    :" REG-CORRECTOS
      D    DISPLAY "SI-EXISTE-CSV    :" SI-EXISTE-CSV
      D    DISPLAY "NO-COINCIDE-REGIS:" NO-COINCIDE-REGIS
      D    DISPLAY "CONT-FALLECIDOS  :" CONT-FALLECIDOS
      D    DISPLAY "NO-COINCIDE-DIAN :" NO-COINCIDE-DIAN
      D    DISPLAY "NO-EXISTE-REGIS  :" NO-EXISTE-REGIS
      D    DISPLAY "NO-EXISTE-DIAN   :" NO-EXISTE-DIAN
      D    DISPLAY "TIPO-ID-INVALIDO :" TIPO-ID-INVALIDO
      D    DISPLAY "Alertas insertadas: " N-ALE-INSERTADAS
      D    " modificadas: " N-ALE-MODIFICADAS
      D    " eliminadas: " N-ALE-ELIMINADAS.


       4300-MODIFICAR-BDIIALE.
      ******************************************************************
      * Se impacta el archivo BDIIALE con una o dos inserciones,
      * modificaciones o eliminaciones segun haya o no demandas y
      * embargos
      ******************************************************************
           INITIALIZE REG-BDIIALE

      *    DEMANDAS
           MOVE "J001" TO LLASEC-BDIIALE
      *    ARMA DATOS DE DEMANDAS
           MOVE JURAD-DEMANDAS-VIGENTES TO D-VIGENTES
           MOVE JURAD-DEMANDAS-TERMINADAS TO D-TERMINADOS
           MOVE JURAD-DEMANDAS-FECHA-ULTIMA TO D-FECHA-ULTIMA
           PERFORM 4300-MODIFICAR-BDIIALE-DATOS

      *    EMBARGOS
           IF JURAD-EMBARGOS-VIGENTES > ZERO
           OR JURAD-EMBARGOS-TERMINADOS > ZERO
           MOVE "J002" TO LLASEC-BDIIALE
      *    ARMA DATOS DE EMBARGOS
           MOVE JURAD-EMBARGOS-VIGENTES TO D-VIGENTES
           MOVE JURAD-EMBARGOS-TERMINADOS TO D-TERMINADOS
           MOVE JURAD-EMBARGOS-FECHA-ULTIMO TO D-FECHA-ULTIMA
           PERFORM 4300-MODIFICAR-BDIIALE-DATOS.


       4300-MODIFICAR-BDIIALE-DATOS.
      ******************************************************************
      * PARTE DE LA CARGA DE DATOS EN EL REGISTRO BDIIALE Y GRABACION
      * DEL MISMO QUE ES COMUN A DEMANDAS Y EMBARGOS
      * EL DATO LLASEC-BDIIALE YA TIENE "J001" O "J002"
      ******************************************************************
      *    COMPLETA LA CLAVE E INTENTA LEER LA ALERTA, SI SE LEYO OK
      *    EL FILE STATUS DE BDIIALE QUEDA CON "00"
           PERFORM 4333-LEER-BDIIALE-RANDOM

      *    PROCESA SEGUN EL TIPO DE NOVEDAD I M E
           EVALUATE JURAD-TIPO-NOVEDAD
           WHEN "I"
               ADD 1 TO N-REG-TIPO-I-OK
               PERFORM 4300-MODIFICAR-BDIIALE-I
           WHEN "M"
               ADD 1 TO N-REG-TIPO-M-OK
               PERFORM 4300-MODIFICAR-BDIIALE-M
           WHEN "E"
               ADD 1 TO N-REG-TIPO-E-OK
               PERFORM 4300-MODIFICAR-BDIIALE-E
           WHEN OTHER
      *        ESTO ES "IMPOSIBLE" POR CONTROL DE CONSISTENCIA ANTERIOR
               DISPLAY "ERROR DE LOGICA: TIPO-NOVEDAD="
               JURAD-TIPO-NOVEDAD " REG NUM: " N-REG-LEIDOS
               DISPLAY "REGISTRO DE JURAD: " DISPLAY JURAD-REG
               PERFORM 9000-CANCELAR-PROGRAMA
           END-EVALUATE.


       4300-MODIFICAR-BDIIALE-I.
      ******************************************************************
      * INSERCION DE UNA ALERTA NUEVA
      ******************************************************************
      *    SI EL REGISTRO EXISTIA ES UN ERROR
           IF EST-BDIIALE = "00"
               CALL "PONERR" USING "19" JURAD-REG E-ERRORES
               DISPLAY "LLAVE: " LLAVE-BDIIALE
               ADD 1 TO N-ALE-YA-EXISTE
               EXIT PARAGRAPH
           END-IF

      *    COMPLETA LOS DATOS DEL REGISTRO
           INITIALIZE ENCABEZADO-BDIIALE
           MOVE P-FECHA-PROCESO-YYYYMMDD TO FEC-NOV-BDIIALE
           FEC-REP-BDIIALE
           MOVE P-FECHA-PROCESO-50 TO FEC-VEN-BDIIALE
           MOVE 0 TO BLOQUEO-BDIIALE
           MOVE D-DATAHOST TO INF-DATAHOST-BDIIALE
           MOVE SPACES TO LINEAS-BDIIALE
           PERFORM 4333-GRABAR-BDIIALE.


       4300-MODIFICAR-BDIIALE-M.
      ******************************************************************
      * MODIFICACION DE UNA ALERTA EXISTENTE
      ******************************************************************
      *    SI EL REGISTRO NO EXISTIA ES UN ERROR
           IF EST-BDIIALE NOT = "00"
               CALL "PONERR" USING "17" JURAD-REG E-ERRORES
               "LLAVE: " LLAVE-BDIIALE
               ADD 1 TO N-ALE-NO-EXISTE
               EXIT PARAGRAPH
           END-IF

      *    REEMPLAZA LA FECHA DE REPORTE Y LOS VALORES
           MOVE P-FECHA-PROCESO-YYYYMMDD TO FEC-REP-BDIIALE
           MOVE D-DATAHOST TO INF-DATAHOST-BDIIALE
           MOVE SPACES TO LINEAS-BDIIALE
           PERFORM 4333-REGRABAR-BDIIALE.


       4300-MODIFICAR-BDIIALE-E.
      ******************************************************************
      * ELIMINACION DE UNA ALERTA
      ******************************************************************
      *    ELIMINA EL REGISTRO
           IF EST-BDIIALE = "00"
               PERFORM 4333-ELIMINAR-BDIIALE
           ELSE
               CALL "PONERR" USING "18" JURAD-REG E-ERRORES
               "LLAVE: " LLAVE-BDIIALE
               ADD 1 TO N-ALE-NO-EXISTE
           END-IF.


       4333-LEER-BDIIALE-RANDOM.
      ******************************************************************
      * ARMA CLAVE EXACTA Y LEE UN REGISTRO DE BDIIALE
      * USA LOS DATOS DEL REG INPUT, EXCEPTO LLASEC QUE ESTA PRECARGADO
      * CUANDO PUDO LEER OK EL FILE STATUS EST-BDIIALE QUEDA CON "00"
      ******************************************************************
           MOVE JURAD-ID-TIPO TO TIP-IDE-BDIIALE
           MOVE JURAD-ID-NUMERO TO NUM-IDE-BDIIALE
           MOVE P-COD-SUSCRIPTOR TO FUENTE-BDIIALE
           MOVE 000 TO COD-ALERTA-BDIIALE
      *    LLASEC-BDIIALE FUÉ PREVIAMENTE CARGADO CON J001 O J002

           READ BDIIALE
           EVALUATE EST-BDIIALE
           WHEN "00"
               CONTINUE
           WHEN "23"
               CONTINUE
           WHEN OTHER
               DISPLAY "ERROR AL LEER BDIIALE FS: " EST-BDIIALE
               " KEY: " LLAVE-BDIIALE
               DISPLAY "ULTIMO REGISTRO PROCESADO: "
               DISPLAY JURAD-REG-ANTERIOR
               PERFORM 9000-CANCELAR-PROGRAMA
           END-EVALUATE.

       4333-GRABAR-BDIIALE.
      ******************************************************************
      * INSERTA UN REGISTRO NUEVO EN BDIIALE
      ******************************************************************
           IF NOT PROCESO-REAL
               ADD 1 TO N-ALE-INSERTADAS
               EXIT PARAGRAPH
           END-IF
           WRITE REG-BDIIALE
           EVALUATE EST-BDIIALE
           WHEN "00"
               ADD 1 TO N-ALE-INSERTADAS
           WHEN OTHER
               DISPLAY "ERROR AL GRABAR BDIIALE FS: " EST-BDIIALE
               " KEY: " LLAVE-BDIIALE
               DISPLAY "REGISTRO BDIIALE: "
               DISPLAY REG-BDIIALE
               PERFORM 9000-CANCELAR-PROGRAMA
           END-EVALUATE.


       4333-REGRABAR-BDIIALE.
      ******************************************************************
      * REGRABA UN REGISTRO MODIFICADO EN BDIIALE
      ******************************************************************
           IF NOT PROCESO-REAL
               ADD 1 TO N-ALE-MODIFICADAS
               EXIT PARAGRAPH
           END-IF
           REWRITE REG-BDIIALE
           EVALUATE EST-BDIIALE
           WHEN "00"
               ADD 1 TO N-ALE-MODIFICADAS
           WHEN OTHER
               DISPLAY "ERROR AL REGRABAR BDIIALE FS: " EST-BDIIALE
               " KEY: " LLAVE-BDIIALE
               DISPLAY "REGISTRO BDIIALE: "
               DISPLAY REG-BDIIALE
               PERFORM 9000-CANCELAR-PROGRAMA
           END-EVALUATE.

       4333-ELIMINAR-BDIIALE.
      ******************************************************************
      * ELIMINA UN REGISTRO DE BDIIALE
      ******************************************************************
           IF NOT PROCESO-REAL
               ADD 1 TO N-ALE-ELIMINADAS
               EXIT PARAGRAPH
           END-IF
           DELETE BDIIALE
           EVALUATE EST-BDIIALE
           WHEN "00"
               ADD 1 TO N-ALE-ELIMINADAS
           WHEN OTHER
               DISPLAY "ERROR AL DELETEAR BDIIALE FS: " EST-BDIIALE
               " KEY: " LLAVE-BDIIALE
               DISPLAY "REGISTRO BDIIALE: "
               DISPLAY REG-BDIIALE
               PERFORM 9000-CANCELAR-PROGRAMA
           END-EVALUATE.

       4800-PROCESO-REG-T.
      ******************************************************************
      * EL REGISTRO LEIDO EN EL ARCHIVO JURAD ES TIPO "T" CON TOTALES
      * DE CONTROL, Y DEBERÍA SER EL ULTIMO DEL ARCHIVO
      ******************************************************************
      * CONTROL DE LOS TOTALES CALCULADOS CONTRA LOS ESPERADOS
           MOVE "NO" TO E-HAY-ERROR
      *    CANTIDAD DE REGISTROS I M E
           IF FUNCTION NUMVAL(JURADT-REGISTROS) = N-REG-LEIDOS
               CONTINUE
           ELSE
               DISPLAY " "
               DISPLAY "NO COINCIDE LA CANTIDAD DE REGISTROS:"
               JURADT-REGISTROS " CON LOS LEIDOS:" N-REG-LEIDOS
               MOVE "SI" TO E-HAY-ERROR
           END-IF.

       4900-GRABAR-ERROR.
      ******************************************************************
      * EL REGISTRO DE JURAD TIENE ERROR: SE GRABA EN EL ARCHIVO ERRS
      * CON INDICACION DEL ERROR DETECTADO
      ******************************************************************
           MOVE JURAD-REG TO ERRS-REG
      *    LISTA DE LOS CODIGOS DE LOS ERRORES DEL REGISTRO
           MOVE E-ERRORES-REG TO ERRS-CAUSA-RECHAZO
           WRITE ERRS-REG
           EVALUATE ERRS-FS
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR AL GRABAR EL ARCHIVO DE ERRORES "
                   DISPLAY "NOMBRE DEL ARCHIVO: " ERRS-FILE-NAME
                   DISPLAY "FILE STATUS: " ERRS-FS
                   PERFORM 9000-CANCELAR-PROGRAMA
                   GOBACK
           END-EVALUATE.


       1000-RUTINA-INICIAL.
      ******************************************************************
      * Todas las operaciones que se realizan una sola vez, al comienzo
      * del programa:
      *   - obtencion de los parametros
      *   - apertura del archivo de entrada
      *   - apertura de todos los demas archivos
      *   - eliminacion de datos existentes si es refresque completo
      *   - carga en memoria de los IDs de inconsistentes
      ******************************************************************
      * Registra hora de inicio
           ACCEPT W-HORA-INICIO FROM TIME

      * Obtiene informacion sobre la corrida de la linea de comando: el
      * nombre del archivo de entrada (que contiene el tipo de proceso),
      * el indicador de proceso real o en falso (default)
           ACCEPT P-COMMAND-LINE FROM COMMAND-LINE
      D    MOVE "0000011000905140104.txt 2" to P-COMMAND-LINE
           UNSTRING P-COMMAND-LINE DELIMITED BY ALL " " INTO
           P-FILE-NAME P-PROCESO-REAL
           DISPLAY "PARAMETROS: " P-COMMAND-LINE

      * El programa se anuncia en la consola
           IF PROCESO-REAL
               DISPLAY PGM " CARGA DE ARCHIVO JURAD - PROCESO REAL"
           ELSE
               DISPLAY PGM " CARGA DE ARCHIVO JURAD - PROCESO EN FALSO"
           END-IF

      * El indicador de proceso real debe ser 1 o 2
           IF P-PROCESO-REAL NOT = "1" AND "2"
               DISPLAY "EL INDICADOR DE PROCESO REAL ES INVALIDO"
               DISPLAY "DEBE SER 1 O 2"
               PERFORM 9000-CANCELAR-PROGRAMA
           END-IF

      * Control del tipo de proceso, del nombre del archivo
           EVALUATE P-TIPO-PROCESO
           WHEN "01"
               DISPLAY "01 ENVIO ORIGINAL - CARGA TOTAL 1A VEZ"
           WHEN "02"
               DISPLAY "02 ARCHIVO SEMANAL - CARGA PARCIAL"
           WHEN "03"
               DISPLAY "03 ARCHIVO MENSUAL - CARGA PARCIAL"
           WHEN "04"
               DISPLAY "04 REFRESQUE TOTAL - CARGA TOTAL"
           WHEN OTHER
      *        valor invalido
               DISPLAY "EL TIPO DE PROCESO " P-TIPO-PROCESO
               " ES INVALIDO (DEBE SER 01 02 03 O 04)"
               PERFORM 9000-CANCELAR-PROGRAMA
           END-EVALUATE.

      * Edita la fecha de proceso DDMMAA del parametro en formato
      * YYYYMMDD
           STRING "20" P-FECHA-PROCESO(5:6) P-FECHA-PROCESO(3:4)
           P-FECHA-PROCESO(1:2) DELIMITED SIZE
           INTO P-FECHA-PROCESO-YYYYMMDD
           DISPLAY "FECHA DE PROCESO: " P-FECHA-PROCESO-YYYYMMDD

      * Calcula una fecha 50 anios posterior a la de proceso
           MOVE P-FECHA-PROCESO-YYYYMMDD TO P-FECHA-PROCESO-50
           ADD 50 TO P-FECHA-PROCESO-50YYYY

      * Apertura del archivo de entrada
           STRING "$TEMPORALES/" P-FILE-NAME INTO P-FILE-PATH
           OPEN INPUT JURAD
           EVALUATE JURAD-FS
               WHEN "00"
                    CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR EN OPEN DEL ARCHIVO JURAD "
                   DISPLAY "PATH DEL ARCHIVO: " P-FILE-PATH
                   DISPLAY "FILE STATUS: " JURAD-FS
                   PERFORM 9000-CANCELAR-PROGRAMA *> NO RETORNA
                   GOBACK
           END-EVALUATE.

      * Apertura del archivo de errores
           STRING "$TEMPORALES/" P-FILE-NAME(1:19) ".ERRS"
           INTO ERRS-FILE-NAME
           OPEN OUTPUT ERRS
           EVALUATE ERRS-FS
               WHEN "00"
                    CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR EN OPEN DEL ARCHIVO DE ERRORES "
                   DISPLAY "NOMBRE DEL ARCHIVO: " ERRS-FILE-NAME
                   DISPLAY "FILE STATUS: " ERRS-FS
                   PERFORM 9000-CANCELAR-PROGRAMA *> NO RETORNA
                   GOBACK
           END-EVALUATE.

      * APERTURA DE LOS ARCHIVOS MAESTROS
      * SI ALGÚN OPEN FALLA EL PROGRAMA CANCELA
           OPEN INPUT BDIIREGI
           OPEN INPUT BDIIDIAN
           OPEN INPUT BDIIDAS.

      * APERTURA DEL ARCHIVO DE ALERTAS EN LINEA
           IF PROCESO-REAL
               OPEN I-O BDIIALE
           ELSE
               OPEN INPUT BDIIALE
           END-IF.

      * SI ES UN REFRESQUE TOTAL (CODIGO 04) SE ELIMINAN LOS REGISTROS
      * DE JURAD EXISTENTES
           IF P-TIPO-PROCESO = "04" AND PROCESO-REAL
               PERFORM 1001-ELIMINAR-TODO
           END-IF.


       1001-ELIMINAR-TODO.
      ******************************************************************
      * ELIMINA LOS REGISTROS DE ALERTAS EXISTENTES EN EL ARCHIVO DE
      * ALERTAS EN LINEA, PREVIO A UN REFRESQUE TOTAL
      ******************************************************************
           DISPLAY "ELIMINANDO TODA LA INFORMACION CON FUENTE JURAD"
      *    POSICIONA EN EL PRIMER REG DE JURAD
           MOVE ZERO TO TIP-IDE-BDIIALE
           MOVE ZERO TO NUM-IDE-BDIIALE
           MOVE P-COD-SUSCRIPTOR TO FUENTE-BDIIALE
           MOVE ZEROS TO COD-ALERTA-BDIIALE
           MOVE LOW-VALUES TO LLASEC-BDIIALE
           START BDIIALE KEY > LLAVE-BDIIALE
           EVALUATE EST-BDIIALE
           WHEN "00"
      *        POSICIONADO
               CONTINUE
           WHEN OTHER
      *        NO HAY REGISTROS
               DISPLAY "NO HAY REGISTROS A ELIMINAR FS:" EST-BDIIALE
               EXIT PARAGRAPH
           END-EVALUATE.

      *    LEE SECUENCIALMENTE Y ELIMINA LOS REGISTROS DE JURAD
      *    HASTA EL FIN DEL ARCHIVO
           PERFORM UNTIL EST-BDIIALE NOT = "00"
               READ BDIIALE NEXT RECORD
               EVALUATE EST-BDIIALE
               WHEN "00"
                   IF FUENTE-BDIIALE = P-COD-SUSCRIPTOR
      *                ES UN ALERTA DE JURAD
                       DELETE BDIIALE
                       IF EST-BDIIALE = "00"
                           ADD 1 TO N-ALE-BORRADAS
                       ELSE
      *                    ERROR EN LA ELIMINACION
                           DISPLAY "ERROR AL ELIMINAR UN REGISTRO DE "
                           "BDIIALE VACIANDO LOS DATOS"
                           DISPLAY "CLAVE: " LLAVE-BDIIALE
                           DISPLAY "REG BORRADOS: " N-ALE-BORRADAS
                           PERFORM 9000-CANCELAR-PROGRAMA
                       END-IF
                   END-IF
               WHEN "10"
      *            FIN DE ARCHIVO: SALE NORMALMENTE
                   DISPLAY "TERMINO LA ELIMINACION TOTAL DE JURAD"
                   DISPLAY "REG BORRADOS: " N-ALE-BORRADAS
               WHEN OTHER
      *            LECTURA ANORMAL: CANCELACION DEL PROGRAMA
                   DISPLAY "ERROR EN LECTURA DEL ARCHIVO DE ALERTAS "
                   "ONLINE"
                   DISPLAY "FILE STATUS: " EST-BDIIALE
                   PERFORM 9000-CANCELAR-PROGRAMA *> NO RETORNA
               END-EVALUATE
           END-PERFORM.


       7000-RUTINA-FINAL.
      ******************************************************************
      * Registra hora de finalizacion
           ACCEPT W-HORA-FIN FROM TIME

      * Leer JURAD a ver si hay registros despues del "T"

      * Close todos los archivos
           CLOSE JURAD ERRS BDIIREGI BDIIDIAN BDIIDAS BDIIALE.
           DISPLAY "FECHA DE PROCESO: " P-FECHA-PROCESO-YYYYMMDD
           display " "
      * Totales tipo PESVNO
           DISPLAY "**************************************"
           DISPLAY "        VALIDACION REGISTROS          "
           DISPLAY "  BDIIREGIS BDIIDIAN BDIIDAS BDACSV   "
           DISPLAY "**************************************"
           DISPLAY " "
           DISPLAY "REGISTROS MAL FORMADOS          :" REG-MAL-FORMADOS
           DISPLAY "REGISTROS LEIDOS                :" REG-LEIDOS
           DISPLAY "REGISTROS CORRECTOS             :" REG-CORRECTOS
           DISPLAY "REGISTROS CUENTAS SIN VALIDAR   :" SI-EXISTE-CSV
           DISPLAY "REGISTROS NO COINCIDEN BDIIREGI :" NO-COINCIDE-REGIS
           DISPLAY "REGISTROS DE FALLECIDOS         :" CONT-FALLECIDOS
           DISPLAY "REGISTROS NO COINCIDEN BDIIDIAN :" NO-COINCIDE-DIAN
           DISPLAY "REGISTROS NO EXISTEN   BDIIREGI :" NO-EXISTE-REGIS
           DISPLAY "REGISTROS NO EXISTEN   BDIIDIAN :" NO-EXISTE-DIAN
           DISPLAY "TIPO IDENTIFICACION INVALIDO    :" TIPO-ID-INVALIDO
           DISPLAY " "
           DISPLAY "**************************************"
           DISPLAY "        PORCENTAJE REGISTROS          "
           DISPLAY "            PROCESADOS  OK            "
           DISPLAY "**************************************"
      *    PORCENTAJE REGISTROS PROCESADOS OK

           COMPUTE N-EDITP100 ROUNDED = 100.0 * N-REG-PROCESADOS
           / N-REG-LEIDOS
           DISPLAY N-EDITP100 "% DE REGISTROS CORRECTOS"
           MOVE N-REG-PROCESADOS TO N-EDIT
           DISPLAY N-EDIT "REGISTROS PROCESADOS SIN ERROR"
           IF P-TIPO-PROCESO = "04"
               MOVE N-ALE-BORRADAS TO N-EDIT
               DISPLAY N-EDIT
               "ALERTAS BORRADAS PREVIO AL REFRESQUE TOTAL"
           END-IF
           MOVE N-ALE-INSERTADAS TO N-EDIT
           DISPLAY N-EDIT "ALERTAS INSERTADAS"
           MOVE N-ALE-MODIFICADAS TO N-EDIT
           DISPLAY N-EDIT "ALERTAS MODIFICADAS"
           MOVE N-ALE-ELIMINADAS TO N-EDIT
           DISPLAY N-EDIT "ALERTAS ELIMINADAS"
           MOVE N-ALE-YA-EXISTE TO N-EDIT
           DISPLAY N-EDIT "INSERCION DE ALERTA EXISTENTE"
           MOVE N-ALE-NO-EXISTE TO N-EDIT
           DISPLAY N-EDIT "MODIF O ELIM DE ALERTA INEXISTENTE".

           DISPLAY " "
           DISPLAY "**************************************"
           DISPLAY "            REPORTE ERRORES           "
           DISPLAY "**************************************"
      * EMISION DEL REPORTE DE ERRORES
           DISPLAY " "
           DISPLAY "TOTALES POR TIPO DE ERROR DETECTADO:"
           PERFORM VARYING EX FROM 1 BY 1 UNTIL EX > 50
           OR E-CODIGO(EX) = "00"
               IF E-CONTADOR(EX) NOT = 0
                   MOVE E-CONTADOR(EX) TO E-EDIT
                   DISPLAY E-EDIT E-CODIGO(EX) "-" E-DESCRIPCION(EX)
               END-IF
           END-PERFORM.


       9000-CANCELAR-PROGRAMA.
      ******************************************************************
      * Procedimiento standard de cancelacion de programa batch
      ******************************************************************
           DISPLAY " "
           DISPLAY PGM " - PROCESO CANCELADO"
           GOBACK RETURNING 99.


       513-VALIDA-EN-CSV.
      **************************************************************
      * Valida si la identificación está en la base de datos alterna
      **************************************************************
      *    HACE VALIDACION DE TIPO DE IDENTIFICACION
           MOVE TIPOID-E TO TIP-IDE-BDIIVIDE
           MOVE NUMID-E TO NUM-IDE-BDIIVIDE
      *    RETORNA SW-TID-BDIIVIDE=0 SI EL NUM ID NO ESTÁ DENTRO
      *    DEL RANGO QUE CORRESPONDE AL TIPO DE ID
           PERFORM 80400-VALIDAR-TIPO-ID
           IF SW-TID-BDIIVIDE = 0
      *        EL TIPO DE ID ES OK
               IF HACER-MATCH = ZEROS
                   MOVE TIPOID-E TO TIP-IDE-INPUT-BDAMATCH
                   MOVE NUMID-E  TO NUM-IDE-INPUT-BDAMATCH
      *            SE RECORRE TODA LA ESTRUCTURA BDACSV PARA LA
      *            IDENTIFICACION BUSCADA. EN CASO DE HALLAR
      *            REGISTROS SE PROCEDE A REALIZAR EL MATCH
                   PERFORM 1000-U-RECORRER-CSV-BDAMATCH
                   IF IDE-TIPO-BDAMATCH = 1
                       IF TIPOID-E = 1 OR 4
                           INSPECT NOM-OUTPUT-BDAMATCH REPLACING ALL
                           "," BY " ", ";" BY " "
                       END-IF
                       MOVE NOMBRE-E TO NOMBRE-INPUT-VALNOM
                       MOVE NOM-OUTPUT-BDAMATCH TO NOMBOK-INPUT-VALNOM
                       MOVE TIPOID-E TO  TIP-ID-INPUT-VALNOM
                       MOVE NUM-BLOQUES-BDIIVIDE TO BLOQUES-INPUT-VALNOM
                       PERFORM 10000-MATCH-NOMBRES-VALNOM
                       IF MATCH-OK-OUT-VALNOM = 1
                           MOVE 1 TO IDE-TIPO-BDAMATCH
                       END-IF
                   END-IF

                   IF IDE-TIPO-BDAMATCH = 1
                       ADD 1 TO SI-EXISTE-CSV
                   END-IF
               END-IF
           END-IF.

       9800-FILTRAR-ALFABETICO.
      ******************************************************************
      * SE ELIMINAN CARACTERES NO ALFABETICOS A LA CADENA DE ENTRADA
      ******************************************************************
           MOVE FUNCTION UPPER-CASE (CADENA-ENTRA) TO CADENA-ENTRA
           INITIALIZE CADENA-SALE
           MOVE 1 TO J
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 60
              MOVE CADENA-ENTRA (I:1) TO IND-ALFABETICO
              IF SI-ALFABETICO
                 MOVE CADENA-ENTRA (I:1) TO CADENA-SALE (J:1)
                 ADD 1 TO J
              END-IF
           END-PERFORM.
      ******************************************************************

           COPY BDIIVIDE.PROC.
           copy BDLDIR.PROC.
           COPY BDAMATCH.PROC.
           COPY VALNOM.PROC.
      /
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
      * REGISTRO DE INPUT (PRIMEROS 86 CARACTERES)
       01  REG-INPUT-ANTERIOR           PIC X(86) VALUE LOW-VALUES.
      * POSICION DEL PROXIMO ERROR EN LA LISTA DE ERRORES DEL REG
       01  ERRINDEX                     PIC 999 COMP.
      ******************************************************************

       LINKAGE SECTION.
      ******************************************************************
      * CODIGO DEL ERROR DETECTADO
       01  COD-ERROR                    PIC XX.
      ******************************************************************
      * REGISTRO DE INPUT (PRIMEROS 86 CARACTERES)
       01  REG-INPUT                    PIC X(102).
      ******************************************************************
      * TABLA DE ERRORES DE CONSISTENCIA DEL INPUT
       01 E-ERRORES.
      *    INDICA SI EL REGISTRO ACTUAL TIENE ERROR
           03 E-HAY-ERROR               PIC XX.
      *    INDICA SI EL REGISTRO ACTUAL IMPACTA LA BASE DE DATOS
           03 E-IMPACTAR-BDIIALE        PIC XX.
      *    ERRORES DEL REGISTRO ACTUAL
           03 E-ERRORES-REG             PIC X(300).
      *    CONTADOR DE REGISTROS CON ERROR
           03 E-CONT-REGS-CON-ERROR     PIC 9(8).
      *    TABLA DE ERRORES
           03 E-ERROR                   OCCURS 50 INDEXED EX.
               05 E-CODIGO              PIC XX.
               05 E-IMPACTAR            PIC X.
               05 E-CONTADOR            PIC 9(7).
               05                       PIC X.
               05 E-DESCRIPCION         PIC X(59).

      ******************************************************************

       PROCEDURE DIVISION USING COD-ERROR REG-INPUT E-ERRORES.
       00.
      *    CUENTA Y MUESTRA EL REGISTRO DE INPUT CON ERROR
           IF E-HAY-ERROR NOT = "SI"
               ADD 1 TO E-CONT-REGS-CON-ERROR
               DISPLAY " "
               DISPLAY REG-INPUT
               MOVE REG-INPUT TO REG-INPUT-ANTERIOR
               MOVE SPACES TO E-ERRORES-REG
               MOVE 2 TO ERRINDEX
           END-IF

      *    MARCA AL REG ACTUAL COMO ERRONEO
           MOVE "SI" TO E-HAY-ERROR

      *    CONTABILIZA EL ERROR EN LA TABLA DE ERRORES
           SET EX TO 1
           SEARCH E-ERROR VARYING EX
           AT END
               DISPLAY "--- FALLA EN LA TABLA DE ERRORES ---" COD-ERROR
           WHEN E-CODIGO(EX) = "00"
               ADD 1 TO E-CONTADOR(EX)
               DISPLAY "--- ERROR TIPO EXOTICO: " COD-ERROR
               ADD 1 TO E-CONTADOR(EX)
           WHEN E-CODIGO(EX) = COD-ERROR
               ADD 1 TO E-CONTADOR(EX)
               DISPLAY "ERROR " COD-ERROR " " E-DESCRIPCION(EX)
      *        si el error está marcado para impactar BDIIALE o no
               IF E-IMPACTAR(EX) NOT = "+"
                   MOVE "NO" TO E-IMPACTAR-BDIIALE
               END-IF
           END-SEARCH

      *    AGREGA A LA LISTA DE ERRORES DEL REGISTRO
           IF ERRINDEX < (300 - 59 - 3)
               MOVE COD-ERROR TO E-ERRORES-REG(ERRINDEX:2)
               ADD 3 TO ERRINDEX
               MOVE E-DESCRIPCION(EX) TO E-ERRORES-REG(ERRINDEX:59)
               ADD 59 TO ERRINDEX
           END-IF

           EXIT PROGRAM.


       END PROGRAM PONERR.

       END PROGRAM CARGAJUR.

      ******************************************************************
      ******************************************************************
