      $SET LIST SETTING 
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ACTALEJUR.
      ******************************************************************
      * Actualizacion del archivo de alertas con los datos provistos
      * por Jurad
      * Procesa el archivo JURAD. Controla cada registro: si es OK usa
      * los datos para actualizar BDIIALE, si tiene defecto lo graba en
      * el archivo ERRS.
      ******************************************************************
       AUTHOR.        Globant JL.
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION section.
       SOURCE-COMPUTER. microfocus.
       OBJECT-COMPUTER. microfocus.
       INPUT-OUTPUT section.
       FILE-CONTROL.

      ******************************************************************
      ******************************************************************
       select JURAD assign to P-file-path
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

      ******************************************************************
      ******************************************************************
           copy BDIIDIAN.FS.
 
      ******************************************************************
      ******************************************************************
           copy BDIIDAS.FS.

      ******************************************************************
      ******************************************************************
           copy BDIIALE.FS replacing == on record.== by == on record
              file status is EST-BDIIALE.==.

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
           03 JURAD-tipo-novedad        pic x.
      *    Tipo de identificacion Numero {1, 2, 3, 4}
           03 JURAD-id-tipo             pic x.
      *    Numero de identificacion
           03 JURAD-id-numeroX          pic x(11).
           03 JURAD-id-numero           redefines JURAD-id-numeroX
                                        pic 9(11).
      *    Nombres y Apellidos
           03 JURAD-nombre              pic x(45).
      *    Numero de demandas vigentes
           03 JURAD-demandas-vigentes   pic 999.
      *    Numero de demandas terminadas
           03 JURAD-demandas-terminadas pic 999.
      *    Fecha ultima demanda vigente YYYYMMDD
           03 JURAD-demandas-fecha-ultima pic x(8).
      *    Numero de embargos vigentes
           03 JURAD-embargos-vigentes   pic 999.
      *    Numero de embargos terminados
           03 JURAD-embargos-terminados pic 999.
      *    Fecha ultimo embargo vigente YYYYMMDD
           03 JURAD-embargos-fecha-ultimo pic x(8).

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
      * Archivo de salida con los registros defectuosos del arch JURAD
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
           03 ERRS-demandas-fecha-ultima pic x(8).
      *    Numero de embargos vigentes
           03 ERRS-embargos-vigentes    pic 999.
      *    Numero de embargos terminados
           03 ERRS-embargos-terminados  pic 999.
      *    Fecha ultimo embargo vigente YYMMDD
           03 ERRS-embargos-fecha-ultimo pic x(8).
      *    Causa del rechazo
           03 ERRS-causa-rechazo        pic x(100).

      ******************************************************************
      * Contiene los ids y nombres de personas
      ******************************************************************
       copy BDIIREGI.FD.

      ******************************************************************
      * Maestro de personas juridicas nacionales y extranjeras reportada
      * por la DIAN
      ******************************************************************
       copy BDIIDIAN.FD.

      ******************************************************************
      * Archivo maestro de cedulas de extranjeria suministrado por DAS
      ******************************************************************
       copy BDIIDAS.FD.

      ******************************************************************
      * Maestro de alertas reportadas por suscriptores
      ******************************************************************
       copy BDIIALE.FD.

      ******************************************************************
       WORKING-STORAGE section.
      ******************************************************************
      * Nombre del programa
       01  pgm                          pic x(10) value "CARGAJUR".

      ******************************************************************
      * Variables para rutina de validacion de identificaciones
      ******************************************************************
       copy BDIIVIDE.WS.

      ******************************************************************
      * Variables relacionadas con las alertas del sistema
      ******************************************************************
       copy BDIIALE.WS.

      ******************************************************************
      * Datos para las rutinas de validacion de nombres de VALNOM.PROC
      ******************************************************************
       copy VALNOM.WS.

      *****************************************************************
      * Direcciones de los archivos en el file system
      *****************************************************************
       copy ICFILE.WS.

      ******************************************************************
      * Indica que se termino el proceso, por agotamiento del archivo
      * de entrada (normal) o alguna otra condicion (error)
       01  IND-fin-proceso              pic xx value "NO".

      * Copia del ultimo reg de input, para mensajes (Ult reg: ...)
       01  JURAD-reg-anterior           pic x(100) value "(vacio)".
       01  JURAD-fs                     pic xx value spaces.

       01  ERRS-fs                      pic xx value spaces.

      * Datos de demandas o embargos que van en el registro BDIIALE
       01 D-datahost.
           05 D-vigentes                pic 9(5).
           05 D-terminados              pic 9(5).
           05 D-fecha-ultima            pic x(8).
      ******************************************************************
      * Parametros iniciales, de la linea de comando
      * La linea de comando que contiene los parametros
       01 P-command-line                pic x(80) value spaces.
       01 P-parametros.
      *    Proceso real: contiene "S" para activar 
      * TODO: fijo en "NO" para pruebas
           03 P-proceso-real            pic xx value "N".
           88 PROCESO-REAL              value "S".

      *    Nombre del archivo de entrada JJJJJJaaaDDMMAAbbXX.txt
           03 P-file-name               pic x(80) value spaces.
           03 P-file-name-R             redefines P-file-name.
      *        JJJJJJ = Codigo de suscriptor JURAD en Experian
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
           03 P-file-path               pic x(80) value spaces.
      * Nombre del archivo de errores JJJJJJaaaDDMMAAbbXX_ERRORES.txt
          03 ERRS-file-name             pic x(80) value spaces.

      * Fecha de proceso editada YYYYMMDD
       01 P-fecha-proceso-YYYYMMDD      pic x(8).
      * Fecha de proceso + 50 años
       01 P-fecha-proceso-50.
           03  P-fecha-proceso-50YYYY   pic 9999.
           03                           pic x(4).
      ******************************************************************
      * Datos para el reporte final
       01 W-tiempo.
           03 W-hora-inicio             pic 9(8).
           03 W-hora-fin                pic 9(8).
       01 N-edit                        pic z(8)B.
       01 N-contadores.
      *    Registros en el archivo de entrada
           03 N-reg-leidos              pic 9(8) value zero.
      *    Registros procesados sin error
           03 N-reg-OK                  pic 9(8) value zero.
      *    Registros con error
           03 N-reg-con-error           pic 9(8) value zero.

      *    Cantidades de registros por tipo de novedad
           03 N-reg-tipo-I-OK           pic 9(8) value zero.
           03 N-reg-tipo-M-OK           pic 9(8) value zero.
           03 N-reg-tipo-E-OK           pic 9(8) value zero.
           03 N-reg-tipo-I-err          pic 9(8) value zero.
           03 N-reg-tipo-M-err          pic 9(8) value zero.
           03 N-reg-tipo-E-err          pic 9(8) value zero.

      *    Alertas borradas previo al refresque total
           03 N-ale-borradas            pic 9(8) value zero.
      *    Alertas insertadas
           03 N-ale-insertadas          pic 9(8) value zero.
      *    Alertas modificadas
           03 N-ale-modificadas         pic 9(8) value zero.
      *    Alertas eliminadas
           03 N-ale-eliminadas          pic 9(8) value zero.
      *    Insercion de alerta existente
           03 N-ale-ya-existe           pic 9(8) value zero.
      *    Modificacion o eliminacion de alerta inexistente
           03 N-ale-no-existe           pic 9(8) value zero.

      *    Sumas de cantidades de demandas y embargos
           03 N-demandas-vigentes       pic 9(5).
           03 N-demandas-terminadas     pic 9(5).
           03 N-embargos-vigentes       pic 9(5).
           03 N-embargos-terminados     pic 9(5).

      ******************************************************************
      * Armado de las lineas de alerta
       01 LA-lineas-alerta.
      *    Los modelos a continuacion deberan obtenerse de un 
      *    archivo, como BDIIALE-ALERTAS
           03 LAend                     pic 999 comp value 0.
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
               05 pic x(50) value high-values. *> end marker
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
      *    conveniente que informe sobre el tamanio del contenido.
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
               05 LA-valor              pic x(50) 
                                        occurs 10 indexed LAv.
      *    Datos para editar valores a insertar
           03 LA-num                    pic -(11)0.
           03 LA-numX                   redefines LA-num
                                        pic x(12).
           03 LA-descarte               pic x(50).


      ******************************************************************
      * Errores de consistencia del input
      * Cada error tiene un codigo de 2 digitos, un espacio, contador
      * de ocurrencias de 7 digitos, un espacio, y descripcion de 59
       01 E-errores.
      *    indica si el registro actual tiene error
           03 E-hay-error               pic xx value "NO".
      *    errores del registro actual
           03 E-errores-reg             pic x(100) value spaces.
      *    contador de registros con error
           03 E-cont-regs-con-error     pic 9(8) value zero.
      *    tabla de errores
           03 pic x(11) value "01 0000000 ".
           03 pic x(59) value
           "Tipo de movimiento invalido, debe ser I M E T".
           03 pic x(11) value "02 0000000 ".
           03 pic x(59) value
           "Tipo de identificacion invalido, debe ser 1 2 3 4".
           03 pic x(11) value "03 0000000 ".
           03 pic x(59) value
           "Numero de identificacion invalido, debe ser numerico".
           03 pic x(11) value "04 0000000 ".
           03 pic x(59) value
           "Tipo de identificacion debe ser mayor que cero".
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
           "Fecha ultima demanda vigente YYYYMMDD: debe ser numerica".
           03 pic x(11) value "09 0000000 ".
           03 pic x(59) value
           "Fecha ultima demanda vigente YYYYMMDD: fuera de rango".
           03 pic x(11) value "10 0000000 ".
           03 pic x(59) value
           "Numero de embargos vigentes: debe ser numerico".
           03 pic x(11) value "11 0000000 ".
           03 pic x(59) value
           "Numero de embargos terminados: debe ser numerico".
           03 pic x(11) value "12 0000000 ".
           03 pic x(59) value
           "Fecha ultimo embargo vigente YYYYMMDD: debe ser numerica".
           03 pic x(11) value "13 0000000 ".
           03 pic x(59) value
           "Fecha ultimo embargo vigente YYYYMMDD: fuera de rango".
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
           03                           pic x(100).
           03                           pic 9(8).
           03 E-error                   occurs 50 indexed Ex.
               05 E-codigo              pic xx.
               05                       pic x.
               05 E-contador            pic 9(7).
               05                       pic x.
               05 E-descripcion         pic x(59).
       01 E-edit                        pic z(8)B.

      ******************************************************************
      * Control de rangos de valores de las fechas del input
       01 VALIDACIONES-FECHAS.
           02 ANO-VAL-X                 pic 9(04).
              88 ANO-VAL                value 1900 thru 2050.
           02 MES-VAL-X                 pic 9(02).
              88 MES-VAL                value 01   thru 12.
           02 DIA-VAL-X                 pic 9(02).
              88 DIA-VAL                value 01   thru 31.

      /*****************************************************************
      ******************************************************************
       PROCEDURE DIVISION.
      *    0000-PROGRAMA-PRINCIPAL
      *    4000-PROCESAR-REGISTRO
      *    4000-PROCESAR-REGISTRO-2
      *    4100-VALIDACION-FORMAL
      *    4200-VALIDACION-SEMANTICA
      *    4300-MODIFICAR-BDIIALE
      *    4300-MODIFICAR-BDIIALE-DATOS
      *    4300-MODIFICAR-BDIIALE-I
      *    4300-MODIFICAR-BDIIALE-M
      *    4300-MODIFICAR-BDIIALE-E
      *    4333-LEER-BDIIALE-RANDOM
      *    4333-GRABAR-BDIIALE
      *    4333-REGRABAR-BDIIALE
      *    4333-ELIMINAR-BDIIALE
      *    4900-GRABAR-ERROR
      *    4320-ARMAR-TEXTO-ALERTA
      *    1000-RUTINA-INICIAL
      *    1001-ELIMINAR-TODO
      *    7000-RUTINA-FINAL
      *    9000-CANCELAR-PROGRAMA


       0000-PROGRAMA-PRINCIPAL.
      ******************************************************************
      *    inicializa el ambiente y abre los archivos
           perform 1000-RUTINA-INICIAL
      *    procesa cada uno de los registros del archivo JURAD hasta
      *    encontrar el registro "T" de totales
           perform 4000-PROCESAR-REGISTRO until IND-fin-proceso = "SI"
      *    cierra archivos y produce el reporte final
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
               perform 4000-PROCESAR-REGISTRO-2
               move JURAD-reg to JURAD-reg-anterior
           when "10"
      *        fin del archivo de input:
      *        esto es un error, no se encontro el registro "T" antes
      *        de que se terminara el archivo
               display "Falta el reg de totales del archivo JURAD "
               display "Nombre del archivo: " P-file-name
               display "File status: " JURAD-fs
               display "Ultimo registro leido: " N-reg-leidos
               display JURAD-reg-anterior
               perform 9000-CANCELAR-PROGRAMA
           when other
      *        error de lectura: cancelacion del proceso
               display "Error en lectura del archivo JURAD "
               display "Nombre del archivo: " P-file-name
               display "File status: " JURAD-fs
               display "Ultimo registro leido: " N-reg-leidos
               display JURAD-reg-anterior
               perform 9000-CANCELAR-PROGRAMA
           end-evaluate.


       4000-PROCESAR-REGISTRO-2.
      ****************************************************************
      * Controla los datos de un registro de JURAD y si es OK lo usa
      * para actualizar el archivo de alertas en linea
      * La informacion esta leida en JURAD-reg
      ****************************************************************
           move "NO" to E-hay-error

      *    validacion del tipo de movimiento
           if JURAD-tipo-novedad not = "I" and "M" and "E" and "T" 
               call "PONERR" using "01" JURAD-reg E-errores
           end-if

      *    si es el registro "T" hay que senialar el fin de proceso
           if JURAD-tipo-novedad = "T" 
               move "SI" to IND-fin-proceso
               perform 4800-PROCESO-REG-T
               exit paragraph
           end-if

           add 1 to N-reg-leidos

      *    validacion formal de los datos
           perform 4100-VALIDACION-FORMAL
           if E-hay-error = "SI"
               perform 4900-GRABAR-ERROR
               exit paragraph
           end-if

      *    si la validacion formal dio OK se hace validacion semantica
           perform 4200-VALIDACION-SEMANTICA
           if E-hay-error = "SI"
               perform 4900-GRABAR-ERROR
               exit paragraph
           end-if

      *    intento de actualizacion de BDIIALE
           perform 4300-MODIFICAR-BDIIALE
           add 1 to N-reg-OK.



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
           move JURAD-demandas-fecha-ultima to VALIDACIONES-FECHAS
           if not ANO-VAL or not MES-VAL or not DIA-VAL
               call "PONERR" using "09" JURAD-reg E-errores
           end-if
      *    Numero de embargos vigentes
           if JURAD-embargos-vigentes not numeric
               call "PONERR" using "10" JURAD-reg E-errores
           end-if
      *    Numero de embargos terminados
           if JURAD-embargos-terminados not numeric
               call "PONERR" using "11" JURAD-reg E-errores
           end-if
      *    Fecha ultimo embargo vigente YYMMDD
           if JURAD-embargos-fecha-ultimo is not numeric
               call "PONERR" using "12" JURAD-reg E-errores
           end-if
           move JURAD-embargos-fecha-ultimo to VALIDACIONES-FECHAS
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
           end-if
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
      * Se impacta el archivo BDIIALE con una o dos inserciones, 
      * modificaciones o eliminaciones segun haya o no demandas y 
      * embargos
      ******************************************************************
           initialize REG-BDIIALE

      *    si hay demandas
           if JURAD-demandas-vigentes > zero
           or JURAD-demandas-terminadas > zero 
               move "J001" to LLASEC-BDIIALE
      *        arma datos de demandas
               move JURAD-demandas-vigentes to D-vigentes
               move JURAD-demandas-terminadas to D-terminados
               string "20" JURAD-demandas-fecha-ultima 
               delimited size into D-fecha-ultima
               perform 4300-MODIFICAR-BDIIALE-DATOS 
           end-if.

      *    si hay embargos
           if JURAD-embargos-vigentes > zero
           or JURAD-embargos-terminados > zero 
               move "J002" to LLASEC-BDIIALE
      *        arma datos de embargos
               move JURAD-embargos-vigentes to D-vigentes
               move JURAD-embargos-terminados to D-terminados
               string "20" JURAD-embargos-fecha-ultimo
               delimited size into D-fecha-ultima
               perform 4300-MODIFICAR-BDIIALE-DATOS 
           end-if.



       4300-MODIFICAR-BDIIALE-DATOS.
      ******************************************************************
      * Parte de la carga de datos en el registro BDIIALE y grabacion
      * del mismo que es comun a demandas y embargos
      * El dato LLASEC-BDIIALE ya tiene "J001" o "J002"
      ******************************************************************
      *    completa la clave e intenta leer la alerta, si se leyo OK
      *    el file status de BDIIALE queda con "00"
           perform 4333-LEER-BDIIALE-RANDOM

      *    procesa segun el tipo de novedad I M E
           evaluate JURAD-tipo-novedad
           when "I"
               add 1 to N-reg-tipo-I-OK
               perform 4300-MODIFICAR-BDIIALE-I
           when "M"
               add 1 to N-reg-tipo-M-OK
               perform 4300-MODIFICAR-BDIIALE-M
           when "E"
               add 1 to N-reg-tipo-E-OK
               perform 4300-MODIFICAR-BDIIALE-E
           when other
      *        esto es "imposible" por control de consistencia anterior
               display "error de logica: tipo-novedad="
               JURAD-tipo-novedad " reg nÂ°: " N-reg-leidos
               display "Registro de JURAD: " display JURAD-reg
               perform 9000-CANCELAR-PROGRAMA
           end-evaluate.


       4300-MODIFICAR-BDIIALE-I.
      ******************************************************************
      * Insercion de una alerta nueva
      * Si ya existia muestra un mensaje y continua
      ******************************************************************
      *    si el registro existia es un error
           if EST-BDIIALE = "00"
               display "Insercion de alerta nueva que ya existe "
               " key: " LLAVE-BDIIALE
               display "Registro de input: "
               display JURAD-reg
               display "Datos en la alerta: " INF-DATAHOST-BDIIALE
               add 1 to N-ale-ya-existe
           end-if

      *    completa los datos del registro
           initialize ENCABEZADO-BDIIALE
           move P-fecha-proceso-YYYYMMDD to FEC-NOV-BDIIALE
           FEC-REP-BDIIALE 
           move P-fecha-proceso-50 to FEC-VEN-BDIIALE
           move 0 to BLOQUEO-BDIIALE
           move D-datahost to INF-DATAHOST-BDIIALE
           perform 4320-ARMAR-TEXTO-ALERTA
           if EST-BDIIALE = "00"
               perform 4333-REGRABAR-BDIIALE
           else
               perform 4333-GRABAR-BDIIALE
           end-if.


       4300-MODIFICAR-BDIIALE-M.
      ******************************************************************
      * Modificacion de una alerta existente
      * Si no existia muestra un mensaje y continua
      ******************************************************************
      *    si el registro no existia es un error
           if EST-BDIIALE not = "00"
               display "Modificacion de alerta inexistente "
               " key: " LLAVE-BDIIALE
               display "Registro de input: "
               display JURAD-reg
               display "Datos en la alerta: " INF-DATAHOST-BDIIALE
               add 1 to N-ale-no-existe
           end-if
      *    reemplaza la fecha de reporte y los valores
           move P-fecha-proceso-YYYYMMDD to FEC-REP-BDIIALE 
           move D-datahost to INF-DATAHOST-BDIIALE
           perform 4320-ARMAR-TEXTO-ALERTA
           if EST-BDIIALE = "00"
               perform 4333-REGRABAR-BDIIALE
           else
               perform 4333-GRABAR-BDIIALE
           end-if.


       4300-MODIFICAR-BDIIALE-E.
      ******************************************************************
      * Eliminacion de una alerta
      * Si no existia muestra un mensaje y continua
      ******************************************************************
      *    elimina el registro
           if EST-BDIIALE = "00"
               perform 4333-ELIMINAR-BDIIALE
           else
               display "Eliminacion de alerta inexistente "
               " key: " LLAVE-BDIIALE
               display "Registro de input: "
               display JURAD-reg
               add 1 to N-ale-no-existe
           end-if.


       4333-LEER-BDIIALE-RANDOM.
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
               continue
           when "20" thru "29"
               continue
           when other
               display "Error al leer BDIIALE fs: " EST-BDIIALE
               " key: " LLAVE-BDIIALE
               display "Ultimo registro procesado: "
               display JURAD-reg-anterior
               perform 9000-CANCELAR-PROGRAMA
           end-evaluate.


       4333-GRABAR-BDIIALE.
      ******************************************************************
      * Inserta un registro nuevo en BDIIALE
      ******************************************************************
           if not PROCESO-REAL
               exit paragraph
           end-if
           write REG-BDIIALE
           evaluate EST-BDIIALE
           when "00"
               add 1 to N-ale-insertadas
           when other
               display "Error al grabar BDIIALE fs: " EST-BDIIALE
               " key: " LLAVE-BDIIALE
               display "Registro BDIIALE: "
               display REG-BDIIALE
               perform 9000-CANCELAR-PROGRAMA
           end-evaluate.


       4333-REGRABAR-BDIIALE.
      ******************************************************************
      * Regraba un registro modificado en BDIIALE
      ******************************************************************
           if not PROCESO-REAL
               exit paragraph
           end-if
           rewrite REG-BDIIALE
           evaluate EST-BDIIALE
           when "00"
               add 1 to N-ale-modificadas
           when other
               display "Error al regrabar BDIIALE fs: " EST-BDIIALE
               " key: " LLAVE-BDIIALE
               display "Registro BDIIALE: "
               display REG-BDIIALE
               perform 9000-CANCELAR-PROGRAMA
           end-evaluate.


       4333-ELIMINAR-BDIIALE.
      ******************************************************************
      * Elimina un registro de BDIIALE
      ******************************************************************
           if not PROCESO-REAL
               exit paragraph
           end-if
           delete BDIIALE
           evaluate EST-BDIIALE
           when "00"
               add 1 to N-ale-eliminadas
           when other
               display "Error al regrabar BDIIALE fs: " EST-BDIIALE
               " key: " LLAVE-BDIIALE
               display "Registro BDIIALE: "
               display REG-BDIIALE
               perform 9000-CANCELAR-PROGRAMA
           end-evaluate.

       4800-PROCESO-REG-T.
      ******************************************************************
      * El registro leido en el archivo JURAD es tipo "T" con totales
      * de control, y debería ser el ultimo del archivo
      ******************************************************************
      * Control de los totales calculados contra los esperados
      *    cantidad de registros I M E
           if JURADT-registros not numeric
               display " "
               display JURADT-reg
               display "Reg T - total de registros no numerico"
               perform 9000-CANCELAR-PROGRAMA
           end-if
           if JURADT-registros not = N-reg-leidos
               display "No coincide cantidad de registros- esperados"
               JURADT-registros " leidos: " N-reg-leidos
           end-if

           03 N-demandas-vigentes       pic 9(5).
           03 N-demandas-terminadas     pic 9(5).
           03 N-embargos-vigentes       pic 9(5).
           03 N-embargos-terminados     pic 9(5).

           if JURADT-demandas-vigentes not numeric
               display " "
               display JURADT-reg
               display "Reg T - total demandas vigentes no numerico"
               perform 9000-CANCELAR-PROGRAMA
           end-if

      *    Total numero de demandas terminadas
           03 JURADT-demandas-terminadas pic 9(7).
           03                           pic x.
      *    Total numero de embargos vigentes
           03 JURADT-embargos-vigentes  pic 9(7).
           03                           pic x.
      *    Total numero de embargos terminados
           03 JURADT-embargos-terminados pic 9(7).


       4900-GRABAR-ERROR.
      ******************************************************************
      * El registro de JURAD tiene error: se graba en el archivo ERRS
      * con indicacion del error detectado
      ******************************************************************
           move JURAD-reg to ERRS-reg
           move E-errores-reg to ERRS-causa-rechazo
      *    lista de los codigos de los errores del registro
           move E-errores-reg to ERRS-causa-rechazo
           write ERRS-reg
           evaluate ERRS-fs
               when "00"
                   continue
               when other
                   display "Error al grabar el archivo de errores "
                   display "Nombre del archivo: " ERRS-file-name
                   display "File status: " ERRS-fs
                   perform 9000-CANCELAR-PROGRAMA
                   goback
           end-evaluate.
 

       4320-ARMAR-TEXTO-ALERTA.
      ****************************************************************
      * Hace el reemplazo de los valores adecuados en los templates
      * de las linea de alerta que corresponde
      ****************************************************************
      *    segun se trate de demandas o embargos
           if JURAD-demandas-vigentes not = 0 
           or JURAD-demandas-terminadas not = 0
               set LAx to 1
               move high-values to LA-valores
               move JURAD-demandas-vigentes to LA-num
      * >>>> esto esta anulado x que no se como hacer para que lo compile
      *        unstring LA-numX into LA-descarte LA-valor(1)
      *        delimited by all space
               move LA-numx(9:12) to LA-valor(1)
               move JURAD-demandas-terminadas to LA-num
      * >>>> esto esta anulado x que no se como hacer para que lo compile
      *        unstring LA-numX into LA-descarte LA-valor(2)
      *        delimited by all space
               move LA-numx(9:12) to LA-valor(2)

      * >>>> esto esta anulado x que no se como hacer para que lo compile
      * >>>> da error en el end-if
      *        string LA-parte(LAx, 1) LA-valor(1)
      *        LA-parte(LAx, 2) LA-valor(2) LA-parte(LAx, 3) LA-valor(3)
      *        LA-parte(LAx, 4) LA-valor(4) LA-parte(LAx, 5) LA-valor(5)
      *        LA-parte(LAx, 6) LA-valor(6) LA-parte(LAx, 7) LA-valor(7)
      *        LA-parte(LAx, 8) LA-valor(8) LA-parte(LAx, 9) LA-valor(9)
      *        LA-parte(LAx, 10) LA-valor(10)
      *        delimited by high-value
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

      * Obtiene informacion sobre la corrida de la linea de comando: el
      * nombre del archivo de entrada (que contiene el tipo de proceso),
      * el indicador de proceso real o en falso (default)
           accept P-command-line from COMMAND-LINE
           display "Parametros: " P-command-line
           unstring P-command-line delimited by all " " into
           P-file-name P-proceso-real

      * El programa se anuncia en la consola
      * TODO: se usa un anuncio mas vistoso
           if PROCESO-REAL
               display pgm " Carga de archivo JURAD - proceso REAL"
           else
               display pgm " Carga de archivo JURAD - proceso en falso"
           end-if

      * El indicador de proceso real debe ser SI|si|NO|no
           if P-proceso-real not = "S" and "N"
               display "El indicador de proceso real es invalido"
               display "debe ser SI o NO"
               perform 9000-CANCELAR-PROGRAMA
           end-if

      * TODO: controlar los componentes del filename
           evaluate P-tipo-proceso
           when "01"
               display "01 Envio Original - Carga TOTAL 1a vez"
           when "02"
               display "02 Archivo Semanal - Carga PARCIAL"
           when "03"
               display "03 Archivo Mensual - Carga PARCIAL"
           when "04"
               display "04 Refresque Total - Carga TOTAL"
           when other
      *        valor invalido
               display "El tipo de proceso " P-tipo-proceso
               " es invalido (debe ser 01 02 03 o 04)"
               perform 9000-CANCELAR-PROGRAMA
           end-evaluate.

      * Edita la fecha de proceso DDMMAA del parametro en formato
      * YYYYMMDD 
           string "20" P-fecha-proceso(5:6) P-fecha-proceso(3:4)
           P-fecha-proceso(1:2) delimited size 
           into P-fecha-proceso-YYYYMMDD
           display "Fecha de proceso: " P-fecha-proceso-YYYYMMDD

      * Calcula una fecha 50 anios posterior a la de proceso
           move P-fecha-proceso-YYYYMMDD to P-fecha-proceso-50
           add 50 to P-fecha-proceso-50YYYY

      * Apertura del archivo de entrada
           string "$TEMPORALES/" P-file-name into P-file-path
           open input JURAD
           evaluate JURAD-fs
               when "00"
                    continue
               when other
                   display "Error en open del archivo JURAD "
                   display "Path del archivo: " P-file-path
                   display "File status: " JURAD-fs
                   perform 9000-CANCELAR-PROGRAMA *> no retorna
                   goback
           end-evaluate.

      * Apertura del archivo de errores
           string "$TEMPORALES/" P-file-name(1:19) "_ERRORES.txt"
           into ERRS-file-name
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

       1000-NOT-USED.
      * >>>> esto da un error de compilacion que no puedo resolver
      * >>>> por ahora esta anulado
      ******************************************************************
      * Carga de los templates para armar el texto de alertas
      * TODO: por ahora los templates son literales
           set LAx to 0
           move high-values to LA-templates
           perform varying LAm from 1 by 1 
           until LA-modelo(LAm) = high-values
               set LAx up by 1
      *        separa las partes de la linea
      *        unstring LA-modelo(LAm) into LA-parte(LAx, 1)
      *        LA-parte(LAx, 2) LA-parte(LAx, 3) LA-parte(LAx, 4)
      *        LA-parte(LAx, 5) LA-parte(LAx, 6) LA-parte(LAx, 7)
      *        LA-parte(LAx, 8) LA-parte(LAx, 9) LA-parte(LAx, 10)
      *        delimited by all "&"
      *        prepara para omitir los espacios al final de c/parte
               perform varying LAy from 1 by 1 until LAy > 10
                   inspect LA-parte(LAx, LAy) replacing trailing
                   spaces by high-values
                   inspect LA-parte(LAx, LAy) replacing first
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
           move zeros to COD-ALERTA-BDIIALE
           move low-values to LLASEC-BDIIALE
           start BDIIALE key > LLAVE-BDIIALE invalid key
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
                           add 1 to N-ale-borradas
                       else
      *                    error en la eliminacion
                           display "Error al eliminar un registro de "
                           "BDIIALE vaciando los datos"
                           display "Clave: " LLAVE-BDIIALE
                           display "Reg borrados: " N-ale-borradas
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
           display "Reg borrados: " N-ale-borradas. 


       7000-RUTINA-FINAL.
      ******************************************************************
      * Registra hora de finalizacion
           accept W-hora-fin from time

      * Leer JURAD a ver si hay registros despues del "T"

      * Close todos los archivos
           close JURAD ERRS BDIIREGI BDIIDIAN BDIIDAS BDIIALE.
      
      * Emision de contadores de registros
           display " "
           display "Totales de registros:"
           move N-reg-leidos to N-edit 
           display N-edit "Registros IME en el archivo de entrada"
           move N-reg-OK to N-edit 
           display N-edit "Registros procesados sin error"

      * >>>> not OK: suma para J001 y tambien J002, hay que cambiar la
      * definicion de estos contadores
      *    move N-reg-tipo-I-OK to N-edit
      *    display N-edit "Registros tipo I aceptados"
      *    move N-reg-tipo-M-OK to N-edit
      *    display N-edit "Registros tipo M aceptados"
      *    move N-reg-tipo-E-OK to N-edit
      *    display N-edit "Registros tipo E aceptados"

           move E-cont-regs-con-error to N-reg-con-error
           move N-reg-con-error to N-edit 
           display N-edit "Registros con error"
           move N-ale-borradas to N-edit 
           display N-edit "Alertas borradas previo al refresque total"
           move N-ale-insertadas to N-edit 
           display N-edit "Alertas insertadas"
           move N-ale-modificadas to N-edit 
           display N-edit "Alertas modificadas"
           move N-ale-eliminadas to N-edit 
           display N-edit "Alertas eliminadas"
           move N-ale-ya-existe to N-edit 
           display N-edit "Insercion de alerta existente"
           move N-ale-no-existe to N-edit 
           display N-edit "Modif o elim de alerta inexistente".

      * Emision del reporte de errores
           display " "
           display "Totales por tipo de error detectado:"
           perform varying Ex from 1 by 1 until Ex > 50
           or E-codigo(Ex) = "00"
               if E-contador(Ex) not = 0 
                   move E-contador(ex) to E-edit
                   display E-edit E-codigo(Ex) "-" E-descripcion(Ex)
               end-if
           end-perform.


       9000-CANCELAR-PROGRAMA.
      ******************************************************************
      * Procedimiento standard de cancelacion de programa batch
      ******************************************************************
      * TODO: hay un standard para comunicar una cancelacion de proceso?
           display " "
           display pgm " - proceso cancelado"
           goback returning 99.


           copy BDIIVIDE.PROC.


           copy VALNOM.PROC.


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
      * Registro de input (primeros 86 caracteres)
       01  reg-input-anterior           pic x(86) value low-values.
      * Posicion del proximo error en la lista de errores del reg
       01  errIndex                     pic 999 comp.
      ******************************************************************

       LINKAGE SECTION.
      ******************************************************************
      * Codigo del error detectado
       01  cod-error                    pic xx.
      ******************************************************************
      * Registro de input (primeros 86 caracteres)
       01  reg-input                    pic x(86).
      ******************************************************************
      * Tabla de errores de consistencia del input
       01 E-errores.
      *    indica si el registro actual tiene error
           03 E-hay-error               pic xx.
      *    errores del registro actual
           03 E-errores-reg             pic x(100).
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

       PROCEDURE DIVISION using cod-error reg-input E-errores.
       00.
      *    marca al reg actual como erroneo
           move "SI" to E-hay-error

      *    cuenta y muestra el registro de input
           if reg-input(1:86) not = reg-input-anterior
               add 1 to E-cont-regs-con-error
               display " "
               display reg-input(1:86)
               move reg-input(1:86) to reg-input-anterior
               move spaces to E-errores-reg
               move 2 to errIndex
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

      *    agrega a la lista de errores del registro
           if errIndex < 99
               move cod-error to E-errores-reg(errIndex:2)
               add 3 to errIndex
           end-if

           exit program.


       end program PONERR.

       end program ACTALEJUR.

      ******************************************************************
      ******************************************************************

ABANDONADO: 
      /
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECKTOTAL.
      ******************************************************************
      * Rutina de conveniencia para controlar la coincidencia de los
      * totales de control y denunciar las diferencias
      * Recibe la descripcion del par de totales a controlar, el total
      * informado, y el total calculado.
      * Compara los dos valores, y si no coinciden muestra un mensaje
      * y retorna una senial. 
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Edicion de los totales
       01  T-infoX                      pic x(12).
       01  T-info                       redefines T-infoX
                                        pic 9(12).
       01  T-calcX                      pic x(12).
       01  T-calc                       redefines T-calcX
                                        pic 9(12).
      * Edicion del texto
       01  T-nombre                     pic x(100).
       01  T-mensaje                    pic x(100).
      ******************************************************************

       LINKAGE SECTION.
      ******************************************************************
      * Nombre del total
       01 TOTAL-nombre                  pic x(100).
       01 TOTAL-nombre-L                pic S9(9) comp-5.
      ******************************************************************
      * Totales informado y calculado
       01 TOTAL-informado               pic x(12).
       01 TOTAL-informado-L             pic S9(9) comp-5.
       01 TOTAL-calculado               pic 9(8).
      ******************************************************************
      * call CHECKTOTAL using "Cant de registros" JURADT-registros
      * length of JURADT-registros N-registros-leidos
      ******************************************************************

       PROCEDURE DIVISION using 
           delimited TOTAL-nombre 
           TOTAL-informado by value TOTAL-informado-L
           TOTAL-calculado.
       00.
      *    long del texto
           inspect TOTAL-nombre-L
      *    total informado
           if TOTAL-informado-L > 12 stop run.
           move zeros to T-info
           move TOTAL-informado(1:TOTAL-informado-L) 
           to T-infox(12 - TOTAL-informado-L:TOTAL-informado-L)
           if T-info not numeric
               string T-nombre delimited by low-value " no numerico"
               delimited size into T-mensaje
               display T-mensaje
           end-if
      *    total calculado - no se controla
           move zeros to T-calc
           move TOTAL-calculado(1:TOTAL-calculado-L) 
           to T-calcx(12 - TOTAL-calculado-L:TOTAL-calculado-L)
      *    control
           if T-calc = T-info 
               goback returning 0
           else
               display T-nombre
               display "Calculado:" T-calc "   Informado:" T-info
               goback returning 1
           end-if.


           
      *    descripcion (nombre del total)
           if TOTAL-nombre-L > 100 
               move 100 to TOTAL-nombre-L
           end-if
           move TOTAL-nombre(1:TOTAL-nombre-L) to T-nombre
           inspect T-nombre replacing trailing spaces by low-values
           inspect T-nombre replacing first low-value by space



           exit program.


       end program CHECKTOTAL.

