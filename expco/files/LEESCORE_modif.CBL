DEBUG $SET db2(db=INGR_DL1 pass=icgssode.IcgssD06,VALIDATE=RUN)
      $SET db2(connect=2)
      $set db2(BIND COLLECTION=COBOLAPP)
      $set db2(FORMAT=LOC VERSION=V8)
      * LSCORE.v.01.0001

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LSCORE.
       AUTHOR.        DATACREDITO.
REMARK******************************************************************
REMARK* Prueba de performance en acceso a la base de datos de SCORE    *
REMARK******************************************************************
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    archivito para los comandos del bind dinamico
           select ARC-BIND assign to disk       TITLE-ARC-BIND
               organization is line sequential.

      *    input: claves de los registros de la base de datos a leer
           select SECUENCIAL-ENTRADA
               assign to disk                   SEC-ENT-TITLE
               organization is line sequential
               access mode is sequential
               file status is COD-RET-ENT.

      *    output: listado de los registros leidos
           select SECUENCIAL-SALIDA
               assign to disk                   SEC-SAL-TITLE
               organization is line sequential
               access mode is sequential
               file status is COD-RET-SAL.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

      *    archivito para los comandos del bind dinamico
           FD ARC-BIND.
           01 REG-BIND pic x(500).

      *    input: claves de los registros de la base de datos a leer
           FD SECUENCIAL-ENTRADA.
           01 REG-SEC-ENT.
              02 ENT-TIP-ID         pic 9(01).
              02 ENT-NUM-ID         pic 9(11).

      *    output: listado de los registros leidos
           FD SECUENCIAL-SALIDA.
           01 REG-SEC-SAL.
              02 SAL-TIP-ID         pic x(01).
              02 SAL-NUM-ID         pic x(11).
              02 SAL-FECHA          pic x(08).
              02 SAL-SCORE          pic x(03).
              02 SAL-EXCLUSION      pic x(02).
              02 SAL-SEGMENTO       pic x(01).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *@DB2CFG_BD=DB2_SSO
      *  Include the SQL Communications Area. This includes the
      *  definitions of SQLCODE, etc
       exec sql include sqlca      end-exec
      * declaracion de host variables: las columnas de la tabla SCORE
       exec sql begin declare section end-exec
       77  BDTipoId                     pic x.
       77  BDNumId                      pic 9(11).
       77  BDFecha                      pic x(10).
       77  BDScore                      pic 9(3).
       77  BDExclusion                  pic 9(2).
       77  BDSegmento                   pic 9.
      * $$$ need indicators for nullable columns
       exec sql end declare section end-exec

       01 TITLE-ARC-BIND.
          02 TXT-ARC-BIND           pic x(80) value spaces.

      * file-status de los archivos de claves y listado
       01 COD-RET-ENT              pic x(2) value zeros.
       01 COD-RET-SAL              pic x(2) value zeros.

      * registros del archivito para el bind
       01 REG-BIND-AUX1            pic x(100) value spaces.
       01 REG-BIND-AUX2            pic x(100) value spaces.
      * comandos de shell usados para el bind
       01 WS-ARCHIVO               pic x(70) value spaces.
       01 I-XX                     pic 9(2) value 0.

      * nombre del programa
       01 WS-PROGRAMA              pic x(06) value spaces.

      * datos de conexión a la base de datos
      * DB alias, user & pw for the connect sommand
       01 WS-ALIAS                 pic x(15).
       01 WS-USR                   pic x(15).
       01 WS-PASS                  pic x(18).

       01 I-INT-COL1               pic 9(04).
       01 sqlcode-I                pic 9(08).
       01 INT-COL1                 pic S9(04) comp-5.
       01 C1                       pic x(40).
       01 C2                       pic x(40).
       01 C3                       pic x(40).
       01 C4                       pic x(40).
       01 int-col                  pic S9(4) comp-5.

      * select para la tabla SCORE
       01 ws-dyn-query.
          49 ws-dyn-query-len      pic s9(4) comp-5.
          49 ws-dyn-query-txt      pic x(900).

      * paths de los archivos secuenciales
       01 SEC-ENT-TITLE pic x(80) value "$TEMPORALES/SCOREENT".
       01 SEC-SAL-TITLE pic x(80) value "$TEMPORALES/SCORESAL".

       01 COMANDO                  pic x(40) value spaces.
       01 PRIVILEGIO               pic x(20) value spaces.

      *** INDICADORAS
       01 IND-SEC-ENT                 pic 9(01) value zeros.

REMARK*** CONTADORES
       01  W-LEIDOS                   pic  9(12) value zeros.
       01  W-GRABADOS                 pic  9(12) value zeros.

       01 VARIABLES-GENERALES.

      *   Stack de mensajes y su indice
          03 AREA-OUTPUT                 pic x(1900).
          03 FILLER  REDEFINES AREA-OUTPUT.
             05 V-OUTPUT                 pic x(80) occurs 24 times.
          03 IND-RMB                     pic 9(3) value 0.

          03 ETX                         pic x value X'03'.
          03 IND-CTA                     pic 9(2) value 1.
          03 FIN-PROGRAMA                pic 9 value 0.
          03 FIN-BIND                    pic 9 value 0.

          copy "CCIWS.CPY".

          copy PE-TIME.WS.

       01 AREA-MSG                    pic x(80) value spaces.
       01 TITULO-BD.
          02 filler pic x(20) value "====================".
          02 filler pic x(10) value "======= [ ".
          02 TITULO-BD-TIT    pic x(20).
          02 filler pic x(10) value " ] =======".
          02 filler pic x(20) value "====================".
       01 AREA-SUBTITULOS             pic x(80).
       01 AREA-SUB-SCORE.
          02 filler     pic x(20) value "TIPID          NUMID".
          02 filler     pic x(20) value "FECHA          SCORE".
          02 filler     pic x(20) value "EXCLUSION           ".
          02 filler     pic x(20) value "SEGMENTO            ".
       01 TITULO-ENCABEZADO.
          02 filler pic x(20) value "================= [ ".
          02 filler pic x(20) value "LECTURA  BASE  DATOS".
          02 filler pic x(09) value "SCORE    ".
          02 AMB-ENCABEZADO   pic x(11) value "DESARROLLO ".
          02 filler pic x(20) value " ] =================".
       01 RAYAS   pic x(80) value all "=".
       01 TEXTO-COMANDO.
          02 T-COMANDO                    pic x(100).
          02 filler                       pic x value x'00'.

       LINKAGE SECTION.
       SCREEN SECTION.

REMARK******************************************************************
REMARK PROCEDURE DIVISION.
REMARK******************************************************************
REMARK 0000-PROGRAMA-PRINCIPAL.
REMARK******************************************************************
REMARK*0000-PROGRAMA-PRINCIPAL.
REMARK******************************************************************
           display "                                                "
           display "************************************************"
           display "* Prueba de performance de accesos random a la *"
           display "* base de datos REGISTRO.SCORE                 *"
           display "************************************************"
           display "*                                              *"
           display "*          PROGRAMA : LSCORE.CBL               *"
           display "*                                              *"
           display "************************************************"
           display "                                                "
      *
           perform 7777-CONTROL-TIEMPO
           display " Fecha Inicio Proceso : " FECHA-PG
           display " Hora  Inicio Proceso : " HORA-PG
      *
           rove "LSCORE" to WS-PROGRAMA
           move "'DESARROL'" to WS-AMBIENTE-INI
      *
           perform 1000-ABRIR-ARCHIVOS
           perform 2000-PROCESO-LECTURA UNTIL IND-SEC-ENT = 1
           perform 6000-CERRAR-ARCHIVOS
           stop run.

       1000-ABRIR-ARCHIVOS.
REMARK******************************************************************
REMARK*1000-ABRIR-ARCHIVOS.
REMARK******************************************************************
           open input SECUENCIAL-ENTRADA
           if COD-RET-ENT NOT = "00"
              display "Error Abriendo el Archivo SECUENCIAL ENTRADA"
              COD-RET-ENT
              stop run
           end-if
           open output SECUENCIAL-SALIDA
           if COD-RET-SAL NOT = "00"
              display "Error Abriendo el Archivo SECUENCIAL SALIDA"
              COD-RET-SAL
              stop run
           end-if
           perform 1001-INICIAR-BASEDEDATOS
      * moví esto acá para que se ejecute una vez, al inicio del prg
           perform 1000-PREPARE.

      * prepara el encabezado de la salida
           move AREA-SUB-SCORE to AREA-SUBTITULOS
           move "       SCORE         " to TITULO-BD-TIT
           move TITULO-BD to AREA-MSG
           perform 9600-ABLOCAR
           move AREA-SUBTITULOS to AREA-MSG
           perform 9600-ABLOCAR
           move RAYAS to AREA-MSG
           perform 9600-ABLOCAR
           move "- REGISTROS EN LA TABLA DE SCORE" to AREA-MSG
           perform 9600-ABLOCAR.


       2000-PROCESO-LECTURA
      ******************************************************************
      *2000-PROCESO-LECTURA
      * Lee input: claves de SCORE en orden aleatorio
      ******************************************************************
           read SECUENCIAL-ENTRADA next record
               at end move 1 to IND-SEC-ENT
           end-read
           if IND-SEC-ENT not = 1
              add 1 to W-LEIDOS
              perform 3000-LEER-BD
           end-if.

       3000-LEER-BD.
REMARK****************************************************************
REMARK*3000-LEER-BD.
REMARK*  LEER REGISTROS BASE DE DATOS SCORE
REMARK****************************************************************
           move TITULO-ENCABEZADO to AREA-MSG
           perform 9600-ABLOCAR
           perform 4000-BUSCAR-SCORE
           perform 9700-FIN-ABLOCAR.

       4000-BUSCAR-SCORE.
REMARK******************************************************************
REMARK*4000-BUSCAR-SCORE.
REMARK*    BUSCAR REGISTROS EN TABLA REGISTRO.SCORE
REMARK******************************************************************
      * $$$$ dónde va el EXECUTE using :BDTipoId :BDNumId ?
      * carga los datos de la clave de la BD
           move ENT-TIP-ID to BDTipoId
           move ENT-NUM-ID to BDNumId
      * abre el cursor para los registros del ID del input
           exec sql
               open vcurs using :int-col
           end-exec
           call "DB2ERR" using SQLCA "Error: cannot open vc " WS-ALIAS
           perform 1000-BUSCAR-REG-SCORE.

       1000-BUSCAR-REG-SCORE.
REMARK******************************************************************
REMARK*1000-BUSCAR-REG-SCORE.
REMARK* Lectura y proceso del proximo registro del cursor
REMARK******************************************************************
           initialize BDFecha, BDScore, BDExclusion, BDSegmento
           exec sql
               fetch vcurs into :BDFecha, :BDScore, :BDExclusion,
               :BDSegmento
           end-exec
           if sqlcode = 100
               string "NO HAY REGISTROS PARA " BDTipoId " " BDNumId
               delimited size into AREA-MSG
               perform 9600-ABLOCAR
           else
               perform until SQLCODE not = 0
                  move spaces to AREA-MSG
                  move INT-COL1 to I-INT-COL1
     
                  string BDFecha(1:04) BDFecha(6:02) BDFecha(9:02)
                     " " BDScore(1:03)
                     " " BDExclusion(1:02)
                     "  " BDSegmento(1:01)
                  into AREA-MSG
                  perform 9600-ABLOCAR
                  perform 5000-GRABAR-SALIDA
                  exec sql
                      fetch vcurs into :BDFecha, :BDScore,
                      :BDExclusion, :BDSegmento
                  end-exec
               end-perform
               if sqlcode not = 100
                    move SQLCODE to sqlcode-I
                    string "Error: cannot fetch " WS-ALIAS
                    sqlcode-I sqlerrmc
                    into AREA-MSG
                    perform 9600-ABLOCAR
               end-if
           end-if
           perform 1000-CLOSEVCURS.

       5000-GRABAR-SALIDA.
REMARK******************************************************************
REMARK*5000-GRABAR-SALIDA.
REMARK*GRABAR ARCHIVO SECUENCIAL SALIDA CON REGISTROS ENCONTRADOS EN BD
REMARK******************************************************************
           initialize REG-SEC-SAL
           move ENT-TIP-ID to SAL-TIP-ID
           move ENT-NUM-ID to SAL-NUM-ID
           move BDFecha(1:4) to SAL-FECHA(1:4)
           move BDFecha(6:2) to SAL-FECHA(5:2)
           move BDFecha(9:2) to SAL-FECHA(7:2)
           move BDScore(1:3) to SAL-SCORE
           move BDExclusion(1:2) to SAL-EXCLUSION
           move BDSegmento(1:1) to SAL-SEGMENTO
           add 1 to W-GRABADOS
           write REG-SEC-SAL
           end-write.

       7777-CONTROL-TIEMPO.
      ****************************************************************
      *7777-CONTROL-TIEMPO
      *RUTINA PARA TOMAR LA FECHA DEL SISTEMA
      ****************************************************************
           accept FECHA-S from DATE
           accept HORA-SIS from TIME
           move HORA-S to HORA-P
           move MINU-S to MINU-P
           move SEGU-S to SEGU-P
           move 20 to ANIO-SIG1
           move ANIO-S to ANIO-P
           move MESE-S to MESE-P
           move DIAS-S to DIAS-P.

       1000-BUSCA-USUPASS.
REMARK******************************************************************
REMARK*1000-BUSCA-USUPASS.
REMARK*    PARA CADA BASE DE DATOS SE BUSCAN LAS CREDENCIALES PARA LA
REMARK*    CONEXION A LA BASE DE DATOS A CONSULTAR
REMARK******************************************************************
           move "lstxclde" to WS-USR
           move "LstxcD8" to WS-PASS
           move "LIST_DL1" to WS-ALIAS
           perform 1000-BIND-RUTSQL.

       1000-CONECTARSE-A-LAS-BDS.
REMARK******************************************************************
REMARK*1000-CONECTARSE-A-LAS-BDS.
REMARK*    CON LAS CREDENCIALES LEIDAS DE LA TABLA CONEXION SE REALIZA
REMARK*    LA CONEXION A LA BD QUE USA EL PROGRAMA
REMARK******************************************************************
           exec sql
                connect to :WS-ALIAS user :WS-USR using :WS-PASS
           end-exec
           call "DB2ERR" using SQLCA "Error: cannot connect " WS-ALIAS.

       1000-SETCONNECTION-XXXX-DL1.
REMARK*****************************************************************
REMARK*1000-SETCONNECTION-XXXX-DL1.
REMARK*    SE SELECCIONA LA BASE DE DATOS A UTILIZAR EN LOS QUERYS DADO
REMARK*    QUE HAY VARIAS ABIERTAS SIMULTANEAMENTE
REMARK*****************************************************************
           exec sql
             set connection :WS-ALIAS
           end-exec
           call "DB2ERR" using SQLCA "Error: cannot set connect "
           WS-ALIAS.
REMARK*   PROCEDIMIENTO PARA VERIFICAR TAKE OVER O FALLA EN LA CONEXION
REMARK*   CON LA BASE DE DATOS; SE REALIZA UN COMMIT PARA VERIFICAR SI
REMARK*   EXISTE LA CONEXION CON LA BASE DE DATOS. SI HAY PROBLEMA
REMARK*   SE INTENTA DE NUEVO LA RECONEXION
           exec sql
               commit
           end-exec
           if (call "DB2ERR" using SQLCA "Error: cannot commit "
           WS-ALIAS) = "1"
                display "SE PROCEDE A DESCONECTAR" WS-ALIAS
                perform 1000-DISCONNECT
                display "SE PROCEDE A CONECTAR DE NUEVO" WS-ALIAS
                perform 1000-CONECTAR-XXXX-DL1
                if sqlcode = 0
                   display "RECONEXION EXITOSA!!!" WS-ALIAS
                else
                   display "FALLA EN RECONEXION!!!!" WS-ALIAS
                end-if
           end-if.

       1001-INICIAR-BASEDEDATOS.
REMARK******************************************************************
REMARK*1001-INICIAR-BASEDEDATOS.
REMARK* SE DECLARA EL CURSOR GENERICO A SER USADO POR LOS QUERYS
REMARK* DINAMICOS Y SE ADQUIEREN TODAS LAS CREDENCIALES PARA CONECTARSE
REMARK* A LAS BASES DE DATOS
REMARK******************************************************************
      * esta linea: creo que debe ir acá, controlar ###########################
           perform 1000-SETCONNECTION-XXXX-DL1

      *    perform 1000-CREDENCIALES-DB2SSO-RUTSQL.
           perform 1000-BUSCA-USUPASS.
      *    perform 1000-HACER-BINDS-DINAMICOS.
           perform 1000-CONECTARSE-A-LAS-BDS.

      * declaración del cursor para lectura de SCORE
           exec sql
               declare vcurs cursor for dynamic_sql
           end-exec
           if (call "DB2ERR" using SQLCA "Error: cannot declare "
           WS-ALIAS) = "1"
                stop run
           end-if.

       1000-CERRAR-XXXX-DL1.
REMARK****************************************************************
REMARK*1000-CERRAR-XXXX-DL1.
REMARK*    SE CIERRA LA CONEXION CON LA BASE DE DATOS ELEGIDA
REMARK****************************************************************
           exec sql
             SET CONNECTION :WS-ALIAS
           end-exec
           call "DB2ERR" using SQLCA "Error: cannot set connection "
           WS-ALIAS
           exec sql
               commit
           end-exec.
           call "DB2ERR" using SQLCA "Error: cannot commit " WS-ALIAS
           perform 1000-DISCONNECT.

       1000-DISCONNECT.
REMARK****************************************************************
REMARK*1000-DISCONNECT.
REMARK*    SE REALIZA LA FUNCION ESTANDARD DE DESCONEXION
REMARK****************************************************************
           exec sql
               disconnect :WS-ALIAS
           end-exec
           call "DB2ERR" using SQLCA "Error: cannot disconnect "
           WS-ALIAS.

       1000-PREPARE.
REMARK****************************************************************
REMARK*1000-PREPARE.
REMARK* Prepara el select de la tabla SCORE
REMARK****************************************************************
      * Define una sentencia select de acceso a la tabla con los
      * datos de ID provistos por el input como parámetros
           move spaces to WS-dyn-query-txt
           move 1 to WS-dyn-query-len
      * Esto es lo que creo que deberíamos ejecutar una sola vez para
      * que el programa sea mucho mas rapido.
      * La idea es reemplazar los ENT-TIP-ID y ENT-NUM-ID por ? y ?
      * Agregué una cláusula ORDER by para mostrar por fecha para que
      * la salida quede más prolija (seguramente sin el ORDER by también
      * iba a salir por fecha por que los registros deben estar
      * organizados físicamente por fecha, seguramente el index es
      * "clustered").
           string
               "select FECHA, SCORE, EXCLUSION, SEGMENTO"
               " from REGISTRO.SCORE"
               " WHERE TIPOID = ? AND NUMID = ? "
               " ORDER by FECHA DESC "
               into WS-dyn-query-txt
               pointer WS-dyn-query-len
           end-string
           subtract 1 from WS-dyn-query-len
      * prepara el select para usarlo en las consultas
           exec sql
               prepare dynamic_sql from :WS-dyn-query
           end-exec
           call "DB2ERR" using SQLCA "Error: cannot prepare " WS-ALIAS.

       1000-CLOSEVCURS.
REMARK****************************************************************
REMARK*1000-CLOSEVCURS.
REMARK* Cierre del cursor de input
REMARK****************************************************************
           exec sql
               close vcurs
           end-exec
           call "DB2ERR" using SQLCA "Error: cannot close vc " WS-ALIAS.

REMARK***************************RUTINAS PARA TRATAMIENTO DE MENSAJES **
       9600-ABLOCAR.
REMARK******************************************************************
REMARK*9600-ABLOCAR.
REMARK* ESTA RUTINA SE PUEDE INVOCAR PARA MANDAR MENSAJES ABLOCADOS,   *
REMARK* LA VARIABLE IND-CTA CONTROLA LA POSICION DENTRO DE V-OUTPUT    *
REMARK* AL ACABAR DE RESPONDER UNA TRANSACCION SIEMPRE HAY QUE         *
REMARK* INVOVAR LA RUTINA 9700-FIN-ABLOCAR PARA DESOCUPAR EL BUFFER    *
REMARK* LOS MENSAJES SON DE 80 BYTES Y MAXIMO 24
REMARK******************************************************************
           move AREA-MSG to V-OUTPUT(IND-CTA)
           add 1 to IND-CTA
           if IND-CTA > 16
              perform 10900-ENVIA-CRCR
              move 1 to IND-CTA
           end-if.

       9700-FIN-ABLOCAR.
REMARK******************************************************************
REMARK*9700-FIN-ABLOCAR.
REMARK* RUTINA QUE SE ENCARGA DE DESOCUPAR EL BUFFER AL CULMINAR LA    *
REMARK* RESPUESTA DE UNA TRANSACCION.                                  *
REMARK******************************************************************
           if IND-CTA > 1
              perform 10900-ENVIA-CRCR
           end-if.

       10900-ENVIA-CRCR.
REMARK******************************************************************
REMARK*10900-ENVIA-CRCR.
REMARK* SE DETERMINA SI SE REALIZA UN display O SE MANDA LA RESPUESTA  *
REMARK* POR MEDIO DE MODULO CCI.                                       *
REMARK******************************************************************
           move 1 to IND-RMB
           perform 10920-ENVIA-DISPLAY.

       10920-ENVIA-DISPLAY.
REMARK******************************************************************
REMARK*10920-ENVIA-DISPLAY.
REMARK* EN CASO DE QUE EL PARAMETRO DE EJECUACION NO FUE SERVER SE     *
REMARK* PROCEDE A REALIZAR DISPLAY EN CAMBIO DE MANDAR AL MODULO CCI   *
REMARK******************************************************************
           if IND-CTA = 1
              perform 10900-DISPLAY
           else
              subtract 1 from IND-CTA
              perform 10900-DISPLAY IND-CTA times
           end-if
           move 1 to IND-CTA
           display ETX with no advancing.

       10900-DISPLAY.
REMARK******************************************************************
REMARK*10900-DISPLAY.
REMARK* SE REALIZA EL display DE LOS MENSAJES A RETORNAR               *
REMARK******************************************************************
           display V-OUTPUT(IND-RMB)
           add 1 to IND-RMB.

REMARK***************************RUTINAS PARA ADQUIRIR CREDENCIALES ****
       1000-BIND-RUTSQL.
REMARK******************************************************************
REMARK*1000-BIND-RUTSQL.
REMARK* SE REALIZA EL BIND DINAMICO PARA UNA BASE DE DATOS. SE CREA
REMARK* UN ARCHIVO CON LAS INSTRUCCINES DB2 Y SE MANDA A EJECUTAR.
REMARK* ESTO SE REALIZA CON LAS CREDENCIALES EXTRACTADAS DE LA
REMARK* TABLA DE CONEXIONES
REMARK* PARAMETROS :
REMARK*          WS-PROGRAMA
REMARK*          WS-ALIAS
REMARK*          WS-USR
REMARK*          WS-PASS
REMARK******************************************************************
REMARK*    REALIZAR LA CREACION DEL PAQUETE (POR SI NO ESTABA)
      *    sentence: connect to DB alias
           move spaces to TXT-ARC-BIND
           string "$TEMPORALES/" WS-PROGRAMA delimited by size ".BIND"
           into TXT-ARC-BIND.
           open output ARC-BIND.
           move spaces to REG-BIND.
           string "db2 CONNECT to " WS-ALIAS delimited by "  "
           " USER " WS-USR delimited by "  "
           " using " WS-PASS  delimited by "  "
           into REG-BIND
           write REG-BIND
      *    get HCOBND environment variable, complain if not defined
           move REG-BIND to REG-BIND-AUX1
           move "HCOBND" to VAR-AMBIENTE
           display VAR-AMBIENTE UPON ENVIRONMENT-NAME
           accept VAR-AMBIENTE from ENVIRONMENT-VALUE
           if VAR-AMBIENTE = "HCOBND"
              display "No se ha fijado: $HCOBND!!!!! "
              stop run
           else
              move spaces to WS-ARCHIVO
              string
                 VAR-AMBIENTE delimited by "  "
                 WS-PROGRAMA delimited by size
                 ".bnd"
              into WS-ARCHIVO
           end-if
      *    sentence: bind - add collection & quit
           move spaces to REG-BIND
           string "db2 bind $HCOBND/" WS-PROGRAMA delimited by size
           ".bnd ACTION ADD COLLECTION COBOLAPP " into REG-BIND
           write REG-BIND
           move "db2 quit" to REG-BIND
           write REG-BIND
           move REG-BIND to REG-BIND-AUX2
REMARK*    REALIZAR LA ACTUALIZACION DEL PAQUETE & quit
           write REG-BIND from REG-BIND-AUX1
           move spaces to REG-BIND
           string "db2 bind $HCOBND/" WS-PROGRAMA delimited by size
           ".bnd ACTION REPLACE REPLVER V8 RETAIN YES "
          "COLLECTION COBOLAPP " into REG-BIND
           write REG-BIND
           write REG-BIND from REG-BIND-AUX2
           close ARC-BIND.
REMARK*    se hace ejecutable el arch BIND y se lo ejecuta
           move spaces to WS-ARCHIVO
           string "chmod +x $TEMPORALES/" WS-PROGRAMA delimited by size
          ".BIND " into WS-ARCHIVO
           move WS-ARCHIVO to T-COMANDO
           call "SYSTEM" using TEXTO-COMANDO
      *    ejecucion del comando
           move spaces to WS-ARCHIVO
           string "$TEMPORALES/" WS-PROGRAMA delimited by size
           ".BIND " into WS-ARCHIVO
           move WS-ARCHIVO to T-COMANDO
           call "SYSTEM" using TEXTO-COMANDO
           if I-XX > 0
              move spaces to WS-ARCHIVO
              string "cp $TEMPORALES/" WS-PROGRAMA delimited by size
              ".BIND $TEMPORALES/RMBBIND" I-XX
              delimited by size into WS-ARCHIVO
              move WS-ARCHIVO to T-COMANDO
              call "SYSTEM" using TEXTO-COMANDO
              add 1 to I-XX
           end-if
      *    eliminacion higienica del archivo comando
           move spaces to WS-ARCHIVO
           string "rm $TEMPORALES/" WS-PROGRAMA  delimited by size
           ".BIND " into WS-ARCHIVO
           move WS-ARCHIVO to T-COMANDO
           call "SYSTEM" using TEXTO-COMANDO.

       6000-CERRAR-ARCHIVOS.
      ******************************************************************
      * 6000-CERRAR-ARCHIVOS.
      * Se cierra la base de datos y los archivos del programa
      ******************************************************************
           perform 7777-CONTROL-TIEMPO
           display " Fecha Fin    Proceso : " FECHA-PG
           display " Hora  Fin    Proceso : " HORA-PG
           perform 1000-CERRAR-XXXX-DL1
           close SECUENCIAL-ENTRADA SECUENCIAL-SALIDA.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2ERR.
      ******************************************************************
      * Emite un mensaje de error con DISPLAY si SQLCODE no es cero, 
      * opcionalmente termina el programa
      ******************************************************************
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           call-convention 1024 id CC1024.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * SQLCODE for displaying
       01  SQLCODEd                     pic z(8).
      * output message (grande, por seuridad)
       01  MSG-texto                    pic x(800).
       01  MSG-longitud                 pic s999 comp.
      * properties of an argument
       01  ARG-count                    comp-1.
       01  ARG-description.
       	   02  ARG-type                 pic 9(2) binary(2).
       	   02  ARG-length               PIC 9(8) binary(4).
       	   02  ARG-digit-count          pic 9(2) binary(2).
       	   02  ARG-scale                pic s9(2) binary(2).
       	   02  ARG-pointer              pointer.
       	   02  ARG-picture              pointer.

       LINKAGE SECTION.
      * return code: "0" is SQLCODE = 0, else "1"
        01  OK                          pic x.
        01 SQLCA.
            05  SQLCAID                 pic x(8) value "SQLCA   ".
            05  SQLCABC                 pic s9(9) comp-5 value 136.
            05  SQLCODE                 pic s9(9) comp-5 value 0.
            05  SQLERRM.
                49  SQLERRML            pic s9(4) comp-5.
                49  SQLERRMC            pic x(70).
            05  SQLERRP                 pic x(8).
            05  SQLERRD               pic S9(9) comp-5 occurs 6 value 0.
            05  SQLWARN.
                10  SQLWARN0            pic x.
                10  SQLWARN1            pic x.
                10  SQLWARN2            pic x.
                10  SQLWARN3            pic x.
                10  SQLWARN4            pic x.
                10  SQLWARN5            pic x.
                10  SQLWARN6            pic x.
                10  SQLWARN7            pic x.
                10  SQLWARN8            pic x.
                10  SQLWARN9            pic x.
                10  SQLWARN10           pic x.
                10  SQLWARNA            pic x redefines SQLWARN10.
            05  SQLSTATE                pic x(5).

       PROCEDURE DIVISION using SQLCA Msg1 Msg2 returning OK.
       00.
      * caso normal: retorna rapidamente
           if SQLCODE = 0
               move "0" to OK
               exit program
           end-if
      * hay error: arma y muestra mensaje
           move spaces to MSG-texto
      *    primera parte del msg, en arg2
           call CC1024 "C$DARG" using 2, argument-description
           move Msg1(1:ARG-length) to MSG-texto
           compute MSG-longitud = ARG-length + 1
      *    segunda parte, opcional, en arg3
           call CC1024 "C$NARG" using ARG-count
           if ARG-count > 2
               call CC1024 "C$DARG" using 3, argument-description
               move Msg2(1:ARG-LENGTH) to MSG-texto(MSG-longitud)
           end-if
      *    muestra el mensaje
           display MSG-texto
      *    
           exit program.
       end program DB2ERR.

       end program LSCORE.
