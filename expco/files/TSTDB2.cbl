      $SET db2(db=INGR_DL1 pass=icgssode.IcgssD06,VALIDATE=RUN)
      $SET db2(connect=2)
      $set db2(BIND COLLECTION=COBOLAPP)
      $set db2(FORMAT=LOC VERSION=V8)
      $set LIST NOANIM
      * TESDB2.v.02.0001

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSTDB2.
       AUTHOR.        GLOBANT.
      ******************************************************************
      * Medición de performance de lectura de la tabla SCORE
      ******************************************************************
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           select BIND-arc assign to disk       BIND-title
               organization is line sequential.

           select CLAVES-file
               assign to disk                   CLAVES-title
               organization is line sequential
               access mode is sequential
               file status is CLAVES-fs
               lock mode is manual with lock on record.

           select OPTIONAL LEIDOS-file
               assign to disk                   LEIDOS-title
               organization is line sequential
               access mode is sequential
               file status is LEIDOS-fs
               lock mode is manual with lock on record.

          select OPTIONAL REPSAL-file
               assign to disk                   REPSAL-title
               organization is line sequential
               access mode is sequential
               file status is REPSAL-fs
               lock mode is manual with lock on record.

      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD BIND-arc.
           01 BIND-reg                  pic x(500).

           FD CLAVES-file.
           01 CLAVES-reg.
              02 CLAVES-tipoid          pic 9(01).
              02 CLAVES-numid           pic 9(11).

           FD LEIDOS-file.
           01 LEIDOS-reg.
              02 LEIDOS-tipoid          pic x(01).
              02 LEIDOS-numid           pic x(11).
              02 LEIDOS-fecha           pic x(10).
              02 LEIDOS-score           pic x(03).
              02 LEIDOS-exclusion       pic x(02).
              02 LEIDOS-segmento        pic x(01).

           FD REPSAL-file.
           01 REPSAL-reg.
              02 REPSAL-tipoid          pic x(01).
              02 REPSAL-numid           pic x(11).
              02 REPSAL-descripcion     pic x(40).
              02 REPSAL-fechai-pg       pic x(10).
              02 REPSAL-horai-pg        pic x(8).
              02 REPSAL-fechaf-pg       pic x(10).
              02 REPSAL-horaf-pg        pic x(8).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *@DB2CFG_BD=DB2_SSO

      * host variables para columnas de la tabla SCORE
       01 SCORE-tipoid                  pic x.
       01 SCORE-numid                   pic x(11).
       01 SCORE-fecha                   pic x(10).
       01 SCORE-score                   pic x(3).
       01 SCORE-exclusion               pic x(2).
       01 SCORE-segmento                pic x.

      * nombre del programa
       01 WS-PROGRAMA                   pic x(06) value "TSTDB2".

      * file-statuses y titles de los archivos
       01 CLAVES-fs                     pic x(2) value zeros.
       01 LEIDOS-fs                     pic x(2) value zeros.
       01 REPSAL-fs                     pic x(2) value zeros.
       01 CLAVES-title pic x(80) value "$TEMPORALES/SCOREENT".
       01 LEIDOS-title pic x(80) value "$TEMPORALES/SCORESAL".
       01 REPSAL-title pic x(80) value "$TEMPORALES/REPSCSAL".

      * datos del archivito BIND
       01 BIND-title                    pic x(80) value spaces.
       01 BIND-reg-connect              pic x(100) value spaces.
       01 BIND-reg-quit                 pic x(100) value spaces.

      * datos de la base de datos
       exec sql include sqlca end-exec
       01 SQLCODE-I                     pic -(9).
       01 DB-alias                      pic x(15).
       01 DB-usr                        pic x(15).
       01 DB-pass                       pic x(18).

      * Contadores de registros
       01 CONT-claves                   pic 9(12) value zeros.
       01 CONT-cursores                 pic 9(12) value zeros.
       01 CONT-filas                    pic 9(12) value zeros.
       01 CONT-edit                     pic ZZZ,ZZZ,ZZZ,ZZZ.

           COPY "CCIWS.CPY".
           COPY PE-TIME.WS.

      * Comandos de shell (terminados con un x'00')
       01 CMD-comando                   pic x(101) value all x'00'.
       01 Spa100                        pic x(100) value spaces. 

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       0000-PROGRAMA-PRINCIPAL.
      ******************************************************************
      *0000-PROGRAMA-PRINCIPAL.
      ******************************************************************
           display "                                                "
           display "************************************************"
           display "* Prueba de performance de lectura de la tabla *"
           display "* REGISTRO.SCORE usando cursores con claves    *"
           display "* parciales aleatorias                         *"
           display "************************************************"
           display "*                                              *"
           display "*           PROGRAMA : TSTDB2.CBL              *"
           display "*                                              *"
           display "************************************************"
           display "                                                "
           perform 7777-CONTROL-TIEMPO
           display " Inicio Proceso : " FECHA-PG " " HORA-PG
      *
           perform 1000-ABRIR-ARCHIVOS
           perform 2000-PROCESO-CLAVE UNTIL CLAVES-fs not = '00'
           perform 6000-FINALIZAR
           stop run.

       1000-ABRIR-ARCHIVOS.
      *    display '1000-ABRIR-ARCHIVOS'
      ******************************************************************
      *1000-ABRIR-ARCHIVOS.
      ******************************************************************
           open input CLAVES-file
           if CLAVES-fs not = "00"
              display "Error Abriendo el Archivo CLAVES fs:" CLAVES-fs
              stop run
           end-if
           open output LEIDOS-file
           if LEIDOS-fs not = "00"
              display "Error abriendo archivo LEIDOS fs:" LEIDOS-fs
              stop run
           end-if
           open output REPSAL-file
           if REPSAL-fs not = "00"
              display "Error abriendo el archivo REPSAL fs:" REPSAL-fs
              stop run
           end-if
           perform 1001-INICIAR-BASEDEDATOS.

       2000-PROCESO-CLAVE.
      *    display '2000-PROCESO-CLAVE'.
      ******************************************************************
      *2000-PROCESO-CLAVE.
      * Lectura archivo secuencial con claves SCORE en orden aleatorio 
      * y de los registros de la tabla SCORE que tienen ese ID
      ******************************************************************
           read CLAVES-file next record
           evaluate CLAVES-fs 
           when "00"
      *        lectura OK: lee los regs correspondientes en tabla SCORE
               add 1 to CONT-claves
               perform 7777-CONTROL-TIEMPO
               initialize REPSAL-reg
               move FECHA-PG to REPSAL-fechai-pg
               move HORA-PG to REPSAL-horai-pg
               move "BUSCAR REGISTRO TABLA" to REPSAL-descripcion
               perform 7777-CONTROL-TIEMPO
               move FECHA-PG to REPSAL-fechaf-pg
               move HORA-PG to REPSAL-horaf-pg
               perform 5000-GRABAR-REPSAL
               perform 3000-LEER-REG-SCORE

           when "10"
      *        EOF: termina
               continue

           when other
      *        error: mensaje y terminación del programa
               display "Error en lectura de CLAVES fs=" CLAVES-fs
               perform 6000-FINALIZAR
               stop run
           end-evaluate.

       1001-INICIAR-BASEDEDATOS.
      *    display '1001-INICIAR-BASEDEDATOS'
      ******************************************************************
      *1001-INICIAR-BASEDEDATOS.
      * Establece una connection con la base de datos DB2 determinada
      * por las credenciales de la corrida
      * Declara el cursor para los registros de la tabla SCORE
      ******************************************************************
      *    obtiene las credenciales
           move "lstxclde" to DB-usr
           move "LstxcD8" to DB-pass
           move "LIST_DL1" to DB-alias
      *    bind del programa WS-PROGRAMA contra la base DB-alias 
           perform 1000-BIND-RUTSQL
      *    connect a la base de datos
           exec sql
                connect to :DB-alias user :DB-usr using :DB-pass
           end-exec
           if sqlcode not = 0
               move SQLCODE to sqlcode-I
               display "Error: cannot connect to " DB-alias
               sqlcode-I sqlerrmc
           end-if
      *    declaración del cursor para la tabla SCORE: posiciona para
      *    leer las filas de un (TIPOID, NUMID) por fecha
           exec sql
               declare SCORES cursor for 
               select FECHA, SCORE, EXCLUSION, SEGMENTO
               from REGISTRO.SCORE
               where TIPOID = :SCORE-tipoid and NUMID = :SCORE-numid
               order by FECHA
               for read only with UR
           end-exec
           if sqlcode not = 0
               display "Error: cannot declare cursor SQLCODE:" sqlcode
               display sqlerrmc
               stop run
           end-if.

       3000-LEER-REG-SCORE.
      *    display '3000-LEER-REG-SCORE'
      ******************************************************************
      *3000-LEER-REG-SCORE.
      * Usando un cursor se leen los registros de SCORE correspondientes
      * a la clave leída en el archivo CLAVES
      ******************************************************************
           perform 7777-CONTROL-TIEMPO
           initialize REPSAL-reg
           move FECHA-PG to REPSAL-fechai-pg
           move HORA-PG to REPSAL-horai-pg
           move "SELECT BD - RECUPERAR DATOS" to REPSAL-descripcion
      *    posiciona el cursor para la nueva clave
           move CLAVES-tipoid to SCORE-tipoid 
           move CLAVES-numid to SCORE-numid
           exec sql open SCORES end-exec
           if sqlcode not = 0
               move SQLCODE to sqlcode-I
               display "Error: cannot open SCORE cursor " DB-alias
               sqlcode-I sqlerrmc
           end-if
      *    loop de lectura de los registros del cursor
           initialize SCORE-fecha SCORE-score SCORE-exclusion 
           SCORE-segmento
           exec sql fetch SCORES into :SCORE-fecha, :SCORE-score,
           :SCORE-exclusion, :SCORE-segmento end-exec
           if sqlcode = 100
               display "NO HAY SCORES PARA " CLAVES-tipoid
               " " SCORE-tipoid SCORE-numid
           else
               add 1 to CONT-cursores
               perform until SQLCODE not = 0
                   add 1 to CONT-filas
                   perform 5000-GRABAR-LEIDOS
                   perform 7777-CONTROL-TIEMPO
                   move FECHA-PG to REPSAL-fechaf-pg
                   move HORA-PG to REPSAL-horaf-pg
                   perform 5000-GRABAR-REPSAL
      *            avanza al próximo registro del cursor
                   initialize SCORE-fecha SCORE-score SCORE-exclusion 
                   SCORE-segmento
                   exec sql fetch SCORES into :SCORE-fecha,
                   :SCORE-score, :SCORE-exclusion, :SCORE-segmento
                   end-exec
               end-perform
           end-if
      *    si hubo error lo publica
           if sqlcode not = 100
               move SQLCODE to sqlcode-I
               display "Error: cannot fetch " DB-alias sqlcode-I
               sqlerrmc 
           end-if
      *    close del cursor
           exec sql close SCORES end-exec
           if sqlcode not = 0
               move SQLCODE to sqlcode-I
               display "Error: cannot close cursor " DB-alias
               sqlcode-I sqlerrmc 
           end-if.

       5000-GRABAR-LEIDOS.
      *    display '5000-GRABAR-LEIDOS'
      ******************************************************************
      *5000-GRABAR-LEIDOS.
      * Graba un registro en arch LEIDOS por cada fia leída de SCORES
      ******************************************************************
           initialize LEIDOS-reg
           move SCORE-tipoid to LEIDOS-tipoid
           move SCORE-numid to LEIDOS-numid
           move SCORE-fecha to LEIDOS-fecha
           move SCORE-score to LEIDOS-score
           move SCORE-exclusion to LEIDOS-exclusion
           move SCORE-segmento to LEIDOS-segmento
           write LEIDOS-reg.

       5000-GRABAR-REPSAL.
      *    display '5000-GRABAR-REPSAL'
      ******************************************************************
      *5000-GRABAR-REPSAL.
      * Graba reporte de salida para medir tiempos
      ******************************************************************
           move CLAVES-tipoid to REPSAL-tipoid
           move CLAVES-numid to REPSAL-numid
           write REPSAL-reg.

       1000-BIND-RUTSQL.
      *    display '1000-BIND-RUTSQL'
      ******************************************************************
      *1000-BIND-RUTSQL.
      * Se realiza el bind dinamico para una base de datos. Se crea
      * un archivo con las instruccines DB2 y se manda a ejecutar.
      * Parametros:
      *    WS-PROGRAMA
      *    DB-alias
      *    DB-usr
      *    DB-pass
      ******************************************************************
      * Se usa un archivito para poner los comandos de shell
           string "$TEMPORALES/" WS-PROGRAMA delimited size ".BIND"
           Spa100 into BIND-title
           open output BIND-arc
           move spaces to BIND-reg
      * Creacion del paquete (por si no estaba)
      *    connect
           string "db2 CONNECT to " DB-alias delimited by "  "
           " USER " DB-usr delimited by "  "
           " USING " DB-pass delimited by "  "
           Spa100 into BIND-reg-connect
           write BIND-reg from BIND-reg-connect
      *    control: $HCOBND debe estar
           move "HCOBND" to VAR-AMBIENTE
           display VAR-AMBIENTE upon environment-name
           accept VAR-AMBIENTE from ENVIRONMENT-VALUE
           if VAR-AMBIENTE = "HCOBND"
              display "No se ha fijado: $HCOBND!!!!! "
              stop run
           end-if
      *    bind - add collection
           string "db2 bind $HCOBND/" WS-PROGRAMA delimited size ".bnd"
           " ACTION ADD COLLECTION COBOLAPP " Spa100 into BIND-reg
           write BIND-reg
      *    quit
           move "db2 quit" to BIND-reg-quit
           write BIND-reg from BIND-reg-quit
      * Actualizacion del paquete
      *    connect
           write BIND-reg FROM BIND-reg-connect.
      *    bind
           string "db2 bind $HCOBND/" WS-PROGRAMA delimited by SIZE
           ".bnd" " ACTION REPLACE REPLVER V8 RETAIN YES "
           "COLLECTION COBOLAPP " Spa100 into BIND-reg
           write BIND-reg
      *    quit
           write BIND-reg FROM BIND-reg-quit
           close BIND-arc
      *    chmod y ejecución de los comandos DB2 del archivito BIND
           string "chmod +x $TEMPORALES/" WS-PROGRAMA delimited size
           ".BIND " X'00' Spa100 into CMD-comando
           call "SYSTEM" using CMD-comando
           string "$TEMPORALES/" WS-PROGRAMA delimited by size
           ".BIND " X'00' Spa100 into CMD-comando
           call "SYSTEM" using CMD-comando
      * Eliminación del comandito
           string "rm $TEMPORALES/" WS-PROGRAMA delimited size ".BIND "
           X'00' Spa100 into CMD-comando
           call "SYSTEM" using CMD-comando.

       6000-FINALIZAR.
      *    display '6000-FINALIZAR'
      ****************************************************************
      *6000-FINALIZAR
      * Muestra contadores de registros, cierra archivos y base
      ****************************************************************
           move CONT-claves to CONT-edit
           display "Claves leídas ......... " CONT-edit
           move CONT-cursores to CONT-edit
           display "Cursores creados ............. " CONT-edit
           move CONT-filas to CONT-edit
           display "Filas de SCORE leídas ........ " CONT-edit
           perform 7777-CONTROL-TIEMPO
           display " Fin Proceso : " FECHA-PG " " HORA-PG
      *
           close CLAVES-file LEIDOS-file REPSAL-file

      *    Cierra la conexion con la base de datos
      *    exec sql connect reset end-exec
      *    if sqlcode not = 0
      *        move SQLCODE to sqlcode-I
      *        display "Error: cannot disconnect " DB-alias
      *        sqlcode-I sqlerrmc
      *    end-if
           .

       7777-CONTROL-TIEMPO.
      *    display '7777-CONTROL-TIEMPO'
      ****************************************************************
      *7777-CONTROL-TIEMPO
      * Obtiene y edita la fecha y la hora del sistema
      ****************************************************************
           accept FECHA-S from date
           accept HORA-SIS from time
           move HORA-S to HORA-P
           move MINU-S to MINU-P
           move SEGU-S to SEGU-P
           move 20 to ANIO-SIG1
           move ANIO-S to ANIO-P
           move MESE-S to MESE-P
           move DIAS-S to DIAS-P.

