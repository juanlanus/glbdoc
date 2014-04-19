DEBUG $SET db2(db=INGR_DL1 pass=icgssode.IcgssD06,VALIDATE=RUN)
      $SET db2(connect=2)
      $set db2(BIND COLLECTION=COBOLAPP)
      $set db2(FORMAT=LOC VERSION=V8)
      * TESDB2.v.01.0001

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESDB2.
       AUTHOR.        GLOBANT.
REMARK******************************************************************
REMARK* Medición de performance de lectura de la tabla SCORE
REMARK******************************************************************
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BIND-arc ASSIGN TO DISK       BIND-title
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SECENT
               ASSIGN TO DISK                   SEC-ENT-TITLE
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS COD-RET-ENT
               LOCK MODE IS MANUAL WITH LOCK ON RECORD.

           SELECT OPTIONAL SECSAL
               ASSIGN TO DISK                   SEC-SAL-TITLE
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS COD-RET-SAL
               LOCK MODE IS MANUAL WITH LOCK ON RECORD.

          SELECT OPTIONAL REPSAL
               ASSIGN TO DISK                   REP-SAL-TITLE
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS COD-RET-REPSAL
               LOCK MODE IS MANUAL WITH LOCK ON RECORD.

      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD BIND-arc.
           01 BIND-reg                  PIC X(500).

           FD SECENT.
           01 REG-SEC-ENT.
              02 ENT-TIP-ID             PIC 9(01).
              02 ENT-NUM-ID             PIC 9(11).

           FD SECSAL.
           01 REG-SEC-SAL.
              02 SAL-TIP-ID             PIC X(01).
              02 SAL-NUM-ID             PIC X(11).
              02 SAL-FECHA              PIC X(10).
              02 SAL-SCORE              PIC 9(03).
              02 SAL-EXCLUSION          PIC 9(02).
              02 SAL-SEGMENTO           PIC 9(01).

           FD REPSAL.
           01 REG-REP-SAL.
              02 REPSAL-TIP-ID          PIC X(01).
              02 REPSAL-NUM-ID          PIC X(11).
              02 REPSAL-DESCRIPCION     PIC X(40).
              02 REPSAL-FECHAI-PG       pic x(10).
              02 REPSAL-HORAI-PG        pic x(8).
              02 REPSAL-FECHAF-PG       pic x(10).
              02 REPSAL-HORAF-PG        pic x(8).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *@DB2CFG_BD=DB2_SSO
      *  Include the SQL Communications Area. This includes the
      *  definitions of SQLCODE, etc
       exec sql include sqlca end-exec
       01 SQLCODE-I                     PIC 9(08).
      *    host variables para columnas de la tabla SCORE
       01  SCORE-tipoid                 PIC X.
       01  SCORE-numid                  PIC X(11).
       01  SCORE-fecha                  PIC X(10).
       01  SCORE-score                  PIC X(3).
       01  SCORE-exclusion              PIC X(2).
       01  SCORE-segmento               PIC X.

       01 COD-RET-ENT                   PIC X(2) VALUE ZEROS.
       01 COD-RET-SAL                   PIC X(2) VALUE ZEROS.
       01 COD-RET-REPSAL                PIC X(2) VALUE ZEROS.
       01 SEC-ENT-TITLE PIC X(80) VALUE "$TEMPORALES/SCOREENT".
       01 SEC-SAL-TITLE PIC X(80) VALUE "$TEMPORALES/SCORESAL".
       01 REP-SAL-TITLE PIC X(80) VALUE "$TEMPORALES/REPSCSAL".
       01 BIND-title                PIC X(80) VALUE SPACES.

       01 BIND-reg-connect              PIC X(100) VALUE SPACES.
       01 BIND-reg-quit                 PIC X(100) VALUE SPACES.
       01 WS-PROGRAMA                   PIC X(06) VALUE SPACES.
      *    nombre del programa
       01 WS-ARCHIVO                    PIC X(70) VALUE SPACES.
       01 WS-ALIAS                      PIC X(15).
      *    db user and pssword
       01 WS-USR                        PIC X(15).
       01 WS-PASS                       PIC X(18).

       01 int-col                       PIC S9(4) comp-5.

       01 COMANDO                       PIC X(40) VALUE SPACES.

REMARK*** CONTADORES
       01  W-LEIDOS                     PIC  9(12) VALUE ZEROS.
       01  W-GRABADOS                   PIC  9(12) VALUE ZEROS.
       01  W-EDIT-CONTADOR              PIC ZZZ,ZZZ,ZZZ,ZZZ.

       01 VARIABLES-GENERALES.
          03  Spa100                    pic x(100) value spaces. 
          03 AREA-MSG                   PIC X(80) VALUE SPACES.
          03 I-XX                       PIC 9(2) VALUE 0.
          03 AREA-OUTPUT                PIC X(1900).
          03 FILLER  REDEFINES AREA-OUTPUT.
             05 V-OUTPUT                PIC X(80) OCCURS 24 TIMES.
          03 IND-RMB                    PIC 9(3) VALUE 0.
          03 ETX                        PIC X VALUE X'03'.
          03 IND-CTA                    PIC 9(2) VALUE 1.
          COPY "CCIWS.CPY".
          COPY PE-TIME.WS.

       01 TEXTO-COMANDO.
          02 T-COMANDO                  PIC X(100).
          02 FILLER                     PIC X VALUE X'00'.

REMARK******************************************************************
REMARK PROCEDURE DIVISION.
REMARK******************************************************************
REMARK 0000-PROGRAMA-PRINCIPAL.
REMARK******************************************************************
REMARK*0000-PROGRAMA-PRINCIPAL.
REMARK******************************************************************
           DISPLAY "                                                "
           DISPLAY "************************************************"
           DISPLAY "* Prueba de performance de lectura de la tabla *"
           DISPLAY "* REGISTRO.SCORE usando cursores con claves    *"
           DISPLAY "* con claves parciales aleatorias              *"
           DISPLAY "************************************************"
           DISPLAY "*                                              *"
           DISPLAY "*           PROGRAMA : TESDB2.CBL              *"
           DISPLAY "*                                              *"
           DISPLAY "************************************************"
           DISPLAY "                                                "
           PERFORM 7777-CONTROL-TIEMPO
           DISPLAY " Inicio Proceso : " FECHA-PG " " HORA-PG
      *
           MOVE "TESDB2" TO WS-PROGRAMA
           PERFORM 1000-ABRIR-ARCHIVOS
           PERFORM 2000-PROCESO-REGISTRO UNTIL COD-RET-ENT not = '00'
           PERFORM 6000-FINALIZAR
           STOP RUN.

       1000-ABRIR-ARCHIVOS.
REMARK******************************************************************
REMARK*1000-ABRIR-ARCHIVOS.
REMARK******************************************************************
           OPEN INPUT SECENT
           IF COD-RET-ENT NOT = "00"
              DISPLAY "Error Abriendo el Archivo SECUENCIAL ENTRADA"
              COD-RET-ENT
              STOP RUN
           END-IF
           OPEN OUTPUT SECSAL
           IF COD-RET-SAL NOT = "00"
              DISPLAY "Error Abriendo el Archivo SECUENCIAL SALIDA"
              COD-RET-SAL
              STOP RUN
           END-IF
           OPEN OUTPUT REPSAL
           IF COD-RET-REPSAL NOT = "00"
              DISPLAY "Error Abriendo el Archivo REPORTE SALIDA"
              COD-RET-REPSAL
              STOP RUN
           END-IF
           PERFORM 1001-INICIAR-BASEDEDATOS.

       2000-PROCESO-REGISTRO.
      ******************************************************************
      *2000-PROCESO-REGISTRO.
      * Lectura archivo secuencial con IDs SCORE en orden aleatorio 
      * y de los registros de la tabla SCORE que tienen ese ID
      ******************************************************************
           READ SECENT NEXT RECORD
           evaluate COD-RET-ENT 
           when "00"
      *       lectura OK: lee los regs correspondientes en tabla SCORE
              ADD 1 TO W-LEIDOS
              PERFORM 3000-LEER-BD
           when "10"
      *        EOF: termina
               continue
           when else
      *        error: mensaje y terminación del programa
               display "Error en lectura de SEC-ENT fs=" COD-RET-ENT
               perform 6000-FINALIZAR
               stop run
           end-evaluate.

       3000-LEER-BD.
REMARK****************************************************************
REMARK*3000-LEER-BD.
REMARK*  Lee registros base de datos SCORE con la clave del input
REMARK****************************************************************
           PERFORM 7777-CONTROL-TIEMPO
           INITIALIZE REG-REP-SAL
           MOVE FECHA-PG TO REPSAL-FECHAI-PG
           MOVE HORA-PG TO REPSAL-HORAI-PG
           MOVE "BUSCAR REGISTRO TABLA" TO REPSAL-DESCRIPCION
      *
           PERFORM 7777-CONTROL-TIEMPO
           MOVE FECHA-PG                 TO REPSAL-FECHAF-PG
           MOVE HORA-PG                  TO REPSAL-HORAF-PG
           PERFORM 5000-GRABAR-REPSAL
      *
           PERFORM 3000-BUSCAR-REG-SCORE.
 
       1001-INICIAR-BASEDEDATOS.
      ******************************************************************
      *1001-INICIAR-BASEDEDATOS.
      * Establece una connection con la base de datos DB2 determinada
      * por las credenciales de la corrida
      * Declara el cursor para los registros de la tabla SCORE
      ******************************************************************
      *    obtiene las credenciales
           MOVE "lstxclde" TO WS-USR
           MOVE "LstxcD8" TO WS-PASS
           MOVE "LIST_DL1" TO WS-ALIAS
      *    bind del programa WS-PROGRAMA contra la base WS-ALIAS 
           PERFORM 1000-BIND-RUTSQL

      *    connect a la base de datos: primeto elige la conexión que va
      *    a usar, luego conecta con las credenciales del proceso
           PERFORM 1002-SETCONNECTION-XXXX-DL1
           EXEC SQL
                CONNECT TO :WS-ALIAS USER :WS-USR USING :WS-PASS
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot connect " WS-ALIAS
                sqlcode-I sqlerrmc
           end-if.

      *    preparación del cursor para la tabla SCORE: posiciona para
      *    leer las filas de un (TIPOID, NUMID) por fecha
           EXEC SQL
               declare vcurs cursor for 
               SELECT FECHA, SCORE, EXCLUSION, SEGMENTO
               FROM REGISTRO.SCORE
               WHERE TIPOID = :SCORE-tipoid AND NUMID = :SCORE-numid
               FOR READ ONLY
           END-EXEC
           if sqlcode not = 0
               display "Error: cannot declare cursor SQLCODE:" sqlcode
               display sqlerrmc
               stop run
           end-if
           PERFORM 1000-CONECTARSE-A-LAS-BDS.

       3000-BUSCAR-REG-SCORE.
REMARK*******************************************************************
REMARK*3000-BUSCAR-REG-SCORE.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 7777-CONTROL-TIEMPO
           INITIALIZE REG-REP-SAL
           MOVE FECHA-PG TO REPSAL-FECHAI-PG
           MOVE HORA-PG TO REPSAL-HORAI-PG
           MOVE "SELECT BD - RECUPERAR DATOS" TO REPSAL-DESCRIPCION
      *    inicia el cursor para una nueva clave
           move ENT-TIP-ID to SCORE-tipoid 
           move ENT-NUM-ID to SCORE-numid
           EXEC SQL
               open vcurs
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                display "Error: cannot open SCOPE cursor " WS-ALIAS
                sqlcode-I sqlerrmc
           end-if.

           INITIALIZE SCORE-fecha SCORE-score SCORE-exclusion 
           SCORE-segmento
           EXEC SQL
               fetch vcurs into :SCORE-fecha, :SCORE-score,
               :SCORE-exclusion, :SCORE-segmento
           END-EXEC
           if sqlcode = 100
               display "NO HAY SCORES PARA " ENT-TIP-ID " " SCORE-tipoid 
               SCORE-numid
           END-IF.
           PERFORM UNTIL SQLCODE NOT = 0
              PERFORM 5000-GRABAR-SALIDA
              PERFORM 7777-CONTROL-TIEMPO
              MOVE FECHA-PG TO REPSAL-FECHAF-PG
              MOVE HORA-PG TO REPSAL-HORAF-PG
              PERFORM 5000-GRABAR-REPSAL
      *       avanza al próximo registro del cursor
              EXEC SQL
                 fetch vcurs into :SCORE-fecha, :SCORE-score,
                 :SCORE-exclusion, :SCORE-segmento
              END-EXEC
           END-PERFORM
      *    si terminó con error lo publica
           if sqlcode not = 100
                MOVE SQLCODE TO sqlcode-I
                display "Error: cannot fetch " WS-ALIAS sqlcode-I
                sqlerrmc 
           END-IF
           PERFORM 1000-CLOSEVCURS.

       5000-GRABAR-SALIDA.
      ******************************************************************
      *5000-GRABAR-SALIDA.
      *GRABAR ARCHIVO SECUENCIAL SALIDA CON REGISTROS ENCONTRADOS EN BD
      ******************************************************************
           INITIALIZE REG-SEC-SAL
           MOVE SCORE-tipoid TO SAL-TIP-ID
           MOVE SCORE-numid TO SAL-NUM-ID
           MOVE SCORE-fecha TO SAL-FECHA
           MOVE SCORE-score TO SAL-SCORE
           MOVE SCORE-exclusion TO SAL-EXCLUSION
           MOVE SCORE-segmento TO SAL-SEGMENTO
           ADD 1 TO W-GRABADOS
           WRITE REG-SEC-SAL.

       5000-GRABAR-REPSAL.
      ******************************************************************
      *5000-GRABAR-REPSAL.
      *GRABAR ARCHIVO SECUENCIAL REPORTE DE SALIDA PARA MEDIR TIEMPOS
      ******************************************************************
           MOVE ENT-TIP-ID                TO REPSAL-TIP-ID
           MOVE ENT-NUM-ID                TO REPSAL-NUM-ID
           WRITE REG-REP-SAL
           END-WRITE.


       1000-CONECTARSE-A-LAS-BDS.
REMARK******************************************************************
REMARK*1000-CONECTARSE-A-LAS-BDS.
REMARK*    CON LAS CREDENCIALES LEIDAS DE LA TABLA CONEXION SE REALIZAN
REMARK*    TODAS LAS CONEXIONES A LAS BD QUE USA EL PROGRAMA
REMARK******************************************************************
           EXEC SQL
                CONNECT TO :WS-ALIAS USER :WS-USR USING :WS-PASS
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot connect " WS-ALIAS
                sqlcode-I sqlerrmc
           end-if.

       1002-SETCONNECTION-XXXX-DL1.
REMARK*****************************************************************
REMARK*1002-SETCONNECTION-XXXX-DL1.
REMARK* Selecciona la base de datos a utilizar en los querys dado
REMARK* que hay varias abiertas simultaneamente
REMARK*****************************************************************
           exec sql
             SET CONNECTION :WS-ALIAS
           end-exec
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                STRING "Error: cannot set connect " WS-ALIAS
                sqlcode-I sqlerrmc INTO AREA-MSG
                display AREA-MSG
           end-if.
REMARK*   PROCEDIMIENTO PARA VERIFICAR TAKE OVER O FALLA EN LA CONEXION
REMARK*   CON LA BASE DE DATOS; SE REALIZA UN COMMIT PARA VERIFICAR SI
REMARK*   EXISTE LA CONEXION CON LA BASE DE DATOS. SI HAY PROBLEMA
REMARK*   SE INTENTA DE NUEVO LA RECONEXION
           EXEC SQL commit END-EXEC
           IF SQLCODE NOT = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot commit  " WS-ALIAS
                sqlcode-I sqlerrmc
                DISPLAY "SE PROCEDE A DESCONECTAR" WS-ALIAS
                EXEC SQL disconnect :WS-ALIAS END-EXEC
                if sqlcode not = 0
                    MOVE SQLCODE TO sqlcode-I
                    DISPLAY "Error: cannot disconnect " WS-ALIAS
                    sqlcode-I sqlerrmc
                end-if
                DISPLAY "SE PROCEDE A CONECTAR DE NUEVO" WS-ALIAS
                1002-SETCONNECTION-XXXX-DL1
                IF sqlcode = 0
                   DISPLAY "RECONEXION EXITOSA!!!" WS-ALIAS
                ELSE
                   DISPLAY "FALLA EN RECONEXION!!!!" WS-ALIAS
                END-IF
           END-IF.

       1000-CLOSEVCURS.
REMARK****************************************************************
REMARK*1000-CLOSEVCURS.
REMARK*    SE REALIZA LA FUNCION ESTANDAR DE CERRAR EL CURSOR
REMARK****************************************************************
           EXEC SQL
               close vcurs
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                STRING "Error: cannot close cursor " WS-ALIAS
                sqlcode-I sqlerrmc INTO AREA-MSG
                display AREA-MSG
           end-if.

REMARK***************************RUTINAS PARA ADQUIRIR CREDENCIALES ****
       1000-BIND-RUTSQL.
REMARK******************************************************************
REMARK*1000-BIND-RUTSQL.
REMARK* Se realiza el bind dinamico para una base de datos. Se crea
REMARK* un archivo con las instruccines DB2 y se manda a ejecutar.
REMARK* Parametros:
REMARK*    WS-PROGRAMA
REMARK*    WS-ALIAS
REMARK*    WS-USR
REMARK*    WS-PASS
REMARK******************************************************************
      * Se usa un archivito para poner los comandos de shell
           STRING "$TEMPORALES/" WS-PROGRAMA DELIMITED SIZE ".BIND"
           Spa100 INTO BIND-title
           OPEN OUTPUT BIND-arc
      * Creacion del paquete (por si no estaba)
      *    connect
           MOVE SPACES TO BIND-reg.
           STRING "db2 CONNECT TO " WS-ALIAS DELIMITED BY "  "
           " USER " WS-USR DELIMITED BY "  "
           " USING " WS-PASS  DELIMITED BY "  "
           Spa100 INTO BIND-reg
           WRITE BIND-reg.
           MOVE BIND-reg TO BIND-reg-connect.
      *    bind - add collection
           MOVE "HCOBND" TO VAR-AMBIENTE.
           DISPLAY VAR-AMBIENTE UPON ENVIRONMENT-NAME.
           ACCEPT VAR-AMBIENTE FROM ENVIRONMENT-VALUE.
           IF VAR-AMBIENTE = "HCOBND"
              DISPLAY "No se ha fijado: $HCOBND!!!!! "
              STOP RUN
           ELSE
              MOVE SPACES TO WS-ARCHIVO
              STRING
                 VAR-AMBIENTE DELIMITED BY "  "
                 WS-PROGRAMA DELIMITED BY SIZE
                 ".bnd"
              Spa100 INTO WS-ARCHIVO
           END-IF.
           MOVE SPACES TO BIND-reg.
           STRING "db2 bind $HCOBND/"
              WS-PROGRAMA DELIMITED BY SIZE
              ".bnd"
              " ACTION ADD COLLECTION COBOLAPP "
           Spa100 INTO BIND-reg.
           WRITE BIND-reg.
      *    quit
           MOVE "db2 quit" TO BIND-reg.
           WRITE BIND-reg.
           MOVE BIND-reg TO BIND-reg-quit.
REMARK*    REALIZAR LA ACTUALIZACION DEL PAQUETE
      *    connect
           WRITE BIND-reg FROM BIND-reg-connect.
      *    bind
           MOVE SPACES TO BIND-reg.
           STRING "db2 bind $HCOBND/"
              WS-PROGRAMA DELIMITED BY SIZE
              ".bnd"
              " ACTION REPLACE REPLVER V8 RETAIN YES "
              "COLLECTION COBOLAPP "
           Spa100 INTO BIND-reg.
           WRITE BIND-reg.
      *    quit
           WRITE BIND-reg FROM BIND-reg-quit.
           CLOSE BIND-arc.
REMARK*    SE MANDA A EJECUTAR LA MACRO
      *    chmod para que sea ejecutable
           STRING "chmod +x $TEMPORALES/" WS-PROGRAMA DELIMITED SIZE
           ".BIND " X'00' Spa100 INTO TEXTO-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO.
      *    ejecución del comando
           STRING "$TEMPORALES/" WS-PROGRAMA DELIMITED BY SIZE
           ".BIND " X'00' Spa100 INTO TEXTO-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO.
      *    SPS?
           IF I-XX > 0
              MOVE SPACES TO WS-ARCHIVO
              STRING "cp $TEMPORALES/" WS-PROGRAMA DELIMITED BY SIZE
              ".BIND $TEMPORALES/RMBBIND" I-XX DELIMITED SIZE
              X'00' Spa100 INTO TEXTO-COMANDO
              CALL "SYSTEM" USING TEXTO-COMANDO
              ADD 1 TO I-XX
           END-IF.
      *    eliminación del comandito
           STRING "rm $TEMPORALES/" WS-PROGRAMA DELIMITED SIZE ".BIND "
           X'00' Spa100 INTO TEXTO-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO.

       6000-FINALIZAR.
      ****************************************************************
      *6000-FINALIZAR
      * Muestra contadores de registros, cierra archivos y base
      ****************************************************************
           MOVE W-LEIDOS TO W-EDIT-CONTADOR
           DISPLAY "Claves leídas ......... " W-EDIT-CONTADOR
           MOVE W-GRABADOS TO W-EDIT-CONTADOR
           DISPLAY "Claves procesadas OK ......... " W-EDIT-CONTADOR.
      *
           PERFORM 7777-CONTROL-TIEMPO
           DISPLAY " Fin Proceso : " FECHA-PG " " HORA-PG.
      *
           CLOSE SECENT SECSAL.

      *    Cierra la conexion con la base de datos
           EXEC SQL commit END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot commit  " WS-ALIAS
                sqlcode-I sqlerrmc
           end-if
           EXEC SQL disconnect :WS-ALIAS END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot discon  " WS-ALIAS
                sqlcode-I sqlerrmc
           end-if.

       7777-CONTROL-TIEMPO.
      ****************************************************************
      *7777-CONTROL-TIEMPO
      * Obtiene y edita la fecha y la hora del sistema
      ****************************************************************
           ACCEPT FECHA-S FROM DATE
           ACCEPT HORA-SIS FROM TIME
           MOVE HORA-S TO HORA-P
           MOVE MINU-S TO MINU-P
           MOVE SEGU-S TO SEGU-P
           MOVE 20 TO ANIO-SIG1
           MOVE ANIO-S TO ANIO-P
           MOVE MESE-S TO MESE-P
           MOVE DIAS-S TO DIAS-P.
