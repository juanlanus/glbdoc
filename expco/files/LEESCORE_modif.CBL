DEBUG $SET db2(db=INGR_DL1 pass=icgssode.IcgssD06,VALIDATE=RUN)
      $SET db2(connect=2)
      $set db2(BIND COLLECTION=COBOLAPP)
      $set db2(FORMAT=LOC VERSION=V8)
      * LEESCORE.v.01.0001

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LEESCORE.
       AUTHOR.        DATACREDITO.
REMARK******************************************************************
REMARK* ESTE PROGRAMA PERMITE ACCEDER LOS DATOS DE LA BASE DE DATOS DE *
REMARK* SCORE                                                          *
REMARK******************************************************************
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARC-BIND ASSIGN TO DISK TITLE-ARC-BIND
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SECUENCIAL-ENTRADA
               ASSIGN TO DISK                   SEC-ENT-TITLE
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS COD-RET-ENT
               LOCK MODE IS MANUAL WITH LOCK ON RECORD.

           SELECT SECUENCIAL-SALIDA
               ASSIGN TO DISK                   SEC-SAL-TITLE
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS COD-RET-SAL
               LOCK MODE IS MANUAL WITH LOCK ON RECORD.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

           FD ARC-BIND.
           01 REG-BIND PIC X(500).

           FD SECUENCIAL-ENTRADA.
           01 REG-SEC-ENT.
              02 ENT-TIP-ID         PIC 9(01).
              02 ENT-NUM-ID         PIC 9(11).

           FD SECUENCIAL-SALIDA.
           01 REG-SEC-SAL.
              02 SAL-TIP-ID         PIC X(01).
              02 SAL-NUM-ID         PIC X(11).
              02 SAL-FECHA          PIC X(08).
              02 SAL-SCORE          PIC X(03).
              02 SAL-EXCLUSION      PIC X(02).
              02 SAL-SEGMENTO       PIC X(01).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *@DB2CFG_BD=DB2_SSO
      *  Include the SQL Communications Area. This includes the
      *  definitions of SQLCODE, etc
       exec sql include sqlca      end-exec

       01 TITLE-ARC-BIND.
          02 TXT-ARC-BIND           PIC X(80) VALUE SPACES.
       01 TITULO-BD.               
          02 FILLER PIC X(20) VALUE "====================".
          02 FILLER PIC X(10) VALUE "======= [ ".  
          02 TITULO-BD-TIT    PIC X(20).
          02 FILLER PIC X(10) VALUE " ] =======".
          02 FILLER PIC X(20) VALUE "====================".
       01 COD-RET-ENT              PIC X(2) VALUE ZEROS.
       01 COD-RET-SAL              PIC X(2) VALUE ZEROS.
       01 REG-BIND-AUX1            pic X(100) VALUE SPACES.
       01 REG-BIND-AUX2            pic X(100) VALUE SPACES.

       01 WS-TOKEN                 pic X(70) VALUE SPACES.
       01 WS-P1                    pic X(70) VALUE SPACES.
       01 WS-P2                    pic X(70) VALUE SPACES.
       01 WS-DBCONEXION            pic X(70) VALUE SPACES.
       01 WS-PROGRAMA              pic X(06) VALUE SPACES.
       01 WS-ARCHIVO               pic X(70) VALUE SPACES.
       01 WS-DB                    pic X(15).
       01 WS-ALIAS                 pic X(15).
       01 WS-AMBIENTE              pic X(15).
       01 WS-AMBIENTE-INI          pic X(15).
       01 WS-USR                   pic X(15).
       01 WS-PASS                  pic X(18).

       01 I-INT-COL1               PIC 9(04).
       01 SQLCODE-I                PIC 9(08).
       01 INT-COL1                 pic S9(04) comp-5.
       01 C1                       PIC X(40).
       01 C2                       PIC X(40).
       01 C3                       PIC X(40).
       01 C4                       PIC X(40).
       01 int-col                  pic S9(4) comp-5.
       01 ws-dyn-query.
          49 ws-dyn-query-len      pic s9(4) comp-5.
          49 ws-dyn-query-txt      pic x(900).
       01 SEC-ENT-TITLE            PIC X(80)
                                   VALUE "$TEMPORALES/SCOREENT".
       01 SEC-SAL-TITLE            PIC X(80)
                                   VALUE "$TEMPORALES/SCORESAL".

       01 COMANDO                  PIC X(40) VALUE SPACES.
       01 PRIVILEGIO               PIC X(20) VALUE SPACES.
      *** INDICADORAS
      ****************
       01 IND-SEC-ENT                 PIC 9(01) VALUE ZEROS.
REMARK*** CONTADORES
REMARK***************
       01  W-LEIDOS                   PIC  9(12) VALUE ZEROS.
       01  W-GRABADOS                 PIC  9(12) VALUE ZEROS.

       01 VARIABLES-GENERALES.
          03 AREA-MSG                    PIC X(80) VALUE SPACES.
          03 I-XX                        PIC 9(2) VALUE 0.
          03 AREA-OUTPUT                 PIC X(1900).
          03 FILLER  REDEFINES AREA-OUTPUT.
             05 V-OUTPUT                 PIC X(80) OCCURS 24 TIMES.
          03 IND-RMB                     PIC 9(3) VALUE 0.
          03 ETX                         PIC X VALUE X'03'.
          03 IND-CTA                     PIC 9(2) VALUE 1.
          03 FIN-PROGRAMA                PIC 9 VALUE 0.
          03 FIN-BIND                    PIC 9 VALUE 0.
          copy "CCIWS.CPY".
          COPY PE-TIME.WS.
       01 AREA-SUBTITULOS             PIC X(80).
       01 AREA-SUB-SCORE.
          02 FILLER     PIC X(20) VALUE "TIPID          NUMID".
          02 FILLER     PIC X(20) VALUE "FECHA          SCORE".
          02 FILLER     PIC X(20) VALUE "EXCLUSION           ".
          02 FILLER     PIC X(20) VALUE "SEGMENTO            ".
       01 TITULO-ENCABEZADO.
          02 FILLER PIC X(20) VALUE "================= [ ".
          02 FILLER PIC X(20) VALUE "LECTURA  BASE  DATOS".
          02 FILLER PIC X(09) VALUE "SCORE    ".
          02 AMB-ENCABEZADO   PIC X(11) VALUE "DESARROLLO ".
          02 FILLER PIC X(20) VALUE " ] =================".
       01 RAYAS   PIC X(80) VALUE ALL "=".
       01 TEXTO-COMANDO.
          02 T-COMANDO                    PIC X(100).
          02 FILLER                       PIC X VALUE X'00'.
       LINKAGE SECTION.
       SCREEN SECTION.

REMARK******************************************************************
REMARK PROCEDURE DIVISION.
REMARK******************************************************************
REMARK 0000-PROGRAMA-PRINCIPAL.
REMARK******************************************************************
REMARK*0000-PROGRAMA-PRINCIPAL.
REMARK* SE TOMAN LOS PARAMETROS DE EJECUCION, SE PROCEDE DE ACUERDO    *
REMARK* A ESTOS A INICIAR EL TIPO DE COMUNICACION, PROCESA HASTA QUE   *
REMARK* LE LLEGUE LA ORDEN DE TERMINAR,CIERRA ARCHIVOS Y TERMINA       *
REMARK******************************************************************
           DISPLAY "                                                "
           DISPLAY "************************************************"
           DISPLAY "*PROGRAMA PARA BUSCAR REGISTROS EN LA TABLA    *"
           DISPLAY "*REGISTRO.SCORE DE FORMA ALEATORIA             *"
           DISPLAY "************************************************"
           DISPLAY "*                                              *"
           DISPLAY "*           PROGRAMA : LEESCORE .CBL           *"
           DISPLAY "*                                              *"
           DISPLAY "************************************************"
           DISPLAY "                                                "
           PERFORM 10-CONTROL-TIEMPO
      *
           DISPLAY " Fecha Inicio Proceso : " FECHA-PG
           DISPLAY " Hora  Inicio Proceso : " HORA-PG.
      *
           MOVE "LEESCORE" TO WS-PROGRAMA.
           MOVE "'DESARROL'" TO WS-AMBIENTE-INI
           PERFORM 1000-ABRIR-ARCHIVOS.
           PERFORM 2000-LEER-SECUENCIAL-ENTRADA
             UNTIL IND-SEC-ENT = 1.
           PERFORM 1000-CERRAR-XXXX-DL1.
           PERFORM 6000-CERRAR-ARCHIVOS.
           STOP RUN.
      *
       10-CONTROL-TIEMPO.
      ****************************************************************
      *10-CONTROL-TIEMPO
      *RUTINA PARA TOMAR LA FECHA DEL SISTEMA
      ****************************************************************
           ACCEPT FECHA-S                   FROM DATE
           ACCEPT HORA-SIS                  FROM TIME
           MOVE HORA-S                      TO HORA-P
           MOVE MINU-S                      TO MINU-P
           MOVE SEGU-S                      TO SEGU-P
           MOVE 20                          TO ANIO-SIG1
           MOVE ANIO-S                      TO ANIO-P
           MOVE MESE-S                      TO MESE-P
           MOVE DIAS-S                      TO DIAS-P.

       1000-ABRIR-ARCHIVOS.
REMARK******************************************************************
REMARK*1000-ABRIR-ARCHIVOS.
REMARK******************************************************************
           OPEN INPUT  SECUENCIAL-ENTRADA
           IF COD-RET-ENT NOT = "00"
              DISPLAY "Error Abriendo el Archivo SECUENCIAL ENTRADA"
              COD-RET-ENT
              STOP RUN
           END-IF
           OPEN OUTPUT SECUENCIAL-SALIDA
           IF COD-RET-SAL NOT = "00"
              DISPLAY "Error Abriendo el Archivo SECUENCIAL SALIDA"
              COD-RET-SAL
              STOP RUN
           END-IF
           PERFORM 1001-INICIAR-BASEDEDATOS.

       2000-LEER-SECUENCIAL-ENTRADA.
      ******************************************************************
      *2000-LEER-SECUENCIAL-ENTRADA.
      *LECTURA ARCHIVO SECUENCIAL CON REGISTROS DE FORMA ALEATORIA
      ******************************************************************
           READ SECUENCIAL-ENTRADA NEXT RECORD
               AT END MOVE 1              TO IND-SEC-ENT
           END-READ.
           IF IND-SEC-ENT NOT = 1
              ADD 1                       TO W-LEIDOS
              PERFORM 3000-LEER-BD
           END-IF.

       3000-LEER-BD.
REMARK****************************************************************
REMARK*3000-LEER-BD.
REMARK*  LEER REGISTROS BASE DE DATOS SCORE
REMARK****************************************************************
           MOVE TITULO-ENCABEZADO TO AREA-MSG.
           PERFORM 9600-ABLOCAR.
           PERFORM 4000-BUSCAR-SCORE
           PERFORM 9700-FIN-ABLOCAR.

       4000-BUSCAR-SCORE.
REMARK******************************************************************
REMARK*4000-BUSCAR-SCORE.
REMARK*    BUSCAR REGISTROS EN TABLA REGISTRO.SCORE
REMARK******************************************************************
           MOVE SPACES TO WS-DYN-QUERY-TXT.
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING
             "SELECT FECHA,"
                 " SCORE,"
                 " EXCLUSION,"
                 " SEGMENTO"
             " FROM REGISTRO.SCORE"
             " WHERE TIPOID ="
                     ENT-TIP-ID
                " AND NUMID =" ENT-NUM-ID
             INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.
           SUBTRACT 1                  FROM     WS-DYN-QUERY-LEN
           MOVE AREA-SUB-SCORE TO AREA-SUBTITULOS
           MOVE "       SCORE         " TO TITULO-BD-TIT
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE AREA-SUBTITULOS     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE RAYAS               TO AREA-MSG
           PERFORM 9600-ABLOCAR
           MOVE "- REGISTROS EN LA TABLA DE SCORE"  TO AREA-MSG.
           PERFORM 9600-ABLOCAR
*
           PERFORM 1000-SETCONNECTION-XXXX-DL1.
           PERFORM 1000-BUSCAR-REG-SCORE.
*
       1001-INICIAR-BASEDEDATOS.
REMARK******************************************************************
REMARK*1001-INICIAR-BASEDEDATOS.
REMARK* SE DECLARA EL CUROSR GENERICO A SER USADO POR LOS QUERYS
REMARK* DINAMICOS Y DE ADQUIEREN TODAS LAS CREDENCIALES PARA CONECTARSE
REMARK* A LAS BASES DE DATOS
REMARK******************************************************************
           EXEC SQL
               declare vcurs cursor for dynamic_sql
           END-EXEC
           if sqlcode not = 0
                display "Error: cannot declare "
                display sqlcode
                display sqlerrmc
                stop run
           end-if.
      *    PERFORM 1000-CREDENCIALES-DB2SSO-RUTSQL.
           PERFORM 1000-BUSCA-USUPASS.
      *    PERFORM 1000-HACER-BINDS-DINAMICOS.
           PERFORM 1000-CONECTARSE-A-LAS-BDS.


       1000-BUSCAR-REG-SCORE.
REMARK*******************************************************************
REMARK*1000-BUSCAR-REG-SCORE.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 1000-PREPARE.
           INITIALIZE C1,C2,C3,C4 .
           EXEC SQL
               fetch vcurs into :C1,:C2,:C3,:C4
           END-EXEC
           if sqlcode = 100
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM UNTIL SQLCODE NOT =0
              MOVE SPACES TO AREA-MSG
              MOVE INT-COL1 TO I-INT-COL1

              STRING C1(1:04)
                     C1(6:02)
                     C1(9:02)
                     " "
                     C2(1:03)
                     " "
                     C3(1:02)
                     "  "
                     C4(1:01)
              INTO AREA-MSG
              PERFORM 9600-ABLOCAR
              PERFORM 5000-GRABAR-SALIDA
              EXEC SQL
                  fetch vcurs into :C1,:C2,:C3,:C4
              END-EXEC
           END-PERFORM.
           if sqlcode not = 100
                MOVE SQLCODE TO sqlcode-I
                STRING "Error: cannot fetch   ",
                       WS-ALIAS
                       sqlcode-I
                       sqlerrmc
                INTO AREA-MSG
                PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM 1000-CLOSEVCURS.

      ******************************************************************
      *5000-GRABAR-SALIDA.
      *GRABAR ARCHIVO SECUENCIAL SALIDA CON REGISTROS ENCONTRADOS EN BD
      ******************************************************************
       5000-GRABAR-SALIDA.
           INITIALIZE REG-SEC-SAL
           MOVE ENT-TIP-ID                TO SAL-TIP-ID
           MOVE ENT-NUM-ID                TO SAL-NUM-ID
           MOVE C1(1:4)                   TO SAL-FECHA(1:4)
           MOVE C1(6:2)                   TO SAL-FECHA(5:2)
           MOVE C1(9:2)                   TO SAL-FECHA(7:2)
           MOVE C2(1:3)                   TO SAL-SCORE
           MOVE C3(1:2)                   TO SAL-EXCLUSION
           MOVE C4(1:1)                   TO SAL-SEGMENTO
           ADD 1                          TO W-GRABADOS
           WRITE REG-SEC-SAL
           END-WRITE.


       1000-BUSCA-USUPASS.
REMARK******************************************************************
REMARK*1000-BUSCA-USUPASS.
REMARK*    PARA CADA BASE DE DATOS SE BUSCAN LAS CREDENCIALES PARA LA
REMARK*    CONEXION A LA BASE DE DATOS A CONSULTAR
REMARK******************************************************************
           MOVE "lstxclde"      TO WS-USR
           MOVE "LstxcD8"       TO WS-PASS
           MOVE "LIST_DL1"      TO WS-ALIAS
           PERFORM 1000-BIND-RUTSQL.

       1000-CONECTARSE-A-LAS-BDS.
REMARK******************************************************************
REMARK*1000-CONECTARSE-A-LAS-BDS.
REMARK*    CON LAS CREDENCIALES LEIDAS DE LA TABLA CONEXION SE REALIZAN
REMARK*    TODAS LAS CONEXIONES A LAS BD QUE USA EL PROGRAMA 
REMARK******************************************************************
           PERFORM 1000-CONECTAR-XXXX-DL1.
       1000-CONECTAR-XXXX-DL1.
REMARK******************************************************************
REMARK*1000-CONECTAR-XXXX-DL1.
REMARK*    RUTINA PARA REALIZAR UNA CONEXION ESTANDARD A UNA BD
REMARK******************************************************************
           EXEC SQL
                CONNECT TO :WS-ALIAS USER :WS-USR USING :WS-PASS
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot connect ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
           end-if.
       1000-SETCONNECTION-XXXX-DL1.
REMARK*****************************************************************
REMARK*1000-SETCONNECTION-XXXX-DL1.
REMARK*    SE SELECCIONA LA BASE DE DATOS A UTILIZAR EN LOS QUERYS DADO
REMARK*    QUE HAY VARIAS ABIERTAS SIMULTANEAMENTE
REMARK*****************************************************************
           exec sql
             SET CONNECTION :WS-ALIAS
           end-exec
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                STRING "Error: cannot set connect ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                INTO AREA-MSG
                PERFORM 9600-ABLOCAR
           end-if.
REMARK*   PROCEDIMIENTO PARA VERIFICAR TAKE OVER O FALLA EN LA CONEXION
REMARK*   CON LA BASE DE DATOS; SE REALIZA UN COMMIT PARA VERIFICAR SI
REMARK*   EXISTE LA CONEXION CON LA BASE DE DATOS. SI HAY PROBLEMA
REMARK*   SE INTENTA DE NUEVO LA RECONEXION
           EXEC SQL
               commit
           END-EXEC.
           IF SQLCODE NOT = 0 
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot commit  ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                DISPLAY "SE PROCEDE A DESCONECTAR",WS-ALIAS
                PERFORM 1000-DISCONNECT
                DISPLAY "SE PROCEDE A CONECTAR DE NUEVO",WS-ALIAS
                PERFORM 1000-CONECTAR-XXXX-DL1
                IF sqlcode = 0
                   DISPLAY "RECONEXION EXITOSA!!!",WS-ALIAS
                ELSE
                   DISPLAY "FALLA EN RECONEXION!!!!",WS-ALIAS
                END-IF
           END-IF.

       1000-CERRAR-XXXX-DL1.
REMARK****************************************************************
REMARK*1000-CERRAR-XXXX-DL1.
REMARK*    SE CIERRA LA CONEXION CON LA BASE DE DATOS ELEGIDA
REMARK****************************************************************
           exec sql
             SET CONNECTION :WS-ALIAS
           end-exec
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot set connect ",WS-ALIAS
                   sqlcode-I
                   sqlerrmc
           end-if.
           EXEC SQL
               commit                  
           END-EXEC.
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot commit ",WS-ALIAS
                   sqlcode-I
                   sqlerrmc
           end-if.
           PERFORM 1000-DISCONNECT.

       1000-DISCONNECT.
REMARK****************************************************************
REMARK*1000-DISCONNECT.
REMARK*    SE REALIZA LA FUNCION ESTANDARD DE DESCONEXION
REMARK****************************************************************
           EXEC SQL
               disconnect :WS-ALIAS
           END-EXEC.
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot disconnect ",WS-ALIAS
                   sqlcode-I
                   sqlerrmc
           end-if.

       1000-PREPARE.
REMARK****************************************************************
REMARK*1000-PREPARE.
REMARK*    SE REALIZA LA FUNCION ESTANDAR DE PREPARAR EL QUERY
REMARK*    Y DE ABRIR EL CURSOR PARA LAS BUSQUEDAS                 
REMARK****************************************************************
           EXEC SQL
               prepare dynamic_sql from :ws-dyn-query
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                STRING "Error: cannot prepare ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                INTO AREA-MSG
                PERFORM 9600-ABLOCAR
           end-if.
*
           EXEC SQL
               open vcurs using :int-col
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                STRING "Error: cannot open vc ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                INTO AREA-MSG
                PERFORM 9600-ABLOCAR
           end-if.

       1000-CLOSEVCURS.
REMARK****************************************************************
REMARK*1000-CLOSEVCURS.
REMARK*    SE REALIZA LA FUNCION ESTANDAR DE CERRAR EL CURSOR
REMARK****************************************************************
           EXEC SQL
               close vcurs
           END-EXEC.
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                STRING "Error: cannot clos vc ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                INTO AREA-MSG
                PERFORM 9600-ABLOCAR
           end-if.

       9600-ABLOCAR.
REMARK******************************************************************
REMARK*9600-ABLOCAR.
REMARK* ESTA RUTINA SE PUEDE INVOCAR PARA MANDAR MENSAJES ABLOCADOS,   *
REMARK* LA VARIABLE IND-CTA CONTROLA LA POSICION DENTRO DE V-OUTPUT    *
REMARK* AL ACABAR DE RESPONDER UNA TRANSACCION SIEMPRE HAY QUE         *
REMARK* INVOVAR LA RUTINA 9700-FIN-ABLOCAR PARA DESOCUPAR EL BUFFER    *
REMARK* LOS MENSAJES SON DE 80 BYTES Y MAXIMO 24
REMARK******************************************************************
           MOVE AREA-MSG TO V-OUTPUT(IND-CTA)
           ADD 1 TO IND-CTA
           IF IND-CTA > 16 
              PERFORM 10900-ENVIA-CRCR
              MOVE 1 TO IND-CTA
           END-IF.

       9700-FIN-ABLOCAR.
REMARK******************************************************************
REMARK*9700-FIN-ABLOCAR.
REMARK* RUTINA QUE SE ENCARGA DE DESOCUPAR EL BUFFER AL CULMINAR LA    *
REMARK* RESPUESTA DE UNA TRANSACCION.                                  *
REMARK******************************************************************
           IF IND-CTA > 1 
              PERFORM 10900-ENVIA-CRCR 
           END-IF.

       10900-ENVIA-CRCR.
REMARK******************************************************************
REMARK*10900-ENVIA-CRCR.
REMARK* SE DETERMINA SI SE REALIZA UN DISPLAY O SE MANDA LA RESPUESTA  *
REMARK* POR MEDIO DE MODULO CCI.                                       *
REMARK******************************************************************
           MOVE 1 TO IND-RMB
           PERFORM 10920-ENVIA-DISPLAY.

       10920-ENVIA-DISPLAY.
REMARK******************************************************************
REMARK*10920-ENVIA-DISPLAY.
REMARK* EN CASO DE QUE EL PARAMETRO DE EJECUACION NO FUE SERVER SE     *
REMARK* PROCEDE A REALIZAR DISPLAY EN CAMBIO DE MANDAR AL MODULO CCI   *
REMARK******************************************************************
           IF IND-CTA = 1 
              PERFORM 10900-DISPLAY
           ELSE
              SUBTRACT 1 FROM IND-CTA
              PERFORM 10900-DISPLAY IND-CTA TIMES
           END-IF
           MOVE 1 TO IND-CTA
           DISPLAY ETX WITH NO ADVANCING.

       10900-DISPLAY. 
REMARK******************************************************************
REMARK*10900-DISPLAY.
REMARK* SE REALIZA EL DISPLAY DE LOS MENSAJES A RETORNAR               *
REMARK******************************************************************
           DISPLAY V-OUTPUT(IND-RMB)
           ADD 1 TO IND-RMB. 

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
           MOVE SPACES TO TXT-ARC-BIND
           STRING
              "$TEMPORALES/"
              WS-PROGRAMA DELIMITED BY SIZE
              ".BIND" 
           INTO TXT-ARC-BIND.
           OPEN OUTPUT ARC-BIND.
           MOVE SPACES TO REG-BIND.
           STRING "db2 CONNECT TO "
              WS-ALIAS DELIMITED BY "  "
              " USER "
              WS-USR    DELIMITED BY "  "
              " USING "
              WS-PASS  DELIMITED BY "  "
           INTO REG-BIND.
           WRITE REG-BIND.
           MOVE REG-BIND TO REG-BIND-AUX1.
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
              INTO WS-ARCHIVO 
           END-IF.
           MOVE SPACES TO REG-BIND.
           STRING "db2 bind $HCOBND/"
              WS-PROGRAMA DELIMITED BY SIZE
              ".bnd"             
              " ACTION ADD COLLECTION COBOLAPP "        
           INTO REG-BIND.
           WRITE REG-BIND.
           MOVE "db2 quit" TO REG-BIND.
           WRITE REG-BIND.
           MOVE REG-BIND TO REG-BIND-AUX2.
REMARK*    REALIZAR LA ACTUALIZACION DEL PAQUETE  
           WRITE REG-BIND FROM REG-BIND-AUX1.
           MOVE SPACES TO REG-BIND.
           STRING "db2 bind $HCOBND/"
              WS-PROGRAMA DELIMITED BY SIZE
              ".bnd"             
              " ACTION REPLACE REPLVER V8 RETAIN YES "
              "COLLECTION COBOLAPP "
           INTO REG-BIND.
           WRITE REG-BIND.
           WRITE REG-BIND FROM REG-BIND-AUX2.
           CLOSE ARC-BIND.
REMARK*    SE MANDA A EJECUTAR LA MACRO
           MOVE SPACES TO WS-ARCHIVO 
           STRING 
                 "chmod +x $TEMPORALES/"
                 WS-PROGRAMA  DELIMITED BY SIZE 
                 ".BIND "               
           INTO WS-ARCHIVO 
           MOVE WS-ARCHIVO  TO T-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO.
           MOVE SPACES TO WS-ARCHIVO.
           STRING 
                 "$TEMPORALES/"
                 WS-PROGRAMA  DELIMITED BY SIZE 
                 ".BIND "               
           INTO WS-ARCHIVO 
           MOVE WS-ARCHIVO  TO T-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO.
           IF I-XX > 0
              MOVE SPACES TO WS-ARCHIVO
              STRING 
                    "cp $TEMPORALES/"
                    WS-PROGRAMA  DELIMITED BY SIZE 
                    ".BIND $TEMPORALES/RMBBIND"               
                    I-XX DELIMITED BY SIZE
              INTO WS-ARCHIVO 
              MOVE WS-ARCHIVO  TO T-COMANDO
              CALL "SYSTEM" USING TEXTO-COMANDO
              ADD 1 TO I-XX 
           END-IF.
           MOVE SPACES TO WS-ARCHIVO.
           STRING 
                 "rm $TEMPORALES/"
                 WS-PROGRAMA  DELIMITED BY SIZE 
                 ".BIND "               
           INTO WS-ARCHIVO 
           MOVE WS-ARCHIVO  TO T-COMANDO.
           CALL "SYSTEM" USING TEXTO-COMANDO.

       2000-HALLAR-USU-PASS-RUTSQL. 
REMARK*****************************************************************
REMARK*2000-HALLAR-USU-PASS-RUTSQL. 
REMARK*    SE BUSCA EN LA TABLA CONEXION LAS CREDENCIALES PARA
REMARK*    ACCEDER A LA BASES DE DATOS A UTILIZAR. SE ACCEDEN POR
REMARK*    AMBIENTE Y NOMBRE DE LA BASE DE DATOS.
REMARK*    PARAMETROS DE ENTRADA:
REMARK*         WS-ALIAS  (USADO PARA LA CONEXION A LA TABLA)
REMARK*         WS-USR    (USADO PARA LA CONEXION A LA TABLA)
REMARK*         WS-PASS   (USADO PARA LA CONEXION A LA TABLA)
REMARK*         WS-AMBIENTE (USADO PARA EL QUERY A LA TABLA)
REMARK*         WS-DB       (USADO PARA EL QUERY A LA TABLA)
REMARK*    SALIDAS
REMARK*         C1   USUARIO (RESPUESTA DEL QUERY)
REMARK*         C2   PASSWOR (RESPUESTA DEL QUERY)
REMARK*         C3   ALIAS   (RESPUESTA DEL QUERY)
REMARK*****************************************************************
           MOVE SPACES TO WS-DYN-QUERY-TXT.
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING
             "SELECT "
                 " NOMBRE_USER,"
                 " CONTRASEÑA,"
                 " ALIAS"
             " FROM PARAM.CONEXION "
             " WHERE AMBIENTE="
                WS-AMBIENTE
                " AND NOMBRE_BD="
                WS-DB
                " AND APLICACION='"
                WS-PROGRAMA
                "' WITH UR"
             INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.
           SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN
      *
           EXEC SQL
                CONNECT TO :WS-ALIAS USER :WS-USR USING :WS-PASS
           END-EXEC
           IF sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot connect ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           END-IF.
      *
           EXEC SQL
               prepare dynamic_sql from :ws-dyn-query
           END-EXEC
           IF sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot prepare ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
               STOP RUN
           END-IF.
      *
           EXEC SQL
               open vcurs using :int-col
           END-EXEC
           IF sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot open vc ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           END-IF.
      *
           INITIALIZE C1,C2,C3.
           EXEC SQL
               fetch vcurs into :C1,:C2,:C3
           END-EXEC
           IF sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot fetch   ",
                       WS-ALIAS," "
                       WS-DB," "
                       WS-AMBIENTE," "
                       sqlcode-I
                       sqlerrmc
                IF sqlcode = 100
                   MOVE "NOHAY" TO C1 C2 C3
                ELSE
                   STOP RUN
                END-IF
           END-IF.
      *
           EXEC SQL
               close vcurs
           END-EXEC.
           IF sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot clos vc ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           END-IF.
           EXEC SQL
               commit
           END-EXEC.
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot commit  ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           end-if.
           EXEC SQL
               disconnect current
           END-EXEC.
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot discon  ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           end-if.
       9999-GENERAR-MASIVO.
           MOVE SPACES TO TXT-ARC-BIND.
           STRING
               "$TEMPORALES/"
               WS-PROGRAMA DELIMITED BY SIZE
               ".MASIVO"
           INTO TXT-ARC-BIND.
           OPEN OUTPUT ARC-BIND.
           MOVE "P0000000000000000" TO REG-BIND.
           WRITE REG-BIND.
           CLOSE ARC-BIND.
      ******************************************************************
      *6000-CERRAR-ARCHIVOS.
      *SE CIERRAN LOS ARCHIVOS QUE SE UTILIZAN EN EL PROGRAMA
      ******************************************************************
       6000-CERRAR-ARCHIVOS.

           PERFORM 10-CONTROL-TIEMPO
      *
           DISPLAY " Fecha Fin    Proceso : " FECHA-PG
           DISPLAY " Hora  Fin    Proceso : " HORA-PG.
      *

           CLOSE SECUENCIAL-ENTRADA SECUENCIAL-SALIDA.

