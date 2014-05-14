       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ACTALEJUR.
REMARK*******************************************************************
REMARK*   PROGRAMA QUE ACTUALIZA EL ARCHIVO DE ALERTAS CON LA INFORMACION
REMARK*   ENTREGADA POR JURISCOOP 
REMARK*******************************************************************
       AUTHOR.        DATACREDITO.
       DATE-WRITTEN.
       DATE-COMPILED.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MICROFOCUS.
       OBJECT-COMPUTER. MICROFOCUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY BDIIREGI.FS.
       COPY BDIIDIAN.FS.
       COPY BDIIDAS.FS.
       COPY BDIIALE.FS.
       SELECT ENTRADA ASSIGN TO "$TEMPORALES/ACTALEJUR.ENT"
                   ORGANIZATION IS LINE SEQUENTIAL.
            SELECT SALIDA ASSIGN TO "$TEMPORALES/ACTALEJUR.SAL"
                   ORGANIZATION IS LINE SEQUENTIAL.
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       COPY BDIIREGI.FD. 
       COPY BDIIDIAN.FD. 
       COPY BDIIDAS.FD. 
       COPY BDIIALE.FD.
       FD ENTRADA.
       01 REG-ENTRADA                   PIC X(200).
       FD SALIDA.
       01 REG-SALIDA.                    
          02 ERRORES-SALIDA              PIC X(50).
          02 FILLER-SALIDA               PIC X(1).
          02 REGISTRO-SALIDA             PIC X(200).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       COPY BDIIVIDE.WS.     
       COPY BDIIALE.WS.      
       COPY VALNOM.WS.       
       COPY ICFILE.WS.
       01 PARAMETROS.
          03 COMANDO                     PIC X(80)  VALUE SPACES.
          03 LINEA                       PIC X(80)  VALUE SPACES.
       01 INDICES.
          03 I                           PIC 9(5) VALUE 0.
          03 J                           PIC 9(5) VALUE 0.
       01 CONTADORES.
          03 CONT-LEIDOS                 PIC 9(10)  VALUE 0.
          03 CONT-WRITE-OK               PIC 9(10)  VALUE 0.
          03 CONT-WRITE-ERR              PIC 9(10)  VALUE 0.
          03 CONT-REWRITE-OK             PIC 9(10)  VALUE 0.
          03 CONT-REWRITE-ERR            PIC 9(10)  VALUE 0.
          03 CONT-REGISTROS-OK           PIC 9(10)  VALUE 0.
          03 CONT-REGISTROS-ERR          PIC 9(10)  VALUE 0.
OJO    01 LINEA1-AUX.
          02 FILLER    PIC X(29) 
                       VALUE "DEMANDAS JUDICIALES VIGENTES:".
          02 NUM-VIG-1 PIC 9(05) VALUE 0.  
          02 FILLER    PIC X(08) VALUE " ULTIMA:".                 
          02 FEC-VIG-1 PIC 9(08) VALUE 0.   
OJO    01 LINEA2-AUX.
          02 FILLER    PIC X(31) 
                       VALUE "DEMANDAS JUDICIALES TERMINADAS:".
          02 NUM-TER-2 PIC 9(05) VALUE 0.  
          02 FILLER    PIC X(14) VALUE SPACES.
       01 VARIABLES-GENERALES.
          03 REAL                        PIC 9      VALUE 0.
          03 FIN-ENTRADA                 PIC 9      VALUE 0.
          03 HAY-BDIIALE                 PIC 9      VALUE 0.
          03 FECHA-PROCESO               PIC 9(8)   VALUE 0.
          03 FECHA-VENCIMIENTO           PIC 9(8)   VALUE 0.
          03 DIAS-DESDE-1600             PIC 9(8)   VALUE 0.
       01 VALIDACIONES-FECHAS.
          02 ANO-VAL-X                   PIC 9(04).
             88 ANO-VAL  VALUE 1900 THRU 2050.
          02 MES-VAL-X                   PIC 9(02).
             88 MES-VAL  VALUE 01   THRU 12.
          02 DIA-VAL-X                   PIC 9(02).
             88 DIA-VAL  VALUE 01   THRU 31.     
       01 INF-DATAHOST-AUX.
          02 NUM-DEMA-VIG-AUX            PIC 9(5).
          02 NUM-DEMA-TER-AUX            PIC 9(5).
          02 FEC-DEMA-VIG-AUX            PIC 9(8).
       01 VARIABLES-INPUT.
          03 TIPO-IDE-INP                PIC 9.
          03 NUM-IDE-INP                 PIC 9(11).
          03 NOMBRES-INP                 PIC X(30).
          03 APELLIDO-PAT-INP            PIC X(30).
          03 APELLIDO-MAT-INP            PIC X(30).
          03 NUM-DEMA-VIG-INP            PIC 9(05).
          03 NUM-DEMA-TER-INP            PIC 9(05).
          03 FEC-DEMA-VIG-INP            PIC 9(08).
          03 TIPO-IDE-INP-X              PIC X.
          03 NUM-IDE-INP-X               PIC X(11).
          03 NUM-DEMA-VIG-INP-X          PIC X(05).
          03 NUM-DEMA-TER-INP-X          PIC X(05).
          03 FEC-DEMA-VIG-INP-X          PIC X(08).
       01 M-ERRORES.
          03 V-ERR OCCURS 50 TIMES   PIC 9(1).
       PROCEDURE DIVISION.
       0000-PROGRAMA-PRINCIPAL.
      ******************************************************************
REMARK*    PROGRAMA QUE EXTRAE INFORMACION DEL ARCHIVO DE LA REGIS
REMARK*    EJECUCION : X RMBREGIS 10 1                                  
REMARK*     MAXIMO HUECO DE 10,MINIMO DE UNO (DEFAULT SI NO SE DA NADA)
      ******************************************************************
           ACCEPT LINEA FROM COMMAND-LINE.
           UNSTRING LINEA   DELIMITED BY ALL " " INTO
              COMANDO 
           EVALUATE COMANDO 
             WHEN "CREARDEF"
                PERFORM 1000-CREARDEF                        
             WHEN "REAL"            
                MOVE 1 TO REAL
                OPEN I-O BDIIALE
                PERFORM 1000-PROCESAR-DEMANDAS
             WHEN "FALSO"                 
                MOVE 0 TO REAL
                OPEN INPUT BDIIALE
                PERFORM 1000-PROCESAR-DEMANDAS
             WHEN ANY 
               DISPLAY "COMANDO INVALIDO:" COMANDO(1:10)
               DISPLAY "A) REAL          "
               DISPLAY "B) FALSO         "
           END-EVALUATE.
           STOP RUN.
       1000-CREARDEF.
           MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-PROCESO.
           OPEN I-O BDIIALE.
           MOVE 0               TO TIP-IDE-BDIIALE.   
           MOVE 0               TO NUM-IDE-BDIIALE.   
           MOVE 000001          TO FUENTE-BDIIALE
           MOVE 001             TO COD-ALERTA-BDIIALE
           MOVE "a"             TO LLASEC-BDIIALE.
           MOVE 1 TO HAY-BDIIALE.
           READ BDIIALE
                INVALID KEY 
                  MOVE 0 TO HAY-BDIIALE
           END-READ.
           IF HAY-BDIIALE = 0
              MOVE 
              "LA INDENTIFICACION NO TIENE NI HA TENIDO PROCESOS "
                  TO LINEA1-BDIIALE
              MOVE 
              "JUDICIALES REGISTRADOS                            "
                  TO LINEA2-BDIIALE
              MOVE FECHA-PROCESO              TO FEC-NOV-BDIIALE
                                                 FEC-REP-BDIIALE
              MOVE 20501231                   TO FEC-VEN-BDIIALE
              MOVE 0                          TO BLOQUEO-BDIIALE
              MOVE ZEROS                      TO INF-DATAHOST-AUX
              MOVE INF-DATAHOST-AUX           TO INF-DATAHOST-BDIIALE
              MOVE SPACES                     TO FILLER-BDIIALE
              WRITE REG-BDIIALE
                 INVALID KEY
                   DISPLAY "ERROR EN REWRITE"    
                 NOT INVALID KEY
                   DISPLAY "ALERTA CREADA OK"    
              END-WRITE
           ELSE
              DISPLAY "YA EXISTE LA ALERTA"
           END-IF.
       1000-PROCESAR-DEMANDAS.   
REMARK****************************************************************
REMARK****************************************************************
           PERFORM 2000-ABRIR-ARCHIVOS.
           READ ENTRADA AT END MOVE 1 TO FIN-ENTRADA.
           PERFORM UNTIL FIN-ENTRADA = 1
             ADD 1 TO CONT-LEIDOS
             PERFORM 4000-PROCESAR-ENTRADA
             READ ENTRADA AT END MOVE 1 TO FIN-ENTRADA
             END-READ
           END-PERFORM.
           PERFORM 6000-SACAR-RESUMEN.
       2000-ABRIR-ARCHIVOS.
REMARK****************************************************************
REMARK****************************************************************
           OPEN INPUT BDIIREGI.
           OPEN INPUT BDIIDIAN.
           OPEN INPUT BDIIDAS.
           OPEN INPUT ENTRADA.
           OPEN OUTPUT SALIDA.
       4000-PROCESAR-ENTRADA.
REMARK****************************************************************
REMARK****************************************************************
           STRING REG-ENTRADA DELIMITED BY "                  "
                  ";"
           INTO REG-ENTRADA.
           INITIALIZE VARIABLES-INPUT.
           UNSTRING REG-ENTRADA DELIMITED BY ALL ";" INTO
             TIPO-IDE-INP-X
             NUM-IDE-INP-X     
             NOMBRES-INP       
             APELLIDO-PAT-INP  
             APELLIDO-MAT-INP  
             NUM-DEMA-VIG-INP-X
             NUM-DEMA-TER-INP-X
             FEC-DEMA-VIG-INP-X .
           UNSTRING REG-ENTRADA DELIMITED BY ALL ";" INTO
             TIPO-IDE-INP       
             NUM-IDE-INP       
             NOMBRES-INP       
             APELLIDO-PAT-INP  
             APELLIDO-MAT-INP  
             NUM-DEMA-VIG-INP  
             NUM-DEMA-TER-INP  
             FEC-DEMA-VIG-INP .
           PERFORM 4100-VALIDACIONES-BASICAS.
           IF M-ERRORES = ZEROS 
              PERFORM 4200-VALIDACION-OFICIALES
           END-IF
           IF M-ERRORES = ZEROS 
              PERFORM 4300-MODIFICAR-BDIIALE
              ADD 1 TO CONT-REGISTROS-OK
           ELSE
              PERFORM 4400-GRABAR-REGISTROS-ERR
              ADD 1 TO CONT-REGISTROS-ERR
           END-IF.
       4100-VALIDACIONES-BASICAS.
REMARK****************************************************************
REMARK****************************************************************
           INSPECT TIPO-IDE-INP-X     REPLACING ALL " " BY "0"
           INSPECT NUM-IDE-INP-X      REPLACING ALL " " BY "0"
           INSPECT NUM-DEMA-VIG-INP-X REPLACING ALL " " BY "0"
           INSPECT NUM-DEMA-TER-INP-X REPLACING ALL " " BY "0"
           INSPECT FEC-DEMA-VIG-INP-X REPLACING ALL " " BY "0"
           MOVE ZEROS TO M-ERRORES.
           IF (TIPO-IDE-INP <> 1 AND TIPO-IDE-INP <> 4) OR
               TIPO-IDE-INP-X NOT NUMERIC
              MOVE 1 TO V-ERR(1)
           END-IF.
           IF NUM-IDE-INP-X NOT NUMERIC
              MOVE 1 TO V-ERR(2)
           END-IF.
           IF NOMBRES-INP = SPACES OR NOMBRES-INP NUMERIC
              MOVE 1 TO V-ERR(3)
           END-IF.
           IF APELLIDO-PAT-INP  NUMERIC
              MOVE 1 TO V-ERR(4)
           END-IF.
           IF APELLIDO-MAT-INP  NUMERIC
              MOVE 1 TO V-ERR(5)
           END-IF.
           IF APELLIDO-PAT-INP  NUMERIC
              MOVE 1 TO V-ERR(5)
           END-IF.
           IF NUM-DEMA-VIG-INP-X NOT NUMERIC
              MOVE 1 TO V-ERR(6)
           END-IF.
           IF NUM-DEMA-TER-INP-X NOT NUMERIC
              MOVE 1 TO V-ERR(7)
           END-IF.
           IF FEC-DEMA-VIG-INP-X NOT NUMERIC
              MOVE 1 TO V-ERR(8)
           ELSE
              MOVE FEC-DEMA-VIG-INP(1:4) TO ANO-VAL-X(1:4)
              MOVE FEC-DEMA-VIG-INP(5:2) TO MES-VAL-X(1:2)
              MOVE FEC-DEMA-VIG-INP(5:2) TO DIA-VAL-X(1:2)
              IF NOT ANO-VAL OR NOT MES-VAL OR NOT DIA-VAL
                 MOVE 1 TO V-ERR(8)
              END-IF
           END-IF.
*          TRIANGULACIONES
           IF APELLIDO-PAT-INP = SPACES AND APELLIDO-MAT-INP = SPACES
              MOVE 1 TO V-ERR(21)
           END-IF.
           IF NUM-DEMA-TER-INP = ZEROS AND NUM-DEMA-VIG-INP = ZEROS
              MOVE 1 TO V-ERR(22)
           END-IF.
       4200-VALIDACION-OFICIALES.
REMARK****************************************************************
REMARK****************************************************************
           MOVE TIPO-IDE-INP    TO TIP-IDE-BDIIVIDE.
           MOVE NUM-IDE-INP     TO NUM-IDE-BDIIVIDE.
           MOVE SPACES TO NOMBRE-BDIIVIDE.
           STRING NOMBRES-INP      DELIMITED BY "   "
                  " "
                  APELLIDO-PAT-INP DELIMITED BY "   "
                  " "
                  APELLIDO-MAT-INP DELIMITED BY "   "
           INTO NOMBRE-BDIIVIDE.
           MOVE 1 TO SW-VERIFNOM-BDIIVIDE          
           MOVE 0 TO SW-DIGCHEQUEO-BDIIVIDE SW-ASGTIPID-BDIIVIDE  
           PERFORM 80100-VALIDAR-IDENTIFICACION                  
OJO        IF SW-ID-BDIIVIDE = 0    *> EXISTE LA CEDULA
              IF SW-NOMBRES-BDIIVIDE = 1    *> NOMBRE NO COINCIDE
                 MOVE 1 TO V-ERR(31)
              END-IF
           END-IF.
       4300-MODIFICAR-BDIIALE.
REMARK****************************************************************
REMARK****************************************************************
           MOVE TIPO-IDE-INP    TO TIP-IDE-BDIIALE.   
           MOVE NUM-IDE-INP     TO NUM-IDE-BDIIALE.   
           MOVE 000001          TO FUENTE-BDIIALE
           MOVE 001             TO COD-ALERTA-BDIIALE
           MOVE "a"             TO LLASEC-BDIIALE.
           MOVE 1 TO HAY-BDIIALE.
           READ BDIIALE
                INVALID KEY 
                  MOVE 0 TO HAY-BDIIALE
           END-READ.
           PERFORM 4310-CALCULAR-FECHAS.
           PERFORM 4320-ARMAR-TEXTO-ALERTA.
           IF HAY-BDIIALE = 0
              PERFORM 4330-CREAR-BDIIALE
           ELSE
              PERFORM 4340-ACTUALIZAR-BDIIALE
           END-IF.
       4310-CALCULAR-FECHAS.
REMARK****************************************************************
REMARK****************************************************************
           MOVE FUNCTION CURRENT-DATE (1:8) TO FECHA-PROCESO 
           MOVE FUNCTION INTEGER-OF-DATE (FECHA-PROCESO)     
                                            TO DIAS-DESDE-1600                        
OJO        ADD  360                         TO DIAS-DESDE-1600                
           MOVE FUNCTION DATE-OF-INTEGER (DIAS-DESDE-1600)      
                                            TO FECHA-VENCIMIENTO.
       4320-ARMAR-TEXTO-ALERTA.
REMARK****************************************************************
REMARK****************************************************************
           MOVE NUM-DEMA-VIG-INP            TO NUM-DEMA-VIG-AUX
                                               NUM-VIG-1
           MOVE NUM-DEMA-TER-INP            TO NUM-DEMA-TER-AUX
                                               NUM-TER-2
           MOVE FEC-DEMA-VIG-INP            TO FEC-DEMA-VIG-AUX
                                               FEC-VIG-1.
       4330-CREAR-BDIIALE.
REMARK****************************************************************
REMARK****************************************************************
           MOVE FECHA-PROCESO              TO FEC-NOV-BDIIALE
                                              FEC-REP-BDIIALE.
           MOVE FECHA-VENCIMIENTO          TO FEC-VEN-BDIIALE.
           MOVE 0                          TO BLOQUEO-BDIIALE.
           MOVE INF-DATAHOST-AUX           TO INF-DATAHOST-BDIIALE.
           MOVE SPACES                     TO FILLER-BDIIALE.
           MOVE LINEA1-AUX                 TO LINEA1-BDIIALE.
           MOVE LINEA2-AUX                 TO LINEA2-BDIIALE.
           IF REAL = 1
              WRITE REG-BDIIALE
                 INVALID KEY
                   ADD 1 TO CONT-WRITE-ERR
                 NOT INVALID KEY
                   ADD 1 TO CONT-WRITE-OK   
              END-WRITE
           ELSE
              ADD 1 TO CONT-WRITE-OK   
           END-IF.
       4340-ACTUALIZAR-BDIIALE.
REMARK****************************************************************
REMARK****************************************************************
           MOVE FECHA-PROCESO              TO FEC-NOV-BDIIALE.
           MOVE FECHA-VENCIMIENTO          TO FEC-VEN-BDIIALE.
           MOVE INF-DATAHOST-AUX           TO INF-DATAHOST-BDIIALE.
           MOVE LINEA1-AUX                 TO LINEA1-BDIIALE
           MOVE LINEA2-AUX                 TO LINEA2-BDIIALE.
           IF REAL = 1
              REWRITE REG-BDIIALE
                 INVALID KEY
                   ADD 1 TO CONT-REWRITE-ERR
                 NOT INVALID KEY
                   ADD 1 TO CONT-REWRITE-OK   
              END-REWRITE
           ELSE
              ADD 1 TO CONT-REWRITE-OK   
           END-IF.
       4400-GRABAR-REGISTROS-ERR.
REMARK****************************************************************
REMARK****************************************************************
           MOVE M-ERRORES   TO ERRORES-SALIDA.   
           MOVE ";"         TO FILLER-SALIDA. 
           MOVE REG-ENTRADA TO REGISTRO-SALIDA.
           WRITE REG-SALIDA.
       6000-SACAR-RESUMEN.
REMARK****************************************************************
REMARK****************************************************************
          DISPLAY "==============[ RESUMEN DEL PROCESO ]============"
          DISPLAY "TIPO DE PROCESO             = "   COMANDO(1:10)
          DISPLAY "DEMANDADOS LEIDOS           = " CONT-LEIDOS  
          DISPLAY "  DEMANDADOS VALIDOS        = " CONT-REGISTROS-OK 
          DISPLAY "    DEMANDADOS CREADOS      = " CONT-WRITE-OK   
          DISPLAY "    DEMANDADOS ACTUALIZADOS = " CONT-REWRITE-OK  
          DISPLAY "    DEMANDADOS ERR CREAR    = " CONT-WRITE-ERR 
          DISPLAY "    DEMANDADOS ERR ACTUALIZ = " CONT-REWRITE-ERR 
          DISPLAY "  DEMANDADOS NO VALIDOS     = " CONT-REGISTROS-ERR 
          DISPLAY "=================================================".
       COPY BDIIVIDE.PROC.
       COPY VALNOM.PROC. 
