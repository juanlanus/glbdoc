DEBUG *SET db2(db=INGR_DL1 pass=icgssode.IcgssD06,VALIDATE=RUN)
      $SET db2(connect=2)
      $set db2(BIND COLLECTION=COBOLAPP)
      $set db2(FORMAT=LOC VERSION=V8)
      * ICGCLV.v.01.0003
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ICGCLV.
       AUTHOR.        DATACREDITO.
REMARK******************************************************************
REMARK* ESTE PROGRAMA PERMITE EXAMINAR TODAS LAS BASES DE INFORMACION  *
REMARK* DE LAS CLAVES EN LOS DIFERENTES SISTEMAS DE DATACREDITO        *
REMARK* ESTA VERSION REALIZA UNA SOLA CONEXION INICIAL PARA TODAS
REMARK* ADEMAS ACCEDE CON LOS LOGONES ENCONTRADOS EN LA BD
REMARK* INICIALMENTE ES NECESARIO HACER UN CREATE (ADD) CON LOS CODIGOS
REMARK* DEFINIDOS EN lA BASE DE DATOS!!!!
REMARK******************************************************************
REMARK* R.MARTINEZ  * DECI01 - SE INCLUYE LOGICA PARA QUE TAMBIEN SE 
REAMRK* RMB0002     *   EXAMEINE Y PRESENTE LA INFORMACION DE LA BASE
REMARK* ENERO/2013  *   DE DATOS DE DECISOR  
REMARK******************************************************************
REMARK* R.MARTINEZ  * PRO001 - SE ADAPTA EL PROGRAMA PARA MANEJAR    
REAMRK* RMB0003     *   PRODUCTOS EN FORMATO ALFANUMERICO               
REMARK* ABRIL/2013  *   
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
           COPY BDIIDIAN.FS.      
           COPY BDIIDAS.FS.        
           COPY BDIIREGI.FS.     
           COPY ICBSUS.FS.
           COPY ICBCLA0001.FS.  
           COPY ICBPRO0001.FS.     
           COPY SCOREXTR.FS.                                            
           COPY ICBTCO.FS.                                              
           SELECT SECUENCIAL-BUFF
                  ASSIGN TO DISK                   SECUENCIAL-TITLE
                  ORGANIZATION IS RELATIVE
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS COD-RETORNO
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
           COPY ICBTCO001.FD.
           FD ARC-BIND.
           01 REG-BIND PIC X(500).
           COPY BDIIDIAN.FD.     
           COPY BDIIDAS.FD.     
           COPY BDIIREGI.FD.   
           COPY ICBSUS.FD.
           COPY ICBCLA.FD.
           COPY ICBPRO0001.FD.    
           COPY SCOREXTR.FD.         
           FD SECUENCIAL-BUFF.
           01 REG-SEC-BUFF .
              02 BUFFER-X OCCURS 399  TIMES.
                 03 REGISTRO-X      PIC X(131).
                 03 CAR-CONTROL     PIC X.
              02 REGISTRO400        PIC X(131).
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
       01 COD-RETORNO              PIC X(2) VALUE ZEROS.
       01 REG-BIND-AUX1            pic X(100) VALUE SPACES.
       01 REG-BIND-AUX2            pic X(100) VALUE SPACES.
       01 WS-ALIAS-VICTOR          pic X(15).
       01 WS-USR-VICTOR            pic X(15).
       01 WS-PASS-VICTOR           pic X(15).
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
       01 WS-AMBIENTE-FILE         pic X(15).
       01 WS-USR                   pic X(15).
       01 WS-PASS                  pic X(18).
       01 WS-USR-INGR              pic X(08).
       01 WS-PASS-INGR             pic X(18).
       01 WS-ALIAS-INGR            pic X(18).
       01 WS-USR-AUTE              pic X(08).
       01 WS-PASS-AUTE             pic X(18).
       01 WS-ALIAS-AUTE            pic X(18).
       01 WS-USR-MOVI              pic X(08).
       01 WS-PASS-MOVI             pic X(18).
       01 WS-ALIAS-MOVI            pic X(18).
       01 WS-USR-LIST              pic X(08).
       01 WS-PASS-LIST             pic X(18).
       01 WS-ALIAS-LIST            pic X(18).
DECI01 01 WS-USR-DECI              pic X(08).                           RMB0002
DECI01 01 WS-PASS-DECI             pic X(18).                           RMB0002
DECI01 01 WS-ALIAS-DECI            pic X(18).                           RMB0002
       01 I-INT-COL1               PIC 9(04).
       01 SQLCODE-I                PIC 9(08).
       01 INT-COL1                 pic S9(04) comp-5.
       01 I1                       pic S9(04) comp-5.
       01 I1-C                     PIC 9(08).
       01 I2                       pic S9(04) comp-5.
       01 I2-C                     PIC 9(08).
       01 C1                       PIC X(40).
       01 C2                       PIC X(40).
       01 C3                       PIC X(40).
       01 C4                       PIC X(40).
       01 C5                       PIC X(40).
       01 C6                       PIC X(40).
       01 C7                       PIC X(40).
       01 int-col                  pic S9(4) comp-5.
       01 ws-dyn-query.
          49 ws-dyn-query-len      pic s9(4) comp-5.
DECI01    49 ws-dyn-query-txt      pic x(900).                          RMB0002 
       01 COMANDO                  PIC X(40) VALUE SPACES.
       01 PRIVILEGIO               PIC X(20) VALUE SPACES.
       01 PRIV                     PIC 9(1)  VALUE 0.
       COPY BDIIVIDE.WS.
       COPY VALNOM.WS.       
       COPY BDUUNI.WS.       
       COPY UTLCRI.WS.                                                  
       COPY "CCIRWS.CPY".
       01 ERROR-SEND-ICGCON           PIC 9(1) VALUE 0.
       01 CONTADOR                    PIC 9(6) VALUE 000.
       01 ABIERTO                     PIC 9(6) VALUE 000.
       01 PAR-ENTIDAD                 PIC 9(3) VALUE 004.
       01 PAR-MARCA                   PIC 9(2) VALUE 04.
       01 INSSQL-BD                   PIC X(5000).
       01 DESERR-BD.
          03 DESCRIPCION-ERR-BD       PIC X(255) VALUE SPACES.
          03 FILLER                   PIC X(01)  VALUE X"00".
       01 DESERR-W                    PIC X(255) VALUE SPACES.
       01 INS-ERR-BD.
          03 INSERR-BD                PIC X(2000) VALUE SPACES.
          03 FILLER                   PIC X(01)   VALUE X"00".
       01 LONREG-BD.
          03 LONREG-WS                PIC X(02).
          03 FILLER                   PIC X(01)  VALUE X"00".
       01 CODERR-AUX                  PIC X(06).
       01 TITLE-TMP-BIN.
          03 FILLER                   PIC X(11)  VALUE "$TEMPORALES".
          03 ARCHIVO-TITLE-TMP-BIN.
             05 FILLER                PIC X(13)  VALUE "/SCOVGEN-BIN.".
             05 MAR-TIT-TMP-BIN       PIC 9(02).
             05 ENT-TIT-TMP-BIN       PIC X(03).
       01 TIEMPO1                 .
          02 H1                       PIC 99.
          02 M1                       PIC 99.
          02 S1                       PIC 99.
          02 D1                       PIC 99.
       01 TIEMPO2                 .
          02 H2                       PIC 99.
          02 M2                       PIC 99.
          02 S2                       PIC 99.
          02 D2                       PIC 99.
       01 DIFERENCIA                  PIC 9(8)V99.
       01 DIF-EDI                     PIC Z(8).99.
       01 NOM-TMP-BIN-BD              PIC X(30).
       01 NOM-TAB-BIN                 PIC X(13) VALUE "BIN_PRE      ".
       01 CODMAR-AUX                  PIC 9(02).
       01 CODRET-BD.
          03 CODIGO-RETORNO-BD        PIC X(06)  VALUE SPACES.
          03 FILLER                   PIC X(01)  VALUE X"00".
       01 FDA-TMP                     PIC 9(01).
       01 FDA-SCOVTAR                 PIC 9(01).
       01 SW-BDIIREGI                 PIC 9 VALUE 0.
       01 NULO                        PIC X(01)  VALUE X'00'.
       01 VECTOR-BIN.
          03 VEC-BIN                  OCCURS 1000 TIMES
             ASCENDING KEY IS CODBIN-BIN INDEXED BY I-BIN.
             05 CODBIN-BIN            PIC 9(06).
             05 CODMAR-BIN            PIC 9(02).
             05 CODENT-BIN            PIC 9(03).
             05 CODSCO-BIN            PIC 9(02).
             05 CODEXT-BIN            PIC 9(02).

       77  COD-ENTRADA             pic x(20).
           COPY EMAILDG.WS.                                             ERR.001
       01 INFORMACION-PROGRAMA.
          03 FILLER                      PIC X(33).
          03 INF-PROG-NOMBRE-HOST        PIC X(17).
          03 FILLER                      PIC X(50).
       01 VARIABLES-GENERALES.
          03 AREA-MSG                    PIC X(80) VALUE SPACES.
          03 PUB-AUX                     PIC X(30) VALUE SPACES.
          03 AREA-OUTPUT                 PIC X(1900).
          03 FILLER  REDEFINES AREA-OUTPUT.
             05 V-OUTPUT                 PIC X(80) OCCURS 24 TIMES.
          03 IND-RMB                     PIC 9(3) VALUE 0.
          03 ETX                         PIC X VALUE X'03'.
          03 CARRY-R                     PIC X VALUE X'0D'.
          03 IND-CTA                     PIC 9(2) VALUE 1.
          03 ERROR-SEND                  PIC 9 VALUE 0.
          03 SESSION-AUX                 PIC X(4) COMP-5 VALUE 0.
          03 FIN-PROGRAMA                PIC 9 VALUE 0.
          03 FIN-BIND                    PIC 9 VALUE 0.
          03 FIN-ICBTCO                  PIC 9 VALUE 0.
          03 CONT-CLAVESH                PIC 9(6)  VALUE ZEROS.
          03 NUEVA-CLAVEH                PIC 9(1)  VALUE ZEROS.
          03 CONT-ICBTCO                 PIC 9(8)  VALUE ZEROS.
       01 TABLA-CLAVESH.
           03 V-CLAVESH OCCURS 10000 TIMES.
              04 CODSUS-CLAVESH           PIC 9(06).
              04 CLAVET-CLAVESH           PIC X(05).
              04 NIT-CLAVESH              PIC 9(11).
          02 TABLA-MULTTY.                                              
          03 TABLA-EST OCCURS 200 TIMES.
             05 MULT-TTY               PIC X(8) . 
             05 MULT-SESION            PIC X(4) COMP-5 .
REMARK*    ***********************************************************
REMARK*    * AQUI SE DEFINEN VARIABLES ADICIONALES DE CONTROL POR USU*  
REMARK*    * 05 MULT-LOGIN             PIC 9.
REMARK*    ***********************************************************
          03 X-RCODE                   PIC XX.
          03 X-HORA                    PIC 9(8) VALUE 0.
          03 MIN3                      PIC 99 VALUE 0.
          copy ICFILE.WS.     
          copy "CCIWS.CPY".
       01 TRAMA-XDHOST.
          02 FILLER              PIC X(07) VALUE "MXDHOST".
          02 IDE-XDHOST          PIC 9(11).
          02 FILLER              PIC X(1) VALUE ".".
          02 CLAVE-XDHOST        PIC X(05).
          02 FILLER              PIC X(12) VALUE ".5.AAA.TEST.".
       01 VARIABLES-PROPIAS.
          02 FLAG-SISTEMA-N PIC 9(1) VALUE 0.
          02 HAY-ICBCLA     PIC 9(1) VALUE 0.
          02 TIP-IDE-ALFA   PIC X.
          02 NUM-IDE-ALFA   PIC X(11).
          02 TIP-IDE        PIC 9.
          02 NUM-IDE        PIC 9(11).
          02 TIP-TRA       PIC X(20).
          02 FIN-REPORTE    PIC 9 VALUE 0.
          02 MSG-FINAL      .
             03 FILLER PIC X(20) VALUE "!!!!!!!!!!!!!!!!!!!!".
             03 FILLER PIC X(05) VALUE "!!!!!".
             03 TEXTO-FINAL PIC X(30) VALUE SPACES.
             03 FILLER PIC X(05) VALUE "!!!!!".
             03 FILLER PIC X(20) VALUE "!!!!!!!!!!!!!!!!!!!*".
       01 AREA-SUBTITULOS             PIC X(80).
       01 AREA-SUB-INGR.
          02 FILLER     PIC X(20) VALUE "NIT         NOMBRE E".
          02 FILLER     PIC X(20) VALUE "NTIDAD         APLIC".
          02 FILLER     PIC X(20) VALUE "ACIONES   ESTADO    ".
          02 FILLER     PIC X(20) VALUE "MIGRACION           ".
       01 AREA-SUB-MOVI.
          02 FILLER     PIC X(20) VALUE "NIT         NOMBRE E".
          02 FILLER     PIC X(20) VALUE "NTIDAD          SUSC".
          02 FILLER     PIC X(20) VALUE "RI      ESTADO    HO".
          02 FILLER     PIC X(20) VALUE "RA-INI    HORA-FIN  ".
       01 AREA-SUB-LIST.
          02 FILLER     PIC X(20) VALUE "CEDULA       OPCIONE".
          02 FILLER     PIC X(20) VALUE "S   (ESTO APLICA IND".
          02 FILLER     PIC X(20) VALUE "EPENDIENTE DEL NIT D".
          02 FILLER     PIC X(20) VALUE "E LA ENTIDAD)       ".
       01 AREA-SUB-AUTE.
          02 FILLER     PIC X(20) VALUE "NIT         NOMBRE E".
          02 FILLER     PIC X(20) VALUE "NTIDAD     PRODUCTO ".
          02 FILLER     PIC X(20) VALUE "SEQ             APLI".
          02 FILLER     PIC X(20) VALUE "CACIONES            ".
DECI01 01 AREA-SUB-DECI.                                                RMB0002
DECI01    02 FILLER     PIC X(20) VALUE "NIT         EMPRESA ".         RMB0002
DECI01    02 FILLER     PIC X(20) VALUE "             ROL    ".         RMB0002
DECI01    02 FILLER     PIC X(20) VALUE "              ESTRAT".         RMB0002
DECI01    02 FILLER     PIC X(20) VALUE "EGIA          ESTADO".         RMB0002
       01 TITULO-ENCABEZADO.
          02 FILLER PIC X(20) VALUE "================= [ ".
          02 FILLER PIC X(20) VALUE "CONTROL DE CALIDAD C".
          02 FILLER PIC X(09) VALUE "LAVES DE ".
          02 AMB-ENCABEZADO   PIC X(11) VALUE "DESARROLLO ".
          02 FILLER PIC X(20) VALUE " ] =================".
       01 RAYAS   PIC X(80) VALUE ALL "=".
       01 FECHA1            PIC 9(8).
       01 FILLER REDEFINES FECHA1.
          02 AAAAF1         PIC 9(4).
          02 MMF1           PIC 9(2).
          02 DDF1           PIC 9(2).
       01 FECHA2            PIC 9(8).
       01 FILLER REDEFINES FECHA2.
          02 AAAAF2         PIC 9(4).
          02 MMF2           PIC 9(2).
          02 DDF2           PIC 9(2).
       01 LINEA01-ENCABEZADO.
          02 FILLER              PIC X(05) VALUE "TIP: ".
          02 TIPIDE-LINEA01-ENC  PIC 9(01).
          02 FILLER              PIC X(06) VALUE " NUM: ".
          02 NUMIDE-LINEA01-ENC  PIC 9(11).
          02 FILLER              PIC X(09) VALUE " NOMBRE: ".
          02 NOMBRE-LINEA01-ENC  PIC X(34).
          02 FILLER              PIC X(06) VALUE " FEC: ".
          02 FECHA-LINEA01-ENC   PIC X(08).
REMARK***************************CLAVES ICBCLA***********************
       01  M-SISTEMA.
           02 FILLER PIC X(12) VALUE "-CONSULTAS  ".
           02 FILLER PIC X(12) VALUE "-NOVEDAT    ".
           02 FILLER PIC X(12) VALUE "-RECLAMOS   ".
           02 FILLER PIC X(12) VALUE "-MANTENIMIEN".
           02 FILLER PIC X(12) VALUE "-AUDITORIA  ".
           02 FILLER PIC X(12) VALUE "-ENTREGA INF".
           02 FILLER PIC X(12) VALUE "-OPERACIONES".
           02 FILLER PIC X(12) VALUE "-NO DEFINIDO".
           02 FILLER PIC X(12) VALUE "-NO DEFINIDO".
           02 FILLER PIC X(12) VALUE "-NO DEFINIDO".
       01  FILLER REDEFINES M-SISTEMA.
           02 V-SISTEMA OCCURS 10 TIMES PIC X(12).
PRO001*01  VECTOR-T-PRODUCTOS OCCURS 100 TIMES.                         RMB0003
PRO001 01  VECTOR-T-PRODUCTOS OCCURS 2000 TIMES.                        RMB0003
            02 PRODU-ICBPRO-AUX     PIC 9(2)  VALUE ZEROES.             
PRO001      02 PRODU-ICBPRO-AUX-R REDEFINES PRODU-ICBPRO-AUX PIC X(2).  RMB0003
            02 CORTA-ICBPRO-AUX     PIC X(3)  VALUE SPACES.             
            02 LARGA-ICBPRO-AUX     PIC X(20) VALUE SPACES.             
            02 HUELL-ICBPRO-AUX     PIC 9     VALUE 0.                  
            02 EXTER-ICBPRO-AUX     PIC X(01) VALUE SPACES.             
            02 NOM-CLA-ICBPRO-AUX   PIC X(03).                          
       01 VARIABLES-ICBCLA.
          02 NIT                     PIC 9(11).
          02 I                       PIC 9(5).
          02 I-XX                    PIC 9(2) VALUE 0.
          02 J                       PIC 9(5).
          02 JJ                      PIC 9(5).
          02 K                       PIC 9(5).
          02 N                       PIC 9(5).
          02 MENS-ERR                PIC 9(5).
       01  V-DESC.                                                      
            02 DESC  OCCURS 50   PIC X(40) VALUE SPACES.                
       01  AUXILIARES-SERVIS.                                           
           03 CADENA                        PIC X(80).                  
           03 SUB-PRI                       PIC 9(01).                  
           03 NUM-PRO                       PIC 9(2).                   
           03 BDU                           PIC X(3).                   
           03 SCO                           PIC 9(4).                   
           03 V-PRODUCTOS                   PIC 9(7).                   
       01 SDTHOST-01.                                                   
           05 SDTH01-LEYENDA              PIC X(05) .                   
           05 SDTH01-DESCRIPCION          PIC X(75).                    
       01 SDTHOST-99.                                                   
           05 SDTH99-TIPREG               PIC 9(02) VALUE 99.           
           05 SDTH99-LONGITUD             PIC 9(04) VALUE 0080.         
           05 SDTH99-TIPCON               PIC 9(02) VALUE 0.            
           05 SDTH99-MSGORI               PIC X(70) VALUE SPACES.       
           05 SDTH99-FINCON               PIC X(02) VALUE "!*".         
       01 DATOS-CLAVE0.
           05 FILLER                      PIC X(08) VALUE "    NIT:".
           05 NIT-CLAVE                   PIC 9(11) .
           05 FILLER                      PIC X(05) VALUE " NOM:".
           05 NOMSUS-CLAVE                PIC X(35) .
           05 FILLER                      PIC X(05) VALUE " SUS:".
           05 SUS-CLAVE                   PIC 9(06) .
           05 FILLER                      PIC X(05) VALUE " CLA:".
           05 AUT-CLAVE                   PIC X(05) .
       01 DATOS-CLAVE1.
           05 FILLER                      PIC X(8) VALUE "    COS:".
           05 COSTO-CLAVE                 PIC 9(6) .
           05 FILLER                      PIC X(5) VALUE " BLO:".
           05 BLOQUEO-CLAVE               PIC 9(1) .
           05 FILLER                      PIC X(5) VALUE " PRV:".
           05 PRIVILEGIO-CLAVE            PIC 9(1) .
           05 FILLER                      PIC X(5) VALUE " ASG:".
           05 ASIGNA-CLAVE                PIC X(1) . *> C,L,N
           05 FILLER                      PIC X(5) VALUE " PRG:".
           05 PROGRAMA-CLAVE              PIC X(6) . 
           05 CSVEXT-CLAVE                PIC X(3).
           05 FILLER                      PIC X(5) VALUE " FRM:".
           05 FORMULARIO-CLAVE            PIC 9(4) . 
           05 FILLER                      PIC X(5) VALUE " CRE:".
           05 FCREACION-CLAVE             PIC 9(8) . 
           05 FILLER                      PIC X(5) VALUE " FLG:".
           05 FLAGCOOKI-CLAVE             PIC 9(1) . 
           05 FILLER                      PIC X(5) VALUE " WAP:".
           05 WAP-CLAVE                   PIC X(1) . *> S,N
       01 DATOS-CLAVE2.
           05 FILLER                      PIC X(8) VALUE "    VEN:".
           05 FVENC-CLAVE2                PIC 9(8) .
           05 FILLER                      PIC X(5) VALUE " ULT:".
           05 FULTI-CLAVE2                PIC 9(8) .
           05 FILLER                      PIC X(9) VALUE " FORMATO:".
           05 FORMATO-CLAVE2              PIC X(12) .
           05 FILLER                      PIC X(9) VALUE " SISTEMA:".
           05 SISTEMA-CLAVE2              PIC X(12) .
           05 FILLER                      PIC X(7) VALUE " DTHOS:".
           05 DTHOST-CLAVE2               PIC X(02) .
       01 TEXTO-COMANDO.
          02 T-COMANDO                    PIC X(100).
          02 FILLER                       PIC X VALUE X'00'.
       01 VARIABLES-SCOREXTR-AUX.                                       
          02 HAY-SCOREXTR               PIC 9 VALUE 1.                  
          02 FIN-SCOREXTR                PIC 9 VALUE 0.                 
          02 I-EXT                      PIC 9(5) VALUE 0.               
          02 LLAVE-SCOREXTR-INPUT.                                      
             03 NIT-SCOREXTR-INPUT       PIC 9(11).                     
             03 PROGRAMA-SCOREXTR-INPUT  PIC X(06).                     
          02 CSV-SCOREXTR-OUTPUT         PIC 9(01).                     
          02 TABLA-SCOREXTR-AUX.                                        
             03 V-SCOREXTR-AUX OCCURS 500 TIMES ASCENDING KEY IS        
                               LLAVE-SCOREXTR-AUX INDEXED BY INDEX-EXT. 
                04 LLAVE-SCOREXTR-AUX.                                  
                   05 NIT-SUSCR-SCOREXTR-AUX PIC 9(11).                 
                   05 SCORE-SCOREXTR-AUX     PIC X(06).                 
                04 CSV-SCOREXTR-AUX       PIC 9(01).                    
       01 REG-MASIVO.
          02 TRANCODE-MASIVO              PIC X(1) VALUE "M".
          02 TIPIDE-MASIVO                PIC 9(1) VALUE 0.
          02 FILLER                       PIC X(1) VALUE ".".
          02 NUMIDE-MASIVO                PIC 9(11) VALUE 0.
          02 FILLER                       PIC X(1) VALUE ".".
          02 TRANSACCION-MASIVO           PIC X(6) VALUE "[TODO]".
          02 FILLER                       PIC X(1) VALUE ".".
       01  FEC-JULIANOS.                                     
           02 FEC-JULIANO1               PIC 9(8) VALUE 0.   
           02 FEC-JULIANO2               PIC 9(8) VALUE 0.   
PRO001 01 VARIABLES-CONV  .                                             RMB0003
PRO001    02 TABLA-CONV.                                                RMB0003
PRO001       03 T-CONV OCCURS 2000 TIMES PIC  X(2) VALUE SPACES.        RMB0003
PRO001    02 I-CONV                      PIC  9(4) VALUE 0.             RMB0003
PRO001    02 PROALF-CONV       PIC X(2) VALUE SPACES.                   RMB0003
PRO001    02 PRONUM-CONV       PIC 9(4) VALUE 0.                        RMB0003
       LINKAGE SECTION.
          COPY "CCIRMEN.CPY".
          COPY "CCITAB.CPY".
       01 MENSAJE-CS.
          02 HEADER.
             03 HEADER-USUARIO           PIC X(10).
             03 HEADER-TIPO-MENSAJE      PIC 9.
             03 HEADER-LSN               PIC 9(4).
             03 HEADER-LONGITUD          PIC 9(4).
             03 HEADER-TERMINAL          PIC X(8).
             03 HEADER-NOMB-PROG         PIC X(6).
             03 HEADER-FILA              PIC 99.
             03 HEADER-COLUMNA           PIC 99.
             03 HEADER-CARACT-INICIO     PIC 9.
             03 HEADER-CARACT-FIN        PIC 9.
             03 HEADER-TIPO-TERMINAL     PIC X(6).
             03 HEADER-FILLER            PIC X(5).
          02 TEXTO                       PIC X(2000). 
          02 TEXTO-INPUT REDEFINES TEXTO.
             03 AREA-INPUT               PIC X(80).
             03 FILLER                   PIC X(1920).
       SCREEN SECTION.
       01 AAA .
          02 FILLER       PIC X(1000).
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
           MOVE "ICGCLV" TO WS-PROGRAMA.
           ACCEPT COMANDO FROM COMMAND-LINE.
           UNSTRING COMANDO DELIMITED BY ALL " " 
               INTO COD-ENTRADA PRIVILEGIO.
           IF COD-ENTRADA = "SERVERDEBUG"
              MOVE "SERVER" TO COD-ENTRADA
              MOVE 01 TO I-XX   
              DISPLAY "EJECUTANDO CON COPIA .BIN EN TEMPORALES!!"
           END-IF.
           IF COD-ENTRADA = "MASIVO" 
              DISPLAY "GENERANDO TRANSACCIONES PARA PRUEBA MASIVA"
              PERFORM 9999-GENERAR-MASIVO
              STOP RUN
           END-IF.
           IF PRIVILEGIO = "GENERICO"
              DISPLAY "EJECUCION CON CODIGOS Y PASSWORDS GENERICOS"
              MOVE 1 TO PRIV 
              MOVE "'DESARROL'" TO WS-AMBIENTE-INI
           ELSE
              IF PRIVILEGIO = SPACES
                 MOVE "'DESARROL'" TO WS-AMBIENTE-INI
                 DISPLAY "SE ASUMEN AMBIENTE DESARROL"
              ELSE
                 STRING "'"
                        PRIVILEGIO DELIMITED BY "  "
                        "'"
                 INTO WS-AMBIENTE-INI
                 DISPLAY "EJECUCION EN AMBIENTE=",WS-AMBIENTE-INI
              END-IF
           END-IF.
           IF PRIV = 1
              MOVE "$TEMPORALES/ICBCLA" TO  CLAVE-TITLE
           END-IF.
           MOVE WS-AMBIENTE-INI TO AMB-ENCABEZADO
           IF COD-ENTRADA = "?" 
              DISPLAY "     =========================================="
              DISPLAY "     |     REALIZA CONSULTAS ON-LINE          |"
              DISPLAY "     |     FORMA DE EJECUCION:                |"
              DISPLAY "     |       x ICGESQ SERVER (Servidor ICC)   |"
              DISPLAY "     |       x ICGESQ        (Monousuario)    |"
              DISPLAY "     |       x ICGESQ ?      (AYUDA      )    |"
              DISPLAY "     =========================================="
              STOP RUN
           END-IF.
           MOVE "ICGESQ:  " TO PROGRAMA-EMAIL   
           PERFORM 1000-ABRIR-ARCHIVOS.
           PERFORM 2000-RECIBA-UNIX
             UNTIL FIN-PROGRAMA = 1.
           PERFORM 8400-FINALIZAR.
           MOVE WS-ALIAS-MOVI TO WS-ALIAS.
           PERFORM 1000-CERRAR-XXXX-DL1.
           MOVE WS-ALIAS-AUTE TO WS-ALIAS.
           PERFORM 1000-CERRAR-XXXX-DL1.
           MOVE WS-ALIAS-LIST TO WS-ALIAS.
           PERFORM 1000-CERRAR-XXXX-DL1.
           MOVE WS-ALIAS-INGR TO WS-ALIAS.
           PERFORM 1000-CERRAR-XXXX-DL1.
DECI01     MOVE WS-ALIAS-DECI TO WS-ALIAS.                              RMB0002
DECI01     PERFORM 1000-CERRAR-XXXX-DL1.                                RMB0002
           STOP RUN.
      *
       1000-ABRIR-ARCHIVOS.
REMARK******************************************************************
REMARK*1000-ABRIR-ARCHIVOS.
REMARK* SE LINKEAN LAS AREAS DE TRABAJO DEL CCI AL PROGRAMA DE COBOL,  *
REMARK* SE INICIALIZA LA ESTRUCTURA PARA MANEJAR EL CONTROL DE USUARIOS*
REMARK* CONECTADOS AL SISTEMA. SI EL PARAMETRO DE EJECUCION FUE "SERVER*
REMARK* PROCEDE A INICIAR ESTA COMUNICACION CCI.                       *
REMARK******************************************************************
           CALL "CBL_ALLOC_MEM" USING BY REFERENCE MEM-PTR
                     BY VALUE MEM-SIZE BY VALUE MEM-FLAGS
           SET ADDRESS OF MENSAJE-CS TO MEM-PTR.
           MOVE 1 TO HEADER-LSN 
           PERFORM UNTIL HEADER-LSN > 200 
             MOVE SPACES TO MULT-TTY(HEADER-LSN)
REMARK*     ***********************************************************
REMARK*     *AQUI SE INICILIZAN LAS VARIABLES USUARIAS DE LA MULTITABLA
REMARK*     *MOVE 0 TO MULT-LOGON(HEADER-LSN)
REMARK*     ************************************************************
             ADD 1 TO HEADER-LSN
           END-PERFORM.
           OPEN INPUT SUSCRIPTOR
                      CLAVE
                      BDIIREGI                                          
                      BDIIDIAN                                          
                      BDIIDAS.                                          
           PERFORM 1000-BUSCARDATAHOST.
           IF COD-ENTRADA = "SERVER"
              PERFORM 1100-INICIAR-SERVER.
           PERFORM 1001-INICIAR-BASEDEDATOS.
PRO001*    MOVE "$DATABASE/ICBPROX" TO TITLE-ICBPRO.                    RMB0003
           OPEN INPUT  ICBPRO.                                          
           MOVE 0 TO N.
           MOVE 1 TO K.
           READ ICBPRO IGNORE LOCK AT END MOVE 19 TO K.  
           PERFORM 1001-CARGAR-PRODUCTOS UNTIL K > 18
PRO001     ADD 1 TO I-CONV                                              RMB0003
PRO001     MOVE "##" TO T-CONV(I-CONV)                                  RMB0003
           CLOSE ICBPRO.                                                
           PERFORM 9000-CARGAR-MEM-SCOREXTR.    
       1000-BUSCARDATAHOST.
REMARK****************************************************************
REMARK*1000-BUSCARDATAHOST.
REMARK*    SE BUSCAN EN UN ICBTCO LAS CLAVES QUE HACEN TRANSACCIONES
REMARK*    DATAHOST Y SE ALMACENAN EN MEMORIA         
REMARK****************************************************************
          INITIALIZE TABLA-CLAVESH.
          DISPLAY "LEYENDO ICBFAC PARA CLAVES DATAHOST!!!!!!!"
          MOVE "$DATABASE/ICBFAC" TO SECUENCIAL-TITLE.
          IF PRIV = 1
             MOVE "/dicper/ricardo/ICBFAC201103" TO SECUENCIAL-TITLE
          END-IF.
          OPEN INPUT SECUENCIAL-BUFF.
          READ SECUENCIAL-BUFF IGNORE LOCK AT END
                DISPLAY "ICBFAC VACIO,PROCESO CANCELADO"
                STOP RUN
          END-READ.
          IF COD-RETORNO NOT = "00" 
             MOVE 1 TO FIN-ICBTCO 
          END-IF
          PERFORM UNTIL FIN-ICBTCO = 1 OR CONT-ICBTCO > 4000000
            PERFORM 6100-SACAR-DEL-BUFFER
            INITIALIZE REG-SEC-BUFF
            READ SECUENCIAL-BUFF IGNORE LOCK AT END
                MOVE 1 TO FIN-ICBTCO
            END-READ
            IF COD-RETORNO NOT = "00" AND FIN-ICBTCO = 0
               DISPLAY "COD-RETONO<> 0"
               PERFORM 6100-SACAR-DEL-BUFFER
               MOVE 1 TO FIN-ICBTCO
            END-IF
          END-PERFORM.
          CLOSE SECUENCIAL-BUFF.
          DISPLAY "SE ENCONTRARON CLAVES DATAHOST=",CONT-CLAVESH.
       6100-SACAR-DEL-BUFFER.
REMARK**********************************************************************
REMARK*6100-SACAR-DEL-BUFFER.
REMARK*   COMO SE LEE EL ICBTCO ABLOCADO SE SACA CADA UNO DE LOS REGISTROS
REMARK*   LOGICOS AL AREA ESTANDAR DEL ICBTCO. SE EXAMINA SI ES UNA
REMARK*   CLAVE CON PERFIL DATAHOST
REMARK**********************************************************************
           MOVE 1 TO I
           PERFORM UNTIL I > 399
              IF CAR-CONTROL(I) = x'0A'
                 MOVE REGISTRO-X(I) TO REG-SEC
                 ADD 1 TO CONT-ICBTCO
                 IF FORMATO-VER-SEC = ("D" OR "d")
                    AND LLAVE-SEC(1:1) <> "2"   *> SOLO ICGCON
                    AND PRODU-SEC      = 01   *> SOLO HISTORIA DE CREDITO
                    PERFORM 1100-BUSCAR-CLAVEH
                    IF NUEVA-CLAVEH = 1
                       ADD 1 TO CONT-CLAVESH
                    END-IF
                 END-IF
              ELSE
                 MOVE 500 TO I
              END-IF
              ADD 1 TO I
           END-PERFORM.
           IF REGISTRO400 NOT = SPACES
              ADD 1 TO CONT-ICBTCO
              MOVE REGISTRO400   TO REG-SEC
              IF FORMATO-VER-SEC = ("D" OR "d")
                 AND LLAVE-SEC(1:1) <> "2"   *> SOLO ICGCON
                 AND PRODU-SEC      = 01   *> SOLO HISTORIA DE CREDITO
                 PERFORM 1100-BUSCAR-CLAVEH
                 IF NUEVA-CLAVEH = 1
                    ADD 1 TO CONT-CLAVESH
                 END-IF
              END-IF
           END-IF.
       1100-BUSCAR-CLAVEH.
REMARK*********************************************************************
REMARK*1100-BUSCAR-CLAVEH.
REMARK*  SE VERIFICA SI LA CLAVE YA FUE ALMACENADA O NO . EN CASO DE SER
REMARK*  NUEVA SE COLOCA EN LA TABLA
REMARK*********************************************************************
           MOVE 1 TO NUEVA-CLAVEH
           MOVE 1 TO J.
           PERFORM UNTIL J > 9999 OR CODSUS-CLAVESH(J) = 0
                                  OR NUEVA-CLAVEH = 0
             IF CODSUS-CLAVESH(J) = COD-SUSCRIP-SEC            AND
                CLAVET-CLAVESH(J)(1:2) = COD-CLAVE-SEC(1:2)    AND
                CLAVET-CLAVESH(J)(3:3) = CLAVE-AUT-SEC(1:3) 
                MOVE 0 TO NUEVA-CLAVEH
             END-IF
             ADD 1 TO J
           END-PERFORM.
           IF NUEVA-CLAVEH = 1 AND J < 10000
             MOVE COD-SUSCRIP-SEC       TO CODSUS-CLAVESH(J)
             MOVE COD-CLAVE-SEC(1:2)    TO CLAVET-CLAVESH(J)(1:2)
             MOVE CLAVE-AUT-SEC(1:3)    TO CLAVET-CLAVESH(J)(3:3)
             MOVE COD-SUSCRIP-SEC       TO COD-SUSCRIP-ABD02 
             READ SUSCRIPTOR IGNORE LOCK 
                INVALID KEY                                             
                MOVE 0 TO NIT-SUSCRIP-S-ABD02     
             END-READ                                                   
             MOVE NIT-SUSCRIP-S-ABD02 TO NIT-CLAVESH(J)     
           END-IF.
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
           end-if
           PERFORM 1000-CREDENCIALES-DB2SSO-RUTSQL
           PERFORM 1000-BUSCA-USUPASS
           PERFORM 1000-HACER-BINDS-DINAMICOS
           PERFORM 1000-CONECTARSE-A-LAS-BDS.
       1000-BUSCA-USUPASS.
REMARK******************************************************************
REMARK*1000-BUSCA-USUPASS.
REMARK*    PARA CADA BASE DE DATOS SE BUSCAN LAS CREDENCIALES PARA LA
REMARK*    CONEXION A LA BASE DE DATOS A CONSULTAR
REMARK******************************************************************
           IF PRIV = 1
              MOVE "icgssode"   TO WS-USR   *>default
              MOVE "IcgssD06"   TO WS-PASS  *>default
              MOVE "INGR_DL1"   TO WS-ALIAS *>default
              MOVE "icdlmd  "   TO WS-USR
              MOVE "icdlmd  "   TO WS-PASS
           END-IF.
*          HAGO EL BIND INICIAL A INGRESO
           MOVE WS-USR-VICTOR   TO WS-USR   
           MOVE WS-PASS-VICTOR  TO WS-PASS  
           MOVE WS-ALIAS-VICTOR TO WS-ALIAS 
           PERFORM 1000-BIND-RUTSQL.  
*
*          BUSCO CREDENCIALES PARA INGRESO 
           MOVE WS-AMBIENTE-INI TO WS-AMBIENTE.
           MOVE "'INGRESO'"     TO WS-DB.
           MOVE WS-ALIAS-VICTOR TO WS-ALIAS.
           PERFORM 2000-HALLAR-USU-PASS-RUTSQL. 
           MOVE C1 TO WS-USR-INGR.
           MOVE C2 TO WS-PASS-INGR.
           MOVE " " TO C3(9:1)
           MOVE C3 TO WS-ALIAS-INGR.
           IF PRIV = 1
              MOVE "icdlmd  "   TO WS-USR-INGR
              MOVE "icdlmd  "   TO WS-PASS-INGR
              MOVE "INGR_DL1"   TO WS-ALIAS-INGR
           END-IF
*          BUSCO CREDENCIALES PARA AUTENTIC
           MOVE WS-AMBIENTE-INI TO WS-AMBIENTE.
           MOVE "'AUTENTIC'"    TO WS-DB.
           MOVE WS-ALIAS-VICTOR TO WS-ALIAS.
           PERFORM 2000-HALLAR-USU-PASS-RUTSQL. 
           MOVE C1 TO WS-USR-AUTE.
           MOVE C2 TO WS-PASS-AUTE.
           MOVE " " TO C3(9:1)
           MOVE C3 TO WS-ALIAS-AUTE.
           IF PRIV = 1
              MOVE "icdlmd  "   TO WS-USR-AUTE
              MOVE "icdlmd  "   TO WS-PASS-AUTE
              MOVE "AUTE_DL1"   TO WS-ALIAS-AUTE
           END-IF
*          BUSCO CREDENCIALES PARA MOVILES. 
           MOVE WS-AMBIENTE-INI TO WS-AMBIENTE.
           MOVE "'MOVILES'"     TO WS-DB.
           MOVE WS-ALIAS-VICTOR TO WS-ALIAS.
           PERFORM 2000-HALLAR-USU-PASS-RUTSQL. 
           MOVE C1 TO WS-USR-MOVI.
           MOVE C2 TO WS-PASS-MOVI.
           MOVE " " TO C3(9:1)
           MOVE C3 TO WS-ALIAS-MOVI.
           IF PRIV = 1
              MOVE "icdlmd  "   TO WS-USR-MOVI
              MOVE "icdlmd  "   TO WS-PASS-MOVI
              MOVE "MOVI_PR1"   TO WS-ALIAS-MOVI
           END-IF.
*          BUSCO CREDENCIALES PARA LISTEXCL 
           MOVE WS-AMBIENTE-INI TO WS-AMBIENTE.
           MOVE "'LISTEXCL'"    TO WS-DB.
           MOVE WS-ALIAS-VICTOR TO WS-ALIAS.
           PERFORM 2000-HALLAR-USU-PASS-RUTSQL. 
           MOVE C1 TO WS-USR-LIST.
           MOVE C2 TO WS-PASS-LIST.
           MOVE " " TO C3(9:1)
           MOVE C3 TO WS-ALIAS-LIST.
           IF PRIV = 1
              MOVE "icdlmd  "   TO WS-USR-LIST
              MOVE "icdlmd  "   TO WS-PASS-LIST
              MOVE "LIST_DL1"   TO WS-ALIAS-LIST
           END-IF.
DECI01*    BUSCO CREDENCIALES PARA DECISESP                             RMB0002
DECI01     MOVE WS-AMBIENTE-INI TO WS-AMBIENTE.                         RMB0002
DECI01     MOVE "'DECISESP'"    TO WS-DB.                               RMB0002
DECI01     MOVE WS-ALIAS-VICTOR TO WS-ALIAS.                            RMB0002
DECI01     PERFORM 2000-HALLAR-USU-PASS-RUTSQL.                         RMB0002
DECI01     MOVE C1 TO WS-USR-DECI.                                      RMB0002
DECI01     MOVE C2 TO WS-PASS-DECI.                                     RMB0002
DECI01     MOVE " " TO C3(9:1)                                          RMB0002
DECI01     MOVE C3 TO WS-ALIAS-DECI.                                    RMB0002
DECI01     IF PRIV = 1                                                  RMB0002
DECI01        MOVE "icdlmd  "   TO WS-USR-LIST                          RMB0002
DECI01        MOVE "icdlmd  "   TO WS-PASS-LIST                         RMB0002
DECI01        MOVE "DECI_DL1"   TO WS-ALIAS-LIST                        RMB0002
DECI01     END-IF.                                                      RMB0002
       1000-HACER-BINDS-DINAMICOS.
REMARK*****************************************************************
REMARK*1000-HACER-BINDS-DINAMICOS.
REMARK*    SE ACTUALIZAN LOS PAQUETES DE COMPILACION EN CADA BD
REMARK*****************************************************************
DECI01     DISPLAY ">>>>>>>>>>REALIZO BIND DINAMICO CON MOVILES "       RMB0002
*          REALIZO BIND DINAMICO CON MOVILES 
           MOVE WS-USR-MOVI TO WS-USR.
           MOVE WS-PASS-MOVI TO WS-PASS.
           MOVE WS-ALIAS-MOVI TO WS-ALIAS.
           PERFORM 1000-BIND-RUTSQL.  
DECI01     DISPLAY ">>>>>>>>>>REALIZO BIND DINAMICO CON AUTENTICA"      RMB0002
*          REALIZO BIND DINAMICO CON AUTENTICACION 
           MOVE WS-USR-AUTE TO WS-USR.
           MOVE WS-PASS-AUTE TO WS-PASS.
           MOVE WS-ALIAS-AUTE TO WS-ALIAS.
           PERFORM 1000-BIND-RUTSQL.  
DECI01     DISPLAY ">>>>>>>>>>REALIZO BIND DINAMICO CON LISTAS EXCLN"   RMB0002
*          REALIZO BIND DINAMICO CON LISTXCL       
           MOVE WS-USR-LIST TO WS-USR.
           MOVE WS-PASS-LIST TO WS-PASS.
           MOVE WS-ALIAS-LIST TO WS-ALIAS.
           PERFORM 1000-BIND-RUTSQL.  
DECI01     DISPLAY ">>>>>>>>>>REALIZO BIND DINAMICO CON INGRESO "       RMB0002
*          REALIZO BIND DINAMICO CON INGRESO.      
           MOVE WS-USR-INGR TO WS-USR.
           MOVE WS-PASS-INGR TO WS-PASS.
           MOVE WS-ALIAS-INGR TO WS-ALIAS.
           PERFORM 1000-BIND-RUTSQL.  
DECI01     DISPLAY ">>>>>>>>>>REALIZO BIND DINAMICO CON DECISOR "       RMB0002
DECI01*    REALIZO BIND DINAMICO CON DECISOR.                           RMB0002
DECI01     MOVE WS-USR-DECI TO WS-USR.                                  RMB0002
DECI01     MOVE WS-PASS-DECI TO WS-PASS.                                RMB0002
DECI01     MOVE WS-ALIAS-DECI TO WS-ALIAS.                              RMB0002
DECI01     PERFORM 1000-BIND-RUTSQL.                                    RMB0002
DECI01     DISPLAY "ACABE TODOS LOS BINDS    !!!!!!!! ".                RMB0002
       1000-CONECTARSE-A-LAS-BDS.
REMARK******************************************************************
REMARK*1000-CONECTARSE-A-LAS-BDS.
REMARK*    CON LAS CREDENCIALES LEIDAS DE LA TABLA CONEXION SE REALIZAN
REMARK*    TODAS LAS CONEXIONES A LAS BD QUE USA EL PROGRAMA 
REMARK******************************************************************
           MOVE WS-USR-MOVI TO WS-USR.
           MOVE WS-PASS-MOVI TO WS-PASS.
           MOVE WS-ALIAS-MOVI TO WS-ALIAS.
           PERFORM 1000-CONECTAR-XXXX-DL1.
           MOVE WS-USR-AUTE TO WS-USR.
           MOVE WS-PASS-AUTE TO WS-PASS.
           MOVE WS-ALIAS-AUTE TO WS-ALIAS.
           PERFORM 1000-CONECTAR-XXXX-DL1.
           MOVE WS-USR-LIST TO WS-USR.
           MOVE WS-PASS-LIST TO WS-PASS.
           MOVE WS-ALIAS-LIST TO WS-ALIAS.
           PERFORM 1000-CONECTAR-XXXX-DL1.
           MOVE WS-USR-INGR TO WS-USR.
           MOVE WS-PASS-INGR TO WS-PASS.
           MOVE WS-ALIAS-INGR TO WS-ALIAS.
           PERFORM 1000-CONECTAR-XXXX-DL1.
DECI01     MOVE WS-USR-DECI TO WS-USR.                                  RMB0002
DECI01     MOVE WS-PASS-DECI TO WS-PASS.                                RMB0002
DECI01     MOVE WS-ALIAS-DECI TO WS-ALIAS.                              RMB0002
DECI01     PERFORM 1000-CONECTAR-XXXX-DL1.                              RMB0002
       1001-CARGAR-PRODUCTOS.
REMARK******************************************************************
REMARK*1001-CARGAR-PRODUCTOS.
REMARK*SE CARGA EN MEMORIA LA TABLA DE PRODUCTOS OFRECIDOS
REMARK******************************************************************
           ADD 1 TO N
PRO001     IF SEC-PRODUCTO-ICBPRO-R NUMERIC                             RMB0003
PRO001        MOVE SEC-PRODUCTO-ICBPRO TO I-CONV                        RMB0003
PRO001     ELSE                                                         RMB0003
PRO001        MOVE N                   TO I-CONV                        RMB0003
PRO001        ADD  100                 TO I-CONV                        RMB0003
PRO001     END-IF.                                                      RMB0003
PRO001     MOVE SEC-PRODUCTO-ICBPRO-R TO T-CONV(I-CONV)                 RMB0003
PRO001     MOVE SEC-PRODUCTO-ICBPRO   TO PRODU-ICBPRO-AUX(I-CONV)       RMB0003
PRO001     MOVE DES-CORTA-ICBPRO      TO CORTA-ICBPRO-AUX(I-CONV)       RMB0003
PRO001     MOVE DES-LARGA-ICBPRO      TO LARGA-ICBPRO-AUX(I-CONV)       RMB0003
PRO001     MOVE HUELLA-ICBPRO         TO HUELL-ICBPRO-AUX(I-CONV)       RMB0003
PRO001     MOVE PRO-EXTERNO-ICBPRO    TO EXTER-ICBPRO-AUX(I-CONV)       RMB0003
PRO001     MOVE NOM-CLA-ICBPRO        TO NOM-CLA-ICBPRO-AUX(I-CONV)     RMB0003
PRO001     IF SEC-PRODUCTO-ICBPRO-R NUMERIC AND                         RMB0003
PRO001        SEC-PRODUCTO-ICBPRO > 70 AND <= 80                        RMB0003
PRO001        COMPUTE I = SEC-PRODUCTO-ICBPRO - 70                      RMB0003
PRO001        MOVE DES-CORTA-ICBPRO  TO PUB-VEC-COM(I)                  RMB0003
PRO001        MOVE DES-LARGA-ICBPRO  TO PUB-VEC-EXP(I)                  RMB0003
PRO001        MOVE HUELLA-ICBPRO     TO PUB-VEC-HUE(I)                  RMB0003
PRO001        MOVE HIPERVIN-ICBPRO   TO PUB-VEC-HIP(I)                  RMB0003
PRO001        MOVE PRO-EXTERNO-ICBPRO  TO PUB-VEC-EXT(I)                RMB0003
PRO001     END-IF                                                       RMB0003
PRO001*    IF N < 31                                                    RMB0003
PRO001*       MOVE SEC-PRODUCTO-ICBPRO TO PRODU-ICBPRO-AUX(N)           RMB0003
PRO001*       MOVE DES-CORTA-ICBPRO    TO CORTA-ICBPRO-AUX(N)           RMB0003
PRO001*       MOVE DES-LARGA-ICBPRO    TO LARGA-ICBPRO-AUX(N)           RMB0003
PRO001*       MOVE HUELLA-ICBPRO       TO HUELL-ICBPRO-AUX(N)           RMB0003
PRO001*       MOVE PRO-EXTERNO-ICBPRO  TO EXTER-ICBPRO-AUX(N)           RMB0003
PRO001*       MOVE NOM-CLA-ICBPRO      TO NOM-CLA-ICBPRO-AUX(N)         RMB0003
PRO001*    ELSE                                                         RMB0003
PRO001*       MOVE SEC-PRODUCTO-ICBPRO TO J                             RMB0003
PRO001*       MOVE SEC-PRODUCTO-ICBPRO TO PRODU-ICBPRO-AUX(J)           RMB0003
PRO001*       MOVE DES-CORTA-ICBPRO    TO CORTA-ICBPRO-AUX(J)           RMB0003
PRO001*       MOVE DES-LARGA-ICBPRO    TO LARGA-ICBPRO-AUX(J)           RMB0003
PRO001*       MOVE HUELLA-ICBPRO       TO HUELL-ICBPRO-AUX(J)           RMB0003
PRO001*       MOVE PRO-EXTERNO-ICBPRO  TO EXTER-ICBPRO-AUX(J)           RMB0003
PRO001*       MOVE NOM-CLA-ICBPRO      TO NOM-CLA-ICBPRO-AUX(J)         RMB0003
PRO001*       IF SEC-PRODUCTO-ICBPRO > 70 AND <= 80                     RMB0003
PRO001*          COMPUTE I = SEC-PRODUCTO-ICBPRO - 70                   RMB0003
PRO001*          MOVE DES-CORTA-ICBPRO  TO PUB-VEC-COM(I)               RMB0003
PRO001*          MOVE DES-LARGA-ICBPRO  TO PUB-VEC-EXP(I)               RMB0003
PRO001*          MOVE HUELLA-ICBPRO     TO PUB-VEC-HUE(I)               RMB0003
PRO001*          MOVE HIPERVIN-ICBPRO   TO PUB-VEC-HIP(I)               RMB0003
PRO001*          MOVE PRO-EXTERNO-ICBPRO  TO PUB-VEC-EXT(I)             RMB0003
PRO001*       END-IF                                                    RMB0003
PRO001*    END-IF.                                                      RMB0003
           READ ICBPRO IGNORE LOCK AT END MOVE 19 TO K. 
       1100-INICIAR-SERVER.
REMARK******************************************************************
REMARK*1100-INICIAR-SERVER.
REMARK* LEE LA VARIABLE DE AMBIENTE SERV-CCI PARA MANEJAR EL AMBIENTE  *
REMARK* DE LA EJECUCION DINAMICAMENTE. ARMA EL NOMBRE PARA FIRMARSE    *
REMARK* AL MODULO CCI. LLAMA AL MODULO Y SE INICIALIZA ANTE EL. SI YA  *
REMARK* HABIA UNO PRESENTE LO CIERRA Y EL TOMA EL CONTROL.             *
REMARK******************************************************************
           MOVE "SERV_CCI" TO VAR-AMBIENTE.
           DISPLAY VAR-AMBIENTE UPON ENVIRONMENT-NAME.
           ACCEPT VAR-AMBIENTE FROM ENVIRONMENT-VALUE.
           IF VAR-AMBIENTE = "SERV_CCI"
              DISPLAY "No se ha fijado: $SERV_CCI !!!"
              STOP RUN
           ELSE
              MOVE VAR-AMBIENTE TO PUB-AUX      
              STRING WS-PROGRAMA ,  PUB-AUX DELIMITED BY SIZE
                  INTO PUBLICNAME
           END-IF
           CALL CCI-MODULE USING SIGNATURE-BLOCK.
           IF CCISIGN NOT = "MFCCI-"
              STRING "ERROR AL INICIALIZAR: ", CCI-MODULE DELIMITED  
                     BY SIZE INTO MENSAJE-EMAIL                     
              CALL "SYSTEM" USING COMANDO-EMAIL                    
              STOP RUN
           ELSE
              SET ADDRESS OF PROCTAB TO CCICALTAB
              MOVE 0 TO CCIEND
              CALL CCI-INITSERVER USING PUBLICNAME, SRVRHANDLE, CCIEND
              IF RETURN-CODE = 4 OR 5
                 CALL CCI-CLOSESERVER USING SRVRHANDLE
                 CALL CCI-INITCLIENT USING PUBLICNAME, MACHINENAME,  
                         SESSID,ASYNC, CCIEND                       
                 CALL CCI-INITSERVER USING PUBLICNAME, SRVRHANDLE, 
                      CCIEND
                 IF RETURN-CODE NOT = 0
                    MOVE RETURN-CODE TO X-RCODE
                    STRING "ERROR AL LLAMAR: CCI-INITSERVER ", X-RCODE  
                           DELIMITED BY SIZE INTO MENSAJE-EMAIL         
                    CALL "SYSTEM" USING COMANDO-EMAIL                   
                    STOP RUN
                 ELSE
                    DISPLAY "REINICIO EXITOSO!!!!!!!!"
                 END-IF
              ELSE
                 IF RETURN-CODE NOT = 0
                    MOVE RETURN-CODE TO X-RCODE
                    STRING "ERROR AL LLAMAR: CCI-INITSERVER ", X-RCODE  
                           DELIMITED BY SIZE INTO MENSAJE-EMAIL         
                    CALL "SYSTEM" USING COMANDO-EMAIL                   
                    STOP RUN
              END-IF.
       2000-RECIBA-UNIX.
REMARK******************************************************************
REMARK*2000-RECIBA-UNIX.
REMARK* RECIBE LA TRANSACCION YA SEA POR PANTALLA O MODULO CCI. OBVIA  *
REMARK* LOS MENSAJES TIPO TIMEOUT, SI ES UN MENSAJE NORMAL LO VA A     *
REMARK* PROCESAR PARA ENVIAR LA RESPUESTA Y VUELVE A QUEDAR EN ESTADO  *
REMARK* DE RECEPCION DEL SIGUIENTE MENSAJE. SE EXAMINA CADA 20 MINUTOS *
REMARK* SI NO HA HABIDO ACTIVIDAD EN CUYO CASO CIERRA LA COMUNICACION  *
REMARK* CON CCI Y LA VUELVE A ABRIR PARA LIBERAR RECURSOS.             *
REMARK* SI EL MENSAJE ES DE DESCONECCION DE UN USUARIO PROCEDE A CERRAR*
REMARK* LA COMUNICACION CON EL.
REMARK******************************************************************
           MOVE SPACES TO AREA-INPUT                              
           IF COD-ENTRADA = "SERVER"
              MOVE LENGTH OF MENSAJE-CS TO MAXLEN
              MOVE ZEROES TO CCIEND, ASYNC, HEADER-TIPO-MENSAJE         
              CALL CCI-RECEIVEALL USING SRVRHANDLE, SESSID,MENSAJE-CS 
                                        MAXLEN, RECVLEN, ASYNC, CCIEND
              INSPECT AREA-INPUT REPLACING ALL NULO BY " "
              IF RETURN-CODE = 0                                 
                 IF HEADER-TIPO-MENSAJE = 9                      
                    MOVE 8 TO RETURN-CODE                        
                 END-IF                                         
              END-IF                                            
           ELSE                                                         
              MOVE 0 TO RETURN-CODE
              MOVE 1 TO HEADER-LSN
              ACCEPT AREA-INPUT 
           END-IF
           IF RETURN-CODE = 0
              IF HEADER-LSN = 0                                    
                 MOVE 1 TO HEADER-LSN                               
                 PERFORM UNTIL MULT-TTY (HEADER-LSN) = SPACES OR    
                         MULT-TTY (HEADER-LSN) = HEADER-TERMINAL OR  
                         HEADER-LSN > 200                           
                    ADD 1 TO HEADER-LSN                             
                 END-PERFORM                                        
                 IF HEADER-LSN > 200
                    DISPLAY "LIMITE LSN 200 ALCANZADO!!!" AT 2405
                    STOP RUN
                 END-IF
                 IF MULT-TTY(HEADER-LSN) = HEADER-TERMINAL         
                    MOVE MULT-SESION(HEADER-LSN) TO SESSION-AUX      
                    CALL CCI-HANGUP USING MULT-SESION(HEADER-LSN)     
                 END-IF                                               
                 MOVE HEADER-TERMINAL  TO MULT-TTY(HEADER-LSN)
                 MOVE SESSID           TO MULT-SESION(HEADER-LSN)    
REMARK*          ******************************************************
REMARK*          *AQUI SE ASIGNA UN VALOR PARA EL INICIO DE LA SESION.*
REMARK*          *MOVE 1 TO MULT-LOGON(HEADER-LSN)
REMARK*          ******************************************************
              END-IF                                                
              MOVE 0 TO ERROR-SEND
              PERFORM 2010-PROCESE-MENSAJE-INPUT
              IF COD-ENTRADA = "SERVER"
                 CALL CCI-SUSPENDSERVER USING SESSID
                 IF RETURN-CODE NOT = 0
                    DISPLAY "ERROR SUSPEND"
                 END-IF
              END-IF
            ELSE
              if return-code > 1 
                 ADD 0 TO RETURN-CODE
              else  
                 ACCEPT X-HORA FROM TIME
                 ADD 1 TO MIN3 
REMARK*          *****************************************************
REMARK*          * SE EXAMINA SI HAN PASADO 20 MINUTOS DE INACTIVIDAD*
REMARK*          * SI ES ASI SE LIBERAN RECURSOS Y SE FIRMA DE NUEVO *
REMARK*          *****************************************************
                 IF MIN3 > 20 
                    MOVE 0 TO MIN3
                    PERFORM 8400-FINALIZAR
                    PERFORM 1100-INICIAR-SERVER
                    INITIALIZE TABLA-MULTTY                             
                 END-IF
              end-if
              IF RETURN-CODE = 8
                 CALL CCI-HANGUP USING SESSID
                 MOVE 1 TO HEADER-LSN 
                 PERFORM UNTIL HEADER-LSN > 200                   
                   IF MULT-SESION(HEADER-LSN) = SESSID 
                      MOVE SPACES TO MULT-TTY(HEADER-LSN)
                      MOVE 201 TO HEADER-LSN
REMARK*          ******************************************************
REMARK*          *AQUI SE BORRA LOS VALORES POR DESCONEXION DE SESION.*
REMARK*          *MOVE 0 TO MULT-LOGON(HEADER-LSN)
REMARK*          ******************************************************
                   ELSE
                      ADD 1 TO HEADER-LSN
                   END-IF
                 END-PERFORM
              END-IF
           END-IF.
       2010-PROCESE-MENSAJE-INPUT.
REMARK******************************************************************
REMARK*2010-PROCESE-MENSAJE-INPUT.
REMARK* ENVIA UN MENSAJE DE RESPUESTA A LA TRANSACCION QUE LLEGO. ESTA *
REMARK* RUTINA YA DEBE SER ADECUADA PARA REALIZAR LAS FUNCIONES        *
REMARK* ESPECIFICAS DEL PROGRAMA A REALIZAR.                           *
REMARK*                                                                *
REMARK******************************************************************
           IF AREA-INPUT(2:09) = "PROGRAICG"
              MOVE 1 TO IND-CTA
              MOVE MSG-FINAL TO V-OUTPUT(1)
              PERFORM 10900-ENVIA-CRCR
           ELSE
              IF COD-ENTRADA = "SERVER" AND
                 AREA-INPUT(2:6) = "4/#$$)"
                 MOVE 1 TO FIN-PROGRAMA
              ELSE
                 IF COD-ENTRADA <> "SERVER" AND
                    AREA-INPUT(2:8) = "00000000"
                    MOVE 1 TO FIN-PROGRAMA
                 ELSE
                    PERFORM 2011-PROCESO-MSG
                 END-IF
              END-IF
           END-IF.
       2011-PROCESO-MSG.
REMARK******************************************************************
REMARK*2011-PROCESO-MSG.
REMARK*    SE EXAMINA EL TIPO DE TRANSACCION PARA REALIZAR LA FUNCION
REMARK*    ESPECIFICA
REMARK******************************************************************
           MOVE "    TRANSACCION CORRECTA" TO TEXTO-FINAL
           PERFORM 3000-DESEMPACAR-MSG.
           EVALUATE TIP-TRA         
             WHEN "[TODO]"
               PERFORM 4000-REALIZAR-TODO
             WHEN "[VENC]"
               PERFORM 4000-REALIZAR-VENC
             WHEN "[HOST]"
               PERFORM 4000-REALIZAR-HOST
             WHEN ANY
               MOVE "TRANSACCION INVALIDA" TO TEXTO-FINAL
           END-EVALUATE.
           MOVE MSG-FINAL   TO AREA-OUTPUT
           MOVE 1 TO IND-CTA
           PERFORM 10900-ENVIA-CRCR.
       3000-DESEMPACAR-MSG.
REMARK*******************************************************************
REMARK*3000-DESEMPACAR-MSG.
REMARK*    SE DESEMPACA LA CADENA DE LA TRANSACCION EN LAS VARIABLES
REMARK*******************************************************************
           INITIALIZE TIP-IDE NUM-IDE TIP-TRA
           UNSTRING AREA-INPUT (2:100) DELIMITED BY ALL "."
              INTO TIP-IDE NUM-IDE TIP-TRA.          
           UNSTRING AREA-INPUT (2:100) DELIMITED BY ALL "."
              INTO TIP-IDE-ALFA NUM-IDE-ALFA TIP-TRA.          
       4000-REALIZAR-HOST.
REMARK*****************************************************************
REMARK*4000-REALIZAR-HOST.
REMARK*    SE BUSCAN LAS CLAVES PARA UN NIT DADO DEL TIPO DATHOST-WEBSE
REMARK*****************************************************************
           MOVE " CLAVES HOST-WEBSE " TO TITULO-BD-TIT
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           PERFORM 4100-ENCABEZADO-TODO.   
           MOVE RAYAS         TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE 1 TO JJ.
           MOVE 0 TO HAY-ICBCLA.
           PERFORM UNTIL JJ > 9999 OR CODSUS-CLAVESH(JJ) = 0
             IF NIT-CLAVESH(JJ) = NUM-IDE 
                MOVE CODSUS-CLAVESH(JJ) TO COD-SUSCR-ABD09
                MOVE CLAVET-CLAVESH(JJ) TO CLAVE-TOT-ABD09
                READ CLAVE IGNORE LOCK
                     INVALID KEY
                        MOVE 0 TO NUEVA-CLAVEH
                     NOT INVALID KEY
                        MOVE 1 TO NUEVA-CLAVEH
                END-READ
                IF NUEVA-CLAVEH = 1
                   PERFORM 4100-MIRAR-INF-CLAVE
                END-IF
             END-IF
             ADD 1 TO JJ
           END-PERFORM.
           IF HAY-ICBCLA = 0
              MOVE "NO HAY REGISTROS DATAHOST PARA EL NIT" TO 
                                              AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM 9700-FIN-ABLOCAR.
       4000-REALIZAR-VENC.
REMARK*****************************************************************
REMARK*4000-REALIZAR-VENC.
REMARK*    TRANSACCION DE VENCIMIENTOS PROXIMOS 30 DIAS DE CLAVES
REMARK*    NO BLOQUEADAS
REMARK*****************************************************************
           MOVE " POR VENCER ICBCLA " TO TITULO-BD-TIT
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE " CLAVES POR VENCER EN LOS SIGUIENTES 30 DIAS" TO 
                                                 AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE RAYAS         TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE 0 TO HAY-ICBCLA.
           MOVE 0 TO MENS-ERR.                                          
           INITIALIZE LLAVE-CLAVE
           START CLAVE          KEY >= LLAVE-CLAVE                      
              INVALID KEY  MOVE 1 TO MENS-ERR                           
           END-START                                                    
           IF MENS-ERR = 0                                              
              READ CLAVE NEXT RECORD IGNORE LOCK AT END                 
                         MOVE 1 TO MENS-ERR                             
           END-IF.                                                      
           PERFORM UNTIL     MENS-ERR = 1 
              IF IND-BLOQUEO-ABD09 = 0
                 MOVE FEC-VENCE-ABD09 TO FECHA1       
                 MOVE FUNCTION CURRENT-DATE TO FECHA2(1:8)  
                 IF FECHA1 >= FECHA2
                    MOVE FUNCTION                        
                    INTEGER-OF-DATE(FECHA1) TO FEC-JULIANO1
                    MOVE FUNCTION                        
                    INTEGER-OF-DATE(FECHA2) TO FEC-JULIANO2
                    COMPUTE I = FEC-JULIANO1 - FEC-JULIANO2
                    IF I <= 30 
                       MOVE 1 TO HAY-ICBCLA
                       INITIALIZE V-DESC AUXILIARES-SERVIS
                       MOVE "DESCRIPCION:" TO AREA-MSG
                       MOVE DESCRIPCION-ABD09  TO AREA-MSG(13:)
                       MOVE "DIAS:"            TO AREA-MSG(56:)
                       MOVE I(4:2)             TO AREA-MSG(62:)
                       MOVE "IDE:"             TO AREA-MSG(65:)
                       MOVE NUM-ID-ABD09 TO IDE-XDHOST
                       MOVE IDE-XDHOST         TO AREA-MSG(70:)
                       PERFORM 9600-ABLOCAR
                       PERFORM 2400-DETERMINAR-PRODUCTO          
                    END-IF
                 END-IF
              END-IF
              READ CLAVE NEXT RECORD IGNORE LOCK AT END 
                   MOVE 1 TO MENS-ERR                 
              END-READ                                 
           END-PERFORM.                               
           IF HAY-ICBCLA = 0
              MOVE "NO HAY REGISTROS POR VENCER 30 DIAS " TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM 9700-FIN-ABLOCAR.
       4000-REALIZAR-TODO.
REMARK****************************************************************
REMARK*4000-REALIZAR-TODO.
REMARK*  TRANSACCION DE INFORMACION TOTAL DE UNA IDENTIFICACION EN LAS
REMATK*  DIFERENTES ESTRUCTURAS QUE SE TIENEN
REMARK****************************************************************
           MOVE TITULO-ENCABEZADO TO AREA-MSG.
           PERFORM 9600-ABLOCAR.
           PERFORM 4100-ENCABEZADO-TODO.   
           IF SW-ID-BDIIVIDE = 0
              PERFORM 4200-BUSCAR-ICBCLA-TODO
              PERFORM 4600-BUSCAR-INGR-TODO   
              PERFORM 4400-BUSCAR-AUTE-TODO   
              PERFORM 4300-BUSCAR-MOVI-TODO   
              PERFORM 4500-BUSCAR-LIST-TODO   
DECI01        PERFORM 4700-BUSCAR-DECI-TODO                             RMB0002
           END-IF.
           PERFORM 9700-FIN-ABLOCAR.
       4100-ENCABEZADO-TODO.   
REMARK******************************************************************
REMARK*4100-ENCABEZADO-TODO.   
REMARK*    SE ENVIA EL ENCABEZADO DEL INFORME 
REMARK******************************************************************
           MOVE TIP-IDE TO TIPIDE-LINEA01-ENC
           MOVE NUM-IDE TO NUMIDE-LINEA01-ENC
           MOVE NUM-IDE TO NUM-IDE-BDIIVIDE  
           MOVE TIP-IDE TO TIP-IDE-BDIIVIDE           
           MOVE 0       TO SW-VERIFNOM-BDIIVIDE            
           MOVE 1       TO SW-VALTIPID-BDIIVIDE            
           PERFORM 80100-VALIDAR-IDENTIFICACION      
           IF SW-ID-BDIIVIDE = 0                     
              MOVE NOM-LEIDO-BDIIVIDE TO NOMBRE-LINEA01-ENC
           ELSE
              MOVE "IDENTIFICACION NO EXISTE" TO NOMBRE-LINEA01-ENC
           END-IF
           MOVE FUNCTION CURRENT-DATE(1:8) TO FECHA-LINEA01-ENC
           MOVE LINEA01-ENCABEZADO TO AREA-MSG.
           PERFORM 9600-ABLOCAR.
       4200-BUSCAR-ICBCLA-TODO.
REMARK***************************************************************
REMARK*4200-BUSCAR-ICBCLA-TODO.
REMARK*    SE BUSCA TODA LA INFORMACION DE LA IDENTIFICACION EN LA
REMARK*    ESTRUCTURA DE COBOL ICBCLA
REMARK***************************************************************
           MOVE "      ICBCLA        " TO TITULO-BD-TIT 
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE 0 TO HAY-ICBCLA.
           MOVE 0 TO MENS-ERR.                                          
           MOVE NUM-IDE     TO NUM-ID-ABD09.                            
           MOVE 00             TO COD-CLAVE-ABD09.                      
           MOVE SPACES         TO CLAVE-AUT-ABD09.                      
           START CLAVE          KEY >= LLAVE-ALTERNA-ABD09              
              INVALID KEY  MOVE 1 TO MENS-ERR                           
           END-START                                                    
           IF MENS-ERR = 0                                              
              READ CLAVE NEXT RECORD IGNORE LOCK AT END                 
                         MOVE 1 TO MENS-ERR                             
           END-IF.                                                      
           PERFORM UNTIL NUM-ID-ABD09 NOT = NUM-IDE     OR MENS-ERR = 1 
              PERFORM 4100-MIRAR-INF-CLAVE
              READ CLAVE NEXT RECORD IGNORE LOCK AT END                 
                   MOVE 999999999 TO NUM-ID-ABD09                       
              END-READ                                                  
           END-PERFORM.                                                 
           IF HAY-ICBCLA = 0
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
       4100-MIRAR-INF-CLAVE.
           MOVE 1 TO HAY-ICBCLA
           MOVE NUM-ID-ABD09 TO IDE-XDHOST
           MOVE CLAVE-TOT-ABD09 TO CLAVE-XDHOST
           INITIALIZE V-DESC AUXILIARES-SERVIS   
           MOVE "DESCRIPCION:" TO AREA-MSG              
           MOVE DESCRIPCION-ABD09  TO AREA-MSG(13:)         
           MOVE "IDE:"             TO AREA-MSG(65:)
           MOVE IDE-XDHOST         TO AREA-MSG(70:)
           PERFORM 9600-ABLOCAR
           PERFORM 2400-DETERMINAR-PRODUCTO          
           IF DESC(1) = SPACES                       
              MOVE CLAVE-TOT-ABD09 TO DESC(1)        
           END-IF                                    
           MOVE 1 TO I                                           
           PERFORM UNTIL DESC(I) = SPACES OR I > 30 
              MOVE "    PRO:" TO AREA-MSG(1:)     
              MOVE DESC(I)    TO AREA-MSG(9:)                  
              PERFORM 9600-ABLOCAR 
              ADD 1 TO I          
           END-PERFORM.
       2400-DETERMINAR-PRODUCTO.                                        
REMARK***********************************************************************
REMARK* 2400-DETERMINAR-PRODUCTO.
REMARK* SE UBICAN LOS DIFERENTES PRODUCTOS QUE PUEDE TENER UNA CLAVE PARA SACAR
REMARK* LA MEJOR DESCRIPCION
REMARK***********************************************************************
           MOVE 1 TO I                                                  
           MOVE COD-SUSCR-ABD09 TO COD-SUSCRIP-ABD02                    
           READ SUSCRIPTOR IGNORE LOCK                                  
                INVALID KEY                                             
                MOVE 0 TO NIT-SUSCRIP-S-ABD02
                MOVE "SUSCRIPTOR NO EXISTE EN ICBSUS!!" TO
                     NOM-SUSCRIP-ABD02
           END-READ.                                                    
           MOVE NIT-SUSCRIP-S-ABD02 TO NIT               
           MOVE NIT                 TO NIT-CLAVE
           MOVE NOM-SUSCRIP-ABD02   TO NOMSUS-CLAVE     
           MOVE COD-SUSCR-ABD09     TO SUS-CLAVE     
           MOVE CLAVE-TOT-ABD09     TO AUT-CLAVE     
           MOVE 724 TO SEMILLA3-UTLCRI
           MOVE SPACES              TO INPUT-UTLCRI
           MOVE CLAVE-TOT-ABD09     TO INPUT-UTLCRI(1:5)    
           PERFORM 0001-ENCRIPX-TRIPLE-UTLCRI               
           MOVE OUTPUT-UTLCRI(1:5)  TO  AUT-CLAVE
           MOVE DATOS-CLAVE0 TO AREA-MSG                  
           PERFORM 9600-ABLOCAR

           MOVE CENTRO-COSTO-ABD09 TO COSTO-CLAVE
           MOVE IND-BLOQUEO-ABD09  TO BLOQUEO-CLAVE        
           MOVE PRIVILEGIO-ABD09   TO PRIVILEGIO-CLAVE    
           IF TAB-SCOR-ABD09(9) NOT = 0      
              MOVE "C"                TO ASIGNA-CLAVE    
           ELSE
              IF SUB-PRIVILEGIO-ABD09 = "P"
                 MOVE "L"                TO ASIGNA-CLAVE    
              ELSE
                 MOVE "N"                TO ASIGNA-CLAVE    
              END-IF
           END-IF
           MOVE SCOR-EXTERNO-ABD09 TO PROGRAMA-CLAVE    
           IF PROGRAMA-CLAVE <> SPACES
              MOVE NIT-CLAVE      TO NIT-SCOREXTR-INPUT  
              MOVE PROGRAMA-CLAVE TO PROGRAMA-SCOREXTR-INPUT 
              PERFORM 9000-BUSCAR-MEM-SCOREXTR
              IF HAY-SCOREXTR = 1    
                 IF CSV-SCOREXTR-OUTPUT = 1
                    MOVE "-SI" TO CSVEXT-CLAVE
                 ELSE
                    MOVE "-NO" TO CSVEXT-CLAVE
                 END-IF
              ELSE
                    MOVE "-NO" TO CSVEXT-CLAVE
              END-IF
           ELSE
              MOVE "O  "      TO CSVEXT-CLAVE
              MOVE "NINGUN"   TO PROGRAMA-CLAVE
           END-IF       
           MOVE FORMATO-ABD09      TO FORMULARIO-CLAVE    
           MOVE FEC-CREACION-ABD09 TO FCREACION-CLAVE     
           MOVE FLAG-COOKIE-ABD09  TO FLAGCOOKI-CLAVE     
           IF CODIGO-TERMINAL-ABD09 = 7777
              MOVE "S" TO WAP-CLAVE
           ELSE
              MOVE "N" TO WAP-CLAVE
           END-IF
           MOVE DATOS-CLAVE1 TO AREA-MSG                  
           PERFORM 9600-ABLOCAR
           MOVE FEC-VENCE-ABD09 TO FVENC-CLAVE2
           MOVE FEC-ULT-UTIL-ABD09 TO FULTI-CLAVE2 
           MOVE SENAL-DATAHOST-ABD02 TO FORMATO-CLAVE2 
           IF SENAL-DATAHOST-ABD02 = "P"
              MOVE "-ANTERIOR" TO FORMATO-CLAVE2(2:)
           ELSE
              MOVE "-ACTUAL  " TO FORMATO-CLAVE2(2:)
           END-IF
           MOVE FLAG-SISTEMA-ABD09(1:1) TO SISTEMA-CLAVE2(1:1)
           IF FLAG-SISTEMA-ABD09 = 0 OR 
              FLAG-SISTEMA-ABD09  NOT NUMERIC
              MOVE "9" TO FLAG-SISTEMA-ABD09
           END-IF
           MOVE FLAG-SISTEMA-ABD09 TO FLAG-SISTEMA-N
           MOVE V-SISTEMA(FLAG-SISTEMA-N) TO SISTEMA-CLAVE2(2:)
           PERFORM 1100-BUSQUE-DATAHOST
           IF NUEVA-CLAVEH = 1
              MOVE "SI" TO DTHOST-CLAVE2
           ELSE
              MOVE "NO" TO DTHOST-CLAVE2
           END-IF
           MOVE DATOS-CLAVE2 TO AREA-MSG                  
           PERFORM 9600-ABLOCAR
           IF SUB-PRIVILEGIO-ABD09 > "1" AND < "9"  *> EXCLUYENTES      
              IF SUB-PRIVILEGIO-ABD09 NOT = "2"     *> SOLO BDUS        
                 MOVE SUB-PRIVILEGIO-ABD09 TO SUB-PRI                   
                 MOVE LARGA-ICBPRO-AUX(SUB-PRI + 9)(2:19) TO DESC(I)    
                 ADD 1 TO I                                             
              END-IF                                                    
           ELSE                                                         
              MOVE FACT-FIJA-ABD02 TO V-PRODUCTOS                       
              IF IND-REGIST-ABD02 NOT NUMERIC
                 MOVE ZEROS TO  IND-REGIST-ABD02 
              END-IF
              MOVE LARGA-ICBPRO-AUX(IND-REGIST-ABD02 + 1)(2:19) TO      
                                                         DESC(I)        
              ADD 1 TO I                                                
              MOVE 1 TO J                                               
              PERFORM UNTIL J > 7                                       
                IF V-PRODUCTOS(J:1) NOT = 0 AND                         
                   J - 1 NOT = IND-REGIST-ABD02                         
                   MOVE LARGA-ICBPRO-AUX(J)(2:19) TO DESC(I)            
                   ADD 1 TO I                                           
                END-IF                                                  
                ADD 1 TO J                                              
              END-PERFORM                                               
           END-IF                                                       
           IF SUB-PRIVILEGIO-ABD09 = "2"                                
              PERFORM 2400-BUSQUE-BDU                                   
           ELSE                                                         
              EVALUATE SUB-PRIVILEGIO-ABD09                             
                WHEN "A"                                                
                 MOVE "NO APELLIDO  " TO DESC(I)                        
                 ADD 1 TO I                                             
                WHEN "P"                                                
                 MOVE "VER ASIGNA"    TO DESC(I)                        
                 ADD 1 TO I                                             
              END-EVALUATE                                              
              PERFORM 2400-BUSQUE-SCO                                   
              PERFORM 2400-BUSQUE-BDU                                   
              PERFORM 2400-BUSQUE-ADI
           END-IF                                                       
           IF PRODUCTO-UNICO-ABD09 NOT = ZEROS AND 
              PRODUCTO-UNICO-ABD09 NOT = SPACES      
                MOVE "PS+ " TO DESC(I)
PRO001*         MOVE PRODUCTO-UNICO-ABD09 TO K                          RMB0003
PRO001          MOVE PRODUCTO-UNICO-ABD09-R TO PROALF-CONV              RMB0003
PRO001          PERFORM 1000-ALFA-A-NUMERO                              RMB0003
PRO001          MOVE PRONUM-CONV TO K                                   RMB0003
                MOVE LARGA-ICBPRO-AUX(K)(2:19) TO DESC(I)(5:)
                ADD 1 TO I                                              
           END-IF.
       2400-BUSQUE-BDU.                                                 
REMARK***********************************************************************
REMARK*2400-BUSQUE-BDU.
REMARK* SE UBICAN LOS DIFERENTES BDUS   QUE PUEDE TENER UNA CLAVE PARA SACAR
REMARK* LA MEJOR DESCRIPCION
REMARK***********************************************************************
           MOVE 1 TO J                                                  
           PERFORM UNTIL J > 5                                          
             IF V-BDUS-ABD09(J) NOT = SPACES                            
                 MOVE 1  TO K                                          
                 PERFORM UNTIL K > 10                                   
                   IF PUB-VEC-COM(K) = V-BDUS-ABD09(J)                  
                      MOVE "BDU "                TO DESC(I)
                      MOVE PUB-VEC-EXP(K)     (2:19) TO DESC(I)(5:)  
                      ADD 1 TO I                                        
                   END-IF                                               
                   ADD 1 TO K                                           
                 END-PERFORM                                            
              END-IF                                                    
              ADD 1 TO J                                                
           END-PERFORM.                                                 
       2400-BUSQUE-SCO.                                                 
REMARK***********************************************************************
REMARK*2400-BUSQUE-SCO.
REMARK* SE UBICAN LOS DIFERENTES SCORES QUE PUEDE TENER UNA CLAVE PARA SACAR
REMARK* LA MEJOR DESCRIPCION
REMARK***********************************************************************
           MOVE 1 TO J                                                  
           PERFORM UNTIL J > 10                                         
             IF TAB-SCOR-ABD09(J) NOT = 0                               
                COMPUTE K = 20 + J                                      
                MOVE "SCO " TO DESC(I)
                MOVE LARGA-ICBPRO-AUX(K)(2:19) TO DESC(I)(5:) 
                ADD 1 TO I                                              
             END-IF                                                     
             ADD 1 TO J                                                 
           END-PERFORM.                                                 
       2400-BUSQUE-ADI.
REMARK*****************************************************************
REMARK*2400-BUSQUE-ADI.
REMARK*    SE EXTRAEN LOS PRODUCTOS ADICIONALES DECLARADOS EN LA CLAVE
REMARK*****************************************************************
           MOVE 1 TO J                                                  
           PERFORM UNTIL J > 10                                         
             IF V-PROD-ADICIONAL-ABD09(J)  NOT = 0 AND 
                V-PROD-ADICIONAL-ABD09(J) NOT = SPACES
PRO001*         MOVE V-PROD-ADICIONAL-ABD09(J) TO K                     RMB0003
REMARK*         CAMPO DE 3 POSICIONES SE APLICA REGLA DE ALINEAMIENTO
PRO001          IF V-PROD-ADICIONAL-ABD09-R(J) NUMERIC                  RMB0003
PRO001             MOVE V-PROD-ADICIONAL-ABD09-R(J)  TO PROALF-CONV     RMB0003
PRO001          ELSE                                                    RMB0003
PRO001             MOVE V-PROD-ADICIONAL-ABD09-R(J)  TO PROALF-CONV     RMB0003
PRO001          END-IF                                                  RMB0003
PRO001          PERFORM 1000-ALFA-A-NUMERO                              RMB0003
PRO001          MOVE PRONUM-CONV TO K                                   RMB0003
                MOVE "ADI " TO DESC(I)
                MOVE LARGA-ICBPRO-AUX(K)(2:19) TO DESC(I)(5:)     
                ADD 1 TO I                                              
             END-IF                                                     
             ADD 1 TO J                                                 
           END-PERFORM.                                                 
       1100-BUSQUE-DATAHOST.
REMARK******************************************************************
REMARK*1100-BUSQUE-DATAHOST.
REMARK*    SE BUSCA EN MEMORIA SI LA CLAVE ES UNA DE DATAHOST O WEBSERVI
REMARK******************************************************************
           MOVE COD-SUSCR-ABD09    TO SUS-CLAVE     
           MOVE CLAVE-TOT-ABD09    TO AUT-CLAVE     
           MOVE 0 TO NUEVA-CLAVEH
           MOVE 1 TO J.
           PERFORM UNTIL J > 9999 OR CODSUS-CLAVESH(J) = 0
                                  OR NUEVA-CLAVEH = 1
             IF CODSUS-CLAVESH(J) = COD-SUSCR-ABD09 AND
                CLAVET-CLAVESH(J) = CLAVE-TOT-ABD09 
                MOVE 1 TO NUEVA-CLAVEH
             END-IF
             ADD 1 TO J
           END-PERFORM.
       4300-BUSCAR-MOVI-TODO.   
REMARK******************************************************************
REMARK*4300-BUSCAR-MOVI-TODO.   
REMARK*    SE ARMA EL QUERY PARA BUSCAR EN LA BD DE MOVILES
REMARK******************************************************************
           MOVE WS-ALIAS-MOVI TO WS-ALIAS.
           MOVE WS-USR-MOVI   TO WS-USR  .
           MOVE WS-PASS-MOVI  TO WS-PASS.   
           MOVE SPACES TO WS-DYN-QUERY-TXT.
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING
             "SELECT rtrim(cast(SUSC.NIT_ID as num(12))),"
                 " SUSC.SUS_NOMBRE,"
                 " rtrim(cast(SUSC.SUS_ID as num(6))),"
                 " rtrim(cast(REL_SUSC_SUP.SUSU_ESTADO as num(4))),"
                 " rtrim(cast(SUP.SUP_HORARIO_INICIAL as char(10))),"
                 " rtrim(cast(SUP.SUP_HORARIO_FINAL as char(10)))"
             " FROM PERFIL.REL_SUSCRIPTOR_SUPERVISOR REL_SUSC_SUP,"
                " PERFIL.ADM_SUSCRIPTORES SUSC,"
                " PERFIL.USU_SUPERVISORES SUP"
             " WHERE REL_SUSC_SUP.USU_ID ="
                     NUM-IDE
                " AND REL_SUSC_SUP.USU_ID = SUP.USU_ID"
                " AND REL_SUSC_SUP.SUS_ID = SUSC.SUS_ID WITH UR"
             INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.
           SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN
           MOVE AREA-SUB-MOVI TO AREA-SUBTITULOS
           MOVE "        WAP         " TO TITULO-BD-TIT 
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE AREA-SUBTITULOS     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE RAYAS               TO AREA-MSG
           PERFORM 9600-ABLOCAR
           MOVE "- CARACTERISTICAS COMO SUPERVISOR"  TO AREA-MSG.
           PERFORM 9600-ABLOCAR
*
           PERFORM 1000-SETCONNECTION-XXXX-DL1.
           PERFORM 1000-BUSCAR-MOVI-DL1.
*
           MOVE WS-ALIAS-MOVI TO WS-ALIAS.
           MOVE SPACES TO WS-DYN-QUERY-TXT.
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING
             "SELECT rtrim(cast(SUSC.NIT_ID as num(12))),"
                 " SUSC.SUS_NOMBRE,"
                 " rtrim(cast(SUSC.SUS_ID as num(6))),"
                 " PROD.PRO_NOMBRE, CARACT.CAR_NOMBRE"
            " FROM PERFIL.REL_SUSCRIPTOR_SUPERVISOR REL_SUSC_SUP,"
            " PERFIL.ADM_SUSCRIPTORES SUSC,"
            " PERFIL.USU_SUPERVISORES SUP,"
            " PERFIL.REL_CARACT_SUPERVISOR CAR_SUP,"
            " APOYO.ADM_CARACTERISTICAS CARACT,"
            " APOYO.ADM_PRODUCTOS PROD"
            " WHERE REL_SUSC_SUP.USU_ID ="
              NUM-IDE
            " AND REL_SUSC_SUP.USU_ID = SUP.USU_ID"
            " AND SUP.USU_ID = CAR_SUP.USU_ID"
            " AND REL_SUSC_SUP.SUS_ID = SUSC.SUS_ID"
            " AND CAR_SUP.CAR_ID = CARACT.CAR_ID"
            " AND CARACT.PRO_ID = PROD.PRO_ID"
            " ORDER BY SUSC.NIT_ID, PROD.PRO_NOMBRE,"
            " CARACT.CAR_NOMBRE WITH UR"
             INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.
           SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN
           MOVE "- CARACTERISTICAS DE LOS PRODUCTO ASOCIADOS" 
                                     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           PERFORM 1000-BUSCAR-MOVI-DL2.
       1000-BUSCAR-MOVI-DL1.
REMARK*******************************************************************
REMARK*1000-BUSCAR-MOVI-DL1.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 1000-PREPARE.      
           INITIALIZE C1,C2,C3,C4,C5,C6 .
           EXEC SQL
               fetch vcurs into :C1,:C2,:C3,:C4,:C5,:C6        
           END-EXEC
           if sqlcode = 100
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM UNTIL SQLCODE NOT =0
              MOVE SPACES TO AREA-MSG
              MOVE INT-COL1 TO I-INT-COL1
              IF C4 = "0" 
                 MOVE "INACTIVO" TO C4
              ELSE
                 IF C4 = "1"
                    MOVE "ACTIVO" TO C4
                 END-IF
              END-IF
                    
              STRING C1(1:09)           
                     " "
                     C2(1:23)
                     " "
                     C3(1:12)
                     C4(1:08)
                     "  "
                     C5(1:12)
                     C6(1:12)
              INTO AREA-MSG  
              PERFORM 9600-ABLOCAR
              EXEC SQL
                  fetch vcurs into :C1,:C2,:C3,:C4,:C5,:C6           
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
       1000-BUSCAR-MOVI-DL2.
REMARK*******************************************************************
REMARK*1000-BUSCAR-MOVI-DL2.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 1000-PREPARE.      
           INITIALIZE C1,C2,C3,C4,C5 .
           EXEC SQL
               fetch vcurs into :C1,:C2,:C3,:C4,:C5
           END-EXEC
           if sqlcode = 100
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM UNTIL SQLCODE NOT =0
              MOVE SPACES TO AREA-MSG
              MOVE INT-COL1 TO I-INT-COL1
              STRING C1(1:09)
                     " "
                     C2(1:23)
                     " "
                     C3(1:08)
                     C4(1:16)
                     " "
                     C5(1:20)
              INTO AREA-MSG
              PERFORM 9600-ABLOCAR
              EXEC SQL
                  fetch vcurs into :C1,:C2,:C3,:C4,:C5
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
       4400-BUSCAR-AUTE-TODO.   
REMARK******************************************************************
REMARK*4400-BUSCAR-AUTE-TODO.   
REMARK*    SE ARMA EL QUERY PARA BUSCAR EN LA BD DE AUTENTICACION
REMARK******************************************************************
           MOVE WS-ALIAS-AUTE TO WS-ALIAS.
           MOVE WS-USR-AUTE   TO WS-USR  .
           MOVE WS-PASS-AUTE  TO WS-PASS.   
           MOVE SPACES TO WS-DYN-QUERY-TXT.
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING 
              "SELECT rtrim(cast(NP.NIT as num(12))),"
                 " G.DESCRIPCION AS SUSCRIPTOR,"
                 " A.COD_ACCION, A.COD_PRODUCTO AS PROD, A.DESCRIPCION"
              " FROM PARAM.ACCIONES AS A, PERFIL.NIT_PRODUCTO AS NP,"
                 " APOYO.NITS AS G"
              " WHERE NP.NIT =G.NIT AND A.COD_PRODUCTO=NP.COD_PRODUCTO"
                 " AND  A.COD_ACCION"
              " IN (SELECT COD_ACCION FROM"
                 " PERFIL.NIT_USUARIO_PRODUCTO_ACCION"
              " WHERE COD_USUARIO ="
                NUM-IDE
                " AND NIT = NP.NIT)"
              " ORDER BY NP.NIT, A.DESCRIPCION WITH UR"
               INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.

           SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN
           MOVE AREA-SUB-AUTE TO AREA-SUBTITULOS
           MOVE "     EVIDENTE       " TO TITULO-BD-TIT 
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE AREA-SUBTITULOS     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE RAYAS               TO AREA-MSG
           PERFORM 9600-ABLOCAR
           MOVE "- APLICACIONES  "  TO AREA-MSG.
           PERFORM 9600-ABLOCAR
*
           PERFORM 1000-SETCONNECTION-XXXX-DL1.
           PERFORM 2100-BUSCAR-AUTE-DL1.
*
           MOVE WS-ALIAS-AUTE TO WS-ALIAS.
           MOVE SPACES TO WS-DYN-QUERY-TXT.
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING
              "SELECT rtrim(cast(NPPP.NIT as num(12))),"
                 " G.DESCRIPCION AS SUSCRIPTOR,"
                 " NPPP.CON_PAR_PRODUCTO,"
                 " NPPP.DESCRIPCION,"
                 " P.COD_PRODUCTO,"
                 " P.DESCRIPCION PROD"
              " FROM PERFIL.NIT_PRO_PAR_PRODUCTO NPPP,"
                 " APOYO.PRODUCTO P, APOYO.NITS AS G"
              " WHERE NPPP.NIT = G.NIT"
                 " AND P.COD_PRODUCTO=NPPP.COD_PRODUCTO"
                 " AND NPPP.COD_BLOQUEO='01'" *>NO BLOQUEADAS
                 " AND NPPP.CON_PAR_PRODUCTO"
              " IN (SELECT CON_PAR_PRODUCTO"
              " FROM PERFIL.NIT_USU_PAR_PRODUCTO"
              " WHERE COD_USUARIO="
                NUM-IDE
                      ")"
             "ORDER BY NPPP.NIT, P.COD_PRODUCTO,"
               " NPPP.CON_PAR_PRODUCTO WITH UR"
               INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.

           SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN
           MOVE AREA-SUB-AUTE TO AREA-SUBTITULOS
           MOVE "- PARAMETRIZACIONES NO BLOQUEADAS" TO AREA-MSG.
           PERFORM 9600-ABLOCAR
*
           PERFORM 1000-SETCONNECTION-XXXX-DL1.
           PERFORM 2100-BUSCAR-AUTE-DL2.
       2100-BUSCAR-AUTE-DL1.
REMARK*******************************************************************
REMARK*2100-BUSCAR-AUTE-DL1.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 1000-PREPARE.      
           INITIALIZE C1,C2,C6,C7,C5 .
           EXEC SQL
               fetch vcurs into :C1,:C2,:C6,:C7,:C5
           END-EXEC
           if sqlcode = 100
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM UNTIL SQLCODE NOT =0
              MOVE SPACES TO AREA-MSG
              STRING C1 (1:09) 
                     " "
                     C2 (1:23)
                     " "
                     C7 (1:5)
                     C6 (1:5)
                     C5 (1:35)
              INTO AREA-MSG
              PERFORM 9600-ABLOCAR
              EXEC SQL
                 fetch vcurs into :C1,:C2,:C6,:C7,:C5
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
           end-if.
           PERFORM 1000-CLOSEVCURS.
       2100-BUSCAR-AUTE-DL2.
REMARK*******************************************************************
REMARK*2100-BUSCAR-AUTE-DL2.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 1000-PREPARE.      
           INITIALIZE C1,C2,I1,C4,C6,C5 .
           EXEC SQL
               fetch vcurs into :C1,:C2,:I1,:C4,:C6,:C5
           END-EXEC
           if sqlcode = 100
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM UNTIL SQLCODE NOT =0
              MOVE SPACES TO AREA-MSG
              MOVE I1 TO I1-C
              STRING C1(1:09)
                     " "
                     C2(1:23)
                     " "
                     C6(1:05)
                     C5(1:20)
                   I1-C(6:03)
                   "  "
                     C4(1:17)
              INTO AREA-MSG
              PERFORM 9600-ABLOCAR
              EXEC SQL
                 fetch vcurs into :C1,:C2,:I1,:C4,:C6,:C5
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
           end-if.
           PERFORM 1000-CLOSEVCURS.
       4500-BUSCAR-LIST-TODO.   
REMARK******************************************************************
REMARK*4500-BUSCAR-LIST-TODO.   
REMARK*    SE ARMA EL QUERY PARA BUSCAR EN LA BD DE LISTEXCL        
REMARK******************************************************************
           MOVE WS-ALIAS-LIST TO WS-ALIAS.
           MOVE WS-USR-LIST   TO WS-USR  .
           MOVE WS-PASS-LIST  TO WS-PASS.   
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING
              "SELECT rtrim(cast(A.IDENTIFICACION as num(12))),"
                   " A.IDPRODUCTO,"
                   " B.DESCRIPCION FROM RECONOCER.PARAMETRIZACION A,"
                   " RECONOCER.PRODUCTOS B"
              " WHERE A.IDPRODUCTO = B.IDPRODUCTO AND"
                   " A.IDENTIFICACION = "
                   NUM-IDE 
              " ORDER BY A.IDPRODUCTO WITH UR"
               INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.

           SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN
           MOVE AREA-SUB-LIST TO AREA-SUBTITULOS
           MOVE "     RECONOCER+     " TO TITULO-BD-TIT 
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE AREA-SUBTITULOS     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE RAYAS               TO AREA-MSG
           PERFORM 9600-ABLOCAR
*
           PERFORM 1000-SETCONNECTION-XXXX-DL1.
           PERFORM 3100-BUSCAR-LIST-DL1.
       3100-BUSCAR-LIST-DL1.
REMARK*******************************************************************
REMARK*3100-BUSCAR-LIST-DL1.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 1000-PREPARE.      
           INITIALIZE C1,C2,C3,I1
           EXEC SQL
               fetch vcurs into :C1,:I1,:C3
           END-EXEC
           if sqlcode = 100
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM UNTIL SQLCODE NOT =0
              MOVE SPACES TO AREA-MSG
              MOVE I1  TO I1-C                
              STRING C1(1:09)    
                     " "
                     I1-C(6:03)
                     " "
                     C3(1:20)
              INTO AREA-MSG  
              PERFORM 9600-ABLOCAR
              EXEC SQL
                  fetch vcurs into :C1,:I1,:C3
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
           end-if.
           PERFORM 1000-CLOSEVCURS.
       4600-BUSCAR-INGR-TODO.   
REMARK******************************************************************
REMARK*4600-BUSCAR-INGR-TODO.   
REMARK*    SE ARMA EL QUERY PARA BUSCAR EN LA BD DE INGRESO         
REMARK******************************************************************
           MOVE WS-ALIAS-INGR TO WS-ALIAS.
           MOVE WS-USR-INGR   TO WS-USR  .
           MOVE WS-PASS-INGR  TO WS-PASS.   
           MOVE 1 TO WS-DYN-QUERY-LEN.
           STRING
           "SELECT distinct(rtrim(cast(UES.NIT as num(12)))),"
           " ENT.NOMBRE,"
           " SER.NOMBRE,"
           " BLO.DESCRIPCION,"
           " BLO2.DESCRIPCION "
           " FROM     PERFIL.USUARIO USU"
           " LEFT JOIN PERFIL.USU_ENT_SERVICIO UES ON"
           " UES.COD_USUARIO = USU.COD_USUARIO"
           " LEFT JOIN PERFIL.ENTIDAD ENT ON ENT.NIT = UES.NIT"
           " LEFT JOIN PARAM.SERVICIO SER ON"
           " SER.COD_SERVICIO = UES.COD_SERVICIO"
           " LEFT JOIN APOYO.TIP_BLOQUEO BLO ON"
           " BLO.COD_BLOQUEO = USU.COD_BLOQUEO"
           " LEFT JOIN APOYO.TIP_BLOQUEO BLO2 ON"
           " BLO2.COD_BLOQUEO = USU.COD_BLO_SECUNDARIO"
           " WHERE   USU.COD_TIP_IDENTIFICA = "
             TIP-IDE
           " AND USU.COD_USUARIO = "
             NUM-IDE
           " ORDER BY ENT.NOMBRE"
           " WITH UR"
               INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN
           END-STRING.
           SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN
           MOVE AREA-SUB-INGR TO AREA-SUBTITULOS
           MOVE "        SUI         " TO TITULO-BD-TIT 
           MOVE TITULO-BD     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE AREA-SUBTITULOS     TO AREA-MSG.
           PERFORM 9600-ABLOCAR
           MOVE RAYAS               TO AREA-MSG
           PERFORM 9600-ABLOCAR
*
           PERFORM 1000-SETCONNECTION-XXXX-DL1.
           PERFORM 4100-BUSCAR-INGR-DL1.
       4100-BUSCAR-INGR-DL1.
REMARK*******************************************************************
REMARK*4100-BUSCAR-INGR-DL1.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
           PERFORM 1000-PREPARE.      
           INITIALIZE C1,C2,C3,C4,C5 .
           EXEC SQL
               fetch vcurs into :C1,:C2,:C3,:C4,:C5    
           END-EXEC
           if sqlcode = 100
              MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG
              PERFORM 9600-ABLOCAR
           END-IF.
           PERFORM UNTIL SQLCODE NOT =0
              MOVE SPACES TO AREA-MSG
              STRING C1 (1:09)
                     " "
                     C2 (1:23)
                     " "
                     C3 (1:15)
                     " "
                     C4 (1:10)
                     C5 (1:20)
              INTO AREA-MSG  
              PERFORM 9600-ABLOCAR
              EXEC SQL
                 fetch vcurs into :C1,:C2,:C3,:C4,:C5    
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
           end-if.
           PERFORM 1000-CLOSEVCURS.
DECI01 4700-BUSCAR-DECI-TODO.                                           RMB0002
REMARK******************************************************************
REMARK*4700-BUSCAR-DECI-TODO.   
REMARK*    SE ARMA EL QUERY PARA BUSCAR EN LA BD DE INGRESO         
REMARK******************************************************************
DECI01     MOVE WS-ALIAS-DECI TO WS-ALIAS.                              RMB0002
DECI01     MOVE WS-USR-DECI   TO WS-USR  .                              RMB0002
DECI01     MOVE WS-PASS-DECI  TO WS-PASS.                               RMB0002
DECI01     MOVE 1 TO WS-DYN-QUERY-LEN.                                  RMB0002
DECI01     STRING                                                       RMB0002
DECI01     "SELECT"                                                     RMB0002 
DECI01     " ten.NIT as NIT,ten.NAME as EMPRESA,"                       RMB0002
DECI01     " tur.DESCRIPTION as ROL,"                                   RMB0002
DECI01     " ' ' as ESTRATEGIA,"                                        RMB0002
DECI01     " ' ' as ACTIVA"                                             RMB0002
DECI01     " FROM"                                                      RMB0002
DECI01     " DECISOR3.TENANTS ten,"                                     RMB0002
DECI01     " DECISOR3.TENANT_USERS tus,"                                RMB0002
DECI01     " DECISOR3.USER_ROLE_MAP urm,"                               RMB0002
DECI01     " DECISOR3.TENANT_USER_ROLES tur"                            RMB0002
DECI01     " WHERE"                                                     RMB0002
DECI01     " tus.USER_CC = "                                            RMB0002
DECI01       NUM-IDE                                                    RMB0002
DECI01     " and tus.PARENT_TENANT = ten.NIT"                           RMB0002
DECI01     " and tus.USER_CC = urm.USER_CC"                             RMB0002
DECI01     " and urm.ROLE_ID = tur.ROLE_ID"                             RMB0002
DECI01     " and tur.ROLE_ID NOT IN(4)"                                 RMB0002
DECI01     " UNION"                                                     RMB0002
DECI01     " SELECT"                                                    RMB0002 
DECI01     " ten.NIT as NIT,ten.NAME as EMPRESA,"                       RMB0002
DECI01     " tur.DESCRIPTION as ROL,"                                   RMB0002
DECI01     " str.STRATEGY_NAME as ESTRATEGIA,"                          RMB0002
DECI01     " str.IS_ACTIVE as ACTIVA"                                   RMB0002
DECI01     " FROM"                                                      RMB0002
DECI01     " DECISOR3.TENANTS ten,"                                     RMB0002
DECI01     " DECISOR3.TENANT_USERS tus,"                                RMB0002
DECI01     " DECISOR3.USER_ROLE_MAP urm,"                               RMB0002
DECI01     " DECISOR3.TENANT_USER_ROLES tur,"                           RMB0002
DECI01     " DECISOR3.USER_ROLE_STRATEGY_MAP ursm,"                     RMB0002
DECI01     " DECISOR3.STRATEGIES str"                                   RMB0002
DECI01     " WHERE"                                                     RMB0002
DECI01     " tus.USER_CC = "                                            RMB0002
DECI01       NUM-IDE                                                    RMB0002
DECI01     " and tus.PARENT_TENANT = ten.NIT"                           RMB0002
DECI01     " and tus.USER_CC = urm.USER_CC"                             RMB0002
DECI01     " and urm.ROLE_ID = tur.ROLE_ID"                             RMB0002
DECI01     " and tur.ROLE_ID = 4"                                       RMB0002
DECI01     " and tus.USER_CC = ursm.USER_CC"                            RMB0002
DECI01     " and ursm.STRATEGY_ID = str.STRATEGY_ID"                    RMB0002
DECI01         INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN       RMB0002
DECI01     END-STRING.                                                  RMB0002
DECI01     SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN      RMB0002
DECI01     MOVE AREA-SUB-DECI TO AREA-SUBTITULOS                        RMB0002
DECI01     MOVE "      DECISOR       " TO TITULO-BD-TIT                 RMB0002
DECI01     MOVE TITULO-BD     TO AREA-MSG.                              RMB0002
DECI01     PERFORM 9600-ABLOCAR                                         RMB0002
DECI01     MOVE AREA-SUBTITULOS     TO AREA-MSG.                        RMB0002
DECI01     PERFORM 9600-ABLOCAR                                         RMB0002
DECI01     MOVE RAYAS               TO AREA-MSG                         RMB0002
DECI01     PERFORM 9600-ABLOCAR                                         RMB0002
DECI01                                                                  RMB0002
DECI01     PERFORM 1000-SETCONNECTION-XXXX-DL1.                         RMB0002
*
DECI01     PERFORM 4710-BUSCAR-DECI-DL1.                                RMB0002
*
DECI01     MOVE WS-ALIAS-DECI TO WS-ALIAS.                              RMB0002
DECI01     MOVE SPACES TO WS-DYN-QUERY-TXT.                             RMB0002
DECI01     MOVE 1 TO WS-DYN-QUERY-LEN.                                  RMB0002
DECI01     STRING                                                       RMB0002
DECI01     "SELECT tn.NAME as ENTIDAD,"                                 RMB0002
DECI01     " tn.NIT,"                                                   RMB0002
DECI01     " usr.USER_NAME as USUSARIO,"                                RMB0002
DECI01     " usr.USER_CC as ID_USUARIO,"                                RMB0002
DECI01     " usr.IS_ACTIVE as ACTIVO"                                   RMB0002
DECI01     " FROM DECISOR3.TENANT_USERS usr"                            RMB0002
DECI01     " JOIN decisor3.TENANTS tn on tn.NIT = usr.PARENT_TENANT"    RMB0002
DECI01     " WHERE usr.USER_CC = "                                      RMB0002
DECI01       NUM-IDE                                                    RMB0002
DECI01         INTO WS-DYN-QUERY-TXT     POINTER WS-DYN-QUERY-LEN       RMB0002
DECI01     END-STRING.                                                  RMB0002
DECI01     SUBTRACT 1                    FROM     WS-DYN-QUERY-LEN      RMB0002
DECI01     MOVE "-ESTADO DEL USUARIO EN LA ENTIDAD         "            RMB0002
DECI01                               TO AREA-MSG.                       RMB0002
DECI01     PERFORM 9600-ABLOCAR                                         RMB0002
DECI01     PERFORM 4720-BUSCAR-DECI-DL2.                                RMB0002
DECI01 4710-BUSCAR-DECI-DL1.                                            RMB0002
REMARK*******************************************************************
REMARK*4710-BUSCAR-DECI-DL1.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
DECI01     PERFORM 1000-PREPARE.                                        RMB0002
DECI01     INITIALIZE C1,C2,C3,C4.                                      RMB0002
DECI01     EXEC SQL                                                     RMB0002
DECI01         fetch vcurs into :C1,:C2,:C3,:C4,:C5                     RMB0002
DECI01     END-EXEC                                                     RMB0002
DECI01     if sqlcode = 100                                             RMB0002
DECI01        MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG   RMB0002
DECI01        PERFORM 9600-ABLOCAR                                      RMB0002
DECI01     END-IF.                                                      RMB0002
DECI01     PERFORM UNTIL SQLCODE NOT =0                                 RMB0002
DECI01        IF C5 = SPACES                                            RMB0002
DECI01           MOVE " N/A"     TO C5                                  RMB0002
DECI01        ELSE                                                      RMB0002
DECI01           IF C5 = "Y"                                            RMB0002
DECI01              MOVE "ACTIV"     TO C5                              RMB0002
DECI01           ELSE                                                   RMB0002
DECI01              MOVE "INACT"     TO C5                              RMB0002
DECI01           END-IF                                                 RMB0002
DECI01        END-IF                                                    RMB0002
DECI01        MOVE SPACES TO AREA-MSG                                   RMB0002
DECI01        STRING C1 (1:11)     *> NIT                               RMB0002
DECI01               " "                                                RMB0002
DECI01               C2 (1:20)     *> EMPRESA                           RMB0002
DECI01               " "                                                RMB0002
DECI01               C3 (1:20)     *> ROL                               RMB0002
DECI01               " "                                                RMB0002
DECI01               C4 (1:20)     *> ESTRATEGIA                        RMB0002
DECI01               " "                                                RMB0002
DECI01               C5 (1:05)     *> ACTIVA                            RMB0002
DECI01        INTO AREA-MSG                                             RMB0002
DECI01        PERFORM 9600-ABLOCAR                                      RMB0002
DECI01        EXEC SQL                                                  RMB0002
DECI01           fetch vcurs into :C1,:C2,:C3,:C4,:C5                   RMB0002
DECI01        END-EXEC                                                  RMB0002
DECI01     END-PERFORM.                                                 RMB0002
DECI01     if sqlcode not = 100                                         RMB0002
DECI01          MOVE SQLCODE TO sqlcode-I                               RMB0002
DECI01          STRING "Error: cannot fetch   ",                        RMB0002
DECI01                 WS-ALIAS                                         RMB0002
DECI01                 sqlcode-I                                        RMB0002
DECI01                 sqlerrmc                                         RMB0002
DECI01          INTO AREA-MSG                                           RMB0002
DECI01          PERFORM 9600-ABLOCAR                                    RMB0002
DECI01     end-if.                                                      RMB0002
DECI01     PERFORM 1000-CLOSEVCURS.                                     RMB0002
DECI01 4720-BUSCAR-DECI-DL2.                                            RMB0002
REMARK*******************************************************************
REMARK*4720-BUSCAR-DECI-DL2.
REMARK*    SE REALIZA EL SELECT EN LA BASE DE DATOS Y SE RECUPERAN LOS
REMARK*    DATOS
REMARK*******************************************************************
DECI01     PERFORM 1000-PREPARE.                                        RMB0002
DECI01     INITIALIZE C1,C2,C3,C4,C5 .                                  RMB0002
DECI01     EXEC SQL                                                     RMB0002
DECI01         fetch vcurs into :C1,:C2,:C3,:C4,:C5                     RMB0002
DECI01     END-EXEC                                                     RMB0002
DECI01     if sqlcode = 100                                             RMB0002
DECI01        MOVE "NO HAY REGISTROS EN LA BASE DE DATOS" TO AREA-MSG   RMB0002
DECI01        PERFORM 9600-ABLOCAR                                      RMB0002
DECI01     END-IF.                                                      RMB0002
DECI01     PERFORM UNTIL SQLCODE NOT =0                                 RMB0002
DECI01        IF C5 = "1"                                               RMB0002
DECI01           MOVE "ACTIV"     TO C5                                 RMB0002
DECI01        ELSE                                                      RMB0002
DECI01           MOVE "INACT"     TO C5                                 RMB0002
DECI01        END-IF                                                    RMB0002
DECI01        MOVE SPACES TO AREA-MSG                                   RMB0002
DECI01        STRING C2 (1:11)                                          RMB0002
DECI01               " "                                                RMB0002
DECI01               C1 (1:40)                                          RMB0002
DECI01               "                       "                          RMB0002
DECI01               C5 (1:05)                                          RMB0002
DECI01        INTO AREA-MSG                                             RMB0002
DECI01        PERFORM 9600-ABLOCAR                                      RMB0002
DECI01        EXEC SQL                                                  RMB0002
DECI01           fetch vcurs into :C1,:C2,:C3,:C4,:C5                   RMB0002
DECI01        END-EXEC                                                  RMB0002
DECI01     END-PERFORM.                                                 RMB0002
DECI01     if sqlcode not = 100                                         RMB0002
DECI01          MOVE SQLCODE TO sqlcode-I                               RMB0002
DECI01          STRING "Error: cannot fetch   ",                        RMB0002
DECI01                 WS-ALIAS                                         RMB0002
DECI01                 sqlcode-I                                        RMB0002
DECI01                 sqlerrmc                                         RMB0002
DECI01          INTO AREA-MSG                                           RMB0002
DECI01          PERFORM 9600-ABLOCAR                                    RMB0002
DECI01     end-if.                                                      RMB0002
DECI01     PERFORM 1000-CLOSEVCURS.                                     RMB0002
       1000-CONECTAR-XXXX-DL1.
REMARK******************************************************************
REMARK*1000-CONECTAR-XXXX-DL1.
REMARK*    RUTINA PARA REALIZAR UNA CONEXION ESTANDARD A UNA BASE DE DATOS
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
TAKEOV*    IF SQLCODE NOT = 0 OR 1 = 1
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
                DISPLAY "Error: cannot commit  ",WS-ALIAS
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
                DISPLAY "Error: cannot discon  ",WS-ALIAS
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
       8400-FINALIZAR.
REMARK******************************************************************
REMARK*8400-FINALIZAR.
REMARK* SE CIERRA LA COMUNICACION CON EL MODULO CCI.                   *
REMARK******************************************************************
           IF COD-ENTRADA = "SERVER" 
              CALL CCI-CLOSESERVER USING SRVRHANDLE
              IF RETURN-CODE NOT = 0
                 MOVE RETURN-CODE TO X-RCODE
                 STRING "ERROR AL LLAMAR: CCI-CLOSESERVER ", X-RCODE  
                        DELIMITED BY SIZE INTO MENSAJE-EMAIL          
                 CALL "SYSTEM" USING COMANDO-EMAIL                    
              END-IF
           END-IF.
       9600-ABLOCAR.
REMARK******************************************************************
REMARK*9600-ABLOCAR.
REMARK* ESTA RUTINA SE PUEDE INVOCAR PARA MANDAR MENSAJES ABLOCADOS,   *
REMARK* LA VARIABLE IND-CTA CONTROLA LA POSICION DENTRO DE V-OUTPUT    *
REMARK* AL ACABAR DE RESPONDER UNA TRANSACCION SIEMPRE HAY QUE         *
REMARK* INVOVAR LA RUTINA 9700-FIN-ABLOCAR PARA DESOCUPAR EL BUFFER    *
REMARK* LOS MENSAJES SON DE 80 BYTES Y MAXIMO 24
REMARK******************************************************************
           MOVE AREA-MSG  TO V-OUTPUT(IND-CTA)
           ADD 1 TO IND-CTA .
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
           MOVE 1 TO IND-RMB.
           IF COD-ENTRADA = "SERVER" 
              PERFORM 10910-ENVIA-SERVER
           ELSE
              PERFORM 10920-ENVIA-DISPLAY.
       10910-ENVIA-SERVER.
REMARK******************************************************************
REMARK*10910-ENVIA-SERVER.
REMARK* SE PROCEDE A CONTESTAR AL MODULO CCI, SE MANEJA LA SIGUIENTE   *
REMARK* CONVENCION: SI EL IND-CTA TIENE UN VALOR DE:                   *
REMARK*        0 ===> Mensaje de control, no mas y no mandar al cliente*
REMARK*        1 ===> Mensaje normal,     no mas y si mandar al cliente*
REMARK*       >1 ===> Mensaje multiple,   hay mas y si mandar al cliente
REMARK*                                   ind-cta menos uno            *
REMARK* Si hubo error en el envio se prende un flag para dejar de enviar
REMARK******************************************************************
             MOVE 1 TO HEADER-CARACT-INICIO
             MOVE 2 TO HEADER-CARACT-FIN.
             MOVE AREA-OUTPUT TO TEXTO.
             IF IND-CTA = 0 
                MOVE 80 TO HEADER-LONGITUD 
                MOVE 1  TO HEADER-TIPO-MENSAJE
             ELSE
                IF IND-CTA = 1
                   MOVE 2 TO HEADER-TIPO-MENSAJE
                   MOVE 80 TO HEADER-LONGITUD 
                ELSE
                   MOVE 0 TO HEADER-TIPO-MENSAJE
                   COMPUTE HEADER-LONGITUD = 80 * (IND-CTA - 1).
             MOVE LENGTH OF MENSAJE-CS TO SENDLEN.
             MOVE ZEROES TO CCIEND
             MOVE 0 TO ASYNC
             IF ERROR-SEND = 0 
                CALL CCI-SEND USING SESSID , MENSAJE-CS , SENDLEN ,
                           ASYNC , CCIEND
             END-IF
             IF RETURN-CODE not = 0 
                MOVE 1 TO ERROR-SEND
             END-IF
             MOVE 1 TO IND-CTA.
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
              PERFORM 10900-DISPLAY IND-CTA TIMES. 
           MOVE 1 TO IND-CTA.
           DISPLAY ETX WITH NO ADVANCING.
       10900-DISPLAY. 
REMARK******************************************************************
REMARK*10900-DISPLAY.
REMARK* SE REALIZA EL DISPLAY DE LOS MENSAJES A RETORNAR               *
REMARK******************************************************************
           DISPLAY V-OUTPUT(IND-RMB) .
           ADD 1 TO IND-RMB. 
       9000-CARGAR-MEM-SCOREXTR.                                        
REMARK*****************************************************************
REMARK*9000-CARGAR-MEM-SCOREXTR.                                        
REMARK*    SE CARGA EN MEMORIA DE LA TABLA DE SCORES EXTERNOS SU
REMARK*    DEFINICION
REMARK*****************************************************************
         INITIALIZE TABLA-SCOREXTR-AUX.                                 
         OPEN INPUT SCOREXTR.                                           
         MOVE 0 TO FIN-SCOREXTR.                                        
         MOVE 1 TO I-EXT                                                
         READ SCOREXTR IGNORE LOCK AT END MOVE 1 TO FIN-SCOREXTR. 
         PERFORM UNTIL FIN-SCOREXTR = 1                                 
           MOVE NIT-SUSCR-SCOREXTR TO NIT-SUSCR-SCOREXTR-AUX (I-EXT)    
           MOVE SCORE-SCOREXTR     TO SCORE-SCOREXTR-AUX    (I-EXT)     
           MOVE CSV-SCOREXTR       TO CSV-SCOREXTR-AUX     (I-EXT)      
           ADD 1 TO I-EXT                                               
           READ SCOREXTR IGNORE LOCK AT END MOVE 1 TO FIN-SCOREXTR
           END-READ                                                     
         END-PERFORM.                                                   
         CLOSE SCOREXTR.                                                
         SORT V-SCOREXTR-AUX ASCENDING NIT-SUSCR-SCOREXTR-AUX           
                                        SCORE-SCOREXTR-AUX .            
       9000-BUSCAR-MEM-SCOREXTR.                                        
REMARK****************************************************************
REMARK*9000-BUSCAR-MEM-SCOREXTR.                                        
REMARK*    SE BUSCA EN MEMORIA EL SCORE EXTERNO PARA HALLAR SUS
REMARK*    CARACTERISTICAS
REMARK****************************************************************
           PERFORM 9000-SEARCH-MEM-SCOREXTR.                            
           MOVE 0 TO CSV-SCOREXTR-OUTPUT     
           IF HAY-SCOREXTR = 1                                          
              MOVE CSV-SCOREXTR-AUX(I-EXT) TO CSV-SCOREXTR-OUTPUT       
           END-IF.                                                      
       9000-SEARCH-MEM-SCOREXTR.                                        
REMARK*****************************************************************
REMARK*9000-SEARCH-MEM-SCOREXTR.                                        
REMARK*    SE BUSCA BINARIAMENTE EL SCORE EXTERNO
REMARK*****************************************************************
           SEARCH ALL V-SCOREXTR-AUX  AT END                            
                   MOVE 0   TO HAY-SCOREXTR                             
              WHEN LLAVE-SCOREXTR-AUX(INDEX-EXT) = LLAVE-SCOREXTR-INPUT 
                   MOVE 1 TO HAY-SCOREXTR                               
                   SET I-EXT TO INDEX-EXT                               
           END-SEARCH.                                                 
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
           MOVE SPACES TO TXT-ARC-BIND
           STRING "$TEMPORALES/" WS-PROGRAMA DELIMITED BY SIZE ".BIND" 
           INTO TXT-ARC-BIND.
           OPEN OUTPUT ARC-BIND.
           MOVE SPACES TO REG-BIND.
           STRING "db2 CONNECT TO " WS-ALIAS DELIMITED BY "  "
           " USER " WS-USR DELIMITED BY "  "
           " USING " WS-PASS  DELIMITED BY "  "
           INTO REG-BIND
           WRITE REG-BIND
      *    get HCOBND environment variable, complain if not defined
           MOVE REG-BIND TO REG-BIND-AUX1
           MOVE "HCOBND" TO VAR-AMBIENTE
           DISPLAY VAR-AMBIENTE UPON ENVIRONMENT-NAME
           ACCEPT VAR-AMBIENTE FROM ENVIRONMENT-VALUE
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
           END-IF
      *    sentence: bind - add collection & quit
           MOVE SPACES TO REG-BIND
           STRING "db2 bind $HCOBND/" WS-PROGRAMA DELIMITED BY SIZE
           ".bnd ACTION ADD COLLECTION COBOLAPP " INTO REG-BIND
           WRITE REG-BIND
           MOVE "db2 quit" TO REG-BIND
           WRITE REG-BIND
           MOVE REG-BIND TO REG-BIND-AUX2
REMARK*    REALIZAR LA ACTUALIZACION DEL PAQUETE & quit
           WRITE REG-BIND FROM REG-BIND-AUX1
           MOVE SPACES TO REG-BIND
           STRING "db2 bind $HCOBND/" WS-PROGRAMA DELIMITED BY SIZE
           ".bnd ACTION REPLACE REPLVER V8 RETAIN YES "
          "COLLECTION COBOLAPP " INTO REG-BIND
           WRITE REG-BIND
           WRITE REG-BIND FROM REG-BIND-AUX2
           CLOSE ARC-BIND.
REMARK*    se hace ejecutable el arch BIND y se lo ejecuta
           MOVE SPACES TO WS-ARCHIVO 
           STRING "chmod +x $TEMPORALES/" WS-PROGRAMA DELIMITED BY SIZE 
          ".BIND " INTO WS-ARCHIVO 
           MOVE WS-ARCHIVO  TO T-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO
      *    ejecucion del comando
           MOVE SPACES TO WS-ARCHIVO
           STRING "$TEMPORALES/" WS-PROGRAMA DELIMITED BY SIZE 
           ".BIND " INTO WS-ARCHIVO 
           MOVE WS-ARCHIVO  TO T-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO
           IF I-XX > 0
              MOVE SPACES TO WS-ARCHIVO
              STRING "cp $TEMPORALES/" WS-PROGRAMA DELIMITED BY SIZE 
              ".BIND $TEMPORALES/RMBBIND"               
              I-XX DELIMITED BY SIZE INTO WS-ARCHIVO 
              MOVE WS-ARCHIVO  TO T-COMANDO
              CALL "SYSTEM" USING TEXTO-COMANDO
              ADD 1 TO I-XX 
           END-IF
      *    eliminacion higienica del archivo comando
           MOVE SPACES TO WS-ARCHIVO
           STRING "rm $TEMPORALES/" WS-PROGRAMA  DELIMITED BY SIZE 
           ".BIND " INTO WS-ARCHIVO 
           MOVE WS-ARCHIVO  TO T-COMANDO
           CALL "SYSTEM" USING TEXTO-COMANDO.

       1000-CREDENCIALES-DB2SSO-RUTSQL.
REMARK******************************************************************
REMARK*1000-CREDENCIALES-DEB2SSO-RUTSQL.
REMARK*    BUSQUEDA DE EL CODIGO-PASSWORD PARA ACCEDER A LA T.CONEXION
REMARK*    ACCEDE ARCHVO DE LA VARIABLE DE AMBIENTE $DB2_SSO
REMARK*    INICIALMENTE. SE COLOCA EN UN ARCHIVO GENERICO DANDO:
REMARK*        AMBIENTE
REMARK*        ALIAS     
REMARK*        USUARIO   
REMARK*        PASSWORD  
REMARK*    SALIDA: 
REMARK*         WS-ALIAS 
REMARK*         WS-USR 
REMARK*         WS-PASS 
REMARK******************************************************************
           MOVE "DB2_SSO" TO WS-DBCONEXION
           DISPLAY WS-DBCONEXION UPON ENVIRONMENT-NAME
           ACCEPT WS-DBCONEXION FROM ENVIRONMENT-VALUE
           IF WS-DBCONEXION = "DB2_SSO"
              DISPLAY "No se ha fijado: $DB2_SSO  !!!"
              STOP RUN
           ELSE
              MOVE WS-DBCONEXION TO TXT-ARC-BIND
           END-IF
           OPEN INPUT ARC-BIND
           READ ARC-BIND IGNORE LOCK AT END MOVE 1 TO FIN-BIND
           INITIALIZE WS-USR WS-PASS WS-ALIAS
           PERFORM UNTIL FIN-BIND = 1
              INITIALIZE WS-TOKEN WS-P1 WS-P2  
              UNSTRING REG-BIND DELIMITED BY "=" INTO
                 WS-TOKEN
              UNSTRING REG-BIND DELIMITED BY "'" INTO
                 WS-P1 WS-P2  
                 EVALUATE WS-TOKEN
                   WHEN "@BaseDatos"
                      MOVE WS-P2 TO WS-ALIAS-VICTOR        
                   WHEN "@Usuario"    
                      MOVE WS-P2 TO WS-USR-VICTOR         
                   WHEN "@Clave"    
                      MOVE WS-P2 TO WS-PASS-VICTOR         
                 END-EVALUATE
                 READ ARC-BIND IGNORE LOCK AT END MOVE 1 TO FIN-BIND
                 END-READ
           END-PERFORM.
           CLOSE ARC-BIND.
           IF WS-ALIAS-VICTOR = SPACES AND PRIV = 0
              DISPLAY "ERROR,NO SE ENCONTRO AMBIENTE DECLARADO"
              STOP RUN
           END-IF.
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
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot connect ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           end-if.
*
           EXEC SQL
               prepare dynamic_sql from :ws-dyn-query
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot prepare ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
               STOP RUN
           end-if.
*
           EXEC SQL
               open vcurs using :int-col
           END-EXEC
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot open vc ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           end-if.
*
           INITIALIZE C1,C2,C3.
           EXEC SQL
               fetch vcurs into :C1,:C2,:C3
           END-EXEC
           if sqlcode not = 0
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
           end-if.
*
           EXEC SQL
               close vcurs
           END-EXEC.
           if sqlcode not = 0
                MOVE SQLCODE TO sqlcode-I
                DISPLAY "Error: cannot clos vc ",WS-ALIAS
                      sqlcode-I
                      sqlerrmc
                STOP RUN
           end-if.
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
           IF PRIV = 1
              MOVE "$TEMPORALES/ICBCLA" TO  CLAVE-TITLE
           END-IF.
           MOVE SPACES TO TXT-ARC-BIND.
           STRING
               "$TEMPORALES/"
               WS-PROGRAMA DELIMITED BY SIZE
               ".MASIVO"
           INTO TXT-ARC-BIND.
           OPEN OUTPUT ARC-BIND.
           MOVE 0 TO MENS-ERR
           OPEN INPUT CLAVE.
           READ CLAVE NEXT RECORD IGNORE LOCK AT END
                         MOVE 1 TO MENS-ERR
           END-READ.
           PERFORM UNTIL MENS-ERR = 1
              MOVE TIP-ID-ABD09 TO TIPIDE-MASIVO
              MOVE NUM-ID-ABD09 TO NUMIDE-MASIVO
              WRITE REG-BIND FROM REG-MASIVO
              READ CLAVE NEXT RECORD IGNORE LOCK AT END
                         MOVE 1 TO MENS-ERR
              END-READ
           END-PERFORM.
           MOVE "P0000000000000000" TO REG-BIND.
           WRITE REG-BIND.
           CLOSE CLAVE.
           CLOSE ARC-BIND.
REMARK******************************************************************
PRO001 1000-ALFA-A-NUMERO.                                              RMB0003
REMARK******************************************************************
REMARK*1000-ALFA-A-NUMERO.
REMARK******************************************************************
PRO001     MOVE 2000 TO PRONUM-CONV                                     RMB0003
PRO001     MOVE 1 TO I-CONV.                                            RMB0003
PRO001     PERFORM UNTIL I-CONV = 2000 OR T-CONV(I-CONV) = "##"         RMB0003
PRO001                                 OR PRONUM-CONV <> 2000           RMB0003
PRO001       IF PROALF-CONV = T-CONV(I-CONV)                            RMB0003
PRO001          MOVE I-CONV TO PRONUM-CONV                              RMB0003
PRO001       END-IF                                                     RMB0003
PRO001       ADD 1 TO I-CONV                                            RMB0003
PRO001     END-PERFORM.                                                 RMB0003
       COPY BDIIVIDE.PROC.
       COPY VALNOM.PROC.     
       COPY UTLCRI.PROC.  
